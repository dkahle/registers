#' Registers
#'
#' Set registers and register actions
#'
#' In Vim-speak, `focus` is the unnamed register.
#'
#' @param name The binding name to focus on, as a string
#' @return Invisible \code{NULL}
#' @name set_register
#' @examples
#'
#' # registers has a built in generic (function) called spy() that
#' # helps summarize objects succintly. it's a lot like dplyr::glimpse()
#' spy(cars)
#'
#'
#' # a register is a kind of placeholder for a chunk of R code, usually
#' # a chunk of R code that you like to run a lot.
#' # right now, you don't have any registers set
#' registers()
#'
#'
#' # you set registers with set_register()
#' set_register(spy(cars), "f")
#' registers()
#'
#'
#' # ring_register() evaluates the chunk of code you've registered on demand
#' ring_register("f")
#'
#'
#' # there's an addin to ring_register().
#' # in RStudio, go to Tools > Modify Keyboard Shortcuts...
#' # in the box type "ring", and change the shortcut of Ring register
#' # to Cmd-R and click Apply. now push Cmd-R, type "f", and enter.
#'
#'
#' # with this you can compose registers
#' set_register(cars, "d")
#' set_register(spy(), "g")
#' registers()
#' ring_register("dg")
#' print_head <- function(.) print(tibble::as_tibble(.))
#' set_register(print_head(), "h")
#' ring_register("dh")
#'
#'
#' # registers are functions desirable for their side effects, their
#' # values are invisibly returned to nowhere
#'
#' # "f" is the focus register. you can assign to the focus register
#' # more quickly with
#' set_focus(spy(airquality))
#' registers()
#'
#' # it's commonly the case that you're manipulating a single dataset
#' # and want to check in on it each time you touch it. this is where
#' # the focus action comes in. the focus action is a register like any
#' # other with one key difference: it's bindable without a popup window.
#' set_focus_action(spy(data))
#'
#' # in RStudio, go to Tools > Modify Keyboard Shortcuts...
#' # next to Set focus - highlighted, use Alt-Shift-Cmd-F
#' # next to Set focus - window, use Alt-F
#' # next to Glimpse focus, use Shift-Cmd-G
#'
#' # press Shift-Cmd-G
#'
#' airquality
#' # highlight airquality above and push Alt-Shift-Cmd-F
#' # press Shift-Cmd-G
#'
#' # in Tools > Modify Keyboard Shortcuts...,
#' # set Glimpse highlighted to Alt-Shift-H, then highlight the line below and
#' # press Shift-Cmd-H
#' subset(cars, speed > 10)
#'
#'
#' # interesting use case: generate a random subsample (that has a desired property)
#' # and save it. highlight the whole next line and Shift-Cmd-H
#' (cars_sample <- cars[sample(nrow(cars), 5),])
#' cars_sample
#'
#'
#'
#'









#' @rdname set_register
#' @export
set_register <- function(x, key,
  override = get_registers_option("override"),
  envir = parent.frame()
) {

  if (!is_valid_key(key)) { message("Key `{key}` not valid."); return(invisible()) }

  if (is_registered(key) && isFALSE(override)) {
    message(glue("Key {key} already registered."))
    return(invisible())
  }

  rset(
    "x" = key,
    "value" = structure(
      list("name" = substitute(x), "envir" = envir),
      dynamic_register = TRUE
    )
  )

}





#' @rdname set_register
#' @export
set_static_register <- function(x, key, override = get_registers_option("override")) {

  if (!is_valid_key(key)) { message("Key `{key}` not valid."); return(invisible()) }

  if (is_registered(key) && isFALSE(override)) {
    message(glue("Key {key} already registered."))
    return(invisible())
  }

  rset("x" = key, "value" = x)

}




#' @rdname set_register
#' @export
get_register <- function(key) {

  stopifnot(is_valid_key(key), length(key) == 1L)

  if (nchar(key) > 1) {
    if (key %notin% history_symbols) {
      message("Key should be a single character.", call. = FALSE)
      return(invisible())
    }
    key <- history_symbols_to_key(key)
  }

  val <- rget(key)

  if (is_dynamic_register(val)) val <- eval(val$name, envir = val$envir)

  val

}



#' @rdname set_register
#' @export
ring_register <- function(key) {

  if (missing(key)) {
    key <- rstudioapi::showPrompt(
      title = "Ring register",
      message = "Input register to ring.",
      default = ""
    )
  }

  if (is.null(key)) return(invisible())
  # if (!rexists(key)) {
  #   message(glue("Register `{key}` is empty."))
  #   return(invisible())
  # }

  if (nchar(key) > 1) {
    registers <- strsplit(key, "")[[1]]
    for( reg in registers ) {
      promise <- rget(reg)
      if (is.call(promise$name)) {
        fn <- eval(promise$name[[1]], envir = promise$envir)
        out <- fn(out)
      # } else if (bindingIsActive(promise$name, promise$envir)) {
      #   stop("not yet implemented")
      } else {
        val <- eval(promise$name, envir = promise$envir)
        out <- if (is.function(val)) {
          val(out)
        } else {
          val
        }
      }

    }
  } else {
    invisible(get_register(key))
  }

}





#' @rdname set_register
#' @export
set_focus <- function(x) {

  eval(
    substitute(
      set_register(x, "f", override = TRUE, envir = parent.frame(n = 3))
    ),
    envir = parent.frame()
  )

}



#' @rdname set_register
#' @export
set_focus_highlighted <- function() {

  context <- rstudioapi::getActiveDocumentContext()
  x <- context$selection[[1]]$text

  cli_alert_success("Focusing on `{x}`...")

  eval(
    substitute(
      set_register(expr, "f", override = TRUE, envir = parent.frame(n = 3)),
      list(expr = as.name(x))
    ),
    envir = parent.frame()
  )

}



#' @rdname set_register
#' @export
set_focus_window <- function() {

  x <- rstudioapi::showPrompt(
    title = "Set focus",
    message = "Name of the R object to focus on?"
  )

  cli_alert_success("Focusing on `{x}`...")

  eval(
    substitute(
      set_register(expr, "f", override = TRUE, envir = parent.frame(n = 3)),
      list(expr = as.name(x))
    ),
    envir = parent.frame()
  )

}






