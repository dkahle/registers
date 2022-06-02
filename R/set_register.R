#' Registers
#'
#' Set registers and register actions
#'
#' In Vim-speak, `focus` is the unnamed register.
#'
#' @param x Expression to store in register
#' @param key Register name, a character
#' @param clobber Should the current value of the register be overwritten?
#'   Defaults to `get_registers_option("clobber")`, which defaults to `TRUEi`.
#' @param envir Environment in which to evaluate the expression, defaults to
#'   `parent.frame()`.
#' @param reserved_key_bypass Used internally to override setting invalid keys.
#' @return Invisible \code{NULL}
#' @name set_register
#' @examples
#'
#' # registers has a built in generic (function) called spy() that
#' # helps summarize objects succinctly. it's a lot like dplyr::glimpse()
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
#' if (interactive()) ring_register()
#'
#'
#' # there's an addin to ring_register() that will open the same window.
#' # in RStudio, go to Tools > Modify Keyboard Shortcuts...
#' # in the box type "ring", and change the shortcut of Ring register
#' # to Cmd-R and click Apply. now push Cmd-R, type "f", and enter.
#'
#'
#' # unset_register() clears registers
#' unset_register("f")
#' registers()
#'
#'
#' # you can compose registers by listing several keys in ring_register()
#' set_register(cars, "d")
#' set_register(spy(), "g")
#' registers()
#' ring_register("dg")
#' ring_register("dg 3")
#' assigner <- function(x) assign("tmp", x, parent.frame(n = 3))
#' set_register(assigner(), "f")
#' ring_register("df")
#'
#'
#' # registers are R code you want to make hotkeyable.
#' # the code is mainly of interested for its side effects:
#' # the output will only be printed if there's an explicit call to something
#' # that prints to the screen, like print() or spy()
#' # values are invisibly returned to nowhere, and thus unbindable*
#'
#'
#' # registers 1-9 are hotkeyable via RStudio's addin feature.
#' # you can set them in either of two ways.
#' # way 1:
#' set_register(spy(mtcars), "1")
#' registers()
#' ring_register("1")
#' # way 2 is to map set_register_1() to a hotkey.
#' # i recommend setting Store highlighted to register 1 to Alt-Cmd-1
#' # then highlight the next line and press the hotkey
#' spy(mtcars)
#'
#'
#' # like storing code into numbered registers,
#' # ringing numbered registers is hotkeyable, too
#' # these are hotkeyed as Ring register 1 (2, 3, ...)
#' # i recommend setting these to Cmd-1, Cmd-2, and so on
#' # try ringing the register 1 you set above.
#'
#'
#'
#' # sometimes you have code in a register that you want to operate as an
#' # action, like the spy() and cars example above. spy() is code that
#' # is an action you want to run against cars. we're still thinking about
#' # the cleanest way to implement this, but for the time being you can
#' # just store each piece to a register, and then set ring_register() to
#' # a numbered action for example:
#' reset_registers()
#' set_register(cars, "d")
#' set_register(spy(), "g")
#' set_register(ring_register("dg"), "1")
#' registers()
#' ring_register("1")
#'
#'
#'
#' # one common workflow is to want to run a registers action on a highlighted
#' # object. currently, to do this you'd need to save the highlighted code
#' # to a register, bind a composite ring_register() to a register, and then
#' # ring that register. that requires typing, but the whole point is to move
#' # away from typing.
#' # to get around this, have the ring_register_#_on_highlighted() functions
#' # that can be hotkeyed. i recommend setting Ring register 1 on highlighted
#' # to Shift-Cmd-1 and so on.
#' # (note: on a mac, you can change built-in OS bindings in System Preferences
#' #        > Keyboard > Shortcuts)
#' reset_registers()
#' registers()
#' set_register(spy(), "1")
#'
#' print_lm <- function(df) print(summary(lm(dist ~ speed, data = df)))
#' set_register(print_lm(), "2")
#'
#' plot_sample <- function(df) with(df[sample(nrow(df),5),], plot(speed, dist))
#' set_register(plot_sample(), "3")
#'
#' cars # highlight this and Shift-Cmd-1, Shift-Cmd-2, Shift-Cmd-3
#'
#' # the highlighted text is stored to the 0 register
#' ring_register("01")
#'
#' # note: with ggplot2 graphics, you'll need to wrap the graphic in print()
#' # since ggplot() returns an object that renders when printed
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
#' # hint: your registers are almost entirely for interactive use. as you
#' # use them more, you'll develop a habit for the kinds of things you
#' # like to see (you'll probably know many off the bat).
#' # this is a tool for you by you.
#' # it seems to me like this one of the very few places where it's reasonable
#' # to put your own default registers into your .Rprofile file.
#' # you can use usethis::edit_r_profile() to get started
#' # i recommend starting by adding these lines:
#' # library("registers")
#' set_register(spy(), "4") # map to Shift-Cmd-G
#'
#' # note: this rstudio webpage explains where rstudio keybindings are
#' # stored: https://support.rstudio.com/hc/en-us/articles/206382178-Customizing-Keyboard-Shortcuts-in-the-RStudio-IDE
#'





#' @rdname set_register
#' @export
unset_register <- function(key) {

  if (length(key) > 1) vapply(key, unset_register, logical(1))

  if (!is_valid_key(key)) { message(glue("Key `{key}` not valid.")); return(invisible(FALSE)) }

  if (!rexists(key)) { message(glue("Key `{key}` is not registered.")); return(invisible(FALSE)) }

  if (is_reserved_key(key)) { message(glue("Key `{key}` is reserved.")); return(invisible(FALSE)) }

  rm(list = key, envir = registers())

}






#' @rdname set_register
#' @export
set_register <- function(
  x,
  key,
  clobber = get_registers_option("clobber"),
  envir = parent.frame(),
  reserved_key_bypass = FALSE
) {

  if (length(key) > 1)  { message("`set_register()` only accepts a single key."); return(invisible()) }

  if (reserved_key_bypass && !is_valid_key(key)) { message(glue("Key `{key}` not valid.")); return(invisible()) }

  if (reserved_key_bypass && is_reserved_key(key)) { message(glue("Key `{key}` is reserved.")); return(invisible()) }

  if (reserved_key_bypass && is_registered(key) && isFALSE(clobber)) {
    message(glue("Key {key} already registered."))
    return(invisible())
  }

  rset(
    "x" = key,
    "value" = structure(
      list("name" = substitute(x), "envir" = envir),
      active_register = TRUE
    )
  )

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

  if (is.numeric(key)) key <- as.character(key)

  registers <- strsplit(key, "")[[1]]

  if (any(registers == " ")) {
    if (sum(registers == " ") > 1) cli_alert_danger("Invalid register composition.")
    break_ndx <- which(registers == " ")
    n_reg <- length(registers)
    key <- paste(registers[1:(break_ndx-1)], collapse = "")
    times <- as.integer(paste(registers[(break_ndx+1):n_reg], collapse = ""))
    for (k in 1:times) ring_register(key)
    return(invisible())
  }

  registers <- key_to_history_symbols(registers)

  # return early if no registers exist
  registers_exist <- vapply(registers, rexists, logical(1))
  if ( !all(registers_exist) ) {
    if (sum(!registers_exist) > 1) {
      cli_alert_danger("Registers {paste0('`', registers[!registers_exist], '`', collapse = ', ')} are empty.")
    } else {
      cli_alert_danger("Register `{registers[!registers_exist]}` is empty.")
    }
    return(invisible())
  }


  # iterate over registers
  for( reg in registers ) {

    if (reg %in% history_symbols) {
      out <- rget(reg)
      next
    }

    # extract promise
    promise <- rget(reg)

    # make short-hand references
    n <- promise$name; e <- promise$envir

    # process promise
    if (is.call(n)) {
      if (exists("out")) {
        fn <- eval(n[[1]], envir = e)
        out <- fn(out)
      } else {
        out <- eval(n, envir = e)
      }
    } else if (
      (deparse(n) %in% ls(e, all.names = TRUE)) && bindingIsActive(deparse(n), e)
    ) {
      stop("not yet implemented")
    } else {
      val <- eval(n, envir = e)
      out <- if (is.function(val)) val(out) else val
    }

  }

  # return
  invisible(out)

}






# ring_numbered_register() is needed for hotkeying ringing numbered registers
ring_numbered_register <- function(key) {

  function() {

    if (is.expression(key)) key <- deparse(key)
    if (is.numeric(key)) key <- as.character(key)

    if (!rexists(key)) {
      cli_alert_danger(glue("Register {key} has not key been set.")) # if stop(), addin makes window
      return(invisible(NULL))
    }

    f <- rget(key)

    # message(glue("Evaluating {capture.output(print(f$name))} in {capture.output(print(f$envir))}"))
    tryCatch(
      eval(f$name, envir = f$envir),
      error = function(e) cli_alert_danger(glue("Ringing {key} failed."))
    )

    return(invisible(NULL))

  }

}


#' @rdname set_register
#' @export
ring_register_1 <- ring_numbered_register("1")

#' @rdname set_register
#' @export
ring_register_2 <- ring_numbered_register("2")

#' @rdname set_register
#' @export
ring_register_3 <- ring_numbered_register("3")

#' @rdname set_register
#' @export
ring_register_4 <- ring_numbered_register("4")

#' @rdname set_register
#' @export
ring_register_5 <- ring_numbered_register("5")

#' @rdname set_register
#' @export
ring_register_6 <- ring_numbered_register("6")

#' @rdname set_register
#' @export
ring_register_7 <- ring_numbered_register("7")

#' @rdname set_register
#' @export
ring_register_8 <- ring_numbered_register("8")

#' @rdname set_register
#' @export
ring_register_9 <- ring_numbered_register("9")

#' @rdname set_register
#' @export
ring_register_dot <- ring_numbered_register(".")

#' @rdname set_register
#' @export
ring_register_dot_dot <- ring_numbered_register(":")









set_register_highlighted <- function(key, message = TRUE) {

  function() {

    if (is.expression(key)) key <- deparse(key)
    if (is.numeric(key)) key <- as.character(key)

    context <- rstudioapi::getActiveDocumentContext()
    x <- context$selection[[1]]$text
    if (x == "") {
      cli_alert_danger("No text highlighted.")
      return(invisible())
    }

    eval(
      substitute(
        registers::set_register(
          expr,
          key,
          clobber = get_registers_option("clobber"),
          envir = parent.frame(n = 3)
        ),
        list(
          expr = parse(text = x)[[1]],
          key = key
        )
      ),
      envir = parent.frame()
    )

    x_formatted <- ez_trunc(ez_distill(x), console_width() - 22L)
    if (message) cli_alert_success("Register {key} set to `{x_formatted}`")

    invisible(x)

  }

}

#' @rdname set_register
#' @export
set_register_1_highlighted <- set_register_highlighted("1")

#' @rdname set_register
#' @export
set_register_2_highlighted <- set_register_highlighted("2")

#' @rdname set_register
#' @export
set_register_3_highlighted <- set_register_highlighted("3")

#' @rdname set_register
#' @export
set_register_4_highlighted <- set_register_highlighted("4")

#' @rdname set_register
#' @export
set_register_5_highlighted <- set_register_highlighted("5")

#' @rdname set_register
#' @export
set_register_6_highlighted <- set_register_highlighted("6")

#' @rdname set_register
#' @export
set_register_7_highlighted <- set_register_highlighted("7")

#' @rdname set_register
#' @export
set_register_8_highlighted <- set_register_highlighted("8")

#' @rdname set_register
#' @export
set_register_9_highlighted <- set_register_highlighted("9")







ring_register_on_highlighted <- function(key, ...) {

  function() {

    # save highlighted to register 0
    set_register_highlighted("0", message = FALSE)()

    # ring register on highlighted
    ring_register(paste0("0", key))

  }

}

#' @rdname set_register
#' @export
ring_register_1_on_highlighted <- ring_register_on_highlighted("1")

#' @rdname set_register
#' @export
ring_register_2_on_highlighted <- ring_register_on_highlighted("2")

#' @rdname set_register
#' @export
ring_register_3_on_highlighted <- ring_register_on_highlighted("3")

#' @rdname set_register
#' @export
ring_register_4_on_highlighted <- ring_register_on_highlighted("4")

#' @rdname set_register
#' @export
ring_register_5_on_highlighted <- ring_register_on_highlighted("5")

#' @rdname set_register
#' @export
ring_register_6_on_highlighted <- ring_register_on_highlighted("6")

#' @rdname set_register
#' @export
ring_register_7_on_highlighted <- ring_register_on_highlighted("7")

#' @rdname set_register
#' @export
ring_register_8_on_highlighted <- ring_register_on_highlighted("8")

#' @rdname set_register
#' @export
ring_register_9_on_highlighted <- ring_register_on_highlighted("9")


#' @rdname set_register
#' @export
ring_register_dot_on_highlighted <- ring_register_on_highlighted(".")

#' @rdname set_register
#' @export
ring_register_dot_dot_on_highlighted <- ring_register_on_highlighted(":")

