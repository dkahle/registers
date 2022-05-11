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
#' registers()
#'
#' set_focus("cars")
#' glimpse_focus()
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
set_focus <- function(name) {

  assign(
    "x" = "focus",
    "value" = list(
      "name" = name,
      "envir" = parent.frame()
    ),
    "envir" = registers()
  )

}





#' @rdname set_register
#' @export
set_focus_highlighted <- function() {

  context <- rstudioapi::getActiveDocumentContext()

  cli_alert_success("Focusing on `{context$selection[[1]]$text}`...")

  assign(
    "x" = "focus",
    "value" = list(
      # "name" = substitute(data_binding),
      "name" = as.name(context$selection[[1]]$text),
      "envir" = parent.frame()
    ),
    "envir" = registers()
  )

}



#' @rdname set_register
#' @export
set_focus_window <- function() {

  response <- rstudioapi::showPrompt(
    title = "Set focus",
    message = "Name of the R object to focus on?"
  )

  assign(
    "x" = "focus",
    "value" = list(
      "name" = as.name(response),
      "envir" = parent.frame()
    ),
    "envir" = registers()
  )

}









#' @rdname set_register
#' @export
glimpse_focus <- function() {

  f <- get("focus", envir = registers())
  data <- eval(parse(text = f$name), envir = f$envir)

  cli_h1("`{f$name}` [{paste(class(data), collapse = ', ')}]")

  if (inherits(data, "data.frame")) {
    dplyr::glimpse( data )
  } else if (is.vector(data)) {
    str(data)
  } else if (is.function(data)) {
    print(data)
  } else {
    dplyr::glimpse( dplyr::collect(data) )
  }
  cat("\n")

}








#' @rdname set_register
#' @export
glimpse_highlighted <- function() {

  context <- rstudioapi::getActiveDocumentContext()

  data <- eval(
    parse(text = context$selection[[1]]$text),
    envir = parent.frame()
  )

  cli_h1("Glimpsing highlighted object [{paste(class(data), collapse = ', ')}]")

  if (inherits(data, "data.frame")) {
    dplyr::glimpse( data )
  } else if (is.vector(data)) {
    str(data)
  } else if (is.function(data)) {
    print(data)
  } else {
    dplyr::glimpse( dplyr::collect(data) )
  }

  cat("\n")
}



