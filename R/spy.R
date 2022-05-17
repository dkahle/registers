#' Spy
#'
#' Spy
#'
#' @param data A dataset (or expression that returns one)
#' @param name The binding name to focus on, as a string
#' @return Invisible \code{NULL}
#' @name spy
#' @examples
#'
#' spy(cars)
#'
#' # Shift-Cmd-H
#' cars
#'





# spy below should be a generic
#' @rdname set_register
#' @export
spy <- function(data, name = deparse(substitute(data))) {

  title <- glue("`{name}` [{paste(class(data), collapse = ', ')}]")

  if (inherits(data, "data.frame")) {
    msg <- capture.output(dplyr::glimpse(data))
  } else if (is.vector(data)) {
    msg <- capture.output(str(data))
  } else if (is.function(data)) {
    msg <- capture.output(print(data))
  } else {
    msg <- capture.output(dplyr::glimpse( dplyr::collect(data) ))
  }

  print(boxx(msg, header = title, padding = 0))
  invisible(data)

}
# spy(mtcars)
# spy(cars)





#' @rdname set_register
#' @export
spy_focus <- function() {

  if (!rexists("f")) {
    cli_alert_danger("No focus has been set.") # if stop(), addin makes window
    return(invisible(NULL))
  }

  f <- rget("f")
  data <- eval(f$name, envir = f$envir)

  spy(data, deparse(f$name))

}





#' @rdname set_register
#' @export
spy_highlighted <- function() {

  context <- rstudioapi::getActiveDocumentContext()
  highlighted_text <- context$selection[[1]]$text
  highlighted_text <- ez_distill(highlighted_text)
  data <- eval(parse(text = highlighted_text), envir = parent.frame())
  spy(data, highlighted_text)

}




