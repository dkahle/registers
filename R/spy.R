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
spy <- function(
  data,
  name = if (!is.null(attr(data, "name")))
    attr(data, "name") else deparse(substitute(data))
) {

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

  print(boxx(msg, header = title, padding = 0, border_style = "round", border_col = "gray75"))
  invisible(data)

}
# spy(mtcars)
# spy(cars)








