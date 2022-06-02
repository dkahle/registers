#' Spy
#'
#' Spy
#'
#' @param x Object to be printed
#' @param ... Additional arguments, currently discarded
#' @param n For data frames, rows to be printed
#' @param name Name of object to be printed
#' @return Invisible \code{NULL}
#' @name spy
#' @examples
#'
#' spy(cars)
#' spy_top(cars)
#' spy_btm(cars)
#' spy_all(cars)
#' # spy_all(ggplot2::diamonds)
#'
#' # in .Rprofile, set
#' set_register(    spy(), "4") # map to Shift-Cmd-G
#' set_register(spy_top(), "5") # map to Shift-Cmd-H
#' set_register(spy_btm(), "6") # map to Shift-Cmd-J
#' set_register(spy_all(), "7") # map to Shift-Cmd-A
#'
#' # highlight cars, then Shift-Cmd-(G,H,J,A)
#' cars
#'




#' @rdname spy
#' @export
spy <- function(x, ...) {
  UseMethod("spy")
}


#' @rdname spy
#' @export
spy.data.frame <- function(x, ..., name = deparse(substitute(x))) {
  title <- glue("`{name}` {paste(class(x), collapse = ', ')} [{nrow(x)} \u00d7 {ncol(x)}]")
  msg <- capture.output(dplyr::glimpse(x))[-(1:2)]
  print(boxx(msg, header = title, padding = 0, border_style = "round", border_col = "gray75"))
  invisible(x)
}



#' @rdname spy
#' @export
spy.character <- function(x, ..., name = deparse(substitute(x))) {
  title <- glue("`{name}` {paste(class(x), collapse = ', ')} [{length(x)}]")
  msg <- ez_trunc(capture.output(cat(x)), width = console_width() - 2L)
  print(boxx(msg, header = title, padding = 0, border_style = "round", border_col = "gray75"))
  invisible(x)
}










#' @rdname spy
#' @export
spy_top <- function(x, ...) {
  UseMethod("spy_top")
}


#' @rdname spy
#' @export
spy_top.data.frame <- function(x, ..., n = 20) {
  msg <- capture.output(print(x[1:min(nrow(x),n),]))[-1]
  print(
    cli::boxx(
      msg,
      padding = 0,
      border_style = "round",
      border_col = "gray75",
      header = glue::glue("{nrow(x)} \u00d7 {ncol(x)}")
    )
  )
}










#' @rdname spy
#' @export
spy_btm <- function(x, ...) {
  UseMethod("spy_btm")
}


#' @rdname spy
#' @export
spy_btm.data.frame <- function(x, ..., n = 20) {
  msg <- capture.output(print(x[nrow(x):1,][1:min(nrow(x),n),]))[-1]
  print(
    cli::boxx(
      msg,
      padding = 0,
      border_style = "round",
      border_col = "gray75",
      header = glue::glue("{nrow(x)} \u00d7 {ncol(x)} in reverse")
    )
  )
}











#' @rdname spy
#' @export
spy_all <- function(x, ...) {
  UseMethod("spy_all")
}


#' @rdname spy
#' @export
spy_all.data.frame <- function(x, ..., n = getOption("max.print") - 5L) {
  spy_top.data.frame(x, n = n)
}

