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
#' spy_top(cars)
#' spy_btm(cars)
#' spy_all(cars)
#' spy_all(ggplot2::diamonds)
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
spy <- function(...) {
  UseMethod("spy")
}


#' @rdname spy
#' @export
spy.data.frame <- function(x, name = deparse(substitute(x))) {
  title <- glue("`{name}` [{paste(class(x), collapse = ', ')}]")
  msg <- capture.output(dplyr::glimpse(x))
  print(boxx(msg, header = title, padding = 0, border_style = "round", border_col = "gray75"))
  invisible(x)
}










#' @rdname spy
#' @export
spy_top <- function(...) {
  UseMethod("spy_top")
}


#' @rdname spy
#' @export
spy_top.data.frame <- function(df, n = 20) {
  msg <- capture.output(print(tibble::as_tibble(df), n = n))[-1]
  print(
    cli::boxx(
      msg,
      padding = 0,
      border_style = "round",
      border_col = "gray75",
      header = glue::glue("{nrow(df)} \u00d7 {ncol(df)}")
    )
  )
}










#' @rdname spy
#' @export
spy_btm <- function(...) {
  UseMethod("spy_btm")
}


#' @rdname spy
#' @export
spy_btm.data.frame <- function(df, n = 20) {
  tb <- tibble::as_tibble(df[nrow(df):1,])
  msg <- capture.output(print(tb, n = n))[-1]
  print(
    cli::boxx(
      msg,
      padding = 0,
      border_style = "round",
      border_col = "gray75",
      header = glue::glue("{nrow(df)} \u00d7 {ncol(df)} in reverse")
    )
  )
}











#' @rdname spy
#' @export
spy_all <- function(...) {
  UseMethod("spy_all")
}


#' @rdname spy
#' @export
spy_all.data.frame <- function(df) {
  spy_top.data.frame(df, n = getOption("max.print") - 5L)
}

