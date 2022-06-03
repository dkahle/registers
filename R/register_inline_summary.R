#' Registers inline summaries
#'
#' Registers inline summaries
#'
#' @param x An object of class `registers`
#' @param ... Additional arguments to pass to [cat()]
#' @return `x`
#' @name registers_inline_summary
#' @examples
#'
#' register_inline_summary(c("foo", "bar"))
#' register_inline_summary(rnorm(3))
#' register_inline_summary(1:3)
#' register_inline_summary(c(TRUE, FALSE, TRUE))
#'
#' register_inline_summary(cars)
#' register_inline_summary(list(a = 1, b = 2))
#' register_inline_summary(list(1, 2))
#'
#' register_inline_summary(function(x, y) x + y)
#' register_inline_summary(function(x, y) {
#'   z <- x^2
#'   z + y
#' })
#'
#' e <- new.env()
#' assign("a", 1, envir = e)
#' e
#' ls(e)
#' e$a
#' register_inline_summary(e)
#' g <- new.env(parent = e)
#' register_inline_summary(g)
#' register_inline_summary(asNamespace("dplyr"))
#'
#'
#'
#'






#' @rdname registers
#' @export
register_inline_summary <- function(x, ...) {
  UseMethod("register_inline_summary")
}


convert_nas <- function(x) {
  replacement_na <- paste0(" ", na_char(), " ")
  while (grepl(" NA ", x, fixed = TRUE)) {
    x <- gsub(" NA ", replacement_na, x, fixed = TRUE)
  }
  x
}


#' @rdname registers
#' @export
register_inline_summary.character <- function(x, ...) {
  n <- length(x)
  ez_trunc(
    if (n == 0L) {
      "character(0)"
    } else {
      convert_nas(
        paste(glue("chr[{n}]"), paste(x, collapse = " "))
      )
    },
    console_width() - 5L
  )
}
# register_inline_summary(character(0))
# register_inline_summary("a")
# register_inline_summary(sample(letters, 100, rep = TRUE))
# register_inline_summary(sample(c(letters, rep(NA, 10)), 100, rep = TRUE))



#' @rdname registers
#' @export
register_inline_summary.numeric <- function(x, ...) {
  n <- length(x)
  ez_trunc(
    if (n == 0L) {
      "numeric(0)"
    } else {
      convert_nas(
        paste(glue("num[{n}]"), paste(round(x, digits = 3), collapse = " "))
      )
    },
    console_width() - 5L
  )
}
# register_inline_summary(numeric(0))
# register_inline_summary(rnorm(1))
# register_inline_summary(rnorm(100))
# register_inline_summary(sample(c(rnorm(4), NA), 100, TRUE))



#' @rdname registers
#' @export
register_inline_summary.logical <- function(x, ...) {
  n <- length(x)
  ez_trunc(
    if (n == 0L) {
      "logical(0)"
    } else {
      convert_nas(
        paste(glue("lgl[{n}]"), paste(ifelse(x, "T", "F"), collapse = " "))
      )
    },
    console_width() - 5L
  )
}
# register_inline_summary(logical(0))
# register_inline_summary(TRUE)
# register_inline_summary(FALSE)
# register_inline_summary(c(TRUE, FALSE, TRUE))
# register_inline_summary(sample(c(T, F, NA), 100, TRUE))
# register_inline_summary(c(T, NA, NA, NA, F))



#' @rdname registers
#' @export
register_inline_summary.function <- function(x, ...) {

  formal_args_names <- names(formals(x))

  chrs <- ez_trim(ez_distill(deparse(body(x))))
  inline <- !(chrs[1] == "{")
  if (!inline) chrs <- chrs[-c(1, length(chrs))]
  chrs <- paste(
    glue("function({paste(formal_args_names, collapse = ',')})"),
    if (!inline) "{",
    paste(chrs, collapse = ";  "),
    if (!inline) "}"
  )
  ez_trunc(ez_trim(ez_distill(chrs)), console_width() - 5L)
}
# f <- function(x) x^2
# register_inline_summary(f)
# g <- function(x, y) {
#   z <- x + y
#   y^2
# }
# register_inline_summary(g)



#' @rdname registers
#' @export
register_inline_summary.environment <- function(x, ...) {

  env_printed <- capture.output(print(x))
  env_name <- substr(env_printed, start = 15, stop = nchar(env_printed) - 1L)
  env_formatted <- if (substr(env_name, 1, 2) == "0x" && nchar(env_name) == 14L) {
    paste0("<", env_name, ">")
  } else {
    env_name
  }

  parent <- parent.env(x)
  parent_printed <- capture.output(print(parent))[1]
  parent_name <- substr(parent_printed, start = 15, stop = nchar(parent_printed) - 1L)
  parent_formatted <- if (substr(parent_name, 1, 2) == "0x" && nchar(parent_name) == 14L) {
    paste0("<", parent_name, ">")
  } else {
    parent_name
  }

  bindings <- ls(x)
  bindings <- if (length(bindings) > 0) paste0(': ', paste(bindings, collapse = ", ")) else ''

  ez_trunc(
    glue("env {env_formatted} \U2190 {parent_formatted} {bindings} "),
    console_width() - 5L
  )

}


# register_inline_summary(new.env())
# register_inline_summary(environment(registers))



#' @rdname registers
#' @export
register_inline_summary.tbl_df <- function(x, ...) {

  ez_trunc(
    glue("tibble [{nrow(x)} \U00D7 {ncol(x)}] {paste(names(x), collapse = ', ')}"),
    console_width() - 5L
  )

}
# register_inline_summary(ggplot2::diamonds)



#' @rdname registers
#' @export
register_inline_summary.data.frame <- function(x, ...) {

  ez_trunc(
    glue("data.frame [{nrow(x)} \U00D7 {ncol(x)}] {paste(names(x), collapse = ', ')}"),
    console_width() - 5L
  )

}
# register_inline_summary(cars)



#' @rdname registers
#' @export
register_inline_summary.list <- function(x, ...) {

  ez_trunc(
    glue("list [{length(x)}] {paste(names(x), collapse = ', ')}"),
    console_width() - 5L
  )

}
# register_inline_summary(cars)




#' @rdname registers
#' @export
register_inline_summary.default <- function(x, ...) {

  if (is.null(x)) return("NULL")

}
# register_inline_summary(NULL)




