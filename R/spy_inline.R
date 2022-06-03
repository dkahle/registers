#' Registers inline summaries
#'
#' Registers inline summaries
#'
#' @param x An object of class `registers`
#' @param ... Additional arguments to pass to [cat()]
#' @return `x`
#' @name spy_inline
#' @examples
#'
#' spy_inline(c("foo", "bar"))
#' spy_inline(rnorm(3))
#' spy_inline(1:3)
#' spy_inline(c(TRUE, FALSE, TRUE))
#'
#' spy_inline(cars)
#' spy_inline(list(a = 1, b = 2))
#' spy_inline(list(1, 2))
#'
#' spy_inline(function(x, y) x + y)
#' spy_inline(function(x, y) {
#'   z <- x^2
#'   z + y
#' })
#'
#' e <- new.env()
#' assign("a", 1, envir = e)
#' e
#' ls(e)
#' e$a
#' spy_inline(e)
#' g <- new.env(parent = e)
#' spy_inline(g)
#' spy_inline(asNamespace("dplyr"))
#'
#'
#'
#'






#' @rdname spy_inline
#' @export
spy_inline <- function(x, ...) {
  UseMethod("spy_inline")
}


convert_nas <- function(x) {
  replacement_na <- paste0(" ", na_char(), " ")
  while (grepl(" NA ", x, fixed = TRUE)) {
    x <- gsub(" NA ", replacement_na, x, fixed = TRUE)
  }
  x
}


#' @rdname spy_inline
#' @export
spy_inline.character <- function(x, ...) {
  n <- length(x)

  out <- ez_trunc(
    if (n == 0L) {
      "character(0)"
    } else {
      convert_nas(
        paste(glue("chr[{n}]"), paste(x, collapse = " "))
      )
    },
    console_width() - 5L
  )

  cat(out, "\n")

  invisible(x)

}
# spy_inline(character(0))
# spy_inline("a")
# spy_inline(sample(letters, 100, rep = TRUE))
# spy_inline(sample(c(letters, rep(NA, 10)), 100, rep = TRUE))



#' @rdname spy_inline
#' @export
spy_inline.numeric <- function(x, ...) {

  n <- length(x)

  out <- ez_trunc(
    if (n == 0L) {
      "numeric(0)"
    } else {
      convert_nas(
        paste(glue("num[{n}]"), paste(round(x, digits = 3), collapse = " "))
      )
    },
    console_width() - 5L
  )

  cat(out, "\n")

  invisible(x)

}
# spy_inline(numeric(0))
# spy_inline(rnorm(1))
# spy_inline(rnorm(100))
# spy_inline(sample(c(rnorm(4), NA), 100, TRUE))



#' @rdname spy_inline
#' @export
spy_inline.logical <- function(x, ...) {

  n <- length(x)

  out <- ez_trunc(
    if (n == 0L) {
      "logical(0)"
    } else {
      convert_nas(
        paste(glue("lgl[{n}]"), paste(ifelse(x, "T", "F"), collapse = " "))
      )
    },
    console_width() - 5L
  )

  cat(out, "\n")

  invisible(x)

}
# spy_inline(logical(0))
# spy_inline(TRUE)
# spy_inline(FALSE)
# spy_inline(c(TRUE, FALSE, TRUE))
# spy_inline(sample(c(T, F, NA), 100, TRUE))
# spy_inline(c(T, NA, NA, NA, F))



#' @rdname spy_inline
#' @export
spy_inline.function <- function(x, ...) {

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

  out <- ez_trunc(ez_trim(ez_distill(chrs)), console_width() - 5L)

  cat(out, "\n")

  invisible(x)

}
# f <- function(x) x^2
# spy_inline(f)
# g <- function(x, y) {
#   z <- x + y
#   y^2
# }
# spy_inline(g)



#' @rdname spy_inline
#' @export
spy_inline.environment <- function(x, ...) {

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
  bindings <- if (length(bindings) > 0) paste0(': ', paste(bindings, collapse = " ")) else ''

  out <- ez_trunc(
    glue("env {env_formatted} \U2190 {parent_formatted} {bindings} "),
    console_width() - 5L
  )

  cat(out, "\n")

  invisible(x)

}


# spy_inline(new.env())
# spy_inline(environment(registers))



#' @rdname spy_inline
#' @export
spy_inline.tbl_df <- function(x, ...) {

  out <- ez_trunc(
    glue("tibble [{nrow(x)} \U00D7 {ncol(x)}] {paste(names(x), collapse = ' ')}"),
    console_width() - 5L
  )

  cat(out, "\n")

  invisible(x)

}
# spy_inline(ggplot2::diamonds)



#' @rdname spy_inline
#' @export
spy_inline.data.frame <- function(x, ...) {

  out <- ez_trunc(
    glue("data.frame[{nrow(x)} \U00D7 {ncol(x)}] {paste(names(x), collapse = ' ')}"),
    console_width() - 5L
  )

  cat(out, "\n")

  invisible(x)

}
# spy_inline(cars)



#' @rdname spy_inline
#' @export
spy_inline.list <- function(x, ...) {

  out <- ez_trunc(
    glue("list[{length(x)}] {paste(names(x), collapse = ' ')}"),
    console_width() - 5L
  )

  cat(out, "\n")

  invisible(x)

}
# spy_inline(cars)




#' @rdname spy_inline
#' @export
spy_inline.default <- function(x, ...) {

  if (is.null(x)) return("NULL")

}
# spy_inline(NULL)




