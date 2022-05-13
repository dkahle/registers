#' Registers environment
#'
#' See registers
#'
#' @param x An object of class `registers`
#' @param ... Additional arguments to pass to [cat()]
#' @param symb Symbol to use for tracked assignment, either `"<-"` (default) or
#'   `"<+"`
#' @return Invisible \code{NULL}
#' @name registers
#' @examples
#'
#' registers()
#' a <- 1
#' set_focus("a")
#' registers()
#' show_registers()
#' ls(registers())
#'
#' # in RStudio, go to Tools > Modify Keyboard Shortcuts...
#' # set registers to Alt-Shift-Cmd-R
#' # try Alt-Shift-Cmd-R
#'
#' f <- function(x) x^2
#' set_focus("f")
#' set_focus("plot")
#'
#' # in RStudio, go to Tools > Modify Keyboard Shortcuts...
#' # filter by "registers", change "Show registers" to Shift-Cmd-R
#'
#' track_assignment()
#' ls()
#' ls(all.names = TRUE)
#' registers()
#' a <- 1
#' .
#' b <- 2
#' .
#' ..
#' registers()
#' c <- 3
#' .
#' ..
#' c(._1, ._2, ._3)
#' f <- function(x, y) {
#'   x + y
#' }
#' e <- list2env(list(a = 1, b = 2))
#' df <- cars
#' registers()
#'
#'
#'
#' if (FALSE) { # oddities
#'   ..
#'   .. <- 10
#'   .
#'
#'   x <- 1:3
#'   .
#'   x[1] <- 2
#'   x # unchanged
#'   . # yet 2 was evaluated and stored
#' }
#' untrack_assignment()




#' @rdname registers
#' @export
registers <- function() {

  if ( !registers_option_exists() ) reset_registers()
  getOption("registers")$env

}



#' @rdname registers
#' @export
show_registers <- function() {

  x <- registers()
  print.registers(x)
  invisible(x)

}



#' @rdname registers
#' @export
reset_registers <- function() {

  options(
    "registers" = list(
      "env" = structure(
        new.env(parent = emptyenv()),
        class = "registers"
      )
    )
  )

}




#' @rdname registers
#' @export
print.registers <- function(x, ...) {
  cli_h1("Registers")
  register_names <- ls(registers(), all.names = TRUE)
  if (length(register_names) > 0) {
    cli_div(theme = list(
      span.register = list(color = "firebrick"),
      span.name = list(color = "black"),
      span.envir = list(color = "gray"),
      span.value = list(color = "steelblue")
    ))
    for (register in register_names) {
      if (register %in% history_symbols) {
        value <- rget(register)
        cli_alert("{.emph {.register {register}}}: {.value {register_inline_summary(value)}}")
      } else {
        name <- rget(register)$name
        envir <- rget(register)$envir
        cli_alert("{.emph {.register {register}}}: {.name {name}} {.envir {capture.output(print(envir))}}")
      }
    }
    cli_end()
  } else {
    cli_div(theme = list(span.key = list(color = "gray")))
    cli_text("{.key No bindings are registered.}")
    cli_end()
  }
  cat("\n")
  x
}






history_symbols <- c(".", "..", paste0("._", 1:9))

rget <- function(s) get(s, envir = registers())
rset <- function(s, value) assign(s, value, envir = registers())
rexists <- function(s) exists(s, envir = registers())




# make active bindings in global
make_active_history_binding <- function(string) {
  makeActiveBinding(
    sym = string,
    fun = function(x) {
      if (!missing(x)) stop("Assigning to history objects is not allowed.", call. = FALSE)
      rget(string)
    },
    env = .GlobalEnv
  )
}


assignment_waterfall <- function() {
  rset("._9", rget("._8"))
  rset("._8", rget("._7"))
  rset("._7", rget("._6"))
  rset("._6", rget("._5"))
  rset("._5", rget("._4"))
  rset("._4", rget("._3"))
  rset("._3", rget("._2"))
  rset("._2", rget( ".."))
  rset("._1", rget(  "."))
}




#' @rdname registers
#' @export
track_assignment <- function(symb = c("<-", "<+")) {


  # check for one of the allowable symbols
  symb <- match.arg(symb)


  # initialize history registers and active bindings in global
  for (s in history_symbols) { rset(s, NULL); make_active_history_binding(s) }


  # overwrite `<-`
  if (symb == "<-") {

    new_assignment <- function(lhs, rhs) {

      assign("name", substitute(lhs))
      assign("value", eval(substitute(rhs), envir = parent.frame()))

      rset("..", rget("."))
      rset(".", value)
      assignment_waterfall()

      assign(deparse(name), value, envir = parent.frame())
    }

    assign("<-", new_assignment, envir = .GlobalEnv)

  }


  # overwrite `<`
  if (symb == "<+") {

    new_assignment <- function(lhs, rhs) {

      rhs_sub <- substitute(rhs)
      rhs_string <- deparse(rhs_sub)
      if (substring(rhs_string, 1, 1) != "+") return(!(lhs >= rhs))
      rhs_string <- substring(rhs_string, 2) # remove leading "+"

      assign("name", substitute(lhs))
      assign("value", eval(parse(text = rhs_string), envir = parent.frame()))

      rset("..", rget("."))
      rset(".", value)
      assignment_waterfall()

      assign(deparse(name), value, envir = parent.frame())

    }

    assign("<", new_assignment, envir = .GlobalEnv)

  }


  # check global symbols and alert
  if (all(c(symb, history_symbols) %in% ls(.GlobalEnv, all.names = TRUE))) {
    cli_alert_info("`{symb}` and history symbols loaded to workspace")
  }


}




#' @rdname registers
#' @export
untrack_assignment <- function() {

  # remove history registers and links in global
  rm(list = history_symbols, envir = registers())
  rm(list = history_symbols, envir =  .GlobalEnv)

  # remove assignment operators
  if (exists( "<-", envir = .GlobalEnv, inherits = FALSE)) rm("<-", envir = .GlobalEnv)
  if (exists(  "<", envir = .GlobalEnv, inherits = FALSE)) rm("<",  envir = .GlobalEnv)

}












#' @rdname registers
#' @export
register_inline_summary <- function(x, ...) {
  UseMethod("register_inline_summary")
}



#' @rdname registers
#' @export
register_inline_summary.character <- function(x, ...) {
  n <- length(x)
  ez_trunc(
    if (n == 0L) {
      "character(0)"
    } else if (n == 1L) {
      paste(glue("chr"), paste(x, collapse = ", "))
    } else {
      paste(glue("chr[1:{n}]"), paste(x, collapse = ", "))
    },
    console_width() - 5L
  )
}
# register_inline_summary(character(0))
# register_inline_summary("a")
# register_inline_summary(sample(letters, 100, rep = TRUE))
# register_inline_summary(sample(c(letters, NA), 100, rep = TRUE))



#' @rdname registers
#' @export
register_inline_summary.numeric <- function(x, ...) {
  n <- length(x)
  ez_trunc(
    if (n == 0L) {
      "numeric(0)"
    } else if (n == 1L) {
      paste(glue("num"), paste(round(x, digits = 3), collapse = ", "))
    } else {
      paste(glue("num[1:{n}]"), paste(round(x, digits = 3), collapse = ", "))
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
    } else if (n == 1L) {
      paste(glue("logi"), paste(x, collapse = ", "))
    } else {
      paste(glue("logi[1:{n}]"), paste(x, collapse = ", "))
    },
    console_width() - 5L
  )
}
# register_inline_summary(logical(0))
# register_inline_summary(TRUE)
# register_inline_summary(FALSE)
# register_inline_summary(sample(c(T, F, NA), 100, TRUE))



#' @rdname registers
#' @export
register_inline_summary.function <- function(x, ...) {

  formal_args_names <- names(formals(x))

  chrs <- ez_trim(text_squeeze(deparse(body(x))))
  inline <- !(chrs[1] == "{")
  if (!inline) chrs <- chrs[-c(1, length(chrs))]
  chrs <- paste(
    glue("function({paste(formal_args_names, collapse = ',')})"),
    if (!inline) "{",
    paste(chrs, collapse = ";  "),
    if (!inline) "}"
  )
  ez_trunc(chrs, console_width() - 5L)
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

  ez_trunc(
    paste(
      capture.output(print(x)),
      paste(ls(x), collapse = ", ")
    ),
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
register_inline_summary.default <- function(x, ...) {

  if (is.null(x)) return("NULL")

}
# register_inline_summary(NULL)




