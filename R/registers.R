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
#' register_keys()
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
#' f <- function(x, y = 1) {
#'   x + y
#' }
#' e <- list2env(list(a = 1, b = 2))
#' o <- options()
#' l <- list(1, 2, 3)
#' df <- cars
#' registers()
#' rm(a, b, c, f, e, o, l, df)
#' ls()
#' registers()
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
      "options" = list(
        override = TRUE
      ),
      "env" = structure(
        new.env(parent = emptyenv()),
        class = "registers"
      )
    )
  )

}




#' @rdname registers
#' @export
set_registers_option <- function(...) {

  new_options <- as.list(match.call())[-1]
  current_options <- getOption("registers")
  current_options$options <- current_options$options[
    names(current_options$options) %notin% names(new_options)
  ]
  current_options$options <- c(current_options$options, new_options)
  options("registers" = current_options)

}



#' @rdname registers
#' @export
get_registers_option <- function(option) {

  getOption("registers")$options[[option]]

}







#' @rdname registers
#' @export
print.registers <- function(x, ...) {
  cli_h1("Registers")
  register_names <- ls(registers(), all.names = TRUE)

  # put . next to ..
  if ("." %in% register_names && ".." %in% register_names) {
    ndx_.. <- which(".." == register_names)
    .. <- register_names[ndx_..]
    register_names <- register_names[-ndx_..]
    ndx_.  <- which("." == register_names)
    register_names <- c(register_names[1:ndx_.], .., register_names[(ndx_.+1):length(register_names)])
  }

  # print
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
        name <- deparse(rget(register)$name)
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

history_symbols_to_key <- function(x) {
  c(
    "." = ".",
    ".." = ":",
    "._1" = "1",
    "._2" = "2",
    "._3" = "3",
    "._4" = "4",
    "._5" = "5",
    "._6" = "6",
    "._7" = "7",
    "._8" = "8",
    "._9" = "9"
  )[x]
}

key_to_history_symbols <- function(x) {
  c(
    "." =   ".",
    ":" =  "..",
    "1" = "._1",
    "2" = "._2",
    "3" = "._3",
    "4" = "._4",
    "5" = "._5",
    "6" = "._6",
    "7" = "._7",
    "8" = "._8",
    "9" = "._9"
  )[x]
}








rget <- function(x) get(x, envir = registers())
rset <- function(x, value) assign(x, value, envir = registers())
rexists <- function(x) exists(x, envir = registers())




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











