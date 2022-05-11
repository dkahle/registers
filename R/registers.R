#' Registers environment
#'
#' See registers
#'
#' @param x An object of class `registers`
#' @param ... Additional arguments to pass to [cat()]
#' @param symb Symbol to use for tracked assignment, either `"<+"` (default) or
#'   `"<-"`
#' @return Invisible \code{NULL}
#' @name registers
#' @examples
#'
#' registers()
#' a <- 1
#' set_focus("a")
#' registers()
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
#' a <+ 1
#' .
#' b <+ 2
#' .
#' ..
#' c <+ 3
#' .
#' ..
#' ._3
#' c(._1, ._2, ._3)
#' untrack_assignment()
#'
#'



#' @rdname registers
#' @export
registers <- function() {

  if ( !registers_option_exists() ) reset_registers()
  getOption("registers")$env

}



#' @rdname registers
#' @export
show_registers <- function() {

  print.registers(registers())

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
  register_names <- ls(registers(), all.names = FALSE)
  if (length(register_names) > 0) {
    cli_div(theme = list(
      span.register = list(color = "firebrick"),
      span.binding = list(color = "black"),
      span.env = list(color = "gray")
    ))
    for (register in register_names) {
      binding <- get(register, envir = registers())$name
      env <- get(register, envir = registers())$envir
      cli_alert("{.emph {.register {register}}}: {.binding {binding}} {.env {capture.output(print(env))}}")
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


assignment_waterfall <- function() {
  assign("._9", get("._8", envir = registers()), envir = registers())
  assign("._8", get("._7", envir = registers()), envir = registers())
  assign("._7", get("._6", envir = registers()), envir = registers())
  assign("._6", get("._5", envir = registers()), envir = registers())
  assign("._5", get("._4", envir = registers()), envir = registers())
  assign("._4", get("._3", envir = registers()), envir = registers())
  assign("._3", get("._2", envir = registers()), envir = registers())
  assign("._2", get( "..", envir = registers()), envir = registers())
  assign("._1", get(  ".", envir = registers()), envir = registers())
}




#' @rdname registers
#' @export
track_assignment <- function(symb = c("<-", "<+")) {


  # check for one of the allowable symbols
  symb <- match.arg(symb)


  # initialize history registers
  for (s in history_symbols) assign(s, NULL, envir = registers())


  # overwrite `<-`
  if (symb == "<-") {

    new_assignment <- function(lhs, rhs) {

      assign("name", substitute(lhs))
      assign("value", eval(substitute(rhs), envir = parent.frame()))

      assign("..", get(".", envir = registers()), envir = registers())
      assign(".", value, envir = registers())
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

      assign("..", get(".", envir = registers()), envir = registers())
      assign(".", value, envir = registers())
      assignment_waterfall()

      assign(deparse(name), value, envir = parent.frame())

    }

    assign("<", new_assignment, envir = .GlobalEnv)

  }


  make_active_history_binding <- function(string) {
    makeActiveBinding(
      sym = string,
      fun = function() get(string, envir = registers()),
      env = .GlobalEnv
    )
  }

  for (s in history_symbols) make_active_history_binding(s)


}




#' @rdname registers
#' @export
untrack_assignment <- function() {
  if (exists(  ".", envir = .GlobalEnv, inherits = FALSE)) rm(".",  envir = .GlobalEnv)
  if (exists( "..", envir = .GlobalEnv, inherits = FALSE)) rm("..", envir = .GlobalEnv)
  if (exists("._1", envir = .GlobalEnv, inherits = FALSE)) rm("._1", envir = .GlobalEnv)
  if (exists("._2", envir = .GlobalEnv, inherits = FALSE)) rm("._2", envir = .GlobalEnv)
  if (exists("._3", envir = .GlobalEnv, inherits = FALSE)) rm("._3", envir = .GlobalEnv)
  if (exists("._4", envir = .GlobalEnv, inherits = FALSE)) rm("._4", envir = .GlobalEnv)
  if (exists("._5", envir = .GlobalEnv, inherits = FALSE)) rm("._5", envir = .GlobalEnv)
  if (exists("._6", envir = .GlobalEnv, inherits = FALSE)) rm("._6", envir = .GlobalEnv)
  if (exists("._7", envir = .GlobalEnv, inherits = FALSE)) rm("._7", envir = .GlobalEnv)
  if (exists("._8", envir = .GlobalEnv, inherits = FALSE)) rm("._8", envir = .GlobalEnv)
  if (exists("._9", envir = .GlobalEnv, inherits = FALSE)) rm("._9", envir = .GlobalEnv)
  if (exists( "<-", envir = .GlobalEnv, inherits = FALSE)) rm("<-", envir = .GlobalEnv)
  if (exists(  "<", envir = .GlobalEnv, inherits = FALSE)) rm("<",  envir = .GlobalEnv)
}


