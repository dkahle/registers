#' Registers environment
#'
#' See registers
#'
#' @param x An object of class `registers`
#' @param ... Additional arguments to pass to [cat()]
#' @return Invisible \code{NULL}
#' @name registers
#' @examples
#'
#' registers()
#' a <- 1
#' set_focus("a")
#' registers()
#'


#' @rdname registers
#' @export
registers <- function() {

  if ( !registers_option_exists() ) reset_registers()
  getOption("registers")$env

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
  cli_h1("Registers:")
  register_names <- ls(registers())
  if (length(register_names) > 0) {
    cli_div(theme = list(
      span.register = list(color = "orange"),
      span.binding = list(color = "black"),
      span.env= list(color = "gray")
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

}
