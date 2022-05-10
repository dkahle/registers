#' Registers environment
#'
#' See registers
#'
#' @param x An object of class `registers_env`
#' @param ... Additional arguments to pass to [cat()]
#' @return Invisible \code{NULL}
#' @name registers_env
#' @examples
#'
#' registers_env()
#' a <- 1
#' set_focus("a")
#' registers_env()
#'


#' @rdname registers_env
#' @export
registers_env <- function() {

  if ( !registers_option_exists() ) reset_registers_env()
  getOption("registers")$env

}



#' @rdname registers_env
#' @export
reset_registers_env <- function() {

  options(
    "registers" = list(
      "env" = structure(
        new.env(parent = emptyenv()),
        class = "registers_env"
      )
    )
  )

}



#' @rdname registers_env
#' @export
print.registers_env <- function(x, ...) {
  cli_h1("Registers:")
  register_names <- ls(registers_env())
  if (length(register_names) > 0) {
    cli_div(theme = list(
      span.register = list(color = "black"),
      span.binding = list(color = "black"),
      span.env= list(color = "gray")
    ))
    for (register in register_names) {
      binding <- get(register, envir = registers_env())$name
      env <- get(register, envir = registers_env())$envir
      cli_alert("{.register {register}}: {.binding {binding}} {.env {capture.output(print(env))}}")
    }
    cli_end()
  } else {
    cli_div(theme = list(span.key = list(color = "gray")))
    cli_text("{.key No bindings are registered.}")
    cli_end()
  }

}
