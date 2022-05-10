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
#'
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

}
