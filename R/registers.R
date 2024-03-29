#' Registers environment
#'
#' See registers
#'
#' @param x An object of class `registers`
#' @param option A named registers option to retrieve
#' @param ... Additional arguments
#' @return Invisible \code{NULL}
#' @name registers
#' @seealso [set_register()]
#' @examples
#'
#' registers()
#' set_register(a <- rnorm(1), "a")
#' registers()
#' ls(registers())
#'
#' # in RStudio, go to Tools > Modify Keyboard Shortcuts...
#' # set Show registers to Alt-Shift-Cmd-R, Apply, then try it
#'
#' reset_registers()
#' registers()
#'
#' get_registers_option("clobber")
#' set_registers_option("clobber" = FALSE)
#' get_registers_option("clobber")
#' set_registers_option("clobber" = FALSE)
#' get_registers_option("clobber")
#' reset_registers()
#' get_registers_option("clobber")
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

  x <- registers()
  print.registers(x)
  invisible(x)

}



#' @rdname registers
#' @export
print.registers <- function(x, ...) {

  cli_h1("Registers")
  register_names <- ls(registers(), all.names = TRUE)

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
        cli_alert("{.emph {.register {register}}}: {.value {capture.output(spy_inline(value))}}")
      } else {
        name <- paste0(ez_trim(deparse(rget(register)$name)), collapse = " ")
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








#' @rdname registers
#' @export
reset_registers <- function() {

  options(
    "registers" = list(
      "options" = list(
        clobber = TRUE
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
  for (k in seq_along(new_options)) {
    cli_alert_info("Registers option `{names(new_options)[k]}` set to `{new_options[k]}`.")
  }

}



#' @rdname registers
#' @export
get_registers_option <- function(option) {

  getOption("registers")$options[[option]]

}


