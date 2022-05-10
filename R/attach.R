.onAttach <- function(...) {

  reset_registers()

}


registers_option_exists <- function() {

  !is.null(getOption("registers"))

}







