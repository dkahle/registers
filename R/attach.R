.onAttach <- function(...) {

  reset_registers_env()

}


registers_option_exists <- function() {

  !is.null(getOption("registers"))

}







