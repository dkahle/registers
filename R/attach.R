.onAttach <- function(...) {

  reset_registers()

}



.onDetach <- function(...) {

  untrack_assignment()

}



registers_option_exists <- function() {

  !is.null(getOption("registers"))

}







