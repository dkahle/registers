

ez_trunc <- function(string, width, ellipsis = "\u2026") {
  too_long <- !is.na(string) & nchar(string) > width
  width... <- width - nchar(ellipsis)
  string[too_long] <- paste0(substr(string[too_long], 1, width...), ellipsis)
  string
}
# ez_trunc("Hello how are you today?", 10)
# [1] "Hello how…"



ez_distill <- function(string, with = " ") gsub("\\s+", with, string)



ez_trim <- function(string) {
  string <- gsub("^\\s+", "", string)
  gsub("\\s+$", "", string)
}
# ez_trim("      hello! ")
# ez_trim(c("      hello! ", "      hello! "))






is_valid_key <- function(key) {
  if (length(key) > 1) return(unname(vapply(key, is_valid_key, logical(1))))
  is.character(key) && nchar(key) == 1L
}
# is_valid_key("h")
# is_valid_key("!")
# is_valid_key(c("h", "i"))
# is_valid_key(c("h", "i"))
# is_valid_key(c("h", "ii", 5)) # note coercion



reserved_keys <- c(" ", as.character(0:9))

is_reserved_key <- function(key) {

  if (!all(is_valid_key(key))) { message("Invalid key specified."); return(invisible()) }
  key %in% reserved_keys

}

# is_reserved_key(c("h", "i", " "))
# is_reserved_key(c("h", "!"))
# is_reserved_key(c("h", "ii", 5)) # note coercion





is_registered <- function(key) {
  stopifnot(length(key) == 1, is.character(key), nchar(key) == 1L)
  rexists(key)
}



is_active_register <- function(x) {
  . <- attr(x, "active_register")
  !is.null(.) && isTRUE(.)
}



`%notin%` <- function(x, y) {
  !(x %in% y)
}




registers_option_exists <- function() {
  !is.null(getOption("registers"))
}


na_char <- function() {

  if (!registers_option_exists()) {
    cur_options <- getOption("registers")
    cur_options$options$na_char
  } else {
    "_" # default to _
  }

}


set_na_char <- function(x) {

  stopifnot(is.character(x), length(x) == 1L)

  if (!registers_option_exists()) {

    cli_alert_danger("registers options do not exist.")

  } else {

    cur_options <- getOption("registers")
    cur_options$options$na_char <- x
    options("registers" = cur_options)
    cli_alert_success("registsers NA character set to {x}")

  }

  invisible()

}
