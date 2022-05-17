

ez_trunc <- function(string, width, ellipsis = "...") {
  too_long <- !is.na(string) & nchar(string) > width
  width... <- width - nchar(ellipsis)
  string[too_long] <- paste0(substr(string[too_long], 1, width...), ellipsis)
  string
}
# ez_trunc("Hello how are you today?", 10)



ez_distill <- function(string, with = " ") gsub("\\s+", with, string)



ez_trim <- function(string) {
  string <- gsub("^\\s+", "", string)
  gsub("\\s+$", "", string)
}
# ez_trim("      hello! ")
# ez_trim(c("      hello! ", "      hello! "))






is_valid_key <- function(x) {
  if (length(x) > 1) return(unname(vapply(x, is_valid_key, logical(1))))
  is.character(x) && nchar(x) == 1L
}
# is_valid_key("h")
# is_valid_key("!")
# is_valid_key(c("h", "i"))
# is_valid_key(c("h", "i"))
# is_valid_key(c("h", "ii", 5)) # note coercion


is_registered <- function(key) {
  stopifnot(length(key) == 1, is.character(key), nchar(key) == 1L)
  rexists(key)
}



is_dynamic_register <- function(x) {
  . <- attr(x, "dynamic_register")
  !is.null(.) && isTRUE(.)
}

is_imperative <- function(x) {
  . <- attr(x, "imperative")
  !is.null(.) && isTRUE(.)
}

`%notin%` <- function(x, y) {
  !(x %in% y)
}

