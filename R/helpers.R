

ez_trunc <- function(string, width, ellipsis = "...") {
  too_long <- !is.na(string) & nchar(string) > width
  width... <- width - nchar(ellipsis)
  string[too_long] <- paste0(substr(string[too_long], 1, width...), ellipsis)
  string
}
# ez_trunc("Hello how are you today?", 10)



text_squeeze <- function(string, with = " ") gsub("\\s+", with, string)




ez_trim <- function(string) {
  string <- gsub("^\\s+", "", string)
  gsub("\\s+$", "", string)
}
# ez_trim("      hello! ")
# ez_trim(c("      hello! ", "      hello! "))
