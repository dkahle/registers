glimpse <- function(data) {
  UseMethod("glimpse")
}

glimpse.default <- function(data) {
  if (is.vector(data)) {
    # could use dplyr::glimpse() as its default method calls str()
    str(data)
  } else {
    print(data)
  }
}

# Other common data structures
glimpse.data.frame <- function(data) {
  dplyr::glimpse(data)
}
