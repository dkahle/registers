#' Track assignment
#'
#' Track assignment
#'
#' @param symb Symbol to use for tracked assignment, either `"<-"` (default) or
#'   `"<+"`
#' @return Invisible \code{NULL}
#' @name tracking
#' @examples
#'
#' track_assignment()
#' ls()
#' ls(all.names = TRUE)
#' registers()
#' a <- "first"
#' .
#' b <- "second"
#' .
#' ..
#' registers()
#' c <- "third"
#' .
#' ..
#'
#'
#' # composing with history registers
#' registers()
#' set_register(spy(), "s")
#' a <- "first"
#' b <- "second"
#' c <- "third"
#' c(., ..)
#' ring_register(".s")
#' ring_register(":s")
#'
#'
#' if (FALSE) { # oddities
#'
#'   # assigning to history symbols errors but does update history
#'   ..
#'   .. <- 10
#'   .
#'
#'   # replacement functions don't work in the usual way but do update history
#'   x <- 1:3
#'   .
#'   x[1] <- 2
#'   x
#'   .
#'
#' }
#' untrack_assignment()



# history_symbols <- c(".", "..", paste0("._", 1:9))
history_symbols <- c(".", "..")

history_symbols_to_key <- function(x) {
  x[x %in% history_symbols] <- c(
    "." = ".",
    ".." = ":"#,
    # "._1" = "1",
    # "._2" = "2",
    # "._3" = "3",
    # "._4" = "4",
    # "._5" = "5",
    # "._6" = "6",
    # "._7" = "7",
    # "._8" = "8",
    # "._9" = "9"
  )[x[x %in% history_symbols]]
  x
}
# x <- c("._1", "s", "..")
# history_symbols_to_key(x)
# [1] "1" "s" ":"

history_reg_symbols <- history_symbols_to_key(history_symbols)
#  [1] "." ":" "1" "2" "3" "4" "5" "6" "7" "8" "9"

key_to_history_symbols <- function(x) {
  x[x %in% history_reg_symbols] <- c(
    "." =   ".",
    ":" =  ".."#,
    # "1" = "._1",
    # "2" = "._2",
    # "3" = "._3",
    # "4" = "._4",
    # "5" = "._5",
    # "6" = "._6",
    # "7" = "._7",
    # "8" = "._8",
    # "9" = "._9"
  )[x[x %in% history_reg_symbols]]
  x
}
# key_to_history_symbols(c("1", "s", ":"))
# [1] "._1" "s"   ".."



rget <- function(x) get(x, envir = registers())
rset <- function(x, value) assign(x, value, envir = registers())
rexists <- function(x) exists(x, envir = registers())




# make active bindings in global
make_active_history_binding <- function(string) {
  makeActiveBinding(
    sym = string,
    fun = function(x) {
      if (!missing(x)) stop("Assigning to history objects is not allowed.", call. = FALSE)
      rget(string)
    },
    env = .GlobalEnv
  )
}


# assignment_waterfall <- function() {
#   rset("._9", rget("._8"))
#   rset("._8", rget("._7"))
#   rset("._7", rget("._6"))
#   rset("._6", rget("._5"))
#   rset("._5", rget("._4"))
#   rset("._4", rget("._3"))
#   rset("._3", rget("._2"))
#   rset("._2", rget( ".."))
#   rset("._1", rget(  "."))
# }




#' @rdname tracking
#' @export
track_assignment <- function(symb = c("<-", "<+")) {


  # check for one of the allowable symbols
  symb <- match.arg(symb)


  # initialize history registers and active bindings in global
  for (s in history_symbols) { rset(s, NULL); make_active_history_binding(s) }


  # overwrite `<-`
  if (symb == "<-") {

    new_assignment <- function(lhs, rhs) {

      assign("name", substitute(lhs))
      assign("value", eval(substitute(rhs), envir = parent.frame()))

      rset("..", rget("."))
      rset(".", value)
      # assignment_waterfall()

      assign(deparse(name), value, envir = parent.frame())
    }

    assign("<-", new_assignment, envir = .GlobalEnv)

  }


  # overwrite `<`
  if (symb == "<+") {

    new_assignment <- function(lhs, rhs) {

      rhs_sub <- substitute(rhs)
      rhs_string <- deparse(rhs_sub)
      if (substring(rhs_string, 1, 1) != "+") return(!(lhs >= rhs))
      rhs_string <- substring(rhs_string, 2) # remove leading "+"

      assign("name", substitute(lhs))
      assign("value", eval(parse(text = rhs_string), envir = parent.frame()))

      rset("..", rget("."))
      rset(".", value)
      # assignment_waterfall()

      assign(deparse(name), value, envir = parent.frame())

    }

    assign("<", new_assignment, envir = .GlobalEnv)

  }


  # check global symbols and alert
  if (all(c(symb, history_symbols) %in% ls(.GlobalEnv, all.names = TRUE))) {
    cli_alert_info("`{symb}` and history symbols loaded to workspace")
  }


}




#' @rdname tracking
#' @export
untrack_assignment <- function() {

  # remove history registers and links in global
  rm(list = history_symbols, envir = registers())
  rm(list = history_symbols, envir =  .GlobalEnv)

  # remove assignment operators
  if (exists( "<-", envir = .GlobalEnv, inherits = FALSE)) rm("<-", envir = .GlobalEnv)
  if (exists(  "<", envir = .GlobalEnv, inherits = FALSE)) rm("<",  envir = .GlobalEnv)

}







#' @rdname tracking
#' @export
pause_tracking <- function() {

  cli_alert_info("Reverting to regular `<-`")
  rm(list = "<-", envir = .GlobalEnv)

}





#' @rdname tracking
#' @export
resume_tracking <- function() {

  cli_alert_info("Tracking `<-` loaded to workspace")

  new_assignment <- function(lhs, rhs) {

    assign("name", substitute(lhs))
    assign("value", eval(substitute(rhs), envir = parent.frame()))

    rset("..", rget("."))
    rset(".", value)
    # assignment_waterfall()

    assign(deparse(name), value, envir = parent.frame())
  }

  assign("<-", new_assignment, envir = .GlobalEnv)

}










