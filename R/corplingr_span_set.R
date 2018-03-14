#' Prepare window-span vector for collocates
#'
#' @description Utility internal-function for constructing window-span vector in the collocate function.
#' @param window window-span direction of the collocates: \code{"r"} ('\bold{right} of the node'), \code{"l"} ('\bold{left} of the node'), or the DEFAULT is \code{"b"} ('both \bold{left} and \bold{right} context-window').
#' @param span integer vector indicating the span of the collocate scope.
#' @return Named integer vectors of window-span. The name attributes refer to the window position of the vector (e.g. "l2", "l1", "node", "r1", "r2").
#' @examples
#' span <- span_set(window = "b", span = 3)
#' @export
span_set <- function(window = "character", span = "integer") {

  if (window == "l") {
    span1 <- span
    span <- -span:0
    names(span) <- c(stringr::str_c("l", span1:1, sep = ""), "node")
  } else if (window == "r") {
    span1 <- span
    span <- 0:span
    names(span) <- c("node", stringr::str_c("r", 1:span1))
  } else {
    span1 <- span
    span <- -span:span
    names(span) <- c(stringr::str_c("l", span1:1, sep = ""), "node", stringr::str_c("r", 1:span1, sep = ""))
  }
  return(span)
}
