#' Insert the base pipe
#'
#' \code{insert_pipe} inserts a pipe wherever the cursor currently is
#' (console or editor)
#'
#' @examples
#' insert_pipe()
#'
insert_pipe <- function() {
  rstudioapi::insertText(" |> ")
}
