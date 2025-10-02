#' Combine Two Sequences
#'
#' This function combines two sequences. It is designed to explore what series
#' with periodic coefficients
#'
#' @param .f A function returning a (periodic) sequence
#' @param .g A function returning a sequence
#'
#' @return A list containing four elements:
#'   \item{fg_dot}{The dot product of the .f and .g sequences}
#'   \item{fg_terms}{The individual terms of the fg_dot series}
#'   \item{f_seq}{The .f sequence}
#'   \item{g_seq}{The .g sequence}
#'
#' @examples
#' combine()
#'
#' @export
combine <- function(.f = \(x) (1:4)/4,
                    .g = \(x) unlist(lapply(1:8, \(x)2^x))){

  # Get vectors
  tryCatch({ .f_vec <- .f()}, error = function(e) e)
  tryCatch({ .g_vec <- .g()}, error = function(e) e)

  # Get lengths
  .f_len <- length(.f_vec)
  .g_len <- length(.g_vec)

  if (.f_len != .g_len) .f_vec <- rep_len(.f_vec, .g_len)

  # Get the terms
  fg_terms <- vector(mode = "numeric", length(.g_len))
  for (i in 1:.g_len){
    fg_terms[[i]] <- .f_vec[[i]]*.g_vec[[i]]
  }

  # Get dot product
  fg_dot <- as.numeric(.f_vec%*%.g_vec)

  # Return
  .ret <- list(fg_dot = fg_dot,
               fg_terms = fg_terms,
               f_seq = .f_vec,
               g_seq = .g_vec)

  return(.ret)

}



#combine <- function(.f = \(x) 1:4, .g = \(x) for (i in 1:4) 2^i){
