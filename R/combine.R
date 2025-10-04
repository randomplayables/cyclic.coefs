#' Combine Two Sequences
#'
#' This function combines two sequences. It is designed to explore what series
#' with periodic coefficients
#'
#' @param .f A function returning a (periodic) sequence
#' @param .g A function returning a sequence
#'
#' @return An object of class \code{combine} containing:
#'   \item{fg_dot}{The dot product of f and g: sum of \code{f[i]*g[i]}}
#'   \item{fg_terms}{Element-wise products: \code{f[i]*g[i]} for each i}
#'   \item{f_seq}{The periodic coefficient sequence}
#'   \item{g_seq}{The base sequence}
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

  class(.ret) <- "combine"
  return(.ret)

}


#' Print Method for combine Objects
#'
#' @param x A combine object
#' @param n Number of terms to display (default 6)
#' @param ... Additional arguments (unused)
#' @export
print.combine <- function(x, n = 6, ...) {
  cat("Cyclic Coefficient Series Combination\n")
  cat("======================================\n\n")
  cat("Dot Product (f * g):", x$fg_dot, "\n")
  cat("Sequence Length:    ", length(x$g_seq), "\n\n")

  n_terms <- min(n, length(x$fg_terms))
  cat("First", n_terms, "element-wise products (f[i]*g[i]):\n")
  print(utils::head(x$fg_terms, n_terms))

  if (length(x$fg_terms) > n_terms) {
    cat("... (", length(x$fg_terms) - n_terms, " more terms)\n", sep = "")
  }

  invisible(x)
}


#' Summary Method for combine Objects
#'
#' @param object A combine object
#' @param ... Additional arguments (unused)
#' @export
summary.combine <- function(object, ...) {
  cat("Cyclic Coefficient Series Combination\n")
  cat("======================================\n\n")

  cat("Dot Product (f * g):", object$fg_dot, "\n\n")

  cat("Periodic Sequence (f):\n")
  cat("  Length:  ", length(object$f_seq), "\n")
  cat("  Range:   [", min(object$f_seq), ", ", max(object$f_seq), "]\n", sep = "")
  cat("  Mean:    ", mean(object$f_seq), "\n")
  cat("  SD:      ", stats::sd(object$f_seq), "\n\n")

  cat("Base Sequence (g):\n")
  cat("  Length:  ", length(object$g_seq), "\n")
  cat("  Range:   [", min(object$g_seq), ", ", max(object$g_seq), "]\n", sep = "")
  cat("  Mean:    ", mean(object$g_seq), "\n")
  cat("  SD:      ", stats::sd(object$g_seq), "\n\n")

  cat("Element-wise Products (f[i]*g[i]):\n")
  cat("  Range:   [", min(object$fg_terms), ", ", max(object$fg_terms), "]\n", sep = "")
  cat("  Mean:    ", mean(object$fg_terms), "\n")
  cat("  SD:      ", stats::sd(object$fg_terms), "\n")

  invisible(object)
}


#' Plot Method for combine Objects
#'
#' @param x A combine object
#' @param legend_pos Position for the legend in plot 4 (default "topleft")
#' @param ... Additional arguments (unused)
#' @export
plot.combine <- function(x, legend_pos = "topleft", ...) {
  # Set up 2x2 plot layout
  old_par <- graphics::par(mfrow = c(2, 2), mar = c(4, 4, 2, 1))
  on.exit(graphics::par(old_par))

  idx <- seq_along(x$f_seq)

  # Plot 1: Periodic sequence (f)
  plot(idx, x$f_seq, type = "b", pch = 16, col = "blue",
       xlab = "Index", ylab = "f[i]", main = "Periodic Sequence (f)")
  graphics::grid()

  # Plot 2: Base sequence (g)
  plot(idx, x$g_seq, type = "b", pch = 16, col = "red",
       xlab = "Index", ylab = "g[i]", main = "Base Sequence (g)")
  graphics::grid()

  # Plot 3: Combined view of sequences
  plot(idx, x$g_seq, type = "l", col = "red", lwd = 2,
       xlab = "Index", ylab = "Value", main = "Sequence Comparison")
  graphics::lines(idx, x$f_seq, col = "blue", lwd = 2)
  graphics::lines(idx, x$fg_terms, col = "purple", lwd = 2, lty = 2)
  graphics::legend("topleft",
         legend = c("f (periodic)", "g (base)", "f*g (product)"),
         col = c("blue", "red", "purple"),
         lty = c(1, 1, 2), lwd = 2, cex = 0.7, bg = "white")
  graphics::grid()

  # Plot 4: Running dot product
  cumsum_fg <- cumsum(x$fg_terms)
  plot(idx, cumsum_fg, type = "l", col = "darkgreen", lwd = 3,
       xlab = "Index", ylab = "Cumulative Sum",
       main = paste("Running Dot Product (Final =", round(x$fg_dot, 3), ")"),
       ylim = c(0, max(cumsum_fg) * 1.1))
  graphics::points(idx, cumsum_fg, pch = 16, col = "darkgreen")
  graphics::abline(h = x$fg_dot, lty = 2, col = "gray40")
  graphics::grid()

  invisible(x)
}
