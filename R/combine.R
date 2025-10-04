#' Combine Two Sequences
#'
#' This function combines two sequences. It is designed to explore series
#' with periodic coefficients.
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
  .f_vec <- tryCatch(.f(), error = function(e) stop("combine(.f) failed: ", conditionMessage(e), call. = FALSE))
  .g_vec <- tryCatch(.g(), error = function(e) stop("combine(.g) failed: ", conditionMessage(e), call. = FALSE))


  # Get lengths
  .f_len <- length(.f_vec)
  .g_len <- length(.g_vec)

  if (.f_len != .g_len) .f_vec <- rep_len(.f_vec, .g_len)

  # Get the terms (complex-safe pre-allocation)
  is_comp <- is.complex(.f_vec) || is.complex(.g_vec)
  fg_terms <- vector(mode = if (is_comp) "complex" else "numeric", length = .g_len)
  for (i in seq_len(.g_len)) {
    fg_terms[[i]] <- .f_vec[[i]] * .g_vec[[i]]
  }

  # Get dot product (complex-safe; avoid coercion)
  fg_dot <- sum(.f_vec * .g_vec)

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

  # Complex-safety: compute summaries on real parts if needed
  has_complex <- is.complex(object$f_seq) || is.complex(object$g_seq) || is.complex(object$fg_terms)
  f_seq <- if (has_complex) Re(object$f_seq) else object$f_seq
  g_seq <- if (has_complex) Re(object$g_seq) else object$g_seq
  fg_terms <- if (has_complex) Re(object$fg_terms) else object$fg_terms

  cat("Dot Product (f * g):", object$fg_dot, "\n\n")
  if (has_complex) cat("(Complex inputs detected; summaries below are over the REAL parts.)\n\n")

  cat("Periodic Sequence (f):\n")
  cat("  Length:  ", length(f_seq), "\n")
  cat("  Range:   [", min(f_seq), ", ", max(f_seq), "]\n", sep = "")
  cat("  Mean:    ", mean(f_seq), "\n")
  cat("  SD:      ", stats::sd(f_seq), "\n\n")

  cat("Base Sequence (g):\n")
  cat("  Length:  ", length(g_seq), "\n")
  cat("  Range:   [", min(g_seq), ", ", max(g_seq), "]\n", sep = "")
  cat("  Mean:    ", mean(g_seq), "\n")
  cat("  SD:      ", stats::sd(g_seq), "\n\n")

  cat("Element-wise Products (f[i]*g[i]):\n")
  cat("  Range:   [", min(fg_terms), ", ", max(fg_terms), "]\n", sep = "")
  cat("  Mean:    ", mean(fg_terms), "\n")
  cat("  SD:      ", stats::sd(fg_terms), "\n")

  invisible(object)
}


#' Plot Method for combine Objects
#'
#' @param x A combine object
#' @param legend_pos Position for legend. Either a keyword ("right", "topleft", etc.) or numeric coordinates.
#' @param ... Additional arguments (unused)
#' @export
plot.combine <- function(x, legend_pos = "right", ...) {
  # Set up 2x2 plot layout
  old_par <- graphics::par(mfrow = c(2, 2), mar = c(4, 4, 2, 1))
  on.exit(graphics::par(old_par))

  idx <- seq_along(x$f_seq)

  # Complex-safety: plot real parts if needed
  has_complex <- is.complex(x$f_seq) || is.complex(x$g_seq) || is.complex(x$fg_terms)
  f_plot <- if (has_complex) Re(x$f_seq) else x$f_seq
  g_plot <- if (has_complex) Re(x$g_seq) else x$g_seq
  fg_plot <- if (has_complex) Re(x$fg_terms) else x$fg_terms

  # Plot 1: Periodic sequence (f)
  plot(idx, f_plot, type = "b", pch = 16, col = "blue",
       xlab = "Index", ylab = "f[i]", main = "Periodic Sequence (f)")
  graphics::grid()

  # Plot 2: Base sequence (g)
  plot(idx, g_plot, type = "b", pch = 16, col = "red",
       xlab = "Index", ylab = "g[i]", main = "Base Sequence (g)")
  graphics::grid()

  # Plot 3: Combined view of sequences
  plot(idx, g_plot, type = "l", col = "red", lwd = 2,
       xlab = "Index", ylab = "Value", main = "Sequence Comparison")
  graphics::lines(idx, f_plot, col = "blue", lwd = 2)
  graphics::lines(idx, fg_plot, col = "purple", lwd = 2, lty = 2)
  graphics::legend(legend_pos,
                   legend = c("f (periodic)", "g (base)", "f*g (product)"),
                   col = c("blue", "red", "purple"),
                   lty = c(1, 1, 2), lwd = 2, cex = 0.7, bg = "white")
  graphics::grid()

  # Plot 4: Running dot product
  cumsum_fg <- cumsum(fg_plot)
  plot(idx, cumsum_fg, type = "l", col = "darkgreen", lwd = 3,
       xlab = "Index", ylab = "Cumulative Sum",
       main = paste("Running Dot Product (Final =",
                    round(if (has_complex) Re(x$fg_dot) else x$fg_dot, 3), ")"),
       ylim = c(min(0, min(cumsum_fg)), max(cumsum_fg) * 1.1))
  graphics::points(idx, cumsum_fg, pch = 16, col = "darkgreen")
  graphics::abline(h = if (has_complex) Re(x$fg_dot) else x$fg_dot, lty = 2, col = "gray40")
  graphics::grid()

  invisible(x)
}
