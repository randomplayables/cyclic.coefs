#' Harmonic Sequence
#'
#' Generates a harmonic sequence 1/n for use as base coefficients in series analysis.
#' This sequence exhibits slow decay and is fundamental in mathematical analysis.
#'
#' @param n Integer vector of indices (default 1:32)
#' @return A numeric vector with values 1/n
#'
#' @examples
#' harmonic_g()
#' harmonic_g(1:10)
#' combine(.f = \(x) sin_cyc(), .g = \(x) harmonic_g())
#'
#' @export
harmonic_g <- function(n = 1:32) 1/n


#' Power Sequence
#'
#' Generates a power sequence n^p for use as base coefficients in series analysis.
#' Controls polynomial growth (p > 0) or decay (p < 0).
#'
#' @param n Integer vector of indices (default 1:32)
#' @param p Numeric power exponent (default 2)
#' @return A numeric vector with values n^p
#'
#' @examples
#' power_g()
#' power_g(1:10, p = 3)
#' power_g(1:10, p = -2)  # Decay like 1/n^2
#'
#' @export
power_g <- function(n = 1:32, p = 2) n^p


#' Logarithmic Sequence
#'
#' Generates a logarithmic sequence log(n) for use as base coefficients in series analysis.
#' This sequence exhibits very slow growth.
#'
#' @param n Integer vector of indices (default 1:32)
#' @return A numeric vector with values log(n)
#'
#' @examples
#' log_g()
#' log_g(1:20)
#'
#' @export
log_g <- function(n = 1:32) log(n)


#' Exponential Decay Sequence
#'
#' Generates an exponential decay sequence exp(-rate*n) for use as base coefficients.
#' This sequence exhibits rapid decay controlled by the rate parameter.
#'
#' @param n Integer vector of indices (default 1:32)
#' @param rate Numeric decay rate (default 1)
#' @return A numeric vector with values exp(-rate*n)
#'
#' @examples
#' exp_decay_g()
#' exp_decay_g(1:10, rate = 0.5)
#' exp_decay_g(1:10, rate = 2)  # Faster decay
#'
#' @export
exp_decay_g <- function(n = 1:32, rate = 1) exp(-rate * n)


#' Geometric Sequence
#'
#' Generates a geometric sequence r^n for use as base coefficients in series analysis.
#' Exhibits exponential growth (r > 1) or decay (0 < r < 1).
#'
#' @param n Integer vector of indices (default 1:32)
#' @param r Numeric ratio (default 0.5)
#' @return A numeric vector with values r^n
#'
#' @examples
#' geometric_g()
#' geometric_g(1:10, r = 0.8)
#' geometric_g(1:10, r = 1.5)  # Growth
#'
#' @export
geometric_g <- function(n = 1:32, r = 0.5) r^n


#' Alternating Geometric Sequence
#'
#' Generates an alternating geometric sequence (-1)^n * r^n for use as base coefficients.
#' Oscillates between positive and negative values with geometric growth or decay.
#'
#' @param n Integer vector of indices (default 1:32)
#' @param r Numeric ratio (default 0.5)
#' @return A numeric vector with values (-1)^n * r^n
#'
#' @examples
#' alt_geometric_g()
#' alt_geometric_g(1:10, r = 0.8)
#'
#' @export
alt_geometric_g <- function(n = 1:32, r = 0.5) (-1)^n * r^n


#' Binomial Coefficient Sequence
#'
#' Generates a sequence of binomial coefficients choose(n, k) for use as base coefficients.
#' Exhibits polynomial growth for fixed k.
#'
#' @param n Integer vector of indices (default 1:32)
#' @param k Integer parameter for binomial coefficient (default 2)
#' @return A numeric vector with values choose(n, k)
#'
#' @examples
#' binomial_g()
#' binomial_g(1:10, k = 3)
#'
#' @export
binomial_g <- function(n = 1:32, k = 2) choose(n, k)


#' Fibonacci Sequence
#'
#' Generates a Fibonacci sequence for use as base coefficients in series analysis.
#' Each term is the sum of the two preceding terms.
#'
#' @param n Integer vector of indices (default 1:32)
#' @return A numeric vector with Fibonacci values
#'
#' @examples
#' fibonacci_g()
#' fibonacci_g(1:10)
#'
#' @export
fibonacci_g <- function(n = 1:32) {
  fib <- numeric(max(n))
  fib[1:2] <- c(1, 1)
  for(i in 3:max(n)) fib[i] <- fib[i-1] + fib[i-2]
  return(fib[n])
}
