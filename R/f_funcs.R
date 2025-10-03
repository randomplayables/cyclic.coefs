#' Sine Sequence
#'
#' Generates a sine sequence for use as periodic coefficients in series analysis.
#' The default excludes the endpoint to prevent duplicate values when tiling.
#'
#' @param inputs A numeric input vector (in radians by default)
#' @param radians A Boolean. If TRUE (default), inputs are in radians; if FALSE, in degrees.
#' @return A numeric vector of sine values
#'
#' @examples
#' sin_cyc()
#' sin_cyc(seq(0, 2*pi, length.out = 17)[-17])
#' sin_cyc(c(0, 90, 180, 270), radians = FALSE)
#'
#' @export
sin_cyc <- function(inputs = seq(0, 2*pi, length.out = 9)[-9],
                    radians = TRUE) {

  # Convert to radians if needed
  if (!radians) {
    inputs <- inputs * (pi/180)
  }

  # Return sine values
  return(sin(inputs))
}


#' Cosine Sequence
#'
#' Generates a cosine sequence for use as periodic coefficients in series analysis.
#' The default excludes the endpoint to prevent duplicate values when tiling.
#'
#' @param inputs A numeric input vector (in radians by default)
#' @param radians A Boolean. If TRUE (default), inputs are in radians; if FALSE, in degrees.
#' @return A numeric vector of cosine values
#'
#' @examples
#' cos_cyc()
#' cos_cyc(seq(0, 2*pi, length.out = 17)[-17])
#' cos_cyc(c(0, 90, 180, 270), radians = FALSE)
#'
#' @export
cos_cyc <- function(inputs = seq(0, 2*pi, length.out = 9)[-9],
                    radians = TRUE) {

  # Convert to radians if needed
  if (!radians) {
    inputs <- inputs * (pi/180)
  }

  # Return sine values
  return(cos(inputs))
}

#' Triangle Wave Sequence
#'
#' Generates a triangle wave sequence for use as periodic coefficients in series analysis.
#' The triangle wave rises linearly from 0 to 1 and falls back to 0 over period 1.
#' The default excludes the endpoint to prevent duplicate values when tiling.
#'
#' @param inputs A numeric input vector (period 1 by default)
#' @return A numeric vector of triangle wave values in \code{[0, 1]}
#'
#' @examples
#' triangle_cyc()
#' triangle_cyc(seq(0, 1, length.out = 17)[-17])
#' triangle_cyc(seq(0, 3, length.out = 25)[-25])  # 3 complete periods
#'
#' @export
triangle_cyc <- function(inputs = seq(0, 1, length.out = 9)[-9]) {
  return(1 - abs((inputs %% 1) / 0.5 - 1))
}


#' Sawtooth Wave Sequence
#'
#' Generates a sawtooth wave sequence for use as periodic coefficients in series analysis.
#' The sawtooth wave ramps linearly from 0 to 1 over period 1, then resets.
#' The default excludes the endpoint to prevent duplicate values when tiling.
#'
#' @param inputs A numeric input vector (period 1 by default)
#' @return A numeric vector of sawtooth wave values in \code{[0, 1)}
#'
#' @examples
#' sawtooth_cyc()
#' sawtooth_cyc(seq(0, 1, length.out = 17)[-17])
#' sawtooth_cyc(seq(0, 2.5, length.out = 20)[-20])  # 2.5 periods
#'
#' @export
sawtooth_cyc <- function(inputs = seq(0, 1, length.out = 9)[-9]) {
  return(inputs %% 1)
}


#' Pulse Wave Sequence
#'
#' Generates a rectangular pulse wave sequence for use as periodic coefficients in series analysis.
#' Returns 1 for the duty cycle portion of each period 1, 0 otherwise.
#' The default excludes the endpoint to prevent duplicate values when tiling.
#'
#' @param inputs A numeric input vector (period 1 by default)
#' @param duty_cycle A numeric value in (0, 1) specifying the fraction of the period where the pulse is 1 (default 0.5)
#' @return A numeric vector of pulse wave values (0 or 1)
#'
#' @examples
#' pulse_cyc()
#' pulse_cyc(seq(0, 1, length.out = 17)[-17], duty_cycle = 0.25)
#' pulse_cyc(seq(0, 2, length.out = 20)[-20], duty_cycle = 0.75)  # 2 periods
#'
#' @export
pulse_cyc <- function(inputs = seq(0, 1, length.out = 9)[-9],
                      duty_cycle = 0.5) {
  return(as.numeric((inputs %% 1) < duty_cycle))
}

#' Square Wave Sequence
#'
#' Generates a square wave sequence for use as periodic coefficients in series analysis.
#' The square wave alternates between -1 and 1, creating maximum contrast.
#' The default excludes the endpoint to prevent duplicate values when tiling.
#'
#' @param inputs A numeric input vector (in radians by default)
#' @param radians A Boolean. If TRUE (default), inputs are in radians; if FALSE, in degrees.
#' @return A numeric vector of square wave values (-1 or 1)
#'
#' @examples
#' square_cyc()
#' square_cyc(seq(0, 2*pi, length.out = 17)[-17])
#' square_cyc(c(0, 90, 180, 270), radians = FALSE)
#'
#' @export
square_cyc <- function(inputs = seq(0, 2*pi, length.out = 9)[-9],
                       radians = TRUE) {
  if (!radians) inputs <- inputs * (pi/180)
  return(sign(sin(inputs)))
}


#' Complex Exponential Sequence
#'
#' Generates a complex exponential sequence for use as periodic coefficients in
#' series analysis. Returns e^(ix) = cos(x) + i*sin(x), enabling Fourier-like
#' complex analysis.The default excludes the endpoint to prevent duplicate values
#' when tiling.
#'
#' @param inputs A numeric input vector (in radians by default)
#' @param radians A Boolean. If TRUE (default), inputs are in radians; if FALSE, in degrees.
#' @return A complex vector of values on the unit circle
#'
#' @examples
#' complex_cyc()
#' complex_cyc(seq(0, 2*pi, length.out = 17)[-17])
#' complex_cyc(c(0, 90, 180, 270), radians = FALSE)
#'
#' @export
complex_cyc <- function(inputs = seq(0, 2*pi, length.out = 9)[-9],
                        radians = TRUE) {
  if (!radians) inputs <- inputs * (pi/180)
  return(exp(1i * inputs))  # Uses complex trig, so radians matters
}
