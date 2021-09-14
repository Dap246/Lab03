#' @title euclidean
#'
#'
#' @param b a number
#' @param a a number
#' @description \code{euclidean} use the euclidean algorithm to calculate the Greatest Common Divisor (GCD) of 2 numbers.
#' @return The GCD of \code{b} and \code{a}.
#' @export
#' @references https://en.wikipedia.org/wiki/Euclidean_algorithm
#' @examples
#' euclidean(123612, 13892347912)
#' euclidean(100, 1000)


euclidean <- function(b, a) {

  stopifnot( class(a) == 'numeric' && length(a) == 1 && class(b) == 'numeric' && length(b) == 1 )

  while (b != 0) {

    q <- b
    b <- a %% b
    a <- q

  }

  return(abs(q))

}

euclidean(123612, 13892347912)
euclidean(100,1000)
