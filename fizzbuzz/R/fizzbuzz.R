#' @title FizzBuzz function
#'
#' @description a
#'
#' @param input a numeric vector
#'
#' @return fizzbuzz returns a character vector
#'
#' @examples
#' fizzbuzz(c(1,3,5,15))
#'
#' @export


fizzbuzz = function(input) {
  if (any(is.nan(input)) == TRUE) {
    stop("Error: there is at least one NaN")
  }
  if (any(is.na(input)) == TRUE) {
    stop("Error: there is at least one NA")
  }
  if (any(is.finite(input) == FALSE)) {
    if (any((input < 0) == TRUE)) {
      stop("Error: there is at least one negative inifinite number")
    }
    stop("Error: there is at least one positive inifinite number")
  }
  int_vec = as.integer(input)
  if (any((int_vec == input) == FALSE)) {
    stop("Error: there is at least one element that can't be rounded as an integer")
  }
  if (any(int_vec < 0)) {
    stop("Error: not all numbers in the vector is non-negative")
  }
  result = c()
  for (int in int_vec) {
    if (int %% 15 == 0) {
      result = append(result, "FizzBuzz")
    }
    else if (int %% 3 == 0) {
      result = append(result, "Fizz")
    }
    else if (int %% 5 == 0) {
      result = append(result, "Buzz")
    }
    else {
      result = append(result, as.character(int))
    }
  }
  return(result)
}
