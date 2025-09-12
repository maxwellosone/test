fizzbuzz_s3 = function(input) {
  UseMethod("fizzbuzz_s3")
}

add_fizzbuzz <- function(x) {
  result <- ifelse(x %% 15 == 0, "FizzBuzz",
                  ifelse(x %% 3 == 0, "Fizz",
                  ifelse(x %% 5 == 0, "Buzz", as.character(x))))
  return(result)
}

fizzbuzz_s3.default <- function(input) {
  stop("Error: character cannot be a input")
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
  if (any(input < 0)) {
    stop("Error: not all numbers in the vector is non-negative")
  }
}

fizzbuzz_s3.double <- function(x) {
  x
  int_vec = as.integer(x)
  if (any((int_vec == x) == FALSE)) {
    stop("Error: there is at least one element that can't be rounded as an integer")
  }
  add_fizzbuzz(x)
}

fizzbuzz_s3.integer <- function(x) {
  x <- NextMethod()
  add_fizzbuzz(x)
}

x = c(1L, 3L, 5L, 6L, 15L)
fizzbuzz_s3(x)
print(fizzbuzz_s3(x))
x = c(1, 3L, 5, 6, 15L)
print(fizzbuzz_s3(x))
x = c(1, 3L, 5.1, 6, 15L)
print(fizzbuzz_s3(x))
x = c(1.0, 3L, 5.0, 6L, 15)
fizzbuzz_s3(x)
x = c(-1, 3L, 5.0, 6L, 15)
print(fizzbuzz_s3(x))
x = c(1, NA, 3L, 5.0, 6L, 15)
print(fizzbuzz_s3(x))
x = c(1, NaN, 3L, 5.0, 6L, 15)
print(fizzbuzz_s3(x))
x = c(1, Inf, 3L, 5.0, 6L, 15)
print(fizzbuzz_s3(x))
x = c(1, -Inf, 3L, 5.0, 6L, 15)
print(fizzbuzz_s3(x))
add_fizzbuzz(x)