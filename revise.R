secret <- function(x) {
  UseMethod("secret")
  structure(x, class = "secret")
}

secret.default <- function(x) {
  if (!is.atomic(x)) {
    stop("secret() only supports atomic vectors.")
  }
  ifelse (is.na(x) | is.nan(x), warning("There is a unprocessable NA or NaN included"),x)
  structure(x, class = "secret")
}

secret.character <- function(x){
  secret.default(x)
  out = paste0('"', strrep("*", nchar(x)), '"')
  structure(out, class = "secret")
}

secret.numeric <- function(x){
  secret.default(x)
  out = ifelse(x < 0 , paste0("-", strrep("*", nchar(x)-1)), strrep("*", nchar(x)))
  structure(out, class = "secret")
}

secret.complex <- function(x){
  secret.default(x)
  re = Re(x)
  im = Im(x)
  re_out=ifelse(re < 0 , paste0("-", strrep("*", nchar(re)-1)), strrep("*", nchar(re)))
  im_out=ifelse(im < 0 , paste0("-", strrep("*", nchar(im)-1),"i"),
                paste0("+", strrep("*", nchar(im)),"i"))
  out = noquote(paste0(re_out, im_out))
  structure(out, class = "secret")
}

secret.logical <- function(x) {
  secret.default(x)
  #ifelse (is.nan(x), warning("There is a unprocessable NA or NaN included"),x)
  out = noquote(rep("*****", length(x)))
  structure(out, class = "secret")
}

print.secret <- function(x, ...) {
  out = noquote(unclass(x))
  print(out)
}

print(secret(c(TRUE,FALSE)))
print(secret(c("TRUE","FALSE")))
secret(c("abc","abc"))
secret(c(0L,-11L))
secret(1:5)
secret(-(1:5))
secret( c(123, 456, 789) )
secret( c(123.456, -789) )
secret( c(-123.456, 789) )
secret( c(123+1234i, -123+1234i) )
secret( c(123-1234i, -123-1234i) )
secret( c(TRUE, FALSE) )
secret( c(FALSE, TRUE) )
secret( c("abc", "def") )
secret(c(1.2, 3L))
secret(c(15,-15,1.23,-1.23))
secret(c(123.456, -789) )
secret(list("ABC", 123))
secret(c())
a=print(secret(c(-123.12+1234i, -123-1234i,-123-1234.12i)))
a
# if considering dropping NA, Inf, -Inf, and NaN
secret(c(-123.12+1234i, NA, Inf, -Inf, NaN))
secret(c("abc", NA, Inf, -Inf, NaN))
secret(c(5.1, NA, Inf, -Inf, NaN))
secret(c(TRUE, FALSE, NA))
secret(c(Inf, -Inf, NaN))
secret(c(NA, Inf, -Inf, NaN))
secret(c(NaN,"a"))
secret(c(NA,"a"))
secret(c(Inf, -Inf))
secret(NA)