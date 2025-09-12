secret <- function(x) {
  UseMethod("secret")
  structure(x, class = "secret")
}

secret.default <- function(x) {
  if (length(x) == 0) {
    warning("Warning: the input or output vector has lenght 0.")
  }
  if (!is.atomic(x)) {
    stop("secret() only supports atomic vectors.")
  }
  structure(x, class = "secret")
}

secret.character = function(x){
  result = x[!(x %in% c("Inf", "-Inf", "NaN"))]
  result = result[!(is.na(result))]
  result = strrep("*", nchar(result))
  secret.default(result)
  structure(result, class = "secret")
}

secret.numeric = function(x){
  x = x[is.finite(x)]
  out = ifelse(x < 0 , paste0("-", strrep("*", nchar(x)-1)), strrep("*", nchar(x)))
  result = noquote(out)
  secret.default(result)
  structure(result, class = "secret")
}

secret.complex = function(x){
  x = x[!(is.na(x))]
  temp = Re(x)
  index = which(is.finite(temp))
  x = x[index]
  re = Re(x)
  im = Im(x)
  re_out=ifelse(re < 0 , paste0("-", strrep("*", nchar(re)-1)), strrep("*", nchar(re)))
  im_out=ifelse(im < 0 , paste0("-", strrep("*", nchar(im)-1),"i"), paste0("+", strrep("*", nchar(im)),"i"))
  out = paste0(re_out, im_out)
  noquote(out)
  structure(out, class = "secret")
}

secret.logical = function(x) {
  x = x[!(is.na(x))]
  result = noquote(rep("*****", length(x)))
  secret.default(result)
  structure(result, class = "secret")
}

print.secret = function(x, ...) {
  out = unclass(x)
  print(out)
}


### Example Usage
secret(c(TRUE,FALSE))
secret(c("TRUE","FALSE"))
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
print.secret(c(-123.12+1234i, -123-1234i,-123-1234.12i))
a=print.secret(c(-123.12+1234i, -123-1234i,-123-1234.12i))
a
secret(list("ABC", 123))
secret(c())
a=print(secret(c(-123.12+1234i, -123-1234i,-123-1234.12i)))
a
# if considering dropping NA, Inf, -Inf, and NaN
secret(c(-123.12+1234i, NA, Inf, -Inf, NaN))ｘ
secret(c("abc", NA, Inf, -Inf, NaN))
secret(c(5.1, NA, Inf, -Inf, NaN))
secret(c(TRUE, FALSE, NA))
secret(c(Inf, -Inf, NaN))
secret(c(NA, Inf, -Inf, NaN))
secret(c(NA))
secret(c(Inf, -Inf))