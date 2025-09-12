secret <- function(x) {
  UseMethod("secret")
  structure(x, class = "secret")
}

secret.default <- function(x) {
  if (!is.atomic(x)) {
    stop("secret() only supports atomic vectors.")
  }
  structure(x, class = "secret")
}

secret.character <- function(x){
  out = strrep("*", nchar(x))
  structure(out, class = "secret")
}

secret.numeric <- function(x){
  out = ifelse(x < 0 , paste0("-", strrep("*", nchar(x)-1)), strrep("*", nchar(x)))
  out = noquote(out)
  structure(out, class = "secret")
}

secret.complex <- function(x){
  re = Re(x)
  im = Im(x)
  re_out = ifelse(re < 0 , paste0("-", strrep("*", nchar(re)-1)), strrep("*", nchar(re)))
  im_out = ifelse(im < 0 , paste0("-", strrep("*", nchar(im)-1),"i"), paste0("+", strrep("*", nchar(im)),"i"))
  out = paste0(re_out, im_out)
  out = noquote(out)
  structure(out, class = "secret")
}

secret.logical <- function(x) {
  out = noquote(rep("*****", length(x)))
  structure(out, class = "secret")
}

print.secret = function(x, ...) {
  out = unclass(x)
  print(out)
}

secret(c(TRUE,FALSE))
secret(c("TRUE","FALSE"))
secret(c(0L,-11L))
secret(1:5)
secret(c(1.2, 3L))
secret(c(1.2, 3L,NaN))
secret(c(1.2, 3L))
secret(c(15,-15,1.23,-1.23))
secret(c(123.456, -789) )
secret(list("a",12))