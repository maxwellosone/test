secret <- function(x) {
  structure(x, class = "secret")
  UseMethod("secret")
  }

secret.default <- function(x) {
  if (!is.atomic(x)) {
    stop("secret() only supports atomic vectors.")
  }
}


secret.character = function(x){
  out=strrep("*", nchar(x))
  out
}

secret.numeric = function(x){
  out = noquote(ifelse(x < 0 , paste0("-", strrep("*", nchar(x)-1)), strrep("*", nchar(x))))
  out
}


secret.complex = function(x){
  re = Re(x)
  im = Im(x)
  re_out=ifelse(re < 0 , paste0("-", strrep("*", nchar(re)-1)), strrep("*", nchar(re)))
  im_out=ifelse(im < 0 , paste0("-", strrep("*", nchar(im)-1),"i"), paste0("+", strrep("*", nchar(im)),"i"))
  out = noquote(paste0(re_out, im_out))
  return(out)
}

secret.logical = function(x) {
  out=noquote(rep("*****", length(x)))
  out
}

print <- function(x) {
  UseMethod("print")
}

print.secret <- function(x, ...) {
  if (is.character(x)) {
    out=secret.character(x)
  } 
  if (is.numeric(x)) {
    out=secret.numeric(x)
  }
  if (is.complex(x)) {
    out=secret.complex(x)
  } 
  if (is.logical(x)) {
    out=secret.logical(x)
  } #else {
    #stop("Unsupported type for secret object.")
  #}
  
  print(out)
  
  return(invisible(unclass(x)))
}

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
a=print.secret(c("TRUE","FALSE"))
a
a=print.secret(c(-123.12+1234i, -123-1234i,-123-1234.12i))
mean(a)
print(secret(c(1.23, -4.56)))
s = secret(c(1.23, -4.56))