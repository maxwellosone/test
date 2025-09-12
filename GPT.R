secret <- function(x) {
  if (!is.atomic(x)) {
    stop("`secret` only supports atomic vectors.")
  }
  structure(x, class = "secret")
}


sec_char <- function(x) {
  ifelse(is.na(x), NA_character_, paste0('"', strrep("*", nchar(x)), '"'))
}

sec_numeric <- function(x) {
  ifelse(is.na(x), NA_character_,
         ifelse(x < 0 , paste0("-", strrep("*", nchar(x)-1)), strrep("*", nchar(x))))
}

sec_logical <- function(x) {
  ifelse(is.na(x), NA_character_, "*****")
}

sec_complex <- function(x) {
  re = Re(x)
  im = Im(x)
  re_out=ifelse(re < 0 , paste0("-", strrep("*", nchar(re)-1)), strrep("*", nchar(re)))
  im_out=ifelse(im < 0 , paste0("-", strrep("*", nchar(im)-1),"i"), paste0("+", strrep("*", nchar(im)),"i"))
  out = paste0(re_out, im_out)
  ifelse(is.na(x), NA_character_,out)
}

print.secret = function(x, ...) {
  secret = switch(typeof(x),
                   "character" = sec_char(x),
                   "integer"   = sec_numeric(x),
                   "double"    = sec_numeric(x),
                   "logical"   = sec_logical(x),
                   "complex"   = sec_complex(x),
                   stop("Unsupported type: ", typeof(x))
  )
  print(noquote(secret))
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