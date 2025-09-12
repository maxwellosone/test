test_that("fizzbuzz works correctly", {
  result = fizzbuzz(c(1,3,5,15))
  expect_equal(
    result,
    c("1","Fizz","Buzz","FizzBuzz")
  )
})
