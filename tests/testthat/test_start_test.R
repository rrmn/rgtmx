library(rgtmx)

test_that("start_stest returns error if no or malformed URL supplied", {
  expect_error(start_test(url = NULL))
  expect_error(start_test(url = NA))
  expect_error(start_test(url = ""))
  expect_error(start_test(url = 1))
  expect_error(start_test(url = list(list("A", "B"))))
})
#> Test passed 🌈
