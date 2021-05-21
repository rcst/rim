test_that("apropos returns formatted strings", {
  expect_match(maxima.apropos("int"), "^[[:space:]|[:print:]]*$")
})

