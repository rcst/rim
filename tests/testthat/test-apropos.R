test_that("apropos returns formatted strings", {
		  maxima.start()
  expect_match(maxima.apropos("int"), "^\\$\\$[[:space:]|[:print:]]*\\$\\$$")
})

