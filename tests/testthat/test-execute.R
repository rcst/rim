test_that("execution returns formatted strings", {
		  maxima.start()
  expect_match(maxima.tell("2+2"), "^\\$\\$[[:space:]|[:print:]]*\\$\\$$")
  expect_match(maxima.tell("2+2;"), "^\\$\\$[[:space:]|[:print:]]*\\$\\$$")
  expect_match(maxima.tell("jacobian( [alpha / (alpha + beta), 1 / sqrt(alpha + beta)], [alpha, beta] )"), "^\\$\\$[[:space:]|[:print:]]*\\$\\$$")
  expect_match(maxima.tell("4+4$"), "")
  expect_match(maxima.tell("example(append)"), "^\\$\\$[[:space:]|[:print:]]*\\$\\$$")
})
