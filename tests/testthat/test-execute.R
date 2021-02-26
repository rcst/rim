test_that("execution returns formatted strings", {
  expect_match(maxima$execute("2+2"), "^\\$\\$[[:space:]|[:print:]]*\\$\\$$")
  expect_match(maxima$execute("2+2;"), "^\\$\\$[[:space:]|[:print:]]*\\$\\$$")
  expect_match(maxima$execute("jacobian( [alpha / (alpha + beta), 1 / sqrt(alpha + beta)], [alpha, beta] )"), "^\\$\\$[[:space:]|[:print:]]*\\$\\$$")
  expect_match(maxima$execute("example(append)"), "^\\$\\$[[:space:]|[:print:]]*\\$\\$$")
})
