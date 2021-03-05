test_that("execution returns formatted strings", {
		  maxima.start()
  expect_match(maxima.tell("2+2"), "^\\$\\$[[:space:]|[:print:]]*\\$\\$$")
  expect_match(maxima.tell("2+2;"), "^\\$\\$[[:space:]|[:print:]]*\\$\\$$")
  expect_match(maxima.tell("jacobian( [alpha / (alpha + beta), 1 / sqrt(alpha + beta)], [alpha, beta] )"), "^\\$\\$[[:space:]|[:print:]]*\\$\\$$")
  expect_match(maxima.tell("4+4$"), "")

  # returns a error message between output prompt and new input prompt
  # argument needs to be quoted
  expect_match(maxima.tell("apropos(int)"), "^\\$\\$[[:space:]|[:print:]]*\\$\\$$")
  expect_match(maxima.tell("demo(append)"), "^\\$\\$[[:space:]|[:print:]]*\\$\\$$")

  expect_match(maxima.tell("example(append)"), "^\\$\\$[[:space:]|[:print:]]*\\$\\$$")
  expect_match(maxima.tell("describe(append)"), "^\\$\\$[[:space:]|[:print:]]*\\$\\$$")

  # test commands that depend on each other
  expect_match(maxima.tell("V: 4/3 * %pi * r^3"), "^\\$\\$[[:space:]|[:print:]]*\\$\\$$")
  expect_match(maxima.tell("r: 10;"), "^\\$\\$[[:space:]|[:print:]]*\\$\\$$")
  expect_match(maxima.tell("V;"), "^\\$\\$[[:space:]|[:print:]]*\\$\\$$")

  # test re-evaluation of defined variable
  expect_match(maxima.tell("''V;"), "^\\$\\$[[:space:]|[:print:]]*\\$\\$$")

})
