test_that("maxima.apropos returns exact result", {
  if(!maxima.env$maxima$isInstalled()) 
    skip("Maxima not installed") 

  to <- maxima.apropos("int")

  expect_s3_class(to, "maxima")
  expect_type(to, "list")
  expect_match(attr(to, "input.label"), "^%\\i[[:digit::]]*$")
  expect_match(attr(to, "output.label"), "^\\%o[[:digit::]]*$")
  expect_equal(attr(to, "command"), "apropos(\"int\");")
  expect_true(!attr(to, "suppressed"))

  expect_named(to, expected = c("wtl", "wol"))
  expect_named(to$wtl, expected = c("linear", "ascii", "latex", "inline", "mathml"))
  expect_named(to$wol, expected = c("linear", "ascii", "latex", "inline", "mathml"))

  expect_length(to$wtl$linear, 25L)
  expect_length(to$wtl$ascii, 22L)
  expect_length(to$wtl$latex, 1L)
  expect_length(to$wtl$mathml, 63L)

  expect_length(to$wol$linear, 21L)
  expect_length(to$wol$ascii, 22L)
  expect_length(to$wol$latex, 1L)
  expect_length(to$wol$mathml, 62L)
})
