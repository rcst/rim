test_that("ask prompts are returned", {
  if(!maxima.env$maxima$isInstalled()) 
    skip("Maxima not installed")
  expect_match(maxima.get("integrate(x^n,x)"), "Is n equal to -1?")

  to <- maxima.get("y;")
  expect_s3_class(to, "maxima")
  expect_type(to, "list")
  expect_match(attr(to, "input.label"), "^%\\i[[:digit::]]*$")
  expect_match(attr(to, "output.label"), "^\\%o[[:digit::]]*$")
  expect_equal(attr(to, "command"), "y;")
  expect_true(!attr(to, "suppressed"))
  # expect_match(maxima.get("y;"), "^[[:space:]|[:print:]]*$")

  expect_length(to$wtl$linear, 1L)
  expect_length(to$wtl$ascii, 1L)
  expect_length(to$wtl$latex, 1L)
  expect_length(to$wtl$mathml, 3L)

  expect_length(to$wol$linear, 1L)
  expect_length(to$wol$ascii, 1L)
  expect_length(to$wol$latex, 1L)
  expect_length(to$wol$mathml, 2L)
})
