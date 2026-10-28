test_that("ask prompts are returned", {
  if(!maxima.env$maxima$isInstalled()) 
    skip("Maxima not installed")

  maxima.start(restart = TRUE)

  expect_match(maxima.get("integrate(x^n,x)"), "Is n equal to -1?")
  to <- maxima.get("y;")

  # expect_s3_class(to, "maxima")
  # expect_type(to, "list")
  # expect_match(attr(to, "input.label"), "^%\\i[[:digit::]]*$")
  # expect_match(attr(to, "output.label"), "^\\%o[[:digit::]]*$")
  # expect_equal(attr(to, "command"), "y;")
  # expect_true(!attr(to, "suppressed"))
  # expect_match(maxima.get("y;"), "^[[:space:]|[:print:]]*$")

  expected_list <- readRDS(test_path("fixtures", "test_ask_user_expected_list.rds"))
  expect_equal(object = to, expected = expected_list)
})
