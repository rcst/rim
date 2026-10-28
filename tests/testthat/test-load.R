test_that("maxima.load returns exact result", {
  if(!maxima.env$maxima$isInstalled()) 
    skip("Maxima not installed") 

  maxima.start(restart = TRUE)

  to <- maxima.load("ratpow", quote = FALSE, stuck = TRUE, suppress = FALSE)

  # expect_s3_class(to, "maxima")
  # expect_type(to, "list")
  # expect_match(attr(to, "input.label"), "^%\\i[[:digit::]]*$")
  # expect_match(attr(to, "output.label"), "^\\%o[[:digit::]]*$")
  # expect_equal(attr(to, "command"), "load(ratpow)$")
  # expect_true(attr(to, "suppressed"))

  expected_list <- readRDS(test_path("fixtures", "test_load_expected_list.rds"))
  expect_equal(object = to, expected = expected_list)
})

