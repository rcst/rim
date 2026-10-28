test_that("execution returns formatted strings", {
  if(!maxima.env$maxima$isInstalled()) 
    skip("Maxima not installed")

  maxima.start(restart = TRUE)

  to <- maxima.get("jacobian( [alpha / (alpha + beta), 1 / sqrt(alpha + beta)], [alpha, beta] );")
  # expect_s3_class(to, "maxima")
  # expect_type(to, "list")
  # expect_match(attr(to, "input.label"), "^%\\i[[:digit::]]*$")
  # expect_match(attr(to, "output.label"), "^\\%o[[:digit::]]*$")
  # expect_equal(attr(to, "command"), "jacobian( [alpha / (alpha + beta), 1 / sqrt(alpha + beta)], [alpha, beta] );")
  # expect_true(!attr(to, "suppressed"))
  expected_list <- readRDS(test_path("fixtures", "test_execute_expected_list_1.rds"))
  expect_equal(object = to, expected = expected_list)

  maxima.start(restart = TRUE)

  to <- maxima.get("2+2$")
  # expect_s3_class(to, "maxima")
  # expect_type(to, "list")
  # expect_length(to, 0L)
  # expect_match(attr(to, "input.label"), "^%\\i[[:digit::]]*$")
  # expect_match(attr(to, "output.label"), "^\\%o[[:digit::]]*$")
  # expect_equal(attr(to, "command"), "2+2$")
  # expect_true(attr(to, "suppressed"))
  expected_list <- readRDS(test_path("fixtures", "test_execute_expected_list_2.rds"))
  expect_equal(object = to, expected = expected_list)
})
