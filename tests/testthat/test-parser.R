test_that("parsing and evaluation of parsed results works", {
  if(!maxima.env$maxima$isInstalled()) 
    skip("Maxima not installed")

  t1 <- maxima.get("integrate(1/(1 - x^3), x);")
  p1 <- maxima.eval(t1, code = TRUE, envir = list(x = 1:5))
  expect_s3_class(t1, "maxima")
  expect_type(t1, "list")
  expect_equal(nchar(paste0(deparse(attr(t1, "parsed")), collapse = "")), 130L)
  expect_length(p1, 5)
  expect_equal(p1[1], Inf)
  expect_false(attr(t1, "suppressed"))

  t2 <- maxima.get("2+2$")
  p2 <- maxima.eval(t2, code = TRUE)
  expect_s3_class(t2, "maxima")
  expect_type(t2, "list")
  expect_true(is.na(nchar(attr(t2, "parsed"))))
  expect_true(is.na(p2))
  expect_length(p2, 1)
  expect_true(attr(t2, "suppressed"))

  p3 <- maxima.eval("integrate(x, x);", code = TRUE, list(x = 1))
  expect_length(p3, 1)
  expect_gte(nchar(deparse(attr(p3, "maxima"))), 18L)

  p4 <- maxima.eval("x^2+1;", code = FALSE, list(x = 3))
  expect_null(attr(p4, "maxima"))
})

