test_that("changing the output format works", {
  maxima.start()

  # test linear output format
  expect_null(maxima.setformat("linear"))
  expect_match(maxima.get("2+2;"), "4")

  # test latex output format
  expect_null(maxima.setformat("latex"))
  expect_match(maxima.get("2+2;"), "^\\$\\$4\\$\\$$")
})

