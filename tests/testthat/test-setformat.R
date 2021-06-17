test_that("changing the output format works", {
  if(!maxima.env$maxima$isInstalled()) 
    skip("Maxima not installed")

  # test linear output format
  expect_match(maxima.setformat("linear"), 
	       "linear")
  expect_match(maxima.get("2+2;"), 
	       "4")

  # test latex output format
  expect_match(maxima.setformat("latex"), 
	       "latex")
  expect_match(maxima.get("2+2;"), 
	       "^\\$\\$4\\$\\$$")

  # test mathml output format
  expect_match(maxima.setformat("mathml"), 
	       "mathml")
  expect_match(maxima.get("2+2;"), 
	       "<math xmlns=\"http://www.w3.org/1998/Math/MathML\"> <mn>4</mn> </math>")
})

