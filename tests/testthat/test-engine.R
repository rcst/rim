test_that("maxima knitr engine works", {
  if(!maxima.env$maxima$isInstalled()) 
    skip("Maxima not installed")
  if(.Platform$OS.type == "windows")
    skip("Engine test takes too long under windows")

  fr <- system.file("extdata", c("test.Rmd", "result.md"),
		    package = "rim", mustWork = TRUE)
  fo <- paste0(dirname(fr[1]), "/test.md")

  hash <- digest::digest(readLines(con = fr[2]), "sha256")
  suppressWarnings(knit(input = fr[1], output = fo, quiet = TRUE))

  expect_match(digest::digest(readLines(fo), "sha256"), hash)

  # clean up
  file.remove(fo)

  # gnuplot files
  td <- dirname(dirname(tempfile()))
  rf <- list.files(path = td, 
		   pattern = "(?:maxout|data)[[:digit:]]*\\.gnuplot", 
		   full.names = TRUE)

  file.remove(rf)
})

