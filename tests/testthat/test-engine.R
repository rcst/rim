test_that("maxima knitr engine works", {
  if(!maxima.env$maxima$isInstalled()) 
    skip("Maxima not installed")
  if(.Platform$OS.type == "windows")
    skip("Engine test takes too long under windows")

  # currently fails with R-devel and fedora
  # due to escaping of percent sign
  # to check current R-Version
  # getRversion()

  fr <- system.file("extdata", c("test.Rmd", "result.md"),
		    package = "rim", mustWork = TRUE)
  # fo <- paste0(dirname(fr[1]), "/test.md")

  # hash <- digest::digest(readLines(con = fr[2]), "sha256")
  # suppressWarnings(knit(input = fr[1], output = fo, quiet = TRUE))
  testIn <- readLines(con = fr[1])
  result <- readLines(con = fr[2])
  suppressWarnings(testOut <- knit(text = testIn, quiet = TRUE))
  testOut <- unlist(strsplit(testOut, "\n"))

  # expect_match(digest::digest(readLines(fo), "sha256"), hash)
  expect_equal(object = testOut, expected = result)

  # clean up
  # file.remove(fo)

  # gnuplot files
  td <- dirname(dirname(tempfile()))
  rf <- list.files(path = td, 
		   pattern = "(?:maxout|data)[[:digit:]]*\\.gnuplot", 
		   full.names = TRUE)

  file.remove(rf)
})

