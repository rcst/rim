test_that("maxima knitr engine works", {
  if(!maxima.env$maxima$isInstalled()) 
    skip("Maxima not installed")
  if(.Platform$OS.type == "windows")
    skip("Engine test takes too long under windows")

  fr <- system.file("extdata", c("test.Rmd", "result.html"),
		    package = "rim", mustWork = TRUE)
  fo <- paste0(dirname(fr[1]), "/test.html")

  hash <- digest(readLines(con = fr[2]), "sha256")
  # suppressWarnings(knit(input = fr[1], output = fo, quiet = TRUE, envir = .GlobalEnv))
  suppressWarnings(rmarkdown::render(input=fr[1], 
		    output_dir = dirname(fr[1]), 
		    output_file = "test.html", 
		    quiet = TRUE))

  expect_match(digest(readLines(fo), "sha256"), hash)

  # clean up
  file.remove(fo)
})

