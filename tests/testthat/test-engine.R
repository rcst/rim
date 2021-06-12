test_that("maxima knitr engine works", {
	    fr <- system.file("extdata", c("test.Rmd", "result.md"), 
			      package = "rmaxima")
	    fo <- paste0(dirname(fr[1]), "/test.md")

	    hash <- digest(readLines(fr[2]), "sha256")

	    # rmarkdown::render(fr[1], quiet = TRUE, run_pandoc = FALSE)
	    knit(input = fr[1], output = fo, quiet = TRUE, envir = .GlobalEnv)
	    # render(input = fr[1], output_file = fo, quiet = TRUE)

	    expect_match(digest(readLines(fo), "sha256"), hash)

	    # clean up
	    # file.remove(fo)
})

