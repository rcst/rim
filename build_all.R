if (require(devtools) & require(roxygen2) & require(Rcpp)) {
	rmarkdown::render(input = "README.Rmd")
	file.remove("README.html")
	Rcpp::compileAttributes() 
	roxygen2::roxygenise()
	devtools::build()
	devtools::load_all()
	devtools::test()
}
