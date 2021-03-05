if (require(devtools) & require(roxygen2) & require(Rcpp)) {
	Rcpp::compileAttributes() 
	roxygen2::roxygenise()
	devtools::build() 
	devtools::load_all()
	devtools::test()
}
