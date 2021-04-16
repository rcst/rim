if (require(devtools) & require(roxygen2) & require(Rcpp) & require(drat)) {
	rmarkdown::render(input = "README.Rmd")
	file.remove("README.html")
	Rcpp::compileAttributes() 
	devtools::document()
	devtools::load_all()
	devtools::test()
	devtools::build()
	devtools::check()

	# devtools::check_win_devel()
	# devtools::check_win_oldrelease()
	# devtools::check_win_release()

	drat::insertPackage(file = "../rmaxima_0.0.0.9000.tar.gz", repodir = "../drat")
}
