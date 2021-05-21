if (require(devtools) & require(roxygen2) & require(Rcpp) & require(drat)) { 
  rmarkdown::render(input = "docs/index.Rmd", output_file = "index.html") 
  rmarkdown::render(input = "README.Rmd", output_file = "README.md") 
  Rcpp::compileAttributes() 
  devtools::document()
  devtools::load_all()
  devtools::test()
  devtools::build()
  devtools::check()

  devtools::check_win_devel()
  devtools::check_win_oldrelease()
  devtools::check_win_release()

  drat::insertPackage(file = "../builds/rmaxima/source/rmaxima_0.3.2.9000.tar.gz",  
		      repodir = "../drat")
  drat::insertPackage(file = "../builds/rmaxima/win/devel/rmaxima_0.3.2.9000.zip", 
		      repodir = "../drat")
  drat::insertPackage(file = "../builds/rmaxima/win/release/rmaxima_0.3.2.9000.zip", 
		      repodir = "../drat")

  detach("package:rmaxima", unload = TRUE)
}
