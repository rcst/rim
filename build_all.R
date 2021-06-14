if (require(devtools) & require(roxygen2) & require(Rcpp) & require(drat)) { 
  rmarkdown::render(input = "docs/index.Rmd", output_file = "index.html") 
  rmarkdown::render(input = "README.Rmd", output_file = "README.md") 
  Rcpp::compileAttributes() 
  devtools::document()
  devtools::load_all()
  devtools::test()
  devtools::test(filter = "setformat")
  devtools::build()
  devtools::check()

  devtools::check_win_devel()
  devtools::check_win_oldrelease()
  devtools::check_win_release()

  drat::insertPackage(file = "../builds/rmaxima/source/rim_0.4.0.tar.gz",  
		      repodir = "../drat")
  drat::insertPackage(file = "../builds/rmaxima/win/devel/rim_0.4.0.zip", 
		      repodir = "../drat")
  drat::insertPackage(file = "../builds/rmaxima/win/release/rim_0.4.0.zip", 
		      repodir = "../drat")

  detach("package:rim", unload = TRUE)
}
