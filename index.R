if (require(devtools) & require(roxygen2) & require(Rcpp) & require(drat)) { 
  rmarkdown::render(input = "docs/index.Rmd", output_file = "index.html") 
  rmarkdown::render(input = "README.Rmd", output_file = "README.md") 


  Rcpp::compileAttributes() 
  devtools::document()
  devtools::load_all()
  devtools::test()
  devtools::test_active_file("tests/testthat/test-parser.R")
  devtools::test_active_file("tests/testthat/test-engine.R")
  devtools::build()
  devtools::check()

rmarkdown::render(input="inst/extdata/test.Rmd", 
                  output_dir = "inst/extdata/", 
                  output_file = "result.html")
  
  rmarkdown::render(input="inst/extdata/test.Rmd", 
                    output_dir = "inst/extdata/", 
                    output_file = "test.html")

  devtools::check_win_release()
  devtools::check_win_oldrelease()
  devtools::check_win_devel()

  devtools::submit_cran()
  devtools::release()

  # drat
  options(dratRepo = "/home/eric/documents/R/packages/drat/")
  drat::insertPackage(file = "../builds/rim/source/rim_0.5.0.tar.gz",  
		      action = "archive", commit = TRUE)

  a <- drat::insertPackage(file = "../builds/rim/win/release/rim_0.4.0.zip", 
			   action = "archive",
			   commit = TRUE)

  a <- drat::insertPackage(file = "../builds/rim/win/oldrelease/rim_0.4.0.zip", 
			   action = "archive",
			   commit = TRUE)
  a  <- drat:::getRepoInfo()
  drat::pruneRepo(pkg = "rim", version = "0.4.0", remove = TRUE)
  drat:::getPackageInfo("../builds/rim/win/release/rim_0.4.0.zip")
  drat:::identifyPackageType("../builds/rim/win/release/rim_0.4.0.zip")
  drat::updateRepo()

  detach("package:rim", unload = TRUE)
}
