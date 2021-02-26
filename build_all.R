if (require(devtools)) { 
	devtools::build() 
	install.packages("../rmaxima_0.0.0.9000.tar.gz", 
			 verbose = TRUE, 
			 INSTALL_opts = c("--install-tests"))

	testthat::test_package("rmaxima")
}
