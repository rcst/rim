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
  devtools::run_examples()
  devtools::check()

rmarkdown::render(input="inst/extdata/test.Rmd", 
                  output_dir = "inst/extdata/", 
                  output_file = "result.html")
  
  rmarkdown::render(input="inst/extdata/test.Rmd", 
                    output_dir = "inst/extdata/", 
                    output_file = "test.html")

  knitr::knit(input = "inst/extdata/test.Rmd", output = "inst/extdata/result.md")

  devtools::check_win_release()
  devtools::check_win_oldrelease()
  devtools::check_win_devel()
  devtools::check_rhub()

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

test_strings <- c("1+1; /*comment*/", 
                  "/*comment */1+1;", 
                  "1+1 /*comment*/;", 
                  "1 + 1; /* comment:\n another line */")

# does not cover all cases
gsub(pattern = "/\\*.*\\*/", replacement = "", x = test_strings)

# 0 - IN_TERMINATOR,
# 1 - IN_BACKSLASH,
# 2 - IN_QUOTEMARK,
# 3 - IN_SLASH,
# 4 - IN_ASTERIX,
# 5 - IN_COMMENT_ASTERIX,
# 6 - IN_COMMENT,
# 7 - IN_OTHER
trim("1+1; /* comment */") |> checkCommand()
checkCommand("1+1; /* comment */") |> trim()
checkCommand("1+1 /* comment */;")
checkCommand("1+1 /* comment 2+2; */;")
checkCommand("1 /*comment*/ + /*more comment*/ 1;")
checkCommand("1+1 /* comment; /* inside */*/")


checkCommand("/* aa is a variable of interest */  aa : 1234;")
checkCommand(" bb : aa^2; /* Value of bb depends on aa */ ")
checkCommand("/* User-defined infix operator */  infix (\"Q\");")
checkCommand("/* Parses same as a b c, not abc */  a/* foo */Q/* bar */c;")
checkCommand("/* Comments /* can be nested /* to any depth */ */ */  1 + xyz;")

maxima.get("/* comment */")

maxima.get("1+1; /* comment */")
maxima.get("1+1 /* comment */;")
maxima.get("1+1 /* comment 2+2; */;")
maxima.get("1 /*comment*/ + /*more comment*/ 1;")
maxima.get("1+1 /* comment; /* inside */*/")
maxima.get("1+1; 2+2;")

test_code <- c("1+1; /* comment */",
               "/* comment only line */",
               "/**/",
               "1+1; /* comment; /* inside */*/",
               "1+1 /* comment */;")
gather(test_code)


engine <- function() {
  called_from_fn(.GlobalEnv$subs_name)
}

called_from_fn <- function(pattern) {
  call_st <- lapply(sys.calls(), `[[`, 1)
  any(unlist(lapply(call_st, function(x) grepl(pattern, deparse(x)))))
}

knitr_engine_wrapper <- function() {
  # calls maxima engine function with a unique name
  nm <- paste0("g", as.integer(runif(n = 1, min = 1000, max = 9999)))
  .GlobalEnv$subs_name <- nm
  assign(x = nm, value = engine)
  do.call(what = nm, args = list())
}

main()
