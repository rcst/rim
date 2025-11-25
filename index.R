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
  knitr::knit(input = "inst/extdata/test.Rmd", output = fo <- tempfile(fileext = ".md"))

  devtools::check_win_release()
  devtools::check_win_oldrelease()
  devtools::check_win_devel()
  devtools::check_mac_release()
  devtools::check_rhub()

  devtools::release()
}

# command reception in Maxima

# [x] multiple complete
# (%i1) 1+1;2+2;
# -> runs both commands, displays two consecutive output labels
dissect_repl_input("1+1;2+2;")

# [x] mutltiple, partial incomplete
# (%i1) 1+1; 2+
# -> runs only first, ignores second
dissect_repl_input("1+1;2+")

# [x] incomplete
# (%i1) 1+;
# -> returns error
dissect_repl_input("1+;")

# [x] multiple complete with comments
dissect_repl_input("1+1; /* hello */ 2+2;")
dissect_repl_input("1+1 /* hello /* world */ */; 2+2;")
dissect_repl_input("1+1 /* hello 0.5*2; /* world */ */; 2+2;")

# [ ] mutltiple, partial incomplete, with comments
dissect_repl_input("1+1;2+ /* xxx */")
dissect_repl_input("/*xxx */1+1$2+ /* xxx */")
dissect_repl_input("/*xxx */1+1;2+ /* xxx */; 3+3;")

# split user input line into separate commands and sequentially pass to maxima$get()
assign(x = "a", value = 1L, env = rim_global())
a
names(rim_global())
rim_global()$a
parent.env(env = globalenv()) |> parent.env() |> parent.env()
