maxima.start(restart = TRUE)
maxima.get("integrate(x^n,x);")
x <- maxima.get("y;")
saveRDS(x, file = "tests/testthat/fixtures/expected_list.rds")
