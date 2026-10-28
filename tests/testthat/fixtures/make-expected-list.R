maxima.start(restart = TRUE)
maxima.get("integrate(x^n,x);")
x <- maxima.get("y;")
saveRDS(x, file = "tests/testthat/fixtures/test_ask_user_expected_list.rds")

maxima.start(restart = TRUE)
x <- maxima.load("ratpow", quote = FALSE, stuck = TRUE)
saveRDS(x, file = "tests/testthat/fixtures/test_load_expected_list.rds")

maxima.start(restart = TRUE)
x <- maxima.apropos("int")
saveRDS(x, file = "tests/testthat/fixtures/test_apropos_expected_list.rds")

maxima.start(restart = TRUE)
x <- maxima.get("jacobian( [alpha / (alpha + beta), 1 / sqrt(alpha + beta)], [alpha, beta] );")
saveRDS(x, file = "tests/testthat/fixtures/test_execute_expected_list_1.rds")

maxima.start(restart = TRUE)
x <- maxima.get("2+2$")
saveRDS(x, file = "tests/testthat/fixtures/test_execute_expected_list_2.rds")
