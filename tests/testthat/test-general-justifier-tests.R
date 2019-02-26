context("general justifier tests")

###-----------------------------------------------------------------------------
###-----------------------------------------------------------------------------
###-----------------------------------------------------------------------------

testthat::test_that("reading a file with justifications works", {

  res <- load_justifications(here::here("tests",
                                        "testthat",
                                        "example-minutes.jmd"));

  testthat::expect_equal(length(res), 2);

});

###-----------------------------------------------------------------------------

testthat::test_that("reading a directory with justifications works", {

  res <- load_justifications_dir(here::here("tests",
                                            "testthat"));

  testthat::expect_equal(length(res), 2);

});

###-----------------------------------------------------------------------------

