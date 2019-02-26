context("general justifier tests")

###-----------------------------------------------------------------------------
###-----------------------------------------------------------------------------
###-----------------------------------------------------------------------------

test_that("reading a file with justifications works", {

  res <- load_justifications(here::here("tests",
                                        "testthat",
                                        "example-minutes.jmd"));

}

###-----------------------------------------------------------------------------

test_that("reading a directory with justifications works", {

  res <- load_justifications_dir(here::here("tests",
                                            "testthat"));

}

###-----------------------------------------------------------------------------

