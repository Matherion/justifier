context("general justifier tests")

###-----------------------------------------------------------------------------
###-----------------------------------------------------------------------------
###-----------------------------------------------------------------------------

testthat::test_that("reading a file with justifications works", {

  res <- load_justifications(here::here("tests",
                                        "testthat",
                                        "example-minutes.jmd"));

  testthat::expect_equal(length(res$raw), 2);

});

###-----------------------------------------------------------------------------

testthat::test_that("reading a directory with justifications works", {

  res <- load_justifications_dir(here::here("tests",
                                            "testthat"));

  testthat::expect_equal(length(res), 7);

});

###-----------------------------------------------------------------------------

testthat::test_that("parsing justifications works", {

  res <- load_justifications(here::here("tests",
                                        "testthat",
                                        "pp19.1-target-behavior-selection.jmd"));

  testthat::expect_equal(res$supplemented$decisions$
                           decision_to_select_behavior_1$
                           justification$justification_05$
                           assertion$assertion_nocturnal_2$
                           source$source_Lange$xdoi,
                         "doi:10.1111/j.1749-6632.2009.05300.x");

  testthat::expect_equal(res$supplemented$assertions$
                           assertion_sleep_memory_1$
                           source$source_Diekelmann$xdoi,
                         "doi:10.1038/nrn2762");

  testthat::expect_equal(res$supplemented$sources$
                           source_Diekelmann$
                           comment,
                         "test of a comment");

});

###-----------------------------------------------------------------------------

testthat::test_that("parsing simplified, just extracted justifications works", {

  res1 <- yum::load_and_simplify(here::here("tests",
                                            "testthat",
                                            "pp19.1-target-behavior-selection.jmd"));

  res2 <- parse_justifications(res1);


  testthat::expect_equal(res$supplemented$decisions$
                           decision_to_select_behavior_1$
                           justification$justification_05$
                           assertion$assertion_nocturnal_2$
                           source$source_Lange$xdoi,
                         "doi:10.1111/j.1749-6632.2009.05300.x");

});

###-----------------------------------------------------------------------------


