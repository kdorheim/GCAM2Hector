test_that("get_pregcam_emiss works", {

    expect_true(is.data.frame(PREGCAM_EMISS_DF))

    out <- get_pregcam_emiss(file = NULL)
    expect_equal(out, PREGCAM_EMISS_DF)

    # TODO could try testing with the wrong emissions file aka the default one
    # we would expect an error then.


})

test_that("get_default_emiss works", {

    expect_true(is.data.frame(DEFAULT_EMISS_DF))

    out <- get_default_emiss(file = NULL)
    expect_equal(out, DEFAULT_EMISS_DF)

    # TODO could try testing with the wrong emissions file aka the default one
    # we would expect an error then.
    expect_error(out == PREGCAM_EMISS_DF)

})
