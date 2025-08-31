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


test_that("get_nonCO2_emiss", {

    # TODO there should be a better way to handle this!
    if(basename(getwd()) == "testthat") {
        base_dir <- "."
    } else {
        base_dir <- file.path("tests", "testthat")
    }

    prj_data <- loadProject(file.path(base_dir, "gcam_db.dat"))
    out <- get_nonCO2_emiss(prj_data)

    # All of the emissions other than the CO2 ones should be returned
    # here. Make sure that the correct variables are being returned a
    # sand thaty we are not missing any expected ones...
    HECTOR_INPUTS <- c(GCAM_EMISS, DEFAULT_EMISS)
    co2_emiss <- c(LUC_EMISSIONS(), LUC_UPTAKE(), FFI_EMISSIONS(), DACCS_UPTAKE())
    expect_true(all(!out$variable %in% co2_emiss))

    non_co2_emiss <- setdiff(HECTOR_INPUTS, co2_emiss)
    expect_true(all(out$variable %in% non_co2_emiss))

})
