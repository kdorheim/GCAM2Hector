

# TODO there should be a better way to handle this!
if(basename(getwd()) == "testthat") {
    base_dir <- "."
} else {
    base_dir <- file.path("tests", "testthat")
}
prj_file <- file.path(base_dir, "prj_data.dat")
prj_data <- loadProject(prj_file)


test_that("get_pregcam_emiss works", {

    expect_true(is.data.frame(PREGCAM_EMISS_DF))

    out <- get_pregcam_emiss(file = NULL)
    expect_equal(out, PREGCAM_EMISS_DF)

    # TODO could try testing with the wrong emissions file aka the default one
    # we would expect an error then.


})

test_that("get_default_emiss", {

    expect_true(is.data.frame(DEFAULT_EMISS_DF))

    out <- get_default_emiss(file = NULL)
    expect_equal(out, DEFAULT_EMISS_DF)

    # TODO could try testing with the wrong emissions file aka the default one
    # we would expect an error then.
    expect_error(out == PREGCAM_EMISS_DF)

})


test_that("get_nonCO2_emiss", {

    out <- get_nonCO2_emiss(prj_data)

    # All of the emissions other than the CO2 ones should be returned
    # here. Make sure that the correct variables are being returned a
    # sand that we are not missing any expected ones...
    HECTOR_INPUTS <- c(GCAM_EMISS, DEFAULT_EMISS)
    co2_emiss <- c(LUC_EMISSIONS(), LUC_UPTAKE(), FFI_EMISSIONS(), DACCS_UPTAKE())
    expect_true(all(!out$variable %in% co2_emiss))

    non_co2_emiss <- setdiff(HECTOR_INPUTS, co2_emiss)
    expect_true(all(out$variable %in% non_co2_emiss))

})


test_that("get_CO2_emiss", {

    out1 <- get_CO2_emiss(prj_data)
    expect_true(req_check(out1$year, 1745:2100))

    co2_emiss <- c(LUC_EMISSIONS(), LUC_UPTAKE(), FFI_EMISSIONS(), DACCS_UPTAKE())
    expect_true(req_check(out1$variable, co2_emiss))

})


test_that("get_hector_inputs", {

    db_dir = "."
    db_name = "fake"

    # Get the hector inputs
    suppressMessages({ out <- get_hector_inputs(db_dir, db_name, prj_file = prj_file)})

    # Check to make sure that we are not missing years for any
    # of the variables.
    split(out, interaction(out$variable, out$scenario)) %>%
        sapply(function(x){
            req_check(x$year, 1750:2100)
        }, simplify = TRUE) ->
        yrs_check
    expect_true(all(yrs_check))

    # Make sure all inputs are included here
    all_inputs <- c(DEFAULT_EMISS, GCAM_EMISS)
    expect_true(req_check(x = out$variable, req = all_inputs))
})


