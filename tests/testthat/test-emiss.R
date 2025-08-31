

# TODO there should be a better way to handle this!
if(basename(getwd()) == "testthat") {
    base_dir <- "."
} else {
    base_dir <- file.path("tests", "testthat")
}
prj_file <- file.path(base_dir, "gcam_db.dat")
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


test_that("handle_neg_CO2_emiss", {

    # Make example data frames, one that is strictly positive and another
    # that has a mix of negative values.
    yrs <- 5
    postive <- data.frame(scenario = "x",
                          year = rep(seq(2020, by = 5, length.out = yrs), 2),
                          variable = rep(c(LUC_EMISSIONS(), FFI_EMISSIONS()),  each = yrs),
                          value = sample(x = c(1:100), size = yrs * 2, replace = TRUE),
                          units = "fake")
    mix <- postive
    mix$value[4] <-  mix$value[4] * -1
    mix$value[9] <-  mix$value[9] * -1
    mix$value[10] <-  mix$value[10] * -1

    # If there are no negative emissions then we would
    # expect the uptake variables to equal 0.
    out1 <- handle_neg_CO2_emiss(postive)

    # There should be no change in the FFI and LUC emissions
    expect_equal({out1 %>%
            filter(variable %in% c(LUC_EMISSIONS(), FFI_EMISSIONS())) %>%
            pull(value) %>%
            sum}, sum(postive$value))

    # The uptake emissions should all be 0
    expect_true({all(out1 %>%
                         filter(variable %in% c(LUC_UPTAKE(), DACCS_UPTAKE())) %>%
                         pull(value) == 0)})



    out2 <- handle_neg_CO2_emiss(mix)
    # All of the values should be positive!
    expect_true(all(out2$value >= 0))


    # The total absolute values should be equal
    expect_equal(sum(out2$value), sum(abs(mix$value)))


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


