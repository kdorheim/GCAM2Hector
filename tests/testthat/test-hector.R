# TODO: figure out a better way to supress tests in the sake of testing run time.
TEST <- FALSE
if(TEST){

test_that("internal.single_hector_run", {

    # use a default scenario inputs to make sure
    # we can run hector
    ini <- system.file(package = "hector", "input/hector_ssp245.ini")
    hc  <- newcore(ini)

    system.file(package = "hector",
                "input/tables/ssp434_emiss-constraints_rf.csv") %>%
        read_hector_csv %>%
        filter(variable %in% c(GCAM_EMISS, DEFAULT_EMISS)) %>%
        mutate(scenario = "fake") ->
        inputs

    out <- internal.single_hector_run(hc, inputs)
    expect_true(is.data.frame(out))

    # default run that should have different results
    hc  <- newcore(ini)
    run(hc, runtodate = 2100)
    default_out <- fetchvars(hc, dates = 2025:2100, vars = out$variable)

    default_out %>%
        select(year, variable, default = value) %>%
        left_join(out, by = join_by(year, variable)) %>%
        mutate(SE = (default - value)^2) %>%
        summarise(MSE = mean(SE), .by = c("variable")) ->
        MSE_df

    expect_true(all(MSE_df$MSE > 1e-8))




})

}

# TODO there should be a better way to handle this!
if(basename(getwd()) == "testthat") {
    base_dir <- "."
} else {
    base_dir <- file.path("tests", "testthat")
}
prj_file <- file.path(base_dir, "gcam_db.dat")
prj_data <- loadProject(prj_file)

test_that("run_GCAM2hector", {

    db_dir <- "fake_dir"
    db_name <- "fake_basexdb"

    out <- run_GCAM2hector(db_dir = db_dir,
                           db_name = db_name,
                           prj_file = prj_file)

    expect_true(is.data.frame(out))

})
