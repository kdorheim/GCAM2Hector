# TODO there is probably a better way to handle but for now allow
# tests to be launched from different env.
if(basename(getwd()) == "testthat") {
    base_dir <- "."
} else {
    base_dir <- file.path("tests", "testthat")
}
prj_file <- file.path(base_dir, "prj_data.dat")
prj_data <- loadProject(prj_file)


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


# test_that("internal.write_hector_csv and internal.write_ini", {
#     # TODO need to add!
# })

# # TODO there is a problem with this!!! FML wht is going on...
# test_that("write_GCAM2hector", {
#
#     ini <-  write_GCAM2hector(db_dir = "fake",
#                               db_name = "fake",
#                               outdir = ".",
#                               prj_file = prj_file)
#
#
#     # UGH there is a problem, although for the life of me I cannot figure
#     # out what is wrong!
#     hc <- newcore(ini)
#
#
# })
