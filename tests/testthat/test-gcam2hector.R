# TODO there is probably a better way to handle but for now allow
# tests to be launched from different env.
if(basename(getwd()) == "testthat") {
    base_dir <- "."
} else {
    base_dir <- file.path("tests", "testthat")
}
prj_file <- file.path(base_dir, "prj_data.dat")
prj_data <- loadProject(prj_file)


test_that("run_GCAM2hector", {

    # This should run with a single scenario.
    out <- run_GCAM2hector(db_dir = "gcam_output",
                           db_name = "database_basexdb",
                           prj_file = prj_file)

    expect_true(is.data.frame(out))


    # Get comparison data.
    fetch_GCAM_vs_hector(prj_file) %>%
        select(scenario, year, comp = value, variable) ->
        comp_data

    out %>%
        select(scenario, year, variable, value) %>%
        left_join(comp_data, by = join_by(scenario, year, variable)) %>%
        na.omit %>%
        mutate(AE = (value - comp)) ->
        AE_error


    AE_error %>%
        summarise(value = mean(AE), .by = "variable") ->
       MAE_data

    # Read in the benchmark data...
    threshold <- read.csv(file.path(base_dir, "error_benchmark.csv"))

    # The MAE should be less than or equal to the benchmarked error
    # values per variable.
    MAE_data %>%
        left_join(threshold, by = join_by(variable)) %>%
        mutate(passing = value <= MAE) ->
        error_resutls

    expect_true(all(error_resutls$passing), label = "future MAE is too large")

    # Out of curiosity let's check to see if the
    # historical results pass...
    AE_error %>%
        filter(year <= TRANSITION_DATE) %>%
        summarise(value = mean(AE), .by = "variable") %>%
        left_join(threshold, by = join_by(variable)) %>%
        mutate(passing = value <= MAE) ->
        hist_error

    # Even if the future period is not passing confirm that
    # the historical results are passing, if they are not
    # then there is a serious problem. With the
    # default/historical inputs being used in the framework..
    expect_true(all(hist_error$passing))


})
