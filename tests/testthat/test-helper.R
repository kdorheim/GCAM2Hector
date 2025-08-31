

test_that("req_check works", {

    # This should work on a data frame and the column.
    req <- c("mpg", "cyl", "disp", "hp", "drat", "wt")
    expect_true(req_check(names(mtcars), req))

    req <- c("setosa", "versicolor")
    expect_true(req_check(iris$Species, req))

    # Errors should be thrown in if one of the required
    # elements are missing.
    expect_error(req_check(names(mtcars), "fake"), regexp = "x is misisng: fake")

})

test_that("repeat_for_scns works", {

    scns <- c("fake1", "test2")
    out <- repeat_for_scns(x = mtcars, scns = scns)

    expect_equal(nrow(out), nrow(mtcars) * length(scns))
    expect_equal(ncol(out), ncol(mtcars) + 1)
    expect_true(req_check(x = names(out), req = "scenario"))
    expect_error(req_check(x = names(mtcars), req = "scenario"))

})

test_that("read_hector_csv works", {

    hector_file <- system.file(package = "hector", "input/tables/ssp245_emiss-constraints_rf.csv")
    out <- read_hector_csv(hector_file)

    expect_true(is.data.frame(out))
    expect_true(req_check(names(out), req = c("year", "variable", "units")))

})

test_that("add_missing_yrs works", {

    # Make first example data
    req_years <- 2005:2050
    year <- floor(seq(from = min(req_years), to = max(req_years), length.out = 10))
    value <- (10 * year + 2) + rnorm(length(year), mean = 0, sd = 100)
    datx <- data.frame(year, value, scenario = "x", variable = "k")

    # Make data for a second scenario
    daty <- datx
    daty$scenario <- "y"
    daty$value <- (10 * year + 2) + rnorm(length(year), mean = 0, sd = 50)

    # Example data frame with two scenarios
    datxy <- rbind(datx, daty)


    # The internal function should work when passed only 1 scenario.
    out1 <- internal.add_missing_yrs_1scn(datx, req_years)
    expect_equal(nrow(out1), length(req_years))

    # When the internal function is fed data for multiple scenarios
    # the function should run but it will return different and we should
    # get some sort of warning.
    expect_warning(out2 <- internal.add_missing_yrs_1scn(datxy, req_years))

    # The internal function should work when passed only 1 scenario and
    # should return the same data as the single call.
    out3 <- add_missing_yrs(datx, req_years)
    expect_equal(out1$value, out3$value)

    out4 <- add_missing_yrs(datxy, req_years)
    expect_equal(nrow(out4), length(req_years) * 2)


})



