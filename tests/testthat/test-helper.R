

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
