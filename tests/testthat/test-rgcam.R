
# Users might want to skip this test if there is no GCAM xml output bd to work with.
# TODO there is probably a better way to do this.
if(FALSE){
    test_that("get_all_queries works", {

        # There is probably a better way to do this but for now
        # let the code run with
        if(basename(getwd()) == "testthat") {
            output_dir <- file.path("..", "..")
            base_dir <- "."
        } else {
            output_dir <- "."
            base_dir   <- file.path("tests", "testthat")
        }

        db_dir   <- file.path(output_dir, "gcam_output")
        db_name  <- file.path(output_dir, "database_basexdb")
        prj_file <- file.path(base_dir, "prj_data.dat")

        x <- get_all_queries(db_dir = db_dir, db_name = db_name, prj_file = prj_file)
        expect_true(file.exists(x))

        expect_true(is.list(loadProject(x)))

        xx <- get_all_queries(db_dir = db_dir, db_name = db_name, prj_file = x)

        expect_error({get_all_queries(db_dir = db_dir,
                                             db_name = db_name,
                                             prj_file = "prj_data.dat",
                                             query_file = "fake.xml")})

    })
}

test_that("fetch_GCAM_vs_hector works", {

    if(basename(getwd()) == "testthat") {
        base_dir <- "."
    } else {
        base_dir <- file.path("tests", "testthat")
    }

    # Use the example data
    file <- file.path(base_dir, "prj_data.dat")
    expect_true(file.exists(file))

})

