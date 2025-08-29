
# Users might want to skip this test if there is no GCAM xml output bd to work with.
# TODO there is probably a better way to do this.
if(FALSE){
    test_that("intrnl.run_all_queries works", {

        if(basename(getwd()) == "testthat") {
            db_dir <- "../../gcam_output"
        } else {
            db_dir <- "gcam_output"
        }

        db_name <- "database_basexdb"

        x <- intrnl.run_all_queries(db_dir = db_dir, db_name = db_name)
        expect_true(file.exists(x))

        expect_true(is.list(loadProject(x)))

        xx <- intrnl.run_all_queries(db_dir = db_dir, db_name = db_name, prj_file = x)

        expect_error({intrnl.run_all_queries(db_dir = db_dir,
                                             db_name = db_name,
                                             prj_file = "prj_data.dat",
                                             query_file = "fake.xml")})

        file.remove(x)

    })
}


