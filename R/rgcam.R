

#' Process a GCAM xml output data base
#'
#' Get all of the data from a GCAM xml output data base by running all
#' of the climate and emission queries.
#'
#' @param db_dir path to the directory that contains the GCAM xml db to process see \code{\link[rgcam]{localDBConn}}.
#' @param db_name str name of xml db to process see see \code{\link[rgcam]{localDBConn}}.
#' @param query_file XML query file to pass to the GCAM Model Interface. If NULL, use a default query file containing hector relevant queries.
#' @param prj_file R project data file to add extracted results to. Can be name of a project data file. The file will be created if it doesn't already exist. Default set to NULL will write the project data out to a temporary directory.
#' @returns the path to the rgcam project file.
#' @noRd
intrnl.run_all_queries <- function(db_dir,
                                   db_name,
                                   query_file = NULL,
                                   prj_file = NULL){

    if(is.null(query_file)){
        query_file <- system.file("extdata", "hector-queries.xml", package = "GCAM2Hector")
    }

    if(is.null(prj_file)){
        prj_file <- file.path(tempdir(), 'gcam_db.dat')
    }

    if(!file.exists(prj_file)){

        message("Querying GCAM XML DB, this may take a moement.")
        conn <- localDBConn(db_dir, db_name)


        lapply( listScenariosInDB(conn)$name, function(name){
            gcam_data <- addScenario(conn = conn,
                                     proj = prj_file,
                                     scenario = name,
                                     queryFile = query_file)
            return(invisible())
        })

    }

    return(prj_file)

}





