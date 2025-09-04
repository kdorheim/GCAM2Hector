

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
#' @export
get_all_queries <- function(db_dir,
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



#' Get hector variables from the GCAM results
#'
#' Get hector variables from the GCAM results, to be used in a comparison
#' between stand alone hector and the GCAM-hector results.
#'
#' @param prj_file path to the directory that contains the GCAM xml db created by \code{\link{get_all_queries}}.
#' @param vars hector variables to save, if NULL will return the CO2 concentrations, gmst, and a handful of radiative forcing results.
#' @returns data frame of the GCAM-hector results
#' @export
fetch_GCAM_vs_hector <- function(prj_file, vars = NULL){

    stopifnot(file.exists(prj_file))
    prjdata <- rgcam::loadProject(prj_file)

    queries <- c("CO2_concentration", "RF_aci", "RF_OC", "RF_H2O_strat",
                 "RF_O3_trop", "RF_BC", "RF_SO2", "RF_NH3", "RF_N2O", RF_CH4(),
                 "RF_CO2", "RF_tot", "gmst", CONCENTRATIONS_CH4(), CONCENTRATIONS_N2O())
    if(!is.null(vars)){
        # Make sure the vars selected are compatible with the supported queries.
        req_check(queries, vars)
        queries <- vars
    }

    lapply(X = queries, function(X){
        out <- getQuery(prjdata, query = X)
        out$variable <- X
        return(out)
    }) ->
        query_list

    out <- do.call(what = "rbind", args = query_list)
    names(out) <- tolower(names(out))
    out$source <- "gcam xmldb"

    # Exclude the 1975 value as per guidance from P.Patel
    out <- out[out$year > 1975, ]

    return(out)



}



