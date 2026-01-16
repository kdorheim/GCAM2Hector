#' Get the historical emissions used to drive Hector before GCAM emissions
#'
#' Get the historical emissions used to drive Hector before GCAM emissions
#' are supplied to the Hector core.
#'
#'
#' @param file path to the gcam emissions file, default is the data included in the auxiliary data
#' @noRd
get_pregcam_emiss <- function(file = NULL){

    if(is.null(file)){
        return(PREGCAM_EMISS_DF)
    }

    emiss <- read_hector_csv(file)

    # There should be no emissions greater than the transition date.
    stopifnot(all(emiss$year <= TRANSITION_DATE))

    # Make sure that all the required emissions are present!
    missing <- setdiff(emiss$variable, GCAM_EMISS)
    stopifnot(length(missing) == 0)

    return(emiss)
}

#' Get the default emissions used regardless of GCAM scenario
#'
#' @param file path to the gcam emissions file, if set to NULL it will return internal data
#' @return data.frame of the emissions used to drive Hector's historical period
#' @noRd
get_default_emiss <- function(file = NULL){

    if(is.null(file)){
        return(DEFAULT_EMISS_DF)
    }

    emiss <- read_hector_csv(file)

    # There should be emissions up until th year 2100
    stopifnot(max(emiss$year) >= 2100)

    # Make sure that all the required emissions are present!
    missing <- setdiff(emiss$variable, DEFAULT_EMISS)
    stopifnot(length(missing) == 0)

    return(emiss)
}


#' Get the the non CO2 emissions from an GCAM output database
#'
#' @param prjdata path to the GCAM project file
#' @param name path to the gcam emissions table otherwise will use internal data
#' @return data.frame of the non CO2 emissions that can be used as inputs to hector
#' @import dplyr
#' @import hector
#' @noRd
get_nonCO2_emiss <- function(prjdata, gcam_emiss_file = NULL){

    # Extract the relevant query.
    lapply(X = c("nonCO2 emissions by region"), function(X){
        out <- getQuery(prjdata, query = X)
        out$variable <- X
        return(out)
    })  %>%
        bind_rows %>%
        # There should be no CO2 emissions in this query, if there are
        # this remove them (need to make some changes to the xml query).
        filter(!ghg %in% c("CO2", "CO2_FUG")) %>%
        # Map the GCAM emissions to Hector variable names and
        # convert to Hector units.
        left_join(EMISS_MAP_DF, by = join_by(ghg)) %>%
        mutate(value = value * unit.conv) %>%
        summarise(value = sum(value), .by = c("scenario", "hector.units", "scenario", "year", "hector.name")) %>%
        select(scenario, year, value, variable = hector.name, units = hector.units) %>%
        # The future emissions start after the transition date.
        filter(year > TRANSITION_DATE) ->
        incomplete_future_emiss


    # Get the historical emissions aka the emissions Hector should
    # use before switching over to the GCAM emissions. This should be
    # the same for all the scenarios.
    get_pregcam_emiss(file = gcam_emiss_file) %>%
        filter(year <= TRANSITION_DATE) %>%
        repeat_for_scns(scns = unique(incomplete_future_emiss$scenario)) %>%
        as.data.frame ->
        pre_gcam_emiss

    # Complete the emissions
    incomplete_future_emiss %>%
        bind_rows(pre_gcam_emiss) %>%
        # Make sure the CO2 variables are not included here.
        filter(!variable %in% c(FFI_EMISSIONS(), LUC_UPTAKE(), DACCS_UPTAKE(), LUC_EMISSIONS())) %>%
        na.omit %>%
        add_missing_yrs(req_years = 1750:2100) ->
        out

    return(out)
}


#' Change net CO2 emissions to strictly positive emissions and uptake
#'
#' This helper functions takes the CO2 emissions from FFI and LUC and
#' changes them to the respective strictly positive values.
#'
#' @param df long data frame of the luc and ffi emissions
#' @returns long data frame of the luc & ffi emissions and uptake, strictly positive.
#' @import dplyr
#' @importFrom tidyr pivot_longer
#' @importFrom tidyr pivot_wider
#' @noRd
handle_neg_CO2_emiss <- function(df){

    req_check(names(df),  c("year", "value", "variable", "units", "scenario"))
    req_check(df$variable,  c(LUC_EMISSIONS(), FFI_EMISSIONS()))

    df %>%
        tidyr::pivot_wider(names_from = variable, values_from = value) %>%
        # Move the "negative emissions" to the uptake variables.
        mutate(daccs_uptake = ifelse(ffi_emissions <= 0, -1 * ffi_emissions, 0),
               luc_uptake = ifelse(luc_emissions <= 0, -1 * luc_emissions, 0)) %>%
        # Now replace the "negative emissions" with the 0 value.
        mutate(ffi_emissions = ifelse(ffi_emissions <= 0, 0, ffi_emissions),
               luc_emissions = ifelse(luc_emissions <= 0, 0, luc_emissions)) %>%
        tidyr::pivot_longer(cols = 4:7, names_to = "variable", values_to = "value") ->
        out

    return(out)


}

#' Get the the CO2 emissions from an GCAM output database
#'
#' @param prjdata path to the GCAM project file
#' @param name path to the gcam emissions table otherwise will use internal data
#' @return data.frame of the CO2 emissions that can be used as inputs to hector
#' @import dplyr
#' @import hector
#' @noRd
get_CO2_emiss <- function(prjdata, gcam_emiss_file = NULL){

    # Extract the two CO2 queries!
    query <- "CO2 emissions by region"
    ffi   <- getQuery(prjdata, query = query)


    # Get the fug CO2 emissions
    # TODO this should be addressed at the query level
    getQuery(prjdata, query = "nonCO2 emissions by region") %>%
        filter(ghg == "CO2_FUG") %>%
        mutate(ghg = "CO2") %>%
        bind_rows(ffi) %>%
        summarise(value = sum(value), .by = c("Units", "scenario", "ghg", "year")) ->
        ffi

    query <- "luc_emissions"
    luc   <- getQuery(prjdata, query = query)
    luc$variable <- query
    luc$ghg <- query

    # Map the GCAM emissions to Hector variable names and
    # convert to Hector units.
    ffi %>%
        bind_rows(luc) %>%
        left_join(EMISS_MAP_DF, by = join_by(ghg)) %>%
        mutate(value = value * unit.conv) %>%
        dplyr::summarise(value = sum(value),
                         .by = c("scenario", "hector.units", "scenario", "year", "hector.name")) %>%
        select(scenario, year, value, variable = hector.name, units = hector.units) %>%
        # The future emissions start after the transition date.
        filter(year > TRANSITION_DATE) ->
        df

    # Use the FFI and LUC emissions from GCAM to determine
    # co2 emissions and uptake.
    incomplete_future_emiss <- handle_neg_CO2_emiss(df = df)

    # Get the historical emissions aka the emissions Hector should
    # use before switching over to the GCAM emissions.
    get_pregcam_emiss(file = gcam_emiss_file) %>%
        filter(year <= TRANSITION_DATE) %>%
        filter(variable %in% c(LUC_EMISSIONS(), LUC_UPTAKE(),
                               FFI_EMISSIONS(), DACCS_UPTAKE())) %>%
        repeat_for_scns(scns = unique(incomplete_future_emiss$scenario)) ->
        pre_gcam_emiss


    # Combine the historical emissions with the GCAM emissions,
    # fill in the missing years.
    pre_gcam_emiss %>%
        rbind(incomplete_future_emiss) %>%
        add_missing_yrs(req_years = 1750:2100)->
        out

    return(out)

}



#' Prepare hector inputs from a GCAM xml output db
#'
#' @param db_dir str directory where the GCAM database to be processed lives
#' @param db_name str name of the GCAM XML db
#' @param query_file str to the query file to run, default is set to internal data
#' @param prj_file str name where to save the rgcam project data at, if NULL will save to a temporary location
#' @param gcam_emiss_file str path to the hector gcam emissions csv table, default is set to internal data
#' @param gcam_default_file str path to the hector default emissions csv table, default is set to internal data
#' @returns data frame of Hector inputs
#' @import dplyr
#' @import hector
#' @export
#' @examples
#' \donotrun{
#'
#' # Get the inputs for Hector using GCAM emissions during the
#' future period.
#' inputs <- get_hector_inputs(db_dir = "gcam_output",
#'                            db_name = "database_basexdb",
#'                            prj_file = "gcam_db.dat")
#' head(inputs)
#'
#' # The data frame returned can be used be used in
#' }
get_hector_inputs <- function(db_dir, db_name,
                              query_file = NULL,
                              prj_file = NULL,
                              gcam_emiss_file = NULL,
                              gcam_default_file = NULL){


    if(is.null(query_file)){
        query_file <- system.file("extdata", "hector-queries.xml", package = "GCAM2Hector")
    }

    # Run all the queries and save as an rgcam data object. If there
    # is already a .dat file that exists load the existing one...
    prj_file <- get_all_queries(db_dir = db_dir,
                                db_name = db_name,
                                query_file = query_file,
                                prj_file = prj_file)

    #message(paste0("GCAM data set saved at: ", prj_file))

    # Load the project file
    prjdata <- rgcam::loadProject(prj_file)


    # Get emissions from the GCAM XML output database.
    nonCO2_emiss <- get_nonCO2_emiss(prjdata, gcam_emiss_file)
    CO2_emiss    <- get_CO2_emiss(prjdata, gcam_emiss_file)


    # Get the default emissions/RF inputs associated
    # with the GCAM run.
    get_default_emiss(gcam_default_file) %>%
        repeat_for_scns(scns = unique(CO2_emiss$scenario)) ->
        default_emiss

    # Return the output!
    dplyr::bind_rows(default_emiss,
                     nonCO2_emiss,
                     CO2_emiss) %>%
        data.frame(row.names = NULL) %>%
        mutate(source = "GCAM-hector",
               db = db_name) ->
        out

    return(out)
}


