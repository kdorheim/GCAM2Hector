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

    # Make sure to exclude the CO2 emissions!
    getQuery(prjdata, query = "emissions by region") %>%
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


#' Get the the CO2 emissions from an GCAM output database
#'
#' @param prjdata path to the GCAM project file
#' @param name path to the gcam emissions table otherwise will use internal data
#' @return data.frame of the CO2 emissions that can be used as inputs to hector
#' @import dplyr
#' @import hector
#' @noRd
get_CO2_emiss <- function(prjdata, gcam_emiss_file = NULL){

    # Use the total antro emissions and the negative emissions to determine
    # FFI emissions and DACCS uptake for hector.
    getQuery(prjdata, query = "emissions by region") %>%
        filter(ghg %in% c("CO2")) %>%
        summarise(total = sum(value), .by = c(Units, scenario, region, year)) ->
        co2.tot

    getQuery(prjdata, "CO2_negative") %>%
        rename(negative = value) ->
        co2.neg

    co2.tot %>%
        left_join(co2.neg, by = join_by(Units, scenario, region, year)) %>%
        mutate(positive = total - negative, negative = -negative) %>%
        select(Units, scenario, region, year, ffi_emissions = positive, daccs_uptake = negative) %>%
        gather(ghg, value, ffi_emissions, daccs_uptake) %>%
        select(-Units) %>%
        summarize(value = sum(value), .by = c(scenario, year, ghg)) ->
        co2.partition

    # Extract all the LUC emissions and uptake.
    getQuery(prjdata, "LUC_above_total") %>%
        summarise(value = sum(value), .by = c(scenario, year)) %>%
        mutate(ghg = "above.total") ->
        above.tot

    getQuery(prjdata, "LUC_above_post") %>%
        summarise(value = sum(value), .by = c(scenario, year)) %>%
        mutate(ghg = "above.positive") ->
        above.positive

    getQuery(prjdata, "LUC_below_total") %>%
        summarise(value = sum(value), .by = c(scenario, year)) %>%
        mutate(ghg = "below.total") ->
        below.total

    bind_rows(above.tot, above.positive, below.total) %>%
        spread(ghg, value) %>%
        mutate(above.negative = above.total - above.positive,
               below.positive = if_else(below.total > 0, below.total, 0),
               below.negative = if_else(below.total < 0, below.total, 0),
               luc_emissions = above.positive + below.positive,
               luc_uptake = -(above.negative + below.negative)) %>%
        select(scenario, year, luc_emissions, luc_uptake) %>%
        gather(ghg, value, luc_emissions, luc_uptake) %>%
        summarize(value = sum(value), .by = c(scenario, year, ghg)) ->
        luc_partition

    co2.partition %>%
        bind_rows(luc_partition) %>%
        filter(year > TRANSITION_DATE) ->
        gcam_co2_emiss

    gcam_co2_emiss %>%
        inner_join(EMISS_MAP_DF, by = join_by(ghg),
                   relationship = "many-to-many") %>%
        mutate(value = value * unit.conv) %>%
        select(scenario, year, value, variable = hector.name, units = hector.units) ->
        incomplete_future_emiss

    # Get the historical emissions aka the emissions Hector should
    # use before switching over to the GCAM emissions.
    get_pregcam_emiss(file = gcam_emiss_file) %>%
        filter(year <= TRANSITION_DATE) %>%
        filter(variable %in% incomplete_future_emiss$variable) %>%
        repeat_for_scns(scns = unique(incomplete_future_emiss$scenario)) ->
        pre_gcam_emiss

    # Combine the historical emissions with the GCAM emissions,
    # fill in the missing years.
    pre_gcam_emiss %>%
        bind_rows(incomplete_future_emiss) %>%
        distinct() %>%
        add_missing_yrs(req_years = 1750:2100) ->
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


