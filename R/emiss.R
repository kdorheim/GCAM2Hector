# Get the historical emissions used to drive Hector before GCAM emissions
# are supplied to the Hector core.
# Args
#   file: path to the gcam emissions file, default is the data included in the auxiliary data
# Returns: data.frame of the emissions used to drive Hector's historical period


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
