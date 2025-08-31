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

# TODO delte this but this may be helpful during dev and testing...
# write.csv(df, "neg_co2_emiss.csv", row.names = FALSE)
# df <- read.csv("neg_co2_emiss.csv")


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



