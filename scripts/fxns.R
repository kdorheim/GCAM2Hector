
# data.table implementation of the gcamdata repeat_add_columns
# Args
#   x: data.table to add to
#   y: data.table containing the column that should be repeated and added to dt x
# return: data.table
repeat_add_columns <- function(x, y){

    assert_that(is.data.table(x))
    assert_that(is.data.table(y))
    assert_that(!any(names(x) %in% names(y)))
    assert_that(!any(names(y) %in% names(x)))

    x$join <- 1
    y$join <- 1

    df <- merge(x, y, all = TRUE, by = .EACHI, allow.cartesian=TRUE)
    df$join <- NULL
    return(df)

}


# Transform a rgcam project file into a data frame with emissions in Hector appropriate units and names.
# Args
#   dat_file: file path to the .dat file created by rgcam
# Return: Hector input emissions for all the GCAM generated emission species for all the scenarios
# listed in the dat_file. The returned data frame is set up to be used with some sort of hector::setvars
# function
get_hector_emissions <- function(dat_file){

    # Load the GCAM project.
    assert_that(file.exists(dat_file))
    gcam_rslts <- loadProject(dat_file)

    # Get the GCAM emission results for the non CO2 emissions.
    names(gcam_rslts) %>%
        lapply(function(x){
            gcam_rslts[[x]]$`nonCO2 emissions by region`
        }) %>%
        rbindlist ->
        nonCO2_emissions

    # Get the FFI CO2 emissions
    names(gcam_rslts) %>%
        lapply(function(x){
            gcam_rslts[[x]]$`CO2 emissions by region`
        }) %>%
        rbindlist %>%
        # TODO need to check to see if this is the correct
        # way to handle this!
        mutate(ghg = if_else(value < 0, "daccs_uptake", ghg)) %>%
        mutate(value = abs(value)) ->
        ffi_emissions


    # Since the LUC emissions are treated differently extract them here.
    names(gcam_rslts) %>%
        lapply(function(x){

            gcam_rslts[[x]]$LUC_emissions %>%
                mutate(ghg = "luc_emissions")

        }) %>%
        rbindlist %>%
        # If there are any negative values change them to LUC Uptake.
        mutate(ghg = if_else(value < 0, "luc_uptake", ghg)) %>%
        mutate(value = abs(value)) ->
        luc_emissions

    # Combine all of the emissions into a single data frame!
    gcam_df <- bind_rows(nonCO2_emissions, ffi_emissions, luc_emissions)

    # Add the mapping information to the data frame
    gcam_emissions_map <- emissions_map[gcam_df, on = "ghg"]

    # Check to make sure that the un-mapped emissions are the ones we are expecting.
    no_matches <- unique(gcam_emissions_map[is.na(gcam_emissions_map$agg.gas), ]$ghg)
    expected_emissions <- c("H2", "H2_AWB", "PM10", "PM2.5", "CO2_FUG")
    assert_that(all(no_matches %in% expected_emissions), msg = "unexpected emission not being passed to Hector")

    # First convert from GCAM units to Hector units, this is important for some of the halocarbons, multiple GCAM
    # halocarbons may be aggregated into one Hector halocarbon category.
    gcam_emissions_map$converted_value <- gcam_emissions_map[ , list(value * unit.conv)]
    gcam_inputs_for_hector <- gcam_emissions_map[ , list(value = sum(converted_value)), by = c("agg.gas", "hector.name", "scenario", "year",  "hector.units")]

    # Drop the exepcted NAs
    d <- na.omit(gcam_inputs_for_hector)

    # error checking
    req_cols <- c("year", "value")
    assert_that(all(req_cols %in% names(d)))


    # The expected years of data we want are from 2005 until 2100, before the year
    # 2005 Hector is using the GCAM inputs
    expected_years <- data.table(year = 2005:2100)

    # Construct a df of all the variables for all the 2005 until 2100. This will
    # create a df with NA values when no GCAM emissions are available that will be
    # fill in the next step.
    save_cols <- names(d)[!names(d) %in% c("year", "value")]
    to_replicate <- distinct(d[, ..save_cols])
    df_with_all_yrs <- repeat_add_columns(x = to_replicate, y = expected_years)
    df_NA <- d[df_with_all_yrs,  on=names(df_with_all_yrs), nomatch = NA]

    # Replace the NA emissions with linearly interpolated values.
    split(x = df_NA,
          f = interaction(df_NA$hector.name, df_NA$scenario, df_NA$hector.units, drop = TRUE)) %>%
        lapply(function(X){
            new_vals <- na.approx(object = X$value, x = X$year)
            X$value <- new_vals
            return(X)
        }) %>%
        rbindlist ->
        complete_hetor_emissions


    # Finally format the data
    out <- complete_hetor_emissions[, .(scenario,variable = hector.name, year, value, units = hector.units)]




    # Make sure that there are no missing emissions! Otherwise throw an error here!
    req_names <- c("ffi_emissions", "luc_emissions", "daccs_uptake", "luc_uptake", "BC_emissions",
                   "C2F6_emissions", "CF4_emissions", "CH4_emissions", "CO_emissions", "HFC125_emissions",
                   "HFC134a_emissions", "HFC143a_emissions", "HFC227ea_emissions", "HFC23_emissions",
                   "HFC245fa_emissions", "HFC32_emissions", "N2O_emissions", "NH3_emissions",
                   "NMVOC_emissions", "NOX_emissions", "OC_emissions", "SF6_emissions", "SO2_emissions")
    missing <- setdiff(req_names, out$variable)
    stopifnot(length(missing) == 0)

    return(out)
}




use_gcam_emissions <- function(ini_path,
                               emissions_df,
                               out_yrs = 1850:2100,
                               out_vars = c(GLOBAL_TAS(), RF_TOTAL(), CONCENTRATIONS_CO2())){

    # There should only be one scenario per emissions data frame.
    scn <- unique(emissions_df$scenario)
    assert_that(length(scn) == 1)
    stopifnot(file.exists(ini_path))

    # Set up the Hector core
    hc <- newcore(ini_path, name = scn)


    # Pass all of the emissions to the Hector core.
    split(emissions_df, emissions_df$variable) %>%
        lapply(function(d){

            setvar(core = hc,
                   dates = d$year,
                   var = d$variable,
                   values = d$value,
                   unit = d$units)
            reset(hc)

        })

    run(hc, runtodate = 2100)
    out <- fetchvars(core = hc, dates = out_yrs, vars = out_vars)
    return(out)

}
