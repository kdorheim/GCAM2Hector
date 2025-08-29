source("scripts/env.R")

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

# replicate a data frame for n number of scenarios
# Args
#   x: data frame containing information that needs to be replicated for some number of scenarios
#   scns: vector of the scenarios
# Returns: data frame x replicated n times with a scenario column
repeat_for_scns <- function(x, scns){

    stopifnot(!"scenario" %in% names(x))

    x %>%
        sapply(rep.int, times=length(scns))  %>%
        as.data.frame() %>%
        mutate(scenario = rep(scns, each = nrow(.)/length(scns))) %>%
        mutate(value = as.numeric(value), year = as.numeric(year)) ->
        out

    return(out)

}

# Helper function that reads in a hector CSV table into a nice format
# Args:
#   file: path the to a hector input emissions table
# Return: long data.frame of hector inputs (emissions, constraints, rf)
read_hector_csv <- function(file){

    wide_df <- read.csv(file, comment.char = ";")
    long_df <- tidyr::pivot_longer(wide_df, -Date)
    names(long_df) <- c("year", "variable", "value")
    long_df$units <- hector::getunits(long_df$variable)

    return(long_df)
}

# Get the historical emissions used to drive Hector before GCAM emissions
# are supplied to the Hector core.
# Args
#   file: path to the gcam emissions file, default is the data included in the auxiliary data
# Returns: data.frame of the emissions used to drive Hector's historical period
get_pregcam_emiss <- function(file = GCAM_EMISS_FILE){

    emiss <- read_hector_csv(file)

    # There should be no emissions greater than the transition date.
    stopifnot(all(emiss$year <= TRANSITION_DATE))

    # Make sure that all the required emissions are present!
    missing <- setdiff(emiss$variable, GCAM_EMISS)
    stopifnot(length(missing) == 0)

    return(emiss)
}

# Get the default emissions used regardless of GCAM scenario.
# Args
#   file: path to the default emissions file, default is the data included in the auxiliary data
# Returns: data.frame of the emissions used to drive Hector's historical period
get_default_emiss <- function(file = DEFAULT_EMISS_FILE){

    emiss <- read_hector_csv(file)

    # There should be emissions up until th year 2100
    stopifnot(max(emiss$year) >= 2100)

    # Make sure that all the required emissions are present!
    missing <- setdiff(emiss$variable, DEFAULT_EMISS)
    stopifnot(length(missing) == 0)

    return(emiss)
}

# Run all the queries on a GCAM xml db
# Args
#   db_dir: path to xml db
#   db_name: db name
#   query_file: path to xml of the hector queries to run
#   prj_file: path to the .dat file that contiains query results, if NULL the .dat file will be saved in a temp directory
run_all_queries <- function(db_dir, db_name, query_file, prj_file = NULL){

    stopifnot(dir.exists(file.path(db_dir, db_name)))
    if(is.null(prj_file)){
        prj_file <- file.path(tempdir(), 'gcam_db.dat')
    }

    if(!file.exists(prj_file)){

        message("Querying GCAM XML DB, this may take a moement.")
        conn <- localDBConn(db_dir, db_name)

        listScenariosInDB(conn)$name %>%
            lapply(function(name){
                gcam_data <- addScenario(conn = conn,
                                         proj = prj_file,
                                         scenario = name,
                                         queryFile = query_file)
                return(invisible())
            })

    }

    return(prj_file)
}

# Get comparison data from the GCAM xml db
# Args
#   prj_file: path to the .dat file of extracted GCAM xml db, created by run_all_queries
# Return: data.frame of gcam results
get_GCAM_hector_comparison_data <- function(prj_file){

    stopifnot(file.exists(prj_file))
    prjdata <- rgcam::loadProject(prj_file)

    queries <- c("CO2_concentration", "RF_aci", "RF_OC", "RF_H2O_strat",
                 "RF_O3_trop", "RF_BC", "RF_SO2", "RF_NH3", "RF_N2O", "FCH4",
                 "RF_CO2", "RF_tot", "gmst")

    lapply(X = queries, function(X){
        out <- getQuery(prjdata, query = X)
        out$variable <- X
        return(out)
    }) ->
        query_list

    out <- do.call(what = "rbind", args = query_list)
    names(out) <- tolower(names(out))
    out$source <- "gcam xmldb"

    out %>%
        filter(year > 1975) ->
        out


    return(out)

}

# Internal function used by add_missing_years which will interpolate
# missing emissions
# Args
#   df: data frame for a single variable
#   req_years: vector of the years needed in this df.
# Returns: data frame with no NAs with values for all the required years
internal_fxn_missing_yrs <- function(df, req_years){

    df %>%
        select(variable, units, scenario) %>%
        distinct ->
        meta_data

    miss_years <- setdiff(req_years, df$year)

    data.frame(year = miss_years,
               value = NA) %>%
        cbind(meta_data) ->
        missing_yrs_df

    df %>%
        dplyr::bind_rows(missing_yrs_df) %>%
        arrange(year) %>%
        mutate(value = na.approx(value)) ->
        complete_df

    return(complete_df)

}

# Use linear interpolation to get annual emissions
# Args
#   df: data frame of emissions from GCAM (only includes results every 5 years)
#   yrs: vector of the years needed in this df.
# Returns: data frame with no NAs with values for all the required years
add_missing_years <- function(df, yrs){

    # Make sure we are working with the right data
    req_cols <- c("year", "value", "variable", "units", "scenario")
    missing <- setdiff(req_cols, names(df))
    stopifnot(length(missing) == 0)


    # Use linear interpolation to fill in the
    split(df, interaction(df$variable, df$scenario)) %>%
        lapply(internal_fxn_missing_yrs, req_years = yrs) %>%
        do.call(what = "rbind") ->
        out

    return(out)

}


# Get the the non CO2 emissions for Hector from a GCAM output database
# Args
#   prjdata: path to the GCAM output file
#   gcam_emiss_file: path to the gcam emissions table
# Returns: data frame of the non CO2 emissions for Hector
internal_fxn_nonCO2_emissions <- function(prjdata, gcam_emiss_file = GCAM_EMISS_FILE){

    # The query to run
    queries <- c("nonCO2 emissions by region")

    # Extract the relevant query.
    lapply(X = queries, function(X){
        out <- getQuery(prjdata, query = X)
        out$variable <- X
        return(out)
    })  %>%
        do.call(what = "rbind") %>%
        # There should be no CO2 emissions in this query...
        # TODO need to check to see if there is a porblem with this query
        filter(!ghg %in% c("CO2", "CO2_FUG")) %>%
        # Map the GCAM emissions to Hector variable names and
        # convert to Hector units.
        left_join(EMISS_MAP, by = join_by(ghg)) %>%
        mutate(value = value * unit.conv) %>%
        dplyr::summarise(value = sum(value), .by = c("scenario", "hector.units", "scenario", "year", "hector.name")) %>%
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
        dplyr::bind_rows(pre_gcam_emiss) %>%
        # Make sure the CO2 variables are not included here.
        filter(!variable %in% c(FFI_EMISSIONS(), LUC_UPTAKE(), DACCS_UPTAKE(), LUC_EMISSIONS())) %>%
        add_missing_years(yrs = 1750:2100) ->
        out

    return(out)
}


# A helper function that transforms the possibly negative emissions to the
# emissions and uptake variables required by Hector.
# Args
#   df: long data frame of the luc and ffi emissions
# Returns: long data frame of the luc & ffi emissions and uptake, strictly positive.
internal_fxn_co2_processing <- function(df){
    # Make sure that the required columns are included in the df
    req_cols <- c("year", "value", "variable", "units", "scenario")
    missing  <- setdiff(req_cols, names(df))
    stopifnot(length(missing) == 0)

    # Make sure the required variables are here and no more.
    req_cols <- c(LUC_EMISSIONS(), FFI_EMISSIONS())
    missing  <- setdiff(unique(df$variable), req_cols)
    stopifnot(length(missing) == 0)

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


# Internal function that extracts the co2 emissions and prepares them
# from an GCAM XML DB.
# Args
#   prjdata: path to the GCAM output file
#   gcam_emiss_file: path to the gcam emissions table
# Returns: data frame of the CO2 emissions and uptake from FFI/LUC for Hector
internal_fxn_co2_emissions <- function(prjdata, gcam_emiss_file = GCAM_EMISS_FILE){

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
        left_join(EMISS_MAP, by = join_by(ghg)) %>%
        mutate(value = value * unit.conv) %>%
        dplyr::summarise(value = sum(value),
                         .by = c("scenario", "hector.units", "scenario", "year", "hector.name")) %>%
        select(scenario, year, value, variable = hector.name, units = hector.units) %>%
        # The future emissions start after the transition date.
        filter(year > TRANSITION_DATE) ->
        df


    # Use the FFI and LUC emissions from GCAM to determine
    # co2 emissions and uptake.
    internal_fxn_co2_processing(df = df) ->
        incomplete_future_emiss


    # Get the historical emissions aka the emissions Hector should
    # use before switching over to the GCAM emissions.
    get_pregcam_emiss(file = gcam_emiss_file) %>%
        filter(year <= TRANSITION_DATE) %>%
        filter(variable %in% c(LUC_EMISSIONS(), LUC_UPTAKE(),
                               FFI_EMISSIONS(), DACCS_UPTAKE())) %>%
        repeat_for_scns(scns = unique(incomplete_future_emiss$scenario)) ->
        pre_gcam_emiss

    # Combine the pre
    pre_gcam_emiss %>%
        rbind(incomplete_future_emiss) %>%
        split(., interaction(.$variable, .$scenario)) %>%
        lapply(internal_fxn_missing_yrs, req_years = 1750:2100) %>%
        do.call(what = "rbind") ->
        out

    return(out)
}


# Extract and format emissions for Hector from a GCAM data base.
# Args
#   db_dir: str directory where the GCAM database to be processed lives
#   db_name: str name of the GCAM XML db
#   query_file: str to the query file to run, default is set to internal data
#   prj_file: str name where to save the rgcam project data at, if NULL will save to a temporary location
#   gcam_emiss_file: str path to the hector gcam emissions csv table, default is set to internal data
#   gcam_default_file: str path to the hector default emissions csv table, default is set to internal data
# Returns: data frame of Hector inputs
get_hector_emiss <- function(db_dir, db_name,
                              query_file = QUERY_FILE,
                              prj_file = NULL,
                              gcam_emiss_file = GCAM_EMISS_FILE,
                              gcam_default_file = DEFAULT_EMISS_FILE){

    # Run all the queries and save as an rgcam data object. If there
    # is already a .dat file that exists load the existing one...
    prj_file <- run_all_queries(db_dir = db_dir,
                                db_name = db_name,
                                query_file = query_file,
                                prj_file = prj_file)

    message(paste0("GCAM data set saved at: ", prj_file))


    # Load the project file
    prjdata <- rgcam::loadProject(prj_file)

    # Get emissions from the GCAM XML output database.
    nonCO2_emiss <- internal_fxn_nonCO2_emissions(prjdata, gcam_emiss_file)
    CO2_emiss    <- internal_fxn_co2_emissions(prjdata, gcam_emiss_file)

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
        mutate(source = "GCAM-hector") ->
        out

    return(out)

}


# Complete a single Hector run using the input data frame returned by get_hector_emiss
# Args
#   hc: active hector core (should be configured with the same ini file used in the gcam run)
#   inputs: data frame of Hector emissions returned by get_hector_emiss
# Returns: data frame of Hector results
run_single_scn <- function(hc, inputs){

    # Make sure that there is only input scenario.
    scn <- unique(inputs$scenario)
    stopifnot(length(scn) == 1)

    # Set the input variables for the Hector run.
    split(inputs, inputs$variable) %>%
        sapply(function(df){

            var <- unique(df$variable)
            units <- getunits(var)
            setvar(hc, dates = df$year, values = df$value, var = var, unit = units)
            reset(hc)
        })

    run(hc, runtodate = 2100)

    VARS <- c("CO2_concentration", "RF_aci", "RF_OC", "RF_H2O_strat",
              "RF_O3_trop", "RF_BC", "RF_SO2", "RF_NH3", "RF_N2O", "FCH4",
              "RF_CO2", "RF_tot", "gmst")

    fetchvars(hc, dates = 1750:2100, vars = VARS) %>%
        mutate(scenario = scn,
               source = "hector") ->
        out

    return(out)
}


# Run stand alone Hector with emisisons for all the scenarios included in
# the GCAM xml data base.
# Args
#   hc: active hector core (should be configured with the same ini file used in the gcam run)
#   db_dir: str directory where the GCAM database to be processed lives
#   db_name: str name of the GCAM XML db
#   prj_file: str name where to save the rgcam project data at, if NULL will save to a temporary location
#   gcam_emiss_file: str path to the hector gcam emissions csv table, default is set to internal data
#   gcam_default_file: str path to the hector default emissions csv table, default is set to internal data
# Returns: data frame of Hector results
run_gcamHector <- function(hc, db_dir,
                           db_name,
                           prj_file = NULL,
                           query_file = QUERY_FILE,
                           gcam_emiss_file = GCAM_EMISS_FILE,
                           gcam_default_file = DEFAULT_EMISS_FILE){

    # Check to make sure that the hc is active
    stopifnot(class(hc)[1] == "hcore")
    stopifnot(isactive(hc))

    # Prep the emissions for Hector
    inputs <- get_hector_emiss(db_dir,
                               db_name,
                               query_file,
                               prj_file = prj_file)

    # Feed the inputs into Hector and get results.
    split(inputs, inputs$scenario) %>%
        sapply(run_single_scn, hc = hc, simplify = FALSE, USE.NAMES = FALSE) %>%
        do.call(what = "rbind") ->
        hector_out

    rownames(hector_out) <- NULL

    return(hector_out)

}
