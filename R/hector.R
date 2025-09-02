#' Complete a single Hector run using the input data frame returned by \code{\link{get_hector_inputs}}
#'
#' @param hc active hector core, it should be configured with the same ini file used in the gcam run.
#' @param inputs data frame of Hector emissions returned by \code{\link{get_hector_inputs}}.
#' @returns data frame of Hector results.
#' @noRd
internal.single_hector_run <- function(hc, inputs, vars = NULL){

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


    if(is.null(vars)){
        vars <- c(CONCENTRATIONS_CO2(), RF_ACI(), RF_OC(), RF_H2O_STRAT(),
                  RF_O3_TROP(), RF_BC(), RF_SO2(), RF_NH3(), RF_N2O(), RF_CH4(),
                  RF_CO2(), RF_TOTAL(), GMST())
    }

    fetchvars(hc, dates = 1750:2100, vars = vars) %>%
        mutate(scenario = scn) ->
        out

    return(out)

}

# TODO there should be a better way to handle the db arguments, and the emiss files
#' Run stand alone Hector with emissions for all the scenarios included in a GCAM db
#'
#' @param db_dir str directory where the GCAM database to be processed lives
#' @param db_name str name of the GCAM XML db
#' @param ini path to the ini file to use, it should be the same one used by the GCAM run
#' @param prj_file str name where to save the rgcam project data at, if NULL will save to a temporary location
#' @param gcam_emiss_file str path to the hector gcam emissions csv table, default is set to internal data
#' @param gcam_default_file str path to the hector default emissions csv table, default is set to internal data
#' @param vars vector of the hector variable names
#' @returns data frame of Hector results.
#' @export
run_GCAM2hector <- function(db_dir, db_name,
                            prj_file = NULL,
                            ini = NULL,
                            query_file = NULL,
                            gcam_emiss_file = NULL,
                            gcam_default_file = NULL,
                            vars = NULL){

    if(is.null(ini)){
        ini <-  system.file("extdata", "hector-gcam.ini", package = "GCAM2Hector")
    }

    hc <- newcore(ini)

    # Prep the emissions for Hector
    inputs <- get_hector_inputs(db_dir,
                                db_name,
                                query_file,
                                prj_file = prj_file)

    # Feed the inputs into Hector and get results.
    split(inputs, inputs$scenario) %>%
        sapply(internal.single_hector_run,
               hc = hc,
               vars = vars,
               simplify = FALSE, USE.NAMES = FALSE) %>%
        do.call(what = "rbind") ->
        hector_out

    rownames(hector_out) <- NULL

    return(hector_out)

}

#' Run stand alone Hector with emissions for all the scenarios included in a GCAM db
#'
#' @param df long data frame of inputs for hector.
#' @param outdir location where to write the hector input csv file to.
#' @returns path to the hector input data file.
#' @noRd
internal.write_hector_csv <- function(df, outdir){

    # Check the inputs
    req_check(names(df), c("scenario", "year", "value", "variable", "units"))
    req_check(df$variable, req = c(GCAM_EMISS, DEFAULT_EMISS))

    # Get
    scn <- unique(df$scenario)
    db <- unique(df$db)

    f_name <- paste0(scn, "_inputs.csv")
    f_path <- file.path(outdir, f_name)

    # Transform the data frame into the wide format that Hector expects.
    df %>%
        select(Date = year, variable, value) %>%
        tidyr::pivot_wider(names_from = "variable") ->
        wide_df

    # Save the header information...
    suppressWarnings({var_units <- getunits(names(wide_df))})
    var_units[1] <- "# UNITS"
    units_list  <- paste(var_units, collapse = ', ')
    create_info <- paste0('# created by GCAM2hector using ', db)
    scn <- paste0('# ', scn)
    date <-  paste0("# date ", date())

    writeLines(paste(scn, date, create_info, units_list, sep = "\n"), f_path)
    write.table(wide_df, f_path, sep = ",", append = TRUE, col.names = TRUE, row.names = FALSE)

    #
    # # Save intermediate results
    # write.csv(wide_df, f_path, row.names = FALSE)
    # lines <- readLines(f_path)
    #
    # # Add the meta data to hector input table.
    # suppressWarnings({var_units <- getunits(names(wide_df))})
    # var_units[1] <- "# UNITS"
    # units_list  <- paste(var_units, collapse = ', ')
    # create_info <- paste0('# created by GCAM2hector using ', db)
    # final_lines <- append(c(paste0('# ', scn),
    #                         create_info,
    #                         paste0("# date ", date()),
    #                         units_list),
    #                       lines)
    #
    # writeLines(final_lines, f_path)
    return(f_path)

}


#' Write out a single gcam hector ini file
#'
#' @param csv_file path of the hector input csv file.
#' @param ini_path path to the ini gcam template to use, it should be the ini used in the gcam run.
#' @returns path to the ini file is written out to.
#' @noRd
internal.write_ini <- function(csv_file, ini_path=NULL){

    if(is.null(ini_path)){
        ini_path <-  system.file("extdata", "hector-gcam.ini", package = "GCAM2Hector")
    }

    name <- gsub(x = basename(csv_file), pattern = "_inputs.csv", replacement = "")

    ini_lines <- readLines(ini_path)
    ini_lines[1] <- paste0("; Configuration file for hector model: ",  name, ", GCAM2Hector ", date())

    input_name <- paste0("=csv:./", basename(csv_file), "     ;")
    new_ini_lines <- gsub(pattern = "=csv:.*\\.csv", x = ini_lines, replacement = input_name)

    ofile <- file.path(dirname(csv_file), paste0(name, "-gcam.ini"))
    writeLines(new_ini_lines, ofile)
    return(ofile)

}

# TODO there is a problem with how the csv file is being written out it can't
# seem to be read into ini also need to add the testing element.
#' Write out a single gcam hector ini file
#'
#' @param db_dir str directory where the GCAM database to be processed lives
#' @param db_name str name of the GCAM XML db
#' @param outdir path to where to write the hector input files to
#' @param ini path to the ini file to use, it should be the same one used by the GCAM run
#' @param prj_file str name where to save the rgcam project data at, if NULL will save to a temporary location
#' @param gcam_emiss_file str path to the hector gcam emissions csv table, default is set to internal data
#' @param gcam_default_file str path to the hector default emissions csv table, default is set to internal data
#' @returns path to the ini file.
#' @export
write_GCAM2hector <- function(db_dir, db_name, outdir,
                              prj_file = NULL,
                              ini = NULL,
                              query_file = NULL,
                              gcam_emiss_file = NULL,
                              gcam_default_file = NULL){

    if(is.null(ini)){
        ini <-  system.file("extdata", "hector-gcam.ini", package = "GCAM2Hector")
    }

    # Prep the emissions for Hector
    inputs <- get_hector_inputs(db_dir,
                                db_name,
                                query_file,
                                prj_file = prj_file) %>%
        filter(year <= 2100)

    # For each scenario in the input data frame write
    # the csv and ini files.
    split(inputs, inputs$scenario) %>%
        sapply(function(x){

            csv_table <- internal.write_hector_csv(df = x, outdir)
            out <- internal.write_ini(csv_table, ini)
            return(out)

        }, simplify = TRUE, USE.NAMES = FALSE) ->
        out

    names(out) <- NULL

    warning("Note there may be an issue with the ini file open in excel")
    return(out)

}





