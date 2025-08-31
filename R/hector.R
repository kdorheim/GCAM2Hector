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

