
source("scripts/env.R")
source("scripts/fxns.R")

db_dir <- here::here("gcam_output/")
db_name <- "database_basexdb"
query_file <- here::here("auxiliary_data", "hector-queries.xml")

gcam_emiss_file = GCAM_EMISS_FILE
gcam_default_file = DEFAULT_EMISS_FILE
prj_file = "proj_data.dat"




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


run_gcamHector <- function(hc, db_dir, db_name, prj_file,
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
                               prj_file = "proj_data.dat")

    # Feed the inputs into Hector and get results.
    split(inputs, inputs$scenario) %>%
        sapply(run_single_scn, hc = hc, simplify = FALSE, USE.NAMES = FALSE) %>%
        do.call(what = "rbind") ->
        hector_out

    rownames(hector_out) <- NULL

    return(hector_out)

}
# ------------------------------------------------------------------------------

prjdata <- loadProject(prj_file)

# Prep the emissions for Hector
inputs <- get_hector_emiss(db_dir,
                           db_name,
                           query_file,
                           prj_file = "proj_data.dat")

inputs %>%
    filter(variable == LUC_EMISSIONS()) %>%
    ggplot(aes(year, value)) +
    geom_line() +
    facet_wrap("variable", scales = "free")


# ------------------------------------------------------------------------------
prj_file <- "proj_data.dat"
gcam_emiss_file = GCAM_EMISS_FILE
gcam_default_file = DEFAULT_EMISS_FILE

ini <- "auxiliary_data/hector-gcam.ini"
hc <- newcore(ini)





comp_data  <- get_GCAM_hector_comparison_data(prj_file)
hector_out <- run_gcamHector(hc, db_dir, db_name, prj_file)



bind_rows(comp_data, hector_out) %>%
    select(-units) %>%
    tidyr::pivot_wider(names_from = source, values_from = value) %>%
    na.omit %>%
    mutate(SE = (`gcam xmldb` - hector)^2) %>%
    summarise(MSE = mean(SE), .by = c("scenario", "variable")) %>%
    arrange(desc(MSE))


bind_rows(comp_data, hector_out) %>%
    filter(year >= min(comp_data$year)) %>%
    filter(variable == GMST()) %>%
    ggplot(aes(year, value, color = source, linetype = source)) +
    geom_line(linewidth = 1)








