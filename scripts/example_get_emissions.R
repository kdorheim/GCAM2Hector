# Example extract emissions from a GCAM xml db and use with stand alone hector.

# 0. Set Up --------------------------------------------------------------------
source("scripts/env.R")
source("scripts/fxns.R")


library(ggplot2)
theme_set(theme_bw())

# The mapping file used to convert GCAM emission units to Hector units.
emissions_map <- as.data.table(read.csv("auxiliary_data/GCAM_hector_emissions_map.csv"))
queryFile <- "auxiliary_data/hector-queries.xml"


# TODO users will need to change this.
# Create the connection to the database of interest.
conn      <- localDBConn("gcam_output", "database_basexdb")
proj_path <- 'gcam_db.dat'

# The variables to take a look at!
vars <- c(CONCENTRATIONS_CH4(), RF_CH4(), GLOBAL_TAS(), RF_TOTAL(), RF_N2O())
vars <- c(RF_OC())

# 1. Main Chunk ----------------------------------------------------------------

# Extract results from all the scenarios in the xml db.
listScenariosInDB(conn)$name %>%
    lapply(function(name){
        gcam_data <- addScenario(conn = conn,
                                 proj = proj_path,
                                 scenario = name,
                                 queryFile = queryFile)
        return(invisible())
    })


# Extract the emissions from the GCAM project.
emissions  <- get_hector_emissions(proj_path)
hector_out <- use_gcam_emissions("auxiliary_data/hector-gcam.ini",
                                 emissions,
                                 out_vars = vars)


# 2. Comparison ----------------------------------------------------------------

# Confirm that the Hector results results are consistent with the GCAM results!
"gcam_output/gcam-hector-outputstream.csv" %>%
    read.csv(comment.char = "#") %>%
    filter(spinup == 0) %>%
    filter(variable %in% hector_out$variable) %>%
    mutate(name = "outputstream") ->
    gcam_hector

hector_out %>%
    mutate(name = "stnd. alone") ->
    hector_out


gcam_hector %>%
    bind_rows(hector_out) %>%
    ggplot(aes(year, value, color = name)) +
    geom_line() +
    facet_wrap("variable", scales = "free")






