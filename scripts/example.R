# Demonstrate how to use GCAM2Hector to recreate hector results from
# the new GCAM xml output dbs.

devtools::load_all()
library(ggplot2)

# Define the path to the GCAM xml output db
gcam_output_dir <- "~/Documents/GCAM-WD/gcam-core/output/"
db_name <- "database_basexdb"
prj_file <- "scripts/gcam_example.dat"

# 1. Use GCAM2Hector functions -------------------------------------------------
# Can skip over running GCAM2Hector to cut down on time (if it has already been run)
if(FALSE){
    # Extract the hector inputs from the GCAM output db.
    inputs <- get_hector_inputs(db_dir = gcam_output_dir,
                                db_name = db_name,
                                prj_file = prj_file)
    write.csv(inputs, "scripts/example_inputs.csv", row.names = TRUE)


    # Run hector with the different emission inputs.
    # TODO there should be a better connection between get_hector_inputs
    # and the run_GCAM2hector if we already have inputs that we have already generated.
    out <- run_GCAM2hector(db_dir = gcam_output_dir,
                           db_name = db_name,
                           prj_file = prj_file)
    write.csv(out, "scripts/example_hector.csv", row.names = TRUE)


    gcam_hector_rslts <- fetch_GCAM_vs_hector(prj_file)
    write.csv(out, "scripts/gcam_hector_rslts.csv", row.names = TRUE)

}
# 2. Comparisons ---------------------------------------------------------------

out$source <- "stand alone hector"
zero <- 1e-6

# TODO I am worried that there is something going on where we are missing
# CO2 emissions... the RF BC is
# There is a slight difference between the results which I don't love...
# Which makes me worried about some level of precision...
rbind(gcam_hector_rslts, out) %>%
    select(-units) %>%
    spread(source, value) %>%
    na.omit() %>%
    filter(year >= 1975) %>%
    mutate(AE = abs(`gcam xmldb` - `stand alone hector`)) %>%
    summarise(MAE = mean(AE), .by = c("variable", "scenario")) %>%
    filter(variable == RF_SO2()) %>%
    filter(MAE >= zero)


