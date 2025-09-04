# Prep the raw data
devtools::load_all()

# DEFAULT_EMISS_DF -------------------------------------------------------------
# The emissions/inputs that are going to be constant regardless of GCAM scenario
file  <-  system.file("extdata", "default_emissions.csv", package = "GCAM2Hector")
emiss <- read_hector_csv(file)

# There should be emissions up until the end of the century
stopifnot(max(emiss$year) >= 2100)

# Make sure that all the required emissions are present!
missing <- setdiff(emiss$variable, DEFAULT_EMISS)
stopifnot(length(missing) == 0)

DEFAULT_EMISS_DF <- emiss

# Save the processed data to the data/ directory
usethis::use_data(DEFAULT_EMISS_DF, overwrite = TRUE)

# PREGCAM_EMISS_DF -------------------------------------------------------------
# The emissions/inputs that are fed into Hector until the transition date.
file  <-  system.file("extdata", "gcam_emissions.csv", package = "GCAM2Hector")
emiss <- read_hector_csv(file)

# There should be no emissions greater than the transition date.
stopifnot(all(emiss$year <= TRANSITION_DATE))

# Make sure that all the required emissions are present!
missing <- setdiff(emiss$variable, GCAM_EMISS)
stopifnot(length(missing) == 0)

PREGCAM_EMISS_DF <- emiss

# Save the processed data to the data/ directory
usethis::use_data(PREGCAM_EMISS_DF, overwrite = TRUE)



# EMISS_MAP_DF -----------------------------------------------------------------
# The mapping file to convert the GCAM emissions to the proper Hector input
# name and units.
EMISS_MAP_DF <- read.csv("data-raw/GCAM_hector_emissions_map.csv")
# Save the processed data to the data/ directory
usethis::use_data(EMISS_MAP_DF, overwrite = TRUE)
