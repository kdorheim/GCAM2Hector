# Prep the raw data
devtools::load_all()

# DEFAULT_EMISS_DF -------------------------------------------------------------
# The emissions/inputs that are going to be constant regardless of GCAM scenario
emiss <- read_hector_csv("data-raw/default_emissions.csv")

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
emiss <- read_hector_csv("data-raw/gcam_emissions.csv")

# There should be no emissions greater than the transition date.
stopifnot(all(emiss$year <= TRANSITION_DATE))

# Make sure that all the required emissions are present!
missing <- setdiff(emiss$variable, GCAM_EMISS)
stopifnot(length(missing) == 0)

PREGCAM_EMISS_DF <- emiss

# Save the processed data to the data/ directory
usethis::use_data(PREGCAM_EMISS_DF, overwrite = TRUE)
