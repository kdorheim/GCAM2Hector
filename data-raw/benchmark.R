# Define what we will consider to be "zero" or the acceptable error for
# hector variables, while re constructing the hector inputs from the
# xml output database. While this is going to be a comparison of Hector
# results that were run at the same time they were saved differently and may
# have different levels of precision.

# TODO
# 1) It might be worth looking into if these differences are consistent across
#       scenarios, if so could we be looking at a numeric sensitivity of the inputs?

# 0. Set Up --------------------------------------------------------------------
devtools::load_all()

# This script assumes that a GCAM output data base and the associated
# outputstream exist.
prj_file <- "./gcam_output/gcam_data.dat"
db_dir <- "gcam_output"
db_name <- "database_basexdb"

out_file <- "./gcam_output/gcam-hector-outputstream.csv"


# 1. GCAM data------------------------------------------------------------------

get_all_queries(db_dir = db_dir, db_name = db_name, prj_file = prj_file)

fetch_GCAM_vs_hector(prj_file = prj_file) %>%
    select(scenario, year, gcam = value, variable) ->
    gcam_data

# 2. Hector --------------------------------------------------------------------
# The outputstream does not include the scenario name, which is kind of a shame
# and might be a worth while TODO.
read.csv(out_file, comment.char = "#") %>%
    filter(!spinup) %>%
    filter(variable %in% gcam_data$variable) %>%
    select(year, hector = value, variable) ->
    hector



# 3. Comparison ----------------------------------------------------------------

gcam_data %>%
    inner_join(hector, by = join_by(year, variable)) %>%
    mutate(AE = abs(gcam - hector)) ->
    AE_df

# Interesting, it does not seem like the error is consistent over time...
AE_df %>%
    ggplot(aes(year, AE)) +
    geom_vline(xintercept = 2000, color = "red", linetype = 2) +
    geom_line() +
    facet_wrap("variable", scales = "free") +
    labs(title = "GCAM xml db vs. hector output stream",
         y = "absolute error")


# Our definition of zero typically...
zero <- 1e-8

# Save the mean absolute error (MAE) per variable over time... this will be
# our threshold for testing per variable...
AE_df %>%
    summarise(MAE = mean(AE), .by = c("variable")) %>%
    mutate(MAE = MAE + zero) ->
    error_benchmark

write.csv(error_benchmark, file = "tests/testthat/error_benchmark.csv", row.names = FALSE)


