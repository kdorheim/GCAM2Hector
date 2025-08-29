
source("scripts/env.R")
source("scripts/fxns.R")

db_dir <- here::here("gcam_output/")
db_name <- "database_basexdb"
query_file <- here::here("auxiliary_data", "hector-queries.xml")

gcam_emiss_file = GCAM_EMISS_FILE
gcam_default_file = DEFAULT_EMISS_FILE
prj_file = "proj_data.dat"


# ------------------------------------------------------------------------------
comp_data  <- get_GCAM_hector_comparison_data(prj_file)
hector_out <- run_gcamHector(hc, db_dir, db_name, prj_file)


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

