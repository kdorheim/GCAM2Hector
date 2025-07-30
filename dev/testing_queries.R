# Run the hector-queries file with an gcam output database, this is helpful for
# when tyring out new queries.


# 0. Set up & define helper functions ------------------------------------------
library(assertthat)
library(dplyr)
library(ggplot2)
remotes::install_github("jgcri/rgcam")
library(rgcam)


BASE_DIR <- here::here()



# 1. Extract Queries -----------------------------------------------------------
dir <- file.path(BASE_DIR, "gcam_output")
file <- "database_basexdbGCAM_SSP1_2p6"
conn <- localDBConn(dbPath = dir, file)

queryFile <- file.path(BASE_DIR, "hector-emissions-queries.xml")


name <- listScenariosInDB(conn)$name

gcam_data <- addScenario(conn = conn, proj = 'gcam_db.dat', scenario = name, queryFile = queryFile)


qnames <- listQueries(gcam_data)
queries <- qnames[grepl(pattern = "RF", x = qnames)]


lapply(queries, FUN = getQuery, projData = gcam_data) %>%
    rbind ->
    out

total <- getQuery(projData = gcam_data, query = "Climate forcing")


lapply(queries, function(x){

    getQuery(projData = gcam_data, query = x) %>%
        mutate(variable = x)

}) %>%
    do.call(what = "rbind") ->
    out

ggplot(out) +
    geom_line(aes(year, value, color = variable))

out %>%
    summarise(value = sum(value), .by = year) ->
    my_total


ggplot() +
    geom_line(data = total, aes(year, value)) +
    geom_line(data = my_total, aes(year, value, color = "mine"))


