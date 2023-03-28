# Extract GCAM emissions and process to use as Hector inputs. Note the TODOS 
# this code relies on some hard coded pathways and not everything is implemented 
# in GCAM for the hector v3 - GCAM integration. 
# IMPORTANT TODOs
#   1. daccs & luc things 
#   2. NBP constraint? 
# IDEAs 
# should this be a package? 
# search TODOs for other ideas

# 0. Set up & define helper functions ----------------------------------------------------------
library(assertthat)
library(data.table)
library(ggplot2)
library(hector)
#remotes::install_github("jgcri/hector@gcam-integrationv3", force = TRUE)
library(rgcam)
library(magrittr)
library(zoo)

# Output directory 
BASE_DIR <- here::here()

# data.table implementation of the gcamdata repeat_add_columns
# Args 
#   x: data.table to add to 
#   y: data.table containing the column that should be repeated and added to dt x
# return: data.table
repeat_add_columns <- function(x, y){
  
  assert_that(is.data.table(x))
  assert_that(is.data.table(y))
  assert_that(!any(names(x) %in% names(y)))
  assert_that(!any(names(y) %in% names(x)))
  
  x$join <- 1
  y$join <- 1
  
  df <- merge(x, y, all = TRUE, by = .EACHI, allow.cartesian=TRUE)
  df$join <- NULL
  return(df)
  
}


# 1. Import mapping file ---------------------------------------------------------------
emissions_map <- as.data.table(read.csv(file.path(BASE_DIR, "GCAM_hector_emissions_map.csv")))

# 2. Extract data from a GCAM  output database -----------------------------------------

# Set up the paths to the data base and the query file. 
# TODO might want to make this more generalize ablee
conn      <- localDBConn('/Users/dorh012/projects/GCAM/gcam-core/output', 'database_basexdb')

# TODO this is missing queries for LUC & some other CO2 emissions & uptake but these need to be added 
# to GCAM. 
queryFile <- here::here("hector-emissions-queries.xml")


# Run queries on all of the scenarios listed in the GCAM xml database
# then load the gcam results and extract the scenario name. 
# TODO instead of writing out to disk pass in memory? 
lapply(X = listScenariosInDB(conn)$name, function(name){
  print(name)
  gcam_data <- addScenario(conn = conn, proj = 'gcam_db.dat', scenario = name, queryFile = queryFile)
  return(invisible())
})



# 3. Convert GCAM emissions into Hector emissions -----------------------------------------

# Transform a rgcam project file into a data frame with emissions in Hector appropriate units and names. 
# Args 
#   dat_file: file path to the .dat file created by rgcam
# Return: Hector input emissions for all the GCAM generated emission species for all the scenarios 
# listed in the dat_file. The returned data frame is set up to be used with some sort of hector::setvars 
# function
get_hector_emissions <- function(dat_file){
  
  # Check that the gcam data file actually exists 
  assert_that(file.exists(dat_file))
  # TODO is there some check to make sure that this a rgcam product? 
  gcam_rslts <- loadProject(dat_file)
  
  # Format gcam results into a data frame 
  lapply(names(gcam_rslts), function(x){
    gcam_df <- rbindlist(list(gcam_rslts[[x]]$`CO2 emissions by region`, 
                              gcam_rslts[[x]]$`nonCO2 emissions by region`), fill = TRUE)
    return(gcam_df)
  }) %>% 
    rbindlist -> 
    gcam_df
  
  # Add the mapping information to the data frame 
  gcam_emissions_map <- emissions_map[gcam_df, on = "ghg"]

  # Check to make sure that the un-mapped emissions are the ones we are expecting. 
  no_matches <- unique(gcam_emissions_map[is.na(gcam_emissions_map$agg.gas), ]$ghg)
  expected_emissions <- c("H2", "H2_AWB", "PM10", "PM2.5")
  assert_that(all(no_matches %in% expected_emissions), msg = "unexpected emission not being passed to Hector")
  
  # First convert from GCAM units to Hector units, this is important for some of the halocarbons, mulitple GCAM 
  # halocarbons may be aggregated into one Hector halocarbon category. 
  gcam_emissions_map$converted_value <- gcam_emissions_map[ , list(value * unit.conv)]
  gcam_inputs_for_hector <- gcam_emissions_map[ , list(value = sum(converted_value)), by = c("agg.gas", "hector.name", "scenario", "year",  "hector.units")]
  
  # Drop the exepcted NAs 
  d <- na.omit(gcam_inputs_for_hector)
  
  # error checking 
  req_cols <- c("year", "value")
  assert_that(all(req_cols %in% names(d)))
  
  
  # The expecgted years of data we want are from 2005 until 2100, before the year 
  # 2005 Hector is using the GCAM inputs 
  expected_years <- data.table(year = 2005:2100)
  
  # Construct a df of all the variables for all the 2005 until 2100. This will 
  # create a df with NA values when no GCAM emissions are available that will be 
  # fill in the next step. 
  save_cols <- names(d)[!names(d) %in% c("year", "value")]
  to_replicate <- distinct(d[, ..save_cols])
  df_with_all_yrs <- repeat_add_columns(x = to_replicate, y = expected_years)
  df_NA <- d[df_with_all_yrs,  on=names(df_with_all_yrs), nomatch = NA] 
  
  # Replace the NA emissions with linearlly interpolated values. 
  split(x = df_NA, 
        f = interaction(df_NA$hector.name, df_NA$scenario, df_NA$hector.units, drop = TRUE)) %>% 
    lapply(function(X){
      new_vals <- na.approx(object = X$value, x = X$year)
      X$value <- new_vals
      return(X)
    }) %>% 
    rbindlist ->
    complete_hetor_emissions
  
  
  # Finally format the data 
  out <- complete_hetor_emissions[, .(scenario,variable = hector.name, year, value, units = hector.units)]
  return(out)
}


# Get Hector comparison data 
# Args 
#   dat_file: file path to the .dat file created by rgcam
# Return: data table of Hector temp and rf values 
get_hector_comparison_data <- function(dat_file){
  
  assert_that(file.exists(dat_file))
  # TODO is there some check to make sure that this a rgcam product? 
  gcam_rslts <- loadProject(dat_file)
  
  # Format gcam results into a data frame 
  lapply(names(gcam_rslts), function(x){
    
    tas <- gcam_rslts[[x]]$`Climate forcing`
    tas$variable <- GLOBAL_TAS()
    rf_tot <- gcam_rslts[[x]]$`Climate forcing`
    rf_tot$variable <- RF_TOTAL()
    co2_con <- gcam_rslts[[x]]$`CO2 concentrations`
    co2_con$variable <- CONCENTRATIONS_CO2()
    
    gcam_df <- rbind(tas, rf_tot, co2_con)
    return(gcam_df)
  }) %>% 
    rbindlist -> 
    gcam_df
  
  return(gcam_df)
  
}


# Get the Hector emissions! 
dat_file <- file.path(BASE_DIR, "gcam_db.dat")
hector_emissions <- get_hector_emissions(dat_file)
hector_comparison <- get_hector_comparison_data(dat_file)


# 4. Run Hector with the GCAM set up & emissions! -----------------------------------------
# these are the two emissions that are causing the problems! FML "HFC23_emissions" "HFC32_emissions"


use_gcam_emissions<- function(ini_path, emissions_df, out_vars = c(GLOBAL_TAS(), RF_TOTAL(), CONCENTRATIONS_CO2())){
  
  # There should only be one scenario per emissions data frame. 
  assert_that(length(unique(emissions_df$scenario)) == 1)
  # TODO check to make sure that all the required emissions are included? 
  # TODO add a check that makes sure all the required columns are included in emissions_df? 
  
  # Set up the Hector core
  hc <- newcore(ini_path, name = unique(emissions_df$scenario))
  setvar(core = hc, dates = emissions_df$year, var = emissions_df$variable, values = emissions_df$value, unit = emissions_df$units)
  reset(hc)
  
  # TODO this should be dropped when the dacccs & luc stuff is implemented in GCAM & in the query, wait why the fuck is this throwing 
  # a Hector error! 
  luc_emissions <- data.frame(year = unique(emissions_df$year), 
                              var = LUC_EMISSIONS(), 
                              values = .5, 
                              unit = getunits(FFI_EMISSIONS()))
  setvar(core = hc, dates = 2005:2100, var = luc_emissions$var, values = 10, unit = getunits(FFI_EMISSIONS()))
  reset(hc)

  luc_uptake <- data.frame(year = 2005:2100,
                              var = LUC_UPTAKE(), 
                              values = 0, 
                              unit = getunits(FFI_EMISSIONS()))
  setvar(hc, luc_uptake$year, LUC_UPTAKE(), 0, getunits(FFI_EMISSIONS()))
  reset(hc)
  
  # why the fuck is this still not running??? 
  reset(hc)
  run(hc, runtodate = 2050)
  out <- fetchvars(core = hc, dates = 1750:2050, vars = out_vars)
  return(out)

  }


hector_gcam_driven <- use_gcam_emissions(ini_path = "~/projects/GCAM/gcam-core/input/climate/hector-gcam.ini", 
                                         emissions_df = hector_emissions)



rbindlist(list(hector_comparison %>%  
        filter(variable == GLOBAL_TAS()) %>% 
        mutate(source = "GCAM"),
      hector_gcam_driven %>% 
        filter(variable == GLOBAL_TAS()) %>% 
        mutate(source = "GCAM driven")), fill = TRUE) %>% 
  ggplot(aes(year, value, color = source)) + 
  geom_line() + 
  labs(title = "TAS")


rbindlist(list(hector_comparison %>%  
                 filter(variable == RF_TOTAL()) %>% 
                 mutate(source = "GCAM"),
               hector_gcam_driven %>% 
                 filter(variable == RF_TOTAL()) %>% 
                 mutate(source = "GCAM driven")), fill = TRUE) %>% 
  ggplot(aes(year, value, color = source)) + 
  geom_line() + 
  labs(title = "RF total")
rbindlist(list(hector_comparison %>%  
                 filter(variable == CONCENTRATIONS_CO2()) %>% 
                 mutate(source = "GCAM"),
               hector_gcam_driven %>% 
                 filter(variable == CONCENTRATIONS_CO2()) %>% 
                 mutate(source = "GCAM driven")), fill = TRUE) %>% 
  ggplot(aes(year, value, color = source)) + 
  geom_line() + 
  labs(title = "Co2 concc")


