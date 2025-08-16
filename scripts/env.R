# Set up the environment with the required packages.

library(assertthat)
library(data.table)
library(dplyr)
library(ggplot2)
# This should be V3.2.0
remotes::install_github("jgcri/hector@main")
library(hector)
library(magrittr)
library(zoo)
remotes::install_github("jgcri/rgcam")
library(rgcam)
