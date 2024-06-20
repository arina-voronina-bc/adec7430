#==================================
# Setup file
# Purpose: collect all project-general information, pathing, libraries etc.
#==================================

# this is to disable the annoying "enter an item from the menu, or 0 to exit" prompt
options(rlib_downstream_check = FALSE)


library(rstudioapi)

library("data.table")
library("readr")
# library("bit64")
#library("ff")
library("dplyr")
library("tidyverse")
library("ggplot2")
library("feather")


# options(scipen = 0)


#=== Paths
projPath <- dirname(dirname(rstudioapi::documentPath()))
projPath # this should be the folder of the whole class/project, if this script is inside Code inside that larger folder
codePath <- file.path(projPath, "adec7430")
dataPath <- file.path(projPath, "Data")
rawdataPath <- file.path(dataPath, "rawdata")
saveddataPath <- file.path(dataPath, "saveddata")
outputPath <- file.path(dataPath, "output")

