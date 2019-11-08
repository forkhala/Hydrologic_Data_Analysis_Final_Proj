
##### Discharge Analysis #####

# Load Required Packages
pacman::p_load(tidyverse, dataRetrieval)
theme_set(theme_classic())

# Import site nos
site.nos <- read.csv("./Data/Raw/bestsiteslists.csv", colClasses = "character")[,2]
site.nos










