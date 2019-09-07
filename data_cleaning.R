library(shiny)
library(ggplot2)
library(DT)
library(stringr)
library(tools)
library(plyr)
library(dplyr)
library(tidyr)
library(magrittr)
library(readr)
library(knitr)
library(tidyr)
library(scales)
library(markdown)



# import data ----
population <- read_csv("World_Population.csv", skip = 4)
exports <- read_csv("RIO_HTECH_EXPORTS-data.csv")

# data cleaning, merging, and subsetting ----

# cleaning data
exports$GEO_DESC <- as.character(exports$GEO_DESC)
exports$GEO_DESC[exports$GEO_DESC == "Czechia"] <- "Czech Republic"
exports$GEO_DESC[exports$GEO_DESC == "Slovakia"] <- "Slovak Republic"
exports <- subset(exports, GEO_DESC != "European Union" & UNIT != "PC_TOT")
exports$GEO <- NULL
exports$PARTNER <- revalue(exports$PARTNER, c("EXT_EU28"="non-EU Countries", "INT_EU28"="EU Member States", "WORLD"="the World"))


# get list of years and countries needed and columns needed from exports dataset
years <- unique(exports$TIME_PERIOD)
exports.cols <- c('TIME_PERIOD', 'PARTNER', 'GEO','OBS_VALUE','GEO_DESC')
countries <- sort(unique(exports$GEO_DESC))


# drop NULL columns from population dataset
population <- dplyr::select(population, -c(""))
# in population dataset, keep only years and countries that are in exports dataset
# Reference: https://stackoverflow.com/questions/5234117/how-to-drop-columns-by-name-in-a-data-frame
population <- population[ , (colnames(population) %in% years | colnames(population) %in% c('Country Name'))]
population <- subset(population, population$`Country Name` %in% countries)


# gather population data from wide to long
# Reference: http://www.cookbook-r.com/Manipulating_data/Converting_data_between_wide_and_long_format/
population %<>%
  gather(key = "Year", value = "Population", -'Country Name')

# clean and merge exports data
exports <- exports[,colnames(exports) %in% exports.cols] 
population$Year <- as.integer(population$Year)
exports <- left_join(exports, population, by = c("TIME_PERIOD" = "Year", "GEO_DESC" = "Country Name"))
colnames(exports) <- c("Year", "Partner", "Value (in Millions)","Country", "Population")

write_csv(exports, "EU_high_tech_exports_2007-2018.csv")
