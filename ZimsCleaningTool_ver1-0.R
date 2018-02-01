# script for data cleaning from ZIMS extraction tool
# subset the data, format, insert the coding
# vers1-0 build over versions 0-04
# added functions in separate script for better visibility, included DISKo max life spans
# include a detached script for functions
# written by Lionel Jouvet and Rita Da Silva

# packages to be installed
# install.packages("rstudioapi")

rm(list = ls())
#libraries ----

library(rstudioapi)
library(data.table)
library(plyr)
library(dplyr)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))
source("Functions/DataExplorationFunctions_ver0-0.R") 

# prerequisites ----


Sys.setenv(LANG = "en")
cat("\014")
suppressMessages(wdpathGeneral <- choose.dir(getwd(), "Choose a suitable folder"))
setwd(wdpathGeneral)
FileList <- list.files(wdpathGeneral)
print(FileList, fill = T)

#PRE REQUISITS----
# loading in the data 
# enter the file name from the list displayed
# the core data as well as the column name header need to be entered
# header:
HeaderFile <- FileList[25]
# corefile
CoreFile <- FileList[18]
# species to be subset
SpeciesSubset <- c()
NameOfDataFile <- "PrimateZIMS.txt"
NameOfQCFile <- "PrimateZIMSQC.txt"
headernames <- as.vector(unlist(read.csv(HeaderFile, sep = "@", header = F)))
headernames[1] <- "AnonID"
df <- read.csv(CoreFile, sep = "@", header = F)
colnames(df) <- headernames
data.source <- "ZIMS"
extraction.date <- as.Date("2018-01-11")
problematic.dates.to.na <- as.vector(as.Date(c("1753-01-01", "1899-12-30")))
CutOffDate <- as.Date("198-01-01")
minimum.individual.number <- 100

# data contruction ----
if (!empty(SpeciesSubset)) {
df <- subset(df, SpeciesName  %in% SpeciesSubset)
}
cat("\014")
File4Basta <- DataFormatting(df)
ID.List <- as.vector(File4Basta$Animal.id)
df <- subset(df, AnonID %in% ID.List)
QC <- DataQC(File4Basta, df)

###### data saving, Baboons  ----
write.table(QC, NameOfQCFile, sep = "\t", row.names = F)
write.table(File4Basta, NameOfDataFile, sep = "\t", row.names = F)
