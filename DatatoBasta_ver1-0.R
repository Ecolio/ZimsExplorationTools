# script for data cleaning from ZIMS extraction tool
# subset the data, format, insert the coding
# vers1-0 build over versions 0-04
# added functions in separate script for better visibility, included DISKo max life spans
# include a detached script for functions
# written by Lionel Jouvet and Rita Da Silva

# functions
# install.packages("gridExtra")
# install.packages("rstudioapi")

rm(list = ls())
#libraries ----
library(ggplot2)
library(gridExtra)
library(rstudioapi)
library(data.table)
library(plyr)
library(dplyr)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))
source("Functions/DataExplorationFunctions_ver0-0.R") 

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



# graphical representation of the data for visual QC
df$MeanDate <- df$BirthDate + floor((df$LastTXDate-df$BirthDate)/2) 
ggplot(df) + geom_segment(aes(x = LastTXDate, 
                              xend =  BirthDate, 
                              y = MeanDate, 
                              yend = MeanDate, 
                              colour = LastTXDate), size = 0.5) +
  geom_vline(xintercept = as.numeric(as.Date("1980-01-01"))) + geom_vline(xintercept = as.numeric(as.Date("2000-12-31")))



# enter the quality check graph name
graph.name <- paste("File4BastaQC", ".pdf", sep = "")

if (!is.na(QCinfo)) {
  list.Id.outliers <- unlist(strsplit(QC$outlier.ID, split = ", "))
}
QCinfo <- read.csv(filesList[QCfilenumber], sep = "\t", header = T)
remove.ind <- unlist(strsplit(outliers, split = ", "))
a <- as.vector(sFile4Basta$Animal.id)
a <- a[!a %in% remove.ind]
sFile4Basta <- File4Basta
sFile4Basta <- subset(sFile4Basta, sFile4Basta$Animal.id %in% a)
dataExtract <- subset(dataExtract, dataExtract$Animal.id %in% a)

File4Basta <- sFile4Basta

# detection of the problems ----



