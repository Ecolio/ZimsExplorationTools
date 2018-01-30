# script for data cleaning for the Primate Workshop
# subset the data, format, insert the coding
# verso.4 added the possiblity to include the individuals to be excluded from the QC analysis
# vers1-0 added functions in separate script for better visibility, included DISKo max life spans
# written by Lionel Jouvet and Rita Da Silva

# prerequisites ----
Sys.setenv(LANG = "en")
rm(list = ls())
cat("\014")
suppressMessages(wdpathGeneral <- choose.dir(getwd(), "Choose a suitable folder"))
setwd(wdpathGeneral)
filesList <- list.files(wdpathGeneral)
print(filesList, row.names=T)

setwd("..")
parent <- getwd()
# functions
source("code\\Functions\\DataExplorationFunctions_ver0-0.R") 
MaxLifeSpanDisko <- read.csv("Functions\\ML_DISKo.csv", header = T)
setwd(wdpathGeneral)


cat("enter the file number to be analysed", "\n")
filenumber <-7
cat("enter the QC file number", "\n")
QCfilenumber <-20

#PRE REQUISITS
# loading in the data ----
# enter the file name

df <- read.csv(filesList[filenumber], sep = "@", header = F)
QCinfo <- read.csv(filesList[QCfilenumber], sep = "\t", header = T)
# enter the quality check graph name
graph.name <- paste("File4BastaQC", ".pdf", sep = "")
minimum.individual.number <- 100
data.source <- "ZIMS"
extraction.date <- as.Date("2018-01-11")
CutOffDate <- as.Date("198-01-01")
problematic.dates.to.na <- as.vector(as.Date(c("1753-01-01", "1899-12-30")))


#libraries ----
library(ggplot2)
# install.packages("gridExtra")
library(gridExtra)
library(grid)
library(data.table)
library(plyr)


# first clean-up of the data to remove data query artefacts

#headers
headernames <- c("AnonID", "GAN", "Class", "Order", "Family", "SpeciesName", 
                "CommonName", "IUCNRedList", "CITES", "CITESE", 
                "FirstHoldingInstitution", "LastHoldingInstitution", "LatitudeZone",
                "BirthDate", "BirthDateEstimateType", "BirthDateEstimateStart", 
                "BirthDateEstimateEnd", "BirthObserved", "FirstAcqusitionDate", 
                "BirthType", "SexType", "PhysicalMoveCount", "DeathDate", 
                "DeathDateEstimateType", "DeathDateEstimateStart", "DeathDateEstimateEnd",
                "GlobalStatus", "AnimalType", "RDI", "LastCommentEntryDate", "LastTXDate")
# data contruction ----
colnames(df) <- headernames

# construction of the basta data
### collapsed region ----
###
if (!is.na(QCinfo)) {
  list.Id.outliers <- unlist(strsplit(QC$outlier.ID, split = ", "))
}
df <- subset(df, df$SpeciesName == "Panthera leo")

dataExtract <- df
File4Basta <- as.data.frame(matrix(data = "", nrow = nrow(dataExtract), ncol = 1))
File4Basta$Species.Name <- as.character(dataExtract$SpeciesName)
File4Basta$Species.Common.Name <- as.character(dataExtract$CommonName)


File4Basta$Animal.id <- as.character(dataExtract$AnonID)
File4Basta$Study.id <- data.source
### collapsed region ----
File4Basta$Birth.Date <- as.Date(dataExtract$BirthDate)
File4Basta$Min.Birth.Date <- as.character(dataExtract$BirthDateEstimateStart)
File4Basta$Max.Birth.Date <- as.character(dataExtract$BirthDateEstimateEnd)
File4Basta$Physical.move <- dataExtract$PhysicalMoveCount
File4Basta$Birth.Date.Distribution <- "U"
File4Basta$Sex <- dataExtract$SexType

File4Basta$BirthType[which(dataExtract$BirthObserved != "YES")] <- "T"
File4Basta$BirthType[which(dataExtract$BirthObserved == "YES")] <- "B"

File4Basta$Entry.Date <- as.Date(dataExtract$FirstAcqusitionDate)
list.entry.date <- which(is.na(File4Basta$Entry.Date))
File4Basta <- droplevels(File4Basta)
File4Basta$Entry.Date[list.entry.date] <- File4Basta$Birth.Date[list.entry.date]

File4Basta$Depart.Date <- as.Date(dataExtract$LastTXDate)
File4Basta$Depart.Date <- as.Date(File4Basta$Depart.Date)
File4Basta$Depart.Date[which(is.na(File4Basta$Depart.Date))] <- extraction.date
File4Basta$Depart.Type[which(dataExtract$GlobalStatus == "Dead")] <- "D"
File4Basta$Depart.Type[which(dataExtract$GlobalStatus != "Dead")] <- "C"

File4Basta$Birth.Date[which(File4Basta$Birth.Date %in% 
                              problematic.dates.to.na)] <- NA
File4Basta$Max.Birth.Date[which(File4Basta$Max.Birth.Date %in% 
                                  problematic.dates.to.na)] <- NA
File4Basta$Min.Birth.Date[which(File4Basta$Min.Birth.Date %in% 
                                  problematic.dates.to.na)] <- NA
File4Basta <- subset(File4Basta, File4Basta$Entry.Date > CutOffDate)
dataExtract <- subset(dataExtract, as.Date(dataExtract$FirstAcqusitionDate) > CutOffDate)


min.number <- as.data.frame(table(File4Basta$Species.Name))
nim.species.names <- as.vector(droplevels(min.number$Var1[which(min.number[, 2] >= 
                                          minimum.individual.number)]))
File4Basta <- subset(File4Basta, Species.Name %in% nim.species.names)
dataExtract <- subset(dataExtract, SpeciesName %in% nim.species.names)

File4Basta$lifespan <- difftime(as.Date(File4Basta$Depart.Date, format = "%Y-%m-%d"),
                                as.Date(File4Basta$Birth.Date, format = "%Y-%m-%d"),
                                units = "days") / 365
File4Basta$lifespan <- gsub("Time difference of ", "", File4Basta$lifespan)
File4Basta$lifespan <- gsub(" days", "", File4Basta$lifespan)
File4Basta$lifespan <- round(as.numeric(File4Basta$lifespan), 2)
cat("Evaluation of the data", "\n")
aa <- ddply(File4Basta, ~ Species.Name, summarise, mean = mean(lifespan, na.rm = T),
                                        qrt = quantile(lifespan, 0.98, na.rm = T),
                                        .progress = progress_text(char = "."))

File4Basta <- merge(File4Basta, aa, by.x = "Species.Name", by.y = "Species.Name")
File4Basta <- File4Basta[, - which(colnames(File4Basta) == "V1")]

remove.ind <- unlist(strsplit(outliers, split = ", "))
a <- as.vector(sFile4Basta$Animal.id)
a <- a[!a %in% remove.ind]
sFile4Basta <- File4Basta
sFile4Basta <- subset(sFile4Basta, sFile4Basta$Animal.id %in% a)
dataExtract <- subset(dataExtract, dataExtract$Animal.id %in% a)

File4Basta <- sFile4Basta

# detection of the problems ----
File4Basta$Ind <- 1
File4Basta$P.Birth.over.Entry <- 0
File4Basta$P.Min.Birth.over.Entry <- 0
File4Basta$P.Max.Birth.over.Entry <- 0
File4Basta$P.Depart.over.Entry <- 0
File4Basta$P.Depart.equal.TX <- 0
File4Basta$P.Depart.B4.Entry <- 0
File4Basta$P.Birth.over.Death <- 0
File4Basta$P.neg.lifespan <- 0
File4Basta$P.old.date <- 0
File4Basta$P.extrem.lifespan <- 0

File4Basta$P.Birth.over.Entry[which(File4Basta$Birth.Date > 
                                      File4Basta$Entry.Date)] <- 1
File4Basta$P.Min.Birth.over.Entry[which(File4Basta$Min.Birth.Date > 
                                          File4Basta$Entry.Date)] <- 1
File4Basta$P.Max.Birth.over.Entry[which(File4Basta$Max.Birth.Date > 
                                          File4Basta$Entry.Date)] <- 1
File4Basta$P.Depart.B4.Entry[which(File4Basta$Entry.Date < 
                                     File4Basta$Entry.Date)] <- 1
File4Basta$P.Depart.equal.TX[which(as.Date(File4Basta$Depart.Date) != 
                                     as.Date(dataExtract$LastTXDate))] <- 1
File4Basta$P.Birth.over.Death[which(File4Basta$Birth.Date > 
                                      File4Basta$Death.Date)] <- 1
File4Basta$P.neg.lifespan[which(File4Basta$lifespan < 0)] <- 1
File4Basta$P.old.date[which(File4Basta$Entry.Date < "1980-01-01")] <- 1
File4Basta$P.old.date[which(File4Basta$Birth.Date < "1980-01-01")] <- 1
File4Basta$P.old.date[which(File4Basta$Depart.Date < "1980-01-01")] <- 1
File4Basta$P.extrem.lifespan[which(File4Basta$lifespan >= File4Basta$qrt)] <- 1
# quality control ----
cat("Quality control of the data", "\n")

QC <- ddply(File4Basta, ~ Species.Name, summarise, 
            nb.of.individuals = sum(Ind),
            min.life.span = min(lifespan, na.rm = T),
            quantile98.life.span = round(quantile(lifespan, 0.98, na.rm = T), 1),
            max.life.span = max(lifespan, na.rm = T),
            nb.of.outliers = sum(P.extrem.lifespan),
            neg.lifespan.outliers = sum(P.neg.lifespan),
            .progress = progress_text(char = "."))
QC$outlier.ID <- NA
species.interogated <- as.vector(QC$Species.Name)
for (i in 1:length(species.interogated)) {
    outliers.detection <- subset(File4Basta, File4Basta$Species.Name == 
                                   species.interogated[i])
    
    outliers <- paste(outliers.detection$Animal.id[which(outliers.detection$P.Birth.over.Entry == 1)], collapse = ", ")
    QC$Birth.over.Entry.ID[i] <- outliers
    outliers <- paste(outliers.detection$Animal.id[which(outliers.detection$P.Min.Birth.over.Entry == 1)], collapse = ", ")
    QC$Min.Birth.over.Entry.ID[i] <- outliers
    outliers <- paste(outliers.detection$Animal.id[which(outliers.detection$P.Max.Birth.over.Entry == 1)], collapse = ", ")
    QC$Max.Birth.over.Entry.ID[i] <- outliers
    outliers <- paste(outliers.detection$Animal.id[which(outliers.detection$P.Depart.B4.Entry == 1)], collapse = ", ")
    QC$Depart.B4.Entry.ID[i] <- outliers
    outliers <- paste(outliers.detection$Animal.id[which(outliers.detection$P.Depart.equal.TX == 1)], collapse = ", ")
    QC$Depart.equal.TX.ID[i] <- outliers
    outliers <- paste(outliers.detection$Animal.id[which(outliers.detection$P.Birth.over.Death == 1)], collapse = ", ")
    QC$Birth.over.Death.ID[i] <- outliers
    outliers <- paste(outliers.detection$Animal.id[which(outliers.detection$P.neg.lifespan == 1)], collapse = ", ")
    QC$neg.lifespan.ID[i] <- outliers
    outliers <- paste(outliers.detection$Animal.id[which(outliers.detection$P.old.date == 1)], collapse = ", ")
    QC$old.date.ID[i] <- outliers
    outliers <- paste(outliers.detection$Animal.id[which(outliers.detection$P.extrem.lifespan == 1)], collapse = ", ")
    QC$outlier.ID[i] <- outliers
    
}




###### data saving, Baboons  ----
write.table(QC, "test_File4BastaQC.txt", sep = "\t", row.names = F)
write.table(File4Basta, "test_File4Basta.txt", sep = "\t", row.names = F)
