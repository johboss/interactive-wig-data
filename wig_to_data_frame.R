###############graph wig read files in interactive R shiny and plotly#####################
##########################################################################################
#install.packages("stringr")
library(data.table)
library(magrittr)
library(stringr)
library(plotly)
library(tidyr)
library(dplyr)
########################Pars .wig files###################################################
##########################################################################################
####[Instructions:]make an empty folder and putt all [deseq].csv files, then continue with script, add folder location to code underneath#####
desecLocation <- "/Volumes/Samsung_T5/reademption/coverage/notunique/coverage-tnoar_mil_normalized/"
file_names = list.files(path=desecLocation, pattern="*.wig", full.names=FALSE)

#####Working dir is set to empty files so the list of filenames don't need the full directory
setwd(desecLocation)

rm(cord_MC58)
cord_MC58 <- data.table(as.integer(seq(1, 2272360, by=1)))
cord_MC58 <- data.table(cord_MC58)
colnames(cord_MC58) <- c("ID")
rm(wig_data)
  ###consolidates all files in folder as one data frame
wig_data <- data.frame(lapply(file_names, function(x) {
    fo <- read.table(x, header=FALSE, skip=2, sep=' ')
    colnames(fo) <- c("ID", str_remove(x,'.wig'))
    fo <- data.table(fo, key = "ID")
    merge(cord_MC58, fo, by="ID", all=TRUE)
  }))
str(wig_data)
###Isolate only data colums
wig_data <- wig_data[, -grep("^ID.\\d$", colnames(wig_data))]

wig_data <- mutate_all(wig_data, ~replace(., is.na(.), 0))

###Change file names as they are changed for Colum names
df_cnames <- gsub(".wig", "", gsub("-",".",file_names))


###########################Merge meta data to wig data #############################################
####################################################################################################
####Make meta data
desecLocation <- "/Volumes/Samsung_T5/reademption/coverage/notunique/coverage-tnoar_mil_normalized/"
file_names = list.files(path=desecLocation, pattern="*.wig", full.names=FALSE)
filname_meta_split <- str_split_fixed(file_names, "-|_", 5)

### Wtiting tab sep .csv to working dir, the finishing is done manually for now to get a working metadata file
#write.table(filname_meta_split, "metfdata.csv", sep="\t",
#            row.names = FALSE, col.names = FALSE, quote = FALSE)



meta_data <- read.table("/Volumes/Samsung_T5/reademption/coverage/notunique/coverage-tnoar_mil_normalized/temp/metadata.csv", header = TRUE, sep=";")
meta_data$file <- gsub("-",".",meta_data$file)

### pivot data to single read position data frame

wig_data_piv <- pivot_longer(wig_data, 2:ncol(wig_data), names_to = "strain", values_to = "reads")

read_data <- merge(x = wig_data_piv, y = meta_data, by.x = "strain", by.y = "file", all.x = TRUE)
head(read_data)
#View(read_data)

read_data <- read_data[with(read_data, order(ID, sample, run, texTreatment, replicate, strand)), ]

###make .csv file
write.table(read_data, "Wig_read_data.csv", sep="\t", row.names = TRUE, col.names = TRUE, quote = FALSE)
###Make testing file
#read_data2 <- read_data[1:10000,]
#write.table(read_data2, "Wig_read_data_first10000.csv", sep="\t", row.names = TRUE, col.names = TRUE, quote = FALSE)



#paste((meta_data$strain, ".", meta_data$replicate
#unique(wig_data_piv$strain)

       
