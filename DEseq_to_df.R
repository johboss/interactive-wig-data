##########################Moddify deseq2 results for further analysis################
######Im using a single data frame for multiple deseq runs and consolidate in order to...
#######facilitate further statistica and visual annalysis
##########################
######if problems with dataset check for [NULL] data: sum(is.null(data))####
#install.packages("dygraphs")
#install.packages("xts")
library(stringr)
library(dygraphs)
library(dygraphs)
####[Instructions:]make an empty folder and putt all [deseq].csv files, then continue with script, add folder location to code underneath#####
desecLocation <- "/Volumes/Samsung_T5/reademption/deseqEdit/Consolidated/"
file_names <- dir(desecLocation)

#####Working dir is set to empty files so the list of filenames don't need the full directory
setwd(desecLocation)

######consolidates all files in folder as one data frame
df <- do.call(cbind, lapply(file_names, function(x) {
  cbind(read.delim(x, row.names=NULL, comment.char="#", col.names = {
    ######Reads the column names and modify them with the file name, minus the .csv end 
    paste(str_remove(x,'.csv'), colnames(read.delim(x, row.names=NULL, comment.char="#")), sep = "_")
  }))
}))
View(df)


################Pick out Attributes names#########

#### gene ID
gene_id <- str_extract(df$Annotations_Attributes, '^?ID=.+?(?=;)')
df$genenid <- gsub('^.{3}', '', gene_id)
#### gene name
gene_name <- str_extract(df$Annotations_Attributes, 'Name=[:graph:]*?;')
gene_name <- gsub('.{1}$', '', gene_name)
df$genename <- gsub('^.{5}', '', gene_name)
#### Loci name
gene_locus <- str_extract(df$Annotations_Attributes, 'locus_tag=[:graph:]*$')
gene_locus <- gsub('^.{10}', '', gene_locus)
df$genelocus <- gsub(';pseudo=.*$', '', gene_locus)
#### assosiated parent genes name
gene_parent <- str_extract(df$Annotations_Attributes, 'Parent=.+?(?=;)')
df$geneparent <- gsub('^.{7}', '', gene_parent)
#### Notes
gene_note <- str_extract(df$Annotations_Attributes, 'Note=.+?(?=;gbkey)')
df$genenote <- gsub('^.{5}', '', gene_note)

#### Reorder colums #ncol(df)
df <- df[,c(159,160,161,162,163,1:158)]

################Trimming away Tex treated data, anti-sense data and if wanted: delta59 data############
#df <- df[,grep("-T_",names(df), invert = TRUE)]
df <- df[(grep("anti",df$Annotations_Orientation.of.counted.reads.relative.to.the.strand.location.of.the.annotation, invert = T, fixed = TRUE)),]
#df <- df[,grep("delta59",names(df), invert = TRUE)]
df <- df[,grep("Annotations_Score",names(df), invert = TRUE)]
df <- df[,grep("Annotations_Sequence.name",names(df), invert = TRUE)]

sum(df$Annotations_P5004_101_R)
sum(df$Annotations_P5004_109_R.raw.countings)

View(df)


###write data to file
write.table(df, "DEseq2_AllFeatures.csv", sep="\t", row.names = FALSE, col.names = TRUE, quote = FALSE)

###Extract raw gene reads to file
df_temp <- df[grep("gene", df$Annotations_Feature),]
df_temp <- df_temp[,c(3,14:22)]
META_table <- data.frame(colnames(df_temp[2:10]), rep(c("wt","hfq","hfq_comp"), 3), rep(1:3, each = 3))
colnames(META_table) <- c("sample", "treatment", "batch")
write.table(df_temp, "RawP4GeneReads.csv", sep="\t", row.names = FALSE, col.names = TRUE, quote = FALSE)
write.table(META_table, "RawP4GeneReads_META.txt", sep="\t", row.names = FALSE, col.names = TRUE, quote = FALSE)

################################################################################
