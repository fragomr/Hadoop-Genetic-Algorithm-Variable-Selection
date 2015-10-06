#! /usr/bin/env Rscript

options(warn=-1)

#http://ultips.kuntzmann.info/2011/08/how-to-use-uuid-and-blkid-to-manage-devices/

rm(list=ls())

sink("/dev/null")

#library(e1071)

#library(ROCR)

#set.seed(23) don't set seed for his one

input <- file("stdin", "r")

#input <- file("/home/ic10636/Genetic_Algorithm/v2/modeldata_sample_lessvars1.csv", "r")   

#dataset <- read.csv('/home/ic10636/Genetic_Algorithm/modeldata_sample_lessvars.csv',head=FALSE) #first column must be bad_flag

dataset <- NA

dataset<-na.omit(dataset)

while(length(currentLine <- readLines(input, n=1, warn=FALSE)) > 0) {

fields <- unlist(strsplit(currentLine, ","))

#dataset <- as.data.frame(rbind(dataset,t(as.numeric(fields))))

sink()

cat(sample(1:200,1), currentLine, "\n", sep="\t")

sink("/dev/null")

#print (dataset)

} #end of while loop                                                               
   
#cat(fields[1], fields[2], mean, "\n", sep="\t")    

#cat(population, sum(fitnessChromosome)/nrow(population),"\n", sep="\t")

#cat(t(population), sum(fitnessChromosome)/nrow(population),"\n")

#cat("X", indices_to_keep,"#",paste(paste(chrom_max_fitness,sep="",collapse=""),"#",max_fitness,sep="",collapse=""), sep="\t")
                                   
close(input)
