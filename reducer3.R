#! /usr/bin/env Rscript

options(warn=-1)

rm(list=ls())

library(ROCR)

library(xlsx)

set.seed(23)

input <- file("stdin", "r")     

dataset <- NA

dataset<-na.omit(dataset)

key <- "Z"
keylist<-NA

num_records=0

#if (FALSE){

while(length(currentLine <- readLines(input, n=1, warn=FALSE)) > 0) {

fields <- unlist(strsplit(currentLine, "\t"))

if (fields[1]!=key) {
keylist <- c(keylist,"key:",fields[1])
key<-fields[1]}

fields<-fields[-1] #get rid of key

fields<-unlist(strsplit(fields, ","))

dataset <<- as.data.frame(rbind(dataset,t(as.numeric(fields))))

num_records <<- num_records + 1

} #end of while loop

#}#end of if false

sink()

#cat(key,class(dataset),nrow(dataset),ncol(dataset))

#cat("a")

close(input)              


if (num_records > 100){

dataset_training <- dataset[1:(.7*nrow(dataset)),] #first column must be bad_flag

dataset_validation <- dataset[-(1:(.7*nrow(dataset))),] #first column must be bad_flag

number_of_predictors = ncol(dataset_training)-1

#cat(class(dataset_training[,1]),class(dataset_training[,10]))
#cat(nrow(dataset_training),nrow(dataset_validation),number_of_predictors,ncol(dataset_validation)-1)


colnames(dataset_training) <- c("bad_flag",c(apply(matrix(1:number_of_predictors),1,function(x) paste("V",x,sep=""))))

colnames(dataset_validation) <- c("bad_flag",c(apply(matrix(1:number_of_predictors),1,function(x) paste("V",x,sep=""))))

Best_model_by_model_order <- NULL
Best_model_by_model_order <- data.frame(matrix(rep(0, number_of_predictors*(2+number_of_predictors)), nrow = number_of_predictors, ncol = 2+number_of_predictors))

colnames(Best_model_by_model_order) <- c("Model Order",paste("V",1:number_of_predictors,sep=""), "AUC")
Best_model_by_model_order["Model Order"] <- 1:number_of_predictors
Best_model_by_model_order["AUC"] <- rep(0,number_of_predictors)


Genetic_Algorithm <- function(Rho, population_size, Max_limit_numberGenerations) {

#Fitness of chromosome

fitness <- function(chromosome){

#return 0 fitness for a chromosome with all zeros
if (sum(chromosome == 1) == 0) {
return(0)
}

rhs = ""
for (i in 1:number_of_predictors) {
if (chromosome[i] == 1) {rhs=paste(rhs,"+","V",i,sep="")}
}
rhs=substr(rhs,2,nchar(rhs))

#to prevent null input on rhs of regression equation
#if (rhs == "") {rhs = "V1"}

logistic.model <- glm(as.formula(paste("bad_flag ~ ",rhs)), data = dataset_training, family=binomial(logit))
logistic.scores <- predict(logistic.model, newdata = dataset_validation, type="response")
logistic.rocr <- prediction(logistic.scores, dataset_validation$bad_flag)
auc <- performance(logistic.rocr, "auc")
auc <- unlist(slot(auc, "y.values"))

#testset$score<-predict(model,type='response',testset[-21])
#pred<-prediction(testset$score,testset$bad_flag)
#perf <- performance(pred,"tpr","fpr")
#plot(perf)

#calculate model's order (number of predictors included)
model_order <- sum(chromosome == 1)

#Replace the previous model in variable by new if the new one has higher AUC for that model order
if (Best_model_by_model_order[model_order, ]$AUC < auc) 
{
#print(c("model order is :", model_order))
#print(c("AUC is : ",Best_model_by_model_order[model_order, ]$AUC," and auc is : ",auc))
Best_model_by_model_order[model_order, ][2:(number_of_predictors+1)] <<- chromosome
Best_model_by_model_order[model_order, ]$AUC <<- auc
}

#return(AIC(model))
return(auc+Rho*(number_of_predictors - model_order)/number_of_predictors)
}

#Initialization of starting population

initializer <- function(){

chromosomeSet = NULL

readinteger <- function()
{ 
#n is tentative population size. Would be adjusted to make it divisible by 4
  n <- readline(prompt="Enter an integer: ")
  n <- as.integer(n)
  if (is.na(n)){
    n <- readinteger()
  }
  return(n)
}

#population_size <- readinteger()


for (i in 1:population_size) {
#new_chromosome = rep(0,number_of_predictors)
#new_chromosome[sample(1:number_of_predictors, model_order, replace=FALSE)]=1
new_chromosome = sample(0:1,number_of_predictors,replace=TRUE)
chromosomeSet = rbind(chromosomeSet, unname(new_chromosome))
}

#Treatment of starting population to make the number of chromosomes divisible by 4

if (population_size%%4 != 0) {

fitnessChromosome <- matrix(rep(0,population_size),population_size)

for (i in 1:population_size) {
fitnessChromosome[i,1] <- fitness(chromosomeSet[i,])
}

if (population_size%%4 == 3){
chromosomeSet = rbind(chromosomeSet,chromosomeSet[order(fitnessChromosome, decreasing = TRUE)[1],])
}
else if (population_size%%4 == 2)
{
chromosomeSet = rbind(chromosomeSet,chromosomeSet[order(fitnessChromosome, decreasing = TRUE)[c(1,2)],])
}
else if (population_size%%4 == 1)
{
chromosomeSet = rbind(chromosomeSet,chromosomeSet[order(fitnessChromosome, decreasing = TRUE)[c(1,2,3)],])
}
                           }
return(chromosomeSet)

}


#selectParents such that half of the population is selected

selectParents <- function(population){

fitnessChromosome <- matrix(rep(0,nrow(population)),nrow(population))

for (i in 1:nrow(population)) {

fitnessChromosome[i,1] <- fitness(population[i,])

}

#print(c("population", "fitnessChromosome"))
#write.table(c("population", "fitnessChromosome"), append=TRUE, file_name)
#print(cbind(population, fitnessChromosome))
#write.table(cbind(population, fitnessChromosome), append=TRUE, file_name)
#print("Best_model_by_model_order")
#print(Best_model_by_model_order)
#print(order(fitnessChromosome, decreasing=TRUE)[1:2])

probabilityChromosome <- fitnessChromosome/sum(fitnessChromosome)

#Giving zero value to probability of selection of top two fitness chromosomes 
#so that they don't get selected. We will add their indices anyway
indices_of_elite_two = order(probabilityChromosome, decreasing=TRUE)[1:2]
probabilityChromosome[indices_of_elite_two]=0

indices_to_return_without_elite_two = sample(1:nrow(population), (nrow(population)/2) - 2, replace = FALSE, as.vector(probabilityChromosome))

indices_to_return_with_elite_two = unname(c(unname(indices_to_return_without_elite_two), unname(indices_of_elite_two)))

#print(c("indices_to_return_without_elite_two", indices_to_return_without_elite_two))
#write.table(c("indices_to_return_without_elite_two", indices_to_return_without_elite_two), append=TRUE, file_name)
#print(c("indices_of_elite_two",indices_of_elite_two))
#write.table(c("indices_of_elite_two",indices_of_elite_two), append=TRUE, file_name)
#print(c("indices_to_return_with_elite_two",indices_to_return_with_elite_two))
#write.table(c("indices_to_return_with_elite_two",indices_to_return_with_elite_two), append=TRUE, file_name)

#return(sample(1:nrow(population), nrow(population)/2, replace = FALSE, as.vector(probabilityChromosome)))
return(unname(indices_to_return_with_elite_two))
}


#crossover

crossover <- function(chromosome1,chromosome2){

randomNumberCrossover <- runif(1, 0, 1)

orig_chromosome1 <- chromosome1
orig_chromosome2 <- chromosome2

#We want probability of crossover to be .6
#So crossover should occur 60% of times the function is called
if (randomNumberCrossover < .6)
{
crossoverPoint <- sample(1:(number_of_predictors-1),1)

temp <- chromosome1[(crossoverPoint+1):number_of_predictors]

chromosome1 <- c(chromosome1[1:crossoverPoint],chromosome2[(crossoverPoint+1):number_of_predictors])

chromosome2 <- c(chromosome2[1:crossoverPoint],temp)
}

#Checking condition that model order is not exceeded due to crossover
#Crossed over chromosomes revert to original if model order is exceeded
#if ((sum(chromosome1 == 1) > model_order) || (sum(chromosome2 == 1) > model_order))
#{
#chromosome1 <- orig_chromosome1
#chromosome2 <- orig_chromosome2
#}

return(rbind(chromosome1,chromosome2))

}


#mutation

mutation <- function(chromosome){

orig_chromosome <- chromosome

for (i in 1:number_of_predictors){

randomNumberMutation <- runif(1, 0, 1)

#We want probability of mutation to be .005 at any bit

if (randomNumberMutation < .005)
{
#This makes 1 as 0 and 0 as 1
chromosome[i]=1-chromosome[i] 
}

             } #end of for loop

#Checking condition that model order is not exceeded due to mutation
#if (sum(chromosome == 1) > model_order)
#{
#chromosome <- orig_chromosome
#}

return(chromosome)

}

numberGenerations = 1

#start of generations

population <- initializer()


#Average population fitness global variable

AvgPopulationFitness = NULL

#Max_limit_numberGenerations = 1

repeat {

#if (numberGenerations > 20 && mean(AvgPopulationFitness[max((numberGenerations-20),1):#(numberGenerations-1)]) >= AvgPopulationFitness[numberGenerations])

if (numberGenerations > Max_limit_numberGenerations)
{
break
}

fitnessChromosome <- matrix(rep(0,nrow(population)),nrow(population))

for (i in 1:nrow(population)) {

fitnessChromosome[i,1] <- fitness(population[i,])
#cat("fitnessChromosome[i,1]",fitnessChromosome[i,1])
}

#Add new Avg population fitness to AvgPopulationFitness global variable 
AvgPopulationFitness<-cbind(AvgPopulationFitness,sum(fitnessChromosome)/nrow(population))

parentsIndices <- selectParents(population)

parents <- population[parentsIndices,]

remainingRowIndices <- 1:nrow(parents)

#Keep only parents in the population
#population <- population[(1:nrow(population)) %in% parentsIndices]

population <- parents

for (i in 1:(nrow(parents)/2))
{
tupleRowIndices <- sample(remainingRowIndices,2,replace=FALSE)
remainingRowIndices <- remainingRowIndices[!remainingRowIndices %in% tupleRowIndices]
chromosomeTuple <- population[tupleRowIndices,]
newChromosomeTupleAfterCrossover <- crossover(chromosomeTuple[1,],chromosomeTuple[2,])
newChromosomeTupleAfterMutation <- rbind(mutation(newChromosomeTupleAfterCrossover[1,]),mutation(newChromosomeTupleAfterCrossover[2,]))
population <- rbind(population,newChromosomeTupleAfterMutation)
#" tupleRowIndices:", tupleRowIndices, " remainingRowIndices:", remainingRowIndices, "\n", " #chromosomeTuple:", "\n", chromosomeTuple, 
#"\n",newChromosomeTupleAfterCrossover:", "\n", newChromosomeTupleAfterCrossover,
#"\n"," newChromosomeTupleAfterMutation:","\n",  newChromosomeTupleAfterMutation,"\n" ," population:", "\n",population,sep="")
}
#for ends here

#print(c("Generation:", numberGenerations), sep=" ")
#write.table(c("Generation: ", numberGenerations), append=TRUE, file_name)
#print("population")
#print(population)
#write.table("population: ", append=TRUE, file_name)
#write.table(population, append=TRUE, file_name)
#print(c("Population Fitness:",sum(fitnessChromosome)/nrow(population)))
#write.table(c("Population Fitness:",sum(fitnessChromosome)/nrow(population)), append=TRUE, file_name)
#print(c("Avg Population Fitness:", AvgPopulationFitness))
#write.table(c("Avg Population Fitness:", AvgPopulationFitness), append=TRUE, file_name)

numberGenerations=numberGenerations+1

}
#repeat ends here

print("Final Best_model_by_model_order:")
print(Best_model_by_model_order)

}
#Genetic_Algorithm definition ends here

#sink("/dev/null") 

#Run Genetic_Algorithm function
#cat("length(unique(dataset_validation$bad_flag))",length(unique(dataset_validation$bad_flag)))
if (length(unique(dataset_validation$bad_flag))==2){

#Genetic_Algorithm(Rho, population_size, Max_limit_numberGenerations)
cat("Running GA")
#Genetic_Algorithm(0, 40, 25)
#Genetic_Algorithm(0.25, 40, 25)
#Genetic_Algorithm(.5, 40, 25)
#Genetic_Algorithm(.75, 40, 25)
Genetic_Algorithm(1, 40, 25)
                                      }

#cat("Best_model_by_model_order", Best_model_by_model_order[model_order, ]$AUC)
#cat("Best_model_by_model_order", Best_model_by_model_order[2, ]$AUC, Best_model_by_model_order[3, ]$AUC, Best_model_by_model_order[4, ]$AUC, Best_model_by_model_order[5, ]$AUC, Best_model_by_model_order[6, ]$AUC, Best_model_by_model_order[7, ]$AUC)

} #if num_records condition ends here



