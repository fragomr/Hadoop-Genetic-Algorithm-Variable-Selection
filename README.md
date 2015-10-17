# Hadoop-Genetic-Algorithm-Variable-Selection

I have coded Genetic algorithm from scratch in R.It is an implementation of Genetic algorithm on hadoop for Variable selection problem in logistic regression modeling. Given a dataset, a particular combination of variables is coded as 1s (variable present) and 0s (variable absent) representing a chromosome. 

The usual steps of Genetic Algorithm are performed starting from random Initialization of chromosome population, selection of half the population based upon fitness of chromosomes, crossover, mutation, selection again. 

Given a training and validation datasets, the Genetic algorithm fits a logistic regression model and computes the Area under ROC curve using the validation dataset. The fitness function consists of linear combination of the AUC and rewards for smaller number of variables in the chromosome (number of 1s) : Fitness = auc+Rho*(number_of_predictors - model_order)/number_of_predictors), where Rho is the intensity of the reward term.

Steps of Genetic Algorithm
1.#Initialization of starting population 
initializer function randomly initializes 1 and 0 into chromosomes where the total number of chromosomes (population_size) can be supplied by the . This function gives us the initial population (initial models) for the genetic algorithm to begin.
The population size has to be divisible by 4 as we are going to select exactly half of it and then need to form crossover pairs from this selected half.

2.#Select Parents such that half of the population is selected

