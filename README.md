# Hadoop-Genetic-Algorithm-Variable-Selection

I have coded Genetic algorithm from scratch in R.It is an implementation of Genetic algorithm on hadoop for Variable selection problem in logistic regression modeling. Given a dataset, a particular combination of variables is coded as 1s (variable present) and 0s (variable absent) thus representing a chromosome. 

The usual steps of Genetic Algorithm are performed starting from random Initialization of chromosome population, selection of half the population based upon fitness of chromosomes, crossover, mutation, selection again. 

Given a training and validation datasets, the Genetic algorithm fits a logistic regression model and computes the Area under ROC curve using the validation dataset. The fitness function consists of linear combination of the AUC and penalty for number of variables in the chromosome (number of 1s).
