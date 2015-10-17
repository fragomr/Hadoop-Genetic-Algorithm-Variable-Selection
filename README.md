# Hadoop-Genetic-Algorithm-Variable-Selection

I have coded Genetic algorithm from scratch in R.It is an implementation of Genetic algorithm on hadoop for Variable selection problem in logistic regression modeling. Given a dataset, a particular combination of variables is coded as 1s (variable present) and 0s (variable absent) representing a chromosome. 

The usual steps of Genetic Algorithm are performed starting from random Initialization of chromosome population, selection of half the population based upon fitness of chromosomes, crossover, mutation, selection again. 

Given a training and validation datasets, the Genetic algorithm fits a logistic regression model and computes the Area under ROC curve using the validation dataset. The fitness function consists of linear combination of the AUC and rewards for smaller number of variables in the chromosome (number of 1s) : Fitness = auc+Rho*(number_of_predictors - model_order)/number_of_predictors), where Rho is the intensity of the reward term.

Steps of Genetic Algorithm
1.Initialization of starting population 
initializer function randomly initializes 1 and 0 into chromosomes where the total number of chromosomes (population_size) can be supplied by the . This function gives us the initial population (initial models) for the genetic algorithm to begin.
The population size has to be divisible by 4 as we are going to select exactly half of it and then need to form crossover pairs from this selected half.

2.Select Parents such that half of the population is selected
selectParents assigns fitness to each chromosome in the population using the fitness function. Top 2 chromosomes in terms of fitness are always selected (called as Elitism). Then (half population - 2) chromsomes are selected with probability of selection proportinal to fitness of chromosome.

3.Crossover
Crossover function takes 2 chromosomes as arguments and implements crossover at a random location with probability 0.6
So crossover will only hapen 60% of times between 2 randomly selected chromosomes.

4.Mutation
With a probability of 0.005, mutation can occur at random location in the chromosome supplied as argument to mutation function.

As outlined above, an initial population is generated before Genetic Algorithm can begin. Then Genetic algorithm is performed in an iterative fashion: a.A termination condition is checked. If it is met then GA is stopped. The termination condition used here is number of generations (iterations of GA). b.Fitness is calculated for each chromosome. An Avg Population fitness is also calculated equal to total fitness of the population/total size of population. c.Half of the population is selected based on fitness. This half is called the parent population. d.Within this half population (parents), pairs are randomly formed and crossover and mutation is executed for these pairs. e.After crossover and mutation steps, the resulting progeny chromosomes are put in the same set as the Parent chromosomes to give back the original population number. Thus we get the next generation (or population). All these steps keep iterating until th etermination condition is met.

We can see that the Avg fitness of the population keeps increasing (in each iteration) indicating that Genetic Algorithm is successfully working giving rise to populations which are fitter than the previous ones.

The final population thus, would have chromosomes which are generally fitter and have better AUC Vs. Model order tradeoff than Starting population for fitting logistic regression model. As a chromosome corresponds to a particular variable combination, we have used genetic algorithm to select variables for logistic regression.

The above algorithm is implemented in Genetic_algorithm_local.R

Now taking this one step further, to leverage the above method for Big data, I have also implemented it on hadoop using mapreduce framework. While there can be various ways to do this as per the paper : http://arxiv.org/pdf/1312.0086.pdf,
I have used mapper for load balancing so that almost equal number of observations are passed to each reducer. Then the whole Genetic algorithm is run within each reducer independently and results are collected as output. Load balancing in mapper is achieved by sampling keys for each observation using uniform random sampling. As all observations with same key go to same reducer in Hadoop, due to uniform random sampling, we will be able to uniformly allocate bservations across the reducers.
