
2 + 3

[3:50]

2 * 3 * 4 * 5  # multiply
sqrt(10)       # square root
pi             # pi
sqrt(-2)

y = 5;    # stores the value 5 in y
3 + y

fred = [74, 31, 95, 61, 76, 34, 23, 54, 96]

fred[3]
fred[2:4]
fred[[1, 3, 5, 8]]

simpsons = ["Homer", "Marge", "Bart", "Lisa", "Maggie"];
typeof(simpsons)

x = 5;
x >= 6

sum(fred)
length(fred)
sum(fred)/length(fred)
mean(fred)   # sample mean, should be same answer

mary = [4, 5, 3, 6, 4, 6, 7, 3, 1];
fred + mary
fred - mean(mary)

fred.*mary

fred'*mary

help("factorial")

using RDatasets, DataFrames, Distributions, GLM

trees = data("datasets", "trees")
treeslm = lm(:(Girth ~ Height + Volume), trees)

coef(treeslm)

coeftable(treeslm)
