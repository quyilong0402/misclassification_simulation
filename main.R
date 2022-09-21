library(tidyverse)
source("/home/lele/Documents/DUKE/721/simulation/utils.R")

total = 2000
n_exp = 1000
r1 = 0.2
r2 = 0.4
bias = c(40, 0)

# construct a study object with utils.R constructor
this_study = constructer(total, n_exp, r1, r2)

# visualize true
true_table = true_table(this_study)

# visualize bias

bias_vector = bias_table(this_study, bias)

# generate df
result = data.frame(cbind(true_vector, bias_vector))
rownames(result) = c('RR','RD','OR','OD')
result