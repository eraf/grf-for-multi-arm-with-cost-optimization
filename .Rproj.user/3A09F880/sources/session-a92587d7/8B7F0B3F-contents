library(tidyverse)
library(grf)
library(policytree)
library(DiagrammeR)

data <- read_csv("https://raw.githubusercontent.com/grf-labs/grf/refs/heads/master/r-package/grf/vignettes/data/bruhn2016.csv")
head(data)
names(data)

Y <- data$outcome.test.score
W <- data$treatment
school <- data$school
X <- data[-(1:3)]

# it was a completely randomized trial with known propensity score w = .5
cf <- grf::causal_forest(X, Y, W, W.hat = .5, clusters = school)
average_treatment_effect(cf)

ranked_vars <- order(variable_importance(cf), decreasing = T)
colnames(X)[ranked_vars[1:5]]

blp <- best_linear_projection(cf, X[ranked_vars])

# policy tree
samples.by.school <- split(seq_along(school), school)
num.schools <- length(samples.by.school)
train <- unlist(samples.by.school[sample(1:num.schools, num.schools / 2)])
eval <- (1:nrow(X))[-train]
not.missing <- which(complete.cases(X))
train <- train[which(train %in% not.missing)]
eval <- eval[which(eval %in% not.missing)]

dr.scores <- get_scores(cf)
ate <- average_treatment_effect(cf)
cost <- ate[["estimate"]]
dr.rewards <- cbind(control=-dr.scores, treat=dr.scores - cost)

tree <- policy_tree(X = X[train, ], Gamma = dr.rewards[train, ], min.node.size = 100)
plot(tree)


# depth-1 tree

tree.depth1 <- policy_tree(X[train, ], dr.rewards[train, ], depth = 1, 
                           min.node.size = 100)
pi.hat.eval <- predict(tree.depth1, X[eval, ]) - 1

treat.eval <- eval[pi.hat.eval == 1]
dont.treat.eval <- eval[pi.hat.eval == 0]

average_treatment_effect(cf, subset = treat.eval)
#> estimate  std.err 
#>      3.9      0.8
average_treatment_effect(cf, subset = dont.treat.eval)
#> estimate  std.err 
#>      1.8      2.3

plot(tree.depth1)


# tree depth 2
tree.depth2 <- policy_tree(X[train, ], dr.rewards[train, ], depth = 2, 
                           min.node.size = 100)
pi.hat.eval <- predict(tree.depth2, X[eval, ]) - 1

treat.eval <- eval[pi.hat.eval == 1]
dont.treat.eval <- eval[pi.hat.eval == 0]

average_treatment_effect(cf, subset = treat.eval)
average_treatment_effect(cf, subset = dont.treat.eval)
plot(tree.depth2)
