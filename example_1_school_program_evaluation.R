library(tidyverse)
library(grf)
library(policytree)
library(DiagrammeR)

data <- read_csv("https://raw.githubusercontent.com/grf-labs/grf/refs/heads/master/r-package/grf/vignettes/data/bruhn2016.csv")

Y <- data$outcome.test.score
W <- data$treatment
school <- data$school
X <- data[-(1:3)]

# it was a completely randomized trial with known propensity score w = .5
cf <- grf::causal_forest(X, Y, W, W.hat = .5, clusters = school)

ranked_vars <- order(variable_importance(cf), decreasing = T)

best_linear_projection(cf, X[ranked_vars])

# policy tree
samples.by.school <- split(seq_along(school), school)
num.schools <- length(samples.by.school)
train <- unlist(samples.by.school[sample(1:num.schools, num.schools / 2)])
eval <- (1:nrow(X))[-train]
not.missing <- which(complete.cases(X))
train <- train[which(train %in% not.missing)]
eval <- eval[which(eval %in% not.missing)]

dr.scores <- get_scores(cf) #individual treatment effects
ate <- average_treatment_effect(cf) #average treatment effect
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
average_treatment_effect(cf, subset = dont.treat.eval)
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

ate_grf <- average_treatment_effect(cf, subset = treat.eval)

tibble(
  treatments = c(0, 1),
  regular_gain = c(NA, ate[["estimate"]]),
  optimized_gain = c(NA, ate_grf[["estimate"]])
)

get_scores(cf)
predict(cf)$prediction
