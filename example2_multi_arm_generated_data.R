library(tidyverse)
library(grf)
library(policytree)

n <- 500
p <- 10
X <- matrix(rnorm(n * p), n, p)
W <- as.factor(sample(c("A", "B", "C"), n, replace = TRUE))
Y <- X[, 1] + X[, 2] * (W == "B") - 1.5 * X[, 2] * (W == "C") + rnorm(n)

data <- tibble(data.frame(X), W, Y)
head(data)

mc.forest <- multi_arm_causal_forest(X, Y, W, 
                                     W.hat = matrix(1 / 3, nrow = n, ncol = 3))

average_treatment_effect(mc.forest) # average of ite's
ite <- double_robust_scores(mc.forest) # this function gives a matrix of ite's
rewards <- conditional_means(mc.forest) # not sure about this function
cate <- predict(mc.forest)$prediction # Heterogenous treatment effect
cate_mat <- matrix(data = cate, nrow = n)

#fitting policy tree and using that to find the potential gain by optimal allocation
tree <- hybrid_policy_tree(
  X = X, Gamma = cate_mat, depth = 3, min.node.size = 20
)

pi_hat <- predict(tree, X) # recommended allocation of treatments


new_dat <- tibble(
  X,
  W,
  Y,
  pi_hat,
  A = cate[1:500],
  B = cate[501:1000],
  C = cate[1001:1500]
)

new_dat <- new_dat |> 
  mutate(pi_hat = case_when(
    pi_hat == 1 ~ "A",
    pi_hat == 2 ~ "B",
    pi_hat == 3 ~ "C"
  ))

new_dat |> 
  summarise(
    mean(W == pi_hat)
  ) # just wanted to check how many were already allocated accurately

# judging the effectiveness of optimal allocation
new_dat |> 
  mutate(
    new_effect = case_when(
      pi_hat == "A" ~ A,
      pi_hat == "B" ~ B,
      pi_hat == "C" ~ C
    )
  ) |> 
  mutate(
    new_Y = Y + new_effect
  ) |> 
  summarise(
    old_Y_mean = mean(Y),
    new_Y_mean = mean(new_Y),
    old_Y_sum = sum(Y),
    new_Y_sum = sum(new_Y)
  )
