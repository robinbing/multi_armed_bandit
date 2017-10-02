library(R6)
library(msm)

Season = R6Class("Season",
                 
                 public = list(
                   num.test = NULL,
                   candidates = NULL,
                   crop.info = NULL,
                   temperature = NULL,
                   
                   # Initialize function
                   initialize = function(num.test, crop.info, temperature) {
                     self$num.test = num.test
                     self$crop.info = crop.info
                     self$candidates = list(count = rep(0, 20 * 10000),
                                            avg.reward = rep(0, 20 * 10000),
                                            true.reward = rep(0, 20 * 10000),
                                            probability = rep(1 / (20 * 10000), 20 * 10000))
                     self$temperature = temperature
                   },
                   
                   # Select tests crops and location
                   selectTest = function() {
                     # Transfer to matrix: row:crop, colum: location
                     prob.mat = matrix(self$candidates$probability, nrow = 20)
                     # select location first based on marginal prob
                     prob.column = apply(prob.mat, 2, sum)
                     selected.column = sample(10000, size = self$num.test, prob = prob.column, replace = FALSE)
                     # select crop for each column
                     selected.row = sapply(selected.column, 
                                           function(x) {
                                             sample(1:20, 1, prob = prob.mat[, x])
                                           })
                     selected = (selected.column - 1) * 20 + selected.row
                     return(selected)
                   },
                   
                   # Softmax transfermation
                   softMax = function(x) {
                     log.sum.exp = max(x) + log(sum(exp(x - max(x))))
                     transformed = exp(x - log.sum.exp)
                     return(transformed)
                   },
                   
                   # Get reward: output x (price – cost)
                   getReward = function(selected) {
                     # prepare row, column index
                     selected.col = floor((selected - 1) / 20) + 1
                     selected.row = selected %% 20
                     selected.row[selected.row == 0] = 20
                     
                     # compute reward
                     output = rtnorm(n = length(selected),
                                     mean = self$crop.info$output.mean[selected],
                                     sd = self$crop.info$output.sd[selected],
                                     lower = 0)
                     reward = output * (self$crop.info$price[selected.row] - self$crop.info$cost[selected.row])
                     
                     return(reward)
                   },
                   
                   # update candidates
                   updateCandidate = function() {
                     selected = self$selectTest()
                     # update count
                     self$candidates$count[selected] = self$candidates$count[selected] + 1
                     # update avgreward
                     self$candidates$avg.reward[selected] = self$candidates$avg.reward[selected] + 
                       1 / self$candidates$count[selected] * 
                       (self$getReward(selected) - self$candidates$avg.reward[selected])
                     # update probability 
                     self$candidates$probability = self$softMax(self$candidates$avg.reward / self$temperature)
                     # update true reward
                     self$candidates$true.reward[selected] = self$candidates$true.reward[selected] + self$getReward(selected)
                   },
                   
                   # return total reward
                   returnSumReward = function() {
                     return(
                       sum(self$candidates$true.reward)
                     )
                   }
                   
                 ))