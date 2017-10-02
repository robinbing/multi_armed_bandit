source("runTest.R")
source("Season.R")


# 1. Prepare variable -----------------------------------------------------

price = 1:20
# spring
output.mean = runif(1:(20 * 10000), 1, 20)
crop.spring = list( price = price,
                    cost =  price / runif(20, 1, 1.5),
                    output.mean = output.mean / 1,
                    output.sd = output.mean )

# summer
output.mean = runif(1:(20 * 10000), 1, 20)
crop.summer = list( price = price,
                    cost =  price / runif(20, 1, 1.5),
                    output.mean = output.mean / 1.1,
                    output.sd = output.mean )

# fall
output.mean = runif(1:(20 * 10000), 1, 20)
crop.fall = list( price = price,
                    cost =  price / runif(20, 1, 1.5),
                    output.mean = output.mean / 0.9,
                    output.sd = output.mean )

# winter
output.mean = runif(1:(20 * 10000), 1, 20)
crop.winter = list( price = price,
                    cost =  price / runif(20, 1, 1.5),
                    output.mean = output.mean / 1.2,
                    output.sd = output.mean )


# 2. Run tests ------------------------------------------------------------

# a. compare different parameters
test.pct = compareRun(crop.spring, crop.summer, crop.fall, crop.winter, object = 'pct', 
                      value = c(0.3, 0.5, 0.7), len = 20)
test.temperature = compareRun(crop.spring, crop.summer, crop.fall, crop.winter, object = 'temperature', 
                              value = c(50, 30, 10), len = 20)
test.pct$box.chart
test.temperature$box.chart

# b. multi-armed vs. random
test.muli_arm = sapply(1:15, 
                       function(x) {
                         multi_armed = mainProcess(crop.spring, crop.summer, crop.fall, crop.winter, temperature = 50, pct = 0.5)
                         sum(multi_armed$season.cum.reward[20, ])
                       })

test.random = sapply(1:15,
                     function(x) {
                       sum(sapply(1:20, 
                                  function(x){
                                    randomPick(crop.spring, crop.summer, crop.fall, crop.winter)
                                  }))
                     })
comp = data.frame(method = as.factor(c(rep('multi_arm', 15), rep('random', 15))),
                  value = c(test.muli_arm, test.random))
comp = melt(comp, id.vars = 'method')
ggplot(data = comp, aes(x = method, y = value)) +
  geom_boxplot(aes(fill = method)) +
  xlab('methods') +
  ylab('reward')
