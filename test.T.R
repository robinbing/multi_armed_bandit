source('Season.R')
num.test = 500 
price = 1:20
output.mean = runif(1:(20 * 10000), 1, 20)
crop.info = list( price = price,
                  cost =  price / runif(20, 1, 1.5),
                  output.mean = output.mean,
                  output.sd = output.mean )
temperature = 50
summer = Season$new(num.test, crop.info, temperature)

summer$updateCandidate()

sort(summer$candidates$probability, decreasing = TRUE)[1:100]
sort(summer$candidates$avg.reward, decreasing = TRUE)[1:100]

summer$returnSumReward()

#####
num.test = 500 
price = 1:20
output.mean = runif(1:(20 * 10000), 1, 20)
crop.summer = list( price = price,
                  cost =  price / runif(20, 1, 1.5),
                  output.mean = output.mean,
                  output.sd = output.mean )
temperature = 10
summer = Season$new(num.test, crop.summer, temperature)
winter = fall = summer

um.test = 500 
price = 1:20
output.mean = runif(1:(20 * 10000), 1, 20)
crop.spring = list( price = price,
                  cost =  price / runif(20, 1, 1.5),
                  output.mean = output.mean/1.2,
                  output.sd = output.mean )
temperature = 50
spring = Season$new(num.test, crop.spring, temperature)

