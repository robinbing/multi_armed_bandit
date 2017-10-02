library(parallel)
library(reshape2)
library(ggplot2)

allocSeason = 
  #
  # count the numbers of tests for each season in top highest rewards
  #
  function(spring, summer, fall, winter, pct) {
    # Prepare reward dataframe
    seasons = c(spring, summer, fall, winter)
    mix.reward = lapply(1:4, 
                        function(x) {
                          data.frame(season = x,
                                     reward = sort(seasons[[x]]$candidates$avg.reward[seasons[[x]]$candidates$avg.reward != 0], 
                                                   decreasing = TRUE)
                                     )
                        })
    mix.reward = Reduce(rbind, mix.reward)
    # sort and filter by pct
    mix.reward = mix.reward[order(mix.reward$reward, decreasing = TRUE), ]
    mix.reward = mix.reward[mix.reward$reward >= quantile(mix.reward$reward, 1 - pct), ]
    
    # do counts
    counts = table(mix.reward$season, dnn = 'season')
    
    return(counts)
  }

mainProcess =
  #
  # Main process.
  #
  function(crop.spring, crop.summer, crop.fall, crop.winter, temperature, pct) {
    # initialize vairables
    season.test.num = rep(250, 4)
    season.cum.reward = NULL
    spring = Season$new(season.test.num[1], crop.spring, temperature)
    summer = Season$new(season.test.num[2], crop.summer, temperature)
    fall = Season$new(season.test.num[3], crop.fall, temperature)
    winter = Season$new(season.test.num[4], crop.fall, temperature)
    
    # start loop
    for(i in 1:20) {
      # update temperature
      tmp = temperature / i
      spring$temperature = summer$temperature = fall$temperature = winter$temperature = tmp
      
      # update season.test.num
      spring$num.test = season.test.num[1]
      summer$num.test = season.test.num[2]
      fall$num.test = season.test.num[3]
      winter$num.test = season.test.num[4]
      
      # running tests
      spring$updateCandidate()
      summer$updateCandidate()
      fall$updateCandidate()
      winter$updateCandidate()
      
      # record
      season.cum.reward = rbind(season.cum.reward,
                                c(spring$returnSumReward(), summer$returnSumReward(),
                                  fall$returnSumReward(), winter$returnSumReward()))
      
      # update season.test.num
      high.rank.pct = allocSeason(spring, summer, fall, winter, pct)
      high.rank.pct = high.rank.pct / sum(high.rank.pct) * 1000
      season.test.num.tmp = season.test.num + 1 / i * (high.rank.pct - season.test.num)
      season.test.num.tmp = sample(1:4, size = 1000, replace = TRUE, prob = season.test.num.tmp)
      season.test.num = table(season.test.num.tmp)
    }
    
    return(list(
      season.cum.reward = season.cum.reward,
      season.test.num = season.test.num,
      spring = spring,
      summer = summer,
      fall = fall,
      winter = winter
    ))
  }

compareRun = 
  #
  # compare different parameter settings
  #
  function(crop.spring, crop.summer, crop.fall, crop.winter, object = c('pct', 'temperature'), value, len) {
    # check test boject
    object = match.arg(object, several.ok = FALSE)
    if(object == 'pct') {
      pct = rep(value, each = len)
      temperature = rep(30, length(pct))
    } else {
      temperature = rep(value, each = len)
      pct = rep(0.5, length(temperature))
    }
    
    # paralle testing
    cl = makeCluster(4)
    clusterExport(cl, varlist = c('crop.spring', 'crop.summer', 'crop.fall', 'crop.winter', 'pct',
                                  'temperature'),
                  envir = environment())
    clusterExport(cl, varlist = c('mainProcess', 'allocSeason', 'Season'), envir = globalenv())
    clusterCall(cl, library, 'msm', character.only = TRUE)
    reward = parSapply(cl, 1:length(pct), 
                       function(x) {
                         print(x)
                         result = mainProcess(crop.spring, 
                                              crop.summer, 
                                              crop.fall, 
                                              crop.winter, 
                                              temperature = temperature[x], 
                                              pct = pct[x])
                         return(sum(result$season.cum.reward[20, ]))
                       })
    stopCluster(cl)
    
    # prepare output 
    output.long = data.frame(object = get(object),
                             reward = reward)
    output = melt(output.long, id.vars = 'object')
    box.chart = ggplot(data = output, aes(x = as.factor(object), y = value)) +
      geom_boxplot(aes(fill = object)) +
      xlab(object) +
      ylab('reward')
    
    # return
    return(list(
      box.chart = box.chart,
      reward = output.long
    ))
  }

randomPick =
  #
  # simulate running test randomly
  # 
  function(crop.spring, crop.summer, crop.fall, crop.winter) {
    # define season
    seasons = list(crop.spring, crop.summer, crop.fall, crop.winter)
    # define select 
    selected.row = sample(1:20, size = 1000, replace = TRUE)
    selected.col = sample(1:10000, size = 1000, replace = FALSE)
    selected.season = sample(1:4, size = 1000, replace = TRUE)
    
    # comput reward
    reward = lapply(1:4, 
                    function(x) {
                      index = selected.season == x
                      row = selected.row[index]
                      col = selected.col[index]
                      selected = (col - 1) * 20 + row
                      
                      output = rtnorm(n = length(row),
                                      mean = seasons[[x]]$output.mean[selected],
                                      sd = seasons[[x]]$output.sd[selected],
                                      lower = 0)
                      
                      reward = output * (seasons[[x]]$price[row] - seasons[[x]]$cost[row])
                    })
    total = Reduce(sum, reward)
    return(total)
  }

