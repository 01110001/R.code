# Load library
library(quantmod)
library(tidyverse)
library(PerformanceAnalytics)
library(xts)
library(ggplot2)
library(dplyr)



getSymbols("SPY",periodicity = "monthly")

SPY$monthlyret <- dailyReturn(SPY)

table.return <- table.CalendarReturns(SPY$monthlyret) %>% as.data.frame() %>% select(-monthlyret)



calculate_monthly_drawdown <- function(data) {
  library(PerformanceAnalytics)
  data <- as.xts(data)
  
  drawdown <- apply.monthly(data, function(x) maxDrawdown(x, geometric = FALSE))
  
  return(drawdown)
  
}

getSymbols("SPY")

SPY$dailyret <- dailyReturn(SPY)

calculate_monthly_drawdown(SPY$dailyret)

monthly_drawdown <- calculate_monthly_drawdown(SPY$dailyret)


table.drawdown <- table.CalendarReturns(monthly_drawdown) %>% as.data.frame() %>% select(-dailyret)

# Calculate the drawdown mean of each column(month)
mean.monthly.drawdown <- apply(table.drawdown,2, median) %>% as.data.frame()
mean.monthly.drawdown
mean.monthly.drawdown[,0]

# Calculate the return mean of each column(month)


#ploting

plot.drawdown <- boxplot(table.drawdown, main = "Monthly Drawdown SPY Boxplot", 
        sub = "From 2007-2022", xlab ="Month", ylab = "Drawdown in %")

plot.return <-boxplot(table.return, main = "Monthly Return SPY Boxplot", 
                       sub = "From 2007-2022", xlab ="Month", ylab = "Return in %")



# Implement the trading strategy
SPY$trades <- ifelse(months.Date(index(SPY)) == "décembre", 1,
                     ifelse(months.Date(index(SPY)) == "janvier", 1, 0))

# Calculate the return
strat.return <- SPY$trades * SPY$monthly.return



strat.return$drawdown <- Drawdowns(strat.return)

#convert to dataframe + deleting 0000
strat.return <- strat.return %>% as.data.frame() %>% filter(trades != 0)

strat.return <- strat.return %>% as.xts()

maxDrawdown(strat.return$trades)

cumsum(strat.return$trades)

mean(strat.return$trades)

VaR(strat.return$trades, p = 0.95)

UpsideFrequency(strat.return$trades)


table.DrawdownsRatio(strat.return$trades)

mean(strat.return$drawdown)

