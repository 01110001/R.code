# Load library
library(quantmod)
library(tidyverse)
library(PerformanceAnalytics)
library(xts)
library(ggplot2)
library(dplyr)

#--------------------------RETURN TABLE BY MONTH----------------------->
#Import data from yahoo
getSymbols("SPY",periodicity = "monthly")
#Calculate Monthly return + adding column to the data frame
SPY$monthlyret <- dailyReturn(SPY)
#return a matrix of monthly return column = month, row = year
table.return <- table.CalendarReturns(SPY$monthlyret) %>% as.data.frame() %>% select(-monthlyret)


#Since there is no function to calculate monthly drawdown I have to create it
calculate_monthly_drawdown <- function(data) {
  library(PerformanceAnalytics)
  data <- as.xts(data) #here I need to make sure the data are in time series and not data frame
  
  drawdown <- apply.monthly(data, function(x) maxDrawdown(x, geometric = FALSE))
  #apply.monthly is used to group the data by month
  return(drawdown)
  
}

#-------------------------DRAWDOWN TABLE BY MONTH------------------------------------->

#Here I have reloaded the SPY data for calculating the drawdown
getSymbols("SPY")
SPY$dailyret <- dailyReturn(SPY)
calculate_monthly_drawdown(SPY$dailyret)
monthly_drawdown <- calculate_monthly_drawdown(SPY$dailyret)


table.drawdown <- table.CalendarReturns(monthly_drawdown) %>% as.data.frame() %>% select(-dailyret)


#HERE IS JUST BASIC STATISTIC YOU CAN TEST ON DRAWDOWN OR RETURN BY MONTH

# Calculate the drawdown mean OR median or sd of each column(month)
mean.monthly.drawdown <- apply(table.drawdown,2, median) %>% as.data.frame()
mean.monthly.drawdown
mean.monthly.drawdown[,0]




#ploting the box plot

plot.drawdown <- boxplot(table.drawdown, main = "Monthly Drawdown SPY Boxplot", 
        sub = "From 2007-2022", xlab ="Month", ylab = "Drawdown in %")

plot.return <-boxplot(table.return, main = "Monthly Return SPY Boxplot", 
                       sub = "From 2007-2022", xlab ="Month", ylab = "Return in %")


#--------------------------BACKTESTING-------------------------------->
#NO NEED TO LAG THE SIGNAL WHEN BACKTESTING SEASONALITY STRATEGY
# Implement the trading strategy
SPY$trades <- ifelse(months.Date(index(SPY)) == "décembre", 1,
                     ifelse(months.Date(index(SPY)) == "janvier", 1, 0))

#THE LINE OF CODE ABOVE IS FOR ENTRY AND EXIT 1 = ENTRY, 0 = EXIT.
#SO BASICALLY IT WILL CUMULATE RETURN FOR DECEMBER AND JANUARY ONLY

# Calculate the return
strat.return <- SPY$trades * SPY$monthly.return
strat.return$drawdown <- Drawdowns(strat.return)

#convert to dataframe + REMOVING 0 FROM DATAFRAME, 0= NO TRADE
strat.return <- strat.return %>% as.data.frame() %>% filter(trades != 0)
strat.return <- strat.return %>% as.xts()

#-------------------------BACKTESTING RESULT----------------------------->
maxDrawdown(strat.return$trades)

cumsum(strat.return$trades)

mean(strat.return$trades)

VaR(strat.return$trades, p = 0.95)

UpsideFrequency(strat.return$trades)


table.DrawdownsRatio(strat.return$trades)

mean(strat.return$drawdown)
