## Group Katie Scheer, Bradley Mika, Varun, Dante Quinones

library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)

IBM_Data = read.csv(file = 'Desktop/Midterm/IBM.csv')[, c("Close", "Adj.Close")]
names(IBM_Data) <- c("IBM.Close","IBM.Adj.Close")

MSFT_Data = read.csv(file = 'Desktop/Midterm/MSFT.csv')[, c("Close", "Adj.Close")]
names(MSFT_Data) <- c("MSFT.Close","MSFT.Adj.Close")

GOOG_Data = read.csv(file = 'Desktop/Midterm/GOOG.csv')[, c("Close", "Adj.Close")]
names(GOOG_Data) <- c("GOOG.Close","GOOG.Adj.Close")

AAPL_Data = read.csv(file = 'Desktop/Midterm/AAPL.csv')[, c("Close", "Adj.Close")]
names(AAPL_Data) <- c("AAPL.Close","AAPL.Adj.Close")

AMZN_Data = read.csv(file = 'Desktop/Midterm/AMZN.csv')[, c("Close", "Adj.Close")]
names(AMZN_Data) <- c("AMZN.Close","AMZN.Adj.Close")

META_Data = read.csv(file = 'Desktop/Midterm/META.csv')[, c("Close", "Adj.Close")]
names(META_Data) <- c("META.Close","META.Adj.Close")

NFLX_Data = read.csv(file = 'Desktop/Midterm/NFLX.csv')[, c("Close", "Adj.Close")]
names(NFLX_Data) <- c("NFLX.Close","NFLX.Adj.Close")

TSLA_Data = read.csv(file = 'Desktop/Midterm/TSLA.csv')[, c("Close", "Adj.Close")]
names(TSLA_Data) <- c("TSLA.Close","TSLA.Adj.Close")

ORCL_Data = read.csv(file = 'Desktop/Midterm/ORCL.csv')[, c("Close", "Adj.Close")]
names(ORCL_Data) <- c("ORCL.Close","ORCL.Adj.Close")

SAP_Data = read.csv(file = 'Desktop/Midterm/SAP.csv')[, c("Close", "Adj.Close")]
names(SAP_Data) <- c("SAP.Close","SAP.Adj.Close")


#Retrieves dataframe with Close and Adj Close prices for the 10 stocks on each day
universe = cbind(IBM_Data, MSFT_Data, GOOG_Data, AAPL_Data, AMZN_Data, META_Data, NFLX_Data, TSLA_Data, ORCL_Data, SAP_Data)
universe

# function to buy stocks for given day

buyStocks = function(df) {
  temp = data.frame(stock=df[["stock"]], amount = 0, remainingCash = 0)
  stock = paste(temp[["stock"]] , ".Close", sep="")
  day = df[["day"]]
  price = universe[[stock]][day]
  
  numberOfStockPurchased = df[["cash"]] %/% price
  temp$amount = numberOfStockPurchased
  cashLeft = df[["cash"]] - (numberOfStockPurchased * price)
  
  temp$remainingCash = cashLeft
  return(temp)
}


# function to get stock value for given day
stockValue = function(stock, day){
  stock = paste(stock , ".Close", sep="")
  price = universe[[stock]][day]
  return(price)
}

#function to calculate MTM
mtmValue = function(today, cash, stocks, shares){
  
  total = 0
  for (x in 1:length(stocks)){
    value = stockValue(stocks[x], today) * as.numeric(shares[x])
    total = total + value
  }
  
  total = total + cash
  return(total)
  
}

#function to find dropped the most
droppedMost = function(today, interval){
  mydata = universe
  mydata = select(mydata,ends_with("Adj.Close"))
  
  droppedmost = mydata %>% slice(today-interval,today) %>%
    mutate(IBM = (IBM.Adj.Close - lag(IBM.Adj.Close))/lag(IBM.Adj.Close) * 100) %>%
    mutate(AMZN = (AMZN.Adj.Close - lag(AMZN.Adj.Close))/lag(AMZN.Adj.Close) * 100) %>%
    mutate(AAPL = (AAPL.Adj.Close - lag(AAPL.Adj.Close))/lag(AAPL.Adj.Close) * 100) %>%
    mutate(FB = (FB.Adj.Close - lag(FB.Adj.Close))/lag(FB.Adj.Close) * 100) %>%
    mutate(GOOG = (GOOG.Adj.Close - lag(GOOG.Adj.Close))/lag(GOOG.Adj.Close) * 100) %>%
    mutate(MSFT = (MSFT.Adj.Close - lag(MSFT.Adj.Close))/lag(MSFT.Adj.Close) * 100) %>%
    mutate(NFLX = (NFLX.Adj.Close - lag(NFLX.Adj.Close))/lag(NFLX.Adj.Close) * 100) %>%
    mutate(ORCL = (ORCL.Adj.Close - lag(ORCL.Adj.Close))/lag(ORCL.Adj.Close) * 100) %>%
    mutate(SAP = (SAP.Adj.Close - lag(SAP.Adj.Close))/lag(SAP.Adj.Close) * 100) %>%
    mutate(TSLA = (TSLA.Adj.Close - lag(TSLA.Adj.Close))/lag(TSLA.Adj.Close) * 100) %>%
    select(-contains(".Adj.Close")) %>%
    slice(2)
  allCols = sort(droppedmost[1,1:10])
  cols = c(colnames(allCols))
  return(cols[1:5])
}

#function to find surged the most
surgedMost = function(today, interval){
  mydata = universe
  mydata = select(mydata,ends_with("Adj.Close"))
  
  surgedMost = mydata %>% slice(today-interval,today) %>%
    mutate(IBM = (IBM.Adj.Close - lag(IBM.Adj.Close))/lag(IBM.Adj.Close) * 100) %>%
    mutate(AMZN = (AMZN.Adj.Close - lag(AMZN.Adj.Close))/lag(AMZN.Adj.Close) * 100) %>%
    mutate(AAPL = (AAPL.Adj.Close - lag(AAPL.Adj.Close))/lag(AAPL.Adj.Close) * 100) %>%
    mutate(FB = (FB.Adj.Close - lag(FB.Adj.Close))/lag(FB.Adj.Close) * 100) %>%
    mutate(GOOG = (GOOG.Adj.Close - lag(GOOG.Adj.Close))/lag(GOOG.Adj.Close) * 100) %>%
    mutate(MSFT = (MSFT.Adj.Close - lag(MSFT.Adj.Close))/lag(MSFT.Adj.Close) * 100) %>%
    mutate(NFLX = (NFLX.Adj.Close - lag(NFLX.Adj.Close))/lag(NFLX.Adj.Close) * 100) %>%
    mutate(ORCL = (ORCL.Adj.Close - lag(ORCL.Adj.Close))/lag(ORCL.Adj.Close) * 100) %>%
    mutate(SAP = (SAP.Adj.Close - lag(SAP.Adj.Close))/lag(SAP.Adj.Close) * 100) %>%
    mutate(TSLA = (TSLA.Adj.Close - lag(TSLA.Adj.Close))/lag(TSLA.Adj.Close) * 100) %>%
    select(-contains(".Adj.Close")) %>%
    slice(2)
  allCols = sort(surgedMost[1,1:10])
  cols = c(colnames(allCols))
  return(cols[6:10])
}

#function to calculate percent change
percentChange = function(prev,curr){
  p = curr - prev
  p = p/prev
  return(p*100)
}

# Initialize variables
buyStocksDf = data.frame(day = 12, stock = "IBM", cash = 10000)
stocks = list()
shares = list()
totalCash = 5000000
dayInterval = 5
mtmprev = 0
mtmcurr = 0
mtm1 = list()
mtm1Val = list()

# 5 days rebalancing of buying low
for (today in 1:NROW(date)) {
  
  if (today %% dayInterval == 1) {
    
    # Initialize day
    if (today == 1) {
      stocks = list('IBM', 'MSFT', 'GOOG', 'AAPL', 'AMZN')
      splitCash = totalCash/5
      totalCash = 0
      
      for (x in stocks) {
        buyStocksDf = data.frame(day = today, stock = x, cash = splitCash)
        stockDf = buyStocks(buyStocksDf)
        
        totalCash = totalCash + stockDf$remainingCash
        shares = append(shares, stockDf$amount)
      }
    }
    
    # Rebalance
    else {
      # Find the 5 stocks whose "Adj Close" prices dropped the most
      mostDropped = droppedMost(today, dayInterval)
      
      # Sell all current holdings
      for (x in stocks) {
        totalCash = totalCash + stockValue(x, today)
      }
      stocks = list()
      shares = list()
      # Split your cash
      splitCash = totalCash/5
      
      # Buy the max shares of stock
      stocks = mostDropped
      for (x in stocks) {
        buyStocksDf = data.frame(day = today, stock = x, cash = splitCash)
        stockDf = buyStocks(buyStocksDf)
        
        totalCash = totalCash + stockDf$remainingCash
        shares = append(shares, stockDf$amount)
      }
    }
  }
  
  mtmprev = mtmcurr
  # USD to JPY
  mtmcurr = mtmValue(today, totalCash, stocks, shares)
  mtm1Val = append(mtm1Val, mtmcurr)
  mtm1 = append(mtm1, percentChange(mtmprev, mtmcurr))
}


#total
plot((2:250),mtm1Val[-1])
#percent change
plot((2:250),mtm1[-1])
#function for high-tech index
highTech = function(){
  ht = universe
  htday = ht %>% select(1,3,5,7,9,11,13,15,17,19)
  return(rowMeans(htday))
}
myHighTechAvg = highTech()
myHighTechPercent = list()
for(x in 2:250){
  myHighTechPercent = append(myHighTechPercent, percentChange(x-1,x))
}
#High-Tech Stock Percentage Comparison
plot((2:250),myHighTechPercent)

# 5 days rebalancing of buying high
# initialize variables
buyStocksDf = data.frame(day = 12,stock = "IBM",cash = 10000)
stocks = list()
shares = list()
totalCash = 5000000
dayInterval = 5
mtmprev = 0
mtmcurr = 0
mtm1 = list()
mtm1Val = list()

# 5 days rebalancing of buying low
for (today in 1:NROW(date)){
  
  
  if(today %% dayInterval == 1){
    
    #initial day
    if(today == 1){
      stocks = list('IBM', 'MSFT', 'GOOG', 'AAPL', 'AMZN')
      splitCash = totalCash/5
      totalCash = 0
      
      for(x in stocks){
        buyStocksDf = data.frame(day = today ,stock = x,cash = splitCash)
        stockDf = buyStocks(buyStocksDf)
        
        totalCash = totalCash + stockDf$remainingCash
        shares = append(shares, stockDf$amount)
      }
    }
    
    #rebalance
    else {
      #find the 5 stocks whose "Adj Close" prices dropped the most 
      mostDropped = surgedMost(today,dayInterval)
      
      
      #sell all current holdings
      for(x in stocks){
        totalCash = totalCash + stockValue(x, today)
        
      }
      stocks = list()
      shares = list()
      #split your cash
      splitCash = totalCash/5
      
      #buy the max shares of stock
      stocks = mostDropped
      
      for(x in stocks){
        buyStocksDf = data.frame(day = today ,stock = x,cash = splitCash)
        stockDf = buyStocks(buyStocksDf)
        
        totalCash = totalCash + stockDf$remainingCash
        shares = append(shares, stockDf$amount)
      }
    }
    
  }
  mtmprev = mtmcurr
  #USD to JPY
  mtmcurr = mtmValue(today, totalCash, stocks, shares)
  mtm1Val = append(mtm1Val, mtmcurr)
  mtm1 = append(mtm1, percentChange(mtmprev,mtmcurr))
  
}

#total          
plot((2:250),mtm1Val[-1])
#percent change
plot((2:250),mtm1[-1])
#function for high-tech index
highTech = function(){
  ht = universe
  htday = ht %>% select(1,3,5,7,9,11,13,15,17,19)
  return(rowMeans(htday))
}
myHighTechAvg = highTech()
myHighTechPercent = list()
for(x in 2:250){
  myHighTechPercent = append(myHighTechPercent, percentChange(x-1,x))
}
#High-Tech Stock Percentage Comparison
plot((2:250),myHighTechPercent)


