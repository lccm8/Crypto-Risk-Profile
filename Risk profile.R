options(java.parameters = "-Xmx2048m")
Sys.setlocale("LC_TIME","English_United States.1252")
options(scipen = 999)
rm(list=ls())         # Empty memory
library(plyr)         # Wrangling.
library(dplyr)        # Wrangling: tbl_df(), group_by(), print(), glimpse().
library(tidyr)        # Wrangling: gather().
library(purrr)        # cross_df for cross validation
library(magrittr)     # Pipe operator %>% %<>% %T>% equals().
library(lubridate)    # Date operations
library(httr)         # API connection
library(jsonlite)
library(readtext)
library(psych)
library(xlsx)         # Excel connection
library(openxlsx)
library(quantmod)     # CCY conversion
library(stringr)
library(timeDate)
library(Quandl)
library(PerformanceAnalytics)
library(LSPM)
library(ggplot2)
library(fBasics)
library(hrbrthemes)
options(stringsAsFactors = FALSE)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
  
Data <- read.csv("BSZ012.csv", sep = ";", stringsAsFactors = FALSE, row.names = NULL)
colnames(Data) <- c("Date","Pair","Side","Price","Quantity","Amount","Fee","RealizedProfit")
Data$Date %<>% substr(1,19) %>% strptime(format = "%d/%m/%Y %H:%M",tz = "utc")
Data$Quantity[which(Data$Side == "SELL")] %<>% multiply_by(-1)
Data$Fee %<>% str_remove(" USDT") %>% as.numeric()

bySymbol <- Data %>% split(Data$Pair)

Start <- min(Data$Date) %>% floor_date("hour")
Finish <- max(Data$Date) %>% ceiling_date("hour")
Dates <- seq((Start-(60*60)),Finish,"hour")
which(weekdays(Dates,abbreviate = TRUE) %in% c("Sat","Sun")) %>% length() %>% sum
  
PLbySymbol <- list()

for(s in 1:length(bySymbol)) {
  cat(paste("Processing", length(Dates), "hours of", names(bySymbol)[s], "- still", (length(bySymbol)-s),"to go","\n"))
  
  LeftFloating <- 0
  VWAP <- 0
  
  PL <- matrix(ncol = 8, nrow = length(Dates)) %>% as.data.frame()
  colnames(PL) <- c("TimeStamp","USDVol","PriceEOH","FloatingPL","NetFloating","ClosedPL","Cost","Total PL")
  PL$TimeStamp <- Dates
    
  Prices <- fromJSON(paste0("https://api.binance.com/api/v3/klines?symbol=",names(bySymbol)[s],"&interval=1h&startTime=",
                     (as.numeric(Dates[1])*1000),"&endTime=",(as.numeric(tail(Dates,1)))*1000)) %>% as.data.frame()
  Prices <- Prices[,c(1,5)]
  colnames(Prices) <- c("startTime", "close")
  Prices$startTime %<>% as.numeric %>% divide_by(1000) %>% as.POSIXct(origin="1970-01-01", tz = "utc")
  Prices$close %<>% as.numeric()
  
  Df.temp <- bySymbol[[s]]
  #
    
  for(d in 1:length(Dates)) {
    PL$USDVol[d] <- 0
    PL$ClosedPL[d] <- 0
    PL$FloatingPL[d] <- 0
    PL$NetFloating[d] <- 0
    PL$Cost[d] <- 0
    PL$`Total PL`[d] <- 0
    
    index <- which(Df.temp$Date >= PL$TimeStamp[d] & Df.temp$Date < (PL$TimeStamp[d]+(60*60)))
    
    if((length(index) > 0) | LeftFloating != 0) {
     if(length(index) > 0) {
      Df.today <- Df.temp[index,]
      Df.today <- Df.today[order(Df.today$Date),]
      for(t in 1:nrow(Df.today)) {
        # Close
        ClosedNow <- Df.today$RealizedProfit[t]
        PL$ClosedPL[d] <- PL$ClosedPL[d] + ClosedNow
        PL$ClosedPL[d] %<>% round(2)
        if((Df.today$Quantity[t] > 0 & LeftFloating >= 0) |
           (Df.today$Quantity[t] < 0 & LeftFloating <= 0)) {
          # Update VWAP, LeftFloating
          VWAP <- ((Df.today$Quantity[t]*Df.today$Price[t])+(LeftFloating*VWAP))/(LeftFloating+Df.today$Quantity[t])
          LeftFloating <- LeftFloating + Df.today$Quantity[t]
        }
        if((Df.today$Quantity[t] > 0 & LeftFloating < 0) |
           (Df.today$Quantity[t] < 0 & LeftFloating > 0)) {
          # Update VWAP, LeftFloating
          if(abs(Df.today$Quantity[t]) > abs(LeftFloating)) VWAP <- Df.today$Price[t]
          if(abs(Df.today$Quantity[t]) == abs(LeftFloating)) VWAP <- 0
          # if only partial close (LeftFloating > size), then no change in VWAP (only volume left)
          LeftFloating <- LeftFloating + Df.today$Quantity[t]
        }
        
        PL$Cost[d] <- PL$Cost[d] + Df.today$Fee[t]
      }
     } # close if index
      
     last_price_index <- tail(which(Prices$startTime <= PL$TimeStamp[d]),1)
     PL$PriceEOH[d] <- Prices$close[last_price_index]
    
     # Calculate EOH floating PL with EOH price
     PL$FloatingPL[d] <- (PL$PriceEOH[d]-VWAP)*LeftFloating
     PL$FloatingPL[d] %<>% round(2)
     if(d > 1) PL$NetFloating[d] <- PL$FloatingPL[d] - PL$FloatingPL[(d-1)]
     
     # Volume in USD
     PL$USDVol[d] <- PL$PriceEOH[d]*LeftFloating
     PL$USDVol[d] %<>% round(2)
     
     # Total PL
     PL$`Total PL`[d] <- PL$ClosedPL[d] + PL$NetFloating[d] + PL$Cost[d]
     PL$`Total PL`[d] %<>% round(2)
     
    } # close if index AND LeftFloating
  } 
  PLbySymbol[[s]] <- PL
}

for(s in 1:length(bySymbol)) {
  if(s == 1) {
    PL <- PLbySymbol[[1]]
    PL$PriceEOH <- NA
  } else {
    PL$USDVol <- PL$USDVol + PLbySymbol[[s]]$USDVol
    PL$FloatingPL <- PL$FloatingPL + PLbySymbol[[s]]$FloatingPL
    PL$NetFloating <- PL$NetFloating + PLbySymbol[[s]]$NetFloating
    PL$ClosedPL <- PL$ClosedPL + PLbySymbol[[s]]$ClosedPL
    PL$Cost <- PL$Cost + PLbySymbol[[s]]$Cost
    PL$`Total PL` <- PL$`Total PL` + PLbySymbol[[s]]$`Total PL`
    #
    print(paste(names(bySymbol)[[s]],sum(PLbySymbol[[s]]$NetFloating),"\n"))
  }
}

colnames(PL) <- c("TimeStamp","USDExposure","PriceEOH","FloatingPL","NetFloating","ClosedPL","Cost","Total PL")
PL$USDExposure %<>% multiply_by(-1)

wb<-xlsx::createWorkbook(type="xlsx")
sheet <- xlsx::createSheet(wb, sheetName = "Final PL")
xlsx::addDataFrame(PL, sheet, row.names=FALSE)
for(s in 1:length(bySymbol)) {
  sheet <- xlsx::createSheet(wb, sheetName = names(bySymbol)[s])
  xlsx::addDataFrame(PLbySymbol[[s]], sheet, row.names=FALSE)
}
xlsx::saveWorkbook(wb, "H1 PL.xlsx")

###

# INITIAL DEPOSIT - 1000 USDT

InitDep <- 1000
PL$Equity <- cumsum(c(InitDep,PL$`Total PL`[2:nrow(PL)]))

PL.xts <- PL$Equity %>% as.xts(order.by = PL$TimeStamp)
Returns <- CalculateReturns(PL.xts)

# Equity plot
ggplot(PL, aes(x=TimeStamp, y=Equity)) +
  geom_line( color="#100D54") + 
  xlab("") +
  theme_ipsum() +
  theme(axis.text.x=element_text(angle=60, hjust=1)) +
  labs(title="Equity")

## Histogram of returns
Returns.df <- Returns %>% as.data.frame()
ggplot(data=Returns.df, aes(V1)) + 
  geom_histogram(aes(y =..density..), 
                 breaks=seq(-0.07, 0.18, by = 0.005), 
                 col="blue", 
                 fill="green", 
                 alpha = .2) + 
  geom_density(col="blue") + 
  labs(title="Histogram of H1 returns") +
  labs(x="Return", y="Frequency")

basicStats(Returns.df$V1*100) # ALREADY IN %!!
which(Returns.df$V1==max(na.exclude(Returns.df$V1)))
prod(1+na.exclude(Returns.df$V1))

# Get benchmark data

Prices <- fromJSON(paste0("https://ftx.com/api/markets/BTC/USD/candles?resolution=3600&start_time=",
                          as.numeric(Dates[1]),"&end_time=",as.numeric(tail(Dates,1))))
Prices <- Prices$result
Prices$startTime %<>% substr(1,19) %>% strptime(format = "%Y-%m-%dT%H:%M:%S",tz = "utc")
BTC <- Prices$close %>% as.xts(order.by = Prices$startTime)
BTC.returns <- CalculateReturns(BTC)

#
Prices <- fromJSON(paste0("https://ftx.com/api/markets/SPY/USD/candles?resolution=3600&start_time=",
                          as.numeric(Dates[1]),"&end_time=",as.numeric(tail(Dates,1))))
Prices <- Prices$result
Prices$startTime %<>% substr(1,19) %>% strptime(format = "%Y-%m-%dT%H:%M:%S",tz = "utc")
SPY <- Prices$close %>% as.xts(order.by = Prices$startTime)
SPY.returns <- CalculateReturns(SPY)

# Alpha & Beta
CAPM.alpha(Returns,BTC.returns)
CAPM.beta(Returns,BTC.returns)
#
CAPM.alpha(Returns,SPY.returns)
CAPM.beta(Returns,SPY.returns)

# Rolling Alpha & Beta (n = 100)
PL$AlphaBTC <- NA
PL$BetaBTC <- NA
PL$AlphaSPY <- NA
PL$BetaSPY <- NA

for(i in 101:nrow(PL)) {
  benchmark1 <- BTC.returns[(i-100):i,]
  benchmark2 <- SPY.returns[(i-100):i,]
  Df.temp <- Returns[(i-100):i,]
  
  PL$AlphaBTC[i] <- CAPM.alpha(Df.temp,benchmark1)
  PL$BetaBTC[i] <- CAPM.beta(Df.temp,benchmark1)
  PL$AlphaSPY[i] <- CAPM.alpha(Df.temp,benchmark2)
  PL$BetaSPY[i] <- CAPM.beta(Df.temp,benchmark2)
}

# Opt f
TotPL.lsp <- lsp(PL$`Total PL`)
Optf <- optimalf(TotPL.lsp)

#Drawdown
maxDrawdown(Returns)
# Drawdown generalized in all open symbols (market beta) on 23.02 8:00, most are MATICUSDT, KAVAUSDT, FTMUSDT, BNBUSDT, RENUSDT

# VaR
VaR(Returns, method = "historical")
CVaR(Returns, method = "historical")

# Sharpe
SharpeRatio(Returns, FUN = "StdDev")





# BTC Exposure
eGrowth <- (1+Returns) %>% as.data.frame()
eGrowth$V1[1] <- 1
btcGrowth <- (1+BTC.returns)  %>% as.data.frame()
btcGrowth$V1[1] <- 1
eGrowth <- cumprod(eGrowth$V1) %>% as.data.frame()
btcGrowth <- cumprod(btcGrowth$V1) %>% as.data.frame()
BTC_compare <- cbind(PL$TimeStamp, eGrowth, btcGrowth)
colnames(BTC_compare) <- c("TimeStamp","Returns","BTC")

ggplot() +
  geom_line(data = BTC_compare, aes(x = TimeStamp, y = Returns), color = "red") + 
  geom_line(data = BTC_compare, aes(x = TimeStamp, y = BTC), color = "blue") +
  xlab("") +
  theme_ipsum() +
  theme(axis.text.x=element_text(angle=60, hjust=1)) +
  labs(title="BTC price vs Returns")




## Build BTC hedge trades
HedgeWeight <- 0.5

BTC.port <- matrix(ncol = 8, nrow = length(Dates)) %>% as.data.frame()
colnames(BTC.port) <- c("TimeStamp","USDVol","PriceEOH","FloatingPL","NetFloating","ClosedPL","Cost","Total PL")
BTC.port$TimeStamp <- Dates

LeftFloating <- rep(0,nrow(PL))
VWAP <- rep(0,nrow(PL))
BTC.port$USDVol <- 0
BTC.port$ClosedPL <- 0
BTC.port$FloatingPL <- 0
BTC.port$NetFloating <- 0
BTC.port$Cost <- 0
BTC.port$`Total PL` <- 0

for(b in 2:nrow(PL)) {
  
  # Calculate volume
  BTC.port$PriceEOH[b] <- BTC[which(index(BTC) == PL$TimeStamp[b])] %>% as.numeric()
  QuantityNow <- ((PL$USDExposure[b]-PL$USDExposure[(b-1)])*HedgeWeight)/BTC.port$PriceEOH[b]
  
  if((QuantityNow > 0 & LeftFloating[(b-1)] >= 0) |
     (QuantityNow < 0 & LeftFloating[(b-1)] <= 0)) {
    # Update VWAP, LeftFloating
    VWAP[b] <- ((QuantityNow*BTC.port$PriceEOH[b])+(LeftFloating[(b-1)]*VWAP[(b-1)]))/(LeftFloating[(b-1)]+QuantityNow)
    LeftFloating[b] <- LeftFloating[(b-1)] + QuantityNow
    
    # Estimate cost
    BTC.port$Cost[b] <- (QuantityNow*BTC.port$PriceEOH[b])*0.001 %>% abs() %>% multiply_by(-1)
  }
  if((QuantityNow > 0 & LeftFloating[(b-1)] < 0) |
     (QuantityNow < 0 & LeftFloating[(b-1)] > 0)) {
    # Close
    ifelse(abs(QuantityNow) > abs(LeftFloating[(b-1)]), AmountClosed <- LeftFloating[(b-1)], AmountClosed <- QuantityNow*-1)
    BTC.port$ClosedPL[b] <- ((BTC.port$PriceEOH[b] - VWAP[(b-1)])*AmountClosed) %>% round(2)
    
    # Estimate cost
    BTC.port$Cost[b] <- (QuantityNow*BTC.port$PriceEOH[b])*0.001 %>% abs() %>% multiply_by(-1)
    
    # Update VWAP, LeftFloating
    if(abs(QuantityNow) > abs(LeftFloating[(b-1)])) VWAP[b] <- BTC.port$PriceEOH[b]
    if(abs(QuantityNow) == abs(LeftFloating[(b-1)])) VWAP[b] <- 0
    if(abs(QuantityNow) < abs(LeftFloating[(b-1)])) VWAP[b] <- VWAP[(b-1)]
    LeftFloating[b] <- LeftFloating[(b-1)] + QuantityNow
  }
  
  # Calculate EOH floating PL with EOH price
  BTC.port$FloatingPL[b] <- (BTC.port$PriceEOH[b]-VWAP[b])*LeftFloating[b]
  BTC.port$FloatingPL[b] %<>% round(2)
  if(b > 1) BTC.port$NetFloating[b] <- BTC.port$FloatingPL[b] - BTC.port$FloatingPL[(b-1)]
  
  # Volume in USD
  BTC.port$USDVol[b] <- BTC.port$PriceEOH[b]*LeftFloating[b]
  BTC.port$USDVol[b] %<>% round(2)
  
  # Total PL
  BTC.port$`Total PL`[b] <- BTC.port$ClosedPL[b] + BTC.port$NetFloating[b] + BTC.port$Cost[b]
  BTC.port$`Total PL`[b] %<>% round(2)
}

Val.data <- cbind(as.data.frame(LeftFloating),VWAP)

SimPort <- PL
SimPort$`Total PL` <- PL$`Total PL` + BTC.port$`Total PL`
SimPort$PriceEOH <- BTC.port$PriceEOH
SimPort$BTCUSDExposure <- BTC.port$USDVol
SimPort$BTCFloatingPL <- BTC.port$FloatingPL
SimPort$BTCNetFloating <- BTC.port$NetFloating
SimPort$BTCClosedPL <- BTC.port$ClosedPL
SimPort$BTCCost <- BTC.port$Cost
InitDep <- 1000 # Same deposit with 100% hedge = double leverage
SimPort$Equity <- cumsum(c(InitDep,SimPort$`Total PL`[2:nrow(SimPort)]))

# Validation file
wb<-xlsx::createWorkbook(type="xlsx")
sheet <- xlsx::createSheet(wb, sheetName = "Final PL")
xlsx::addDataFrame(SimPort, sheet, row.names=FALSE)
for(s in 1:length(bySymbol)) {
  sheet <- xlsx::createSheet(wb, sheetName = names(bySymbol)[s])
  xlsx::addDataFrame(PLbySymbol[[s]], sheet, row.names=FALSE)
}
xlsx::saveWorkbook(wb, "Hedge simulation.xlsx")


SimPort.xts <- SimPort$Equity %>% as.xts(order.by = SimPort$TimeStamp)
SimPort.ret <- CalculateReturns(SimPort.xts)

maxDrawdown(SimPort.ret)

eGrowth <- (1+Returns) %>% as.data.frame()
eGrowth$V1[1] <- 1
btcGrowth <- (1+SimPort.ret)  %>% as.data.frame()
btcGrowth$V1[1] <- 1
eGrowth <- cumprod(eGrowth$V1) %>% as.data.frame()
btcGrowth <- cumprod(btcGrowth$V1) %>% as.data.frame()
BTC_compare <- cbind(PL$TimeStamp, eGrowth, btcGrowth)
colnames(BTC_compare) <- c("TimeStamp","Returns","BTC")

ggplot() +
  geom_line(data = BTC_compare, aes(x = TimeStamp, y = Returns), color = "red") + 
  geom_line(data = BTC_compare, aes(x = TimeStamp, y = BTC), color = "blue") +
  xlab("") +
  theme_ipsum() +
  theme(axis.text.x=element_text(angle=60, hjust=1)) +
  labs(title="Returns (red) vs Hedged (blue)")


# Compare
#Drawdown
maxDrawdown(Returns)
maxDrawdown(SimPort.ret)

# VaR
VaR(Returns, method = "historical")
VaR(SimPort.ret, method = "historical")
CVaR(Returns, method = "historical")
CVaR(SimPort.ret, method = "historical")

# Sharpe
SharpeRatio(Returns, FUN = "StdDev")
SharpeRatio(SimPort.ret, FUN = "StdDev")

