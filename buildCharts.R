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
library(tidyquant)
options(stringsAsFactors = FALSE)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

Data <- read.csv("algo006.csv", sep = ";", stringsAsFactors = FALSE, row.names = NULL)
colnames(Data) <- c("Date","Pair","Side","Price","Quantity","Amount","Fee","RealizedProfit")
Data$Date %<>% substr(1,19) %>% strptime(format = "%d/%m/%Y %H:%M",tz = "utc")
Data$Quantity[which(Data$Side == "SELL")] %<>% multiply_by(-1)
Data$Fee %<>% str_remove(" USDT") %>% as.numeric()


# Subset to understand entries
Cutoff <- "11/02/2021 00:00" %>% strptime(format = "%d/%m/%Y %H:%M",tz = "utc")
Data <- Data[which(Data$Date < Cutoff),]

Start <- min(Data$Date) %>% floor_date("hour")
Finish <- max(Data$Date) %>% ceiling_date("hour")
Dates <- seq((Start-(60*60*24*7)),Finish,"hour")
which(weekdays(Dates,abbreviate = TRUE) %in% c("Sat","Sun")) %>% length() %>% sum

# 

bySymbol <- Data %>% split(Data$Pair)

for(s in 1:length(bySymbol)) {
  cat(paste("Processing", names(bySymbol)[s], "- still", (length(bySymbol)-s),"to go","\n"))
  
  Df.temp <- bySymbol[[s]]
  
  Prices <- fromJSON(paste0("https://api.binance.com/api/v3/klines?symbol=",names(bySymbol)[s],"&interval=1h&startTime=",
                            (as.numeric(Dates[1])*1000),"&endTime=",(as.numeric(tail(Dates,1)))*1000)) %>% as.data.frame()
  Prices <- Prices[,1:5]
  colnames(Prices) <- c("startTime","open","high","low","close")
  Prices$startTime %<>% as.numeric %>% divide_by(1000) %>% as.POSIXct(origin="1970-01-01", tz = "utc")
  Prices$open %<>% as.numeric()
  Prices$high %<>% as.numeric()
  Prices$low %<>% as.numeric()
  Prices$close %<>% as.numeric()
  Prices$VWAP_sell <- NA
  Prices$VWAP_buy <- NA
  
  for(i in 1:nrow(Prices)) {
    index <- which(Df.temp$Date >= Prices$startTime[i] & Df.temp$Date < (Prices$startTime[i]+60*60))
    if(length(index) > 0) {
      Df.today <- Df.temp[index,]
      VWAP <- sum(Df.today$Price*Df.today$Quantity)/abs(sum(Df.today$Quantity))
      ifelse(VWAP > 0, Prices$VWAP_buy[i] <- abs(VWAP), Prices$VWAP_sell[i] <- abs(VWAP))
    }
  }
  
  fig <- Prices %>%
  ggplot(aes(x = startTime, y = close)) +
  geom_candlestick(aes(open = open, high = high, low = low, close = close)) +
  geom_point(aes(x=startTime, y = VWAP_buy), shape=24, fill="blue", color="darkred", size=3) +
  geom_point(aes(x=startTime, y = VWAP_sell), shape=25, fill="red", color="darkred", size=3) +
  labs(title = names(bySymbol)[s], y = "Price", x = "") +
  theme_tq()
    
  ggsave(paste0(names(bySymbol)[s],".jpeg"))
}


