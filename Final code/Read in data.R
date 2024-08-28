library(quantmod)
library(xts)
library(zoo)
library(dplyr)
library(ggplot2)

WTI <- read.csv("Data/WTI_price_data.csv", skip = 5, header = FALSE, col.names = c("Day","Price"))
WTI$Day <- as.Date(WTI$Day, format = "%m/%d/%Y")

WTI_xts <- xts(WTI$Price, order.by = WTI$Day)
WTI_df <- data.frame(Date = index(WTI_xts), Price = coredata(WTI_xts))

returns <- -dailyReturn(WTI_xts, type = "log") * 100
returns <- na.omit(returns)
returns_df <- data.frame(Date = index(returns), Daily_Return = coredata(returns))

WTI_data <- as.numeric(returns)


Brent <- read.csv("Data/Brent_price_data.csv", skip = 5, header = FALSE, col.names = c("Day","Price"))

Brent$Day <- as.Date(Brent$Day, format = "%m/%d/%Y")

Brent_xts <- xts(Brent$Price, order.by = Brent$Day)
Brent_df <- data.frame(Date = index(Brent_xts), Price = coredata(Brent_xts))

returns_Brent <- -dailyReturn(Brent_xts, type = "log") * 100
returns_Brent = na.omit(returns_Brent)
returns_Brent_df <- data.frame(Date = index(returns_Brent), Daily_Return = coredata(returns_Brent))

Brent_data <- as.numeric(returns_Brent)
