setwd("E:/STOCK/Dividend")
library("quantmod")
library("tidyverse")
library(stringr)



divs <- getDividends("MSFT")
divs <- as.data.frame(divs)
divs$dates <- rownames(divs)
divs$dates <-unlist(lapply(strsplit(divs$dates, "-"), function(x)unlist(x)[1]))
asc <- aggregate(divs$MSFT.div,by=list((divs$dates)), FUN =  sum)
asc2 <- diff(asc$x)/lag(asc$x,1)[-1]

x <- (asc$x[length(asc$x)]-asc$x[1])/asc$x[1]
x^(1/length(asc$x))-1


a <- as.numeric(unique(divs$dates))

sum(unique(a) == seq(a[1],a[length(a)],by=1))/length(seq(a[1],a[length(a)],by=1))

dividends <- function(symbols){
  ticker <- getDividends(symbols)
  ticker <- as.data.frame(ticker)
  ticker$dates <- rownames(ticker)
  ticker$dates <-unlist(lapply(strsplit(ticker$dates, "-"), function(x)unlist(x)[1]))
  a <- as.numeric(unique(ticker$dates))
  #conse <- sum(unique(a) == seq(a[1],a[length(a)],by=1))/length(seq(a[1],a[length(a)],by=1))
  #after2012 <- seq(unique(a)[length(unique(a))],unique(a)[length(unique(a))]-10,by=-1)
  
  if(sum(unique(a) == seq(a[1],a[length(a)],by=1))/length(seq(a[1],a[length(a)],by=1)) == 1){
    annualdiv <- aggregate(ticker[,1],by=list((ticker$dates)), FUN =  sum)
    annualdiv2 <- diff(annualdiv$x)/lag(annualdiv$x,1)[-1]
    div_returns <- (annualdiv$x[length(annualdiv$x)]/annualdiv$x[1])^(1/length(annualdiv$x))-1
    
    annualdiv_10yr <- aggregate(ticker[which(as.numeric(ticker$dates)>=2012),1],by=list((ticker$dates)[which(as.numeric(ticker$dates)>=2012)]), FUN =  sum)
    annualdiv2_10yr <- diff(annualdiv_10yr$x)/lag(annualdiv_10yr$x,1)[-1]
    div_returns_10yr <- (annualdiv_10yr$x[length(annualdiv_10yr$x)]/annualdiv_10yr$x[1])^(1/10)-1

    
    return(list(annual_dividend_growth = data.frame(annual_dividend_growth = paste(round(div_returns*100,2),"%",sep=""), since=a[1]),
                past_10yr = data.frame(annual_dividend_growth_past10yrs = paste(round(div_returns_10yr*100,2),"%", sep=""), since=2012)))
    

  }
  if( sum(seq(unique(a)[length(unique(a))],unique(a)[length(unique(a))]-10,by=-1) == seq(2022,2012,-1))==length(after2012)){
    annualdiv_10yr <- aggregate(ticker[which(as.numeric(ticker$dates)>=2012),1],by=list((ticker$dates)[which(as.numeric(ticker$dates)>=2012)]), FUN =  sum)
    annualdiv2_10yr <- diff(annualdiv_10yr$x)/lag(annualdiv_10yr$x,1)[-1]
    div_returns_10yr <- (annualdiv_10yr$x[length(annualdiv_10yr$x)]/annualdiv_10yr$x[1])^(1/10)-1
    return(list(annual_dividend_growth_past10yrs = paste(paste(round(div_returns_10yr*100,2),"%",sep=""), "Since 2012")))
    
  }
  
  else{
    return("The stock paused dividend at some point")
  }

}

dividends("aapl")
getDividends("vz")
symbols ="vz"

symbols="ko"

getDividends("AAPL")
