remove(list=ls())

# Running all codes may take 2 hours maximum. Fundamental datas are in csv file in zip file,
# you need to set right directory to extract them.

library(quantmod); library(PerformanceAnalytics); library(plm); library(zoo)
library(TTR); library(RedditExtractoR); library(SentimentAnalysis)
library(SnowballC); library(dplyr); library(car); library(gvlma)

# Get 5 Stock Prices from Yahoo Finance
sym.vec <-c("TSLA","AMC","MRNA","CSCO","NFLX")
getSymbols(sym.vec, from = "2010-12-31", to = "2021-08-31") 
TSLA <- TSLA[, "TSLA.Adjusted"]
TSLA_monthly <-to.monthly(TSLA)
TSLA.logret = CalculateReturns(TSLA_monthly, method="log") 
TSLA_reta<-TSLA.logret$TSLA.Close
TSLA_ret <- TSLA_reta[-1,]
colnames(TSLA_ret)="Return"
AMC <- AMC[, "AMC.Adjusted"]
AMC_monthly <-to.monthly(AMC)
AMC.logret = CalculateReturns(AMC_monthly, method="log") 
AMC_reta<-AMC.logret$AMC.Close
AMC_ret <- AMC_reta[-1,]
colnames(AMC_ret)="Return"
MRNA <- MRNA[, "MRNA.Adjusted"]
MRNA_monthly <-to.monthly(MRNA)
MRNA.logret = CalculateReturns(MRNA_monthly, method="log") 
MRNA_reta<-MRNA.logret$MRNA.Close
MRNA_ret <- MRNA_reta[-1,]
colnames(MRNA_ret)="Return"
CSCO <- CSCO[, "CSCO.Adjusted"]
CSCO_monthly <-to.monthly(CSCO)
CSCO.logret = CalculateReturns(CSCO_monthly, method="log") 
CSCO_reta<-CSCO.logret$CSCO.Close
CSCO_ret <- CSCO_reta[-1,]
colnames(CSCO_ret)="Return"
NFLX <- NFLX[, "NFLX.Adjusted"]
NFLX_monthly <-to.monthly(NFLX)
NFLX.logret = CalculateReturns(NFLX_monthly, method="log") 
NFLX_reta<-NFLX.logret$NFLX.Close
NFLX_ret <- NFLX_reta[-1,]
colnames(NFLX_ret)="Return"

return <- cbind(TSLA_ret,AMC_ret,MRNA_ret,CSCO_ret,NFLX_ret) # combining stock as columns

#converting stock prices from return above to csv files

write.csv(return$Return,"TSLA_ret.csv", row.names = FALSE)
write.csv(return$Return.1,"AMC_ret.csv", row.names = FALSE)
write.csv(return$Return.2,"MRNA_ret.csv", row.names = FALSE)
write.csv(return$Return.3,"CSCO_ret.csv", row.names = FALSE)
write.csv(return$Return.4,"NFLX_ret.csv", row.names = FALSE)

# Upload csv files and make column names same
TSLA_ret1 <- read.csv("TSLA_ret.csv")  
AMC_ret1 <-  read.csv("AMC_ret.csv")  
MRNA_ret1 <- read.csv("MRNA_ret.csv")  
CSCO_ret1 <- read.csv("CSCO_ret.csv")  
NFLX_ret1 <- read.csv("NFLX_ret.csv")  
colnames(TSLA_ret1) = "Return"
colnames(AMC_ret1) = "Return"
colnames(MRNA_ret1) = "Return"
colnames(CSCO_ret1) = "Return"
colnames(NFLX_ret1) = "Return"

# combines stock prices from csv files as rows 
returns <-rbind(TSLA_ret1,AMC_ret1,MRNA_ret1,CSCO_ret1,NFLX_ret1)

#Fundamentals taken from Yahoo Finance and adjuted in Excel as to be exctracted as csv file to get important ratios
TSLAfundamentals <- read.csv(file="TSLA.csv",header = TRUE)
AMCfundamentals <- read.csv(file="AMC.csv",header = TRUE)  
MRNAfundamentals <- read.csv(file="MRNA.csv",header = TRUE)
CSCOfundamentals <- read.csv(file="CSCO.csv",header = TRUE)
NFLXfundamentals <- read.csv(file="NFLX.csv",header = TRUE)

# combining fundamentals for 5 companies as columns

fundamentals<- cbind(TSLAfundamentals,AMCfundamentals,MRNAfundamentals,CSCOfundamentals,NFLXfundamentals) 

# Deleting year column in fundamentals
TSLA_fundamental <-TSLAfundamentals[,-1]
AMC_fundamental <- AMCfundamentals[,-1]
MRNA_fundamental <- MRNAfundamentals[,-1]
CSCO_fundamental <- CSCOfundamentals[,-1]
NFLX_fundamental <- NFLXfundamentals[,-1]

# combining fundamentals as rows
fund_matrix <-rbind(TSLA_fundamental,AMC_fundamental,MRNA_fundamental,CSCO_fundamental,NFLX_fundamental) 

# extracting ETFs and some stocks as factors
sym.vec2 <-c("XLE","AWAY","USAI","GRID","LIT","PALL","URA","FAN","MJO","CURE","CTEC","HERO","CLOU","GAMR","AAPL","DIS","AMZN","GM","F","TM","^GSPC","GME")
getSymbols(sym.vec2, from = "2010-12-31", to = "2021-08-31") 
XLE<- XLE[, "XLE.Adjusted"]
XLE_monthly <-to.monthly(XLE)
XLE.logret = CalculateReturns(XLE_monthly, method="log") 
XLE_reta<-XLE.logret$XLE.Close
XLE_ret <- XLE_reta[-1,]
AWAY <- AWAY[, "AWAY.Adjusted"]
AWAY_monthly <-to.monthly(AWAY)
AWAY.logret = CalculateReturns(AWAY_monthly, method="log") 
AWAY_reta<-AWAY.logret$AWAY.Close
AWAY_ret <- AWAY_reta[-1,]
USAI <- USAI[, "USAI.Adjusted"]
USAI_monthly <-to.monthly(USAI)
USAI.logret = CalculateReturns(USAI_monthly, method="log") 
USAI_reta<-USAI.logret$USAI.Close
USAI_ret <- USAI_reta[-1,]
GRID <- GRID[, "GRID.Adjusted"]
GRID_monthly <-to.monthly(GRID)
GRID.logret = CalculateReturns(GRID_monthly, method="log") 
GRID_reta<-GRID.logret$GRID.Close
GRID_ret <- GRID_reta[-1,]
LIT <- LIT[, "LIT.Adjusted"]
LIT_monthly <-to.monthly(LIT)
LIT.logret = CalculateReturns(LIT_monthly, method="log") 
LIT_reta<-LIT.logret$LIT.Close
LIT_ret <- LIT_reta[-1,]
PALL<- PALL[, "PALL.Adjusted"]
PALL_monthly <-to.monthly(PALL)
PALL.logret = CalculateReturns(PALL_monthly, method="log") 
PALL_reta<-PALL.logret$PALL.Close
PALL_ret <- PALL_reta[-1,]
URA <- URA[, "URA.Adjusted"]
URA_monthly <-to.monthly(URA)
URA.logret = CalculateReturns(URA_monthly, method="log") 
URA_reta<-URA.logret$URA.Close
URA_ret <- URA_reta[-1,]
FAN <- FAN[, "FAN.Adjusted"]
FAN_monthly <-to.monthly(FAN)
FAN.logret = CalculateReturns(FAN_monthly, method="log") 
FAN_reta<-FAN.logret$FAN.Close
FAN_ret <- FAN_reta[-1,]
MJO <- MJO[, "MJO.Adjusted"]
MJO_monthly <-to.monthly(MJO)
MJO.logret = CalculateReturns(MJO_monthly, method="log") 
MJO_reta<-MJO.logret$MJO.Close
MJO_ret <- MJO_reta[-1,]
CURE <- CURE[, "CURE.Adjusted"]
CURE_monthly <-to.monthly(CURE)
CURE.logret = CalculateReturns(CURE_monthly, method="log") 
CURE_reta<-CURE.logret$CURE.Close
CURE_ret <- CURE_reta[-1,]
CTEC <- CTEC[, "CTEC.Adjusted"]
CTEC_monthly <-to.monthly(CTEC)
CTEC.logret = CalculateReturns(CTEC_monthly, method="log") 
CTEC_reta<-CTEC.logret$CTEC.Close
CTEC_ret <- CTEC_reta[-1,]
HERO <- HERO[, "HERO.Adjusted"]
HERO_monthly <-to.monthly(HERO)
HERO.logret = CalculateReturns(HERO_monthly, method="log") 
HERO_reta<-HERO.logret$HERO.Close
HERO_ret <- HERO_reta[-1,]
CLOU <- CLOU[, "CLOU.Adjusted"]
CLOU_monthly <-to.monthly(CLOU)
CLOU.logret = CalculateReturns(CLOU_monthly, method="log") 
CLOU_reta<-CLOU.logret$CLOU.Close
CLOU_ret <- CLOU_reta[-1,]
GAMR <- GAMR[, "GAMR.Adjusted"]
GAMR_monthly <-to.monthly(GAMR)
GAMR.logret = CalculateReturns(GAMR_monthly, method="log") 
GAMR_reta<-GAMR.logret$GAMR.Close
GAMR_ret <- GAMR_reta[-1,]
AAPL <- AAPL[, "AAPL.Adjusted"]
AAPL_monthly <-to.monthly(AAPL)
AAPL.logret = CalculateReturns(AAPL_monthly, method="log") 
AAPL_reta<-AAPL.logret$AAPL.Close
AAPL_ret <- AAPL_reta[-1,]
DIS <- DIS[, "DIS.Adjusted"]
DIS_monthly <-to.monthly(DIS)
DIS.logret = CalculateReturns(DIS_monthly, method="log") 
DIS_reta<-DIS.logret$DIS.Close
DIS_ret <- DIS_reta[-1,]
AMZN <- AMZN[, "AMZN.Adjusted"]
AMZN_monthly <-to.monthly(AMZN)
AMZN.logret = CalculateReturns(AMZN_monthly, method="log") 
AMZN_reta<-AMZN.logret$AMZN.Close
AMZN_ret <- AMZN_reta[-1,]
GM <- GM[, "GM.Adjusted"]
GM_monthly <-to.monthly(GM)
GM.logret = CalculateReturns(GM_monthly, method="log") 
GM_reta<-GM.logret$GM.Close
GM_ret <- GM_reta[-1,]
F <- F[, "F.Adjusted"]
F_monthly <-to.monthly(F)
F.logret = CalculateReturns(F_monthly, method="log") 
F_reta<-F.logret$F.Close
F_ret <- F_reta[-1,]
TM <- TM[, "TM.Adjusted"]
TM_monthly <-to.monthly(TM)
TM.logret = CalculateReturns(TM_monthly, method="log") 
TM_reta<-TM.logret$TM.Close
TM_ret <- TM_reta[-1,]
GSPC <- GSPC[, "GSPC.Adjusted"]
GSPC_monthly <-to.monthly(GSPC)
GSPC.logret = CalculateReturns(GSPC_monthly, method="log") 
GSPC_reta<-GSPC.logret$GSPC.Close
GSPC_ret <- GSPC_reta[-1,]
GME <- GME[, "GME.Adjusted"]
GME_monthly <-to.monthly(GME)
GME.logret = CalculateReturns(GME_monthly, method="log") 
GME_reta<-GME.logret$GME.Close
GME_ret <- GME_reta[-1,]


# combining ETFs and some stocks as columns
fact_etf<-cbind(XLE_ret,AWAY_ret,USAI_ret,GRID_ret,LIT_ret,PALL_ret,URA_ret,FAN_ret,MJO_ret,CURE_ret,CTEC_ret,HERO_ret,CLOU_ret,GAMR_ret,AAPL_ret,DIS_ret,AMZN_ret,GM_ret,F_ret,TM_ret,GSPC_ret,GME_ret)

# replicate 5 times for 5 stocks and change column names
XLEf<-data.frame(rep(fact_etf$XLE.Close,5))
colnames(XLEf)="XLE"
AWAYf<-data.frame(rep(fact_etf$AWAY.Close,5))
colnames(AWAYf)="AWAY"
USAIf<-data.frame(rep(fact_etf$USAI.Close,5))
colnames(USAIf)="USAI"
GRIDf<-data.frame(rep(fact_etf$GRID.Close,5))
colnames(GRIDf)="GRID"
LITf<-data.frame(rep(fact_etf$LIT.Close,5))
colnames(LITf)="LIT"
PALLf<-data.frame(rep(fact_etf$PALL.Close,5))
colnames(PALLf)="PALL"
URAf<-data.frame(rep(fact_etf$URA.Close,5))
colnames(URAf)="URA"
FANf<-data.frame(rep(fact_etf$FAN.Close,5))
colnames(FANf)="FAN"
MJOf<-data.frame(rep(fact_etf$MJO.Close,5))
colnames(MJOf)="MJO"
CUREf<-data.frame(rep(fact_etf$CURE.Close,5))
colnames(CUREf)="CURE"
CTECf<-data.frame(rep(fact_etf$CTEC.Close,5))
colnames(CTECf)="CTEC"
HEROf<-data.frame(rep(fact_etf$HERO.Close,5))
colnames(HEROf)="HERO"
CLOUf<-data.frame(rep(fact_etf$CLOU.Close,5))
colnames(CLOUf)="CLOU"
GAMRf<-data.frame(rep(fact_etf$GAMR.Close,5))
colnames(GAMRf)="GAMR"
AAPLf<-data.frame(rep(fact_etf$AAPL.Close,5))
colnames(AAPLf)="AAPL"
DISf<-data.frame(rep(fact_etf$DIS.Close,5))
colnames(DISf)="DIS"
AMZNf<-data.frame(rep(fact_etf$AMZN.Close,5))
colnames(AMZNf)="AMZN"
GMf<-data.frame(rep(fact_etf$GM.Close,5))
colnames(GMf)="GM"
Ff<-data.frame(rep(fact_etf$F.Close,5))
colnames(Ff)="F"
TMf<-data.frame(rep(fact_etf$TM.Close,5))
colnames(TMf)="TM"
GSPCf<-data.frame(rep(fact_etf$GSPC.Close,5))
colnames(GSPCf)="GSPC"
GMEf<-data.frame(rep(fact_etf$GME.Close,5))
colnames(GMEf)="GME"

# combining all ETFs and some stock prices for factors as columns after replicated
funda_fact<-cbind(XLEf,AWAYf,USAIf,GRIDf,LITf,PALLf,URAf,FANf,MJOf,CUREf,CTECf,HEROf,CLOUf,GAMRf,AAPLf,DISf,AMZNf,GMf,Ff,TMf,GSPCf,GMEf)


# macro-events data

getSymbols("UNRATE",src="FRED") 
unrate1 <- UNRATE["2010-12-31/2021-08-31"]
unrate_fin <- data.frame(rep(unrate1$UNRATE,1))
colnames(unrate_fin)="Unemployment"
unrate_final <- data.frame(rep(unrate_fin$Unemployment,5))
colnames(unrate_final)="Unemployment"

getSymbols("GDP",src="FRED") 
GDP1 <- GDP["2010-12-31/2021-08-31"]
GDP_ret <-data.frame(diff(log(GDP1$GDP)))
GDP_u <- data.frame(rep(GDP_ret$GDP,each=3))
colnames(GDP_u)="GDP"
lastpart_GDP <- data.frame(rep(GDP_ret$GDP[42],2))
colnames(lastpart_GDP)="GDP"
GDP_fin <- rbind(GDP_u,lastpart_GDP)
GDP_final <- data.frame(rep(GDP_fin$GDP,5))
colnames(GDP_final)="GDP"

getSymbols("T10YIEM",src="FRED") 
Inflation_10Y_u<- T10YIEM["2010-12-31/2021-08-31"]
Inflation_10Y_fin <-data.frame(rep(Inflation_10Y_u$T10YIEM,1))
colnames(Inflation_10Y_fin)="Inflation_10Y"
inflation10Y_final <- data.frame(rep(Inflation_10Y_fin$Inflation_10Y,5))
colnames(inflation10Y_final)="Inflation_10Y"

getSymbols("INTDSRUSM193N",src="FRED") 
Interest_10Y<- INTDSRUSM193N["2010-12-31/2021-08-31"]
lastpart_int <- data.frame(rep(Interest_10Y$INTDSRUSM193N[126],2))
colnames(lastpart_int)= "Interest10Y"
Interest_10Y_u <- data.frame(rep(Interest_10Y$INTDSRUSM193N[1:126],1))
colnames(Interest_10Y_u)= "Interest10Y"
Interest10Y_fin <- rbind(Interest_10Y_u,lastpart_int)
interest10Y_final <- data.frame(rep(Interest10Y_fin$Interest10Y,5))
colnames(interest10Y_final)="Interest_10Y"

getSymbols("HQMCB10YR",src="FRED") 
Bond_Spot_HQM<- HQMCB10YR["2010-12-31/2021-08-31"]
Bond_HQM_u<- data.frame(rep(Bond_Spot_HQM$HQMCB10YR,1))
colnames(Bond_HQM_u)="Bond_HQM"
bondhqm_final <- data.frame(rep(Bond_HQM_u$Bond_HQM,5))
colnames(bondhqm_final)="Bond_HQM"

getSymbols("CES0500000003",src="FRED") 
Avg_wage<- CES0500000003["2010-12-31/2021-08-31"]
Avg_wage_ret<-data.frame(diff(log(Avg_wage$CES0500000003)))
avgwage_fin <- data.frame(rep(Avg_wage_ret$CES0500000003,1))
colnames(avgwage_fin)="Avg_Earnings"
avgwage_final <- data.frame(rep(avgwage_fin$Avg_Earnings,5))
colnames(avgwage_final)="Avg_Earnings"

getSymbols("TOTALSA",src="FRED") 
vehicle_sales<- TOTALSA["2010-12-31/2021-08-31"]
vehiclesales_fin<-data.frame(rep(vehicle_sales$TOTALSA,1))
colnames(vehiclesales_fin)="Vehicle_Sales"
vehiclesales_final <- data.frame(rep(vehiclesales_fin$Vehicle_Sales,5))
colnames(vehiclesales_final)="Vehicle_Sales"

getSymbols("APU000072610",src="FRED") 
avgelectrictiy_price<- APU000072610["2010-12-31/2021-08-31"]
avgelelectricity_fin<-data.frame(rep(avgelectrictiy_price$APU000072610,1))
colnames(avgelelectricity_fin)="Avg_Elec_Price"
avgelectricity_final <- data.frame(rep(avgelelectricity_fin$Avg_Elec_Price,5))
colnames(avgelectricity_final)="Avg_Elec_Price"

getSymbols("MRTSIM441USS",src="FRED") 
mvpart_inv<- MRTSIM441USS["2010-12-31/2021-08-31"]
mvpart_inv_u<-data.frame(rep(mvpart_inv$MRTSIM441USS,1))
lastpartmv <- data.frame(rep(mvpart_inv$MRTSIM441USS[127],1))
colnames(lastpartmv)="Part_Dealers"
colnames(mvpart_inv_u)="Part_Dealers"
mvpart_inv_fin <- rbind(mvpart_inv_u,lastpartmv)
mvpart_final <- data.frame(rep(mvpart_inv_fin$Part_Dealers,5))
colnames(mvpart_final)="Part_Dealers"

macro_factors <- cbind(unrate_final,GDP_final,inflation10Y_final,interest10Y_final,bondhqm_final,avgwage_final,vehiclesales_final,avgelectricity_final,mvpart_final)

# Covid Data:

covid_u <- read.csv("Covid.csv",header=TRUE)
covid_f <- data.frame(rep(covid_u,1))
colnames(covid_f) = "Covid"
firstpart <- data.frame(rep(NA,109))
colnames(firstpart) = "Covid"
covid_final <- rbind(firstpart,covid_f)


# 2.2. Technical Analysis

# Factor Part:

Tsma20_u <- SMA(TSLA$TSLA.Adjusted,n=20) # 20-day moving average
Tsma20_c <- to.monthly(Tsma20_u)
Tsma20 <- data.frame(rep(Tsma20_c$Tsma20_u.Close,1))
colnames(Tsma20)= "SMA"

Asma20_u <- SMA(AMC$AMC.Adjusted,n=20) # 20-day moving average
Asma20_c <- to.monthly(Asma20_u)
Asma20 <- data.frame(rep(c(rep(NA,36),Asma20_c$Asma20_u.Close),1))
colnames(Asma20)= "SMA"

Msma20_u <- SMA(MRNA$MRNA.Adjusted,n=20) # 20-day moving average
Msma20_c <- to.monthly(Msma20_u)
Msma20 <- data.frame(rep(c(rep(NA,96),Msma20_c$Msma20_u.Close),1))
colnames(Msma20)= "SMA"

Csma20_u <- SMA(CSCO$CSCO.Adjusted,n=20) # 20-day moving average
Csma20_c <- to.monthly(Csma20_u)
Csma20 <- data.frame(rep(Csma20_c$Csma20_u.Close,1))
colnames(Csma20)= "SMA"

Nsma20_u <- SMA(NFLX$NFLX.Adjusted,n=20) # 20-day moving average
Nsma20_c <- to.monthly(Nsma20_u)
Nsma20 <- data.frame(rep(Nsma20_c$Nsma20_u.Close,1))
colnames(Nsma20)= "SMA"

all_sma20 <- rbind(Tsma20,Asma20,Msma20,Csma20,Nsma20)


Tema14_u <- EMA(TSLA$TSLA.Adjusted,n=14) #14-day EMA
Tema14_c <- to.monthly(Tema14_u)
Tema14 <- data.frame(rep(Tema14_c$Tema14_u.Close,1))
colnames(Tema14)= "EMA"

Aema14_u <- EMA(AMC$AMC.Adjusted,n=14) #14-day EMA
Aema14_c <- to.monthly(Aema14_u)
Aema14 <- data.frame(rep(c(rep(NA,36),Aema14_c$Aema14_u.Close),1))
colnames(Aema14)= "EMA"

Mema14_u <- EMA(MRNA$MRNA.Adjusted,n=14) #14-day EMA
Mema14_c <- to.monthly(Mema14_u)
Mema14 <- data.frame(rep(c(rep(NA,95),Mema14_c$Mema14_u.Close),1))
colnames(Mema14)= "EMA"

Cema14_u <- EMA(CSCO$CSCO.Adjusted,n=14) #14-day EMA
Cema14_c <- to.monthly(Cema14_u)
Cema14 <- data.frame(rep(Cema14_c$Cema14_u.Close,1))
colnames(Cema14)= "EMA"

Nema14_u <- EMA(NFLX$NFLX.Adjusted,n=14) #14-day EMA
Nema14_c <- to.monthly(Nema14_u)
Nema14 <- data.frame(rep(Nema14_c$Nema14_u.Close,1))
colnames(Nema14)= "EMA"

all_ema14 <- rbind(Tema14,Aema14,Mema14,Cema14,Nema14)


Tbb20_u <- BBands(TSLA$TSLA.Adjusted, sd=2.0) #bolliger band, default is 20day
Tbb20_c <- to.monthly(Tbb20_u$pctB)
Tbb20 <- data.frame(rep(Tbb20_c$`Tbb20_u$pctB.Close`,1))
colnames(Tbb20)="BB20_%B"

Abb20_u <- BBands(AMC$AMC.Adjusted, sd=2.0) #bolliger band, default is 20day
Abb20_c <- to.monthly(Abb20_u$pctB)
Abb20 <- data.frame(rep(c(rep(NA,36),Abb20_c$`Abb20_u$pctB.Close`),1))
colnames(Abb20)="BB20_%B"

Mbb20_u <- BBands(MRNA$MRNA.Adjusted, sd=2.0) #bolliger band, default is 20day
Mbb20_c <- to.monthly(Mbb20_u$pctB)
Mbb20 <- data.frame(rep(c(rep(NA,96),Mbb20_c$`Mbb20_u$pctB.Close`),1))
colnames(Mbb20)="BB20_%B"

Cbb20_u <- BBands(CSCO$CSCO.Adjusted, sd=2.0) #bolliger band, default is 20day
Cbb20_c <- to.monthly(Cbb20_u$pctB)
Cbb20 <- data.frame(rep(Cbb20_c$`Cbb20_u$pctB.Close`,1))
colnames(Cbb20)="BB20_%B"

Nbb20_u <- BBands(NFLX$NFLX.Adjusted, sd=2.0) #bolliger band, default is 20day
Nbb20_c <- to.monthly(Nbb20_u$pctB)
Nbb20 <- data.frame(rep(Nbb20_c$`Nbb20_u$pctB.Close`,1))
colnames(Nbb20)="BB20_%B"

all_bb20 <- rbind(Tbb20,Abb20,Mbb20,Cbb20,Nbb20)

Trsi14_u <- RSI(TSLA$TSLA.Adjusted, n=14)
Trsi14_c <- to.monthly(Trsi14_u)
Trsi14 <- data.frame(rep(Trsi14_c$Trsi14_u.Close,1))
colnames(Trsi14)="RSI"

Arsi14_u <- RSI(AMC$AMC.Adjusted, n=14)
Arsi14_c <- to.monthly(Arsi14_u)
Arsi14 <- data.frame(rep(c(rep(NA,36),Arsi14_c$Arsi14_u.Close),1))
colnames(Arsi14)="RSI"

Mrsi14_u <- RSI(MRNA$MRNA.Adjusted, n=14)
Mrsi14_c <- to.monthly(Mrsi14_u)
Mrsi14 <- data.frame(rep(c(rep(NA,95),Mrsi14_c$Mrsi14_u.Close),1))
colnames(Mrsi14)="RSI"

Crsi14_u <- RSI(CSCO$CSCO.Adjusted, n=14)
Crsi14_c <- to.monthly(Crsi14_u)
Crsi14 <- data.frame(rep(Crsi14_c$Crsi14_u.Close,1))
colnames(Crsi14)="RSI"

Nrsi14_u <- RSI(NFLX$NFLX.Adjusted, n=14)
Nrsi14_c <- to.monthly(Nrsi14_u)
Nrsi14 <- data.frame(rep(Nrsi14_c$Nrsi14_u.Close,1))
colnames(Nrsi14)="RSI"

all_rsi14 <- rbind(Trsi14,Arsi14,Mrsi14,Crsi14,Nrsi14)


Tmacd_u <- MACD(TSLA$TSLA.Adjusted, nFast=12, nSlow=26, nSig=9, maType=SMA)
Tmacd_u$diff <- Tmacd_u$macd-Tmacd_u$signal
Tmacd_c <- to.monthly(Tmacd_u$diff)
Tmacd <-  data.frame(rep(c(NA,Tmacd_c$`Tmacd_u$diff.Close`),1))
colnames(Tmacd) = "MACD-Signal"

Amacd_u <- MACD(AMC$AMC.Adjusted, nFast=12, nSlow=26, nSig=9, maType=SMA)
Amacd_u$diff <- Amacd_u$macd-Amacd_u$signal
Amacd_c <- to.monthly(Amacd_u$diff)
Amacd <-  data.frame(rep(c(rep(NA,37),Amacd_c$`Amacd_u$diff.Close`),1))
colnames(Amacd) = "MACD-Signal"

Mmacd_u <- MACD(MRNA$MRNA.Adjusted, nFast=12, nSlow=26, nSig=9, maType=SMA)
Mmacd_u$diff <- Mmacd_u$macd-Mmacd_u$signal
Mmacd_c <- to.monthly(Mmacd_u$diff)
Mmacd <-  data.frame(rep(c(rep(NA,96),Mmacd_c$`Mmacd_u$diff.Close`),1))
colnames(Mmacd) = "MACD-Signal"

Cmacd_u <- MACD(CSCO$CSCO.Adjusted, nFast=12, nSlow=26, nSig=9, maType=SMA)
Cmacd_u$diff <- Cmacd_u$macd-Cmacd_u$signal
Cmacd_c <- to.monthly(Cmacd_u$diff)
Cmacd <-  data.frame(rep(c(NA,Cmacd_c$`Cmacd_u$diff.Close`),1))
colnames(Cmacd) = "MACD-Signal"

Nmacd_u <- MACD(NFLX$NFLX.Adjusted, nFast=12, nSlow=26, nSig=9, maType=SMA)
Nmacd_u$diff <- Nmacd_u$macd-Nmacd_u$signal
Nmacd_c <- to.monthly(Nmacd_u$diff)
Nmacd <-  data.frame(rep(c(NA,Nmacd_c$`Nmacd_u$diff.Close`),1))
colnames(Nmacd) = "MACD-Signal"

all_macd <- rbind(Tmacd,Amacd,Mmacd,Cmacd,Nmacd)

tech_factors <- cbind(all_sma20,all_ema14,all_bb20,all_rsi14,all_macd)


# Chart part:

getSymbols("TSLA")
chartSeries(TSLA,subset='2019-08::2021-08',type='candlesticks',
            theme=chartTheme('white',up.col='green',dn.col='red'),
            TA=c(addBBands(n=20,sd=2,),addSMA(n=50,col="blue"),addSMA(n=10,col="black"), addRSI(n=14),addVo(),addMACD()))


getSymbols("AMC")
chartSeries(AMC,subset='2019-08::2021-08',type='candlesticks',
            theme=chartTheme('white',up.col='green',dn.col='red'),
            TA=c(addBBands(n=20,sd=2,),addSMA(n=50,col="blue"),addSMA(n=10,col="black"), addRSI(n=14),addVo(),addMACD()))



getSymbols("MRNA")
chartSeries(MRNA,subset='2019-08::2021-08',type='candlesticks',
            theme=chartTheme('white',up.col='green',dn.col='red'),
            TA=c(addBBands(n=20,sd=2,),addSMA(n=50,col="blue"),addSMA(n=10,col="black"), addRSI(n=14),addVo(),addMACD()))



getSymbols("CSCO")
chartSeries(CSCO,subset='2019-08::2021-08',type='candlesticks',
            theme=chartTheme('white',up.col='green',dn.col='red'),
            TA=c(addBBands(n=20,sd=2,),addSMA(n=50,col="blue"),addSMA(n=10,col="black"), addRSI(n=14),addVo(),addMACD()))


getSymbols("NFLX")
chartSeries(NFLX,subset='2019-08::2021-08',type='candlesticks',
            theme=chartTheme('white',up.col='green',dn.col='red'),
            TA=c(addBBands(n=20,sd=2,),addSMA(n=50,col="blue"),addSMA(n=10,col="black"), addRSI(n=14),addVo(),addMACD()))




# To see narrow picture:

getSymbols("TSLA")
chartSeries(TSLA,subset='2021-01::2021-08',type='candlesticks',
            theme=chartTheme('white',up.col='green',dn.col='red'),
            TA=c(addBBands(n=20,sd=2,),addSMA(n=50,col="blue"),addSMA(n=10,col="black"), addRSI(n=14),addVo(),addMACD()))


getSymbols("AMC")
chartSeries(AMC,subset='2021-01::2021-08',type='candlesticks',
            theme=chartTheme('white',up.col='green',dn.col='red'),
            TA=c(addBBands(n=20,sd=2,),addSMA(n=50,col="blue"),addSMA(n=10,col="black"), addRSI(n=14),addVo(),addMACD()))

getSymbols("MRNA")
chartSeries(MRNA,subset='2021-01::2021-08',type='candlesticks',
            theme=chartTheme('white',up.col='green',dn.col='red'),
            TA=c(addBBands(n=20,sd=2,),addSMA(n=50,col="blue"),addSMA(n=10,col="black"), addRSI(n=14),addVo(),addMACD()))

getSymbols("CSCO")
chartSeries(CSCO,subset='2021-01::2021-08',type='candlesticks',
            theme=chartTheme('white',up.col='green',dn.col='red'),
            TA=c(addBBands(n=20,sd=2,),addSMA(n=50,col="blue"),addSMA(n=10,col="black"), addRSI(n=14),addVo(),addMACD()))

getSymbols("NFLX")
chartSeries(NFLX,subset='2021-01::2021-08',type='candlesticks',
            theme=chartTheme('white',up.col='green',dn.col='red'),
            TA=c(addBBands(n=20,sd=2,),addSMA(n=50,col="blue"),addSMA(n=10,col="black"), addRSI(n=14),addVo(),addMACD()))


# Sentimental Analysis

# For TSLA

tsla_thread <- find_thread_urls(
  keywords = "Tesla",
  sort_by = "new",
  subreddit = "wallstreetbets",
  period= "all")
tsla_thread_content <- get_thread_content(tsla_thread$url)
tsla_sent <- analyzeSentiment(tsla_thread_content$comments$comment)
tsla_sent_and_thread<-bind_cols(tsla_thread_content$comments$date,tsla_thread_content$comments$comment,tsla_sent$SentimentGI)
colnames(tsla_sent_and_thread)<-c("date","comment","sentiment score")
tesla_sentiment<- data.frame(as.Date(tsla_sent_and_thread$date),tsla_sent_and_thread$`sentiment score`)
colnames(tesla_sentiment)<-c("date","score")
tesla.sentiment<-tesla_sentiment[order(tesla_sentiment$date),]
tesla.sentiment <-na.omit(tesla.sentiment)
tesla.fin <- data.table(tesla.sentiment)
tesla_final<-tesla.fin[,sum(score),by=date]
colnames(tesla_final)[2]="score"
tesla_final <- tesla_final[1:75,]

ggplot(tesla_final,aes(date,score)) + geom_point() + scale_x_date(date_labels = "%Y %b %d") + labs(title="TSLA") + geom_hline(yintercept = 10, color="green") + geom_hline(yintercept = 0, color="red") + theme_classic()


# For AMC

amc_thread <- find_thread_urls(
  keywords = "$AMC",
  sort_by = "new",
  subreddit = "wallstreetbets",
  period= "all")
amc_thread_content <- get_thread_content(amc_thread$url)
amc_sent <- analyzeSentiment(amc_thread_content$comments$comment)
amc_sent_and_thread<-bind_cols(amc_thread_content$comments$date,amc_thread_content$comments$comment,amc_sent$SentimentGI)
colnames(amc_sent_and_thread)<-c("date","comment","sentiment score")
AMC_sentiment<- data.frame(as.Date(amc_sent_and_thread$date),amc_sent_and_thread$`sentiment score`)
colnames(AMC_sentiment)<-c("date","score")
AMC.sentiment<-AMC_sentiment[order(AMC_sentiment$date),]
AMC.sentiment <-na.omit(AMC.sentiment)
AMC.fin <- data.table(AMC.sentiment)
AMC_final<-AMC.fin[,sum(score),by=date]
colnames(AMC_final)[2]="score"
AMC_final <- AMC_final[1:63,]

ggplot(AMC_final,aes(date,score)) + geom_point() + scale_x_date(date_labels = "%Y %b %d") + labs(title="AMC") + geom_hline(yintercept = 10, color="green") + geom_hline(yintercept = 0, color="red") + theme_classic()


# For MRNA 

mrna_thread <- find_thread_urls(
  keywords = "$MRNA",
  sort_by = "new",
  subreddit = "wallstreetbets",
  period= "all")
mrna_thread_content <- get_thread_content(mrna_thread$url)
mrna_sent <- analyzeSentiment(mrna_thread_content$comments$comment)
mrna_sent_and_thread<-bind_cols(mrna_thread_content$comments$date,mrna_thread_content$comments$comment,mrna_sent$SentimentGI)
colnames(mrna_sent_and_thread)<-c("date","comment","sentiment score")
Moderna_sentiment<- data.frame(as.Date(mrna_sent_and_thread$date),mrna_sent_and_thread$`sentiment score`)
colnames(Moderna_sentiment)<-c("date","score")
Moderna.sentiment<-Moderna_sentiment[order(Moderna_sentiment$date),]
Moderna.sentiment <-na.omit(Moderna.sentiment)
Moderna.fin <- data.table(Moderna.sentiment)
Moderna_final<-Moderna.fin[,sum(score),by=date]
colnames(Moderna_final)[2]="score"
Moderna_final <- Moderna_final[126:315,]

ggplot(Moderna_final,aes(date,score)) + geom_point() + scale_x_date(date_labels = "%Y %b %d") + labs(title="MRNA") + geom_hline(yintercept = 10, color="green")+geom_hline(yintercept = 0, color="red") + theme_classic()

# For CSCO

csco_thread <- find_thread_urls(
  keywords = "$CSCO",
  sort_by = "new",
  subreddit = "wallstreetbets",
  period= "all")
csco_thread_content <- get_thread_content(csco_thread$url)
csco_sent <- analyzeSentiment(csco_thread_content$comments$comment)
csco_sent_and_thread<-bind_cols(csco_thread_content$comments$date,csco_thread_content$comments$comment,csco_sent$SentimentGI)
colnames(csco_sent_and_thread)<-c("date","comment","sentiment score")
Cisco_sentiment<- data.frame(as.Date(csco_sent_and_thread$date),csco_sent_and_thread$`sentiment score`)
colnames(Cisco_sentiment)<-c("date","score")
Cisco.sentiment<-Cisco_sentiment[order(Cisco_sentiment$date),]
Cisco.sentiment <-na.omit(Cisco.sentiment)
Cisco.fin <- data.table(Cisco.sentiment)
Cisco_final<-Cisco.fin[,sum(score),by=date]
colnames(Cisco_final)[2]="score"
Cisco_final <- Cisco_final[258:311,]

ggplot(Cisco_final,aes(date,score)) + geom_point() + scale_x_date(date_labels = "%Y %b %d") + labs(title="CSCO") + geom_hline(yintercept = 10, color="green") + geom_hline(yintercept = 0, color="red") + theme_classic()

# For NFLX

nflx_thread <- find_thread_urls(
  keywords = "Netflix",
  sort_by = "new",
  subreddit = "wallstreetbets",
  period= "all")
nflx_thread_content <- get_thread_content(nflx_thread$url)
nflx_sent <- analyzeSentiment(nflx_thread_content$comments$comment)
nflx_sent_and_thread<-bind_cols(nflx_thread_content$comments$date,nflx_thread_content$comments$comment,nflx_sent$SentimentGI) 
colnames(nflx_sent_and_thread)<-c("date","comment","sentiment score")
nflx_sentiment<- data.frame(as.Date(nflx_sent_and_thread$date),nflx_sent_and_thread$`sentiment score`)
colnames(nflx_sentiment)<-c("date","score")
nflx.sentiment<-nflx_sentiment[order(nflx_sentiment$date),]
nflx.sentiment <-na.omit(nflx.sentiment)
nflx.fin <- data.table(nflx.sentiment)
nflx_final<-nflx.fin[,sum(score),by=date]
colnames(nflx_final)[2]="score"
nflx_final <- nflx_final[42:259,]

ggplot(nflx_final,aes(date,score)) + geom_point() + ggtitle("Score for Netflix") + scale_x_date(date_labels = "%Y %b %d") + labs(title="NFLX") + geom_hline(yintercept = 10, color="green") + geom_hline(yintercept = 0, color="red") + theme_classic()


# Performance

#create date and stock name for panel data
date <- data.frame(as.Date(rep(seq(as.Date("2011/01/01"), as.Date("2021/08/31"), "months"),5)))
colnames(date)="Time"
Stock<- rep(c("TSLA","AMC","MRNA","CSCO","NFLX"),each=128)
panel1 <- cbind(Stock,date,returns,fund_matrix,funda_fact,macro_factors,tech_factors,covid_final)
Tesla <- panel1[1:128,]
AMC_enter <- panel1[165:256,]
Moderna <- panel1[353:384,]
Cisco <- panel1[385:512,]
Netflix <- panel1[513:640,]

# Our panel Data with 508 rows
panel_final <- rbind(Tesla,AMC_enter,Moderna,Cisco,Netflix)

# Linear Regression

# For TSLA

# this linear regression is final changed according to statistical analyses below:
Tesla_linear<- lm(Return~D.E + CashR + AssetT + SMA + Rdgrowth + Gross.Margin  + Stockholdergrowth + XLE + GRID + LIT + PALL + URA + FAN + AAPL + GM + F + TM + GSPC + Unemployment + Inflation_10Y + Interest_10Y + Bond_HQM + Avg_Earnings + Vehicle_Sales + Avg_Elec_Price + EMA +`BB20_%B`+ RSI + `MACD-Signal`, data = Tesla  )

# Multi-collinearity test # change or remove some variables according to analyses which deleted after done
vif(Tesla_linear)

# SMA vs EMA (EMA is chosen)
# ROE vs ROA (ROE is chosen)
# CurrentR vs CashR (CashR is chosen)
# AssetT vs InventoryT (AssetT is chosen)
# Part_Dealers and ROE are removed

# Linear Assumption at level 1%
gvmodel_tesla <- gvlma(Tesla_linear,alphalevel = 0.01)                                  
summary(gvmodel_tesla)

# There are some nonlinear relation in Xs with Y but all others are good with normality

# Output
summary(Tesla_linear)

# For AMC

# this linear regression is final changed according to statistical analyses below:
AMC_linear <- lm(Return~D.E + CashR  + Gross.Margin + Book.Value + Stockholdergrowth + DIS + GSPC + GME + Unemployment + GDP + Inflation_10Y + Interest_10Y + Bond_HQM + Avg_Earnings + EMA +`BB20_%B`+ RSI + `MACD-Signal`, data = AMC_enter)

# Multi-collinearity test # change or remove some variables according to analyses which deleted after done
vif(AMC_linear)

# CurrentR vs CashR (CashR is chosen)
# SMA vs EMA (EMA is chosen)
# ROE vs ROA (ROE is chosen)
# ROE and AssetT is removed

# Output
summary(AMC_linear)

# For MRNA

# this linear regression is final changed according to statistical analyses below:
MRNA_linear <-lm(Return~D.E + EPSgrowth + ROE + Book.Value + GSPC  + Inflation_10Y + Interest_10Y + Bond_HQM + EMA +`BB20_%B`+ RSI + `MACD-Signal`,data = Moderna)

# Multi-collinearity test # change or remove some variables according to analyses which deleted after done
vif(MRNA_linear)


# Output
summary(MRNA_linear)
# there might be problem of poor model because of dof.

# For CSCO

# this linear regression is final changed according to statistical analyses below:
CSCO_linear <- lm(Return~CashR + AssetT + InventoryT + ROE + Rdgrowth + EPSgrowth + Gross.Margin + Book.Value + Stockholdergrowth + AAPL + AMZN + GSPC + Unemployment + Inflation_10Y + Interest_10Y + Bond_HQM + Avg_Earnings + SMA +`BB20_%B`+ RSI + `MACD-Signal`, data = Cisco)


# Multi-collinearity test # change or remove some variables according to analyses which deleted after done
vif(CSCO_linear)
                                            
# Output
summary(CSCO_linear)      

# For NFLX

# this linear regression is finally changed according to statistical analyses below:
NFLX_linear <-lm(Return~D.E + AssetT  + ROE + Rdgrowth + Gross.Margin + Stockholdergrowth + AAPL + DIS + AMZN + GSPC + Unemployment + Inflation_10Y + Interest_10Y + Bond_HQM + Avg_Earnings + Avg_Elec_Price + SMA +`BB20_%B`+ RSI + `MACD-Signal`, data = Cisco)

# Multi-collinearity test # change or remove some variables according to analyses which deleted after done
vif(NFLX_linear)

# good
gvmodel_nflx <- gvlma(NFLX_linear,alphalevel = 0.01)                                  
summary(gvmodel_nflx)

# Output
summary(NFLX_linear)

# Our strategy according to all analysis and performance of our strategy

sym.vec3 <-c("TSLA","AMC","MRNA","CSCO","NFLX")
getSymbols(sym.vec3, from = "2021-09-01", to = "2021-10-01") 

TSLA <- TSLA[, "TSLA.Adjusted"]
TSLAstart <-  data.frame(rep(TSLA$TSLA.Adjusted[2],1)) 
colnames(TSLAstart) = "TSLA"
TSLAend <- data.frame(rep(TSLA$TSLA.Adjusted[20],1)) 
colnames(TSLAend) = "TSLA"
Teslaret = (TSLAend - TSLAstart)/TSLAstart

AMC <- AMC[, "AMC.Adjusted"]
AMCstart = data.frame(rep(AMC$AMC.Adjusted[2],1))
colnames(AMCstart) = "AMC"
AMCend = data.frame(rep(AMC$AMC.Adjusted[7],1))
colnames(AMCend) = "AMC"
AMcret = (AMCend - AMCstart)/AMCstart

MRNA <- MRNA[, "MRNA.Adjusted"]
MRNAstart = data.frame(rep(MRNA$MRNA.Adjusted[2],1)) 
colnames(MRNAstart) = "MRNA"
MRNAend = data.frame(rep(MRNA$MRNA.Adjusted[20],1)) 
colnames(MRNAend) = "MRNA"
MRNaret = (MRNAend - MRNAstart)/MRNAstart

CSCO<- CSCO[, "CSCO.Adjusted"]
CSCOstart = data.frame(rep(CSCO$CSCO.Adjusted[2],1)) 
colnames(CSCOstart) = "CSCO"
CSCOend = data.frame(rep(CSCO$CSCO.Adjusted[20],1)) 
colnames(CSCOend) = "CSCO"
CSCoret = (CSCOend - CSCOstart)/CSCOstart

NFLX <- NFLX[, "NFLX.Adjusted"]
NFLXstart = data.frame(rep(NFLX$NFLX.Adjusted[2],1)) 
colnames(NFLXstart) = "NFLX"
NFLXend = data.frame(rep(NFLX$NFLX.Adjusted[20],1)) 
colnames(NFLXend) = "NFLX"
NFLxret = (NFLXend - NFLXstart)/NFLXstart

# Our returns and weights
total <- matrix(as.numeric(cbind(Teslaret,AMcret,MRNaret,CSCoret,NFLxret),nrow=1))
weights <- c(1.3,0.5,-0.30,-0.2,-0.3)
portfolio_ret <- (weights%*%total)*100

# Our portfolio return according to our strategy
portfolio_ret # 17.4355 



                                                                                       
