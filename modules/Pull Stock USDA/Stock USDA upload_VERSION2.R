#UPLOAD USDA stock data into STOCK Domain after eliminating USDA stocks for years where data on production for FAO and USDA is not in line. Eliminate also USDA stocks for years in which USDA data on trade is not in line with FAO data on trade

## load the library
rm(list=ls())
setwd("C:/Users/VALDIVIACR/Documents/faoswsStock/modules/Pull Stock USDA")

library(faosws)
library(data.table)
library(faoswsUtil)
library(ggplot2)
library(faoswsBalancing)
library(faoswsModules)
library(sendmailR)
library(dplyr)
library(readxl)
# library(xlsx)
# library(writexl)

if (CheckDebug()) {
  R_SWS_SHARE_PATH <- "//hqlprsws1.hq.un.fao.org/sws_r_share"
  
  mydir <- "C:/Users/VALDIVIACR/Documents/faoswsStock/modules/Pull Stock USDA"
  
  SETTINGS <- faoswsModules::ReadSettings(file.path(mydir, "sws.yml"))
  
  SetClientFiles(SETTINGS[["certdir"]])
  
  GetTestEnvironment(baseUrl = SETTINGS[["server"]], token = SETTINGS[["token"]])
}

startYear = as.numeric(2010)
endYear = as.numeric(2019)
stopifnot(startYear <= endYear)
yearVals = startYear:endYear

##' Get data configuration and session
sessionKey = swsContext.datasets[[1]]

sessionCountries =
  getQueryKey("geographicAreaM49", sessionKey)

geoKeys = GetCodeList(domain = "agriculture", dataset = "aproduction",
                      dimension = "geographicAreaM49")[type == "country", code]


################################################
##### Pull Prduction Data #####
################################################

message("Pulling data from Agriculture Production")

## if the 

geoDim = Dimension(name = "geographicAreaM49", keys = geoKeys)


eleDim <- Dimension(name = "measuredElement", keys = c("5510"))


itemKeys = GetCodeList(domain = "agriculture", dataset = "aproduction",
                       dimension = "measuredItemCPC")[, code]

itemDim = Dimension(name = "measuredItemCPC", keys = itemKeys)
timeDim = Dimension(name = "timePointYears", keys = as.character(yearVals))
ProdKey = DatasetKey(domain = "agriculture", dataset = "aproduction",
                     dimensions = list(
                       geographicAreaM49 = geoDim,
                       measuredElement = eleDim,
                       measuredItemCPC = itemDim,
                       timePointYears = timeDim)
)
ProdFAOData = GetData(ProdKey)
setnames(ProdFAOData, c("measuredElement", "measuredItemCPC"),
         c("measuredElementSuaFbs", "measuredItemSuaFbs"))



################################################
##### Pull Imports and Exports Data #####
################################################

message("Pulling data from Trade")

eleTradeDim = Dimension(name = "measuredElementTrade",
                        keys = c("5610"))

eleTradeDim2 = Dimension(name = "measuredElementTrade",
                         keys = c("5910"))

tradeKey = DatasetKey(
  domain = "trade", dataset = "total_trade_cpc_m49",
  dimensions = list(geographicAreaM49 = geoDim,
                    measuredElementTrade = eleTradeDim,
                    measuredItemCPC = itemDim,
                    timePointYears = timeDim)
)

tradeKey2 = DatasetKey(
  domain = "trade", dataset = "total_trade_cpc_m49",
  dimensions = list(geographicAreaM49 = geoDim,
                    measuredElementTrade = eleTradeDim2,
                    measuredItemCPC = itemDim,
                    timePointYears = timeDim)
)
ImportData = GetData(tradeKey)
setnames(ImportData, c("measuredElementTrade", "measuredItemCPC"),
         c("measuredElementSuaFbs", "measuredItemSuaFbs"))

ExportData = GetData(tradeKey2)
setnames(ExportData, c("measuredElementTrade", "measuredItemCPC"),
         c("measuredElementSuaFbs", "measuredItemSuaFbs"))




################################################
##### Pull from SUA BAL #####
################################################

message("Pulling data from SUA Balanced")

geoDim = Dimension(name = "geographicAreaM49", keys = geoKeys)


eleDim <- Dimension(name = "measuredElementSuaFbs", keys = c("5510"))


itemKeys = GetCodeList(domain = "suafbs", dataset = "sua_unbalanced",
                       dimension = "measuredItemFbsSua")[, code]

itemDim = Dimension(name = "measuredItemFbsSua", keys = itemKeys)
timeDim = Dimension(name = "timePointYears", keys = as.character(yearVals))
SUAKey = DatasetKey(domain = "suafbs", dataset = "sua_unbalanced",
                    dimensions = list(
                      geographicAreaM49 = geoDim,
                      measuredElement = eleDim,
                      measuredItemCPC = itemDim,
                      timePointYears = timeDim)
)
SUABALData = GetData(SUAKey)

setnames(SUABALData, "measuredItemFbsSua", "measuredItemSuaFbs")


##read downloaded data from USDA website-->data to be downloaded locally

StocksUSDA<-read.csv("C:/Users/VALDIVIACR/Documents/faoswsStock/modules/Pull Stock USDA/psd_alldata.csv")

#convert quantities from USDA unit measurement
StocksUSDA$unit <- NA
StocksUSDA$unit[StocksUSDA$Unit_ID==2]<- 60
StocksUSDA$unit[StocksUSDA$Unit_ID==7]<- 1000
StocksUSDA$unit[StocksUSDA$Unit_ID==8]<- 1000
StocksUSDA$unit[StocksUSDA$Unit_ID==21]<- 1
StocksUSDA$unit[StocksUSDA$Unit_ID==27]<- (217.72434*1000)/1000

StocksUSDA<- StocksUSDA%>% dplyr::mutate(ValueConv=Value*unit)

StocksUSDA<- StocksUSDA %>% group_by(Country_Code, Commodity_Code, Market_Year) %>% dplyr::mutate(Beg_stock=sum(ValueConv[Attribute_ID==20]))
StocksUSDA<- StocksUSDA %>% group_by(Country_Code, Commodity_Code, Market_Year) %>% dplyr::mutate(End_stock=sum(ValueConv[Attribute_ID==176]))
StocksUSDA<- StocksUSDA %>% group_by(Country_Code, Commodity_Code, Market_Year) %>% dplyr::mutate(Prod_USDA=sum(ValueConv[Attribute_ID==28]))
StocksUSDA<- StocksUSDA %>% group_by(Country_Code, Commodity_Code, Market_Year) %>% dplyr::mutate(Imports_USDA=sum(ValueConv[Attribute_ID==57]))
StocksUSDA<- StocksUSDA %>% group_by(Country_Code, Commodity_Code, Market_Year) %>% dplyr::mutate(Exports_USDA=sum(ValueConv[Attribute_ID==88]))

#eliminate series that do not have stocks

StocksUSDA$BEGSTOCKyes[StocksUSDA$Attribute_ID==20]<- 1
StocksUSDA$BEGSTOCKyes[is.na(StocksUSDA$BEGSTOCKyes)]<-0

StocksUSDA<- StocksUSDA %>% group_by(Country_Code, Commodity_Code) %>% dplyr::mutate(BEGSTOCKyes2=max(BEGSTOCKyes))
StocksUSDA<-StocksUSDA%>%dplyr:: filter(BEGSTOCKyes2==1)

#create delta stocks and supply USDA

StocksUSDA<- StocksUSDA%>% dplyr::mutate(Stock_Delta=End_stock-Beg_stock)
StocksUSDA<- StocksUSDA%>% dplyr::mutate(Supply_USDA=Prod_USDA+Imports_USDA-Exports_USDA)

#create variable for USDA supply percentage change by year
USDA_supplychange<-StocksUSDA%>%dplyr:: filter(Attribute_ID==28)
USDA_supplychange <- USDA_supplychange%>%
  dplyr::arrange(Market_Year) %>%
  mutate(USDA_supplychange = (Supply_USDA - lag(Supply_USDA))/lag(Supply_USDA))

USDA_supplychange <- subset(USDA_supplychange, select = c("Commodity_Code", "Country_Code", "Market_Year", "USDA_supplychange"))
StocksUSDA  <- merge(StocksUSDA,USDA_supplychange,  by = c("Commodity_Code", "Country_Code", "Market_Year"), all.x = TRUE, all.y =TRUE)

#clean data, create cumulative stocks for USDA data

StocksUSDA <- subset(StocksUSDA, select = c("Commodity_Code", "Commodity_Description", "Country_Code", "Country_Name",  "Market_Year", "Beg_stock", "End_stock", "Stock_Delta", "Prod_USDA", "Imports_USDA", "Exports_USDA", "Supply_USDA", "USDA_supplychange"))

StocksUSDA<-unique(StocksUSDA)

StocksUSDA<-data.table(StocksUSDA)


StocksUSDA[, CumUSDAadj := `End_stock`- `Beg_stock`[Market_Year==1960], by = list(`Country_Code`, `Commodity_Code`)]


##MATCHING CODE USDA AND FAO FOR COMMODITIES AND COUNTRIES. CREATING COLUMNS FOR PRODCTION USDA YEAR T+1, T-1 TO MATCH USDA PRODUCTION TO FAO PRODUCTION 

USDAconv_codes=ReadDatatable("usda_mapping_codes")

USDAconv_codes$usda_code<- as.character(USDAconv_codes$usda_code)


USDAconv_codes_ITEM<-USDAconv_codes%>%dplyr::filter(element=="ITEM")
USDAconv_codes_country<-USDAconv_codes%>%dplyr::filter(element=="COUNTRY")

names(StocksUSDA)[names(StocksUSDA) == "Country_Code"] <- "usda_code"

StocksUSDA  <- merge(StocksUSDA,USDAconv_codes_country,  by = c("usda_code"), all.x = TRUE, all.y =TRUE)

StocksUSDA<-StocksUSDA%>%dplyr:: filter(sws_code!="NA")
StocksUSDA<-StocksUSDA%>%dplyr:: filter(Country_Name!="NA")

StocksUSDA <- subset(StocksUSDA, select = c("Commodity_Code", "Commodity_Description", "Market_Year", "Beg_stock", "CumUSDAadj", "End_stock", "Stock_Delta", "Prod_USDA", "Imports_USDA", "Exports_USDA", "Supply_USDA", "USDA_supplychange", "sws_code"))
names(StocksUSDA)[names(StocksUSDA) == "sws_code"] <- "geographicAreaM49"

names(StocksUSDA)[names(StocksUSDA) == "Commodity_Code"] <- "usda_codes"

StocksUSDA$usda_codes<- as.numeric(StocksUSDA$usda_codes)
USDAconv_codes_ITEM$usda_code<- as.numeric(USDAconv_codes_ITEM$usda_code)
names(USDAconv_codes_ITEM)[names(USDAconv_codes_ITEM) == "usda_code"] <- "usda_codes"


StocksUSDA  <- merge(StocksUSDA,USDAconv_codes_ITEM,  by = c("usda_codes"), all.x = TRUE, all.y =TRUE)

StocksUSDA<-StocksUSDA%>%dplyr:: filter(sws_code!="NA")

StocksUSDA <- subset(StocksUSDA, select = c("geographicAreaM49", "sws_code", "Market_Year", "Beg_stock", "CumUSDAadj", "End_stock", "Stock_Delta", "Prod_USDA", "Imports_USDA", "Exports_USDA", "Supply_USDA", "USDA_supplychange"))
setnames(StocksUSDA, "sws_code", "measuredItemSuaFbs")
setnames(StocksUSDA, "Market_Year", "timePointYears")
StocksUSDA$geographicAreaM49<- as.character(StocksUSDA$geographicAreaM49)
StocksUSDA$geographicAreaM49<- as.numeric(StocksUSDA$geographicAreaM49)
StocksUSDA$geographicAreaM49<- as.integer(StocksUSDA$geographicAreaM49)
StocksUSDA$geographicAreaM49<- as.character(StocksUSDA$geographicAreaM49)

StocksUSDA$timePointYears<- as.integer(StocksUSDA$timePointYears)
StocksUSDA$measuredItemSuaFbs<- as.character(StocksUSDA$measuredItemSuaFbs)

setnames(SUABALData, "Value", "ProdFAOSUA")
setnames(ProdFAOData, "Value", "ProdFAOPROD")

ProdFAOfin<-merge(SUABALData,ProdFAOData, by = c("geographicAreaM49", "measuredItemSuaFbs", "timePointYears"), all.x = T, all.y=T)
ProdFAOfin<-ProdFAOfin%>%dplyr::mutate(Value=ifelse(ProdFAOPROD==0|is.na(ProdFAOPROD), ProdFAOSUA, ProdFAOPROD))
ProdFAOfin <- subset(ProdFAOfin, select = c("geographicAreaM49", "measuredItemSuaFbs", "timePointYears", "Value"))
setnames(ProdFAOfin, "Value", "ProdFAO")
ProdFAOfin$timePointYears<- as.character(ProdFAOfin$timePointYears)

StocksUSDA<-merge(StocksUSDA,ProdFAOfin, by = c("geographicAreaM49", "measuredItemSuaFbs", "timePointYears"), all.x = T, all.y=T)

setnames(ImportData, "Value", "ImportFAO")
setnames(ExportData, "Value", "ExportFAO")

ImportData$timePointYears<-as.integer(ImportData$timePointYears)
ExportData$timePointYears<-as.integer(ExportData$timePointYears)

StocksUSDA<-merge(StocksUSDA,ImportData, by = c("geographicAreaM49", "measuredItemSuaFbs", "timePointYears"), all.x = T)
StocksUSDA<-merge(StocksUSDA,ExportData, by = c("geographicAreaM49", "measuredItemSuaFbs", "timePointYears"), all.x = T)
StocksUSDA$ProdFAO[is.na(StocksUSDA$ProdFAO)]<-0
StocksUSDA$ImportFAO[is.na(StocksUSDA$ImportFAO)]<-0
StocksUSDA$ExportFAO[is.na(StocksUSDA$ExportFAO)]<-0
StocksUSDA<-StocksUSDA%>%dplyr:: filter(timePointYears>=2010)

StocksUSDA<- StocksUSDA%>% dplyr::mutate(SupplyFAO=ProdFAO+ImportFAO-ExportFAO)

#create variable for FAO supply percentage change by year
StocksUSDA$timePointYears<-as.integer(StocksUSDA$timePointYears)
StocksUSDA<-arrange(StocksUSDA, geographicAreaM49, measuredItemSuaFbs, timePointYears) 
  
StocksUSDA <-StocksUSDA%>%mutate(FAO_supplychange = (SupplyFAO - lag(SupplyFAO))/lag(SupplyFAO))
StocksUSDA<-as.data.table(StocksUSDA)

setnames(StocksUSDA, "Prod_USDA", "ProdUSDA_T")
setnames(StocksUSDA, "Beg_stock", "OPENING_STOCKS_USDA")

StocksUSDA[, "ProdUSDA_T+1":= as.integer(shift(ProdUSDA_T, type= "lead"), by= list ("geographicAreaM49", "measuredItemSuaFbs"))]
StocksUSDA[, "ProdUSDA_T-1":= as.integer(shift(ProdUSDA_T, type= "lag"), by= list ("geographicAreaM49", "measuredItemSuaFbs"))]



###creating a table with closing stocks FAO by year


#StocksFAOclosing <- read_excel("StocksUSDA_FAOfinal2.xlsx")

#StocksFAOclosing<-StocksFAOclosing[ , c(1,3,5,8)]

#setnames(StocksFAOclosing, "COUNTRY", "geographicaream49")
#setnames(StocksFAOclosing, "ITEM CPC_code", "measureditemsuafbs")
#setnames(StocksFAOclosing, "YEAR", "timepointyears")
#setnames(StocksFAOclosing, "CUMULATIVE STOCKS FAO", "cumulative_stocks_fao")

#StocksFAOclosing<-StocksFAOclosing%>%dplyr:: filter(!is.na(cumulative_stocks_fao))

#StocksFAOclosing$geographicaream49<- as.integer(StocksFAOclosing$geographicaream49)
#StocksFAOclosing$measureditemsuafbs<- as.character(StocksFAOclosing$measureditemsuafbs)
#StocksFAOclosing$timepointyears<- as.integer(StocksFAOclosing$timepointyears)
#StocksFAOclosing$cumulative_stocks_fao<- as.numeric(StocksFAOclosing$cumulative_stocks_fao)

#write.csv(StocksFAOclosing, "stocks_fao_closing.csv", row.names = F)


StocksFAOclosing=ReadDatatable("stocks_fao_closing")

setnames(StocksFAOclosing, "geographicaream49", "geographicAreaM49")
setnames(StocksFAOclosing, "measureditemsuafbs", "measuredItemSuaFbs")
setnames(StocksFAOclosing, "timepointyears", "timePointYears")
setnames(StocksFAOclosing, "cumulative_stocks_fao", "CUMULATIVE STOCKS FAO")


StocksFAOclosing$geographicAreaM49<- as.character(StocksFAOclosing$geographicAreaM49)
StocksUSDA$timePointYears<- as.numeric(StocksUSDA$timePointYears)

StocksUSDA<-merge(StocksUSDA,StocksFAOclosing, by = c("geographicAreaM49", "measuredItemSuaFbs", "timePointYears"), all.x = T)

#DIVIDE DATA USDA FROM DATA FAO BEFORE MATCHING
StocksFAO<- subset(StocksUSDA, select = c("geographicAreaM49", "measuredItemSuaFbs", "timePointYears", "ProdFAO", "ImportFAO", "ExportFAO", "SupplyFAO", "FAO_supplychange", "CUMULATIVE STOCKS FAO"))
StocksUSDA<- subset(StocksUSDA, select = c("geographicAreaM49", "measuredItemSuaFbs", "timePointYears", "OPENING_STOCKS_USDA", "CumUSDAadj", "End_stock", "Stock_Delta", "ProdUSDA_T", "Imports_USDA", "Exports_USDA", "Supply_USDA",  "USDA_supplychange", "ProdFAO", "ProdUSDA_T+1", "ProdUSDA_T-1"))


##MATCH PRODUCTION BY USING MINIMUM DIFFERENCE

StocksUSDA[, "DiffT":= abs(`ProdFAO`-`ProdUSDA_T`)]
StocksUSDA[, "DiffTplus1":= abs(`ProdFAO`-`ProdUSDA_T+1`)]
StocksUSDA[, "DiffTmin1":= abs(`ProdFAO`-`ProdUSDA_T-1`)]

StocksUSDA <- transform(StocksUSDA, min = pmin(`DiffT`, `DiffTplus1`,`DiffTmin1`))

StocksUSDA[, Match :=  ifelse((min==DiffT), 0,
                                     ifelse((min==DiffTplus1), 1,
                                            ifelse((min==DiffTmin1), -1,  NA
                                            )))]


mode_fun <- function(x) {
  
  mode0 <- names(which.max(table(x)))
  
  if(is.numeric(x)) return(as.numeric(mode0))
  
  mode0
  
}

StocksUSDA[
  ,Yearmatch := mode_fun(Match[timePointYears >= 2014]),
  by = c("geographicAreaM49", "measuredItemSuaFbs")
]
StocksUSDA$timePointYears<- as.integer(StocksUSDA$timePointYears)

StocksUSDA[, "Year":= (`timePointYears`-`Yearmatch`)]
StocksUSDA[,timePointYears:=NULL]
setnames(StocksUSDA, "Year", "timePointYears")


StocksUSDA<-StocksUSDA[StocksUSDA$timePointYears>=2010]
StocksUSDA$timePointYears<-as.character(StocksUSDA$timePointYears)

StocksUSDA[,ProdFAO:=NULL]

StocksFAO$timePointYears<-as.character(StocksFAO$timePointYears)

StocksUSDA<-merge(StocksUSDA,StocksFAO, by = c("geographicAreaM49", "measuredItemSuaFbs", "timePointYears"), all.x = T, all.y=T)

StocksUSDA$ProdFAO[is.na(StocksUSDA$ProdFAO)]<-0


##ONCE YEAR IS MATCHED REJECT USDA DATA WITH TOO LARGE DIFFERENCE WITH FAO DATA ON PRODUCTION

Stocks_TO_USE<-StocksUSDA[, MatchT := ifelse(ProdFAO<=1000 & ProdUSDA_T<=1000, 1,
                                                           ifelse (ProdFAO>1000 & ProdFAO <=5000 &
                                                                     ProdFAO<=(ProdUSDA_T+0.4*ProdUSDA_T)& ProdFAO>=(ProdUSDA_T-0.4*ProdUSDA_T),1,
                                                                   ifelse (ProdFAO>5000 & ProdFAO <=10000 & ProdFAO<=(ProdUSDA_T+0.3*ProdUSDA_T)& 
                                                                             ProdFAO>=(ProdUSDA_T-0.3*ProdUSDA_T),1,
                                                                           ifelse (ProdFAO>10000 & ProdFAO<=(ProdUSDA_T+0.25*ProdUSDA_T) & 
                                                                                     ProdFAO>=(ProdUSDA_T-0.25*ProdUSDA_T),1,NA))))]
#consider blank 0 as matching

Stocks_TO_USE[(ProdFAO==0 | is.na(ProdFAO))&(ProdUSDA_T==0 | is.na(ProdUSDA_T)  ), MatchT := 0]
Stocks_TO_USE$timePointYears<-as.integer(Stocks_TO_USE$timePointYears)

Stocks_TO_USE<- Stocks_TO_USE %>% group_by(geographicAreaM49, measuredItemSuaFbs) %>% dplyr::mutate(MatchT2=min(MatchT[timePointYears==2014]))

Stocks_TO_USE<-as.data.table(Stocks_TO_USE)
Stocks_TO_USE<-Stocks_TO_USE[MatchT2!=Inf & timePointYears>=2013]

##FOR PRIMARY PRODUCTS REJECT USDA DATA WITH TOO LARGE DIFFERENCE BETWEEN USDA CUMULATIVE STOCKS AND FAO CUMULATIVE STOCKS


##16.06.19 increase more percentages for cumulative stock comparison (100%)


Stocks_TO_USE[, OpeningComparisonAdj3 := ifelse(`CUMULATIVE STOCKS FAO`<=(CumUSDAadj+1*CumUSDAadj) &
                                                         `CUMULATIVE STOCKS FAO`>=(CumUSDAadj-1*CumUSDAadj),"yes", "no")]

#give yes to NA and zero

Stocks_TO_USE[(`CUMULATIVE STOCKS FAO`==0 | is.na(`CUMULATIVE STOCKS FAO`) &
                        CumUSDAadj==0 | is.na(CumUSDAadj)), OpeningComparisonAdj3 := "yes"]


## Filter cases with cumulative match in 2013

List <- Stocks_TO_USE[(OpeningComparisonAdj3=="yes"& timePointYears=="2013")]

# table(List$OpeningComparisonAdj3)
# table(List$`COUNTRY DESCRIPTION`)

ListYES<-List[ ,c(1,2), with=FALSE]


StockYES <- merge(ListYES, Stocks_TO_USE, by = c("geographicAreaM49", "measuredItemSuaFbs"), all.x = )

##SElect data to be uploaded, choose years 2014-2019


Stockupload<- as.data.table(StockYES[ timePointYears >=2014 & timePointYears <=2019])
Stockupload2 <- Stockupload[ , c(1,2,3,4), with=FALSE]
colnames(Stockupload2) <- c("geographicAreaM49", "measuredItemCPC", "timePointYears", "Value")

Stockupload2$measuredElement <-5113
Stockupload2$flagObservationStatus <-"T"
Stockupload2$flagMethod <-"h"


###test

Stocktest<-Stockupload

Stocktest <- Stocktest[ , c(1,2,3,4,7,8,9,10,11,12,21,22,23,24,25,26)]
Stocktest<- Stocktest %>% dplyr::mutate(net_trade_usda=(Imports_USDA-Exports_USDA))


Stocktest$ProdFAO[is.na(Stocktest$ProdFAO)]<-0
Stocktest$ImportFAO[is.na(Stocktest$ImportFAO)]<-0
Stocktest$ExportFAO[is.na(Stocktest$ExportFAO)]<-0

Stocktest<- Stocktest %>% dplyr::mutate(net_trade_fao=(ImportFAO-ExportFAO))
Stocktest<- Stocktest %>% dplyr::mutate(diff_trade=(net_trade_fao-net_trade_usda))

Stocktest<- Stocktest %>% dplyr::mutate(ratio_supp=(diff_trade/SupplyFAO))
Stocktest<- Stocktest %>% dplyr::mutate(ratio_delta=(Stock_Delta/SupplyFAO))
Stocktest<- Stocktest %>% dplyr::mutate(ratio_trade=(diff_trade/net_trade_fao))
Stocktest<- Stocktest %>% dplyr::mutate(trade_delta=(diff_trade/Stock_Delta))
Stocktest<- Stocktest %>% dplyr::mutate(supp_stock=(SupplyFAO-Stock_Delta))

#Stocktest<-Stocktest%>%dplyr::mutate(exclude1=ifelse(timePointYears<=2018 & ((FAO_supplychange>=0 & USDA_supplychange<=0 & ratio_delta<(-0.08)) | (FAO_supplychange<=0 & USDA_supplychange>=0 & ratio_delta>0.08)) & !(FAO_supplychange==0 & USDA_supplychange==0) & Stock_Delta!=0 & (abs(diff_trade))>40000, 1, 0))

Stocktest<-Stocktest%>%dplyr::mutate(exclude2=ifelse(timePointYears<=2018 & supp_stock<0, 1, 0))
Stocktest<-Stocktest%>%dplyr::mutate(exclude3=ifelse(timePointYears<=2018 & (abs(ratio_supp))>10 & diff_trade!=0, 1, 0))
Stocktest<-Stocktest%>%dplyr::mutate(exclude4=ifelse(timePointYears<=2018 & (abs(ratio_supp))>1 & ((ratio_supp<0 & ratio_delta>0)|(ratio_supp>0 & ratio_delta<0)) & Stock_Delta!=0 & diff_trade!=0 &  (abs(ratio_delta))>0.1, 1, 0))
Stocktest<-Stocktest%>%dplyr::mutate(excludeFIN=ifelse((exclude2==1|exclude3==1|exclude4==1), 1, 0))
Stocktest<-Stocktest%>%dplyr::mutate(yearexclude=ifelse(excludeFIN==1, timePointYears, NA))
Stocktest <- Stocktest %>%group_by(geographicAreaM49, measuredItemSuaFbs)  %>% dplyr::  mutate(minyear=min(yearexclude, na.rm = T))
Stocktest<-Stocktest%>%dplyr::mutate(excludeFIN2=ifelse(timePointYears>=minyear, 1, 0))
Stocktest$excludeFIN2[is.na(Stocktest$excludeFIN2)]<-0

Stocktest<-Stocktest%>%dplyr:: filter(excludeFIN2==0)
Stockupload <- Stocktest[ , c(1,2,3,4)]


colnames(Stockupload) <- c("geographicAreaM49", "measuredItemCPC", "timePointYears", "Value")
Stockupload$measuredElement<-"5113"
Stockupload$flagObservationStatus<-"T"
Stockupload$flagMethod<-"h"
Stockupload<-Stockupload[,c("geographicAreaM49", "measuredItemCPC", "measuredElement", "timePointYears", "Value", "flagObservationStatus", "flagMethod")]

write.csv(Stockupload, "Stocks_uploadedSept2020.csv", row.names = F)


##Pull data from stock and change Flag for USDA data

if (CheckDebug()) {
  R_SWS_SHARE_PATH <- "//hqlprsws1.hq.un.fao.org/sws_r_share"
  
  mydir <- "D:/FAO office folder/R/version faostandardization october 2018/faoswsStandardization/modules/FullstandardizationAndBalancing"
  
  SETTINGS <- faoswsModules::ReadSettings(file.path(mydir, "sws.yml"))
  
  SetClientFiles(SETTINGS[["certdir"]])
  
  GetTestEnvironment(baseUrl = SETTINGS[["server"]], token = SETTINGS[["token"]])
}

stockEleDim = Dimension(name = "measuredElement",
                        keys = c("5071", "5113"))

itemKeys = GetCodeList(domain = "Stock", dataset = "stocksdata", "measuredItemCPC")
itemKeys = itemKeys[, code]

itemDim <- Dimension(name = "measuredItemCPC", keys = itemKeys)

geoKeys = GetCodeList(domain = "Stock", dataset = "stocksdata", "geographicAreaM49")
geoKeys = geoKeys[, code]

geoDim <- Dimension(name = "geographicAreaM49", keys = geoKeys)

startYear = as.character(2014)
endYear = as.character(2019)

yearVals = as.character(startYear:endYear)
timeDim <- Dimension(name = "timePointYears", keys = as.character(yearVals))


stockKey = DatasetKey(domain = "Stock", dataset = "stocksdata",
                      dimensions = list(
                        geographicAreaM49 = geoDim,
                        measuredElement = stockEleDim,
                        measuredItemCPC = itemDim,
                        timePointYears = timeDim)
)

stockDataSWS = GetData(stockKey)
stockDataSWS2<-stockDataSWS%>%dplyr:: filter(flagObservationStatus=="T" & flagMethod=="h")

stockDataSWS2$Value <-""
stockDataSWS2$flagObservationStatus <-""
stockDataSWS2$flagMethod <-""
stockDataSWS2<-as.data.table(stockDataSWS2)
SaveData(domain = "Stock", dataset = "stocksdata", data= stockDataSWS2)

protected_stock<- stockDataSWS %>% 
  dplyr::mutate(official=ifelse(flagObservationStatus %in% c("", "T")&flagMethod=="p",TRUE,FALSE)) %>% 
  dplyr::filter(official==TRUE) %>% 
  dplyr::select(geographicAreaM49,measuredElement,measuredItemCPC,
                timePointYears,Value,flagObservationStatus,flagMethod)


##check to perform before saving. all should be character except value
setDT(Stockupload)[, ("timePointYears") := lapply(.SD, as.character), .SDcols = "timePointYears"]

setDT(Stockupload)[, ("measuredElement") := lapply(.SD, as.character), .SDcols = "measuredElement"]

setDT(Stockupload)[, ("measuredItemCPC") := lapply(.SD, as.character), .SDcols = "measuredItemCPC"]

setDT(Stockupload)[, ("geographicAreaM49") := lapply(.SD, as.character), .SDcols = "geographicAreaM49"]

setDT(Stockupload)[, ("Value") := lapply(.SD, as.numeric), .SDcols = "Value"]


##Exclude protected values from the upload

Stockupload<-Stockupload %>% dplyr::anti_join(protected_stock,
                                              by=c("geographicAreaM49","measuredElement",
                                                   "measuredItemCPC","timePointYears"))

Stockupload<-Stockupload%>%dplyr:: filter(!is.na(Value))


Stockupload<- as.data.table(Stockupload)

SaveData(domain = "Stock", dataset = "stocksdata", data= Stockupload)



####Recalculate all stock variations based on official opening stocks


##Pull all Opening stock data from stock domain

message("Pulling data from Stock domain")

stockEleDim = Dimension(name = "measuredElement",
                        keys = "5113")

itemKeys = GetCodeList(domain = "Stock", dataset = "stocksdata", "measuredItemCPC")
itemKeys = itemKeys[, code]

itemDim <- Dimension(name = "measuredItemCPC", keys = itemKeys)

geoKeys = GetCodeList(domain = "Stock", dataset = "stocksdata", "geographicAreaM49")
geoKeys = geoKeys[, code]

geoDim <- Dimension(name = "geographicAreaM49", keys = geoKeys)

startYear = as.character(2014)
endYear = as.character(2019)

yearVals = as.character(startYear:endYear)
timeDim <- Dimension(name = "timePointYears", keys = as.character(yearVals))


stockKey = DatasetKey(domain = "Stock", dataset = "stocksdata",
                      dimensions = list(
                        geographicAreaM49 = geoDim,
                        measuredElement = stockEleDim,
                        measuredItemCPC = itemDim,
                        timePointYears = timeDim)
)

stockDataSWS = GetData(stockKey)

##Recalculate stock variations for T-h data

stockDataSWS<-stockDataSWS[flagObservationStatus=="T"&flagMethod=="h"]

stockDataSWS[,
             delta := shift(Value, type = "lag") - Value,
             by = c("geographicAreaM49", "measuredItemCPC")
]

stockDataSWS[,
             delta :=
               ifelse(
                 timePointYears == max(timePointYears) & is.na(delta),
                 0,
                 delta
               ),
             by = c("geographicAreaM49", "measuredItemCPC")
]

stockDataSWS[flagObservationStatus == "T", `:=`(delta_flag_obs = "T", delta_flag_method = "h")]


Variationtoupload <-
  stockDataSWS[
    timePointYears %in% 2014:2019,
    .(
      geographicAreaM49,
      measuredItemCPC,
      timePointYears,
      measuredElement = "5071",
      Value = delta,
      flagObservationStatus = delta_flag_obs,
      flagMethod = delta_flag_method
    )
  ]

##check to perform before saving. all should be character but value
setDT(Variationtoupload)[, ("timePointYears") := lapply(.SD, as.character), .SDcols = "timePointYears"]

setDT(Variationtoupload)[, ("measuredElement") := lapply(.SD, as.character), .SDcols = "measuredElement"]

setDT(Variationtoupload)[, ("measuredItemCPC") := lapply(.SD, as.character), .SDcols = "measuredItemCPC"]

setDT(Variationtoupload)[, ("geographicAreaM49") := lapply(.SD, as.character), .SDcols = "geographicAreaM49"]

setDT(Variationtoupload)[, ("Value") := lapply(.SD, as.numeric), .SDcols = "Value"]

###save recalculated variations

Variationtoupload<- as.data.table(Variationtoupload)

SaveData(domain = "Stock", dataset = "stocksdata", data= Variationtoupload)


##Isolation forest test
options('repos' = c(CRAN = "https://cran.rstudio.com/")) 
library(isotree)



train=Stocktest
train<-as.data.table(train)
train<-train[!is.na(train$OPENING_STOCKS_USDA)] 

cols <- c("measuredItemSuaFbs","geographicAreaM49","timePointYears","ProdUSDA_T", "Imports_USDA","Exports_USDA","ProdFAO", "ImportFAO","ExportFAO", "FAO_supplychange", "USDA_supplychange" )

iso <- isolation.forest(train[,cols,with=FALSE], ntrees = 100, nthreads = 1)
### Check which row has the highest outlier score
pred <- predict(iso, train[,cols,with=FALSE])
train$iso_score <- pred

train[order(iso_score)]

treshold <- 0.33

final_set <- train[train$iso_score>treshold]
final_set<-final_set[final_set$timePointYears<=2018] 


setnames(final_set, "measuredItemSuaFbs", "measuredItemFbsSua")

final_set <- nameData(domain = "suafbs", dataset = "sua_unbalanced", final_set)



final_set <- final_set[ , c(1,2,3,4,5,7,8,9,10,11,12,13,14,15,16,17,18,36)]
final_set<-arrange(final_set, geographicAreaM49, measuredItemFbsSua, timePointYears) 


write.csv(final_set, "Stocks_anomalous_USDA.csv", row.names = F)
