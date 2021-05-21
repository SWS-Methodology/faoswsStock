
## load the library

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
  
  mydir <- "C:/Users/VALDIVIACR/FAO office folder/R/version faostandardization october 2018/faoswsStandardization/modules/FullstandardizationAndBalancing"
  
  SETTINGS <- faoswsModules::ReadSettings(file.path(mydir, "sws.yml"))
  
  SetClientFiles(SETTINGS[["certdir"]])
  
  GetTestEnvironment(baseUrl = SETTINGS[["server"]], token = SETTINGS[["token"]])
}



#PULL DATA FROM STOCK DOMAIN AND CANCEL DATA with Ef


geoKeys = GetCodeList(domain = "Stock", dataset = "stocksdata",
                      dimension = "geographicAreaM49")[type == "country", code]


stockEleDim = Dimension(name = "measuredElement",
                        keys = c("5071", "5113"))

geoDim = Dimension(name = "geographicAreaM49", keys = geoKeys)


itemKeys = GetCodeList(domain = "Stock", dataset = "stocksdata", "measuredItemCPC")
itemKeys = itemKeys[, code]

itemDim <- Dimension(name = "measuredItemCPC", keys = itemKeys)

startYear = as.character(2010)
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

stockDataSWScancel<-stockDataSWS[(flagObservationStatus== "E" & flagMethod=="f")]
stockDataSWScancel<-stockDataSWScancel[timePointYears>=2010 & timePointYears<=2013]


stockDataSWScancel <- stockDataSWScancel[, Value:=as.character(Value)]

stockDataSWScancel$Value <-""
stockDataSWScancel$flagObservationStatus <-""
stockDataSWScancel$flagMethod <-""

SaveData(domain = "Stock", dataset = "stocksdata", data= stockDataSWScancel)




################################################
##### Pull data on production and trade from SUA BAL#####
################################################


geoKeys = GetCodeList(domain = "suafbs", dataset = "sua_balanced",
                      dimension = "geographicAreaM49")[type == "country", code]

startYear = as.numeric(2010)
endYear = as.numeric(2019)
stopifnot(startYear <= endYear)
yearVals = startYear:endYear


message("Pulling data from SUA Balanced")

geoDim = Dimension(name = "geographicAreaM49", keys = geoKeys)


eleDim <- Dimension(name = "measuredElementSuaFbs", keys = c("5510", "5610", "5910"))


itemKeys = GetCodeList(domain = "suafbs", dataset = "sua_unbalanced",
                       dimension = "measuredItemFbsSua")[, code]

itemDim = Dimension(name = "measuredItemFbsSua", keys = itemKeys)
timeDim = Dimension(name = "timePointYears", keys = as.character(yearVals))
SUAKey = DatasetKey(domain = "suafbs", dataset = "sua_balanced",
                    dimensions = list(
                      geographicAreaM49 = geoDim,
                      measuredElement = eleDim,
                      measuredItemCPC = itemDim,
                      timePointYears = timeDim)
)
SUABAL = GetData(SUAKey)
setnames(SUABAL, "measuredItemFbsSua", "measuredItemCPC")
ProdFAO<-SUABAL[measuredElementSuaFbs=="5510"]
ImportData<-SUABAL[measuredElementSuaFbs=="5610"]
ExportData<-SUABAL[measuredElementSuaFbs=="5910"]

setnames(ProdFAO, "Value", "ProdFAO")
setnames(ImportData, "Value", "ImportFAO")
setnames(ExportData, "Value", "ExportFAO")


FAOdata<-merge(ProdFAO,ImportData, by = c("geographicAreaM49", "measuredItemCPC", "timePointYears"), all = T)
FAOdata<-merge(FAOdata,ExportData, by = c("geographicAreaM49", "measuredItemCPC", "timePointYears"), all=T)

FAOdata$ProdFAO[is.na(FAOdata$ProdFAO)]<-0
FAOdata$ImportFAO[is.na(FAOdata$ImportFAO)]<-0
FAOdata$ExportFAO[is.na(FAOdata$ExportFAO)]<-0
ProdFAO$ProdFAO[is.na(FAOdata$ProdFAO)]<-0


##USDA stocks 2000 to 2013. read downloaded data from USDA website-->data to be downloaded locally

StocksUSDA<-fread("C:/Users/VALDIVIACR/FAO office folder/FBS METHODOLOGY ISSUES/analysis stocks/USDA DATA/USDA FOR BACKCOMP 2010-2013 ALL COUNTRIES/psd_alldata_AT_14April2021.csv")

#convert quantities from USDA unit measurement

StocksUSDA[Unit_ID==2, unit := 60]
StocksUSDA[Unit_ID==7, unit := 1000]
StocksUSDA[Unit_ID==8, unit := 1000]
StocksUSDA[Unit_ID==21, unit := 1]
StocksUSDA[Unit_ID==27, unit := (217.72434*1000)/1000]

StocksUSDA[, ValueConv := Value*unit]

StocksUSDA[,
           `:=`(
             Beg_stock = ValueConv[Attribute_ID==20],
             End_stock = ValueConv[Attribute_ID==176],
             Prod_USDA = ValueConv[Attribute_ID==28],
             Imports_USDA=ValueConv[Attribute_ID==57],
             Exports_USDA=ValueConv[Attribute_ID==88]
             
           ),
           by = c('Country_Code', 'Commodity_Code', 'Market_Year')
           
]


#eliminate series that do not have stocks

StocksUSDA[Attribute_ID==20, BEGSTOCKyes := 1]
StocksUSDA[is.na(BEGSTOCKyes),BEGSTOCKyes:=0]

StocksUSDA[,
           `:=`(
             BEGSTOCKyes2 = max(BEGSTOCKyes)
           ),
           by = c('Country_Code', 'Commodity_Code')
]


StocksUSDA <- StocksUSDA[BEGSTOCKyes2==1]

#create delta stocks

StocksUSDA[,Stock_Delta:=End_stock-Beg_stock]


StocksUSDA <- StocksUSDA[,.(Commodity_Code, Commodity_Description, Country_Code, Country_Name,  Market_Year, Beg_stock, End_stock, Stock_Delta, Prod_USDA, Imports_USDA, Exports_USDA)]

StocksUSDA<-unique(StocksUSDA)


##MATCHING CODE USDA AND FAO FOR COMMODITIES AND COUNTRIES. CREATING COLUMNS FOR PRODCTION USDA YEAR T+1, T-1 TO MATCH USDA PRODUCTION TO FAO PRODUCTION 

USDAconv_codes=ReadDatatable("usda_mapping_codes")

USDAconv_codes_ITEM<-USDAconv_codes[element=="ITEM"]
USDAconv_codes_country<-USDAconv_codes[element=="COUNTRY"]

setnames(StocksUSDA, "Country_Code", "usda_code")

StocksUSDA  <- merge(StocksUSDA,USDAconv_codes_country,  by = c("usda_code"), all.x = TRUE, all.y =TRUE)

StocksUSDA<-StocksUSDA[!is.na(sws_code)]
StocksUSDA<-StocksUSDA[!is.na(Country_Name)]

StocksUSDA<-StocksUSDA[,.(Commodity_Code, Commodity_Description, Market_Year, Beg_stock, End_stock, Stock_Delta, Prod_USDA, Imports_USDA, Exports_USDA, sws_code)]

setnames(StocksUSDA, "sws_code", "geographicAreaM49")
setnames(StocksUSDA, "Commodity_Code", "usda_codes")

StocksUSDA <- StocksUSDA[, usda_codes:=as.numeric(usda_codes)]
USDAconv_codes_ITEM <- USDAconv_codes_ITEM[, usda_code:=as.numeric(usda_code)]

setnames(USDAconv_codes_ITEM, "usda_code", "usda_codes")

StocksUSDA  <- merge(StocksUSDA,USDAconv_codes_ITEM,  by = c("usda_codes"), all.x = TRUE, all.y =TRUE)

StocksUSDA<-StocksUSDA[!is.na(sws_code)]

StocksUSDA<-StocksUSDA[,.(geographicAreaM49, sws_code, Market_Year, Beg_stock, End_stock, Stock_Delta, Prod_USDA, Imports_USDA, Exports_USDA)]

setnames(StocksUSDA, "sws_code", "measuredItemCPC")
setnames(StocksUSDA, "Market_Year", "timePointYears")


#change class variables to allow merge of data FAO and data USDA

StocksUSDAforMerge <- StocksUSDA[, timePointYears:=as.character(timePointYears)]

StocksUSDAforMerge <- StocksUSDAforMerge[, geographicAreaM49:=as.numeric(geographicAreaM49)]
StocksUSDAforMerge <- StocksUSDAforMerge[, geographicAreaM49:=as.character(geographicAreaM49)]

StocksUSDAforMerge<-merge(StocksUSDAforMerge,ProdFAO, by = c("geographicAreaM49", "measuredItemCPC", "timePointYears"), all.x = T, all.y=T)

StocksUSDAforMerge[is.na(ProdFAO),ProdFAO:=0]

StocksUSDAforMerge<-StocksUSDAforMerge[timePointYears>=2000]

setnames(StocksUSDAforMerge, "Prod_USDA", "ProdUSDA_T")
setnames(StocksUSDAforMerge, "Beg_stock", "OPENING_STOCKS_USDA")

#create columns with Production USDA for year t+1 and year t-1

StocksUSDAforMerge[, "ProdUSDA_T+1":= as.integer(shift(ProdUSDA_T, type= "lead"), by= list ("geographicAreaM49", "measuredItemSuaFbs"))]
StocksUSDAforMerge[, "ProdUSDA_T-1":= as.integer(shift(ProdUSDA_T, type= "lag"), by= list ("geographicAreaM49", "measuredItemSuaFbs"))]

StocksUSDAforMerge <- StocksUSDAforMerge[,.(geographicAreaM49, measuredItemCPC, timePointYears, OPENING_STOCKS_USDA, End_stock, Stock_Delta, ProdUSDA_T, Imports_USDA, Exports_USDA, ProdFAO, `ProdUSDA_T+1`, `ProdUSDA_T-1`)]

##MATCH PRODUCTION BY USING MINIMUM DIFFERENCE

StocksUSDAforMerge[,
                   `:=`(
                     DiffT= abs(`ProdFAO`-`ProdUSDA_T`),
                     DiffTplus1= abs(`ProdFAO`-`ProdUSDA_T+1`),
                     DiffTmin1= abs(`ProdFAO`-`ProdUSDA_T-1`)
                   )]

StocksUSDAforMerge <- transform(StocksUSDAforMerge, min = pmin(`DiffT`, `DiffTplus1`,`DiffTmin1`))

StocksUSDAforMerge[, Match :=  ifelse((min==DiffT), 0,
                                      ifelse((min==DiffTplus1), 1,
                                             ifelse((min==DiffTmin1), -1,  NA
                                             )))]


mode_fun <- function(x) {
  
  mode0 <- names(which.max(table(x)))
  
  if(is.numeric(x)) return(as.numeric(mode0))
  
  mode0
  
}

StocksUSDAforMerge[
  ,Yearmatch := mode_fun(Match[timePointYears >= 2010 & timePointYears<= 2018 ]),
  by = c("geographicAreaM49", "measuredItemCPC")
]

StocksUSDAforMerge <- StocksUSDAforMerge[, timePointYears:=as.integer(timePointYears)]


#save year match to match year to stocks before 2014


Yearmatch <- StocksUSDAforMerge[,.(geographicAreaM49, measuredItemCPC, Yearmatch )]

Yearmatch<-unique(Yearmatch)

StocksUSDA<-merge(StocksUSDA,Yearmatch, by = c("geographicAreaM49", "measuredItemCPC"), all.x = T, all.y=T)
StocksUSDA <- StocksUSDA[, timePointYears:=as.numeric(timePointYears)]

StocksUSDA[, "Year":= (`timePointYears`-`Yearmatch`)]

StocksUSDA <- StocksUSDA [!is.na(Year)]

StocksUSDA[,timePointYears:=NULL]
setnames(StocksUSDA, "Year", "timePointYears")

StocksUSDAformatch<-StocksUSDA
StocksUSDA<-StocksUSDA[timePointYears>=2009 & timePointYears<=2018]



stockDataSWS2merge<-stockDataSWS[measuredElement==5113]


stockDataSWS2merge <- stockDataSWS2merge[,.(geographicAreaM49, measuredItemCPC, timePointYears, Value )]

stockDataSWS2merge<-unique(stockDataSWS2merge)


stockDataSWS2merge<-stockDataSWS2merge[timePointYears>=2009 & timePointYears<=2018]

stockDataSWS2merge <- stockDataSWS2merge[, timePointYears:=as.character(timePointYears)]

StocksUSDA <- StocksUSDA[, timePointYears:=as.character(timePointYears)]

Stockallsua<-merge(StocksUSDA,stockDataSWS2merge, by = c("geographicAreaM49", "measuredItemCPC", "timePointYears"), all.x=T, all.y=T)
Stockallsua <-Stockallsua[, Value:=as.integer(Value)]
Stockallsua <-Stockallsua[, Beg_stock:=as.integer(Beg_stock)]

Stockallsua[, notupdated := ifelse((Value==Beg_stock) & timePointYears>=2010 & timePointYears<=2014, 1, 0)]

Stockallsua <-Stockallsua[
  order(geographicAreaM49, measuredItemCPC, timePointYears),
  notupdated2 :=  sum(notupdated),
  by = .(geographicAreaM49, measuredItemCPC)
]

Stockallsua[, NOTUP := ifelse(notupdated2==5|(measuredItemCPC=="01921.02" & (geographicAreaM49!="140" & geographicAreaM49!="31" & geographicAreaM49!=4 & geographicAreaM49!="404" & geographicAreaM49!="768" & geographicAreaM49!="894")), 1, 0)]


StocksUSDAnotupdatedT <- Stockallsua [NOTUP==1]

Stockallsua <- Stockallsua [NOTUP!=1]
Stockallsua[,NOTUP:=NULL]


Stockallsua[, "Beg_stock_T+1":= as.integer(shift(Beg_stock, type= "lead"), by= list ("geographicAreaM49", "measuredItemSuaFbs"))]

Stockallsua[, notupdated := ifelse((Value==`Beg_stock_T+1`) & timePointYears>=2010 & timePointYears<=2014, 1, 0)]

Stockallsua <-Stockallsua[
  order(geographicAreaM49, measuredItemCPC, timePointYears),
  notupdated2 :=  sum(notupdated),
  by = .(geographicAreaM49, measuredItemCPC)
]

Stockallsua[, NOTUP := ifelse(notupdated2==5|(measuredItemCPC=="01921.02" & (geographicAreaM49=="140" | geographicAreaM49!="31")), 1, 0)]

StocksUSDAnotupdatedTplus1 <- Stockallsua [NOTUP==1]

Stockallsua <- Stockallsua [NOTUP!=1]
Stockallsua[,NOTUP:=NULL]

Stockallsua[, "Beg_stock_T-1":= as.integer(shift(Beg_stock, type= "lag"), by= list ("geographicAreaM49", "measuredItemCPC"))]


Stockallsua[, notupdated := ifelse((Value==`Beg_stock_T-1`) & timePointYears>=2010 & timePointYears<=2014, 1, 0)]

Stockallsua <-Stockallsua[
  order(geographicAreaM49, measuredItemCPC, timePointYears),
  notupdated2 :=  sum(notupdated),
  by = .(geographicAreaM49, measuredItemCPC)
]

Stockallsua[, NOTUP := ifelse(notupdated2==5|(measuredItemCPC=="01921.02" & (geographicAreaM49=="4" | geographicAreaM49!="404"| geographicAreaM49!="768"| geographicAreaM49!="894")), 1, 0)]

StocksUSDAnotupdatedTminus1<- Stockallsua [NOTUP==1]

Stockallsua <- Stockallsua [NOTUP!=1]
Stockallsua[,NOTUP:=NULL]

stocksupdated<-Stockallsua[,.(geographicAreaM49, measuredItemCPC, timePointYears)]
stocksupdated<-stocksupdated [timePointYears>=2010 & timePointYears<=2017]
stocksupdated<-stocksupdated[,.(geographicAreaM49, measuredItemCPC)]


stocksupdated<-unique(stocksupdated)

write.csv(stocksupdated, "series_with_updated_stocks2.csv", row.names =F)



YearmatchT<-StocksUSDAnotupdatedT
YearmatchT<-YearmatchT[,.(geographicAreaM49, measuredItemCPC, Yearmatch)]
YearmatchT<-unique(YearmatchT)



YearmatchTplus1<-StocksUSDAnotupdatedTplus1
YearmatchTplus1 <-YearmatchTplus1[, timePointYears:=as.numeric(timePointYears)]


YearmatchTplus1[, "Year":= (`timePointYears`+1)]


YearmatchTplus1[,timePointYears:=NULL]
setnames(YearmatchTplus1, "Year", "timePointYears")

YearmatchTplus1[, Yearmatch2:= (`Yearmatch`+1)]
YearmatchTplus1[,Yearmatch:=NULL]
setnames(YearmatchTplus1, "Yearmatch2", "Yearmatch")

YearmatchT2<-YearmatchTplus1[,.(geographicAreaM49, measuredItemCPC, Yearmatch)]
YearmatchT2<-unique(YearmatchT2)

StocksUSDA2<-YearmatchTplus1[,.(geographicAreaM49, measuredItemCPC, timePointYears, Beg_stock, Stock_Delta, Prod_USDA, Imports_USDA, Exports_USDA)]

StocksUSDA2<-StocksUSDA2 [timePointYears>=2010 & timePointYears<=2013]

YearmatchTminus1<-StocksUSDAnotupdatedTminus1
YearmatchTminus1 <-YearmatchTminus1[, timePointYears:=as.numeric(timePointYears)]


YearmatchTminus1[, "Year":= (`timePointYears`-1)]


YearmatchTminus1[,timePointYears:=NULL]
setnames(YearmatchTminus1, "Year", "timePointYears")

YearmatchTminus1[, Yearmatch2:= (`Yearmatch`-1)]
YearmatchTminus1[,Yearmatch:=NULL]
setnames(YearmatchTminus1, "Yearmatch2", "Yearmatch")

YearmatchT3<-YearmatchTminus1[,.(geographicAreaM49, measuredItemCPC, Yearmatch)]
YearmatchT3<-unique(YearmatchT3)

StocksUSDA3<-YearmatchTminus1[,.(geographicAreaM49, measuredItemCPC, timePointYears, Beg_stock, Stock_Delta, Prod_USDA, Imports_USDA, Exports_USDA)]

StocksUSDA3<-StocksUSDA3 [timePointYears>=2010 & timePointYears<=2013]


YearmatchFINAL<-rbind(YearmatchT, YearmatchT2, YearmatchT3)

write.csv(YearmatchFINAL, "YearmatchFINAL.csv", row.names = F)

StocksUSDAnotupdatedT <-StocksUSDAnotupdatedT[, timePointYears:=as.numeric(timePointYears)]

StocksUSDAnotupdatedT<-StocksUSDAnotupdatedT[,.(geographicAreaM49, measuredItemCPC, timePointYears, Beg_stock, Stock_Delta, Prod_USDA, Imports_USDA, Exports_USDA)]

StocksUSDAnotupdatedT<-StocksUSDAnotupdatedT [timePointYears>=2010 & timePointYears<=2013]

StocksUSDAfinal<-rbind(StocksUSDAnotupdatedT, StocksUSDA2, StocksUSDA3)

FAOdata<-FAOdata[timePointYears>=2010 & timePointYears<=2013]

FAOdata <- FAOdata[,.(geographicAreaM49, measuredItemCPC, timePointYears, ProdFAO, ImportFAO, ExportFAO)]

FAOdata <-FAOdata[, timePointYears:=as.numeric(timePointYears)]


StocksUSDAtest<-merge(StocksUSDAfinal,FAOdata, by = c("geographicAreaM49", "measuredItemCPC", "timePointYears"), all.x = T)

StocksUSDAtest$ProdFAO[is.na(StocksUSDAtest$ProdFAO)]<-0
StocksUSDAtest$ImportFAO[is.na(StocksUSDAtest$ImportFAO)]<-0
StocksUSDAtest$ExportFAO[is.na(StocksUSDAtest$ExportFAO)]<-0

##ONCE YEAR IS MATCHED REJECT USDA DATA WITH TOO LARGE DIFFERENCE WITH FAO DATA ON PRODUCTION

StocksUSDAtest<-StocksUSDAtest[, MatchT := ifelse(ProdFAO<=1000 & Prod_USDA<=1000, 1,
                                                  ifelse (ProdFAO>1000 & ProdFAO <=5000 &
                                                            ProdFAO<=(Prod_USDA+0.4*Prod_USDA) & ProdFAO>=(Prod_USDA-0.4*Prod_USDA),1,
                                                          ifelse (ProdFAO>5000 & ProdFAO <=10000 & ProdFAO<=(Prod_USDA+0.3*Prod_USDA) & 
                                                                    ProdFAO>=(Prod_USDA-0.3*Prod_USDA),1,
                                                                  ifelse (ProdFAO>10000 & ProdFAO<=(Prod_USDA+0.25*Prod_USDA) & 
                                                                            ProdFAO>=(Prod_USDA-0.25*Prod_USDA),1,NA))))]


#consider blank 0 as matching

StocksUSDAtest[(ProdFAO==0 | is.na(ProdFAO))&(Prod_USDA==0 | is.na(Prod_USDA)  ), MatchT := 0]
StocksUSDAtest$timePointYears<-as.integer(StocksUSDAtest$timePointYears)

StocksUSDAtest <- StocksUSDAtest[, Prod_USDA:=as.integer(Prod_USDA)]
write.csv(StocksUSDAtest, "stocks.csv", row.names = F)


StocksUSDAtest<- StocksUSDAtest %>% group_by(geographicAreaM49, measuredItemCPC) %>% dplyr::mutate(MatchT2=min(MatchT[timePointYears==2010]))

StocksUSDAtest<-as.data.table(StocksUSDAtest)
StocksUSDAtest2<-StocksUSDAtest[(MatchT2!=Inf)]


then<-StocksUSDAtest[is.na(MatchT2)]
then<-then[,.(geographicAreaM49, measuredItemCPC)]
then<-unique(then)

write.csv(then, "stockslostProductioncriteria.csv", row.names = F)


Stockseliminate<-StocksUSDAtest[is.na(MatchT2)]


Stockseliminate$Value <-""
Stockseliminate$flagObservationStatus <-""
Stockseliminate$flagMethod <-""
Stockseliminate$measuredElement <-"5113"

Stockseliminate2=Stockseliminate
Stockseliminate2$measuredElement <-"5071"

Stockseliminate3<-rbind(Stockseliminate,Stockseliminate2)

Stockseliminate3<-Stockseliminate3[,.(geographicAreaM49, measuredElement, measuredItemCPC, timePointYears, Value, flagObservationStatus, flagMethod )]


SaveData(domain = "Stock", dataset = "stocksdata", data= Stockseliminate3)






###Test using trade data to compare USDA and FAO

Stocktest<-StocksUSDAtest2


Stocktest<- Stocktest %>% dplyr::mutate(net_trade_usda=(Imports_USDA-Exports_USDA))
Stocktest<- Stocktest %>% dplyr::mutate(net_trade_fao=(ImportFAO-ExportFAO))
Stocktest<- Stocktest %>% dplyr::mutate(diff_trade=(net_trade_fao-net_trade_usda))
Stocktest<- Stocktest%>% dplyr::mutate(SupplyFAO=ProdFAO+ImportFAO-ExportFAO)

Stocktest<- Stocktest %>% dplyr::mutate(ratio_supp=(diff_trade/SupplyFAO))
Stocktest<- Stocktest %>% dplyr::mutate(ratio_delta=(Stock_Delta/SupplyFAO))
Stocktest<- Stocktest %>% dplyr::mutate(ratio_trade=(diff_trade/net_trade_fao))
Stocktest<- Stocktest %>% dplyr::mutate(trade_delta=(diff_trade/Stock_Delta))
Stocktest<- Stocktest %>% dplyr::mutate(supp_stock=(SupplyFAO-Stock_Delta))


Stocktest<-Stocktest%>%dplyr::mutate(exclude2=ifelse(supp_stock<0, 1, 0))
Stocktest<-Stocktest%>%dplyr::mutate(exclude3=ifelse((abs(ratio_supp))>10 & diff_trade!=0, 1, 0))
Stocktest<-Stocktest%>%dplyr::mutate(exclude4=ifelse((abs(ratio_supp))>1 & ((ratio_supp<0 & ratio_delta>0)|(ratio_supp>0 & ratio_delta<0)) & Stock_Delta!=0 & diff_trade!=0 &  (abs(ratio_delta))>0.1, 1, 0))
Stocktest<-Stocktest%>%dplyr::mutate(excludeFIN=ifelse((exclude2==1|exclude3==1|exclude4==1), 1, 0))

write.csv(Stocktest,"seriesTOeliminate.csv", row.names = F)

Stocktest<-as.data.table(Stocktest)

Stocktest2<-Stocktest[(excludeFIN==1)]

Stocktest2<-Stocktest2[,.(geographicAreaM49, measuredItemCPC)]

Stocktest2<-unique(Stocktest2)



write.csv(Stocktest2,"seriesTOeliminateTRADESUPPLY.csv", row.names = F)


Stocktest2$Value <-""
Stocktest2$flagObservationStatus <-""
Stocktest2$flagMethod <-""
Stocktest2$measuredElement <-"5113"
Stocktest2$timePointYears <-"2013"

Stocktest2_1<-Stocktest2
Stocktest2_1$timePointYears <-"2012"

Stocktest2_2<-Stocktest2
Stocktest2_2$timePointYears <-"2011"

Stocktest2_3<-Stocktest2
Stocktest2_3$timePointYears <-"2010"

stocktestfinal<-rbind(Stocktest2,Stocktest2_1, Stocktest2_2, Stocktest2_3)

Stocktestfinal2=stocktestfinal
Stocktestfinal2$measuredElement <-"5071"

Stocktestfinal3<-rbind(stocktestfinal,Stocktestfinal2)

SaveData(domain = "Stock", dataset = "stocksdata", data= Stocktestfinal3)



