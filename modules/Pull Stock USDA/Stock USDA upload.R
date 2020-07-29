#UPLOAD USDA stock data into STOCK Domain

## load the library

setwd("C:/Users/Giulia/Desktop/R work/Stock USDA upload")

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


##read downloaded data from USDA website-->data to be downloaded locally

StocksUSDA<-read.csv("C:/Users/Giulia/Desktop/R work/psd_alldata.csv")

StocksUSDA$unit <- NA
StocksUSDA$unit[StocksUSDA$Unit_ID==2]<- 60
StocksUSDA$unit[StocksUSDA$Unit_ID==7]<- 1000
StocksUSDA$unit[StocksUSDA$Unit_ID==8]<- 1000
StocksUSDA$unit[StocksUSDA$Unit_ID==21]<- 1
StocksUSDA$unit[StocksUSDA$Unit_ID==27]<- (217.72434*1000)/1000

StocksUSDA<- StocksUSDA%>% dplyr::mutate(ValueConv=Value*unit)

StocksUSDA<- StocksUSDA %>% group_by(Country_Code, Commodity_Code, Market_Year) %>% dplyr::mutate(Beg_stock=sum(ValueConv[Attribute_ID==20]))
StocksUSDA<- StocksUSDA %>% group_by(Country_Code, Commodity_Code, Market_Year) %>% dplyr::mutate(End_stock=sum(ValueConv[Attribute_ID==176]))
StocksUSDA<- StocksUSDA %>% group_by(Country_Code, Commodity_Code, Market_Year) %>% dplyr::mutate(Prod=sum(ValueConv[Attribute_ID==28]))

StocksUSDA<- StocksUSDA%>% dplyr::mutate(Stock_Delta=End_stock-Beg_stock)

StocksUSDA <- subset(StocksUSDA, select = c("Commodity_Code", "Commodity_Description", "Country_Code", "Country_Name",  "Market_Year", "Beg_stock", "End_stock", "Stock_Delta", "Prod"))

StocksUSDA<-unique(StocksUSDA)

#StocksUSDA<-StocksUSDA%>%dplyr:: filter(Prod!=0)

##use the datatable in sws - domain stock - check if it is transferred and permission
#USDAconv_codes=ReadDatatable("usda_mapping_codes")

##for now used excel file saved locally

USDAconv_codes<- read_excel("C:/Users/Giulia/Desktop/R work/usda_mapping_codes.xlsx")


USDAconv_codes_ITEM<-USDAconv_codes%>%dplyr::filter(element=="ITEM")
USDAconv_codes_country<-USDAconv_codes%>%dplyr::filter(element=="COUNTRY")

names(StocksUSDA)[names(StocksUSDA) == "Country_Code"] <- "usda_codes"

StocksUSDA  <- merge(StocksUSDA,USDAconv_codes_country,  by = c("usda_codes"), all.x = TRUE, all.y =TRUE)

StocksUSDA<-StocksUSDA%>%dplyr:: filter(sws_code!="NA")
StocksUSDA<-StocksUSDA%>%dplyr:: filter(Country_Name!="NA")

StocksUSDA <- subset(StocksUSDA, select = c("Commodity_Code", "Commodity_Description", "Market_Year", "Beg_stock", "End_stock", "Stock_Delta", "Prod", "sws_code"))
names(StocksUSDA)[names(StocksUSDA) == "sws_code"] <- "geographicAreaM49"

names(StocksUSDA)[names(StocksUSDA) == "Commodity_Code"] <- "usda_codes"

StocksUSDA$usda_codes<- as.integer(StocksUSDA$usda_codes)
USDAconv_codes_ITEM$usda_codes<- as.integer(USDAconv_codes_ITEM$usda_codes)

StocksUSDA  <- merge(StocksUSDA,USDAconv_codes_ITEM,  by = c("usda_codes"), all.x = TRUE, all.y =TRUE)

StocksUSDA<-StocksUSDA%>%dplyr:: filter(sws_code!="NA")

StocksUSDA <- subset(StocksUSDA, select = c("geographicAreaM49", "sws_code", "Market_Year", "Beg_stock", "End_stock", "Stock_Delta", "Prod"))


###create input file in order to apply criteria, using old file prepared by Cristina 
##(we need to write a piece of code to build this table each time)


StocksUSDA_FAOfinal2 <- read_excel("StocksUSDA_FAOfinal2.xlsx")
setnames(StocksUSDA, "geographicAreaM49", "COUNTRY")
setnames(StocksUSDA, "sws_code", "ITEM CPC_code")
setnames(StocksUSDA, "Market_Year", "YEAR")
Stock_TO_USE<-merge(StocksUSDA, StocksUSDA_FAOfinal2, by = c("COUNTRY", "ITEM CPC_code", "YEAR"), all.x = T)

Stock_TO_USE<-Stock_TO_USE[ , c(1,2,3,4,7,12,13,16)]
setnames(Stock_TO_USE, "Beg_stock", "OPENING STOCKS USDA")
setnames(Stock_TO_USE, "Prod", "ProdUSDA_T")

Stock_TO_USE<-as.data.table(Stock_TO_USE)

Stock_TO_USE[, CUMULATIVE_USDA:= as.integer(shift(`OPENING STOCKS USDA`, type= "lead"), by= list (`COUNTRY`, `ITEM CPC_code`))]

Stock_TO_USE[,`CUMULATIVE STOCKS USDA`:=NULL]

setnames(Stock_TO_USE, "CUMULATIVE_USDA", "CUMULATIVE STOCKS USDA")
setnames(Stock_TO_USE, "PRODUCTION FAO", "ProdFao")

#create production T+1 and T-1

Stock_TO_USE[, "ProdUSDA_T+1":= as.integer(shift(ProdUSDA_T, type= "lead"), by= list (`COUNTRY`, `ITEM CPC_code`))]
Stock_TO_USE[, "ProdUSDA_T-1":= as.integer(shift(ProdUSDA_T, type= "lag"), by= list (`COUNTRY`, `ITEM CPC_code`))]


##fare matching della production


##match with T

StocksUSDA_FAOfinal<-Stock_TO_USE[, MatchT := ifelse(ProdFao<=1000 & ProdUSDA_T<=1000, `ProdUSDA_T`,
                                                             ifelse (ProdFao>1000 & ProdFao <=5000 &
                                                                       ProdFao<=(ProdUSDA_T+0.4*ProdUSDA_T)&ProdFao>=(ProdUSDA_T-0.4*ProdUSDA_T),ProdUSDA_T,
                                                                     ifelse (ProdFao>5000 & ProdFao <=10000 & ProdFao<=(ProdUSDA_T+0.3*ProdUSDA_T)& 
                                                                               ProdFao>=(ProdUSDA_T-0.3*ProdUSDA_T),ProdUSDA_T,
                                                                             ifelse (ProdFao>10000 & ProdFao<=(ProdUSDA_T+0.25*ProdUSDA_T) & 
                                                                                       ProdFao>=(ProdUSDA_T-0.25*ProdUSDA_T),ProdUSDA_T,NA))))]




#consider blank 0 as matching
StocksUSDA_FAOfinal[(ProdFao==0 | is.na(ProdFao))&(ProdUSDA_T==0 | is.na(ProdUSDA_T)  ), MatchT := 0]


#match con t+1 e T-1

StocksUSDA_FAOfinal<-StocksUSDA_FAOfinal[, `MatchT+1` := ifelse(ProdFao<=1000 & `ProdUSDA_T+1`<=1000, `ProdUSDA_T+1`,
                                                             ifelse (ProdFao>1000 & ProdFao <=5000 &
                                                                       ProdFao<=(`ProdUSDA_T+1`+0.4*`ProdUSDA_T+1`)&ProdFao>=(`ProdUSDA_T+1`-0.4*`ProdUSDA_T+1`),`ProdUSDA_T+1`,
                                                                     ifelse (ProdFao>5000 & ProdFao <=10000 & ProdFao<=(`ProdUSDA_T+1`+0.3*`ProdUSDA_T+1`)& 
                                                                               ProdFao>=(`ProdUSDA_T+1`-0.3*`ProdUSDA_T+1`),`ProdUSDA_T+1`,
                                                                             ifelse (ProdFao>10000 & ProdFao<=(`ProdUSDA_T+1`+0.25*`ProdUSDA_T+1`) & 
                                                                                       ProdFao>=(`ProdUSDA_T+1`-0.25*`ProdUSDA_T+1`),`ProdUSDA_T+1`,NA))))]



StocksUSDA_FAOfinal<-StocksUSDA_FAOfinal[, `MatchT-1` := ifelse(ProdFao<=1000 & `ProdUSDA_T-1`<=1000, `ProdUSDA_T-1`,
                                                                ifelse (ProdFao>1000 & ProdFao <=5000 &
                                                                          ProdFao<=(`ProdUSDA_T-1`+0.4*`ProdUSDA_T-1`)&ProdFao>=(`ProdUSDA_T-1`-0.4*`ProdUSDA_T-1`),`ProdUSDA_T-1`,
                                                                        ifelse (ProdFao>5000 & ProdFao <=10000 & ProdFao<=(`ProdUSDA_T-1`+0.3*`ProdUSDA_T-1`)& 
                                                                                  ProdFao>=(`ProdUSDA_T-1`-0.3*`ProdUSDA_T-1`),`ProdUSDA_T-1`,
                                                                                ifelse (ProdFao>10000 & ProdFao<=(`ProdUSDA_T-1`+0.25*`ProdUSDA_T-1`) & 
                                                                                          ProdFao>=(`ProdUSDA_T-1`-0.25*`ProdUSDA_T-1`),`ProdUSDA_T-1`,NA))))]






##se non match with t, vedi match con t+1 O T+2

#Attribute the matching year 

StocksUSDA_FAOfinal[, NewUSDA := ifelse(all(!is.na(MatchT[YEAR %in% 2013:2014])), 
                                        "T",ifelse(all(!is.na(`MatchT-1`[YEAR %in% 2013:2014])), 
                                                         "T-1", ifelse(all(!is.na(`MatchT+1`[YEAR %in% 2013:2014])), 
                                                                             "T+1", NA_character_))), by = list(`COUNTRY`, `ITEM CPC_code`)] 

#sottoinsieme dei T

StocksUSDA_TimeT<-StocksUSDA_FAOfinal[StocksUSDA_FAOfinal$NewUSDA=="T"]


# # Comparison cumulative USDA adjusted and cumulative FAO 

#create cumulative USDA adjusted, considering first year equal to 0, as it is for FAO cumulative. In this way they are comparable

StocksUSDA_TimeT[, CumUSDAadj := `CUMULATIVE STOCKS USDA`- `OPENING STOCKS USDA`[YEAR==1960], by = list(`COUNTRY`, `ITEM CPC_code`)]

StocksUSDA_TimeT[, OpeningComparisonAdj := ifelse(`CUMULATIVE STOCKS FAO`<=(CumUSDAadj+0.2*CumUSDAadj) &
                                                     `CUMULATIVE STOCKS FAO`>=(CumUSDAadj-0.2*CumUSDAadj),"yes", "no")]
#give yes to NA and zero

StocksUSDA_TimeT[(`CUMULATIVE STOCKS FAO`==0 | is.na(`CUMULATIVE STOCKS FAO`) &
                    CumUSDAadj==0 | is.na(CumUSDAadj)), OpeningComparisonAdj := "yes"]

# increase percentage for differences in cumulative stocks

StocksUSDA_TimeT[, OpeningComparisonAdj2 := ifelse(`CUMULATIVE STOCKS FAO`<=(CumUSDAadj+0.5*CumUSDAadj) &
                                                    `CUMULATIVE STOCKS FAO`>=(CumUSDAadj-0.5*CumUSDAadj),"yes", "no")]
#give yes to NA and zero

StocksUSDA_TimeT[(`CUMULATIVE STOCKS FAO`==0 | is.na(`CUMULATIVE STOCKS FAO`) &
                    CumUSDAadj==0 | is.na(CumUSDAadj)), OpeningComparisonAdj2 := "yes"]


##16.06.19 increase more percentages for cumulative stock comparison (100%)


StocksUSDA_TimeT[, OpeningComparisonAdj3 := ifelse(`CUMULATIVE STOCKS FAO`<=(CumUSDAadj+1*CumUSDAadj) &
                                                     `CUMULATIVE STOCKS FAO`>=(CumUSDAadj-1*CumUSDAadj),"yes", "no")]

#give yes to NA and zero

StocksUSDA_TimeT[(`CUMULATIVE STOCKS FAO`==0 | is.na(`CUMULATIVE STOCKS FAO`) &
                    CumUSDAadj==0 | is.na(CumUSDAadj)), OpeningComparisonAdj3 := "yes"]

# table(StocksUSDA_TimeT$OpeningComparisonAdj3)

# write_xlsx(StocksUSDA_TimeT,"OpeningComparisons_changed.xlsx")  



## Filter cases with cumulative match in 2013

List <- StocksUSDA_TimeT[(OpeningComparisonAdj3=="yes"& YEAR=="2013")]

# table(List$OpeningComparisonAdj3)
# table(List$`COUNTRY DESCRIPTION`)

List<- as.data.table(List)
ListYES<-List[ ,c(1,2), with=FALSE]
StocksUSDA_TimeT<-as.data.table(StocksUSDA_TimeT)


StockYES <- merge(ListYES, StocksUSDA_TimeT, all.x=T, by = c("COUNTRY", "ITEM CPC_code"))


##SElect data to be uploaded, choose years 2014-2019

StockYES<-as.data.table(StockYES)

Stockupload<- as.data.table(StockYES[ YEAR >=2014 & YEAR <=2019])
Stockupload <- Stockupload[ , c(1,2,3,4), with=FALSE]
colnames(Stockupload) <- c("geographicAreaM49", "measuredItemCPC", "timePointYears", "Value")

Stockupload$measuredElement <-5113
Stockupload$flagObservationStatus <-"T"
Stockupload$flagMethod <-"h"

Stockupload$Value<-format(Stockupload$Value, scientific= F)

write.csv(Stockupload, "Stock_upload_07.2020.csv", row.names = F)


### Connection with SWS and direct upload


## set up for the test environment and parameters
R_SWS_SHARE_PATH = Sys.getenv("R_SWS_SHARE_PATH")

if (CheckDebug()) {
  library(faoswsModules)
  message("Not on server, so setting up environment...")
  
  # Read settings file sws.yml in working directory. See 
  # sws.yml.example for more information
  PARAMS <- ReadSettings("sws.yml")
  message("Connecting to server: ", PARAMS[["current"]])
  
  R_SWS_SHARE_PATH = PARAMS[["share"]]
  apiDirectory = "./R"
  
  ## Get SWS Parameters
   SetClientFiles(dir = PARAMS[["certdir"]])
  # SetClientFiles(dir='C:/Users/Giulia/Documents/.R/qa',p12password='changeme') 
  GetTestEnvironment(
    baseUrl = PARAMS[["server"]],
    token = PARAMS[["token"]]
  )
  
  
  batchnumber = 1 # !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! SET IT   
  
} else {
  batchnumber = 000 # !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! SET IT   
  message("Running on server, no need to call GetTestEnvironment...")
  
}


#User name is what's after the slash
SWS_USER = regmatches(swsContext.username, 
                      regexpr("(?<=/).+$", swsContext.username, perl = TRUE))

# instead of replace the existing 
# (for example save example files for different batches)
# put the name in the .yml file
# default is NULL

if(CheckDebug()){
  SUB_FOLDER = paste0(PARAMS[["subShare"]],batchnumber) 
}

#up to here connection to SWS established

##Pull data from stock and change Flag for USDA data

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

stockMetadata = GetMetadata(stockKey)

stockMetadata <- stockMetadata [ , c(1,2,3,4,9), with = FALSE]

stockMetadata_tomerge <- stockMetadata [Metadata_Value== "USDA" | Metadata_Value== "USDA_stocks.csv"]


Stock_newflag <- merge(stockMetadata_tomerge, stockDataSWS,all.x = T, by= c("geographicAreaM49", "measuredItemCPC", "timePointYears"))

Stock_newflag<-Stock_newflag[, flagMethod:= "h"] 

Stock_newflag_up<- Stock_newflag[ , c(1,2,3,4,7,8,9), with = FALSE]

setnames(Stock_newflag_up, "measuredElement.x", "measuredElement")

SaveData(domain = "Stock", dataset = "stocksdata", data= Stock_newflag_up)
                       
####PROTECT T-P and official data to not overwrite them

##Re-download stock data with new flag

stockDataSWS = GetData(stockKey)

#Contain all stock Data which are official (manually inserted)

protected_stock<- stockDataSWS %>% 
  dplyr::mutate(official=ifelse(flagObservationStatus %in% c("", "T")&flagMethod=="p",TRUE,FALSE)) %>% 
  dplyr::filter(official==TRUE) %>% 
  dplyr::select(geographicAreaM49,measuredElement,measuredItemCPC,
                timePointYears,Value,flagObservationStatus,flagMethod)

##check to perform before saving. all should be character but value
setDT(Stockupload)[, ("timePointYears") := lapply(.SD, as.character), .SDcols = "timePointYears"]

setDT(Stockupload)[, ("measuredElement") := lapply(.SD, as.character), .SDcols = "measuredElement"]

setDT(Stockupload)[, ("measuredItemCPC") := lapply(.SD, as.character), .SDcols = "measuredItemCPC"]

setDT(Stockupload)[, ("geographicAreaM49") := lapply(.SD, as.character), .SDcols = "geographicAreaM49"]

setDT(Stockupload)[, ("Value") := lapply(.SD, as.numeric), .SDcols = "Value"]


##Exclude protected values from the upload

Stockupload<-Stockupload %>% dplyr::anti_join(protected_stock,
                              by=c("geographicAreaM49","measuredElement",
                                   "measuredItemCPC","timePointYears"))

Stockupload<- as.data.table(Stockupload)

SaveData(domain = "Stock", dataset = "stocksdata", data= Stockupload)





# Stockupload <- Stockupload[complete.cases(Value), ]

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
