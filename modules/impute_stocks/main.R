## This module is used to load the stock changes model and use this model for 
## predictions on an open session in the agriculture production tables.  The 
## stock change model requires an estimate of the variability of historical 
## stock changes, and this estimate always uses initialYear (set in the code
## below, currently to 1991) to the estimation year (provided by the user). 

## load the library
library(faosws)
library(data.table)
library(ggplot2)
library(faoswsUtil)

## set up for the test environment and parameters
initialYear = 1961 # Change documentation if this changes!

R_SWS_SHARE_PATH <- Sys.getenv("R_SWS_SHARE_PATH")

# This return FALSE if on the Statistical Working System
if(CheckDebug()){
  
  message("Not on server, so setting up environment...")
  
  library(faoswsModules)
  SETTINGS <- ReadSettings("modules/impute_stocks/sws.yml")
  
  # If you're not on the system, your settings will overwrite any others
  R_SWS_SHARE_PATH <- SETTINGS[["share"]]
  
  # Define where your certificates are stored
  SetClientFiles(SETTINGS[["certdir"]])
  
  # Get session information from SWS. Token must be obtained from web interface
  GetTestEnvironment(baseUrl = SETTINGS[["server"]],
                     token = SETTINGS[["token"]])
  
}

## Using the new data on SWS instead of oldSuaData
stockCode <- "5071"
prodCode <- "5510"
codes <- c(stockCode, prodCode) 
# item <- c("0111", "0112", "0114", "0115",
#           "0116", "0117")
item <- ReadDatatable("fbs_stocks_comm_codes")[fbs_key == "measuredItemCPC", 
                                               fbs_code]
key = DatasetKey(
  domain = "agriculture",
  dataset = "aproduction",
  dimensions = list(
    Dimension(
      name = "geographicAreaM49",
      keys = GetCodeList("agriculture", "aproduction", "geographicAreaM49")[, code]
    ),
    Dimension(name = "measuredElement", keys = codes),
    Dimension(name = "timePointYears", keys = as.character(1961:1998)),
    Dimension(
      name = "measuredItemCPC",
      keys = GetCodeList("agriculture", "aproduction", "measuredItemCPC")[code %in% item, code]
    )
  )
)

newSuaData <- GetData(key)

## Using just official or estimated data 
newSuaData <- newSuaData[flagObservationStatus %in% c("", "E")]

newSuaData <- dcast.data.table(
  newSuaData, geographicAreaM49 + measuredItemCPC + timePointYears ~ measuredElement,
  value.var = "Value")

setnames(newSuaData, 
         old = c("5071", "5510"), 
         new = c("deltaStocks", "production"))

for(cname in c("deltaStocks", "production")){
  newSuaData[is.na(get(cname)), c(cname) := 0]
}

setkey(newSuaData, geographicAreaM49, measuredItemCPC, timePointYears)


## Compute delta production
newSuaData[, deltaProduction := c(0, diff(production)), 
     by = list(geographicAreaM49, measuredItemCPC)]

## Fitting a model. This model should use as independent variable the 
## "Delta Total Supply" that comes from the "Total Supply" (Production + Imports)
## At the moment we are using just the "Delta Production" because there isn't data
## for Imports.
stocksModel <- lm(deltaStocks ~ deltaProduction, 
          data = newSuaData[deltaProduction != 0 & 
                              deltaStocks != 0])

## Data from 1999 to 2014
keyPredData = DatasetKey(
  domain = "agriculture",
  dataset = "aproduction",
  dimensions = list(
    Dimension(
      name = "geographicAreaM49",
      keys = GetCodeList("agriculture", "aproduction", "geographicAreaM49")[, code]
    ),
    Dimension(name = "measuredElement", keys = codes),
    Dimension(name = "timePointYears", keys = as.character(1999:2014)),
    Dimension(
      name = "measuredItemCPC",
      keys = GetCodeList("agriculture", "aproduction", "measuredItemCPC")[code %in% item, code]
    )
  )
)

finalPredictData <- GetData(keyPredData)
stocksData <- finalPredictData[measuredElement == 5071]


finalPredictData <- dcast.data.table(
  finalPredictData, geographicAreaM49 + measuredItemCPC + timePointYears ~ measuredElement,
  value.var = "Value")

setnames(finalPredictData, 
         old = c("5071", "5510"), 
         new = c("deltaStocks", "production"))

for(cname in c("deltaStocks", "production")){
  finalPredictData[is.na(get(cname)), c(cname) := 0]
}

setkey(finalPredictData, geographicAreaM49, measuredItemCPC, timePointYears)

## Compute delta supply
finalPredictData[, deltaProduction := c(0, diff(production)), 
           by = list(geographicAreaM49, measuredItemCPC)]

## Impute data 
finalPredictData[, predicted := predict(stocksModel,
                                            newdata = finalPredictData,
                                            allow.new.levels = TRUE)]

## Merge
keys = c("geographicAreaM49", "measuredItemCPC", "timePointYears")
stocksData <- merge(stocksData, finalPredictData[, 
      c("geographicAreaM49", "measuredItemCPC", "timePointYears", "predicted"), with = F],
      by = keys)


stocksData[flagObservationStatus %in% c("E", "I", "T", "M", NA), 
           Value := predicted]

stocksData[flagObservationStatus %in% c("E", "I", "T", "M", NA), 
           flagObservationStatus := "I"]

stocksData[flagObservationStatus %in% c("E", "I", "T", "M", NA), 
           flagMethod := "e"]

stocksData[, predicted := NULL]
stocksData[!is.na(Value), ]

stats = SaveData(domain = "agriculture", dataset = "aproduction", data = stocksData)

paste0(stats$inserted, " observations written, ",
       stats$ignored, " weren't updated, ",
       stats$discarded, " had problems.")
