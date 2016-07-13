## load the library
suppressMessages({
  library(faosws)
  library(data.table)
  library(ggplot2)
  library(faoswsUtil)
  library(faoswsFlag)
  library(countrycode)
})

## set up for the test environment and parameters
#initialYear = 1961 # Change documentation if this changes!

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
  
  files = dir("~/Github/faoswsStock/R", full.names = TRUE)
  sapply(files, source)
  
}

## Stocks
stockCode <- "5071"
openingStockCode <- "5113"
stockData <- getStockData(measuredElement = c(stockCode), 
                          as.character(1999:2014))
setnames(stockData, "Value", "deltaStocks")

## Opening stocks data
# openingStockData <- getStockData(measuredElement = c(openingStockCode), 
#                           as.character(1961:2014))
# setnames(openingStockData, "Value", "openingStocks")


## Merge stockData with the flagValidTable
keys = c("flagObservationStatus", "flagMethod")
stockData <- merge(
  stockData, flagValidTable, by = keys, all.x = T
)

## Merge openingStocksData with the flagValidTable
# keys = c("flagObservationStatus", "flagMethod")
# openingStockData <- merge(
#   openingStockData, flagValidTable, by = keys, all.x = T
# )

## Filtering just Protected figures
# openingStockData <- openingStockData[Protected == TRUE]

## Production
productionData <- getProductionData(as.character(1998:2014))
productionData[, c("measuredElement") := NULL]
setnames(productionData, "Value", "production")

## Total Trade
totalTradeData <- getTotalTradeData(as.character(1998:2014))
totalTradeData <- dcast.data.table(totalTradeData, geographicAreaM49 + measuredItemCPC + 
                                     timePointYears ~ measuredElementTrade, value.var = "Value")

setnames(totalTradeData, "5610", "imports")
setnames(totalTradeData, "5910", "exports")

totalTradeData[is.na(imports), imports := 0]
totalTradeData[is.na(exports), exports := 0]
totalTradeData[, netTrade := (imports - exports)]

## Country groups
# dlpath <- file.path("C:", "Users", "caetano", "Documents", "Github", "faoswsStock",
#                     "data-raw", "class.csv")

countryGroup <- fread(system.file("extdata/class.csv", package = "faoswsStock"))
#countryGroup[, .N, GroupName]
#countryGroup[grepl("income", GroupName), .N, GroupName]

countryIncomeGroup <- countryGroup[GroupCode %in% c("HIC", "LIC", "UMC", "LMC"), ]
countryIncomeGroup[, geographicAreaM49 := as.character(countrycode(CountryCode, "wb", "iso3n"))]

# Sudan has the wrong name (it should be former Sudan)
countryIncomeGroup[geographicAreaM49 == "736", CountryName := "Sudan (former)"]
# China should be 1248
countryIncomeGroup[geographicAreaM49 == "156", geographicAreaM49 := "1248"]
#Exclude Channel Islands and Kosovo (not separately recognised by the UN)
countryIncomeGroup <- countryIncomeGroup[!is.na(geographicAreaM49)]
setnames(countryIncomeGroup, "GroupName", "incomeGroup")

## Merge Stocks, Production, Total Trade and Income
keys <- c("geographicAreaM49", "measuredItemCPC", "timePointYears")
data <- merge(stockData, productionData, by = keys, all = T)
data <- merge(data, totalTradeData[, c(keys, "netTrade"), with = F], 
              by = keys, all = T)
data <- merge(data, countryIncomeGroup[, c("geographicAreaM49", "incomeGroup"), 
                                       with = F], by = "geographicAreaM49", all.x=T)

## Groups
data[substr(measuredItemCPC, 1, 3) == "011", itemGroup := "cereals"]
data[substr(measuredItemCPC, 1, 3) == "017", itemGroup := "pulses"]
data[substr(measuredItemCPC, 1, 3) == "235", itemGroup := "refined_sugar"]

data[is.na(production), production := 0]
data[is.na(netTrade), netTrade := 0]
data[, totalSupply := production + netTrade]

setkey(data, geographicAreaM49, measuredItemCPC, timePointYears)

## Compute delta production
data[, deltaTotalSupply := c(0, diff(totalSupply)), 
     by = list(geographicAreaM49, measuredItemCPC)]

# Let's apply the coefficients fitted by AMIS data for cereals and pulses.
# For refined_sugar we will use coefficients fitted from the F.O Lichts data.


# Amis coefficients
coefficients_cereals_pulses <- fread(system.file("extdata/coefficients_cereals_pulses.csv", package = "faoswsStock"))

# Fo_Licht coefficients
coefficients_sugar <- fread(system.file("extdata/coefficients_sugar.csv", package = "faoswsStock"))

# Coefficients for USA and cereals
data[itemGroup == "cereals" & geographicAreaM49 == 840, 
     deltaStocksEstimated :=  coefficients_cereals_pulses[names.coef. == "(Intercept)", coef] +
       coefficients_cereals_pulses[names.coef. == "deltaTotalSupply", coef] * deltaTotalSupply + 
       coefficients_cereals_pulses[names.coef. == "regionUnited States of America", coef]]

# coefficients for USA and pulses
data[itemGroup == "pulses" & geographicAreaM49 == 840, 
     deltaStocksEstimated :=  coefficients_cereals_pulses[names.coef. == "(Intercept)", coef] +
       coefficients_cereals_pulses[names.coef. == "deltaTotalSupply", coef] * deltaTotalSupply + 
       coefficients_cereals_pulses[names.coef. == "regionUnited States of America", coef] +
       coefficients_cereals_pulses[names.coef. == "typepulses", coef]]

# coefficients for USA and refined_sugar
data[itemGroup == "refined_sugar" & geographicAreaM49 == 840, 
     deltaStocksEstimated :=  coefficients_sugar[names.coefSugar. == "(Intercept)", coefSugar] +
       coefficients_sugar[names.coefSugar. == "delta_total_supply", coefSugar] * deltaTotalSupply]

# coefficients for EU and cereals

data[itemGroup == "cereals" & geographicAreaM49 %in% c(40, 56, 100, 196, 203, 208,
                                                       233, 246, 250, 276, 300, 348,
                                                       372, 380, 428, 440, 442, 470,
                                                       528, 616, 620, 642, 703, 705,
                                                       724, 752, 826), 
     deltaStocksEstimated := coefficients_cereals_pulses[names.coef. == "(Intercept)", coef] +
       coefficients_cereals_pulses[names.coef. == "deltaTotalSupply", coef] * deltaTotalSupply]

# coefficients for EU and pulses

data[itemGroup == "pulses" & geographicAreaM49 %in% c(40, 56, 100, 196, 203, 208,
                                                      233, 246, 250, 276, 300, 348,
                                                      372, 380, 428, 440, 442, 470,
                                                      528, 616, 620, 642, 703, 705,
                                                      724, 752, 826), 
     deltaStocksEstimated := coefficients_cereals_pulses[names.coef. == "(Intercept)", coef] +
       coefficients_cereals_pulses[names.coef. == "deltaTotalSupply", coef] * deltaTotalSupply + 
       coefficients_cereals_pulses[names.coef. == "typepulses", coef]]

# coefficients for EU and refined_sugar
data[itemGroup == "refined_sugar" & geographicAreaM49 %in% c(40, 56, 100, 196, 203, 
                                                             208, 233, 246, 250, 276,
                                                             300, 348, 372, 380, 428,
                                                             440, 442, 470, 528, 616,
                                                             620, 642, 703, 705, 724,
                                                             752, 826), 
     deltaStocksEstimated := coefficients_sugar[names.coefSugar. == "(Intercept)", coefSugar] +
       coefficients_sugar[names.coefSugar. == "delta_total_supply", coefSugar] * deltaTotalSupply]

# coefficients for other countries

data[itemGroup == "cereals" & !(geographicAreaM49 %in% c(840, 40, 56, 100, 196, 
                                                         203, 208, 233, 246, 250,
                                                         276, 300, 348, 372, 380,
                                                         428, 440, 442, 470, 528,
                                                         616, 620, 642, 703, 705,
                                                         724, 752, 826)), 
     deltaStocksEstimated := coefficients_cereals_pulses[names.coef. == "(Intercept)", coef] + 
       coefficients_cereals_pulses[names.coef. == "deltaTotalSupply", coef] * deltaTotalSupply + 
       coefficients_cereals_pulses[names.coef. == "regionOthers countries", coef]]

data[itemGroup == "pulses" & !(geographicAreaM49 %in% c(840, 40, 56, 100, 196, 203,
                                                        208, 233, 246, 250, 276, 300,
                                                        348, 372, 380, 428, 440, 442,
                                                        470, 528, 616, 620, 642, 703,
                                                        705, 724, 752, 826)), 
     deltaStocksEstimated := coefficients_cereals_pulses[names.coef. == "(Intercept)", coef] + 
       coefficients_cereals_pulses[names.coef. == "deltaTotalSupply", coef] * deltaTotalSupply + 
       coefficients_cereals_pulses[names.coef. == "regionOthers countries", coef] + 
       coefficients_cereals_pulses[names.coef. == "typepulses", coef]]

data[itemGroup == "refined_sugar" & geographicAreaM49 %in% !(geographicAreaM49 %in% 
                                                               c(840, 40, 56, 100,
                                                                 196, 203, 208, 233,
                                                                 246, 250, 276, 300,
                                                                 348, 372, 380, 428,
                                                                 440, 442, 470, 528,
                                                                 616, 620, 642, 703,
                                                                 705, 724, 752, 826)) &
       incomeGroup %in% c("Lower middle income", "Low income"), 
     deltaStocksEstimated := coefficients_sugar[names.coefSugar. == "(Intercept)", coefSugar] + 
       coefficients_sugar[names.coefSugar. == "delta_total_supply", coefSugar] * deltaTotalSupply + 
       coefficients_sugar[names.coefSugar. == "country_groupindustrialised", coefSugar]]


data[itemGroup == "refined_sugar" & geographicAreaM49 %in% !(geographicAreaM49 %in% 
                                                               c(840, 40, 56, 100,
                                                                 196, 203, 208, 233,
                                                                 246, 250, 276, 300,
                                                                 348, 372, 380, 428,
                                                                 440, 442, 470, 528,
                                                                 616, 620, 642, 703,
                                                                 705, 724, 752, 826)) &
       incomeGroup %in% c("Upper middle income", "High income"), 
     deltaStocksEstimated := coefficients_sugar[names.coefSugar. == "(Intercept)", coefSugar] + 
       coefficients_sugar[names.coefSugar. == "delta_total_supply", coefSugar] * deltaTotalSupply]

## Merge stocks Data with Opening Stocks

# data <- merge(data, openingStockData[, c("geographicAreaM49", "measuredItemCPC",
#                                          "timePointYears", "openingStocks"), with = F],
#               all.x = T)


setkey(data, geographicAreaM49, measuredItemCPC, timePointYears)
data[Protected == FALSE | is.na(Protected), deltaStocks := deltaStocksEstimated]
data[Protected == TRUE, deltaStocks := deltaStocks]

data[is.na(deltaStocks), deltaStocks := 0]

## Calculating opening stocks
data[, openingStocksCalculated := cumsum(deltaStocks),
     by = list(geographicAreaM49, measuredItemCPC)]

## Get rid of some of the columns that we don't need anymore:
data[, c("deltaStocksEstimated", "production", "netTrade",
               "incomeGroup", "itemGroup", "totalSupply", "deltaTotalSupply") := NULL]


data <- data[timePointYears != 1998]
setkey(data, geographicAreaM49, measuredItemCPC, timePointYears)

dataDF <- data.frame(data)

for(i in seq_len(nrow(dataDF))){
  if(dataDF[i, "openingStocksCalculated"] < 0){
    dataDF[i, "deltaStocks"] <- dataDF[i, "deltaStocks"] - dataDF[i, "openingStocksCalculated"]
  }
}
dataDF$openingStocksCalculated <- cumsum(dataDF$deltaStocks)
# summary(dataDF$openingStocksCalculated)
# summary(dataDF$deltaStocks)

## Save data

## We have to make a filter using just Protected != TRUE
dataToSave <- data.table(dataDF)
dataToSave <- dataToSave[Protected == FALSE | is.na(Protected)]

dataToSave[, c("Valid", "Protected") := NULL]
dataToSave[, measuredElement := "5071"]
dataToSave[, flagObservationStatus := "I"]
dataToSave[, flagMethod := "e"]

dataToSave <- melt.data.table(dataToSave, id.vars = c("geographicAreaM49", "measuredItemCPC", 
                                        "timePointYears", "flagObservationStatus",
                                        "flagMethod", "measuredElement"), 
                measure.vars = c("deltaStocks", "openingStocksCalculated"))
dataToSave[variable == "deltaStocks", measuredElement := "5071"]
dataToSave[variable == "openingStocksCalculated", measuredElement := "5113"]

setnames(dataToSave, "value", "Value")
dataToSave[, c("variable") := NULL]

setcolorder(dataToSave,
            c("timePointYears", "geographicAreaM49", "measuredItemCPC",
              "measuredElement", "Value", "flagObservationStatus", "flagMethod"))


###
stats = SaveData(domain = "agriculture", dataset = "aproduction", data = dataToSave)

paste0(stats$inserted, " observations written, ",
       stats$ignored, " weren't updated, ",
       stats$discarded, " had problems.")

