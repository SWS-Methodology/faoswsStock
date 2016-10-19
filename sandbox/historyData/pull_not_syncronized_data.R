## Stocks
stockCode <- "5071"
openingStockCode <- "5113"
stockData <- getStockData(measuredElement = c(stockCode), 
                          as.character(1990:2015))
setnames(stockData, "Value", "deltaStocks")

## Opening stocks data
openingStockData <- getStockData(measuredElement = c(openingStockCode),
                                 as.character(1990:2015))
setnames(openingStockData, "Value", "openingStocks")

## Merge stockData and openingStockData with the flagValidTable (specific for the stocks module)
flagValidTableStocks <- fread(system.file("extdata/flagValidTable.csv", package = "faoswsStock"))

keys = c("flagObservationStatus", "flagMethod")
stockData <- merge(
  stockData, flagValidTableStocks, by = keys, all.x = T
)

setnames(stockData, "flagObservationStatus", "flagObservationStatus_5071")
setnames(stockData, "flagMethod", "flagMethod_5071")
setnames(stockData, "Protected", "Protected_5071")

setkey(stockData, "geographicAreaM49", "measuredItemCPC", "timePointYears")
stockData[geographicAreaM49 == 840 & measuredItemCPC == "0113"]

openingStockData <- merge(
  openingStockData, flagValidTableStocks, by = keys, all.x = T
)

setnames(openingStockData, "flagObservationStatus", "flagObservationStatus_5113")
setnames(openingStockData, "flagMethod", "flagMethod_5113")

## Computing delta stocks based on opening stocks
setkey(openingStockData, "geographicAreaM49", "measuredItemCPC", "timePointYears")

openingStockData[, deltaStocksBasedonOpening := openingStocks - shift(openingStocks, type = "lead"), 
                 by = list(geographicAreaM49, measuredItemCPC)]

## This flag means whether the opening stocks used to computed the delta stocks are 
## both official or not. 
openingStockData[, flagdeltaStocksBasedonOpening_5113 := pmin(Protected, shift(Protected, type = "lead")),
                 by = list(geographicAreaM49, measuredItemCPC)]

openingStockData[is.na(flagdeltaStocksBasedonOpening_5113), 
                 flagdeltaStocksBasedonOpening_5113 := 0]
setnames(openingStockData, "Protected", "Protected_5113")

## Merge openingStockData and stockData
keys = c("geographicAreaM49", "measuredItemCPC", "timePointYears")

dataMerge <- merge(stockData, 
  openingStockData[, c("geographicAreaM49", "measuredItemCPC", "timePointYears",
                       "openingStocks", "deltaStocksBasedonOpening",
                       "Protected_5113", "flagdeltaStocksBasedonOpening_5113"),
                   with = F], all.x = T, by = keys)

dataMerge[is.na(flagdeltaStocksBasedonOpening_5113), 
                 flagdeltaStocksBasedonOpening_5113 := 0]

## There are inconsistencies when the official data for delta stocks is different
## from the delta stocks computed from the official data for opening stocks.

dataMerge[, diff := deltaStocks - deltaStocksBasedonOpening]

dataMerge <- nameData("agriculture", "aproduction", dataMerge)

##

dataProblems <- dataMerge[Protected_5071 == T & 
                flagdeltaStocksBasedonOpening_5113 == 1 & 
                diff != 0]

dataProblems[, .N, c("geographicAreaM49_description", "geographicAreaM49",
                     "measuredItemCPC_description", "measuredItemCPC")]

## Get history
# cpcStock <- getStocksCPC()
# print(paste(cpcStock, sep = "", collapse = ","), quote = FALSE)

stockKey = DatasetKey(
  domain = "agriculture",
  dataset = "aproduction",
  dimensions = list(
    Dimension(name = "geographicAreaM49",
              keys = getAllCountries()),
    Dimension(name = "measuredElement",
              keys = "5071"),
    Dimension(name = "measuredItemCPC",
              keys = getStocksCPC()),
    Dimension(name = "timePointYears",
              keys = as.character(1990:2015)))
)

historyStockData <- GetHistory(stockKey)

historyStockData[, timeDate := as.POSIXct(StartDate/1000, origin="1970-01-01")]
historyStockData[, date := as.Date(as.POSIXct(StartDate/1000, origin="1970-01-01"))]
summary(historyStockData$date)

historyStockData[, Version2 := max(Version, na.rm = T) + 1,
                 by = list(geographicAreaM49, measuredItemCPC, timePointYears)]

historyStockData[is.na(Version), Version := Version2]

## The historyStockData has duplicated rows for a combination of country-commodity-year
## and we need to remain the most up-to-date figures and its date.

historyStockData <- historyStockData[order(-historyStockData$Version)]
historyStockData$duplicated <- duplicated(historyStockData, 
           by = c("geographicAreaM49", "measuredItemCPC", "timePointYears"))

historyStockData <- historyStockData[duplicated == F]

## Get rid of some variables

historyStockData[, c("StartDate", "EndDate", "Metadata", "Metadata_Language", 
                     "Metadata_Group", "Metadata_Element", "Metadata_Value",
                     "duplicated", "Version2") := NULL]

setkey(historyStockData, geographicAreaM49, measuredItemCPC, timePointYears) 


## merge dataMerge with history

keys = c("geographicAreaM49", "measuredElement", "measuredItemCPC", "timePointYears")
dataMergeDates = merge(dataMerge, historyStockData[, c("geographicAreaM49",
                                                       "measuredElement", 
                                                       "measuredItemCPC", 
                                                       "timePointYears", 
                                                       "date"), with = F],
                       by=keys, all.x =  T)

setnames(dataMergeDates, "date", "dateDeltaStocks")

dataMergeDates[!is.na(openingStocks), measuredElement5113 := "5113"]
dataMergeDates[, measuredElement5113 := as.factor(measuredElement5113)]

# stockData <- getStockData(measuredElement = c(stockCode), 
#                           as.character(1990:2015))
# setnames(stockData, "Value", "deltaStocks")

## Let's merge stockData and stock history

# keys = c("geographicAreaM49", "measuredElement", "measuredItemCPC", "timePointYears")
# historyStockDataMerge = merge(historyStockData, stockData, by=keys, 
#       all.x =  T)
# 
# historyStockDataMerge[, diff := deltaStocks - Value]
# historyStockDataMerge[is.na(diff), .N, c("geographicAreaM49", "measuredItemCPC")]

# historyStockVersion <- historyStockDataMerge[is.na(diff)]
# 
# historyStockVersion[, c("Version", "Value", "flagObservationStatus.x", "flagMethod.x", 
#                           "flagObservationStatus.y", "flagMethod.y", "deltaStocks",
#                         "diff", "timeDate", "date") := NULL]
# write.csv(historyStockVersion, "sandbox/historyData/historyStock.csv", row.names = F)

##

openingKey = DatasetKey(
  domain = "agriculture",
  dataset = "aproduction",
  dimensions = list(
    Dimension(name = "geographicAreaM49",
              keys = getAllCountries()),
    Dimension(name = "measuredElement",
              keys = "5113"),
    Dimension(name = "measuredItemCPC",
              keys = getStocksCPC()),
    Dimension(name = "timePointYears",
              keys = as.character(1990:2015)))
)

historyOpeningData <- GetHistory(openingKey)

historyOpeningData[, timeDate := as.POSIXct(StartDate/1000, origin="1970-01-01")]
historyOpeningData[, date := as.Date(as.POSIXct(StartDate/1000, origin="1970-01-01"))]
# summary(historyOpeningData$date)

historyOpeningData[, Version2 := max(Version, na.rm = T) + 1,
                 by = list(geographicAreaM49, measuredItemCPC, timePointYears)]

historyOpeningData[is.na(Version), Version := Version2]


historyOpeningData <- historyOpeningData[order(-historyOpeningData$Version)]
historyOpeningData$duplicated <- duplicated(historyOpeningData, 
                                          by = c("geographicAreaM49", "measuredItemCPC", "timePointYears"))

historyOpeningData <- historyOpeningData[duplicated == F]

## Get rid of some variables

historyOpeningData[, c("StartDate", "EndDate", "Metadata", "Metadata_Language", 
                     "Metadata_Group", "Metadata_Element", "Metadata_Value",
                     "duplicated", "Version2") := NULL]

## merge dataMergeDates with opening history
setnames(historyOpeningData, "measuredElement", "measuredElement5113")
setnames(historyOpeningData, "date", "dateOpeningStocks")

keys = c("geographicAreaM49", "measuredElement5113", "measuredItemCPC", "timePointYears")
dataMergeDates = merge(dataMergeDates, historyOpeningData[, c("geographicAreaM49",
                                                       "measuredElement5113", 
                                                       "measuredItemCPC", 
                                                       "timePointYears", 
                                                       "dateOpeningStocks"), with = F],
                       by=keys, all.x =  T)


## Now we can check all the combination of country/commodity that is no
## sincronized delta stocks and opening stocks

dataProblems[geographicAreaM49 == 840 & measuredItemCPC == "0113"]
dataMergeDates 

##


## Corrections for the opening stocks

dataCorrected <- fread("sandbox/historyData/opening_stocks_updated_syncronized_to_save_back_to SWS.csv")
setnames(dataCorrected, "New Value", "newOpening")


sapply(dataCorrected, class)
dataCorrected[, geographicAreaM49 := as.character(geographicAreaM49)]
dataCorrected[, measuredItemCPC := as.character(measuredItemCPC)]
dataCorrected[, timePointYears := as.character(timePointYears)]


## Countries and commodities that have not syncronized data 

dataNotSyncronized <- dataMerge[
  geographicAreaM49 %in% c("840", "203", "348", "756") & 
    measuredItemCPC %in% c("0111", "0112", "0113", "0115", "0116", "0117", 
                           "01191", "01199.90")]

dataNotSyncronized[, c("timePointYears_description", "measuredElement_description",
                       "Valid", "diff") := NULL]

setnames(dataNotSyncronized, "measuredItemCPC_description", "Item")

# Let's merge dataNotSyncronized and dataCorrected

setkey(dataCorrected, geographicAreaM49, Item, timePointYears)
setkey(dataNotSyncronized, geographicAreaM49, Item, timePointYears)

keys <- c("geographicAreaM49", "Item", "timePointYears")

dataFinal <- merge(dataNotSyncronized, 
                   dataCorrected[, c("geographicAreaM49", "Item", "timePointYears", "newOpening"), with = F], 
                   all.x = T, by=keys)

dataFinal[, .N, c("geographicAreaM49_description", "Item")]
dataFinal <- dataFinal[!(geographicAreaM49 == 756 & measuredItemCPC == "0115") &
                         !(geographicAreaM49 == 203 & measuredItemCPC == "0112") &
                         !(geographicAreaM49 == 203 & measuredItemCPC == "0113") &
                         !(geographicAreaM49 == 203 & measuredItemCPC == "01191")]

dataFinal[is.na(newOpening), flagUpdated := 0]
dataFinal[!is.na(newOpening), flagUpdated := 1]

dataFinal[flagUpdated == 0, newOpening := openingStocks]
dataFinal[flagUpdated == 1]
dataFinal[, diff := openingStocks - newOpening]
dataFinal[, c("flagObservationStatus_5071", "flagMethod_5071", "measuredElement",
              "deltaStocks", "Protected_5071", "deltaStocksBasedonOpening",
              "flagdeltaStocksBasedonOpening_5113", "Protected_5113") := NULL]

setcolorder(dataFinal, c("geographicAreaM49_description", "geographicAreaM49", 
                         "Item", "measuredItemCPC", "timePointYears", 
                         "openingStocks", "newOpening", "flagUpdated", "diff"))

dataFinal <- dataFinal[flagUpdated == 1]

library(xlsx)
write.xlsx(dataFinal, "sandbox/historyData/opening_stocks_to_be_updated.xlsx", 
           row.names = F)

