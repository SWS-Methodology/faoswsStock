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
## R_SWS_SHARE_PATH = Sys.getenv("R_SWS_SHARE_PATH")
## R_SWS_SHARE_PATH = paste0(R_SWS_SHARE_PATH, "/browningj/stock/")
## DEBUG_MODE = Sys.getenv("R_DEBUG_MODE")
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
  
  files = dir("~/Github/faoswsStock/R", full.names = TRUE)
  sapply(files, source)
  
}

## opening stocks

## Using the new data on SWS instead of oldSuaData
stockCode <- "5071"
openingStock <- "5113"
# # prodCode <- "5510"
# codes <- c(stockCode, openingStock) 
# # item <- c("0111", "0112", "0114", "0115",
# #           "0116", "0117")
# item <- ReadDatatable("fbs_stocks_comm_codes")[fbs_key == "measuredItemCPC", 
#                                                fbs_code]
# key = DatasetKey(
#   domain = "agriculture",
#   dataset = "aproduction",
#   dimensions = list(
#     Dimension(
#       name = "geographicAreaM49",
#       keys = GetCodeList("agriculture", "aproduction", "geographicAreaM49")[, code]
#     ),
#     Dimension(name = "measuredElement", keys = codes),
#     Dimension(name = "timePointYears", keys = as.character(1961:1999)),
#     Dimension(
#       name = "measuredItemCPC",
#       keys = GetCodeList("agriculture", "aproduction", "measuredItemCPC")[code %in% item, code]
#     )
#   )
# )
# 
# newSuaData <- GetData(key)


openingStockData <- getStockData(measuredElement = c(openingStock), 
                          yearRange = as.character(1961:2014))

# stockData[, list(length(unique(measuredElement))), by=list(timePointYears, 
# geographicAreaM49, measuredItemCPC)][order(-V1)]

## Merge stockData with the flagValidTable
# keys = c("flagObservationStatus", "flagMethod")
# stockData <- merge(
#   stockData, flagValidTable, by = keys, all.x = T
# )

# stockData[, combineValueFlag
#           := paste0("(", flagObservationStatus, ", ", flagMethod, ")")]


head(flagValidTable)

keys = c("flagObservationStatus", "flagMethod")

openingStockData <- merge(openingStockData, flagValidTable, 
      by = keys, all.x = T)

setkey(openingStockData, geographicAreaM49, measuredItemCPC, timePointYears)

openingStockData <- nameData("agriculture", "aproduction", openingStockData)

## Create a .csv file
# opening_stocks_country_item <- openingStockData[Protected == TRUE, .N, c("geographicAreaM49_description", "measuredItemCPC_description")]
# setkey(opening_stocks_country_item, geographicAreaM49_description, measuredItemCPC_description)
# 
# getwd()
# 
# write.csv(opening_stocks_country_item, 
#             file = "sandbox/opening_stocks/opening_stocks_country_item.csv", row.names = F)

openingStockData[geographicAreaM49 == 100 & measuredItemCPC == "0111" & 
                   timePointYears %in% as.character(1999:2003)]

openingStockData[Protected == TRUE]



stockData[, openingStocks := cumsum(Value),
           by=list(geographicAreaM49, measuredItemCPC)]


stockData[timePointYears == 1998]

#############################################

stockData <- dcast.data.table(
  stockData, geographicAreaM49 + measuredItemCPC + timePointYears + combineValueFlag ~ measuredElement,
  value.var = "Value")

setnames(stockData, 
         old = c("5071", "5113"), 
         new = c("deltaStocks", "openingStocks"))

for(cname in c("deltaStocks", "openingStocks")){
  stockData[is.na(get(cname)), c(cname) := 0]
}

setkey(stockData, geographicAreaM49, measuredItemCPC, timePointYears)


newSuaData[, absStocks := cumsum(deltaStocks),
           by=list(geographicAreaM49, measuredItemCPC)]

newSuaData[, desltaStocks2 := deltaStocks * (-1)]


newSuaData[, absStocks2 := cumsum(desltaStocks2),
           by=list(geographicAreaM49, measuredItemCPC)]


newSuaData <- nameData("agriculture", "aproduction", newSuaData)

### 

summaryTable <- newSuaData[timePointYears >= 1981 & timePointYears <= 1998 & absStocks2 < 0, 
           list(numberNegativeFigures = length(unique(timePointYears))), 
           by=list(geographicAreaM49_description, measuredItemCPC_description)]


summaryTable <- summaryTable[order(-numberNegativeFigures)] 
summaryTable

## let's take the opening stocks and the production for these countries/commodities in 1999
country <- unique(summaryTable$geographicAreaM49_description)
item <- unique(summaryTable$measuredItemCPC_description)

year1999 <- newSuaData[geographicAreaM49_description %in% country & 
             measuredItemCPC_description %in% item & timePointYears == 1999]

# merge summaryTable and year1999


key = c("geographicAreaM49_description", "measuredItemCPC_description")

summaryTable <- merge(summaryTable, year1999[, 
                                             c("geographicAreaM49_description", 
                                               "measuredItemCPC_description", 
                                               "production", "absStocks2"), with = F],
      by = key, all.x = T)

setnames(summaryTable, "geographicAreaM49_description", "country")

setnames(summaryTable, "measuredItemCPC_description", "item")

setnames(summaryTable, "absStocks2", "opening_stocks")

write.xlsx(summaryTable, "C:/Users/caetano/Documents/summaryTable.xlsx", row.names = F)
