## load the library
suppressMessages({
  library(faosws)
  library(data.table)
  library(faoswsUtil)
  library(faoswsFlag)
  library(countrycode)
  # library(faoswsStock)
})

## Set up for the test environment and parameters

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

# Parameters: 
minYearToProcess <- as.numeric(ifelse(is.null(swsContext.computationParams$minYearToProcess), "1990",
                                      swsContext.computationParams$minYearToProcess))

maxYearToProcess <- as.numeric(ifelse(is.null(swsContext.computationParams$maxYearToProcess), "2016",
                                      swsContext.computationParams$maxYearToProcess))

if(minYearToProcess > maxYearToProcess | maxYearToProcess < minYearToProcess) 
  stop("Please check the time range for the years to be processed")

################################################################################
##' Obtain computation parameter, this parameter determines whether only
##' selected session should be validated or the complete domain.
validationRange = swsContext.computationParams$validation_selection

# if(CheckDebug()){
#   ## validationRange <- "session"
#   validationRange <- "all"
# }

##' Get session key and dataset configuration
sessionKey = swsContext.datasets[[1]]

##' Obtain the complete imputation Datakey
completeImputationKey = getCompleteImputationKey("stocks")

##' Selected the key based on the input parameter
selectedKey =
  switch(validationRange,
         "session" = sessionKey,
         "all" = completeImputationKey)

upToYear = 2013

## Stocks
stockCode <- "5071"
openingStockCode <- "5113"

keyStock = copy(selectedKey)
keyStock@dimensions$measuredElement@keys = "5071"
keyStock@dimensions$timePointYears@keys = as.character(minYearToProcess:(maxYearToProcess))
m49 <- keyStock@dimensions$geographicAreaM49@keys
m49 <- m49[!(m49 %in% c("831", "832"))]

# The sua_validated_2015 starts from 2000. In order to get the data from 1990-1999,
# we will keep pulling data from updated_sua_2013_data. If in future we not need the 
# data before 2000, just ignore this piece of code.

stockDataUpTo1999 <- getFAOSTAT1Data(geographicAreaM49 = m49, 
                                     measuredItemCPC = selectedKey@dimensions$measuredItemCPC@keys, 
                                     component = "71", yearRange = as.character(minYearToProcess:1999), 
                                     dataset = "updated_sua_2013_data")

setnames(stockDataUpTo1999, old = c("measuredElementFS", "Value"), 
         new = c("measuredElement", "deltaStocks"))

## WARNING: the stocks data from the old system must be multiplied by -1 as in the
## new system it's the opposite signal.

stockDataUpTo1999[, deltaStocks := deltaStocks * (-1)]

# Data from 2000 onwards. It doesn't need to be multiplied by -1.

stockSua2015Key = DatasetKey(
  domain = "suafbs",
  dataset = "sua_validated_2015",
  dimensions = list(
    Dimension(name = "geographicAreaM49",
              keys = m49),
    Dimension(name = "measuredElementSuaFbs", keys = '5071'),
    Dimension(name = "measuredItemFbsSua",
              keys = selectedKey@dimensions$measuredItemCPC@keys),
    Dimension(name = "timePointYears", keys = as.character(2000:maxYearToProcess))
  )
)

stockDataFrom2000 <- GetData(stockSua2015Key, flags = TRUE)

setnames(stockDataFrom2000, old = c("measuredElementSuaFbs", "measuredItemFbsSua", "Value"), 
         new = c("measuredElement", "measuredItemCPC", "deltaStocks"))

stockData <- rbind(stockDataUpTo1999, stockDataFrom2000)

# Changing flags
stockData[flagObservationStatus == "M" & flagMethod == "-", flagMethod := "u"]

keyOpening = copy(selectedKey)
keyOpening@domain <- "agriculture"
keyOpening@dataset <- "aproduction"
keyOpening@dimensions$measuredElement@keys = "5113"
keyOpening@dimensions$timePointYears@keys = as.character(minYearToProcess:(maxYearToProcess))
openingStockData <- GetData(keyOpening, flags = T)
setnames(openingStockData, "Value", "openingStocks")
openingStockData[flagObservationStatus == "M" & flagMethod == "-", flagMethod := "u"]

## Merge stockData and openingStockData with the flagValidTable (specific for the stocks module)
# flagValidTableStocks <- fread(system.file("extdata/flagValidTable.csv", package = "faoswsStock"))

## Warning: let's use flagValidTable from faoswsFlag

keys = c("flagObservationStatus", "flagMethod")
stockData <- merge(
  stockData, flagValidTable, by = keys, all.x = T
)

setnames(stockData, "flagObservationStatus", "flagObservationStatus_5071")
setnames(stockData, "flagMethod", "flagMethod_5071")

openingStockData <- merge(
  openingStockData, flagValidTable, by = keys, all.x = T
)

setnames(openingStockData, "flagObservationStatus", "flagObservationStatus_5113")
setnames(openingStockData, "flagMethod", "flagMethod_5113")

## Computing delta stocks based on opening stocks
setkey(openingStockData, "geographicAreaM49", "measuredItemCPC", "timePointYears")

openingStockData[, deltaStocksBasedonOpening := shift(openingStocks, type = "lead") - openingStocks, 
                  by = list(geographicAreaM49, measuredItemCPC)]

## This flag means whether the opening stocks used to computed the delta stocks are 
## both official or not. 
openingStockData[, flagdeltaStocksBasedonOpening_5113 := pmin(Protected, shift(Protected, type = "lead")),
                 by = list(geographicAreaM49, measuredItemCPC)]

openingStockData[is.na(flagdeltaStocksBasedonOpening_5113), 
                 flagdeltaStocksBasedonOpening_5113 := 0]

openingStockData <- openingStockData[timePointYears <= maxYearToProcess]

## Production
keyProd = copy(selectedKey)
keyProd@domain <- "agriculture"
keyProd@dataset <- "aproduction"
keyProd@dimensions$measuredElement@keys = "5510"
keyProd@dimensions$timePointYears@keys = as.character(minYearToProcess:(maxYearToProcess))

productionData <- GetData(keyProd, flags = T)

productionData[, c("measuredElement") := NULL]
setnames(productionData, old = c("Value", "flagObservationStatus", "flagMethod"), 
         new = c("production", "flagObservationStatus_5510", "flagMethod_5510"))

## Total Trade

# New trade
keyNewTrade = copy(selectedKey)
keyNewTrade@dimensions$measuredElement@name <- "measuredElementTrade"
keyNewTrade@dimensions$measuredElement@keys = c("5610", "5910")
keyNewTrade@dimensions$timePointYears@keys = as.character(minYearToProcess:(maxYearToProcess))
keyNewTrade@domain = "trade"
keyNewTrade@dataset = "total_trade_cpc_m49"
totalTradeData <- GetData(keyNewTrade, flags = FALSE)

totalTradeData <- dcast.data.table(totalTradeData, geographicAreaM49 + measuredItemCPC +
                                        timePointYears ~ measuredElementTrade, value.var = "Value")

setnames(totalTradeData, old=c("5610", "5910"), 
         new = c("imports", "exports"))

totalTradeData[is.na(imports), imports := 0]
totalTradeData[is.na(exports), exports := 0]
totalTradeData[, netTrade := (imports - exports)]

# countryGroup <- fread(system.file("extdata/class.csv", package = "faoswsStock"))
countryGroup <- ReadDatatable("country_group")
setnames(countryGroup, old = c("group_code","group_name","country_code","country_name"), 
         new = c("GroupCode", "GroupName", "CountryCode", "CountryName"))

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
productionData[, timePointYears := as.character(timePointYears)]
data <- merge(stockData, productionData[, c(keys, "production"), with=F], 
              by = keys, all = T)
data <- merge(data, totalTradeData[, c(keys, "imports"), with = F],
              by = keys, all = T)

## Expand data for maxYearToProcess + 1
nextYearData <- as.data.table(expand.grid(geographicAreaM49 = as.character(unique(data$geographicAreaM49)),
                                          measuredItemCPC = unique(data$measuredItemCPC),
                                          timePointYears = as.character(maxYearToProcess + 1),
                                          flagObservationStatus_5071 = NA,
                                          flagMethod_5071 = NA,
                                          measuredElemet = NA,
                                          deltaStocks = NA,
                                          Valid = NA,
                                          Protected = NA,
                                          production = NA,
                                          imports = NA))

nextYearData[, `:=` (geographicAreaM49 = as.character(geographicAreaM49),
                     measuredItemCPC = as.character(measuredItemCPC),
                     timePointYears = as.character(timePointYears),
                     flagObservationStatus_5071 = as.character(flagObservationStatus_5071),
                     measuredElemet = as.character(measuredElemet),
                     deltaStocks = as.numeric(deltaStocks),
                     production = as.numeric(production),
                     imports = as.numeric(imports))]

# Combine with the data
data <- rbind(data, nextYearData, fill = T)

data <- merge(data, countryIncomeGroup[, c("geographicAreaM49", "incomeGroup"), 
                                       with = F], by = "geographicAreaM49", all.x=T)

## Groups
data[substr(measuredItemCPC, 1, 3) == "011", itemGroup := "cereals"]
data[substr(measuredItemCPC, 1, 3) == "017", itemGroup := "pulses"]
data[substr(measuredItemCPC, 1, 3) == "235", itemGroup := "refined_sugar"]

data[is.na(production), production := 0]
data[is.na(imports), imports := 0]
data[, totalSupply := production + imports]

setkey(data, geographicAreaM49, measuredItemCPC, timePointYears)

# Let's apply the coefficients fitted by AMIS data for cereals and pulses.
# For refined_sugar we will use coefficients fitted from the F.O Lichts data.

# Amis coefficients
# coefficients_cereals_pulses <- fread(system.file("extdata/coefficients_cereals_pulses.csv", package = "faoswsStock"))
coefficients_cereals_pulses <- ReadDatatable("coefficients_cereals_pulses")
setnames(coefficients_cereals_pulses, "names_coef", "names.coef.")

# Fo_Licht coefficients
# coefficients_sugar <- fread(system.file("extdata/coefficients_sugar.csv", package = "faoswsStock"))
coefficients_sugar <- ReadDatatable("coefficients_sugar")
setnames(coefficients_sugar, "names_coef", "names.coef.")

# Excluding countries that does not have classification for "income"
data <- data[!is.na(incomeGroup)]

# Region: US, EU and others 
data[geographicAreaM49 == 840, region := "United States of America"]
data[geographicAreaM49 %in% c(40, 56, 100, 196, 203, 208, 233, 246, 250, 276, 
                              300, 348, 372, 380, 428, 440, 442, 470, 528, 616, 
                              620, 642, 703, 705, 724, 752, 826), 
     region := "European Union"]

data[!geographicAreaM49 %in% c(40, 56, 100, 196, 203, 208, 233, 246, 250, 276, 
                               300, 348, 372, 380, 428, 440, 442, 470, 528, 616, 
                               620, 642, 703, 705, 724, 752, 826, 840), 
     region := "Others countries"]

# Coefficients for USA and cereals
data[itemGroup == "cereals" & region == "United States of America", 
     closingStocksEstimated :=  coefficients_cereals_pulses[names.coef. == "(Intercept)", coef] + 
       coefficients_cereals_pulses[names.coef. == "totalSupply", coef] * totalSupply + 
       coefficients_cereals_pulses[names.coef. == "totalSupply:regionUnited States of America", coef] * totalSupply]


# coefficients for USA and pulses
data[itemGroup == "pulses" & region == "United States of America", 
     closingStocksEstimated :=  coefficients_cereals_pulses[names.coef. == "(Intercept)", coef] + 
       coefficients_cereals_pulses[names.coef. == "totalSupply", coef] * totalSupply + 
       coefficients_cereals_pulses[names.coef. == "totalSupply:regionUnited States of America", coef] * totalSupply + 
       coefficients_cereals_pulses[names.coef. == "totalSupply:typepulses", coef] * totalSupply]


# coefficients for USA and refined_sugar
data[itemGroup == "refined_sugar" & region == "United States of America",
     closingStocksEstimated :=  coefficients_sugar[names.coef. == "supply", coef] * totalSupply + 
       coefficients_sugar[names.coef. == "supply:country_groupdeveloping", coef] * totalSupply]

# coefficients for EU and cereals
data[itemGroup == "cereals" & region == "European Union", 
     closingStocksEstimated := coefficients_cereals_pulses[names.coef. == "(Intercept)", coef] + 
       coefficients_cereals_pulses[names.coef. == "totalSupply", coef] * totalSupply]

# coefficients for EU and pulses
data[itemGroup == "pulses" & region == "European Union", 
     closingStocksEstimated := coefficients_cereals_pulses[names.coef. == "(Intercept)", coef] + 
       coefficients_cereals_pulses[names.coef. == "totalSupply", coef] * totalSupply + 
       coefficients_cereals_pulses[names.coef. == "totalSupply:typepulses", coef] * totalSupply]

# coefficients for EU and refined_sugar
data[itemGroup == "refined_sugar" & region == "European Union",
     closingStocksEstimated := coefficients_sugar[names.coef. == "supply", coef] * totalSupply + 
       coefficients_sugar[names.coef. == "supply:country_groupdeveloping", coef] * totalSupply]

# coefficients for other countries and cereals
data[itemGroup == "cereals" & region == "Others countries", 
     closingStocksEstimated := coefficients_cereals_pulses[names.coef. == "(Intercept)", coef] + 
       coefficients_cereals_pulses[names.coef. == "totalSupply", coef] * totalSupply + 
       coefficients_cereals_pulses[names.coef. == "totalSupply:regionOthers countries", coef] * totalSupply]

# coefficients for other countries and pulses
data[itemGroup == "pulses" & region == "Others countries", 
     closingStocksEstimated := coefficients_cereals_pulses[names.coef. == "(Intercept)", coef] + 
       coefficients_cereals_pulses[names.coef. == "totalSupply", coef] * totalSupply + 
       coefficients_cereals_pulses[names.coef. == "totalSupply:regionOthers countries", coef] * totalSupply +
       coefficients_cereals_pulses[names.coef. == "totalSupply:typepulses", coef] * totalSupply]

# coefficients for other countries with "lower middle and low income" and refined_sugar
data[itemGroup == "refined_sugar" & region == "Others countries" &
       incomeGroup %in% c("Lower middle income", "Low income"),
     closingStocksEstimated := coefficients_sugar[names.coef. == "supply", coef] * totalSupply]

# coefficients for other countries with "upper middle and high income" and refined_sugar
data[itemGroup == "refined_sugar" & region == "Others countries" &
       incomeGroup %in% c("Upper middle income", "High income"), 
     closingStocksEstimated := coefficients_sugar[names.coef. == "supply", coef] * totalSupply +
       coefficients_sugar[names.coef. == "supply:country_groupdeveloping", coef] * totalSupply]

## Opening Stocks computed from delta stocks as a cumulated sum
tabOpeningStocksFromDelta <- computeOpeningStocksfromDelta(minYearToProcess)

# Computing opening and delta stocks
data[closingStocksEstimated < 0, closingStocksEstimated := 0]

# data[, openingStocksEstimated := closingStocksEstimated - deltaStocksEstimated]
data[, openingStocksEstimated := shift(closingStocksEstimated),
     by = list(geographicAreaM49, measuredItemCPC)]

data <- merge(data, tabOpeningStocksFromDelta, all.x = T)
data[timePointYears == minYearToProcess, openingStocksEstimated := openingStocks]
data[, "openingStocks" := NULL]

## For those countries/commodities that do not have the initial opening stocks
data[, meanCummulated := 
       cumsum(closingStocksEstimated) / seq_along(closingStocksEstimated), 
     by = list(geographicAreaM49, measuredItemCPC)]

data[, aux := replace(meanCummulated, is.na(openingStocksEstimated), meanCummulated[3]),
     by=list(geographicAreaM49, measuredItemCPC)]

data[is.na(openingStocksEstimated), openingStocksEstimated := aux]
data[, c("meanCummulated", "aux") := NULL]

data[timePointYears <= maxYearToProcess, deltaStocksEstimated := closingStocksEstimated - openingStocksEstimated]

setkey(data, geographicAreaM49, measuredItemCPC, timePointYears)

## There are two sources of information for delta stocks. Pulling this information
## directly or compute delta stocks as a function of opening stocks. There are
## cases where there is no information for delta stocks but exists the information
## for opening stocks. This happens for example with USA and wheat.
## Let's merge data with openingStockData.

setkey(openingStockData, geographicAreaM49, measuredItemCPC, timePointYears)

setnames(openingStockData, old = c("openingStocks", "Protected", "deltaStocksBasedonOpening"), 
         new = c("openingStocks_5113", "Protected_5113", "deltaStocksBasedonOpening_5113"))

keys <- c("geographicAreaM49", "measuredItemCPC", "timePointYears")

data <- merge(data, 
      openingStockData[, c(keys, "deltaStocksBasedonOpening_5113", "openingStocks_5113",
                           "Protected_5113", "flagdeltaStocksBasedonOpening_5113",
                           "flagObservationStatus_5113", "flagMethod_5113"),
                       with = F], by = keys, all.x = T)

data[is.na(Protected), Protected := FALSE]
data[is.na(Protected_5113), Protected_5113 := FALSE]
data[is.na(flagdeltaStocksBasedonOpening_5113), flagdeltaStocksBasedonOpening_5113 := 0]

## Updating delta stocks
data[Protected == FALSE & flagdeltaStocksBasedonOpening_5113 == 0, 
     deltaStocksUpdated := deltaStocksEstimated]

data[Protected == TRUE & flagdeltaStocksBasedonOpening_5113 == 0, 
     deltaStocksUpdated := deltaStocks]

data[flagdeltaStocksBasedonOpening_5113 == 1, 
     deltaStocksUpdated := deltaStocksBasedonOpening_5113]

data[Protected == TRUE, deltaStocksUpdated := deltaStocks]

## Updating opening stocks
data[Protected_5113 == FALSE, openingStocksEstimatedUpdated := openingStocksEstimated]
data[Protected_5113 == TRUE, openingStocksEstimatedUpdated := openingStocks_5113]

## We can split the data into 2 parts. The first one with official data and the
## another one with not official data for delta stocks. If there is at least one official figure for a
## country/commodity all the values for this combination will go to 
## the "official data".

data[Protected == TRUE | flagdeltaStocksBasedonOpening_5113 == 1 | Protected_5113 == TRUE, 
     protectedFlag := TRUE]

data[Protected == FALSE & flagdeltaStocksBasedonOpening_5113 == 0 & Protected_5113 == FALSE,
     protectedFlag := FALSE]

## From the combination of minFlag and maxFlag is easy to detect if the country/item
## has just official data, or just unofficial or a mixture.
data[, minFlag := min(protectedFlag), 
     by = list(geographicAreaM49, measuredItemCPC)]

data[, maxFlag := max(protectedFlag), 
     by = list(geographicAreaM49, measuredItemCPC)]

data[maxFlag == 0 & minFlag == 0, flagMix := "unofficial"]
data[maxFlag == 1 & minFlag == 1, flagMix := "official"]
data[maxFlag == 1 & minFlag == 0, flagMix := "mix"]

## Only unofficial figures
onlyUnofficialFigures <- data[flagMix == "unofficial"]

onlyUnofficialFigures[, c("deltaStocks", "production", "imports", "incomeGroup",
                            "itemGroup", "totalSupply", "region", "closingStocksEstimated",
                            "openingStocksEstimated", "deltaStocksEstimated", 
                            "deltaStocksBasedonOpening_5113", "openingStocks_5113",
                            "minFlag", "maxFlag") := NULL]
  
setnames(onlyUnofficialFigures, "deltaStocksUpdated", "deltaStocks")
setnames(onlyUnofficialFigures, "openingStocksEstimatedUpdated", "openingStocks")
  
onlyUnofficialFigures <- melt.data.table(
    onlyUnofficialFigures, 
    id.vars = c("geographicAreaM49", "measuredItemCPC", "timePointYears", 
                "flagObservationStatus_5071", "flagMethod_5071", "Protected",
                "flagObservationStatus_5113", "flagMethod_5113", "Protected_5113",
                "flagdeltaStocksBasedonOpening_5113", "flagMix"), 
    measure.vars = c("deltaStocks", "openingStocks"))
  
onlyUnofficialFigures[variable == "deltaStocks", measuredElement := "5071"]
onlyUnofficialFigures[variable == "openingStocks", measuredElement := "5113"]
onlyUnofficialFigures[, c("variable") := NULL]
setnames(onlyUnofficialFigures, "value", "Value")

if(nrow(onlyUnofficialFigures) > 0) {  
# flags for delta stock 
onlyUnofficialFigures[measuredElement == "5071" & 
                          (Protected == F | flagdeltaStocksBasedonOpening_5113 == 0), 
                        flagObservationStatus := "I"]
  
onlyUnofficialFigures[measuredElement == "5071" & 
                          (Protected == F | flagdeltaStocksBasedonOpening_5113 == 0), 
                        flagMethod := "i"]
  
# opening flags
  
onlyUnofficialFigures[measuredElement == "5113" & Protected_5113 == F, 
                        flagObservationStatus := "I"]
  
onlyUnofficialFigures[measuredElement == "5113" & Protected_5113 == T, 
                        flagObservationStatus := ""]
  
onlyUnofficialFigures[measuredElement == "5113" & Protected_5113 == F, 
                        flagMethod := "e"]
  
onlyUnofficialFigures[measuredElement == "5113" & Protected_5113 == T, 
                        flagMethod := flagMethod_5113]

onlyUnofficialFigures <- onlyUnofficialFigures[, c("timePointYears", "geographicAreaM49", 
                                                   "measuredItemCPC", "measuredElement", 
                                                   "Value", "flagObservationStatus", 
                                                   "flagMethod", "flagMix"), with = F]

setcolorder(onlyUnofficialFigures, c("timePointYears", "geographicAreaM49", 
                                     "measuredItemCPC", "measuredElement", 
                                     "Value", "flagObservationStatus", 
                                     "flagMethod", "flagMix"))
} else{
  onlyUnofficialFigures[, `:=` (flagObservationStatus = NA, 
                                measuredElement = NA,
                                flagMethod = NA)]
  
  onlyUnofficialFigures <- onlyUnofficialFigures[, c("timePointYears", "geographicAreaM49", 
                                                                   "measuredItemCPC", "measuredElement", 
                                                                   "Value", "flagObservationStatus", 
                                                                   "flagMethod", "flagMix"), with = F]
  
  setcolorder(onlyUnofficialFigures, c("timePointYears", "geographicAreaM49", 
                                              "measuredItemCPC", "measuredElement", 
                                              "Value", "flagObservationStatus", 
                                              "flagMethod", "flagMix"))
  
}

## Only official figures
onlyOfficialFigures <- data[flagMix == "official"]

onlyOfficialFigures[, c("deltaStocks", "production", "imports", "incomeGroup",
                          "itemGroup", "totalSupply", "region", "closingStocksEstimated",
                          "openingStocksEstimated", "deltaStocksEstimated", 
                          "deltaStocksBasedonOpening_5113", "openingStocks_5113",
                          "minFlag", "maxFlag") := NULL]
  
setnames(onlyOfficialFigures, "deltaStocksUpdated", "deltaStocks")
setnames(onlyOfficialFigures, "openingStocksEstimatedUpdated", "openingStocks")
  
onlyOfficialFigures[Protected == TRUE & Protected_5113 == FALSE, 
                      closingStocksUpdated := openingStocks[1] - cumsum(deltaStocks),
                      by = list(geographicAreaM49, measuredItemCPC)]

if(nrow(onlyOfficialFigures) > 1) {  
onlyOfficialFigures[, openingStocksUpdated := shift(closingStocksUpdated)]
onlyOfficialFigures[is.na(openingStocksUpdated), openingStocksUpdated := openingStocks]
onlyOfficialFigures[, deltaStocksUpdated := shift(shift(openingStocksUpdated) - openingStocksUpdated, type="lead"),
                      by = list(geographicAreaM49, measuredItemCPC)]
  
onlyOfficialFigures[is.na(deltaStocksUpdated), deltaStocksUpdated := deltaStocks]
  
onlyOfficialFigures[, c("deltaStocks", "openingStocks", "closingStocksUpdated") := NULL]
  
onlyOfficialFigures <- melt.data.table(
    onlyOfficialFigures, 
    id.vars = c("geographicAreaM49", "measuredItemCPC", "timePointYears", 
                "flagObservationStatus_5071", "flagMethod_5071", "Protected", 
                "flagObservationStatus_5113", "flagMethod_5113", "Protected_5113",
                "flagdeltaStocksBasedonOpening_5113", "flagMix"), 
    measure.vars = c("deltaStocksUpdated", "openingStocksUpdated"))
  
  
onlyOfficialFigures[variable == "deltaStocksUpdated", measuredElement := "5071"]
onlyOfficialFigures[variable == "openingStocksUpdated", measuredElement := "5113"]
  
# flags for delta stocks
onlyOfficialFigures[measuredElement == "5071" & Protected == F, 
                      flagObservationStatus := "I"]
  
onlyOfficialFigures[measuredElement == "5071" & Protected == T, 
                      flagObservationStatus := flagObservationStatus_5071]
  
onlyOfficialFigures[measuredElement == "5071" & Protected == F, 
                      flagMethod := "i"]
  
onlyOfficialFigures[measuredElement == "5071" & Protected == T, 
                      flagMethod := flagMethod_5071]
  
  
# flags for opening stocks
  
onlyOfficialFigures[measuredElement == "5113" & Protected_5113 == T, 
                      flagObservationStatus := flagObservationStatus_5113]
  
  
onlyOfficialFigures[measuredElement == "5113" & Protected_5113 == T, 
                      flagMethod := flagMethod_5113]
  
onlyOfficialFigures[, c("flagObservationStatus_5071", "flagMethod_5071", "Protected",
                          "flagObservationStatus_5113", "flagMethod_5113", 
                          "Protected_5113", "flagdeltaStocksBasedonOpening_5113",
                          "variable") := NULL]
  
setnames(onlyOfficialFigures, "value", "Value")
  
setcolorder(onlyOfficialFigures, c("timePointYears", "geographicAreaM49", 
                                     "measuredItemCPC", "measuredElement", 
                                     "Value", "flagObservationStatus", 
                                     "flagMethod", "flagMix"))
} else {
  
  onlyOfficialFigures[, `:=` (Value = NA,
                              flagObservationStatus = NA,
                              flagMethod = NA)]
  
  onlyOfficialFigures <- onlyOfficialFigures[, c("timePointYears", "geographicAreaM49", 
                                                 "measuredItemCPC", "measuredElement", 
                                                 "Value", "flagObservationStatus", 
                                                 "flagMethod", "flagMix"), with = F]
  
  setcolorder(onlyOfficialFigures, c("timePointYears", "geographicAreaM49", 
                                     "measuredItemCPC", "measuredElement", 
                                     "Value", "flagObservationStatus", 
                                     "flagMethod", "flagMix"))
} 

## For the data that has at least one official row we have to compute the opening 
## stocks based on the deltaStocksUpdatedCummulated.
## We need to find the minimun "minors" deltaStocksUpdatedCummulated and then multiply
## the value by -1. If the result is more than zero this is the 
## minium stocks we have to have for having at least stocks equals to zero. 
## If the result is negative we assume the initial stocks equals to zero. 
## If there is no official data in the combination geographicAreaM49/commodity
## we already have the closing, opening and delta stocks estimated.

## When there are official and unofficial together

mixOfficialUnofficialFigures <- data[flagMix == "mix"]

if(nrow(mixOfficialUnofficialFigures) > 1) {
  
  setkey(mixOfficialUnofficialFigures, geographicAreaM49, measuredItemCPC, timePointYears)
  
  ## Get rid of some of the columns that we don't need anymore:
  mixOfficialUnofficialFigures[, c("deltaStocks", "Valid", "production", "imports", "incomeGroup",
                                   "itemGroup", "totalSupply", "region", 
                                   "closingStocksEstimated","openingStocksEstimated",
                                   "deltaStocksEstimated", "deltaStocksBasedonOpening_5113",
                                   "openingStocks_5113") := NULL]
  
  ## Let`s compute the cumsum for the deltaStocksUpdated by country and commodity
  mixOfficialUnofficialFigures[, minorsDeltaStocksUpdatedCummulated := 
                                 cumsum(ifelse(is.na(deltaStocksUpdated), 0, deltaStocksUpdated * (1))),
                               by = list(geographicAreaM49, measuredItemCPC)]
  
  initialStocks <- mixOfficialUnofficialFigures[, list(
    initialStocks = min(minorsDeltaStocksUpdatedCummulated, na.rm=T) * 1,
    # valueAdd = mean(openingStocksEstimated, na.rm = T),
    timePointYears = min(as.numeric(timePointYears))),
    by = list(geographicAreaM49, measuredItemCPC)]
  
  initialStocks[initialStocks < 0, initialStocks := 0]
  
  ## Now, we need to merge mixOfficialUnofficialFigures with initialStocks
  setkey(mixOfficialUnofficialFigures, geographicAreaM49, measuredItemCPC, timePointYears)
  initialStocks[, timePointYears := as.character(timePointYears)]
  setkey(initialStocks, geographicAreaM49, measuredItemCPC, timePointYears)
  
  mixOfficialUnofficialFigures <- merge(
    mixOfficialUnofficialFigures, 
    initialStocks[, c("geographicAreaM49", "measuredItemCPC", "timePointYears", "initialStocks"),
                  with = F], all.x = T)
  
  ## If there is official figure to the initial stocks so we have to use it. Otherwise not.
  mixOfficialUnofficialFigures[, initialStocksUpdated := NA_real_]
  
  mixOfficialUnofficialFigures[Protected_5113 == T & timePointYears == min(timePointYears), 
                               initialStocksUpdated := openingStocksEstimatedUpdated]
  
  mixOfficialUnofficialFigures[is.na(initialStocksUpdated), initialStocksUpdated := initialStocks]
  mixOfficialUnofficialFigures[is.na(initialStocksUpdated), initialStocksUpdated := 0]
  
  auxTab <- mixOfficialUnofficialFigures[, list(flagStocks = max(Protected), 
                                                flagOpening = max(Protected_5113)),
                                         by = list(geographicAreaM49, measuredItemCPC)]
  
  auxTab[flagStocks == 1 & flagOpening == 0, flagProtectComb := "delta_stocks_protected"]
  auxTab[flagStocks == 0 & flagOpening == 1, flagProtectComb := "opening_stocks_protected"]
  auxTab[flagStocks == 1 & flagOpening == 1, flagProtectComb := "delta_stocks_opening_protected"]
  
  # Merge auxtab with mixOfficialUnofficialFigures
  setkey(mixOfficialUnofficialFigures, geographicAreaM49, measuredItemCPC)
  setkey(auxTab, geographicAreaM49, measuredItemCPC)
  
  mixOfficialUnofficialFigures <- merge(mixOfficialUnofficialFigures, 
                                        auxTab[, c("geographicAreaM49", "measuredItemCPC", "flagProtectComb"), with = F],
                                        all = T)
  
  mixOfficialUnofficialFigures[, closingStocksUpdated := cumsum(initialStocksUpdated) + cumsum(deltaStocksUpdated),
                               by=list(geographicAreaM49, measuredItemCPC)]
  

  mixOfficialUnofficialFigures[, openingFinal := shift(closingStocksUpdated),
                               by=list(geographicAreaM49, measuredItemCPC)]
  
  mixOfficialUnofficialFigures[is.na(openingFinal), openingFinal := initialStocksUpdated,
                               by=list(geographicAreaM49, measuredItemCPC)]
  
  mixOfficialUnofficialFigures[Protected_5113 == TRUE, openingFinal := openingStocksEstimatedUpdated]
  mixOfficialUnofficialFigures[openingFinal < 0, openingFinal := 0]
  
  mixOfficialUnofficialFigures[, deltaStocksUpdatedFinal := NA_real_]
  
  mixOfficialUnofficialFigures[flagProtectComb == "opening_stocks_protected",
                               deltaStocksUpdatedFinal := shift(openingFinal, type = "lead") - openingFinal, 
                               by = list(geographicAreaM49, measuredItemCPC)]
  
  mixOfficialUnofficialFigures[is.na(deltaStocksUpdatedFinal) & timePointYears != (maxYearToProcess + 1), deltaStocksUpdatedFinal := deltaStocksUpdated]
  
  mixOfficialUnofficialFigures <- 
    mixOfficialUnofficialFigures[, c("geographicAreaM49", "measuredItemCPC", "timePointYears", 
                                     "flagObservationStatus_5071", "flagMethod_5071", "Protected", 
                                     "Protected_5113", "flagdeltaStocksBasedonOpening_5113", 
                                     "flagObservationStatus_5113", "flagMethod_5113", "openingFinal", 
                                     "deltaStocksUpdatedFinal", "protectedFlag", "minFlag", 
                                     "maxFlag", "flagMix", "flagProtectComb"), with = F]
  
  mixOfficialUnofficialFigures <- melt.data.table(mixOfficialUnofficialFigures, 
                                                  id.vars = c("geographicAreaM49", "measuredItemCPC", 
                                                              "timePointYears", 
                                                              "flagObservationStatus_5071",
                                                              "flagMethod_5071",
                                                              "Protected", "Protected_5113",
                                                              "flagdeltaStocksBasedonOpening_5113",
                                                              "flagObservationStatus_5113", 
                                                              "flagMethod_5113",
                                                              "protectedFlag", "minFlag", 
                                                              "maxFlag", "flagMix", "flagProtectComb"), 
                                                  measure.vars = c("deltaStocksUpdatedFinal", "openingFinal"))
  
  # Flags for delta stocks
  mixOfficialUnofficialFigures[Protected == T & variable == "deltaStocksUpdatedFinal", 
                               flagObservationStatus := flagObservationStatus_5071]
  
  mixOfficialUnofficialFigures[Protected == F & variable == "deltaStocksUpdatedFinal", 
                               flagObservationStatus := "I"]
  
  mixOfficialUnofficialFigures[Protected == T & variable == "deltaStocksUpdatedFinal",
                               flagMethod := flagMethod_5071]
  
  mixOfficialUnofficialFigures[Protected == F & variable == "deltaStocksUpdatedFinal", 
                               flagMethod := "i"]
  
  # Flags for opening stocks
  
  mixOfficialUnofficialFigures[Protected_5113 == T & variable == "openingFinal",
                               flagObservationStatus := flagObservationStatus_5113]
  
  mixOfficialUnofficialFigures[Protected_5113 == F & variable == "openingFinal",
                               flagObservationStatus := "I"]
  
  mixOfficialUnofficialFigures[Protected_5113 == T & variable == "openingFinal",
                               flagMethod := flagMethod_5113]
  
  mixOfficialUnofficialFigures[Protected_5113 == F & variable == "openingFinal",
                               flagMethod := "e"]
  
  
  mixOfficialUnofficialFigures[variable == "deltaStocksUpdatedFinal", measuredElement := "5071"]
  mixOfficialUnofficialFigures[variable == "openingFinal", measuredElement := "5113"]
  
  mixOfficialUnofficialFigures[, c("flagObservationStatus_5071", "flagMethod_5071",
                                   "Protected", "Protected_5113", 
                                   "flagdeltaStocksBasedonOpening_5113",
                                   "flagObservationStatus_5113", "flagMethod_5113",
                                   "protectedFlag", "minFlag", "maxFlag", 
                                   "variable", "flagProtectComb") := NULL]
  
  setnames(mixOfficialUnofficialFigures, "value", "Value")
  
  setcolorder(mixOfficialUnofficialFigures, c("timePointYears", "geographicAreaM49", 
                                              "measuredItemCPC", "measuredElement", 
                                              "Value", "flagObservationStatus", 
                                              "flagMethod", "flagMix"))
} else {
    
  mixOfficialUnofficialFigures[, `:=` (timePointYears = NA,
                                        Value = NA,
                                        flagObservationStatus = NA,
                                        flagMethod = NA)]
  
  mixOfficialUnofficialFigures <- mixOfficialUnofficialFigures[, c("timePointYears", "geographicAreaM49", 
                                                 "measuredItemCPC", "measuredElement", 
                                                 "Value", "flagObservationStatus", 
                                                 "flagMethod", "flagMix"), with = F]
  
  setcolorder(mixOfficialUnofficialFigures, c("timePointYears", "geographicAreaM49", 
                                     "measuredItemCPC", "measuredElement", 
                                     "Value", "flagObservationStatus", 
                                     "flagMethod", "flagMix"))
}

## Now we can combine the official and unofficial data sets again in order to
## save the data set to SWS.


dataToSave <- rbindlist(list(onlyUnofficialFigures, mixOfficialUnofficialFigures, onlyOfficialFigures), 
                        fill = T)
dataToSave <- nameData("Stock", "stocksdata", dataToSave)

## Delta Stocks

dataToSaveDeltaStocks <- dataToSave[measuredElement == "5071"]

setkey(dataToSaveDeltaStocks, geographicAreaM49, measuredItemCPC, timePointYears)
setkey(stockData, geographicAreaM49, measuredItemCPC, timePointYears)
dataToSaveDeltaStocks <- merge(dataToSaveDeltaStocks, stockData[, c("geographicAreaM49", "measuredItemCPC", 
                                                              "timePointYears", "deltaStocks", 
                                                              "Protected"), with = F], all.x = T)

dataToSaveDeltaStocks[, diff := Value - deltaStocks]
# dataToSaveDeltaStocks[Protected == T & diff != 0]  # 0 cases 

## Excluding Protected != T
dataToSaveDeltaStocks[is.na(Protected), Protected := FALSE]

dataToSaveDeltaStocks[, c("timePointYears_description", "geographicAreaM49_description", 
                      "measuredItemCPC_description", "measuredElement_description",
                      "flagMix", "deltaStocks", "Protected", "diff") := NULL]


## Opening
dataToSaveOpening <- dataToSave[measuredElement == "5113"]
# Merge with openingStocksData
setkey(dataToSaveOpening, geographicAreaM49, measuredItemCPC, timePointYears)
setkey(openingStockData, geographicAreaM49, measuredItemCPC, timePointYears)
dataToSaveOpening <- merge(dataToSaveOpening, openingStockData[, c("geographicAreaM49", "measuredItemCPC", 
                           "timePointYears", "openingStocks_5113", 
                           "Protected_5113"), with = F], all.x = T)

dataToSaveOpening[, diff := Value - openingStocks_5113]
dataToSaveOpening[Protected_5113 == T & diff != 0] # 1 case for usa and rice
dataToSaveOpening[Protected_5113 == T & diff != 0, flagObservationStatus := "I"]
dataToSaveOpening[Protected_5113 == T & diff != 0, flagMethod := "e"]

## Excluding Protected_5113 != T
dataToSaveOpening[is.na(Protected_5113), Protected_5113 := FALSE]
dataToSaveOpening[, c("timePointYears_description", "geographicAreaM49_description", 
                      "measuredItemCPC_description", "measuredElement_description",
                      "flagMix", "openingStocks_5113", "Protected_5113", "diff") := NULL]

# Let's put together these data sets

dataFinalToSave <- rbindlist(list(dataToSaveDeltaStocks, dataToSaveOpening))

setcolorder(dataFinalToSave,
            c("timePointYears", "geographicAreaM49", "measuredItemCPC",
              "measuredElement", "Value", "flagObservationStatus", "flagMethod"))

dataFinalToSave <- dataFinalToSave[!is.na(Value)]
dataFinalToSave <- dataFinalToSave[timePointYears <= maxYearToProcess]

stats = SaveData(domain = "Stock", dataset = "stocksdata", data = dataFinalToSave)

paste0("Module completed successfully!!! ",
       stats$inserted, " observations written, ",
       stats$ignored, " weren't updated, ",
       stats$discarded, " had problems.")
