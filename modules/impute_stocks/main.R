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
stockData <- getStockData(as.character(1999:2014))
setnames(stockData, "Value", "deltaStocks")

## Merge stockData with the flagValidTable
keys = c("flagObservationStatus", "flagMethod")
stockData <- merge(
  stockData, flagValidTable, by = keys, all.x = T
)

# stockData[is.na(flagMethod)]
# stockData[is.na(flagObservationStatus)]
# stockData[is.na(Valid)]

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

# coefficients for USA and cereals
# amis coefficients
################################################################################
# str(fit[["coefficients"]])
# coef <- fit[["coefficients"]]
# out.df <- data.frame(names(coef), coef)
# rownames(out.df) <- NULL
# #dir.create("data-raw")
# write.csv(out.df, file = "data-raw/coefficients_cereals_pulses.csv", row.names = FALSE)
# devtools::document()
# devtools::install()
coefficients_cereals_pulses <- fread(system.file("extdata/coefficients_cereals_pulses.csv", package = "faoswsStock"))

# fo_licht coefficients
# str(sugarModel[["coefficients"]])
# coefSugar <- sugarModel[["coefficients"]]
# outSugar <- data.frame(names(coefSugar), coefSugar)
# rownames(outSugar) <- NULL
# #dir.create("data-raw")
# write.csv(outSugar, file = "data-raw/coefficients_sugar.csv", row.names = FALSE)
coefficients_sugar <- fread(system.file("extdata/coefficients_sugar.csv", package = "faoswsStock"))

# coefficients for USA and CEREALS
data[itemGroup == "cereals" & geographicAreaM49 == 840, 
     deltaStocksEstimated :=  coefficients_cereals_pulses[names.coef. == "(Intercept)", coef] +
       coefficients_cereals_pulses[names.coef. == "deltaTotalSupply", coef] * deltaTotalSupply + 
       coefficients_cereals_pulses[names.coef. == "regionUnited States of America", coef]]

# coefficients for USA and PULSES
data[itemGroup == "pulses" & geographicAreaM49 == 840, 
     deltaStocksEstimated :=  coefficients_cereals_pulses[names.coef. == "(Intercept)", coef] +
       coefficients_cereals_pulses[names.coef. == "deltaTotalSupply", coef] * deltaTotalSupply + 
       coefficients_cereals_pulses[names.coef. == "regionUnited States of America", coef] +
       coefficients_cereals_pulses[names.coef. == "typepulses", coef]]

# coefficients for USA and Refined_sugar
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
     deltaStocksEstimated := -1.32490 + 0.28250 * deltaTotalSupply + 1.00563]

data[itemGroup == "pulses" & !(geographicAreaM49 %in% c(840, 40, 56, 100, 196, 203,
                                                        208, 233, 246, 250, 276, 300,
                                                        348, 372, 380, 428, 440, 442,
                                                        470, 528, 616, 620, 642, 703,
                                                        705, 724, 752, 826)), 
     deltaStocksEstimated := -1.32490 + 0.28250 * deltaTotalSupply + 1.00563 + 0.32012]

data[itemGroup == "refined_sugar" & geographicAreaM49 %in% !(geographicAreaM49 %in% 
                                                               c(840, 40, 56, 100,
                                                                 196, 203, 208, 233,
                                                                 246, 250, 276, 300,
                                                                 348, 372, 380, 428,
                                                                 440, 442, 470, 528,
                                                                 616, 620, 642, 703,
                                                                 705, 724, 752, 826)) &
       incomeGroup %in% c("Lower middle income", "Low income"), 
     deltaStocksEstimated := -2321.6041785 + 0.8164414 * deltaTotalSupply + 2167.8907130]


data[itemGroup == "refined_sugar" & geographicAreaM49 %in% !(geographicAreaM49 %in% 
                                                               c(840, 40, 56, 100,
                                                                 196, 203, 208, 233,
                                                                 246, 250, 276, 300,
                                                                 348, 372, 380, 428,
                                                                 440, 442, 470, 528,
                                                                 616, 620, 642, 703,
                                                                 705, 724, 752, 826)) &
       incomeGroup %in% c("Upper middle income", "High income"), 
     deltaStocksEstimated := -2321.6041785 + 0.8164414 * deltaTotalSupply]


## Save data

## We have to make a filter using just Protected != TRUE

dataToSave <- data[Protected == FALSE | is.na(Protected) & timePointYears >= 1999]

## Get rid of some of the columns that we don't need anymore:
dataToSave[, c("deltaStocks", "Valid", "Protected", "production", "netTrade",
               "incomeGroup", "itemGroup", "totalSupply", "deltaTotalSupply") := NULL]

setnames(dataToSave, "deltaStocksEstimated", "Value")
dataToSave[, measuredElement := "5071"]
dataToSave[, flagObservationStatus := "I"]
dataToSave[, flagMethod := "e"]

setcolorder(dataToSave,
            c("timePointYears", "geographicAreaM49", "measuredItemCPC",
              "measuredElement", "Value", "flagObservationStatus", "flagMethod"))


###
stats = SaveData(domain = "agriculture", dataset = "aproduction", data = dataToSave)

paste0(stats$inserted, " observations written, ",
       stats$ignored, " weren't updated, ",
       stats$discarded, " had problems.")

