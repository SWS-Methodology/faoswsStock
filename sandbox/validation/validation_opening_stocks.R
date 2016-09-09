
## This code is estimating opening stocks based on production and then computes the
## delta stocks.

## load the library
suppressMessages({
  library(faosws)
  library(data.table)
  library(ggplot2)
  library(faoswsUtil)
  library(faoswsFlag)
  library(countrycode)
  library(faoswsStock)
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
# 
# ## Total Trade
# totalTradeData <- getTotalTradeData(as.character(1998:2014))
# totalTradeData <- dcast.data.table(totalTradeData, geographicAreaM49 + measuredItemCPC + 
#                                      timePointYears ~ measuredElementTrade, value.var = "Value")
# 
# setnames(totalTradeData, "5610", "imports")
# setnames(totalTradeData, "5910", "exports")
# 
# totalTradeData[is.na(imports), imports := 0]
# totalTradeData[is.na(exports), exports := 0]
# totalTradeData[, netTrade := (imports - exports)]

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
# data <- merge(data, totalTradeData[, c(keys, "netTrade"), with = F], 
              #by = keys, all = T)
data <- merge(data, countryIncomeGroup[, c("geographicAreaM49", "incomeGroup"), 
                                       with = F], by = "geographicAreaM49", all.x=T)

## Groups
data[substr(measuredItemCPC, 1, 3) == "011", type := "cereals"]
data[substr(measuredItemCPC, 1, 3) == "017", type := "pulses"]
data[substr(measuredItemCPC, 1, 3) == "235", type := "refined_sugar"]

data[is.na(production), production := 0]
# data[is.na(netTrade), netTrade := 0]
# data[, totalSupply := production + netTrade]
data[, totalSupply := production]

setkey(data, geographicAreaM49, measuredItemCPC, timePointYears)

# ## Compute delta production
# data[, deltaTotalSupply := c(0, diff(totalSupply)), 
#      by = list(geographicAreaM49, measuredItemCPC)]


# Here we will apply the coefficients based on AMIS data where 
# the dependent variable is closing stocks and the independent variables
# are total supply, region (US, EU, Others countries) and type of commodity (cereals or pulses).
# For refined_sugar we will use coefficients fitted from the F.O Lichts data.
# We excluded the regions World, China and China Mainland.


# # Amis coefficients
# coefficients_cereals_pulses <- fread(system.file("extdata/coefficients_cereals_pulses.csv", package = "faoswsStock"))
# 
# # Fo_Licht coefficients
# coefficients_sugar <- fread(system.file("extdata/coefficients_sugar.csv", package = "faoswsStock"))

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
# data[type == "cereals" & region == "United States of America", 
#      closingStocksEstimated :=  -0.423402 + 0.109562 * totalSupply + 0.030361 * totalSupply]

data[type == "cereals" & region == "United States of America", 
     closingStocksEstimated :=  coefficients_cereals_pulses[names.coef. == "(Intercept)", coef] + 
       coefficients_cereals_pulses[names.coef. == "totalSupply", coef] * totalSupply + 
       coefficients_cereals_pulses[names.coef. == "totalSupply:regionUnited States of America", coef] * totalSupply]


# coefficients for USA and pulses
data[type == "pulses" & region == "United States of America", 
     closingStocksEstimated :=  -0.423402 + 0.109562 * totalSupply + 0.030361 * totalSupply - 0.069975 * totalSupply]

# coefficients for USA and refined_sugar
data[type == "refined_sugar" & region == "United States of America",
     closingStocksEstimated :=  0.27024 * totalSupply + 0.02658 * totalSupply]

# coefficients for EU and cereals

data[type == "cereals" & region == "European Union", 
     closingStocksEstimated := -0.423402 + 0.109562 * totalSupply]

# coefficients for EU and pulses

data[type == "pulses" & region == "European Union", 
     closingStocksEstimated := -0.423402 + 0.109562 * totalSupply - 0.069975 * totalSupply]

# coefficients for EU and refined_sugar
data[type == "refined_sugar" & region == "European Union",
     closingStocksEstimated := 0.27024 * totalSupply + 0.02658 * totalSupply]

# coefficients for other countries

data[type == "cereals" & region == "Others countries", 
     closingStocksEstimated := -0.423402 + 0.109562 * totalSupply + 0.044929 * totalSupply]

data[type == "pulses" & region == "Others countries", 
     closingStocksEstimated := -0.423402 + 0.109562 * totalSupply + 0.044929 * totalSupply - 0.069975 * totalSupply]

data[type == "refined_sugar" & region == "Others countries" &
       incomeGroup %in% c("Lower middle income", "Low income"),
     closingStocksEstimated := 0.27024 * totalSupply]


data[type == "refined_sugar" & region == "Others countries" &
        incomeGroup %in% c("Upper middle income", "High income"), 
     closingStocksEstimated := 0.27024 * totalSupply + 0.02658 * totalSupply]


setkey(data, geographicAreaM49, measuredItemCPC, timePointYears)

# data[, c("closingStocksEstimated", "deltaStocksEstimated",
#          "openingStocksEstimated", "percent", "percentOpen") := NULL]

# data[openingStocksEstimated < 0, openingStocksEstimated := 0]
# 
# data[, deltaStocksEstimated := c(diff(openingStocksEstimated)), 
#             by = list(geographicAreaM49, measuredItemCPC)]
# 
# data[, closingStocksEstimated := openingStocksEstimated + deltaStocksEstimated]
# 
# data[, percent := closingStocksEstimated/totalSupply]
# data[, percentOpen := openingStocksEstimated/totalSupply]


data[closingStocksEstimated < 0, closingStocksEstimated := 0]
data[, deltaStocksEstimated := c(0, diff(closingStocksEstimated)),
           by = list(geographicAreaM49, measuredItemCPC)]
data[, openingStocksEstimated := closingStocksEstimated - deltaStocksEstimated]
data[, percentOpen := openingStocksEstimated/totalSupply]

setkey(data, geographicAreaM49, measuredItemCPC, timePointYears)

## Make some plots in order to compare the results

data <- data[timePointYears != 1998]
getwd()

data[, timePointYears := as.character(timePointYears)]
data <- nameData("agriculture", "aproduction", data)

write.csv(data, file = "sandbox/validation/2016-08-27-stocks_data.csv", row.names = F)



# data[, timePointYears := as.numeric(timePointYears)]
# 
# plotcolors <- colorRampPalette(c("gray70", "blue", "green", "black"))
# 
# ggplot(data=data[geographicAreaM49 %in% c(840, 76, 276) & measuredItemCPC %in% c("0111", "0113", "01701", "01704", "23520")],
#        aes(x=timePointYears, y=percentOpen, group=measuredItemCPC, col = measuredItemCPC)) +
#   geom_line(aes(), stat = "identity", position=position_dodge(), size=1) +
#   # facet_wrap(~ countryRegionName, scales = "free") +
#   # facet_grid( . ~ countryRegionName, scales = "free") +
#   facet_grid(
#     geographicAreaM49 ~ .
#     # , scales = "free"
#   ) +
#   scale_y_continuous(labels = scales::percent) +
#   # scale_colour_brewer(palette="Blues") +
#   #scale_color_manual(values = plotcolors(length(unique(data$measuredItemCPC))) ) +
#   theme(legend.title=element_blank(), strip.text = element_text(size=7)) +
#   ylab('% stocks') + xlab('') +
#   ggtitle("% Stocks by Year") +
#   scale_x_continuous(lim=c(1999, 2014), breaks=seq(1999, 2014, 4)) +
#   theme_bw() +
#   theme(
#     axis.text.x = element_text(size = 8, face = "bold", angle = 45, vjust = .5),
#     legend.position = "top"
#   )
# 

################################################################################

## Read data to check opening stocks with production and other simple analysis

dlpath <- file.path("C:", "Users", "caetano", "Documents", "Github", 
                    "faoswsStock", "sandbox", "validation", "2016-08-27-stocks_data.csv")

dataValidation <- fread(dlpath)

dataValidation[, percent := openingStocksCalculated/production]

dataValidation[production == 0 | openingStocksCalculated == 0, 
               percent := 0]


dataValidation[geographicAreaM49 == 840]
dataValidation[percent > 1, .N, geographicAreaM49]
dataValidation[percent < 1 & percent > 0]
