
# rewriting

## load the library

suppressMessages({
library(faosws)
library(data.table)
library(ggplot2)
library(faoswsUtil)
library(faoswsFlag)
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
  
}

## Using the new data on SWS instead of oldSuaData
stockCode <- "5071"
prodCode <- "5510"
codes <- c(stockCode, prodCode) 

item <- ReadDatatable("fbs_stocks_comm_codes")[fbs_key == "measuredItemCPC", 
                                               fbs_code]
stockProdkey = DatasetKey(
  domain = "agriculture",
  dataset = "aproduction",
  dimensions = list(
    Dimension(
      name = "geographicAreaM49",
      keys = GetCodeList("agriculture", "aproduction", "geographicAreaM49")[, code]
    ),
    Dimension(name = "measuredElement", keys = codes),
    Dimension(name = "timePointYears", keys = as.character(2009:2013)),
    Dimension(
      name = "measuredItemCPC",
      keys = GetCodeList("agriculture", "aproduction", "measuredItemCPC")[code %in% item, code]
    )
  )
)

stockProdData <- GetData(stockProdkey, flags = T)

stockData <- stockProdData[measuredElement == 5071]
# stockData[, c("measuredElement", "flagObservationStatus", "flagMethod") := NULL]
setnames(stockData, "Value", "deltaStocks")

# production data
prodData <- stockProdData[measuredElement == 5510]
prodData[, c("measuredElement", "flagObservationStatus", "flagMethod") := NULL]
setnames(prodData, "Value", "production")

# Trade
tradeCode <- c("5610", "5910")

totalTradeKey = DatasetKey(
  domain = "trade",
  dataset = "total_trade_cpc_m49",
  dimensions = list(
    Dimension(
      name = "geographicAreaM49",
      keys = GetCodeList("trade", "total_trade_cpc_m49", "geographicAreaM49")[, code]
    ),
    Dimension(name = "measuredElementTrade", keys = tradeCode),
    Dimension(name = "timePointYears", keys = as.character(2009:2013)),
    Dimension(
      name = "measuredItemCPC",
      keys = GetCodeList("trade", "total_trade_cpc_m49", "measuredItemCPC")[code %in% item, code]
    )
  )
)

totalTradeData <- GetData(totalTradeKey, flags = F)
totalTradeData <- dcast.data.table(totalTradeData, geographicAreaM49 + measuredItemCPC + 
                                     timePointYears ~ measuredElementTrade, value.var = "Value")

setnames(totalTradeData, "5610", "imports")
setnames(totalTradeData, "5910", "exports")

totalTradeData[is.na(imports), imports := 0]
totalTradeData[is.na(exports), exports := 0]
totalTradeData[, netTrade := (imports - exports)]

library(countrycode)

## Country groups
dlpath <- file.path("C:", "Users", "caetano", "Documents", "Github", "faoswsFeed",
                    "data-raw", "IR_factor", "class.csv")

countryGroup <- fread(dlpath)
countryGroup[, .N, GroupName]

countryGroup[grepl("income", GroupName), .N, GroupName]

countryIncomeGroup <- countryGroup[GroupCode %in% c("HIC", "LIC", "UMC", "LMC"), ]
# countryRegionGroup <- countryGroup[GroupCode %in% c("NAC", "LCN", "AFR", "EAS", "EUU"),]

# library(countrycode)

countryIncomeGroup[, geographicAreaM49 := as.character(countrycode(CountryCode, "wb", "iso3n"))]

# Sudan has the wrong name (it should be former Sudan)
countryIncomeGroup[geographicAreaM49 == "736", CountryName := "Sudan (former)"]
# China should be 1248
countryIncomeGroup[geographicAreaM49 == "156", geographicAreaM49 := "1248"]
#Exclude Channel Islands and Kosovo (not separately recognised by the UN)
countryIncomeGroup <- countryIncomeGroup[!is.na(geographicAreaM49)]

setnames(countryIncomeGroup, "GroupName", "incomeGroup")

keys <- c("geographicAreaM49", "measuredItemCPC", "timePointYears")
data <- merge(stockData, prodData, by = keys, all = T)
data <- merge(data, totalTradeData, by = keys, all = T)
data <- merge(data, countryIncomeGroup[, c("geographicAreaM49", "incomeGroup"), with = F],
              by = "geographicAreaM49", all.x=T)

data[substr(measuredItemCPC, 1, 3) == "011", itemGroup := "cereals"]
data[substr(measuredItemCPC, 1, 3) == "017", itemGroup := "pulses"]
data[substr(measuredItemCPC, 1, 3) == "235", itemGroup := "refined_sugar"]

data <- data[timePointYears %in% c("2011", "2012")]


data[is.na(production), production := 0]
data[is.na(imports), imports := 0]
data[is.na(exports), exports := 0]
data[is.na(netTrade), netTrade := 0]
data[, totalSupply := production + netTrade]
data

setkey(data, geographicAreaM49, measuredItemCPC, timePointYears)

## Compute delta production
data[, deltaTotalSupply := c(0, diff(totalSupply)), 
           by = list(geographicAreaM49, measuredItemCPC)]

# Let's apply the coefficients fitted by AMIS data for cereals and pulses.
# For refined_sugar we will use coefficients fitted from the F.O Lichts data.

# coefficients for USA and cereals

data[itemGroup == "cereals" & geographicAreaM49 == 840, 
     deltaStocksEstimated := -1.32490 + 0.28250 * deltaTotalSupply + 0.41081]

# coefficients for USA amd PULSES
data[itemGroup == "pulses" & geographicAreaM49 == 840, 
     deltaStocksEstimated := -1.32490 + 0.28250 * deltaTotalSupply + 0.41081 * 1 + 0.32012]

data[itemGroup == "refined_sugar" & geographicAreaM49 == 840, 
     deltaStocksEstimated := -2321.6041785 + 0.8164414 * deltaTotalSupply]

# coefficients for EU and cereals

data[itemGroup == "cereals" & geographicAreaM49 %in% c(40, 56, 100, 196, 203, 208, 233, 246, 250, 276, 300, 348, 372, 380, 428,
                                                       440, 442, 470, 528, 616, 620, 642, 703, 705, 724, 752, 826), 
     deltaStocksEstimated := -1.32490 + 0.28250 * deltaTotalSupply]


data[itemGroup == "pulses" & geographicAreaM49 %in% c(40, 56, 100, 196, 203, 208, 233, 246, 250, 276, 300, 348, 372, 380, 428,
                                                       440, 442, 470, 528, 616, 620, 642, 703, 705, 724, 752, 826), 
     deltaStocksEstimated := -1.32490 + 0.28250 * deltaTotalSupply + 0.32012]


data[itemGroup == "refined_sugar" & geographicAreaM49 %in% c(40, 56, 100, 196, 203, 
                                                        208, 233, 246, 250, 276,
                                                        300, 348, 372, 380, 428,
                                                        440, 442, 470, 528, 616,
                                                        620, 642, 703, 705, 724,
                                                        752, 826), 
     deltaStocksEstimated := -2321.6041785 + 0.8164414 * deltaTotalSupply]

# other countries

data[itemGroup == "cereals" & !(geographicAreaM49 %in% c(840, 40, 56, 100, 196, 203, 208, 233, 246, 250, 276, 300, 348, 372, 380, 428,
                                                       440, 442, 470, 528, 616, 620, 642, 703, 705, 724, 752, 826)), 
     deltaStocksEstimated := -1.32490 + 0.28250 * deltaTotalSupply + 1.00563]

data[itemGroup == "pulses" & !(geographicAreaM49 %in% c(840, 40, 56, 100, 196, 203, 208, 233, 246, 250, 276, 300, 348, 372, 380, 428,
                                                         440, 442, 470, 528, 616, 620, 642, 703, 705, 724, 752, 826)), 
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





tab <- data[measuredItemCPC %in% c("0111", "0112", "0113", "0114", "0141", "23520"), 
     c("geographicAreaM49", "timePointYears", "measuredItemCPC", "deltaStocksEstimated"), with=F]

tab <- dcast.data.table(tab, geographicAreaM49 + timePointYears ~ measuredItemCPC, value.var = "deltaStocksEstimated")
setnames(tab, "0111", "wheat")
setnames(tab, "0112", "maize")
setnames(tab, "0113", "rice")
setnames(tab, "0114", "sorghum")
setnames(tab, "23520", "refined_sugar")

tab <- tab[timePointYears == 2012]
tab[geographicAreaM49 == 840]

data[itemGroup == "refined_sugar"]
tabItemIncomeGroup <- data[, list(production = sum(production, na.rm = T),
                                  netTrade = sum(netTrade, na.rm = T),
                                  deltaStocks = sum(deltaStocks, na.rm = T),
                                  nCountry = length(unique(geographicAreaM49))),
                           by = list(timePointYears, itemGroup, incomeGroup)]

tabItemIncomeGroup <- tabItemIncomeGroup[!is.na(incomeGroup)]
tabItemIncomeGroup[itemGroup == "cereals"]
tabItemIncomeGroup[itemGroup == "pulses"]
tabItemIncomeGroup[itemGroup == "refined_sugar"]

tabItemIncomeGroup$incomeGroup <- factor(tabItemIncomeGroup$incomeGroup, 
                                         levels = c("High income", 
                                                    "Upper middle income", 
                                                    "Lower middle income", 
                                                    "Low income"))

plotcolors <- colorRampPalette(c("gray70", "blue", "green", "black"))

ggplot(data=tabItemIncomeGroup,
       aes(x=as.factor(timePointYears), y=netTrade, group=incomeGroup, col = incomeGroup)) +
  geom_line(aes(), stat = "identity", position=position_dodge(), size=1) +
  facet_wrap(~ itemGroup
             , scales = "free"
  ) +
  scale_color_manual( values = plotcolors(length(unique(tabItemIncomeGroup$incomeGroup)))) +
  theme(strip.text.x = element_text(size = 10, colour = "black", angle = 0)) +
  theme(legend.title=element_blank(), strip.text = element_text(size=7)) +
  geom_hline(yintercept=0, col = "red") +
  theme(legend.title=element_blank(), strip.text = element_text(size=7)) +
  xlab('Year') + ylab('Imports - Exports') +
  #scale_x_continuous(lim=c(2005, 2012), breaks=seq(2005, 2012, 1)) +
  theme(
    axis.text.x = element_text(size = 8, face = "bold", angle = 45, vjust = .5),
    legend.position = "top"
  )

# F.0 Lichts

dlpath <- file.path("C:", "Users", "caetano", "Documents", "Github", "faoswsStock", "sandbox", "fo_licht", "sugar_prod_trade_stocks_fo_lichts2016_06_08.csv")
sugarData <- fread(dlpath)
sugarData[, supply := production + imports - exports + opening_stocks]
sugarData[, utilization := consumption + ending_stocks]
sugarData[, delta_stocks := ending_stocks - opening_stocks]
sugarData[, stocks_percent := ending_stocks/supply]
sugarData[, delta_total_supply := diff(c(NA, supply)),
          by=list(country_group)]
sugarData[, netTrade := imports - exports]

ggplot(data=sugarData,
       aes(x=as.factor(timePointYears), y=netTrade, group=country_group, col = country_group)) +
  geom_line(aes(), stat = "identity", position=position_dodge(), size=1) +
  # facet_wrap(~ itemGroup
  #            , scales = "free"
  # ) +
  #scale_color_manual( values = plotcolors(length(unique(sugarData$country_group)))) +
  # theme(strip.text.x = element_text(size = 10, colour = "black", angle = 0)) +
  theme(legend.title=element_blank(), strip.text = element_text(size=7)) +
  geom_hline(yintercept=0, col = "black") +
  theme(legend.title=element_blank(), strip.text = element_text(size=7)) +
  xlab('Year') + ylab('Imports - Exports') +
  #scale_x_continuous(lim=c(2005, 2012), breaks=seq(2005, 2012, 1)) +
  theme(
    axis.text.x = element_text(size = 8, face = "bold", angle = 45, vjust = .5),
    legend.position = "top"
  )



