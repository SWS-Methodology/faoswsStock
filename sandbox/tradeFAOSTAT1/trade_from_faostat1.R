## Total Trade

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


## Trade from FAOSTAT 1
getTotalTradeDataFAOSTAT <- function(yearRange) {
  tradeCode <- c("61", "91")
  fcl =  suppressWarnings(as.character(as.numeric(cpc2fcl(getStocksCPC(), returnFirst = T))))
  fcl <- fcl[!is.na(fcl)]
  totalTradeKey = DatasetKey(
    domain = "faostat_one",
    dataset = "FS1_SUA_UPD",
    dimensions = list(
      Dimension(name = "geographicAreaFS",
                keys = GetCodeList(domain = "faostat_one",
                                   dataset = "FS1_SUA_UPD",
                                   dimension = "geographicAreaFS")[type == "country", code]),
      Dimension(name = "measuredElementFS", keys = tradeCode),
      Dimension(name = "timePointYears", keys = yearRange),
      Dimension(name = "measuredItemFS",
                keys = fcl)
    )
  )
  
  totalTradeData = GetData(
    totalTradeKey,
    flags = FALSE)
  
  totalTradeData[, geographicAreaM49 := fs2m49(geographicAreaFS)]
  totalTradeData[, measuredItemCPC := fcl2cpc(formatC(as.numeric(measuredItemFS), width = 4,
                                                                   flag = "0"))]
 
  totalTradeData[, c("measuredItemFS") := NULL]
  setcolorder(totalTradeData, c("geographicAreaM49", "geographicAreaFS", "measuredElementFS", 
                                "measuredItemCPC", "Value", "timePointYears"))
  
  totalTradeData <- totalTradeData[!is.na(geographicAreaM49)]
  
  return(totalTradeData[])

}

# 61 = inflow
# 91 = outflow


tradeFAOSTAT <- getTotalTradeDataFAOSTAT(as.character(1990:2014))
yearTab <- tradeFAOSTAT[, .N, timePointYears]
yearTab <- yearTab[order(-yearTab$timePointYears)]
yearTab

# library(xlsx)
# write.xlsx(yearTab, "sandbox/tradeFAOSTAT1/records_per_year.xlsx", row.names = F)




tradeFAOSTAT <- dcast.data.table(tradeFAOSTAT, geographicAreaM49 + measuredItemCPC +
                                     timePointYears ~ measuredElementFS, value.var = "Value")

setnames(tradeFAOSTAT, "61", "imports")
setnames(tradeFAOSTAT, "91", "exports")

tradeFAOSTAT[is.na(imports), imports := 0]
tradeFAOSTAT[is.na(exports), exports := 0]
tradeFAOSTAT[, netTrade := (imports - exports)]

setkey(tradeFAOSTAT, "geographicAreaM49", "measuredItemCPC", "timePointYears")
tradeFAOSTAT[, .N, timePointYears]

