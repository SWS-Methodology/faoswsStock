##' Compute Opening Stocks From Stock Variation
##'
##' This function pulls the old total trade data from FAOSTAT1
##'
##' @param year Numeric.
##'
##' @return The dataset with the opening stocks computed.
##'
##' @export
##'

computeOpeningStocksfromDelta <- function(year) {
  
  yearRange = as.character(1961:(year - 1))
  # stockCode <- "5071"
  keyStock = copy(completeImputationKey)
  # keyStock@dimensions$measuredElement@keys = "5071"
  # keyStock@dimensions$timePointYears@keys = as.character(startYear:(endYear))
  # stockData <- GetData(keyStock, flags = TRUE)
  m49 <- keyStock@dimensions$geographicAreaM49@keys
  m49 <- m49[!(m49 %in% c("831", "832"))]
  
  # deltaStockData <- getStockData(measuredElement = c(stockCode), 
  #                                yearRange)
  
  deltaStockData <- getFAOSTAT1Data(m49,
                                    completeImputationKey@dimensions$measuredItemCPC@keys,
                                    "71",
                                    yearRange = yearRange,
                                    "updated_sua_2013_data")
  
  
  setnames(deltaStockData, "Value", "deltaStocks")
  deltaStockData[, deltaStocks := deltaStocks * (-1)]
  
  setkey(deltaStockData, geographicAreaM49, measuredItemCPC, timePointYears)
  
  deltaStockData[is.na(deltaStocks), deltaStocks := 0]
  deltaStockData[, openingStockFromDelta := cumsum(deltaStocks),
                  by=list(geographicAreaM49, measuredItemCPC)]
  
  openingStockComputed <- deltaStockData[timePointYears == year-1]
  # openingStockComputed[, openingStocks := openingStockFromDelta * -1]
  openingStockComputed[openingStockFromDelta < 0, openingStockFromDelta := 0]
  setnames(openingStockComputed, "openingStockFromDelta", "openingStocks")
  openingStockComputed[, c("measuredElementFS", "deltaStocks",
                           "flagObservationStatus", "flagMethod") := NULL]
  openingStockComputed[, timePointYears := as.character(year)]
  return(openingStockComputed[])
}

