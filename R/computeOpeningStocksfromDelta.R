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
  stockCode <- "5071"
  deltaStockData <- getStockData(measuredElement = c(stockCode), 
                                 yearRange)
  setnames(deltaStockData, "Value", "deltaStocks")
  
  setkey(deltaStockData, geographicAreaM49, measuredItemCPC, timePointYears)
  
  deltaStockData[is.na(deltaStocks), deltaStocks := 0]
  deltaStockData[, openingStockFromDelta := cumsum(deltaStocks),
                  by=list(geographicAreaM49, measuredItemCPC)]
  
  openingStockComputed <- deltaStockData[timePointYears == year-1]
  openingStockComputed[, openingStocks := openingStockFromDelta * -1]
  openingStockComputed[openingStocks < 0, openingStocks := 0]
  openingStockComputed[, c("openingStockFromDelta", "measuredElement", "deltaStocks",
                           "flagObservationStatus", "flagMethod") := NULL]
  openingStockComputed[, timePointYears := as.character(year)]
  return(openingStockComputed[])
}

