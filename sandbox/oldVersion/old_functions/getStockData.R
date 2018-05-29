##' Get Stocks Data
##'
##' This function pulls the stock variation data from the SWS
##'
##' @param yearRange Character vector with the range of the years.
##'
##' @return The dataset with the stock variation.
##'
##' @export
##'

getStockData <- function(measuredElement, yearRange) {
  
  stockKey = DatasetKey(
    domain = "agriculture",
    dataset = "aproduction",
    dimensions = list(
      Dimension(name = "geographicAreaM49",
                keys = getAllCountries()),
      Dimension(name = "measuredElement",
                keys = measuredElement),
      Dimension(name = "measuredItemCPC",
                keys = getStocksCPC()),
      Dimension(name = "timePointYears",
                keys = yearRange))
  )
  
  stockData = GetData(
    stockKey,
    flags = TRUE)
  return(stockData)
}
