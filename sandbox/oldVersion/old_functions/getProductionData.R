##' Get Production Data
##'
##' This function pulls the production data from the SWS
##'
##' @param yearRange Character vector with the range of the years.
##'
##' @return The dataset with the production data.
##'
##' @export
##'

getProductionData <- function(yearRange) {
  
  productionKey = DatasetKey(
    domain = "agriculture",
    dataset = "aproduction",
    dimensions = list(
      Dimension(name = "geographicAreaM49",
                keys = getAllCountries()),
      Dimension(name = "measuredElement",
                keys = "5510"),
      Dimension(name = "measuredItemCPC",
                keys = getStocksCPC()),
      Dimension(name = "timePointYears",
                keys = yearRange))
  )
  
  productionData = GetData(
    productionKey,
    flags = TRUE)
  return(productionData)
}
