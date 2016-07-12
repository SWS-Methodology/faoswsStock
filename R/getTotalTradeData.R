##' Get Total Trade Data
##'
##' This function pulls the total trade data from the SWS
##'
##' @param yearRange Character vector with the range of the years.
##'
##' @return The dataset with the total trade data.
##'
##' @export
##'

getTotalTradeData <- function(yearRange) {
  
  tradeCode <- c("5610", "5910")
  totalTradeKey = DatasetKey(
    domain = "trade",
    dataset = "total_trade_cpc_m49",
    dimensions = list(
      Dimension(name = "geographicAreaM49",
                keys = getAllCountries()),
      Dimension(name = "measuredElementTrade", keys = tradeCode),
      Dimension(name = "timePointYears", keys = yearRange),
      Dimension(name = "measuredItemCPC",
                keys = getStocksCPC())
    )
  )
  
  totalTradeData = GetData(
    totalTradeKey,
    flags = FALSE)
  return(totalTradeData)
}
