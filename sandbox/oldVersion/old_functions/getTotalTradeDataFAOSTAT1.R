##' Get Total Trade Data From FAOSTAT1
##'
##' This function pulls the old total trade data from FAOSTAT1
##'
##' @param yearRange Character vector with the range of the years.
##'
##' @return The dataset with the total trade data.
##'
##' @export
##'

getTotalTradeDataFAOSTAT1 <- function(yearRange) {
  tradeCode <- c("61", "91")
  fcl =  suppressWarnings(as.character(as.numeric(cpc2fcl(getStocksCPC(), returnFirst = T))))
  fcl <- fcl[!is.na(fcl)]
  totalTradeKey = DatasetKey(
    domain = "faostat_one",
    dataset = "updated_sua_2013_data",
    dimensions = list(
      Dimension(name = "geographicAreaFS",
                keys = GetCodeList(domain = "faostat_one",
                                   dataset = "updated_sua_2013_data",
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

  totalTradeData[measuredElementFS == "61", measuredElement := "5610"]
  totalTradeData[measuredElementFS == "91", measuredElement := "5910"]  
  totalTradeData[, c("geographicAreaFS", "measuredItemFS", "measuredElementFS") := NULL]

  setcolorder(totalTradeData, c("geographicAreaM49", "measuredElement", 
                                "measuredItemCPC", "Value", "timePointYears"))
  
  totalTradeData <- totalTradeData[!is.na(geographicAreaM49)]
  
  
  return(totalTradeData[])
  
}
