##' Get FAOSTAT1 Data
##'
##' This function pulls the old food data from FAOSTAT1 and converts the codes 
##' for the countries, items and flags.
##'
##' @param geographicAreaM49 Character vector with the countries in m49 code.
##' @param measuredItemCPC Character vector with the cpc codes.
##' @param component String with the component code of the FBS. For instance, could be 141 for food or 71 for stock variation.
##' @param yearRange Character vector with the range of the years.
##' @param dataset String with the id name of the dataset in the domain FAOSTAT1. 
##'
##' @return The dataset with the stock data.
##'
##' @export
##'

getFAOSTAT1Data <- function(geographicAreaM49, measuredItemCPC, component, yearRange, dataset) {
    code <- component
    fcl =  suppressWarnings(as.character(as.numeric(cpc2fcl(measuredItemCPC, returnFirst = T))))
    fcl = fcl[!is.na(fcl)]
    countryFS =  suppressWarnings(m492fs(geographicAreaM49))
    countryFS = countryFS[!is.na(countryFS)]
    stockKey = DatasetKey(
        domain = "faostat_one",
        dataset = dataset,
        dimensions = list(
            Dimension(name = "geographicAreaFS",
                      keys = countryFS),
            Dimension(name = "measuredElementFS", keys = code),
            Dimension(name = "timePointYears", keys = yearRange),
            Dimension(name = "measuredItemFS",
                      keys = fcl)
        )
    )
    
    stockata = GetData(
      stockKey,
        flags = TRUE)
    
    if(nrow(stockata) > 0) {
      stockata[, geographicAreaM49 := fs2m49(geographicAreaFS)]
      stockata[, measuredItemCPC := fcl2cpc(formatC(as.numeric(measuredItemFS), width = 4,
                                                        flag = "0"))]
    
      stockata[, flagObservationStatus := getFlagObservationStatus(flagFaostat)]
      stockata[, flagMethod := getFlagMethod(flagFaostat)]    
    
      stockata[, c("geographicAreaFS", "measuredItemFS", "flagFaostat") := NULL]
    
    setcolorder(stockata, c("geographicAreaM49", "measuredElementFS",
                                  "measuredItemCPC", "Value", "timePointYears",
                                  "flagObservationStatus", "flagMethod"))

    stockata <- stockata[!is.na(geographicAreaM49)]
    
} else {stop('Empty data.table (0 rows)')}
}