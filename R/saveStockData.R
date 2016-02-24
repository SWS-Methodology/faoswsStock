##' Save Stock Data
##' 
##' This function is essentially a SaveData call, but it does a few checks and 
##' "clean-ups" to the stock data to prepare it for saving.
##' 
##' @param data The data.table containing the stock data, as would be passed to
##'   SaveData.
##' @param verbose Logical.  Should messages be printed updating the user on
##'   what is happening?
##' @param stockCode The element code for the stock commodity.
##'   
##' @return No R objects are returned, as this functions purpose is solely to 
##'   write to the database.
##'   
##' @export
##' 

saveStockData = function(data, verbose = FALSE, stockCode = "5071"){
    
    ## Data Quality Checks
    stopifnot(is(data, "data.table"))
    
    ## Remove columns to match the database
    requiredColumns = c("geographicAreaM49", "measuredItemCPC",
                        "timePointYears", "measuredElement")
    missingColumns = requiredColumns[!requiredColumns %in% colnames(data)]
    if(length(missingColumns) > 0)
        stop("Missing required columns, so data cannot be saved!  Missing:\n",
             paste0(missingColumns, collapse = "\n"))
    data = data[, c(requiredColumns, "Value",
                    "flagObservationStatus", "flagMethod"), with = FALSE]

    ## Filter the data by removing any invalid date/country combinations
    if(verbose)
        cat("Removing invalid date/country combinations from the dataset.\n")
    data = removeInvalidDates(data)
    
    ## Can't save NA's back to the database, so convert to 0M
    data[is.na(Value) | flagObservationStatus == "M",
             `:=`(c("Value", "flagObservationStatus", "flagMethod"),
                  list(0, "M", "n"))]

    ## Remove official data
    key = DatasetKey(domain = "agriculture", dataset = "agriculture", dimensions = list(
        geographicAreaM49 = Dimension(name = "geographicAreaM49",
                                      keys = unique(data$geographicAreaM49)),
        measuredItemCPC = Dimension(name = "measuredItemCPC",
                                    keys = unique(data$measuredItemCPC)),
        measuredElement = Dimension(name = "measuredElement",
                                      keys = stockCode),
        timePointYears = Dimension(name = "timePointYears",
                                      keys = unique(data$timePointYears))))
    officialData = GetData(key)
    officialData = officialData[!(flagObservationStatus == "M" & flagMethod == "u"), ]
    setkeyv(officialData, c("geographicAreaM49", "measuredItemCPC", "timePointYears"))
    setkeyv(data, c("geographicAreaM49", "measuredItemCPC", "timePointYears"))
    data = data[!officialData, ]
    
    ## Save the data back
    if(verbose)
        cat("Attempting to write data back to the database.\n")
    warning("HACK!  Sorting because of SaveData issue!")
    attr(data, "sorted") = NULL
    if(nrow(data) >= 1){ # If invalid dates/official data caused 0 rows, don't try to save.
        return(faosws::SaveData(domain = "agriculture",
                                dataset = "agriculture",
                                data = data, normalized = TRUE))
    } else {
        return(list(inserted = 0, appended = 0, ignored = 0, discarded = 0,
                    warnings = NULL))
    }
}