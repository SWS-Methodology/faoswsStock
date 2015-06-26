##' Choose Stock Model
##' 
##' This function takes a vector of strings (of .RData objects that exist in
##' some directory) and chooses the one that corresponds to the model that
##' should be used.
##' 
##' @param fileNames A vector of file names.
##' 
##' @return The file name which should be loaded to provide the model.
##' 

chooseStockModel = function(fileNames){
    frozenModels = fileNames[grepl("frozen", tolower(fileNames))]
    ## If we have no frozen models then we should just use the most recent
    ## model.
    if(length(frozenModels) == 0){
        dates = gsub("(.*/stockModel|.RData)", "", fileNames)
        dates = as.POSIXct(dates, format = "%Y.%m.%d.%H.%M.%S")
        return(fileNames[which.max(dates)])
    } else {
    ## If we do have frozen models, then we should use the most recent one.
        dates = gsub("(.*/stockModel|frozen.RData)", "", frozenModels)
        dates = as.POSIXct(dates, format = "%Y.%m.%d.%H.%M.%S")
        return(frozenModels[which.max(dates)])
    }
}