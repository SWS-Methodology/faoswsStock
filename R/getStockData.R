##' Get Stock Data
##' 
##' This function pulls historical, measured data on stock changes.  Currently
##' it's just reading a .csv file, as that's all we have.
##' 
##' @return The data, stored in a data.table format.
##' 

getStockData = function(){
    ## Check if on server or not:
    if(exists("DEBUG_MODE") && DEBUG_MODE == FALSE){
        R_SWS_SHARE_PATH = Sys.getenv("R_SWS_SHARE_PATH")
        out = fread(paste0(R_SWS_SHARE_PATH, "/browningj/stock/usaStockVariabilityFAOSTAT.csv"))
    } else if(Sys.info()[7] == "josh"){
        out = fread("~/Documents/Github/faoswsStock/sandbox/Data/usaStockVariabilityFAOSTAT.csv")
    } else if(Sys.info()[7] == "rockc_000"){
        out = fread("~/Github/faoswsStock/sandbox/Data/usaStockVariabilityFAOSTAT.csv")
    } else {
        stop("No directory set up for this user yet!")
    }
    out = out[ItemCode <= 2520, ] # Cereals
    return(out)
}