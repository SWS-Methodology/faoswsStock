##' Get Stock Data
##' 
##' This function pulls historical, measured data on stock changes.  Currently
##' it's just reading a .csv file, as that's all we have.
##' 
##' @return The data, stored in a data.table format.
##' 

getStockData = function(){
    read.csv("~/Documents/Github/faoswsStock/sandbox/Data/usaStockVariabilityFAOSTAT.csv")
}