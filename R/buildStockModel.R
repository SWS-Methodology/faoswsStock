##' Build Stock Model
##' 
##' This function takes a dataset of historical, observed stock changes and 
##' estimates a simple, linear regression model using the cumulative previous 
##' drawdowns/inputs as the independent variable and the current drawdown/input 
##' as the dependent variable.  Normality seemed like a reasonable assumption 
##' after some initial testing, and so we feel comfortable with a simple linear 
##' regression.  Thus, the distribution of the stock change in a year t, given
##' the previous cumulative drawdowns/inputs, will be normal with some mean and
##' variance estimated from this model.
##' 
##' @param data
##' @param valueColumn
##' @param groupingColumns
##' 
##' @return 
##' 

buildStockModel = function(data, valueColumn = "Value",
                           groupingColumns = c("AreaCode", "ItemCode")){
    ## Input Checks
    
    ## Estimate variance within each group to scale the model
    data[, Variance := var(get(valueColumn)), by = groupingColumns]
}