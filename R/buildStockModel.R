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
##' @param data A data.table containing the data on the historical stock 
##'   changes.
##' @param yearColumn The column name of data which contains the years.
##' @param valueColumn The column name of data which contains the values of the 
##'   historical stock changes.
##' @param groupingColumns The column name(s) of data which contain the 
##'   variables which should be used for grouping.  Usually this will be country
##'   and commodity variables: we don't want to use all data when building the 
##'   model because a stock change of 1000 for one country and commodity is very
##'   different than a change of 1000 for a different country/commodity.  This 
##'   argument allows the user to specify these "groups" and then each group is 
##'   scaled according to it's variance (i.e. all stock changes are divided by 
##'   the standard deviation).  Then, we estimate the model on all the scaled 
##'   data.  Note that the cumulative stock computation will not be correct 
##'   without the correct grouping columns.
##' @param cumulativeYears Numeric.  How many years should be used to compute 
##'   the historical cumulative stock changes?  Defaults to 15.
##' @param plot Logical.  Should a plot be generated which shows the observed 
##'   data as well as the model fit?  The error bars represent the confidence 
##'   interval of the mean, not a prediction interval (and thus we don't expect 
##'   that most observations will fall inside this interval).
##' 
##' @return A list with three elements
##'   
##'   \itemize{
##'   
##'   \item model An object of type lm to use for prediction
##'   
##'   \item yearColumn The passed yearColumn.
##'   
##'   \item valueColumn The passed valueColumn.
##'   
##'   \item groupingColumns The passed groupingColumns.
##'   
##'   \item cumulativeYears The passed cumulativeYears. }
##'   

buildStockModel = function(data, yearColumn = "Year",
                           valueColumn = "Value",
                           groupingColumns = c("AreaCode", "ItemCode"),
                           cumulativeYears = 15, plot = FALSE){
    
    ## Input Checks
    stopifnot(is(data, "data.table"))
    stopifnot(yearColumn %in% colnames(data))
    stopifnot(valueColumn %in% colnames(data))
    stopifnot(groupingColumns %in% colnames(data))
    stopifnot(cumulativeYears < length(unique(data[[yearColumn]])))
    ## You can't group by year, that wouldn't make sense.
    stopifnot(!yearColumn %in% groupingColumns)
    ## Make sure each group (as defined by groupingColumns) has at most one
    ## record per year.
    test = data[, length(unique(get(yearColumn))) == .N, by = groupingColumns]
    if(!all(test$V1))
        stop("There is more than one observation for the same year within ",
             "some group.  The stock model is not valid in that case.")

    ## Estimate variance within each group and scale the values
    data[, Variance := var(get(valueColumn)), by = groupingColumns]
    data[, scaledValue := get(valueColumn) / sqrt(Variance)]
    ## For cumulative calcs, we need to ensure we've ordered the data by year. 
    ## Since we do calcs within each grouping variable, we don't need to worry
    ## about sorting by them too.
    data = data[order(get(yearColumn)), ]
    ## Compute the cumulative stock changes.  For the first cumulativeYears
    ## values, we won't have any available data.  Then, rollsumr will give us
    ## the values we need, but the last observation it provides will be the sum
    ## of the last cumulativeYears observations.  We don't need this one, so we
    ## use .N+1 to throw it out.
    data[, cumulativeStock := c(rep(NA, cumulativeYears),
            zoo::rollsumr(scaledValue, k = cumulativeYears))[-(.N+1)],
         by = groupingColumns]
    
    ## Fit the model
    model = data[, lm(scaledValue ~ cumulativeStock + 0)]
    
    ## Generate a plot of the data with the fit
    if(plot){
        ## Suppress ggplot2 warnings, and print out the graph so it is displayed
        suppressWarnings(print(
            ggplot(data, aes_string(x = "cumulativeStock", y = valueColumn)) +
                geom_point() +
                geom_smooth(method = "lm", formula = y ~ x + 0)
        ))
    }
    
    return(list(model = model,
                yearColumn = yearColumn,
                valueColumn = valueColumn,
                groupingColumns = groupingColumns,
                cumulativeYears = cumulativeYears))
}