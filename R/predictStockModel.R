##' Predict Stock Model
##' 
##' This function takes a fitted model (from buildStockModel) and fits it to a 
##' provided dataset.  Essentially, this just requires calling predict.lm on the
##' newdata, but the complicated bit is that the cumulative stock changes must 
##' also be computed for estimation.
##' 
##' @param model A list object containing the relevant model information. Rather
##'   than constructing this directly, it is easier to just pass the result of a
##'   call to buildStockModel.
##' @param newdata A data.table containing the data which should be used for 
##'   prediction.  If not provided, prediction is performed on the data used to
##'   fit the model.
##'   
##' @return
##' 

predictStockModel = function(model, newdata){
    
    ## Input Checks
    stopifnot(is(model, "list"))
    stopifnot(names(model) == c("model", "yearColumn", "valueColumn",
                                "groupingColumns", "cumulativeYears"))
    stopifnot(is(newdata, "data.table"))
    stopifnot(model$yearColumn %in% colnames(newdata))
    stopifnot(model$valueColumn %in% colnames(newdata))
    stopifnot(model$groupingColumns %in% colnames(newdata))

    ## Estimate variance within each group and scale the values
    newdata[, Variance := var(get(model$valueColumn), na.rm = TRUE),
            by = c(model$groupingColumns)]
    newdata[, scaledValue := get(model$valueColumn) / sqrt(Variance)]
    ## For cumulative calcs, we need to ensure we've ordered the data by year. 
    ## Since we do calcs within each grouping variable, we don't need to worry
    ## about sorting by them too.
    newdata = newdata[order(get(model$yearColumn)), ]
    ## Compute the cumulative stock changes.  For the first cumulativeYears
    ## values, we won't have any available data.  Then, rollsumr will give us
    ## the values we need, but the last observation it provides will be the sum
    ## of the last cumulativeYears observations.  We don't need this one, so we
    ## throw it out.
    ## 
    ## First, wrap rollsumr to avoid errors and return the same length
    rollsumr = function(x, k, ...){
        if(length(x) >= k){
            out = zoo::rollsumr(x, k, ...)
            out = c(rep(NA, k), out[-length(out)])
            return(out)
        } else {
            return(rep(NA_real_, length(x)))
        }
    }
    newdata[, cumulativeStock := rollsumr(scaledValue, k = model$cumulativeYears),
         by = c(model$groupingColumns)]
    
    out = predict(model$model, newdata = newdata, se.fit = TRUE)
    newdata[, expectedValue := out$fit]
    newdata[, sdEstimate := out$se.fit]
    ## Scale up by the variance
    newdata[, expectedValue := expectedValue * sqrt(Variance)]
    newdata[, sdEstimate := sdEstimate * sqrt(Variance)]
    return(newdata)
}