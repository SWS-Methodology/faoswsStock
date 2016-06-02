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
##' @param estimateYear The year for which stock estimates should be generated. 
##'   Must be a single value.
##' @param elementVar The column name of the variable containing the element. 
##'   Defaults to "measuredElement".
##' @param warn Logical, defaults to FALSE.  The variability must be estimated 
##'   on data prior to the estimateYear.  If data after estimateYear exists, an 
##'   error is thrown by default or a warning generated (if TRUE) and data after
##'   estimateYear is deleted.
##'   
##' @return A data.table object with the predictions.
##'   
##' @export
##' 

predictStockModel = function(model, newdata, estimateYear,
                             elementVar = "measuredElement", warn = FALSE){
    
    ## Input Checks
    stopifnot(is(model, "list"))
    stopifnot(names(model) == c("model", "yearColumn", "valueColumn",
                                "groupingColumns", "cumulativeYears"))
    stopifnot(is(newdata, "data.table"))
    stopifnot(model$yearColumn %in% colnames(newdata))
    stopifnot(model$valueColumn %in% colnames(newdata))
    stopifnot(model$groupingColumns %in% colnames(newdata))
    stopifnot(length(estimateYear) == 1)
    if(any(newdata$timePointYears > estimateYear)){
        if(warn){
            warning("newdata should not have observations occurring after ",
                    "estimateYear!  Such observations have been deleted.")
            newdata = newdata[timePointYears <= estimateYear, ]
        } else {
            stop("newdata should not have observations",
                 " occurring after estimateYear!")
        }
    }
    
    ## Ensure that we have records for all groups in the estimation year
    toEstimate = unique(newdata[, model$groupingColumns, with = FALSE])
    toEstimate[, c(model$yearColumn) := as.character(estimateYear)]
    toEstimate[, c(elementVar) := "5071"]
    newdata = merge(newdata, toEstimate, all = TRUE,
                    by = c(model$groupingColumns, model$yearColumn,
                           elementVar))

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
        # If stock change was missing, assume it was zero.
        x[is.na(x)] = 0
        if(length(x) >= k){
            out = zoo::rollsumr(x, k, ...)
            out = c(rep(NA, k), out[-length(out)])
            return(out)
        } else {
            ## If length(x) < k, we can't do any computation.  However, we could
            ## make an assumption that the missing stock changes are all zeros. 
            ## This is a heroic assumption, but it allows us to impute for the
            ## current year.
            warning("HACK!  When historic stock changes are not available, ",
                    "assume zeros.")
            return(c(rep(NA_real_, length(x)-1),
                     sum(x, na.rm = TRUE)))
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
    newdata = newdata[get(model$yearColumn) == estimateYear, ]
    return(newdata)
}