## This module is used to load the stock changes model and use this model for 
## predictions on an open session in the agriculture production tables.  The 
## stock change model requires an estimate of the variability of historical 
## stock changes, and this estimate always uses 1991-2011.  Additionally, this
## module only estimates for 2012, as estimating for prior years will not be
## valid as the variance is being estimated assuming the data is available.

## load the library
library(faosws)
library(data.table)
library(ggplot2)
library(faoswsUtil)

## set up for the test environment and parameters
R_SWS_SHARE_PATH = Sys.getenv("R_SWS_SHARE_PATH")
R_SWS_SHARE_PATH = paste0(R_SWS_SHARE_PATH, "/browningj/stock/")
DEBUG_MODE = Sys.getenv("R_DEBUG_MODE")
timeRange = 1991:2011 # Change documentation if this changes!
estimateYear = 2012 # Change documentation if this changes!

if(!exists("DEBUG_MODE") || DEBUG_MODE == ""){
    cat("Not on server, so setting up environment...\n")
    
    ## Define directories
    if(Sys.info()[7] == "josh"){
        apiDirectory = "~/Documents/Github/faoswsStock/R/"
        R_SWS_SHARE_PATH = "~/Documents/Github/faoswsStock/savedModels/"
    }
    if(Sys.info()[7] == "rockc_000"){ #Josh's laptop
        apiDirectory = "~/Github/faoswsStock/R/"
        R_SWS_SHARE_PATH = "~/Github/faoswsStock/savedModels/"
    }

    ## Get SWS Parameters
    GetTestEnvironment(
        baseUrl = "https://hqlprswsas1.hq.un.fao.org:8181/sws",
        token = "59202d09-d085-41bc-8736-13ac5533e669"
        ## baseUrl = "https://hqlqasws1.hq.un.fao.org:8181/sws",
        ## token = "90bb0f92-e345-4401-945d-1e43af801167"
    )

    ## Source local scripts for this local test
    for(file in dir(apiDirectory, full.names = T))
        source(file)
    if(length(dir(apiDirectory, full.names = T)) == 0)
        stop("No files in apiDirectory.  Are you sure it's the right one?")
}

## Get historic stock change variability
areaCode = GetCodeList("faostat_one", "FS1_SUA", "geographicAreaFS")
areaCode = areaCode[type == "country", code]
areaDim = Dimension(name = "geographicAreaFS", keys = areaCode)
itemCode = GetCodeList("faostat_one", "FS1_SUA", "measuredItemFS")[, code]
itemDim = Dimension(name = "measuredItemFS", keys = itemCode)
yearDim = Dimension(name = "timePointYears",
                    keys = as.character(c(timeRange, estimateYear)))
elemDim = Dimension(name = "measuredElementFS", keys = "71")
key = DatasetKey(domain = "faostat_one", dataset = "FS1_SUA",
                 dimensions = list(areaDim, itemDim, yearDim, elemDim))
oldSuaData = GetData(key)

## Add in the estimateYear so that you have a place to write to
toBind = copy(oldSuaData)
toBind[, c("timePointYears", "Value", "flagFaostat") :=
           list(as.character(estimateYear), NA_real_, "M")]
oldSuaData = rbind(oldSuaData, unique(toBind))

## Load Stock Model
availableModels = list.files(R_SWS_SHARE_PATH, full.names = TRUE)
choosenModel = chooseStockModel(availableModels)
## This loads an object called "model"
load(choosenModel)

## Predict with the stock model
model$groupingColumns[1] = "geographicAreaFS"
model$groupingColumns[2] = "measuredItemFS"
model$yearColumn = "timePointYears"
model$valueColumn = "Value"
preds = predictStockModel(model, newdata = oldSuaData)

## Reshape data to write back to SWS:
preds = preds[timePointYears == estimateYear, ]
preds[, Value := expectedValue]
preds[, geographicAreaM49 := fs2m49(geographicAreaFS)]
preds[, measuredItemCPC := fcl2cpc(formatC(as.numeric(measuredItemFS),
                                           width = 4, format = "g", flag = "0"))]
preds[, measuredElement := measuredElementFS]
preds = preds[, list(geographicAreaM49, measuredItemCPC, measuredElement,
                     timePointYears, Value)]
preds[, flagObservationStatus := "I"]
preds[, flagMethod := "e"]
preds = preds[!is.na(Value), ]
SaveData(domain = "agriculture", dataset = "agriculture", data = preds)