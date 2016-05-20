## This module is used to load the stock changes model and use this model for 
## predictions on an open session in the agriculture production tables.  The 
## stock change model requires an estimate of the variability of historical 
## stock changes, and this estimate always uses initialYear (set in the code
## below, currently to 1991) to the estimation year (provided by the user). 

## load the library
library(faosws)
library(data.table)
library(ggplot2)
library(faoswsUtil)

## set up for the test environment and parameters
## R_SWS_SHARE_PATH = Sys.getenv("R_SWS_SHARE_PATH")
## R_SWS_SHARE_PATH = paste0(R_SWS_SHARE_PATH, "/browningj/stock/")
## DEBUG_MODE = Sys.getenv("R_DEBUG_MODE")
initialYear = 1991 # Change documentation if this changes!

if(CheckDebug()){
  cat("Not on server, so setting up environment...\n")
  
  ## Define directories
  if(Sys.info()[7] == "josh"){
    apiDirectory = "~/Documents/Github/faoswsStock/R/"
    R_SWS_SHARE_PATH = "/media/hqlprsws1_qa/"
    SetClientFiles("~/R certificate files/Production/")
    # SetClientFiles("~/R certificate files/QA/")
  } else if(Sys.info()[7] == "caetano"){ # bruno's work computer
    apiDirectory = "~/Github/faoswsStock/R"
    R_SWS_SHARE_PATH = "//hqlprsws1.hq.un.fao.org/sws_r_share"
    SetClientFiles(dir = "~/.R/QA/")
    #files = dir("~/Github/faoswsFood/R", full.names = TRUE)
    #token = "66a36f31-1a29-4a49-8626-ae62117c251a"
  }
  ## Get SWS Parameters
  GetTestEnvironment(
    baseUrl = "https://hqlqasws1.hq.un.fao.org:8181/sws",
    token = "b4995d52-cc4c-4e21-a6d8-de90b1f42e63"
    ## baseUrl = "https://hqlprswsas1.hq.un.fao.org:8181/sws",
    ## token = "90c00557-e740-46f4-8cd7-6c90e6d96124"
  )
  
  ## Source local scripts for this local test
  for(file in dir(apiDirectory, full.names = T))
    source(file)
  if(length(dir(apiDirectory, full.names = T)) == 0)
    stop("No files in apiDirectory.  Are you sure it's the right one?")
  
  availableModels = list.files(paste0(R_SWS_SHARE_PATH, "/browningj/stock"),
                               full.names = TRUE)
}

startYear = as.numeric(swsContext.computationParams$startYear)
endYear = as.numeric(swsContext.computationParams$endYear)
stopifnot(startYear <= endYear)
years = startYear:endYear

timeRange = initialYear:endYear

## Get historic stock change variability
## m49Area = swsContext.datasets[[1]]@dimensions$geographicAreaM49@keys
## fsArea  = m492fs(m49Area)
## fsArea  = fsArea[!is.na(fsArea)]
## areaDim = Dimension(name = "geographicAreaFS", keys = fsArea)
## cpcItem = swsContext.datasets[[1]]@dimensions$measuredItemCPC@keys
## fclItem = as.character(as.numeric(cpc2fcl(cpcItem, returnFirst = TRUE)))
## fclItem = fclItem[!is.na(fclItem)]
## itemDim = Dimension(name = "measuredItemFS", keys = fclItem)
## yearDim = Dimension(name = "timePointYears",
##                  keys = as.character(initialYear:endYear))
## elemDim = Dimension(name = "measuredElementFS", keys = "71")
## key = DatasetKey(domain = "faostat_one", dataset = "FS1_SUA",
##                  dimensions = list(areaDim, itemDim, yearDim, elemDim))
## oldSuaData = GetData(key)

## Using the new data on SWS instead of oldSuaData
stockCode = "5071"
key = DatasetKey(
  domain = "agriculture",
  dataset = "aproduction",
  dimensions = list(
    Dimension(
      name = "geographicAreaM49",
      keys = GetCodeList("agriculture", "aproduction", "geographicAreaM49")[, code]
    ),
    Dimension(name = "measuredElement", keys = stockCode),
    Dimension(name = "timePointYears", keys = as.character(initialYear:endYear)),
    Dimension(
      name = "measuredItemCPC",
      keys = GetCodeList("agriculture", "aproduction", "measuredItemCPC")[, code]
    )
  )
)

newSuaData = GetData(key)

## Add in the estimateYear so that you have a place to write
toBind = copy(newSuaData)
toBind[, c("timePointYears", "Value", "flagObservationStatus") :=
           list(NA_character_, NA_real_, "M")]
toBind = unique(toBind)
for(year in years){
    toBind[, timePointYears := year]
    newSuaData = rbind(newSuaData, unique(toBind))
}
## We may have two records for estimateYear now (previous data and an NA). 
## So, clean those out:
flagAgg = function(flags){
    ## if(length(flags) > 2)
    ##    stop("Need 2 or fewer flags!")
    ## if(any(flags != "M")){
  if(!any(is.na(flags != "M")) & any(flags != "M")){
        stopifnot(sum(flags != "M") == 1)
        return(flags[flags != "M"])
    }
    return("M")
}
valAgg = function(values){
    ## if(length(values) > 2)
    ##  stop("Need 2 or fewer flags!")
    if(any(!is.na(values))){
        stopifnot(sum(!is.na(values)) == 1)
        return(values[!is.na(values)])
    }
    return(NA_real_)
}
newSuaData = newSuaData[, list(Value = valAgg(Value),
                               flagObservationStatus = flagAgg(flagObservationStatus)),
                        by = c("geographicAreaM49", "measuredElement",
                               "measuredItemCPC", "timePointYears")]

## Load Stock Model
## availableModels = list.files(R_SWS_SHARE_PATH, full.names = TRUE)
choosenModel = chooseStockModel(availableModels)
## This loads an object called "model"
load(choosenModel)

## Predict with the stock model
model$groupingColumns[1] = "geographicAreaM49"
model$groupingColumns[2] = "measuredItemCPC"
model$yearColumn = "timePointYears"
model$valueColumn = "Value"
preds = lapply(years, function(year){
    predictStockModel(model, newdata = newSuaData[timePointYears <= year, ],
                      estimateYear = year)
})
preds = do.call("rbind", preds)

## Reshape data to write back to SWS:
preds[, Value := expectedValue]
## preds[, geographicAreaM49 := fs2m49(geographicAreaFS)]
## preds[, measuredItemCPC := fcl2cpc(formatC(as.numeric(measuredItemFS),
##                                         width = 4, format = "g", flag = "0"))]
# preds[, measuredElement := measuredElementFS]
## preds[, measuredElement := "5071"]
preds = preds[, list(geographicAreaM49, measuredItemCPC, measuredElement,
                     timePointYears, Value)]
preds[, flagObservationStatus := "I"]
preds[, flagMethod := "e"]
preds = preds[!is.na(Value), ]
stats = saveStockData(data = preds)

paste0(stats$inserted, " observations written, ",
       stats$ignored, " weren't updated, ",
       stats$discarded, " had problems.")
