## setwd("C:/Users/caetano/Documents/Github/faoswsStock")
## OBJECTIVE:
##   merge basic_main.R and faoswsProduction/modules/production_input_validation/main.R
##
## Find "sws.yml":
## setwd("../../faoswsLoss") # set wd to repository
## list.files("modules")
## name = "impute_loss" # name for AddModule - TODO (Sebastian): use moustache template?
## validationRange = "session"
## swsContext.userEmail <- "bo.werth@fao.org"
##
## TODO:
##   assess heterogeneity across domains-specific "processingParameters"
## used for autoValueCorrection
##   attachments hard-coded:
## "<test>ErrorAttachmentObject"
##   attachment path is hard-coded:
## prodValueErrorAttachmentPath =
##     paste0(R_SWS_SHARE_PATH, "/kao/", prodValueErrorAttachmentName)
##   module domainhard-coded in final returned message:
## "Production Input"


## This is a sample main file for a module
suppressMessages({
  library(faosws)
  library(faoswsUtil)
  library(faoswsFlag)
  library(faoswsImputation)
  library(faoswsProduction)
  library(faoswsProcessing)
  library(faoswsEnsure)
  library(magrittr)
  library(dplyr)
  library(sendmailR)
})

## If we're on the server, there's a global variable that defines with share
## folder
R_SWS_SHARE_PATH <- Sys.getenv("R_SWS_SHARE_PATH")

## This return FALSE if on the Statistical Working System
if(CheckDebug()){

  library(faoswsModules)

  ## REVISION START ##
  ## SETTINGS <- ReadSettings("sws.yml")
  SETTINGS <- ReadSettings(file.path("modules/stock_input_validation/sws.yml"))
  ## REVISION END ##

  ## If you're not on the system, your settings will overwrite any others
  R_SWS_SHARE_PATH <- SETTINGS[["share"]]

  ## Define where your certificates are stored
  SetClientFiles(SETTINGS[["certdir"]])

  ## Get session information from SWS. Token must be obtained from web interface
  GetTestEnvironment(baseUrl = SETTINGS[["server"]],
                     token = SETTINGS[["token"]])

}

## START ##
## copy faoswsProduction/modules/production_input_validation/main.R

##' Obtain input computation parameter
validationRange = swsContext.computationParams$validation_selection
if(CheckDebug()){
    validationRange = "all"
}

##' Get session key and dataset configuration
sessionKey = swsContext.datasets[[1]]
datasetConfig = GetDatasetConfig(domainCode = sessionKey@domain,
                                 datasetCode = sessionKey@dataset)


##' Obtain the complete imputation Datakey
completeImputationKey = getCompleteImputationKey(table = "stocks")

##' Selected the key based on the input parameter
selectedKey =
  switch(validationRange,
         "session" = sessionKey,
         "all" = completeImputationKey)


## cpc_items = "Selected CPC codes are not in the dataset"

dataset_cpc_codes <- GetCodeList("agriculture", "aproduction", "measuredItemCPC")[["code"]]
session_cpc_codes <- selectedKey@dimensions$measuredItemCPC@keys


## length(new_cpc_codes) - length(old_cpc_codes) 

## ISSUE: previous codes are not in dataset - please provide new codes
missing_cpc_codes <- setdiff(session_cpc_codes, dataset_cpc_codes)


##' Build processing parameters
# processingParameters =
#   productionProcessingParameters(datasetConfig = datasetConfig)


##' Extract the data from the Statistical Working System



##' Perform autocorrection, certain incorrectly specified flag can be
##' automatically corrected as per agreement with team B/C.
## autoFlagCorrection = function(data,
  #                             flagObservationStatusVar = "flagObservationtatus",
  #                             flagMethodVar = "flagMethod"){
  # dataCopy = copy(data)

  ## ## Correction (1): (M, -) --> (M, u)
  ## correctionFilter =
  ##     dataCopy[[flagObservationStatusVar]] == "M" &
  ##     dataCopy[[flagMethodVar]] == "-"
  ## dataCopy[correctionFilter,
  ##          `:=`(c(flagObservationStatusVar, flagMethodVar),
  ##               c("M", "u"))]

  ## ## Correction (2): (E, t) --> (E, -)
  ## correctionFilter =
  ##     dataCopy[[flagObservationStatusVar]] == "E" &
  ##     dataCopy[[flagMethodVar]] == "t"
  ## dataCopy[correctionFilter,
  ##          `:=`(c(flagObservationStatusVar, flagMethodVar),
  ##               c("E", "-"))]

  ## ## Correction (3): (E, e) --> (I, e)
  # correctionFilter =
  #   dataCopy[[flagObservationStatusVar]] == "E" &
  #   dataCopy[[flagMethodVar]] == "e"
  # dataCopy[correctionFilter,
  #          `:=`(c(flagObservationStatusVar, flagMethodVar),
  #               c("I", "e"))]

  ## ## Correction (4): (E, p) --> (E, f)
  ## correctionFilter =
  ##     dataCopy[[flagObservationStatusVar]] == "E" &
  ##     dataCopy[[flagMethodVar]] == "p"
  ## dataCopy[correctionFilter,
  ##          `:=`(c(flagObservationStatusVar, flagMethodVar),
  ##               c("E", "f"))]

#   dataCopy
# }

## autoValueCorrection = function(data,
##                                processingParameters,
##                                formulaParameters){
##     correctedData =
##         with(formulaParameters,
##              with(processingParameters,
##                   data %>%
##                   removeZeroConflict(data = .,
##                                      value1 = productionValue,
##                                      value2 = areaHarvestedValue,
##                                      observationFlag1 = productionObservationFlag,
##                                      observationFlag2 =
##                                          areaHarvestedObservationFlag,
##                                      methodFlag1 = productionMethodFlag,
##                                      methodFlag2 = areaHarvestedMethodFlag,
##                                      missingObservationFlag =
##                                          missingValueObservationFlag,
##                                      missingMethodFlag =
##                                          missingValueMethodFlag) %>%
##                   removeZeroYield(data = .,
##                                   yieldValue = yieldValue,
##                                   yieldObsFlag = yieldObservationFlag,
##                                   yieldMethodFlag = yieldMethodFlag)
##                   )
##              )
##     correctedData
## }


## TODO (Michael): Need to auto correct values. (e.g. conflict area harvested
##                 production value.

# selectedItem =
#   unique(slot(slot(selectedKey, "dimensions")[[processingParameters$itemVar]],
#               "keys"))

## ## COSMETIC MODIFICATION: combine "tests" and "testMessage" into named list:
## tests = c("flag_validity", "production_range",
##           "areaHarvested_range", "yield_range", "balanced")
## testMessage = c("The following entry contains invalid flag",
##                 "The following entry contain invalid production value",
##                 "The following entry contain invalid area harvsted value",
##                 "The following entry contain invalid yield value",
##                 "The following entry is imbalanced, check the values then re-run balance production identity module")
testMessageList <- list(
  cpc_items = paste("The selected CPC codes are not in the dataset.\nPlease update the fbs_stock_comm_codes datatable:\n", 
                    paste(missing_cpc_codes, collapse = " ")
                    )
  # production_range = "The following entry contain invalid production value",
  # areaHarvested_range = "The following entry contain invalid area harvsted value",
  # yield_range = "The following entry contain invalid yield value",
  # balanced = "The following entry is imbalanced, check the values then re-run balance production identity module"
)
tests <- names(testMessageList)
testMessage <- unname(unlist(testMessageList))

errorList = vector("list", length(tests))


##' Perform input validation item by item
## iter <- 1
# for(iter in seq(selectedItem)){
# 
#   currentItem = selectedItem[iter]
#   message("Performing input validation for item: ", currentItem)
  ## currentFormula =
  ##     getYieldFormula(currentItem) %>%
  ##     removeIndigenousBiologicalMeat
  # currentKey = selectedKey

  ## ## Update the item and element key to for current item
  ## currentKey@dimensions$measuredItemCPC@keys = currentItem
  ## currentKey@dimensions$measuredElement@keys =
  ##   with(currentFormula,
  ##        c(input, productivity, output))

  ## ## Create the formula parameter list
  ## formulaParameters =
  ##   with(currentFormula,
  ##        productionFormulaParameters(datasetConfig = datasetConfig,
  ##                                    productionCode = output,
  ##                                    areaHarvestedCode = input,
  ##                                    yieldCode = productivity,
  ##                                    unitConversion = unitConversion))

  ## Extract the current data
  # currentData =
  #   currentKey %>%
  #   GetData(key = .)
  ## class(currentData)

  ## NOTE (Michael): The test should be conducted on the raw data, that is
  ##                 excluding any imputation, statistical estimation and
  ##                 previous calculated values. However, auto corrected
  ##                 values will be saved back to the database.
  # if(nrow(currentData) > 0){
  # 
  #   autoCorrectedData =
  #     currentData %>%
  #     fillRecord(data = .) %>%
  #     preProcessing(data = .) %>%
  #     autoFlagCorrection(data = .) # %>%
      ## denormalise(normalisedData = .,
      ##             denormaliseKey = processingParameters$elementVar) %>%
      ## autoValueCorrection(data = .,
      ##                     processingParameters = processingParameters,
      ##                     formulaParameters = formulaParameters) %>%
      ## normalise

    # rawData =
    #   autoCorrectedData # %>%
      ## denormalise(normalisedData = .,
      ##             denormaliseKey = processingParameters$elementVar) %>%
      ## processProductionDomain(data = .,
      ##                         processingParameters = processingParameters,
      ##                         formulaParameters = formulaParameters)

## Check cpc codes
errorList[[1]] =
    missing_cpc_codes %>%
    rbind(errorList[[1]], .)

    ## Check flag validity
    # errorList[[1]] =
    #   rawData %>%
    #   ## normalise %>%
    #   ensureFlagValidity(data = .,
    #                      getInvalidData = TRUE) %>%
    #   rbind(errorList[[1]], .)

    ## ## Check production value
    ## errorList[[2]] =
    ##   with(formulaParameters,
    ##   {
    ##     if(productionCode %in% rawData[["measuredElement"]]){
    ##       rawData %>%
    ##         ensureValueRange(data = .,
    ##                          ensureColumn = productionValue,
    ##                          getInvalidData = TRUE) %>%
    ##         normalise %>%
    ##         rbind(errorList[[2]], .)
    ##     } else {
    ##       rbind(errorList[[3]], data.table())
    ##     }
    ##   }
    ##   )


    ## ## Check area harvested value
    ## errorList[[3]] =
    ##   with(formulaParameters,
    ##   {
    ##     if(areaHarvestedCode %in% rawData[["measuredElement"]]){
    ##       rawData %>%
    ##         ensureValueRange(data = .,
    ##                          ensureColumn = areaHarvestedValue,
    ##                          getInvalidData = TRUE) %>%
    ##         normalise %>%
    ##         rbind(errorList[[3]], .)
    ##     } else {
    ##       rbind(errorList[[3]], data.table())
    ##     }
    ##   }
    ##   )

    ## ## Check yield value
    ## errorList[[4]] =
    ##   with(formulaParameters,
    ##   {
    ##     if(yieldCode %in% rawData[["measuredElement"]]){
    ##       rawData %>%
    ##         ensureValueRange(data = .,
    ##                          ensureColumn = yieldValue,
    ##                          getInvalidData = TRUE,
    ##                          includeEndPoint = FALSE) %>%
    ##         normalise %>%
    ##         rbind(errorList[[4]], .)
    ##     } else {
    ##       rbind(errorList[[3]], data.table())
    ##     }
    ##   }
    ##   )


    ## ## Check production domain balanced.
    ## errorList[[5]] =
    ##   with(formulaParameters,
    ##   {
    ##     if(all(c(yieldCode, areaHarvestedCode, productionCode) %in%
    ##            rawData[["measuredElement"]])){
    ##       rawData %>%
    ##         ensureProductionBalanced(data = .,
    ##                                  areaVar = areaHarvestedValue,
    ##                                  yieldVar = yieldValue,
    ##                                  prodVar = productionValue,
    ##                                  conversion = unitConversion,
    ##                                  getInvalidData = TRUE) %>%
    ##         rbind(errorList[[5]], .)
    ##     } else {
    ##       rbind(errorList[[5]], data.table())
    ##     }
    ##   }
    ##   )

    ## ## Save the auto-corrected data back
    ## autoCorrectedData %>%
    ##     SaveData(domain = sessionKey@domain,
    ##              dataset = sessionKey@dataset,
    ##              data = .)

#   } else {
#     message("Current Item has no data")
#   }
# 
# }



if(max(sapply(errorList, length)) > 0){
  # from = "notifier@ess-a.com"
  # to = swsContext.userEmail
  # subject = "Validation Result"
  ## body = paste0("There are 5 tests current in the system:\n",
  ##               "1. Flag validity: Whether a flag is valid\n",
  ##               "2. Production range: [0, Inf)\n",
  ##               "3. Area Harvested range: [0, Inf)\n",
  ##               "4. Yield range: (0, Inf)\n",
  ##               "5. Production balanced")
  body = paste0("Currently there are ", length(tests), " tests in the system:\n",
                # paste0(tests, collapse = "\n")
                paste0(
                    paste0("===\n", tests, ": ", testMessage, "\n---\n")
                    , 
                    collapse = "\n")
                )
  ## cat(body)
  ## CPC code error
  # cpc_codeErrorAttachmentName = "cpc_code_error.csv"
  # cpc_codeErrorAttachmentPath =
  #     paste0(R_SWS_SHARE_PATH, "/kao/", cpc_codeErrorAttachmentName)
  # write.csv(errorList[[1]], file = cpc_codeErrorAttachmentPath,
  #           row.names = FALSE)
  # cpc_codeErrorAttachmentObject = mime_part(x = cpc_codeErrorAttachmentPath,
  #                                       name = cpc_codeErrorAttachmentName)
  
  ## Flag error
  # flagErrorAttachmentName = "flag_error.csv"
  # flagErrorAttachmentPath =
  #   paste0(R_SWS_SHARE_PATH, "/kao/", flagErrorAttachmentName)
  # write.csv(errorList[[1]], file = flagErrorAttachmentPath,
  #           row.names = FALSE)
  # flagErrorAttachmentObject = mime_part(x = flagErrorAttachmentPath,
  #                                       name = flagErrorAttachmentName)

  ## ## production value error
  ## prodValueErrorAttachmentName = "prodValue_error.csv"
  ## prodValueErrorAttachmentPath =
  ##   paste0(R_SWS_SHARE_PATH, "/kao/", prodValueErrorAttachmentName)
  ## write.csv(errorList[[2]], file = prodValueErrorAttachmentPath,
  ##           row.names = FALSE)
  ## prodValueErrorAttachmentObject =
  ##   mime_part(x = prodValueErrorAttachmentPath,
  ##             name = prodValueErrorAttachmentName)


  ## ## area harvested value error
  ## areaValueErrorAttachmentName = "areaValue_error.csv"
  ## areaValueErrorAttachmentPath =
  ##   paste0(R_SWS_SHARE_PATH, "/kao/", areaValueErrorAttachmentName)
  ## write.csv(errorList[[3]], file = areaValueErrorAttachmentPath,
  ##           row.names = FALSE)
  ## areaValueErrorAttachmentObject =
  ##   mime_part(x = areaValueErrorAttachmentPath,
  ##             name = areaValueErrorAttachmentName)


  ## ## yield value error
  ## yieldValueErrorAttachmentName = "yieldValue_error.csv"
  ## yieldValueErrorAttachmentPath =
  ##   paste0(R_SWS_SHARE_PATH, "/kao/", yieldValueErrorAttachmentName)
  ## write.csv(errorList[[4]], file = yieldValueErrorAttachmentPath,
  ##           row.names = FALSE)
  ## yieldValueErrorAttachmentObject =
  ##   mime_part(x = yieldValueErrorAttachmentPath,
  ##             name = yieldValueErrorAttachmentName)


  ## ## imbalance error
  ## imbalanceErrorAttachmentName = "imbalance_error.csv"
  ## imbalanceErrorAttachmentPath =
  ##   paste0(R_SWS_SHARE_PATH, "/kao/", imbalanceErrorAttachmentName)
  ## write.csv(errorList[[5]], file = imbalanceErrorAttachmentPath,
  ##           row.names = FALSE)
  ## imbalanceErrorAttachmentObject =
  ##   mime_part(x = imbalanceErrorAttachmentPath,
  ##             name = imbalanceErrorAttachmentName)



  # bodyWithAttachment =
  #   list(body,
  #        cpc_codeErrorAttachmentObject
  #        # flagErrorAttachmentObject
  #       ## ,
  #       ##  prodValueErrorAttachmentObject,
  #       ##  areaValueErrorAttachmentObject,
  #       ##  yieldValueErrorAttachmentObject,
  #       ##  imbalanceErrorAttachmentObject
  #        )
  # sendmail(from = from, to = to, subject = subject, msg = bodyWithAttachment)
  # stop("Stock Input Invalid, please check follow up email on invalid data")
  msg = paste("Stock Input Invalid", body, sep = "\n")
  ## cat(msg)
} else {
  msg = "Stock Input Validation passed without any error!"
}
msg
