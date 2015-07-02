## load the library
library(faosws)
library(data.table)
library(ggplot2)

## set up for the test environment and parameters
R_SWS_SHARE_PATH = Sys.getenv("R_SWS_SHARE_PATH")
R_SWS_SHARE_PATH = paste0(R_SWS_SHARE_PATH, "/browningj/stock/")
DEBUG_MODE = Sys.getenv("R_DEBUG_MODE")

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
        token = "19d9c8a6-8520-4fd3-95a3-274d84ca061a"
        ## baseUrl = "https://hqlqasws1.hq.un.fao.org:8181/sws",
        ## token = "90bb0f92-e345-4401-945d-1e43af801167"
    )

    ## Source local scripts for this local test
    for(file in dir(apiDirectory, full.names = T))
        source(file)
    if(length(dir(apiDirectory, full.names = T)) == 0)
        stop("No files in apiDirectory.  Are you sure it's the right one?")
}

data = getStockData()
model = buildStockModel(data = data)
currentSession = sessionInfo()
save(model, currentSession,
     file = paste0(R_SWS_SHARE_PATH, "stockModel",
                   gsub("(-| |:)", ".", Sys.time()), ".RData"))