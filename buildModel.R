## load the library
library(faosws)
library(data.table)
library(ggplot2)

## set up for the test environment and parameters
## R_SWS_SHARE_PATH = Sys.getenv("R_SWS_SHARE_PATH")
## DEBUG_MODE = Sys.getenv("R_DEBUG_MODE")

if(CheckDebug()){
    cat("Not on server, so setting up environment...\n")
    
  ## Define directories
  if(Sys.info()[7] == "josh"){
    apiDirectory = "~/Documents/Github/faoswsStock/R/"
    R_SWS_SHARE_PATH = "/media/hqlprsws2_prod/"
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
    #baseUrl = "https://hqlprswsas1.hq.un.fao.org:8181/sws",
    #token = "19d9c8a6-8520-4fd3-95a3-274d84ca061a"
    baseUrl = "https://hqlqasws1.hq.un.fao.org:8181/sws",
    token = "90bb0f92-e345-4401-945d-1e43af801167"
  )

    ## Source local scripts for this local test
    for(file in dir(apiDirectory, full.names = T))
        source(file)
    if(length(dir(apiDirectory, full.names = T)) == 0)
        stop("No files in apiDirectory.  Are you sure it's the right one?")
}

data = getStockData()
model = buildStockModel(data = data, cumulativeYears = 10)
currentSession = sessionInfo()
save(model, currentSession,
     file = paste0(R_SWS_SHARE_PATH, "stockModel",
                   gsub("(-| |:)", ".", Sys.time()), ".RData"))