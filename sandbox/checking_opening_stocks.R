
## load the library
library(faosws)
library(data.table)
library(ggplot2)
library(faoswsUtil)

## set up for the test environment and parameters
## R_SWS_SHARE_PATH = Sys.getenv("R_SWS_SHARE_PATH")
## R_SWS_SHARE_PATH = paste0(R_SWS_SHARE_PATH, "/browningj/stock/")
## DEBUG_MODE = Sys.getenv("R_DEBUG_MODE")
initialYear = 1961 # Change documentation if this changes!

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
    token = "1e9d1161-9d0a-4151-8dc1-e33a95ef81f2"
    #token = "b4995d52-cc4c-4e21-a6d8-de90b1f42e63"
    ## baseUrl = "https://hqlprswsas1.hq.un.fao.org:8181/sws",
    ## token = "90c00557-e740-46f4-8cd7-6c90e6d96124"
  )
  
  ## Source local scripts for this local test
  for(file in dir(apiDirectory, full.names = T))
    source(file)
  if(length(dir(apiDirectory, full.names = T)) == 0)
    stop("No files in apiDirectory.  Are you sure it's the right one?")
  
}

## opening stocks

## Using the new data on SWS instead of oldSuaData
stockCode <- "5071"
openingStock <- "5113"
prodCode <- "5510"
codes <- c(stockCode, openingStock, prodCode) 
# item <- c("0111", "0112", "0114", "0115",
#           "0116", "0117")
item <- ReadDatatable("fbs_stocks_comm_codes")[fbs_key == "measuredItemCPC", 
                                               fbs_code]
key = DatasetKey(
  domain = "agriculture",
  dataset = "aproduction",
  dimensions = list(
    Dimension(
      name = "geographicAreaM49",
      keys = GetCodeList("agriculture", "aproduction", "geographicAreaM49")[, code]
    ),
    Dimension(name = "measuredElement", keys = codes),
    Dimension(name = "timePointYears", keys = as.character(1961:1999)),
    Dimension(
      name = "measuredItemCPC",
      keys = GetCodeList("agriculture", "aproduction", "measuredItemCPC")[code %in% item, code]
    )
  )
)

newSuaData <- GetData(key)

newSuaData[geographicAreaM49 == 100 & timePointYears %in% c("1993", "1994", "1995") &
             measuredItemCPC == "0111" & measuredElement %in% c("5071", "5113")]

newSuaData <- dcast.data.table(
  newSuaData, geographicAreaM49 + measuredItemCPC + timePointYears ~ measuredElement,
  value.var = "Value")

setnames(newSuaData, 
         old = c("5071", "5113", "5510"), 
         new = c("deltaStocks", "openingStocks", "production"))

for(cname in c("deltaStocks", "openingStocks", "production")){
  newSuaData[is.na(get(cname)), c(cname) := 0]
}

setkey(newSuaData, geographicAreaM49, measuredItemCPC, timePointYears)


newSuaData[, absStocks := cumsum(deltaStocks),
           by=list(geographicAreaM49, measuredItemCPC)]

newSuaData[, desltaStocks2 := deltaStocks * (-1)]


newSuaData[, absStocks2 := cumsum(desltaStocks2),
           by=list(geographicAreaM49, measuredItemCPC)]


newSuaData <- nameData("agriculture", "aproduction", newSuaData)

### 

summaryTable <- newSuaData[timePointYears >= 1981 & timePointYears <= 1998 & absStocks2 < 0, 
           list(numberNegativeFigures = length(unique(timePointYears))), 
           by=list(geographicAreaM49_description, measuredItemCPC_description)]


summaryTable <- summaryTable[order(-numberNegativeFigures)] 
summaryTable

## let's take the opening stocks and the production for these countries/commodities in 1999
country <- unique(summaryTable$geographicAreaM49_description)
item <- unique(summaryTable$measuredItemCPC_description)

year1999 <- newSuaData[geographicAreaM49_description %in% country & 
             measuredItemCPC_description %in% item & timePointYears == 1999]

# merge summaryTable and year1999


key = c("geographicAreaM49_description", "measuredItemCPC_description")

summaryTable <- merge(summaryTable, year1999[, 
                                             c("geographicAreaM49_description", 
                                               "measuredItemCPC_description", 
                                               "production", "absStocks2"), with = F],
      by = key, all.x = T)

setnames(summaryTable, "geographicAreaM49_description", "country")

setnames(summaryTable, "measuredItemCPC_description", "item")

setnames(summaryTable, "absStocks2", "opening_stocks")

write.xlsx(summaryTable, "C:/Users/caetano/Documents/summaryTable.xlsx", row.names = F)
