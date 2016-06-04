## This script does analysis  for sugar from FO data.

library(data.table)

dlpath <- file.path("T:", "Team_working_folder", "A", "FBS-Modules", "Stocks", "Sugar", "sugar_data_fo_2014_2015.csv")

sugarData <- fread(dlpath)
sugarData[, .N, item]

sugarData <- dcast.data.table(sugarData, 
                              country + item + unit ~ variable, 
                              value.var = "value")


for(cname in c("consumption", "endingStocks", "exportableProd", "exports", 
               "imports", "initialStocks", "netExports", "netImports", "production")){
  sugarData[get(cname) == "--" | is.na(get(cname)), c(cname) := 0]
}

sapply(sugarData, class)


sugarData[, consumption := as.numeric(consumption)]
sugarData[, endingStocks := as.numeric(endingStocks)]
sugarData[, exportableProd := as.numeric(exportableProd)]
sugarData[, exports := as.numeric(exports)]
sugarData[, imports := as.numeric(imports)]
sugarData[, initialStocks := as.numeric(initialStocks)]
sugarData[, netExports := as.numeric(netExports)]
sugarData[, netImports := as.numeric(netImports)]
sugarData[, production := as.numeric(production)]

sugarData[, totalSupply := initialStocks + production + imports - exports]
sugarData[, totalUtilization := consumption + endingStocks]

# sugarData[, diff := totalSupply - totalUtilization]
# summary(sugarData$diff)

sugarData[, percentStocks := endingStocks/totalSupply]
summary(sugarData$percentStocks)

sugarData <- sugarData[item == "sugar"]


### data between 2010 - 2016

dlpath <- file.path("T:", "Team_working_folder", "A", "FBS-Modules", "Stocks", "Sugar", "sugar_data_eu_2010_2016.csv")

sugar2010To2016 <- fread(dlpath)

for(cname in c("initialStocks", "production", "imports", "consumption", 
               "exports", "endingStocks", "netImports", "netExports", "exportableProd")){
  sugar2010To2016[get(cname) == "--" | is.na(get(cname)), c(cname) := 0]
}

sapply(sugar2010To2016, class)


sugar2010To2016[, production := as.numeric(production)]
sugar2010To2016[, exports := as.numeric(exports)]
sugar2010To2016[, netImports := as.numeric(netImports)]
sugar2010To2016[, netExports := as.numeric(netExports)]
sugar2010To2016[, exportableProd := as.numeric(exportableProd)]


sugar2010To2016[, totalSupply := initialStocks + production + imports - exports]
sugar2010To2016[, totalUtilization := consumption + endingStocks]

sugar2010To2016[, percentStocks := endingStocks/totalSupply]
summary(sugar2010To2016$percentStocks)
