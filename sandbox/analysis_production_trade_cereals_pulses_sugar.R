---
  title: "**Stocks Module**"

author: |
  | **Bruno Caetano Vidigal**
  | Food and Agriculture Organization of the United Nations
output: pdf_document
---
  ---
  
  ```{r include=FALSE, cache=FALSE}
## Load required functions
library(data.table)
library(ggplot2)
library(scales)
library(knitr)
library(devtools)
#install_github("ndphillips/yarrr")
library("yarrr")
library(RColorBrewer)
```

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

```{r include=FALSE, cache=FALSE}
key = DatasetKey(domain = "faostat_one", dataset = "FS1_SUA_UPD", dimensions = list(
  Dimension(name = "geographicAreaFS", keys = GetCodeList("faostat_one", "FS1_SUA_UPD", "geographicAreaFS")[, code]),
  Dimension(name = "measuredElementFS", keys = elements),
  Dimension(name = "timePointYears", keys = as.character(2005:2012)),
  Dimension(name = "measuredItemFS", keys = GetCodeList("faostat_one", "FS1_SUA_UPD", "measuredItemFS")[, code]))
)
data = GetData(key)
```


## Abstract

This vignette provides a simple analysis for Cereals, Pulses and Sugar.


# 1. Data

The data below was pulled from the domain "faostat_one" and dataset "FS1_SUA_UPD". The measuredElementFS are **51** (Production), 
**61** (Imports), **71** (Variation Intial Exstenc) and **91** (Exports).
```{r, echo=FALSE}
head(data, 5)
```

# 2. Data Manipulation
```{r include=FALSE, cache=FALSE}
data[, geographicAreaM49 := fs2m49(as.character(geographicAreaFS))]
data[, measuredItemCPC := fcl2cpc(as.character(formatC(as.numeric(measuredItemFS), width = 4,
                                                       flag = "0")))]
# data = data[!is.na(geographicAreaM49) & 
#               !(flagFaostat %in% c("M", "T"))]

data[, c("geographicAreaFS", "measuredItemFS", "flagFaostat") := NULL]
setcolorder(data, c("geographicAreaM49", "measuredItemCPC", "measuredElementFS", 
                    "timePointYears", "Value"))

# data[grepl("Hen", measuredItemCPC_description)]

data = data[, list(Value = sum(Value)), 
            by = list(geographicAreaM49, measuredItemCPC, 
                      measuredElementFS, timePointYears)]

item <- ReadDatatable("fbs_stocks_comm_codes")
item <- item[fbs_key == "measuredItemCPC", fbs_code]

# Select just cereals, pulses and refined sugar

data <- data[measuredItemCPC %in% item]
data[substr(measuredItemCPC, 1, 3) == "011", group := "cereals"]
data[substr(measuredItemCPC, 1, 3) == "017", group := "pulses"]
data[substr(measuredItemCPC, 1, 3) == "235", group := "refined_sugar"]
data

## More data manipulation 
data <- dcast.data.table(data, geographicAreaM49 + measuredItemCPC + timePointYears + group ~ measuredElementFS,
                         value.var = "Value")

setnames(data, old=c("51", "61", "71", "91"),
         new=c("production", "imports", "variationIntialExstenc", "exports"))

## Checking

for(cname in c("production", "imports", "variationIntialExstenc", "exports")){
  data[is.na(get(cname)), c(cname) := 0]
}

data[, netTrade := imports - exports]

## Country groups
dlpath <- file.path("C:", "Users", "caetano", "Documents", "Github", "faoswsFeed",
                    "data-raw", "IR_factor", "class.csv")

countryGroup <- fread(dlpath)
countryGroup[, .N, GroupName]

countryGroup[grepl("income", GroupName), .N, GroupName]

countryIncomeGroup <- countryGroup[GroupCode %in% c("HIC", "LIC", "UMC", "LMC"), ]
# countryRegionGroup <- countryGroup[GroupCode %in% c("NAC", "LCN", "AFR", "EAS", "EUU"),]

# library(countrycode)
# ?countrycode

countryIncomeGroup[, geographicAreaM49 := as.character(countrycode(CountryCode, "wb", "iso3n"))]

# Sudan has the wrong name (it should be former Sudan)
countryIncomeGroup[geographicAreaM49 == "736", CountryName := "Sudan (former)"]
# China should be 1248
countryIncomeGroup[geographicAreaM49 == "156", geographicAreaM49 := "1248"]
#Exclude Channel Islands and Kosovo (not separately recognised by the UN)
countryIncomeGroup <- countryIncomeGroup[!is.na(geographicAreaM49)]
countryIncomeGroup

# Merge

data <- merge(data, countryIncomeGroup[, c("geographicAreaM49", "GroupName"), with = F], 
              by = "geographicAreaM49", all.x=T)

data <- data[!is.na(geographicAreaM49)]
data <- data[!is.na(GroupName)]

tabGroup <- data[, list(production = sum(production, na.rm = T),
                        imports = sum(imports, na.rm = T),
                        exports = sum(exports, na.rm = T),
                        deltaStocks = sum(variationIntialExstenc, na.rm = T)),
                 by=list(GroupName, group, timePointYears)]

tabGroup <- tabGroup[, trade := imports - exports]


```

# 3. Data Visualization

```{r, echo=FALSE, fig.height=8}
options(warn=-1)
ggplot(data=tabGroup,
       aes(x=as.numeric(timePointYears), y=production, group=GroupName, col = GroupName)) +
  geom_line(aes(), stat = "identity", position=position_dodge(), size=1.5) +
  facet_wrap(~ group
             , scales = "free"
  ) +
  theme(legend.title=element_blank(), strip.text = element_text(size=7)) +
  ylab('Year') + xlab('Production') +
  scale_x_continuous(lim=c(2005, 2012), breaks=seq(2005, 2012, 1)) +
  theme_fao(45)
```

```{r, echo=FALSE, fig.height=4}
options(warn=-1)
ggplot(data=tabGroup,
       aes(x=as.numeric(timePointYears), y=trade, group=GroupName, col = GroupName)) +
  geom_line(aes(), stat = "identity", position=position_dodge(), size=1.5) +
  facet_wrap(~ group
             , scales = "free"
  ) +
  theme(legend.title=element_blank(), strip.text = element_text(size=7)) +
  ylab('Year') + xlab('Imports - Exports') +
  scale_x_continuous(lim=c(2005, 2012), breaks=seq(2005, 2012, 1)) +
  geom_hline(yintercept=0) +
  theme_fao(45)
```