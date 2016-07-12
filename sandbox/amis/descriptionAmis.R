## ----include=FALSE, cache=FALSE------------------------------------------
## Load required functions
library(data.table)
library(ggplot2)
library(scales)
library(knitr)
library(devtools)
#install_github("ndphillips/yarrr")
library("yarrr")
library(RColorBrewer)

## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ----include=FALSE, cache=FALSE------------------------------------------
data4Items = fread("C:\\Users\\caetano\\Desktop\\stocksModule\\amis\\riceWheatSoybeansMaizeStocks.csv")

## ---- echo=FALSE---------------------------------------------------------
head(data4Items, 4)

## ----include=FALSE, cache=FALSE------------------------------------------
data4Items <- dcast.data.table(data4Items, 
                             countryRegionName + productName + units + year ~ elementName,
                             value.var = "value")

setnames(data4Items, old = c("Closing Stocks", "Opening Stocks", "Total Supply"),
         new = c("closingStocks", "openingStocks", "totalSupply"))

setkey(data4Items, countryRegionName, year)
data4Items[, deltaTotalSupply := c(0, diff(totalSupply)),
         by = list(countryRegionName, productName)]

data4Items[, deltaStocks := closingStocks - openingStocks]
data4Items[, percentStocks := closingStocks/totalSupply]

data4Items[countryRegionName == "United States of America", region := "United States of America"]
data4Items[countryRegionName == "European Union", region := "European Union"]
data4Items[!(countryRegionName %in% c("European Union", "United States of America")), region := "Others countries"]

data4Items[productName == "Soybeans", type := "pulses"]
data4Items[productName != "Soybeans", type := "cereals"]

## ---- echo=FALSE---------------------------------------------------------
head(data4Items, 4)

## ---- echo=FALSE, fig.height=8-------------------------------------------
options(warn=-1)
data4Items[, yearNumeric := as.numeric(substr(year, 1, 4))]

plotcolors <- colorRampPalette(c("gray70", "blue", "green", "black"))

ggplot(data=data4Items[countryRegionName %in% c("World", "European Union", "United States of America")],
       aes(x=yearNumeric, y=percentStocks, group=productName, col = productName)) +
  geom_line(aes(), stat = "identity", position=position_dodge(), size=1) +
  # facet_wrap(~ countryRegionName, scales = "free") +
  # facet_grid( . ~ countryRegionName, scales = "free") +
  facet_grid(
    countryRegionName ~ .
    # , scales = "free"
    ) +
  scale_y_continuous(labels = scales::percent) +
  # scale_colour_brewer(palette="Blues") +
  scale_color_manual( values = plotcolors(length(unique(data4Items$productName))) ) +
  theme(legend.title=element_blank(), strip.text = element_text(size=7)) +
  ylab('% stocks') + xlab('') +
  ggtitle("% Stocks by Year") +
  scale_x_continuous(lim=c(2000, 2016), breaks=seq(2000, 2016, 4)) +
  theme_bw() +
  theme(
    axis.text.x = element_text(size = 8, face = "bold", angle = 45, vjust = .5),
    legend.position = "top"
  )
  

## ---- echo=FALSE, fig.height=4-------------------------------------------
options(warn=-1)
ggplot(data = data4Items[countryRegionName %in% c("World", "European Union", "United States of America") & productName == "TOTAL CEREALS"],
       aes(x=deltaTotalSupply, y=deltaStocks, group=1)) +
  geom_point(aes(), stat = "identity", position=position_dodge(), size=1.5) +
  facet_wrap(~ countryRegionName, scales = "free") +
  theme_bw() +
  theme(legend.title=element_blank(), strip.text = element_text(size=7)) +
  ggtitle("Total Cereals \n Delta Total Supply vs. Delta Stocks") +
  ylab('Delta Stocks (Million tonnes)') + xlab('Delta Total Supply (Million tonnes)') +
  geom_smooth(method=lm)

## ----include=FALSE, cache=FALSE------------------------------------------
tabCor = data4Items[, list(
  corDeltaStocksDeltaTotalSupply = 100 * round(cor(deltaStocks, deltaTotalSupply,
                                       use = "pairwise.complete.obs"), 3)),
                                       by = list(countryRegionName, productName)]

## ---- echo=FALSE---------------------------------------------------------

kable(dcast.data.table(tabCor[countryRegionName %in% c("World", "European Union", "United States of America")],
countryRegionName ~ productName, value.var = "corDeltaStocksDeltaTotalSupply"), format = "markdown", padding = 0,
      col.names=c("Region", "Maize", "Rice (milled)", "Soybeans", "Cereals", "Wheat"))

## ---- echo=FALSE---------------------------------------------------------
fit <- lm(deltaStocks ~ deltaTotalSupply + region + type, 
          data = data4Items[productName %in% c("TOTAL CEREALS", "Soybeans") & 
                              !countryRegionName %in% c("World", "China", "China Mainland")])

## ---- echo=FALSE---------------------------------------------------------
 
kable(data.frame(fit$coefficients), format = "markdown", padding = 0)

