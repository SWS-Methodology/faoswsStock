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
dlpath <- file.path("C:", "Users", "caetano", "Documents", "Github", "faoswsStock", "sandbox", "fo_licht", "sugar_prod_trade_stocks_fo_lichts2016_06_08.csv")
sugarData <- fread(dlpath)
sugarData[, supply := production + imports - exports + opening_stocks]
sugarData[, utilization := consumption + ending_stocks]
sugarData[, delta_stocks := ending_stocks - opening_stocks]
sugarData[, stocks_percent := ending_stocks/supply]
sugarData[, delta_total_supply := diff(c(NA, supply)),
          by=list(country_group)]
sugarData[, netTrade := imports - exports]

          

## ---- echo=FALSE---------------------------------------------------------
head(sugarData[, c("timePointYears", "country_group", "opening_stocks", "production", "imports", "consumption", "exports", "ending_stocks"), with = F], 3)

## ---- echo=FALSE---------------------------------------------------------
head(sugarData, 3)

## ---- echo=FALSE, fig.height=8-------------------------------------------
options(warn=-1)
plotcolors <- colorRampPalette(c("blue", "black"))

ggplot(data=sugarData,
       aes(x=as.factor(timePointYears), y=netTrade, group=country_group, col = country_group)) +
  geom_line(aes(), stat = "identity", position=position_dodge(), size=1) +
  # facet_wrap(~ itemGroup
  #            , scales = "free"
  # ) +
  scale_color_manual(values = plotcolors(length(unique(sugarData$country_group)))) +
  # theme(strip.text.x = element_text(size = 10, colour = "black", angle = 0)) +
  theme(legend.title=element_blank(), strip.text = element_text(size=7)) +
  geom_hline(yintercept=0, col = "black") +
  theme(legend.title=element_blank(), strip.text = element_text(size=7)) +
  xlab('Year') + ylab('Imports - Exports') +
  ggtitle("Net Trade") +
  #scale_x_continuous(lim=c(2005, 2012), breaks=seq(2005, 2012, 1)) +
theme_bw() +
  theme(
    axis.text.x = element_text(size = 8, face = "bold", angle = 45, vjust = .5),
    legend.position = "top", 
    legend.title=element_blank()
  )


## ---- echo=FALSE, fig.height=8-------------------------------------------
options(warn=-1)

ggplot(data=sugarData,
       aes(x=timePointYears, y=stocks_percent, group=country_group, col = country_group)) +
  geom_line(aes(), stat = "identity", position=position_dodge(), size=1) +
  # facet_wrap(~ countryRegionName, scales = "free") +
  # facet_grid( . ~ countryRegionName, scales = "free") +
  # facet_grid(
  #   country_group ~ .
  #   # , scales = "free"
  # ) +
  scale_y_continuous(lim=c(0, 0.35), breaks=seq(0, 0.35, 0.05), labels = percent) +
  #scale_colour_brewer(palette="Blues") +
  scale_color_manual( values = plotcolors(length(unique(sugarData$country_group)))) +
  # theme(legend.title=element_blank(), strip.text = element_text(size=7)) +
  ylab('% stocks') + xlab('Year') +
  ggtitle("% Stocks by Year") +
  
  #scale_x_continuous(lim=c(2000, 2016), breaks=seq(2000, 2016, 4)) +
  theme_bw() +
  theme(
    axis.text.x = element_text(size = 8, face = "bold", angle = 45, vjust = .5),
    legend.position = "top", 
    legend.title=element_blank()
  )
  

## ---- echo=FALSE, fig.height=4-------------------------------------------
options(warn=-1)
ggplot(data = sugarData,
       aes(x=delta_total_supply, y=delta_stocks, group=1)) +
  geom_point(aes(), stat = "identity", position=position_dodge(), size=1.5) +
  facet_wrap(~ country_group
             #, scales = "free"
             ) +
  theme_bw() +
  theme(legend.title=element_blank(), strip.text = element_text(size=7)) +
  ggtitle("Refined Sugar \n Delta Total Supply vs. Delta Stocks \n (1,000 tonnes, raw value)") +
  ylab('Delta Stocks') + xlab('Delta Total Supply') +
  geom_smooth(method=lm)


## ----include=FALSE, cache=FALSE------------------------------------------
tabCor <- sugarData[, list(corr = 100 * round(cor(delta_stocks, delta_total_supply, use = "pairwise.complete.obs"), 3)),
          by=list(country_group)]

## ---- echo=FALSE---------------------------------------------------------

kable(tabCor, format = "markdown", padding = 0)

## ---- echo=FALSE---------------------------------------------------------
sugarModel <- lm(delta_stocks ~ delta_total_supply + country_group, data = sugarData)

## ---- echo=FALSE---------------------------------------------------------
 
kable(data.frame(sugarModel$coefficients), format = "markdown", padding = 0)

