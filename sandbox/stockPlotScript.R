library(data.table)
library(ggplot2)
library(magrittr)
library(zoo)

d = fread("~/Documents/Github/faoswsStock/tests/Data/usaStockVariabilityFAOSTAT.csv")
setwd("~/Documents/Github/faoswsStock/tests/Results/")

## Verify ItemCode and Year partition the data:
d[, .N, c("ItemCode", "Year")][, .N, N]

## Plot cumulative sums against next value
coeff = vector("list", 20)
for(windowLength in 1:20){
    d[, pastStockSum := c(rep(NA, windowLength - 1),
                          rollsum(Value, k = windowLength)),
       by = ItemCode]
    ## pastStockSum currently includes current line.  Must bump back one.
    d[, pastStockSum := c(NA, pastStockSum[-length(pastStockSum)]),
       by = ItemCode]
    ggsave(paste0("Stock_plots_summed_lag_", windowLength, ".png"),
        ggplot(d, aes(x = pastStockSum, y = Value)) + geom_point() +
            facet_wrap( ~ ItemName, scale = "free") +
            geom_smooth(method = "lm", formula = y ~ x + 0),
        width = 16, height = 16)
    coeff[[windowLength]] = d[, lm(Value ~ pastStockSum + 0)$coeff[1],
                               by = "ItemName"]
    coeff[[windowLength]]$windowLength = windowLength
}

windowLength = 15
wheatItems = c("Wheat and products", "Maize and products",
               "Cereals - Excluding Beer", "Cereals, Other",
               "Barley and products")
d[, pastStockSum := c(rep(NA, windowLength - 1),
                      rollsum(Value, k = windowLength)),
   by = ItemCode]
## pastStockSum currently includes current line.  Must bump back one.
d[, pastStockSum := c(NA, pastStockSum[-length(pastStockSum)]),
   by = ItemCode]
ggsave(paste0("Stock_cereal_summed_lag_", windowLength, ".png"),
    ggplot(d[ItemName %in% wheatItems, ], aes(x = pastStockSum, y = Value)) +
        geom_point() + facet_wrap( ~ ItemName, scale = "free") +
        geom_smooth(method = "lm", formula = y ~ x + 0) +
        labs(x = "Sum of previous 15 stock changes",
             y = "Current stock change"),
    width = 8, height = 8)


coeff = do.call("rbind", coeff)
setnames(coeff, "V1", "slope")
ggsave("Slope_Distribution.png",
    ggplot(coeff, aes(x = windowLength, y = slope, group = windowLength)) +
        geom_boxplot() + labs(y = "Distribution of fitted slopes"),
    width = 8, height = 6)
