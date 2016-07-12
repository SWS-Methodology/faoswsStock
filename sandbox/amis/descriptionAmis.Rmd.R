---
title: "**Stock Module**"

author: |
  | **Bruno Caetano Vidigal**
  | Food and Agriculture Organization of the United Nations
output: pdf_document
---
---








## Abstract

This vignette provides a detailed analysis of the AMIS data for the stock module.


# 1. AMIS Data

The data below was pulled from the AMIS website by the link **http://statistics.amis-outlook.org**. 
The variables studied are: **Closing Stocks**, **Opening Stocks** and **Total Supply** for all **23** countries present in **AMIS** database.

```
##    dataSource countryRegionName productName    elementName          units
## 1:        CBS         Argentina       Maize Closing Stocks Million tonnes
## 2:        CBS         Argentina       Maize Closing Stocks Million tonnes
## 3:        CBS         Argentina       Maize Closing Stocks Million tonnes
## 4:        CBS         Argentina       Maize Closing Stocks Million tonnes
##       year value
## 1: 2016/17   3.5
## 2: 2015/16   4.0
## 3: 2014/15   4.0
## 4: 2013/14   2.5
```

# 2. Data Manipulation


We computed the **Delta Total Supply**, **Delta Stocks** and the **% of Stocks**

```
##    countryRegionName   productName          units    year closingStocks
## 1:         Argentina         Maize Million tonnes 2000/01          0.92
## 2:         Argentina Rice (milled) Million tonnes 2000/01          0.09
## 3:         Argentina TOTAL CEREALS Million tonnes 2000/01          2.32
## 4:         Argentina         Wheat Million tonnes 2000/01          1.00
##    openingStocks totalSupply deltaTotalSupply deltaStocks percentStocks
## 1:          0.97       17.75                0       -0.05    0.05183099
## 2:          0.18        0.81                0       -0.09    0.11111111
## 3:          1.85       40.14                0        0.47    0.05779771
## 4:          0.50       16.46                0        0.50    0.06075334
##              region    type
## 1: Others countries cereals
## 2: Others countries cereals
## 3: Others countries cereals
## 4: Others countries cereals
```

# 3. Data Visualization

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-1.png)

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7-1.png)

# 4. Correlation




|Region                  |Maize|Rice (milled)|Soybeans|Cereals|Wheat|
|:-----------------------|----:|------------:|-------:|------:|----:|
|European Union          | 58.0|         46.9|    48.8|   88.1| 92.3|
|United States of America| 68.8|         71.5|    62.6|   64.9| 47.5|
|World                   | 76.0|         96.3|    88.6|   89.8| 87.7|

# 5. Model




A simple linear regression was fitted excluding the regions **World** and **China** and **China** **Mainland**. The region was divided in: USA, EU and others countries. The items fitted were **Total** **Cereals** and **Soybeans**.

* Dependent variable: stocks variation
* Independent variable: total supply variation

The **R Square** is **49.6%** 
The coefficients are:


|                              |fit.coefficients|
|:-----------------------------|---------------:|
|(Intercept)                   |      -1.3249028|
|deltaTotalSupply              |       0.2825014|
|regionOthers countries        |       1.0056324|
|regionUnited States of America|       0.4108115|
|typepulses                    |       0.3201229|

