Weather Events Effect on Public Health and Economy in U.S.
========================================================

## Synopsis
There're a large variety of natural disasters across America, which can cause serious public health problem and economic issue at both community level and muncipacity level. This report analyzes the relevants storm data from U.S. National Oceanic and Atmospheric Administration's (NOAA) during the time range 1950-2011. It concludes that the hurrican disaster can cause the most serious public health problem, leading the most injuries and fatalities. The major weather event that results in the economic issue is the flood, and the hurrican comes second.

### Data Processing

```r
rawData <- read.table("repdata_data_StormData.csv.bz2", header = T, sep = ",")
names(rawData)
```

```
##  [1] "STATE__"    "BGN_DATE"   "BGN_TIME"   "TIME_ZONE"  "COUNTY"    
##  [6] "COUNTYNAME" "STATE"      "EVTYPE"     "BGN_RANGE"  "BGN_AZI"   
## [11] "BGN_LOCATI" "END_DATE"   "END_TIME"   "COUNTY_END" "COUNTYENDN"
## [16] "END_RANGE"  "END_AZI"    "END_LOCATI" "LENGTH"     "WIDTH"     
## [21] "F"          "MAG"        "FATALITIES" "INJURIES"   "PROPDMG"   
## [26] "PROPDMGEXP" "CROPDMG"    "CROPDMGEXP" "WFO"        "STATEOFFIC"
## [31] "ZONENAMES"  "LATITUDE"   "LONGITUDE"  "LATITUDE_E" "LONGITUDE_"
## [36] "REMARKS"    "REFNUM"
```



```r
dataMessy <- rawData[, c("EVTYPE", "FATALITIES", "INJURIES", "PROPDMG", "PROPDMGEXP", 
    "CROPDMG", "CROPDMGEXP")]
dataMessy <- subset(dataMessy, INJURIES > 0 | FATALITIES > 0 | PROPDMG > 0 | 
    CROPDMG > 0)
strs <- c(".*tide.*", ".*avalanche.*", ".*blizzard.*", ".*flood.*", ".*chill.*", 
    ".*drought.*", ".*microburst.*", ".*dust.*", ".*heat.*", ".*cold.*", ".*fog.*", 
    ".*rain.*", ".*freeze.*", ".*cloud.*", ".*hail.*", ".*snow.*", ".*sleet.*", 
    ".*surf.*", ".*wind.*", ".*hurricane.*", ".*ice.*", ".*land.*", ".*lightning.*", 
    ".*marine.*", ".*thunderstorm.*", ".*tornado.*", ".*wint.*", ".*fire.*", 
    ".*water.*", ".*stream.*", ".*warm.*", ".*storm.*", ".*current.*", ".*mud.*")

index <- list()
for (i in seq_along(strs)) {
    index[[i]] <- grepl(strs[i], tolower(dataMessy$EVTYPE))
}

indexx <- rep(0, length(dataMessy$EVTYPE))
for (i in 1:length(index)) {
    indexx = indexx + index[[i]]
}

dataMessy <- dataMessy[as.logical(indexx), ]

temp <- gsub(strs[1], "Marine", tolower(dataMessy$EVTYPE))
temp <- gsub(strs[2], "Winter", temp)
temp <- gsub(strs[3], "Winter", temp)
temp <- gsub(strs[4], "Flood", temp)
temp <- gsub(strs[5], "Winter", temp)
temp <- gsub(strs[6], "Dry", temp)
temp <- gsub(strs[7], "Dry", temp)
temp <- gsub(strs[8], "Dry", temp)
temp <- gsub(strs[9], "Extreme_temp", temp)
temp <- gsub(strs[10], "Extreme_temp", temp)
temp <- gsub(strs[11], "Other", temp)
temp <- gsub(strs[12], "Other", temp)
temp <- gsub(strs[13], "Extreme_temp", temp)
temp <- gsub(strs[14], "Other", temp)
temp <- gsub(strs[15], "Winter", temp)
temp <- gsub(strs[16], "Winter", temp)
temp <- gsub(strs[17], "Winter", temp)
temp <- gsub(strs[18], "Marine", temp)
temp <- gsub(strs[19], "Other", temp)
temp <- gsub(strs[20], "Hurricane", temp)
temp <- gsub(strs[21], "Winter", temp)
temp <- gsub(strs[22], "Other", temp)
temp <- gsub(strs[23], "Lightning", temp)
temp <- gsub(strs[24], "Marine", temp)
temp <- gsub(strs[25], "Storm", temp)
temp <- gsub(strs[26], "Hurricane", temp)
temp <- gsub(strs[27], "Winter", temp)
temp <- gsub(strs[28], "Other", temp)
temp <- gsub(strs[29], "Flood", temp)
temp <- gsub(strs[30], "Flood", temp)
temp <- gsub(strs[31], "Other", temp)
temp <- gsub(strs[32], "Storm", temp)
temp <- gsub(strs[33], "Marine", temp)
temp <- gsub(strs[34], "Other", temp)
dataMessy$EVTYPE <- temp
```

In order to expedite the furter data analysis, only relevant column attributes are selected. For observations, those causing zero public health ("FATALITIES" and "INJURIES") and economy ("PROPDMG", wheter or not including "CROPDMG" is an open end question. It's not disscussed in this report) are deleted. The event type attribute is very messy. I delete those that's hard to understand. For the keeped ones, I grouped them into 9 catagories (Dry, Extrem temperature, Flood, Hurrican, Lightning, Marine, Storm, Winter, and others). All of those steps are to get a relatively neat data sets to further the analysis.



```r
dataMessyHealth <- dataMessy[, c("EVTYPE", "FATALITIES", "INJURIES")]
table(dataMessy$PROPDMGEXP)
```

```
## 
##             -      ?      +      0      1      2      3      4      5 
##  11491      1      0      5    210      0      1      1      4     18 
##      6      7      8      B      h      H      K      m      M 
##      3      3      0     40      1      6 231283      7  11303
```

```r
table(dataMessy$CROPDMGEXP)
```

```
## 
##             ?      0      2      B      k      K      m      M 
## 152485      6     17      0      7     21  99862      1   1978
```

```r
dataMessyEconomy <- subset(dataMessy, PROPDMGEXP %in% c(" ", "m", "M", "k", 
    "K", "B", "b") & CROPDMGEXP %in% c("", "m", "M", "k", "K", "B", "b"))[, 
    c("EVTYPE", "PROPDMG", "PROPDMGEXP", "CROPDMG", "CROPDMGEXP")]
dim(dataMessyHealth)
```

```
## [1] 254377      3
```

```r
dim(dataMessyEconomy)
```

```
## [1] 242612      5
```

```r
temp <- tolower(dataMessyEconomy$PROPDMGEXP)
temp <- gsub(" ", 10^0, temp)
temp <- gsub("k", 10^3, temp)
temp <- gsub("m", 10^6, temp)
temp <- gsub("b", 10^9, temp)
dataMessyEconomy$PROPDMGEXP <- temp

temp <- tolower(dataMessyEconomy$CROPDMGEXP)
temp <- gsub("", 10^0, temp)
temp <- gsub("k", 10^3, temp)
temp <- gsub("m", 10^6, temp)
temp <- gsub("b", 10^9, temp)
dataMessyEconomy$CROPDMGEXP <- temp
```

Create two new sets of data, dataMessyEconomy and dataMessyHealth for discussing two questions (one for public health; one for economy issue). For PROPDMGEXP and CROPDMGEXP attributes, only letters 'k', 'K', 'm', 'M', 'b', 'b' , ' ' keeps. others have small portions, and are discarded due to ambiguous interpretation.


### Results

```r

library("plyr")
dataNeatHealth <- ddply(dataMessyHealth, .(EVTYPE), summarize, Health = sum(FATALITIES + 
    INJURIES))
dataNeatEconomy <- ddply(dataMessyEconomy, .(EVTYPE), summarize, Economy = sum(PROPDMG * 
    as.numeric(PROPDMGEXP) + 0 * CROPDMG * as.numeric(CROPDMGEXP)))
```



```r
library("ggplot2")

ggplot(dataNeatHealth, aes(EVTYPE, Health), fill = EVTYPE) + geom_bar(position = "dodge", 
    stat = "identity") + labs(title = "PUBLIC HEALTH")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-51.png) 

```r

ggplot(dataNeatEconomy, aes(EVTYPE, Economy)) + geom_bar(stat = "identity") + 
    labs(title = "ECONOMY ISSUE")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-52.png) 

From the plot "PUBBLIC HEALTH", The Hurricane disaster dominate the reason resulting the people injuries and fatalites, and Extreme_temp, Flood, Lightning, Winter and others comes second.
From the plot "ECONOMY ISSUE", The Flood and Hurricane are two natural disasters which cause american serious economy problem, and it seems the Flood is more severe.

