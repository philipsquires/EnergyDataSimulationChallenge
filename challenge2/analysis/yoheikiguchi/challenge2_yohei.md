Data Challenge 2: yoheikiguchi
========================================================


### 1. visualization of the data-set as values per 30mins  ###


```r
library(lattice)
```

```
## Warning: package 'lattice' was built under R version 2.15.3
```

```r
library(plyr)

data = read.table("../../data/total_watt.csv", sep = ",", header = F)

minute <- as.POSIXct(data$V1, origin = "1970-01-01", tz = "GMT")
watt <- data$V2
Data <- data.frame(minute = minute, watt = watt)

getsum <- function(Df) c(watt = sum(Df$watt))

Sums <- ddply(Data, .(minute), getsum)
Sums$day <- cut(Sums$minute, breaks = "day")
Sums$week <- cut(Sums$minute, breaks = "week")
```


xyplot for task 1. 


```r
xyplot(watt ~ minute | day, data = Sums, type = "l", as.table = TRUE, layout = c(7, 
    5), scales = list(x = list(relation = "free", rot = 90)))
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2.png) 


### 2. visualization of the data-set as values per 1 day  ###


```r
daySums <- ddply(Sums, .(day), getsum)
daySums$week <- cut(as.Date(daySums$day), breaks = "week")
```




```r
xyplot(watt ~ day | week, data = daySums, type = "l", scales = list(x = list(relation = "free", 
    rot = 90)))
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4.png) 


### 3. visualization of the data-set as clusters: K-means  ###


```r
km <- kmeans(daySums$watt, 3, iter.max = 10, nstart = 1)
result <- km$cluster
daySums$cluster <- result

# Centroid Plot against 1st 2 discriminant functions
library(cluster)
library(fpc)
```

```
## Loading required package: MASS
```

```
## Loading required package: mclust
```

```
## Package 'mclust' version 4.0
```

```
## Loading required package: flexmix
```



```r
plotcluster(daySums$watt, km$cluster)
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6.png) 



### 3. visualization of the data-set as clusters: gglot 


```r
library(ggplot2)
x = nrow(daySums)

for (i in 1:x) {
    if (as.numeric(daySums$cluster[i]) == 1) {
        daySums$category[i] = "Low"
    } else if (as.numeric(daySums$cluster[i]) == 3) {
        daySums$category[i] = "Medium"
    } else {
        daySums$category[i] = "High"
    }
}
```



```r
p <- ggplot(daySums, aes(day, watt))
p + geom_area(aes(colour = category, fill = category), position = "stack") + 
    opts(axis.text.x = theme_text(angle = 90, hjust = 1))
```

```
## 'opts' is deprecated. Use 'theme' instead. (Deprecated; last used in
## version 0.9.1)
```

```
## theme_text is deprecated. Use 'element_text' instead. (Deprecated; last
## used in version 0.9.1)
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8.png) 

