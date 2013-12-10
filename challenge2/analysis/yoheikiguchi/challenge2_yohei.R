library(lattice)
library(plyr)

data = read.table("../../data/total_watt.csv", sep="," , header =F)

minute <- as.POSIXct(data$V1, origin = "1970-01-01", tz="GMT") 
watt <- data$V2
Data  <- data.frame(minute = minute, watt = watt)

getsum  <- function(Df) c(watt = sum(Df$watt))

Sums <- ddply(Data, .(minute), getsum) 
Sums$day <- cut(Sums$minute, breaks = "day")
Sums$week <- cut(Sums$minute, breaks = "week")


### 1. visualization of the data-set as values per 30mins  ###

xyplot(watt ~ minute | day, data = Sums, type = "l",
       as.table = TRUE, layout = c(7,5),
       scales = list(x = list(relation = "free", rot = 90)))

### 2. visualization of the data-set as values per 1 day  ###

daySums <- ddply(Sums, .(day), getsum) 
daySums$week <- cut(as.Date(daySums$day), breaks = "week")



xyplot(watt ~ day | week, data = daySums, type = "l",
       scales = list(x = list(relation = "free", rot = 90)))

### 3. visualization of the data-set as clusters  ###

km <- kmeans(daySums$watt, 3, iter.max = 10, nstart=1)
result <- km$cluster
daySums$cluster <-result

# Centroid Plot against 1st 2 discriminant functions
library(cluster) 
library(fpc)

plotcluster(daySums$watt, km$cluster)

# gglot Area chart
library(ggplot2)
x = nrow(daySums)
  
for(i in 1:x){
  if(as.numeric(daySums$cluster[i]) == 1){
    daySums$category[i] = "Low"
  }
  else if(as.numeric(daySums$cluster[i]) == 3){
    daySums$category[i] = "Medium"
  }
  else{
    daySums$category[i] = "High"
  }
}

if (Sys.info()['sysname'] == "Darwin") { ## Open new window
  quartz()
}

p <- ggplot(daySums, aes( day, watt))
p + geom_area(aes(colour = category, fill= category), position = 'stack') + opts(axis.text.x=theme_text(angle=90, hjust=1))

