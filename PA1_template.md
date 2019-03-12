---
title: "Reproducible Research"
output: 
  html_document: 
    keep_md: yes
    toc: yes
---

## Getting Packages and Data


```r
library("data.table")
```

```
## Warning: package 'data.table' was built under R version 3.4.4
```

```r
library(ggplot2)
```

```
## Warning: package 'ggplot2' was built under R version 3.4.4
```

```r
library(dplyr)
```

```
## Warning: package 'dplyr' was built under R version 3.4.4
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:data.table':
## 
##     between, first, last
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(fileUrl, destfile = paste0(getwd(), '/repdata%2Fdata%2Factivity.zip'))
unzip("repdata%2Fdata%2Factivity.zip",exdir = "data")
```

##Reading Data


```r
activityDT <- data.table::fread(input = "data/activity.csv")
activityDT<- tbl_df(activityDT)
```

## What is the mean total steps taken by day


```r
activityDay <- activityDT%>%group_by(date)
Total_steps <- summarise(activityDay, Steps = sum(steps,na.rm = TRUE))
head(Total_steps)
```

```
## # A tibble: 6 x 2
##   date       Steps
##   <chr>      <int>
## 1 2012-10-01     0
## 2 2012-10-02   126
## 3 2012-10-03 11352
## 4 2012-10-04 12116
## 5 2012-10-05 13294
## 6 2012-10-06 15420
```

```r
hist(Total_steps$Steps, main = "Total Steps by day", xlab = "Sum of steps by day")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```r
mean_steps <- mean(Total_steps$Steps)
print(mean_steps)
```

```
## [1] 9354.23
```

```r
median_steps <- median(Total_steps$Steps)
print(median_steps)
```

```
## [1] 10395
```

##What is the average daily activity pattern?

```r
activityInterval <- activityDT%>%
  group_by(interval)%>%
  summarise(Steps = mean(steps, na.rm = TRUE))
ggplot(activityInterval, aes(x= interval, y = Steps))+
  theme_bw()+
  labs(title = "Average daily steps")+
  geom_line(color = "red", size = 1)
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->


```r
activityInterval[which.max(activityInterval$Steps), ]
```

```
## # A tibble: 1 x 2
##   interval Steps
##      <int> <dbl>
## 1      835  206.
```

##Imputing missing values

```r
table(is.na(activityDT))
```

```
## 
## FALSE  TRUE 
## 50400  2304
```


```r
imput.activity <- activityDT%>%
  mutate(steps.imputed = ifelse(is.na(activityDT$steps) == TRUE, mean(activityDT$steps, na.rm = TRUE), activityDT$steps))%>%
  group_by(date)%>%
  summarise(steps.imputed = sum(steps.imputed))
```

```
## Warning: package 'bindrcpp' was built under R version 3.4.4
```

```r
hist(imput.activity$steps.imputed, xlab = "Total steps", main = "Histogram Total Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

```r
data.frame(type.data =c("Original", "Imputed"),
           median.activity = c(median(Total_steps$Steps, na.rm = TRUE),median(imput.activity$steps.imputed)),
           mean.activity = c(mean(Total_steps$Steps, na.rm = TRUE)), mean(imput.activity$steps.imputed))
```

```
##   type.data median.activity mean.activity
## 1  Original        10395.00       9354.23
## 2   Imputed        10766.19       9354.23
##   mean.imput.activity.steps.imputed.
## 1                           10766.19
## 2                           10766.19
```
##Are there differences in activity patterns between weekdays and weekends?

```r
activityDT$date <- as.Date(activityDT$date, format = "%Y-%m-%d")
wt <- weekdays(activityDT$date)
activityDT$typeday <- as.factor(ifelse(wt %in% c("samedi", "dimanche"), "weekend", "weekday" ))
head(activityDT)
```

```
## # A tibble: 6 x 4
##   steps date       interval typeday
##   <int> <date>        <int> <fct>  
## 1    NA 2012-10-01        0 weekday
## 2    NA 2012-10-01        5 weekday
## 3    NA 2012-10-01       10 weekday
## 4    NA 2012-10-01       15 weekday
## 5    NA 2012-10-01       20 weekday
## 6    NA 2012-10-01       25 weekday
```


```r
activity_w <- activityDT%>%
  group_by(interval,typeday)%>%
  summarise(Steps = mean(steps, na.rm = TRUE))

ggplot(activity_w, aes(x= interval, y = Steps, colour =typeday))+
  theme_bw()+
  facet_grid(typeday~.)+
  geom_line(size =0.7)
```

![](PA1_template_files/figure-html/unnamed-chunk-13-1.png)<!-- -->

