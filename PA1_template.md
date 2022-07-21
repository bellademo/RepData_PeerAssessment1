---
title: "PA1_template"
output:
  html_document:
    keep_md: TRUE
date: '2022-07-21'
---

**Loading and Processing the Data**

``` r
library(knitr)
library(data.table)
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:data.table':
    ## 
    ##     between, first, last

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(ggplot2)

activity <- read.csv("/Users/isabellademo/Desktop/Hopkins/Data Coursera/RepData_PeerAssessment1/activity.csv",
         header = TRUE)

activity$date <- as.Date(activity$date, "%Y-%m-%d")
```

**What is the mean total number of steps taken per day?**\
1. Calculate total number of steps taken per day

``` r
step_sum <- aggregate(activity["steps"], by=activity["date"], sum)
```

2.  Make a histogram of the total number of steps taken each day.

``` r
ggplot(step_sum, aes(x = steps)) +
  geom_histogram(fill = "red", binwidth = 1000) +
  labs(title = "Total Number of Steps per Day", x = "Number of    Steps", y = "Frequency of Days")
```
![000014](https://user-images.githubusercontent.com/103701074/180260615-a8488bc4-8d05-400b-9aec-a08cd192256a.png)


![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

3.  Calculate and report the mean and median of the total number of steps per day.

``` r
mean(step_sum$steps, na.rm = TRUE)
```

    ## [1] 10766.19

``` r
median(step_sum$steps, na.rm = TRUE)
```

    ## [1] 10765

**What is the average daily activity pattern?**\
1. Make a time serires plot of the 5- minute interval and average number of steps taken.

``` r
avg_step <- aggregate(steps ~ interval, data = activity, mean, na.rm = TRUE)

plot(avg_step$interval, avg_step$steps, type = "l", lwd = 2,
     main = "Average Number of Steps Taken Across All Days",
     xlab = "5-minute interval", ylab = "Average number of steps")
```
![000013](https://user-images.githubusercontent.com/103701074/180260720-aa2c5454-766c-4f98-88e7-ea351b110f56.png)

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

``` r
avg_step$interval[which.max(avg_step$steps)]
```

    ## [1] 835

2.  Which 5-minute interval contains the maximum number of steps?

``` r
avg_step$interval[which.max(avg_step$steps)]
```

    ## [1] 835

**Imputing Missing Values**\
1. Calculate and report total number of missing values.

``` r
sum(is.na(activity))
```

    ## [1] 2304

2 and 3. Devise a strategy for filling in missing values. Create a new dataset that is equal to the original with missing data filled in.

``` r
full_df <- activity
full_df[is.na(full_df)] = avg_step$steps
```

4.  Make a histogram of the total number of steps taken each day. Calculate and report the mean and median total number of steps taken per day.

``` r
total_steps_full <- aggregate(steps ~ date, data = full_df, sum, na.rm = TRUE)
hist(total_steps_full$steps, breaks = 20, 
     main = "Total Number of Steps w/o NAs",
     xlab = "Steps", ylab = "Frequency")
```
![000010](https://user-images.githubusercontent.com/103701074/180260766-c7272c96-64fc-4996-8781-b42390dbcd62.png)

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

``` r
mean(total_steps_full$steps)
```

    ## [1] 10766.19

``` r
median(total_steps_full$steps)
```

    ## [1] 10766.19

1.  Create a new factor variable in the dataset with two levels--"weekday and "weekend"

``` r
full_df$day <- weekdays(full_df$date)
full_df$week <- weekdays(full_df$date)
full_df[full_df$day == "Saturday" | full_df$day == "Sunday",]$week <- "weekend"
full_df[!full_df$day == "Saturday" | full_df$day == "Sunday",]$week <- "weekday"
full_df$week <- as.factor(full_df$week)
```

2.  Make a panel plot containing a time series plot of the 5-minute interval and the average number of steps taken across all weekdays or weekends.

``` r
library(ggplot2)
last_df <- aggregate(steps ~ interval + week, data = full_df, mean)
ggplot(last_df, aes(x = interval , y = steps, color= week)) + 
  geom_line() + labs(title = "Average Steps Across Weekdays/Weekends", 
                     x = "Interval", y = "Number of Steps") +
  facet_wrap(~ week , ncol = 1, nrow=2)
```
![000017](https://user-images.githubusercontent.com/103701074/180260829-eff6d552-01c6-43cc-9c7c-ceda857a3427.png)

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png)<!-- -->
