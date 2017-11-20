Peer-graded Assignment: Course Project 1
============================================


Total number of steps taken per day 

```r
Total_per_day<- tapply(main$steps,main$date,sum)
Total_per_day<- as.data.frame.table(Total_per_day)
colnames(Total_per_day)<- c("Date","Sum")
hist(Total_per_day$Sum, main = "Histogram of Total number of steps taken per day")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png)

```r
mean1<- mean(Total_per_day$Sum)
median<- median(Total_per_day$Sum)
```

The mean of the total steps taken per day is 1.0766189 &times; 10<sup>4</sup>
The median of the total steps taken perday is 10765


Average Daily activity pattern 


```r
library(ggplot2)
step_per_interval<- tapply(main$steps,main$interval,mean)
step_per_interval<- as.data.frame.table(step_per_interval)
colnames(step_per_interval)<- c("Interval", "Average.steps")
ggplot(data = step_per_interval, aes(x = Interval,y = Average.steps, group=1))+geom_line()
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png)

```r
max<-step_per_interval$Interval[(which.max(step_per_interval$Average.steps))]
```
The 5 minute interval across all days which has maximum steps is 835

Imputing Missing Values


```r
main<-read.csv("activity.csv")
main$date<- as.character(main$date)
main$date<- as.Date(main$date)
missing_number<- length(which(is.na(main$steps)))
```
The total number of missing values is 2304

Replacing of missing data by the mean of the existing set

```r
## replacing missing values
missing_data<- which(is.na(main$steps))
missing_vector<- rep(mean(main$steps, na.rm= TRUE), times= length(missing_data))
main[missing_data,"steps"]<- missing_vector
```
Plotting Histogram with NA values replaced by mean values


```r
New_Total_per_day<- tapply(main$steps,main$date,sum)
New_Total_per_day<- as.data.frame.table(New_Total_per_day)
colnames(New_Total_per_day)<- c("Date","Sum")
hist(New_Total_per_day$Sum, main="Histogram of total number of steps taken", xlab = "Total number of steps", ylab = "Frequency" )
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-1.png)


```r
New_mean<- mean(New_Total_per_day$Sum)
New_median<- median(New_Total_per_day$Sum)
Difference_mean<- New_mean-mean1
Difference_median<- New_median-median
```
The difference in mean is 0
The difference in median is 1.1886792

Clearly difference in the mean is zero because missing values have been replaced by mean

Activity difference between weekdays and weekends


```r
main$days<-weekdays(main$date)
temp<- which(main$days=="Saturday"|main$days=="Sunday")
main$weekday<-c("Weekdays")
main$weekday[temp]<-c("Weekends")
main$days<- as.factor(main$days)
main$weekday<- as.factor(main$weekday)
temp_weekdays<- which(main$weekday=="Weekdays")
main_weekdays<- main[temp_weekdays,]
temp_weekdays<- which(main$weekday=="Weekdays")
main_weekdays<- main[temp_weekdays,]
temp_weekends<- which(main$weekday=="Weekends")
main_weekends<- main[temp_weekends,]
library(ggplot2)
main_weekdays$interval<-as.factor(main_weekdays$interval)
main_days<- aggregate(steps ~ weekday+interval,main,mean)
ggplot(main_days,aes(x=interval, y= steps))+geom_line()+facet_grid(weekday~.)
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8-1.png)

