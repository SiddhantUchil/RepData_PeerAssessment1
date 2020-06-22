getwd()
a = read.csv("activity.csv")
str(a)
a$date = as.Date(as.character(a$date, "%d-%m-%Y"))
a$day = day(a$date)
View(a)
a$month = month(a$date)

b = aggregate(a$steps ~ a$day + a$month, data = a, FUN = sum, na.rm = TRUE)
hist(b$`a$steps`, xlim = c, xlab = "steps taken per day", ylab = "frequency", main = "frequency of steps taken eveery day")
c = range(b$`a$steps`)
?axis
axis(1, at = seq(0, 22000, by = 2000), labels = seq(0, 22000, by = 2000))

library(dplyr)
library(tidyr)
b
summary(b)
d = is.na(a)
sum(d)
install.packages(impute)


if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

BiocManager::install("impute")
library(impute)
?impute.knn()



mean((a$steps)[a$interval == 25], na.rm = TRUE)

e  = subset(a, a$day == 2)
mean(e$steps)
e$steps


m = aggregate(a$steps ~ a$interval, data = a, FUN = mean)
aggregate(f$steps ~ f$interval, data = f, FUN = mean)
plot(m$`a$interval`, m$`a$steps`, type = "l")
max(m[,2])
m
length(a$day)



install.packages("Hmisc")
library(Hmisc)
f = a
f$steps = impute(f$steps, fun = mean)
View(f)

m$`a$interval`[m$`a$steps` == max(m$`a$steps`)]


seq = 1 : length(f$steps)
interval1 = 0
mean1 = 0
for (i in seq){
  if (is.na(f$steps[i])){
      interval1 = f$interval[i]
      mean1 = mean(f$steps[f$interval == interval1], na.rm = TRUE)
      f$steps[i] = mean1
      
  }
}

g =subset(f, f$interval == 0)
mean(g$steps)
sum(g$steps)

j = aggregate(f$steps ~ f$day + f$month, data = f, FUN = sum)
k = j
k$`f$day` = as.numeric(k$`f$day`)
str(k)
hist(k$`f$day`)
length(k$`f$day` == 1)
hist(j$`f$steps`)
f$day.num = wday(f$date)

if (f$day.num == 1 || f$day.num == 7) { 
  f$daytype = "weekend" 
  } else{
  f$daytype = "weekday"
  }

f$daytype = ifelse(f$day.num == 1 | f$day.num == 7, "weekend", "weekday")
View(f)
str(f)

f$daytype = factor(f$daytype)


h = aggregate(f$steps ~ f$interval + f$daytype, data = f, FUN = mean)
h


library(ggplot2)

l = ggplot(data = h, mapping = aes(h$`f$interval`, h$`f$steps`))
l + geom_line() + facet_wrap(. ~ h$`f$daytype`)




?axis
max(b$`a$steps`)
