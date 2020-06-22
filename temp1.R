getwd()
a = read.csv("activity.csv")
str(a)
a$date = as.Date(as.character(a$date, "%d-%m-%Y"))
a$day = day(a$date)
View(a)
a$month = month(a$date)

b = aggregate(a$steps ~ a$day + a$month, data = a, FUN = sum, na.rm = TRUE)
hist(b$`a$steps`, xlim = c, right = TRUE)
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

m[m$`a$interval` == max(m$`a$steps`), m$`a$interval`]


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








