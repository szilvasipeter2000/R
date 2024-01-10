x <- seq(0,20,by= 0.01)
plot(x,sin(x),type = 'l')

?pie
pie(c(Sky = 78, "Sunny side of pyramid" = 17, "Shady side of pyramid" = 5),
    init.angle = 315, col = c("deepskyblue", "yellow", "yellow3"), border = FALSE)
?curve
## one liner
curve(sin,from = 0, to = pi*2)
curve(cos,from = 0, to = pi*2,add = TRUE,col = 'red')

## TODO
## brownian motion / random walk
## 100 iteration
x <- 0
for(i in 1:100) {
  if (runif(1) < 0.5) {
    x <- x+1
  } else
  x <- x - 1
}
x

## vectorize
set.seed(42)
cumsum(round(runif(100))*2 - 1)

h <- c(174,170,160)
w <- c(90,80,70)
min(w)
max(h)
diff(range(w))
mean(h)
median(w)
summary(w)
cor(w,h)
lm(w ~ h)
-146.154 +165*1.346 
fit <- lm(w~h)
str(fit)
summary(fit)
predict(fit,newdata=list(h=165))
plot(h,w)
abline(fit,col = "red")

df <- data.frame(weight=w,height=h)
df$weight[1]
df[1]
df[1,1]
nrow(df)
ncol(df)
plot(df)
cor(df)
df$bmi <- df$weight / (df$height/100) ^ 2
df <- read.csv("http://bit.ly/CEU-R-heights")

df$weight <- df$weightLb * 0.453592
df$height <- df$heightIn * 2.54
df$bmi <- df$weight / (df$height/100) ^ 2
df$weightLb <- df$heightIn <- NULL
plot(df)

## install.packages("pairsD3")
library(pairsD3)
pairsD3::pairsD3(df)

install.packages("GGally")
library(GGally)
ggpairs(df)
library(ggplot2)
ggplot(df, aes(x = height)) + geom_histogram()

system.time(g <- ggplot(df, aes(x = height,y=weight,color = sex))+
              geom_point())
g+theme_light()
g+geom_smooth(method="lm",se = FALSE)

ggplot(df, aes(x=height,y=weight)) +
  geom_point(aes(color=sex))+
  geom_smooth(method = 'lm',se=TRUE,color='black')+
  geom_smooth(aes(color = sex),method = 'lm',se=FALSE)


g + scale_y_log10()
ggplot(df,aes(sex,height))+geom_boxplot()

ggplot(df,aes(sex,height))+
  geom_boxplot()+
  geom_violin(alpha=0.5)+
  geom_jitter()

ggplot(df,aes(x=height,fill=sex))+
  geom_density(alpha=0.6)+ggtitle("Height of boys and girls")+
  xlab("Height (cm)")+ ylab("")+
  theme(legend.position = "top")

?theme

ggplot(df,aes(sex))+geom_bar()

ggplot(df,aes(weight))+geom_histogram()

ggplot(df, aes(weight)) +
  facet_wrap(~ sex) +
  geom_histogram()

df$below160cm <- cut(df$height,breaks=c(0,160,Inf))

ggplot(df, aes(x=sex)) +
  facet_wrap(~ below160cm) +
  geom_bar()

ggplot(df,aes(sex,fill = below160cm))+geom_bar(position="dodge")


## avg weight per gender
mean(df$weight)
mean(df[df$sex == 'f',"weight"])
mean(df[df$sex == 'm',"weight"])

?aggregate
aggregate(weight ~ sex, FUN = mean,data = df)

## tidyverse, dplyr
subset(df,sex== "f")

## data.table
install.packages("data.table")
library(data.table)
dt <- data.table(df)

## dt[i]

dt[sex == "f"][1:5]
dt[ageYear]<12

dt[ageYear == min(ageYear)][order(bmi)]

## dt[i,j]
dt[,mean(height)]
dt[,summary(height)]
dt[,hist(height)]

dt[sex == "m", mean(height)]
dt[sex == "f", mean(height)]

dt[,mean(height),by= sex]

dt[,list(
  height=mean(height),
  weight=mean(weight)
  ),
  by=list(gender=sex,below160cm)]

## new variable elementary school
dt$elementary_school <- dt$ageYear<14
dt$elementary_school <- cut(dt$ageYear,breaks = c(0,14,18))
dt[,elementary_school:= ageYear < 14]
## compute the median weight for elementary vs non-elementary school
dt[ageYear < 18, median(weight),by= elementary_school]

dt[1:5]

dt[,.N]
dt[.N]
?sample
dt[sample(1:.N,5)]
dt[round(runif(5,min=1,max=.N))]

?fread

booking <- fread("http://bit.ly/CEU-R-hotels-2018-prices")
str(booking)

## TODO count the number of bookings below 100 EUR
booking[price < 100,.N]
## TODO count the number of bookings below 100 EUR without an offer
booking[price < 100 & offer == 0,.N]
## TODO avg price of bookings below 100 EUR
booking[price < 100,mean(price)]
## TODO avg price of bookings on weekdays
booking[weekend ==0,mean(price)]
## TODO avg price of bookings on weekends
booking[weekend ==1,mean(price)]
## TODO include nnights, holiday and year in the aggregate variables
booking[,mean(price),by=list(weekend,nnights,holiday)]
## TODO compute the average price per number of stars
features <- fread("http://bit.ly/CEU-R-hotels-2018-features")
merge(booking,feature)

booking[!hotel_id %in% feature$hotel_id]

dt <- merge(booking,feature,all = TRUE)

dt[,mean(price),by= stars][order(-stars)]
dt[,mean(price),by= stars][order(stars,decreasing = TRUE)]
  
