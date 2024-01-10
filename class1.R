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
