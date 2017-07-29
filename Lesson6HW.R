sample(10)
sample(10, replace=T)
prob1 <- c(rep(.15, 5), rep (.05, 5))
prob1
sample(10, replace=T, prob=prob1)
y1 <- matrix( round(rnorm(25,5)), ncol=5)
y1
x1 <- matrix( round(rnorm(40, 5)), ncol=5)
x1
y2 <- matrix (round(rnorm(40,5)), ncol=5)
y2
data <- round(rnorm(100, 5, 5))
data[1:10]
#TODO 1
resamples <- lapply(1:20, function(i) sample(data, replace=T))
resamples[1]
r.median <-sapply(resamples, median)
r.median
sqrt(var(r.median))
#TODO 2
hist(r.median)

b.median <- function(data, num) {
  resamples <- lapply(1:20, function(i) sample(data, replace=T))
  r.median <- sapply(resamples, median)
  std.err <- sqrt(var(r.median))
  list(std.err=std.err, resamples=resamples, medians=r.median)   
}
data1 <- round(rnorm(100, 5, 5))
b1 <- b.median(data1, 30)
b1$resamples[1]
b1$std.err
hist(b1$medians)

b.median(rnorm(100, 5, 2), 50)$std.err

#TODO 3
b.stat <- function(data, num, stat) {
  resamples <- lapply (1:20, function(i) sample(data, replace=T))
  r.stat <- sapply(resamples, stat)
  std.err <- sqrt(mean(r.stat))
  list(std.err=std.err, resamples=resamples, stats=r.stat)
}
b.stat(rnorm(100, 5, 2), 50, mean)$std.err

#TODO 4
hist(b.stat(rnorm(100, 5, 2), 50, var)$stats)