######################################################
### Experimental Code.  Experimental R Interface for IBM Watson Services / Cognitive Computing
### This code is foundation for Visualizing Retail / Brand / Reputational DRIFT over time for user segments
### Assume we are combing social media, bucketizing, and then looking at changes over time
### For now, it's all dummy data to get the basics of viz in place
######################################################


library(graphics)

## Setup up coordinate system
plot(c(-10,10), c(-10,10), type = "n", 
     main="Twitter Luxury Brand(X) Perception \n by Archetype - 1/2010 to 5/2016 ", 
     sub="*dummy data ",
     xlab="NOSTALGA-INNOVATION SPECTRUM", 
     ylab="BRAND ENTHUSIASM")
abline(h = -10:10, v = -10:10, col = "lightgray", lty=3)

scale <- 1.8



####### RED is TRENDING GOOD
data <- matrix(0,10,ncol=2)
data[1,1] <- rnorm(1)*scale
data[1,2] <- rnorm(1)*scale
for (i in 2:10)
{
  data[i,1]  <- data[(i-1),1]+runif(1)*scale
  data[i,2]  <- data[(i-1),2]+runif(1)*scale
}
data <- data.frame(data)
s <- 1:dim(data)[1]
points(data, main = "arrows(.) and segments(.)")
arrows(data$X1[s], data$X2[s], data$X1[s+1], data$X2[s+1], col= "red")


### BLUE IS TRENDING BAD
data <- matrix(0,10,ncol=2)
data[1,1] <- rnorm(1)*scale
data[1,2] <- rnorm(1)*scale
for (i in 2:10)
{
  print(i)
  data[i,1]  <- data[(i-1),1]-runif(1)*scale
  data[i,2]  <- data[(i-1),2]+runif(1)*scale
}
data <- data.frame(data)
s <- 1:dim(data)[1]
points(data, main = "arrows(.) and segments(.)")
arrows(data$X1[s], data$X2[s], data$X1[s+1], data$X2[s+1], col= "blue")



### YELLOW IS MIXED IS TRENDING BAD
data <- matrix(0,10,ncol=2)
data[1,1] <- rnorm(1)*scale
data[1,2] <- rnorm(1)*scale
for (i in 2:10)
{
  print(i)
  data[i,1]  <- data[(i-1),1]+runif(1)*scale*.3
  data[i,2]  <- data[(i-1),2]-runif(1)*scale
}
data <- data.frame(data)
s <- 1:dim(data)[1]
points(data, main = "arrows(.) and segments(.)")
arrows(data$X1[s], data$X2[s], data$X1[s+1], data$X2[s+1], col= "orange")


legend('top', c("Progressive","Traditionalists","Disaffected"), 
       lty=1, 
       col=c('red', 'blue', 'orange'), 
       bty='n', cex=1.5)

