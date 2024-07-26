library(data.table)
library(ggplot2)
library(minpack.lm)

k<-c(0.25, 0.5, 1, 2, 3)
k<-0.5
x1<-0; x2<-0.5; x3<-1
y1<-0; y2<-c(0.0001, 0.001, 0.01, 
             seq(0.1, 0.9, by=0.1),
             0.99, 0.999, 0.9999); 
y3<-1
y2<-y2[y2!=x2]



# Define the exponential function form
exp_func <- function(x, a, b, c) {
  a * exp(b * x) + c
}
x<-c(x1, 0.1, x3)
y<-c(y1, y2[1], y3)
fit <- nlsLM(y ~ a * exp(b * x) + c, start = list(a = 1, b = 1, c = 1),
             control = nls.control(maxiter=1024))
coefficients <- coef(fit)
a <- coefficients["a"]
b <- coefficients["b"]
c <- coefficients["c"]

# Generate points for plotting the curve
x_vals <- seq(min(x), max(x), length.out = 100)
y_vals <- exp_func(x_vals, a, b, c)

# Plot the original points and the fitted curve
plot(x, y, pch = 16, col = "blue", ylim = c(0, max(y) * 1.2),
     xlab = "x", ylab = "y", main = "Exponential Curve Fitting with nlsLM")
lines(x_vals, y_vals, col = "red", lwd = 2)
legend("topleft", legend = c("Data Points", "Fitted Curve"),
       col = c("blue", "red"), pch = c(16, NA), lwd = c(NA, 2))
# Use nls to fit the curve
fit <- nls(y ~ exp_func(x, a, b, c), 
           start = list(a = -2, b = -20, c = -2),
           control = nls.control(minFactor=1/1e7,
                                 maxiter =1e4,
                                 warnOnly =T))

# Print the summary of the fit
summary(fit)

# Extract the coefficients
coefficients <- coef(fit)
a <- coefficients["a"]
b <- coefficients["b"]
c <- coefficients["c"]

# Generate points for plotting the curve
x_vals <- seq(min(x), max(x), length.out = 100)
y_vals <- exp_func(x_vals, a, b, c)

# Plot the original points and the fitted curve
plot(x, y, pch = 16, col = "blue", ylim = c(0, max(y) * 1.2),
     xlab = "x", ylab = "y", main = "Exponential Curve Fitting")
lines(x_vals, y_vals, col = "red", lwd = 2)
legend("topleft", legend = c("Data Points", "Fitted Curve"),
       col = c("blue", "red"), pch = c(16, NA), lwd = c(NA, 2))




d<-x2-x1
r<-(y3-y2)/(y2-y1)
b<-log(r)/d
a1<-(y2-y1)/(r^(x2/d)-r^(x1/d))
a2<--1 * a1
c1<-y1-a1^exp(1)^(b*x1)
c2<-y1-a2^exp(1)^(b*x1)

i=1
x=seq(0, 1, 0.01)
all<-list()
for (i in c(1:length(r))){
  item1<-data.table(a=a1[i], b=b[i], c=c1[i],
                    d=d, r=r[i], y_threshold=y2[i], 
                   x=x,
                   y=a1[i]*exp(1)^(b[i]*x)+c1[i],
                   type="winner",
                   group=paste(i, "a1")
  )
  item2<-data.table(a=a2[i], b=b[i], c=c2[i],
                    d=d, r=r[i], y_threshold=y2[i], 
                    x=x,
                    y=a2[i]*exp(1)^(b[i]*x)+c2[i]+1,
                    type="loser",
                    group=paste(i, "a2")
  )
  item<-rbindlist(list(item1, item2))
  all[[i]]<-item
}
all<-rbindlist(all)
ggplot(all)+geom_line(aes(x=x, y=y, color=type, group=group))

x<-seq(0, 1, 0.1)
y<-a*exp(1)^(b*x)+c
plot(y, x, type="l")
points(c(x1, x2, x3), c(y1, y2, y3), col="red")

