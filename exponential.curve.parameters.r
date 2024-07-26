library(data.table)
library(ggplot2)
library(minpack.lm)
rm(list=ls())
k<-seq(0.1, 0.9, by=0.1)

x2<-seq(0.1, 0.9, by=0.1)
y2<-c(1/(10^c(1:6)), 
      seq(0.1, 0.9, by=0.01))
y2<-c(seq(0.01, 0.9, by=0.01))
y2<-c(1/(10^c(1:6)))
inflection.points<-data.table(x1=0, x3=1, y1=0, y3=1, 
                              expand.grid(x2=x2, y2=y2)
                              )
inflection.points<-inflection.points[x2>=y2]
i=1
all<-list()
xx<-seq(0, 1, by=0.01)
for (i in c(1:nrow(inflection.points))){
  x<-c(inflection.points[i]$x1,
       inflection.points[i]$x2,
       inflection.points[i]$x3)
  y<-c(inflection.points[i]$y1,
       inflection.points[i]$y2,
       inflection.points[i]$y3)
  tryCatch({
    fit <- nlsLM(y ~ a * exp(b * x) + c, start = list(a = 0.1, b = 0.1, c = 0.1),
                 control = nls.control(maxiter=1024,
                                       minFactor = 1/1024))
  },
  error = function(cond) {
    
  }
  )
  if (!exists("fit")){
    print(paste("skip", inflection.points[i]$x2, inflection.points[i]$y2))
    next()
  }
 
  coefficients <- coef(fit)
  a <- coefficients["a"]
  b <- coefficients["b"]
  c <- coefficients["c"]
  yy1<-a*exp(1)^(b*xx)+c
  yy2<-(-1*a)*exp(1)^(b*xx)+c+1
  item1<-data.table(a=a, b=b, c=c,
                    x_threshold=inflection.points[i]$x2, 
                    y_threshold=inflection.points[i]$y2, 
                    x=xx,
                    y=yy1,
                    type="winner",
                    group=paste(i, "a1")
  )
  item2<-data.table(a=a, b=b, c=c,
                    x_threshold=inflection.points[i]$x2, 
                    y_threshold=inflection.points[i]$y2, 
                    x=xx,
                    y=yy2,
                    type="loser",
                    group=paste(i, "a2")
  )
  item<-rbindlist(list(item1, item2))
  item<-item1
  all[[i]]<-item
  rm("fit")
}
all<-rbindlist(all)
ggplot(all)+geom_line(aes(x=x, y=y, color=type, group=group))+
  facet_wrap(~x_threshold, scale="free")

ggplot(all)+geom_line(aes(x=x, y=1-y, color=type, group=group))+
  facet_wrap(~x_threshold, scale="free")
ggplot(all)+geom_line(aes(x=1-x, y=1-y, color=type, group=group))+
  facet_wrap(~x_threshold, scale="free")

ggplot(all)+geom_line(aes(x=1-x, y=y, color=type, group=group))+
  facet_wrap(~x_threshold, scale="free")

