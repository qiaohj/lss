library(ggplot2)
library(data.table)
setwd("C:/Users/qiaoh/GIT/LSS_Project/lss")

yield.function<-function(type="loser"){
  #a is seq(0.1, 10, by=0.1)
  if (type=="loser"){
    f<-function(yield=0, parameters=list("a"=1)){
      (-yield+1)^parameters[["a"]]
    }
  }
  
  if (type=="winner"){
    f<-function(yield=0, parameters=list("a"=1)){
      yield^parameters[["a"]]
    }
  }
  
  if (type=="hump"){
    f<-function(yield=0,
                parameters=list("alpha"=2, "beta"=10)){
      alpha<-parameters[["alpha"]]
      beta<-parameters[["beta"]]
      max<-rbeta(10e5, alpha, beta)
      max<-max(density(max)$y)
      dbeta(yield,
            shape1=alpha,
            shape2=beta)/max
    }
  }  
  f
}

#need a better scale
a<-10^seq(0, 2, 0.1)
a<-c(1/a, a)
yield<-seq(0, 1, by=0.01)
type<-c("loser", "winner")
coms<-data.table(expand.grid(a=a, type=type))
all_curves<-list()
for (i in c(1:nrow(coms))){
  f<-yield.function(type=coms[i]$type)
  lines<-data.table(yield=yield, 
                    v=f(yield, parameters=list("a"=coms[i]$a)),
                    a=coms[i]$a,
                    type=coms[i]$type)
  all_curves[[i]]<-lines
}
all_curves<-rbindlist(all_curves)
ggplot(all_curves)+geom_line(aes(x=yield, y=v, group=a))+
  facet_wrap(~type)

alpha<-c(2:10)
beta<-c(2:10)
alpha_beta<-data.table(expand.grid(alpha=alpha, beta=beta))

type<-"hump"
f<-yield.function(type=type)
all_curves<-list()
for (i in c(1:nrow(alpha_beta))){
  print(paste(i, nrow(alpha_beta)))
  lines<-data.table(yield=yield, 
                    v=f(yield,
                    parameters=list(
                      "alpha"=alpha_beta[i]$alpha,
                      "beta"=alpha_beta[i]$beta)),
                    alpha=alpha_beta[i]$alpha,
                    beta=alpha_beta[i]$beta,
                    type=type)
  all_curves[[i]]<-lines
}

all_curves<-rbindlist(all_curves)
all_curves$label<-paste(all_curves$alpha, all_curves$beta)
ggplot(all_curves)+geom_line(aes(x=yield, y=v, color=label))+
  facet_wrap(~type)+
  theme(legend.position = "none")

ggplot(all_curves[alpha==beta])+geom_line(aes(x=yield, y=v, color=label))+
  facet_wrap(~type)+
  theme(legend.position = "none")

ggplot(lines)+geom_line(aes(x=yield, y=v))
sp.conf<-list(yield.function=yield.function(type="loser", parameters=list(a=10)))
sp.conf[["fc.i"]]<-sp.conf$yield.function(0, parameters=list(a=10))
sp.conf[["fc.e"]]<-sp.conf$yield.function(0.05, parameters=list(a=10))

sp.list<-list(sp.conf)

