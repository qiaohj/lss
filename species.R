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
  
 
  if (type=="loser.linear"){
    f<-function(yield=0, parameters=list("alpha"=0.3, "beta"=1)){
      alpha<-parameters[["alpha"]]
      beta<-parameters[["beta"]]
      yield_df<-data.table(yield=yield)
      yield_df$f<-ifelse(yield<=alpha, 1, 2)
      b1<-1
      a1<-(beta-1)/alpha
      a2<-beta/(alpha-1)
      b2<-a2*-1
      yield_df$v<-a1 * yield_df$yield +b1
      yield_df[f==2]$v<-a2 * yield_df[f==2]$yield +b2
      yield_df$v
    }
  }
  
  if (type=="winner.linear"){
    f<-function(yield=0, parameters=list("alpha"=0.3, "beta"=1)){
      alpha<-parameters[["alpha"]]
      beta<-parameters[["beta"]]
      yield_df<-data.table(yield=yield)
      yield_df$f<-ifelse(yield<=alpha, 1, 2)
      b1<-0
      a1<-beta/alpha
      
      b2<-(alpha-beta)/(alpha-1)
      a2<-1-b2
      yield_df$v<-a1 * yield_df$yield +b1
      yield_df[f==2]$v<-a2 * yield_df[f==2]$yield +b2
      yield_df$v
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
#
alpha<-c(0.30, 0.70)
beta<-seq(0, 1, by=0.05)
type<-c("loser.linear", "winner.linear")
coms<-data.table(expand.grid(alpha=alpha, 
                             beta=beta,
                             type=type))
all_curves<-list()
for (i in c(1:nrow(coms))){
  f<-yield.function(type=coms[i]$type)
  lines<-data.table(yield=yield, 
                    v=f(yield, parameters=list("alpha"=coms[i]$alpha,
                                               "beta"=coms[i]$beta)),
                    alpha=coms[i]$alpha,
                    beta=coms[i]$beta,
                    type=coms[i]$type)
  all_curves[[i]]<-lines
}
all_curves<-rbindlist(all_curves)
all_curves$shape<-"none"
all_curves[(alpha+beta)<1 & type=="loser.linear"]$shape<-"convex"
all_curves[(alpha+beta)>1 & type=="loser.linear"]$shape<-"concave"
all_curves[(alpha>beta) & type=="winner.linear"]$shape<-"convex"
all_curves[(alpha<beta) & type=="winner.linear"]$shape<-"concave"
all_curves[round(alpha*100)==round(beta*100) & type=="winner.linear"]$shape<-"none"

all_curves$sub.type<-"forest"
all_curves[alpha==0.7]$sub.type<-"crop"
all_curves$label<-paste(all_curves$alpha, all_curves$beta)
ggplot(all_curves)+geom_line(aes(x=yield, y=v, group=label, color=shape))+
  facet_grid(sub.type~type)

#need a better scale
a<-10^seq(0, 2, 0.2)
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
all_curves$shape<-"none"
all_curves[a>1]$shape<-"convex"
all_curves[a<1]$shape<-"concave"
ggplot(all_curves)+geom_line(aes(x=yield, y=v, group=a, color=shape))+
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
all_curves$shape<-"mirro"
all_curves[alpha>beta]$shape<-"right"
all_curves[alpha<beta]$shape<-"left"


ggplot(all_curves)+geom_line(aes(x=yield, y=v, group=label, color=shape))+
  facet_wrap(~type)#+
  #theme(legend.position = "none")

ggplot(all_curves[alpha==beta])+geom_line(aes(x=yield, y=v, color=label))+
  facet_wrap(~type)+
  theme(legend.position = "none")


f<-expression(y=x^exp(1))
df1<-deriv(f, "x", function.arg = T)
df2<-deriv3(f, "x", function.arg = T)
x<-seq(0, 1, by=0.1)
df1(x)

df2(x)

