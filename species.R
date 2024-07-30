library(ggplot2)
library(data.table)
# setwd("C:/Users/qiaoh/GIT/LSS_Project/lss")

yield.function<-function(type="loser"){
  if (type=="exponential"){
    f<-function(yield=0, parameters=list("alpha"=0.3, "beta"=1, "a"=0)){
      alpha<-parameters[["alpha"]]
      beta<-parameters[["beta"]]
      a<-parameters[["a"]]
      v_shift<-1
      yield_shift<-1
      
      if (alpha<0){
        
        yield_shift<--1
        if (beta<0){
          type<-"exponential.winner"
          shape<-"concave"
          v_shift<- -1
        }else{
          type<-"exponential.loser"
          shape<-"convex"
        }
      }else{
        
        if (beta<0){
          type<-"exponential.loser"
          shape<-"concave"
          v_shift<- -1
        }else{
          type<-"exponential.winner"
          shape<-"convex"
        }
      }
      if (yield_shift==-1){
        yield<-1-yield
      }
      v<-abs(alpha)*exp(1)^(abs(beta)*(yield))+a
      if (v_shift==1){
        v<-v
      }else{
        v<-1-v
      }
      v[v>1]<-1
      v[v<0]<-0
      v
    }
  }
  if (type=="Hill.loser"){
    f<-function(yield=0, parameters=list("alpha"=0.3, "beta"=1)){
      alpha<-parameters[["alpha"]]
      beta<-parameters[["beta"]]
      if (alpha==0){
        yield<-yield*2
        v<-(yield^beta)/(beta+yield^beta)
        v<-v/max(v)
        v[is.nan(v)]<-0
        1-v
      }else{
        yield<-yield
        v<-((yield * beta)/(1-yield))^(1/beta)
        v<-v/max(v)
        v[is.nan(v)]<-0
        1-v
      }
    }
  }
  
  if (type=="Hill.winner"){
    f<-function(yield=0, parameters=list("alpha"=0.3, "beta"=1)){
      alpha<-parameters[["alpha"]]
      beta<-parameters[["beta"]]
      
      if (alpha==0){
        yield<-yield*2
        v<-(yield^beta)/(beta+yield^beta)
        v<-v/max(v)
        v[is.nan(v)]<-0
        v
      }else{
        yield<-yield
        v<-((yield * beta)/(1-yield))^(1/beta)
        v<-v/max(v)
        v[is.nan(v)]<-0
        v
      }
    }
  }
  
  #a is seq(0.1, 10, by=0.1)
  if (type=="loser"){
    f<-function(yield=0, parameters=list("a"=1)){
      yield<-yield
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
#exponential
yield<-seq(0, 1, by=0.01)
exponential_conf<-readRDS("../Data/exponential.curve.conf.rda")
all_curves<-list()
i=1
coms<-data.table(expand.grid(a=c(1, -1), b=c(1, -1)))
for (j in c(1:nrow(coms))){
  for (i in c(1:nrow(exponential_conf))){
    a<-exponential_conf[i]$a * coms[j]$a
    b<-exponential_conf[i]$b * coms[j]$b
    f<-yield.function(type="exponential")
    
    if (a<0){
      if (b<0){
        type<-"exponential.winner"
        shape<-"concave"
      }else{
        type<-"exponential.loser"
        shape<-"convex"
      }
    }else{
      if (b<0){
        type<-"exponential.loser"
        shape<-"concave"
      }else{
        type<-"exponential.winner"
        shape<-"convex"
      }
    }
    
    lines<-data.table(yield=yield, 
                      v=f(yield, parameters=list(
                        "alpha"=a,
                        "beta"=b,
                        "a"=exponential_conf[i]$c)),
                      a=exponential_conf[i]$c,
                      alpha=a,
                      beta=b,
                      type=type,
                      shape=shape)
    all_curves[[length(all_curves)+1]]<-lines
  }
}
all_curves<-rbindlist(all_curves)

all_curves$label<-paste(all_curves$alpha, all_curves$beta)

ggplot(all_curves)+geom_line(aes(x=yield, y=v, group=label, color=shape))+
  facet_wrap(~type)

#need a better scale
alpha<-c(0,1)
a<-8
#alpha<-c(1/alpha, alpha)
beta<-seq(1, 5, by=0.5)
beta<-c(1/beta, beta)
yield<-seq(0, 0.95, by=0.01)
type<-c("Hill.loser", "Hill.winner")
coms<-data.table(expand.grid(alpha=alpha, 
                             beta=beta,
                             type=type))
all_curves<-list()
for (i in c(1:nrow(coms))){
  f<-yield.function(type=coms[i]$type)
  lines<-data.table(yield=yield, 
                    v=f(yield, parameters=list(
                      "alpha"=coms[i]$alpha,
                      "beta"=coms[i]$beta)),
                    alpha=coms[i]$alpha,
                    beta=coms[i]$beta,
                    type=coms[i]$type)
  all_curves[[i]]<-lines
}
all_curves<-rbindlist(all_curves)
all_curves$shape<-"none"
all_curves[alpha==0 & type=="Hill.loser"]$shape<-"convex"
all_curves[alpha==1 & type=="Hill.loser"]$shape<-"concave"
all_curves[alpha==0 & type=="Hill.winner"]$shape<-"concave"
all_curves[alpha==1 & type=="Hill.winner"]$shape<-"convex"

all_curves$label<-paste(all_curves$alpha, all_curves$beta)

ggplot(all_curves)+geom_line(aes(x=yield, y=v, group=label, color=shape))+
  facet_wrap(~type)

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




#
alpha<-c(0.30, 0.40, 0.60,0.70)
beta<-seq(0, 1, by=0.05)
yield<-seq(0, 1, by=0.01)
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
all_curves[alpha==0.7 | alpha == 0.6]$sub.type<-"crop"
all_curves$label<-paste(all_curves$alpha, all_curves$beta)
ggplot(all_curves)+geom_line(aes(x=yield, y=v, group=label, color=shape))+
  facet_grid(sub.type~type)

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

