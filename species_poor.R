library(ggplot2)
library(data.table)
setwd("C:/Users/qiaoh/GIT/LSS_Project/lss")

#need a better scale
a<-exp(1)^seq(0.05, 1, 0.01)
a<-c(1/a, a)
yield<-seq(0, 1, by=0.01)
type<-c("loser", "winner")
coms<-data.table(expand.grid(a=a, type=type))
coms$sub.type<-ifelse(coms$a<1, "concave", "convex")
all_conf<-list()
for (i in c(1:nrow(coms))){
  f<-yield.function(type=coms[i]$type)
  item<-data.table(yield.function=f, 
                    a=coms[i]$a,
                    alpha=0,
                    beta=0,
                    type=coms[i]$type,
                    sub.type=coms[i]$sub.type)
  all_conf[[i]]<-item
}
all_conf<-rbindlist(all_conf)

alpha<-c(2:10)
beta<-c(2:10)
alpha_beta<-data.table(expand.grid(alpha=alpha, beta=beta),
                       yield.function=yield.function(type="hump"),
                       a=0,
                       type="hump",
                       sub.type="mirro")
alpha_beta[alpha>beta]$sub.type<-"right"
alpha_beta[alpha<beta]$sub.type<-"left"

species.all<-rbindlist(list(all_conf, alpha_beta), use.names = T)

saveRDS(species.all, "../Data/species.all.rda")

table(species.all$type)

species.pool.50.by.type<- species.all[,.SD[sample(.N, 50)],by = type]
saveRDS(species.pool.50.by.type, "../Data/species.pool.50.by.type.rda")


