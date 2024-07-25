library(ggplot2)
library(data.table)
# setwd("C:/Users/qiaoh/GIT/LSS_Project/lss")

#### hill ####
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
all_hill<-list()
for (i in c(1:nrow(coms))){
  f<-yield.function(type=coms[i]$type)
  lines<-data.table(yield.function=f, 
                    a=0,
                    alpha=coms[i]$alpha,
                    beta=coms[i]$beta,
                    type=coms[i]$type
                    )
  all_hill[[i]]<-lines
}
all_hill<-rbindlist(all_hill)
all_hill$shape<-"none"
all_hill[alpha==0 & type=="Hill.loser"]$shape<-"convex"
all_hill[alpha==1 & type=="Hill.loser"]$shape<-"concave"
all_hill[alpha==0 & type=="Hill.winner"]$shape<-"concave"
all_hill[alpha==1 & type=="Hill.winner"]$shape<-"convex"

all_hill$label<-paste(all_hill$alpha, all_hill$beta)
all_hill$sub.type<-"NA"
all_hill$final.type<-paste(all_hill$type,all_hill$shape)


#### winner & loser ####
#need a better scale
a<-10^seq(0, 2, 0.05)
a<-c(1/a, a)
yield<-seq(0, 1, by=0.01)
type<-c("loser", "winner")
coms<-data.table(expand.grid(a=a, type=type))
all_conf<-list()
for (i in c(1:nrow(coms))){
  f<-yield.function(type=coms[i]$type)
  lines<-data.table(yield.function=f, 
                    a=coms[i]$a,
                    alpha = 0,
                    beta=0,
                    type=coms[i]$type
                    )
  all_conf[[i]]<-lines
}
all_conf<-rbindlist(all_conf)
all_conf$shape<-"none"
all_conf[a>1]$shape<-"convex"
all_conf[a<1]$shape<-"concave"

all_conf$label<-NA
all_conf$sub.type<-"NA"
all_conf$final.type<-paste(all_conf$type,all_conf$shape)


#### linear ####
alpha<-c(0.30, 0.70)
beta<-seq(0, 1, by=0.05)
yield<-seq(0, 1, by=0.01)
type<-c("loser.linear", "winner.linear")
coms<-data.table(expand.grid(alpha=alpha, 
                             beta=beta,
                             type=type))
all_linear<-list()
for (i in c(1:nrow(coms))){
  # i=1
  f<-yield.function(type=coms[i]$type)
  lines<-data.table(yield.function=f,
                    a=0,
                    alpha=coms[i]$alpha,
                    beta=coms[i]$beta,
                    type=coms[i]$type
                    )
  all_linear[[i]]<-lines
}
all_linear<-rbindlist(all_linear)
all_linear$shape<-"none"
all_linear[(alpha+beta)<1 & type=="loser.linear"]$shape<-"convex"
all_linear[(alpha+beta)>1 & type=="loser.linear"]$shape<-"concave"
all_linear[(alpha>beta) & type=="winner.linear"]$shape<-"convex"
all_linear[(alpha<beta) & type=="winner.linear"]$shape<-"concave"
all_linear[round(alpha*100)==round(beta*100) & type=="winner.linear"]$shape<-"none"

all_linear$sub.type<-"forest"
all_linear[alpha==0.7]$sub.type<-"crop"
all_linear$label<-paste(all_linear$alpha, all_linear$beta)
all_linear$final.type<-paste(all_linear$type,all_linear$shape,all_linear$sub.type)


#### hump ####
alpha<-c(2:10)
beta<-c(2:10)
alpha_beta<-data.table(expand.grid(alpha=alpha, beta=beta))

type<-"hump"
f<-yield.function(type=type)
all_hump<-list()
for (i in c(1:nrow(alpha_beta))){
  print(paste(i, nrow(alpha_beta)))
  lines<-data.table(yield.function=f,
                    a=0,
                    alpha=alpha_beta[i]$alpha,
                    beta=alpha_beta[i]$beta,
                    type=type
                    )
  all_hump[[i]]<-lines
}

all_hump<-rbindlist(all_hump)
all_hump$label<-paste(all_hump$alpha, all_hump$beta)
all_hump$shape<-"mirro"
all_hump[alpha>beta]$shape<-"right"
all_hump[alpha<beta]$shape<-"left"

all_hump$sub.type<-"NA"
all_hump$final.type<-paste(all_hump$type,all_hump$shape)


species.all<-rbindlist(list(all_hill,all_conf, all_linear,all_hump), use.names = T)

saveRDS(species.all, "../Data/species/species.all.rda")

saveRDS(all_hill, "../Data/species/all_hill.rda")
saveRDS(all_conf, "../Data/species/all_conf.rda")
saveRDS(all_linear, "../Data/species/all_linear.rda")
saveRDS(all_hump, "../Data/species/all_hump.rda")

table(species.all$final.type)

species.now<-rbindlist(list(all_conf,all_hump), use.names = T) ##delete hill curve
table(species.now$type)
species.now.50.by.type <- species.now[,.SD[sample(.N, 50)],by = type]
saveRDS(species.now.50.by.type, "../Data/species/species.now.50.by.type.rda")
# species.pool.50.by.type<- species.all[,.SD[sample(.N, 50)],by = type]
# saveRDS(species.pool.50.by.type, "../Data/species.pool.50.by.type.rda")