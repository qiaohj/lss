library(ggplot2)
library(data.table)
# setwd("C:/Users/qiaoh/GIT/LSS_Project/lss")

#### hill ####
if (F)
{
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
}

#### winner & loser ####
if (F)
{
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
}


#### linear ####
alpha<-c(0.30, 0.40, 0.60,0.70)
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
all_linear[alpha==0.7 | alpha == 0.6]$sub.type<-"crop"
all_linear$label<-paste(all_linear$alpha, all_linear$beta)
all_linear$final.type<-paste(all_linear$type,all_linear$shape,all_linear$sub.type)

all_linear$ID<-c(1:nrow(all_linear))
all_linear$speciesID <- paste(all_linear$final.type,all_linear$ID)

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

all_hump$ID<-c(1:nrow(all_hump))
all_hump$speciesID <- paste(all_hump$final.type,all_hump$ID)


#### exponential ####
yield<-seq(0, 1, by=0.01)
exponential_conf<-readRDS("../Data/exponential.curve.conf.rda")
all_exponential<-list()
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
    
    lines<-data.table(yield.function = f,
                      alpha=a,
                      beta=b,
                      a=exponential_conf[i]$c,
                      type=type,
                      shape=shape)
    all_exponential[[length(all_exponential)+1]]<-lines
  }
}
all_exponential<-rbindlist(all_exponential)

all_exponential$label<-paste(all_exponential$alpha, all_exponential$beta)
all_exponential$sub.type <- NA
all_exponential$final.type <- paste(all_exponential$type,all_exponential$shape)

all_exponential$ID<-c(1:nrow(all_exponential))
all_exponential$speciesID <- paste(all_exponential$final.type,all_exponential$ID)

if (F){
  all_curves<-list()
  for (i in c(1:nrow(all_exponential))){
    item<-all_exponential[i]
    type<-item$type
    shape<-item$shape
    
    f<-item$yield.function[[1]]
    
    
    lines<-data.table(yield=yield, 
                      v=f(yield, parameters=list(
                        "alpha"=item$alpha,
                        "beta"=item$beta,
                        "a"=item$a)),
                      a=item$a,
                      alpha=item$alpha,
                      beta=item$beta,
                      type=type,
                      shape=shape,
                      sp_id=item$ID)
    all_curves[[length(all_curves)+1]]<-lines
  }
  all_curves<-rbindlist(all_curves)
  
  all_curves$label<-paste(all_curves$alpha, all_curves$beta)
  saveRDS(all_curves, "../Data/species/species.curve.exponential.rda")
  ggplot(all_curves)+geom_line(aes(x=yield, y=v, group=label, color=shape))+
    facet_wrap(~type)
}


########
# species.all<-rbindlist(list(all_hill,all_conf, all_linear,all_hump), use.names = T)
species.all<-rbindlist(list(all_linear,all_hump,all_exponential), use.names = T)

saveRDS(species.all, "../Data/species/species.all.rda")

# saveRDS(all_hill, "../Data/species/all_hill.rda")
# saveRDS(all_conf, "../Data/species/all_conf.rda")
saveRDS(all_linear, "../Data/species/all_linear.rda")
saveRDS(all_hump, "../Data/species/all_hump.rda")
saveRDS(all_exponential, "../Data/species/all_exponential.rda")

table(species.all$type)


#### species pool ####
species.linear.all <- rbindlist(list(all_linear,all_hump), use.names = T)
species.curve.all <- rbindlist(list(all_hump,all_exponential), use.names = T)

table(species.linear.all$final.type)

species.curve.pool <- species.linear.all[,.SD[sample(.N, 30)],by = type]

yield<-seq(0, 1, by=0.01)
all_curves<-list()
for (i in c(1:nrow(species.now.30.by.type))){
  f<-yield.function(type=species.now.30.by.type[i]$type)
  lines<-data.table(yield=yield, 
                    v=f(yield, parameters=list(
                      "a" = species.now.30.by.type[i]$a,
                      "alpha"=species.now.30.by.type[i]$alpha,
                      "beta"=species.now.30.by.type[i]$beta)),
                    a = species.now.30.by.type[i]$a,
                    alpha =species.now.30.by.type[i]$alpha,
                    beta =species.now.30.by.type[i]$beta,
                    group1 = paste(species.now.30.by.type[i]$a,
                                   species.now.30.by.type[i]$alpha,
                                   species.now.30.by.type[i]$beta),
                    type=species.now.30.by.type[i]$type)
  all_curves[[i]]<-lines
}
all_curves<-rbindlist(all_curves)
ggplot(all_curves)+geom_line(aes(x=yield, y=v,group = group1))+
  facet_wrap(~type)
# plot(x=yield,y=all_curves[[1]]$v)

saveRDS(species.now.30.by.type, "../Data/species/species.now.30.by.type.rda")
# species.pool.50.by.type<- species.all[,.SD[sample(.N, 50)],by = type]
# saveRDS(species.pool.50.by.type, "../Data/species.pool.50.by.type.rda")