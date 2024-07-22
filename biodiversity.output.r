library(ggplot2)
library(data.table)
setwd("C:/Users/qiaoh/GIT/LSS_Project/lss")
resolution<-100
species.pool.50.by.type<-readRDS("../Data/species.pool.50.by.type.rda")
lands<-readRDS("../Data/land_conf.rda")
hist(lands$forest_p_real)
total_product<-seq(0, 10000, by=1000)
product<-8000

for (product in total_product){
  species_pool<-species.pool.50.by.type
  
  lands_species<-list()
  for (i in c(1:nrow(species_pool))){
    print(paste(i, nrow(species_pool)))
    lands_item<-lands
    lands_item<-lands_item[N_crop>0]
    lands_item$yield<-product/lands_item$N_crop
    item<-species_pool[i]
    f<-item$yield.function[[1]]
    paramaters<-list("a"=item$a,
                     "alpha"=item$alpha,
                     "beta"=item$beta)
    lands_item$biodiversity_output_crop<-f(lands_item$yield, paramaters) * lands_item$N_crop
    lands_item$biodiversity_output_core<-f(0, paramaters) * lands_item$N_core 
    lands_item$biodiversity_output_edge<-f(0.05, paramaters) * lands_item$N_edge  
    lands_item$biodiversity_output<-
      lands_item$biodiversity_output_core + 
      lands_item$biodiversity_output_crop + 
      lands_item$biodiversity_output_edge
    lands_item<-lands_item[yield<=1]
    if (F){
      plot(lands_item$biodiversity_output, lands_item$yield)
    }
    lands_item$type<-item$type
    lands_item$a<-item$a
    lands_item$alpha <-item$alpha 
    lands_item$beta <-item$beta 
    lands_item$sub.type<-item$sub.type
    lands_item$total_product<-product
    
    lands_species[[i]]<-lands_item
  }
  
  lands_species_df<-rbindlist(lands_species)
  lands_species_df[a==lands_species_df[1]$a & rep==1
                   & forest_p %in% c(0.2, 0.5)
                   & block_size ==20]
  
  if (F){
    
    lands_species_se<-lands_species_df[]
    ggplot(lands_species_df[sample(nrow(lands_species_df), 1e4)])+
      geom_point(aes(x=forest_p_real, y=biodiversity_output, color=sub.type))+
      facet_wrap(~type+sub.type)
    
    lands_species_sum<-lands_species_df[,.(biodiversity_output=sum(biodiversity_output),
                                           N_sp=.N),
                                        by=list(forest_p, rep, block_size, land.type,
                                                type, sub.type, total_product)]
    hist(lands[forest_p_real>0]$forest_p_real)
    
    p<-ggplot(lands_species_sum)+geom_point(aes(x=forest_p , y=biodiversity_output, color=type))+
      facet_grid(block_size~sub.type+type)
    p
    ggsave(p, filename="../Figures/fig1.png", width=15, height=15)
    
    lands_species_sum<-lands_species_df[,.(biodiversity_output=sum(biodiversity_output),
                                           N_sp=.N),
                                        by=list(forest_p, rep, block_size, land.type,total_product, type)]
    
    
    p<-ggplot(lands_species_sum)+geom_point(aes(x=block_size , y=biodiversity_output, color=type))+
      facet_wrap(~forest_p, nrow=3)
    p
    ggsave(p, filename="../Figures/fig2.png", width=15, height=15)
  }
  
}

