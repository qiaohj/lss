library(ggplot2)
library(data.table)
# setwd("C:/Users/qiaoh/GIT/LSS_Project/lss")
rm(list=ls())
resolution<-100
# species.pool.50.by.type<-readRDS("../Data/species.pool.50.by.type.rda")
## all_hill,all_conf, all_linear,all_hump


species_pool <- readRDS("../Data/species/all_linear.rda")

lands<-readRDS("../Data/land_conf.rda")
lands$land_id<-c(1:nrow(lands))
hist(lands$forest_p_real)
total_product<-seq(0, 10000, by=1000)
product<-5000


sampling_core <- sampling_edge<-seq(0, 1, by=0.1)
coms.sampling<-data.table(expand.grid(core=sampling_core, edge=sampling_edge))
coms.sampling <- coms.sampling[core+edge == 1,]
coms.sampling<-data.table(core=seq(0, 1, by=0.1), 
                          edge=seq(1, 0, by=-0.1))
for (product in total_product){
  print(product)
  # species_pool<-species.pool.50.by.type
  species_pool<-species_pool
  
  lands_species<-list()
  lands_species_sampled<-list()
  for (i in c(1:nrow(species_pool))){
    print(paste(i, nrow(species_pool)))
    lands_item<-lands
    lands_item<-lands_item[N_crop>0]
    lands_item$yield<-product/lands_item$N_crop
    item<-species_pool[i]
    f<-item$yield.function[[1]]
    # f<-item$f[[1]]
    paramaters<-list("a"=item$a,
                     "alpha"=item$alpha,
                     "beta"=item$beta)
    lands_item$biodiversity_output_crop<-f(lands_item$yield, paramaters)
    lands_item$biodiversity_output_core<-f(0, paramaters)
    lands_item$biodiversity_output_edge<-f(0.05, paramaters) 
    lands_item$biodiversity_output<-
      lands_item$biodiversity_output_core * lands_item$N_core  + 
      lands_item$biodiversity_output_crop * lands_item$N_crop + 
      lands_item$biodiversity_output_edge * lands_item$N_edge 
    
    #### sampling based start
    if (F)
    {
      ## sampling edge:core = 1:1
      lands_item$biodiversity_output_equal <- 
        lands_item$biodiversity_output_crop * lands_item$N_crop + 
        mean(c(lands_item$biodiversity_output_edge,lands_item$biodiversity_output_core)) * (lands_item$N_edge+lands_item$N_core)
      
      ## sampling all in edge
      lands_item$biodiversity_output_all_edge <- 
        lands_item$biodiversity_output_crop * lands_item$N_crop + 
        lands_item$biodiversity_output_edge * (lands_item$N_edge+lands_item$N_core)
      
      ## sampling all in core
      lands_item$biodiversity_output_all_core <- 
        lands_item$biodiversity_output_crop * lands_item$N_crop + 
        lands_item$biodiversity_output_core * (lands_item$N_edge+lands_item$N_core)
    }
    lands_item<-lands_item[yield<=1]
    ## 
    #here is a problem. No bias means bias because the sampling strength are different between crop and forest.
    
    
    sampling_crop <- lands_item$biodiversity_output_crop ## no sampling bias in crop
    j=1
    for (j in 1:nrow(coms.sampling))
    {
      sample_item<-data.table(sampling_crop=sampling_crop,
                              sampling_core=lands_item$biodiversity_output_core * coms.sampling$core[j],
                              sampling_edge=lands_item$biodiversity_output_edge * coms.sampling$edge[j],
                              core_p=coms.sampling[j]$core,
                              edge_p=coms.sampling[j]$edge,
                              N_core=lands_item$N_core,
                              N_edge=lands_item$N_edge,
                              N_crop=lands_item$N_crop,
                              land_id=lands_item$land_id,
                              forest_p=lands_item$forest_p,
                              forest_p_real=lands_item$forest_p_real,
                              type=item$type,
                              shape=item$shape,
                              sub.type=item$sub.type,
                              sp_id=item$ID,
                              rep=lands_item$rep)
      sample_item$N_core_sampled<-sample_item$core_p * sample_item$N_core
      sample_item$N_edge_sampled<-sample_item$edge_p * sample_item$N_edge
      sample_item$N_forest_sampled<-sample_item$N_core_sampled + sample_item$N_edge_sampled
      
      sample_item<-sample_item[N_forest_sampled>=10 & N_crop>=10]
      
      sample_item$biodiversity_output<-sample_item$sampling_crop+sample_item$sampling_core+sample_item$sampling_edge
      lands_species_sampled[[length(lands_species_sampled)+1]]<-sample_item
    }
   
    #### sampling based end
    
    
    if (F){
      plot(lands_item$biodiversity_output, lands_item$yield)
    }
    lands_item$type<-item$type
    lands_item$a<-item$a
    lands_item$alpha <-item$alpha 
    lands_item$beta <-item$beta 
    lands_item$sub.type<-item$sub.type
    lands_item$shape<-item$shape
    lands_item$final.type<-item$final.type
    lands_item$total_product<-product
    lands_item$speciesID<-item$speciesID
    lands_item$sp_id=item$ID
    lands_species[[i]]<-lands_item
  }
  
  lands_species_sampled<-rbindlist(lands_species_sampled)
  
  lands_species_df<-rbindlist(lands_species)
  saveRDS(lands_species_df, sprintf("../Data/land_species/lands_all_linear_%d.rda", product))
  saveRDS(lands_species_sampled, sprintf("../Data/land_species/lands_species_sampled_all_linear_%d.rda", product))
  
  #lands_species_df[a==lands_species_df[1]$a & rep==1
  #                 & forest_p %in% c(0.2, 0.5)
  #                 & block_size ==20]
  
  if (F){
    
   table(lands_species_df$final.type)
    ggplot(lands_species_df[sample(nrow(lands_species_df), 1e3)])+
      geom_point(aes(x=forest_p_real, y=biodiversity_output, color=sub.type))+
      facet_wrap(~type+shape)
    
    
    df_temp<-lands_species_df[type=="loser.linear" &shape=="concave"]
    ggplot(df_temp[sample(nrow(df_temp), 1e4)])+
      geom_point(aes(x=forest_p_real, y=biodiversity_output, color=sub.type))+
      facet_wrap(~type)
    
    table(lands_species_df$sp_id)
    lands_species_sum<-lands_species_df[,.(biodiversity_output=sum(biodiversity_output),
                                           N_sp=.N),
                                        by=list(forest_p, rep, land.type,
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

## verfication
if (F)
{
  temp <- lands_species_all[5542450,]
  temp$N_core*temp$biodiversity_output_core + temp$N_edge*temp$biodiversity_output_edge + temp$N_crop*temp$biodiversity_output_crop
  temp$biodiversity_output
  
  core_p <- 1
  lamda_f <- core_p * temp$biodiversity_output_core + (1-core_p) * temp$biodiversity_output_edge
  temp$N_crop*temp$biodiversity_output_crop + (10000-temp$N_boundard-temp$N_crop) * lamda_f
  temp$core1.0
  
  data.5000 <- lands_species_all[lands_species_all$total_product == 5000
                                 & lands_species_all$final.type == "loser.linear convex forest",] 
  ## exponential.loser convex, hump left, loser.linear convex forest
}