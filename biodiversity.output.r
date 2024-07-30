library(ggplot2)
library(data.table)
# setwd("C:/Users/qiaoh/GIT/LSS_Project/lss")

resolution<-100
# species.pool.50.by.type<-readRDS("../Data/species.pool.50.by.type.rda")
## all_hill,all_conf, all_linear,all_hump
species_pool <- readRDS("../Data/species/all_linear.rda")

lands<-readRDS("../Data/land_conf.rda")
hist(lands$forest_p_real)
total_product<-seq(0, 10000, by=1000)
product<-8000
lands_species_list<-list()

sampling_core <- sampling_edge<-seq(0, 1, by=0.1)
coms.sampling<-data.table(expand.grid(core=sampling_core, edge=sampling_edge))
coms.sampling <- coms.sampling[core+edge == 1,]

for (product in total_product){
  print(product)
  # species_pool<-species.pool.50.by.type
  species_pool<-species_pool
  
  lands_species<-list()
  for (i in c(1:nrow(species_pool))){
    #print(paste(i, nrow(species_pool)))
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
    
    ## 
    # biodiversity_output_sampling_bias <- list()
    biodiversity_output_sampling_bias <- NA
    sampling_crop <- lands_item$biodiversity_output_crop * lands_item$N_crop ## no sampling bias in crop
    j=1
    for (j in 1:nrow(coms.sampling))
    {
      biodiversity_output_sampling <- 
        sampling_crop + 
        lands_item$biodiversity_output_core * coms.sampling$core[j] * (lands_item$N_edge+lands_item$N_core) +
        lands_item$biodiversity_output_edge * coms.sampling$edge[j] * (lands_item$N_edge+lands_item$N_core)
      biodiversity_output_sampling_bias <- data.frame(cbind(biodiversity_output_sampling_bias,biodiversity_output_sampling))
    }
    biodiversity_output_sampling_bias <- biodiversity_output_sampling_bias[,2:12]
    # biodiversity_output_sampling_biasdata.frame(biodiversity_output_sampling_bias)
    colnames(biodiversity_output_sampling_bias) <- c("core1.0","core0.9","core0.8","core0.7",
                                                     "core0.6","core0.5","core0.4","core0.3",
                                                     "core0.2","core0.1","core0.0")
    lands_item <- cbind(lands_item,biodiversity_output_sampling_bias)
    #### sampling based end
    
    lands_item<-lands_item[yield<=1]
    if (F){
      plot(lands_item$biodiversity_output, lands_item$yield)
    }
    lands_item$type<-item$type
    lands_item$a<-item$a
    lands_item$alpha <-item$alpha 
    lands_item$beta <-item$beta 
    lands_item$sub.type<-item$sub.type
    lands_item$final.type<-item$final.type
    lands_item$total_product<-product
    lands_item$speciesID<-item$speciesID
    
    lands_species[[i]]<-lands_item
  }
  
  lands_species_df<-rbindlist(lands_species)
  lands_species_list[[length(lands_species_list)+1]]<-lands_species_df
  #lands_species_df[a==lands_species_df[1]$a & rep==1
  #                 & forest_p %in% c(0.2, 0.5)
  #                 & block_size ==20]
  
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
lands_species_all<-rbindlist(lands_species_list)
saveRDS(lands_species_all, "../Data/land_species/lands_all_linear.rda")
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