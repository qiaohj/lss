library(ggplot2)
library(data.table)
library(terra)
library(ggpubr)
# setwd("C:/Users/qiaoh/GIT/LSS_Project/lss")
rm(list=ls())
resolution<-100
# species.pool.50.by.type<-readRDS("../Data/species.pool.50.by.type.rda")
## all_hill,all_conf, all_linear,all_hump


species_pool <- readRDS("../Data/species/all_exponential.rda")

lands<-readRDS("../Data/land/land_conf.rda")
lands$land_id<-c(1:nrow(lands))
hist(lands$forest_p_real)
total_product<-seq(0, 10000, by=1000)
product<-5000


sampling_core <- sampling_edge<-seq(0, 1, by=0.1)
#coms.sampling<-data.table(expand.grid(core=sampling_core, edge=sampling_edge))
#coms.sampling <- coms.sampling[core+edge == 1,]
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
                              # sampling_core=lands_item$biodiversity_output_core * coms.sampling$core[j],
                              # sampling_edge=lands_item$biodiversity_output_edge * coms.sampling$edge[j],
                              sampling_core=lands_item$biodiversity_output_core,
                              sampling_edge=lands_item$biodiversity_output_edge,
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
                              final.type=paste(item$type,item$shape),
                              sub.type=item$sub.type,
                              sp_id=item$ID,
                              rep=lands_item$rep)
      sample_item$N_core_sampled<-sample_item$core_p * sample_item$N_core
      sample_item$N_edge_sampled<-sample_item$edge_p * sample_item$N_edge
      sample_item$N_forest_sampled<-sample_item$N_core_sampled + sample_item$N_edge_sampled
      
      # sample_item<-sample_item[N_forest_sampled>=10 & N_crop>=10]
      
      ## lamda forest
      sample_item$sampling_forest_mean <- (sample_item$sampling_core + sample_item$sampling_edge)/2
      sample_item$sampling_forest_weighted <- sample_item$sampling_core * sample_item$core_p + sample_item$sampling_edge * sample_item$edge_p
      
      # sample_item$biodiversity_output<-sample_item$sampling_crop+sample_item$sampling_core+sample_item$sampling_edge
      sample_item$biodiversity_output_mean <- sample_item$sampling_forest_mean * (sample_item$N_core + sample_item$N_edge) + sample_item$sampling_crop * sample_item$N_crop
      sample_item$biodiversity_output_weighted <- sample_item$sampling_forest_weighted * (sample_item$N_core + sample_item$N_edge) + sample_item$sampling_crop * sample_item$N_crop
      
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
  saveRDS(lands_species_df, sprintf("../Data/land_species/lands_all_exponential_%d.rda", product))
  saveRDS(lands_species_sampled, sprintf("../Data/land_species/lands_species_sampled_all_exponential_%d.rda", product))
  
  #lands_species_df[a==lands_species_df[1]$a & rep==1
  #                 & forest_p %in% c(0.2, 0.5)
  #                 & block_size ==20]
  
  if (F){
    lands_species_df<-readRDS(sprintf("../Data/land_species/lands_all_exponential_%d.rda", product))
    lands_species_sampled<-readRDS(sprintf("../Data/land_species/lands_species_sampled_all_exponential_%d.rda", product))
    sp_curves<-readRDS("../Data/species/species.curve.exponential.rda") 
    #exponential.loser convex
   sample_spid=100
   
   species_pool[a==sp_curves[sp_id==sample_spid]$a[1]]
   
   sample_spid<-species_pool[a==sp_curves[sp_id==sample_spid]$a[1]]$ID
   
   sp_curves_item<-sp_curves[sp_id%in%sample_spid]
   p1<-ggplot(sp_curves_item)+geom_line(aes(x=yield, y=v, group=label, color=shape, linetype=type))
   p1
   land_rasters<-rast("../Data/land/land_rasters.tif")
   forest_per<-0.2
   lands_species_item<-lands_species_df[forest_p ==0.2]
   #lands_species_item<-lands_species_item[block_size.x==20 & block_size.y==20]
   lands_species_item<-lands_species_item[sp_id %in% sample_spid]
   #lands_species_item<-lands_species_item[rep==1]
   lands_species_sampled_item<-lands_species_sampled[forest_p==0.2]
   #lands_species_sampled_item<-lands_species_sampled[rep==1]
   lands_species_sampled_item<-lands_species_sampled_item[land_id %in% lands_species_item$land_id]
   lands_species_sampled_item<-lands_species_sampled_item[sp_id %in% sample_spid]
   lands_species_sampled_item<-lands_species_sampled_item[N_forest_sampled>=10 & N_crop >=10]
   #lands_species_sampled_item<-lands_species_sampled_item[rep==1]
   lands_species_item<-lands_species_item[land_id %in% unique(lands_species_sampled_item$land_id)]
   points<-data.frame(rasterTopoints(land_rasters[[unique(lands_species_item[rep==1]$land_id)]]))
   pointlist<-list()
   for (i in c(3:ncol(points))){
      item_p<-points[,c(1, 2, i)]
      colnames(item_p)[3]<-"v"
      item_p$rep<-i-2
      pointlist[[length(pointlist)+1]]<-item_p
   }
   pointdf<-rbindlist(pointlist)
   p3<-ggplot(pointdf)+geom_tile(aes(x=x, y=y, fill=factor(v)))+
     coord_equal()+
     facet_wrap(~rep)
   
   points<-data.frame(rasterTopoints(land_rasters[[c(8491, 8601)]]))
   pointlist<-list()
   for (i in c(3:ncol(points))){
     item_p<-points[,c(1, 2, i)]
     colnames(item_p)[3]<-"v"
     item_p$rep<-i-2
     pointlist[[length(pointlist)+1]]<-item_p
   }
   pointdf<-rbindlist(pointlist)
   p5<-ggplot(pointdf[])+geom_tile(aes(x=x, y=y, fill=factor(v)))+
     coord_equal()+
     facet_wrap(~rep)
   #p3
   
   item1<-unique(lands_species_sampled_item[, c("core_p", "edge_p", 
                                                "type", "shape", 
                                                "biodiversity_output_weighted",
                                                "rep", "land_id",
                                                "N_edge", "N_core")])
   colnames(item1)[5]<-"biodiversity_output"
   item1$group<-"biodiversity_output_weighted"
   
   item2<-unique(lands_species_sampled_item[, c("core_p", "edge_p", 
                                                "type", "shape", 
                                                "biodiversity_output_mean",
                                                "rep", "land_id",
                                                "N_edge", "N_core")])
   colnames(item2)[5]<-"biodiversity_output"
   item2$group<-"biodiversity_output_mean"
   
   item3<-unique(lands_species_item[, c("N_core", "N_edge", 
                                        "type", "shape", 
                                        "biodiversity_output",
                                        "rep", "land_id")])
   item3$core_p<-item3$N_core/(item3$N_core + item3$N_edge)
   item3$edge_p<-item3$N_edge/(item3$N_core + item3$N_edge)
   
   item3$group<-"biodiversity_output_real"
   df_p<-rbindlist(list(item1, item2, item3), use.names = T, fill=T)
   test_lands<-unique(df_p[rep==1]$land_id)
   test_lands<-test_lands[sample(length(test_lands), 10)]
   p2<-ggplot(df_p[land_id %in% test_lands])+ 
     geom_boxplot(aes(x=type, y=biodiversity_output, fill=group))+
     labs(title=paste("forest", 0.2))+
     facet_wrap(~shape+land_id, scale="free")
   p2
   
   p6<-ggplot(df_p[land_id %in% test_lands])+ 
     geom_boxplot(aes(x=type, y=biodiversity_output, fill=group))+
     labs(title=paste("forest", 0.2))+
     facet_wrap(~shape, scale="free")
   p6
   unique(df_p$biodiversity_output)
   p7<-ggplot(df_p[group!="biodiversity_output_weighted" &
                     rep==1])+ 
     geom_point(aes(x=N_edge, y=biodiversity_output, color=group))+
     labs(title=paste("forest", 0.2))+
     facet_wrap(~shape+type, scale="free")
   p7
   
   
   ggsave(p2, filename="../Figures/test.land.png", width=20, height=20)
   df_p[core_p==0 & rep==1]
   p4<-ggplot(df_p)+ 
     geom_point(aes(x=core_p, y=biodiversity_output, color=group))+
     #geom_line(aes(x=core_p, y=biodiversity_output, color=group, group=rep))+
     labs(title=paste("forest", 0.2))+
     facet_wrap(~shape+type, scale="free")
   p4
   
   p<-ggarrange(plotlist=list(p1, p5))
   p<-ggarrange(plotlist=list(p, p2), nrow=2)
   p
   
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