library(landscapemetrics)
library(terra)
library(data.table)
library(ggplot2)

# setwd("/media/huijieqiao/WD22T_50/lss/lss")


gen_land<-function(resolution, forest_p, block_size, n_rep=1){
  x.from<-seq(1, resolution, by=block_size$x)
  y.from<-seq(1, resolution, by=block_size$y)
  x.to<-seq(block_size$x, resolution, by=block_size$x)
  y.to<-seq(block_size$y, resolution, by=block_size$y)
  x.from<-x.from[1:length(x.to)]
  y.from<-y.from[1:length(y.to)]
  land_rasters<-list()
  conf<-list()
  for (rep in c(1:n_rep)){
    xy<-data.table(cbind(expand.grid(x.from=x.from, y.from=y.from),
                         expand.grid(x.to=x.to, y.to=y.to)))
    
    n.groups<-nrow(xy)
    n.forest<-round(n.groups * forest_p)
    forest_group<-sample(n.groups, n.forest)
    xy$group<-c(1:n.groups)
    xy$landuse<-0
    xy[group %in% forest_group]$landuse<-1
    
    m <- matrix(0, nrow=resolution, ncol=resolution)
    xy<-xy[landuse==1]
    if (nrow(xy)>0){
      for (i in c(1:nrow(xy))){
        m[between(col(m), xy[i]$x.from, xy[i]$x.to) &
            between(row(m), xy[i]$y.from, xy[i]$y.to)]<-1
      }
    }
    
    rm <- rast(m)
    m_forest<-m
    m_forest[m_forest==0]<-NA
    rm_forest<-rast(m_forest)
    #plot(rm)
    boundary<-boundaries(rm_forest)
    boundary.points<-rasterTopoints(boundary, colname="edge")
    land_points<-rasterTopoints(rm, colname="land")
    land_points$boundary<-ifelse(land_points$x %in% c(1, resolution) |
                                   land_points$y %in% c(1, resolution),
                            T, F)
    points<-merge(land_points, boundary.points, by=c("x", "y"), all=T)
    points[is.na(edge)]$edge<-0
    N_point<-points[, .(N=.N), by=list(land, boundary, edge)]
    N_edge<-N_point[land==1 & boundary==F & edge==1]$N
    N_core<-N_point[land==1 & boundary==F & edge==0]$N
    if (length(N_edge)==0){
      N_edge<-0
    }
    if (length(N_core)==0){
      N_core<-0
    }
    #table(values(boundary))
    #core<-data.table(lsm_c_cpland(rm))
    #forest_core_p<-core[class==1]$value * (resolution^2)
    #crop_core_p<-core[class==0]$value * (resolution^2)
    #boundary_p<-((resolution-1)*4)/(resolution^2)
    #forest_edge_p<-forest_p-forest_core_p-
    N_boundard<-resolution * 4 - 4
    N_crop<-resolution ^ 2 - N_core - N_edge - N_boundard
    land.type<-paste(forest_p, block_size$x, block_size$y)
    
    conf[[rep]]<-data.table(N_core=N_core, 
                            N_edge=N_edge,
                            N_crop=N_crop,
                            N_boundard=N_boundard,
                            rep=rep,
                            resolution=resolution, 
                            forest_p=forest_p, 
                            block_size.x=block_size$x,
                            block_size.y=block_size$y,
                            n.groups=n.groups,
                            n.forest.groups=n.forest,
                            land.type=land.type,
                            forest_p_real=(N_core+N_edge)/(N_core+N_edge+N_crop)
                            
                            
    )
    land_rasters[[rep]]<-rm
  }
  conf_df<-rbindlist(conf)
  
  list(conf=conf_df, land_rasters=land_rasters)
}

rasterTopoints<-function(r, colname="col"){
  points<-data.table(as.data.frame(r, xy=T))
  points$x<-ceiling(points$x)
  points$y<-ceiling(points$y)
  colnames(points)[3]<-colname
  points
}

if (F){
  resolution<-100
  
  
  forest_p<-seq(0, 1, 0.1)
  
  block_size_item<-c(1, seq(10, resolution, by=10))
  
  conf_land<-data.table(expand.grid(forest_p=forest_p, 
                                    block_size.x=block_size_item,
                                    block_size.y=block_size_item
  ))
  land_rasters<-list()
  land_conf<-list()
  land_conf$ed <- NA
  i=200
  for (i in c(1:nrow(conf_land))){
    print(paste(i, nrow(conf_land), 
                conf_land[i]$forest_p,
                conf_land[i]$block_size.x,
                conf_land[i]$block_size.y))
    result<-gen_land(resolution=resolution, 
                     forest_p=conf_land[i]$forest_p,
                     block_size=list("x"=conf_land[i]$block_size.x,
                                     "y"=conf_land[i]$block_size.y),
                     n_rep=10)
    
    land_conf[[i]]<-result$conf
    land_rasters[[i]]<-result
    
    if (F){
      result<-gen_land(resolution=resolution, 
                       forest_p=forest_p,
                       block_size=block_size,
                       n_rep=10)
      
      all_points<-list()
      for (k in c(1:length(result$land_rasters))){
        pp<-rasterTopoints(result$land_rasters[[k]])
        pp$rep<-k
        all_points[[k]]<-pp
      }
      all_points<-rbindlist(all_points)
      ggplot(all_points)+geom_tile(aes(x=x, y=y, fill=factor(col)))+
        facet_wrap(~rep, nrow=2)
    }
  }
  land_conf<-rbindlist(land_conf)
  saveRDS(land_conf, "../Data/land/land_conf.rda")
  saveRDS(land_rasters, "../Data/land/land_rasters.rda")
  all_raster<-list()
  for (i in c(1:length(land_rasters))){
    for (j in c(1:length(land_rasters[[i]]$land_rasters))){
      all_raster[[length(all_raster)+1]]<-land_rasters[[i]]$land_rasters[[j]]
    }
  }
  stack_r<-rast(all_raster)
  writeRaster(stack_r, "../Data//land/land_rasters.tif", overwrite=TRUE)
  if (F){
    land_conf<-readRDS("../Data/land/land_conf.rda")
    land_rasters<-rast("../Data/land/land_rasters.tif")
    if (F){
      land_conf$ID<-c(1:nrow(land_conf))
      # land_conf[forest_p==0.2 & block_size.x==10 & rep==1]
      land_conf[forest_p_real>=0.19 & forest_p_real<=0.21 & rep==1]
      plot(land_rasters[[2551]])
    }
    # land_conf$lsm_c_division_crop<- -1
    # land_conf$lsm_c_division_forest<- -1
    land_conf$ed<- -1
    
    for (i in c(1:nrow(land_conf))){
      print(paste(i, nrow(land_conf)))
      r<-land_rasters[[i]]
      # divi<-data.table(lsm_c_division(r))
      # if (nrow(divi[class==0])>0){
      #   land_conf[i]$lsm_c_division_crop<-divi[class==0]$value
      # }
      # if (nrow(divi[class==1])>0){
      #   land_conf[i]$lsm_c_division_forest<-divi[class==1]$value
      # }
      ed <- data.table(lsm_l_ed(r))
      land_conf[i]$ed<-ed$value
    }
    saveRDS(land_conf, "../Data/land/land_conf.rda")
  }
  
  if (F){
    
    ggplot(land_conf)+geom_point(aes(x=N_core, y=N_edge, color=factor(forest_p)))+
      facet_wrap(~block_size, nrow=3, scale="free_y")
  }
}
