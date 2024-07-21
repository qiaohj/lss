library(sp)
library(landscapemetrics)
library(terra)
library(data.table)

setwd("C:/Users/qiaoh/GIT/LSS_Project/lss")
resolution<-100


forest_p<-0.25

block_size<-1


gen_land<-function(resolution, forest_p, block_size, n_rep=1){
  x.from<-y.from<-seq(1, resolution, by=block_size)
  x.to<-y.to<-seq(block_size, resolution, by=block_size)
  
  xy<-data.table(cbind(expand.grid(x.from=x.from, y.from=y.from),
                       expand.grid(x.to=x.to, y.to=y.to)))
  
  n.groups<-nrow(xy)
  xy$group<-c(1:n.groups)
  
  n.forest<-round(n.groups * forest_p)
  
  forest_group<-sample(n.groups, n.forest)
  
  xy$landuse<-0
  xy[group %in% forest_group]$landuse<-1
  land_rasters<-list()
  conf<-list()
  for (rep in c(1:n_rep)){
    m <- matrix(0, nrow=resolution, ncol=resolution)
    for (i in c(1:nrow(xy))){
      if (xy[i]$landuse==1){
        m[between(col(m), xy[i]$x.from, xy[i]$x.to) &
            between(row(m), xy[i]$y.from, xy[i]$y.to)]<-1
      }
    }
    rm <- rast(m)
    #plot(rm)
    core<-data.table(lsm_c_cpland(rm))
    forest_core_p<-core[class==1]$value/100
    crop_core_p<-core[class==0]$value/100
    forest_edge_p<-forest_p-forest_core_p
    conf[[rep]]<-data.table(forest_core_p=forest_core_p, 
                     crop_core_p=crop_core_p, 
                     forest_edge_p=forest_edge_p,
                     rep=rep
                     )
    land_rasters[[rep]]<-rm
  }
  conf_df<-rbindlist(conf)
  
  list(conf=conf_df, land_rasters=land_rasters)
}
