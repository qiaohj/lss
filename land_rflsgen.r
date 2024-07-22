library(rflsgen)
library(landscapemetrics)
library(terra)
library(data.table)
library(ggplot2)
setwd("C:/Users/qiaoh/GIT/LSS_Project/lss")





cls_forest <- flsgen_create_class_targets(
  "Forest",
  NP = c(1, 100),
  AREA = c(10, 2000),
  PLAND = c(10, 15),
  DIVI = c(0.98, 0.99)
)

cls_crop <- flsgen_create_class_targets(
  "CROP",
  NP = c(1, 100),
  AREA = c(5000, 6000)
)

ls_targets <- flsgen_create_landscape_targets(100, 100, list(cls_forest))
structure <- flsgen_structure(ls_targets)
r<-rflsgen::flsgen_generate(structure)
plot(r)

lsm_c_division(r)

points<-data.table(as.data.frame(r, xy=T))
points$lyr.1<-0
points[between(x,points[10]$x, points[60]$x)]$lyr.1<-1
values(r)<-points$lyr.1
plot(r)

lsm_c_division(r)

