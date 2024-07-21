library(data.table)
setwd("C:/Users/qiaoh/GIT/LSS_Project/lss")

yield.function<-function(type="linear", parameters=list()){
  if (type=="linear"){
    f<-function(yield=0){
      yield
    }
  }
  f
}
sp.conf<-list(yield.function=yield.function(type="linear"))
sp.conf[["fc.i"]]<-sp.conf$yield.function(0)
sp.conf[["fc.e"]]<-sp.conf$yield.function(0.05)

sp.list<-list(sp.conf)

