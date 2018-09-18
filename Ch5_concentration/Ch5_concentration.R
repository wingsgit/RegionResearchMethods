##  5  产业地理集中与地区专业化

# 读入数据
setwd("D:/regionResearchMethods/Ch5_concentration")
industries<-read.csv("provincialIndustries2009.csv",
                     header=T,stringsAsFactors=F)
industries1<-as.matrix(industries[,-1:-2])
rownames(industries1)<-industries[,1]

#  计算产业在各省份空间分布的变异系数CV
industries.cv<-apply(industries1,2,
                     function(x) sd(x)/mean(x))
industries.cv

#  计算CR5
industries.cR5<-apply(industries1,2,
                      function(x) x/sum(x))
industries.cR5<-apply(industries.cR5,2,function(x) 
  sum(x[order(x,decreasing=T)[1:5]]))
industries.cR5


# 计算区位基尼系数
gini <- function(x, correct=FALSE) {  # 定义基尼系数计算函数，x需为数值向量
  n = length(x)
  fun = function(a,b) abs(a-b)
  ifelse(correct,sum(outer(x, x, fun))/(2*n*(n-1)*mean(x)),
         sum(outer(x, x, fun))/(2*n^2*mean(x)))
}
industries.gini<-apply(industries1,2,gini,correct=T)
industries.gini

# 利用ineq包来计算基尼系数
library(ineq) 
industries.gini2=apply(industries1,2,Gini,corr=T) 

# 计算相对基尼系数
pp=apply(industries1,1,function(x) 
  sum(x)/sum(industries1))
pp=apply(industries1,2,function(x) x/sum(x)/pp)
industries.gini.r=apply(pp,2,Gini,corr=T)
industries.gini.r

# 计算绝对地理集中指数
industries.MHHI<-apply(industries1,2,function(x) 
  sqrt(mean((x/sum(x))^2)))
industries.MHHI

# 计算相对地理集中指数
pp=apply(industries1,1,function(x) 
  sum(x)/sum(industries1))
industries.HKMT<-apply(industries1,2,function(x) 
  sqrt(mean((x/sum(x)-pp)^2)))
industries.HKMT

# 计算空间分散度指数
sp.distance<-read.csv("31个省市自治区的铁路里程表.csv",
                      header=T,stringsAsFactors=F)
sp.distance<-as.matrix(sp.distance[,-1])
diag(sp.distance)<-0
sp.distance<-sp.distance/sum(sp.distance)
industries.sp=apply(industries1,2,function(x) 
  x/sum(x))
fun.sp<-function(x) 100*sum(outer(x,x,"*")*sp.distance)
industries.sp<-apply(industries.sp,2,fun.sp)
industries.sp

# 计算泰尔指数
library(ineq)
industries.theil=apply(industries1,2,Theil)
industries.theil

# 各产业地理集中指数的综合比较
industries.indexes<-cbind(industries.cv, industries.cR5, 
                          industries.gini, industries.gini.r, 
                          industries.MHHI, industries.HKMT, 
                          industries.sp, industries.theil)
colnames(industries.indexes)<-c("cv","cR5","gini","gini.r",
                                "MHHI","HKMT","sp","theil")
write.csv(industries.indexes,"concentration_indexes.csv")
write.csv(cor(industries.indexes),"concentration.cor.csv")

#  计算地区专业化CV
industries2<-as.matrix(industries[,-(1:2)])
rownames(industries2)<-industries[,1]
specialization.cv<-apply(industries2,1,
                         function(x) sd(x)/mean(x))

#  计算地区专业化CR5
specialization.cR5<-apply(industries2,1,
                          function(x) x/sum(x))
specialization.cR5<-apply(specialization.cR5,2,function(x) 
  sum(x[order(x,decreasing=T)[1:5]]))

# 计算地区专业化基尼系数
specialization.gini=apply(industries2,1,Gini,corr=T)

# 计算地区专业化相对基尼系数
ss=apply(industries2,2,function(x) 
  sum(x)/sum(industries2))
ss1=apply(industries2,1,function(x) x/sum(x)/ss)
specialization.gini.r=apply(ss1,2,Gini,corr=T)

# 计算地区专业化泰尔指数
specialization.theil=apply(industries2,1,Theil)

# 计算地区专业化赫芬达尔指数
specialization.HHI<-apply(industries2,1,function(x) 
  sum((x/sum(x))^2))

# 地区专业化各指数的综合比较
specialization.indexes<-cbind(specialization.cv, 
                              specialization.cR5, 
                              specialization.gini, 
                              specialization.gini.r, 
                              specialization.theil, 
                              specialization.HHI)
colnames(specialization.indexes)<-c("cv","cR5","gini",
                                    "gini.r","theil","HHI")
# 计算最大5值和最小5值
max5.min5<-apply(specialization.indexes,2,function(x) 
{aa=x[order(x,decreasing=T)[1:5]];
 aa.names<-row.names(specialization.indexes)[order(x,decreasing=T)[1:5]];
 bb=x[order(x)[1:5]];
 bb.names<-row.names(specialization.indexes)[order(x)[1:5]];
 data.frame(province=c(aa.names,bb.names),max5.min5=c(aa,bb))})
max5.min5<-do.call(cbind,max5.min5)
row.names(max5.min5)<-c(paste("max",1:5,sep=""),paste("min",1:5,sep=""))
write.csv(max5.min5,"sp.max5.min5.csv")

write.csv(cor(specialization.indexes),"specialization.cor.csv")
write.csv(specialization.indexes,"specialization_indexes.csv")