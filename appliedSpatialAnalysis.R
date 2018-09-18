#  2.3.1   生成空间权重矩阵

swm<-read.table("D:/regionResearchMethods/Ch2_ArcGIS/provincialPcGDP/pswm.txt",header=T,sep=",")
swm[[3]]<-swm[[3]]+1; swm[[4]]<-swm[[4]]+1
n=length(unique(swm[[3]]))
swmmat<-matrix(0,n,n)
apply(swm,1,function(x) swmmat[x[3],x[4]]<<-x[5])
swmmat


## R语言在空间数据分析中的应用

#  4.1.1   R介绍

help(read.table)
? read.table

help.search("spatial statistics")
??gal

#  4.1.2   R中的数据对象

#向量（vector）

c(1,4.5,91)
c("a","e")
c(T,T,F)

vector(mode = "logical", length = 3)
aa=character(5); aa

mode(aa)
length(aa)

1:5

seq(from=3.5,to=6.5,by=0.5)
seq(2,8,length=5)
bb=rep(1:3,3); cc=rep(c("a","b"),each=2)

bb[2]
bb[c(1,3)]
(cc[-(1:3)]<-4)
cc[c(F,T)]

cc[bb>1]

mean(bb)
sum(bb)
paste(bb[1:2],cc[1:4],sep=" and ")

dd<-c(6,5,10,8,9)
sort(dd)
order(dd)
rank(dd)

ee<-c("Tom","Jerry","Porter","Jefferson")
sort(ee)

# 因子
f1<-rep(c("bad","normal","good"),1:3)
f1
f1<-as.factor(f1)
f1
levels(f1)
f1<-ordered(f1,levels=c("bad","normal","good"))
f1

# 矩阵
matrix(1:6,2,3,
       dimnames=list(c("r1","r2"),c("c1","c2","c3")))
m1=1:6; dim(m1)=c(2,3)
rownames(m1)=c("r1","r2")
colnames(m1)= c("c1","c2","c3")
m1[2,3]
m1[2,]
m1[,3]
m1[,-(2:3)]
m1["r1",]
dimnames(m1)=NULL
m1[m1<3]=6
m1[,m1[1,]>4]

# 数阵
dim1=paste("a",1:2,sep="")
dim2=paste("b",1:3,sep="")
dim3=paste("c",1:2,sep="")
myarray=array(1:12,c(2,3,2),
                dimnames=list(dim1,dim2,dim3))
myarray[2,3,2]
myarray[2,3,]
myarray[2,,]

# 列表
list1<-list(ID=1:2,names=c("Tom","Jerry"),
            species=list("cat","mouse"))
names(list1)
list1["names"]
list1[["names"]]
list1[[3]]
list1$names

# 数据框
mydf<-data.frame(ID=1:2,names=c("Tom","Jerry"),
                 species=c("cat","mouse"))
names(mydf)
dim(mydf)
mydf[2]
mydf[[2]]
mydf[1,3]
mydf[,"names"]
mydf$species

mydata=read.csv("D:/regionResearchMethods/Ch4_R/industrialProduction.csv",
                header=T,stringsAsFactors=F)
mode(mydata)
class(mydata)
summary(mydata)
head(mydata)
mydata[mydata$product>20000,]
subset(mydata,product>20000,
       select=c(province,product))

# 其他
length(NA)
length(NULL)
c(1,NA)
c(1,NULL)

# 4.1.3  运算与函数

myf=function(x,y) {x/2+y}
aa=1:3
bb=4:6
myf(aa,bb)
aa=c(3,8,5,4,9,1)
bb=character(length(aa))    # 生成字符型向量bb，且与aa等长
for(i in 1:length(aa)) {    # for循环，指针i从1遍历到length(aa)，即6
  if(aa[i]%%2) bb[i]="odd"  # 若aa[i]除以2得到1，即逻辑真值T，则将bb第i个元素赋值为"odd"
  else bb[i]="even"         # 若aa[i]除以2得到其他值，此处为0，即逻辑假值F，则将bb第i个元素赋值为"even"
}

aa=c(3,8,5,4,9,1)
bb=ifelse(aa%%2,"odd","even")

aa=c(3,8,5,4,9,1)
bb=character(length(aa))
i=1
while(i<=length(aa)) {
  if(aa[i]%%2) bb[i]="odd"
  else bb[i]="even"
  i=i+1
}

aa=matrix(1:6,2); aa[2,3]=NA
apply(aa,1,mean,na.rm=T)

apply(aa,1,is.na)

aa=c(3,8,5)
lapply(aa,function(x) ifelse(x%%2,"odd","even"))
sapply(aa,function(x) ifelse(x%%2,"odd","even"))

mydata=read.csv("D:/regionResearchMethods/Ch4_R/industrialProduction.csv",
                header=T,stringsAsFactors=F)
sapply(mydata,class)
sapply(mydata[-1],mean,na.rm=T)

# 4.1.4  R图形
plot(product~assets, data=mydata)
abline(lm(product~assets,data=mydata))

library(ggplot2)
qplot(assets,product,data=mydata)

4.2  读入数据与生成空间权重

install.packages("spdep")
library(spdep)

#  4.2.2  生成空间权重对象

setwd("D:/regionResearchMethods/Ch4_R")
library(spdep)
gal<-read.gal("Province31.gal")
gwt<-read.gwt2nb("Province31.gwt")
summary(gal)

str(gal)
gal[[1]]
gwt[[1]]
attr(gwt, "GeoDa")$dist[[1]]
class(gal)
class(gwt)

gal.mat<-nb2mat(gal)
gal.mat[1,15]

dist<-attr(gwt, "GeoDa")$dist
for(i in 1:31) dist[[i]]<-append(dist[[i]],0,after=i-1)
gwt.mat=do.call(rbind,dist)

gwt.mat[22,30]
all(gwt.mat==t(gwt.mat))
all(diag(gwt.mat)==0)

diag(gwt.mat)<-1  # 使矩阵对角线元素非0，便于取倒数
any(gwt.mat==0)   # 判断矩阵中是否还有0元素
gwt.mat<-1/gwt.mat
diag(gwt.mat)<-0  # 将矩阵对角线元素还原为0
gwt.mat<-t(apply(gwt.mat,1,function(x) x/sum(x)))  # 矩阵行标准化

shp.data<-read.csv("shp.csv",header=T,stringsAsFactors=F)
mydata<-read.csv("industrialProduction.csv",
                 header=T,stringsAsFactors=F)

setequal(shp.data$NAME,mydata$province)
dimnames(gal.mat)<-list(shp.data$NAME,shp.data$NAME)
dimnames(gwt.mat)<-list(shp.data$NAME,shp.data$NAME)
gal.mat<-gal.mat[mydata$province,mydata$province]
gwt.mat<-gwt.mat[mydata$province,mydata$province]
write.csv(gal.mat,"gal_mat.csv")
write.csv(gwt.mat,"gwt_mat.csv")

product.lag<-gal.mat %*% mydata$product
product.lag2<-lag.listw(mat2listw(gal.mat),mydata$product)
all(product.lag==product.lag2)

# 4.3  spdep空间自相关分析

lapply(mydata[2:4],moran.test,listw=mat2listw(gal.mat))
moran.test(mydata$product,listw=mat2listw(gwt.mat))

# 莫兰指数随机排列模拟
xx<-replicate(999,sample(mydata$product))
moran999<-apply(xx,2,function(x) 
               {aa=moran.test(x,mat2listw(gal.mat));
                return(aa$estimate["Moran I statistic"])})
moran1<-moran.test(mydata$product,
                   listw=mat2listw(gal.mat))$estimate["Moran I statistic"]
morans<-c(moran999,moran1)

hist(morans,freq=F,breaks=100)
lines(density(morans))
abline(v=moran1)
rank(morans)[1000]
moran.mc(mydata$product,
         listw=mat2listw(gal.mat),nsim=999)

moran.plot(mydata$product,listw=mat2listw(gal.mat),
           xlab="product",ylab="product.slag")

lisa=localmoran(mydata$product,mat2listw(gal.mat))
which(lisa[,"Pr(z > 0)"]<0.05)

# 查看ln(product)的空间自相关性
moran.test(log(mydata$product),listw=mat2listw(gal.mat))


# 4.4  spdep空间回归分析

# 进行普通最小二乘分析
fm=log(product)~log(assets)+log(labors)
lm.results<-lm(fm,data=mydata)
summary(lm.results)

# OLS残差项的空间自相关检验
moran.test(lm.results$residuals,mat2listw(gal.mat))
lm.morantest(lm.results,listw=mat2listw(gal.mat))

# 进行空间滞后模型分析
s.lag<-lagsarlm(fm,data=mydata,listw=mat2listw(gal.mat))
summary(s.lag)

# 进行空间误差模型分析
s.error<-errorsarlm(fm,mydata,mat2listw(gal.mat))
summary(s.error)

# 进行空间通用模型分析
s.sac<-sacsarlm(fm,mydata,mat2listw(gal.mat))
summary(s.sac)

# 模型比较检验
lm.LMtests(lm.results, listw=mat2listw(gal.mat), test="all")

#  4.5  splm空间面板数据分析
library(splm)
data("Produc", package = "Ecdat")
data("usaww")

head(Produc)
help("Produc", package = "Ecdat")

#  进行普通面板数据分析
fm<-log(gsp) ~ log(pcap) + log(pc) + log(emp)
plm.results<-plm(fm,data=Produc, index=c("state","year"),
                 effect="individual",model="within")
summary(plm.results)

#  进行空间面板数据分析

# 个体随机效应，兼有空间滞后和空间误差
spml.results<-spml(fm,data=Produc,index=c("state","year"),
                   listw=mat2listw(usaww),
                   model="random",effect="individual",
                   lag=T,spatial.error="b")
summary(spml.results)

# 个体固定效应，兼有空间滞后和空间误差
spml.results2<-spml(fm,data=Produc,index=c("state","year"),
                    listw=mat2listw(usaww),
                    model="within",effect="individual",
                    lag=T,spatial.error="b")
summary(spml.results2)

#  空间面板数据模型检验
test.LMH<-bsktest(fm,data=Produc,index=c("state","year"),
               listw=mat2listw(usaww),test="LMH")
test.CLMlambda<-bsktest(fm,data=Produc,index=c("state","year"),
               listw=mat2listw(usaww),test="CLMlambda")
# 广义矩估计
spgm.results <- spgm(fm, data = Produc, listw = mat2listw(usaww), 
                     lag = TRUE,model = "random", spatial.error = TRUE)
summary(spgm.results)
spgm.results2 <- spgm(fm, data = Produc, 
                      listw = mat2listw(usaww), lag = TRUE, 
                      model = "within", spatial.error = TRUE)
summary(spgm.results2)
test.hausman<-sphtest(spgm.results,spgm.results2)


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


##  6 区域经济发展差距及其收敛性分析

# 6.2 中国省份经济发展空间关联分析

# 读入并概览数据
setwd("D:/regionResearchMethods/Ch6_convergence")
pcgdp<-read.csv("PcGDP31.csv",header=T,stringsAsFactors=F)
pcgdp<-subset(pcgdp,select=y1994:y2010)
summary(pcgdp)

# 计算每年各省人均GDP的四分位距
sapply(pcgdp,function(x) 
      diff(quantile(x,probs=c(0.25,0.75)))/mean(x))

# 计算pcgdp在1994-2010年间空间分布的Moran's I
library(spdep)
gal.mat<-read.csv("gal_mat.csv",header=T,
                  stringsAsFactors=F)[-1]   # 读入空间权重矩阵
gal.mat<-as.matrix(gal.mat)
pcgdp.moran<-apply(pcgdp,2,function(x) 
                  {aa=moran.test(x,mat2listw(gal.mat));
                   c(aa$estimate,p.value=aa$p.value)})
pcgdp.moran<-t(pcgdp.moran)

# 绘制历年Moran's I趋势图
library(ggplot2)
qplot(1994:2010,pcgdp.moran[,1],xlab="year",
      ylab="Moran I statistic",
      geom=c("point","line","smooth"),method="lm")

# 6.3 中国各省份经济发展水平差距测度

library(ineq)
methods<-c("Gini","Theil","var")
pcgdp.gaps<-sapply(methods,function(x) 
                   apply(pcgdp,2,ineq,type=x))
pcgdp.hhi<-apply(pcgdp,2,function(x) sum((x/sum(x))^2))
pcgdp.gaps<-cbind(pcgdp.gaps,hhi=pcgdp.hhi)

cor(pcgdp.gaps)
cor(pcgdp.gaps,method = "spearman")

# 6.4 中国各省份经济发展收敛性分析

convergence<-lm(log(y2010/y1994)~log(y1994),data=pcgdp)
summary(convergence)

# 收敛性的空间滞后模型分析

moran.test(with(pcgdp,log(y2010/y1994)),mat2listw(gal.mat))
convergence.slm<-lagsarlm(log(y2010/y1994)~log(y1994),data=pcgdp,
                          listw=mat2listw(gal.mat))
summary(convergence.slm)
convergence.sem<-errorsarlm(log(y2010/y1994)~log(y1994),data=pcgdp,
                          listw=mat2listw(gal.mat))
summary(convergence.sem)

lm.LMtests(convergence, listw=mat2listw(gal.mat), test="all")


## 7 区域知识生产及其溢出分析

# 读入数据与空间权重矩阵
setwd("D:/regionResearchMethods/Ch7_spillovers")
library(splm)
hightech<-read.csv("highTech.csv",header=T,
                   stringsAsFactors=F)  # 读入数据
gal.mat<-read.csv("gal_mat.csv",header=T,
                  stringsAsFactors=F)   # 读入空间权重矩阵
all(gal.mat[,1]==unique(hightech$province))  # 确保数据中的省份排列顺序与空间权重矩阵相同
gal.mat<-as.matrix(gal.mat[,-1])

# 数据图形展示
library(lattice)
xyplot(log(PAT)~log(ERD)|year,data=hightech)
xyplot(log(PAT)~log(HRD)|year,data=hightech)

# 空间自相关分析
hightech.moran<-sapply(2001:2010,function(x) {
  aa=subset(hightech,year==x,PAT)[[1]];
  aa=log(aa);
  bb=subset(hightech,year==x,NPO)[[1]];
  bb=log(bb);
  cc=moran.test(aa,mat2listw(gal.mat));
  dd=moran.test(bb,mat2listw(gal.mat));
  c(cc$estimate,cc$p.value,dd$estimate,dd$p.value)
})
hightech.moran<-t(hightech.moran)
colnames(hightech.moran)<-c(paste("PAT",c("Moran","Expectation","Variance","pvalue"),sep="."),
                            paste("NPO",c("Moran","Expectation","Variance","pvalue"),sep="."))

# 普通面板数据分析
hightech.fm1<-log(PAT) ~ log(ERD) + log(HRD)
hightech.fm2<-log(NPO) ~ log(ERD) + log(HRD)
hightech.reg1<-lapply(list(hightech.fm1,hightech.fm2),plm,
                      data=hightech,index=c("province","year"),
                      effect = "individual",model = "within")
lapply(hightech.reg1,summary)

# 空间面板数据分析

# 随机效应广义矩估计
hightech.reg2<-lapply(list(hightech.fm1,hightech.fm2),spgm,
                      data=hightech,index=c("province","year"),
                      listw=mat2listw(gal.mat),
                      model = "random",
                     lag=T, spatial.error=F)
lapply(hightech.reg2, summary)

# 固定效应广义矩估计
hightech.reg3<-lapply(list(hightech.fm1,hightech.fm2),spgm,
                      data=hightech,index=c("province","year"),
                      listw=mat2listw(gal.mat),
                      model = "within",
                      lag=T, spatial.error=F)
lapply(hightech.reg3, summary)

# 空间面板数据模型检验
sphtest(hightech.fm1,data=hightech,
        index=c("province","year"),
        listw=mat2listw(gal.mat),
        spatial.model = "sarar",
        method = "GM",errors = "BSK")
sphtest(hightech.fm2,data=hightech,
        index=c("province","year"),
        listw=mat2listw(gal.mat),
        spatial.model = "sarar",
        method = "GM",errors = "BSK")


##  8   中国省际贸易的空间互动模型分析

#  8.2  中国省际贸易的描述性分析

setwd("D:/regionResearchMethods/Ch8_tradeFlow")
flow31 <- read.csv("flow29_2011.csv",header=T,
                   stringsAsFactors=F,na.strings = "-")
all(flow31$province==names(flow31)[-1])     #  判断行列的省份顺序是否一致
flow29<-subset(flow31,
               !province %in% c("海南","西藏"),
               -c(海南,西藏))

rownames(flow29)<-flow29$province
outflow<-apply(flow29[,-1],1,sum)
sorted.outflow<-sort(outflow)

inflow<-apply(flow29[,-1],2,sum)
sorted.inflow<-sort(inflow)

netflow<-inflow-outflow
sorted.netflow<-sort(netflow)

inout<-data.frame(province=flow29$province,inflow,outflow,netflow)
write.csv(inout,"inout.csv",row.names=F)

library(ineq)
outflow.gini<-apply(flow29[,-1],1,Gini)    #  计算给定省份下流出量分布的绝对基尼系数

numerator<-t(apply(flow29[,-1],1,function(x) x/sum(x)))    #  以下计算给定省份下流出量分布的相对基尼系数
sum.inflow<-apply(flow29[,-1],2,sum)
denominator<-sum.inflow/sum(sum.inflow)
ratio<-t(apply(numerator,1,function(x) x/denominator))
outflow.gini.relative<-apply(ratio,1,Gini)

library(spdep)    #  计算给定省份下货物流出在其他省份的空间分布的Moran's I
province29.table<-read.csv("province29.csv",
                           header=T,stringsAsFactors=F)  #  注：GeoDa导出的csv文件有大量空格，得先在记事本中消掉。
province29.gal <- read.gal("province29.gal")
province29.swm<-nb2mat(province29.gal)
dimnames(province29.swm)<-list(province29.table$NAME,
                               province29.table$NAME)
province29.swm<-province29.swm[flow29$province,
                               flow29$province]  #  按flow29数据表中的省份顺序重排空间权重矩阵的行与列
moran.outflow<-apply(flow29[,-1],1,moran.test,
                     listw=mat2listw(province29.swm))
estimate<-sapply(moran.outflow,"[[","estimate")    #  提取估计统计量
p.value<-sapply(moran.outflow,"[[","p.value")       #  提取显著性水平
moran.outflow<-rbind(estimate,p.value)
moran.outflow<-t(moran.outflow)

mymat<-matrix(0,29,29)    #  计算给定省份下货物流出在邻近省份的占比
mymat[province29.swm!=0]<-flow29[,-1][province29.swm!=0]
flow29.a<-flow29[,-1]
diag(flow29.a)<-0         #  去掉对角线上省份内部的货运数据
rowSums(mymat)/rowSums(flow29.a)

swm2<-province29.swm
swm2 <- ifelse(swm2!=0,1,0)
oswm <- swm2 %x% diag(29)    #  计算源地相邻下的空间权重矩阵
oswm2<-matrix(0,841,841)    #  根据源地相邻下空间权重矩阵的直接意义计算，比较慢
od.names<-paste(rep(flow29[,"province"],each=29),rep(flow29[,"province"],29),sep="-")
dimnames(oswm2)<-list(od.names,od.names)
for(i in rownames(oswm2)) {
  for(j in colnames(oswm2)) {
    if(strsplit(i,"-")[[1]][2]==strsplit(j,"-")[[1]][2]) {
      if(province29.swm[strsplit(i,"-")[[1]][1],strsplit(j,"-")[[1]][1]]!=0) oswm2[i,j]=1
    }
  }
}
all(oswm==oswm2)
dswm<-diag(29) %x% swm2            #  计算汇地相邻下的空间权重矩阵
odswm<-swm2 %x% swm2            #  计算双重相邻下的空间权重矩阵
o_dswm<-oswm+dswm               #  计算源地或汇地相邻下的空间权重矩阵
o.dswm<-oswm+dswm+odswm         #  计算源地、汇地或双重相邻下的空间权重矩阵

swm.list<-list(oswm=oswm,dswm=dswm,odswm=odswm,
               o_dswm=o_dswm,o.dswm=o.dswm)
row.standize<-function(x) t(apply(x,1,function(y) y/sum(y)))    #  定义矩阵行标准化函数
swm.list<-lapply(swm.list,row.standize)    #  将权重矩阵批量标准化

odflow<-as.vector(t(flow29[,-1]))
moran<-function(x) moran.test(odflow,listw=mat2listw(x))
morans=lapply(swm.list,moran)
rbind(sapply(morans,"[[","estimate"),
      p.value=sapply(morans,"[[","p.value"))

#  8.4  中国省际贸易的空间互动模型分析
province.data<-read.csv("provinceData.csv",
                        header=T,stringsAsFactors=F)
province.data<-subset(province.data,
                      !province %in% c("海南","西藏"))
all(province.data$province==flow29$province)    #  判断属性数据表与流量数据矩阵的省份排序是否一致
names(province.data)<-c("province","gdp","export","import",
                        "heavyindustry","lightindustry",
                        "industryproduct","stateproduct")
province.data.origin<-province.data[rep(1:29,each=29),]
names(province.data.origin)<-paste("o",names(province.data.origin),
                                   sep="")
province.data.destination<-province.data[rep(1:29,29),]
names(province.data.destination)<-paste("d",names(province.data.destination),
                                        sep="")
mydata<-cbind(oprovince=province.data.origin[,1],
              dprovince=province.data.destination[,1],
              flow=odflow,province.data.origin[,-1],
              province.data.destination[,-1],
              stringsAsFactors=F)
mydata[,-(1:2)]<-log(mydata[,-(1:2)])
distance<-read.csv("31个省市自治区的铁路里程表.csv",
                   header=T,stringsAsFactors=F)
prailway<-read.csv("各省铁路营业里程数_2011.csv",
                   header=T,stringsAsFactors=F)
parea<-read.csv("各省份面积.csv",
                header=T,stringsAsFactors=F)
pdistance<-merge(prailway,parea,by="province")
pdistance<-subset(pdistance,!province %in% c("海南","西藏"))
dweights<-with(pdistance,
               sum(railway)/sum(area)/(railway/area))
pdistance$pdistance<-with(pdistance,
                          sqrt(area/pi)*dweights)
mydata$gdistance<-numeric(841)    #  以下生成地理距离
for(i in unique(mydata$oprovince)){
  for(j in unique(mydata$dprovince)){
    if(i==j) mydata[mydata$oprovince==i & mydata$dprovince==j,
                    "gdistance"]<-pdistance[pdistance$province==i,
                                            "pdistance"]
    else mydata[mydata$oprovince==i & mydata$dprovince==j,
                "gdistance"]<-distance[distance$province==i,j]
  }
}
mydata$home<-ifelse(mydata$oprovince==mydata$dprovince,1,0)
write.csv(mydata,"mydata.csv",row.names=F)

summary(mydata)

library(car)
scatterplotMatrix(~flow+oindustryproduct+oexport
                  +oheavyindustry+olightindustry
                  +dgdp+dimport+dstateproduct+gdistance,
                  data=mydata,spread=F,lty.smooth=2)

mf <- flow~oindustryproduct+oexport+oheavyindustry+olightindustry+dgdp+dimport+dstateproduct+gdistance+home
lm.results<-lm(mf,data=mydata)
summary(lm.results)

# 空间滞后模型分析
slm.results=lapply(swm.list[c(1,2,4)],
                   function(x) lagsarlm(formula=mf,
                                        data=mydata,listw=mat2listw(x)))
lapply(slm.results,summary)

# 源地相邻下的空间误差模型分析
sem.results.osm<-errorsarlm(formula=mf,
                            data=mydata,listw=mat2listw(oswm))
summary(sem.results.osm)

# 源地相邻下的空间通用模型分析
sac.results.osm<-sacsarlm(formula=mf,data=mydata,
                          listw=mat2listw(oswm))
summary(sac.results.osm)

# 空间模型检验
res<-lm.LMtests(lm.results,listw=mat2listw(oswm,style="W"),
                test="all")
tres<-t(sapply(res,function(x) 
        c(x$statistic,x$parameter,x$p.value)))
colnames(tres)<-c("statistic","df","p.value")
printCoefmat(tres)
