#  2.3.1   ���ɿռ�Ȩ�ؾ���

swm<-read.table("D:/regionResearchMethods/Ch2_ArcGIS/provincialPcGDP/pswm.txt",header=T,sep=",")
swm[[3]]<-swm[[3]]+1; swm[[4]]<-swm[[4]]+1
n=length(unique(swm[[3]]))
swmmat<-matrix(0,n,n)
apply(swm,1,function(x) swmmat[x[3],x[4]]<<-x[5])
swmmat


## R�����ڿռ����ݷ����е�Ӧ��

#  4.1.1   R����

help(read.table)
? read.table

help.search("spatial statistics")
??gal

#  4.1.2   R�е����ݶ���

#������vector��

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

# ����
f1<-rep(c("bad","normal","good"),1:3)
f1
f1<-as.factor(f1)
f1
levels(f1)
f1<-ordered(f1,levels=c("bad","normal","good"))
f1

# ����
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

# ����
dim1=paste("a",1:2,sep="")
dim2=paste("b",1:3,sep="")
dim3=paste("c",1:2,sep="")
myarray=array(1:12,c(2,3,2),
                dimnames=list(dim1,dim2,dim3))
myarray[2,3,2]
myarray[2,3,]
myarray[2,,]

# �б�
list1<-list(ID=1:2,names=c("Tom","Jerry"),
            species=list("cat","mouse"))
names(list1)
list1["names"]
list1[["names"]]
list1[[3]]
list1$names

# ���ݿ�
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

# ����
length(NA)
length(NULL)
c(1,NA)
c(1,NULL)

# 4.1.3  �����뺯��

myf=function(x,y) {x/2+y}
aa=1:3
bb=4:6
myf(aa,bb)
aa=c(3,8,5,4,9,1)
bb=character(length(aa))    # �����ַ�������bb������aa�ȳ�
for(i in 1:length(aa)) {    # forѭ����ָ��i��1������length(aa)����6
  if(aa[i]%%2) bb[i]="odd"  # ��aa[i]����2�õ�1�����߼���ֵT����bb��i��Ԫ�ظ�ֵΪ"odd"
  else bb[i]="even"         # ��aa[i]����2�õ�����ֵ���˴�Ϊ0�����߼���ֵF����bb��i��Ԫ�ظ�ֵΪ"even"
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

# 4.1.4  Rͼ��
plot(product~assets, data=mydata)
abline(lm(product~assets,data=mydata))

library(ggplot2)
qplot(assets,product,data=mydata)

4.2  �������������ɿռ�Ȩ��

install.packages("spdep")
library(spdep)

#  4.2.2  ���ɿռ�Ȩ�ض���

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

diag(gwt.mat)<-1  # ʹ����Խ���Ԫ�ط�0������ȡ����
any(gwt.mat==0)   # �жϾ������Ƿ���0Ԫ��
gwt.mat<-1/gwt.mat
diag(gwt.mat)<-0  # ������Խ���Ԫ�ػ�ԭΪ0
gwt.mat<-t(apply(gwt.mat,1,function(x) x/sum(x)))  # �����б�׼��

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

# 4.3  spdep�ռ�����ط���

lapply(mydata[2:4],moran.test,listw=mat2listw(gal.mat))
moran.test(mydata$product,listw=mat2listw(gwt.mat))

# Ī��ָ���������ģ��
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

# �鿴ln(product)�Ŀռ��������
moran.test(log(mydata$product),listw=mat2listw(gal.mat))


# 4.4  spdep�ռ�ع����

# ������ͨ��С���˷���
fm=log(product)~log(assets)+log(labors)
lm.results<-lm(fm,data=mydata)
summary(lm.results)

# OLS�в���Ŀռ�����ؼ���
moran.test(lm.results$residuals,mat2listw(gal.mat))
lm.morantest(lm.results,listw=mat2listw(gal.mat))

# ���пռ��ͺ�ģ�ͷ���
s.lag<-lagsarlm(fm,data=mydata,listw=mat2listw(gal.mat))
summary(s.lag)

# ���пռ����ģ�ͷ���
s.error<-errorsarlm(fm,mydata,mat2listw(gal.mat))
summary(s.error)

# ���пռ�ͨ��ģ�ͷ���
s.sac<-sacsarlm(fm,mydata,mat2listw(gal.mat))
summary(s.sac)

# ģ�ͱȽϼ���
lm.LMtests(lm.results, listw=mat2listw(gal.mat), test="all")

#  4.5  splm�ռ�������ݷ���
library(splm)
data("Produc", package = "Ecdat")
data("usaww")

head(Produc)
help("Produc", package = "Ecdat")

#  ������ͨ������ݷ���
fm<-log(gsp) ~ log(pcap) + log(pc) + log(emp)
plm.results<-plm(fm,data=Produc, index=c("state","year"),
                 effect="individual",model="within")
summary(plm.results)

#  ���пռ�������ݷ���

# �������ЧӦ�����пռ��ͺ�Ϳռ����
spml.results<-spml(fm,data=Produc,index=c("state","year"),
                   listw=mat2listw(usaww),
                   model="random",effect="individual",
                   lag=T,spatial.error="b")
summary(spml.results)

# ����̶�ЧӦ�����пռ��ͺ�Ϳռ����
spml.results2<-spml(fm,data=Produc,index=c("state","year"),
                    listw=mat2listw(usaww),
                    model="within",effect="individual",
                    lag=T,spatial.error="b")
summary(spml.results2)

#  �ռ��������ģ�ͼ���
test.LMH<-bsktest(fm,data=Produc,index=c("state","year"),
               listw=mat2listw(usaww),test="LMH")
test.CLMlambda<-bsktest(fm,data=Produc,index=c("state","year"),
               listw=mat2listw(usaww),test="CLMlambda")
# ����ع���
spgm.results <- spgm(fm, data = Produc, listw = mat2listw(usaww), 
                     lag = TRUE,model = "random", spatial.error = TRUE)
summary(spgm.results)
spgm.results2 <- spgm(fm, data = Produc, 
                      listw = mat2listw(usaww), lag = TRUE, 
                      model = "within", spatial.error = TRUE)
summary(spgm.results2)
test.hausman<-sphtest(spgm.results,spgm.results2)


##  5  ��ҵ�������������רҵ��

# ��������
setwd("D:/regionResearchMethods/Ch5_concentration")
industries<-read.csv("provincialIndustries2009.csv",
                     header=T,stringsAsFactors=F)
industries1<-as.matrix(industries[,-1:-2])
rownames(industries1)<-industries[,1]

#  �����ҵ�ڸ�ʡ�ݿռ�ֲ��ı���ϵ��CV
industries.cv<-apply(industries1,2,
                     function(x) sd(x)/mean(x))
industries.cv

#  ����CR5
industries.cR5<-apply(industries1,2,
                     function(x) x/sum(x))
industries.cR5<-apply(industries.cR5,2,function(x) 
                sum(x[order(x,decreasing=T)[1:5]]))
industries.cR5


# ������λ����ϵ��
gini <- function(x, correct=FALSE) {  # �������ϵ�����㺯����x��Ϊ��ֵ����
  n = length(x)
  fun = function(a,b) abs(a-b)
  ifelse(correct,sum(outer(x, x, fun))/(2*n*(n-1)*mean(x)),
         sum(outer(x, x, fun))/(2*n^2*mean(x)))
}
industries.gini<-apply(industries1,2,gini,correct=T)
industries.gini

# ����ineq�����������ϵ��
library(ineq) 
industries.gini2=apply(industries1,2,Gini,corr=T) 

# ������Ի���ϵ��
pp=apply(industries1,1,function(x) 
        sum(x)/sum(industries1))
pp=apply(industries1,2,function(x) x/sum(x)/pp)
industries.gini.r=apply(pp,2,Gini,corr=T)
industries.gini.r

# ������Ե�������ָ��
industries.MHHI<-apply(industries1,2,function(x) 
                       sqrt(mean((x/sum(x))^2)))
industries.MHHI

# ������Ե�������ָ��
pp=apply(industries1,1,function(x) 
         sum(x)/sum(industries1))
industries.HKMT<-apply(industries1,2,function(x) 
                 sqrt(mean((x/sum(x)-pp)^2)))
industries.HKMT

# ����ռ��ɢ��ָ��
sp.distance<-read.csv("31��ʡ������������·��̱�.csv",
                     header=T,stringsAsFactors=F)
sp.distance<-as.matrix(sp.distance[,-1])
diag(sp.distance)<-0
sp.distance<-sp.distance/sum(sp.distance)
industries.sp=apply(industries1,2,function(x) 
                    x/sum(x))
fun.sp<-function(x) 100*sum(outer(x,x,"*")*sp.distance)
industries.sp<-apply(industries.sp,2,fun.sp)
industries.sp

# ����̩��ָ��
library(ineq)
industries.theil=apply(industries1,2,Theil)
industries.theil

# ����ҵ��������ָ�����ۺϱȽ�
industries.indexes<-cbind(industries.cv, industries.cR5, 
                          industries.gini, industries.gini.r, 
                          industries.MHHI, industries.HKMT, 
                          industries.sp, industries.theil)
colnames(industries.indexes)<-c("cv","cR5","gini","gini.r",
                                "MHHI","HKMT","sp","theil")
write.csv(industries.indexes,"concentration_indexes.csv")
write.csv(cor(industries.indexes),"concentration.cor.csv")

#  �������רҵ��CV
industries2<-as.matrix(industries[,-(1:2)])
rownames(industries2)<-industries[,1]
specialization.cv<-apply(industries2,1,
                     function(x) sd(x)/mean(x))

#  �������רҵ��CR5
specialization.cR5<-apply(industries2,1,
                      function(x) x/sum(x))
specialization.cR5<-apply(specialization.cR5,2,function(x) 
                   sum(x[order(x,decreasing=T)[1:5]]))

# �������רҵ������ϵ��
specialization.gini=apply(industries2,1,Gini,corr=T)

# �������רҵ����Ի���ϵ��
ss=apply(industries2,2,function(x) 
         sum(x)/sum(industries2))
ss1=apply(industries2,1,function(x) x/sum(x)/ss)
specialization.gini.r=apply(ss1,2,Gini,corr=T)

# �������רҵ��̩��ָ��
specialization.theil=apply(industries2,1,Theil)

# �������רҵ���շҴ��ָ��
specialization.HHI<-apply(industries2,1,function(x) 
                     sum((x/sum(x))^2))

# ����רҵ����ָ�����ۺϱȽ�
specialization.indexes<-cbind(specialization.cv, 
                              specialization.cR5, 
                              specialization.gini, 
                              specialization.gini.r, 
                              specialization.theil, 
                              specialization.HHI)
colnames(specialization.indexes)<-c("cv","cR5","gini",
                                    "gini.r","theil","HHI")
# �������5ֵ����С5ֵ
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


##  6 ���򾭼÷�չ��༰�������Է���

# 6.2 �й�ʡ�ݾ��÷�չ�ռ��������

# ���벢��������
setwd("D:/regionResearchMethods/Ch6_convergence")
pcgdp<-read.csv("PcGDP31.csv",header=T,stringsAsFactors=F)
pcgdp<-subset(pcgdp,select=y1994:y2010)
summary(pcgdp)

# ����ÿ���ʡ�˾�GDP���ķ�λ��
sapply(pcgdp,function(x) 
      diff(quantile(x,probs=c(0.25,0.75)))/mean(x))

# ����pcgdp��1994-2010���ռ�ֲ���Moran's I
library(spdep)
gal.mat<-read.csv("gal_mat.csv",header=T,
                  stringsAsFactors=F)[-1]   # ����ռ�Ȩ�ؾ���
gal.mat<-as.matrix(gal.mat)
pcgdp.moran<-apply(pcgdp,2,function(x) 
                  {aa=moran.test(x,mat2listw(gal.mat));
                   c(aa$estimate,p.value=aa$p.value)})
pcgdp.moran<-t(pcgdp.moran)

# ��������Moran's I����ͼ
library(ggplot2)
qplot(1994:2010,pcgdp.moran[,1],xlab="year",
      ylab="Moran I statistic",
      geom=c("point","line","smooth"),method="lm")

# 6.3 �й���ʡ�ݾ��÷�չˮƽ�����

library(ineq)
methods<-c("Gini","Theil","var")
pcgdp.gaps<-sapply(methods,function(x) 
                   apply(pcgdp,2,ineq,type=x))
pcgdp.hhi<-apply(pcgdp,2,function(x) sum((x/sum(x))^2))
pcgdp.gaps<-cbind(pcgdp.gaps,hhi=pcgdp.hhi)

cor(pcgdp.gaps)
cor(pcgdp.gaps,method = "spearman")

# 6.4 �й���ʡ�ݾ��÷�չ�����Է���

convergence<-lm(log(y2010/y1994)~log(y1994),data=pcgdp)
summary(convergence)

# �����ԵĿռ��ͺ�ģ�ͷ���

moran.test(with(pcgdp,log(y2010/y1994)),mat2listw(gal.mat))
convergence.slm<-lagsarlm(log(y2010/y1994)~log(y1994),data=pcgdp,
                          listw=mat2listw(gal.mat))
summary(convergence.slm)
convergence.sem<-errorsarlm(log(y2010/y1994)~log(y1994),data=pcgdp,
                          listw=mat2listw(gal.mat))
summary(convergence.sem)

lm.LMtests(convergence, listw=mat2listw(gal.mat), test="all")


## 7 ����֪ʶ���������������

# ����������ռ�Ȩ�ؾ���
setwd("D:/regionResearchMethods/Ch7_spillovers")
library(splm)
hightech<-read.csv("highTech.csv",header=T,
                   stringsAsFactors=F)  # ��������
gal.mat<-read.csv("gal_mat.csv",header=T,
                  stringsAsFactors=F)   # ����ռ�Ȩ�ؾ���
all(gal.mat[,1]==unique(hightech$province))  # ȷ�������е�ʡ������˳����ռ�Ȩ�ؾ�����ͬ
gal.mat<-as.matrix(gal.mat[,-1])

# ����ͼ��չʾ
library(lattice)
xyplot(log(PAT)~log(ERD)|year,data=hightech)
xyplot(log(PAT)~log(HRD)|year,data=hightech)

# �ռ�����ط���
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

# ��ͨ������ݷ���
hightech.fm1<-log(PAT) ~ log(ERD) + log(HRD)
hightech.fm2<-log(NPO) ~ log(ERD) + log(HRD)
hightech.reg1<-lapply(list(hightech.fm1,hightech.fm2),plm,
                      data=hightech,index=c("province","year"),
                      effect = "individual",model = "within")
lapply(hightech.reg1,summary)

# �ռ�������ݷ���

# ���ЧӦ����ع���
hightech.reg2<-lapply(list(hightech.fm1,hightech.fm2),spgm,
                      data=hightech,index=c("province","year"),
                      listw=mat2listw(gal.mat),
                      model = "random",
                     lag=T, spatial.error=F)
lapply(hightech.reg2, summary)

# �̶�ЧӦ����ع���
hightech.reg3<-lapply(list(hightech.fm1,hightech.fm2),spgm,
                      data=hightech,index=c("province","year"),
                      listw=mat2listw(gal.mat),
                      model = "within",
                      lag=T, spatial.error=F)
lapply(hightech.reg3, summary)

# �ռ��������ģ�ͼ���
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


##  8   �й�ʡ��ó�׵Ŀռ以��ģ�ͷ���

#  8.2  �й�ʡ��ó�׵������Է���

setwd("D:/regionResearchMethods/Ch8_tradeFlow")
flow31 <- read.csv("flow29_2011.csv",header=T,
                   stringsAsFactors=F,na.strings = "-")
all(flow31$province==names(flow31)[-1])     #  �ж����е�ʡ��˳���Ƿ�һ��
flow29<-subset(flow31,
               !province %in% c("����","����"),
               -c(����,����))

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
outflow.gini<-apply(flow29[,-1],1,Gini)    #  �������ʡ�����������ֲ��ľ��Ի���ϵ��

numerator<-t(apply(flow29[,-1],1,function(x) x/sum(x)))    #  ���¼������ʡ�����������ֲ�����Ի���ϵ��
sum.inflow<-apply(flow29[,-1],2,sum)
denominator<-sum.inflow/sum(sum.inflow)
ratio<-t(apply(numerator,1,function(x) x/denominator))
outflow.gini.relative<-apply(ratio,1,Gini)

library(spdep)    #  �������ʡ���»�������������ʡ�ݵĿռ�ֲ���Moran's I
province29.table<-read.csv("province29.csv",
                           header=T,stringsAsFactors=F)  #  ע��GeoDa������csv�ļ��д����ո񣬵����ڼ��±���������
province29.gal <- read.gal("province29.gal")
province29.swm<-nb2mat(province29.gal)
dimnames(province29.swm)<-list(province29.table$NAME,
                               province29.table$NAME)
province29.swm<-province29.swm[flow29$province,
                               flow29$province]  #  ��flow29���ݱ��е�ʡ��˳�����ſռ�Ȩ�ؾ����������
moran.outflow<-apply(flow29[,-1],1,moran.test,
                     listw=mat2listw(province29.swm))
estimate<-sapply(moran.outflow,"[[","estimate")    #  ��ȡ����ͳ����
p.value<-sapply(moran.outflow,"[[","p.value")       #  ��ȡ������ˮƽ
moran.outflow<-rbind(estimate,p.value)
moran.outflow<-t(moran.outflow)

mymat<-matrix(0,29,29)    #  �������ʡ���»����������ڽ�ʡ�ݵ�ռ��
mymat[province29.swm!=0]<-flow29[,-1][province29.swm!=0]
flow29.a<-flow29[,-1]
diag(flow29.a)<-0         #  ȥ���Խ�����ʡ���ڲ��Ļ�������
rowSums(mymat)/rowSums(flow29.a)

swm2<-province29.swm
swm2 <- ifelse(swm2!=0,1,0)
oswm <- swm2 %x% diag(29)    #  ����Դ�������µĿռ�Ȩ�ؾ���
oswm2<-matrix(0,841,841)    #  ����Դ�������¿ռ�Ȩ�ؾ����ֱ��������㣬�Ƚ���
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
dswm<-diag(29) %x% swm2            #  �����������µĿռ�Ȩ�ؾ���
odswm<-swm2 %x% swm2            #  ����˫�������µĿռ�Ȩ�ؾ���
o_dswm<-oswm+dswm               #  ����Դ�ػ��������µĿռ�Ȩ�ؾ���
o.dswm<-oswm+dswm+odswm         #  ����Դ�ء���ػ�˫�������µĿռ�Ȩ�ؾ���

swm.list<-list(oswm=oswm,dswm=dswm,odswm=odswm,
               o_dswm=o_dswm,o.dswm=o.dswm)
row.standize<-function(x) t(apply(x,1,function(y) y/sum(y)))    #  ��������б�׼������
swm.list<-lapply(swm.list,row.standize)    #  ��Ȩ�ؾ���������׼��

odflow<-as.vector(t(flow29[,-1]))
moran<-function(x) moran.test(odflow,listw=mat2listw(x))
morans=lapply(swm.list,moran)
rbind(sapply(morans,"[[","estimate"),
      p.value=sapply(morans,"[[","p.value"))

#  8.4  �й�ʡ��ó�׵Ŀռ以��ģ�ͷ���
province.data<-read.csv("provinceData.csv",
                        header=T,stringsAsFactors=F)
province.data<-subset(province.data,
                      !province %in% c("����","����"))
all(province.data$province==flow29$province)    #  �ж��������ݱ����������ݾ����ʡ�������Ƿ�һ��
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
distance<-read.csv("31��ʡ������������·��̱�.csv",
                   header=T,stringsAsFactors=F)
prailway<-read.csv("��ʡ��·Ӫҵ�����_2011.csv",
                   header=T,stringsAsFactors=F)
parea<-read.csv("��ʡ�����.csv",
                header=T,stringsAsFactors=F)
pdistance<-merge(prailway,parea,by="province")
pdistance<-subset(pdistance,!province %in% c("����","����"))
dweights<-with(pdistance,
               sum(railway)/sum(area)/(railway/area))
pdistance$pdistance<-with(pdistance,
                          sqrt(area/pi)*dweights)
mydata$gdistance<-numeric(841)    #  �������ɵ�������
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

# �ռ��ͺ�ģ�ͷ���
slm.results=lapply(swm.list[c(1,2,4)],
                   function(x) lagsarlm(formula=mf,
                                        data=mydata,listw=mat2listw(x)))
lapply(slm.results,summary)

# Դ�������µĿռ����ģ�ͷ���
sem.results.osm<-errorsarlm(formula=mf,
                            data=mydata,listw=mat2listw(oswm))
summary(sem.results.osm)

# Դ�������µĿռ�ͨ��ģ�ͷ���
sac.results.osm<-sacsarlm(formula=mf,data=mydata,
                          listw=mat2listw(oswm))
summary(sac.results.osm)

# �ռ�ģ�ͼ���
res<-lm.LMtests(lm.results,listw=mat2listw(oswm,style="W"),
                test="all")
tres<-t(sapply(res,function(x) 
        c(x$statistic,x$parameter,x$p.value)))
colnames(tres)<-c("statistic","df","p.value")
printCoefmat(tres)