# 读取数据集
algae<-read.table('~/Desktop/Analysis.txt',header=F,dec='.',
                  col.names = c('season','size','speed','mxPH','mnO2',
                                               'Cl','NO3','NH4','oPO4','PO4','Chla',
                                               'a1','a2','a3','a4','a5','a6','a7'),
na.strings = c('XXXXXXX'))

# 获得数据摘要
summary(algae)

# 以mxPH为例，直方图
hist(algae$mxPH)
hist(algae$PO4,prob=T)

# 正态性检验与绘制qq图
install.packages(pkgs='car')
library(car)
par(mfrow=c(1,2))
hist(algae$mxPH,prob=T,xlab = '',main = 'Histogram of maximum pH
     value',ylim=0:1)
lines(density(algae$mxPH,na.rm = T))
rug(jitter(algae$mxPH))
qqPlot(algae$mxPH,main='Normal QQ plot of maximum pH')
par(mfrow=c(1,1))

# 绘制盒图
boxplot(algae$mxPH,ylab="Orthophosphate(mxPH)")
rug(jitter(algae$mxPH),side = 2)
abline(h=mean(algae$mxPH,na.rm = T),lty=2)

# 离群点标示
plot(algae$NH4,xlab="")
abline(h=mean(algae$NH4,na.rm = T),lty=1)
abline(h=mean(algae$NH4,na.rm = T)+sd(algae$NH4,na.rm=T),lty=2)
abline(h=median(algae$NH4,na.rm = T),lty=3)
identify(algae$NH4)
algae[algae$NH4>19000,]
alage[!is.na(algae$NH4)&algae$NH4>19000,]

# 7种海藻的条件箱图
install.packages(pkgs='lattice')
library(lattice)
bwplot(size~a1,data = algae,ylab = 'River Size',xlab = 'Algal A1')
bwplot(size~a2,data = algae,ylab = 'River Size',xlab = 'Algal A2')
bwplot(size~a3,data = algae,ylab = 'River Size',xlab = 'Algal A3')
bwplot(size~a4,data = algae,ylab = 'River Size',xlab = 'Algal A4')
bwplot(size~a5,data = algae,ylab = 'River Size',xlab = 'Algal A5')
bwplot(size~a6,data = algae,ylab = 'River Size',xlab = 'Algal A6')
bwplot(size~a7,data = algae,ylab = 'River Size',xlab = 'Algal A7')


# 查看预处理前，数据集中的缺失值个数
algae[!complete.cases(algae),]

# 显示未处理前的数据缺失情况
nrow(algae[!complete.cases(algae),])

# 直接剔除
library(DMwR)
data(algae)
algae<-algae[-manyNAs(algae),]
algae<-centralImputation(algae)
summary(algae)

# 最高频率填补
library(DMwR)
data(algae)
algae<-algae[-manyNAs(algae),]
algae<-centralImputation(algae)
summary(algae)

# 变量相关关系填充
library(DMwR)
cor(algae[,4:18],use = "complete.obs")
symnum(cor(algae[,4:18],use = "complete.obs"))
data(algae)
algae<-algae[-manyNAs(algae),]
lm(formula=PO4~oPO4,data = algae)
algae[28,"PO4"]<-42.897+1.293*algae[28,"oPO4"]
fillPO4<-function(oP)
{
  if(is.na(oP))return(NA)
  else return(42.897+1.293*oP)
}
algae[is.na(algae$PO4),"PO4"]<-sapply(algae[is.na(algae$PO4),"oPO4"],fillPO4)
summary(algae)

# 案例相似性填补
library(DMwR)
data(algae)
algae<-algae[-manyNAs(algae),]
algae<-knnImputation(algae,k=10)
algae<-knnImputation(algae,k=10,meth = "median")
summary(algae)

# 数据预处理后，数据集中缺失值的个数
nrow(algae[!complete.cases(algae),])

# 显示处理过后的数据集
algae

# 重新绘制直方图等统计图
hist(algae$mxPH)
hist(algae$PO4,prob=T)
boxplot(algae$mxPH,ylab="Orthophosphate(mxPH)")
rug(jitter(algae$mxPH),side = 2)
abline(h=mean(algae$mxPH,na.rm = T),lty=2)

# 保存预处理过后的数据集
write.table(algae, file = "~/Desktop/Result2.txt", row.names = F, quote = F)