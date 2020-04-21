data <- read.csv("/Users/timtsang/Dropbox/2019nCoV/casedef/upload/Epidemic_curve_China.csv")
data <- data[-82,]
data$dateid <- 1:nrow(data)

data1 <- data[,c("date","dateid","wuhan.confirmed","hubeiexclwuhan.confirmed","chinaexclhubei.confirmed")]
#1/15 1/18 1/22 1/27 2/4 2/18
# 45   48   52   57   65  79


pred <- matrix(NA,nrow(data),12)
get <- read.csv("/Users/timtsang/Dropbox/2019nCoV/casedef/upload/pred.csv")
pred[,c(4,8,12)] <- log10(as.matrix(get[,c(4,9,14)]))
pred[,c(4,8,12)-1] <- log10(as.matrix(get[,c(4,9,14)-1]))
pred[,c(4,8,12)-2] <- log10(as.matrix(get[,c(4,9,14)-2]))
pred[,c(4,8,12)-3] <- log10(as.matrix(get[,c(4,9,14)-3]))

date.lab.day <- c(1:31,1:31,1:28)
date.lab.day[1:length(date.lab.day)%%7!=1] <- NA
date.lab.day[31+23] <- 23
pdf('/Users/timtsang/Dropbox/2019nCoV/casedef/upload/2020_04_03_Figure2.pdf',width=9, height=8)

layout(matrix( 1:3, ncol=1,byrow=T))

par(mar=c(5,5,4,0))

max.y1=1
bar.hwid=0.45

plot(NA, xlim=c(0,81), ylim=c(0,4), axes=F, ann=F)
#axis(1, at=0:81-0.5, lab=NA, pos=-0.5, padj=-0.5,tick=F) 
axis(1, at=c(c(-1,30,61,81)), lab=NA, pos=-0.25, padj=-1.5)
axis(1, at=0:81-0.5, lab=date.lab.day[0:81+1], pos=0, padj=0.5, tick=F)
#axis(1, at=55:70, lab=c(NA,"Feb",rep(NA,14)), pos=0, padj=1.3, tick=F)
axis(2, at=0:4,labels = c(1,10,100,1000,10000), las=1, pos=-1.5)

mtext('Dec, 2019', 1, line=2.5,at=29/2)
mtext('Jan, 2020', 1, line=2.5,at=30+15.5)
mtext('Feb, 2020', 1, line=2.5,at=61+10)
#1/15 1/18 1/22 1/27 2/4 2/18
# 45   48   52   57   65  79

xvec <- 1:81-0.5
points(xvec[1:47],log10(data1[1:47,3]),col="blue",pch=16)
points(xvec[48:56],log10(data1[48:56,3]),col="red",pch=17)
points(xvec[57:64],log10(data1[57:64,3]),col="orange",pch=18)
points(xvec[65:81],log10(data1[65:81,3]),col="black",pch=19)

#lines(rep(48-0.5,2),c(-2,3.5),col="blue",lty=2)
#lines(rep(57-0.5,2),c(-2,3.5),col="orange",lty=2)
#lines(rep(65-0.5,2),c(-2,3.5),col="black",lty=2)
#lines(rep(47-0.5,2),c(0,1000))
#lines(rep(55-0.5,2),c(0,1000))
#lines(rep(53-0.5,2),c(0,1000))
lines(xvec,pred[,1],col="blue",lty=1)
lines(xvec,pred[,2],col="red",lty=2)
lines(xvec,pred[,3],col="orange",lty=3)
lines(xvec,pred[,4],col="black",lty=4)

mtext('Version 1', 3, line=0,at=(-1+47)/2,cex=0.8)
mtext('Version 2', 3, line=0,at=(56+47)/2,cex=0.8)
mtext('Version 4', 3, line=0,at=(56+64)/2,cex=0.8)
mtext('Version 5', 3, line=0,at=(64+78.5)/2,cex=0.8)
lines(c(-1,-1,47,47),c(-0.08,0.08,0.08,-0.08)+4)
lines(c(47,47,56,56),c(-0.08,0.08,0.08,-0.08)+4)
lines(c(56,56,64,64),c(-0.08,0.08,0.08,-0.08)+4)
lines(c(64,64,78.5,78.5),c(-0.08,0.08,0.08,-0.08)+4)

polygon( c(37,37,47,47),  c(-0.5,4,4,-0.5),col=rgb(1,0,0,0.1),border=NA)
polygon( c(46,46,56,56),  c(-0.5,4,4,-0.5),col=rgb(1,0.63,0,0.1),border=NA)
polygon( c(54,54,64,64),  c(-0.5,4,4,-0.5),col=rgb(0,0,0,0.1),border=NA)

#polygon( c(xvec,rev(xvec)),  c(temp[xvec,32],rev(temp[xvec,33])),col=rgb(1,0,0,1),border=NA)
#polygon( c(xvec,rev(xvec)),  c(temp[xvec,41],rev(temp[xvec,42])),col=rgb(0,0,1,0.1),border=NA)
#polygon( c(xvec,rev(xvec)),  c(temp[xvec,23],rev(temp[xvec,24])),col=rgb(1,0.63,0,0.1),border=NA)
#polygon( c(xvec,rev(xvec)),  c(temp[xvec,14],rev(temp[xvec,15])),col=rgb(0,0,0,0.1),border=NA)

#mtext('Change to case', 3, line=0,at=47.5,cex=0.6)
#mtext('definition 2', 3, line=-1,at=47.5,cex=0.6)

#mtext('Change to case', 3, line=0,at=56.5,cex=0.6)
#mtext('definition 4', 3, line=-1,at=56.5,cex=0.6)

#mtext('Change to case', 3, line=0,at=64.5,cex=0.6)
#mtext('definition 5', 3, line=-1,at=64.5,cex=0.6)


#mtext('Date', 1, line=2.5)
mtext('Number of cases', 2, line=3)
#mtext('Date of onset', 1, line=4)
#mtext('Date of analysis', 1, line=2)
legend(0,4,legend=c("Case definition version 1" ,"Case definition version 2","Case definition version 4","Case definition version 5")
       ,col=c("blue","red","orange","black"),lty=c(1:4),pch=c(16,17,18,19),border=NA,bty="n",cex=1)
legend(19,4,legend=c("Backfill period for version 2" ,"Backfill period for version 4","Backfill period for version 5")
       ,fill=c(rgb(1,0,0,0.1),rgb(1,0.63,0,0.1),rgb(0,0,0,0.1)),border=NA,bty="n",cex=1)


mtext('Epidemic curve of Wuhan', 3, line=2)
title(main="A",adj=0)
lines(rep(31+22-0.5,2),c(-2,4),lty=2)




par(mar=c(5,5,4,0))

max.y1=1
bar.hwid=0.45

plot(NA, xlim=c(0,81), ylim=c(0,4), axes=F, ann=F)
#axis(1, at=0:81-0.5, lab=NA, pos=-0.5, padj=-0.5,tick=F) 
axis(1, at=c(c(-1,30,61,81)), lab=NA, pos=-0.25, padj=-1.5)
axis(1, at=0:81-0.5, lab=date.lab.day[0:81+1], pos=0, padj=0.5, tick=F)
#axis(1, at=55:70, lab=c(NA,"Feb",rep(NA,14)), pos=0, padj=1.3, tick=F)
axis(2, at=0:4,labels = c(1,10,100,1000,10000), las=1, pos=-1.5)

mtext('Dec, 2019', 1, line=2.5,at=29/2)
mtext('Jan, 2020', 1, line=2.5,at=30+15.5)
mtext('Feb, 2020', 1, line=2.5,at=61+10)

xvec <- 1:81-0.5
points(xvec[1:47],log10(data1[1:47,4]),col="blue",pch=16)
points(xvec[48:56],log10(data1[48:56,4]),col="red",pch=17)
points(xvec[57:64],log10(data1[57:64,4]),col="orange",pch=18)
points(xvec[65:81],log10(data1[65:81,4]),col="black",pch=19)

#lines(rep(38-0.5,2),c(0,1000))
#lines(rep(47-0.5,2),c(0,1000))
#lines(rep(55-0.5,2),c(0,1000))
#lines(rep(53-0.5,2),c(0,1000))
lines(xvec,pred[,5],col="blue",lty=1)
lines(xvec,pred[,6],col="red",lty=2)
lines(xvec,pred[,7],col="orange",lty=3)
lines(xvec,pred[,8],col="black",lty=4)

#polygon( c(xvec,rev(xvec)),  c(temp[xvec,32],rev(temp[xvec,33])),col=rgb(1,0,0,1),border=NA)
#polygon( c(xvec,rev(xvec)),  c(temp[xvec,41],rev(temp[xvec,42])),col=rgb(0,0,1,0.1),border=NA)
#polygon( c(xvec,rev(xvec)),  c(temp[xvec,23],rev(temp[xvec,24])),col=rgb(1,0.63,0,0.1),border=NA)
#polygon( c(xvec,rev(xvec)),  c(temp[xvec,14],rev(temp[xvec,15])),col=rgb(0,0,0,0.1),border=NA)

mtext('Version 1', 3, line=0,at=(-1+47)/2,cex=0.8)
mtext('Version 2', 3, line=0,at=(56+47)/2,cex=0.8)
mtext('Version 4', 3, line=0,at=(56+64)/2,cex=0.8)
mtext('Version 5', 3, line=0,at=(64+78.5)/2,cex=0.8)
lines(c(-1,-1,47,47),c(-0.08,0.08,0.08,-0.08)+4)
lines(c(47,47,56,56),c(-0.08,0.08,0.08,-0.08)+4)
lines(c(56,56,64,64),c(-0.08,0.08,0.08,-0.08)+4)
lines(c(64,64,78.5,78.5),c(-0.08,0.08,0.08,-0.08)+4)

#mtext('Date', 1, line=2.5)
mtext('Number of cases', 2, line=3)
#mtext('Date of onset', 1, line=4)
#mtext('Date of analysis', 1, line=2)
#legend(0,4,legend=c("Case definition 1","Case definition 3","Case definition 4","Case definition 5","Observed")
#       ,col=c("red","blue","orange","black","black"),lty=c(1,1,1,1,NA),pch=c(NA,NA,NA,NA,16),border=NA,bty="n",cex=1)

polygon( c(37,37,47,47),  c(-0.5,4,4,-0.5),col=rgb(1,0,0,0.1),border=NA)
polygon( c(46,46,56,56),  c(-0.5,4,4,-0.5),col=rgb(1,0.63,0,0.1),border=NA)
polygon( c(54,54,64,64),  c(-0.5,4,4,-0.5),col=rgb(0,0,0,0.1),border=NA)

mtext('Epidemic curve of Hubei province (excluding Wuhan)', 3, line=2)
title(main="B",adj=0)
lines(rep(31+22-0.5,2),c(-2,4),lty=2)



par(mar=c(5,5,4,0))

max.y1=1
bar.hwid=0.45

plot(NA, xlim=c(0,81), ylim=c(0,4), axes=F, ann=F)
#axis(1, at=0:81-0.5, lab=NA, pos=-0.5, padj=-0.5,tick=F) 
axis(1, at=c(c(-1,30,61,81)), lab=NA, pos=-0.25, padj=-1.5)
axis(1, at=0:81-0.5, lab=date.lab.day[0:81+1], pos=0, padj=0.5, tick=F)
#axis(1, at=55:70, lab=c(NA,"Feb",rep(NA,14)), pos=0, padj=1.3, tick=F)
axis(2, at=0:4,labels = c(1,10,100,1000,10000), las=1, pos=-1.5)

mtext('Dec, 2019', 1, line=2.5,at=29/2)
mtext('Jan, 2020', 1, line=2.5,at=30+15.5)
mtext('Feb, 2020', 1, line=2.5,at=61+10)

xvec <- 1:81-0.5
points(xvec[1:47],log10(data1[1:47,5]),col="blue",pch=16)
points(xvec[48:56],log10(data1[48:56,5]),col="red",pch=17)
points(xvec[57:64],log10(data1[57:64,5]),col="orange",pch=18)
points(xvec[65:81],log10(data1[65:81,5]),col="black",pch=19)

#lines(rep(38-0.5,2),c(0,1000))
#lines(rep(47-0.5,2),c(0,1000))
#lines(rep(55-0.5,2),c(0,1000))
#lines(rep(53-0.5,2),c(0,1000))
lines(xvec,pred[,9],col="blue",lty=1)
lines(xvec,pred[,10],col="red",lty=2)
lines(xvec,pred[,11],col="orange",lty=3)
lines(xvec,pred[,12],col="black",lty=4)

#polygon( c(xvec,rev(xvec)),  c(temp[xvec,32],rev(temp[xvec,33])),col=rgb(1,0,0,1),border=NA)
#polygon( c(xvec,rev(xvec)),  c(temp[xvec,41],rev(temp[xvec,42])),col=rgb(0,0,1,0.1),border=NA)
#polygon( c(xvec,rev(xvec)),  c(temp[xvec,23],rev(temp[xvec,24])),col=rgb(1,0.63,0,0.1),border=NA)
#polygon( c(xvec,rev(xvec)),  c(temp[xvec,14],rev(temp[xvec,15])),col=rgb(0,0,0,0.1),border=NA)

mtext('Version 1', 3, line=0,at=(-1+47)/2,cex=0.8)
mtext('Version 2', 3, line=0,at=(56+47)/2,cex=0.8)
mtext('Version 4', 3, line=0,at=(56+64)/2,cex=0.8)
mtext('Version 5', 3, line=0,at=(64+78.5)/2,cex=0.8)
lines(c(-1,-1,47,47),c(-0.08,0.08,0.08,-0.08)+4)
lines(c(47,47,56,56),c(-0.08,0.08,0.08,-0.08)+4)
lines(c(56,56,64,64),c(-0.08,0.08,0.08,-0.08)+4)
lines(c(64,64,78.5,78.5),c(-0.08,0.08,0.08,-0.08)+4)

#mtext('Date', 1, line=2.5)
mtext('Number of cases', 2, line=3)
mtext('Date of onset', 1, line=4)
#mtext('Date of analysis', 1, line=2)
#legend(0,4,legend=c("Case definition 1","Case definition 3","Case definition 4","Case definition 5","Observed")
#       ,col=c("red","blue","orange","black","black"),lty=c(1,1,1,1,NA),pch=c(NA,NA,NA,NA,16),border=NA,bty="n",cex=1)

polygon( c(37,37,47,47),  c(-0.5,4,4,-0.5),col=rgb(1,0,0,0.1),border=NA)
polygon( c(46,46,56,56),  c(-0.5,4,4,-0.5),col=rgb(1,0.63,0,0.1),border=NA)
polygon( c(54,54,64,64),  c(-0.5,4,4,-0.5),col=rgb(0,0,0,0.1),border=NA)

mtext('Epidemic curve of China (excluding Hubei province)', 3, line=2)
title(main="C",adj=0)
lines(rep(31+22-0.5,2),c(-2,4),lty=2)
dev.off()

