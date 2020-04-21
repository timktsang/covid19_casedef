data <- read.csv("/Users/timtsang/Dropbox/2019nCoV/casedef/upload/Epidemic_curve_China.csv")
data <- data[-82,]
data$dateid <- 1:nrow(data)

data1 <- data[,c("date","dateid","wuhan.confirmed","hubeiexclwuhan.confirmed","chinaexclhubei.confirmed")]
#1/15 1/18 1/22 1/27 2/4 2/18
# 45   48   52   57   65  79


test <- read.csv("/Users/timtsang/Dropbox/2019nCoV/casedef/upload/pred_f3.csv")

testp <- test
testp <- testp[,c(2:5,7:10,12:15)] - testp[,c(2:5,7:10,12:15)-1] 
testp[,1:4] <- testp[,1:4]/rowSums(testp[,1:4])
testp[,1:4+4] <- testp[,1:4+4]/rowSums(testp[,1:4+4])
testp[,1:4+8] <- testp[,1:4+8]/rowSums(testp[,1:4+8])
#pred<- as.matrix(test)
#test <- pred

date.lab.day <- c(1:31,1:31,1:28)
date.lab.day[1:length(date.lab.day)%%7!=1] <- NA
date.lab.day[31+23] <- 23
pdf('/Users/timtsang/Dropbox/2019nCoV/casedef/upload/2020_03_17_Figure3.pdf',width=9, height=8)

#layout(matrix( 1:3, ncol=1,byrow=T))

par(fig=c(0,1,0,1),mar=c(5,5,4,0),mfrow=c(3,1))

max.y1=1
bar.hwid=0.45

plot(NA, xlim=c(0,81), ylim=c(0,8000), axes=F, ann=F)
#axis(1, at=0:81-0.5, lab=NA, pos=-0.5, padj=-0.5,tick=F) 
axis(1, at=c(c(-1,30,61,81)), lab=NA, pos=-0.5, padj=-0.5)
axis(1, at=0:81-0.5, lab=date.lab.day[0:81+1], pos=0, padj=-0.5, tick=F)
#axis(1, at=55:70, lab=c(NA,"Feb",rep(NA,14)), pos=0, padj=1.3, tick=F)
axis(2, at=0:8*1000, las=1, pos=-1.5)

mtext('Dec, 2019', 1, line=2,at=29/2)
mtext('Jan, 2020', 1, line=2,at=30+15.5)
mtext('Feb, 2020', 1, line=2,at=61+10)



#1/15 1/18 1/22 1/27 2/4 2/18
# 45   48   52   57   65  79
bar.hwid <- 0.45
xvec <- 1:81-0.5
for (i in 1:length(xvec)){
  polygon(c(rep(xvec[i]-bar.hwid,2),rep(xvec[i]+bar.hwid,2)), 
          c(0,rep(data1[i,3],2), 0), col='dodgerblue2', border=F)	 #dodgerblue2
  if (test[i,3]>data1[i,3]){
  # compute the proportion for version 2, 4 and 5 (red, orange, black)
    polygon(c(rep(xvec[i]-bar.hwid,2),rep(xvec[i]+bar.hwid,2)), 
            c(data1[i,3],rep(test[i,3],2), data1[i,3]), col=rgb(1,0,0,0.2), border=F)	 #dodgerblue2
  }
  if (test[i,4]>test[i,3]&test[i,4]>data1[i,3]){
    # compute the proportion for version 2, 4 and 5 (red, orange, black)
    polygon(c(rep(xvec[i]-bar.hwid,2),rep(xvec[i]+bar.hwid,2)), 
            c(max(data1[i,3],test[i,3]),rep(test[i,4],2), max(data1[i,3],test[i,3])), col=rgb(1,0.63,0,0.2), border=F)	 #dodgerblue2
  }
  if (test[i,5]>test[i,4]&test[i,5]>data1[i,3]){
    # compute the proportion for version 2, 4 and 5 (red, orange, black)
    polygon(c(rep(xvec[i]-bar.hwid,2),rep(xvec[i]+bar.hwid,2)), 
            c(max(data1[i,3],test[i,4]),rep(test[i,5],2), max(data1[i,3],test[i,4])), col=rgb(0,0,0,0.2), border=F)	 #dodgerblue2
  }
}

mtext('Version 1', 3, line=0,at=(-1+47)/2,cex=0.8)
mtext('Version 2', 3, line=0,at=(56+47)/2,cex=0.8)
mtext('Version 4', 3, line=0,at=(56+64)/2,cex=0.8)
mtext('Version 5', 3, line=0,at=(64+78.5)/2,cex=0.8)
lines(c(-1,-1,47,47),c(-0.08,0.08,0.08,-0.08)*1000+8000)
lines(c(47,47,56,56),c(-0.08,0.08,0.08,-0.08)*1000+8000)
lines(c(56,56,64,64),c(-0.08,0.08,0.08,-0.08)*1000+8000)
lines(c(64,64,78.5,78.5),c(-0.08,0.08,0.08,-0.08)*1000+8000)

#mtext('Date', 1, line=2.5)
mtext('Number of cases', 2, line=3)
#mtext('Date of onset', 1, line=4)
#mtext('Date of analysis', 1, line=2)
legend(31,8000,legend=c("Observed","Estimated by version 2","Estimated by version 4","Estimated by version 5"),fill=c(rgb(0.12,0.56,1,1),rgb(1,0,0,0.2),rgb(1,0.63,0,0.2),rgb(0,0,0,0.2)),,border=NA,bty="n",cex=1)

mtext('Epidemic curve of Wuhan', 3, line=2)
title(main="A",adj=0)
lines(rep(31+22-0.5,2),c(0,7900),lty=2)
library(pBrackets)

brackets(-1, 0, 30, 0, h = 500, ticks = 0.5, curvature = 0.5, type = 1,
         col = 1, lwd = 1, lty = 1, xpd = FALSE)

par(mar=c(5,5,4,0))


max.y1=1
bar.hwid=0.45

plot(NA, xlim=c(0,81), ylim=c(0,4000), axes=F, ann=F)
#axis(1, at=0:81-0.5, lab=NA, pos=-0.5, padj=-0.5,tick=F) 
axis(1, at=c(c(-1,30,61,81)), lab=NA, pos=-0.5, padj=-0.5)
axis(1, at=0:81-0.5, lab=date.lab.day[0:81+1], pos=0, padj=-0.5, tick=F)
#axis(1, at=55:70, lab=c(NA,"Feb",rep(NA,14)), pos=0, padj=1.3, tick=F)
axis(2, at=0:8*500, las=1, pos=-1.5)

mtext('Dec, 2019', 1, line=2,at=29/2)
mtext('Jan, 2020', 1, line=2,at=30+15.5)
mtext('Feb, 2020', 1, line=2,at=61+10)

xvec <- 1:81-0.5


for (i in 1:length(xvec)){
  polygon(c(rep(xvec[i]-bar.hwid,2),rep(xvec[i]+bar.hwid,2)), 
          c(0,rep(data1[i,3+1],2), 0), col='dodgerblue2', border=F)	 #dodgerblue2
  if (test[i,3+5]>data1[i,3+1]){
    # compute the proportion for version 2, 4 and 5 (red, orange, black)
    polygon(c(rep(xvec[i]-bar.hwid,2),rep(xvec[i]+bar.hwid,2)), 
            c(data1[i,3+1],rep(test[i,3+5],2), data1[i,3+1]), col=rgb(1,0,0,0.2), border=F)	 #dodgerblue2
  }
  if (test[i,4+5]>test[i,3+5]&test[i,4+5]>data1[i,3+1]){
    # compute the proportion for version 2, 4 and 5 (red, orange, black)
    polygon(c(rep(xvec[i]-bar.hwid,2),rep(xvec[i]+bar.hwid,2)), 
            c(max(data1[i,3+1],test[i,3+5]),rep(test[i,4+5],2), max(data1[i,3+1],test[i,3+5])), col=rgb(1,0.63,0,0.2), border=F)	 #dodgerblue2
  }
  if (test[i,5+5]>test[i,4+5]&test[i,5+5]>data1[i,3+1]){
    # compute the proportion for version 2, 4 and 5 (red, orange, black)
    polygon(c(rep(xvec[i]-bar.hwid,2),rep(xvec[i]+bar.hwid,2)), 
            c(max(data1[i,3+1],test[i,4+5]),rep(test[i,5+5],2), max(data1[i,3+1],test[i,4+5])), col=rgb(0,0,0,0.2), border=F)	 #dodgerblue2
  }
}


#polygon( c(xvec,rev(xvec)),  c(temp[xvec,32],rev(temp[xvec,33])),col=rgb(1,0,0,1),border=NA)
#polygon( c(xvec,rev(xvec)),  c(temp[xvec,41],rev(temp[xvec,42])),col=rgb(0,0,1,0.1),border=NA)
#polygon( c(xvec,rev(xvec)),  c(temp[xvec,23],rev(temp[xvec,24])),col=rgb(1,0.63,0,0.1),border=NA)
#polygon( c(xvec,rev(xvec)),  c(temp[xvec,14],rev(temp[xvec,15])),col=rgb(0,0,0,0.1),border=NA)

mtext('Version 1', 3, line=0,at=(-1+47)/2,cex=0.8)
mtext('Version 2', 3, line=0,at=(56+47)/2,cex=0.8)
mtext('Version 4', 3, line=0,at=(56+64)/2,cex=0.8)
mtext('Version 5', 3, line=0,at=(64+78.5)/2,cex=0.8)
lines(c(-1,-1,47,47),c(-0.08,0.08,0.08,-0.08)*1000+4000)
lines(c(47,47,56,56),c(-0.08,0.08,0.08,-0.08)*1000+4000)
lines(c(56,56,64,64),c(-0.08,0.08,0.08,-0.08)*1000+4000)
lines(c(64,64,78.5,78.5),c(-0.08,0.08,0.08,-0.08)*1000+4000)

#mtext('Date', 1, line=2.5)
mtext('Number of cases', 2, line=3)
#mtext('Date of onset', 1, line=4)
#mtext('Date of analysis', 1, line=2)
#legend(0,4,legend=c("Case definition 1","Case definition 3","Case definition 4","Case definition 5","Observed")
#       ,col=c("red","blue","orange","black","black"),lty=c(1,1,1,1,NA),pch=c(NA,NA,NA,NA,16),border=NA,bty="n",cex=1)


mtext('Epidemic curve of Hubei province (excluding Wuhan)', 3, line=2)
title(main="B",adj=0)
lines(rep(31+22-0.5,2),c(0,7900),lty=2)
brackets(-1.5, 0, 29.5, 0, h = 500, ticks = 0.5, curvature = 0.5, type = 1,
         col = 1, lwd = 1, lty = 1, xpd = FALSE)


par(mar=c(5,5,4,0))

max.y1=1
bar.hwid=0.45

plot(NA, xlim=c(0,81), ylim=c(0,4000), axes=F, ann=F)


#axis(1, at=0:81-0.5, lab=NA, pos=-0.5, padj=-0.5,tick=F) 
axis(1, at=c(c(-1,30,61,81)), lab=NA, pos=-0.5, padj=-0.5)
axis(1, at=0:81-0.5, lab=date.lab.day[0:81+1], pos=0, padj=-0.5, tick=F)
#axis(1, at=55:70, lab=c(NA,"Feb",rep(NA,14)), pos=0, padj=1.3, tick=F)
axis(2, at=0:8*500, las=1, pos=-1.5)

mtext('Dec, 2019', 1, line=2,at=29/2)
mtext('Jan, 2020', 1, line=2,at=30+15.5)
mtext('Feb, 2020', 1, line=2,at=61+10)

xvec <- 1:81-0.5
for (i in 1:length(xvec)){
  polygon(c(rep(xvec[i]-bar.hwid,2),rep(xvec[i]+bar.hwid,2)), 
          c(0,rep(data1[i,3+2],2), 0), col='dodgerblue2', border=F)	 #dodgerblue2
  if (test[i,3+10]>data1[i,3+2]){
    # compute the proportion for version 2, 4 and 5 (red, orange, black)
    polygon(c(rep(xvec[i]-bar.hwid,2),rep(xvec[i]+bar.hwid,2)), 
            c(data1[i,3+2],rep(test[i,3+10],2), data1[i,3+2]), col=rgb(1,0,0,0.2), border=F)	 #dodgerblue2
  }
  if (test[i,4+10]>test[i,3+10]&test[i,4+10]>data1[i,3+2]){
    # compute the proportion for version 2, 4 and 5 (red, orange, black)
    polygon(c(rep(xvec[i]-bar.hwid,2),rep(xvec[i]+bar.hwid,2)), 
            c(max(data1[i,3+2],test[i,3+10]),rep(test[i,4+10],2), max(data1[i,3+2],test[i,3+10])), col=rgb(1,0.63,0,0.2), border=F)	 #dodgerblue2
  }
  if (test[i,5+10]>test[i,4+10]&test[i,5+10]>data1[i,3+2]){
    # compute the proportion for version 2, 4 and 5 (red, orange, black)
    polygon(c(rep(xvec[i]-bar.hwid,2),rep(xvec[i]+bar.hwid,2)), 
            c(max(data1[i,3+2],test[i,4+10]),rep(test[i,5+10],2), max(data1[i,3+2],test[i,4+10])), col=rgb(0,0,0,0.2), border=F)	 #dodgerblue2
  }
}


#lines(rep(38-0.5,2),c(0,1000))
#lines(rep(47-0.5,2),c(0,1000))
#lines(rep(55-0.5,2),c(0,1000))
#lines(rep(53-0.5,2),c(0,1000))


#polygon( c(xvec,rev(xvec)),  c(temp[xvec,32],rev(temp[xvec,33])),col=rgb(1,0,0,1),border=NA)
#polygon( c(xvec,rev(xvec)),  c(temp[xvec,41],rev(temp[xvec,42])),col=rgb(0,0,1,0.1),border=NA)
#polygon( c(xvec,rev(xvec)),  c(temp[xvec,23],rev(temp[xvec,24])),col=rgb(1,0.63,0,0.1),border=NA)
#polygon( c(xvec,rev(xvec)),  c(temp[xvec,14],rev(temp[xvec,15])),col=rgb(0,0,0,0.1),border=NA)

mtext('Version 1', 3, line=0,at=(-1+47)/2,cex=0.8)
mtext('Version 2', 3, line=0,at=(56+47)/2,cex=0.8)
mtext('Version 4', 3, line=0,at=(56+64)/2,cex=0.8)
mtext('Version 5', 3, line=0,at=(64+78.5)/2,cex=0.8)
lines(c(-1,-1,47,47),c(-0.08,0.08,0.08,-0.08)*1000+4000)
lines(c(47,47,56,56),c(-0.08,0.08,0.08,-0.08)*1000+4000)
lines(c(56,56,64,64),c(-0.08,0.08,0.08,-0.08)*1000+4000)
lines(c(64,64,78.5,78.5),c(-0.08,0.08,0.08,-0.08)*1000+4000)

#mtext('Date', 1, line=2.5)
mtext('Number of cases', 2, line=3)
mtext('Date of onset', 1, line=4)
#mtext('Date of analysis', 1, line=2)
#legend(0,4,legend=c("Case definition 1","Case definition 3","Case definition 4","Case definition 5","Observed")
#       ,col=c("red","blue","orange","black","black"),lty=c(1,1,1,1,NA),pch=c(NA,NA,NA,NA,16),border=NA,bty="n",cex=1)


mtext('Epidemic curve of China (excluding Hubei province)', 3, line=2)
title(main="C",adj=0)
lines(rep(31+22-0.5,2),c(0,7900),lty=2)
brackets(-1.5, 0, 29.5, 0, h = 500, ticks = 0.5, curvature = 0.5, type = 1,
         col = 1, lwd = 1, lty = 1, xpd = FALSE)


##################################################################
par(fig = c(0.03,0.42, 0.7, 0.98), new = T)  

max.y1=1
bar.hwid=0.45

plot(NA, xlim=c(-4,29), ylim=c(-20,1020), axes=F, ann=F)
#axis(1, at=0:81-0.5, lab=NA, pos=-0.5, padj=-0.5,tick=F) 
#axis(1, at=c(0,29), lab=NA, pos=-0.5, padj=-0.5)
lines(c(0,29),c(0,0))
#axis(1, at=55:70, lab=c(NA,"Feb",rep(NA,14)), pos=0, padj=1.3, tick=F)
axis(2, at=0:5*200, las=1, pos=-0.5)
box()

#1/15 1/18 1/22 1/27 2/4 2/18
# 45   48   52   57   6 5  79
bar.hwid <- 0.45
xvec <- 1:29-0.5
for (i in 1:length(xvec)){
  polygon(c(rep(xvec[i]-bar.hwid,2),rep(xvec[i]+bar.hwid,2)), 
          c(0,rep(data1[i,3],2), 0), col='dodgerblue2', border=F)	 #dodgerblue2
  if (test[i,3]>data1[i,3]){
    # compute the proportion for version 2, 4 and 5 (red, orange, black)
    polygon(c(rep(xvec[i]-bar.hwid,2),rep(xvec[i]+bar.hwid,2)), 
            c(data1[i,3],rep(test[i,3],2), data1[i,3]), col=rgb(1,0,0,0.2), border=F)	 #dodgerblue2
  }
  if (test[i,4]>test[i,3]&test[i,4]>data1[i,3]){
    # compute the proportion for version 2, 4 and 5 (red, orange, black)
    polygon(c(rep(xvec[i]-bar.hwid,2),rep(xvec[i]+bar.hwid,2)), 
            c(max(data1[i,3],test[i,3]),rep(test[i,4],2), max(data1[i,3],test[i,3])), col=rgb(1,0.63,0,0.2), border=F)	 #dodgerblue2
  }
  if (test[i,5]>test[i,4]&test[i,5]>data1[i,3]){
    # compute the proportion for version 2, 4 and 5 (red, orange, black)
    polygon(c(rep(xvec[i]-bar.hwid,2),rep(xvec[i]+bar.hwid,2)), 
            c(max(data1[i,3],test[i,4]),rep(test[i,5],2), max(data1[i,3],test[i,4])), col=rgb(0,0,0,0.2), border=F)	 #dodgerblue2
  }
}


##################################################################
par(fig = c(0.03,0.42, 0.368, 0.65), new = T)  

max.y1=1
bar.hwid=0.45

plot(NA, xlim=c(-4,29), ylim=c(-20,520), axes=F, ann=F)
#axis(1, at=0:81-0.5, lab=NA, pos=-0.5, padj=-0.5,tick=F) 
#axis(1, at=c(0,29), lab=NA, pos=-0.5, padj=-0.5)
lines(c(0,29),c(0,0))
#axis(1, at=55:70, lab=c(NA,"Feb",rep(NA,14)), pos=0, padj=1.3, tick=F)
axis(2, at=0:5*100, las=1, pos=-0.5)
box()

#1/15 1/18 1/22 1/27 2/4 2/18
# 45   48   52   57   6 5  79
bar.hwid <- 0.45
xvec <- 1:29-0.5

for (i in 1:length(xvec)){
  polygon(c(rep(xvec[i]-bar.hwid,2),rep(xvec[i]+bar.hwid,2)), 
          c(0,rep(data1[i,3+1],2), 0), col='dodgerblue2', border=F)	 #dodgerblue2
  if (test[i,3+5]>data1[i,3+1]){
    # compute the proportion for version 2, 4 and 5 (red, orange, black)
    polygon(c(rep(xvec[i]-bar.hwid,2),rep(xvec[i]+bar.hwid,2)), 
            c(data1[i,3+1],rep(test[i,3+5],2), data1[i,3+1]), col=rgb(1,0,0,0.2), border=F)	 #dodgerblue2
  }
  if (test[i,4+5]>test[i,3+5]&test[i,4+5]>data1[i,3+1]){
    # compute the proportion for version 2, 4 and 5 (red, orange, black)
    polygon(c(rep(xvec[i]-bar.hwid,2),rep(xvec[i]+bar.hwid,2)), 
            c(max(data1[i,3+1],test[i,3+5]),rep(test[i,4+5],2), max(data1[i,3+1],test[i,3+5])), col=rgb(1,0.63,0,0.2), border=F)	 #dodgerblue2
  }
  if (test[i,5+5]>test[i,4+5]&test[i,5+5]>data1[i,3+1]){
    # compute the proportion for version 2, 4 and 5 (red, orange, black)
    polygon(c(rep(xvec[i]-bar.hwid,2),rep(xvec[i]+bar.hwid,2)), 
            c(max(data1[i,3+1],test[i,4+5]),rep(test[i,5+5],2), max(data1[i,3+1],test[i,4+5])), col=rgb(0,0,0,0.2), border=F)	 #dodgerblue2
  }
}



##################################################################
par(fig = c(0.03,0.42, 0.034, 0.32), new = T)  

max.y1=1
bar.hwid=0.45

plot(NA, xlim=c(-4,29), ylim=c(-20,520), axes=F, ann=F)
#axis(1, at=0:81-0.5, lab=NA, pos=-0.5, padj=-0.5,tick=F) 
#axis(1, at=c(0,29), lab=NA, pos=-0.5, padj=-0.5)
lines(c(0,29),c(0,0))
#axis(1, at=55:70, lab=c(NA,"Feb",rep(NA,14)), pos=0, padj=1.3, tick=F)
axis(2, at=0:5*100, las=1, pos=-0.5)
box()

#1/15 1/18 1/22 1/27 2/4 2/18
# 45   48   52   57   6 5  79
bar.hwid <- 0.45
xvec <- 1:29-0.5
for (i in 1:length(xvec)){
  polygon(c(rep(xvec[i]-bar.hwid,2),rep(xvec[i]+bar.hwid,2)), 
          c(0,rep(data1[i,3+2],2), 0), col='dodgerblue2', border=F)	 #dodgerblue2
  if (test[i,3+10]>data1[i,3+2]){
    # compute the proportion for version 2, 4 and 5 (red, orange, black)
    polygon(c(rep(xvec[i]-bar.hwid,2),rep(xvec[i]+bar.hwid,2)), 
            c(data1[i,3+2],rep(test[i,3+10],2), data1[i,3+2]), col=rgb(1,0,0,0.2), border=F)	 #dodgerblue2
  }
  if (test[i,4+10]>test[i,3+10]&test[i,4+10]>data1[i,3+2]){
    # compute the proportion for version 2, 4 and 5 (red, orange, black)
    polygon(c(rep(xvec[i]-bar.hwid,2),rep(xvec[i]+bar.hwid,2)), 
            c(max(data1[i,3+2],test[i,3+10]),rep(test[i,4+10],2), max(data1[i,3+2],test[i,3+10])), col=rgb(1,0.63,0,0.2), border=F)	 #dodgerblue2
  }
  if (test[i,5+10]>test[i,4+10]&test[i,5+10]>data1[i,3+2]){
    # compute the proportion for version 2, 4 and 5 (red, orange, black)
    polygon(c(rep(xvec[i]-bar.hwid,2),rep(xvec[i]+bar.hwid,2)), 
            c(max(data1[i,3+2],test[i,4+10]),rep(test[i,5+10],2), max(data1[i,3+2],test[i,4+10])), col=rgb(0,0,0,0.2), border=F)	 #dodgerblue2
  }
}


dev.off()

