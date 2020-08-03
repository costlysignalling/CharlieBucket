
sescols<-c("#FF0000",3,"#0000FF")

nice.cor<-function(object,col1,col2){
x<-object$time
y<-object$surv
yu<-object$upper
lou <- loess(yu~x,span=spanset,degree=1)
yl<-object$lower
lol <- loess(yl~x,span=spanset,degree=1)
polygon(c(x,rev(x)),c(predict(lou),rev(predict(lol))),col=col2,border=NA)
lo <- loess(y~x,span=spanset,degree=1)
lines(x,predict(lo), col=col1, lwd=1)
}




###first I will calculate together survival

y_all<-Surv(time.data$time1, time.data$time2, time.data$death)
fit.all <- survfit(y_all ~ 1)

#Define survival corridors for individual SES

y_farmer<-Surv(time.data.farmer$time1, time.data.farmer$time2, time.data.farmer$death)
fit.farmer <- survfit(y_farmer ~ 1)

y_cottager<-Surv(time.data.cottager$time1, time.data.cottager$time2, time.data.cottager$death)
fit.cottager <- survfit(y_cottager~ 1)

y_houseless<-Surv(time.data.houseless$time1, time.data.houseless$time2, time.data.houseless$death)
fit.houseless <- survfit(y_houseless~ 1)

counter<-1

tiff(paste("grandmas",counter,".tif",sep=""),width=13,height=13,units="cm",res=600,compression="lzw");counter<-counter+1
plot(fit.farmer,col=2, ylim=c(botylim,1),xlim=c(bottom*365,threshold*365),xaxt="n",xlab="Years",ylab="Proportion of surviving individuals")
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col="#FFFFFF")
nice.cor(fit.farmer,"#FF0000","#FF000022")
nice.cor(fit.cottager,"#00CD00","#00CD0022")
nice.cor(fit.houseless,"#0000FF","#0000FF22")
title("Parental status",adj=0)
axis(1,c(0:5)*365,0:5)
legend("topright",c("Farm holders","Cottagers","Lodgers","95% CI"),col=c(2,3,4,"#00000022"),lwd=c(2,2,2,18),lty=c(1,1,1,1),bty="n")
legend("topright",c("Farm holders","Cottagers","Lodgers","95% CI"),col=c(2,3,4,"black"),lwd=c(2,2,2,2),lty=c(1,1,1,1),bg="transparent",bty="n")
dev.off()


#Grandmothers in the whole sample
time.data.any<-time.data[time.data$group!="None",]
y_any<-Surv(time.data.any$time1, time.data.any$time2, time.data.any$death)
fit.any <- survfit(y_any ~ 1)

time.data.none<-time.data[time.data$group=="None",]
y_none<-Surv(time.data.none$time1, time.data.none$time2, time.data.none$death)
fit.none <- survfit(y_none ~ 1)

time.data.both<-time.data[time.data$group=="Both",]
y_both<-Surv(time.data.both$time1, time.data.both$time2, time.data.both$death)
fit.both <- survfit(y_both ~ 1)

time.data.paternal<-time.data[time.data$group=="Po",]
y_paternal<-Surv(time.data.paternal$time1, time.data.paternal$time2, time.data.paternal$death)
fit.paternal <- survfit(y_paternal ~ 1)

time.data.maternal<-time.data[time.data$group=="Mo",]
y_maternal<-Surv(time.data.maternal$time1, time.data.maternal$time2, time.data.maternal$death)
fit.maternal <- survfit(y_maternal ~ 1)


any.col<-"#FF00FF"
both.col<-"#66BB00"
paternal.col<-"#6666DD"
maternal.col<-"#DD6666"
none.col<-"#0066FF"

tiff(paste("grandmas",counter,".tif",sep=""),width=13,height=13,units="cm",res=600,compression="lzw");counter<-counter+1
plot(fit.any,col=2, ylim=c(botylim,1),xlim=c(bottom*365,threshold*365),xaxt="n",xlab="Years",ylab="Proportion of surviving individuals")
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col="#FFFFFF")
nice.cor(fit.any,any.col,paste(any.col,"33",sep=""))
nice.cor(fit.none,none.col,paste(none.col,"33",sep=""))
title("Grandmother presence and survival",adj=0)
axis(1,c(0:5)*365,0:5)
legend("topright",c("Grandmothers present","Grandmothers absent","95% CI"),col=c("#FF00FF","#00B0F0","#00000022"),lwd=c(2,2,18),lty=c(1,1,1),bty="n")
legend("topright",c("Grandmothers present","Grandmothers absent","95% CI"),col=c("#FF00FF","#00B0F0","#000000"),lwd=c(2,2,2),lty=c(1,1,1),bty="n")
dev.off()


tiff(paste("grandmas",counter,".tif",sep=""),width=13,height=13,units="cm",res=600,compression="lzw");counter<-counter+1
plot(fit.both,col=2, ylim=c(botylim,1),xlim=c(bottom*365,threshold*365),xaxt="n",xlab="Years",ylab="Proportion of surviving individuals")
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col="#FFFFFF")
nice.cor(fit.both,both.col,paste(both.col,"18",sep=""))
nice.cor(fit.paternal,paternal.col,paste(paternal.col,"18",sep=""))
nice.cor(fit.maternal,maternal.col,paste(maternal.col,"18",sep=""))
nice.cor(fit.none,none.col,paste(none.col,"18",sep=""))
title("Grandmother presence and survival",adj=0)
axis(1,c(0:5)*365,0:5)
legend("topright",c("Both GM present","Paternal GM present","Maternal GM present","Both GM absent","95% CI"),col=c(both.col,paternal.col,maternal.col,none.col,"#00000022"),lwd=c(2,2,2,2,18),lty=c(1,1,1,1,1),bty="n")
legend("topright",c("Both GM present","Paternal GM present","Maternal GM present","Both GM absent","95% CI"),col=c(both.col,paternal.col,maternal.col,none.col,"#000000"),lwd=c(2,2,2,2,2),lty=c(1,1,1,1,1),bty="n")
dev.off()





#Grandmothers in the Farmer sample
time.data.farmer.any<-time.data.farmer[time.data.farmer$group!="None",]
y_any<-Surv(time.data.farmer.any$time1, time.data.farmer.any$time2, time.data.farmer.any$death)
fit.any <- survfit(y_any ~ 1)

time.data.farmer.none<-time.data.farmer[time.data.farmer$group=="None",]
y_none<-Surv(time.data.farmer.none$time1, time.data.farmer.none$time2, time.data.farmer.none$death)
fit.none <- survfit(y_none ~ 1)

time.data.farmer.both<-time.data.farmer[time.data.farmer$group=="Both",]
y_both<-Surv(time.data.farmer.both$time1, time.data.farmer.both$time2, time.data.farmer.both$death)
fit.both <- survfit(y_both ~ 1)

time.data.farmer.paternal<-time.data.farmer[time.data.farmer$group=="Po",]
y_paternal<-Surv(time.data.farmer.paternal$time1, time.data.farmer.paternal$time2, time.data.farmer.paternal$death)
fit.paternal <- survfit(y_paternal ~ 1)

time.data.farmer.maternal<-time.data.farmer[time.data.farmer$group=="Mo",]
y_maternal<-Surv(time.data.farmer.maternal$time1, time.data.farmer.maternal$time2, time.data.farmer.maternal$death)
fit.maternal <- survfit(y_maternal ~ 1)


any.col<-"#FFA034"
none.col<-"#FF0085"

tiff(paste("grandmas",counter,".tif",sep=""),width=13,height=13,units="cm",res=600,compression="lzw");counter<-counter+1
plot(fit.any,col=2, ylim=c(botylim,1),xlim=c(bottom*365,threshold*365),xaxt="n",xlab="Years",ylab="Proportion of surviving individuals")
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col="#FFFFFF")
nice.cor(fit.any,any.col,paste(any.col,"33",sep=""))
nice.cor(fit.none,none.col,paste(none.col,"33",sep=""))
title("Farmers",adj=0,col.main=sescols[1])
axis(1,c(0:5)*365,0:5)
legend("topright",c("Grandmothers present","Grandmothers absent","95% CI"),col=c(any.col,none.col,"#00000022"),lwd=c(2,2,18),lty=c(1,1,1),bty="n")
legend("topright",c("Grandmothers present","Grandmothers absent","95% CI"),col=c(any.col,none.col,"#000000"),lwd=c(2,2,2),lty=c(1,1,1),bty="n")
dev.off()

any.col<-"#FF00FF"
both.col<-"#66BB00"
paternal.col<-"#6666DD"
maternal.col<-"#DD6666"
none.col<-"#0066FF"

tiff(paste("grandmas",counter,".tif",sep=""),width=13,height=13,units="cm",res=600,compression="lzw");counter<-counter+1
plot(fit.both,col=2, ylim=c(botylim,1),xlim=c(bottom*365,threshold*365),xaxt="n",xlab="Years",ylab="Proportion of surviving individuals")
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col="#FFFFFF")
nice.cor(fit.both,both.col,paste(both.col,"18",sep=""))
nice.cor(fit.paternal,paternal.col,paste(paternal.col,"18",sep=""))
nice.cor(fit.maternal,maternal.col,paste(maternal.col,"18",sep=""))
nice.cor(fit.none,none.col,paste(none.col,"18",sep=""))
title("Farmers",adj=0,col.main=sescols[1])
axis(1,c(0:5)*365,0:5)
legend("topright",c("Both GM present","Paternal GM present","Maternal GM present","Both GM absent","95% CI"),col=c(both.col,paternal.col,maternal.col,none.col,"#00000022"),lwd=c(2,2,2,2,18),lty=c(1,1,1,1,1),bty="n")
legend("topright",c("Both GM present","Paternal GM present","Maternal GM present","Both GM absent","95% CI"),col=c(both.col,paternal.col,maternal.col,none.col,"#000000"),lwd=c(2,2,2,2,2),lty=c(1,1,1,1,1),bty="n")
dev.off()





#Grandmothers in the Cottager sample
time.data.cottager.any<-time.data.cottager[time.data.cottager$group!="None",]
y_any<-Surv(time.data.cottager.any$time1, time.data.cottager.any$time2, time.data.cottager.any$death)
fit.any <- survfit(y_any ~ 1)

time.data.cottager.none<-time.data.cottager[time.data.cottager$group=="None",]
y_none<-Surv(time.data.cottager.none$time1, time.data.cottager.none$time2, time.data.cottager.none$death)
fit.none <- survfit(y_none ~ 1)

time.data.cottager.both<-time.data.cottager[time.data.cottager$group=="Both",]
y_both<-Surv(time.data.cottager.both$time1, time.data.cottager.both$time2, time.data.cottager.both$death)
fit.both <- survfit(y_both ~ 1)

time.data.cottager.paternal<-time.data.cottager[time.data.cottager$group=="Po",]
y_paternal<-Surv(time.data.cottager.paternal$time1, time.data.cottager.paternal$time2, time.data.cottager.paternal$death)
fit.paternal <- survfit(y_paternal ~ 1)

time.data.cottager.maternal<-time.data.cottager[time.data.cottager$group=="Mo",]
y_maternal<-Surv(time.data.cottager.maternal$time1, time.data.cottager.maternal$time2, time.data.cottager.maternal$death)
fit.maternal <- survfit(y_maternal ~ 1)


any.col<-"#30FF9E"
none.col<-"#A7FF43"

tiff(paste("grandmas",counter,".tif",sep=""),width=13,height=13,units="cm",res=600,compression="lzw");counter<-counter+1
plot(fit.any,col=2, ylim=c(botylim,1),xlim=c(bottom*365,threshold*365),xaxt="n",xlab="Years",ylab="Proportion of surviving individuals")
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col="#FFFFFF")
nice.cor(fit.any,any.col,paste(any.col,"33",sep=""))
nice.cor(fit.none,none.col,paste(none.col,"33",sep=""))
title("Cottagers",adj=0,col.main=sescols[2])
axis(1,c(0:5)*365,0:5)
legend("topright",c("Grandmothers present","Grandmothers absent","95% CI"),col=c(any.col,none.col,"#00000022"),lwd=c(2,2,18),lty=c(1,1,1),bty="n")
legend("topright",c("Grandmothers present","Grandmothers absent","95% CI"),col=c(any.col,none.col,"#000000"),lwd=c(2,2,2),lty=c(1,1,1),bty="n")
dev.off()

any.col<-"#FF00FF"
both.col<-"#66BB00"
paternal.col<-"#6666DD"
maternal.col<-"#DD6666"
none.col<-"#0066FF"

tiff(paste("grandmas",counter,".tif",sep=""),width=13,height=13,units="cm",res=600,compression="lzw");counter<-counter+1
plot(fit.both,col=2, ylim=c(botylim,1),xlim=c(bottom*365,threshold*365),xaxt="n",xlab="Years",ylab="Proportion of surviving individuals")
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col="#FFFFFF")
nice.cor(fit.both,both.col,paste(both.col,"18",sep=""))
nice.cor(fit.paternal,paternal.col,paste(paternal.col,"18",sep=""))
nice.cor(fit.maternal,maternal.col,paste(maternal.col,"18",sep=""))
nice.cor(fit.none,none.col,paste(none.col,"18",sep=""))
title("Cottagers",adj=0,col.main=sescols[2])
axis(1,c(0:5)*365,0:5)
legend("topright",c("Both GM present","Paternal GM present","Maternal GM present","Both GM absent","95% CI"),col=c(both.col,paternal.col,maternal.col,none.col,"#00000022"),lwd=c(2,2,2,2,18),lty=c(1,1,1,1,1),bty="n")
legend("topright",c("Both GM present","Paternal GM present","Maternal GM present","Both GM absent","95% CI"),col=c(both.col,paternal.col,maternal.col,none.col,"#000000"),lwd=c(2,2,2,2,2),lty=c(1,1,1,1,1),bty="n")
dev.off()










#Grandmothers in the Houseless lodger sample
time.data.houseless.any<-time.data.houseless[time.data.houseless$group!="None",]
y_any<-Surv(time.data.houseless.any$time1, time.data.houseless.any$time2, time.data.houseless.any$death)
fit.any <- survfit(y_any ~ 1)

time.data.houseless.none<-time.data.houseless[time.data.houseless$group=="None",]
y_none<-Surv(time.data.houseless.none$time1, time.data.houseless.none$time2, time.data.houseless.none$death)
fit.none <- survfit(y_none ~ 1)

time.data.houseless.both<-time.data.houseless[time.data.houseless$group=="Both",]
y_both<-Surv(time.data.houseless.both$time1, time.data.houseless.both$time2, time.data.houseless.both$death)
fit.both <- survfit(y_both ~ 1)

time.data.houseless.paternal<-time.data.houseless[time.data.houseless$group=="Po",]
y_paternal<-Surv(time.data.houseless.paternal$time1, time.data.houseless.paternal$time2, time.data.houseless.paternal$death)
fit.paternal <- survfit(y_paternal ~ 1)

time.data.houseless.maternal<-time.data.houseless[time.data.houseless$group=="Mo",]
y_maternal<-Surv(time.data.houseless.maternal$time1, time.data.houseless.maternal$time2, time.data.houseless.maternal$death)
fit.maternal <- survfit(y_maternal ~ 1)


any.col<-"#8500FF"
none.col<-"#0083FF"

tiff(paste("grandmas",counter,".tif",sep=""),width=13,height=13,units="cm",res=600,compression="lzw");counter<-counter+1
plot(fit.any,col=2, ylim=c(botylim,1),xlim=c(bottom*365,threshold*365),xaxt="n",xlab="Years",ylab="Proportion of surviving individuals")
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col="#FFFFFF")
nice.cor(fit.any,any.col,paste(any.col,"33",sep=""))
nice.cor(fit.none,none.col,paste(none.col,"33",sep=""))
title("Lodgers",adj=0,col.main=sescols[3])
axis(1,c(0:5)*365,0:5)
legend("topright",c("Grandmothers present","Grandmothers absent","95% CI"),col=c(any.col,none.col,"#00000022"),lwd=c(2,2,18),lty=c(1,1,1),bty="n")
legend("topright",c("Grandmothers present","Grandmothers absent","95% CI"),col=c(any.col,none.col,"#000000"),lwd=c(2,2,2),lty=c(1,1,1),bty="n")
dev.off()

any.col<-"#FF00FF"
both.col<-"#66BB00"
paternal.col<-"#6666DD"
maternal.col<-"#DD6666"
none.col<-"#0066FF"

tiff(paste("grandmas",counter,".tif",sep=""),width=13,height=13,units="cm",res=600,compression="lzw");counter<-counter+1
plot(fit.both,col=2, ylim=c(botylim,1),xlim=c(bottom*365,threshold*365),xaxt="n",xlab="Years",ylab="Proportion of surviving individuals")
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col="#FFFFFF")
nice.cor(fit.both,both.col,paste(both.col,"18",sep=""))
nice.cor(fit.paternal,paternal.col,paste(paternal.col,"18",sep=""))
nice.cor(fit.maternal,maternal.col,paste(maternal.col,"18",sep=""))
nice.cor(fit.none,none.col,paste(none.col,"18",sep=""))
title("Lodgers",adj=0,col.main=sescols[3])
axis(1,c(0:5)*365,0:5)
legend("topright",c("Both GM present","Paternal GM present","Maternal GM present","Both GM absent","95% CI"),col=c(both.col,paternal.col,maternal.col,none.col,"#00000022"),lwd=c(2,2,2,2,18),lty=c(1,1,1,1,1),bty="n")
legend("topright",c("Both GM present","Paternal GM present","Maternal GM present","Both GM absent","95% CI"),col=c(both.col,paternal.col,maternal.col,none.col,"#000000"),lwd=c(2,2,2,2,2),lty=c(1,1,1,1,1),bty="n")
dev.off()













