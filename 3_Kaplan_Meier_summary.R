
time.data.farmer<-time.data[time.data$SES=="farmer",]
time.data.cottager<-time.data[time.data$SES=="cottager",]
time.data.houseless<-time.data[time.data$SES=="houseless",]

y_all<-Surv(time.data$time1, time.data$time2, time.data$death)
fit.all <- survfit(y_all ~ 1)

#Define survival corridors for individual SES

y_farmer<-Surv(time.data.farmer$time1, time.data.farmer$time2, time.data.farmer$death)
fit.farmer <- survfit(y_farmer ~ 1)

y_cottager<-Surv(time.data.cottager$time1, time.data.cottager$time2, time.data.cottager$death)
fit.cottager <- survfit(y_cottager~ 1)

y_houseless<-Surv(time.data.houseless$time1, time.data.houseless$time2, time.data.houseless$death)
fit.houseless <- survfit(y_houseless~ 1)



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

time.data.one<-time.data[time.data$group=="Po"|time.data$group=="Mo",]
y_one<-Surv(time.data.one$time1, time.data.one$time2, time.data.one$death)
fit.one <- survfit(y_one ~ 1)

time.data.paternal<-time.data[time.data$group=="Po",]
y_paternal<-Surv(time.data.paternal$time1, time.data.paternal$time2, time.data.paternal$death)
fit.paternal <- survfit(y_paternal ~ 1)

time.data.maternal<-time.data[time.data$group=="Mo",]
y_maternal<-Surv(time.data.maternal$time1, time.data.maternal$time2, time.data.maternal$death)
fit.maternal <- survfit(y_maternal ~ 1)



#Grandmothers in the Farmer sample
time.data.farmer.any<-time.data.farmer[time.data.farmer$group!="None",]
y_any<-Surv(time.data.farmer.any$time1, time.data.farmer.any$time2, time.data.farmer.any$death)
fit.f.any <- survfit(y_any ~ 1)

time.data.farmer.none<-time.data.farmer[time.data.farmer$group=="None",]
y_none<-Surv(time.data.farmer.none$time1, time.data.farmer.none$time2, time.data.farmer.none$death)
fit.f.none <- survfit(y_none ~ 1)

time.data.farmer.both<-time.data.farmer[time.data.farmer$group=="Both",]
y_both<-Surv(time.data.farmer.both$time1, time.data.farmer.both$time2, time.data.farmer.both$death)
fit.f.both <- survfit(y_both ~ 1)

time.data.farmer.one<-time.data.farmer[time.data.farmer$group=="Po"|time.data.farmer$group=="Mo",]
y_one<-Surv(time.data.farmer.one$time1, time.data.farmer.one$time2, time.data.farmer.one$death)
fit.f.one <- survfit(y_one ~ 1)

time.data.farmer.paternal<-time.data.farmer[time.data.farmer$group=="Po",]
y_paternal<-Surv(time.data.farmer.paternal$time1, time.data.farmer.paternal$time2, time.data.farmer.paternal$death)
fit.f.paternal <- survfit(y_paternal ~ 1)

time.data.farmer.maternal<-time.data.farmer[time.data.farmer$group=="Mo",]
y_maternal<-Surv(time.data.farmer.maternal$time1, time.data.farmer.maternal$time2, time.data.farmer.maternal$death)
fit.f.maternal <- survfit(y_maternal ~ 1)




#Grandmothers in the Cottager sample
time.data.cottager.any<-time.data.cottager[time.data.cottager$group!="None",]
y_any<-Surv(time.data.cottager.any$time1, time.data.cottager.any$time2, time.data.cottager.any$death)
fit.c.any <- survfit(y_any ~ 1)

time.data.cottager.none<-time.data.cottager[time.data.cottager$group=="None",]
y_none<-Surv(time.data.cottager.none$time1, time.data.cottager.none$time2, time.data.cottager.none$death)
fit.c.none <- survfit(y_none ~ 1)

time.data.cottager.both<-time.data.cottager[time.data.cottager$group=="Both",]
y_both<-Surv(time.data.cottager.both$time1, time.data.cottager.both$time2, time.data.cottager.both$death)
fit.c.both <- survfit(y_both ~ 1)

time.data.cottager.one<-time.data.cottager[time.data.cottager$group=="Po"|time.data.cottager$group=="Mo",]
y_one<-Surv(time.data.cottager.one$time1, time.data.cottager.one$time2, time.data.cottager.one$death)
fit.c.one <- survfit(y_one ~ 1)

time.data.cottager.paternal<-time.data.cottager[time.data.cottager$group=="Po",]
y_paternal<-Surv(time.data.cottager.paternal$time1, time.data.cottager.paternal$time2, time.data.cottager.paternal$death)
fit.c.paternal <- survfit(y_paternal ~ 1)

time.data.cottager.maternal<-time.data.cottager[time.data.cottager$group=="Mo",]
y_maternal<-Surv(time.data.cottager.maternal$time1, time.data.cottager.maternal$time2, time.data.cottager.maternal$death)
fit.c.maternal <- survfit(y_maternal ~ 1)



#Grandmothers in the Houseless lodger sample
time.data.houseless.any<-time.data.houseless[time.data.houseless$group!="None",]
y_any<-Surv(time.data.houseless.any$time1, time.data.houseless.any$time2, time.data.houseless.any$death)
fit.h.any <- survfit(y_any ~ 1)

time.data.houseless.none<-time.data.houseless[time.data.houseless$group=="None",]
y_none<-Surv(time.data.houseless.none$time1, time.data.houseless.none$time2, time.data.houseless.none$death)
fit.h.none <- survfit(y_none ~ 1)

time.data.houseless.both<-time.data.houseless[time.data.houseless$group=="Both",]
y_both<-Surv(time.data.houseless.both$time1, time.data.houseless.both$time2, time.data.houseless.both$death)
fit.h.both <- survfit(y_both ~ 1)

time.data.houseless.one<-time.data.houseless[time.data.houseless$group=="Po"|time.data.houseless$group=="Mo",]
y_one<-Surv(time.data.houseless.one$time1, time.data.houseless.one$time2, time.data.houseless.one$death)
fit.h.one <- survfit(y_one ~ 1)

time.data.houseless.paternal<-time.data.houseless[time.data.houseless$group=="Po",]
y_paternal<-Surv(time.data.houseless.paternal$time1, time.data.houseless.paternal$time2, time.data.houseless.paternal$death)
fit.h.paternal <- survfit(y_paternal ~ 1)

time.data.houseless.maternal<-time.data.houseless[time.data.houseless$group=="Mo",]
y_maternal<-Surv(time.data.houseless.maternal$time1, time.data.houseless.maternal$time2, time.data.houseless.maternal$death)
fit.h.maternal <- survfit(y_maternal ~ 1)



time.end<-length(fit.all$time)


fits<-list(fit.all,
           fit.any,
           fit.none,
           fit.both,
           fit.one,
           fit.paternal,
           fit.maternal,
           
           fit.farmer,
           fit.f.any,
           fit.f.none,
           fit.f.both,
           fit.f.one,
           fit.f.paternal,
           fit.f.maternal,
           
           fit.cottager,
           fit.c.any,
           fit.c.none,
           fit.c.both,
           fit.c.one,
           fit.c.paternal,
           fit.c.maternal,
           
           fit.houseless,
           fit.h.any,
           fit.h.none,
           fit.h.both,
           fit.h.one,
           fit.h.paternal,
           fit.h.maternal)

survs<-sapply(fits,function(fit){fit$surv[length(fit$surv)]})
s.errs<-sapply(fits,function(fit){fit$std.err[length(fit$std.err)]})

rnames<-c("Whole sample","Farmers","Cottagers","Houseless lodgers")
cnames<-c("All","Any","None","Both","One","Mat","Pat")

survs<-matrix(survs,ncol=7,byrow = T)
s.errs<-matrix(s.errs,ncol=7,byrow = T)

rownames(survs)<-rnames
colnames(survs)<-cnames

rownames(s.errs)<-rnames
colnames(s.errs)<-cnames

survs
s.errs

report<-paste(format(round(survs,2),nsmall=2)," (",format(round(s.errs,2),nsmall=2),")",sep="")

report<-matrix(report,ncol=7)

rownames(report)<-rnames
colnames(report)<-cnames


write.table(survs,"survs.txt",sep="\t",col.names=NA)
write.table(s.errs,"s.errs.txt",sep="\t",col.names=NA)
write.table(report,"report.txt",sep="\t",col.names=NA)

save(fits, file = paste("fits",maxdist,".RData",sep=""))

