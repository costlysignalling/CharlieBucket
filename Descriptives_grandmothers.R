data<-read.table("source.data.GM.txt", sep="\t", header=T)

data$SES<-factor(data$SES,levels=c("farmer","cottager","houseless"))

GMM.age.chb<-data$GMM.age.chb
GMM.age.chb[data$GMM.age.chb>data$GMM.age.d]<-NA

GMM.age.d<-data$GMM.age.d
GMM.age.d[data$GMM.age.chb>data$GMM.age.d]<-NA

GMP.age.chb<-data$GMP.age.chb
GMP.age.chb[data$GMP.age.chb>data$GMP.age.d]<-NA

GMP.age.d<-data$GMP.age.d
GMP.age.d[data$GMP.age.chb>data$GMP.age.d]<-NA


#For grandmothers in total
GM<-unique(c(data$GMM,data$GMP))
GM<-GM[!is.na(GM)]

GMstart<-NA
death<-NA
stats<-list()
GMn<-NA

data$code


for(i in 1:length(GM)){
  GMstart[i]<-min(c(GMM.age.chb[which(data$GMM==GM[i])],GMP.age.chb[which(data$GMP==GM[i])]),na.rm=T)
  death[i]<-mean(c(GMM.age.d[which(data$GMM==GM[i])],GMP.age.d[which(data$GMP==GM[i])]),na.rm=T)
  stats[[i]]<-c(data$SES[which(data$GMM==GM[i])],data$SES[which(data$GMP==GM[i])])
  GMn[i]<-length(c(data$code[which(data$GMM==GM[i])],data$code[which(data$GMP==GM[i])]))
}

GMstart[!is.finite(GMstart)]<-NA
death[!is.finite(death)]<-NA

#Proportion of status among grandchildren
stat.r<-lapply(stats,function(i){c(sum(i==1)/length(i),sum(i==2)/length(i),sum(i==3)/length(i))})

stm<-matrix(unlist(stat.r),byrow=T,ncol=3)

#Weighted means
mf<-weighted.mean(GMstart,w=stm[,1],na.rm=T)
mc<-weighted.mean(GMstart,w=stm[,2],na.rm=T)
mh<-weighted.mean(GMstart,w=stm[,3],na.rm=T)

#Weighted standard deviations
sf<-sqrt(sum(((GMstart-mf)^2)*stm[,1],na.rm=T)/sum(stm[,1]))
sc<-sqrt(sum(((GMstart-mc)^2)*stm[,2],na.rm=T)/sum(stm[,2]))
sh<-sqrt(sum(((GMstart-mh)^2)*stm[,3],na.rm=T)/sum(stm[,3]))

m<-mean(GMstart,na.rm=T)
s<-sd(GMstart,na.rm=T)

#Death ages
#Weighted means
dmf<-weighted.mean(death,w=stm[,1],na.rm=T)
dmc<-weighted.mean(death,w=stm[,2],na.rm=T)
dmh<-weighted.mean(death,w=stm[,3],na.rm=T)

#Weighted standard deviations
dsf<-sqrt(sum(((death-dmf)^2)*stm[,1],na.rm=T)/sum(stm[,1]))
dsc<-sqrt(sum(((death-dmc)^2)*stm[,2],na.rm=T)/sum(stm[,2]))
dsh<-sqrt(sum(((death-dmh)^2)*stm[,3],na.rm=T)/sum(stm[,3]))

dm<-mean(death,na.rm=T)
ds<-sd(death,na.rm=T)

#Number of grandchildren
#Weighted means
Nmf<-weighted.mean(GMn,w=stm[,1],na.rm=T)
Nmc<-weighted.mean(GMn,w=stm[,2],na.rm=T)
Nmh<-weighted.mean(GMn,w=stm[,3],na.rm=T)

#Weighted standard deviations
Nsf<-sqrt(sum(((GMn-Nmf)^2)*stm[,1],na.rm=T)/sum(stm[,1]))
Nsc<-sqrt(sum(((GMn-Nmc)^2)*stm[,2],na.rm=T)/sum(stm[,2]))
Nsh<-sqrt(sum(((GMn-Nmh)^2)*stm[,3],na.rm=T)/sum(stm[,3]))

Nm<-mean(GMn,na.rm=T)
Ns<-sd(GMn,na.rm=T)



#As maternal mothers only
GMM<-unique(c(data$GMM))
GMM<-GMM[!is.na(GMM)]

GMMstart<-NA
Mdeath<-NA
Mstats<-list()
GMMn<-NA

for(i in 1:length(GMM)){
  GMMstart[i]<-min(c(GMM.age.chb[which(data$GMM==GMM[i])]),na.rm=T)
  Mdeath[i]<-mean(c(GMM.age.d[which(data$GMM==GMM[i])]),na.rm=T)
  Mstats[[i]]<-c(data$SES[which(data$GMM==GMM[i])])
  GMMn[i]<-length(c(data$code[which(data$GMM==GMM[i])]))
}

GMMstart[!is.finite(GMMstart)]<-NA
Mdeath[!is.finite(Mdeath)]<-NA

#Proportion of status among grandchildren
Mstat.r<-lapply(Mstats,function(i){c(sum(i==1)/length(i),sum(i==2)/length(i),sum(i==3)/length(i))})
Mstm<-matrix(unlist(Mstat.r),byrow=T,ncol=3)

#Weighted means
mfm<-weighted.mean(GMMstart,w=Mstm[,1],na.rm=T)
mcm<-weighted.mean(GMMstart,w=Mstm[,2],na.rm=T)
mhm<-weighted.mean(GMMstart,w=Mstm[,3],na.rm=T)

#Weighted standard deviations
sfm<-sqrt(sum(((GMMstart-mfm)^2)*Mstm[,1],na.rm=T)/sum(Mstm[,1],na.rm=T))
scm<-sqrt(sum(((GMMstart-mcm)^2)*Mstm[,2],na.rm=T)/sum(Mstm[,2],na.rm=T))
shm<-sqrt(sum(((GMMstart-mhm)^2)*Mstm[,3],na.rm=T)/sum(Mstm[,3],na.rm=T))

mm<-mean(GMMstart,na.rm=T)
sm<-sd(GMMstart,na.rm=T)

#Number of grandildren
#Weighted means
Nmfm<-weighted.mean(GMMn,w=Mstm[,1],na.rm=T)
Nmcm<-weighted.mean(GMMn,w=Mstm[,2],na.rm=T)
Nmhm<-weighted.mean(GMMn,w=Mstm[,3],na.rm=T)

#Weighted standard deviations
Nsfm<-sqrt(sum(((GMMn-Nmfm)^2)*Mstm[,1],na.rm=T)/sum(Mstm[,1],na.rm=T))
Nscm<-sqrt(sum(((GMMn-Nmcm)^2)*Mstm[,2],na.rm=T)/sum(Mstm[,2],na.rm=T))
Nshm<-sqrt(sum(((GMMn-Nmhm)^2)*Mstm[,3],na.rm=T)/sum(Mstm[,3],na.rm=T))

Nmm<-mean(GMMn,na.rm=T)
Nsm<-sd(GMMn,na.rm=T)




#As maternal mothers only
GMP<-unique(c(data$GMP))
GMP<-GMP[!is.na(GMP)]

GMPstart<-NA
Pdeath<-NA
Pstats<-list()
GMPn<-NA

for(i in 1:length(GMP)){
  GMPstart[i]<-min(c(GMP.age.chb[which(data$GMP==GMP[i])]),na.rm=T)
  Pdeath[i]<-mean(c(GMP.age.d[which(data$GMP==GMP[i])]),na.rm=T)
  Pstats[[i]]<-c(data$SES[which(data$GMP==GMP[i])])
  GMPn[i]<-length(c(data$code[which(data$GMP==GMP[i])]))
}

GMPstart[!is.finite(GMPstart)]<-NA
Pdeath[!is.finite(Pdeath)]<-NA

#Proportion of status among grandchildren
Pstat.r<-lapply(Pstats,function(i){c(sum(i==1)/length(i),sum(i==2)/length(i),sum(i==3)/length(i))})
Pstm<-matrix(unlist(Pstat.r),byrow=T,ncol=3)

#Weighted means
mfp<-weighted.mean(GMPstart,w=Pstm[,1],na.rm=T)
mcp<-weighted.mean(GMPstart,w=Pstm[,2],na.rm=T)
mhp<-weighted.mean(GMPstart,w=Pstm[,3],na.rm=T)

#Weighted standard deviations
sfp<-sqrt(sum(((GMPstart-mfp)^2)*Pstm[,1],na.rm=T)/sum(Pstm[,1],na.rm=T))
scp<-sqrt(sum(((GMPstart-mcp)^2)*Pstm[,2],na.rm=T)/sum(Pstm[,2],na.rm=T))
shp<-sqrt(sum(((GMPstart-mhp)^2)*Pstm[,3],na.rm=T)/sum(Pstm[,3],na.rm=T))

mp<-mean(GMPstart,na.rm=T)
sp<-sd(GMPstart,na.rm=T)


#Number of grandchildren
#Weighted means
Nmfp<-weighted.mean(GMPn,w=Pstm[,1],na.rm=T)
Nmcp<-weighted.mean(GMPn,w=Pstm[,2],na.rm=T)
Nmhp<-weighted.mean(GMPn,w=Pstm[,3],na.rm=T)

#Weighted standard deviations
Nsfp<-sqrt(sum(((GMPn-Nmfp)^2)*Pstm[,1],na.rm=T)/sum(Pstm[,1],na.rm=T))
Nscp<-sqrt(sum(((GMPn-Nmcp)^2)*Pstm[,2],na.rm=T)/sum(Pstm[,2],na.rm=T))
Nshp<-sqrt(sum(((GMPn-Nmhp)^2)*Pstm[,3],na.rm=T)/sum(Pstm[,3],na.rm=T))

Nmp<-mean(GMPn,na.rm=T)
Nsp<-sd(GMPn,na.rm=T)


MEANS<-matrix(c(mean(GMM.age.chb,na.rm=T),
                tapply(GMM.age.chb,data$SES,mean,na.rm=T),
                mean(GMP.age.chb,na.rm=T),
                tapply(GMP.age.chb,data$SES,mean,na.rm=T),
                m,mf,mc,mh,
                mm,mfm,mcm,mhm,
                mp,mfp,mcp,mhp,
                dm,dmf,dmc,dmh,
                Nm,Nmf,Nmc,Nmh,
                Nmm,Nmfm,Nmcm,Nmhm,
                Nmp,Nmfp,Nmcp,Nmhp),ncol=4,byrow = T)

SDS<-matrix(c(sd(GMM.age.chb,na.rm=T),
                tapply(GMM.age.chb,data$SES,sd,na.rm=T),
                sd(GMP.age.chb,na.rm=T),
                tapply(GMP.age.chb,data$SES,sd,na.rm=T),
                s,sf,sc,sh,
                sm,sfm,scm,shm,
                sp,sfp,scp,shp,
                ds,dsf,dsc,dsh,
                Ns,Nsf,Nsc,Nsh,
                Nsm,Nsfm,Nscm,Nshm,
                Nsp,Nsfp,Nscp,Nshp),ncol=4,byrow = T)

outp<-matrix(paste(format(round(MEANS,2),nsmall=2)," (",format(round(SDS,2),nsmall=2),") ",sep=""),ncol=4)

rownames(outp)<-c("GMM age at birth","GMP age at birth","Age at first grandchild","Age at first grandchild as maternal","Age at first grandchild as paternal","Age at death","Total number of recorded chrandchildren","Total number of recorded chrandchildren as maternal","Total number of recorded chrandchildren as paternal")
colnames(outp)<-c("Total","Farmers","Cottagers","Lodgers")

outp

write.table(outp,"GMages.txt",col.names = NA,sep="\t")




cor(data$GMM.age.chb,data$mother.age,use="complete.obs")
cor(data$GMP.age.chb,data$mother.age,use="complete.obs")

cor(data$GMM.age.chb,data$GMP.age.chb,use="complete.obs")


cor(GMM.age.chb,data$mother.age,use="complete.obs")
cor(GMP.age.chb,data$mother.age,use="complete.obs")

cor(GMM.age.chb,GMP.age.chb,use="complete.obs")



summary(GMM.age.chb[!is.na(data$GMM)])
length(GMM.age.chb[!is.na(data$GMM)])

summary(GMP.age.chb[!is.na(data$GMP)])
length(GMP.age.chb[!is.na(data$GMP)])


