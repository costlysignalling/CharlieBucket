#Function that allows to calculate N risk preisely (the built-in versions ignore transitive cases, where individual starts with two grandmothers but later switches to one etc.)
extractN<-function(vector,label){
value<-vector[names(vector)==label]
value<-ifelse(length(value)>0,value,0)
return(value)
}

rounding<-2

contlabs<-c("Grandmother presence","Number of grandmothers","Grandmother")
testlabs<-c("N risk","N event","HR","CI")
grouplabs1<-c("any","transient","none")
grouplabs2<-c("two","transient","one")
grouplabs3<-c("maternal","","paternal")

time.data$SES<-as.factor(time.data$SES)

ncodes<-names(summary(as.factor(time.data$subject),maxsum=100000))

SES<-NA
DEATH<-NA

ndata<-time.data

for(i in 1:length(ncodes)){
subdata<-ndata[ndata$subject==ncodes[i],]
SES[i]<-as.character(subdata$SES[nrow(subdata)])
DEATH[i]<-subdata$death[nrow(subdata)]
}

tabSES<-table(SES,DEATH)
tabSES<-cbind(rowSums(tabSES),tabSES[,2])
tabSES<-tabSES[c(2,1,3),]
tabSES<-unname(cbind(rownames(tabSES),tabSES))

model <- coxme(Surv(time1, time2, death) ~  SES+ (1|subject),
		 data=time.data, varlist=coxmeMlist(2*kmat, rescale=F))

compare<-glht(model,linfct=mcp(SES="Tukey"))
ests<-summary(compare)[[10]]$coefficient
ses<-summary(compare)[[10]]$sigma
ps<-as.numeric(summary(compare)[[10]]$pval)

asts<-ifelse(ps<0.001,"***",ifelse(ps<0.01,"**",ifelse(ps<0.05,"*","")))

TukCon<-strsplit(names(ests)," - ")
TukCon2<-NA
for(i in 1:length(TukCon)){
TukCon2[i]<-paste(TukCon[[i]],collapse="/")
}

tabSES2<-unname(cbind(
TukCon2,
paste(round(exp(ests),rounding),asts,sep=""),
round(exp(ests-1.96*ses),rounding),round(exp(ests+1.96*ses),rounding)
))

tabSES2<-cbind(tabSES2[,1:2],
paste(apply(tabSES2[,3:4],1,min),apply(tabSES2[,3:4],1,max),sep="-")
)

resSES<-rbind(
c("SES category","N risk","N event","Contrast","HR","CI"),
cbind(tabSES,tabSES2))


mat<- cbind("any vs none"=c(1,1,-3,1),"2 vs 1"=c(2,-1,0,-1),"M vs P"=c(0,1,0,-1))
contrasts(time.data$group) <- mat


time.data.farmer<-time.data[time.data$SES=="farmer",]
time.data.cottager<-time.data[time.data$SES=="cottager",]
time.data.houseless<-time.data[time.data$SES=="houseless",]

nrow(time.data.farmer)
nrow(time.data.cottager)
nrow(time.data.houseless)


giveallres<-function(ndata){
ncodes<-summary(as.factor(ndata$subject),maxsum=100000)

state<-NA
died<-NA
last<-NA

for(i in 1:length(ncodes)){
subdata<-ndata[ndata$subject==names(ncodes)[i],]
state[i]<-paste(unique(subdata$group),collapse=" ")
died[i]<-subdata$death[nrow(subdata)]
last[i]<-as.character(subdata$group[nrow(subdata)])
}

Nrisks<-summary(as.factor(state))
Nevents<-summary(as.factor(last[died==1]))

Rany<-extractN(Nrisks,"Both")+extractN(Nrisks,"Po")+extractN(Nrisks,"Mo")+extractN(Nrisks,"Both Po")+extractN(Nrisks,"Both Mo")
Rtrans1<-extractN(Nrisks,"Mo None")+extractN(Nrisks,"Po None")+extractN(Nrisks,"Both Po None")+extractN(Nrisks,"Both Mo None")
Rnone<-extractN(Nrisks,"None")

Rtwo<-extractN(Nrisks,"Both")
Rtrans2<-extractN(Nrisks,"Both Mo")+extractN(Nrisks,"Both Po")+extractN(Nrisks,"Both Po None")+extractN(Nrisks,"Both Mo None")
Rone<-extractN(Nrisks,"Po")+extractN(Nrisks,"Mo")

Rmat<-extractN(Nrisks,"Mo")+extractN(Nrisks,"Both Mo")+extractN(Nrisks,"Mo None")+extractN(Nrisks,"Both Mo None")
Rpat<-extractN(Nrisks,"Po")+extractN(Nrisks,"Both Po")+extractN(Nrisks,"Po None")+extractN(Nrisks,"Both Po None")


Eany<-extractN(Nevents,"Both")+extractN(Nevents,"Po")+extractN(Nevents,"Mo")
Enone<-extractN(Nevents,"None")

Etwo<-extractN(Nevents,"Both")
Eone<-extractN(Nevents,"Po")+extractN(Nevents,"Mo")

Emat<-extractN(Nevents,"Mo")
Epat<-extractN(Nevents,"Po")


model <- coxme(Surv(time1, time2, death) ~  group + (1|subject),
		 data=ndata, varlist=coxmeMlist(2*kmat, rescale=F))

summary(model)

restab<-extract_coxme_table(model)

probany<-exp(restab[1,1])
probnone<-exp(-3*restab[1,1])
con1HR<-probnone/probany

probany<-exp(restab[1,1]-1.96*restab[1,3])
probnone<-exp(-3*(restab[1,1]-1.96*restab[1,3]))
con1CI1<-probnone/probany

probany<-exp(restab[1,1]+1.96*restab[1,3])
probnone<-exp(-3*(restab[1,1]+1.96*restab[1,3]))
con1CI2<-probnone/probany

con1p<-restab[1,5]
con1ast<-ifelse(con1p<0.001,"***",ifelse(con1p<0.01,"**",ifelse(con1p<0.05,"*","")))



probtwo<-exp(2*restab[2,1])
probone<-exp(-1*restab[2,1])
con2HR<-probone/probtwo

probtwo<-exp(2*(restab[2,1]-1.96*restab[2,3]))
probone<-exp(-1*(restab[2,1]-1.96*restab[2,3]))
con2CI1<-probone/probtwo

probtwo<-exp(2*(restab[2,1]+1.96*restab[2,3]))
probone<-exp(-1*(restab[2,1]+1.96*restab[2,3]))
con2CI2<-probone/probtwo

con2p<-restab[2,5]
con2ast<-ifelse(con2p<0.001,"***",ifelse(con2p<0.01,"**",ifelse(con2p<0.05,"*","")))



probmat<-exp(1*restab[3,1])
probpat<-exp(-1*restab[3,1])
con3HR<-probpat/probmat

probmat<-exp(1*(restab[3,1]-1.96*restab[3,3]))
probpat<-exp(-1*(restab[3,1]-1.96*restab[3,3]))
con3CI1<-probpat/probmat

probmat<-exp(1*(restab[3,1]+1.96*restab[3,3]))
probpat<-exp(-1*(restab[3,1]+1.96*restab[3,3]))
con3CI2<-probpat/probmat

con3p<-restab[3,5]
con3ast<-ifelse(con3p<0.001,"***",ifelse(con3p<0.01,"**",ifelse(con3p<0.05,"*","")))


finaltab<-cbind(
rbind(c(contlabs[1],testlabs),
cbind(unname(grouplabs1),
c(Rany,Rtrans1,Rnone),
c(Eany,"",Enone),
c(1,"",paste(round(con1HR,rounding),con1ast,sep="")),
c("","",paste(round(sort(c(con1CI1,con1CI2)),rounding),collapse="-")))
),
rbind(c(contlabs[2],testlabs),
cbind(unname(grouplabs2),
c(Rtwo,Rtrans2,Rone),
c(Etwo,"",Eone),
c(1,"",paste(round(con2HR,rounding),con2ast,sep="")),
c("","",paste(round(sort(c(con2CI1,con2CI2)),rounding),collapse="-")))
),
rbind(c(contlabs[3],testlabs),
cbind(unname(grouplabs3),
c(Rmat,"",Rpat),
c(Emat,"",Epat),
c(1,"",paste(round(con3HR,rounding),con3ast,sep="")),
c("","",paste(round(sort(c(con3CI1,con3CI2)),rounding),collapse="-")))
)
)

oneliner<-finaltab[4,c(4,9,14)]

return(list(finaltab,oneliner))
}



res<-giveallres(time.data)
finaltab1<-res[[1]]
oneliner1<-res[[2]]

resF<-giveallres(time.data.farmer)
finaltab2<-resF[[1]]
oneliner2<-resF[[2]]

resC<-giveallres(time.data.cottager)
finaltab3<-resC[[1]]
oneliner3<-resC[[2]]

resH<-giveallres(time.data.houseless)
finaltab4<-resH[[1]]
oneliner4<-resH[[2]]


restabfinal<-rbind(
c("Whole sample",rep("",14)),
finaltab1,
c("Farmers",rep("",14)),
finaltab2,
c("Cottagers",rep("",14)),
finaltab3,
c("Houseless lodgers",rep("",14)),
finaltab4
)

contlab2<-c("none/any","one/two","paternal/maternal")

rescol<-cbind(
c("HR","Whole sample",contlab2,"Farmers",contlab2,"Cottagers",contlab2,"Houseless lodgers",contlab2)
,c(paste(bottom,"-",threshold,sep=""),"",
oneliner1,"",
oneliner2,"",
oneliner3,"",
oneliner4))

write.table(resSES,"resSES.txt",sep="\t",row.names=F,col.names=F)
write.table(restabfinal,"restabfinal.txt",sep="\t",row.names=F,col.names=F)
write.table(rescol,"rescol.txt",sep="\t",row.names=F,col.names=F)

















