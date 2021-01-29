substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

families<-read.table("complete_families.txt",sep="\t",header=T,stringsAsFactors=F)
nrow(families)

names(families)
families$SES<-as.factor(families$kat2)

summary(as.factor(families$SES))

levels(families$SES)<-c("farmer","farmer","cottager","houseless","houseless","houseless","houseless","clerk","single","cottager","houseless")

families<-families[families$analyza==1|families$analyza==81|families$analyza==90,]

lev.mother<-summary(as.factor(families$mother.code))
lev.father<-summary(as.factor(families$father.code))

#restriction to families where each spouse had only one marriage

rep.mother<-NA
rep.father<-NA

for(i in 1:nrow(families)){
  rep.mother[i]<-sum(families$mother.code==families$mother.code[i],na.rm=T)
  rep.father[i]<-sum(families$father.code==families$father.code[i],na.rm=T)
}

families$rep.mother<-rep.mother
families$rep.father<-rep.father

families<-families[families$rep.mother==1,]
families<-families[families$rep.father==1,]

borns<-families[,substrRight(names(families),4)=="born"&substr(names(families),1,1)=="d"]
families$oldest<-apply(borns,1,min,na.rm=T)
families$youngest<-apply(borns,1,max,na.rm=T)

nrow(families)
families<-families[families$oldest>="1708-01-01",]
nrow(families)
families<-families[families$youngest<"1835-01-01",]
nrow(families)

families$SES<-as.factor(as.character(families$SES))

#reorder factor levels
families$SES <- factor(families$SES, levels = c("farmer", "cottager", "houseless"))

summary(as.factor(families$SES))

sum(summary(as.factor(families$SES)))
nrow(families)

library(rethinking)
#Number of children per SES class
tapply(families$pocet,families$SES,mean)

d<-list(ses=as.numeric(families$SES),
        nkids=as.numeric(families$pocet))

mnk<-ulam(alist(
  nkids~dpois(lambda),
  lambda<-a[ses],
  a[ses] ~ dnorm(6,2)
  ) ,cores=4,chains=4,data=d)

ank<-precis(mnk, depth=2, prob=0.95)[,c(1,3,4)]

post<-extract.samples(mnk)

farcot<-post$a[,1]-post$a[,2]
farlod<-post$a[,1]-post$a[,3]
cotlod<-post$a[,2]-post$a[,3]

difnk<-matrix(c(mean(farcot),quantile(farcot,c(0.025,0.975)),
              mean(farlod),quantile(farlod,c(0.025,0.975)),
              mean(cotlod),quantile(cotlod,c(0.025,0.975))),ncol=3,byrow = T)

ank<-format(round(ank,2),nsmall=2)
difnk<-format(round(difnk,2),nsmall=2)


#caculate the reproduction onset
onset<-(as.Date(families$d1.born)-as.Date(families$mother.born))/365
tapply(onset,families$SES,mean,na.rm=T)

d<-list(ses=as.numeric(families$SES)[!is.na(onset)],
        onset=as.numeric(onset)[!is.na(onset)])

ma<-ulam(alist(
  onset~dnorm(mu,sigma),
  mu<-a[ses],
  a[ses] ~ dnorm(25,2),
  sigma ~ dexp(1)
) ,cores=4,chains=4,data=d)

aa<-precis(ma, depth=2, prob=0.95)[,c(1,3,4)]

post<-extract.samples(ma)

farcot<-post$a[,1]-post$a[,2]
farlod<-post$a[,1]-post$a[,3]
cotlod<-post$a[,2]-post$a[,3]

difa<-matrix(c(mean(farcot),quantile(farcot,c(0.025,0.975)),
                mean(farlod),quantile(farlod,c(0.025,0.975)),
                mean(cotlod),quantile(cotlod,c(0.025,0.975))),ncol=3,byrow = T)

aa<-format(round(aa,2)[1:3,],nsmall=2)
difa<-format(round(difa,2),nsmall=2)


#Interbirth intervals
names(families)

borns<-families[,substrRight(names(families),4)=="born"&substr(names(families),1,1)=="d"]

nrow(borns)
fromYtoO<-(as.Date(families$youngest)-as.Date(families$oldest))
nInt<-
sapply(1:nrow(families),function(i,borns,youngest){which(borns[i,]==youngest[i])[1]},borns=borns,youngest=families$youngest)-sapply(1:nrow(families),function(i,borns,oldest){which(borns[i,]==oldest[i])[1]},borns=borns,oldest=families$oldest)

families$avg.inter<-fromYtoO/nInt
families$avg.inter<-families$avg.inter/365

tapply(families$avg.inter,families$SES,mean,na.rm=T)

d<-list(ses=as.numeric(families$SES),
        interval=as.numeric(families$avg.inter))

mib<-ulam(alist(
  interval~dnorm(mu,sigma),
  mu<-a[ses],
  a[ses] ~ dnorm(3,2),
  sigma ~ dexp(1)
) ,cores=4,chains=4,data=d)


aib<-precis(mib, depth=2, prob=0.95)[,c(1,3,4)]

post<-extract.samples(mib)

farcot<-post$a[,1]-post$a[,2]
farlod<-post$a[,1]-post$a[,3]
cotlod<-post$a[,2]-post$a[,3]

difib<-matrix(c(mean(farcot),quantile(farcot,c(0.025,0.975)),
               mean(farlod),quantile(farlod,c(0.025,0.975)),
               mean(cotlod),quantile(cotlod,c(0.025,0.975))),ncol=3,byrow = T)

aib<-format(round(aib,2)[1:3,],nsmall=2)
difib<-format(round(difib,2),nsmall=2)


est<-cbind(ank,aa,aib)
rownames(est)<-c("Farmers","Cottagers","Lodgers")
colnames(est)[c(1,4,7)]<-c("N kids","Rep. onset","Interval")

dif<-cbind(difnk,difa,difib)
rownames(dif)<-c("Farmers-Cottagers","Farmers-Lodgers","Cottagers-Lodgers")
colnames(dif)<-colnames(est)


write.table(dif,"SESdifferences.txt",sep="\t",col.names=NA)
write.table(est,"SESestimates.txt",sep="\t",col.names=NA)

snk<-apply(ank,1,function(x){paste(x[1]," (",x[2],"-",x[3],")",sep="")})
sa<-apply(aa,1,function(x){paste(x[1]," (",x[2],"-",x[3],")",sep="")})
sib<-apply(aib,1,function(x){paste(x[1]," (",x[2],"-",x[3],")",sep="")})

rest<-cbind(snk,sa,sib)
write.table(rest,"SESresults_condensed.txt",sep="\t",col.names=NA)


#Mother survival
mother.survival<-as.Date(families$mother.died)-as.Date(families$mother.born)
families$mother.survival<-mother.survival/365

tapply(families$mother.survival,families$SES,mean,na.rm=T)


