folder0<-getwd()

library(coxme)
library(multcomp)
library(kinship2)


setwd(folder0)
pedi.data<-read.table("pedigree_data.txt",sep="\t",header=T)

data<-read.table("source.data.txt",sep="\t",header=T,stringsAsFactors=F)
names(data)
nrow(data)


nrow(data)


mean(post$pda)

data$row<-1:nrow(data)

#Above which survival I take random survival from the known sample if death date is insecure
maxthreshold<-10

#What is the maximum distance between grandmother and grandchild for the grandmother to be considered availible
maxdist<-0

#set the span of life you are interested in - upper border is threshold, lower is bottom
setwd(folder0)

threshold<-5
bottom<-0

source(file.path(folder0,"0_arrange_data_sample.R"))
nrow(time.data)
nrow(data)

names(time.data)

SESnum<-ifelse(time.data$SES=="farmer",1,ifelse(time.data$SES=="cottager",2,ifelse(time.data$SES=="houseless",3,NA)))
summary(as.factor(SESnum))

GMnum<-ifelse(time.data$group=="None",1,ifelse(time.data$group=="Mo",2,ifelse(time.data$group=="Po",3,ifelse(time.data$group=="Both",4,NA))))
summary(as.factor(GMnum))

cond<-(SESnum-1)*4+GMnum



timeser<-function(i){
    start<-time.data[i,]$time1
    end<-time.data[i,]$time2
    time<-seq(start+1,end,1)
    l<-length(time)
    data.frame(time.data[rep(i,l),],time=time,death2=c(rep(0,l-1),time.data[i,]$death))
}    


d<-lapply(1:nrow(time.data),timeser)
d<-do.call(rbind, d)

names(d)

d$day<-as.Date(d$birth)+d$time-1

d$GMMnp<-d$day>(as.Date(d$GMMlk)+5*365)
d$GMPnp<-d$day>(as.Date(d$GMPlk)+5*365)

d$GMMbusy<-ifelse(d$group=="Po"|d$group=="None",0,ifelse(d$GMMnp==F,0.5,-0.5))
d$GMPbusy<-ifelse(d$group=="Mo"|d$group=="None",0,ifelse(d$GMPnp==F,0.5,-0.5))

d$GMMunk<-ifelse(d$group=="Mo"|d$group=="Both",0,ifelse(d$GMM.unknown==T,0.5,-0.5))
d$GMPunk<-ifelse(d$group=="Po"|d$group=="Both",0,ifelse(d$GMP.unknown==T,0.5,-0.5))


#Check if you have all representants
table(cond,time.data$SES,time.data$group)

summary(d$GMM.unknown)
summary(d$GMP.unknown)

summary(d$GMMnp)
summary(d$GMPnp)

d$GMMbusy[is.na(d$GMMbusy)]<-0
d$GMPbusy[is.na(d$GMPbusy)]<-0



m<-as.numeric(cut(d$time,60))
lev<-as.factor(paste(d$subject,formatC(m,width=4,flag=0)))
d$m<-m

dm<-lapply(levels(lev),function(i)d[lev==i,])

#Take the last row
reduceToMonth<-function(dtake){
    dlast<-dtake[nrow(dtake),]
    dlast$group<-names(sort(summary(as.factor(dtake$group)),decreasing=T))[1]
    return(dlast)
}

dfin<-lapply(dm,reduceToMonth)

d<-do.call(rbind, dfin)

#Construct the pedigree
mped <- with(pedi.data, pedigree(code, father, mother, sex,
                                 famid=famid))

#Construct the relatedness matrix
memory.limit(20000)

kmat<-kinship(mped)

kkmat<-kmat@x
taker<-match(data$code,as.numeric(kmat@Dimnames[[1]]))

selmat<-as.matrix(kmat[taker,taker])

#We need to specify maximum relatedness such taht minimal distance is 0
diag(selmat)<-0.5

#Distance matrix from the relatedness matrix
Dmat<-1-selmat*2

save(d,data,Dmat, file = "data_spread_month.RData")


