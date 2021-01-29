
#We need to replace certain mistaken names or typos, or duplicities with a corrected factor level of distinct villages
key<-read.table(file.path(folder0,"key_places.txt"),sep="\t",header=T,stringsAsFactors=F)

place.born<-key$new[match(data$place.born,key$original)]
place.died<-key$new[match(data$place.died,key$original)]
GMP.place<-key$new[match(data$GMP.place,key$original)]
GMM.place<-key$new[match(data$GMM.place,key$original)]
GFP.place<-key$new[match(data$GFP.place,key$original)]
GFM.place<-key$new[match(data$GFM.place,key$original)]

#if I would like to sample data from exsting distribution beyond some maxthreshold, i can change the following lines according to another script (Kuprova)
data$died<-ifelse(is.na(data$died),as.character(as.Date(as.Date(data$born)+365*maxthreshold)),data$died)

#Threshold and bottom were orignally here, now they flow from startscript

survival<-as.Date(data$died)-as.Date(data$born)
GMP.survival<-as.Date(data$GMP.died)-as.Date(data$born)
GMM.survival<-as.Date(data$GMM.died)-as.Date(data$born)

#If the born place is missing, we can try to fill the void with the place of death, it usually coincides
data$place.born<-ifelse(is.na(data$place.born),data$place.died,data$place.born)
data$place.died<-ifelse(is.na(data$place.died),data$place.born,data$place.died)


#We need to replace certain mistaken names or typos, or duplicities with a corrected factor level of distinct villages
distances<-read.table(file.path(folder0,"distances.txt"),sep="\t",header=T,stringsAsFactors=F)


#If you did not set the maximum availibility distance, you have to do it now
#maxdist<-15

WGMP<-match(data$GMP.place,names(distances))-1
WGMM<-match(data$GMM.place,names(distances))-1
WB<-match(data$place.born,names(distances))


GMP.dist<-NA
for(i in 1:nrow(data)){
  dist.pos<-distances[WGMP[i],]
if(is.na(WB[i])){
  GMP.dist[i]<-NA
}else{
  GMP.dist[i]<-dist.pos[WB[i]][[1]]
}
}

GMM.dist<-NA
for(i in 1:nrow(data)){
  dist.pos<-distances[WGMM[i],]
  if(is.na(WB[i])){
    GMM.dist[i]<-NA
  }else{
    GMM.dist[i]<-dist.pos[WB[i]][[1]]
  }
}


GMP.available<-ifelse(GMP.survival>0,ifelse(GMP.dist<=maxdist,TRUE,FALSE),FALSE)
GMM.available<-ifelse(GMM.survival>0,ifelse(GMM.dist<=maxdist,TRUE,FALSE),FALSE)

#If availibility is not known, we assume grandmother was not available
GMP.available<-ifelse(is.na(GMP.available),FALSE,GMP.available)
GMM.available<-ifelse(is.na(GMM.available),FALSE,GMM.available)

#Indicating unknwn grandmothers
data$GMP.unknown<-is.na(data$GMP.died)
data$GMM.unknown<-is.na(data$GMM.died)

#If the grandomther is unknowc, I wnat to make sure she will be labeled as non avilable. This can be done in this script by setting her death date to a random date long in the past.
data$GMP.died<-ifelse(is.na(data$GMP.died),"1400-01-01",as.character(data$GMP.died))
data$GMM.died<-ifelse(is.na(data$GMM.died),"1400-01-01",as.character(data$GMM.died))

#Transfer date vectors to Date format for easy processing
data$born<-as.Date(data$born)
data$died<-as.Date(data$died)
data$GMP.died<-as.Date(data$GMP.died)
data$GMM.died<-as.Date(data$GMM.died)


summary(as.factor(data$SES))

subject<-NA
time1<-NA
time2<-NA
GMP<-NA
GMP.live<-NA
GMP.avai<-NA
GMP.unknown<-NA
GMM<-NA
GMM.live<-NA
GMM.avai<-NA
GMM.unknown<-NA
numeric.rank<-NA
birth.rank<-NA
death<-NA
use.rank1<-NA
first.born.criteria<-NA
grandparents.criteria<-NA
analysis<-NA
SES<-NA
group.GM<-NA
mother.age<-NA
born<-NA
sex<-NA
siblings<-NA
row<-NA
GMMlk<-NA
GMPlk<-NA
birth<-NA

names(data)

time.data<-data.frame(subject,time1,time2,GMP,GMP.live,GMP.avai,GMP.unknown,GMM,GMM.live,GMM.avai,GMM.unknown,death,
numeric.rank,
birth.rank,
use.rank1,
first.born.criteria,
grandparents.criteria,
analysis,
SES,
group.GM,
mother.age,born,sex,siblings,row,GMMlk,GMPlk,birth)

for(i in 1:nrow(data)){

subject<-rep(data$code[i],4)

use.rank1<-rep(data$use.rank1[i],4)
first.born.criteria<-rep(data$first.born.criteria[i],4)
grandparents.criteria<-rep(data$grandparents.criteria[i],4)
analysis<-rep(data$analysis[i],4)
SES<-rep(data$SES[i],4)
group.GM<-rep(data$group.GM[i],4)
mother.age<-rep(data$mother.age[i],4)
born<-rep(data$year.of.birth[i],4)
sex<-rep(data$sex[i],4)
siblings<-rep(data$siblings[i],4)
row<-rep(data$row[i],4)
birth<-rep(data$birth[i],4)


GMMlk<-rep(data$GMMlk[i],4)
GMPlk<-rep(data$GMPlk[i],4)

GMP<-rep(data$GMP[i],4)
GMM<-rep(data$GMM[i],4)

GMP.avai<-rep(GMP.available[i],4)
GMM.avai<-rep(GMM.available[i],4)

GMP.unknown<-rep(data$GMP.unknown[i],4)
GMM.unknown<-rep(data$GMM.unknown[i],4)

numeric.rank<-rep(data$rank[i],4)
birth.rank<-rep(ifelse(data$rank[i]==1,"FB","LB"),4)

days<-c(
data$born[i],
data$died[i],
data$GMP.died[i],
data$GMM.died[i])

names(days)<-c("born","died","GMP.pass","GMM.pass")
tics<-sort(days)-data$born[i]

GMP.switch<-which(names(tics)=="GMP.pass")
GMM.switch<-which(names(tics)=="GMM.pass")

GMP.live<-c(rep(1,GMP.switch-1),rep(0,5-GMP.switch))
GMM.live<-c(rep(1,GMM.switch-1),rep(0,5-GMM.switch))

add.lines<-data.frame(subject,tics[1:4],tics[2:5],GMP,GMP.live,GMP.avai,GMP.unknown,GMM,GMM.live,GMM.avai,GMM.unknown,
numeric.rank,
birth.rank,
use.rank1,
first.born.criteria,
grandparents.criteria,
analysis,
SES,
group.GM,
mother.age,born,sex,siblings,row,GMMlk,GMPlk,birth)

add.lines<-add.lines[which(rownames(add.lines)=="born"):(which(rownames(add.lines)=="died")-1),]

#revise .avai columns if grandomther dies within first five years of life of the child

add.lines$GMP.avai<-ifelse(add.lines$GMP.live==1,add.lines$GMP.avai,FALSE)
add.lines$GMM.avai<-ifelse(add.lines$GMM.live==1,add.lines$GMM.avai,FALSE)

names(add.lines)[2:3]<-c("time1","time2")

add.lines$death<-c(rep(0,nrow(add.lines)-1),1)
rownames(add.lines)<-NULL

if(sum(add.lines$time2>threshold*365)>0){

upcut<-min(which(add.lines$time2>threshold*365))

add.lines<-add.lines[1:upcut,]

add.lines$time2[upcut]<-threshold*365
add.lines$death[upcut]<-0
}


if(all(add.lines$time2<bottom*365)){
add.lines<-add.lines[0,]
}else{

if(sum(add.lines$time2<bottom*365)>0){
downcut<-max(c(which(add.lines$time2<bottom*365),0))
add.lines<-add.lines[(downcut+1):nrow(add.lines),]
}

if(add.lines$time1[1]<bottom*365){

add.lines$time1[1]<-bottom*365
}
}

time.data<-rbind(time.data,add.lines)

print(i)

}

time.data<-time.data[-1,]


time.data<-time.data[!is.na(time.data$time1),]
nrow(time.data)

time.data<-time.data[!is.na(time.data$time2),]
nrow(time.data)

#I add 1 if the child was born and died the same day, otherise some analzsis would return an error
time.data$time2<-ifelse(!(time.data$time1<time.data$time2),time.data$time2+1,time.data$time2)


###Here i add second birth rank, because some rank assignment might be arroneus, so only the sure ones are assigned with this variable, this will not be used when we controll for birth rank, but we might use this when we want to analyze birth rank as a main predictor

time.data$birth.rank2<-ifelse(is.na(time.data$first.born.criteria)&(time.data$use.rank1==TRUE),time.data$birth.rank,NA)

#We set the neccessary groups of grandmother availibility

time.data$group<-ifelse(time.data$GMP.avai==1,ifelse(time.data$GMM.avai==1,"Both","Po"),ifelse(time.data$GMM.avai==1,"Mo","None"))
time.data$group<-as.factor(time.data$group)
