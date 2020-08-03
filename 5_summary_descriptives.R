threshold<-5
bottom<-0

source(file.path(folder0,"1_arrange_data.R"))

names(time.data)

dups<-sapply(1:nrow(data),function(i){sum(data$code==data$code[i])})

wtd<-rep(0,length(dups))
wtd[dups==2]<-1:2
data<-data[wtd<2,]


length(unique(data$GMM))
length(unique(data$GMP))


length(unique(data$GMM))-length(unique(match(data$GMP,data$GMM)))+length(unique(data$GMP))

length(unique(data$GMM))-length(unique(match(data$GMP,data$GMM)))
length(unique(data$GMP))-length(unique(match(data$GMP,data$GMM)))

length(unique(match(data$GMP,data$GMM)))
length(unique(match(data$GMM,data$GMP)))


is.GMM<-match(data$code,data$GMM)
summary(!is.na(is.GMM))

is.GMP<-match(data$code,data$GMP)
summary(!is.na(is.GMP))

summary(!is.na(is.GMP)&!is.na(is.GMM))

156-73+158

156-73

is.M<-match(data$code,data$mother)
summary(!is.na(is.M))

summary(!is.na(is.GMM)&!is.na(is.M))
summary(!is.na(is.GMP)&!is.na(is.M))


summary(!is.na(is.GMM)&!is.na(is.M)&!is.na(is.GMP))

138-73+148


td2<-time.data[match(data$code,time.data$subject),]
nrow(td2)

names(td2)

summary(as.logical(data$GMM.unknown))
summary(as.logical(data$GMP.unknown))

summary(as.logical(!data$GMP.unknown&!data$GMM.unknown))

4023+4118-2627


summary(as.logical(td2$GMP.live))

summary(as.logical(!data$GMM.unknown&td2$GMM.live))
summary(as.logical(td2$GMM.avai))

summary(as.logical(td2$GMP.live))
summary(as.logical(td2$GMP.avai))

