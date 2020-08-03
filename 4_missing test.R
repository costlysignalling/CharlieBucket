folder0<-"D:/vızkum/babicky/analyza_clanek1/final"

library(coxme)
library(multcomp)
library(kinship2)

extract_coxme_table <- function (mod){
  beta <- mod$coefficients
  nvar <- length(beta)
  HR<-exp(beta)
  nfrail <- nrow(mod$var) - nvar
  se <- sqrt(diag(mod$var)[nfrail + 1:nvar])
  z<- round(beta/se, 2)
  p<- signif(1 - pchisq((beta/se)^2, 1), 2)
  table=data.frame(cbind(beta,HR,se,z,p))
  return(table)
}


setwd(folder0)
pedi.data<-read.table("pedigree_data_leden2019.txt",sep="\t",header=T)

data<-read.table("source.data.txt",sep="\t",header=T,stringsAsFactors=F)

#Construct the pedigree
mped <- with(pedi.data, pedigree(code, father, mother, sex,
                                 famid=famid))

#Construct the relatedness matrix
#memory.limit(20000)

kmat<-kinship(mped)

#Above which survival i take random survival from the known sample if death date is insecure
maxthreshold<-10


threshold<-5
bottom<-0

source(file.path(folder0,"1_arrange_data.R"))

#comparison of unknown and conclusively non-available grandmothers

subdata<-time.data[time.data$GMM.avai==F&time.data$GMP.avai==F,]

subdata$unkn<-as.factor(is.na(subdata$GMM)+is.na(subdata$GMP))
summary(subdata$unkn)

model <- coxme(Surv(time1, time2, death) ~  unkn + (1|subject),
               data=subdata)

summary(model)

compare<-glht(model,linfct=mcp(unkn="Tukey"))

summary(compare)

#only the contrast between two unknown and two non-available grandmothers
subdata<-subdata[subdata$unkn!=1,]
subdata$unkn<-as.factor(is.na(subdata$GMM)+is.na(subdata$GMP))

summary(subdata$unkn)

model <- coxme(Surv(time1, time2, death) ~  unkn + (1|subject),
               data=subdata)

summary(model)



#one missing grandmother only

subdata<-time.data[time.data$GMM.avai==T&time.data$GMP.avai==F,]

subdata$unkn<-as.factor(is.na(subdata$GMP))
summary(subdata$unkn)

model <- coxme(Surv(time1, time2, death) ~  unkn + (1|subject),
               data=subdata)

summary(model)


subdata<-time.data[time.data$GMP.avai==T&time.data$GMM.avai==F,]

subdata$unkn<-as.factor(is.na(subdata$GMM))
summary(subdata$unkn)

model <- coxme(Surv(time1, time2, death) ~  unkn + (1|subject),
               data=subdata)

summary(model)



