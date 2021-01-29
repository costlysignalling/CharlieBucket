library(rethinking)
library(splines)
library(survival)

load("data_spread_month.RData")
time<-1:max(d$m)

nrow(d)

num_knots <- 3
knot_list <- quantile( time , probs=c(0,0.2,1) )

Bt <- bs(d$m,
         knots=knot_list[-c(1,num_knots)] ,
         degree=3 , intercept=T )


data$mother.age[data$mother.age<0]<-NA
MA<-standardize(data$mother.age)
plot(density(MA[!is.na(MA)]))

#Spline scores per item
SESnum<-ifelse(d$SES=="farmer",1,ifelse(d$SES=="cottager",2,ifelse(d$SES=="houseless",3,NA)))
summary(as.factor(SESnum))

GMnum<-ifelse(d$group=="None",1,ifelse(d$group=="Mo",2,ifelse(d$group=="Po",3,ifelse(d$group=="Both",4,NA))))
summary(as.factor(GMnum))

Y<-ifelse(d$m<12*1+1,1,ifelse(d$m<12*2+1,2,ifelse(d$m<12*3+1,3,ifelse(d$m<12*4+1,4,5))))

#Condition - interaction between SES and GM presence
cond<-(SESnum-1)*4+GMnum

table(cond,d$SES,d$group)

summary(as.factor(d$numeric.rank))
table(d$numeric.rank,d$birth.rank)

nrow(data)
dim(Dmat)

levels(as.factor(d$row))

names(d)

M<-as.integer(as.factor(data$mother))
GMM<-as.integer(as.factor(data$GMM))
GMP<-as.integer(as.factor(data$GMP))

M[is.na(M)]<-max(M,na.rm=T)+1
GMM[is.na(GMM)]<-max(GMM,na.rm=T)+1
GMP[is.na(GMP)]<-max(GMP,na.rm=T)+1

#Put together all the data
datlist<-list(death=d$death2,
              
              rowi=as.integer(d$row),
              
              M=as.integer(M),
              GMM=as.integer(GMM),
              GMP=as.integer(GMP),
              
              cond=cond,
              SES=SESnum,
              Y=Y,
              
              MA=MA,
              
              BR=data$rank,
              FB=ifelse(data$rank==1,1,ifelse(is.na(data$rank),0,0)),
              
              spl1=as.numeric(Bt[,1]),
              spl2=as.numeric(Bt[,2]),
              spl3=as.numeric(Bt[,3]),
              spl4=as.numeric(Bt[,4]),
              spl5=as.numeric(Bt[,5]),
              
              Mbc=d$GMMbusy,
              Pbc=d$GMPbusy,
              
              Muc=d$GMMunk,
              Puc=d$GMPunk,
              
              Dmat=Dmat)



#Close to 50% chance of sruvival, prior distribution corresponding to the intercept lying between 15 and 78% probability of survival, this is vague wnough..
(1-exp(-4.5))^60

(1-exp(-3.5))^60
(1-exp(-5.5))^60


#Distribution of other priors should allow for substantial differences, but avoid extreme, unrealistic contrasts
#Within 1 SD
exp(0.5)
exp(-0.5)
#Within 2 SD
exp(1)
exp(-1)

#evaluation of rate prior, rate 1 seems reasonable
plot(seq(0,4,0.1),dexp(seq(0,4,0.1),rate=1))

#Prior of individual intercepts based on relatedness between individuals must be 6880x6880
dim(Dmat)
summary(MA)

#Sample the model using MCMC
m <- ulam(
  alist(
    death ~ dbinom( 1 , p ) ,
    
    log(p) <- pda[SES]+w1*spl1+w2*spl2+w3*spl3+w4*spl4+w5*spl5+
      alpha[cond,Y]+bMbc*Mbc+bPbc*Pbc+bMuc*Muc+bPuc*Puc+
      randM[M[rowi]]*sigma_M+randGMM[GMM[rowi]]*sigma_GMM+randGMP[GMP[rowi]]*sigma_GMP+bma*MA[rowi]+bR*log(BR[rowi])+bFB*FB[rowi],
    
    pda[SES] ~ dnorm(-4.5,1),
    
    # adaptive priors - non-centered
    transpars> matrix[cond,5]:alpha <-
      compose_noncentered( sigma_cond , L_Rho_cond , z_cond ),
    
    matrix[5,cond]:z_cond ~ normal( 0 , 1 ),
    
    # fixed priors
    vector[5]:sigma_cond ~ dexp(1),
    cholesky_factor_corr[5]:L_Rho_cond ~ lkj_corr_cholesky( 1 ),
    
    #relatedness
    randM[M] ~ dnorm(0, 1),
    randGMM[GMM] ~ dnorm(0, 1 ),
    randGMP[GMP] ~ dnorm(0, 1 ),
    
    sigma_M ~ dexp(1),
    sigma_GMM ~ dexp(1),
    sigma_GMP ~ dexp(1),
    
    w1~dnorm(0,0.5),
    w2~dnorm(0,0.5),
    w3~dnorm(0,0.5),
    w4~dnorm(0,0.5),
    w5~dnorm(0,0.5),
    
    bMbc~dnorm(0,0.5), #Maternal busy contrast
    bPbc~dnorm(0,0.5), #Paternal busy contrast
    bMuc~dnorm(0,0.5), #Maternal unknown contrast
    bPuc~dnorm(0,0.5), #Paternal unknown contrast
    
    bma~dnorm(0,0.5),
    bR~dnorm(0,0.5),
    bFB~dnorm(0,0.5),
    
    MA~dnorm(0,1),
    
    # compute ordinary correlation matrixes from Cholesky factors
    gq> matrix[5,5]:Rho_cond <<- Chol_to_Corr(L_Rho_cond)
    
  ), data=datlist, start=list(pda=rep(-4.5,3),
                              z_cond=matrix(rep(0,60),nrow=5),
                              w1=0,w2=0,w3=0,w4=0,w5=0,
                              bMbc=0,bPbc=0,bMuc=0,bPuc=0,
                              sigma_M=0.1,sigma_GMM=0.1,sigma_GMP=0.1,
                              bma=0,bR=0,bFB=0),cores=4,chains=4,iter=4000)


save(m,file="01_res_GMCmonth.RData")

precis(m,depth=3)
precis(m,depth=3,pars="alpha")



