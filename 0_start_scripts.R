#Save the working directory
folder0<-getwd()

#Load required packages. Install them prior to the loading if necessary.
library(coxme)
library(multcomp)
library(kinship2)

#This function allows for a comfortable extraction of summary characteristics from the Cox Mixed effect regression model
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

#Set the working directory
setwd(folder0)

#Load the pedigree data that allow to calculate the relatedness between data
pedi.data<-read.table("pedigree_data.txt",sep="\t",header=T)

#Load the information on focal individuals
data<-read.table("source.data.txt",sep="\t",header=T,stringsAsFactors=F)

# names(data)
# data<-data[,c(1:5,12,13:24,29:31,33:42)]
# 
# write.table(data,"source.data.txt",sep="\t",row.names=F)

#Construct the pedigree
mped <- with(pedi.data, pedigree(code, father, mother, sex,
                                 famid=famid))

#Construct the relatedness matrix (increase the meomory limit if necessary)
#memory.limit(200000)

kmat<-kinship(mped)


#Above which survival I take random survival from the known sample if death date is insecure
maxthreshold<-10

#What is the maximum distance between grandmother and grandchild for the grandmother to be considered availible (this should be changed to 0, if the proximity criteria of the same village are used or to arbitrary large threshold if proximity criteria are relaxed.)
maxdist<-15

setwd(folder0)

for(test.type in c("simple","fylo")){
    
    test.set<-paste("2_extract_res_",test.type,".R",sep="")
    
    if (file.exists(test.type)){
        setwd(file.path(folder0,test.type))
    } else {
        dir.create(file.path(folder0,test.type))
        setwd(file.path(folder0,test.type))
    }
    
    #graphical parameters for plotting
    spans<-c(0.1,0.1,0.1,0.2,0.2,0.2)
    botlims<-c(0.5,0.7,0.85,0.9,0.95,0.95)
    
    parcount<-1
    
    #Which age spans should be subject to separate analyses
    #Upper border is later treated as "threshold", lower is "bottom"
    
    for(age.span in list(c(0,5),c(0,1),c(1,2),c(2,3),c(3,4),c(4,5))){
        
        threshold<-age.span[2]
        bottom<-age.span[1]
        
        #Creates folders where results are to be stored
        subdir<-paste(bottom,"-",threshold,sep="")
        if (file.exists(subdir)){
            setwd(file.path(folder0,test.type,subdir))
        } else {
            dir.create(file.path(folder0,test.type,subdir))
            setwd(file.path(folder0,test.type,subdir))
        }
        
        #Arrange the data in the required format
        source(file.path(folder0,"1_arrange_data.R"))
        #Run the selected test - either with or without relatedness control
        source(file.path(folder0,test.set))
        spanset<-spans[parcount]
        botylim<-botlims[parcount]
        if(bottom==0){
            #Plot and report Kaplan-Meier estimates
            source(file.path(folder0,"3_plots.R"))
            source(file.path(folder0,"3_multiplot.R"))
            source(file.path(folder0,"3_Kaplan_Meier_summary.R"))
        }
        
        setwd(file.path(folder0,test.type))
        
        parcount<-parcount+1
    }
    
    setwd(file.path(folder0,test.type))
    
    intervals<-c(list.files()[2],list.files()[c(1,3:6)])
    
    #At the end, connect all columns with hayard ratios per age range into a single table
    setwd(file.path(folder0,test.type,intervals[1]))
    resHR<-read.table("rescol.txt",sep="\t",stringsAsFactors=F)
    resHR<-cbind(resHR,rep("",nrow(resHR)))
    
    for(int in intervals[2:length(intervals)]){
        setwd(file.path(folder0,test.type,int))
        rescol<-read.table("rescol.txt",sep="\t",stringsAsFactors=F)
        resHR<-cbind(resHR,rescol[,2])
    }
    
    resHR<-unname(resHR)
    
    #Save results
    setwd(file.path(folder0,test.type))
    write.table(resHR,"resHR.txt",sep="\t",row.names=F,col.names=F)
    file.copy(file.path(folder0,test.type,intervals[1],"restabfinal.txt"), file.path(folder0,test.type))
    file.copy(file.path(folder0,test.type,intervals[1],"resSES.txt"), file.path(folder0,test.type))
    
    setwd(folder0)
}

