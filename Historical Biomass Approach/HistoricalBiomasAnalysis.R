# Historical biomass approach analysis functions
# Adapted from Ralston et al. 2011
# Original author: Andre E. Punt, aepunt@uw.edu
# Edited by: Kristin Privitera-Johnson, kpjohns@uw.edu
# Started: 12/17/2018
# Last Edited: 01/07/2018

HistBioApproach <- function(YrBack=YrBack)
{
 library(nlme)
 Dirn <- getwd()

 cat("\n", "ORIGINAL ANALYSIS:", "\n", "\n")
 # ORIGINAL ANALYSIS
 pdf("Graphs/Original BiomassPlots.pdf", width=8.5, height=11)
 HistBiomassPlots(Dirn=Dirn,YrBack=YrBack, filename="OriginalRalstonData.Txt", SpecName=c("Canary","Yelloweye","Hake","Lingcod","POP","SST","Darkblotched rockfish",
 	"Bocaccio","Widow","Yellowtail","Chilipepper","Sablefish","Cabezon","Dover","Petrale sole","Pacific mackerel","Pacific sardine"), SpecNameUse=c("Canary","Yelloweye","Hake","Lingcod","POP","SST","Darkblotched rockfish","Bocaccio","Widow",
 	"Yellowtail","Chilipepper","Sablefish","Cabezon","Dover","Petrale sole","Pacific mackerel","Pacific sardine"))
 dev.off()

 pdf(file="Graphs/Original ResidualPlots.pdf", width=8.5, height=11)
 CalculateSigmas(YrBack=YrBack, Dirn=Dirn, filename="OriginalRalstonData.Txt")
 dev.off()

 cat("\n", "ORIGINAL ANALYSIS WITH UPDATED DATA:", "\n", "\n")
 # ORIGINAL ANALYSIS WITH UPDATED DATA AFTER 2009 UNTIL 2017
 pdf("Graphs/Update BiomassPlots.pdf", width=8.5, height=11)
 HistBiomassPlots(Dirn=Dirn,YrBack=YrBack, filename="RalstonUpdateData.Txt", SpecName=c("Canary","Yelloweye","Hake","Lingcod","POP","SST","Darkblotched rockfish",
	"Bocaccio","Widow","Yellowtail","Chilipepper","Sablefish","Cabezon","Dover","Petrale sole","Pacific mackerel","Pacific sardine"), SpecNameUse=c("Canary","Yelloweye","Hake","Lingcod","POP","SST","Darkblotched rockfish","Bocaccio","Widow",
	"Yellowtail","Chilipepper","Sablefish","Cabezon","Dover","Petrale sole","Pacific mackerel","Pacific sardine"))
 dev.off()

 pdf(file="Graphs/Update ResidualPlots.pdf", width=8.5, height=11)
 CalculateSigmas(YrBack=YrBack, Dirn=Dirn, filename="RalstonUpdateData.Txt")
 dev.off()

 # cat("\n", "CATEGORY 2 ANALYSIS:", "\n", "\n")
 # # CATEGORY 2 ANALYSIS
 # pdf("Graphs/Category 2 BiomassPlots.pdf", width=8.5, height=11)
 # HistBiomassPlots(Dirn=Dirn,YrBack=YrBack, filename="Category 2 Data.Txt", SpecName=c("Cowcod", "Blue/Deacon", "China", "Longspine", "Pacific sardine", "Pacific mackerel", "Blackgill"), SpecNameUse=c("Cowcod", "Blue/Deacon", "China", "Longspine", "Pacific sardine", "Pacific mackerel", "Blackgill"))
 # dev.off()

 # pdf(file="Graphs/Category 2 ResidualPlots.pdf", width=8.5, height=11)
 # CalculateSigmas_Cat2(YrBack=YrBack, Dirn=Dirn, filename="Category 2 Data.Txt")
 # dev.off()

}

# ===============================================================================================================
 
CalculateSigmas <- function(YrBack=20,filename,Dirn)
{
 # Hist is a historical analysis
 cat("\n","All 17 Stocks:", "\n")
 SpecName <- c("Canary","Yelloweye","Hake","Lingcod","POP","SST","Darkblotched rockfish","Bocaccio","Widow","Yellowtail","Chilipepper","Sablefish","Cabezon","Dover","Petrale sole","Pacific mackerel","Pacific sardine")
 SpecNameUse <- c("Canary","Yelloweye","Hake","Lingcod","POP","SST","Darkblotched rockfish","Bocaccio","Widow","Yellowtail","Chilipepper","Sablefish","Cabezon","Dover","Petrale sole","Pacific mackerel","Pacific sardine")
 PartName <- c(rep("SSB",100))
# ExtraCV("HistRetroWCFormat.Txt",SpecName,SpecNameUse,PartName,Skip=1,YrBack=YrBack,Plot=T) 
 Type <- c(1,1,2,2,1,1,1,1,1,1,1,2,2,3,3,4,4)
 par(mfrow=c(3,3))
 ExtraCV2(FileName=filename,SpecName=SpecName,SpecNameUse=SpecNameUse,PartName=PartName,Type=Type,Skip=1,YrBack=YrBack,PlotAll=T,Regress=T,SumName="", Dirn=Dirn) 
 # AA

 par(mfrow=c(2,2))
 cat("\n","Rockfish Stocks:", "\n")
 SpecName <- c("Canary","Yelloweye","Hake","Lingcod","POP","SST","Darkblotched rockfish","Bocaccio","Widow","Yellowtail","Chilipepper","Sablefish","Cabezon","Dover","Petrale sole","Pacific mackerel","Pacific sardine")
 SpecNameUse <- c("Canary","Yelloweye","POP","SST","Darkblotched rockfish","Bocaccio","Widow","Yellowtail","Chilipepper")
 PartName <- c(rep("SSB",100))
 ExtraCV2(FileName=filename,SpecName=SpecName,SpecNameUse=SpecNameUse,PartName=PartName,Type=Type,Skip=1,YrBack=YrBack,Plot=T,Regress=F,
    SumName="Rockfish", Dirn=Dirn)

 cat("\n","Flatfish Stocks:", "\n") 
 SpecNameUse <- c("Hake","Lingcod","Sablefish","Cabezon")
 PartName <- c(rep("SSB",100))
 ExtraCV2(FileName=filename,SpecName=SpecName,SpecNameUse=SpecNameUse,PartName=PartName,Type=Type,Skip=1,YrBack=YrBack,Plot=T,Regress=F,
    SumName="Roundfish", Dirn=Dirn) 

 cat("\n","Roundfish Stocks:", "\n")
 SpecNameUse <- c("Dover","Petrale sole")
 PartName <- c(rep("SSB",100))
 ExtraCV2(FileName=filename,SpecName=SpecName,SpecNameUse=SpecNameUse,PartName=PartName,Type=Type,Skip=1,YrBack=YrBack,Plot=T,Regress=F,
    SumName="Flatfish", Dirn=Dirn) 

 cat("\n","CPS Stocks:", "\n")
 SpecNameUse <- c("Pacific mackerel","Pacific sardine")
 PartName <- c(rep("SSB",100))
 ExtraCV2(FileName=filename,SpecName=SpecName,SpecNameUse=SpecNameUse,PartName=PartName,Type=Type,Skip=1,YrBack=YrBack,Plot=T,Regress=F,
    SumName="CPS", Dirn=Dirn) 
}

# ===============================================================================================================

CalculateSigmas_Cat2 <- function(YrBack=20,filename,Dirn)
{

 cat("\n","All 6 Stocks:", "\n")
 SpecName <- c("Cowcod", "Blue/Deacon", "China", "Longspine", "Pacific sardine", "Pacific mackerel", "Blackgill")
 SpecNameUse <- c("Cowcod", "Blue/Deacon", "China", "Longspine", "Pacific sardine", "Pacific mackerel", "Blackgill")
 PartName <- c(rep("SSB",100))

 Type <- c(1,1,1,1,4,4,1)
 par(mfrow=c(3,3))
 ExtraCV2(FileName=filename,SpecName=SpecName,SpecNameUse=SpecNameUse,PartName=PartName,Type=Type,Skip=1,YrBack=YrBack,PlotAll=T,Regress=F,SumName="", Dirn=Dirn) 

 par(mfrow=c(2,2))
 cat("\n","Rockfish Stocks:", "\n")
 SpecName <- c("Cowcod", "Blue/Deacon", "China", "Longspine", "Blackgill")
 SpecNameUse <- c("Cowcod", "Blue/Deacon", "China", "Longspine", "Blackgill")
 PartName <- c(rep("SSB",100))
 ExtraCV2(FileName=filename,SpecName=SpecName,SpecNameUse=SpecNameUse,PartName=PartName,Type=Type,Skip=1,YrBack=YrBack,Plot=T,Regress=F,
    SumName="Rockfish", Dirn=Dirn)

 cat("\n","CPS Stocks:", "\n")
 SpecName <- c("Pacific sardine", "Pacific mackerel")
 SpecNameUse <- c("Pacific sardine", "Pacific mackerel")
 PartName <- c(rep("SSB",100))
 ExtraCV2(FileName=filename,SpecName=SpecName,SpecNameUse=SpecNameUse,PartName=PartName,Type=Type,Skip=1,YrBack=YrBack,Plot=T,Regress=F,
    SumName="CPS", Dirn=Dirn)
}

# ===============================================================================================================

HistBiomassPlots <- function(YrBack=20, SpecName, SpecNameUse, filename, Dirn)
{
 # Hist is a historical analysis
 # SpecName <- c("Canary","Yelloweye","Hake","Lingcod","POP","SST","Darkblotched rockfish","Bocaccio","Widow","Yellowtail","Chilipepper","Sablefish","Cabezon","Dover","Petrale sole","Pacific mackerel","Pacific sardine")
 # SpecNameUse <- c("Canary","Yelloweye","Hake","Lingcod","POP","SST","Darkblotched rockfish","Bocaccio","Widow","Yellowtail","Chilipepper","Sablefish","Cabezon","Dover","Petrale sole","Pacific mackerel","Pacific sardine")
 PartName <- c(rep("SSB",100))
 ExtraCV(Dirn=Dirn, FileName=filename,SpecName=SpecName,SpecNameUse=SpecNameUse,PartName=PartName,Skip=1,YrBack=YrBack,Plot=T) 

 # SpecName <- c("Canary","Yelloweye","Hake","Lingcod","POP","SST","Darkblotched rockfish","Bocaccio","Widow","Yellowtail","Chilipepper","Sablefish","Cabezon","Dover","Petrale sole","Pacific mackerel","Pacific sardine")
 # SpecNameUse <- c("Canary","Yelloweye","Hake","Lingcod","POP","SST","Darkblotched rockfish","Bocaccio","Widow","Yellowtail","Chilipepper","Sablefish","Cabezon","Dover","Petrale sole","Pacific mackerel","Pacific sardine")
 # SpecNameUse <- c("Canary","Yelloweye","Hake","Lingcod","Darkblotched rockfish","Bocaccio","Widow","Cabezon","Petrale sole","Pacific mackerel","Pacific sardine")
 PartName <- c(rep("SSB",100))
 ExtraCV(Dirn=Dirn,FileName=filename,SpecName=SpecName,SpecNameUse=SpecNameUse,PartName=PartName,Skip=1,YrBack=YrBack,Plot=T) 

}
# ===============================================================================================================
 
RetroBiomassPlots <- function(YrBack=20, SpecName, SpecNameUse, filename, Dirn)
{
 # True is a standard retrospective analysis
 # SpecName <- c("Canary","Yelloweye","Hake","Lingcod","Darkblotched rockfish","Bocaccio","Widow","Cabezon","Petrale sole","Pacific mackerel","Pacific sardine")
 # SpecNameUse <- c("Canary","Yelloweye","Hake","Lingcod","Darkblotched rockfish","Bocaccio","Widow","Cabezon","Petrale sole","Pacific mackerel","Pacific sardine")
 PartName <- c(rep("SSB",100))
 ExtraCV(Dirn=Dirn,FileName=filename,SpecName=SpecName,SpecNameUse=SpecNameUse,PartName=PartName,Skip=1,YrBack=YrBack,Plot=T) 

}
# ===============================================================================================================
 
ExtraCV <- function(Dirn=Dirn, FileName,SpecName, SpecNameUse, PartName=rep("",length=length(SpecName)),Skip=1,YrBack=20,Nspecies=length(SpecName),Plot=F,Stat=F,PlotAll=F,ListAll=F)
{
 if (PlotAll==T) { Plot <- F }

 # Files
 # Dirn <- "Z:\\UW Google Drive\\Thesis Stuff\\SigmaOFL\\R\\Ralston 2011 Update\\7 November 18 Run\\"
 FileName <- paste(Dirn,"/",FileName,sep="")
 # FileName <- paste(Dirn,"/",filename,sep="")

 # print(FileName)
 
 par(mfrow=c(4,3))
 
 NspeciesUse <- length(SpecNameUse)
 Resu1 <- matrix(0,nrow=NspeciesUse,ncol=4)
 Resu2 <- matrix(0,nrow=NspeciesUse,ncol=4)
 
 AllResiduals1 <- NULL
 AllResiduals2 <- NULL
 AllResiduals3 <- NULL
 AllResiduals4 <- NULL
 ISpecCnt <- 0
 for (Species in 1:Nspecies)
  {

   if (ListAll==T) print(SpecName[Species])
   
   # Header
   Vec <- scan(FileName,skip=Skip,n=3,quiet=T)
   Nyear <- Vec[1]; Yr1 <- Vec[2]; Yr2 <- Vec[3]
   Nline <- Yr2-Yr1+1
   
   # Read the data
   Years <- scan(FileName,skip=Skip+1,n=(Nyear+1),quiet=T)[-1]
   TheData <- matrix(scan(FileName,skip=Skip+2,n=(Nyear+1)*Nline,quiet=T),ncol=(Nyear+1),byrow=T)
 #  print(TheData)
    Skip <- Skip +Nline +2+1 
 
   if (SpecName[Species] %in% SpecNameUse) 
    {
     ISpecCnt <- ISpecCnt + 1

     # Plot the data
     if (Plot==T)
      {   
       ymax <- max(TheData[,-1])*1.05
       plot(c(Yr1,Yr2),c(0,ymax),xlab="Year",ylab=PartName[Species],type="n",main=SpecName[Species])
       for (Iy in 1:length(Years))
        {
         Use <- TheData[,Iy+1] >=0
         yy <- TheData[Use,Iy+1]
         xx <- TheData[Use,1]
         if (SpecName[Species]=="Bocaccio" & Iy>=5 | SpecName[Species]=="Darkblotched rockfish" & Iy>=5 | SpecName[Species]=="Chilipepper" & Iy>=3) {
          lines(xx,yy,lty=1,col="red", lwd=4)
         }
          lines(xx,yy,lty=Iy)
         if (Iy==length(Years)) {
          lines(xx,yy,lty=1, lwd=2)
         }
        }
      }  

    # Strip out the years
    TheData <- TheData[,-1] 
      
    # Header
    if (ListAll==T) cat("Method       SD  Mean\n") 
    
    # Method #1 (find the log differences but only for the last YrBack years)
    Residuals <- NULL
    for (Ipnt in Nline:(Nline-YrBack+1))
     {
      for (Iy in 1:length(Years)) 
       for (Iy2 in 1:length(Years)) 
        if (Iy != Iy2 & TheData[Ipnt,Iy] > 0 & TheData[Ipnt,Iy2] > 0) 
         {
          Resu <-log(TheData[Ipnt,Iy2]/TheData[Ipnt,Iy])
          #cat(Ipnt,Iy,Iy2,TheData[Ipnt,Iy],TheData[Ipnt,Iy2],Resu,"\n")
          Residuals <- c(Residuals,Resu)
         }
     }

    if (PlotAll==T) hist(Residuals,main="Method #1") 
    if (length(Residuals) > 1)
     {
      if (ListAll==T) cat("Method #1",round(sd(Residuals)/sqrt(2),3),round(mean(Residuals),3),"\n")
      Resu1[ISpecCnt,2] <- sd(Residuals)/sqrt(2)
      Resu2[ISpecCnt,2] <- mean(Residuals)
      AllResiduals2 <- c(AllResiduals2,Residuals)
     } 

    # Method #2 (set "true" to mean)
    Residuals <- NULL
    for (Ipnt in Nline:(Nline-YrBack+1))
     {
      # Find the geometric mean 
      mean <- 0; nval <- 0
      for (Iy in 1:length(Years))
       if (TheData[Ipnt,Iy] > 0)
       { mean <- mean + log(TheData[Ipnt,Iy]); nval <- nval + 1}
      mean <- mean / nval 
  #    cat(TheData[Ipnt,1],exp(mean),"\n")
      if (nval > 1)
       for (Iy in 1:length(Years))
        if (TheData[Ipnt,Iy] > 0)
         Residuals <- c(Residuals,log(TheData[Ipnt,Iy])-mean)
     }
    if (PlotAll==T) hist(Residuals,main="Method #1") 
    if (length(Residuals) > 1)
     {
      if (ListAll==T) cat("Method #2",round(sd(Residuals),3),round(mean(Residuals),3),"\n")
      Resu1[ISpecCnt,3] <- sd(Residuals)
      Resu2[ISpecCnt,3] <- mean(Residuals)
      AllResiduals3 <- c(AllResiduals3,Residuals)
     } 

    # Method #2 (set true to last)
    Residuals <- NULL
    for (Ipnt in Nline:(Nline-YrBack+1))
     {
      for (Iy in 1:(length(Years)-1))
       if (TheData[Ipnt,Iy] > 0)
        {
         Resu <- log(TheData[Ipnt,Iy]/TheData[Ipnt,Nyear])
         Residuals <- c(Residuals,Resu)
        } 
     }
   if (length(Residuals) > 1)
     {
      if (ListAll==T) cat("Method #3",round(sd(Residuals),3),round(mean(Residuals),3),"\n")
      Resu1[ISpecCnt,4] <- sd(Residuals)
      Resu2[ISpecCnt,4] <- mean(Residuals)
      AllResiduals4 <- c(AllResiduals4,Residuals)
     } 

    # Method #4 (Ralston)
    Residuals <- NULL
    NyrCnt <- matrix(10000,ncol=length(Years),nrow=Nline)
    for (Iy in 1:length(Years))
     {
      Yr <- 0
      for (Ipnt in Nline:1) 
       if (TheData[Ipnt,Iy] > 0) 
        { Yr <- Yr + 1; NyrCnt[Ipnt,Iy] <- Yr }
     }  
    for (Iy in 1:length(Years))
     for (Ipnt in Nline:1) 
      if (NyrCnt[Ipnt,Iy] > YrBack) NyrCnt[Ipnt,Iy] <- 10000 
   
  #  print(NyrCnt) 
    for (Ipnt in 1:Nline)
     {
      for (Iy in 1:length(Years)) 
       for (Iy2 in 1:length(Years)) 
        if (Iy != Iy2 & NyrCnt[Ipnt,Iy] <=YrBack & NyrCnt[Ipnt,Iy2] <= YrBack) 
         {
          Resu <-log(TheData[Ipnt,Iy2]/TheData[Ipnt,Iy])
          # cat(Ipnt,Iy,Iy2,TheData[Ipnt,Iy],TheData[Ipnt,Iy2],Resu,"\n")
          Residuals <- c(Residuals,Resu)
         }
     }

    if (PlotAll==T) hist(Residuals,main="Method #4") 
    if (length(Residuals) > 1)
     {
      if (ListAll==T) cat("Method #4",round(sd(Residuals)/sqrt(2),3),round(mean(Residuals),3),"\n")
      Resu1[ISpecCnt,1] <- sd(Residuals)/sqrt(2)
      Resu2[ISpecCnt,1] <- mean(Residuals)
      AllResiduals1 <- c(AllResiduals1,Residuals)
     } 
   }  
  else
   print(paste("No doing",SpecName[Species]))

 } # Species
 
 print(Resu1)
# print(cbind(SpecNameUse,Resu1))
# AA
# print(Resu2)
 
 par(mfrow=c(2,2))
 hist(Resu1[,1],main="Method 1",xlab="Sigma")
 hist(Resu1[,2],main="Method 2",xlab="Sigma")
 hist(Resu1[,3],main="Method 3",xlab="Sigma")
 hist(Resu1[,4],main="Method 4",xlab="Sigma")
 hist(AllResiduals1,main="Method 1",xlab="Residuals")
 hist(AllResiduals2,main="Method 2",xlab="Residuals")
 hist(AllResiduals3,main="Method 3",xlab="Residuals")
 hist(AllResiduals4,main="Method 4",xlab="Residuals")
 
 Resu1[Resu1==0] <- NA
 Resu2[Resu2==0] <- NA
 
 print(apply(Resu1,2,mean,na.rm=T))
# print(apply(Resu2,2,mean,na.rm=T))
 cat(sd(AllResiduals1)/sqrt(2),sd(AllResiduals2)/sqrt(2),sd(AllResiduals3),sd(AllResiduals4),"\n")
 
}

# ===============================================================================================================

ExtraCV2 <- function(Dirn, FileName,SpecName,SpecNameUse,PartName=rep("",length=length(SpecName)),Type,Skip=1,YrBack=20,Nspecies=length(SpecName),Plot=F,Stat=F,PlotAll=F,ListAll=F,Regress=F,SumName="")
{

 if (PlotAll==T) Plot <- F

 # Files
 FileName <- paste(Dirn, "/", FileName,sep="")
 # FileName <- paste(Dirn, "/", filename,sep="")
 
# par(mfrow=c(4,3))
 
 NspeciesUse <- length(SpecNameUse)
 Resu1 <- rep(0,length=NspeciesUse)
 Resu2 <- rep(0,length=NspeciesUse)
 Resu3A <- rep(0,length=NspeciesUse)
 
 Type <- Type[1:NspeciesUse]
 
 AllResiduals3 <- NULL
 ISpecCnt <- 0
 for (Species in 1:Nspecies)
  {

   if (ListAll==T) print(SpecName[Species])
   
   # Header
   Vec <- scan(FileName,skip=Skip,n=3,quiet=T)
   Nyear <- Vec[1]; Yr1 <- Vec[2]; Yr2 <- Vec[3]
   Nline <- Yr2-Yr1+1
   
   # Read the data
   Years <- scan(FileName,skip=Skip+1,n=(Nyear+1),quiet=T)[-1]
   TheData <- matrix(scan(FileName,skip=Skip+2,n=(Nyear+1)*Nline,quiet=T),ncol=(Nyear+1),byrow=T)
 #  print(TheData)
    Skip <- Skip +Nline +2+1 
 
   if (SpecName[Species] %in% SpecNameUse) 
    {
     ISpecCnt <- ISpecCnt + 1

    # Strip out the years
    TheData <- TheData[,-1] 
      
    # Header
    if (ListAll==T) cat("Method       SD  Mean\n") 
    

    # Method #2 (set "true" to mean)
    Residuals <- NULL
    for (Ipnt in Nline:(Nline-YrBack+1))
     {
      # Find the geometric mean 
      mean <- 0; nval <- 0
      for (Iy in 1:length(Years))
       if (TheData[Ipnt,Iy] > 0)
       { mean <- mean + log(TheData[Ipnt,Iy]); nval <- nval + 1}
      mean <- mean / nval 
  #    cat(TheData[Ipnt,1],exp(mean),"\n")
      if (nval > 1)
       for (Iy in 1:length(Years))
        if (TheData[Ipnt,Iy] > 0)
          { Residuals <- c(Residuals,log(TheData[Ipnt,Iy])-mean); Resu3A[ISpecCnt] <- Resu3A[ISpecCnt]+1 }
     }
    if (length(Residuals) > 1)
     {
      if (PlotAll==T) hist(Residuals,main=SpecName[Species]) 
      if (ListAll==T) cat("Method #2",round(sd(Residuals),3),round(mean(Residuals),3),"\n")
      Resu1[ISpecCnt] <- sd(Residuals)
      Resu2[ISpecCnt] <- mean(Residuals)
      AllResiduals3 <- c(AllResiduals3,Residuals)
     } 
   
   }  
  else
   print(paste("No doing",SpecName[Species]))
 print(Resu3A)
 } # Species
 
 print(Resu1)
 print(Resu3A)


# print(Resu2)
# print(cbind(SpecNameUse,Resu1))
# AA
# print(Resu2)
 
 #hist(Resu1,main="Method 3",xlab="Sigma")
 par(mfrow=c(2,2))
 hist(AllResiduals3,main=SumName,xlab="Residuals",xlim=c(-1.5,1.5),yaxs="i")
 
 Resu1[Resu1==0] <- NA

 print(cbind(SpecNameUse,Resu1,Type,Resu3A))
 print(sqrt(mean(Resu1^2)))
# print(apply(Resu2,2,mean,na.rm=T))
 cat("Sigma:",sd(AllResiduals3),"\n")
 Sigma <-sd(AllResiduals3)
 N <- length(AllResiduals3)
 cat("CI:",sqrt((N-1)*Sigma^2/qchisq(0.975,N-1)),sqrt((N-1)*Sigma^2/qchisq(0.025,N-1)),"\n")

 if (Regress==T)
  {
   Regress <- lm(Resu1~factor(Type))
   print(summary(Regress))
   print(anova(Regress))
   TheD <- data.frame(Resu1=Resu1^2,Type)
   TheData <- groupedData(Resu1~1|Type,data=as.data.frame(TheD))
   print(TheData)
   Regress <- lme(Resu1~1,random=~1,data=TheData)
   plot(Regress)
   print(summary(Regress))
   
  } 
 
}

# ================================================================================================================================

Fig1 <- function()
{
 par(mfrow=c(2,2))
 
 xx <- seq(from=-4,to=4,by=0.01)
 yy <- dnorm(xx,0,1)
 plot(xx,yy,yaxs="i",ylim=c(0,max(yy)*1.05),xlab="OFL",ylab="Density",lty=1,type='l',lwd=2,axes=F)
 axis(1,at=c(-4,-1.64,0,4),label=c("","ABC","OFL",""))
 
 xx <- seq(from=-4,to=-1.64,length=100)
 yy <- dnorm(xx,0,1)
 xx <- c(xx,-1.64,-4)
 yy <- c(yy,0,0)
 polygon(xx,yy,density=50)
 text(-3.5,0.1,expression(P["*"]),cex=1.72)
 arrows(-3.3,0.08,-2,0.02,lwd=2,length=0.1)
 axis(2)
}


# ================================================================================================================================

HistBioApproach(YrBack=20)
