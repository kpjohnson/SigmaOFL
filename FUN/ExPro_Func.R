#==================================================================================#
# Extraction, OFL/Biomass Calculation,and Projection Functions
# Original Author: Andre E. Punt
# Altered by: Kristin M. Privitera-Johnson
# E-mail: kpjohns@uw.edu
#==================================================================================#
#==================================================================================#
# Global objects
#==================================================================================#

# SS report files that report spawning output in terms of eggs
EggRep <- as.data.frame(cbind("DBRK 2013", "DBRK 2015", "BOCA 2009", "BOCA 2011", 
  "BOCA 2013", "BOCA 2015", "YEYE 2009"))

#==================================================================================#
# EXTRACT FUNCTION
#==================================================================================#
# FileName is the path to the report file you want to project.
# Year is the Projection Start Year
# PrintAll==TRUE will print many of the extractions to the R Console for review
# SSV is an argument for providing the Stock Synthesis Version number if it is 
# not stated in the report file
Extract <- function(FileName, Year, PrintAll=F, SSV=3.3)
{
  # Read in the whole report file as a super table
  TheD <- read.table(FileName,fill=T,sep="",col.names = paste0("V", seq_len(150)),blank.lines.skip = FALSE, stringsAsFactors=F)
  
  # Version of SS 
  if (TheD[1,1]=="") {
    SSVersion <- as.numeric(substr(TheD[2,1], 5,7))
    if(is.na(SSVersion)) {
      SSVersion <- SSV
    }
  } else {
    SSVersion <- as.numeric(substr(TheD[1,1], 5,7))
  }

  # Report-specific Information
  Index <- which(TheD[,1]=="NUMBERS_AT_AGE")
  if (SSVersion==3.3) {
    # Yelloweye 2017
    Indx <- which(TheD[,1]=="fleet_names:")
    AllFleet <- length(which(TheD[Indx,]!=""))-1
    Nfleet <- length(which(TheD[(Indx+2):(Indx+2+AllFleet-1),1]=="1"))
  } else {
    Nfleet <- length(which(substr(TheD[,2], 1, 6)=="InitF_"))
  }
  
  Nage <- as.numeric(TheD[which(TheD[,1]=="BIOLOGY"),4])
  NAA_After <- which(TheD[,2]=="NUMBERS_AT_AGE")+1    # What section comes after NAA?
  AfterIndex <- which(TheD[,1]==TheD[NAA_After,2])-2  # To get to the end of NAA matrix
  Nsex <- length(unique(TheD[(Index+2):AfterIndex,3]))
  InitialYear <- as.numeric(substr(TheD[(which(TheD[,1]=="SPB_Initial")+1),1],5,8))
  FinalYear <- as.numeric(substr(TheD[(which(TheD[,1]=="Recr_Virgin")-1),1],5,8))
  YearI <- InitialYear
  YearF <- FinalYear
  File <- FileName
  # YEYE 2009/2017
  NArea <- as.numeric(unique(TheD[(Index+2):AfterIndex,1]))
  NBioPat <- as.numeric(unique(TheD[(Index+2):AfterIndex,2]))

  # Report specific Information
  Index <- which(TheD[,1]=="Z_AT_AGE_Annual_2")+2
  ZATAGE <- TheD[Index:(Index+2*(FinalYear-InitialYear-1)+1),1:(Nage+3)]


  if(PrintAll==T) {
    print("Useful Information:")
    print("SSVersion")
    print(SSVersion)
    print("Nfleet")
    print(Nfleet)
    print("Nage")
    print(Nage)
    print("Nsex")
    print(Nsex)
    print("Initial Year")
    print(InitialYear)
    print("Final Year")
    print(FinalYear)
  }

  # Extract the numbers-at-age (NAA) matrix
  ageStruct <- matrix(0,nrow=2,ncol=Nage+1)
  Index <- which(TheD[,1]=="NUMBERS_AT_AGE")
  age0 <- which(TheD[Index+1,]==0)
  ageN <- which(TheD[Index+1,]==Nage)
  Time <- which(TheD[Index+1,]=="Time")
  BM <- which(TheD[Index+1,]=="Beg/Mid")

  NAA_After <- which(TheD[,2]=="NUMBERS_AT_AGE")+1
  AfterIndex <- which(TheD[,1]==TheD[NAA_After,2])-2 # To get to the end of NAA matrix

  if (length(NArea)==1) {
    SexIndex <- which(TheD[1:AfterIndex,Time]==Year)
  } else {
    if (length(NBioPat) > 1) {
      SexIndex <- matrix(nrow=(length(NArea)), ncol=Nsex)
      for (IArea in 1:length(NArea)) {
          SexIndex[IArea,] <- which(TheD[1:AfterIndex,Time]==Year & TheD[1:AfterIndex,1]==as.character(IArea) & TheD[1:AfterIndex,2]==as.character(IArea))
      }
    } else {
      SexIndex <- matrix(nrow=(length(NArea)), ncol=Nsex)
      for (IArea in 1:length(NArea)) {
        SexIndex[IArea,] <- which(TheD[1:AfterIndex,Time]==Year & TheD[1:AfterIndex,1]==as.character(IArea))
      }
    }
  }
  

  if (PrintAll==T) {
    print("Index for NUMBERS-AT-AGE matrix")
    print(Index)
    print("Age0 and Plus Group")
    print(TheD[Index+1,age0])
    print(TheD[Index+1,ageN])
    print("Index(ices) for Sex(es); 1 then 2 if applicable")
    print(SexIndex)
    print(TheD[SexIndex,Time])
    print(TheD[SexIndex,which(TheD[Index+1,]=="Gender")])
   }  
  
  # Combine information for assessment models with multiple areas, if applicable.
  if (length(NArea)==1) {
    ageStruct[1,] <- as.double(TheD[SexIndex[1],age0:ageN])
    if (PrintAll==T) {
      print("Numbers-at-age Matrix, Sex1")
      print(ageStruct[1,])
    }
    if (Nsex==2)
    {  
      ageStruct[2,] <- as.double(TheD[SexIndex[2],age0:ageN])
      if (PrintAll==T) {
        print("Numbers-at-age matrix, Sex2")
        print(ageStruct[2,])
      }
    }  
  } else {
    for (IArea in 1:(length(NArea))) {
      ageStruct[1,] <- ageStruct[1,] + as.double(TheD[SexIndex[IArea,1],age0:ageN])
      if (PrintAll==T) {
        print("Numbers-at-age Matrix, Sex1")
        # print(SexIndex[IArea,1])
        print(ageStruct[1,])
      }
      if (Nsex==2)
      {  
        ageStruct[2,] <- ageStruct[2,] + as.double(TheD[SexIndex[IArea,2],age0:ageN])
        if (PrintAll==T) {
          print("Numbers-at-age matrix, Sex2")
          # print(SexIndex[IArea,2])
          print(ageStruct[2,])
      }
      }  
    }
  }

  # Extract fecundity (at B0 and at present)
  # KPJ created Year0 because sometimes Year0 is not the year before YearI
  # ex: Bocaccio 2011 YearI=1892 and the previous year for Fecund is 1889
  if(SSVersion>=3.2) {
    Year0 <- as.numeric(TheD[which(TheD[,1]=="Fecund"),3][1])
    Index <- which(TheD[,1]=="Fecund" & TheD[,3]==Year0)
    FecundB0 <- as.double(TheD[Index,8:(7+Nage+1)])
    if (PrintAll==T) {
      print("FecundB0")
      print(FecundB0)
    }
    Index <- which(TheD[,1]=="Fecund" & TheD[,3]==Year)
    Fecund <- as.double(TheD[Index,8:(7+Nage+1)])
    if (PrintAll==T) {
      print("Fecund")
      print(Fecund)
    }
  } else {
    Index <- which(TheD[,1]=="Biology_at_age")
    FecundB0 <- as.numeric(TheD[(Index+2):(Index+Nage+2),18])
    Fecund <- as.numeric(FecundB0)

    if (PrintAll==T) {
      print("Fecund")
      print(Fecund)
      print("FecundB0")
      print(FecundB0)
    }
  }
  
  # Extract selectivity, retained selectivity, and selectivity time age
  AgeSelex <- array(0,dim=c(2,Nfleet,Nage+1))
  AgeRetSel <- array(0,dim=c(2,Nfleet,Nage+1))
  AgeRetWghtSel <- array(0,dim=c(2,Nfleet,Nage+1))
  Index <- which(TheD[,1]=="sel*wt")[1]-1
  for (Ifleet in 1:Nfleet)  
   {
    for (Isex in 1:Nsex)
     {
      Index <- Index + 2
      # The following if-statement is required because in Petrale 2009, the columns get shifted
      # more specifically, the label column gets split in two for just sel_nums
      if (TheD[Index,1]=="sel_nums") {
        AgeRetWghtSel[Isex,Ifleet,] <- as.double(TheD[Index,9:(7+Nage+2)])
      } else {
        AgeRetWghtSel[Isex,Ifleet,] <- as.double(TheD[Index,8:(7+Nage+1)])
      }

      Index <- Index + 1
      # The following if-statement is required because in Petrale 2009, the columns get shifted
      # more specifically, the label column gets split in two for just sel_nums
      if (TheD[Index,8]=="sel_nums") {
        AgeSelex[Isex,Ifleet,] <- as.double(TheD[Index,9:(7+Nage+2)])
      } else {
        AgeSelex[Isex,Ifleet,] <- as.double(TheD[Index,8:(7+Nage+1)])
      }

      Index <- Index + 1
      if (TheD[Index,8]=="sel_nums") {
        AgeRetWghtSel[Isex,Ifleet,] <- as.double(TheD[Index,9:(7+Nage+2)])
      } else {
        AgeRetWghtSel[Isex,Ifleet,] <- as.double(TheD[Index,8:(7+Nage+1)])
      }

      Index <- Index + 2
      if (TheD[Index,8]=="sel_nums") {
        AgeRetSel[Isex,Ifleet,] <- as.double(TheD[Index,9:(7+Nage+2)])
      } else {
       AgeRetSel[Isex,Ifleet,] <- as.double(TheD[Index,8:(7+Nage+1)]) 
      }
    }  
  }  

  # You can print these out to check them if you'd like, the dims are ugly to view in Console.
  # if (PrintAll==T) {
  #   print("AgeSel")
  #   print(AgeSelex)
  #   print("AgeRelWghtSel")
  #   print(AgeRetWghtSel)
  #   print("AgeRetSel")
  #   print(AgeRetSel)
  # }
  
  # Extract natural mortality
  Index <- which(TheD[,1]=="Natural_Mortality")[1]+1
  NatM <- matrix(0,nrow=Nsex,ncol=(Nage+1))

  if (TheD[Index,1]=="Bio_Pattern") {
    ini <- 5
    en <- 0
  } else {
    ini <- 6
    en <- 1
  }
  for (Isex in 1:Nsex) {
    NatM[Isex,] <- as.double(TheD[Index+Isex,ini:(5+Nage+en)])
  }

  if (PrintAll==T) {
    print("NatM")
    print(NatM)
  }

  # Extract mean body weight at the beginning of the year
  # This was probably for CPS at some point, does not appear to be currently used
  Index <- which(TheD[,1]=="MEAN_BODY_WT(begin)")[1]+1
  colIndex <- which(TheD[Index,]=="0")
  BeginWght <- matrix(1,nrow=2,ncol=(Nage+1))
  BeginWght[1,] <- as.numeric(TheD[Index+1,colIndex:(colIndex+Nage)])
  BeginWght[2,] <- as.numeric(TheD[Index+2,colIndex:(colIndex+Nage)])
  
  # Extract exploitation rate (Relative F)
  Index <-which(TheD[,1]=="EXPLOITATION")+6+(2+Year-YearI+1-2)
  Fmult <- as.double(TheD[Index,4:(3+Nfleet)])

  if (PrintAll==T) {
    print("Fmult")
    print(Fmult)
  }
  
  # Extract SR parameters
  Index <- which(TheD[,2]=="SR_sigmaR")
  SigmaR <- as.double(TheD[Index,3])
  if(length(which(TheD[,2]=="SR_R0"))==0) {
      Index <- which(TheD[,2]=="SR_LN(R0)")
    } else {
      Index <- which(TheD[,2]=="SR_R0")
    }
  if(Nsex==2) {
    R0 <- exp(as.double(TheD[Index,3]))/2.0
  } else {
    R0 <- exp(as.double(TheD[Index,3]))
  }

  if (PrintAll==T) {
    print("R0*2")
    print(R0*2)
  }

  # Steepness
  Index <- Index + 1
  Steep <- as.double(TheD[Index,3])

  if (PrintAll==T) {
    print("Steepness")
    print(Steep)
  }
  
  # Extract B0 for diagnostic check later
  Index <- which(TheD[,1]=="DERIVED_QUANTITIES")+5
  B0TsT<- as.double(TheD[Index,2])
  if (PrintAll==T) {
    print("B0 Test/Check")
    cat("B0 Test/Check",B0TsT,"\n")
  }
  
  # Extract target biomass and target catch for diagnostic check
  Index <- which(TheD[,1]=="SSB_SPRtgt")
  SSBTsT<- as.double(TheD[Index,2])
  SSBCat<- as.double(TheD[Index+2,2])
  if (PrintAll==T) {
    cat("target Test/Check",SSBTsT,SSBCat,"\n")
  }

  # Extract initial SSB for diagnostic check
  Index <- which(TheD[,1]==paste("SPB_",Year,sep=""))
  SSBCurrentTST <- as.double(TheD[Index,2])

  # egg conversion things to extract
  # These help me locate sections of interest later
  NAA_Index <- which(TheD[,1] == "NUMBERS_AT_AGE")
  Wt_Len_F_Index <- which(TheD[,1]=="BIOLOGY")+1
  Age_Len_Key_Index <- which(TheD[,1]=="AGE_LENGTH_KEY")+6

  # Weight at length to Weight at age
  N_len_bins <- as.numeric(TheD[Wt_Len_F_Index-1, 3])           # Number of length bins
  # Nage <- as.numeric(TheD[Wt_Len_F_Index-1, 4])               # Number of ages
  if (grepl(x=File, "2015")) {
    Wt_Len_F <- TheD[Wt_Len_F_Index:(Wt_Len_F_Index+N_len_bins),5]    # DBRK2015 & BOCA2015 has an extra column here
    Mat_Len <- TheD[Wt_Len_F_Index:(Wt_Len_F_Index+N_len_bins),6]   # Maturity-at-Length
  } else {
    Wt_Len_F <- TheD[Wt_Len_F_Index:(Wt_Len_F_Index+N_len_bins),4]    # This is correct for all others
    Mat_Len <- TheD[Wt_Len_F_Index:(Wt_Len_F_Index+N_len_bins),5]   # Maturity-at-Length
  }
  Age_len_Key <- TheD[Age_Len_Key_Index:(Age_Len_Key_Index+N_len_bins),]  # Age_Length_Key section

  WAA <- matrix(ncol=(Nage+1), nrow=N_len_bins)             
  for (i in 1:N_len_bins) {
    WAA[i,] <- (as.numeric(rev(Wt_Len_F)[i])*as.numeric(rev(Mat_Len)[i]))*as.numeric(Age_len_Key[i+1,2:(Nage+2)])
  }
  colnames(WAA) <- 0:Nage
  # KPJ calculated weight-at-age
  waa <- colSums(WAA)

  # Read in recruitment deviations
  rIndex <- which(TheD[,2]=="init_eq") + 1
  Nyears <- FinalYear - InitialYear+1
  rIndex <- rIndex + 4
  Start <- rIndex + which(TheD[(rIndex+1):(rIndex+Nyears-1),1]==InitialYear)
  SpawnRecSum <- TheD[Start:(Start+Nyears-2),1:9]
  SpawnRecSum <- cbind(SpawnRecSum, rep(0,Nyears-1))
  Parm_StDev <- 0 

  for (Iyear in 1:(Nyears-1))
  {
    if (as.numeric(SpawnRecSum[Iyear,1])<1980 || SpawnRecSum[Iyear,8]=="0")
      SpawnRecSum[Iyear,10] <- SigmaR
    else
     {
      Parm_StDev <- which(TheD[which(TheD[,1]=="PARAMETERS")+1,]=="Parm_StDev")
      # ORIGINAL APPROACH POINTER (METHOD A):
      RecIndex <- which(grepl(paste("RecrDev_",InitialYear+Iyear-1,sep=""), TheD[,2]))

      if (length(RecIndex)==0) {
        SpawnRecSum[Iyear,10] <- SigmaR
      } else {
        # OLD METHOD FOR RECDEV (METHOD A)
        SpawnRecSum[Iyear,10] <- TheD[RecIndex,Parm_StDev]
      }
     }
  }

  # NEW POINTER (METHOD B)
  RecIndex <- rev(which(grepl("Main_RecrDev_", TheD[,2])))[1:10]

  if(any(is.na(RecIndex))) {
    RecIndex <- rev(which(grepl("RecrDev_", TheD[,2])))[1:10]
  }

  # NEW REC DEV (METHOD B)
  RecrDev_ <- 0
  RecrDev_ <- TheD[RecIndex,Parm_StDev]
  
  colnames(SpawnRecSum) <- c("Year", "spawn_bio", "exp_recr", "with_env", 
    "bias_adj", "pred_recr", "dev", "use_biasadj", "era", "RecrDev_")

  # Return results
  DataU <- NULL
  DataU$Nage <- Nage
  DataU$Nsex <- 2
  DataU$Nfleet <- Nfleet
  DataU$R0 <- R0
  DataU$SigmaR <- SigmaR
  DataU$B0TsT <- B0TsT
  DataU$SSBTsT <- SSBTsT
  DataU$SSBCat <- SSBCat
  DataU$SSBCurrentTST <- SSBCurrentTST
  DataU$Steep <- Steep
  DataU$ageStruct <- ageStruct
  DataU$Fecund <- Fecund
  DataU$FecundB0 <- FecundB0
  DataU$AgeSelex <- AgeSelex
  DataU$AgeRelSel <- AgeRetSel
  DataU$AgeRetWghtSel <- AgeRetWghtSel
  DataU$BeginWght <- BeginWght
  DataU$NatM <- NatM
  DataU$Fmult <- Fmult
  DataU$Nfleet <- Nfleet
  DataU$Nsex <- Nsex
  DataU$YearI <- YearI
  DataU$YearF <- YearF
  DataU$waa <- waa
  # DataU$RecVec <- time.vec
  # DataU$Age_len_Key <- Age_len_Key
  # DataU$Wt_Len_F <- Wt_Len_F
  # DataU$Mat_Len <- Mat_Len
  DataU$File <- FileName
  # DataU$ZAA <- ZAA
  # DataU$PathA <- PathA
  DataU$SpawnRecSum <- SpawnRecSum
  DataU$ZATAGE <- ZATAGE
  DataU$RecrDev_ <- RecrDev_
  return(DataU)
}  

#==================================================================================#
# SPRF FUNCTION
#==================================================================================#
# TheOutput is a list object of results from the Extract() function
# Fval is the argument for the value of F you would like to use in calculating OFL
# or SSB per recruit
# InitF is a switch for either Fecundity at B0 (TRUE) or Fecundity in projection start 
#year (FALSE)

SPRF <- function(Fval,TheOutput,InitF=F)
{  
  # Read in report file output
  Nage <- TheOutput$Nage
  Nsex <- TheOutput$Nsex
  NatM <- TheOutput$NatM
  Fecund <- TheOutput$Fecund
  FecundB0 <- TheOutput$FecundB0
  BeginWght <- TheOutput$BeginWght
  Fmult <- TheOutput$Fmult
  Fmult <- Fmult/sum(Fmult)
  AgeSelex <- TheOutput$AgeSelex
  AgeWghtSel <- TheOutput$AgeRetWghtSel
  R0 <- TheOutput$R0
  Nfleet <- TheOutput$Nfleet
  waa <- TheOutput$waa
  File <- TheOutput$File
  
  # Calculate Z
  Z <- matrix(0,nrow=Nsex,ncol=(Nage+1))
  for (Isex in 1:Nsex)
    for (Iage in 1:(Nage+1))  
    {
      Z[Isex,Iage] <- NatM[Isex,Iage]
      for (Ifleet in 1:Nfleet)
        Z[Isex,Iage] <- Z[Isex,Iage] + Fval*AgeSelex[Isex,Ifleet,Iage]*Fmult[Ifleet]   
    }  
  
  # Compute equilibrium N-at-age
  N <- matrix(0,nrow=Nsex,ncol=(Nage+1))
  for (Isex in 1:Nsex) {
    N[Isex,1]  <- R0
    for (Iage in 1:Nage) {
      N[Isex,Iage+1] <- N[Isex,Iage]*exp(-Z[Isex,Iage])
      N[Isex,Nage+1] <- N[Isex,Nage+1]/(1-exp(-Z[Isex,Nage+1]))
    }
  }
  
  # Spawning biomass is females only. Also, use FecundB0 for the unfished state
  # and Fecund for the fished state
  if (InitF==TRUE)
  {  
    if (any(apply(X=EggRep, 2, grepl, x=File))) {
      SSBPerRec <- 0
      for (Iage in 2:(Nage+1))
        SSBPerRec <- SSBPerRec + as.numeric(waa[Iage])*N[1,Iage]
    } else {
    SSBPerRec <- 0
    for (Iage in 1:(Nage+1))
      SSBPerRec <- SSBPerRec + FecundB0[Iage]*N[1,Iage]
    }
  } else {
    if (any(apply(X=EggRep, 2, grepl, x=File))) { 
      SSBPerRec <- 0
      for (Iage in 2:(Nage+1))
        SSBPerRec <- SSBPerRec + as.numeric(waa[Iage])*N[1,Iage]
    } else {
    SSBPerRec <- 0
    for (Iage in 1:(Nage+1))
      SSBPerRec <- SSBPerRec + Fecund[Iage] * N[1,Iage]
    }
  }
  
  # Compute the catch
  CatPerRec <- 0
  for (Isex in 1:Nsex) 
    for (Ifleet in 1:Nfleet)  
    {
      # Compute OFL contribution by sex, age and fleet
      for (Iage in 1:(Nage+1)) 
      { 
        Prop <- N[Isex,Iage]/Z[Isex,Iage]*(1-exp(-Z[Isex,Iage])) 
        Catch <- TheOutput$AgeRetWghtSel[Isex,Ifleet,Iage]*Fval*Fmult[Ifleet]*Prop
        CatPerRec <- CatPerRec + Catch
      } 
    }  

  Outs <- NULL
  Outs$SSBPerRec <- SSBPerRec
  Outs$CatPerRec <- CatPerRec
  return(Outs)
}

#==================================================================================#
# FINDF40 FUNCTION
#==================================================================================#
# TheOutput is a list object of results from the Extract() function
# TargetSPR is the TargetSPR rate used for the stock (PFMC uses 0.5 for rockfishes,
# 0.45 for roundfishes, and 0.30 for flatfishes, respectively).
FindF40 <- function(TheOutput,TargetSPR=0.5)
{   
  # Find unfished Spawning biomass-per-recruit
  SPRF0 <- SPRF(0,TheOutput,InitF=T)$SSBPerRec  
  
  # Solve for Ftarget
  Fmin <- 0; Fmax <- 20
  for (II in 1:40)
  {
    F40 <- (Fmin+Fmax)/2
    SPRFT <- SPRF(F40,TheOutput)$SSBPerRec  
    if (SPRFT > TargetSPR*SPRF0) Fmin <- F40 else Fmax <- F40;
  }  
  
  # This is BH recruitment
  SPR <- SPRFT/TheOutput$R0
  Numer <- 4*TheOutput$R0*TheOutput$Steep*SPR/SPRF0+(TheOutput$Steep-1)
  Denom <- (5*TheOutput$Steep-1)*SPR/SPRF0
  RecDepl <-  Numer/Denom/TheOutput$R0

  
  print("SSB0, SSB0 Calc:SSB0 Report, SSB/F(0), TargeTSPR, FMult(Target), SPRFT:SPRF0, TargetBio Calc, CatchPerRecruit*RecDepl")
  cat(
    SPRF0,
    SPRF0/TheOutput$B0TsT,
    SPRF0/TheOutput$R0/2,
    SPRFT,
    F40,
    SPRFT/SPRF0,
    SPRFT*RecDepl,
    SPRF(F40,TheOutput)$CatPerRec*RecDepl,
    "\n", "\n")

  # Diagnostics
  dont_skip <- TRUE # For skipping the diagnostic for spawning output as eggs assessments (this diagnostic compares to metric tons)
  if (any(apply(X=EggRep, 2, grepl, x=TheOutput$File)) | grepl("PETR 2009", TheOutput$File)) {dont_skip <- FALSE} 
  if (dont_skip) {
  if (abs(SPRF0/TheOutput$B0TsT-1) > 0.06) { 
    print(paste("problem with B0 - stopping because calculated SPRF0/report SPRF0-1 is", 
      SPRF0/TheOutput$B0TsT-1, "> 0.06", "and calculated SPRF0 is", 
      SPRF0, "and the report unfished SPB", TheOutput$B0TsT)); AAA; }

  if (abs(SPRFT*RecDepl/TheOutput$SSBTsT-1) > 0.06) { 
    print(paste("problem with Target Biomass - stopping because calculated Target Bio/report Target Bio-1 is", 
    SPRFT*RecDepl/TheOutput$SSBTsT-1, ">0.06", "and calculated target Bio is", 
    SPRFT*RecDepl, "and report Target Biomass is", TheOutput$SSBTsT)); BBB; }
  }
  return(F40)
}

#==================================================================================#
# PROJECT FUNCTION
#==================================================================================#
# TheOutput is a list object of results from the Extract() function
# YearRef is the Projection Start Year
# Nyear is the number of years to project
# Stoch is whether or not to run the stochastic projections
# knownR is whether to run the stochastic projections, just always leave as T 
# because it is nested in a if (Stoch==TRUE) loop.
# methodA is a switch between two methods for sampling recruitment deviations.
Project <- function(TheOutput,Nproj,YearRef, Diagnostic=F, Stoch=F, knownR=T, methodA=F)
{
  # Read in the SS report file model output
  Nage <- TheOutput$Nage
  Nsex <- TheOutput$Nsex
  NatM <- TheOutput$NatM
  Fecund <- TheOutput$Fecund
  Fmult <- TheOutput$Fmult
  AgeSelex <- TheOutput$AgeSelex
  AgeRetWghtSel <- TheOutput$AgeRetWghtSel
  BeginWght <- TheOutput$BeginWght
  InitialAgeStruc <- TheOutput$ageStruct
  SSB0 <- TheOutput$B0
  Fval <- TheOutput$FTarg
  R0 <- TheOutput$R0
  SigmaR <- TheOutput$SigmaR
  Steep <- TheOutput$Steep
  Nfleet <- TheOutput$Nfleet
  waa <- TheOutput$waa
  File <- TheOutput$File
  ZATAGE <- TheOutput$ZATAGE
  SpawnRecSum <- TheOutput$SpawnRecSum
  B0TsT <- TheOutput$B0TsT
  RecrDev_ <- TheOutput$RecrDev_
  
  # Object for storing projection output 
  res<-NULL
  res$Year[1] <- YearRef  # Projection start year

  # Calculate Z
  Z <- matrix(0,nrow=Nsex,ncol=(Nage+1))
  for (Isex in 1:Nsex)
    for (Iage in 1:(Nage+1))  
    {
      Z[Isex,Iage] <- NatM[Isex,Iage]
      for (Ifleet in 1:Nfleet)
        Z[Isex,Iage] <- Z[Isex,Iage] + Fval*AgeSelex[Isex,Ifleet,Iage]*Fmult[Ifleet]
    }  

  # Set up the N matrix
  N <- array(0,dim=c(Nsex,Nproj+1,Nage+1))  
  
  for (Isex in 1:Nsex)
    N[Isex,1,] <- InitialAgeStruc[Isex,]
    # print("Init N")
    # print(N[1,1,1:Nage])

  # Which row of the Spawn Recruitment Summary corresponds to projection start year?
  SkipIndex <- which(SpawnRecSum[,1]==YearRef)
  # Which rows correspond to the first/last year with estimated "Main" recruitment deviations?
  LastMainIndex <- which(SpawnRecSum[1:SkipIndex,9]=="Main")
  LastMainIndex <- tail(LastMainIndex, 1)
  LastMainYr <- SpawnRecSum[LastMainIndex,1]
  FirstMainYr <- as.numeric(SpawnRecSum[which(SpawnRecSum[1:SkipIndex,9]=="Main")[1],1])
  YrSkip <- YearRef - FirstMainYr + 1

  if (YrSkip > Nage) {YrSkip <- Nage}

  YrTrack <- c()
  if (Stoch==TRUE)
   {
      for (Iyear in 1:YrSkip)
       {  
        # Compute recruitment from the SR relationship
        YrIndex <- YearRef-as.numeric(SpawnRecSum[1,1])+1-Iyear+1
        YrTrack <- c(YrTrack, YrIndex)
        SpawnB <- as.numeric(SpawnRecSum[YrIndex,2])

        Top <- 4*Steep*R0*SpawnB/B0TsT; Bot <- (1-Steep) + (5*Steep-1)*SpawnB/B0TsT
        ExpRec <- Top/Bot
        BiasC <- as.numeric(SpawnRecSum[YrIndex,5])/as.numeric(SpawnRecSum[YrIndex,4])
        ErecDev <-as.numeric(SpawnRecSum[YrIndex,7])

        if (methodA) {
          # OLD POINTER (Method A)     
          SrecDev <-as.numeric(SpawnRecSum[YrIndex,10])
        } else {
          # NEW POINTER (Method B)
          SrecDev <-as.numeric(RecrDev_)
          # Test pointer: fixed at SigmaR
          # SrecDev <- as.numeric(SigmaR)
        }
 
        RecDev <- exp(rnorm(1,ErecDev,SrecDev))

        # Accounts for S_R relationship, bias-correction, and uncertainty 
        NatAge <- ExpRec*BiasC*RecDev
        # Now project forward under Z
        for (Isex in 1:Nsex)
         {
          NatAge0 <- NatAge
          if (Iyear>1)
           for (Iage in 1:(Iyear-1)) 
            {  
             ii <- which(ZATAGE[,2]==Isex & ZATAGE[,3]==(YearRef-Iage))
             Zval <- as.numeric(ZATAGE[ii,3+Iyear-Iage])
             NatAge0 <- NatAge0*exp(-Zval)
            }
           N[Isex,1,Iyear] <- NatAge0
         }  
      }  
  }
  
  OFL <- rep(0,Nproj)

  if (Diagnostic==T) {
      print("SpawnB0, Spawning Biomass, SpawnB/SSB0, OFL[Iyear]")
  }

  for (Iyear in 1:Nproj)
  {
    # Groundfish
      # Catch
      for (Isex in 1:Nsex) 
        for (Ifleet in 1:Nfleet)  
        {
          # Compute OFL contribution by sex, age and fleet
          for (Iage in 1:(Nage+1)) 
          { 
            Prop <- N[Isex,Iyear,Iage]/Z[Isex,Iage]*(1-exp(-Z[Isex,Iage])) 
            Catch <- AgeRetWghtSel[Isex,Ifleet,Iage]*Fval*Fmult[Ifleet]*Prop
            OFL[Iyear] <- OFL[Iyear] + Catch
          } 
        }  
      
      # Update the dynamics
      for (Isex in 1:Nsex) 
      {
        for (Iage in 2:(Nage+1)){
          N[Isex,Iyear+1,Iage] <- N[Isex,Iyear,Iage-1]*exp(-Z[Isex,Iage-1])
        }
        N[Isex,Iyear+1,Nage+1] <- N[Isex,Iyear+1,Nage+1] + N[Isex,Iyear,Nage+1]*exp(-Z[Isex,Nage+1])
      } 
     
    # Compute the spawning biomass
    if (any(apply(X=EggRep, 2, grepl, x=File))) {
      SpawnBO <- 0
      for (Iage in 2:(Nage+1)) {
        SpawnBO <- SpawnBO + as.numeric(waa[Iage])*N[1,1,Iage]
      }
      SpawnB <- 0
      for (Iage in 2:(Nage+1)) {
        SpawnB <- SpawnB + as.numeric(waa[Iage])*as.numeric(N[1,Iyear,Iage])  # KPJ Calculated Biomass by Age
      }
    } else {
      SpawnBO <- 0
      for (Iage in 2:(Nage+1)) {
        SpawnBO <- SpawnBO + Fecund[Iage]*N[1,1,Iage]
        # cat(Fecund[Iage], N[1,1,Iage], "\n")
      }

      # Compute the spawning biomass    
      SpawnB <- 0
      for (Iage in 2:(Nage+1)) {
         SpawnB <- SpawnB + Fecund[Iage]*N[1,Iyear+1,Iage]
      }
    }
    
    # Compute recruitment from the SR relationship
    Top <- 4*Steep*R0*SpawnB/SSB0
    Bot <- (1-Steep) + (5*Steep-1)*SpawnB/SSB0
    
    if (Stoch==TRUE) {
      if (knownR) {
        eps <- rnorm(1, 0, SigmaR)
        for (Isex in 1:Nsex) N[Isex,Iyear+1,1] <- Top/Bot*(exp(eps-(SigmaR^2)/2))
      } 
    } else {
        for (Isex in 1:Nsex) N[Isex,Iyear+1,1] <- Top/Bot 
      }

    if (Iyear==1 & abs(SpawnBO-TheOutput$SSBCurrentTST) > 100)
     {
      cat("Check: Error matching last SSB:",SpawnBO,TheOutput$SSBCurrentTST,"\n");
     }

    if (Diagnostic==T) {
      cat(SpawnBO,SpawnB,SpawnB/SSB0,OFL[Iyear],"\n")
    }
      if (Iyear==1) {
        res$Year[Iyear] <- YearRef
      } else {
          res$Year[Iyear] <- YearRef + (Iyear-1)
      }

      if (Nsex==2) {
        res$SpawnB[Iyear] <- SpawnB
        res$OFL[Iyear] <- OFL[Iyear]
      } else {
        res$SpawnB[Iyear] <- SpawnB*2
        res$OFL[Iyear] <- OFL[Iyear]
      }

  }  

  res$YrTrack <- YrTrack
  # res$RecDev <- RecDev
  # res$RecrDev_ <- RecrDev_
  # res$ErecDev <- ErecDev
  return(res)
}

#==================================================================================#
# PROCESS FUNCTION
#==================================================================================#
# FileName is the path to the report file you want to project.
# YearRef is the Projection Start Year
# Nyear is the number of years to project
# Stoch is whether or not to run the stochastic projections
# knownR is whether to run the stochastic projections, just always leave as T 
# because it is nested in a if (Stoch==TRUE) loop.
# methodA is a switch between two methods for sampling recruitment deviations.
Process <- function(FileName,YearRef,TargetSPR,Nyear=10, Stoch=F, knownR=T, methodA=F)
{
  # Extract report file output
  TheOutput <- Extract(FileName,YearRef,PrintAll=F)
  # Calculate Ftarget
  TheOutput$FTarg <- FindF40(TheOutput,TargetSPR)
  # Calculate B0
  TheOutput$B0 <- SPRF(0,TheOutput,InitF=T)$SSBPerRec
  # Conduct the projections
  Project(TheOutput=TheOutput, Nproj=Nyear, YearRef=YearRef, Stoch=Stoch, knownR=knownR, methodA=F)
}

#==================================================================================#
# TEST CODE
#==================================================================================#
# # Make projections!
# # Inputs (Global)
# YearRef=1994
# TargetSPR=0.50
# Nyear=1
# CPS=FALSE
# Stoch=F
# # Change me:
# FileName="Z:\\UW Google Drive\\Thesis Stuff\\1 - Sigma Publication\\R\\Data\\ALL REPORT FILES\\CNRY 2015.sso"

# set.seed(666) # Set a seed for reproducible stochastic results
# Process(FileName=FileName, YearRef=YearRef, TargetSPR=TargetSPR, Nyear=Nyear, Stoch=Stoch, knownR=T, methodA=F)
