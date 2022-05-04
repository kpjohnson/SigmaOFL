
CI.95 <- function(x, N) {
		CI1 <- sqrt((N-1)*x^2/qchisq(0.975,N-1))
		CI2 <- sqrt((N-1)*x^2/qchisq(0.025,N-1))
		CI <- c(CI1, CI2)
		return(CI)
	}

SigmaAnalysis <- function(RecType, Nproj, Nsim, ProjType, ProjStart) {
	Case <- RecType
	# z will index either OFL or spawning biomass projections
	# 1 is for SpawnB and 2 is for OFL
	z <- ProjType
	Outs <- list()
	nstart <- length(ProjStart)


	if (Case==1) {
		DAT <- list()
		for (t in 1:nstart) {
			load(paste(ProjStart[t], "_StochProjResults.Rdata", sep=""))
			DAT[[t]] <- ProjRes
		}
		# Grab one of the projections to get some starting information
		dat.temp <- as.data.frame(DAT[[1]][[1]][z])
		
		# load("1998_StochProjResults.Rdata")
		# DAT[[1]] <- ProjRes
		# load("2003_StochProjResults.Rdata")
		# DAT[[2]] <- ProjRes
		# load("2008_StochProjResults.Rdata")
		# DAT[[3]] <- ProjRes
		# # Grab one of the projections to get some starting information
		# dat.temp <- as.data.frame(DAT[[1]][[1]][z])
		
	}  

	if (Case==2) {  
	 	load("Deterministic Projections.Rdata")
	 	dat.temp <- as.data.frame(DAT[[1]][z])
	 }  

	Stock <- as.character(dat.temp[1,which(!is.na(dat.temp[1,]))])[-1]
	nstk <- length(Stock)
	LogMean_sp <- matrix(ncol=Nproj, nrow=nstk)
	rownames(LogMean_sp) <- Stock
	LogMean <- list()
	LogObs <- list()

	# Species-, projection year-, and start year-specific mean
	for (y in 1:nstart) {
	  LogObs_sp <- matrix(0, ncol=Nproj, nrow=nstk)
	  rownames(LogObs_sp) <- Stock
	  for (s in 1:nstk) {
			# Find the data for the stock of interest
			Index <- which(dat.temp[,1]==as.character(Stock[s])) + 1
			# How many assessments are available for this stock?
			nasmt <- as.numeric(dat.temp[Index,1])
			for (j in 1:Nsim) {
				if (Case==1) {
					jdata <- DAT[[y]][[j]][[z]]
				} else {
					jdata <- DAT[[y]][[z]]
				}
				for (p in 1:Nproj) {
					# sum over j data sets and i assessments
					LogObs_sp[s,p] <- LogObs_sp[s,p] + sum(log(as.numeric(jdata[Index+1+p,2:(nasmt+1)])))
				}
			}
			################## EQUATION 1 ##################
			# Calculate the species-specific mean
			################################################
			LogMean_sp[s,] <- (1/(Nsim*nasmt))*LogObs_sp[s,]
		}
		LogMean[[y]] <- LogMean_sp
		LogObs[[y]] <- LogObs_sp
	}
	# print("Equation 1: Start year- and species-specific mean")
	# print(LogMean)
	# print(LogObs)

	#### SIGMA TIME ####
	SigmaY <- vector(length=nstart)
	SigmaY.CI <- list()
	SigmaYS <- matrix(ncol=nstk, nrow=nstart)
	YS.CI <- list()
	SigmaYP <- matrix(nrow=nstart, ncol=Nproj)
	YP.CI <- list()
	SigmaYSP <- list()
	YSP.CI <- list()
	TempView <- list()

	for (y in 1:nstart) {
		# cat("Start year #",y,"\n")
	  	temp <- matrix(0,ncol=Nproj, nrow=nstk)
		rownames(temp) <- Stock
		nsp <- c()
		nsp_p <- c()
		mat <- matrix(nrow=nstk, ncol=Nproj)
		SigmaYS.CI <- list()
		SigmaYP.CI <- matrix(nrow=Nproj, ncol=2)
		Spp_YSP.CI <- list()
		SigmaYSP.CI <- matrix(nrow=Nproj, ncol=2)
		for (s in 1:nstk) {
			# Find the data for the stock of interest
			Index <- which(dat.temp[,1]==as.character(Stock[s])) + 1
			# How many assessments are available for this stock?
			nasmt <- as.numeric(dat.temp[Index,1])
			nsp <- c(nsp, rep(nasmt, times=Nproj))
			nsp_p <- c(nsp_p, nasmt)
			for (j in 1:Nsim) {
				if (Case==1) {
					jdata <- DAT[[y]][[j]][[z]]
				} else {
					jdata <- DAT[[y]][[z]]
				}
				for (p in 1:Nproj) {
					# sum over j and i
					temp[s,p] <- temp[s,p] + sum((log(as.numeric(jdata[Index+1+p,2:(nasmt+1)]))-LogMean[[y]][s,p])^2.0)
				}
			}
		}
		################## EQUATION 2a ##################
		# Project year-pooled; species-pooled; start year-specific sigma (SigmaY)
		################################################
		# Sum over species
		SigmaY[y] <- sqrt((1.0/sum(Nsim*nsp-1.0))*sum(temp))
		SigmaY.CI[[y]] <- CI.95(x=SigmaY[y], N=sum(Nsim*nsp-1.0))

		################## EQUATION 2b ##################
		# Projection year-pooled, species-specific; start year-specific sigma
		################################################
		for (k in 1:nstk) {
			SS2 <- temp[k,]
			NSP <- rep(nsp_p[k], times=Nproj)
			SigmaYS[y,k] <- sqrt(1.0/sum(Nsim*NSP-1.0)*sum(SS2))
			SigmaYS.CI[[k]] <- CI.95(x=SigmaYS[y,k], N=sum(Nsim*NSP-1.0))
		}
		YS.CI[[y]] <- SigmaYS.CI

		################## EQUATION 2c ##################
		# Projection year-specific; species-pooled; start year-specific sigma
		################################################
		SS <- colSums(temp)
		SigmaYP[y,] <- sqrt(1.0/sum(Nsim*nsp_p-1.0)*SS)
		for (pci in 1:Nproj) {
			SigmaYP.CI[pci,] <- CI.95(x=SigmaYP[y,pci], N=sum(Nsim*nsp_p-1.0))
		}
		YP.CI[[y]] <- SigmaYP.CI
		################## EQUATION 2d ##################
		# Projection year-specific; species-specific; start year-specific
		################################################
		for (m in 1:nstk) {
			NSP <- rep(nsp_p[m], times=Nproj)
			mat[m,] <- sqrt(1.0/(Nsim*NSP-1.0)*temp[m,])
			if (Case==1) {
				for (pci in 1:Nproj) {
				SigmaYSP.CI[pci,] <- CI.95(x=mat[m,pci], N=(Nsim*NSP[1]-1.0))
				}
				Spp_YSP.CI[[m]] <- SigmaYSP.CI
			}

		}
		SigmaYSP[[y]] <- mat
		if (Case==1) { YSP.CI[[y]] <- Spp_YSP.CI }
		# TempView[[y]] <- temp
	}
	# print(temp)
	# print("Equation 2a: Project year-pooled; species-pooled; start year-specific sigma")
	# # AEP this is the one I care about
	# print(SigmaY)
	# print(SigmaY.CI)
	# print("Equation 2b: Projection year-pooled, species-specific; start year-specific sigma")
	# print(SigmaYS)
	# print(YS.CI)
	# print("Equation 2c: Projection year-specific; species-pooled; start year-specific sigma")
	# print(SigmaYP)
	# print(YP.CI)
	# print("Equation 2d: Projection year-specific; species-specific; start year-specific")
	# print(SigmaYSP)
	# if (Case==1) { print(YSP.CI) }
	# print(TempView)

	# Start year-pooled among assessment sigma estimates

	################## EQUATION 3 ##################
	# Projection year-pooled, species-pooled mean
	#################################################
	SigmaBar <- sqrt((1/nstart)*sum(SigmaY^2))
	SigmaBarCI <- CI.95(x=SigmaBar, N=nstart)

	# print("Equation 3a: Projection year-pooled, species-pooled mean")
	# print(SigmaBar)
	################## EQUATION 4a ##################
	# Projection year-pooled, species-specific mean
	#################################################
	SigmaBarS <- vector(length=nstk)
	SigmaBarSCI <- matrix(ncol=2, nrow=nstk)

	for (ss in 1:nstk) {
		SigmaBarS[ss] <- sqrt((1/nstart)*sum(SigmaYS[,ss]^2))
		SigmaBarSCI[ss,] <- CI.95(x=SigmaBarS[ss], N=nstart)
	}


	# print("Equation 3b: Projection year-pooled, species-specific mean")
	# print(SigmaBarS)
	################## EQUATION 4b ##################
	# Projection year-specific, species-pooled mean
	#################################################
	SigmaBarP <- vector(length=Nproj)
	SigmaBarPCI <- matrix(ncol=2, nrow=Nproj)

	for (pp in 1:Nproj) {
		SigmaBarP[pp] <- sqrt((1/nstart)*sum(SigmaYP[,pp]^2))
		SigmaBarPCI[pp,] <- CI.95(x=SigmaBarP[pp], N=nstart)
	}

	# print("Equation 3c: Projection year-specific, species-pooled mean")
	# print(SigmaBarP)
	################## EQUATION 4c ##################
	# Projection year-specific, species-specific mean
	#################################################
	SigmaBarSP <- matrix(nrow=nstk, ncol=Nproj)
	SigmaBarSPCI <- list()

	for (s in 1:nstk) {
		SigmaBarSPCImat <- matrix(ncol=2, nrow=Nproj)
		for (p in 1:Nproj) {
		  SigmaBarSP[s,p] <- 0
			for (Y in 1:nstart)  {
				SigmaBarSP[s,p] <- SigmaBarSP[s,p] + sum(SigmaYSP[[Y]][s,p]^2)
			}
		  SigmaBarSP[s,p] <- sqrt(1.0/nstart*SigmaBarSP[s,p])
		  SigmaBarSPCImat[p,] <- CI.95(x=SigmaBarSP[s,p], N=nstart)
		}
		SigmaBarSPCI[[s]] <- SigmaBarSPCImat
	}
	# print("Equation 3d: Projection year-specific, species-specific mean")
	# print(SigmaBarSP)
	
	# To avoid crashes
	SigmaYSI <- NULL
	SigmaYSPI <- NULL
	SigmaYSICI <- NULL
	SigmaYSPICI <- NULL
	YSICI <- list()
	
	if (Case==1) {
		# Within assessment sigma estimates
		################## EQUATION 5 ##################
		# Species-, projection year- and start year-specific mean
		#################################################
		LogMean_spi <- matrix(nrow=nstk, ncol=Nproj)
		StartYObsList <- list()
		StartYMeanList <- list()

		for (y in 1:nstart) {
			SpeciesObsList <- list()
			SpeciesMeanList <- list()
			for (s in 1:nstk) {
				# Find the data for the stock of interest
				Index <- which(dat.temp[,1]==as.character(Stock[s])) + 1
				# How many assessments are available for this stock?
				nasmt <- as.numeric(dat.temp[Index,1])
				LogObs_spi <- matrix(0, nrow=nasmt, ncol=Nproj)

				for (p in 1:Nproj) {
					for (j in 1:Nsim) {
						if (Case==1) {
							jdata <- DAT[[y]][[j]][[z]]
						} else {
							jdata <- DAT[[y]][[z]]
						}
						for (i in 1:nasmt) {
							LogObs_spi[i,p] <- LogObs_spi[i,p] + sum(log(as.numeric(jdata[Index+1+p,i+1])))
						}
					}
				}
				SpeciesObsList[[s]] <- LogObs_spi
				SpeciesMeanList[[s]] <- (1/Nsim)*LogObs_spi
				# print(s)
			}
			StartYObsList[[y]] <- SpeciesObsList
			StartYMeanList[[y]] <- SpeciesMeanList
		}

		# print("WITHIN ASMT Start-year and species-specific within assessment observations")
		# print("first list is length 3 (y); second list is length nstk (s); 
		# 	rows are assessments (i); columns are projection years (p)")
		# print(StartYMeanList)
		# print(StartYObsList)

		#### WITHIN ASSESSMENT SIGMA ####
		SSList <- list()
		SSList2 <- list()
		SS_Spp <- list()
		SS_Spp2 <- list()

		for (y in 1:nstart) {
			for (s in 1:nstk) {
				Index <- which(dat.temp[,1]==as.character(Stock[s])) + 1
				# How many assessments are available for this stock?
				nasmt <- as.numeric(dat.temp[Index,1])
				SStemp <- matrix(0, ncol=nasmt, nrow=1)
				SStemp_p <- matrix(0, ncol=nasmt, nrow=Nproj)
				for (p in 1:Nproj) {
					for (j in 1:Nsim) {
						if (Case==1) {
							jdata <- DAT[[y]][[j]][[z]]
						} else {
							jdata <- DAT[[y]][[z]]
						}
						for (i in 1:nasmt) {
							SStemp[1,i] <- SStemp[1,i] + (log(as.numeric(jdata[Index+1+p,i+1])) - StartYMeanList[[y]][[s]][i,p])^2
							SStemp_p[p,i] <- SStemp_p[p,i] + (log(as.numeric(jdata[Index+1+p,i+1])) - StartYMeanList[[y]][[s]][i,p])^2
						}
					}
				}
				SS_Spp[[s]] <- SStemp
				SS_Spp2[[s]] <- SStemp_p
			}
			SSList[[y]] <- SS_Spp
			SSList2[[y]] <- SS_Spp2
		}

		################## EQUATION 6a ##################
		# Projection year-pooled, start-year, species-, and assessment-specific
		#################################################
		SigmaYSI <- list()
		SppTemp <- list()
		Denom <- sum(rep((Nsim-1), times=Nproj))
		SigmaYSICI <- list()
		YSICI <- list()

		for (y in 1:nstart) {
			for (s in 1:nstk) {
				SppTemp[[s]] <- sqrt((1/Denom)*SSList[[y]][[s]])
				SigmaYSICImat <- matrix(ncol=2, nrow=ncol(SppTemp[[s]]))
				SigmaYSICImat[,] <- CI.95(x=SppTemp[[s]], N=Denom)
				SigmaYSICI[[s]] <- SigmaYSICImat
			}
			SigmaYSI[[y]] <- SppTemp
			YSICI[[y]] <- SigmaYSICI
		}

		# print("WITHIN ASMT Projection year-pooled, start-year, species-, and assessment-specific")
		# print(SigmaYSI)

		################## EQUATION 6b ##################
		# Projection and start year-, species-, and assessment-specific
		#################################################
		SigmaYSPI <- list()
		SppTemp2 <- list()
		SigmaYSPICI <- list()

		for (y in 1:nstart) {
			YSPICI2 <- list()
			for (s in 1:nstk) {
				SppTemp2[[s]] <- sqrt((1/(Nsim-1))*SSList2[[y]][[s]])
				YSPICI <- list()
				for (i in 1:(ncol(SppTemp2[[s]]))) {
					SigmaYSPCImat <- matrix(ncol=2, nrow=Nproj)
					SigmaYSPCImat[,] <- CI.95(x=SppTemp2[[s]][,i], N=(Nsim-1))
					YSPICI[[i]] <- SigmaYSPCImat
				}
				YSPICI2[[s]] <- YSPICI
			}
			SigmaYSPI[[y]] <- SppTemp2
			SigmaYSPICI[[y]] <- YSPICI2
		}

		# print("WITHIN ASMT Projection and start year-, species-, and assessment-specific")
		# print(SigmaYSPI)
	}
	
	#### Store the output ####
	Outs$SigmaY <- SigmaY
	Outs$SigmaY.CI <- SigmaY.CI
	Outs$SigmaYS <- SigmaYS
	Outs$YS.CI <- YS.CI
	Outs$SigmaYP <- SigmaYP
	Outs$YP.CI <- YP.CI
	Outs$SigmaYSP <- SigmaYSP
	Outs$YSP.CI <- YSP.CI
	Outs$SigmaBar <- SigmaBar
	Outs$SigmaBarCI <- SigmaBarCI
	Outs$SigmaBarS <- SigmaBarS
	Outs$SigmaBarSCI <- SigmaBarSCI
	Outs$SigmaBarP <- SigmaBarP
	Outs$SigmaBarPCI <- SigmaBarPCI
	Outs$SigmaBarSP <- SigmaBarSP
	Outs$SigmaBarSPCI <- SigmaBarSPCI
	Outs$SigmaYSI <- SigmaYSI
	Outs$SigmaYSICI <- YSICI
	Outs$SigmaYSPI <- SigmaYSPI
	Outs$SigmaYSPICI <- SigmaYSPICI

	return(Outs)
}

# # RecType 1=Stoch; 2=Det; ProjType 1=SpawnB; 2=OFL
# # If doing RecType=2, change Nsim=1!
# DetSpawnB <- SigmaAnalysis(RecType=2, Nproj=25, Nsim=1, ProjType=1)
# DetOFL <- SigmaAnalysis(RecType=2, Nproj=25, Nsim=1, ProjType=2)
# StoSpawnB <- SigmaAnalysis(RecType=1, Nproj=25, Nsim=100, ProjType=1)
# StoOFL <- SigmaAnalysis(RecType=1, Nproj=25, Nsim=100, ProjType=2)

# DetSpawnB$SigmaBarCI
# DetSpawnB$SigmaBarSCI
# DetSpawnB$SigmaBarPCI
# DetSpawnB$SigmaBarSPCI
# StoSpawnB$SigmaYSICI
# StoSpawnB$SigmaYSPICI

# setwd("Z:\\UW Google Drive\\Thesis Stuff\\SigmaOFL\\R\\Additional Model Requests\\Method A\\")
# StoSpawnB <- SigmaAnalysis(RecType=1, Nproj=1, Nsim=2, ProjType=1, ProjStart=1994:2008)
# StoOFL <- SigmaAnalysis(RecType=1, Nproj=1, Nsim=2, ProjType=2, ProjStart=1994:2008)
# save(StoSpawnB, file="StoSpawnB.Rdata")
# save(StoOFL, file="StoOFL.Rdata")
# # 
# setwd("Z:\\UW Google Drive\\Thesis Stuff\\SigmaOFL\\R\\Additional Model Requests\\Method B\\")
# StoSpawnB <- SigmaAnalysis(RecType=1, Nproj=1, Nsim=100, ProjType=1, ProjStart=1994:2008)
# StoOFL <- SigmaAnalysis(RecType=1, Nproj=1, Nsim=100, ProjType=2, ProjStart=1994:2008)
# save(StoSpawnB, file="StoSpawnB.Rdata")
# save(StoOFL, file="StoOFL.Rdata")
# 
# setwd("Z:\\UW Google Drive\\Thesis Stuff\\SigmaOFL\\R\\Additional Model Requests\\Deterministic\\")
# DetSpawnB <- SigmaAnalysis(RecType=2, Nproj=1, Nsim=1, ProjType=1, ProjStart=1994:2008)
# DetOFL <- SigmaAnalysis(RecType=2, Nproj=1, Nsim=1, ProjType=2, ProjStart=1994:2008)
# save(DetSpawnB, file="DetSpawnB.Rdata")
# save(DetOFL, file="DetOFL.Rdata")

# setwd("Z:\\UW Google Drive\\Thesis Stuff\\SigmaOFL\\R\\")
# StoSpawnB <- SigmaAnalysis(RecType=1, Nproj=1, Nsim=100, ProjType=1, ProjStart=c(1998,2003,2008))
# StoOFL <- SigmaAnalysis(RecType=1, Nproj=1, Nsim=100, ProjType=2, ProjStart=c(1998,2003,2008))
# save(StoSpawnB, file="StoSpawnB.Rdata")
# save(StoOFL, file="StoOFL.Rdata")

# setwd("Z:\\UW Google Drive\\Thesis Stuff\\SigmaOFL\\R\\Additional Model Requests\\Method B\\Fixed SigmaR")
# StoSpawnB <- SigmaAnalysis(RecType=1, Nproj=1, Nsim=2, ProjType=1, ProjStart=1994:2008)
# StoOFL <- SigmaAnalysis(RecType=1, Nproj=1, Nsim=2, ProjType=2, ProjStart=1994:2008)
# save(StoSpawnB, file="StoSpawnB.Rdata")
# save(StoOFL, file="StoOFL.Rdata")

# setwd("Z:\\UW Google Drive\\Thesis Stuff\\SigmaOFL\\R\\Additional Model Requests\\Method B\\Nsim 2")
# StoSpawnB <- SigmaAnalysis(RecType=1, Nproj=1, Nsim=2, ProjType=1, ProjStart=1994:2008)
# StoOFL <- SigmaAnalysis(RecType=1, Nproj=1, Nsim=2, ProjType=2, ProjStart=1994:2008)
# save(StoSpawnB, file="StoSpawnB.Rdata")
# save(StoOFL, file="StoOFL.Rdata")

# setwd("Z:\\UW Google Drive\\Thesis Stuff\\SigmaOFL\\R\\Additional Model Requests\\Method A v2")
# StoSpawnB <- SigmaAnalysis(RecType=1, Nproj=1, Nsim=100, ProjType=1, ProjStart=1994:2008)
# StoOFL <- SigmaAnalysis(RecType=1, Nproj=1, Nsim=100, ProjType=2, ProjStart=1994:2008)
# save(StoSpawnB, file="StoSpawnB.Rdata")
# save(StoOFL, file="StoOFL.Rdata")

# setwd("Z:\\UW Google Drive\\Thesis Stuff\\SigmaOFL\\R\\Additional Model Requests\\Method B v2")
# StoSpawnB <- SigmaAnalysis(RecType=1, Nproj=1, Nsim=100, ProjType=1, ProjStart=1994:2008)
# StoOFL <- SigmaAnalysis(RecType=1, Nproj=1, Nsim=100, ProjType=2, ProjStart=1994:2008)
# save(StoSpawnB, file="StoSpawnB.Rdata")
# save(StoOFL, file="StoOFL.Rdata")

# setwd("Z:\\UW Google Drive\\Thesis Stuff\\SigmaOFL\\R\\Additional Model Requests\\Method A v2\\Final run")
# StoSpawnB <- SigmaAnalysis(RecType=1, Nproj=1, Nsim=100, ProjType=1, ProjStart=1994:2008)
# StoOFL <- SigmaAnalysis(RecType=1, Nproj=1, Nsim=100, ProjType=2, ProjStart=1994:2008)
# save(StoSpawnB, file="StoSpawnB.Rdata")
# save(StoOFL, file="StoOFL.Rdata")

# setwd("C:\\Users\\Kristin\\Documents\\UW Google Drive\\Thesis Stuff\\1 - Sigma Publication\\R\\Data\\19JuneTest\\")
# StoSpawnB <- SigmaAnalysis(RecType=1, Nproj=1, Nsim=100, ProjType=1, ProjStart=1994:2008)
# StoOFL <- SigmaAnalysis(RecType=1, Nproj=1, Nsim=100, ProjType=2, ProjStart=1994:2008)
# save(StoSpawnB, file="StoSpawnB.Rdata")
# save(StoOFL, file="StoOFL.Rdata")

# setwd("Z:\\UW Google Drive\\Thesis Stuff\\SigmaOFL\\R\\Final Thesis Run\\Test\\")
# StoSpawnB <- SigmaAnalysis(RecType=1, Nproj=25, Nsim=100, ProjType=1, ProjStart=c(1998,2003,2008))
# StoOFL <- SigmaAnalysis(RecType=1, Nproj=25, Nsim=100, ProjType=2, ProjStart=c(1998,2003,2008))
# DetSpawnB <- SigmaAnalysis(RecType=2, Nproj=25, Nsim=100, ProjType=1, ProjStart=c(1998,2003,2008))
# DetOFL <- SigmaAnalysis(RecType=2, Nproj=25, Nsim=100, ProjType=2, ProjStart=c(1998,2003,2008))
# save(StoSpawnB, file="StoSpawnB.Rdata")
# save(StoOFL, file="StoOFL.Rdata")
# save(DetSpawnB, file="DetSpawnB.Rdata")
# save(DetOFL, file="DetOFL.Rdata")