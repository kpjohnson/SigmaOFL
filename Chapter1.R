# Set the working directory
# this will be specific to your workflow

# Remove global variables
rm(list=ls(all=TRUE))

# Read in the functions required
source("FUN/ExPro_Func.R")
source("FUN/Data_Process.R")
source("FUN/Data_Format.R")
source("FUN/Graph_Proj.R")
source("FUN/SigmaAnalysis.R")
source("FUN/SigmaAnalysis_Table.R")

# Set the seed
set.seed(060419)

# Global variables for function arguments
# Directory
# Dirn <- # add your project directory path
# Number of projection years
Nproj <- 100
# Nproj <- 1
# The start years to use
ProjStart <- c(1998, 2003, 2008)
# ProjStart <- 1994:2008
# Print diagnostics to the Console
PrintAll <- FALSE
# Are we doing the stochastic run? (Local to this script).
DoIter <- T
# How many stochastic iterations?
niter <- 100
# For telling Data_Format() to do stochastic iterations.
knownR <- T

#### Conduct projections based on Deterministic stock-recruitment relationship ####

# Result object for deterministic run
# DAT <- list()

# ny <- length(ProjStart)
# for (y in 1:ny) {
# 	DAT[[y]] <- Data_Format(Nfore=Nproj, Stoch=F, YearRef=ProjStart[y], PrintAll=PrintAll, Dirn=Dirn)
# }

# save(DAT, file="Data//Deterministic Run Nproj1//Deterministic Projections.Rdata")

#### Conduct projections based on stochastic stock-recruitment relationship ####
# Result object for stochastic run
DATList <- list()

# Generate the stochastic projection data sets or read them in:
for (II in 1:length(ProjStart)) {
	if (DoIter) {
		ProjRes <- list()
		for (I in 1:niter) {
			DAT2 <- Data_Format(Nfore=Nproj, Stoch=TRUE, knownR=knownR, YearRef=ProjStart[II], PrintAll=PrintAll, Dirn=Dirn)
			ProjRes[[I]] <- DAT2
		}

		DATList[[II]] <- ProjRes
		# Save runs for later (if needed)
		filename <- paste("Data\\NProj Diagnostic\\", ProjStart[II], "_StochProjResults.Rdata", sep="")
		save(ProjRes, file=filename)
		print(II)
	} 
}

#### Calculate sigmas based on deterministic and stochastic projections ####

# Deterministic sigma analysis
DetSpawnB <- SigmaAnalysis(RecType=2, Nproj=100, Nsim=1, ProjType=1, ProjStart=c(1998, 2003, 2008))
DetOFL <- SigmaAnalysis(RecType=2, Nproj=100, Nsim=1, ProjType=2, ProjStart=c(1998, 2003, 2008))

save(DetSpawnB, file="DetSpawnB.Rdata")
save(DetOFL, file="DetOFL.Rdata")

# DetSpawnB <- SigmaAnalysis(RecType=2, Nproj=1, Nsim=1, ProjType=1, ProjStart=1994:2008)
# DetOFL <- SigmaAnalysis(RecType=2, Nproj=1, Nsim=1, ProjType=2, ProjStart=1994:2008)

# save(DetSpawnB, file="DetSpawnB.Rdata")
# save(DetOFL, file="DetOFL.Rdata")

# Stochastic sigma analysis
StoSpawnB <- SigmaAnalysis(RecType=1, Nproj=100, Nsim=100, ProjType=1, ProjStart=c(1998, 2003, 2008))
StoOFL <- SigmaAnalysis(RecType=1, Nproj=100, Nsim=100, ProjType=2, ProjStart=c(1998,2003,2008))

# Save the results to results directory (for plotting and tables)
save(StoSpawnB, file="StoSpawnB.Rdata")
save(StoOFL, file="StoOFL.Rdata")

# StoSpawnB <- SigmaAnalysis(RecType=1, Nproj=1, Nsim=100, ProjType=1, ProjStart=1994:2008)
# StoOFL <- SigmaAnalysis(RecType=1, Nproj=1, Nsim=100, ProjType=2, ProjStart=1994:2008)

# # Save the results to results directory (for plotting and tables)
# save(StoSpawnB, file="StoSpawnB.Rdata")
# save(StoOFL, file="StoOFL.Rdata")

#### Generate Figures 1-6 ####
source("Plots\\Figure 1.R")
source("Plots\\Figure 2.R")
source("Plots\\Figure 2b.R")
source("Plots\\Figure 3.R")
source("Plots\\Figure 4.R")
source("Plots\\Figure 5.R")
source("Plots\\Figure 6.R")

#### Generate Sigma Tables ####
# You will need to update these paths or change the workflow to point to where your simulation results are located
DirnList <- list()
DirnList[[1]] <- "\\R\\Data\\Deterministic Run Nproj25\\"
DirnList[[2]] <- "\\R\\Data\\Deterministic Run Nproj1\\"
DirnList[[3]] <- "\\R\\Data\\Stochastic Run Nproj25\\"
DirnList[[4]] <- "\\R\\Data\\Stochastic Run Nproj1\\"
DirnList[[5]] <- "\\R\\Data\\Stochastic Run Nproj25_0\\"


# Det Nproj25
Sigma_Table(Dirn=DirnList[[1]], ProjStart=c(1998,2003,2008), Det=T)

# Det Nproj1
Sigma_Table(Dirn=DirnList[[2]], ProjStart=1994:2008, Det=T)

# Sto Nproj25
Sigma_Table(Dirn=DirnList[[3]], ProjStart=c(1998,2003,2008), Det=F)

# Sto Nproj1
Sigma_Table(Dirn=DirnList[[4]], ProjStart=1994:2008, Det=F)

# Sto Nproj25_0
Sigma_Table(Dirn=DirnList[[5]], ProjStart=c(1998,2003,2008), Det=F)
