Data_Process <- function(Stock=1, NFore=2, Stoch=F, knownR=F, YearRef, PrintAll=FALSE, Dirn) {
	# The directory where the report.sso files for the stocks of interest reside
	# if statement added to change to a directory that does not contain 2009 report files when
	# I'm trying to project starting from 2011.
	# if (YearRef!=2011) {
		Rep_Dir <- paste(Dirn,"Data\\ALL REPORT FILES\\",sep="")
	# } else {
		# Rep_Dir <- paste(Dirn,"REPORT FILES\\",sep="")
	# }
	
	# Which assessments are available for analysis?
	Assessments <- dir(Rep_Dir)

	# How many assessments are there?
	nAsmt <- length(Assessments)
	# For which stocks do we have reports?
	Stocks <- unique(substr(Assessments, start=1, stop=4))
	nStock <- length(Stocks)

	# Summary is a table listing the Fishery Management Plan, Stock name, TargetSPR value, and FileName
	# for the stocks of interest.
	Summary <- matrix(nrow=nAsmt, ncol=13)
	colnames(Summary) <- c("FMP", "Stock", "TargetSPR", "Assessment", "SSB0", "SSB0 Calc:SSB0 Report", "SSB/F0", "TargeTSPR", "Fmult(Target)", "SPRFT:SPRF0", "TargetBio Calc", "CatchPerRecruit*RecDepl", "Error Check")
	Summary[,2] <- substr(Assessments, start=1, stop=4)
	Summary[,4] <- Assessments
	# Output is a list of Spawning biomass and OFL projections produced for each stock of interest
	Output <- list()

	for(i in Stock:nAsmt) {
		# print(i)
		# These if else statements assign the appropriate TargetSPR for rockfish, roundfish, and flatfish
		# KPJ needs to look up/add in TargetSPR for pelagics if they are not 0.50
		if(substr(Assessments[i], start=1, stop=4)=="SABL" | substr(Assessments[i], start=1, stop=3)=="LIN") {
			Summary[i,3] <- 0.45
		} else {
			if(substr(Assessments[i], start=1, stop=4)=="PETR" | substr(Assessments[i], start=1, stop=4)=="DOVR") {
				Summary[i,3] <- 0.30
				# if (Assessments[i]=="PETR 2009.sso") {
				# 	Summary[i,3] <- 0.4
				# }
			} else {
				Summary[i,3] <- 0.50
		}
		}
		# This if else statement assigns the Fishery Management Plan the stocks of interest fall under.
		# For personal reference.
		if(substr(Assessments[i], start=1, stop=4)=="SARD" | substr(Assessments[i], start=1, stop=4)=="MACK") {
			Summary[i,1] <- "CPS"
		} else {
			Summary[i,1] <- "Groundfish"
		}

		# The number of years to calculate and project over.
		YrtoPresent <- ((as.numeric(substr(Assessments[i], start=5, stop=9)))-YearRef)+1
		# AEP says all projections might as well end in 2025.
		# FinishLine <- 2025-(YearRef+NFore)+1
		FinishLine <- 0

		# Populates the list with the projection values for each available stock assessment in the Rep_Dir
		if(PrintAll==TRUE) {
			# This will help keep track of which assessment report generates any errors or warnings.
			cat(paste("Now projecting:", i, Summary[i,4]), "\n")
			Output[[substr(Assessments[i],start=1,stop=9)]] <- Process(FileName=paste(Rep_Dir, Summary[i,4], sep=""), 
							   YearRef=YearRef, 
							   TargetSPR=as.numeric(Summary[i,3]), 
							   Nyear=NFore+FinishLine,
							   Stoch=Stoch, knownR=knownR)
		} else {
			Diagnostics <- capture.output(Output[[substr(Assessments[i],start=1,stop=9)]] <- Process(FileName=paste(Rep_Dir, Summary[i,4], sep=""), 
							   YearRef=YearRef, 
							   TargetSPR=as.numeric(Summary[i,3]), 
							   Nyear=NFore+FinishLine,
							   Stoch=Stoch, knownR=knownR))

			diag <- strsplit(Diagnostics[2][[1]], " ")
			Summary[i,5:12] <- diag[[1]][1:8]

			ifelse(length(Diagnostics)==3, Summary[i,13] <- Diagnostics[[3]], Summary[i,13] <- NA)
			}
	}
 
	if (PrintAll==FALSE) {
		if(Stoch==FALSE) {
			cat("\n")
			print("A Projection Diagnostics file has been produced.")
			write.csv(Summary, file=paste(YearRef,"Deter_Projection Diagnostics.csv", sep=""))
			cat("\n")
		}
	}
return(Output)
}