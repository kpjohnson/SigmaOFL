Data_Format <- function(Nfore=1,  Stoch=F, knownR=F, YearRef, Dirn, PrintAll=FALSE) {
	# Create a list of projections for all available stock assessment reports.
	ncast <- Nfore	# Number of years to forecast
	# print("here")
	Projections <- Data_Process(Stock=1, NFore=ncast, Dirn=Dirn, Stoch=Stoch, knownR=knownR, YearRef=YearRef, PrintAll=PrintAll)
	# AAA
	# Which assessments are available?
	Asmt <- names(Projections)
	# What are the stocks of interest and how many are there?
	Stock <- unique(substr(Asmt, start=1, stop=4))
	nStock <- length(Stock)
	# Set up the data frame for export to .csv
	TheD <- list()
	TheD$SpawnB <- as.data.frame(matrix(NA, nrow=0, ncol=20))
	TheD$OFL <- as.data.frame(matrix(NA, nrow=0, ncol=20))
	LingD <- list()
	LingD$SpawnB <- as.data.frame(matrix(NA, nrow=0, ncol=20))
	LingD$OFL <- as.data.frame(matrix(NA, nrow=0, ncol=20))
	TempLing <- list()
	TempLing$SpawnB <- as.data.frame(matrix(0, nrow=ncast, ncol=sum(grepl("LIN", Stock))))
	TempLing$OFL <- as.data.frame(matrix(0, nrow=ncast, ncol=sum(grepl("LIN", Stock))))
	Lingcount <- 0

	# For each stock available, format the projections so they can be processed later.
	for (t in 1:2) {
		for (i in 1:nStock) {
			if (grepl("LIN", Stock[i])) {
				Lingcount <- Lingcount + 1
				# Which elements in the list of projections are assessments of the stock of interest?
				where.r.they <- which(substr(Asmt, start=1, stop=4)==substr(Stock[i], start=1, stop=4))
				
				if (Lingcount==1) {
					startrow <- nrow(LingD[[t]]) + 1
					# Stock identifier
					LingD[[t]][startrow,1] <- "LING"
					# The number of assessments available for this stock.
					LingD[[t]][startrow+1,1] <- length(where.r.they)
					# First year of data.
					LingD[[t]][startrow+1,2] <- Projections[[Asmt[where.r.they[1]]]]$Year[1]
					# Last year of data (Forecast most likely).
					# end <- 2025 - as.numeric(substr(Asmt[tail(where.r.they, n=1)], start=5, stop=9))
					end <- Nfore-1
					LingD[[t]][startrow+1,3] <- Projections[[Asmt[where.r.they[1]]]]$Year[1] + end
					# Row of assessment year headers.
					LingD[[t]][startrow+2,1:(length(where.r.they)+1)] <- c("", as.numeric(substr(Asmt[where.r.they], start=5, stop=9)))
					# The time series of interest.
					LingD[[t]][(startrow+3):(startrow+2+length(Projections[[Asmt[tail(where.r.they,n=1)]]]$Year)),1] <- Projections[[Asmt[tail(where.r.they,n=1)]]]$Year

					for (a in 1:length(where.r.they)) {
						TempLing[[t]][,a] <- TempLing[[t]][,a] + as.numeric(Projections[[Asmt[where.r.they[a]]]][[t+1]])
					}
				} else {
					# For each assessment available for this stock, add a column of the projected values.
					for (a in 1:length(where.r.they)) {
						TempLing[[t]][,a] <- TempLing[[t]][,a] + as.numeric(Projections[[Asmt[where.r.they[a]]]][[t+1]])
					}
				}
			} else {
				# Store the name of the stock
				TheD[[t]][1,1] <- ""
				nstk <- length(which(TheD[[t]][1,]!="NA")) 
				TheD[[t]][1,nstk+1] <- Stock[i]
				TheD[[t]][2,i] <- ""
				# A row counter
				startrow <- nrow(TheD[[t]]) + 1
				# Which elements in the list of projections are assessments of the stock of interest?
				where.r.they <- which(substr(Asmt, start=1, stop=4)==substr(Stock[i], start=1, stop=4))
				# Stock identifier
				TheD[[t]][startrow,1] <- Stock[i]
				# The number of assessments available for this stock.
				TheD[[t]][startrow+1,1] <- length(where.r.they)
				# First year of data.
				TheD[[t]][startrow+1,2] <- Projections[[Asmt[where.r.they[1]]]]$Year[1]
				# Last year of data (Forecast most likely).
				# end <- 2025 - as.numeric(substr(Asmt[tail(where.r.they, n=1)], start=5, stop=9))
				end <- Nfore-1
				TheD[[t]][startrow+1,3] <- Projections[[Asmt[where.r.they[1]]]]$Year[1] + end
				# Row of assessment year headers.
				TheD[[t]][startrow+2,1:(length(where.r.they)+1)] <- c("", as.numeric(substr(Asmt[where.r.they], start=5, stop=9)))
				# The time series of interest.
				TheD[[t]][(startrow+3):(startrow+2+length(Projections[[Asmt[tail(where.r.they,n=1)]]]$Year)),1] <- Projections[[Asmt[tail(where.r.they,n=1)]]]$Year

				# For each assessment available for this stock, add a column of the projected values.
				for (a in 1:length(where.r.they)) {
					TheD[[t]][(startrow+3):(startrow+2+length(Projections[[Asmt[where.r.they[a]]]][[t]])),(1+a)] <- Projections[[Asmt[where.r.they[a]]]][[t+1]]
				}

			}
		}
		# Store the name of the stock
		nstk <- length(which(TheD[[t]][1,]!="NA")) 
		TheD[[t]][1,nstk+1] <- "LING"
		# A row counter
		startrow <- nrow(TheD[[t]]) + 1
		# Append Lingcod header data
		TheD[[t]][startrow:(startrow+nrow(LingD[[1]])-1),] <- LingD[[1]]
		# Append Lingcod projections
		TheD[[t]][(startrow+3):(startrow+nrow(LingD[[1]])-1),2:(ncol(TempLing[[t]])+1)] <- TempLing[[t]]

	}
	# Makes the csv cleaner in appearance
	TheD[is.na(TheD)] <- ""
	return(TheD)
}