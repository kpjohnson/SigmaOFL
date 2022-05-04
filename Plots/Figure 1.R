
#######################################################################################################################################
pdf("Plots\\Figure 2.pdf", width=6.69291, height=5)
# tiff("Plots\\Figure 1.tiff", width=170, height=127, units="mm", res=500)
# pdf("Z:\\UW Google Drive\\Thesis Stuff\\SigmaOFL\\R\\Graphs\\17October18\\Figure 1.pdf", height=8.5, width=11)
# Figure 1
load("Data\\Deterministic Run Nproj25\\Deterministic Projections.Rdata")
Stock <- DAT[[1]]$SpawnB[1,which(DAT[[1]]$SpawnB[1,]!="")]
Nproj <- 25
diffOFLlim <- F
ProjStart <- c(1998, 2003, 2008)
# col <- c("#000000", "#009E73", "#0072B2", "#CC79A7")
col <- c("#FFC107", "#1E88E5","#D81B60","#004D40")
niter <- 100

#######################################################################################################################################
# Column ylims based on Stochastic0
DAT2 <- list()
load("Data\\Stochastic Run Nproj25_0\\1998_StochProjResults.Rdata")
DAT2[[1]] <- ProjRes
load("Data\\Stochastic Run Nproj25_0\\2003_StochProjResults.Rdata")
DAT2[[2]] <- ProjRes
load("Data\\Stochastic Run Nproj25_0\\2008_StochProjResults.Rdata")
DAT2[[3]] <- ProjRes

maxYBLim <- vector(length=3)

for (yy in 1:3) {
	maxYYB <- NULL
	DAT <- DAT2[[yy]]
	for (jj in 1:niter) {
		Index <- which(DAT[[jj]]$SpawnB[,1]==as.character(Stock[1])) + 1
		nasmt <- as.numeric(DAT[[jj]]$SpawnB[Index,1])
		if (yy==1) {nlegend <- nasmt}
			Index <- which(DAT[[jj]]$SpawnB[,1]==as.character(Stock[1])) + 1
			nasmt <- as.numeric(DAT[[jj]]$SpawnB[Index,1])
			startYr <- as.numeric(DAT[[jj]]$SpawnB[Index,2])
			endYr <- as.numeric(DAT[[jj]]$SpawnB[Index,3])
			nyr <- length(startYr:endYr)
			maxYYB <- c(maxYYB, as.numeric(max(as.numeric(unlist(DAT[[jj]]$SpawnB[(Index+2):(Index+1+nyr),2:(nasmt+1)])), na.rm=T)))
		}
		maxYBLim[yy] <- max(maxYYB)*1.1
	}

#######################################################################################################################################
par(mfrow=c(3,3), mar=c(0,0,0,0), oma=c(5,7,3,2))
load("Data\\Deterministic Run Nproj25\\Deterministic Projections.Rdata")

# Deterministic Plot
	for (y in 1:3) {
		maxY <- maxYBLim[y] 
		Index <- which(DAT[[y]]$SpawnB[,1]==as.character(Stock[1])) + 1
		nasmt <- as.numeric(DAT[[y]]$SpawnB[Index,1])
		startYr <- as.numeric(DAT[[y]]$SpawnB[Index,2])
		endYr <- as.numeric(DAT[[y]]$SpawnB[Index,3])
		nyr <- length(startYr:endYr)
		if (y==1) {nasmt_check <- nasmt}

		topmain <- NULL

		if (y==1) {
			ylabel <- ""
			ytemp <- "n"
		} else {
			ylabel <- ""
			ytemp <- "n"
		}

		plot(1, type="n", xlab="", ylab=ylabel, 
			xlim=c((ProjStart[y]),(ProjStart[y]+nyr-1)), ylim=c(3000,25000),
			xaxs="i", yaxs="i", main=topmain, xaxt="n", yaxt=ytemp, cex=1.5,
			cex.lab=1.5, cex.axis=1.2, cex.main=1.5)

		mtext(text=ProjStart[y], side=3, line=1, outer=F, cex=1.5)

		if(y==1) {
			tix <- seq(from=0, to=25000, by=5000)
			ntix <- length(tix)
			axis(side=2, at=tix[c(-1,-ntix)], labels=tix[c(-1,-ntix)], outer=T, cex.axis=1.5)
			legend(x="topleft", legend=c("Oldest (usable)", "Intermediate", "Most recent"), bty="n", 
			lty=1:3, col=c(col[1:(3-1)], col[4]),
			lwd=c(rep(2, times=(3-1)),2), horiz=F, cex=1.15)
			mtext(text="Deterministic", side=2, line=3.7, outer=T, cex=1, adj=0.93)
		}

		for (j in 1:nasmt) {
			yIndex <- which(DAT[[y]]$SpawnB[(Index+2):(Index+1+nyr),j+1]!="")

			if (j==nasmt) {
				lines(x=ProjStart[y]:(ProjStart[y]+nyr-1),
				col=col[4], 
				y=as.numeric(DAT[[y]]$SpawnB[Index+1+yIndex,j+1]),
				lwd=2,
				lty=3)

				# xendasmt <- as.numeric(DAT[[y]]$SpawnB[Index+1,j+1]) - as.numeric(DAT[[y]]$SpawnB[Index,2]) + 1
				# yendasmt <- as.numeric(DAT[[y]]$SpawnB[Index+1+yIndex[xendasmt],j+1])

				# points(x=as.numeric(DAT[[y]]$SpawnB[Index+1,j+1]), y=yendasmt, pch=8, lwd=1)

			} else {
				if (nasmt_check > nasmt) {J <- j+1} else {J=j}
				lines(x=ProjStart[y]:(ProjStart[y]+nyr-1),
				col=col[J], 
				y=as.numeric(DAT[[y]]$SpawnB[Index+1+yIndex,j+1]),
				lwd=2,
				lty=j)

				# xendasmt <- as.numeric(DAT[[y]]$SpawnB[Index+1,j+1]) - as.numeric(DAT[[y]]$SpawnB[Index,2]) + 1
				# yendasmt <- as.numeric(DAT[[y]]$SpawnB[Index+1+yIndex[xendasmt],j+1])

				# points(x=as.numeric(DAT[[y]]$SpawnB[Index+1,j+1]), y=yendasmt, pch=8, lwd=1)
			}
		}
	}

#######################################################################################################################################

# Stochastic 0 Plot
	for (y in 1:3) {
		maxY <- maxYBLim[3] 
		DAT <- DAT2[[y]]

		if (y==1) {
			ylabel <- "Spawning Biomass (mt)"
			ytemp <- "n"
		} else {
			ylabel <- ""
			ytemp <- "n"
		}

		if (y==3) {

		}

		topmain <- NULL

		# plot(1, type="n", xlab="", ylab=ylabel, 
		# 	xlim=c((ProjStart[y]-1), (ProjStart[y]+nyr-1+1)), ylim=c(0,maxY), xaxt="n", yaxt=ytemp,
		# 	xaxs="i", yaxs="i", main=topmain, cex=1.5, cex.lab=1.5, cex.axis=1.5)
		# if(y==1) {
		# 	tix <- seq(from=0, to=maxY, by=10000)
		# 	axis(side=2, at=tix[-1], labels=tix[-1], outer=T)
		# 	mtext(text="Spawning Biomass (mt)", side=2, line=3, outer=T)
		# }

		test <- list()
		matp <- matrix(ncol=25, nrow=niter)
		test[[1]] <- matp
		test[[2]] <- matp
		test[[3]] <- matp

		for (j in 1:niter) {
			Index <- which(DAT[[j]]$SpawnB[,1]==as.character(Stock[1])) + 1
			nasmt <- as.numeric(DAT[[j]]$SpawnB[Index,1])
			startYr <- as.numeric(DAT[[j]]$SpawnB[Index,2])
			endYr <- as.numeric(DAT[[j]]$SpawnB[Index,3])
			nyr <- length(startYr:endYr)
			if (y==1) {nasmt_check <- nasmt}


			for (k in 1:nasmt) {
				yIndex <- which(DAT[[j]]$SpawnB[(Index+2):(Index+1+nyr),k+1]!="")
				if (k==nasmt) {
					# lines(x=ProjStart[y]:(ProjStart[y]+nyr-1),
					# col=col[4], 
					# y=as.numeric(DAT[[j]]$SpawnB[Index+1+yIndex,k+1]),
					# lwd=2,
					# lty=1)

					# xendasmt <- as.numeric(DAT[[j]]$SpawnB[Index+1,k+1]) - as.numeric(DAT[[j]]$SpawnB[Index,2]) + 1
					# yendasmt <- as.numeric(DAT[[j]]$SpawnB[Index+1+yIndex[xendasmt],k+1])

					# points(x=as.numeric(DAT[[j]]$SpawnB[Index+1,k+1]), y=yendasmt, pch=8, lwd=1)
					test[[k]][j,] <- as.numeric(DAT[[j]]$SpawnB[Index+1+yIndex,k+1])

				} else {
					if (nasmt_check > nasmt) {J <- k+1} else {J=k}
					# lines(x=ProjStart[y]:(ProjStart[y]+nyr-1),
					# col=col[J], 
					# y=as.numeric(DAT[[j]]$SpawnB[Index+1+yIndex,k+1]),
					# lwd=2,
					# lty=1)

					# xendasmt <- as.numeric(DAT[[j]]$SpawnB[Index+1,k+1]) - as.numeric(DAT[[j]]$SpawnB[Index,2]) + 1
					# yendasmt <- as.numeric(DAT[[j]]$SpawnB[Index+1+yIndex[xendasmt],k+1])

					# points(x=as.numeric(DAT[[j]]$SpawnB[Index+1,k+1]), y=yendasmt, pch=8, lwd=1)
					test[[k]][j,] <- as.numeric(DAT[[j]]$SpawnB[Index+1+yIndex,k+1])
				}
			}
		}

		answer <- list()
		Ymax <- c()
		for (a in 1:3) {
			answer[[a]] <- matrix(nrow=25, ncol=2)
			answer[[a]][,2] <- apply(X=test[[a]], MARGIN=2, FUN=median) + (qnorm(0.975) * apply(X=test[[a]], MARGIN=2, FUN=sd)/sqrt(100))
			answer[[a]][,1] <- apply(X=test[[a]], MARGIN=2, FUN=median) - (qnorm(0.975) * apply(X=test[[a]], MARGIN=2, FUN=sd)/sqrt(100))
			Ymax <- c(Ymax, max(answer[[a]]))
		}

		plot(1, type="n", xlab="", ylab=ylabel, 
			xlim=c((ProjStart[y]), (ProjStart[y]+nyr-1)), ylim=c(3000,(max(Ymax)*1.1)), xaxt="n",
			yaxt=ytemp, xaxs="i", yaxs="i", main=topmain,
			cex=1.5, cex.main=1.5, cex.lab=1.5, cex.axis=1.5)

		# tix <- seq(from=0, to=(max(Ymax)*1.15), by=20)
		# axis(side=2, at=tix[-1], labels=tix[-1], outer=F)

		if(y==1) {
			tix <- seq(from=0, to=(max(Ymax)*1.1), by=5000)
			axis(side=2, at=tix[-1], labels=tix[-1], outer=F, cex.axis=1.5)
			mtext(text="Spawning Biomass (mt)", side=2, line=5.5, outer=T, cex=1.1)
			mtext(text="Stochastic", side=2, line=3.7, outer=T, cex=1, adj=0.5)
			mtext(text="(future only)", side=2, line=2.5, outer=T, cex=1, adj=0.5)

		}

		# if (y==2) {
		# 	tix <- seq(from=0, to=(max(Ymax)*1.15), by=2)
		# 	axis(side=2, at=tix[-1], labels=tix[-1], outer=F)
		# 	mtext(text="Projection start year", side=1, line=3, outer=T)
		# }

		# if (y==3) {
		# 	tix <- seq(from=0, to=(max(Ymax)*1.15), by=10)
		# 	axis(side=2, at=tix[-1], labels=tix[-1], outer=F)
		# }

		polygon(x=c((ProjStart[y]):(ProjStart[y]+nyr-1), rev((ProjStart[y]):(ProjStart[y]+nyr-1))),
			y=c(answer[[3]][,2], rev(answer[[3]][,1])), border=NA,
			col=rgb(red=0, green=77, blue=64, alpha=200, maxColorValue = 255))

		lines(x=c((ProjStart[y]):(ProjStart[y]+nyr-1)),
			y=apply(X=test[[3]], MARGIN=2, FUN=median),
			col=col[4], lwd=2, lty=2)

		polygon(x=c((ProjStart[y]):(ProjStart[y]+nyr-1), rev((ProjStart[y]):(ProjStart[y]+nyr-1))),
			y=c(answer[[2]][,2], rev(answer[[2]][,1])), border=NA,
			col=rgb(red=30, green=136, blue=229, alpha=200, maxColorValue=255))

		lines(x=c((ProjStart[y]):(ProjStart[y]+nyr-1)),
			y=apply(X=test[[2]], MARGIN=2, FUN=median),
			col=col[2], lwd=2, lty=3)

		polygon(x=c((ProjStart[y]):(ProjStart[y]+nyr-1), rev((ProjStart[y]):(ProjStart[y]+nyr-1))),
			y=c(answer[[1]][,2], rev(answer[[1]][,1])), border=NA,
			col=rgb(red=255,green=193,blue=7,alpha=200, maxColorValue=255))

		lines(x=c((ProjStart[y]):(ProjStart[y]+nyr-1)),
			y=apply(X=test[[1]], MARGIN=2, FUN=median),
			col=col[1], lwd=2, lty=1)
	}

#######################################################################################################################################

# Stochastic Plot
DAT2 <- list()
load("Data\\Stochastic Run Nproj25\\1998_StochProjResults.Rdata")
DAT2[[1]] <- ProjRes
load("Data\\Stochastic Run Nproj25\\2003_StochProjResults.Rdata")
DAT2[[2]] <- ProjRes
load("Data\\Stochastic Run Nproj25\\2008_StochProjResults.Rdata")
DAT2[[3]] <- ProjRes

	for (y in 1:3) {
		maxY <- maxYBLim[3] 
		DAT <- DAT2[[y]]

		if (y==1) {
			ylabel <- "Spawning Biomass (mt)"
			ytemp <- "n"
		} else {
			ylabel <- ""
			ytemp <- "n"
		}

		if (y==3) {

		}

		topmain <- NULL

		# plot(1, type="n", xlab="", ylab=ylabel, 
		# 	xlim=c((ProjStart[y]-1), (ProjStart[y]+nyr-1+1)), ylim=c(0,maxY), xaxt="n", yaxt=ytemp,
		# 	xaxs="i", yaxs="i", main=topmain, cex=1.5, cex.lab=1.5, cex.axis=1.5)
		# if(y==1) {
		# 	tix <- seq(from=0, to=maxY, by=10000)
		# 	axis(side=2, at=tix[-1], labels=tix[-1], outer=T)
		# 	mtext(text="Spawning Biomass (mt)", side=2, line=3, outer=T)
		# }

		test <- list()
		matp <- matrix(ncol=25, nrow=niter)
		test[[1]] <- matp
		test[[2]] <- matp
		test[[3]] <- matp

		for (j in 1:niter) {
			Index <- which(DAT[[j]]$SpawnB[,1]==as.character(Stock[1])) + 1
			nasmt <- as.numeric(DAT[[j]]$SpawnB[Index,1])
			startYr <- as.numeric(DAT[[j]]$SpawnB[Index,2])
			endYr <- as.numeric(DAT[[j]]$SpawnB[Index,3])
			nyr <- length(startYr:endYr)
			if (y==1) {nasmt_check <- nasmt}


			for (k in 1:nasmt) {
				yIndex <- which(DAT[[j]]$SpawnB[(Index+2):(Index+1+nyr),k+1]!="")
				if (k==nasmt) {
					# lines(x=ProjStart[y]:(ProjStart[y]+nyr-1),
					# col=col[4], 
					# y=as.numeric(DAT[[j]]$SpawnB[Index+1+yIndex,k+1]),
					# lwd=2,
					# lty=1)

					# xendasmt <- as.numeric(DAT[[j]]$SpawnB[Index+1,k+1]) - as.numeric(DAT[[j]]$SpawnB[Index,2]) + 1
					# yendasmt <- as.numeric(DAT[[j]]$SpawnB[Index+1+yIndex[xendasmt],k+1])

					# points(x=as.numeric(DAT[[j]]$SpawnB[Index+1,k+1]), y=yendasmt, pch=8, lwd=1)
					test[[k]][j,] <- as.numeric(DAT[[j]]$SpawnB[Index+1+yIndex,k+1])

				} else {
					if (nasmt_check > nasmt) {J <- k+1} else {J=k}
					# lines(x=ProjStart[y]:(ProjStart[y]+nyr-1),
					# col=col[J], 
					# y=as.numeric(DAT[[j]]$SpawnB[Index+1+yIndex,k+1]),
					# lwd=2,
					# lty=1)

					# xendasmt <- as.numeric(DAT[[j]]$SpawnB[Index+1,k+1]) - as.numeric(DAT[[j]]$SpawnB[Index,2]) + 1
					# yendasmt <- as.numeric(DAT[[j]]$SpawnB[Index+1+yIndex[xendasmt],k+1])

					# points(x=as.numeric(DAT[[j]]$SpawnB[Index+1,k+1]), y=yendasmt, pch=8, lwd=1)
					test[[k]][j,] <- as.numeric(DAT[[j]]$SpawnB[Index+1+yIndex,k+1])
				}
			}
		}

		answer <- list()
		Ymax <- c()
		for (a in 1:3) {
			answer[[a]] <- matrix(nrow=25, ncol=2)
			answer[[a]][,2] <- apply(X=test[[a]], MARGIN=2, FUN=median) + (qnorm(0.975) * apply(X=test[[a]], MARGIN=2, FUN=sd)/sqrt(100))
			answer[[a]][,1] <- apply(X=test[[a]], MARGIN=2, FUN=median) - (qnorm(0.975) * apply(X=test[[a]], MARGIN=2, FUN=sd)/sqrt(100))
			Ymax <- c(Ymax, max(answer[[a]]))
		}

		plot(1, type="n", xlab="", ylab=ylabel, 
			xlim=c((ProjStart[y]), (ProjStart[y]+nyr-1)), ylim=c(3000,(max(Ymax)*1.1)), xaxt="n",
			yaxt=ytemp, xaxs="i", yaxs="i", main=topmain,
			cex=1.5, cex.main=1.5, cex.lab=1.5, cex.axis=1.5)

		tix <- seq(from=(ProjStart[y]-1), to=(ProjStart[y]+nyr-1+1), by=2)
		ntix <- length(tix)
		axis(side=1, at=tix[c(-1,-ntix)], labels=tix[c(-1,-ntix)], outer=T, cex.axis=1.5)

		if(y==1) {
			tix <- seq(from=0, to=(max(Ymax)*1.1), by=5000)
			axis(side=2, at=tix[-1], labels=tix[-1], outer=F, cex.axis=1.5)
			# mtext(text="Spawning Biomass (mt)", side=2, line=3, outer=T, cex=1.5)
			mtext(text="Stochastic", side=2, line=3.7, outer=T, cex=1, adj=0.075)
			mtext(text="(past and future)", side=2, line=2.5, outer=T, cex=1, adj=0)

		}

		if (y==2) {
			# tix <- seq(from=0, to=(max(Ymax)*1.15), by=2)
			# axis(side=2, at=tix[-1], labels=tix[-1], outer=F)
			mtext(text="Year", side=1, line=3.5, outer=T, cex=1.5)
		}

		# if (y==3) {
		# 	tix <- seq(from=0, to=(max(Ymax)*1.15), by=10)
		# 	axis(side=2, at=tix[-1], labels=tix[-1], outer=F)
		# }

		polygon(x=c((ProjStart[y]):(ProjStart[y]+nyr-1), rev((ProjStart[y]):(ProjStart[y]+nyr-1))),
			y=c(answer[[3]][,2], rev(answer[[3]][,1])), border=NA,
			col=rgb(red=0, green=77, blue=64, alpha=200, maxColorValue = 255))

		lines(x=c((ProjStart[y]):(ProjStart[y]+nyr-1)),
			y=apply(X=test[[3]], MARGIN=2, FUN=median),
			col=col[4], lwd=2, lty=2)

		polygon(x=c((ProjStart[y]):(ProjStart[y]+nyr-1), rev((ProjStart[y]):(ProjStart[y]+nyr-1))),
			y=c(answer[[2]][,2], rev(answer[[2]][,1])), border=NA,
			col=rgb(red=30, green=136, blue=229, alpha=200, maxColorValue=255))

		lines(x=c((ProjStart[y]):(ProjStart[y]+nyr-1)),
			y=apply(X=test[[2]], MARGIN=2, FUN=median),
			col=col[2], lwd=2, lty=3)

		polygon(x=c((ProjStart[y]):(ProjStart[y]+nyr-1), rev((ProjStart[y]):(ProjStart[y]+nyr-1))),
			y=c(answer[[1]][,2], rev(answer[[1]][,1])), border=NA,
			col=rgb(red=255,green=193,blue=7,alpha=200, maxColorValue=255))

		lines(x=c((ProjStart[y]):(ProjStart[y]+nyr-1)),
			y=apply(X=test[[1]], MARGIN=2, FUN=median),
			col=col[1], lwd=2, lty=1)
	}

#######################################################################################################################################

dev.off()