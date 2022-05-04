
pdf("Plots\\Figure 3.pdf", width=3.34646, height=8.97638)
# tiff("Plots\\Figure 2b.tiff", width=85, height=228, units="mm", res=500)
# pdf("Z:\\UW Google Drive\\Thesis Stuff\\SigmaOFL\\R\\Graphs\\3October18\\Figure 2.pdf", height=8.5, width=11)
# Figure 2
load("Data\\Deterministic Run Nproj25\\Deterministic Projections.Rdata")
Stock <- DAT[[1]]$OFL[1,which(DAT[[1]]$OFL[1,]!="")]
Nproj <- 25
diffOFLlim <- T
ProjStart <- c(1998, 2003, 2008)
# col <- c("#000000", "#009E73", "#0072B2", "#CC79A7")
col <- c("#FFC107", "#1E88E5","#D81B60","#004D40")
niter <- 100

#######################################################################################################################################
# Use Stochastic0 to set column ylims
DAT2 <- list()
load("Data\\Stochastic Run Nproj25_0\\1998_StochProjResults.Rdata")
DAT2[[1]] <- ProjRes
load("Data\\Stochastic Run Nproj25_0\\2003_StochProjResults.Rdata")
DAT2[[2]] <- ProjRes
load("Data\\Stochastic Run Nproj25_0\\2008_StochProjResults.Rdata")
DAT2[[3]] <- ProjRes

#######################################################################################################################################

maxYY <- vector(length=3)
	for (yy in 1:3) {
		Index <- which(DAT[[yy]]$OFL[,1]==as.character(Stock[1])) + 1
		nasmt <- as.numeric(DAT[[yy]]$OFL[Index,1])
		if (yy==1) {nlegend <- nasmt}
		startYr <- as.numeric(DAT[[yy]]$OFL[Index,2])
		endYr <- as.numeric(DAT[[yy]]$OFL[Index,3])
		nyr <- length(startYr:endYr)
		maxYY[yy] <- as.numeric(max(as.numeric(unlist(DAT[[yy]]$OFL[(Index+2):(Index+1+nyr),2:(nasmt+1)])), na.rm=T))
	}
maxY <- maxYY


#######################################################################################################################################
par(mfrow=c(3,1), mar=c(0,3,0,0), oma=c(5,6,5,1))
load("Data\\Deterministic Run Nproj25\\Deterministic Projections.Rdata")

maxY <- max(maxYY)*1.15
plot(1, type="n", xlab="", ylab=ylabel, 
		xlim=c((ProjStart[1]-1),(ProjStart[3]+nyr-1+1)), ylim=c(0,maxY),
		xaxs="i", yaxs="i", main=topmain, xaxt="n", yaxt=ytemp,
		cex=1.5, cex.main=1.5, cex.lab=1.5, cex.axis=1.5)

# Deterministic Plot
	for (y in 1:3) {
		Index <- which(DAT[[y]]$OFL[,1]==as.character(Stock[1])) + 1
		nasmt <- as.numeric(DAT[[y]]$OFL[Index,1])
		startYr <- as.numeric(DAT[[y]]$OFL[Index,2])
		endYr <- as.numeric(DAT[[y]]$OFL[Index,3])
		nyr <- length(startYr:endYr)

		topmain <- NULL

		if (y==1) {
			ylabel <- ""
			ytemp <- "n"
		} else {
			ylabel <- ""
			ytemp <- "n"
		}

		# mtext(text=ProjStart[y], side=3, line=1, outer=F, cex=1.5)

		if(y==1) {
			legend(x="topleft", legend=c("Oldest (usable)", "Intermediate", "Most recent"), bty="n", 
			lty=1:3, col=c(col[1:(3-1)], col[4]),
			lwd=c(rep(2, times=(3-1)),2), horiz=F, cex=1)
			tix <- seq(from=0, to=maxY, by=20)
			axis(side=2, at=tix[-1], labels=tix[-1], outer=F, cex.axis=1.5)
			mtext(text="Deterministic", side=2, line=1.5, outer=T, cex=1.5, adj=0.92)

		}

		# if (y==2) {
		# 	tix <- seq(from=0, to=maxY, by=2)
		# 	axis(side=2, at=tix[-1], labels=tix[-1], outer=F, cex.axis=1)
		# }

		# if (y==3) {
		# 	tix <- seq(from=0, to=maxY, by=10)
		# 	axis(side=2, at=tix[-1], labels=tix[-1], outer=F, cex.axis=1)
		# }

		xendasmt <- c()
		for (j in 1:nasmt) {
			yIndex <- which(DAT[[y]]$OFL[(Index+2):(Index+1+nyr),j+1]!="")

			if (j==nasmt) {
				lines(x=ProjStart[y]:(ProjStart[y]+nyr-1),
				col=col[4], 
				y=as.numeric(DAT[[y]]$OFL[Index+1+yIndex,j+1]),
				lwd=2,
				lty=3)

			} else {
				if (nasmt_check > nasmt) {J <- j+1} else {J=j}
				lines(x=ProjStart[y]:(ProjStart[y]+nyr-1),
				col=col[J], 
				y=as.numeric(DAT[[y]]$OFL[Index+1+yIndex,j+1]),
				lwd=2,
				lty=j)

				xendasmt <- c(xendasmt, as.numeric(DAT[[y]]$OFL[Index+1,j+1]) - as.numeric(DAT[[y]]$OFL[Index,2]) + 1)
			}
		}
	}

#######################################################################################################################################

# # Stochastic 0 Plot
maxY2 <- list()
for (yy in 1:3) {
	maxYY <- c()
	DAT <- DAT2[[yy]]
	for (jj in 1:niter) {
		Index <- which(DAT[[jj]]$OFL[,1]==as.character(Stock[1])) + 1
		nasmt <- as.numeric(DAT[[jj]]$OFL[Index,1])
		if (yy==1) {nlegend <- nasmt}
			startYr <- as.numeric(DAT[[jj]]$OFL[Index,2])
			endYr <- as.numeric(DAT[[jj]]$OFL[Index,3])
			nyr <- length(startYr:endYr)
			maxYY <- c(maxYY, as.numeric(max(as.numeric(unlist(DAT[[jj]]$OFL[(Index+2):(Index+1+nyr),2:(nasmt+1)])), na.rm=T)))
		}
	maxY2[[yy]] <- max(maxYY)*1.15
}

plot(1, type="n", xlab="", ylab=ylabel, 
	xlim=c((ProjStart[1]-1), (ProjStart[3]+nyr-1+1)), ylim=c(0,(max(maxY))), xaxt="n",
	yaxt=ytemp, xaxs="i", yaxs="i", main=topmain,
	cex=1.5, cex.main=1.5, cex.lab=1.5, cex.axis=1.5)

	for (y in 1:3) {
		maxY <- maxY2[[y]]
		DAT <- DAT2[[y]]

		if (y==1) {
			ylabel <- ""
			ytemp <- "n"
		} else {
			ylabel <- ""
			ytemp <- "n"
		}

		topmain <- NULL
		test <- list()
		matp <- matrix(ncol=25, nrow=niter)
		test[[1]] <- matp
		test[[2]] <- matp
		test[[3]] <- matp

		for (j in 1:niter) {
			Index <- which(DAT[[j]]$OFL[,1]==as.character(Stock[1])) + 1
			nasmt <- as.numeric(DAT[[j]]$OFL[Index,1])
			startYr <- as.numeric(DAT[[j]]$OFL[Index,2])
			endYr <- as.numeric(DAT[[j]]$OFL[Index,3])
			nyr <- length(startYr:endYr)
			if (y==1) {nasmt_check <- nasmt}
			xendasmt <- c()

			for (k in 1:nasmt) {
				yIndex <- which(DAT[[j]]$OFL[(Index+2):(Index+1+nyr),k+1]!="")
				if (k==nasmt) {
					# lines(x=ProjStart[y]:(ProjStart[y]+nyr-1),
					# col=col[4], 
					# y=as.numeric(DAT[[j]]$OFL[Index+1+yIndex,k+1]),
					# lwd=2,
					# lty=1)

					# xendasmt <- as.numeric(DAT[[j]]$OFL[Index+1,k+1]) - as.numeric(DAT[[j]]$OFL[Index,2]) + 1
					# yendasmt <- as.numeric(DAT[[j]]$OFL[Index+1+yIndex[xendasmt],k+1])

					# points(x=as.numeric(DAT[[j]]$OFL[Index+1,k+1]), y=yendasmt, pch=8, lwd=1)
					# xendasmt <- c(xendasmt, as.numeric(DAT[[j]]$OFL[Index+1,k+1]) - as.numeric(DAT[[j]]$OFL[Index,2]) + 1)

					test[[k]][j,] <- as.numeric(DAT[[j]]$OFL[Index+1+yIndex,k+1])
				} else {
					if (nasmt_check > nasmt) {J <- k+1} else {J=k}
					# lines(x=ProjStart[y]:(ProjStart[y]+nyr-1),
					# col=col[J], 
					# y=as.numeric(DAT[[j]]$OFL[Index+1+yIndex,k+1]),
					# lwd=2,
					# lty=1)

					# xendasmt <- as.numeric(DAT[[j]]$OFL[Index+1,k+1]) - as.numeric(DAT[[j]]$OFL[Index,2]) + 1
					# yendasmt <- as.numeric(DAT[[j]]$OFL[Index+1+yIndex[xendasmt],k+1])

					# points(x=as.numeric(DAT[[j]]$OFL[Index+1,k+1]), y=yendasmt, pch=8, lwd=1)
					# xendasmt <- c(xendasmt, as.numeric(DAT[[j]]$OFL[Index+1,k+1]) - as.numeric(DAT[[j]]$OFL[Index,2]) + 1)
					test[[k]][j,] <- as.numeric(DAT[[j]]$OFL[Index+1+yIndex,k+1])
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

		tix <- seq(from=0, to=(max(Ymax)*1.15), by=20)
		axis(side=2, at=tix[-1], labels=tix[-1], outer=F, cex.axis=1.5)

		if(y==1) {
			tix <- seq(from=0, to=(max(Ymax)*1.15), by=20)
			axis(side=2, at=tix[-1], labels=tix[-1], outer=F, cex.axis=1.5)
			# mtext(text="OFL (mt)", side=2, line=3, outer=T, cex=1.5)
			mtext(text="Stochastic", side=2, line=1.5, outer=T, cex=1.5, adj=0.5)
			mtext(text="(future only)", side=2, line=0, outer=T, cex=1, adj=0.5)

		}

		# if (y==2) {
		# 	tix <- seq(from=0, to=(max(Ymax)*1.15), by=2)
		# 	axis(side=2, at=tix[-1], labels=tix[-1], outer=F, cex.axis=1)
		# 	mtext(text="Projection start year", side=1, line=3.5, outer=T, cex=1.5)
		# }

		# if (y==3) {
		# 	tix <- seq(from=0, to=(max(Ymax)*1.15), by=10)
		# 	axis(side=2, at=tix[-1], labels=tix[-1], outer=F, cex.axis=1)
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

		# abline(v=(startYr:endYr)[xendasmt[1]], col=rgb(red=255,green=193,blue=7, maxColorValue=255), lty=1)
		# abline(v=(startYr:endYr)[xendasmt[2]], col=rgb(red=30, green=136, blue=229, maxColorValue=255), lty=3)
		# abline(v=(startYr:endYr)[xendasmt[3]], col=rgb(red=0, green=77, blue=64, maxColorValue=255), lty=2)
		
		
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

maxY3 <- list()
for (yy in 1:3) {
	# maxY <- maxYLim[y]
	maxYY <- c()
	DAT <- DAT2[[yy]]
	for (jj in 1:niter) {
		Index <- which(DAT[[jj]]$OFL[,1]==as.character(Stock[1])) + 1
		nasmt <- as.numeric(DAT[[jj]]$OFL[Index,1])
		if (yy==1) {nlegend <- nasmt}
			startYr <- as.numeric(DAT[[jj]]$OFL[Index,2])
			endYr <- as.numeric(DAT[[jj]]$OFL[Index,3])
			nyr <- length(startYr:endYr)
			maxYY <- c(maxYY, as.numeric(max(as.numeric(unlist(DAT[[jj]]$OFL[(Index+2):(Index+1+nyr),2:(nasmt+1)])), na.rm=T))*1.15)
		}
		maxY3[[yy]] <- max(maxYY)#*1.15
	}

plot(1, type="n", xlab="", ylab=ylabel, 
		xlim=c((ProjStart[1]-1), (ProjStart[3]+nyr-1+1)), ylim=c(0,115), xaxt="n",
		yaxt=ytemp, xaxs="i", yaxs="i", main=topmain,
		cex=1.5, cex.main=1.5, cex.lab=1.5, cex.axis=1.5)

	tix <- seq(from=(ProjStart[1]-1), to=(ProjStart[3]+nyr-1+1), by=2)
	ntix <- length(tix)
	axis(side=1, at=tix[c(-1,-ntix)], labels=tix[c(-1,-ntix)], outer=T, cex.axis=1.5)
	mtext(text="Year", side=1, line=3.5, outer=T, cex=1.5)

	for (y in 1:3) {
		maxY <- maxY3[[y]]
		DAT <- DAT2[[y]]

		if (y==1) {
			ylabel <- ""
			ytemp <- "n"
		} else {
			ylabel <- ""
			ytemp <- "n"
		}

		if (y==3) {
			xlabel <- ""
		}

		topmain <- NULL
		test <- list()
		matp <- matrix(ncol=25, nrow=niter)
		test[[1]] <- matp
		test[[2]] <- matp
		test[[3]] <- matp

		for (j in 1:niter) {
			Index <- which(DAT[[j]]$OFL[,1]==as.character(Stock[1])) + 1
			nasmt <- as.numeric(DAT[[j]]$OFL[Index,1])
			startYr <- as.numeric(DAT[[j]]$OFL[Index,2])
			endYr <- as.numeric(DAT[[j]]$OFL[Index,3])
			nyr <- length(startYr:endYr)
			if (y==1) {nasmt_check <- nasmt}
			xendasmt <- c()

			for (k in 1:nasmt) {
				yIndex <- which(DAT[[j]]$OFL[(Index+2):(Index+1+nyr),k+1]!="")
				if (k==nasmt) {
					# lines(x=ProjStart[y]:(ProjStart[y]+nyr-1),
					# col=col[4], 
					# y=as.numeric(DAT[[j]]$OFL[Index+1+yIndex,k+1]),
					# lwd=2,
					# lty=1)

					# xendasmt <- c(xendasmt, as.numeric(DAT[[j]]$OFL[Index+1,k+1]) - as.numeric(DAT[[j]]$OFL[Index,2]) + 1)
					# yendasmt <- as.numeric(DAT[[j]]$OFL[Index+1+yIndex[xendasmt],k+1])

					# points(x=as.numeric(DAT[[j]]$OFL[Index+1,k+1]), y=yendasmt, pch=8, lwd=1)
					test[[k]][j,] <- as.numeric(DAT[[j]]$OFL[Index+1+yIndex,k+1])

				} else {
					if (nasmt_check > nasmt) {J <- k+1} else {J=k}
					# lines(x=ProjStart[y]:(ProjStart[y]+nyr-1),
					# col=col[J], 
					# y=as.numeric(DAT[[j]]$OFL[Index+1+yIndex,k+1]),
					# lwd=2,
					# lty=1)

					# xendasmt <- c(xendasmt, as.numeric(DAT[[j]]$OFL[Index+1,k+1]) - as.numeric(DAT[[j]]$OFL[Index,2]) + 1)
					# yendasmt <- as.numeric(DAT[[j]]$OFL[Index+1+yIndex[xendasmt],k+1])

					# points(x=as.numeric(DAT[[j]]$OFL[Index+1,k+1]), y=yendasmt, pch=8, lwd=1)
					test[[k]][j,] <- as.numeric(DAT[[j]]$OFL[Index+1+yIndex,k+1])
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

		# tix <- seq(from=0, to=(max(Ymax)*1.15), by=20)
		# axis(side=2, at=tix[-1], labels=tix[-1], outer=F)

		if(y==1) {
			tix <- seq(from=0, to=(max(Ymax)*1.15), by=20)
			axis(side=2, at=tix[-1], labels=tix[-1], outer=F, cex.axis=1.5)
			mtext(text="OFL (mt)", side=2, line=3.8, outer=T, cex=1.5)
			mtext(text="Stochastic", side=2, line=1.5, outer=T, cex=1.5, adj=0.115)
			mtext(text="(past and future)", side=2, line=0, outer=T, cex=1, adj=0.11)

		}

		# if (y==2) {
		# 	tix <- seq(from=0, to=(max(Ymax)*1.15), by=2)
		# 	axis(side=2, at=tix[-1], labels=tix[-1], outer=F, cex.axis=1)
		# }

		# if (y==3) {
		# 	tix <- seq(from=0, to=(max(Ymax)*1.15), by=10)
		# 	axis(side=2, at=tix[-1], labels=tix[-1], outer=F, cex.axis=1)
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

		# abline(v=(startYr:endYr)[xendasmt[1]], col=rgb(red=255,green=193,blue=7, maxColorValue=255), lty=1)
		# abline(v=(startYr:endYr)[xendasmt[2]], col=rgb(red=30, green=136, blue=229, maxColorValue=255), lty=3)
		# abline(v=(startYr:endYr)[xendasmt[3]], col=rgb(red=0, green=77, blue=64, maxColorValue=255), lty=2)

		
	}

	######################################################################################################################################

dev.off()