Multi_Graph_Proj <- function(DAT, ProjStart, col=c("#000000", "#009E73", "#0072B2", "#CC79A7"), diffOFLlim=TRUE) {
	# Pull the vector of stock names
	Stock <- DAT[[1]]$OFL[1,which(DAT[[1]]$OFL[1,]!="")]
	nStock <- length(Stock)
	Start <- ProjStart
	ny <- length(ProjStart)
	
	for (i in 1:nStock) {
		maxYY <- vector(length=ny)
		for (yy in 1:ny) {
			Index <- which(DAT[[yy]]$OFL[,1]==as.character(Stock[i])) + 1
			nasmt <- as.numeric(DAT[[yy]]$OFL[Index,1])
			if (yy==1) {nlegend <- nasmt}
			startYr <- as.numeric(DAT[[yy]]$OFL[Index,2])
			endYr <- as.numeric(DAT[[yy]]$OFL[Index,3])
			nyr <- length(startYr:endYr)
			maxYY[yy] <- as.numeric(max(as.numeric(unlist(DAT[[yy]]$OFL[(Index+2):(Index+1+nyr),2:(nasmt+1)])), na.rm=T))*1.15
		}
		maxY <- max(maxYY)

		maxYYB <- vector(length=ny)
		for (zz in 1:ny) {
				Index <- which(DAT[[zz]]$SpawnB[,1]==as.character(Stock[i])) + 1
				nasmt <- as.numeric(DAT[[zz]]$SpawnB[Index,1])
				startYr <- as.numeric(DAT[[zz]]$SpawnB[Index,2])
				endYr <- as.numeric(DAT[[zz]]$SpawnB[Index,3])
				nyr <- length(startYr:endYr)
				maxYYB[zz] <- as.numeric(max(as.numeric(unlist(DAT[[zz]]$SpawnB[(Index+2):(Index+1+nyr),2:(nasmt+1)])), na.rm=T))*1.15
		}
		maxYB <- max(maxYYB)

		if (i%%2==0) {
			plot(1, type="n", axes=FALSE, xlab="", ylab="")
			text(1,1, labels=Stock[i])
		} else {
			plot(1, type="n", axes=FALSE, xlab="", ylab="")
			text(1,1, labels=Stock[i])
		}
		# tiff("SingleSpp_exampleProj2.tiff", width=8.5, height=5, units="in", res=300)
		# par(mfrow=c(1,3))
		for (y in 1:ny) {
			Index <- which(DAT[[y]]$OFL[,1]==as.character(Stock[i])) + 1
			nasmt <- as.numeric(DAT[[y]]$OFL[Index,1])
			startYr <- as.numeric(DAT[[y]]$OFL[Index,2])
			endYr <- as.numeric(DAT[[y]]$OFL[Index,3])
			nyr <- length(startYr:endYr)
			if (y==1) {nasmt_check <- nasmt}

			if (i%%2!=0) {
				topmain <- Start[y]
			} else {
				topmain <- NULL
			}

			if (y==1) {
				ylabel <- "OFL"
			} else {
				ylabel <- ""
			}

			if (i%%2==0) {
				# par(mar=c(5, 5, 2, 2))
			} else {
				par(mar=c(0.5, 5, 2, 2))
			}

			if (diffOFLlim) {
				maxY <- maxYY[y]
			}
			# tiff("SingleSpp_exampleProj.tiff", width=7, height=5, units="in", res=300)
			plot(1, type="n", xlab="", ylab=ylabel, 
				xlim=c(ProjStart[y],(ProjStart[y]+nyr-1)), ylim=c(0,maxY), xaxt="n",
				xaxs="i", yaxs="i", main=topmain, bty="l", cex=1.5, cex.main=1.5, cex.lab=1.5, cex.axis=1.5)

			for (j in 1:nasmt) {
				yIndex <- which(DAT[[y]]$OFL[(Index+2):(Index+1+nyr),j+1]!="")

				if (j==nasmt) {
					lines(x=ProjStart[y]:(ProjStart[y]+nyr-1),
					col=col[4], 
					y=as.numeric(DAT[[y]]$OFL[Index+1+yIndex,j+1]),
					lwd=2,
					lty=1)

					xendasmt <- as.numeric(DAT[[y]]$OFL[Index+1,j+1]) - as.numeric(DAT[[y]]$OFL[Index,2]) + 1
					yendasmt <- as.numeric(DAT[[y]]$OFL[Index+1+yIndex[xendasmt],j+1])

					points(x=as.numeric(DAT[[y]]$OFL[Index+1,j+1]), y=yendasmt, pch=8, lwd=1)

				} else {
					if (nasmt_check > nasmt) {J <- j+1} else {J=j}
					lines(x=ProjStart[y]:(ProjStart[y]+nyr-1),
					col=col[J], 
					y=as.numeric(DAT[[y]]$OFL[Index+1+yIndex,j+1]),
					lwd=2,
					lty=1)

					xendasmt <- as.numeric(DAT[[y]]$OFL[Index+1,j+1]) - as.numeric(DAT[[y]]$OFL[Index,2]) + 1
					yendasmt <- as.numeric(DAT[[y]]$OFL[Index+1+yIndex[xendasmt],j+1])

					points(x=as.numeric(DAT[[y]]$OFL[Index+1,j+1]), y=yendasmt, pch=8, lwd=1)
				}
			}
			# dev.off()
		}

		for (y in 1:ny) {
			Index <- which(DAT[[y]]$SpawnB[,1]==as.character(Stock[i])) + 1
			nasmt <- as.numeric(DAT[[y]]$SpawnB[Index,1])
			startYr <- as.numeric(DAT[[y]]$SpawnB[Index,2])
			endYr <- as.numeric(DAT[[y]]$SpawnB[Index,3])
			nyr <- length(startYr:endYr)

			if (y==1) {
				ylabel <- "Spawning Biomass"
			} else {
				ylabel <- ""
			}

			if (i%%2==0) {
				xthing <- NULL
				xlabel <- "Projection Year"
			} else {
				xthing <- "n"
				xlabel <- ""
			}

			if (i%%2==0) {
				par(mar=c(5, 5, 2, 2))
			} else {
				par(mar=c(0.5, 5, 2, 2))
			}

			plot(1, type="n", xlab=xlabel, ylab=ylabel, 
				xlim=c(ProjStart[y], (ProjStart[y]+nyr-1)), ylim=c(0,maxYB), xaxt=xthing,
				 xaxs="i", yaxs="i", bty="l", cex=1.5, cex.lab=1.5, cex.axis=1.5, cex.main=1.5)

			for (j in 1:nasmt) {
				yIndex <- which(DAT[[y]]$SpawnB[(Index+2):(Index+1+nyr),j+1]!="")

				if (j==nasmt) {
					lines(x=ProjStart[y]:(ProjStart[y]+nyr-1),
					col=col[4], 
					y=as.numeric(DAT[[y]]$SpawnB[Index+1+yIndex,j+1]),
					lwd=2,
					lty=1)

					xendasmt <- as.numeric(DAT[[y]]$SpawnB[Index+1,j+1]) - as.numeric(DAT[[y]]$SpawnB[Index,2]) + 1
					yendasmt <- as.numeric(DAT[[y]]$SpawnB[Index+1+yIndex[xendasmt],j+1])

					points(x=as.numeric(DAT[[y]]$SpawnB[Index+1,j+1]), y=yendasmt, pch=8, lwd=1)

				} else {
					if (nasmt_check > nasmt) {J <- j+1} else {J=j}
					lines(x=ProjStart[y]:(ProjStart[y]+nyr-1), 
					col=col[J], 
					y=as.numeric(DAT[[y]]$SpawnB[Index+1+yIndex,j+1]),
					lwd=2,
					lty=1)

					xendasmt <- as.numeric(DAT[[y]]$SpawnB[Index+1,j+1]) - as.numeric(DAT[[y]]$SpawnB[Index,2]) + 1
					yendasmt <- as.numeric(DAT[[y]]$SpawnB[Index+1+yIndex[xendasmt],j+1])

					points(x=as.numeric(DAT[[y]]$SpawnB[Index+1,j+1]), y=yendasmt, pch=8, lwd=1)
				}
			}
		}

		if (i%%2==0) {
			par(mar=c(0.5, 0.5, 0.75, 0.5))
			plot(1, type="n", axes=FALSE, xlab="", ylab="")
			legend(x="bottom", legend=c("Oldest (usable)", "Intermediate A", "Intermediate B", "Most recent"), bty="n", 
			lty=1, col=c(col[1:ny], col[4]),
			lwd=rep(2, times=4), horiz=TRUE, cex=1.2)
		}
	}

		# legend(x="bottomright", legend=paste("Model", 1:nlegend, sep=" "), bty="n", 
		# 	lty=c(1:(nlegend-1), 1), col=c(rep("black", times=(nlegend-1)), "red"),
		# 	lwd=c(rep(1, times=(nlegend-1)),2))

		# legend(x="bottomright", legend=paste("Model", 1:nlegend, sep=" "), bty="n", 
		# 	lty=1, col=c(col[1:(nlegend-1)], col[4]),
		# 	lwd=c(rep(2, times=(nlegend-1)),2))
}

##############################################################################################################################

Single_Graph_Proj <- function(DAT, col=c("#000000", "#009E73", "#0072B2", "#CC79A7")) {
	# Pull the vector of stock names
	Stock <- DAT[[1]]$OFL[1,which(DAT[[1]]$OFL[1,]!="")]
	nStock <- length(Stock)
	Start <- c(2001, 2006, 2011)

	for (i in 1:nStock) {

		maxY.vec <- vector(length=3)
		for (yy in 1:3) {
			Index <- which(DAT[[yy]]$OFL[,1]==as.character(Stock[i])) + 1
			nasmt <- as.numeric(DAT[[yy]]$OFL[Index,1])
			if (yy==1) {nlegend <- nasmt}
			startYr <- as.numeric(DAT[[yy]]$OFL[Index,2])
			endYr <- as.numeric(DAT[[yy]]$OFL[Index,3])
			nyr <- length(startYr:endYr)
			maxY.vec[yy] <- max(as.numeric(unlist(DAT[[yy]]$OFL[(Index+2):(Index+1+nyr),1:nasmt+1])))
		}
		maxY <- max(maxY.vec)*1.5

		plot(1, type="n", xlab="Projection Year", ylab="OFL", 
			xlim=c(1,nyr), ylim=c(0,maxY),
			xaxs="i", yaxs="i", main=Stock[i], bty="l")

		legend(x="bottomright", legend=paste("Model", 1:nlegend, sep=" "), bty="n", 
			lty=1, col=c(col[1:(nlegend-1)], col[4]),
			lwd=c(rep(2, times=(nlegend-1)),2))
		
		for (y in 1:3) {
			Index <- which(DAT[[y]]$OFL[,1]==as.character(Stock[i])) + 1
			nasmt <- as.numeric(DAT[[y]]$OFL[Index,1])
			if (y==1) {nasmt_check <- nasmt}
			startYr <- as.numeric(DAT[[y]]$OFL[Index,2])
			endYr <- as.numeric(DAT[[y]]$OFL[Index,3])
			nyr <- length(startYr:endYr)
			if (y==1) {start <- 0}
			if (y==2) {start <- 5}
			if (y==3) {start <- 15}

			for (j in 1:nasmt) {
				yIndex <- which(DAT[[y]]$OFL[(Index+2):(Index+1+nyr),j+1]!="")

				if (j==nasmt) {
					lines(x=1:nyr+start,
					col=col[4], 
					y=as.numeric(DAT[[y]]$OFL[Index+1+yIndex,j+1]),
					lwd=2,
					lty=1)

					xendasmt <- as.numeric(DAT[[y]]$OFL[Index+1,j+1]) - as.numeric(DAT[[y]]$OFL[Index,2]) + 1
					yendasmt <- as.numeric(DAT[[y]]$OFL[Index+1+yIndex[xendasmt],j+1])

					# points(x=xendasmt, y=yendasmt, pch=8, lwd=1)

				} else {
					if (nasmt_check > nasmt) {J <- j+1} else {J=j}
					lines(x=1:nyr+start,
					col=col[J], 
					y=as.numeric(DAT[[y]]$OFL[Index+1+yIndex,j+1]),
					lwd=2,
					lty=1)


					xendasmt <- as.numeric(DAT[[y]]$OFL[Index+1,j+1]) - as.numeric(DAT[[y]]$OFL[Index,2]) + 1
					yendasmt <- as.numeric(DAT[[y]]$OFL[Index+1+yIndex[xendasmt],j+1])

					# points(x=xendasmt, y=yendasmt, pch=8, lwd=1)
				}
			}
		}

		maxY.vec <- vector(length=3)
		for (yy in 1:3) {
			Index <- which(DAT[[yy]]$SpawnB[,1]==as.character(Stock[i])) + 1
			nasmt <- as.numeric(DAT[[yy]]$SpawnB[Index,1])
			startYr <- as.numeric(DAT[[yy]]$SpawnB[Index,2])
			endYr <- as.numeric(DAT[[yy]]$SpawnB[Index,3])
			nyr <- length(startYr:endYr)
			maxY.vec[yy] <- max(as.numeric(unlist(DAT[[yy]]$SpawnB[(Index+2):(Index+1+nyr),1:nasmt+1])))
		}
		maxY <- max(maxY.vec)*1.5

		plot(1, type="n", xlab="Projection Year", ylab="SpawnB", 
			xlim=c(1,nyr), ylim=c(0,maxY),
			 xaxs="i", yaxs="i", main=Stock[i], bty="l")

		legend(x="bottomright", legend=paste("Model", 1:nlegend, sep=" "), bty="n", 
			lty=1, col=c(col[1:(nlegend-1)], col[4]),
			lwd=c(rep(2, times=(nlegend-1)),2))

		for (y in 1:3) {
			Index <- which(DAT[[y]]$SpawnB[,1]==as.character(Stock[i])) + 1
			nasmt <- as.numeric(DAT[[y]]$SpawnB[Index,1])
			if (y==1) {nasmt_check <- nasmt}
			startYr <- as.numeric(DAT[[y]]$SpawnB[Index,2])
			endYr <- as.numeric(DAT[[y]]$SpawnB[Index,3])
			nyr <- length(startYr:endYr)

			if (y==1) {start <- 0}
			if (y==2) {start <- 5}
			if (y==3) {start <- 15}

			for (j in 1:nasmt) {
				yIndex <- which(DAT[[y]]$SpawnB[(Index+2):(Index+1+nyr),j+1]!="")

				if (j==nasmt) {
					lines(x=1:nyr+start,
					col=col[4], 
					y=as.numeric(DAT[[y]]$SpawnB[Index+1+yIndex,j+1]),
					lwd=2,
					lty=1)

					xendasmt <- as.numeric(DAT[[y]]$SpawnB[Index+1,j+1]) - as.numeric(DAT[[y]]$SpawnB[Index,2]) + 1
					yendasmt <- as.numeric(DAT[[y]]$SpawnB[Index+1+yIndex[xendasmt],j+1])

					# points(x=xendasmt, y=yendasmt, pch=8, lwd=1)

				} else {
					if (nasmt_check > nasmt) {J <- j+1} else {J=j}
					lines(x=1:nyr+start, 
					col=col[J], 
					y=as.numeric(DAT[[y]]$SpawnB[Index+1+yIndex,j+1]),
					lwd=2,
					lty=1)

					xendasmt <- as.numeric(DAT[[y]]$SpawnB[Index+1,j+1]) - as.numeric(DAT[[y]]$SpawnB[Index,2]) + 1
					yendasmt <- as.numeric(DAT[[y]]$SpawnB[Index+1+yIndex[xendasmt],j+1])

					# points(x=xendasmt, y=yendasmt, pch=8, lwd=1)
				}
			}
		}
	}
}

##############################################################################################################################

SppPooled_Graph <- function(dat, legend=TRUE, ProjStart) {
	Start <- ProjStart
	maxY <- vector(length=3)
	for (yy in 1:3) {
		maxY[yy] <- range(dat[[yy]]$CI_YP[,1],dat[[yy]]$CI_YP[,2])[2]*1.5

	}
	maxY <- max(maxY)

	for (y in 1:3) {
		plot(x=1:dat[[y]]$nyr, y=dat[[y]]$SigmaYP,
			ylim=c(0,maxY),
			pch=19, 
			main=Start[y],
			xlab="Projection Year",
			ylab="Year-specific and species-pooled sigma with 95% CI",
			xaxs="i", yaxs="i", bty="l", xlim=c(0, dat[[y]]$nyr+1))

		arrows(1:dat[[y]]$nyr, dat[[y]]$CI_YP[,1], 1:dat[[y]]$nyr, dat[[y]]$CI_YP[,2],
			length=0.05, angle=90, code=3)

		abline(h=dat[[y]]$SigmaY, lwd=3, col="#009E73")
		abline(h=dat[[y]]$CI_Y[1], lty=2, lwd=3, col="#009E73")
		abline(h=dat[[y]]$CI_Y[2], lty=2, lwd=3, col="#009E73")

		if (legend & y==1) {
		legend(x="topleft", legend=c("Year-pooled, species-pooled sigma", "95% CI"), bty="n",
			lty=c(1,2), col="#009E73", lwd=1)
	}
	}
}

##############################################################################################################################

# dat==OFL, dat2==SpawnB
SppSpecific_Graph <- function(dat, dat2, ProjStart) {
	Start <- ProjStart
	Stock <- dat2[[1]]$Stock
	for (s in 1:dat[[1]]$nstk) {
		
		if(s==1) {
			plot(1, type="n", axes=FALSE, xlab="", ylab="")
			text(1,1, labels=Stock[s])
		}

		maxY <- vector(length=3)
		for (yy in 1:3) {
			maxY[yy] <- range(dat[[yy]][[s]]$CI_YSP[,1],dat[[yy]][[s]]$CI_YSP[,2])[2]
		}
		maxY <- max(maxY)
		
		# tiff("SingleSpp_exampleDetAnalysis2.tiff", width=8.5, height=3.6, units="in", res=300)
		# par(mfrow=c(1,3))
		for (y in 1:3) {
			if (s%%2==0) {
				mainlabel <- ""
				xlabel <- ""
			} else {
				mainlabel <- ProjStart[y]
				xlabel <- ""
			}
			if (y==1) {
				ylabel <- "Variation in OFL"
			} else {
				ylabel <- ""
			}
			par(mar=c(0.5, 5, 2, 2))
			# tiff("SingleSpp_exampleDetAnalysis.tiff", width=7, height=5, units="in", res=300)
			plot(x=1:dat[[y]]$nyr, y=dat[[y]][[s]]$SigmaYSP,
				ylim=c(0,maxY),
				pch=19, 
				main=mainlabel,
				xlab=xlabel,
				ylab=ylabel,
				xaxs="i", yaxs="i", bty="l", xaxt=NULL, xlim=c(0, dat[[y]]$nyr+1))

			arrows(1:dat[[y]]$nyr, dat[[y]][[s]]$CI_YSP[,1], 1:dat[[y]]$nyr, dat[[y]][[s]]$CI_YSP[,2],
				length=0.05, angle=90, code=3)

			abline(h=dat[[y]][[s]]$SigmaYS, lwd=3, col="#009E73")
			abline(h=dat[[y]][[s]]$CI_YS[1], lty=2,lwd=3, col="#009E73")
			abline(h=dat[[y]][[s]]$CI_YS[2], lty=2, lwd=3, col="#009E73")
		}

		if (s%%2!=0) {
			xlabel <- ""
		} else {
			xlabel <- "Projection Year"
		}

		for (yy in 1:3) {
			maxY[yy] <- range(dat2[[yy]][[s]]$CI_YSP[,1],dat2[[yy]][[s]]$CI_YSP[,2])[2]
		}
		maxY <- max(maxY)
		for (y in 1:3) {
			if (y==1) {
				ylabel <- "Variation in Spawning Biomass"
			} else {
				ylabel <- ""
			}
			if (s%%2==0) {par(mar=c(5, 5, 2, 2))}
			plot(x=1:dat2[[y]]$nyr, y=dat2[[y]][[s]]$SigmaYSP,
				ylim=c(0,maxY),
				pch=19, 
				main="",
				xlab=xlabel,
				ylab=ylabel,
				xaxs="i", yaxs="i", bty="l", xlim=c(0, dat2[[y]]$nyr+1))

			arrows(1:dat2[[y]]$nyr, dat2[[y]][[s]]$CI_YSP[,1], 1:dat2[[y]]$nyr, dat2[[y]][[s]]$CI_YSP[,2],
				length=0.05, angle=90, code=3)

			abline(h=dat2[[y]][[s]]$SigmaYS, lwd=3, col="#009E73")
			abline(h=dat2[[y]][[s]]$CI_YS[1], lty=2, lwd=3, col="#009E73")
			abline(h=dat2[[y]][[s]]$CI_YS[2], lty=2, lwd=3, col="#009E73")
		}
		
		if (s%%2==0) {
			par(mar=c(0.5, 0.5, 0.75, 0.5))
			plot(1, type="n", axes=FALSE, xlab="", ylab="")
			legend(x="top", legend=c("Year-pooled sigma", "95% CI"), bty="n",
				lty=c(1,2), col="#009E73", lwd=c(1,1), horiz=TRUE)
		}

		if (s<8) {
		plot(1, type="n", axes=FALSE, xlab="", ylab="")
		text(1,1, labels=Stock[s+1])
		}
	}
}

##############################################################################################################################

Multi_Graph_Proj_S <- function(DAT2, ProjStart, col=c("#000000", "#009E73", "#0072B2", "#CC79A7"), niter, diffOFLlim=TRUE) {
	# Pull the vector of stock names
	Stock <- DAT2[[1]][[1]]$OFL[1,which(DAT2[[1]][[1]]$OFL[1,]!="")]
	nStock <- length(Stock)
	Start <- ProjStart
	
	for (i in 1:nStock) {
		maxYY <- c()
		maxYYB <- c()

		for (yy in 1:3) {
			DAT <- DAT2[[yy]]
			for (jj in 1:niter) {
				Index <- which(DAT[[jj]]$OFL[,1]==as.character(Stock[i])) + 1
				nasmt <- as.numeric(DAT[[jj]]$OFL[Index,1])
				if (yy==1) {nlegend <- nasmt}
				startYr <- as.numeric(DAT[[jj]]$OFL[Index,2])
				endYr <- as.numeric(DAT[[jj]]$OFL[Index,3])
				nyr <- length(startYr:endYr)
				maxYY <- c(maxYY, as.numeric(max(as.numeric(unlist(DAT[[jj]]$OFL[(Index+2):(Index+1+nyr),2:(nasmt+1)])), na.rm=T))*1.15)
			
				Index <- which(DAT[[jj]]$SpawnB[,1]==as.character(Stock[i])) + 1
				nasmt <- as.numeric(DAT[[jj]]$SpawnB[Index,1])
				startYr <- as.numeric(DAT[[jj]]$SpawnB[Index,2])
				endYr <- as.numeric(DAT[[jj]]$SpawnB[Index,3])
				nyr <- length(startYr:endYr)
				maxYYB <- c(maxYYB, as.numeric(max(as.numeric(unlist(DAT[[jj]]$SpawnB[(Index+2):(Index+1+nyr),2:(nasmt+1)])), na.rm=T))*1.15)
			}
		}

		maxY <- max(maxYY)
		maxYB <- max(maxYYB)

		# Begin OFL projections plotting

		if (i%%2==0) {
			plot(1, type="n", axes=FALSE, xlab="", ylab="")
			text(1,1, labels=Stock[i], cex=1.5)
		} else {
			plot(1, type="n", axes=FALSE, xlab="", ylab="")
			text(1,1, labels=Stock[i], cex=1.5)
		}
		# tiff("SingleSpp_exampleProj2.tiff", width=8.5, height=5, units="in", res=300)
		# par(mfrow=c(1,3))
		for (y in 1:3) {
			DAT <- DAT2[[y]]

			if (y==1) {
				ylabel <- "OFL"
			} else {
				ylabel <- ""
			}

			if (i%%2!=0) {
				topmain <- Start[y]
			} else {
				topmain <- NULL
			}
			if (i%%2==0) {
				# par(mar=c(5, 5, 2, 2))
			} else {
				par(mar=c(0.5, 5, 2, 2))
			}

			if (diffOFLlim) {
				if (y==1) {
					maxY <- max(maxYY[1:niter])
				}
				if (y==2) {
					maxY <- max(maxYY[(niter+1):(niter*y)])
				}
				if (y==3) {
					maxY <- max(maxYY[(niter*(y-1)+1):(niter*y)])
				}
			}
			# tiff("SingleSpp_exampleProj.tiff", width=7, height=5, units="in", res=300)
			plot(1, type="n", xlab="", ylab=ylabel, 
				xlim=c(ProjStart[y], (ProjStart[y]+nyr-1)), ylim=c(0,maxY), xaxt="n",
				xaxs="i", yaxs="i", main=topmain, bty="l", cex=1.5, cex.lab=1.5, cex.main=1.5, cex.axis=1.5)

			for (j in 1:niter) {
				Index <- which(DAT[[j]]$OFL[,1]==as.character(Stock[i])) + 1
				nasmt <- as.numeric(DAT[[j]]$OFL[Index,1])
				startYr <- as.numeric(DAT[[j]]$OFL[Index,2])
				endYr <- as.numeric(DAT[[j]]$OFL[Index,3])
				nyr <- length(startYr:endYr)
				if (y==1) {nasmt_check <- nasmt}


				for (k in 1:nasmt) {
					yIndex <- which(DAT[[j]]$OFL[(Index+2):(Index+1+nyr),k+1]!="")

					if (k==nasmt) {
						lines(x=ProjStart[y]:(ProjStart[y]+nyr-1),
						col=col[4], 
						y=as.numeric(DAT[[j]]$OFL[Index+1+yIndex,k+1]),
						lwd=2,
						lty=1)

						xendasmt <- as.numeric(DAT[[j]]$OFL[Index+1,k+1]) - as.numeric(DAT[[j]]$OFL[Index,2]) + 1
						yendasmt <- as.numeric(DAT[[j]]$OFL[Index+1+yIndex[xendasmt],k+1])

						points(x=as.numeric(DAT[[j]]$OFL[Index+1,k+1]), y=yendasmt, pch=8, lwd=1)

					} else {
						if (nasmt_check > nasmt) {J <- k+1} else {J=k}
						lines(x=ProjStart[y]:(ProjStart[y]+nyr-1),
						col=col[J], 
						y=as.numeric(DAT[[j]]$OFL[Index+1+yIndex,k+1]),
						lwd=2,
						lty=1)

						xendasmt <- as.numeric(DAT[[j]]$OFL[Index+1,k+1]) - as.numeric(DAT[[j]]$OFL[Index,2]) + 1
						yendasmt <- as.numeric(DAT[[j]]$OFL[Index+1+yIndex[xendasmt],k+1])

						points(x=as.numeric(DAT[[j]]$OFL[Index+1,k+1]), y=yendasmt, pch=8, lwd=1)
					}
				}
				# dev.off()
			}
		}

		# Begin SpawnB Projections plotting
		for (y in 1:3) {
			DAT <- DAT2[[y]]

			if (y==1) {
				ylabel <- "Spawning Biomass"
			} else {
				ylabel <- ""
			}

			if (i%%2==0) {
				xthing <- NULL
				xlabel <- "Projection Year"
			} else {
				xthing <- "n"
				xlabel <- ""
			}

			if (i%%2==0) {
				par(mar=c(5, 5, 2, 2))
			} else {
				par(mar=c(0.5, 5, 2, 2))
			}

			plot(1, type="n", xlab=xlabel, ylab=ylabel, 
				xlim=c(ProjStart[y], (ProjStart[y]+nyr-1)), ylim=c(0,maxYB), xaxt=xthing,
				 xaxs="i", yaxs="i", bty="l", cex=1.5, cex.lab=1.5, cex.main=1.5, cex.axis=1.5)

			for (j in 1:niter) {
				Index <- which(DAT[[j]]$SpawnB[,1]==as.character(Stock[i])) + 1
				nasmt <- as.numeric(DAT[[j]]$SpawnB[Index,1])
				startYr <- as.numeric(DAT[[j]]$SpawnB[Index,2])
				endYr <- as.numeric(DAT[[j]]$SpawnB[Index,3])
				nyr <- length(startYr:endYr)
				if (y==1) {nasmt_check <- nasmt}


				for (k in 1:nasmt) {
				yIndex <- which(DAT[[j]]$SpawnB[(Index+2):(Index+1+nyr),k+1]!="")

				if (k==nasmt) {
					lines(x=ProjStart[y]:(ProjStart[y]+nyr-1),
					col=col[4], 
					y=as.numeric(DAT[[j]]$SpawnB[Index+1+yIndex,k+1]),
					lwd=2,
					lty=1)

					xendasmt <- as.numeric(DAT[[j]]$SpawnB[Index+1,k+1]) - as.numeric(DAT[[j]]$SpawnB[Index,2]) + 1
					yendasmt <- as.numeric(DAT[[j]]$SpawnB[Index+1+yIndex[xendasmt],k+1])

					points(x=as.numeric(DAT[[j]]$SpawnB[Index+1,k+1]), y=yendasmt, pch=8, lwd=1)

				} else {
					if (nasmt_check > nasmt) {J <- k+1} else {J=k}
					lines(x=ProjStart[y]:(ProjStart[y]+nyr-1), 
					col=col[J], 
					y=as.numeric(DAT[[j]]$SpawnB[Index+1+yIndex,k+1]),
					lwd=2,
					lty=1)

					xendasmt <- as.numeric(DAT[[j]]$SpawnB[Index+1,k+1]) - as.numeric(DAT[[j]]$SpawnB[Index,2]) + 1
					yendasmt <- as.numeric(DAT[[j]]$SpawnB[Index+1+yIndex[xendasmt],k+1])

					points(x=as.numeric(DAT[[j]]$SpawnB[Index+1,k+1]), y=yendasmt, pch=8, lwd=1)
					}
				}
			}
		}

	if (i%%2==0) {
		par(mar=c(0.5, 0.5, 0.75, 0.5))
		plot(1, type="n", axes=FALSE, xlab="", ylab="")
			legend(x="bottom", legend=c("Oldest (usable)", "Intermediate A", "Intermediate B", "Most recent"), bty="n", 
			lty=1, col=c(col[1:3], col[4]),
			lwd=rep(2, times=4), horiz=TRUE, cex=1.2)
	}

	}

		# legend(x="bottomright", legend=paste("Model", 1:nlegend, sep=" "), bty="n", 
		# 	lty=c(1:(nlegend-1), 1), col=c(rep("black", times=(nlegend-1)), "red"),
		# 	lwd=c(rep(1, times=(nlegend-1)),2))

		# legend(x="bottomright", legend=paste("Model", 1:nlegend, sep=" "), bty="n", 
		# 	lty=1, col=c(col[1:(nlegend-1)], col[4]),
		# 	lwd=c(rep(2, times=(nlegend-1)),2))
}

##############################################################################################################################

