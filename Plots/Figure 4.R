
# source("SigmaAnalysis.R")

pdf("Plots\\Figure 6.pdf", width=6.69291, height=5)
# tiff("Plots\\Figure 4.tiff", width=170, height=127, units="mm", res=500)

#######################################################################################################################################
ProjStart <- c(1998, 2003, 2008)
par(mfrow=c(2,3), mar=c(0,0,0,0), oma=c(5,7,5,5))

colr <- c("#FFC107", "#1E88E5","#004D40", "#D81B60")

#######################################################################################################################################

# Spawning Biomass
load("Data\\Stochastic Run Nproj25\\StoSpawnB.Rdata")
res <- StoSpawnB
point <- c(15,17,19)

for (y in 1:3) {

	plot(1, type="n", xlab="", ylab="", xaxt="n", yaxt="n",
		xlim=c(1, (ncol(res$SigmaYSP[[1]]))), ylim=c(0,0.8),
	 	xaxs="i", yaxs="i")

	nasmt <- ncol(res$SigmaYSPI[[y]][[1]])

	if (y==1) {
		tix <- seq(from=0, to=0.8, by=0.1)
		ntix <- length(tix)
		axis(side=2, at=tix[c(-1, -ntix)], labels=tix[c(-1,-ntix)], cex.axis=1.5)
		mtext(text=expression(paste(sigma["y,s,p,i"],sep=" ")), side=2, line=4, outer=T, cex=2)
		mtext(text="Spawning Biomass", side=2, line=2.5, outer=T, adj=0.85, cex=1.2)
		legend(x="topleft", legend=c("Oldest (usable)", "Intermediate", "Most recent"), bty="n",
			lty=c(1:nasmt), col=colr, lwd=c(2,2,2), horiz=F, cex=1.1)
	}

	mtext(side=3, text=ProjStart[y], line=2, cex=1.5)

	for (j in 1:nasmt) {

	lines(x=1:(length(res$SigmaBarP)), y=res$SigmaYSPI[[y]][[1]][,j],
		col=colr[j], lty=j, lwd=2)

	polygon(x=c(1:(length(res$SigmaBarP)), rev(1:(length(res$SigmaBarP)))),
		y=c(res$SigmaYSPICI[[y]][[1]][[j]][,1], rev(res$SigmaYSPICI[[y]][[1]][[j]][,2])), border=NA,
		col=rgb(red=col2rgb(colr[j])[,1][1], green=col2rgb(colr[j])[,1][2], blue=col2rgb(colr[j])[,1][3], alpha=175, maxColorValue=255))
	}
}

#######################################################################################################################################

# OFL
load("Data\\Stochastic Run Nproj25\\StoOFL.Rdata")
res <- StoOFL

for (y in 1:3) {

	plot(1, type="n", xlab="", ylab="", xaxt="n", yaxt="n",
		xlim=c(1, (ncol(res$SigmaYSP[[1]]))), ylim=c(0,0.8),
	 	xaxs="i", yaxs="i")

	tix <- seq(from=0, to=(ncol(res$SigmaYSP[[1]]+1)), by=5)
	ntix <- length(tix)
	axis(side=1, at=tix[-1], labels=tix[-1], cex.axis=1.5)

	if (y==1) {
		tix <- seq(from=0, to=0.8, by=0.1)
		ntix <- length(tix)
		axis(side=2, at=tix[c(-1, -ntix)], labels=tix[c(-1,-ntix)], cex.axis=1.5)
		mtext(text="OFL", side=2, line=2.5, outer=T, adj=0.24, cex=1.2)
	}

	nasmt <- ncol(res$SigmaYSPI[[y]][[1]])

	for (j in 1:nasmt) {

	lines(x=1:(length(res$SigmaBarP)), y=res$SigmaYSPI[[y]][[1]][,j],
		col=colr[j], lty=j, lwd=2)

	polygon(x=c(1:(length(res$SigmaBarP)), rev(1:(length(res$SigmaBarP)))),
		y=c(res$SigmaYSPICI[[y]][[1]][[j]][,1], rev(res$SigmaYSPICI[[y]][[1]][[j]][,2])), border=NA,
		col=rgb(red=col2rgb(colr[j])[,1][1], green=col2rgb(colr[j])[,1][2], blue=col2rgb(colr[j])[,1][3], alpha=175, maxColorValue=255))

	}


	mtext(side=1, text="Projection year", outer=T, line=3, cex=1.5)

}


dev.off()