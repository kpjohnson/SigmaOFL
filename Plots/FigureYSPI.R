source("SigmaAnalysis.R")

ProjStart <- c(1998, 2003, 2008)
res <- SigmaAnalysis(RecType=1, Nproj=25, Nsim=100, ProjType=1, ProjStart=ProjStart)

load("Deterministic Projections.Rdata")
Stock <- DAT[[1]]$OFL[1,which(DAT[[1]]$OFL[1,]!="")]

colr <- c("#000000", "#009E73", "#0072B2", "#CC79A7")
pdf("Z:\\UW Google Drive\\Thesis Stuff\\SigmaOFL\\R\\Final Thesis Run\\YSPI_Figures_SpawnB.pdf", height=11, width=8.5)

par(mfrow=c(3,3))
par(mar=c(5,6,2,2))
for (s in 1:7) {

	for (y in 1:3) {

		if (s==1 | s==4 | s==7) {
			mainlab <- ProjStart[y]
		} else {
			mainlab <- ""
		}

		if (s%%3==0 | s==7) {
			xlabel <- "Projection year"
		} else {
			xlabel <- ""
		}

		if (y==1) {
			# ylabel <-  bquote(paste(.(Stock[s]), "Spawning Biomass  ", sigma["y,s,p,i"],sep=" "))
			ylabel <- bquote(.(as.character(Stock[s])) ~ "Spawning Biomass" ~ sigma["y,s,p,i"])
		} else {
			ylabel <- ""
		}

		plot(1, type="n", xlab=xlabel, ylab=ylabel, 
		xlim=c(0, ncol(res$SigmaYSP[[1]])+1), ylim=c(0,0.8),
		 xaxs="i", yaxs="i", bty="l", main=mainlab, cex=1.5, cex.lab=1.5, cex.axis=1.5, cex.main=1.5)

		nasmt <- ncol(res$SigmaYSPI[[y]][[s]])

		for (j in 1:nasmt) {
			if (j==nasmt) {J <- 4} else {J <- j}
			points(x=1:(length(res$SigmaBarP)), y=res$SigmaYSPI[[y]][[s]][,j],
				col=colr[J], pch=19)

			arrows(1:(length(res$SigmaBarP)), res$SigmaYSPICI[[y]][[s]][[j]][,1], 1:(length(res$SigmaBarP)), res$SigmaYSPICI[[y]][[s]][[j]][,2],
				length=0.05, angle=90, code=3, col=colr[J])
		}

	}

  if (y==3 & s%%3==0 | s==7) {
    legend(x="topright", legend=c("Oldest assessment (usable)", "Intermediate assessment A", "Intermediate assessment B", "Most recent assessment"), bty="n", pch=(c(19,19,19,19)),
           lty=c(0,0,0,0), col=colr, lwd=c(0,0,0,0), horiz=F, cex=1.05)
  }

}

dev.off()

res <- SigmaAnalysis(RecType=1, Nproj=25, Nsim=100, ProjType=2)

pdf("Z:\\UW Google Drive\\Thesis Stuff\\SigmaOFL\\R\\Final Thesis Run\\YSPI_Figures_OFL.pdf", height=11, width=8.5)

par(mfrow=c(3,3))
par(mar=c(5,6,2,2))
for (s in 1:7) {

	for (y in 1:3) {

		if (s==1 | s==4 | s==7) {
			mainlab <- ProjStart[y]
		} else {
			mainlab <- ""
		}


		if (s%%3==0 | s==7) {
			xlabel <- "Projection year"
		} else {
			xlabel <- ""
		}

		if (y==1) {
			# ylabel <- bquote(paste(.(Stock[s]), "OFL ", sigma["y,s,p,i"],sep=" "))
			ylabel <- bquote(.(as.character(Stock[s])) ~ "OFL" ~ sigma["y,s,p,i"])
		} else {
			ylabel <- ""
		}

		plot(1, type="n", xlab=xlabel, ylab=ylabel, 
		xlim=c(0, ncol(res$SigmaYSP[[1]])+1), ylim=c(0,0.8),
		 xaxs="i", yaxs="i", bty="l", main=mainlab, cex=1.5, cex.main=1.5, cex.lab=1.5, cex.axis=1.5)

		nasmt <- ncol(res$SigmaYSPI[[y]][[s]])

		for (j in 1:nasmt) {
			if (j==nasmt) {J <- 4} else {J <- j}
			points(x=1:(length(res$SigmaBarP)), y=res$SigmaYSPI[[y]][[s]][,j],
				col=colr[J], pch=19)

			arrows(1:(length(res$SigmaBarP)), res$SigmaYSPICI[[y]][[s]][[j]][,1], 1:(length(res$SigmaBarP)), res$SigmaYSPICI[[y]][[s]][[j]][,2],
				length=0.05, angle=90, code=3, col=colr[J])
		}

	}

	if (y==3 & s%%3==0 | s==7) {
		legend(x="topright", legend=c("Oldest assessment (usable)", "Intermediate assessment A", "Intermediate assessment B", "Most recent assessment"), bty="n", pch=(c(19,19,19,19)),
			lty=c(0,0,0,0), col=colr, lwd=c(0,0,0,0), horiz=F, cex=1.05)
	}
}

dev.off()