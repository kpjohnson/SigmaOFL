source("SigmaAnalysis.R")

ProjStart <- c(1998, 2003, 2008)
colr <- c("#000000", "#009E73", "#0072B2", "#CC79A7")

load("Deterministic Projections.Rdata")
Stock <- DAT[[1]]$OFL[1,which(DAT[[1]]$OFL[1,]!="")]

resD <- SigmaAnalysis(RecType=2, Nproj=25, Nsim=100, ProjType=2, ProjStart=ProjStart)
res <- SigmaAnalysis(RecType=1, Nproj=25, Nsim=100, ProjType=2, ProjStart=ProjStart)

resD2 <- SigmaAnalysis(RecType=2, Nproj=25, Nsim=100, ProjType=1, ProjStart=ProjStart)
res2 <- SigmaAnalysis(RecType=1, Nproj=25, Nsim=100, ProjType=1, ProjStart=ProjStart)


pdf("Z:\\UW Google Drive\\Thesis Stuff\\SigmaOFL\\R\\Final Thesis Run\\All Species YSP.pdf", width=8.5, height=11)
mat <- as.matrix(read.csv("projmat2.csv", header=FALSE))
layout(mat, heights=c(0.5,0.5,0.5,0.5),
	widths=c(0.5, 1, 1, 1))
# par(mar=c(5, 5, 2, 2))
for (s in 1:7) {
	# par(mfrow=c(2,1))

	if (s%%2==0) {
		plot(1, type="n", axes=FALSE, xlab="", ylab="")
		text(1,1, labels=Stock[s], cex=1.5)
	} else {
		plot(1, type="n", axes=FALSE, xlab="", ylab="")
		text(1,1, labels=Stock[s], cex=1.5)
	}

	if (s%%2==0) {
		xlabel <- "Projection year"
	} else {
		xlabel <- ""
	}

	# Deterministic results OFL
	par(mar=c(5,6,2,2))
	plot(1, type="n", xlab="", ylab=expression(paste("OFL  ", sigma["y,s,p"],sep=" ")), 
		xlim=c(0, ncol(resD$SigmaYSP[[1]])+1), ylim=c(0,1.2),
		 xaxs="i", yaxs="i", bty="l", main="", cex=1.5, cex.lab=1.5, cex.axis=1.5, cex.main=1.5)

	for (i in 1:3) {
		points(x=1:ncol(resD$SigmaYSP[[1]]), y=resD$SigmaYSP[[i]][s,],
			pch=19, col=colr[i]
			)

		# arrows(1:(length(resD$SigmaYSP[[1]])), resD$YSP.CI[[i]][[1]][,1], 1:(length(resD$SigmaYSP[[1]])), resD$YSP.CI[[i]][[1]][,2],
		# 	length=0.05, angle=90, code=3, col=colr[i])
	}

	lines(x=1:ncol(resD$SigmaYSP[[1]]), y=resD$SigmaBarSP[s,],
			lwd=2, col=colr[4])

	# legend(x="bottomright", legend=c(ProjStart, "Mean"), bty="n", pch=(c(19,19,19,NULL)),
	# 	lty=c(0,0,0,NULL), col=colr, lwd=c(0,0,0,1), horiz=F)

	# Stochastic results OFL
	plot(1, type="n", xlab="", ylab="", 
		xlim=c(0, ncol(res$SigmaYSP[[1]])+1), ylim=c(0,1.2),
		 xaxs="i", yaxs="i", bty="l", main="", cex=1.5, cex.lab=1.5, cex.axis=1.5, cex.main=1.5)

	for (i in 1:3) {
		points(x=1:ncol(res$SigmaYSP[[1]]), y=res$SigmaYSP[[i]][s,],
			pch=19, col=colr[i]
			)

		arrows(1:(length(res$SigmaYSP[[1]][s,])), res$YSP.CI[[i]][[s]][,1], 1:(length(res$SigmaYSP[[1]][s,])), res$YSP.CI[[i]][[s]][,2],
			length=0.05, angle=90, code=3, col=colr[i])
	}

	lines(x=1:ncol(res$SigmaYSP[[1]]), y=res$SigmaBarSP[s,],
			lwd=2, col=colr[4])


	###########################################################################################################################################

	# Deterministic results SpawnB

	plot(1, type="n", xlab=xlabel, ylab=expression(paste("Spawning Biomass  ", sigma["y,s,p"],sep=" ")), 
		xlim=c(0, ncol(resD2$SigmaYSP[[1]])+1), ylim=c(0,1.2),
		 xaxs="i", yaxs="i", bty="l", main="", cex=1.5, cex.lab=1.5, cex.axis=1.5, cex.main=1.5)

	for (i in 1:3) {
		points(x=1:ncol(resD2$SigmaYSP[[1]]), y=resD2$SigmaYSP[[i]][s,],
			pch=19, col=colr[i]
			)

	}

	lines(x=1:ncol(resD2$SigmaYSP[[1]]), y=resD2$SigmaBarSP[s,],
			lwd=2, col=colr[4])



	# Stochastic results SpawnB
	plot(1, type="n", xlab=xlabel, ylab="", 
		xlim=c(0, ncol(res2$SigmaYSP[[1]])+1), ylim=c(0,1.2),
		 xaxs="i", yaxs="i", bty="l", main="", cex=1.5, cex.lab=1.5, cex.axis=1.5, cex.main=1.5)

	for (i in 1:3) {
		points(x=1:ncol(res2$SigmaYSP[[1]]), y=res2$SigmaYSP[[i]][s,],
			pch=19, col=colr[i]
			)

		arrows(1:(length(res2$SigmaYSP[[1]][s,])), res2$YSP.CI[[i]][[s]][,1], 1:(length(res2$SigmaYSP[[1]][s,])), res2$YSP.CI[[i]][[s]][,2],
			length=0.05, angle=90, code=3, col=colr[i])
	}

	lines(x=1:ncol(res2$SigmaYSP[[1]]), y=res2$SigmaBarSP[s,],
			lwd=2, col=colr[4])

	if (s%%2==0 | s==7) {
		legend(x="topright", legend=c(ProjStart, expression(bar(sigma["p"]))), 
			bty="n", 
			pch=(c(19,19,19,NA)),
			lty=c(0,0,0,1), 
			col=colr, 
			lwd=c(0,0,0,2), 
			horiz=F,
			cex=1.15)
	}



}




dev.off()