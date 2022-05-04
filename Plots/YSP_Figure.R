source("SigmaAnalysis.R")


ProjStart <- c(1998, 2003, 2008)
colr <- c("#000000", "#009E73", "#0072B2", "#CC79A7")

load("Deterministic Projections.Rdata")
Stock <- DAT[[1]]$OFL[1,which(DAT[[1]]$OFL[1,]!="")]

resD <- SigmaAnalysis(RecType=2, Nproj=25, Nsim=100, ProjType=1)
res <- SigmaAnalysis(RecType=1, Nproj=25, Nsim=100, ProjType=1)


pdf("Z:\\UW Google Drive\\Thesis Stuff\\SigmaOFL\\R\\Graphs\\5October18\\Supplementary\\All Species YSP SpawnB.pdf", width=8.5, height=11)
for (s in 1:7) {
	par(mfrow=c(2,1))
	# Deterministic results SpawnB
	plot(1, type="n", xlab="", ylab=paste(Stock[s], "Spawning biomass SigmaYSP"), 
		xlim=c(0, ncol(resD$SigmaYSP[[1]])+1), ylim=c(0,1.2),
		 xaxs="i", yaxs="i", bty="l")

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

	# Stochastic results SpawnB
	plot(1, type="n", xlab="Projection year", ylab=paste(Stock[s],"Spawning biomass SigmaYSP"), 
		xlim=c(0, ncol(res$SigmaYSP[[1]])+1), ylim=c(0,1.2),
		 xaxs="i", yaxs="i", bty="l")

	for (i in 1:3) {
		points(x=1:ncol(res$SigmaYSP[[1]]), y=res$SigmaYSP[[i]][s,],
			pch=19, col=colr[i]
			)

		arrows(1:(length(res$SigmaYSP[[1]][s,])), res$YSP.CI[[i]][[s]][,1], 1:(length(res$SigmaYSP[[1]][s,])), res$YSP.CI[[i]][[s]][,2],
			length=0.05, angle=90, code=3, col=colr[i])
	}

	lines(x=1:ncol(res$SigmaYSP[[1]]), y=res$SigmaBarSP[s,],
			lwd=2, col=colr[4])

	legend(x="bottom", legend=c(ProjStart, "Mean"), bty="n", pch=(c(19,19,19,NULL)),
		lty=c(0,0,0,NULL), col=colr, lwd=c(0,0,0,1), horiz=T)
	}


dev.off()