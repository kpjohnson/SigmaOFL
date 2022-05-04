# source("SigmaAnalysis.R")

ProjStart <- c(1998, 2003, 2008)
colr <- c("#CC79A7", "#009E73", "#0072B2", "#000000")
point <- c(21, 23, 25) 

#######################################################################################################################################
pdf("Plots\\Figure 4.pdf", width=3.34646, height=6.88976)
# tiff("Plots\\Figure 3.tiff", width=85, height=175, units="mm", res=500)
par(mfrow=c(3,2), mar=c(0,0,0,0), oma=c(5,7,2,1))

#######################################################################################################################################
# Deterministic results SpawnB
load("Data\\Deterministic Run Nproj25\\DetSpawnB.Rdata")
resD <- DetSpawnB
plot(1, type="n",
	xlim=c(1, ncol(resD$SigmaYSP[[1]])), ylim=c(0,0.8), xaxt="n", yaxt="n",
	xaxs="i", yaxs="i")

mtext(side=3, text="SSB", line=0.25, outer=F, cex=1.5)

mtext(side=2, text=expression(sigma["y,s,p"]), line=5.3, outer=T, cex=1.5)

mtext(side=2, text="Deterministic", line=3.9, outer=T, adj=0.91, cex=1.2)

mtext(side=2, text="Stochastic", line=3.9, outer=T, adj=0.11, cex=1.2)
mtext(side=2, text=c("(past and future)"), line=2.2, outer=T, adj=0.06, cex=1.2)
# mtext(side=2, text=c("future)"), line=2.6, outer=T, adj=0.07, cex=1)

mtext(side=2, text="Stochastic", line=3.9, outer=T, adj=0.499, cex=1.2)
mtext(side=2, text="(future only)", line=2.2, outer=T, adj=0.5, cex=1.2)
# mtext(side=2, text="only)", line=2.6, outer=T, adj=0.52, cex=1)


for (i in 1:3) {
	lines(x=1:ncol(resD$SigmaYSP[[1]]), y=resD$SigmaYSP[[i]][1,],
		lty=i, col=colr[i], bg=colr[i]
		)
}

lines(x=1:ncol(resD$SigmaYSP[[1]]), y=resD$SigmaBarSP[1,],
		lwd=2, col=colr[4])

tix <- seq(from=0, to=0.8, by=0.1)
ntix <- length(tix)
axis(side=2, at=tix[c(-1,-ntix)], labels=tix[c(-1,-ntix)], outer=T, cex.axis=1.2)

legend(x="topleft", legend=c(ProjStart, expression(bar(sigma["s,p"]))), 
	bty="n", 
	# pch=(c(point,NA)),
	col=colr, 
	# pt.bg=colr,
	lwd=c(1,1,1,2),
	lty=1:3,
	horiz=F,
	cex=1.4)

#######################################################################################################################################
# Deterministic results OFL
load("Data\\Deterministic Run Nproj25\\DetOFL.Rdata")
resD <- DetOFL
plot(1, type="n", xlab="", xlim=c(1, ncol(resD$SigmaYSP[[1]])), ylim=c(0,0.8), xaxt="n", yaxt="n",
	 xaxs="i", yaxs="i")

mtext(side=3, text="OFL", line=0.25, outer=F, cex=1.5)
# mtext(side=2, text=expression(sigma["y,s,p"], line=3, outer=T))

for (i in 1:3) {
	lines(x=1:ncol(resD$SigmaYSP[[1]]), y=resD$SigmaYSP[[i]][1,],
		lty=i, col=colr[i], bg=colr[i]
		)
}

lines(x=1:ncol(resD$SigmaYSP[[1]]), y=resD$SigmaBarSP[1,],
		lwd=2, col=colr[4])

#######################################################################################################################################
# Stochastic0 results SpawnB
load("Data\\Stochastic Run NProj25_0\\StoSpawnB.Rdata")
res <- StoSpawnB
plot(1, type="n", xlab="", xaxt="n", yaxt="n", xlim=c(1, ncol(res$SigmaYSP[[1]])), ylim=c(0,0.8),
	 xaxs="i", yaxs="i")

for (i in 1:3) {
	lines(x=1:ncol(res$SigmaYSP[[1]]), y=res$SigmaYSP[[i]][1,],
		lty=i, col=colr[i], bg=colr[i]
		)

	# arrows(1:(length(res$SigmaYSP[[1]][1,])), res$YSP.CI[[i]][[1]][,1], 1:(length(res$SigmaYSP[[1]][1,])), res$YSP.CI[[i]][[1]][,2],
	# 	length=0.05, angle=90, code=3, col=colr[i])

	polygon(x=c(1:(ncol(res$SigmaYSP[[1]])), rev(1:(ncol(res$SigmaYSP[[1]])))),
		y=c(res$YSP.CI[[i]][[1]][,2], rev(res$YSP.CI[[i]][[1]][,1])), border=NA,
		col=rgb(red=col2rgb(colr[i])[,1][1], green=col2rgb(colr[i])[,1][2], blue=col2rgb(colr[i])[,1][3], alpha=75, maxColorValue=255))

}

lines(x=1:ncol(res$SigmaYSP[[1]]), y=res$SigmaBarSP[1,],
		lwd=2, col=colr[4])

tix <- seq(from=0, to=0.8, by=0.1)
ntix <- length(tix)
axis(side=2, at=tix[c(-1,-ntix)], labels=tix[c(-1,-ntix)], outer=T, cex.axis=1.2)
tix <- seq(from=0, to=25, by=5)
ntix <- length(tix)
axis(side=1, at=tix[-1], labels=tix[-1], outer=T, cex.axis=1)

#######################################################################################################################################
# Stochastic0 results OFL
load("Data\\Stochastic Run Nproj25_0\\StoOFL.Rdata")
res <- StoOFL
plot(1, type="n", xlab="", xaxt="n", yaxt="n", xlim=c(1, ncol(res$SigmaYSP[[1]])), ylim=c(0,0.8),
	 xaxs="i", yaxs="i")

for (i in 1:3) {
	lines(x=1:ncol(res$SigmaYSP[[1]]), y=res$SigmaYSP[[i]][1,],
		lty=i, col=colr[i], bg=colr[i]
		)

	# arrows(1:(length(res$SigmaYSP[[1]][1,])), res$YSP.CI[[i]][[1]][,1], 1:(length(res$SigmaYSP[[1]][1,])), res$YSP.CI[[i]][[1]][,2],
	# 	length=0.05, angle=90, code=3, col=colr[i])

	polygon(x=c(1:(ncol(res$SigmaYSP[[1]])), rev(1:(ncol(res$SigmaYSP[[1]])))),
		y=c(res$YSP.CI[[i]][[1]][,2], rev(res$YSP.CI[[i]][[1]][,1])), border=NA,
		col=rgb(red=col2rgb(colr[i])[,1][1], green=col2rgb(colr[i])[,1][2], blue=col2rgb(colr[i])[,1][3], alpha=75, maxColorValue=255))

}

lines(x=1:ncol(res$SigmaYSP[[1]]), y=res$SigmaBarSP[1,],
		lwd=2, col=colr[4])

mtext(side=1, text="Projection year", line=3, outer=T, cex=1.2)

tix <- seq(from=0, to=25, by=5)
ntix <- length(tix)
# axis(side=1, at=tix[-1], labels=tix[-1], outer=T, cex.axis=0.8)

#######################################################################################################################################
# Stochastic results SpawnB
load("Data\\Stochastic Run NProj25\\StoSpawnB.Rdata")
res <- StoSpawnB
plot(1, type="n", xlab="", xaxt="n", yaxt="n", xlim=c(1, ncol(res$SigmaYSP[[1]])), ylim=c(0,0.8),
	 xaxs="i", yaxs="i")

for (i in 1:3) {
	lines(x=1:ncol(res$SigmaYSP[[1]]), y=res$SigmaYSP[[i]][1,],
		lty=i, col=colr[i], bg=colr[i]
		)

	# arrows(1:(length(res$SigmaYSP[[1]][1,])), res$YSP.CI[[i]][[1]][,1], 1:(length(res$SigmaYSP[[1]][1,])), res$YSP.CI[[i]][[1]][,2],
	# 	length=0.05, angle=90, code=3, col=colr[i])

	polygon(x=c(1:(ncol(res$SigmaYSP[[1]])), rev(1:(ncol(res$SigmaYSP[[1]])))),
		y=c(res$YSP.CI[[i]][[1]][,2], rev(res$YSP.CI[[i]][[1]][,1])), border=NA,
		col=rgb(red=col2rgb(colr[i])[,1][1], green=col2rgb(colr[i])[,1][2], blue=col2rgb(colr[i])[,1][3], alpha=75, maxColorValue=255))

}

lines(x=1:ncol(res$SigmaYSP[[1]]), y=res$SigmaBarSP[1,],
		lwd=2, col=colr[4])

tix <- seq(from=0, to=0.8, by=0.1)
ntix <- length(tix)
axis(side=2, at=tix[c(-1,-ntix)], labels=tix[c(-1,-ntix)], outer=T, cex.axis=1.2)
tix <- seq(from=0, to=25, by=5)
ntix <- length(tix)
# axis(side=1, at=tix[-1], labels=tix[-1], outer=T, cex.axis=1.2)

#######################################################################################################################################
# Stochastic results OFL
load("Data\\Stochastic Run Nproj25\\StoOFL.Rdata")
res <- StoOFL
plot(1, type="n", xlab="", xaxt="n", yaxt="n", xlim=c(1, ncol(res$SigmaYSP[[1]])), ylim=c(0,0.8),
	 xaxs="i", yaxs="i")

for (i in 1:3) {
	lines(x=1:ncol(res$SigmaYSP[[1]]), y=res$SigmaYSP[[i]][1,],
		lty=i, col=colr[i], bg=colr[i]
		)

	# arrows(1:(length(res$SigmaYSP[[1]][1,])), res$YSP.CI[[i]][[1]][,1], 1:(length(res$SigmaYSP[[1]][1,])), res$YSP.CI[[i]][[1]][,2],
	# 	length=0.05, angle=90, code=3, col=colr[i])

	polygon(x=c(1:(ncol(res$SigmaYSP[[1]])), rev(1:(ncol(res$SigmaYSP[[1]])))),
		y=c(res$YSP.CI[[i]][[1]][,2], rev(res$YSP.CI[[i]][[1]][,1])), border=NA,
		col=rgb(red=col2rgb(colr[i])[,1][1], green=col2rgb(colr[i])[,1][2], blue=col2rgb(colr[i])[,1][3], alpha=75, maxColorValue=255))

}

lines(x=1:ncol(res$SigmaYSP[[1]]), y=res$SigmaBarSP[1,],
		lwd=2, col=colr[4])

# mtext(side=1, text="Projection year", line=3, outer=T, cex=0.8)

tix <- seq(from=0, to=25, by=5)
ntix <- length(tix)
axis(side=1, at=tix[-1], labels=tix[-1], outer=T, cex.axis=1)

dev.off()