
load("Data\\Stochastic Run Nproj1\\StoOFL.Rdata")
res <- StoOFL
# load("StoSpawnB.Rdata")
# res <- StoSpawnB
ProjStart <- 1994:2008
load("Data\\Stochastic Run Nproj1\\1998_StochProjResults.Rdata")
# Stocks <- ProjRes[[1]][[1]][1,c(which(!is.na(ProjRes[[1]][[1]][1,]))[-1])]
Stocks <- c("Bocaccio", "Canary", "Darkblotched", "Petrale sole", "Pacific Ocean perch", "Widow", "Lingcod")

pdf("Plots\\Figure 7.pdf", width=6.69291, height=5)
# tiff("Plots\\Figure 6a.tiff", width=170, height=127, units="mm", res=500)
par(mfrow=c(2,4), mar=c(0,0,0,0), oma=c(5,5,0,0.1))

# for (s in 1:7) {

#   plot(1, type="n", xlab="Projection start year", ylab=expression(paste("OFL  ", sigma["y,s"],sep=" ")), 
#        xlim=c(ProjStart[1]-1, ProjStart[length(ProjStart)]+2), ylim=c(0,1.4),
#        xaxt="n", yaxt="n",
#        xaxs="i", yaxs="i")

#   if (s<5) {
#     mtext(text=Stocks[s], side=3, line=1, cex=1.25)
#   } else {
#     mtext(text=Stocks[s], side=3, line=-3, cex=1.25, outer=F)
#   }

#   if (s==1) {
#     legend(x="topleft", legend=c("1 year projection", expression(bar(sigma["s"])), expression(bar(sigma))), 
#       bty="n", 
#       pch=(c(19,NA,NA)),
#       lty=c(0,2,3), 
#       col=c("black", "#004D40", "#1E88E5"), 
#       lwd=c(0,2,2), 
#       horiz=F,
#       cex=1.5
#     )
#   }
  
#   for (i in 1:length(ProjStart)) {
#     points(x=ProjStart[i], y=res$SigmaYS[i,s],
#            pch=19, col="black"
#     )
    
#     arrows(ProjStart[i], res$YS.CI[[i]][[s]][1], ProjStart[i], res$YS.CI[[i]][[s]][2],
#            length=0.05, angle=90, code=3, col="black")
#   }
  
#   abline(h=res$SigmaBarS[s],
#          lwd=2, col="#004D40", lty=2)

#   if (s>=5) {
#     tix <- seq(from=ProjStart[1]-1, to=ProjStart[length(ProjStart)]+2, by=2)
#     ntix <- length(tix)
#     axis(side=1, at=tix[c(-1, -ntix)], labels=tix[c(-1, -ntix)], cex.axis=1.2)
#   }

#   if (s==1 | s==5) {
#     tix <- seq(from=0, to=1.4, by=0.2)
#     ntix <- length(tix)
#     axis(side=2, at=tix[c(-1,-ntix)], labels=tix[c(-1,-ntix)], cex.axis=1.2)
#   }
# }

for (s in c(2,3,5,7,1,4,6)) {

  if (any(s==c(2,3,5,7))) {
    ylimm <- c(0,1.3)
  } 

  if (any(s==c(1,4,6))) {
    ylimm <- c(0,0.71)
  }
  plot(1, type="n", xlab="Year", ylab=expression(paste(sigma["OFL"],sep=" ")), 
       xlim=c(ProjStart[1]-1, ProjStart[length(ProjStart)]+2), ylim=ylimm,
       xaxt="n", yaxt="n",
       xaxs="i", yaxs="i", cex=1.2)

  if (any(s==c(2,3,5,7))) {
    if(s==5) {
      mtext(text="Pacific Ocean", side=3, line=-1.5, cex=1.2)
      mtext(text="perch", side=3, line=-3, cex=1.2)
    } else {
      mtext(text=Stocks[s], side=3, line=-1.5, cex=1.2)
    }
  } else {
    mtext(text=Stocks[s], side=3, line=-1.5, cex=1.2, outer=F)
  }
  
  for (i in 1:length(ProjStart)) {
    points(x=ProjStart[i], y=res$SigmaYS[i,s],
           pch=19, col="black"
    )
    
    arrows(ProjStart[i], res$YS.CI[[i]][[s]][1], ProjStart[i], res$YS.CI[[i]][[s]][2],
           length=0.05, angle=90, code=3, col="black")
  }
  
  abline(h=res$SigmaBarS[s],
         lwd=2, col="#004D40", lty=2)

  if (any(s==c(1,4,6))) {
    tix <- seq(from=ProjStart[1]-1, to=ProjStart[length(ProjStart)]+2, by=2)
    ntix <- length(tix)
    axis(side=1, at=tix[c(-1, -ntix)], labels=tix[c(-1, -ntix)], cex.axis=1.5)
  }

  if (s==2) {
    tix <- seq(from=0, to=1.4, by=0.2)
    ntix <- length(tix)
    axis(side=2, at=tix[c(-1,-ntix)], labels=tix[c(-1,-ntix)], cex.axis=1.2)
  }

  if (s==1) {
    tix <- seq(from=0, to=0.7, by=0.1)
    ntix <- length(tix)
    axis(side=2, at=tix[c(-1,-ntix)], labels=tix[c(-1,-ntix)], cex.axis=1.2)
  }
}

mtext(text=expression(paste(sigma["OFL"],sep=" ")), side=2, line=3, cex=1.5, outer=T)
mtext(text="Projection start year", side=1, line=3.5, cex=1.5, outer=T)

plot(1, type="n", xlab="Projection start year", ylab=expression(paste("OFL  ", sigma["y"],sep=" ")), 
     xlim=c(ProjStart[1]-1, ProjStart[length(ProjStart)]+2), ylim=c(0,0.71),
     xaxt="n", yaxt="n",
     xaxs="i", yaxs="i")

# plot(1, type="n", xlab="Projection start year", ylab=expression(paste("OFL  ", sigma["y"],sep=" ")), 
#      xlim=c(ProjStart[1]-1, ProjStart[length(ProjStart)]+2), ylim=c(0,1.4),
#      xaxt="n", yaxt="n",
#      xaxs="i", yaxs="i")


mtext(text="Pooled across", side=3, line=-1.5, cex=1.2)
mtext(text="species", side=3, line=-3, cex=1.2)
# mtext(text=expression(paste(sigma["y"],sep=" ")), side=3, line=-5, cex=1.25)
for (i in 1:length(ProjStart)) {
  points(x=ProjStart[i], y=res$SigmaY[i],
         pch=19, col="black"
  )
  
  arrows(ProjStart[i], res$SigmaY.CI[[i]][1], ProjStart[i], res$SigmaY.CI[[i]][2],
         length=0.05, angle=90, code=3, col="black")
}

abline(h=res$SigmaBar,
       lwd=2, col="#1E88E5", lty=3)

tix <- seq(from=ProjStart[1]-1, to=ProjStart[length(ProjStart)]+2, by=2)
ntix <- length(tix)
axis(side=1, at=tix[c(-1, -ntix)], labels=tix[c(-1, -ntix)], cex.axis=1.5)


legend(x="bottomright", legend=c("1 year projection", expression(bar(sigma["s"])), expression(bar(sigma))), 
  bty="n", 
  pch=(c(19,NA,NA)),
  lty=c(0,2,3), 
  col=c("black", "#004D40", "#1E88E5"), 
  lwd=c(0,2,2), 
  horiz=F,
  cex=1.2
)


dev.off()

