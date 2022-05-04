
Sigma_Table <- function(Dirn, ProjStart, Det) {

  setwd(Dirn)
  ncolm <- 2*length(ProjStart)

  for (t in 1:2) {
    if (t==1) {
      if (Det) {
        load("Deterministic Projections.Rdata")
        Stocks <- DAT[[1]]$SpawnB[1,which(DAT[[1]]$SpawnB[1,]!="")]
        load("DetSpawnB.Rdata")
        DAT <- DetSpawnB
        label <- "Det"
      } else {
        load("StoSpawnB.Rdata")
        DAT <- StoSpawnB
        label <- "Sto"
        load("1998_StochProjResults.Rdata")
        Stocks <- ProjRes[[1]][[1]][1,c(which(!is.na(ProjRes[[1]][[1]][1,]))[-1])]
      }
      title <- "Spawning Biomass"
    } else {
      if (Det) {
        load("Deterministic Projections.Rdata")
        Stocks <- DAT[[1]]$SpawnB[1,which(DAT[[1]]$SpawnB[1,]!="")]
        load("DetOFL.Rdata")
        DAT <- DetOFL
        label <- "Det"
      } else {
        load("StoOFL.Rdata")
        DAT <- StoOFL
        label <- "Sto"
        load("1998_StochProjResults.Rdata")
        Stocks <- ProjRes[[1]][[1]][1,c(which(!is.na(ProjRes[[1]][[1]][1,]))[-1])]
      }
      title <- "OFL"
    }
    
    mat <- matrix(nrow=11, ncol=ncolm+2)
    
    mat[1,1] <- "Species"
    mat[2,1] <- ""
    mat[3,1] <- "Pooled"
    mat[4,1] <- ""
    mat[5:11,1] <- as.character(Stocks)
    
    mat[1,2:(ncolm+1)] <- sort(rep(ProjStart, times=2))
    mat[2,2:(ncolm+1)] <- rep(c("SigmaY", "CI"), times=length(ProjStart))
    
    col.ind <- seq(from=2, to=ncolm, by=2)
    col.ind2 <- seq(from=3, to=ncolm+1, by=2)
    
    for (i in 1:length(ProjStart)) {
      mat[3,col.ind[i]] <- round(DAT$SigmaY[i], digits=3)
      mat[3,col.ind2[i]] <- paste(round(DAT$SigmaY.CI[[i]], digits=3), collapse=", ")
    }
    
    mat[4,2:(ncolm+1)] <- rep(c("SigmaYS", "CI"), times=length(ProjStart))
    
    for (s in 1:(length(Stocks))) {
      mat[4+s,col.ind] <- round(DAT$SigmaYS[,s], digits=3)
    }
    
    for (y in 1:length(ProjStart)) {
      for (s in 1:length(Stocks)) {
        mat[4+s,col.ind2[y]] <- paste(round(DAT$YS.CI[[y]][[s]], digits=3), collapse=", ")
      }
    }
    
    mat[2,ncolm+2] <- "SigmaBar"
    mat[3,ncolm+2] <- round(DAT$SigmaBar, digits=3)
    mat[4,ncolm+2] <- "SigmaBarS"
    mat[5:11,ncolm+2] <- round(DAT$SigmaBarS, digits=3)
    
    write.csv(mat, file=paste(label,title,"SigmaAnalysisTables.csv"), row.names=F)
  }

}

