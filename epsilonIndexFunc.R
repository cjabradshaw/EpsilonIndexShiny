epsilonIndexFunc <- function(datsamp, sortindex='e') { 
  
  ## transforms
  datsamp[,2] <- as.character(datsamp[,2])
  datsamp[,1] <- as.character(datsamp[,1])
  ye <- as.numeric(format(Sys.Date(), "%Y")) - datsamp[,6]
  lc10 <- ifelse(datsamp[,3]==0, 0, log(datsamp[,3]))
  lch <- log(datsamp[,4])
  lcmax <- rep(log(1), length(lch))
  li10 <- rep(log(10), length(lch))
  lih <- log(datsamp[,4])
  limax <- log(datsamp[,5])
  mi <- round(datsamp[,4]/ye,4)
  
  ## power-law relationship
  lis.out <- lcs.out <- pers.out <- rep(NA,1)
  xypts.out <- matrix(NA,nrow=2, ncol=2)
  av <- bv <- NA
  for (p in 1:dim(datsamp)[1]) {
    lis <- as.numeric(c(li10[p],lih[p],limax[p]))
    lis.out <- c(lis.out, lis)
    lcs <- as.numeric(c(lc10[p],lch[p],lcmax[p]))
    lcs.out <- c(lcs.out, lcs)
    pers <- rep(datsamp[p, 4],3)
    pers.out <- c(pers.out,pers)
    fitp <- lm(lcs ~ lis)
    av[p] <- coef(fitp)[1]
    bv[p] <- coef(fitp)[2]
    ystart.pt <- av[p] + bv[p]
    yend.pt <- 0
    xpts <- c(1,lis[3])
    ypts <- c(ystart.pt, yend.pt)
    xypts <- cbind(xpts,ypts)
    colnames(xypts) <- c(paste(pers[1],"x",sep=""), paste(pers[1],"y",sep=""))
    xypts.out <- cbind(xypts.out, xypts)
  }

  ## area under the curve
  Alin <- NA
  for (q in 1:dim(datsamp)[1]) {
    if (limax[q] > 1) {
      li.cont <- seq(1, limax[q], 0.05)
      pred.lin <- av[q] + bv[q]*(li.cont)
      Alin[q] <- sum(pred.lin)/(length(li.cont)*(max(limax)))}
    if (limax[q] < 1) {
      Alin[q] <- 0}
  }

  ## residual ranking
  fit.yAlin <- lm(Alin ~ log(ye))
  if (coef(fit.yAlin)[2] < 0)  {
    fit.yAlin <- lm(Alin ~ 0 + log(ye))
  }

  ## calculate expectation relative to sample
  expectation <- as.character(ifelse(resid(fit.yAlin) > 0, "above", "below"))
  dat.out <- data.frame(datsamp[,1], datsamp[,2], ye, resid(fit.yAlin), expectation, mi, datsamp[,4])
  dat.sort1 <- dat.out[order(dat.out[,4],decreasing=T),1:7]
  Rnk <- seq(1,length(datsamp[,1]),1)
  dat.sort <- data.frame(dat.sort1,Rnk)
  colnames(dat.sort) <- c("ID","gen","yrsP", "e","exp","m","h", "Rnk")
  dat.sort[,1] <- as.character(dat.sort[,1])
  dat.sort[,2] <- as.character(dat.sort[,2])
  dat.sort[,5] <- as.character(dat.sort[,5])

  ## gender-debiased ε-index 
  # women
  dat.comb <- data.frame(datsamp,ye,lc10,lch,lcmax,li10,lih,limax,mi,av,bv,Alin)
  colnames(dat.comb)[1:6] <- c("ID","gen","i10","h","maxcit","firstyrpub")
  datsampF <- subset(dat.comb, gen=="F")
  fitF.yAlin <- lm(datsampF[,17] ~ log(datsampF[,7]))
  if (coef(fitF.yAlin)[2] < 0)  {
    fitF.yAlin <- lm(datsampF[,17] ~ 0 + log(datsampF[,7]))
  }

  ## calculate expectation relative to sample
  expectationF <- as.character(ifelse(resid(fitF.yAlin) > 0, "above", "below"))
  datF.out <- data.frame(datsampF[,1], datsampF[,2], datsampF[,7], round(resid(fitF.yAlin),4), expectationF, datsampF[,14], datsampF[,4])
  datF.sort1 <- datF.out[order(datF.out[,4],decreasing=T),1:7]
  rankF <- seq(1,length(datsampF[,1]),1)
  datF.sort <- data.frame(datF.sort1,rankF)
  colnames(datF.sort) <- c("ID","gen","yrsP", "e","exp","m","h", "genRnk")
  datF.sort[,1] <- as.character(datF.sort[,1])
  datF.sort[,2] <- as.character(datF.sort[,2])
  datF.sort[,5] <- as.character(datF.sort[,5])
  
  # men
  datsampM <- subset(dat.comb, gen=="M")
  fitM.yAlin <- lm(datsampM[,17] ~ log(datsampM[,7]))
  if (coef(fitM.yAlin)[2] < 0)  {
    fitM.yAlin <- lm(datsampM[,17] ~ 0 + log(datsampM[,7]))
  }
  
  ## calculate expectation relative to sample
  expectationM <- as.character(ifelse(resid(fitM.yAlin) > 0, "above", "below"))
  datM.out <- data.frame(datsampM[,1], datsampM[,2], datsampM[,7], round(resid(fitM.yAlin),4), expectationM, datsampM[,14], datsampM[,4])
  datM.sort1 <- datM.out[order(datM.out[,4],decreasing=T),1:7]
  rankM <- seq(1,length(datsampM[,1]),1)
  datM.sort <- data.frame(datM.sort1,rankM)
  colnames(datM.sort) <- c("ID","gen","yrsP", "e","exp","m","h", "genRnk")
  datM.sort[,1] <- as.character(datM.sort[,1])
  datM.sort[,2] <- as.character(datM.sort[,2])
  datM.sort[,5] <- as.character(datM.sort[,5])
  
  # combine women & men subsets & re-rank
  datFM <- rbind(datF.sort,datM.sort)
  datFM.sort1 <- datFM[order(datFM[,4],decreasing=T),1:8]
  debRnk <- seq(1,length(datFM.sort1[,1]),1)
  datFM.sort <- data.frame(datFM.sort1,debRnk)
  colnames(datFM.sort)[1:8] <- colnames(datFM)
  colnames(datFM.sort)[4] <- "genE"
  datFM.sort[,4] <- round(datFM.sort[,4],4)
  
  # add rank from pooled sample
  orig.rank <- dat.sort[,c(1,4,8)]
  datFM.mrg <- merge(datFM.sort, orig.rank, by="ID", all=F, no.dups=T)
  colnames(datFM.mrg)[10] <- "poolE"
  datFM.mrg[,10] <- round(datFM.mrg[,10], 4)
  colnames(datFM.mrg)[11] <- "poolRnk"
  full.out1 <- datFM.mrg[order(datFM.mrg[,9],decreasing=F), 1:11]  
  
  origdebias.fit <- lm(full.out1[,11]~full.out1[,9])

  ## scale (normalise)
  eP <- round(scale(full.out1[,10], scale=T, center=F), 4)
  debEP <- round(scale(full.out1[,4], scale=T, center=F), 4)
  full.out <- data.frame(full.out1,eP,debEP)
  
  # sort on desired metric & recalculate expectation based on sort metric
  # 'e' = pooled; 'ep' = normalised; 'd' = gender-debiased; 'dp' = normalised gender-debiased 
  if (sortindex == 'd') {
    sortout <- full.out[order(full.out[,9],decreasing=F), 1:13]}
  if (sortindex == 'e') {
    sortout <- full.out[order(full.out[,10],decreasing=T), 1:13]
    sortout[,5] <- as.character(ifelse(sortout[,10] > 0, 'above', 'below'))}
  if (sortindex == 'ep') {
    sortout1 <- full.out[order(full.out[,12],decreasing=T), 1:13]
    sortout1[,5] <- ifelse(sortout1[,12] > 0, 'above', 'below')
    ePRnk <- seq(1,dim(sortout1)[1], by=1)
    sortout <- data.frame(sortout1,ePRnk)}
  if (sortindex == 'dp') {
    sortout1 <- full.out[order(full.out[,13],decreasing=T), 1:13]
    sortout1[,5] <- ifelse(sortout1[,13] > 0, 'above', 'below')
    ePdebRnk <- seq(1,dim(sortout1)[1], by=1)
    sortout <- data.frame(sortout1,ePdebRnk)}
  
  # print final output
  return(sortout)
  
} # end epsilonIndexFunc
