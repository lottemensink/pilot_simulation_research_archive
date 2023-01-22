generate_starts <- function(tab){
  # calculate n_i++, totals from covariate X 
  tot <- aggregate(tab$freq, by = tab[, "X", drop = FALSE], FUN = sum)
  names(tot) <- c("X", "tot")
  # calculate n_ij+, totals for combinations X and Y
  rtot <- aggregate(tab$freq, by = tab[, c("X", "Y"), drop = FALSE], FUN = sum)
  names(rtot) <- c("X", "Y", "rtot")
  # calculate n_i+k, totals for combinations X and Z
  ktot <- aggregate(tab$freq, by = tab[,c("X", "Z")], FUN = sum)
  names(ktot) <- c("X", "Z", "ktot")
  
  # merge tot, rtot and ktot with tab
  tab0 <- merge(tab, tot, by = "X")
  tab0 <- merge(tab0, rtot, by = c("X", "Y"))
  tab0 <- merge(tab0, ktot, by = c("X", "Z"))
  
  # compute individual contribution of every XY combination to the deviance
  tab0$contr <- log(tab0$freq) + log(tab0$tot) - log(tab0$rtot) - log(tab0$ktot)
  
  
  # order tab0 in the same way that tab is ordered
  tab0 <- tab0[order(tab0$Z, tab0$Y, tab0$X),]

  # if the contribution is negative, the current freq is too small. In this
  # case, we want additional units in the sample for this XY combination. Hence, 
  # we set deltaplus to 1 and deltamin to zero
  # if the contribution is positive, the current freq is too large. In this case, 
  # we want to remove units from the sample for this XY combination. Hence, 
  # we set deltaplus to 0 and deltamin to 1
  dplus <- (tab0$contr < 0)[tab0$Z == 1]
  dmin <- (tab0$contr > 0)[tab0$Z == 1]
  x0 <- rep(0, nrow(tab))
  x0[c(dplus,dmin)] <- 1
  
  return(c(x0))
}
