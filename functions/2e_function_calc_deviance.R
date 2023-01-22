calc_deviance <- function(tab_ext, cst){
  ktot <- aggregate(tab_ext[,4], by = tab_ext[ , c('X','Z')], FUN = sum)
  ind <- which(tab_ext[,4] != 0)
  dev <- cst + 2 * sum(tab_ext$freq[ind] * log(tab_ext$freq[ind])) -
    2 * sum(ktot$x * log(ktot$x))
}
