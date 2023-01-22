prop_W_and_WY <- function(WYX_new, tab_ext){
  
  # 1. estimate the distribution of W P(W) using P(X) * P(W|X) in the audit sample
  
  # first, we estimate P(W|X) = P(W = w, X = x) / P(X = x) 
  tab_WX_new       <- aggregate(freq ~ X + W, data = WYX_new, sum)
  tab_X_new        <- aggregate(freq ~ X, data = WYX_new, sum)
  tab_WX_new       <- merge(tab_WX_new, tab_X_new, by = 'X', all = TRUE,
                            suffixes = c('.wx','.x'))
  tab_WX_new$p.wcx <- tab_WX_new$freq.wx / tab_WX_new$freq.x
  
  # we assume that X is observed for the whole population, so we know X for the 
  # whole population, instead of only in the audit sample. We estimate P(X = x)
  # for the entire population
  X_pop            <- aggregate(freq ~ X, data = tab_ext, sum)
  X_pop$p.x        <- X_pop$freq/sum(tab_ext$freq)
  tab_WX_new       <- merge(tab_WX_new, X_pop[, c('X','p.x')], by = 'X', all = TRUE)
  
  # calculate proportion term and variance term for every combination of X and W
  tab_WX_new$term.prop <- tab_WX_new$p.x * tab_WX_new$p.wcx
  tab_WX_new$term.var  <- (tab_WX_new$p.x^2 / tab_WX_new$freq.x) * tab_WX_new$p.wcx * (1 - tab_WX_new$p.wcx)
  
  # create infrastructure to store P(W = w)
  W_probs           <- matrix(NA, length(unique(WYX_new$W)), 2)
  colnames(W_probs) <- c("prop", "var")
  rownames(W_probs) <- 1:nrow(W_probs)
  
  # loop over the number of categories in W, store terms for every category
  for (w in 1:nrow(W_probs)){
    # store proportions
    W_probs[w,1] <- sum(tab_WX_new$term.prop[tab_WX_new$W == w])
    # store variances
    W_probs[w,2] <-  sum(tab_WX_new$term.var[tab_WX_new$W == w])
  }
  
  
  # 2. estimate distribution P(Y = y | W = w) using the formula:
  # (P(X)*P(Y,W|X))/(P(X)*P(W|X))
  
  # first, we estimate P(X = x, W = w | Y = y), all other terms are already computed
  tab_WXY_new        <- aggregate(freq ~ X + Y + W, data = WYX_new, sum)
  tab_WXY_new        <- merge(tab_WXY_new, tab_WX_new[ , !(names(tab_WX_new) %in% c('term.prop','term.var'))],
                              by = c('W','X'), all = TRUE)
  tab_WXY_new$p.wycx <- tab_WXY_new$freq / tab_WXY_new$freq.x 
  
  # calculate the nominator of the formula P(X)*P(Y,W|X)
  tab_WXY_new$term.prop <- tab_WXY_new$p.x * tab_WXY_new$p.wycx
  # calculate the various terms in the formula for the variance
  tab_WXY_new$term1.var <- (tab_WXY_new$p.x^2 / tab_WXY_new$freq.x) * tab_WXY_new$p.wycx * (1 - tab_WXY_new$p.wycx)
  tab_WXY_new$term2.var <- (tab_WXY_new$p.x^2 / tab_WXY_new$freq.x) * tab_WXY_new$p.wcx * (1 - tab_WXY_new$p.wcx)
  tab_WXY_new$term3.var <- (tab_WXY_new$p.x^2 / tab_WXY_new$freq.x) * tab_WXY_new$p.wycx * (1 - tab_WXY_new$p.wycx)
  
  # create infrastructure to store P(X|W)
  YW_probs <- matrix(NA, 9, 2)
  rownames(YW_probs) <- c("Y1W1","Y2W1","Y3W1","Y1W2","Y2W2","Y3W2","Y1W3","Y2W3","Y3W3")
  colnames(YW_probs) <- c("prop","var")
  
  # loop over all different combinations of X and W, store terms for every combination
  for (k in 1:nrow(YW_probs)){
    # obtain second character in string, representing x category
    y <- as.integer(substr(rownames(YW_probs)[k],2,2))
    # obtain fourth character in string, representing w category
    w <- as.integer(substr(rownames(YW_probs)[k],4,4))
    
    # calculate proportions
    YW_probs[k,1] <- sum(tab_WXY_new$term.prop[tab_WXY_new$Y == y & tab_WXY_new$W == w]) / W_probs[w,1]
    
    # calculate variances
    YW_probs[k,2] <- ( sum(tab_WXY_new$term1.var[tab_WXY_new$Y == y & tab_WXY_new$W == w]) +
                         (YW_probs[k,1])^2 * sum(tab_WXY_new$term2.var[tab_WXY_new$Y == y & tab_WXY_new$W == w]) +
                         (-2 * YW_probs[k,1]) * sum(tab_WXY_new$term3.var[tab_WXY_new$Y == y & tab_WXY_new$W == w]) ) / (W_probs[w,1])^2
    
  }
  
  return(list(W_probs, YW_probs))
  
  
}
