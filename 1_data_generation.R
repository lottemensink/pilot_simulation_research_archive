# load dplyr for operations on dataframes
library(dplyr)

# set random seed for data generation
set.seed(123)

# number of conditions
wy_ncon = c(1, 2)
wx_ncon = c(1, 2)
yz_ncon = c(1, 2)

conditions = list(wy_ncon = wy_ncon, wx_ncon = wx_ncon, yz_ncon = yz_ncon)
xyzw_con = expand.grid(conditions)

WY = list()
WY[[1]] = matrix(c(1/3, 0, 0, 
                   0, 1/3, 0,
                   0, 0, 1/3), nrow = 3, ncol = 3, byrow = TRUE)
WY[[2]] = matrix(c(.9/3, .05/3, .05/3, 
                   .1/3, .8/3, .1/3,
                   .15/3, .15/3, .7/3), nrow = 3, ncol = 3, byrow = TRUE)
WX = list()
WX[[1]] = matrix(c(.8/3, .1/3, .1/3, 
                   .1/3, .8/3, .1/3,
                   .1/3, .1/3, .8/3), nrow = 3, ncol = 3, byrow = TRUE)
WX[[2]] = matrix(c(.8/3, .1/3, .1/3, 
                   .2/3, .6/3, .2/3,
                   .3/3, .3/3, .4/3), nrow = 3, ncol = 3, byrow = TRUE)

YZ = list()
YZ[[1]] <- matrix(c(.97/3, .01,
                    .97/3, .01,
                    .97/3, .01), nrow = 3, ncol = 2, byrow = TRUE)

YZ[[2]] <- matrix(c(.97/3, .018,
                    .97/3, .010,
                    .97/3, .002), nrow = 3, ncol = 2, byrow = TRUE)
X = c(1,2,3)
Y = c(1,2,3)
W = c(1,2,3)
Z = c(0,1)

variables = list(Y = Y, X = X, W = W, Z=Z)
XYWZ = expand.grid(variables)

generated_datasets = list()
generated_distributions = list()

# loop over all conditions (combinations of different relations between X Y W Z )
for (i in 1:nrow(xyzw_con)){
  for (j in 1:nrow(XYWZ)){
    # if Y=... and W=... find the corresponding probability in the matrix WY
    if(XYWZ[j,'W'] == 1 & XYWZ[j,'Y'] == 1){XYWZ[j,'WY'] = WY[[as.numeric(paste0(xyzw_con[i,1]))]][1,1]}
    if(XYWZ[j,'W'] == 1 & XYWZ[j,'Y'] == 2){XYWZ[j,'WY'] = WY[[as.numeric(paste0(xyzw_con[i,1]))]][1,2]}
    if(XYWZ[j,'W'] == 1 & XYWZ[j,'Y'] == 3){XYWZ[j,'WY'] = WY[[as.numeric(paste0(xyzw_con[i,1]))]][1,3]}
    if(XYWZ[j,'W'] == 2 & XYWZ[j,'Y'] == 1){XYWZ[j,'WY'] = WY[[as.numeric(paste0(xyzw_con[i,1]))]][2,1]}
    if(XYWZ[j,'W'] == 2 & XYWZ[j,'Y'] == 2){XYWZ[j,'WY'] = WY[[as.numeric(paste0(xyzw_con[i,1]))]][2,2]}
    if(XYWZ[j,'W'] == 2 & XYWZ[j,'Y'] == 3){XYWZ[j,'WY'] = WY[[as.numeric(paste0(xyzw_con[i,1]))]][2,3]}
    if(XYWZ[j,'W'] == 3 & XYWZ[j,'Y'] == 1){XYWZ[j,'WY'] = WY[[as.numeric(paste0(xyzw_con[i,1]))]][3,1]}
    if(XYWZ[j,'W'] == 3 & XYWZ[j,'Y'] == 2){XYWZ[j,'WY'] = WY[[as.numeric(paste0(xyzw_con[i,1]))]][3,2]}
    if(XYWZ[j,'W'] == 3 & XYWZ[j,'Y'] == 3){XYWZ[j,'WY'] = WY[[as.numeric(paste0(xyzw_con[i,1]))]][3,3]}
    # if X=... and Y=... find the corresponding probability in the matrix WY
    if(XYWZ[j,'W'] == 1 & XYWZ[j,'X'] == 1){XYWZ[j,'WX'] = WX[[as.numeric(paste0(xyzw_con[i,2]))]][1,1]}
    if(XYWZ[j,'W'] == 1 & XYWZ[j,'X'] == 2){XYWZ[j,'WX'] = WX[[as.numeric(paste0(xyzw_con[i,2]))]][1,2]}
    if(XYWZ[j,'W'] == 1 & XYWZ[j,'X'] == 3){XYWZ[j,'WX'] = WX[[as.numeric(paste0(xyzw_con[i,2]))]][1,3]}
    if(XYWZ[j,'W'] == 2 & XYWZ[j,'X'] == 1){XYWZ[j,'WX'] = WX[[as.numeric(paste0(xyzw_con[i,2]))]][2,1]}
    if(XYWZ[j,'W'] == 2 & XYWZ[j,'X'] == 2){XYWZ[j,'WX'] = WX[[as.numeric(paste0(xyzw_con[i,2]))]][2,2]}
    if(XYWZ[j,'W'] == 2 & XYWZ[j,'X'] == 3){XYWZ[j,'WX'] = WX[[as.numeric(paste0(xyzw_con[i,2]))]][2,3]}
    if(XYWZ[j,'W'] == 3 & XYWZ[j,'X'] == 1){XYWZ[j,'WX'] = WX[[as.numeric(paste0(xyzw_con[i,2]))]][3,1]}
    if(XYWZ[j,'W'] == 3 & XYWZ[j,'X'] == 2){XYWZ[j,'WX'] = WX[[as.numeric(paste0(xyzw_con[i,2]))]][3,2]}
    if(XYWZ[j,'W'] == 3 & XYWZ[j,'X'] == 3){XYWZ[j,'WX'] = WX[[as.numeric(paste0(xyzw_con[i,2]))]][3,3]}
    # If Y=.. and Z=... find the corresponding probability in the matrix WZ
    if(XYWZ[j,'Y'] == 1 & XYWZ[j,'Z'] == 0){XYWZ[j,'YZ'] = YZ[[as.numeric(paste0(xyzw_con[i,3]))]][1,1]}
    if(XYWZ[j,'Y'] == 1 & XYWZ[j,'Z'] == 1){XYWZ[j,'YZ'] = YZ[[as.numeric(paste0(xyzw_con[i,3]))]][1,2]}
    if(XYWZ[j,'Y'] == 2 & XYWZ[j,'Z'] == 0){XYWZ[j,'YZ'] = YZ[[as.numeric(paste0(xyzw_con[i,3]))]][2,1]}
    if(XYWZ[j,'Y'] == 2 & XYWZ[j,'Z'] == 1){XYWZ[j,'YZ'] = YZ[[as.numeric(paste0(xyzw_con[i,3]))]][2,2]}
    if(XYWZ[j,'Y'] == 3 & XYWZ[j,'Z'] == 0){XYWZ[j,'YZ'] = YZ[[as.numeric(paste0(xyzw_con[i,3]))]][3,1]}
    if(XYWZ[j,'Y'] == 3 & XYWZ[j,'Z'] == 1){XYWZ[j,'YZ'] = YZ[[as.numeric(paste0(xyzw_con[i,3]))]][3,2]}
  }
  
  # arrange XYWZ in the same way as original simulation study, in order to obtain
  # exactly the same data sets
  XYWZ <- arrange(XYWZ, desc(Z))
  
  # assuming independence (relation between XW is independent of relation XY and YZ, we multiply the 3 probabilities
  # multiply the number by 9 to make all final probabilities sum to one)
  XYWZ[,'prob']                = (XYWZ[,'WY']*XYWZ[,'WX']*XYWZ[,'YZ'])*9
  generated_distributions[[i]] = XYWZ
  
  # now generating 1000 dataset per simulation condition
  generated_datasets[[i]] = cbind(XYWZ[,1:4], rmultinom(1000, size = 10000, prob = XYWZ[,'prob']))
  
}

save(generated_distributions, file = "workspaces/distributions.RData")
save(generated_datasets,      file = "workspaces/datasets.RData")
