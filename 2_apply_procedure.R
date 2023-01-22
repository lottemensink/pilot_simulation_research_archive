# set random seed (distribution WYX is generated randomly after applying procedure)
set.seed(123)

# load nloptr for minimization problem
library(nloptr)

# load data and distributions from script 1_data_generation.R
load("workspaces/datasets.RData")
load("workspaces/distributions.RData")

# load the functions 
source("functions/2a_function_apply_constraints.R")
source("functions/2b_function_generate_starts.R")
source("functions/2c_function_generate_W.R")
source("functions/2d_function_prop_W_and_WY.R")
source("functions/2e_function_calc_deviance.R")

# specify the number of conditions, data sets, and the results to store
n_conditions <- 8
n_datasets <- 1000
n_results <- 9

# the results that are stored in this simulation study are the following: 
# convergence status, iterations, number of added units, number of removed 
# units, deviance before, deviance afterwards, p(W = w), p(Y = y | W = w)

# create infrastructure to store all the results 
results <- vector(mode = "list", length = n_conditions)
for(i in 1:n_conditions){
  results[[i]] <- vector(mode = "list", length = n_datasets)
}

# loop over conditions
for(i in 1:n_conditions){
  
  # loop over the 1000 data sets within the conditions
  for(j in 1:n_datasets){
    
    # print the condition number and the data set number
    cat(sprintf('Condition %d data set %d \n', i, j))
    flush.console()
    
    # select dataset j from condition i
    data <- generated_datasets[[i]][,c(1:4,j+4)]
    
    # create tab for selected data set
    tab <- aggregate(data[,5], by = list(X = data$X, 
                                         Y = data$Y, 
                                         Z = data$Z), 
                     FUN  = sum, drop = TRUE)
    colnames(tab) = c("X","Y","Z","freq")
    
    # the constant term of the deviance does not depend on the procedure 
    # and can already be computed
    tot  <- aggregate(tab$freq, by = tab[ , "X", drop = FALSE], FUN = sum)
    rtot <- aggregate(tab$freq, by = tab[ , c("X","Y")], FUN = sum)
    cst <- 2 * sum(tot$x * log(tot$x), na.rm=TRUE) - 2 * sum(rtot$x * log(rtot$x), na.rm=TRUE)
    
    # set the two user constraints: on deviance and on minimal sample size
    # we want a non-linear deviance constraint: the deviance for each model should 
    # be below a boundary that is specified by the user via alpha
    alpha <- 0.05
    I <- 3
    J <- 3 
    # compute the first constraint: user-specified deviance threshold
    devbound <- qchisq(p = 1 - alpha, df = J * (I-1), lower.tail = TRUE)
    # set second constraint: minimum size of the audit sample
    min_sample <- 0
    
    # nloptr() function can only be used with numeric, tables not integers
    # convert table to numeric:
    tab[] <- lapply(tab, as.numeric)
    
    # get apply_constraints function in the right form for the nloptr function
    hin <- function(x) apply_constraints(x, bound = devbound, min_sample = min_sample)
    .hin <- match.fun(hin)
    hin <- function(x) (-1) * .hin(x)
    hinjac <- function(x) nl.jacobian(x, hin)
    
    # set the right options for the nloptr function 
    opts <- nl.opts()
    opts$algorithm <- "NLOPT_LD_AUGLAG"
    opts$tol_constraints_ineq <- c(1e-2, 1e-2)
    opts$local_opts <- list("algorithm" = "NLOPT_LD_SLSQP", "eval_grad_f" = NULL, 
                            xtol_rel = 1e-2, "maxeval" = 1000, "tol_constraints_ineq" = 1e-2,
                            ftol_abs = 1e-6, ftol_rel = 1e-6)
    
    # create matrix for different sets of starting values
    x0_matrix <- matrix(ncol = 18, nrow = 5)
    # fill matrix with starting values possibilities
    x0_matrix[1,] <- generate_starts(tab)
    x0_matrix[2,] <- c(rep(1, nrow(tab)/2), rep(0, nrow(tab)/2))
    x0_matrix[3,] <- c(rep(0, nrow(tab)/2), rep(0, nrow(tab)/2))
    x0_matrix[4,] <- c(rep(1, nrow(tab)/2), as.integer(tab$freq[which(tab$Z == 1)] != 0))
    x0_matrix[5,] <- c(rep(0, nrow(tab)/2), as.integer(tab$freq[which(tab$Z == 1)] != 0))
    
    for (k in 1:nrow(x0_matrix)){
    
      # apply the constrained minimization procedure using the nloptr function
      sol <- nloptr(
        # specify starting values, sample 1 additional unit for each combination XY
        x0 = x0_matrix[k, ],
        # specify target function: the sum of delta+ and delta- values
        eval_f = function(x) { sum(x) },
        # specify the derivative of the target function
        eval_grad_f = function(x) { rep(1, nrow(tab)) },
        # specify lower bounds for the solution: the delta values cannot be below 0
        lb = rep(0, nrow(tab)),
        # specify upper bounds for the solution: there can't be more units moved 
        # from Z = 0 to Z = 1 then are initially in Z = 0, and vice versa
        ub = tab$freq[c(which(tab$Z == 0), which(tab$Z == 1))],
        # non-linear constraint on the deviance and minimum sample
        eval_g_ineq = hin,
        eval_jac_g_ineq = hinjac,
        # set the prespecified options
        opts = opts)
      
      print(sol$status)
      
      if(sol$status > 0){break}
      
    }
  
    # obtain solution, delta plus and delta min values
    solution <- sol$solution
    deltaplus <- solution[1:(length(solution)/2)]
    deltamin <- solution[(1+(length(solution)/2)):length(solution)]
    
    # create extended frequency table, with original freq and the new freq from solution
    tab_ext <- cbind(tab, freq_new = tab$freq +
                       c(-deltaplus, -deltamin) + 
                       c(deltamin, deltaplus))
    
    # generate distribution WYX as if the audit procedure from the solution were
    # to be applied to the data set from this iteration
    WYX <- generate_W(data, tab_ext)
    props <- prop_W_and_WY(WYX, tab_ext)
    
    # store convergence
    conv <- sol$status
    # store number of iterations to reach convergence 
    iter <- sol$iterations
    # store number of added units
    add_units <- sum(deltaplus)
    # store number of removed units
    rem_units <- sum(deltamin)
    # store deviance before 
    dev_before <- calc_deviance(tab, cst)
    # store deviance afterwards 
    dev_after <- calc_deviance(tab_ext[,c(1:3,5)], cst)
    # store deviance afterwards (after rounding)
    dev_after_rounded <- calc_deviance(cbind(tab_ext[,1:3], round(tab_ext[5],0)), cst)
    # store p(W = w)
    pw <- props[[1]]
    # store p(Y = y | W = w)
    pycw <- props[[2]]
    
    # store all the results in list 
    reslist <- list(conv,
                    iter,
                    add_units, 
                    rem_units, 
                    dev_before, 
                    dev_after,
                    dev_after_rounded,
                    pw, 
                    pycw)
    results[[i]][[j]] <- reslist
    
  }
}

save(results, file = "workspaces/results_pilot_simulation.RData")

