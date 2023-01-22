# load the results from script 2 and the distributions from script 1
load("workspaces/results_pilot_simulation.RData")
load("workspaces/distributions.RData")

# create data frame in which the results need to be stored. 
results_data <- data.frame(matrix(nrow = n_conditions*n_datasets, ncol = 21))
colnames(results_data) <- 
  c("condition", "dataset", "convergence", "iterations", "added_units", 
    "removed_units", "deviance_before", "deviance_after", "deviance_after_rounded",
    "bias_PW1", "bias_PW2", "bias_PW3", "bias_PY1W1", "bias_PY2W1", "bias_PY3W1", 
    "bias_PY1W2", "bias_PY2W2", "bias_PY3W2", "bias_PY1W3", "bias_PY2W3", "bias_PY3W3")

# set number of conditions and number of datasets 
n_conditions <- 8
n_datasets <- 1000

k <- 0 
# loop over conditions and data sets to fill the data frame with the results
for (i in 1:n_conditions){
  popW  <- aggregate(prob ~ W, data = generated_distributions[[i]], sum)
  popYW <- aggregate(prob ~ Y + W, data = generated_distributions[[i]], sum) 
  popYW[1:3,"prob"] <- popYW[1:3,"prob"]/sum(popYW[1:3,"prob"])
  popYW[4:6,"prob"] <- popYW[4:6,"prob"]/sum(popYW[4:6,"prob"])
  popYW[7:9,"prob"] <- popYW[7:9,"prob"]/sum(popYW[7:9,"prob"])
  for (j in 1:n_datasets){
    ind <- j + k
    results_data[ind, "condition"] <- i
    results_data[ind, "dataset"] <- j
    results_data[ind, "convergence"] <- results[[i]][[j]][[1]]
    results_data[ind, "iterations"] <- results[[i]][[j]][[2]]
    results_data[ind, "added_units"] <- results[[i]][[j]][[3]]
    results_data[ind, "removed_units"] <- results[[i]][[j]][[4]]
    results_data[ind, "deviance_before"] <- results[[i]][[j]][[5]]
    results_data[ind, "deviance_after"] <- results[[i]][[j]][[6]]
    results_data[ind, "deviance_after_rounded"] <- results[[i]][[j]][[7]]
    results_data[ind, "bias_PW1"] <- (popW[1, "prob"] - results[[i]][[j]][[8]][1,1])
    results_data[ind, "bias_PW2"] <- (popW[2, "prob"] - results[[i]][[j]][[8]][1,1])
    results_data[ind, "bias_PW3"] <- (popW[3, "prob"] - results[[i]][[j]][[8]][1,1])
    results_data[ind, "bias_PY1W1"] <- (popYW[1, "prob"] - results[[i]][[j]][[9]][1,1])
    results_data[ind, "bias_PY2W1"] <- (popYW[2, "prob"] - results[[i]][[j]][[9]][2,1])
    results_data[ind, "bias_PY3W1"] <- (popYW[3, "prob"] - results[[i]][[j]][[9]][3,1])
    results_data[ind, "bias_PY1W2"] <- (popYW[4, "prob"] - results[[i]][[j]][[9]][4,1])
    results_data[ind, "bias_PY2W2"] <- (popYW[5, "prob"] - results[[i]][[j]][[9]][5,1])
    results_data[ind, "bias_PY3W2"] <- (popYW[6, "prob"] - results[[i]][[j]][[9]][6,1])
    results_data[ind, "bias_PY1W3"] <- (popYW[7, "prob"] - results[[i]][[j]][[9]][7,1])
    results_data[ind, "bias_PY2W3"] <- (popYW[8, "prob"] - results[[i]][[j]][[9]][8,1])
    results_data[ind, "bias_PY3W3"] <- (popYW[9, "prob"] -results[[i]][[j]][[9]][9,1])
  }
  k <- k + n_datasets
}

# save the processed results of the pilot simulation
save(results_data, file = "workspaces/processed_results.RData")

