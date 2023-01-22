# load ggplot to make the plots
library(ggplot2)

# load the processed results from the pilot simulation study
load("workspaces/processed_results.RData")

# load the results of the original simulation study
load("original_simulation_results/results_data_original.RData")

# get the data in the right structure for plotting
total_results <- rbind(results_data, results_data_original)
total_results$study <- as.factor(c(rep("Sample Size Approach", 8000), rep("Deviance Approach", 8000)))
total_results$condition <- as.factor(total_results$condition)
levels(total_results$condition) <- c("WY1WX1YZ1", "WY2WX1YZ1", "WY1WX2YZ1",
                    "WY2WX2YZ1","WY1WX1YZ2","WY2WX1YZ2","WY1WX2YZ2","WY2WX2YZ2")

# for manuscript: plot added units over the conditions
ggplot(aes(x = condition, y = added_units, fill = study), data = total_results) +
  geom_boxplot() +
  theme_classic() + 
  scale_fill_grey() +
  xlab("Condition") +
  ylab("Added Units") +
  theme(legend.title = element_blank(), text = element_text(size = 18),
        axis.text.x = element_text(angle = -30, hjust = 0))

# for manuscript: plot deviance over the conditions
ggplot(aes(x = condition, y = deviance_after, fill = study), data = total_results) +
  geom_boxplot() +
  theme_classic() + 
  scale_fill_grey() +
  xlab("Condition") +
  ylab("Deviance") +
  theme(legend.title = element_blank(), text = element_text(size = 18),
        axis.text.x = element_text(angle = -30, hjust = 0)) +
  geom_hline(aes(yintercept = 12.59, color = "Deviance Threshold"), linetype = "dotted")

# for manuscript: plot bias in P(W=1) over the conditions
ggplot(aes(x = condition, y = bias_PW1, fill = study), data = total_results) +
  geom_boxplot() +
  theme_classic() + 
  scale_fill_grey() +
  xlab("Condition") +
  ylab("Bias P(W=1)") +
  theme(legend.title = element_blank(), text = element_text(size = 18),
        axis.text.x = element_text(angle = -30, hjust = 0)) 
