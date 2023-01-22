# pilot_simulation_research_archive
This repository contains all the necessary files to replicate the simulation study for the manuscript “A Comparison Between Two Approaches for Efficient Audit Sample Selection” by Lotte Mensink, Laura Boeschoten, and Sander Scholtus. 

It is important to note that this simulation study is largely based on the simulation study by Laura Boeschoten and Sander Scholtus. In their paper [“A Note on Efficient Audit Sample Selection”](https://arxiv.org/abs/2105.10737), they introduce a method for efficient audit sample selection and test this using a [simulation study](https://github.com/lauraboeschoten/audit_2021). Now, this method has been revised. In this pilot simulation study, we test how the revised method works by applying the method to a subset of the conditions that were used to test the original approach. In the end, visualizations were created to compare both methods against one another. 

Software requirements are [R](http://www.r-project.org}) and the R-package [‘nloptr’](https://cran.r-project.org/web/packages/nloptr/index.html) (version 2.0.3). Furthermore, the R-package ['ggplot2'](https://cran.r-project.org/web/packages/ggplot2/index.html) (version 3.4.0) was used for plotting, and the R-package ['dplyr'](https://cran.r-project.org/web/packages/dplyr/index.html) (version 1.0.10) was used for data manipulation. 

|Files/Folders|	Description|
|-------------|------------|
|1_data_generation.R|	R-script to generate the data used for the simulation. The script generates 1000 data sets in 8 different conditions.|
|2_apply_procedure.R|	R-script to apply the revised approach to all generated data sets. |
|3_process_results.R| R-script to get the results in the appropriate format for analyzing and creating plots. |
|4_plot_output.R|	R-script to plot the results. This script also uses the results of the original approach simulation study, in order to enable visual comparison. |
|/functions|	Folder containing the functions used by ‘2_apply_procedure.R’|
|/workspaces|	Folder containing workspaces with the data sets and distributions generated by script ‘1_data_generation.R’. This folder also contains workspaces with the results generated by script ‘2_apply_procedure.R’, and the processed results generates by script ‘3_process.results.R’|
|/original_simulation_results.R|	Folder containing the processed results of the original simulation study. Used in script ‘4_plot_output.R’ to enable comparison. |
|Manuscript.pdf|	The manuscript that introduces the revised approach, and presents the pilot simulation study. |


For any help with the files in this archive, please contact Lotte Mensink (l.mensink@uu.nl)
