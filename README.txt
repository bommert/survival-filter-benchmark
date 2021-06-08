# Instructions for running the R code

Run the files in the same order as they are listed in this document.
If not stated otherwise, the files that are sourced or loaded must be placed in the working directory.
The folder "Experiments" contains code related to the execution of the experiments.
The folder "Evaluation" contains code for evaluating the results of the experiments.

## Experiments

- fbs_datasets.R: Downloads the data sets and creates mlr3 tasks. Loads the file "datset_ids.RData" and creates the file "data_names.RData".
- fbs_data_info.R: States information about the data sets. Creates the file "data_info.RData". Loads the following files:
   - data_names.RData
   - The data set files created by fbs_datasets.R. They should be stored in a subfolder of the Experiments folder called "Data".
- fbs_data_similarity_matrices.R: Creates the file "fbs_sim_mats.RData". Loads the following files:
   - data_names.RData
   - The data set files created by fbs_datasets.R. They should be stored in a subfolder of the Experiments folder called "Data".
- fbs_experiments.R: Run this on a high performance compute cluster to conduct the filter benchmark. Sources/loads the following files:
   - data_names.RData
   - helper.R
   - survival_filters.R
   - The data set files created by fbs_datasets.R. They should be stored in a subfolder of the working directory called "Data".
- fbs_results.R: Collects the results of the filter benchmark. Loads the file "data_info.RData". Creates the files "fbs_results_filter.RData" and "fbs_results_ranking.RData".


## Evaluation

- fbs_tuning.R: Performs the tuning. Loads the file "fbs_results_filter.RData" and creates the file "fbs_results_tuning.RData".
- fbs_results_features.R: Collects the selected features for the best configurations. Loads the file "fbs_results_tuning.RData" and creates the file "fbs_results_features.RData".
- fbs_stability_experiments.R: Stability evaluation of the best configurations. Depending on your local hardware, you might want to run this on a compute cluster. Loads the files "fbs_sim_mats.RData" and "fbs_results_features.RData". Creates the file "fbs_results_stability.RData".
- fbs_evaluation.R: Creates the plots for the performance analyses of the filter methods. Loads the files "fbs_results_tuning.RData" and "fbs_results_stability.RData".
- fbs_similarity.R: Creates the plot for the similarity analyses of the filter methods. Loads the file "fbs_results_ranking.RData".








Woher kommt data_info.RData?
