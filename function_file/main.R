#args = commandArgs(trailingOnly = TRUE)
args = c(1)
library(tseries)
library(pheatmap)
library(brms)
library(susieR)
library(glmnet)
library(R6)
library(ggplot2)
library(rstan)
#source("ERAPS/function_file/stepwise_linear_regression.R")
source("/well/band/users/xdy671/function_file/approach_class.R")
source("/well/band/users/xdy671/function_file/genetic_model.R")
source("/well/band/users/xdy671/function_file/heritability_mse_nonezero_truth_rate.R")


data = read.table("/well/band/users/xdy671/function_file/spon_common_af_0.05_r2_0.5_1.dosage",header = TRUE)
data$SNPID = sub("^[^_]*_", "", data$SNPID)
x = as.matrix(t(data[,-1:-7]))
x = apply(x, 2, as.numeric)
colnames(x) = data$SNPID

genetic_model_selection  = genetic_model$new()
genetic_model_set = list(x,
  genetic_model_selection$dominant_model(x),
  genetic_model_selection$recessive_model(x),
  genetic_model_selection$overdominant_model(x))

# #nonezero_coefficients_set = c(1,2,5,10,20,50)
nonezero_coefficients_set = c(1)
x = genetic_model_set[[1]]
#:length(nonezero_coefficients_set)
#for (i in 1){
i = 1
approach = c("susieR","rig_horse")
for(names in approach){
  path_folder = paste0("/well/band/users/xdy671/function_file/additive/",names,"1/")
  res = heritability_mse_nonzero_truth_rate(sig_variable_index = as.numeric(as.integer(args)),rep_val = 1,x =  genetic_model_set[[1]],names)
  write.table(res$mse,file = paste0(path_folder,"MSE","_nonezero_coeff_",nonezero_coefficients_set[i],"_iteration_",as.numeric(as.integer(args)),".txt"))
  write.table(res$beta,file = paste0(path_folder,"beta","_nonezero_coeff_",nonezero_coefficients_set[i],"_iteration_",as.numeric(as.integer(args)),".txt"))
  write.table(res$truth_beta,file = paste0(path_folder,"truth_beta","_nonezero_coeff_",nonezero_coefficients_set[i],"_iteration_",as.numeric(as.integer(args)),".txt"))
}

