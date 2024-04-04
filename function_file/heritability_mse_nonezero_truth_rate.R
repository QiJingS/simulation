source("/well/band/users/xdy671/function_file/rig_horse.R")
heritability_mse_nonzero_truth_rate = function(sig_variable_index,rep_val,x,approach){
  heritability_set = c(seq(0.01,0.1,0.01),seq(0.2,1,0.1))
  mse = data.frame()
  beta = data.frame()
  truth_beta = data.frame()
  nonzero_coefficients_mse = data.frame()
  repetition = 1
  while(repetition <= rep_val){
    for (i in 1:length(heritability_set)){
      w = rep(0,ncol(x))
      w[sig_variable_index] = 0.2
      w[-sig_variable_index] = rnorm(length(w[-sig_variable_index]), mean = 0, sd = sqrt(0.001))
      epsilon = rnorm(dim(x)[1],0,sd = ((1 - heritability_set[i])/heritability_set[i])*var(x %*% w ))
      y = x%*%w + epsilon
      app = approach_class$new()
	  if(approach == "susieR"){
		step_output = app$susie(y,x)$effect_size
	  }else if (approach == "rig_horse") {
		 step_output = rig_horseshoe(y,x,1)
	  }
      #step_output = app$horseshoe3(y,x)$effect_size
      #step_output = app$reg_horseshoe(y,x,nonezero_coefficients)$w
      #nonzero_coefficients_mse[repetition,i] = paste(c(abs((step_output[x_index_effectsize] - w[x_index_effectsize])/w[x_index_effectsize])),collapse = ",")
      mse[repetition,i] = sum((step_output - w)^2)
      beta[1:dim(x)[2],i] = step_output
      truth_beta[1:dim(x)[2],i] = w
    }
    repetition = repetition + 1
  }
  colnames(mse) = heritability_set
  colnames(beta) = heritability_set
  colnames(truth_beta) = heritability_set
  return(list(mse = mse,beta = beta,truth_beta = truth_beta))
}



