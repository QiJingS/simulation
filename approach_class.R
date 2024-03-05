#figure(coef(OLS)[-1],w,"OLS",coef(summary(OLS))[,2][-1])
#figure(fixef(horseshoe1)[,1][-1], w, "Horseshoe",fixef(horseshoe1)[,2][-1])
#figure(fixef(horseshoe3)[,1][-1], w, "Horseshoe",fixef(horseshoe3)[,2][-1])
#figure(susie_get_posterior_mean(res1), w, "Susie",susie_get_posterior_sd(res1))
#figure(coef(ridge.model)[-1], w, "Ridge",0)
#figure(coef(lasso.model)[-1], w, "Lasso",0)


approach_class = R6::R6Class("approaches_class",
                                  public = list(
                                    
                                    original_linear_regression = function(y,x){
                                      return(
                                                list(effect_size = coef(lm(y~x))[-1],
                                                sd = coef(summary(lm(y~x)))[,2][-1])
                                            )
                                    },
                                    
                                    horseshoe1 = function(y,x){
                                      data = data.frame(y = y,x)
                                      horseshoe1 = brms::brm( y ~., family = "normal", data = data, prior = set_prior("horseshoe(1)"))
                                      return(
                                              list(
                                                effect_size = fixef(horseshoe1)[,1][-1],
                                                sd = fixef(horseshoe1)[,2][-1]
                                              )
                                            )
                                    },
                                    
                                    horseshoe3 = function(y,x){
                                      data = data.frame(y = y,x)
                                      horseshoe3 = brms::brm( y ~., family = "normal", data = data, prior = set_prior("horseshoe(3)") )
                                      return(
                                        list(
                                          effect_size = fixef(horseshoe3)[,1][-1],
                                          sd = fixef(horseshoe3)[,2][-1]
                                        )
                                      )
                                    },
                                    
                                    susie = function(y,x){
                                      susie_res = susie_auto(x,y)
                                      return(
                                          list(
                                                effect_size  = susie_get_posterior_mean(susie_res),
                                                sd = susie_get_posterior_sd(susie_res)
                                                )
                                      )
                                    },
                                    
                                    ridge = function(y,x){
                                      cv.lambda <- cv.glmnet(x=x, y=y, 
                                                             alpha = 0,
                                                             lambda=exp(seq(-10,10,.1)))  
                           
                                      min_index = which(cv.lambda$lambda == cv.lambda$lambda.min)
                                      mse.min = cv.lambda$cvm[min_index]
                                      lmin = cv.lambda$lambda.min
                                      ridge.model = glmnet(x=x, y=y,
                                                           alpha = 0, 
                                                           lambda = lmin)
                                      
                                      return(effect_size = coef(ridge.model)[-1])
                                    },
                                    lasso = function(y,x){
                                      cv.lambda <- cv.glmnet(x=x, y=y, 
                                                             alpha = 1,
                                                             lambda=exp(seq(-10,10,.1)))  
                                      
                                      min_index = which(cv.lambda$lambda == cv.lambda$lambda.min)
                                      mse.min = cv.lambda$cvm[min_index]
                                      lmin = cv.lambda$lambda.min
                                      ridge.model = glmnet(x=x, y=y,
                                                           alpha = 0, 
                                                           lambda = lmin)
                                      return(effect_size = coef(ridge.model)[-1])
                                      
                                    },
                                    summary_each = function(y,x){
                                      OLS = self$original_linear_regression(y,x)
                                      horseshoe1 = self$horseshoe1(y,x)
                                      horseshoe3 =  self$horseshoe3(y,x)
                                      susie = self$susie(y,x)
                                      ridge = self$ridge(y,x)
                                      lasso = self$lasso(y,x)
                                      return(list( OLS = OLS,
                                                   horseshoe1 = horseshoe1,
                                                   horseshoe3 = horseshoe3,
                                                   susie = susie,
                                                   ridge = ridge,
                                                   lasso = lasso)
                                             )
                                    }
                                  )
                            )
