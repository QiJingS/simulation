genetic_model = R6::R6Class("genetic_model", 
                            public = list(
                              dominant_model = function(x){
                                x[x == 2] = 1
                                return(x)
                              },
                              recessive_model = function(x){
                                x[x == 1] = 0
                                x[x == 2] = 1
                                return(x)
                              },
                              overdominant_model = function(x){
                                x[x == 2] = 0
                                return(x)
                              }
                            )
)
