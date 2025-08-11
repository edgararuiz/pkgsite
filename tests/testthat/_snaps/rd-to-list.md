# lm() to list works

    Code
      rd_to_list(lm_rd)
    Output
      $title
      [1] "Fitting Linear Models"
      
      $name
      [1] "lm"
      
      $alias
      [1] "lm"
      
      $alias
      [1] "print.lm"
      
      $concept
      [1] "regression"
      
      $keyword
      [1] "regression"
      
      $description
      [1] "'lm' is used to fit linear models, including multivariate ones. It can be used to carry out regression, single stratum analysis of variance and analysis of covariance (although 'aov' may provide a more convenient interface for these)."
      
      $usage
      [1] "lm(formula, data, subset, weights, na.action,"                     
      [2] "   method = \"qr\", model = TRUE, x = FALSE, y = FALSE, qr = TRUE,"
      [3] "   singular.ok = TRUE, contrasts = NULL, offset, ...)"             
      [4] ""                                                                  
      [5] "## S3 method for class 'lm'"                                       
      [6] "print(x, digits = max(3L, getOption(\"digits\") - 3L), ...)"       
      
      $arguments
      $arguments[[1]]
      $arguments[[1]]$argument
      [1] "formula"
      
      $arguments[[1]]$description
      [1] "an object of class '\"formula\"' (or one that can be coerced to that class): a symbolic description of the model to be fitted.  The details of model specification are given under 'Details'."
      
      
      $arguments[[2]]
      $arguments[[2]]$argument
      [1] "data"
      
      $arguments[[2]]$description
      [1] "an optional data frame, list or environment (or object coercible by 'as.data.frame' to a data frame) containing the variables in the model. If not found in 'data', the variables are taken from 'environment(formula)', typically the environment from which 'lm' is called."
      
      
      $arguments[[3]]
      $arguments[[3]]$argument
      [1] "subset"
      
      $arguments[[3]]$description
      [1] "an optional vector specifying a subset of observations to be used in the fitting process.  (See additional details about how this argument interacts with data-dependent bases in the 'Details' section of the 'model.frame' documentation.)"
      
      
      $arguments[[4]]
      $arguments[[4]]$argument
      [1] "weights"
      
      $arguments[[4]]$description
      [1] "an optional vector of weights to be used in the fitting process. Should be 'NULL' or a numeric vector.  If non-NULL, weighted least squares is used with weights 'weights' (that is, minimizing 'sum(w*e^2)'); otherwise ordinary least squares is used.  See also 'Details',"
      
      
      $arguments[[5]]
      $arguments[[5]]$argument
      [1] "na.action"
      
      $arguments[[5]]$description
      [1] "a function which indicates what should happen when the data contain 'NA's.  The default is set by the 'na.action' setting of 'options', and is 'na.fail' if that is unset.  The 'factory-fresh' default is 'na.omit'.  Another possible value is 'NULL', no action.  Value 'na.exclude' can be useful."
      
      
      $arguments[[6]]
      $arguments[[6]]$argument
      [1] "method"
      
      $arguments[[6]]$description
      [1] "the method to be used; for fitting, currently only 'method = \"qr\"' is supported; 'method = \"model.frame\"' returns the model frame (the same as with 'model = TRUE', see below)."
      
      
      $arguments[[7]]
      $arguments[[7]]$argument
      [1] "model, x, y, qr"
      
      $arguments[[7]]$description
      [1] "logicals.  If 'TRUE' the corresponding components of the fit (the model frame, the model matrix, the response, the QR decomposition) are returned."
      
      
      $arguments[[8]]
      $arguments[[8]]$argument
      [1] "singular.ok"
      
      $arguments[[8]]$description
      [1] "logical. If 'FALSE' (the default in S but not in R) a singular fit is an error."
      
      
      $arguments[[9]]
      $arguments[[9]]$argument
      [1] "contrasts"
      
      $arguments[[9]]$description
      [1] "an optional list. See the 'contrasts.arg' of 'model.matrix.default'."
      
      
      $arguments[[10]]
      $arguments[[10]]$argument
      [1] "offset"
      
      $arguments[[10]]$description
      [1] "this can be used to specify an _a priori_ known component to be included in the linear predictor during fitting.  This should be 'NULL' or a numeric vector or matrix of extents matching those of the response.  One or more 'offset' terms can be included in the formula instead or as well, and if more than one are specified their sum is used.  See 'model.offset'."
      
      
      $arguments[[11]]
      $arguments[[11]]$argument
      [1] "..."
      
      $arguments[[11]]$description
      [1] "For 'lm()': additional arguments to be passed to the low level regression fitting functions (see below)."
      
      
      $arguments[[12]]
      $arguments[[12]]$argument
      [1] "digits"
      
      $arguments[[12]]$description
      [1] "the number of _significant_ digits to be passed to 'format(coef(x), .)' when 'print()'ing."
      
      
      
      $details
       [1] "Models for 'lm' are specified symbolically.  A typical model has the form 'response ~ terms' where 'response' is the (numeric) response vector and 'terms' is a series of terms which specifies a linear predictor for 'response'.  A terms specification of the form 'first + second' indicates all the terms in 'first' together with all the terms in 'second' with duplicates removed.  A specification of the form 'first:second' indicates the set of terms obtained by taking the interactions of all terms in 'first' with all terms in 'second'.  The specification 'first*second' indicates the _cross_ of 'first' and 'second'.  This is the same as 'first + second + first:second'. "                                     
       [2] ""                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      
       [3] " If the formula includes an 'offset', this is evaluated and subtracted from the response. "                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            
       [4] ""                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      
       [5] " If 'response' is a matrix a linear model is fitted separately by least-squares to each column of the matrix and the result inherits from '\"mlm\"' (\"multivariate linear model\"). "                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 
       [6] ""                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      
       [7] " See 'model.matrix' for some further details.  The terms in the formula will be re-ordered so that main effects come first, followed by the interactions, all second-order, all third-order and so on: to avoid this pass a 'terms' object as the formula (see 'aov' and 'demo(glm.vr)' for an example). "                                                                                                                                                                                                                                                                                                                                                                                                                             
       [8] ""                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      
       [9] " A formula has an implied intercept term.  To remove this use either 'y ~ x - 1' or 'y ~ 0 + x'.  See 'formula' for more details of allowed formulae. "                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                
      [10] ""                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      
      [11] " Non-'NULL' 'weights' can be used to indicate that different observations have different variances (with the values in 'weights' being inversely proportional to the variances); or equivalently, when the elements of 'weights' are positive integers w_i, that each response y_i is the mean of w_i unit-weight observations (including the case that there are w_i observations equal to y_i and the data have been summarized). However, in the latter case, notice that within-group variation is not used. Therefore, the sigma estimate and residual degrees of freedom may be suboptimal; in the case of replication weights, even wrong. Hence, standard errors and analysis of variance tables should be treated with care. "
      [12] ""                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      
      [13] " 'lm' calls the lower level functions 'lm.fit', etc, see below, for the actual numerical computations.  For programming only, you may consider doing likewise. "                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       
      [14] ""                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      
      [15] " All of 'weights', 'subset' and 'offset' are evaluated in the same way as variables in 'formula', that is first in 'data' and then in the environment of 'formula'."                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   
      
      $value
       [1] "'lm' returns an object of 'class' '\"lm\"' or for multivariate ('multiple') responses of class 'c(\"mlm\", \"lm\")'. "                                                                                                                                                                
       [2] ""                                                                                                                                                                                                                                                                                     
       [3] " The functions 'summary' and 'anova' are used to obtain and print a summary and analysis of variance table of the results.  The generic accessor functions 'coefficients', 'effects', 'fitted.values' and 'residuals' extract various useful features of the value returned by 'lm'. "
       [4] ""                                                                                                                                                                                                                                                                                     
       [5] " An object of class '\"lm\"' is a list containing at least the following components: "                                                                                                                                                                                                
       [6] ""                                                                                                                                                                                                                                                                                     
       [7] " coefficients: a named vector of coefficients "                                                                                                                                                                                                                                       
       [8] ""                                                                                                                                                                                                                                                                                     
       [9] " residuals: the residuals, that is response minus fitted values. "                                                                                                                                                                                                                    
      [10] ""                                                                                                                                                                                                                                                                                     
      [11] " fitted.values: the fitted mean values. "                                                                                                                                                                                                                                             
      [12] ""                                                                                                                                                                                                                                                                                     
      [13] " rank: the numeric rank of the fitted linear model. "                                                                                                                                                                                                                                 
      [14] ""                                                                                                                                                                                                                                                                                     
      [15] " weights: (only for weighted fits) the specified weights. "                                                                                                                                                                                                                           
      [16] ""                                                                                                                                                                                                                                                                                     
      [17] " df.residual: the residual degrees of freedom. "                                                                                                                                                                                                                                      
      [18] ""                                                                                                                                                                                                                                                                                     
      [19] " call: the matched call. "                                                                                                                                                                                                                                                            
      [20] ""                                                                                                                                                                                                                                                                                     
      [21] " terms: the 'terms' object used. "                                                                                                                                                                                                                                                    
      [22] ""                                                                                                                                                                                                                                                                                     
      [23] " contrasts: (only where relevant) the contrasts used. "                                                                                                                                                                                                                               
      [24] ""                                                                                                                                                                                                                                                                                     
      [25] " xlevels: (only where relevant) a record of the levels of the factors used in fitting. "                                                                                                                                                                                              
      [26] ""                                                                                                                                                                                                                                                                                     
      [27] " offset: the offset used (missing if none were used). "                                                                                                                                                                                                                               
      [28] ""                                                                                                                                                                                                                                                                                     
      [29] " y: if requested, the response used. "                                                                                                                                                                                                                                                
      [30] ""                                                                                                                                                                                                                                                                                     
      [31] " x: if requested, the model matrix used. "                                                                                                                                                                                                                                            
      [32] ""                                                                                                                                                                                                                                                                                     
      [33] " model: if requested (the default), the model frame used. "                                                                                                                                                                                                                           
      [34] ""                                                                                                                                                                                                                                                                                     
      [35] " na.action: (where relevant) information returned by 'model.frame' on the special handling of 'NA's. "                                                                                                                                                                                
      [36] ""                                                                                                                                                                                                                                                                                     
      [37] " In addition, non-null fits will have components 'assign', 'effects' and (unless not requested) 'qr' relating to the linear fit, for use by extractor functions such as 'summary' and 'effects'."                                                                                     
      
      $section
      $section$title
      [1] "Using time series"
      
      $section$contents
      [1] "Considerable care is needed when using 'lm' with time series. "                                                                                                                                                                                                                                                                                                                                               
      [2] ""                                                                                                                                                                                                                                                                                                                                                                                                             
      [3] " Unless 'na.action = NULL', the time series attributes are stripped from the variables before the regression is done.  (This is necessary as omitting 'NA's would invalidate the time series attributes, and if 'NA's are omitted in the middle of the series the result would no longer be a regular time series.) "                                                                                         
      [4] ""                                                                                                                                                                                                                                                                                                                                                                                                             
      [5] " Even if the time series attributes are retained, they are not used to line up series, so that the time shift of a lagged or differenced regressor would be ignored.  It is good practice to prepare a 'data' argument by 'ts.intersect(..., dframe = TRUE)', then apply a suitable 'na.action' to that data frame and call 'lm' with 'na.action = NULL' so that residuals and fitted values are time series."
      
      
      $author
      [1] "The design was inspired by the S function of the same name described in Chambers (1992).  The implementation of model formula by Ross Ihaka was based on Wilkinson & Rogers (1973)."
      
      $references
      [1] "Chambers, J. M. (1992) _Linear models._ Chapter 4 of _Statistical Models in S_ eds J. M. Chambers and T. J. Hastie, Wadsworth & Brooks/Cole. "                                                                 
      [2] ""                                                                                                                                                                                                              
      [3] " Wilkinson, G. N. and Rogers, C. E. (1973).  Symbolic descriptions of factorial models for analysis of variance.  _Applied Statistics_, *22*, 392-399.  doi:10.2307/2346786 <https://doi.org/10.2307/2346786>."
      
      $seealso
       [1] "'summary.lm' for more detailed summaries and 'anova.lm' for the ANOVA table; 'aov' for a different interface. "                                   
       [2] ""                                                                                                                                                 
       [3] " The generic functions 'coef', 'effects', 'residuals', 'fitted', 'vcov'. "                                                                        
       [4] ""                                                                                                                                                 
       [5] " 'predict.lm' (via 'predict') for prediction, including confidence and prediction intervals; 'confint' for confidence intervals of _parameters_. "
       [6] ""                                                                                                                                                 
       [7] " 'lm.influence' for regression diagnostics, and 'glm' for *generalized* linear models. "                                                          
       [8] ""                                                                                                                                                 
       [9] " The underlying low level functions, 'lm.fit' for plain, and 'lm.wfit' for weighted regression fitting. "                                         
      [10] ""                                                                                                                                                 
      [11] " More 'lm()' examples are available e.g., in 'anscombe', 'attitude', 'freeny', 'LifeCycleSavings', 'longley', 'stackloss', 'swiss'. "             
      [12] ""                                                                                                                                                 
      [13] " 'biglm' in package 'biglm' for an alternative way to fit linear models to large datasets (especially those with many cases)."                    
      
      $examples
      $examples$code_dont_run
      [1] "\nrequire(graphics)\n\n## Annette Dobson (1990) \"An Introduction to Generalized Linear Models\".\n## Page 9: Plant Weight Data.\nctl <- c(4.17,5.58,5.18,6.11,4.50,4.61,5.17,4.53,5.33,5.14)\ntrt <- c(4.81,4.17,4.41,3.59,5.87,3.83,6.03,4.89,4.32,4.69)\ngroup <- gl(2, 10, 20, labels = c(\"Ctl\",\"Trt\"))\nweight <- c(ctl, trt)\nlm.D9 <- lm(weight ~ group)\nlm.D90 <- lm(weight ~ group - 1) # omitting intercept\n\nanova(lm.D9)\nsummary(lm.D90)\n\nopar <- par(mfrow = c(2,2), oma = c(0, 0, 1.1, 0))\nplot(lm.D9, las = 1)      # Residuals, Fitted, ...\npar(opar)\n\n## model frame :\nstopifnot(identical(lm(weight ~ group, method = \"model.frame\"),\n                    model.frame(lm.D9)))\n\n### less simple examples in \"See Also\" above\n"
      
      

