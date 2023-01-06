
######  Conditional effect plot with confidence envelope  #########

ce.plot <- function(mod, 
                    xvar, 
                    xval = NULL, 
                    nval = 20, 
                    conf = 0.95,
                    fixvar = NULL, 
                    fixval = NULL,
                    ylab = descript, 
                    xlab = paste(xname), 
                    ylim = c(min(pred[, 2]), max(pred[, 3])), 
                    main = ifelse(!is.null(fixvar), title, NA), 
                    pch = 16, 
                    ...
                    ) 
  {  
  
  # get data used for estimating the model
  mf <- model.frame(mod)
  
  
  # turn varname into a character string
  xname <- deparse(substitute(xvar))
  
  
  # values of predictor variable to make the prediction
  if (is.null(xval)) {
    if (is.factor(mf[[xname]])) {
      x <- gl(nlevels(mf[[xname]]), 1, labels = levels(mf[[xname]]))
      
    } else if (is.numeric(mf[[xname]])) {
      x <- seq(min(mf[xname]), max(mf[xname]), length.out = nval)
      
    } else print("xvar is neither a factor nor numeric")
    
  } else {
    if (is.factor(mf[[xname]])) {
      x <- gl(length(xval), 1, labels = xval)
      
    } else if (is.numeric(mf[[xname]])) {
      x <- xval
      
    } else print("xvar is neither a factor nor numeric")
    
  }
  
  
  # set level of fixvar in model frame (if fixvar is provided)
  if (!is.null(fixvar)) {
    if (is.numeric(mf[[fixvar]])) {
      mf[fixvar] <- fixval
      
    } else if (is.factor(mf[[fixvar]])) {
      mf[fixvar] <- gl(1, 1, labels = fixval)
      
    } else print("fixvar is neither a factor nor numeric")
    
    # title showing value of fixvar (to be used in plotting)
    title <- paste(fixvar, "=", fixval)
    
  }
  
  
  # matrix to store avg. predicted values (expected, upper and lower) ncol 
  # depends on the number of fixed values desired for interaction variable
  
  pred <- matrix(as.numeric(NULL), nrow = length(x), ncol = 3)
  
  
  # predict y over the range of x (from observed min to max) while holding 
  # other variables at their fixed or observed values, then average over the 
  # entire sample

  
  if (class(mod)[1] == "lm") {

    for (i in 1:length(x)) {
      
      # set level of xvar in model frame
      mf[xname] <- rep(x[i], nrow(mf))
      
      # prediction, incl. CIs
      pred[i, ] <- apply(predict(mod, mf, interval = "confidence"), 2, mean)
    }
    
    # description of output
    descript <- c("Expected Value")
    
  } else if (class(mod)[1] == "glm") {

    predSE <- matrix(as.numeric(NULL), nrow = length(x), ncol = 2)  
    
    for (i in 1:length(x)) {
    
      # set level of xvar in model frame
      mf[xname] <- rep(x[i], nrow(mf))
    
      # predict responses and SEs and average over sample
      predSE[i, ] <- sapply(predict(mod, mf, se.fit = TRUE, type = "response"), mean)[1:2]

    }
  
    # SEs to CIs
    pred <- cbind(predSE[, 1],
                  predSE[, 1] + predSE[, 2] * qnorm((1 - conf) / 2),
                  predSE[, 1] + predSE[, 2] * qnorm(1 - (1 - conf) / 2)
                  )


  # description of output

  if (class(mod)[1] == "glm" & mod$family$family == "gaussian") {
    descript <- c("Expected Value")
    
  } else if (class(mod)[1] == "glm" & mod$family$family == "poisson") {
    descript <- c("Avg. Predicted Rate")
    
  } else if (class(mod)[1] == "glm" & mod$family$family == "quasipoisson") {
    descript <- c("Avg. Predicted Rate")
    
  } else if (class(mod)[1] == "glm" & mod$family$link == "probit") {
    descript <- c("Avg. Predicted Probability")
    
  } else if (class(mod)[1] == "glm" & mod$family$link == "logit") {
    descript <- c("Avg. Predicted Probability")
    
  } else print("Outcome variable must be either Gaussian, Count, or Binomial")

  }
  
  # plot the predictions for numeric predictors and factors  

  if (is.numeric(x)) {
    plot(x, pred[, 1], type = "n", ylim = ylim, ylab = ylab, xlab = xlab, main = main, ...)
    points(x, pred[, 1], pch = pch, ...)
    segments(as.numeric(x), pred[, 2], as.numeric(x), pred[, 3], ...)
    
  } else if (is.factor(x)) {
    # type='n' impossible when x is a factor, so instead we draw an invisible/white boxplot
    plot(x, pred[, 1], border = "white", ylim = ylim, ylab = ylab, xlab = xlab, main = main, ...)
    points(x, pred[, 1], pch = pch, ...)
    segments(as.numeric(x), pred[, 2], as.numeric(x), pred[, 3], ...)
  }
}