# This file allows you to source in the many functions we have implemented 
# in lab.

# Sourcing is functionally very similar to installing a package with a few less
# perks.

# Required Packages ####
library(psych)
library(car)
library(tidyverse)
###############################################################################
# ggplotPredict.####

ggplotPredict = function (Model, Data = NULL, Label = NULL, Type = "response") 
{
  if (is.null(Data) & class(Model)[1] == "lm") {
    return(fitted.values(Model))
  }
  else {
    if (is.null(Label)) {
      PredictName = "Predicted"
      CILoName = "CILo"
      CIHiName = "CIHi"
      SEName = "SE"
    }
    else {
      PredictName = paste0("Predicted", Label)
      CILoName = paste0("CILo", Label)
      CIHiName = paste0("CIHi", Label)
      SEName = paste0("SE", Label)
    }
    Predictions = matrix(data = NA, nrow = nrow(Data), ncol = 4, 
                         dimnames = list(1:nrow(Data), c(PredictName, CILoName, 
                                                         CIHiName, SEName)))
    if (class(Model)[1] == "lm") {
      CILevel = 1 - 2 * pt(c(1), df = Model$df.residual, 
                           lower.tail = FALSE)
      Predictions[, 1:3] = predict(Model, newdata = Data, 
                                   interval = "confidence", level = CILevel)
      Predictions[, 4] = Predictions[, 1] - Predictions[, 
                                                        2]
      Predictions = as.data.frame(Predictions)
    }
    if (class(Model)[1] == "glm") {
      tmpPred = predict(Model, newdata = Data, type = "link", 
                        se.fit = TRUE)
      upr <- tmpPred$fit + tmpPred$se.fit
      lwr <- tmpPred$fit - tmpPred$se.fit
      fit <- tmpPred$fit
      if (Type == "response") {
        fit <- Model$family$linkinv(fit)
        upr <- Model$family$linkinv(upr)
        lwr <- Model$family$linkinv(lwr)
      }
      Predictions[, 1] = fit
      Predictions[, 2] = lwr
      Predictions[, 3] = upr
      Predictions[, 4] = Predictions[, 1] - Predictions[, 
                                                        2]
      Predictions = as.data.frame(Predictions)
    }
    if ((class(Model)[1] == "lmerMod") || (class(Model)[1] == 
                                           "glmerMod")) {
      Predictions[, c(1, 4)] = predictSE(Model, Data, 
                                         se.fit = TRUE, type = Type, level = 0, print.matrix = TRUE)
      Predictions[, 2] = Predictions[, 1] - Predictions[, 
                                                        4]
      Predictions[, 3] = Predictions[, 1] + Predictions[, 
                                                        4]
    }
    if (any(names(Data) == PredictName) || any(names(Data) == 
                                               CILoName) || any(names(Data) == CIHiName) || any(names(Data) == 
                                                                                                SEName)) {
      warning("Variable names (Predicted, CILo, CIHi, SE with Label PostFix) used in Data.  These variables removed before merging in predicted values")
      Data[, c(PredictName, CILoName, CIHiName, SEName)] = list(NULL)
    }
    Data = data.frame(Predictions, Data)
    return(Data)
  }
}
###############################################################################
# corr_table ####
corr_table <- function(vars, use_method) {
  # Run the corr.test function
  corr_results <- corr.test(d[, vars], use = use_method)
  # Make a dataframe called d_corrs that's empty other than row.names that are our vars
  d_corrs <- data.frame(row.names = vars)
  # Our first 'for' loop!! This loops through the code between the two {} once 
  # for every item in 'vars,' calling each one 'v' in turn. 
  # Once it runs everything between the {} for that item, then it loops back up 
  # to do it all again for the next item, until it runs out of items. 
  for (v in vars) {
    print(v)
    for (pair_v in vars) { # a nested for loop
      # Obtain the r value from the correlation table where row = v, column = pair_v
      # and round it to 3 digits
      r <- round(corr_results$r[v, pair_v], digits = 3) 
      # Obtain the p value from the correlation table where row = v, column = pair_v
      # and round it to 3 digits
      p <- round(corr_results$p[v, pair_v], digits = 3)
      # paste() puts it all together
      v_r_p <- paste("r = ", r, ", (p = ", p, ")", sep = "")
      # Now, in our d_corrs dataframe where row = v, column = pair_v, put our 
      # pasted together result
      d_corrs[v, pair_v] <- v_r_p
    }
  }
  return(d_corrs)
}
###############################################################################
# center ####
center <- function(var) {
  (var - mean(var, na.rm = T))
} # mean center a variable 
###############################################################################
# modelCaseAnalysis ####
modelCaseAnalysis = function (Model, Type = "RESIDUALS", Term = NULL, ID = row.names(Model$model)) 
{
  switch(toupper(Type), UNIVARIATE = {
    d = Model$model
    {
      Vars = names(d)
    }
    for (varname in Vars) {
      par(cex.lab = 1.5, cex.axis = 1.2, lwd = 2, ask = TRUE)
      if (is.factor(d[[varname]])) {
        plot(d[varname], xlab = varname, ylab = "Frequency")
      } else {
        hist(d[[varname]], xlab = varname, main = "Red: Mean +- 3SD; Green: Median +- 2.2IQR")
        points(d[[varname]], rep(0, length(d[[varname]])), 
               pch = "|", col = "blue")
        abline(v = c(-3, 0, 3) * sd(d[[varname]]) + 
                 mean(d[[varname]]), col = "red", lty = c(1, 
                                                          2, 1))
        abline(v = c(-2.2, 0, 2.2) * IQR(d[[varname]]) + 
                 median(d[[varname]]), col = "green", lty = c(1, 
                                                              2, 1))
        if (!is.null(ID)) {
          Indices = identify(d[[varname]], rep(0, length(d[[varname]])), 
                             labels = ID)
          Values = d[[varname]][Indices]
        }
      }
    }
  }, HATVALUES = {
    par(cex.lab = 1.5, cex.axis = 1.2, lwd = 2)
    TheTitle = paste("Model: ", Model$call[2], "\n", "Small sample cut (green) = 3 * mean(Hat)\nLarge sample cut: 2 * mean(Hat)", 
                     sep = "")
    hist(hatvalues(Model), xlab = "Hat Values", main = TheTitle)
    abline(v = c(2, 3) * mean(hatvalues(Model)), col = c("red", 
                                                         "green"))
    points(hatvalues(Model), rep(0, length(hatvalues(Model))), 
           pch = "|", col = "blue")
    if (!is.null(ID)) {
      Indices = identify(hatvalues(Model), rep(0, length(hatvalues(Model))), 
                         labels = ID)
      Values = hatvalues(Model)[Indices]
    }
  }, RESIDUALS = {
    par(cex.lab = 1.5, cex.axis = 1.2, lwd = 2)
    N = length(rstudent(Model))
    k = length(coef(Model)) - 1
    TCut <- qt(p = 0.025/N, df = N - k - 2, lower.tail = FALSE)
    TheTitle = paste("Model: ", Model$call[2], "\n", "Bonferroni corrected p < .05 cut-off in red", 
                     sep = "")
    hist(rstudent(Model), xlab = "Studentized Residuals", 
         main = TheTitle)
    abline(v = c(-1, 1) * TCut, col = "red")
    points(rstudent(Model), rep(0, length(rstudent(Model))), 
           pch = "|", col = "blue")
    if (!is.null(ID)) {
      Indices = identify(rstudent(Model), rep(0, length(rstudent(Model))), 
                         labels = ID)
      Values = rstudent(Model)[Indices]
    }
  }, COOKSD = {
    par(cex.lab = 1.5, cex.axis = 1.2, lwd = 2)
    N = length(cooks.distance(Model))
    k = length(coef(Model)) - 1
    TheTitle = paste("Model: ", Model$call[2], "\n", "4/(N-P) cut-off (red)\nqf(.5,P,N-P) cut-off (green)", 
                     sep = "")
    hist(cooks.distance(Model), xlab = "Cooks d", main = TheTitle)
    abline(v = c((4/(N - k - 1)), qf(0.5, k + 1, N - k - 
                                       1)), col = c("red", "green"))
    points(cooks.distance(Model), rep(0, length(cooks.distance(Model))), 
           pch = "|", col = "blue")
    if (!is.null(ID)) {
      Indices = identify(cooks.distance(Model), rep(0, 
                                                    length(cooks.distance(Model))), labels = ID)
      Values = cooks.distance(Model)[Indices]
    }
  }, DFBETAS = {
    if (is.null(Term)) {
      {
        Vars = dimnames(dfbetas(Model))[[2]]
      }
    } else {
      if (!(Term %in% dimnames(dfbetas(Model))[[2]])) {
        stop("Term specified for DFBETAS not valid")
      } else Vars = Term
    }
    for (varname in Vars) {
      par(cex.lab = 1.5, cex.axis = 1.2, lwd = 2, ask = TRUE)
      TheTitle = paste("Model: ", Model$call[2], "\n", 
                       "B= ", coef(Model)[varname], sep = "")
      hist(dfbetas(Model)[, varname], xlab = paste("DFBETAS:", 
                                                   varname), main = TheTitle)
      points(dfbetas(Model)[, varname], rep(0, length(dfbetas(Model)[, 
                                                                     varname])), pch = "|", col = "blue")
      abline(v = c(-2, 2), col = "red")
      if (!is.null(ID)) {
        Indices = identify(dfbetas(Model)[, varname], 
                           rep(0, length(dfbetas(Model)[, varname])), 
                           labels = ID)
      }
    }
    par(ask = FALSE)
    if (is.null(Term)) {
      avPlots(Model, intercept = TRUE, id.method = "identify", 
              id.n = nrow(dfbetas(Model)), labels = ID)
    } else {
      avPlots(Model, terms = Term, id.method = "identify", 
              id.n = nrow(dfbetas(Model)), labels = ID)
      Values = dfbetas(Model)[Indices, Term]
    }
    if (is.null(Term)) {
      Indices = NULL
      Values = NULL
    }
  }, INFLUENCEPLOT = {
    par(cex.lab = 1.5, cex.axis = 1.2, lwd = 2)
    TheTitle = paste("Influence Bubble plot", "\nModel: ", 
                     Model$call[2], sep = "")
    plot(hatvalues(Model), rstudent(Model), type = "n", 
         xlab = "Hat Values", ylab = "Studentized Residuals", 
         main = TheTitle)
    cooksize = 10 * sqrt(cooks.distance(Model))/max(cooks.distance(Model))
    points(hatvalues(Model), rstudent(Model), cex = cooksize)
    N = length(rstudent(Model))
    k = length(coef(Model)) - 1
    TCut <- qt(p = 0.025/N, df = N - k - 2, lower.tail = FALSE)
    abline(h = c(-1, 0, 1) * TCut, col = "red", lty = c(1, 
                                                        2, 1))
    abline(v = c(1, 2, 3) * mean(hatvalues(Model)), col = "red", 
           lty = c(2, 1, 1))
    if (!is.null(ID)) {
      Indices = identify(hatvalues(Model), rstudent(Model), 
                         labels = ID)
      Values = c(hatvalues(Model)[Indices], rstudent(Model)[Indices])
    }
  }, COVRATIO = {
    N = length(covratio(Model))
    k = length(coef(Model)) - 1
    par(cex.lab = 1.5, cex.axis = 1.2, lwd = 2)
    TheTitle = paste("Model: ", Model$call[2], "\n", "abs((3*P)/N)-1 cut-off in red", 
                     sep = "")
    hist(covratio(Model), xlab = "CovRatio", main = TheTitle)
    abline(v = abs((3 * (k + 1)/N) - 1), col = "red")
    points(covratio(Model), rep(0, length(covratio(Model))), 
           pch = "|", col = "blue")
    if (!is.null(ID)) {
      Indices = identify(covratio(Model), rep(0, length(covratio(Model))), 
                         labels = ID)
      Values = covratio(Model)[Indices]
    }
  }, {
    print("Valid options for type: hatvalues, residuals, cooksd, dfbetas, influenceplot, covratio, univariate")
  })
  Rownames = row.names(Model$model)[Indices]
  Cases = list(Rownames = Rownames, Values = Values)
  return(Cases)
} # Outlier analyses
###############################################################################
# standardize ####
standardize <- function(var) {
  ((var - mean(var, na.rm = T))/(sd(var, na.rm = T)))
} # create z scores
###############################################################################
# figStripChart ####
figStripChart = function (x, side = 1, sshift = 0.3, adjoffset = 1, strip.col = "gray", 
                          strip.pch = 15, strip.cex = 0.2) 
{
  if (is.null(strip.col)) 
    strip.col = getOption("FigPars")$strip.col
  if (is.null(strip.pch)) 
    strip.pch = getOption("FigPars")$strip.pch
  if (is.null(strip.cex)) 
    strip.cex = getOption("FigPars")$strip.cex
  if (side == 1) {
    flip <- 1
    yaxis <- FALSE
    parside <- 3
  }
  else if (side == 2) {
    flip <- 1
    yaxis <- TRUE
    parside <- 1
  }
  else if (side == 3) {
    flip <- -1
    yaxis <- FALSE
    parside <- 4
  }
  else if (side == 4) {
    flip <- -1
    yaxis <- TRUE
    parside <- 2
  }
  base <- par("usr")[parside]
  plotwidth <- diff(par("usr")[1:2])
  plotheight <- diff(par("usr")[3:4])
  shift <- par("pin")[1] * 0.003 * flip
  gap <- par("pin")[1] * 0.003
  meanshift <- par("cin")[1] * 0.5 * flip
  stripshift <- par("cin")[1] * sshift * flip
  if (yaxis) {
    shift <- shift/par("pin")[1] * plotwidth
    meanshift <- meanshift/par("pin")[1] * plotwidth
    stripshift <- stripshift/par("pin")[1] * plotwidth
    gap <- gap/par("pin")[2] * plotheight
  }
  else {
    shift <- shift/par("pin")[2] * plotheight
    meanshift <- meanshift/par("pin")[2] * plotheight
    stripshift <- stripshift/par("pin")[2] * plotheight
    gap <- gap/par("pin")[1] * plotwidth
  }
  if (yaxis) 
    offset = flip * par("cin")[2]/par("cin")[1] * adjoffset
  else offset = flip * adjoffset
  oldxpd <- par(xpd = TRUE)
  on.exit(par(oldxpd))
  stripchart(x, method = "stack", vertical = yaxis, offset = offset, 
             pch = strip.pch, cex = strip.cex, add = TRUE, at = base + 
               shift + stripshift, col = strip.col)
} # This function is needed for varPlot to function
################################################################################
# varPlot ####
varPlot = function (TheVar, VarName = "", IDs = NULL, AddPoints = "Strip", 
          AddDensity = TRUE, Detail = 2) 
{
  print(paste("Descriptive statistics: ", VarName, sep = ""))
  print(describe(TheVar, Detail))
  par(cex.lab = 1.5, cex.axis = 1.2, lwd = 2)
  HistData = hist(TheVar, main = "", xlab = VarName)
  switch(toupper(AddPoints), STRIP = {
    figStripChart(TheVar, strip.col = "red", , strip.cex = 0.3)
  }, RUG = {
    rug(TheVar, col = "red")
  })
  if (AddDensity) {
    DensityData = density(TheVar)
    lines(DensityData$x, DensityData$y * (max(HistData$counts)/max(DensityData$y)), 
          col = "blue")
  }
  if (!is.null(IDs)) {
    Indices = identify(TheVar, rep(0, length(TheVar)), labels = IDs)
    return(Cases = list(Indices = Indices, Rownames = IDs[Indices], 
                        Values = TheVar[Indices]))
  }
}
################################################################################
# modelAssumptions ####
modelAssumptions = function (Model, Type = "NORMAL", ID = row.names(Model$model), 
          one.page = TRUE) 
{
  switch(toupper(Type), NORMAL = {
    if (one.page) {
      dev.new(width = 14, height = 7)
      par(mfrow = c(1, 2))
    } else {
      dev.new(width = 7, height = 7, record = TRUE)
    }
    par(cex.lab = 1.5, cex.axis = 1.2, lwd = 2)
    qqPlot(Model, labels = FALSE, sim = TRUE, main = "Quantile-Comparison Plot to Assess Normality", 
           xlab = "t Quantiles", ylab = "Studentized Residuals")
    plot(density(rstudent(Model)), main = "Density Plot to Assess Normality of Residuals", 
         xlab = "Studentized Residual")
    zx <- seq(-4, 4, length.out = 100)
    lines(zx, dnorm(zx, mean = 0, sd = sd(rstudent(Model))), 
          lty = 2, col = "blue")
    cat("Descriptive Statistics for Studentized Residuals\n")
    describe(rstudent(Model))
  }, CONSTANT = {
    if (one.page) {
      dev.new(width = 14, height = 7)
      par(mfrow = c(1, 2))
    } else {
      dev.new(width = 7, height = 7, record = TRUE)
    }
    par(cex.lab = 1.5, cex.axis = 1.2, lwd = 2)
    plot(rstudent(Model) ~ fitted.values(Model), main = "Studentized Residuals vs. Fitted Values", 
         xlab = "Fitted Values", ylab = "Studentized Residuals")
    abline(h = 0, lty = 2, col = "blue")
    print(spreadLevelPlot(Model))
    cat("\n\n")
    print(ncvTest(Model))
  }, LINEAR = {
    dev.new(width = 7, height = 7, record = TRUE)
    par(cex.lab = 1.5, cex.axis = 1.2, lwd = 2)
    crPlots(Model, ask = TRUE)
  }, {
    print("Valid options for type: normal, constant, linear")
  })
  print(gvlma(Model))
}


################################################################################
# modelBoxCox ####
modelBoxCox = function (Model, Lambdas = seq(-2, 2, by = 0.1)) 
{
  LR <- boxCox(Model, lambda = Lambdas)
  Lambda1Index <- sum(LR$x < 1)
  Chi1 <- mean(c(LR$y[Lambda1Index], LR$y[Lambda1Index + 1]))
  ChiLambda <- LR$y[which.max(LR$y)]
  ChiDiff <- 2 * (ChiLambda - Chi1)
  print(paste("Best Lambda=", round(LR$x[which.max(LR$y)], 
                                    2)))
  print(paste("Chi-square (df=1)=", round(ChiDiff, 2)))
  print(paste("p-value=", round(pchisq(ChiDiff, df = 1, lower.tail = FALSE), 
                                5)))
}

################################################################################
# quiet ####
# Allows you to run modelCaseAnalysis in r markdown without it forcing your to
# click on a point. You MUST still run modelCaseAnalysis in a separate r file if
# you wish to identify problematic points.
quiet = function(x) { 
  sink(tempfile()) 
  on.exit(sink()) 
  invisible(force(x)) 
} 

################################################################################
# read_merge_many ####
read_merge_many <- function(files, merge_by) {
  for (f in files) {
    print(f)
    df_f <- read_csv(paste("", f, sep = ""))
    if (exists('df') && is.data.frame(get('df'))) {
      df <- merge(df, df_f, by = merge_by)
    }
    else {
      df <- df_f
    }
  }
  return(df)
}
################################################################################