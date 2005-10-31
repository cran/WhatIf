whatif <- function(formula = NULL, data, cfact, range = NULL, freq = NULL, nearby = NULL, 
    miss = "list", return.inputs = FALSE, ...)  {

    #INSTALL AND LOAD REQUIRED PACKAGE lpSolve
    if (!("lpSolve" %in% .packages(all = TRUE))) {
      install.packages("lpSolve")
    }
    require(lpSolve)

    #DATA PROCESSING AND RELATED USER INPUT ERROR CHECKING
    #Initial processing of cfact
    if(!((is.character(cfact) && is.vector(cfact) && length(cfact) == 1) || 
      is.data.frame(cfact) || (is.matrix(cfact) && !is.character(cfact)))) {
      stop("'cfact' must be either a string, a R data frame, or a R non-character matrix")
    }
    if (is.character(cfact))  {
      cfact <- read.table(cfact)
    }
    if (dim(cfact)[1] == 0)  {
      stop("no counterfactuals supplied: 'cfact' contains zero rows")
    }
    if (!any(complete.cases(cfact)))  {
      stop("there are no cases in 'cfact' without missing values")
    }
    if ("(Intercept)" %in% dimnames(cfact)[[2]])  {
      cfact <- cfact[, -(which(dimnames(cfact)[[2]] == "(Intercept)"))]
    }
    #Initial processing of data
    if (is.list(data) && !(is.data.frame(data)))  {
      if (!((("formula" %in% names(data)) || ("terms" %in% names(data))) && 
        (("data" %in% names(data)) || ("model" %in% names(data)))))  {
        stop("the list supplied to 'data' is not a valid output object")
      }
      tt <- terms(data)
      attr(tt, "intercept") <- rep(0, length(attr(tt, "intercept")))
      if ("data" %in% names(data))  {
        if (is.data.frame(data$data))  {
          data <- model.matrix(tt, model.frame(tt, data = data$data, na.action = NULL))
        }  else  {
          data <- model.matrix(tt, model.frame(tt, data = eval(data$data, env = .GlobalEnv), 
            na.action = NULL))
        }
      }  else  {
        data <- model.matrix(tt, data = data$model)
      }
      if (!(is.matrix(data)))  {
        stop("observed covariate data could not be extracted from output object")
      }
      rm(tt)
    } else {
      if(!((is.character(data) && is.vector(data) && length(data) == 1) || 
        is.data.frame(data) || (is.matrix(data) && !is.character(data)))) {
        stop("'data' must be either a string, a R data frame, a R non-character matrix, or an output object")
      }
      if (is.character(data))  {
        data <- read.table(data)
      }
    }
    if (dim(data)[1] == 0)  {
      stop("no observed covariate data supplied: 'data' contains zero rows")
    }
    if (!any(complete.cases(data)))  {
      stop("there are no cases in 'data' without missing values")
    }
    #Secondary processing of data and cfact: use formula
    if (!(is.null(formula)))  {
      if (identical(class(formula), "formula"))  {
        if (!(is.data.frame(as.data.frame(data))))  {
          stop("'data' must be coercable to a data frame in order to use 'formula'")
        }
        if (!(is.data.frame(as.data.frame(cfact))))  {
          stop("'cfact' must be coercable to a data frame in order to use 'formula'")
        }
        formula <- update.formula(formula, ~ . -1)
        ttvar <- all.vars(formula)
        for (i in 1:length(ttvar))  {
          if (!(ttvar[i] %in% dimnames(data)[[2]])){
            stop("variable(s) in 'formula' either unlabeled or not present in 'data'")
          }
          if (!(ttvar[i] %in% dimnames(cfact)[[2]])){
            stop("variable(s) in 'formula' either unlabeled or not present in 'cfact'")
          }
        }
        rm(ttvar)
        data <- model.matrix(formula, data = model.frame(formula, as.data.frame(data), 
          na.action = NULL))
        cfact <- model.matrix(formula, data = model.frame(formula, as.data.frame(cfact), 
          na.action = NULL))
      } else {
        stop("'formula' must be of class 'formula'")
      }
    }
    if (!(identical(complete.cases(cfact), rep(TRUE, dim(cfact)[1])))) {
      cfact <- na.omit(cfact)
      warning("Counterfactuals with missing values eliminated from cfact")
    }
    #Tertiary processing of data and cfact:  convert to numeric matrices
    if (is.data.frame(data))  {
      if (is.character(as.matrix(data)))  {
        stop("observed covariate data not coercable to numeric matrix due to character column(s)")
      }
      data <- suppressWarnings(data.matrix(data))
    }  else  {
      data <- data.matrix(as.data.frame(data))
    }
    if (is.data.frame(cfact))  {
      if (is.character(as.matrix(cfact)))  {
        stop("counterfactual data not coercable to numeric matrix due to character column(s)")
      }
      cfact <- suppressWarnings(data.matrix(cfact))
    }  else  {
      cfact <- data.matrix(as.data.frame(cfact))
    }
    #Final checks on data and cfact
    if (!(is.matrix(data) && is.numeric(data)))  {
      stop("observed covariate data not coercable to numeric matrix")
    }
    if (!(is.matrix(cfact) && is.numeric(cfact)))  {
      stop("counterfactual data not coercable to numeric matrix")
    }
    na.fail(cfact)

    #NON DATA-PROCESSING RELATED USER INPUT ERROR CHECKING
    #Check if cfact, data have the same number of dimensions, k
    if (!identical(ncol(cfact), ncol(data)))  {
      stop("number of columns of 'cfact' and 'data' are not equal")
    }
    #Check format of range if user supplies an argument
    if (!(is.null(range)))  {
      if (!(is.vector(range) && is.numeric(range)))  {
        stop("'range' must be a numeric vector")
      }
      if (!identical(length(range), ncol(data)))  {
        stop("length of 'range' does not equal number of columns of 'data'")
      }
    }
    #Check format of freq if user supplies an argument
    if (!(is.null(freq)))  {
      if (!(is.vector(freq) && is.numeric(freq)))  {
        stop("'freq' must be a numeric vector")
      }
      na.fail(freq)
    }
    #Check if nearby argument is numeric, a scalar, and >= 0, if supplied
    if (!(is.null(nearby))) {
      if (!(is.numeric(nearby) && is.vector(nearby) && length(nearby) == 1 && 
        nearby >= 0))  {
        stop("'nearby' must be numeric, greater than or equal to 0, and a scalar")
      }
    }
    #Check if miss argument is valid
    if (!(identical(miss, "list") || identical(miss, "case")))  {
      stop("'miss' must be either ''case'' or ''list''")
    }

    #KEY LOCAL VARIABLES
    n = nrow(data)  #Number of data points in observed data set (initially including missing)
    m = nrow(cfact)  #Number of (fully observed) counterfactuals
    k = ncol(data)  #Number of dimensions (covariates)

    #LOCAL FUNCTIONS
    convex.hull.test <- function(m, dat, cf)  {
      test.result <- FALSE
      #Create objects required by lp function, adding a row of 1s to transposed matrix 
      #dat and a 1 to vector cf[m,].  Note that "A" here corresponds to "A'" in King
      #and Zeng 2006a, Appendix A, and "B" and "C" to their "B" and "C", respectively.
      A <- rbind(t(dat), rep(1, nrow(dat)))
      B <- c(cf[m,],1)
      C <- c(rep(0, nrow(dat)))
      D <- c(rep("=", ncol(dat) + 1))
	
      #Use lp to implement test.
      lp.result <- lp(objective.in=C, const.mat=A, const.dir=D, const.rhs=B)
      if (lp.result$status == 0)  {
        test.result <- TRUE
      }
      return(test.result)
    }	

    calc.gd <- function(m, dat, cf, rang) {
      #If range =  0 for a variable k, set the normalized difference
      #equal to 0 if, for a given observed data point p, its
      #kth element minus the kth element of the counterfactual is 0. 
      #Otherwise set equal to NA, thus ignoring the contribution of the kth
      #variable to the calculation of Gower's distance.
      #Note that an element of the range vector should only be 0 in degenerate 
      #cases.
      if (identical(TRUE, any(rang == 0)))  {
        temp <- t(abs(t(dat) - cf[m,]))
        for (j in 1:length(rang))  {
          for (i in 1:nrow(dat))  {
            if (identical(0, rang[j]))  {
	      if (!identical(0, temp[i, j]))  {
                is.na(temp[i, j]) <- TRUE
              }
            }  else {
	      temp[i, j] <- temp[i, j]/rang[j]
	    }
	  }
        }
        prelim.gd <- apply(temp, 1, mean, na.rm = TRUE)
      }  else  {
        prelim.gd <- apply(t(abs(t(dat) - cf[m,])/rang), 1, mean, na.rm = TRUE)
 
      }
      for (i in 1:length(prelim.gd))  {
        if (is.nan(prelim.gd[i]))  {
          is.na(prelim.gd[i]) <- TRUE
        }
      }
      return(prelim.gd)
    }

    calc.cumfreq <- function(i, freq, dist)  {
      colSums(dist <= freq[i])  
    }

    #MISSING DATA
    is.miss <- !identical(complete.cases(data), rep(TRUE, n))
    if (is.miss && identical(miss, "list"))  {
      data <- na.omit(data)
      n <- nrow(data)
    }

    #CONVEX HULL TEST
    #If the mth counterfactual is one of the points in data,
    #return that it is in the convex hull; if it is not one of the 
    #points in data, use linear programming to test if it is in
    #the convex hull.
    if (is.miss && identical(miss, "case"))  {
      test.result <- sapply(1:m, convex.hull.test,  
        dat = na.omit(data), cf = cfact)
    }  else  {
      test.result <- sapply(1:m, convex.hull.test, dat = data, cf = cfact)
    }

    #GOWER'S DISTANCE
    samp.range <- apply(data, 2, max, na.rm = TRUE) - apply(data, 2, min, na.rm = TRUE)
    if (identical(range, NULL))  {
      gowdist <- matrix(sapply(1:m, calc.gd, dat = data, cf = cfact,
        rang = samp.range), ncol = m)
    }  else  {
      for (i in 1:k)  {
        if (!is.na(range[i]))  {
          samp.range[i] <- range[i]
        }
      }
      gowdist <- matrix(sapply(1:m, calc.gd, dat = data, cf = cfact, rang = samp.range), 
        ncol = m)
    }
    dimnames(gowdist) <- list(seq(1, n, by = 1), seq(1, m, by = 1))
   
    #GEOMETRIC VARIANCE
    gd.x <- matrix(sapply(1:n, calc.gd, dat = data, cf = data,
      rang = samp.range), ncol = n)
    sum.gd.x <- sum(apply(gd.x, 1, sum, na.rm = TRUE))
    gv.x <- (0.5 * sum.gd.x)/(n^2)
	
    #SUMMARY STATISTIC
    if (is.null(nearby))  {
      nearby <- gv.x
    }
    summary <- colSums(gowdist <= nearby) * (1/n)

    #CUMULATIVE FREQUENCIES
    if (is.null(freq))  {
      freqdist <- seq(0, 1, by = 0.05)
    } else {
      freqdist <- freq
    }
    cumfreq <- matrix((sapply(1:length(freqdist), calc.cumfreq,
      freq = freqdist, dist = gowdist))*(1/n), ncol = length(freqdist))
    dimnames(cumfreq) <- list(seq(1, m, by = 1), freqdist)

    #RETURN
    if (return.inputs)  {
      out <- list(call = match.call(), inputs = list(data = data, cfact = cfact),
        in.hull = test.result, gowers.dist = t(gowdist), geom.var = gv.x, 
        sum.stat = summary, cum.freq = cumfreq)
    }  else  {
      out <- list(call = match.call(), in.hull = test.result, gowers.dist = t(gowdist), 
        geom.var = gv.x, sum.stat = summary, cum.freq = cumfreq)
    }
    class(out) <- "whatif"
    if (identical(TRUE, any(samp.range == 0)))  {
      warning("Range of at least one variable equals zero")
    }
    return(invisible(out))
}
