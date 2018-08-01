# Function definition for zcwm

# Dependencies 
library(flexCWM)
library(pscl)


#| FULL ZCWM FUNCTION 
#| =============================================================================================|                                                                  |
#| Nik Pocuca August 1st - 2018                                                                 |
#| Returns a zero-inflated cwm object                                                           |
#| =============================================================================================|

zcwm <- function(inputdata, formulaZP, np, Xnorms, ...){
  
  # Create zero space. 
  data_z <- inputdata
  
    #Get dependent variable.  
    dep_v_name <- as.character(formulaZP[2])
    dep_v_index <- match(dep_v_name,colnames(inputdata))
    
  
  data_z[,dep_v_index] <- as.integer(data_z[,dep_v_index] <= 0)
    
  cat('Beginning Partition using CWM','\n')
  cat('==============================','\n\n')

  # First Partition Poisson
  
  cat('\n')
  cat(' Poisson Model\n\n')
  
  attach(inputdata)
  cwm_poisson <- cwm(formulaY = formulaZP,
                     data = inputdata,
                     familyY = poisson(link= "log" ), Xnorm = Xnorms, k = np, ...)
  detach(inputdata)
  
  # Second Partition Bernoulli 
  
  cat('\n')
  cat('Bernoulli Zero Inflation Model \n\n')
  
  attach(data_z)
  cwm_bernoulli <- cwm(formulaY = formulaZP,
                     data = data_z,
                     familyY = binomial(link= "logit" ), Xnorm = Xnorms, k = np, ...)
  detach(data_z)
  
  
  # Match Partitions. 
  cat('Beginning Zero inflated CWM','\n')
  cat('==============================','\n\n')
  
  # Gather labels for splitting
  c_pois <- getCluster(cwm_poisson)
  c_bern <- getCluster(cwm_bernoulli)
  lex <- paste(c_pois,c_bern, sep="")
  partitions <- match(lex, unique(lex))
  
  dataspace <- cbind(inputdata,c_pois,c_bern,partitions)
  
  # Gather Vectors
  glm_pois <- getParGLM(cwm_poisson)
  glm_bern <- getParGLM(cwm_bernoulli)
  
  v_pois <- returnVectors(glm_pois)
  v_bern <- returnVectors(glm_bern)
  
  subSpaces <- genSubspaces(dataspace = dataspace,
                          vectors_p = v_pois,
                          vectors_b = v_bern)

  count <- 1
  hold_models <- list() 
  for (sub_space in subSpaces){
    zero_model <- optimizeZeroInflation(subspace = sub_space,formulaZ = formulaZP)
    hold_models[[count]] <- zero_model
    count <- count + 1
  }
  
  return(hold_models)
}

#| RESTRUCTURING COEFFICIENTS
#| =============================================================================================|
#| RETURN VECTORS FUNCTION                                                                      |
#| Nik Pocuca July 19th - 2017                                                                  |
#| Returns glm vectors in the form of a dataframe.                                              |
#| =============================================================================================|
returnVectors <- function(cwmGLM){
  
  # Get names
  vNames <-  names(cwmGLM$GLMComp.1$coefficients)
  
  
  # Placeholder dataframe.
  dataPlaceholder <-  data.frame()
  for(i in cwmGLM){
    dataPlaceholder <- rbind(dataPlaceholder, i$coefficients)
  }
  
  
  # Set Names for dataframe
  colnames(dataPlaceholder) <- vNames
  
  return(dataPlaceholder)} # END OF RETURN VECTORS FUNCTION
#| =============================================================================================|



#| GENERATING SUBSPACES FOR DATA
#| =============================================================================================|
#| GENERATE SUBSPACE FUNCTION                                                                   |
#| Nik Pocuca August 1st - 2018                                                                 |
#| Returns object with subspaces and their respective vectors.                                  |
#| =============================================================================================|

genSubspaces <- function(dataspace, vectors_p, vectors_b){
  
  subspace <- list()
  
  for (i in unique(dataspace$partitions)){
  
    s_space <- dataspace[partitions == i,]
    subspace[[i]] <- list(dta = s_space,
                          p_vector = vectors_p[s_space$c_pois[1],],
                          b_vector = vectors_b[s_space$c_bern[1],])
  }

  
  return(subspace)
}




#| ZERO-INFLATED MAXIMIZATION FUNCTIONS
#| =============================================================================================|
#| Modified zeroinfl function from library(pscl)                                                  |
#| Nik Pocuca August 1st - 2018                                                                 |
#| returns zero-inflated cwm object                                                              |
#| =============================================================================================|

optimizeZeroInflation <- function(subspace,formulaZ){

  input_data <- subspace$dta
  input_vectors <- list(pVector = subspace$p_vector,
                        bVector = subspace$b_vector)
  
  zero_model <- tryCatch({
                        
                  zero_run <- z_mk1(formula = formulaZP,
                                    data = subspace$dta,
                                    dist = "poisson",
                                    vectors = input_vectors)          
                  
                  return(zero_run)
                        },
                error = function(cond){
                        print(cond)
                        return("Error has a occured, could not zero-inflate for this partition")  
                        })
  return(zero_model)
}

#| =============================================================================================|
#| June 13th 2017 
#| Zero Inflated Poisson Function Mk-1 
#| =============================================================================================|

zeroinfl.control <- function(method = "BFGS", maxit = 10000, trace = FALSE, EM = FALSE, start = NULL, ...) {
  rval <- list(method = method, maxit = maxit, trace = trace, EM = EM, start = start)
  rval <- c(rval, list(...))
  if(!is.null(rval$fnscale)) warning("fnscale must not be modified")
  rval$fnscale <- -1
  if(!is.null(rval$hessian)) warning("hessian must not be modified")
  rval$hessian <- TRUE
  if(is.null(rval$reltol)) rval$reltol <- .Machine$double.eps^(1/1.6)
  rval
}


## convenience helper function
model_offset_2 <- function(x, terms = NULL, offset = TRUE)
  ## allow optionally different terms
  ## potentially exclude "(offset)"
{
  if(is.null(terms)) terms <- attr(x, "terms")
  offsets <- attr(terms, "offset")
  if(length(offsets) > 0) {
    ans <- if(offset) x$"(offset)" else NULL
    if(is.null(ans)) ans <- 0
    for(i in offsets) ans <- ans + x[[deparse(attr(terms, "variables")[[i + 1]])]]
    ans
  }
  else {
    ans <- if(offset) x$"(offset)" else NULL
  }
  if(!is.null(ans) && !is.numeric(ans)) stop("'offset' must be numeric")
  ans
}

# only one input added from pscl, vectors to be used as initializations, all there rest work the same as in pscl. 
z_mk1 <- function (formula, vectors, data, subset, na.action, weights, offset, 
                   dist = c("poisson", "negbin", "geometric"), link = c("logit", 
                                                                        "probit", "cloglog", "cauchit", "log"), control = zeroinfl.control(...), 
                   model = TRUE, y = TRUE, x = FALSE, ...) 
{
  ziPoisson <- function(parms) {
    mu <- as.vector(exp(X %*% parms[1:kx] + offsetx))
    phi <- as.vector(linkinv(Z %*% parms[(kx + 1):(kx + kz)] + 
                               offsetz))
    loglik0 <- log(phi + exp(log(1 - phi) - mu))
    loglik1 <- log(1 - phi) + dpois(Y, lambda = mu, log = TRUE)
    loglik <- sum(weights[Y0] * loglik0[Y0]) + sum(weights[Y1] * 
                                                     loglik1[Y1])
    loglik
  }
  
  
  gradPoisson <- function(parms) {
    eta <- as.vector(X %*% parms[1:kx] + offsetx)
    mu <- exp(eta)
    etaz <- as.vector(Z %*% parms[(kx + 1):(kx + kz)] + offsetz)
    muz <- linkinv(etaz)
    clogdens0 <- -mu
    dens0 <- muz * (1 - as.numeric(Y1)) + exp(log(1 - muz) + 
                                                clogdens0)
    wres_count <- ifelse(Y1, Y - mu, -exp(-log(dens0) + log(1 - 
                                                              muz) + clogdens0 + log(mu)))
    wres_zero <- ifelse(Y1, -1/(1 - muz) * linkobj$mu.eta(etaz), 
                        (linkobj$mu.eta(etaz) - exp(clogdens0) * linkobj$mu.eta(etaz))/dens0)
    colSums(cbind(wres_count * weights * X, wres_zero * weights * 
                    Z))
  }
  
  
  dist <- match.arg(dist)
  loglikfun <- switch(dist, poisson = ziPoisson, geometric = ziGeom, 
                      negbin = ziNegBin)
  gradfun <- switch(dist, poisson = gradPoisson, geometric = gradGeom, 
                    negbin = gradNegBin)
  
  linkstr <- match.arg(link)
  linkobj <- make.link(linkstr)
  linkinv <- linkobj$linkinv
  if (control$trace) 
    cat("Zero-inflated Count Model\n", paste("count model:", 
                                             dist, "with log link\n"), paste("zero-inflation model: binomial with", 
                                                                             linkstr, "link\n"), sep = "")
  cl <- match.call()
  if (missing(data)) 
    data <- environment(formula)
  mf <- match.call(expand.dots = FALSE)
  m <- match(c("formula", "data", "subset", "na.action", "weights", 
               "offset"), names(mf), 0)
  mf <- mf[c(1, m)]
  mf$drop.unused.levels <- TRUE
  if (length(formula[[3]]) > 1 && identical(formula[[3]][[1]], 
                                            as.name("|"))) {
    ff <- formula
    formula[[3]][1] <- call("+")
    mf$formula <- formula
    ffc <- . ~ .
    ffz <- ~.
    ffc[[2]] <- ff[[2]]
    ffc[[3]] <- ff[[3]][[2]]
    ffz[[3]] <- ff[[3]][[3]]
    ffz[[2]] <- NULL
  }
  else {
    ffz <- ffc <- ff <- formula
    ffz[[2]] <- NULL
  }
  if (inherits(try(terms(ffz), silent = TRUE), "try-error")) {
    ffz <- eval(parse(text = sprintf(paste("%s -", deparse(ffc[[2]])), 
                                     deparse(ffz))))
  }
  mf[[1]] <- as.name("model.frame")
  mf <- eval(mf, parent.frame())
  mt <- attr(mf, "terms")
  mtX <- terms(ffc, data = data)
  X <- model.matrix(mtX, mf)
  mtZ <- terms(ffz, data = data)
  mtZ <- terms(update(mtZ, ~.), data = data)
  Z <- model.matrix(mtZ, mf)
  Y <- model.response(mf, "numeric")
  if (length(Y) < 1) 
    stop("empty model")
  if (all(Y > 0)) 
    stop("invalid dependent variable, minimum count is not zero")
  if (!isTRUE(all.equal(as.vector(Y), as.integer(round(Y + 
                                                       0.001))))) 
    stop("invalid dependent variable, non-integer values")
  Y <- as.integer(round(Y + 0.001))
  if (any(Y < 0)) 
    stop("invalid dependent variable, negative counts")
  if (control$trace) {
    cat("dependent variable:\n")
    tab <- table(factor(Y, levels = 0:max(Y)), exclude = NULL)
    names(dimnames(tab)) <- NULL
    print(tab)
  }
  n <- length(Y)
  kx <- NCOL(X)
  kz <- NCOL(Z)
  Y0 <- Y <= 0
  Y1 <- Y > 0
  weights <- model.weights(mf)
  if (is.null(weights)) 
    weights <- 1
  if (length(weights) == 1) 
    weights <- rep.int(weights, n)
  weights <- as.vector(weights)
  names(weights) <- rownames(mf)
  offsetx <- model_offset_2(mf, terms = mtX, offset = TRUE)
  if (is.null(offsetx)) 
    offsetx <- 0
  if (length(offsetx) == 1) 
    offsetx <- rep.int(offsetx, n)
  offsetx <- as.vector(offsetx)
  offsetz <- model_offset_2(mf, terms = mtZ, offset = FALSE)
  if (is.null(offsetz)) 
    offsetz <- 0
  if (length(offsetz) == 1) 
    offsetz <- rep.int(offsetz, n)
  offsetz <- as.vector(offsetz)
  start <- control$start
  if (!is.null(start)) {
    valid <- TRUE
    if (!("count" %in% names(start))) {
      valid <- FALSE
      warning("invalid starting values, count model coefficients not specified")
      start$count <- rep.int(0, kx)
    }
    if (!("zero" %in% names(start))) {
      valid <- FALSE
      warning("invalid starting values, zero-inflation model coefficients not specified")
      start$zero <- rep.int(0, kz)
    }
    if (length(start$count) != kx) {
      valid <- FALSE
      warning("invalid starting values, wrong number of count model coefficients")
    }
    if (length(start$zero) != kz) {
      valid <- FALSE
      warning("invalid starting values, wrong number of zero-inflation model coefficients")
    }
    if (dist == "negbin") {
      if (!("theta" %in% names(start))) 
        start$theta <- 1
      start <- list(count = start$count, zero = start$zero, 
                    theta = as.vector(start$theta[1]))
    }
    else {
      start <- list(count = start$count, zero = start$zero)
    }
    if (!valid) 
      start <- NULL
  }
  
  if (control$trace) 
    cat("calling optim() for ML estimation:\n")
  method <- control$method
  hessian <- control$hessian
  ocontrol <- control
  
  start$count <- as.double(vectors$pVector)
  start$zero <- as.double(vectors$bVector)

  control$method <- control$hessian <- control$EM <- control$start <- NULL
  fit <- optim(fn = loglikfun, gr = gradfun, par = c(start$count, 
                                                     start$zero, if (dist == "negbin") log(start$theta) else NULL), 
               method = method, hessian = hessian, control = control)
  if (fit$convergence > 0) 
    warning("optimization failed to converge")
  coefc <- fit$par[1:kx]
  names(coefc) <- names(start$count) <- colnames(X)
  coefz <- fit$par[(kx + 1):(kx + kz)]
  names(coefz) <- names(start$zero) <- colnames(Z)
  
  
  
  vc <- -solve(as.matrix(fit$hessian))
  if (dist == "negbin") {
    np <- kx + kz + 1
    theta <- as.vector(exp(fit$par[np]))
    SE.logtheta <- as.vector(sqrt(diag(vc)[np]))
    vc <- vc[-np, -np, drop = FALSE]
  }
  else {
    theta <- NULL
    SE.logtheta <- NULL
  }
  colnames(vc) <- rownames(vc) <- c(paste("count", colnames(X), 
                                          sep = "_"), paste("zero", colnames(Z), sep = "_"))
  mu <- exp(X %*% coefc + offsetx)[, 1]
  phi <- linkinv(Z %*% coefz + offsetz)[, 1]
  Yhat <- (1 - phi) * mu
  res <- sqrt(weights) * (Y - Yhat)
  nobs <- sum(weights > 0)
  rval <- list(coefficients = list(count = coefc, zero = coefz), 
               residuals = res, fitted.values = Yhat, optim = fit, method = method, 
               control = ocontrol, start = start, weights = if (identical(as.vector(weights), 
                                                                          rep.int(1L, n))) NULL else weights, offset = list(count = if (identical(offsetx, 
                                                                                                                                                  rep.int(0, n))) NULL else offsetx, zero = if (identical(offsetz, 
                                                                                                                                                                                                          rep.int(0, n))) NULL else offsetz), n = nobs, df.null = nobs - 
                 2, df.residual = nobs - (kx + kz + (dist == "negbin")), 
               terms = list(count = mtX, zero = mtZ, full = mt), theta = theta, 
               SE.logtheta = SE.logtheta, loglik = fit$value, vcov = vc, 
               dist = dist, link = linkstr, linkinv = linkinv, converged = fit$convergence < 
                 1, call = cl, formula = ff, levels = .getXlevels(mt, 
                                                                  mf), contrasts = list(count = attr(X, "contrasts"), 
                                                                                        zero = attr(Z, "contrasts")))
  if (model) 
    rval$model <- mf
  if (y) 
    rval$y <- Y
  if (x) 
    rval$x <- list(count = X, zero = Z)
  class(rval) <- "zeroinfl"
  return(rval)
}

