
#' Re-order model terms
#' @param m an lm objecct
#' @param d the data frame. If NULL, attempts to extract from m.
#' @param refit TRUE or FALSE
#' @param maxNPred maximum number of predictors to use, defaults to all.
#' @param collapse TRUE or FALSE
#' @return a vector of terms in order last to first, or an lm if refit=TRUE. regsubsetsOrder returns a list of predictor vectors, or a list of fits
#' @name reorderTerms
NULL




extractModelData <- function(f){
  d <- eval(f$call$data, environment(formula(f)))
  if (is.null(d)) d <- model.frame(f)
  d
}


refitModel <- function(m, predord,data){
  # update(m, as.formula(paste(" ~ ",paste(predord ,collapse="+"))))
  yname <- as.character(formula(m))[[2]]
  if (length(predord)==0)
    predord <- "1"
  newform <- as.formula(paste(yname,"~",paste(predord,collapse="+"), collapse=""))
  lm(terms(newform,keep.order=TRUE),data)
}


#' @describeIn reorderTerms Arranges model terms in order of increasing p-value
#' @export
pvalOrder <-function(m, d=NULL,refit=TRUE){
  if (is.null(d)) d <- extractModelData(m)
  pvals <- broom::tidy(car::Anova(m, type=3, singular.ok=TRUE))[-1,]$p.value
  pvals <- pvals[-length(pvals)]
  preds <- attr(terms(m), "term.labels")
  preds <- preds[order(pvals)]
   if (refit)
    refitModel(m, preds,d)
  else preds
}

#'@describeIn reorderTerms Arranges model terms using backwards selection
#' @export
#'
#' @examples bselOrder(lm(mpg~wt+hp+disp, data=mtcars))
bselOrder <- function(m, d=NULL,refit=TRUE,maxNPred=NULL) {
  if (is.null(d)) d <- extractModelData(m)
	mx <- m
	predord1 <- vector(length=length(attr(terms(m), "term.labels")))
	for (i in 1:length(predord1)) {
		a<- drop1(mx, test="F")[-1,]
		pdrop <- rownames(a)[which.min(a$`F value`)]
		predord1[i] <- pdrop
		mx <- update(mx, paste0("~ . - ", pdrop),data=d)
	}
	predord1 <- rev(predord1)
	if (! is.null(maxNPred)) {
	  maxNPred <- min(maxNPred, length(predord1))
	  predord1 <- predord1[1:maxNPred]
	}
	if (refit)
	  refitModel(m, predord1,d)
	else predord1
}



#' @describeIn reorderTerms Forwards selection
#' @export
#'
#' @examples fselOrder(lm(mpg~wt+hp+disp, data=mtcars))
fselOrder <- function(m,d=NULL,refit=TRUE, maxNPred=NULL) {
  if (is.null(d)) d <- extractModelData(m)
  f <- formula(m)
  preds1 <- attr(terms(m), "term.labels")
  # make sure m0 uses data
  m0 <- refitModel(m, NULL, d)
  mx <- m0
  if (! is.null(maxNPred))
    maxNPred <- min(maxNPred, length(preds1))
  else maxNPred <- length(preds1)
  predord1 <- vector(length=maxNPred)
  for (i in 1:maxNPred) {
    a<- add1(mx,f, test="F")[-1,]
    fval <- a$`F value`
    fval[is.na(fval)]<- 0
    padd <- rownames(a)[which.max(fval)]

    predord1[i] <- padd
    #mx <- update(mx, as.formula(paste0("~ . + ", padd)))
    mx <- refitModel(m0,predord1[1:i],d)
  }
  #print(mx)
  if (refit)
    mx
  else predord1
}



#' @describeIn reorderTerms Reverses order of terms in a fit
#' @export
#'
#' @examples revPredOrd(lm(mpg~wt+hp+disp, data=mtcars))

revPredOrd <- function(m, d=NULL,refit=TRUE){
	# does not make sense for models with interactions, but fitting lm will reorder them?
  # predord <- names(model.frame(m))[-1]
  if (is.null(d)) d <- extractModelData(m)
  predord <- attr(terms(m), "term.labels")
  predord1 <- rev(predord)

  if (refit){
    refitModel(m, predord1,d)
    # r <- names(model.frame(m))[1]
    # lm(paste(r," ~ ",paste(predord1 ,collapse="+")), data=model.frame(m))
  }
  else predord1
}

#' @describeIn reorderTerms Reorders terms in a fit randomly
#' @export
#'
#' @examples randomPredOrder(lm(mpg~wt+hp+disp, data=mtcars))

randomPredOrder <- function(m, d=NULL,refit=TRUE){
	# does not make sense for models with interactions, but fitting lm will reorder them?
  # predord <- names(model.frame(m))[-1]
  if (is.null(d)) d <- extractModelData(m)
  predord <- attr(terms(m), "term.labels")
  predord1 <- sample(predord,length(predord))

  if (refit){
    refitModel(m, predord1,d)
    # r <- names(model.frame(m))[1]
    # lm(paste(r," ~ ",paste(predord1 ,collapse="+")), data=model.frame(m))
  }
  else predord1
}

addPred <- function(m, pred,d=NULL, refit=TRUE){
  # add pred, last
  if (is.null(d)) d <- extractModelData(m)
  predord <- attr(terms(m), "term.labels")
  p <- match(pred, predord)
  if ( is.na(p)){
    predord <- c(predord, pred)
  }
  if (refit)
    refitModel(m, predord,d)
  else predord

}

removePred <- function(m, pred, d=NULL,refit=TRUE){
  # removes pred
  if (is.null(d)) d <- extractModelData(m)
  predord <- attr(terms(m), "term.labels")
  p <- match(pred, predord)
  if (! is.na(p)){
    predord <- predord[-p]
  }
  if (refit)
    refitModel(m, predord,d)
  else predord
}


upPredOrd <- function(m, pred, d=NULL,refit=TRUE){
	# moves pred up one position
  if (is.null(d)) d <- extractModelData(m)
  predord <- attr(terms(m), "term.labels")
  p <- match(pred, predord)
  if (p < length(predord)){
  	ind <- 1:length(predord)
  	ind[p]<- p+1
  	ind[p+1]<- p
  	predord <- predord[ind]
  }
  if (refit)
    refitModel(m, predord,d)
  else predord

}


downPredOrd <- function(m, pred,d=NULL, refit=TRUE){
	# moves pred down one position
  if (is.null(d)) d <- extractModelData(m)
  predord <- attr(terms(m), "term.labels")
  p <- match(pred, predord)
  if (p > 1){
  	ind <- 1:length(predord)
  	ind[p]<- p-1
  	ind[p-1]<- p
  	predord <- predord[ind]
  }
  if (refit)
    refitModel(m, predord,d)
  else predord
}

whichPredS <- function(m,y){
	ss <- cumsum(broom::tidy(anova(m))$sumsq)
	predord <- attr(terms(m), "term.labels")
	p <- which.max(ss>y)
	predord[which.max(ss>y)]
}

whichPredA <- function(m,y){
  predord <- attr(terms(m), "term.labels")
  index <- seq(along=predord)
  p <- which.min(abs(index - y))
  predord[p]
}




bind_model_tables <- function(models, summary=identity,...){
	# mseq <-  models %>% dplyr::map(summary) %>% dplyr::map(broom::tidy,...)	%>% dplyr::bind_rows(.id="model")

	models1 <- purrr::map(models, summary)
	models1 <- purrr::map(models1, broom::tidy,...)
	mseq <- dplyr::bind_rows(.id="model", models1)
	if (!is.null(names(models))){
         mseq$model <- factor(mseq$model, levels=names(models))
         } else mseq$model <- factor(mseq$model)
    mseq
}



#' @describeIn reorderTerms Best subsets regression.
#'
#' @export
#' @examples regsubsetsOrder(lm(mpg~wt+hp+disp, data=mtcars))
regsubsetsOrder <- function(m, d=NULL,refit=TRUE, collapse=TRUE){
  if (is.null(d)) d <- extractModelData(m)
  if (any(sapply(model.frame(m), is.factor))){
    print("This option not availabe for models with factors")
    return(NULL)
  }
  anm <- anova(m)
  f <- formula(m)
  nvmax <- length(variable.names(m)) -1
  v<- leaps::regsubsets(f,data=d,nvmax=nvmax)
  v <- summary(v)$which[,-1]
  if (collapse){
    p <- 1
    predlist <- vector(mode="list")
    predlist[[p]] <- which(v[1,])
    for (i in 2:nrow(v)){
      w <- v[i,] - v[i-1,]
      if (all(w >=0)) {
        predlist[[p]] <- c(predlist[[p]], which(w>0))
      }
      else{
        p <- p+1
        drop <- which(w < 0)
        predlist[[p]] <- predlist[[p-1]][- match(drop, predlist[[p-1]])]
        predlist[[p]] <- c(predlist[[p]], which(w>0))
      }
    }
  }
  else {
    vsum <- apply(v, 2, sum)
    vo <- v[, order(vsum, decreasing=T)]
    predlist <- apply(vo, 1, function(r) which(r))
  }

  if (refit){
    best <- lapply(predlist, function(p) refitModel(m, names(p),d))
    bestn <- sapply(predlist, length)
    bestn1 <- c(1, bestn[-length(bestn)]+1)
    paste("Best", paste(bestn1, bestn, sep="-"))
    names(best) <- paste("Best", paste(bestn1, bestn, sep="-"))
    if (bestn[1]==1)
     names(best)[1]<- "Best 1"
    best
  }
  else
    lapply(predlist, names)
  }





drop1_models <- function(model1, preds, data=NULL){
  # model1 changed by single drop of predictor
  if (is.null(data)) data <- extractModelData(model1)
  preds1 <- attr(terms(model1), "term.labels")
  # make sure model1 uses data
  model1 <- refitModel(model1, preds1, data)
  mfits <- list(model1)

   for (i in 1:length(preds)) {
    k <- match(preds[i], preds1)
    mfits[[i+1]] <- refitModel(model1, preds1[-k],data)
  }
  names(mfits)	 <- c("Start", preds)
  mfits
}



add1_models <- function(model1, preds, data=NULL){
  # model1 changed by sequential add of predictor
  if (is.null(data)) data <- extractModelData(model1)
  preds1 <- attr(terms(model1), "term.labels")
  # make sure model1 uses data
  model1 <- refitModel(model1, preds1, data)

  mfits <- list(model1)

  for (i in 1:length(preds)) {
    preds1 <- c(preds1, preds[i])
    mfits[[i+1]] <- refitModel(model1, preds1,data)
  }
  names(mfits)	 <- c("Start", preds)
  mfits
}







checkERModel <- function(m){
  if (!is(m, "lm"))
    return(FALSE)
  if (is(m, "glm"))
    return(FALSE)
  if (is(m, "aov"))
    return(FALSE)
  if (!is.null(weights(m)))
    return(FALSE)
  return(TRUE)
}

#' A function to launch the Exploratory Regression Shiny App
#'
#' @param ERmfull the lm fit to be explored
#' @param ERdata  the data used to fit the model. If NULL, attempts to extract from ERmfull.
#' @param ERbarcols a vector of colours, one per term in lm.
#' Will be expanded via colorRampPalette if not the correct length.

#' @param npcpCols number of colours for the PCP
#' @param pvalOrder if TRUE, re-arranges predictors in order of p-value
#' @return the shiny server
#' @export
#'
#' @examples
#' f <- lm(mpg ~ hp+wt+disp, data=mtcars)
#' \dontrun{exploreReg(f)}
#'
exploreReg <- function(ERmfull,ERdata=NULL, ERbarcols=RColorBrewer::brewer.pal(4, "Set2"),
                       npcpCols = 4,pvalOrder=F) {
  if (!checkERModel(ERmfull))
    stop("ERmfull must be an unweighted lm")
  ui <- createERUI()
  if (is.null(ERdata)) ERdata <- extractModelData(ERmfull)
 server <- createERServer(ERmfull, ERdata,ERbarcols, npcpCols,pvalOrder)
  shiny::shinyApp(ui, server,options=list(
    width="100%", height=900
  ))
}



