#' Plots of model summaries
#' @param fit0 is an lm object
#' @param barcols a vector of colours, one per term in lm
#' @param preds terms to include in plot
#' @param alpha significance level
#' @param type "SS" or "F", from type 3 Anova
#' @param width bar width
#' @param stdunits TRUE or FALSE. If TRUE, coefficients refer to standardised predictor units.
#' @name plotSum
#' @return a ggplot
#' @name plotSum
#' @import ggplot2
#' @import grDevices
#' @import methods
#' @import stats
#' @import RColorBrewer
NULL





#' @describeIn plotSum Plots barchart of F or SS from lm
#' @export
#'
#' @examples plotAnovaStats(lm(mpg ~ wt+hp+disp, data=mtcars))

plotAnovaStats <- function(fit0, barcols=NULL,preds=NULL, alpha=.05, type="SS",width=.3){
   ssadj0 <- broom::tidy(car::Anova(fit0, type=3, singular.ok=TRUE))[-1,] # dont need -1, with type 2
  # ssadj0 <- subset(ssadj0, term != "Residuals")
  ssadj0 <- ssadj0[ssadj0$term != "Residuals",]
  if (! is.null(barcols)) ssadj0 <- correctTerms(ssadj0, names(barcols))
  if (type=="SS"){
    ssadj0$siglevel <-
      qf(1-alpha, ssadj0$df, fit0$df.residual)*summary(fit0)$sigma^2*ssadj0$df
    ylab <- "Adjusted SS"
    ssadj0$xvar <- ssadj0$sumsq
  }
  else  {
    ssadj0$siglevel <-
      qf(1-alpha, ssadj0$df, fit0$df.residual)
    ylab <- "F statistic"
    ssadj0$xvar <- ssadj0$statistic
  }

  if (is.null(preds)) preds <- ssadj0$term
  else {
    preds0 <- setdiff(preds, ssadj0$term)
    if (length(preds0) != 0){
      preds0 <- data.frame(preds0, matrix(NA, ncol=ncol(ssadj0)-1, nrow=length(preds0)))
      names(preds0)<- names(ssadj0)
      ssadj0 <- rbind(ssadj0,preds0)
    }
  }
  ssadj0$term <- factor(ssadj0$term , levels=union(preds, ssadj0$term))
  ssadj0$termnum <- match(ssadj0$term, levels(ssadj0$term))
  ssadj0$termnum1 <- ssadj0$termnum-.45
  ssadj0$termnum2 <- ssadj0$termnum+.45
  p0 <- ggplot(data = ssadj0, aes_string(x = "term", y = "xvar")) +
    geom_col(aes_string(fill = "term"), width=width)  +
    ylab(ylab) + xlab("")+ guides(fill = F) + coord_flip()


  if (! is.null(barcols))
    p0 <- p0 + scale_fill_manual(values = barcols, limits = preds)

  p0 +
    geom_segment(aes_string(x = "termnum1", y = "siglevel", xend = "termnum2", yend = "siglevel"), color = "darkred",
                 linetype = "dashed") +geom_hline(yintercept=0, color="grey30", size=1)

}

#' @describeIn plotSum Plots barchart of t stats from lm
#' @export
#'
#' @examples plottStats(lm(mpg ~ wt+hp+disp, data=mtcars))

plottStats <- function(fit0, barcols=NULL,preds=NULL, alpha=.05, width=.3){
  anFit0 <- anova(fit0)
  ssadj0 <- broom::tidy(summary(fit0))[-1,]

  df <- anFit0[-nrow(anFit0),]$Df
  if (any(df != 1)){
    print("t statistic not shown for terms with > 1 df")
    row <- unlist(sapply(df, function(i) rep(i,i))) ==1
    ssadj0 <- ssadj0[row,]
  }
  ssadj0$term <- labels(terms(fit0))[df==1] # corrects names for factors with 1df

  if (! is.null(barcols)) ssadj0 <- correctTerms(ssadj0, names(barcols))
  ssadj0$siglevel <-
    qt(1-alpha/2,  fit0$df.residual)
  ylab <- "T statistic"

  if (is.null(preds)) preds <- ssadj0$term
  else {
    preds0 <- setdiff(preds, ssadj0$term)
    if (length(preds0) != 0){
      preds0 <- data.frame(preds0, matrix(NA, ncol=ncol(ssadj0)-1, nrow=length(preds0)))
      names(preds0)<- names(ssadj0)
      ssadj0 <- rbind(ssadj0,preds0)
    }
  }
  ssadj0$term <- factor(ssadj0$term , levels=union(preds, ssadj0$term))
  ssadj0$termnum <- match(ssadj0$term, levels(ssadj0$term))

  ssadj0$termnum1 <- ssadj0$termnum-.45
  ssadj0$termnum2 <- ssadj0$termnum+.45

  p0 <- ggplot(data = ssadj0, aes_string(x = "term", y = "statistic")) +
    geom_col(aes_string(fill = "term"), width=width)  +
    ylab("t statistic") + xlab("")+ guides(fill = FALSE) + coord_flip()

  if (! is.null(barcols))
    p0 <- p0 + scale_fill_manual(values = barcols, limits = preds)

  p0 +
    geom_segment(aes_string(x = "termnum1", y = "siglevel", xend = "termnum2", yend = "siglevel"), color = "darkred",
                 linetype = "dashed")+
    geom_segment(aes_string(x = "termnum1", y = "-siglevel", xend = "termnum2", yend = "-siglevel"), color = "darkred",
                 linetype = "dashed") +
    geom_hline(yintercept=0, color="grey30", size=1)

}

#' Plots barcharts of sequential sums of squares of lm
#'
#' @param fits list of lm objects
#' @param barcols a vector of colours, one per term in lms
#' @param legend TRUE or FALSE
#'
#' @return a ggplot
#' @export
#'
#' @examples plotSeqSS(list(fit1= lm(mpg ~ wt+hp+disp, data=mtcars),
#' fit2=lm(mpg ~ wt*hp*disp, data=mtcars)))
plotSeqSS <- function(fits,barcols=NULL, legend=F){

  if (is(fits, "lm")) fits <- list(fits)

  mseq <- bind_model_tables(fits,anova)

  if (is.null(barcols)){
    p2 <- ggplot(data = mseq, aes_string(x = "model", y = "sumsq")) +
      geom_col(aes_string(fill = "term")) +
      guides(fill=F)  + theme(axis.ticks.x = element_blank())
  }
  else {
  mseq <- correctTerms(mseq, names(barcols))
  barcols1 <- barcols[names(barcols) %in% mseq$term]
  barcols2 <- barcols[mseq$term] # for the plot
  p2 <- ggplot(data = mseq, aes_string(x = "model", y = "sumsq")) +
    geom_col(aes(fill = "red"), width=.5)  +
    scale_fill_manual(values = barcols1,
                      limits = names(barcols1)) +
    guides(fill=F) +
    geom_col(fill = barcols2, width=.5) + theme(axis.ticks.x = element_blank())
  }
  if (is.null(names(fits)))
    p2 <- p2 + theme(axis.text.x = element_blank())


  if (legend) p2 <- p2 +   guides(fill = guide_legend(title = NULL, reverse = T))
  p2

}


pcp_data <- function(d, scale="std", color="black"){
  if (scale=="std")
    d<- scale(data.matrix(d, rownames.force=T))
  d <- data.frame(d, case= rownames(d), color=color)
  dg <- tidyr::gather(d, key="var", value="val", 1:(ncol(d) -2),factor_key=T)
  dg$varn <- as.numeric(dg$var)
  dg
}

regPCPdata <- function(fit,data=NULL, type="Variables", resDiff=F, absResid=F, color="black", sequential=F){
   if (is.null(data)) data <- extractModelData(fit)
  if (type == "Variables"){
    d <- model.frame(fit)
    if (ncol(d) != ncol(as.matrix(d)))
        d <- get_all_vars(formula(fit), data)
    dg <- pcp_data(d, color=color)
    ylab<- "Model data"
  }
  else if (type == "Residuals" & sequential){
    m0 <- refitModel(fit, NULL,data)
    d <- sapply(add1_models(m0, attr(terms(fit), "term.labels"),data ), residuals)
    if (resDiff){
      d1 <- d[, -1] - d[, -ncol(d)]
      d <- data.frame(d1, FULLresid=d[,ncol(d)])
      ylab <- "Change Sequential resids"
    }
    else {
      colnames(d)[1]<- "Intercept"
      ylab <- "Sequential resids"
    }
    if (absResid) {
      d<- abs(d)
      ylab <- paste("|", ylab,"|")
    }
    dg <- pcp_data(data.frame(d), scale=F, color=color)
  }
  else if (type == "Residuals"){
    d <- sapply(drop1_models(fit, attr(terms(fit), "term.labels"),data ), residuals)
    if (resDiff){
      d1 <- d[,-1] - d[,1]
      d <- data.frame(d1, FULLresid=d[,1])
      ylab <- "Drop1 resid - Full resid"
    }
    else { colnames(d)[1]<- "FULLresid"
    d <- d[,c(2:ncol(d),1)]
    ylab <- "Drop1 resids"
    }
    if (absResid) {
      d<- abs(d)
      ylab <- paste("|", ylab,"|")
    }
    dg <- pcp_data(data.frame(d), scale=F, color=color)
  }
  else if (type == "Hatvalues" & sequential){
    m0 <- refitModel(fit, NULL,data)
    d <- sapply(add1_models(m0, attr(terms(fit), "term.labels"),data ), hatvalues)
    d <- d[,-1]
     ylab <- "Sequential Hat values"
    dg <- pcp_data(data.frame(d), scale=F, color=color)
  }
  else if (type == "Hatvalues"){
    d <- sapply(drop1_models(fit, attr(terms(fit), "term.labels"),data ), hatvalues)
    colnames(d)[1]<- "FULLHat"
     d <- d[,c(2:ncol(d),1)]
    ylab <- "Drop1 Hat values"
    dg <- pcp_data(data.frame(d), scale=F, color=color)
  }
  else if (type == "CooksD" & sequential){
    m0 <- refitModel(fit, NULL,data)
    d <- sapply(add1_models(m0, attr(terms(fit), "term.labels"),data ), cooks.distance)
    d <- d[,-1]
    ylab <- "Sequential CooksD"
    dg <- pcp_data(data.frame(d), scale=F, color=color)
  }
  else if (type == "CooksD"){
    d <- sapply(drop1_models(fit, attr(terms(fit), "term.labels"),data ), cooks.distance)
    colnames(d)[1]<- "FULLHat"
    d <- d[,c(2:ncol(d),1)]
    ylab <- "Drop1 CooksD"
    dg <- pcp_data(data.frame(d), scale=F, color=color)
  }

  list(pcp_data=dg,ylab=ylab)

}


correctTerms <- function(ssadj0, termNames) {
  predf <- which(!ssadj0$term %in% termNames)
  if (length(predf)==0)
    ssadj0
  else {
    predss <- strsplit(ssadj0$term[predf], ":")
    newterms <- lapply(predss, function(p) sapply(combinat::permn(p), function(a) paste(a, collapse = ":")))
    for (k in 1:length(predf)) {
      predf1 <- ssadj0$term[predf][k]
      predf1
      # find replacement term for predfk. One of the kth element of newterms belows to termNames
      pref1r <- match(newterms[[k]], termNames)
      pref1r <- na.omit(pref1r)
      if (length(pref1r) == 1)
        ssadj0$term[predf][k] <- termNames[na.omit(pref1r)]
      else ssadj0$term[predf][k] <- "Unmatched"
    }
    ssadj0
  }
}









#' Constructs colour vector for model terms
#'
#' @param f a model fit with term labels
#' @param pal use this palette
#'
#' @return a vector of colours. Residuals are given a grey color
#' @export
#'
#' @examples termColours(lm(mpg ~ wt+hp, data=mtcars))
termColours <- function(f, pal=RColorBrewer::brewer.pal(4, "Set2")){
  preds <- attr(terms(f), "term.labels")
  levelnames<-c(preds,"Unmatched", "Residuals")
  if (length(pal)== length(preds))
    cols <- c(pal, "khaki", "Grey")
  else {
    getPalette <- colorRampPalette(pal)
    cols <- getPalette(length(preds))
    colsm<- suppressWarnings(matrix(cols, ncol=2))
    cols <- t(colsm)[1:length(cols)]
    cols <- c(cols, "khaki", "Grey")
  }
  names(cols)<- levelnames
  cols
}


#' @describeIn plotSum Plots confidence intervals from lm
#' @export
#'
#' @examples plotCIStats(lm(mpg ~ wt+hp+disp, data=mtcars))
plotCIStats <- function(fit0, barcols=NULL,preds=NULL, alpha=.05, stdunits=FALSE, width=.3){
  anFit0 <- anova(fit0)
  ci <- confint(fit0,level= 1-alpha)[-1,]
  if (stdunits){
    labs <- labels(terms(fit0))
    f <- model.matrix(fit0)
    for (i in 1:nrow(ci)){
      if (labs[i] %in% colnames(f)){
        x <- f[, labs[i]]
        s <- sd(x)
        ci[i,]<- ci[i,]*s
      }
      }
    }

  ci <- data.frame(term=rownames(ci), ci)
  names(ci) <- c("term", "cil", "ciu")
  df <- anFit0[-nrow(anFit0),]$Df
  if (any(df != 1)){
    print("t statistic not shown for terms with > 1 df")
    row <- unlist(sapply(df, function(i) rep(i,i))) ==1
    ci <- ci[row,]
  }

  ci$term <- labels(terms(fit0))[df==1] # corrects names for factors with 1df

  if (! is.null(barcols)) ci <- correctTerms(ci, names(barcols))



  if (is.null(preds)) preds <- ci$term
  else {
    preds0 <- setdiff(preds, ci$term)
    if (length(preds0) != 0){
      preds0 <- data.frame(preds0, matrix(NA, ncol=ncol(ci)-1, nrow=length(preds0)))
      names(preds0)<- names(ci)
      ci <- rbind(ci,preds0)
    }
  }
  ci$term <- factor(ci$term , levels=union(preds, ci$term))
  ci$termnum <- match(ci$term, levels(ci$term))
  ci$center <- (ci$cil+ci$ciu)/2
  p0 <- ggplot(data = ci, aes_string(y = "termnum", yend="termnum", x = "ciu", xend="cil")) +
    geom_segment(aes_string(colour = "term"), size=30*width)  +
     xlab("Confint") + ylab("")+
    guides(colour = FALSE)

  if (! is.null(barcols))
    p0 <- p0 + scale_colour_manual(values = barcols, limits = preds)

  p0 + geom_point(aes_string(y = "termnum", x = "center"), fill="black",size=15*width)+
    scale_y_continuous(breaks=ci$termnum,labels=ci$term) +
    geom_vline(xintercept=0, color="grey30", size=1)
}


pcpColors <- function(fit,input, npcpCols=4, absRes=FALSE){
  if (input=="Variables") {
    color <- cut(model.frame(fit)[,1],npcpCols)
    clab <- names(model.frame(fit))[1]
  }
  else if (absRes | input == "HAT") {
    color <- cut(abs(residuals(fit)),npcpCols)
    clab <- "|Full resids|"
  }
  else {
    color <- cut(residuals(fit),npcpCols)
    clab <- "Full resids"
  }
  list(clab=clab, color=color)
}



#' A PCP plot of the data, residuals or hat values from regression fits
#'
#' @param data a data frame
#' @param fit a lm for the data frame
#' @param type one of "Variables", "Residuals", "Hatvalues"
#' @param npcpCols number of colours
#' @param resDiff difference residuals, TRUE or FALSE
#' @param absResid absolute residuals, TRUE or FALSE
#' @param sequential use sequential fits (TRUE) or drop1 fits (FALSE)
#' @param selnum row numbers of cases to be highlighted
#'
#' @return ggplot
#' @export
#'
#' @examples
#' f <- lm(mpg ~ wt+hp+disp, data=mtcars)
#' pcpPlot(mtcars, f, type="Residuals")

pcpPlot <- function(data, fit,  type="Variables", npcpCols=4, resDiff=F, absResid=F,sequential=F, selnum=NULL){
  cols <- pcpColors(fit, type, npcpCols,absResid)
  r <- regPCPdata(fit,data,type,resDiff,absResid, color=cols$color, sequential=sequential)
  dg <- r$pcp_data
  #dsel <- dplyr::filter(dg , case %in% selnum)
  dsel <- dg[dg$case%in% selnum, ]
  ggplot(dg, aes_string(x = "varn", y = "val", group = "case",color="color")) +
    geom_line() + xlim(levels(dg$var))+ ylab(r$ylab) +
    geom_line(data = dsel, color="magenta", size=1.5) + xlab("") +labs(colour= cols$clab)
}
