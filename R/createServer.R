
#' A function which returns a shiny server for Exploratory Regression
#'
#' @param ERfit the lm fit to be explored
#' @param ERdata the data used to fit the model. If NULL, attempts to extract from ERfit.
#' @param ERbarcols a vector of colours, one per term in lm.
#' Will be expanded via colorRampPalette if not the correct length.
#' @param ERnpcpCols number of colours for the PCP
#' @param pvalOrder if TRUE, re-arranges predictors in order of p-value
#'
#' @return a function
#' @import shiny

createERServer <- function(ERfit,ERdata=NULL,ERbarcols=RColorBrewer::brewer.pal(4, "Set2"),ERnpcpCols=4,  pvalOrder=F){

  if (!is(ERfit, "lm") | is(ERfit, "glm"))
    stop("ERfit must be an lm and not a glm")
  if (is.null(ERdata)) ERdata <- extractModelData(ERfit)
function(input, output,session) {
  # mfull <- ERfit
  # barcols <- ERbarcols
  # npcpCols <- ERnpcpCols

  ERtermcols <- NULL

  initER <- function(){
    if (pvalOrder)
      ERfit <<- pvalOrder(ERfit, ERdata)
    ERtermcols <<- termColours(ERfit, ERbarcols)
  }



  initER()


  options(warn=-1)

  rv <- reactiveValues(fit0=ERfit,fit1 = ERfit, fit2=revPredOrd(ERfit,ERdata))
  rd <- reactiveValues(mdata=ERdata, pcp_data= NULL,sel=NULL)
  barpYlim <- NULL
  barpXlim <- NULL
  preds <- names(ERtermcols)[1:(length(ERtermcols)-2)]

  updateFit <- function(fit,order, pred){
    fit0 <- rv$fit0
    data <- rd$mdata
    if (order =="Default"){
      fit0
    }
    else if (order == "RevDefault"){
      revPredOrd(fit0,data)
    }
    else if (order == "Forward"){
      fselOrder(fit0,data)
    }
    else if (order == "Backward"){
      bselOrder(fit0, data)
    }
    else if (order == "Random") {
      randomPredOrder(fit0,data)
    }
    else if (order == "Add") {
      addPred(fit, pred, data)
    }
    else if (order == "Remove") {
      removePred(fit, pred, data)
    }
    else if (order == "data") {
      refitModel(fit, attr(terms(fit), "term.labels"), data)
    }
    else fit
  }

  # observeEvent(input$sourceF, {
  #   source(input$sourceF$datapath, local=T)
  #   if (!checkERModel(ERfit))
  #     stop("Source file must contain an unweighted lm")
  #   session$reload()
  #
  # })

  # observeEvent(input$build_model, {
  #   newlm <- input$newlm
  #   newmod <- eval(parse(text=newlm))
  #    if (!checkERModel(newmod)){
  #      print("Text does not evaluate to an unweighted lm")
  #   }
  #    else {
  #     ERfit <<-newmod
  #     session$reload()
  #   }
  # })


  observeEvent(input$all_terms, {
    terms0 <- attr(terms(ERfit), "term.labels")

    rv$fit0 <-refitModel(rv$fit0, terms0, rd$mdata)
    rv$fit1 <- updateFit(rv$fit0, input$order1)
    rv$fit2 <- updateFit(rv$fit0, input$order2)
  })

  observeEvent(input$order1, {
    rv$fit1 <- updateFit(rv$fit1, input$order1)
  })

  observeEvent(input$order2, {
    rv$fit2 <- updateFit(rv$fit2, input$order2)
   })

  observeEvent(input$plot_clickA, {
    click <- input$plot_clickA
    predi <- seq(along=preds)
    p <- which.min(abs(predi - click$y))
    pred <- preds[p]
    terms0 <- attr(terms(rv$fit0), "term.labels")
    change <- NULL
    if  (pred %in% terms0){
      if (length(terms0)> 1){
        #print( paste0("Removing term ", pred))
        change <- "Remove"
        terms0 <- terms0[- match(pred, terms0)]
      }
    }
    else {
      change <- "Add"
      terms0 <- c(terms0, pred)
      terms0 <- preds[sort(match(terms0, preds))]
    }
    rv$fit0 <-refitModel(rv$fit0, terms0, rd$mdata)

    if (! is.null(change)){
    if (input$order1 %in% c("Default","RevDefault", "Forward", "Backward"))
      rv$fit1 <- updateFit(rv$fit0, input$order1)
    else rv$fit1 <- updateFit(rv$fit1, change,pred)

    if (input$order2 %in% c("Default", "RevDefault","Forward", "Backward"))
      rv$fit2 <- updateFit(rv$fit0, input$order2)
    else rv$fit2 <- updateFit(rv$fit2, change,pred)
    }

  })



  # observeEvent(input$plot_hoverS, {
  #  ft1 <- tidy(anova(rv$fit1))
  #  r1 <- ft1$sumsq
  #  r1 <- (1 - last(r1)/sum(r1))*100
  #  r1 <- format(r1, digits=3)
  #  mse <- format(sqrt(last(ft1$meansq)/last(ft1$df)),digits=2)
  #   ft2 <- tidy(anova(rv$fit2))
  #   infoString1 <- paste("Plot2:",paste(ft1$term,sapply(ft1$sumsq, format, digits=2), collapse=" "),
  #                        paste0("Rsq ", r1, "%", " MSE ", mse))
  #   infoString2 <- paste("Plot3:",paste(ft2$term,sapply(ft2$sumsq, format, digits=2), collapse=" "),
  #                        paste0("Rsq ", r1, "%", " MSE ", mse))
  #   rd$infoString <- paste0(infoString1,"\n", infoString2)
  # })

  observeEvent(input$plot_clickS, {
    click <- input$plot_clickS
   #  print(c(click$x, click$y))
    if (click$x > 1.75 & click$x < 2.25 & input$order2=="Interact") {
      p <- whichPredS(rv$fit2,click$y)
      if (!is.na(p)){
        rv$fit2 <- upPredOrd(rv$fit2,p,rd$mdata)
      }
    }
    else if (click$x > .75 & click$x < 1.25  & input$order1=="Interact"){
      p <- whichPredS(rv$fit1,click$y)
      if (!is.na(p)){
        rv$fit1 <- upPredOrd(rv$fit1,p, rd$mdata)
      }
    }
  })

  observeEvent(input$plot_dblclickS, {
    click <- input$plot_dblclickS
    if (click$x > 1.75 & click$x < 2.25 & input$order2=="Interact") {
      p <- whichPredS(rv$fit2,click$y)
      if (!is.na(p)){
        rv$fit2 <- downPredOrd(rv$fit2,p, rd$mdata)
      }
    }
    else if (click$x > 0.75 & click$x < 1.25  & input$order1=="Interact"){
      p <- whichPredS(rv$fit1,click$y)
      if (!is.na(p)){
        rv$fit1 <- downPredOrd(rv$fit1,p, rd$mdata)
        }
    }
  })



  observeEvent(input$restore_all, {
    rd$mdata <- ERdata
    # rd$sel<- NULL

    rv$fit0 <- updateFit(rv$fit0, "data",pred)
    if (input$order1 %in% c("Interact", "Random"))
      rv$fit1 <- updateFit(rv$fit1, "data",pred)
    else  rv$fit1 <- updateFit(rv$fit1, input$order1)

    if (input$order2 %in% c("Interact", "Random"))
      rv$fit2 <- updateFit(rv$fit2, "data",pred)
    else  rv$fit2 <- updateFit(rv$fit2, input$order2)

  })

  observeEvent(input$remove_brushed, {
    sel <- rd$sel
    if (! is.null(sel)){
      del <- rownames(rd$mdata) %in% sel$case
      rd$mdata <- rd$mdata[!del,]

      rv$fit0 <- updateFit(rv$fit0, "data",pred)
      if (input$order1 %in% c("Interact", "Random"))
        rv$fit1 <- updateFit(rv$fit1, "data",pred)
      else  rv$fit1 <- updateFit(rv$fit1, input$order1)

      if (input$order2 %in% c("Interact", "Random"))
        rv$fit2 <- updateFit(rv$fit2, "data",pred)
      else  rv$fit2 <- updateFit(rv$fit2, input$order2)
      # rd$sel<- NULL
    }
  })

  observeEvent(input$pcp_dblclick, {
    if (! is.null(rd$sel)){
      rd$sel<-NULL
    }
  })

  observeEvent(input$plot_brush, {
    sel <-  brushedPoints(rd$pcp_data, input$plot_brush)
    if (nrow(sel) ==0)
      rd$sel<-NULL
    else
    rd$sel<-rbind(sel, rd$sel)
    # if (nrow(sel) !=0){
    #     del <- rownames(rd$mdata) %in% sel$case
    #  }

  })




  output$barPlotA <- renderPlot({
    if (input$stat == "Adj. SS"){
     plotAnovaStats(rv$fit0, ERtermcols, preds,as.numeric(input$alpha), type="SS")+
     xlab("")+ ylab("")
    }
    else if (input$stat == "F stat"){
      plotAnovaStats(rv$fit0, ERtermcols, preds,as.numeric(input$alpha), type="F")+
        xlab("")+ ylab("")
    }
    else if (input$stat == "t stat") {
       plottStats(rv$fit0, ERtermcols, preds,as.numeric(input$alpha))+
        xlab("")+ ylab("")
    }
    else if (input$stat == "CI") {
      plotCIStats(rv$fit0, ERtermcols, preds,as.numeric(input$alpha), F)+
        xlab("")+ ylab("")
    }
    else if (input$stat == "CI stdX") {
      plotCIStats(rv$fit0, ERtermcols, preds,as.numeric(input$alpha), T)+
        xlab("")+ ylab("")
    }

  })

  output$barPlotS <- renderPlot({
    # generate levels based on input$levels from ui.R
    # res <- tidy(anova(rv$fit))
    fits <- NULL
    if (input$order2 == "Best"){
      best <-regsubsetsOrder(rv$fit2,rd$mdata)
      if (! is.null(best)){
        fits <- c("Order1"= list(rv$fit1), best)
       }
    }
    if (is.null(fits))
      fits <- list("Order1"=rv$fit1, "Order2"= rv$fit2)

    if (is.null(barpYlim )){
      p <- plotSeqSS(fits, ERtermcols, legend=F)
      barpYlim <<- ggplot_build(p)$layout$panel_ranges[[1]]$y.range
      barpXlim <<- ggplot_build(p)$layout$panel_ranges[[1]]$x.range
      #print("setting ylim")
      # print(p)
    }
   plotSeqSS(fits, ERtermcols, legend=F) +coord_cartesian(xlim = barpXlim,ylim = barpYlim, expand = FALSE)+ xlab("")+ ylab("")
  })





  output$info <- renderPrint({
    ft1 <- broom::tidy(anova(rv$fit1))
    r1 <- ft1$sumsq
    r1 <- (1 - dplyr::last(r1)/sum(r1))*100
    r1 <- format(r1, digits=3)
    mse <- format(sqrt(dplyr::last(ft1$meansq)/dplyr::last(ft1$df)),digits=2)
    ft2 <- broom::tidy(anova(rv$fit2))
    infoString1 <- paste("Plot2:",paste(ft1$term,sapply(ft1$sumsq, format, digits=2), collapse=" "),
                         paste0("Rsq ", r1, "%", " MSE ", mse))
    infoString2 <- paste("Plot3:",paste(ft2$term,sapply(ft2$sumsq, format, digits=2), collapse=" "),
                         paste0("Rsq ", r1, "%", " MSE ", mse))
    cat(paste0(infoString1,"\n", infoString2))
  })



  output$infoBrushed <- renderPrint({
    sel <- rd$sel
    #d <- rd$mdata

       if (!is.null(sel) && (nrow(sel) !=0)){
         d <- get_all_vars(rv$fit0,rd$mdata)
      # del <- rownames(d) %in% sel$case
      # if (sum(del)> 0)
      #  d[del,]
          del <- na.omit(match(sel$case, rownames(d)))
      if (length(del) > 0)
        d[rev(del),]
      }
  })


  output$pcp <- renderPlot({
    # print("rendering pcp")
    mdata <- rd$mdata
    resD <- "diff" %in% input$Res
    resA <- "abs" %in% input$Res
    cols <- pcpColors(rv$fit0, input$PCP, ERnpcpCols,resA)
    if (input$From == "Plot1"){
      r <- regPCPdata(rv$fit0,mdata,input$PCP,resD,resA, color=cols$color, sequential=F)
    }
    else if (input$From == "Plot2"){
      r <- regPCPdata(rv$fit1,mdata,input$PCP,resD,resA, color=cols$color, sequential=T)
    }
    else {
      r <- regPCPdata(rv$fit2,mdata,input$PCP,resD,resA, color=cols$color, sequential=T)
    }
    dg <- r$pcp_data
    rd$pcp_data <- dg
    sel <-  rd$sel
    # dsel <- dplyr::filter(dg , case %in% sel$case)
    dsel <- dg[dg$case%in% sel$case, ]
    ggplot(dg, aes_string(x = "varn", y = "val", group = "case",color="color")) +
      geom_line() + xlim(levels(dg$var))+ ylab(r$ylab)  + xlab("") +labs(colour= cols$clab) +
      geom_line(data = dsel, color="magenta", size=1.5)

  })



  # output$mytitle <- renderText({
  #   if (!is.null(comment(ERfit)))
  #     comment(ERfit)
  #   else "Explore regression model"
  # })

}
}
