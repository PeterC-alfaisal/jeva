
# This file is a generated template, your changes will not be overwritten

l2waovClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "l2waovClass",
    inherit = l2waovBase,
    private = list(
      #### Init + run functions ----
      .init = function() {
        
        private$.initAnovaTable()
        private$.initSupportTab()
        private$.initcvals()
        
        #        private$.initDescTable()
        #        private$.initDescPlot()
        
        if (is.null(self$options$factor1) || is.null(self$options$factor2) || length(self$options$dep) == 0)
          return()
      },
      .run = function() {
        
        if (is.null(self$options$factor1) || is.null(self$options$factor2) || length(self$options$dep) == 0)
          return()
        
        ready <- TRUE
        
        if (ready) {
          
          data <- private$.cleanData()
          results <- private$.compute(data)
          
          private$.populateAnovaTable(results)
          #          private$.populateDescTable(results)
          #          private$.populateLevenesTable(results)
          #          private$.populateShapiroWilkTable(results)
          #          private$.prepareDescPlot(results)
          #          private$.prepareQQPlot(results)
          
        }
      },
      
      #### Compute results ----
      .compute = function(data) {
        
        factor1 <- self$options$factor1
        factor2 <- self$options$factor2
        dep <- self$options$dep
        r <- list()
        
        dataA <- data.frame(
          dep = jmvcore::toNumeric(data[[dep]]),
          factor1 = data[[factor1]],
          factor2 = data[[factor2]])
        
        options(contrasts = c("contr.sum","contr.poly")) # for type III SS
        model1 <- lm(dep~factor1*factor2,data=dataA)     # full model
        model0 <- lm(dep~1,data=dataA)                   # null model
        m1 <- drop1(model1, .~., test="F")               # m1 for SS
        m2 <- anova(lm(dep~factor1*factor2,data=dataA))  # m2 for overall model
        
        desc <- list()
        
        desc <- tapply(dataA$dep, dataA$factor1, function (x) {
          n <- length(x)
          mean <- mean(x)
          sd <- sd(x)
          se <- sd / sqrt(n)
          #            ci <- se * qt(95 / 200 + .5, n - 1)
          #          si <- se*sqrt((exp(self$options$lint*2/n)-1)*(n-1))     # for S-2 support interval
          #          return(c(n=n, mean=mean, sd=sd, se=se, si=si))
        })
        
        r[[dep]] <- list(model0=model0, model1=model1, m1=m1, m2=m2, dataA=dataA, desc=desc)
        
        return(r)
      },
      
      #### Init tables/plots functions ----
      .initAnovaTable = function() {
        
        varF1name <- self$options$factor1
        varF2name <- self$options$factor2
        vardepname <- self$options$dep
        
        
        if(self$options$correction=="ob") { notext <- "S uses Occam's Bonus correction for parameters (Param). "
        } else if(self$options$correction=="aic") { notext <- "S uses AIC correction for parameters (Param). "
        } else if(self$options$correction=="aicsm") { notext <- "S uses AIC small sample correction for parameters (Param). "
        } else {
          notext <- "S uses no correction for parameters (Param). "
        }
        
        table <- self$results$anova
        table$setTitle(.("Support"))
        table$setNote('Note', notext)
        table$setRow(rowNo=1, values=list(var='H\u2080 versus full model'))
        table$setRow(rowNo=2, values=list(var=paste0('H\u2080 versus ', varF1name)))
        table$setRow(rowNo=3, values=list(var=paste0('H\u2080 versus ',varF2name)))
        table$setRow(rowNo=4, values=list(var=paste0('H\u2080 versus both main effects')))
        table$setRow(rowNo=5, values=list(var=paste0('H\u2080 versus Interaction')))
        table$setRow(rowNo=6, values=list(var='Full model versus main effects'))
        table$setRow(rowNo=7, values=list(var='Full model versus interaction'))
        table$setRow(rowNo=8, values=list(var=paste0('H\u2080 versus Contrast 1')))
        table$setRow(rowNo=9, values=list(var=paste0('H\u2080 versus Contrast 2')))
        table$setRow(rowNo=10, values=list(var=paste0('Contrast 1 versus Contrast 2')))
        
      },
      #### Populate tables functions ----
      .populateAnovaTable = function(results) {
        
        #        data <- private$.cleanData()
        
        dep <- self$options$dep
        
        r <- results[[dep]]
        
        
        ############################################# begin old code
        
        overMS <- sum(r$m2$`Sum Sq`[1:3])/sum(r$m2$Df[1:3])
        overF <- overMS/r$m2$`Mean Sq`[4]
        overP <- pf(overF,sum(r$m2$Df[1:3]),r$m2$Df[4],lower.tail = FALSE)
        
        tss <- sum(r$m1$`Sum of Sq`[2:4],r$m1$RSS[1])
        eta_sq_1 <- r$m1$`Sum of Sq`[2]/(r$m1$`Sum of Sq`[2] + r$m1$RSS[1]) # partial eta-squared
        eta_sq_2 <- r$m1$`Sum of Sq`[3]/(r$m1$`Sum of Sq`[3] + r$m1$RSS[1])
        eta_sq_12 <- r$m1$`Sum of Sq`[4]/(r$m1$`Sum of Sq`[4] + r$m1$RSS[1])
        N <- length(r$dataA$dep)
        
        # Corrections
        Ac <- function(c,k1,k2,N) { 
          if(c=="nc") { 0
          } else if(c=="ob") { 0.5*(k2-k1) 
          } else if(c=="aic") { 1*(k2-k1)
          } else {
            k2 * N/(N - k2 - 1) - k1 * (N/(N - k1 - 1)) 
          }
        }
        # parameter numbers
        np <- attr(logLik(r$model0),"df")  # null, a parameter each for variance and grand mean
        fp <- attr(logLik(r$model1),"df")  # full model
        mp <- np + sum(r$m1$Df[2:3])       # main effects
        ip <- np + r$m1$Df[4]              # interaction
        mp1 <- np + r$m1$Df[2]             # first main effect
        mp2 <- np + r$m1$Df[3]             # second main effect
        cp <- np + 1                     # contrast
        
        dfv <- r$m1$Df[2:4]
        dfres <- (N-sum(dfv)-1)
        
        LSS_null <- log(tss)
        LSS_full <- log(r$m1$RSS[1])
        LSS_F1 <- log(sum(r$m1$`Sum of Sq`[3:4],r$m1$RSS[1]))
        LSS_F2 <- log(sum(r$m1$`Sum of Sq`[2],r$m1$`Sum of Sq`[4],r$m1$RSS[1]))
        LSS_I <- log(sum(r$m1$`Sum of Sq`[2:3],r$m1$RSS[1]))
        LSS_main <- log(sum(r$m1$`Sum of Sq`[4],r$m1$RSS[1]))
        
        # Null vs full model
        S_12 <- -0.5 * N * (LSS_null - LSS_full)
        S_12c <- S_12 + Ac(self$options$correction,np,fp,N)
        
        # First main effect vs null model
        S_F1N <- -0.5 * N * (LSS_null - LSS_F1)
        S_F1Nc <- S_F1N + Ac(self$options$correction,mp1,np,N)
        
        # Second main effect vs null model
        S_F2N <- -0.5 * N * (LSS_null - LSS_F2)
        S_F2Nc <- S_F2N + Ac(self$options$correction,mp2,np,N)
        
        # Both main effects vs null model
        S_FMN <- -0.5 * N * (LSS_null - log(sum(r$m1$`Sum of Sq`[4],r$m1$RSS[1])))
        S_FMNc <- S_FMN + Ac(self$options$correction,mp,np,N)
        Fval_MN <- (sum(r$m1$`Sum of Sq`[2:3])/sum(r$m1$Df[2:3]))/(r$m1$RSS[1]/(N-sum(r$m1$Df[2:4])-1))
        DFval_FMN <- paste0(c(sum(r$m1$Df[2:3]),r$m2$Df[4]), collapse = ', ')
        Pval_FMN <- pf(Fval_MN, sum(r$m1$Df[2:3]), dfres, lower.tail = FALSE)
        
        # Interaction versus vs null model
        S_IN <- -0.5 * N * (LSS_null - LSS_I)
        S_INc <- S_IN + Ac(self$options$correction,ip,np,N)
        
        # full model versus both main effects
        S_FM <- -0.5 * N * (LSS_full - LSS_main)
        S_FMc <- S_FM + Ac(self$options$correction,fp,mp,N)
        
        # full model versus interaction only
        S_FI <- -0.5 * N * (LSS_full - LSS_I)
        S_FIc <- S_FI + Ac(self$options$correction,fp,ip,N)
        
        # Interaction versus main effects
        S_IM <- -0.5 * N * (LSS_I - LSS_main)
        S_IMc <- S_IM + Ac(self$options$correction,ip,mp,N)
        
        # Interaction versus first main effect
        S_IM1 <- -0.5 * N * (LSS_I - LSS_F1)
        S_IM1c <- S_IM1 + Ac(self$options$correction,ip,mp1,N)
        
        # Interaction versus second main effect
        S_IM2 <- -0.5 * N * (LSS_I - LSS_F2)
        S_IM2c <- S_IM2 + Ac(self$options$correction,ip,mp2,N)
        
        # Contrast 1 
        datf <- r$dataA
        mean_out <- aggregate(datf[1], by=c(datf[2], datf[3]), mean)
        gp_n <- aggregate(datf[1], by=c(datf[2], datf[3]), FUN = length)
        
        S_c1M <- NULL
        S_c1Mc <- ''
        Parval_c1M <- ''
        S_c1Nc <- ''
        Parval_c1N <- ''
        Fval_c1 <- ''
        DFval_c1 <- ''
        Pval_c1 <- ''
        S_c1c2 <- ''
        Parval_c2 <- ''
        SS_cont1 <- NULL
        Parval_c1M1 <- ''
        Parval_c1M2 <- ''
        Parval_c1I <- ''
        S_c1M1c <- ''
        S_c1M2c <- ''
        S_c1Ic <- ''
        S_c2Nc <- ''
        Parval_c2N <- ''
        Fval_c2 <- ''
        DFval_c2 <- ''
        Pval_c2 <- ''
        
        # contrasts
        
        contrast1 <- 0
        contrast2 <- 0
        
        for (i in 1:10) {
          contrast1[i] <- ''
          contrast2[i] <- ''
        }
        
        if ( self$options$ct1 ){
          contrast1 <- private$.getCt1Values()
          if (length(contrast1) != r$model1$rank){
            jmvcore::reject(.("Contrast 1 has the wrong number of values"))
          }
          
        }
        
        if ( self$options$ct2 ){
          contrast2 <- private$.getCt2Values()
          if (length(contrast2) != r$model1$rank){
            jmvcore::reject(.("Contrast 2 has the wrong number of values"))
          }
        }
        
        orth <- NULL
        footnote <- NULL
        means <- unlist(mean_out[3])
        numbers <- unlist(gp_n[3])
        
        if ( self$options$ct1 ) {
          if (abs(sum(contrast1)) >= 0.03) footnote <- 1
          
          # compare null versus contrast 1 
          SS_cont1 <- sum(contrast1*means)^2/(sum(contrast1^2/(numbers)))
          LSS_c1 <- log(tss - SS_cont1)
          S_c1N <- -0.5 * N * (LSS_null - LSS_c1)
          S_c1Nc <- S_c1N + Ac(self$options$correction,cp,np,N)
          
          # compare contrast 1 versus main effects
          S_c1M <- -0.5 * N * (LSS_c1 - LSS_main)
          S_c1Mc <- S_c1M + Ac(self$options$correction,cp,mp,N)
          
          # contrast 1 vs 1st main effect
          S_c1M1 <- -0.5 * N * (LSS_c1 - LSS_F1)
          S_c1M1c <- S_c1M1 + Ac(self$options$correction,cp,mp1,N)
          
          # contrast 1 vs 2nd main effect
          S_c1M2 <- -0.5 * N * (LSS_c1 - LSS_F2)
          S_c1M2c <- S_c1M2 + Ac(self$options$correction,cp,mp2,N)
          
          # contrast 1 vs interaction
          S_c1I <- -0.5 * N * (LSS_c1 - LSS_I)
          S_c1Ic <- S_c1I + Ac(self$options$correction,cp,ip,N)
        }
        
        # Null versus contrast 2
        if ( self$options$ct2 ) {
          
          SS_cont2 <- sum(contrast2*means)^2/(sum(contrast2^2/(numbers)))
          LSS_c2 <- log(tss - SS_cont2)
          S_c2N <- -0.5 * N * (LSS_null - LSS_c2)
          S_c2Nc <- S_c2N  + Ac(self$options$correction,cp,np,N)
          if (abs(sum(contrast2)) >= 0.03) footnote <- 1
          # no correction necessary using contrasts
        }
        
        # compare contrast 1 versus contrast 2
        if ( self$options$ct1 && self$options$ct2) {
          S_c1c2 <- -0.5 * N * (LSS_c1 - LSS_c2)
          if (abs(sum(contrast1*contrast2)) >= 0.1) orth <- 1
          if (abs(sum(contrast1)) >= 0.03 || abs(sum(contrast2)) >= 0.03) footnote <- 1
        }
        
        Fval <- r$m1$`F value`[2:4]
        Pval <- r$m1$`Pr(>F)`[2:4]
        res_msq <- r$m1$RSS[1]/dfres
        
        if ( self$options$ct1 ){
          Parval_c1N <- paste0(c(cp,np), collapse = ', ')
          Parval_c1M <- paste0(c(cp,mp), collapse = ', ')
          Parval_c1M1 <- paste0(c(cp,mp1), collapse = ', ')
          Parval_c1M2 <- paste0(c(cp,mp2), collapse = ', ')
          Parval_c1I <- paste0(c(cp,ip), collapse = ', ')
          Fval_c1 <- unname(SS_cont1/res_msq)
          DFval_c1 <- paste0(c(1,r$m2$Df[4]), collapse = ', ')
          Pval_c1 <- pf(Fval_c1, 1, dfres, lower.tail = FALSE)
        }
        
        if ( self$options$ct2 ){        
          Parval_c2N <- paste0(c(cp,np), collapse = ', ')
          Fval_c2 <- unname(SS_cont2/res_msq)
          DFval_c2 <- paste0(c(1,r$m2$Df[4]), collapse = ', ')
          Pval_c2 <- pf(Fval_c2, 1, dfres, lower.tail = FALSE)
          
        }
        if ( self$options$ct1 && self$options$ct2) {
          Parval_c2 <- paste0(c(cp,cp), collapse = ', ')
        }
        
        ############################################# end old code
        if(self$options$correction=="ob") { notext <- "S uses Occam's Bonus correction for parameters (Param). "
        } else if(self$options$correction=="aic") { notext <- "S uses AIC correction for parameters (Param). "
        } else if(self$options$correction=="aicsm") { notext <- "S uses AIC small sample correction for parameters (Param). "
        } else {
          notext <- "S uses no correction for parameters (Param). "
        }
        
        table <- self$results$anova
        if ( ! is.null(orth))
          table$setNote('Note', paste0("The contrasts are not orthogonal. ", notext))
        if ( ! is.null(footnote))
          table$setNote('Note', paste0("Contrast weights do not sum to zero. ", notext))
        table$setRow(rowNo=1, values=list(S=S_12c, Param=paste0(c(np,fp), collapse = ', '), 
                                          F=overF, df=paste0(c(sum(r$m2$Df[1:3]),r$m2$Df[4]), collapse = ', '), p = overP))
        table$setRow(rowNo=2, values=list(S=S_F1Nc, Param=paste0(c(mp1,np), collapse = ', '),
                                          F=Fval[1], df=paste0(c(r$m2$Df[1],r$m2$Df[4]), collapse = ', '), p=Pval[1]))
        table$setRow(rowNo=3, values=list(S=S_F2Nc, Param=paste0(c(mp2,np), collapse = ', '),
                                          F=Fval[2], df=paste0(c(r$m2$Df[2],r$m2$Df[4]), collapse = ', '), p=Pval[2]))
        table$setRow(rowNo=4, values=list(S=S_FMNc, Param=paste0(c(mp,np), collapse = ', '),
                                          F=Fval_MN, df=DFval_FMN, p=Pval_FMN))
        table$setRow(rowNo=5, values=list(S=S_INc, Param=paste0(c(ip,np), collapse = ', '),
                                          F=Fval[3], df=paste0(c(r$m2$Df[3],r$m2$Df[4]), collapse = ', '), p=Pval[3]))
        table$setRow(rowNo=6, values=list(S=S_FMc, Param=paste0(c(fp,mp), collapse = ', '),
                                          F="", df="", p=""))
        table$setRow(rowNo=7, values=list(S=S_FIc, Param=paste0(c(fp,ip), collapse = ', '), 
                                          F="", df="", p=""))
        table$setRow(rowNo=8, values=list(S=S_c1Nc, Param=Parval_c1N,
                                          F=Fval_c1, df=DFval_c1, p=Pval_c1))
        table$setRow(rowNo=9, values=list(S=S_c2Nc, Param=Parval_c2N,
                                          F=Fval_c2, df=DFval_c2, p=Pval_c2))
        table$setRow(rowNo=10, values=list(S=S_c1c2, Param=Parval_c2,
                                           F="", df="", p=""))
        
        cmn <- 0
        cct <- 0
        cct2 <- 0
        for (i in 1:30) {
          cmn[i] <- ''
          cct[i] <- ''
          cct2[i] <- ''
        }
        
        for (i in 1:length(means)) {
          cmn[i] <- signif(means[i],4)
          cct[i] <- contrast1[i]
          cct2[i] <- contrast2[i]
        }
        
        table <- self$results$cvals
        table$setRow(rowNo=1, values=list(L1=cct[1], L2=cct[2], L3=cct[3], 
                                          L4=cct[4], L5=cct[5], L6=cct[6], L7=cct[7], L8=cct[8], L9=cct[9], L10=cct[10]))
        table$setRow(rowNo=2, values=list(L1=cct2[1], L2=cct2[2], L3=cct2[3], 
                                          L4=cct2[4], L5=cct2[5], L6=cct2[6], L7=cct2[7], L8=cct2[8], L9=cct2[9], L10=cct2[10]))
        table$setRow(rowNo=3, values=list(L1=cmn[1], L2=cmn[2], L3=cmn[3], L4=cmn[4],
                                          L5=cmn[5], L6=cmn[6], L7=cmn[7], L8=cmn[8], L9=cmn[9], L10=cmn[10]))
        
        stats <- list(S1 = S_12c,
                      S2 = S_F1Nc,
                      S3 = S_F2Nc,
                      S4 = S_FMNc,
                      S5 = S_INc,
                      S6 = S_FMc,
                      S7 = S_FIc,
                      S8 = S_c1Nc,
                      S9 = S_c2Nc,
                      S10 = S_c1c2)
        
        # Populate Explanation & table
        private$.populateSupportText(stats)
        private$.populateMoreSupportText()
        #
        
        plotData <- r$dataA
        
        image <- self$results$plot
        image$setState(plotData)
        
        if(isTRUE(self$options$plt)) {
          
          plot <- self$results$plot
          plot$setVisible(TRUE)
          
        }
        
        
      },
      
      #### Plot functions ----
      .plot=function(image, ggtheme, theme, ...) {
        plotData <- image$state
        Factor1 <- plotData$factor1; Factor2 <- plotData$factor2
        F1_margin <- aggregate(plotData[1], by=plotData[2], mean)
        F2_margin <- aggregate(plotData[1], by=plotData[3], mean)
        nl1 <- nlevels(Factor1)
        nl2 <- nlevels(Factor2)
        cell_means <- array()
        k=0
        for( j in  1:nl1 ) {
          for ( i in 1:nl2 ) {
            k=k+1
            cell_means[k] <- mean(c(F2_margin[[2]][i],F1_margin[[2]][j]))
          }
        }
        yvme <- matrix(cell_means, nrow=nl2, ncol=nl1)
        gm <- mean(unname(unlist(plotData[1])))
        nl <- nlevels(Factor1)
        xv <- seq(1:nl)
        yv <- rep(gm,nl)
        
        plot <-   interaction.plot(Factor1, Factor2, plotData$dep, 
                                   xlab = self$options$factor1, trace.label = self$options$factor2, lwd=2, ylab="Means")
        lines(xv,yv,lty=3,col="red") # add mean as dashed line
        text(x=nl,y=gm,pos=4,label = "Grand mean", col="red")
        if ( self$options$ME ) {
          for (i in 1:nl2) {
            lines(xv,yvme[i,],lwd=3, col="blue")
          }
        }
        #  print(plot)
        TRUE
        
      },
      
      
      .initDescTable = function() {
        
        table <- self$results$desc
        
        group <- self$options$factor1
        group2 <- self$options$factor2
        
        if (is.null(group))
          return()
        
        levels <- levels(self$data[[group]])
        levels2 <- levels(self$data[[group2]])
        
        table$getColumn('group')$setTitle(group)
        
        index <- 1
        dep <- self$options$dep
        #  for (j in seq_along(levels2)){
        for (i in seq_along(levels)) {
          table$addRow(paste0(dep,levels[i]), list(dep=dep, group=levels[i]))
          
          if (i == 1)
            table$addFormat(rowKey=paste0(dep, levels[i]), col=1, jmvcore::Cell.BEGIN_GROUP)
          #          table$addFormat(rowKey=paste0(dep,levels2[j], levels[i]), col=1, jmvcore::Cell.BEGIN_GROUP)
        }
        #  }
      },
      
      .populateDescTable = function(results) {
        
        table <- self$results$desc
        
        group <- self$options$factor1
        group2 <- self$options$factor2
        levels <- levels(self$data[[group]])
        levels2 <- levels(self$data[[group2]])
        
        dep <- self$options$dep          
        r <- results[[dep]]$desc
        
        #  for ( level2 in levels2) {
        for (level in levels) {
          
          row <- list(
            "num" = as.numeric(r[[level]]['n']),
            "mean" = as.numeric(r[[level]]['mean']),
            "sd" = as.numeric(r[[level]]['sd']),
            "se" = as.numeric(r[[level]]['se'])
          )
          table$setRow(rowKey=paste0(dep,level), row)
        }
        #  }
      },
      
      .cleanData = function() {
        
        data <- self$data
        
        data
      },
      .descPlotSize = function() {
        
        group <- self$options$factor1
        
        if (is.null(group))
          return(c(300, 350))
        
        levels <- levels(self$data[[group]])
        nLevels <- length(levels)
        
        xAxis <- 30 + 20
        yAxis <- 30 + 20
        
        width <- max(250, 45 * nLevels)
        height <- 300
        
        width <- yAxis + width
        height <- xAxis + height
        
        return(c(width, height))
        
      },
      .getCt1Values = function() {
        
        Ct1Values<-self$options$Ct1Values
        if ( is.character(Ct1Values) )
          Ct1Values <- as.numeric(unlist(strsplit(Ct1Values,",")))
        Ct1Values <- Ct1Values[!is.na(Ct1Values)]
        
        return(Ct1Values)
      },
      .getCt2Values = function() {
        
        Ct2Values<-self$options$Ct2Values
        if ( is.character(Ct2Values) )
          Ct2Values <- as.numeric(unlist(strsplit(Ct2Values,",")))
        Ct2Values <- Ct2Values[!is.na(Ct2Values)]
        
        return(Ct2Values)
      },
      
      .initcvals = function() {
        
        table <- self$results$cvals
        table$setRow(rowNo=1, values=list(var=paste0('Contrast 1')))
        table$setRow(rowNo=2, values=list(var=paste0('Contrast 2')))
        table$setRow(rowNo=3, values=list(var='Means'))
      },
      
      .initSupportTab = function() {
        
        table <- self$results$SupportTab
        
        Interp <- c('No evidence either way', 'Weak evidence', 'Moderate evidence', 
                    'Strong evidence', 'Extremely strong evidence', 
                    'More than a thousand to one', 'More than a million to one')
        SS=integer(); LR=numeric(); row=list()
        for (i in 0:4) {
          SS[i+1] <- i; LR[i+1] <- exp(i)
          row <- list('SS' = SS[i+1], 'LR' = LR[i+1], 'Interp' = Interp[i+1])
          table$setRow(rowNo=i+1, values=row)
        }
        SS[6] <- as.integer(7); LR[6] <- exp(7)
        row <- list('SS' = SS[6], 'LR' = LR[6], 'Interp' = Interp[6])
        table$setRow(rowNo=6, values=row)
        SS[7] <- as.integer(14); LR[7] <- exp(14)
        row <- list('SS' = SS[7], 'LR' = LR[7], 'Interp' = Interp[7])
        table$setRow(rowNo=7, values=row)
        
      },
      
      .populateSupportText = function(st) {
        
        html <- self$results$tabText
        varF1name <- self$options$factor1
        varF2name <- self$options$factor2
        
        stg8=''; stg9=''; stg10=''
        Sxl = list(s=st$S1, "<i>H</i>\u2080", "full model")                       
        stg1 <- private$.strength(Sxl)
        Sxl = list(s=st$S2, "<i>H</i>\u2080", varF1name)                       
        stg2 <- private$.strength(Sxl)
        Sxl = list(s=st$S3, "<i>H</i>\u2080", varF2name)                       
        stg3 <- private$.strength(Sxl)
        Sxl = list(s=st$S4, "<i>H</i>\u2080", "both main effects")                       
        stg4 <- private$.strength(Sxl)
        Sxl = list(s=st$S5, "<i>H</i>\u2080", "the Interaction")                       
        stg5 <- private$.strength(Sxl)
        Sxl = list(s=st$S6, "the Full model", "main effects")                       
        stg6 <- private$.strength(Sxl)
        Sxl = list(s=st$S7, "the Full model", "interaction")
        stg7 <- private$.strength(Sxl)
        if ( self$options$ct1 ){ 
          Sxl = list(s=st$S8, "<i>H</i>\u2080", "Contrast 1")
          stg8 <- private$.strength(Sxl)
          if ( self$options$ct2 ){
            Sxl = list(s=st$S9, "<i>H</i>\u2080", "Contrast 2")                       
            stg9 <- private$.strength(Sxl)
            Sxl = list(s=st$S10, "Contrast 1", "Contrast 2")                       
            stg10 <- private$.strength(Sxl)
          }
        }
        if(self$options$correction=="ob") { stg0 <- "<i>Using Occam's Bonus correction, the analysis shows that:</i>"
        } else if(self$options$correction=="aicsm") { stg0 <- "<i>Using AIC small sample correction, the analysis shows that:</i>"
        } else if(self$options$correction=="aic") { stg0 <- "<i>Using AIC correction, the analysis shows that:</i>"
        } else {
          stg0 <- "<i>Using no correction, the analysis shows that:</i>"
        }
        str = paste0("<br> <h2>Summarizing the evidential analysis</h2>", "<br>",
                     stg0, "<br>", stg1, "<br>", stg2," <br>", stg3,"<br>", stg4,"<br>", stg5, "<br>", stg6,
                     "<br>", stg7,"<br>", stg8,"<br>", stg9,"<br>", stg10,"<br>",
                     "Also give sample sizes and a graphical interaction plot. The available <i>p</i> values may be supplied to allow 
                             comparison with a conventional analysis. 
                             <br>There are no thresholds for <i>S</i> values, just guidelines on 
                             the strength of evidence for one hypothesis versus the other. They range from 
                             \u2212\u221E to +\u221E, with zero representing no evidence either way. 
                             Positive values are evidence for, while negative values are evidence against. 
                             The table below shows the interpretation of <i>S</i> values generally accepted 
                             in science. In contrast, UK law courts regard an <i>S</i> of 4 as 
                             moderate evidence and 8.6 as strong evidence! 
                             <i>S</i> values represent the weight of evidence, and are additive 
                             across independent data.")
        
        html$setContent(str)
      },
      
      .strength = function(Sxl) {
        
        stgx <- ifelse(Sxl$s < -3.9, paste0("There was extremely strong evidence, <i>S</i> = ", 
                                            round(Sxl$s,1), ", against ", Sxl[2], " and in favour of ", Sxl[3]),
                       ifelse(Sxl$s < -2.9, paste0("There was strong evidence, <i>S</i> = ",
                                                   round(Sxl$s,1), ", against ", Sxl[2], " and in favour of ", Sxl[3]),
                              ifelse(Sxl$s < -1.9, paste0("There was moderate evidence, <i>S</i> = ",
                                                          round(Sxl$s,1), ", against ", Sxl[2], " and in favour of ", Sxl[3]),
                                     ifelse(Sxl$s < -0.9, paste0("There was weak evidence, <i>S</i> = ",
                                                                 round(Sxl$s,1), ", against ", Sxl[2], " and in favour of ", Sxl[3]),
                                            ifelse(Sxl$s < -0.1, paste0("There was less than weak evidence, <i>S</i> = ",
                                                                        round(Sxl$s,1), ", against ", Sxl[2], " and in favour of ", Sxl[3]),
                                                   ifelse(Sxl$s > 3.9, paste0("There was extremely strong evidence, <i>S</i> = ",
                                                                              round(Sxl$s,1), ", for ", Sxl[2], " against ", Sxl[3]),
                                                          ifelse(Sxl$s > 2.9, paste0("There was strong evidence, <i>S</i> = ",
                                                                                     round(Sxl$s,1), ", for ", Sxl[2], " against ", Sxl[3]),
                                                                 ifelse(Sxl$s > 1.9, paste0("There was moderate evidence, <i>S</i> = ",
                                                                                            round(Sxl$s,1), ", for ", Sxl[2], " against ", Sxl[3]),
                                                                        ifelse(Sxl$s > 0.9, paste0("There was weak evidence, <i>S</i> = ",
                                                                                                   round(Sxl$s,1), ", for ", Sxl[2], " against ", Sxl[3]),
                                                                               ifelse(Sxl$s > 0.1, paste0("There was less than weak evidence, <i>S</i> = ",
                                                                                                          round(Sxl$s,1), ", for ", Sxl[2], " against ", Sxl[3]),
                                                                                      paste0("There was no evidence either way, <i>S</i> = ", 
                                                                                             round(Sxl$s,3), ", for ", Sxl[2], " against ", Sxl[3]))))))))))) 
        return(stgx)
        
      },
      
      
      .populateMoreSupportText = function(st) {
        
        html <- self$results$MoretabText
        
        str1 <- "<i>Advantages of the Evidential Approach</i> 
          <br> One advantage of the evidential approach is that <i>S</i> quantifies the strength of evidence for or against the 
          null hypothesis. "
        str2 <- "Another advantage is that we can select hypothesis values that reflect our research interests. "
        str3 <- "For example, we could choose a meaningful contrast to compare with <i>H</i>\u2080 or with a second contrast. 
        In some of the analyses the <i>p</i> value cannot be calculated, as indicated by the absence of <i>F</i> and <i>p</i> 
        values in some of the rows of the Support table. "
        str = paste0(str1, str2, str3, "As data accumulates the strength of evidence for one hypothesis over another will tend to increase.")
        
        html$setContent(str)
        
      }
    )
)
