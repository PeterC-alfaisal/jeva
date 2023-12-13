
# This file is a generated template, your changes will not be overwritten

lrttestClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "lrttestClass",
    inherit = lrttestBase,
    private = list(
      .init = function() {      
        if(self$options$correction=="ob") { notext <- "S uses Occam's Bonus correction for parameters (Param)"
        } else if(self$options$correction=="AIC") { notext <- "S uses AIC correction for parameters (Param)"
        } else {
          notext <- "S uses no correction for parameters (Param)"
        }
        table <- self$results$lrttest
        table$setNote('Note', notext) 
        table$setRow(rowNo=1, values=list(var= "H\u2080 vs observed mean difference"))
        table$setRow(rowNo=2, values=list(var="H\u2090 vs observed mean difference"))
        table$setRow(rowNo=3, values=list(var="H\u2090 vs H\u2080"))
        
        table <- self$results$lrttest2
        siWidthTitle <- jmvcore::format(.('S-{lint} Likelihood Interval'), lint=self$options$lint)
        table$getColumn('Lower')$setSuperTitle(siWidthTitle)
        table$getColumn('Upper')$setSuperTitle(siWidthTitle)
        
        private$.initSupportTab()
        
      },
      .run = function() {
        
        data1 <- jmvcore::toNumeric(self$data[[self$options$depa]])
        data2 <- jmvcore::toNumeric(self$data[[self$options$depb]])
        
        if (is.null(data1) || is.null(data2))
          return()
        
        data <- data.frame(data1,data2)
        pairsData <- naOmit(data)       # remove missing listwise
        adata <- pairsData[1] - pairsData[2]
        
        results <- t.test(adata, mu = self$options$nul)
        m.obs <- results$estimate
        se.obs <- results$stderr
        df <- results$parameter
        N=df+1
        m1.obs <- mean(pairsData$data1)
        m2.obs <- mean(pairsData$data2)
        med1.obs <- median(pairsData$data1)
        med2.obs <- median(pairsData$data2)
        sd1 <- sd(pairsData$data1)
        sd2 <- sd(pairsData$data2)
        sed1 <- sd1/sqrt(N)
        sed2 <- sd2/sqrt(N)
        
        sed <- results$stderr
        sdd <- sed*sqrt(N)
        tval <- unname(results$statistic)
        like0 <- (1 + tval^2/df)^-(N/2) #L0
        # Maximum likelihood ratio and S
        S_m <- log(like0)
        
        results1 <- t.test(adata, mu = self$options$alt)
        like1 <- unname((1 + results1$statistic^2/df)^-(N/2)) #Alt H
        S_1 <- log(like1)
        
        # Add Likelihood interval
        x=0
        toler=0.0001
        f <- function(x, m.obs, sed, df, N, goal) {
          (-N/2*log(1 + ((m.obs-x)/sed)^2/df)-goal)^2
        }
        
        goalx <- self$options$supplot   # with e^-10 we get x values for when curve is down to 0.00004539
        suppressWarnings(xmin1x <- optimize(f, c(m.obs-100*sed, m.obs), tol = toler, m.obs, sed, df, N, goalx))
        suppressWarnings(xmin2x <- optimize(f, c(m.obs, m.obs+100*sed), tol = toler, m.obs, sed, df, N, goalx))
        xmin <- xmin1x$minimum
        xmax <- xmin2x$minimum
        
        lolim <- m.obs - sed*sqrt((exp(self$options$lint*2/N)-1)*df)
        hilim <- m.obs + sed*sqrt((exp(self$options$lint*2/N)-1)*df)
        
        sel    <- m.obs - sed*sqrt((exp(self$options$lint*2/N)-1)*(df))  # S-2 lower bound
        seu    <- m.obs + sed*sqrt((exp(self$options$lint*2/N)-1)*(df))  # upper bound
        
        if(self$options$correction=="aic") correct <- -1  # correction applied to S
        if(self$options$correction=="ob") correct <- -0.5
        if(self$options$correction=="nc") correct <- 0
        k1 <- 2
        k2 <- 3
        
        table <- self$results$lrttest
        table$setRow(rowNo=1, values=list(Value=results$null.value, mdiff= self$options$nul-m.obs, 
                                          sed=sed, S=S_m-correct, Param=paste0(c(k1,k2), collapse = ', '), 
                                          t=tval, df=df, p=results$p.value))
        table$setRow(rowNo=2, values=list(Value=results1$null.value, mdiff= self$options$alt-m.obs, 
                                          sed=sed, S=S_1, Param=paste0(c(k2,k2), collapse = ', '), 
                                          t=results1$statistic, df=df, p=results1$p.value))
        table$setRow(rowNo=3, values=list(Value="", mdiff= self$options$alt-self$options$nul, 
                                          sed=sed, S=S_1-S_m+correct, Param=paste0(c(k2,k1), collapse = ', '),
                                          t="", df="", p=""))
        # stats for summary        
        stats <- list(S1 = S_m,
                      S2 = S_1,
                      S3 = S_1-S_m,
                      correct=correct,
                      mobs = m.obs,
                      lolim = lolim,
                      hilim = hilim,
                      lint = self$options$lint)
        
        
        # Populate Explanation & table
        private$.populateSupportText(stats)
        private$.populateMoreSupportText()
        #
        
        table <- self$results$lrttest2
        table$setRow(rowNo=1, values=list(diff=m.obs, Lower=lolim, Upper=hilim))
        
        table <- self$results$lrttestd
        table$setRow(rowNo=1, values=list(gp=self$options$depa, N=N, Mean=m1.obs, Median=med1.obs, SD=sd1, SE=sed1))
        table$setRow(rowNo=2, values=list(gp=self$options$depb, N=N, Mean=m2.obs, Median=med2.obs, SD=sd2, SE=sed2))
        
        if(isTRUE(self$options$dtab)) { 
          
          table <- self$results$lrttestd
          table$setVisible(TRUE)
          
        }
        
        plotData <- data.frame(Mean=m.obs, sel=sel, seu=seu)
        
        image <- self$results$plot
        image$setState(plotData)
        
        if(isTRUE(self$options$plt)) {
          
          plot <- self$results$plot
          plot$setVisible(TRUE)
          
        }
        
        g <- data.frame(mobs=m.obs,sed=sed, df=df, N=N, 
                        null=results$null.value, alth=results1$null.value, lolim=lolim, hilim=hilim,
                        xmin=xmin, xmax=xmax)
        imagec <- self$results$plotc
        imagec$setState(g)
        
        if(isTRUE(self$options$pll)) {
          
          plotc <- self$results$plotc
          plotc$setVisible(TRUE)
          
        }
        
        
      },
      
      .plot=function(image, ggtheme, theme, ...) {
        plotData <- image$state
        
        xaxis_lab <- paste(self$options$depa, " - ", self$options$depb)
        plot <- ggplot(plotData, aes(x="", y=Mean)) +
          geom_errorbar(aes(ymin=sel, ymax=seu, width=.1), colour=theme$color[2]) +
          geom_point(shape=21, size=3, fill="white", colour=theme$color[1]) +
          labs(x=xaxis_lab)
        ggtheme +
          theme(
            plot.title=ggplot2::element_text(margin=ggplot2::margin(b = 5.5 * 1.2)),
            plot.margin = ggplot2::margin(5.5, 5.5, 5.5, 5.5)
          )
        
        print(plot)
        TRUE
        
      },
      .plotc=function(imagec, ...) {
        
        g <- imagec$state
        
        if(self$options$plotype=="lplot") {
          plot <- curve((1 + ((g$mobs-x)/g$sed)^2/g$df)^-(g$N/2),
                        xlim = c(g$xmin, g$xmax), ylab = "Likelihood",
                        xlab = "Observed mean difference")
          segments(g$lolim, exp(-self$options$lint), g$hilim, exp(-self$options$lint), col = "red")
          lines(c(g$mobs,g$mobs),c(0,1),lty=2) # add mean as dashed line
          lines(c(g$null, g$null), c(0,(1 + ((g$mobs-g$null)/g$sed)^2/g$df)^-(g$N/2)),lty=1, col = "black") # for null
          lines(c(g$alth,g$alth),c(0,(1 + ((g$mobs-g$alth)/g$sed)^2/g$df)^-(g$N/2)),lty=1, col = "blue") # alt.h
        } else {
          plot <- curve(-g$N/2*log(1 + ((g$mobs-x)/g$sed)^2/g$df),
                        xlim = c(g$xmin, g$xmax), 
                        ylim=c(self$options$supplot,0), ylab = "Log Likelihood",
                        xlab = "Observed mean difference")
          segments(g$lolim, -self$options$lint, g$hilim, -self$options$lint, col = "red")
          lines(c(g$mobs,g$mobs),c(self$options$supplot,0),lty=2) # add mean as dashed line
          lines(c(g$null, g$null), c(self$options$supplot,-g$N/2*log(1 + ((g$mobs-g$null)/g$sed)^2/g$df)),lty=1, col = "black") # for null
          lines(c(g$alth,g$alth),c(self$options$supplot,-g$N/2*log(1 + ((g$mobs-g$alth)/g$sed)^2/g$df)),lty=1, col = "blue") # alt.h
        }
        TRUE
        
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
        
        Sxl = list(s=st$S1-st$correct, "<i>H</i>\u2080", "observed mean difference")                       
        stg1 <- private$.strength(Sxl)
        Sxl = list(s=st$S2, "<i>H</i>\u2090", "observed mean difference")                       
        stg2 <- private$.strength(Sxl)
        Sxl = list(s=st$S3+st$correct, "<i>H</i>\u2090", "<i>H</i>\u2080")                       
        stg3 <- private$.strength(Sxl)
        if(self$options$correction=="ob") { stg0 <- "<i>Using Occam's Bonus correction, the analysis shows that:</i>"
        } else if(self$options$correction=="aic") { stg0 <- "<i>Using AIC correction, the analysis shows that:</i>"
        } else {
          stg0 <- "<i>Using no correction, the analysis shows that:</i>"
        }
        str = paste0("<br> <h2>Summarizing the evidential analysis</h2>", "<br>",
                     stg0, "<br>", stg1, "<br>", stg2, "<br>", stg3,
                     "<p>Observed mean difference = ", signif(st$mobs,3), ", with an <i>S</i>-",st$lint, " 
                     support interval was from ", signif(st$lolim,3), " to ", signif(st$hilim,3), 
                     ". <p>
                     Also give the sample size and a graphical plot that includes support 
                     interval. The available <i>p</i> values may also be supplied to allow 
                     comparison with a conventional analysis.</p> 
                     <br> <br> 
                     There are no thresholds for <i>S</i> values, just guidelines on 
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
                                            round(Sxl$s,1), ", against ", Sxl[2], " versus ", Sxl[3]),
                       ifelse(Sxl$s < -2.9, paste0("There was strong evidence, <i>S</i> = ",
                                                   round(Sxl$s,1), ", against ", Sxl[2], " versus ", Sxl[3]),
                              ifelse(Sxl$s < -1.9, paste0("There was moderate evidence, <i>S</i> = ",
                                                          round(Sxl$s,1), ", against ", Sxl[2], " versus ", Sxl[3]),
                                     ifelse(Sxl$s < -0.9, paste0("There was weak evidence, <i>S</i> = ",
                                                                 round(Sxl$s,1), ", against ", Sxl[2], " versus ", Sxl[3]),
                                            ifelse(Sxl$s < -0.1, paste0("There was less than weak evidence, <i>S</i> = ",
                                                                        round(Sxl$s,1), ", against ", Sxl[2], " versus ", Sxl[3]),
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
        
        str1 <- "<i>Support Intervals</i> 
          <br> The log likelihood ratio interval identifies a supported range of values which are consistent with the observed 
          statistic. In jeva it is denoted as <i>S</i>-<i>X</i>, where <i>X</i> can be any number between 1 and 100. 
          The <i>S</i>-2 interval is commonly used since it is numerically close to the 95% confidence interval. For the <i>S</i>-2 
          interval, it means that the values within the interval have likelihood ratios in the range 0.135 to 7.38, corresponding 
          to e\u207B\u00B2 to e\u00B2. Simply put, within an <i>S</i>-2 interval, no likelihoods are more than 7.38 times different 
          from each other. Similarly, for the <i>S</i>-3 interval, likelihood ratios will range from 0.050 to 20.09, corresponding 
          to e\u207B\u00B3 to e\u00B3, and no likelihoods will be more than 20.09 times different from each other.
          <br> <i>Advantages of the Evidential Approach</i> 
          <br> One advantage of the evidential approach is that <i>S</i> quantifies the strength of evidence for or against the 
          null hypothesis. "
        str2 <- "Another advantage is that we can select hypothesis values that reflect our research interests. "
        str3 <- "For example, we could choose a meaningful effect size <i>H</i>\u2090 to compare with any <i>H</i>\u2080. 
        This is shown by the last line of the main Support table, where the <i>p</i> value cannot be calculated. "
        str = paste0(str1, str2, str3, "As data accumulates the strength of evidence for one hypothesis over another will tend to increase.")
        
        html$setContent(str)
        
      }
      
    )
)
