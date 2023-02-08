
# This file is a generated template, your changes will not be overwritten

l2sttestClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "l2sttestClass",
    inherit = l2sttestBase,
    private = list(
      .init = function() {      
      table <- self$results$l2sttest
      if(isTRUE (self$options$welch))
        table$setNote('Note', "Uses Welch's procedure for unequal variances") 
      table$setRow(rowNo=1, values=list(var= "<i>H</i>\u2080 vs observed mean difference"))
      table$setRow(rowNo=2, values=list(var="<i>H</i>\u2090 vs observed mean difference"))
      table$setRow(rowNo=3, values=list(var="<i>H</i>\u2090 vs <i>H</i>\u2080"))

      table <- self$results$l2sttest2
      siWidthTitle <- jmvcore::format(.('<i>S</i>-{lint} Likelihood Interval'), lint=self$options$lint)
      table$getColumn('Lower')$setSuperTitle(siWidthTitle)
      table$getColumn('Upper')$setSuperTitle(siWidthTitle)
      
      private$.initSupportTab()
      
      },
      .run = function() {
        
        groupVarName <- self$options$group
        depVarNames <- self$options$dep
        varNames <- c(groupVarName, depVarNames)
        
        if (is.null(groupVarName) || length(depVarNames) == 0)
          return()
        
        formula <- jmvcore::constructFormula(depVarNames, groupVarName)
        formula <- as.formula(formula)
        if (isTRUE(self$options$welch)) {veq <- FALSE} else {veq <- TRUE}
        results <- t.test(formula, self$data, mu = self$options$nul, var.equal = veq)
        m.obs <- results$estimate[1]-results$estimate[2]
        df <- results$parameter
        nsam   <- aggregate(formula, self$data, function(x) length(x))[,2]
        N <- nsam[1] + nsam[2]
        sed <- results$stderr
        tval <- unname(results$statistic)
        like0 <- (1 + tval^2/df)^-(N/2) #L0
        # Maximum likelihood ratio and S
        S_m <- log(like0)
        results1 <- t.test(formula, self$data, mu = self$options$alt, var.equal = veq)
        like1 <- unname((1 + results1$statistic^2/df)^-(N/2)) #Alt H
        S_1 <- log(like1)
        
        # Add Likelihood interval
        lolim <- m.obs - sed*sqrt((exp(self$options$lint*2/N)-1)*df)
        hilim <- m.obs + sed*sqrt((exp(self$options$lint*2/N)-1)*df)
        
        means  <- aggregate(formula, self$data, mean)[,2]
        Median  <- aggregate(formula, self$data, median)[,2]
        sds  <- aggregate(formula, self$data, sd)[,2]
        ses    <- aggregate(formula, self$data, function(x) sd(x)/sqrt(length(x)))[,2]
        sel    <- means - ses*sqrt((exp(self$options$lint*2/nsam)-1)*(nsam-1))  # S-2 lower bound
        seu    <- means + ses*sqrt((exp(self$options$lint*2/nsam)-1)*(nsam-1))  # upper bound
        levels <- base::levels(self$data[[groupVarName]])
        
        table <- self$results$l2sttest
        table$setRow(rowNo=1, values=list(Value=results$null.value, 
                                          mdiff= self$options$nul-m.obs, sed=sed, S=S_m, t=tval, df=df, p=results$p.value))
        table$setRow(rowNo=2, values=list(Value=results1$null.value, mdiff=self$options$alt-m.obs, 
                                          sed=sed, S=S_1, t=results1$statistic, df=df, p=results1$p.value))
        table$setRow(rowNo=3, values=list(Value="", 
                                          mdiff= self$options$alt-self$options$nul, sed=sed, S=S_1-S_m, t="", df="", p=""))
        # stats for summary        
        stats <- list(S1 = S_m,
                      S2 = S_1,
                      S3 = S_1-S_m,
                      mobs = m.obs,
                      lolim = lolim,
                      hilim = hilim,
                      lint = self$options$lint)
        
        
        # Populate Explanation & table
        private$.populateSupportText(stats)
        private$.populateMoreSupportText()
        #
        
        table <- self$results$l2sttest2
        table$setRow(rowNo=1, values=list(mdiff=m.obs, Lower=lolim, Upper=hilim))
        
        table <- self$results$l2sttestd
        table$setRow(rowNo=1, values=list(gp=levels[1], N=nsam[1], Mean=means[1], 
                                          Median=Median[1], SD=sds[1], SE=ses[1]))
        table$setRow(rowNo=2, values=list(gp=levels[2], N=nsam[2], Mean=means[2], 
                                          Median=Median[2], SD=sds[2], SE=ses[2]))
        
        if(isTRUE(self$options$dtab)) { 
          
          table <- self$results$l2sttestd
          table$setVisible(TRUE)
          
        }
        
        plotData <- data.frame(level=levels, mean=means, sel=sel, seu=seu)
        
        image <- self$results$plot
        image$setState(plotData)
        
        if(isTRUE(self$options$plt)) {
          
          plot <- self$results$plot
          plot$setVisible(TRUE)
          
        }
        
        g <- data.frame(mobs=m.obs,sed=sed, df=df, N=N, 
                        null=results$null.value, alth=results1$null.value, lolim=lolim, hilim=hilim)
        imagec <- self$results$plotc
        imagec$setState(g)
        
        if(isTRUE(self$options$pll)) {
          
          plot <- self$results$plotc
          plot$setVisible(TRUE)
          
        }
         
      },
      
      .plot=function(image, ggtheme, theme, ...) {
        plotData <- image$state
        
        pd <- position_dodge(0.2)
        
        plot <- ggplot(plotData, aes(x=level, y=mean)) +
          geom_errorbar(aes(ymin=sel, ymax=seu, width=.1), colour=theme$color[2], position=pd) +
          geom_point(shape=21, size=3, fill="white", colour=theme$color[1], position=pd) +
          labs(y=self$options$dep, x=self$options$group, xlab=base::levels(self$data[[self$options$group]])) +
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
        
        plot <- curve((1 + ((g$mobs-x)/g$sed)^2/g$df)^-(g$N/2),
                      xlim = c(g$mobs-5*g$sed,g$mobs+5*g$sed), ylab = "Likelihood",
                      xlab = "Observed mean difference")
        segments(g$lolim, exp(-self$options$lint), g$hilim, exp(-self$options$lint), col = "red")
        lines(c(g$mobs,g$mobs),c(0,1),lty=2) # add mean as dashed line
        lines(c(g$null, g$null), c(0,(1 + ((g$mobs-g$null)/g$sed)^2/g$df)^-(g$N/2)),lty=1, col = "black") # for null
        lines(c(g$alth,g$alth),c(0,(1 + ((g$mobs-g$alth)/g$sed)^2/g$df)^-(g$N/2)),lty=1, col = "blue") # alt.h
        
        print(plot)
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
        
        Sxl = list(s=st$S1, "<i>H</i>\u2080", "observed means")                       
        stg1 <- private$.strength(Sxl)
        Sxl = list(s=st$S2, "<i>H</i>\u2090", "observed means")                       
        stg2 <- private$.strength(Sxl)
        Sxl = list(s=st$S3, "<i>H</i>\u2090", "<i>H</i>\u2080")                       
        stg3 <- private$.strength(Sxl)
        
        str = paste0("<br> <h2>Summarizing the evidential analysis</h2>
                         <br>  <i>The analysis shows that:</i> <br>", 
                     stg1, "<br>", stg2, "<br>", stg3, "
                     <p>Observed mean difference = ", signif(st$mobs,3), ", with an <i>S</i>-",st$lint, " 
                     support interval was from ", signif(st$lolim,3), " to ", signif(st$hilim,3), 
                     ". 
                     <p> Also give the sample sizes and a graphical plot that includes support 
                     intervals. The available <i>p</i> values may also be supplied to allow 
                     comparison with a conventional analysis.</p>
                     <br><br>
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
          <br> The log likelihood ratio interval identifies a supported range of values which are consistent with the observed statistic. 
          In jeva it is denoted as <i>S</i>-<i>X</i>, where <i>X</i> can be any number between 1 and 100. The <i>S</i>-2 interval is 
          commonly used since it is numerically close to the 95% confidence interval. For the <i>S</i>-2 interval, it means that the values 
          within the interval have likelihood ratios in the range 0.135 to 7.38, corresponding to e\u207B\u00B2 to e\u00B2. 
          Simply put, within an <i>S</i>-2 interval, no likelihoods are more than 7.38 times different from each other. Similarly, for the 
          <i>S</i>-3 interval, likelihood ratios will range from 0.050 to 20.09, corresponding to e\u207B\u00B3 to e\u00B3, and no 
          likelihoods will be more than 20.09 times different from each other.
          <br> <i>Advantages of the Evidential Approach</i> 
          <br> One advantage of the evidential approach is that <i>S</i> quantifies the strength of 
        evidence for or against the null hypothesis. "
        str2 <- "Another advantage is that we can select hypothesis values that reflect our research interests. "
        str3 <- "For example, we could choose a meaningful effect size <i>H</i>\u2090 to compare with any <i>H</i>\u2080. 
        This is shown by the last line of the main Support table, where the <i>p</i> value cannot be calculated. "
        str = paste0(str1, str2, str3, "As data accumulates the strength of evidence for one hypothesis over 
        another will tend to increase.")
        
        html$setContent(str)
        
      }
        
    )
)
