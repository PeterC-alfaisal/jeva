lcorrClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "lcorrClass",
    inherit = lcorrBase,
    private = list(
      .init = function() {
        
        if(self$options$correction=="ob") { notext <- "S uses Occam's Bonus correction for parameters (Param). "
        } else if(self$options$correction=="aic") { notext <- "S uses AIC correction for parameters (Param). "
        } else if(self$options$correction=="aicsm") { notext <- "S uses AIC small sample correction for parameters (Param). "
        } else {
          notext <- "S uses no correction for parameters (Param). "
        }
        table <- self$results$lcor
        table$setNote('Note', paste0(notext,"The last column p(z) gives the p value for the preceding z value"))
        table$setRow(rowNo=1, values=list(var= "H\u2080 vs observed correlation"))
        table$setRow(rowNo=2, values=list(var= "H\u2090 versus observed correlation"))
        table$setRow(rowNo=3, values=list(var="H\u2090 vs H\u2080"))
        
        table <- self$results$lcor1
        siWidthTitle <- jmvcore::format(.('Interval'))
        table$getColumn('Lower')$setSuperTitle(siWidthTitle)
        table$getColumn('Upper')$setSuperTitle(siWidthTitle)
        table$setRow(rowNo=1, values=list(Interval="Support"))
        table$setRow(rowNo=2, values=list(Interval="Likelihood-based"))
        table$addFootnote(rowNo=2, col="Interval", "See reference Pritikin et al (2017) such intervals 
                          are more accurate and are parameterization-invariant compared to conventional 
                          confidence intervals")
        
        private$.initSupportTab()
        
      },
      
      .run = function() {
        
        data1 <- jmvcore::toNumeric(self$data[[self$options$depa]])
        data2 <- jmvcore::toNumeric(self$data[[self$options$depb]])
        adata <- data1 - data2
        
        exp.r <- self$options$alt
        L.int <- self$options$lint
        ciw <- self$options$ciWidth
        
        m <- cor.test(data1, data2, conf.level = ciw/100)
        null <- unname(m$null.value)
        r <- unname(m$estimate)
        t <- unname(m$statistic)
        p <- unname(m$p.value)
        df <- unname(m$parameter)
        N <- df + 2
        
        # Correction
        Ac <- function(c,k1,k2) { 
          if(c=="nc") { 0
          } else if(c=="ob") { 0.5*(k2-k1) 
          } else { 1*(k2-k1)
          } 
        }
        model0 <- stats::lm(data1 ~ 1)
        model1 <- stats::lm(data1 ~ data2)
        np <- attr(logLik(model0),"df")  # parameter each for variance and grand mean
        mp <- attr(logLik(model1),"df")
        
        # using z
        rtrans <- function(r_value) (0.5 * log(abs((1 + r_value)/(1 - r_value))))
        se <- 1/(sqrt(N - 3))
        z0 <- (rtrans(r)-null)/se
        
        # support for observed versus null
        S0 <- -z0^2/2
        S0c <- -z0^2/2 + Ac(self$options$correction,np,mp) # corrected
        
        z1 <- (rtrans(r)-rtrans(exp.r))/se
        S1 <- -z1^2/2             # support for exp.r versus observed r
        
        x_lim <- c(r-3.5*se, r+3.5*se)   # adjust to suit x limits on plot
        if (x_lim[1] < -1) x_lim[1]=-1
        if (x_lim[2] > 1) x_lim[2]=1
        
        r_dash_lower <- rtrans(r)-se*sqrt(L.int*2)
        r_dash_upper <- rtrans(r)+se*sqrt(L.int*2)
        
        lolim <- (exp(2*r_dash_lower)-1)/(exp(2*r_dash_lower)+1)
        hilim <- (exp(2*r_dash_upper)-1)/(exp(2*r_dash_upper)+1)
        
        # likelihood-based % confidence intervals
        toler <- 0.0001
        f <- function(x, r, goal) {
          (-((rtrans(x)-rtrans(r))/se)^2/2-goal)^2
        }
        
        goal = -qchisq(self$options$ciWidth/100,1)/2
        xmin1 <- optimize(f, c(-1, r), tol = toler, r, goal)
        xmin2 <- optimize(f, c(r, 1), tol = toler, r, goal)
        beg <- xmin1$minimum
        end <- xmin2$minimum
        
        # x axis limits
        goalx <- self$options$supplot   # with e^-10 we get x values for when curve is down to 0.00004539
        suppressWarnings(xmin1x <- optimize(f, c(-1, r), tol = toler, r, goalx))
        suppressWarnings(xmin2x <- optimize(f, c(r, 1), tol = toler, r, goalx))
        xmin <- xmin1x$minimum
        xmax <- xmin2x$minimum
        
        table <- self$results$lcor
        table$setRow(rowNo=1, values=list(Value=0, rdiff= null-r, S=S0c, Param=paste0(c(np,mp), collapse = ', '),
                                          t=t, df=df, p=p, z=z0, pz=2*(1-pnorm(abs(z0)))))
        table$setRow(rowNo=2, values=list(Value=exp.r, rdiff= exp.r-r, S=S1, Param=paste0(c(mp,mp), collapse = ', '), 
                                          t="", df="", p="", z=z1, pz=2*(1-pnorm(abs(z1)))))
        table$setRow(rowNo=3, values=list(Value="", rdiff= exp.r-null, S=S1-S0-Ac(self$options$correction,np,mp), 
                                          Param=paste0(c(mp,np), collapse = ', '), t="", df="", p="", z="", pz=""))
        
        # stats for summary        
        stats <- list(S1 = S0c,
                      S2 = S1,
                      S3 = S1-S0-Ac(self$options$correction,np,mp),
                      mobs = r,
                      lolim = lolim,
                      hilim = hilim,
                      lint = L.int)
        
        # Populate Explanation & table
        private$.populateSupportText(stats)
        private$.populateMoreSupportText()
        #
        
        lintlev <- toString(self$options$lint); conflev <- paste0(self$options$ciWidth,"%")
        
        table <- self$results$lcor1
        table$setRow(rowNo=1, values=list(Level=lintlev, r=r, Lower=lolim, Upper=hilim))
        table$setRow(rowNo=2, values=list(Level=conflev, r=r, Lower=beg, Upper=end))
        
        g <- data.frame(r=r,se=se, L.int=L.int, null=m$null.value, 
                        exp.r=exp.r, lolim=lolim, hilim=hilim, xmin=xmin, xmax=xmax)
        
        imagec <- self$results$plotc
        imagec$setState(g)
        
        if(isTRUE(self$options$pll)) {
          
          plotc <- self$results$plotc
          plotc$setVisible(TRUE)
          
        }
        
        plotData <- data.frame(y=data1,x=data2)
        image <- self$results$plot
        image$setState(plotData)
        
        if(isTRUE(self$options$plt)) {
          
          plot <- self$results$plot
          plot$setVisible(TRUE)
          
        }
        
      },
      
      .plot=function(image, ggtheme, theme, ...) {
        plotData <- image$state
        line <- self$options$line
        method <- if (line == 'linear') 'lm' else 'auto'
        
        p <- ggplot2::ggplot(
          plotData, ggplot2::aes(x=x, y=y)
        )  + 
          ggplot2::geom_point(alpha=.8, size=2.5) + ggtheme +
          ggplot2::labs(
            x=self$options$depb, 
            y=self$options$depa
          )
        
        if (line != 'none') {
          p <- p + ggplot2::geom_smooth(
            method = method, se = self$options$se
          )
        }
        
        
        print(p)
        TRUE
        
      },
      .plotc=function(imagec, ...) {
        
        g <- imagec$state
        
        rtrans <- function(r_value) (0.5 * log(abs((1 + r_value)/(1 - r_value))))
        
        if(self$options$plotype=="lplot") {
          curve(exp(-((rtrans(g$r)-rtrans(x))/g$se)^2/2), xlim = c(g$xmin, g$xmax), xlab = "r", ylab = "Likelihood")
          lines(c(g$r,g$r),c(0,1),lty=2) # add MLE as dashed line
          lines(c(g$null,g$null),c(0,exp(-((rtrans(g$r)-rtrans(g$null))/g$se)^2/2)), lty=1, col = "black") # add H prob as black line
          lines(c(g$exp.r,g$exp.r), c(0,exp(-((rtrans(g$r)-rtrans(g$exp.r))/g$se)^2/2)), lty=1, col = "blue") # add H prob as blue line
          segments(g$lolim, exp(-g$L.int), g$hilim, exp(-g$L.int), lwd = 1, col = "red")
        } else {
          curve(-((rtrans(g$r)-rtrans(x))/g$se)^2/2, xlim = c(g$xmin, g$xmax), xlab = "r", 
                ylim=c(self$options$supplot,0), ylab = "Log Likelihood")
          lines(c(g$r,g$r),c(self$options$supplot,0),lty=2) # add MLE as dashed line
          lines(c(g$null,g$null),c(self$options$supplot,-((rtrans(g$r)-rtrans(g$null))/g$se)^2/2), lty=1, col = "black") # add H prob as black line
          lines(c(g$exp.r,g$exp.r), c(self$options$supplot,-((rtrans(g$r)-rtrans(g$exp.r))/g$se)^2/2), lty=1, col = "blue") # add H prob as blue line
          segments(g$lolim, -g$L.int, g$hilim, -g$L.int, lwd = 1, col = "red")
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
        
        Sxl = list(s=st$S1, "<i>H</i>\u2080", "observed correlation")                       
        stg1 <- private$.strength(Sxl)
        Sxl = list(s=st$S2, "<i>H</i>\u2090", "observed correlation")                       
        stg2 <- private$.strength(Sxl)
        Sxl = list(s=st$S3, "<i>H</i>\u2090", "<i>H</i>\u2080")                       
        stg3 <- private$.strength(Sxl)
        if(self$options$correction=="ob") { stg0 <- "<i>Using Occam's Bonus correction, the analysis shows that:</i>"
        } else if(self$options$correction=="aic") { stg0 <- "<i>Using AIC correction, the analysis shows that:</i>"
        } else {
          stg0 <- "<i>Using no correction, the analysis shows that:</i>"
        }
        str = paste0("<br> <h2>Summarizing the evidential analysis</h2>", "<br>",
                     stg0, "<br>",stg1, "<br>", stg2, "<br>", stg3, "
                     <p>Observed correlation <i>r</i> = ", signif(st$mobs,3), ", with an <i>S</i>-",st$lint, " 
                     support interval was from ", signif(st$lolim,3), " to ", signif(st$hilim,3), 
                     ". The likelihood-based % confidence interval can be given (see Pritikin et al, 2017).
                     <p>The sample size should be given. The available <i>p</i> values may 
                     also be supplied to allow comparison with a conventional analysis.</p>
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
          <br> One advantage of the evidential approach is that <i>S</i> quantifies the strength of evidence 
          for or against the null hypothesis. "
        str2 <- "Another advantage is that we can select hypothesis values that reflect our research interests. "
        str3 <- "For example, we could choose a meaningful <i>H</i>\u2090 correlation to compare with the <i>H</i>\u2080 of 0. 
        This is shown by the last line of the main Support table, where no <i>p</i> value can be calculated. "
        
        str = paste0(str1, str2, str3, "As data accumulates the strength of evidence for one hypothesis over another will tend 
                       to increase.")
        
        
        html$setContent(str)
        
      }
      
    )
)