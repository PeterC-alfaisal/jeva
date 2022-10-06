
# This file is a generated template, your changes will not be overwritten

lcorrClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "lcorrClass",
    inherit = lcorrBase,
    private = list(
      .init = function() {
        
        table <- self$results$lcor
        table$setNote('Note', "The last column p(z) gives the <i>p</i> value for the preceding <i>z</i> value")
        table$setRow(rowNo=1, values=list(var= "<i>H</i>\u2080 vs observed correlation"))
        table$setRow(rowNo=2, values=list(var= "<i>H</i>\u2090 versus observed correlation"))
        table$setRow(rowNo=3, values=list(var="<i>H</i>\u2090 vs <i>H</i>\u2080"))
        
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
        
        # using z
        rtrans <- function(r_value) (0.5 * log(abs((1 + r_value)/(1 - r_value))))
        se <- 1/(sqrt(N - 3))
        z0 <- (rtrans(r)-null)/se
        
        # support for observed versus null
        S0 <- -z0^2/2

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
        
        table <- self$results$lcor
        table$setRow(rowNo=1, values=list(Value=0, rdiff= null-r, 
                                          S=S0, t=t, df=df, p=p, z=z0, pz=2*(1-pnorm(abs(z0)))))
        table$setRow(rowNo=2, values=list(Value=exp.r, rdiff= exp.r-r, 
                                          S=S1, t="", df="", p="", z=z1, pz=2*(1-pnorm(abs(z1)))))
        table$setRow(rowNo=3, values=list(Value="", rdiff= exp.r-null, 
                                          S=S1-S0, t="", df="", p="", z="", pz=""))
        
        # stats for summary        
        stats <- list(S1 = S0,
                      S2 = S1,
                      S3 = S1-S0,
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
                        exp.r=exp.r, lolim=lolim, hilim=hilim)
        
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
        
        # to determine x axis space for plot
        x_lim <- c(g$r-3*g$se, g$r+3*g$se)   # adjust to suit x limits on plot
        if (x_lim[1] < -1) x_lim[1]=-1
        if (x_lim[2] > 1) x_lim[2]=1
        
        curve(exp(-((rtrans(g$r)-rtrans(x))/g$se)^2/2), xlim = x_lim, xlab = "r", ylab = "Likelihood")
        lines(c(g$r,g$r),c(0,1),lty=2) # add MLE as dashed line
        lines(c(g$null,g$null),c(0,exp(-((rtrans(g$r)-rtrans(g$null))/g$se)^2/2)), lty=1, col = "black") # add H prob as black line
        lines(c(g$exp.r,g$exp.r), c(0,exp(-((rtrans(g$r)-rtrans(g$exp.r))/g$se)^2/2)), lty=1, col = "blue") # add H prob as blue line
        segments(g$lolim, exp(-g$L.int), g$hilim, exp(-g$L.int), lwd = 1, col = "red")
        
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
        
        Sxl = list(s=st$S1, "<i>H</i>\u2080", "observed correlation")                       
        stg1 <- private$.strength(Sxl)
        Sxl = list(s=st$S2, "<i>H</i>\u2090", "observed correlation")                       
        stg2 <- private$.strength(Sxl)
        Sxl = list(s=st$S3, "<i>H</i>\u2090", "<i>H</i>\u2080")                       
        stg3 <- private$.strength(Sxl)
        
        str = paste0("<br> <h2>Summarizing the evidential analysis</h2>
                         <br>  <i>The analysis shows that:</i> <br>", 
                     stg1, "<br>", stg2, "<br>", stg3, "
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
        
        str1 <- "One advantage of the evidential approach is that <i>S</i> quantifies the strength of evidence 
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