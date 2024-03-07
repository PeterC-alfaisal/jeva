propClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
  "propClass",
  inherit = propBase,
  private = list(
    .init = function() {   
      
      if(self$options$correction=="ob") { notext <- "S uses Occam's Bonus correction for parameters (Param). "
      } else if(self$options$correction=="aic") { notext <- "S uses AIC correction for parameters (Param). "
      } else if(self$options$correction=="aicsm") { notext <- "S uses AIC small sample correction for parameters (Param). "
      } else {
        notext <- "S uses no correction for parameters (Param). "
      }
      table <- self$results$tests
      table$setNote('Note', notext)
      table$setRow(rowNo=1, values=list(var= "H\u2080 vs observed proportions"))
      table$setRow(rowNo=2, values=list(var="H\u2090 vs observed proportion"))
      table$setRow(rowNo=3, values=list(var="H\u2090 vs H\u2080"))
      
      private$.initSupportTab()
      
    },
    .run = function() {
      
      # note that .u.yaml file must use jus: '2.0' (not 3)
      
      if (is.null(self$options$var))
        return()
      
      var <- self$data[[self$options$var]]
      
      if ( ! is.null(self$options$counts)) {
        countsData <- self$data[[self$options$counts]]
        if (jmvcore::canBeNumeric(countsData))
          countsData <- jmvcore::toNumeric(countsData)
        else
          countsData <- suppressWarnings(as.numeric(as.character(countsData)))
        
        data <- data.frame(var=var, counts=countsData)
        counts <- xtabs(counts ~ var, data=data)
        
      } else {
        
        counts <- table(var)
      }
      
      len <- length(counts)
      
      ratio <- self$options$ratio
      if (is.null(ratio))
        expProps <- rep(1/len, len)
      else
        expProps <- ratio / sum(ratio)
      
      total <- sum(counts)
      
      table <- self$results$props
      
      keys <- table$rowKeys
      for (i in seq_along(keys)) {
        key <- keys[[i]]
        if (key %in% names(counts)) {
          count <- counts[[key]]
          expProp <- expProps[i]
          values <- list(
            `count[obs]`=count,
            `prop[obs]`=count / total,
            `count[exp]`=expProp * total,
            `prop[exp]`=expProp)
          table$setRow(rowKey=key, values=values)
        }
      }
      
      #  likelihood code
      toler=0.0001
      if (len < 2)
        jmvcore::reject(.("Error: fewer than 2 levels"))
      
      n <- sum(counts)
      
      tests <- self$results$tests
      exp.p <- rep(1/len,each=len)   # null expected values
      result <- try(chisq.test(counts, p=exp.p))   # versus null
      if ( ! base::inherits(result, 'try-error')) {
        chi_n=result$statistic
        p_n=result$p.value
      } else {
        chi_n=NaN; df=''; p_n=''
      }
      result1 <- try(chisq.test(counts, p=expProps))   # versus specified expected
      if ( ! base::inherits(result1, 'try-error')) {
        chi_a=result1$statistic
        p_a=result1$p.value
      } else {
        chi_a=NaN; df=''; p_a=''
      }
      
      df <- (len-1)
      exp_n <- exp.p*n
      exp_ntext <- paste(round(exp_n,2),collapse=" | ")
      ratio_ntext <- paste(round(exp_n/exp_n,2),collapse=" : ")
      count1 <- counts               # removing zero counts for support calculations
      for (i in 1:length(count1)) {
        count1[i] <- counts[i]
        if (counts[i] < 1) count1[i]=1   # turn 0s into 1s for one table used for log
      }
      
      # Correction
      Ac <- function(c,k1,k2) { 
        if(c=="nc") { 0
        } else if(c=="ob") { 0.5*(k2-k1) 
        } else { 1*(k2-k1)
        } 
      }
      
      Sup_n <- -sum(counts*(log(count1)-log(exp_n)))
      Supc_n <- Sup_n + Ac(self$options$correction,1,len) # corrected for params
      
      exp <- expProps*n
      exp_text <- paste(round(exp,2),collapse=" | ")
      ratio_atext <- paste(round(ratio,2),collapse=" : ")
      Sup <- -sum(counts*(log(count1)-log(exp)))
      Supc <- Sup + Ac(self$options$correction,2,len) # corrected for df
      
      Sup_an <- Sup - Sup_n
      Supc_an <- Sup_an + Ac(self$options$correction,2,1)
      
      lrt_n <- abs(2*Sup_n)  # likelihood ratio statistic
      LRt_p_n <- 1-pchisq(lrt_n,df)
      lrt <- abs(2*Sup)  # likelihood ratio statistic
      LRt_p <- 1-pchisq(lrt,df)
      lrt_an <- abs(2*Sup_an)  # likelihood ratio statistic
      LRt_p_an <- 1-pchisq(lrt_an,df)
      
      toogood_n <- df/2*(log(df/chi_n)) - (df - chi_n)/2
      toogood_a <- df/2*(log(df/chi_a)) - (df - chi_a)/2
      
      table <- self$results$tests
      table$setRow(rowNo=1, values=list(rat=ratio_ntext, Values=exp_ntext, S=Supc_n, 
                                        Param=paste0(c(1,len), collapse = ', '), G=lrt_n, df=df, p=LRt_p_n))
      table$setRow(rowNo=2, values=list(rat=ratio_atext, Values=exp_text, S=Supc, 
                                        Param=paste0(c(2,len), collapse = ', '), G=lrt, df=df, p=LRt_p))
      table$setRow(rowNo=3, values=list(rat="", Values="", S=Supc_an, 
                                        Param=paste0(c(2,1), collapse = ', '), G=lrt_an, df=df, p=LRt_p_an))
      
      table <- self$results$ctt3
      table$setNote('Note', "Unlike the \u03C7\u00B2 statistic, a large S value indicates 
          that the proportions are either more different or too similar compared with those expected") 
      table$setRow(rowNo=1, values=list(var= "H\u2080", Sv=toogood_n, X2=chi_n, dfv=df, 
                                        pv=p_n, pv1=1-p_n))
      table$setRow(rowNo=2, values=list(var= "H\u2090", Sv=toogood_a, X2=chi_a, dfv=df, 
                                        pv=p_a, pv1=1-p_a))
      
      # stats for summary        
      stats <- list(S1 = Supc_n,
                    S2 = Supc,
                    S3 = Supc_an,
                    tgn = toogood_n,
                    tga = toogood_a,
                    chi_n = chi_n,
                    chi_a = chi_a)
      
      # Populate Explanation & table
      private$.populateSupportText(stats)
      private$.populateMoreSupportText()
      #
      
      
      if(isTRUE(self$options$varA)) { 
        
        table <- self$results$ctt3
        table$setVisible(TRUE)
        
      }
      
      if (isTRUE(self$options$bi) & isTRUE(len==2)) {
        # for binomial
        switch <- 0
        a <- counts[1]; r <- counts[2]
        n <- a + r
        if(a > r && r == 0) {
          a=r
          r=n-a
          switch=1
        }
        # likelihood-based % confidence interval
        p = a/n; goal = -qchisq(self$options$ciWidth/100,1)/2
        p1 <- p
        if(p == 0) p1 <- 1
        x=0
        f <- function(x,a,r,p,goal) (a*log(x)+r*log(1-x)-(a*log(p1)+r*log(1-p))-goal)^2
        xmin1 <- optimize(f, c(0, p1), tol = toler, a, r, p, goal)
        xmin2 <- optimize(f, c(p, 1), tol = toler, a, r, p, goal)
        xmin1 <- xmin1$minimum
        if(a==0) xmin1 <- 0
        xmin1a <- xmin1
        xmin2 <- xmin2$minimum
        # if(r==0) xmin2 <- 1
        # likelihood interval
        goal <- -self$options$lint
        xmin1L <- optimize(f, c(0, p1), tol = toler, a, r, p, goal)
        xmin2L <- optimize(f, c(p, 1.01), tol = toler, a, r, p, goal)
        xmin1L <- xmin1L$minimum
        if(a==0) xmin1L <- 0
        xmin1La <- xmin1L
        xmin2L <- xmin2L$minimum
        #  if(r==0) xmin2L <- 1
        
        # x axis limits
        goalx <- self$options$supplot   # with e^-10 we get x values for when curve is down to 0.00004539
        suppressWarnings(xmin1x <- optimize(f, c(0, p1), tol = toler, a, r, p, goalx))
        suppressWarnings(xmin2x <- optimize(f, c(p, 1.01), tol = toler, a, r, p, goalx))
        xmin <- xmin1x$minimum
        if(a==0) xmin <- 0
        xmina <- xmin
        xmax <- xmin2x$minimum
        
        if(switch == 1) {
          xmin1 <- 1 - xmin2
          xmin2 <- 1 - xmin1a
          xmin1L <- 1 - xmin2L
          xmin2L <- 1 - xmin1La
          xmin <- 1 - xmax
          xmax <- 1 - xmina
          p <- 1
        }
        
        lintlev <- toString(self$options$lint); conflev <- paste0(self$options$ciWidth,"%")
        
        table <- self$results$ctt2
        siWidthTitle <- jmvcore::format(.('Interval'))
        table$getColumn('Lower')$setSuperTitle(siWidthTitle)
        table$getColumn('Upper')$setSuperTitle(siWidthTitle)
        table$setRow(rowNo=1, values=list(Interval="Support", Level=lintlev, P = p, 
                                          Lower=xmin1L, Upper=xmin2L))
        table$setRow(rowNo=2, values=list(Interval="Likelihood-based", Level=conflev, P = p, 
                                          Lower=xmin1, Upper=xmin2))
        table$addFootnote(rowNo=2, col="Interval", "See reference Pritikin et al (2017) such intervals 
                          are more accurate and are parameterization-invariant compared to conventional 
                          confidence intervals")          
        
        
        table <- self$results$ctt2
        table$setVisible(TRUE)
        
        g <- data.frame(p=p, p1=p1, a=a, r=r, n=n, expprop=expProps[1], switch=switch,
                        xmin1L=xmin1L, xmin2L=xmin2L, xmin=xmin, xmax=xmax, goal=goal)
        imagec <- self$results$plotc
        imagec$setState(g)
        
        if(isTRUE(self$options$pll) & isTRUE(self$options$bi)) {
          
          plotc <- self$results$plotc
          plotc$setVisible(TRUE)
          
        }
      }
    },
    .plotc=function(imagec, ...) {
      
      g <- imagec$state
      
      if(g$switch == 1) {
        g$a <- g$r
        g$r <- g$n - g$a
        g$p <- 0
      }
      if(self$options$plotype=="lplot") {
        plot <- curve(exp(g$a*log(x)+g$r*log(1-x)-(g$a*log(g$p1)+g$r*log(1-g$p))), xlim = c(g$xmin,g$xmax), 
                      ylim = c(0,1), xlab = "Proportion", ylab = "Likelihood")
        lines(c(g$p,g$p),c(0,1),lty=2) # add MLE as dashed line
        lines(c(g$expprop,g$expprop),c(0,(g$expprop^g$a*(1-g$expprop)^g$r)/(g$p^g$a*(1-g$p)^g$r)),
              lty=1, col = "blue") # add H prob as blue line
        segments(g$xmin1L, exp(g$goal), g$xmin2L, exp(g$goal), lwd = 1, col = "red")
        lines(c(0.5,0.5), c(0,(0.5^g$a*(0.5)^g$r)/(g$p^g$a*(1-g$p)^g$r)), lty=1) # add Null as black line
      } else {
        plot <- curve(g$a*log(x)+g$r*log(1-x)-(g$a*log(g$p1)+g$r*log(1-g$p)), xlim = c(g$xmin,g$xmax), 
                      ylim = c(self$options$supplot,0), xlab = "Proportion", ylab = "Log Likelihood")
        lines(c(g$p,g$p),c(self$options$supplot,0),lty=2) # add MLE as dashed line
        lines(c(g$expprop,g$expprop),c(self$options$supplot, g$a*log(g$expprop)+g$r*log(1-g$expprop)-(g$a*log(g$p)+g$r*log(1-g$p))),
              lty=1, col = "blue") # add H prob as blue line
        segments(g$xmin1L, g$goal, g$xmin2L, g$goal, lwd = 1, col = "red")
        lines(c(0.5,0.5), c(self$options$supplot,g$a*log(0.5)+g$r*log(1-0.5)-(g$a*log(g$p)+g$r*log(1-g$p))), lty=1) # add Null as black line
      }
      #        a*log(x)+r*log(1-x)-(a*log(p)+r*log(1-p))
      #        print(plot)
      TRUE
    },
    
    .sourcifyOption = function(option) {
      if (option$name %in% c('var', 'counts'))
        return('')
      super$.sourcifyOption(option)
    },
    .formula=function() {
      jmvcore:::composeFormula(self$options$counts, self$options$var)
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
      
      Sxl = list(s=st$S1, "<i>H</i>\u2080", "observed proportions")                       
      stg1 <- private$.strength(Sxl)
      Sxl = list(s=st$S2, "<i>H</i>\u2090", "observed proportions")                       
      stg2 <- private$.strength(Sxl)
      Sxl = list(s=st$S3, "<i>H</i>\u2090", "<i>H</i>\u2080")  
      stg3 <- private$.strength(Sxl)
      
      
      Sxl = list(s=st$tgn)                   
      svn <- private$.strength2(Sxl)
      Sxl = list(s=st$tga)                       
      sva <- private$.strength2(Sxl)
      
      stg4 <- paste0("First, ", svn, ", that the 
            observed frequencies were more different than the <i>H</i>\u2080 frequencies")
      if (2*st$tgn > st$chi_n) {
        stg4 <- paste0("First, ", svn, ", that the 
            observed frequencies were too close to the <i>H</i>\u2080 frequencies")
      }
      
      stg5 <- paste0("Second, ", sva, ", that the 
            observed frequencies were more different than the <i>H</i>\u2090 frequencies")
      if (2*st$tga > st$chi_a) {
        stg5 <- paste0("Second, ", sva, ", that the 
            observed frequencies were too close to the <i>H</i>\u2090 frequencies")
      }
      if(self$options$correction=="ob") { stg0 <- "<i>Using Occam's Bonus correction, the analysis shows that:</i>"
      } else if(self$options$correction=="aic") { stg0 <- "<i>Using AIC correction, the analysis shows that:</i>"
      } else {
        stg0 <- "<i>Using no correction, the analysis shows that:</i>"
      }
      str = paste0("<br> <h2>Summarizing the evidential analysis</h2>", "<br>",
                   stg0, "<br>", stg1, "<br>", stg2, "<br>", stg3, 
                   "<br><p>
                       <i>The variance analysis (not necessarily required) shows that:</i><br>", 
                   stg4, "<br>", stg5,
                   "<p>Give the observed frequencies, and the available <i>p</i> values 
                       for the <i>G</i> test (likelihood ratio test) may also be supplied to allow 
                       comparison with a conventional analysis.
                       <p>If a binomial analysis is done then the support interval for the proportion can be given, 
                       along with the likelihood-based % confidence interval (see Pritikin et al, 2017).</p> 
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
    
    .strength2 = function(Sxl) {
      stgx <- ifelse(Sxl$s > 3.9, paste0(" there was extremely strong evidence, <i>S</i> = ",
                                         round(Sxl$s,1)),
                     ifelse(Sxl$s > 2.9, paste0(" there was strong evidence, <i>S</i> = ",
                                                round(Sxl$s,1)),
                            ifelse(Sxl$s > 1.9, paste0(" there was moderate evidence, <i>S</i> = ",
                                                       round(Sxl$s,1)),
                                   ifelse(Sxl$s > 0.9, paste0(" there was weak evidence, <i>S</i> = ",
                                                              round(Sxl$s,1)),
                                          ifelse(Sxl$s > 0.1, paste0(" there was less than weak evidence, <i>S</i> = ",
                                                                     round(Sxl$s,1)),
                                                 paste0(" there was no evidence either way, <i>S</i> = ", round(Sxl$s,3)))))))
      
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
      str3 <- "For example, we could choose meaningful <i>H</i>\u2090 proportions to compare with the <i>H</i>\u2080 
          (equal frequencies). This is shown by the last line of the main Support table. "
      str = paste0(str1, str2, str3, "As data accumulates the strength of evidence for one hypothesis over another will tend 
                       to increase.")
      
      html$setContent(str)
      
      str2 <- "Another advantage is that we can select hypothesis values that reflect our research interests. "
      str3 <- "For example, we could choose a meaningful effect size <i>H</i>\u2090 to compare with any <i>H</i>\u2080. 
        This is shown by the last line of the main Support table, where the <i>p</i> value cannot be calculated. "
      str = paste0(str1, str2, str3, "As data accumulates the strength of evidence for one hypothesis over another will tend to increase.")
      
    }
    
  )
)
