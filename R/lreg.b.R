lregClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "lregClass",
    inherit = lregBase,
    private = list(
      .init = function() {
        
        if(self$options$correction=="ob") { notext <- "S uses Occam's Bonus correction for parameters (Param). "
        } else if(self$options$correction=="aic") { notext <- "S uses AIC correction for parameters (Param). "
        } else if(self$options$correction=="aicsm") { notext <- "S uses AIC small sample correction for parameters (Param). "
        } else {
          notext <- "S uses no correction for parameters (Param). "
        }
        
        table <- self$results$lreg
        table$setNote('Note', paste0(notext,"S is calculated hierarchically from all remaining SSq, while p values are 
                      calculated using residual MSq from the highest polynomial model."))
        table$setRow(rowNo=1, values=list(var= "H\u2080 vs Linear"))
        table$setRow(rowNo=2, values=list(var= "Quadratic vs Linear"))
        table$setRow(rowNo=3, values=list(var="Cubic vs Lower Orders"))
        table$setRow(rowNo=4, values=list(var="Quartic vs Lower Orders"))
        
        table <- self$results$coef
        siWidthTitle <- jmvcore::format(.('S-{lint} Likelihood Interval'), lint=self$options$lint)
        table$getColumn('Lower')$setSuperTitle(siWidthTitle)
        table$getColumn('Upper')$setSuperTitle(siWidthTitle)
        table$setRow(rowNo=1, values=list(var="Intercept"))
        table$setRow(rowNo=2, values=list(var="Linear"))
        table$setRow(rowNo=3, values=list(var= "Quadratic"))
        table$setRow(rowNo=4, values=list(var="Cubic"))
        table$setRow(rowNo=5, values=list(var="Quartic"))
        
        private$.initSupportTab()
      },
      .run = function() {
        
        dep <- jmvcore::toNumeric(self$data[[self$options$dep]])
        pred <- jmvcore::toNumeric(self$data[[self$options$pred]])
        lint <- self$options$lint
        
        if (is.na(anova(lm(dep ~ pred + I(pred^2) + I(pred^3) + I(pred^4)))$Df[5]))
          jmvcore::reject(.("Too few levels or mismatch in variable lengths"), code='')
        
        m1 <- stats::anova(lm(dep ~ pred))
        model0 <- stats::lm(dep ~ 1)
        model1 <- stats::lm(dep ~ pred)
        np <- attr(logLik(model0),"df")  # parameter each for variance and grand mean
        mp <- attr(logLik(model1),"df")
        
        # Correction
        Ac <- function(c,k1,k2) { 
          if(c=="nc") { 0
          } else if(c=="ob") { 0.5*(k2-k1) 
          } else { 1*(k2-k1)
          } 
        }
        
        tss <- sum(m1$`Sum Sq`)
        N <- sum(unname(m1$Df))+1
        lin_df <- unname(m1$Df[1])
        
        # support for null versus linear
        S_NL <- -0.5 * N * (log(tss) - log(unname(m1$`Sum Sq`[2])))
        
        S_NLc <- S_NL + Ac(self$options$correction,np,mp) # corrected
        
        # examining non-linearity... alternative calculations could use $r.squared
        # comparing quadratic fit to linear
        m4 <- anova(lm(dep ~ pred + I(pred^2) + I(pred^3) + I(pred^4)))
        
        Q_ss_r <- tss - sum(m4$`Sum Sq`[1:2])
        
        S_Q <- -0.5 * N * (log(Q_ss_r) - log(m1$`Sum Sq`[2]))
        S_Qc <- S_Q - Ac(self$options$correction,np,mp)   # additional parameter for the quadratic
        
        
        # quadratic versus cubic
        C_ss_r <- tss - sum(m4$`Sum Sq`[1:3])
        S_C <- -0.5 * N * (log(C_ss_r) - log(Q_ss_r))
        S_Cc <- S_C - Ac(self$options$correction,np,mp)    # additional parameter for the cubic
        
        # cubic versus quartic
        Qt_ss_r <- tss - sum(m4$`Sum Sq`[1:4])
        S_Qt <- -0.5 * N * (log(Qt_ss_r) - log(C_ss_r))
        S_Qtc <- S_Qt - Ac(self$options$correction,np,mp)   # additional parameter for the quartic
        
        # for R squared & AIC
        m_l <- lm(dep ~ pred)
        m_q <- lm(dep ~ pred + I(pred^2))
        m_c <- lm(dep ~ pred + I(pred^2) + I(pred^3))
        m_qt <- lm(dep ~ pred + I(pred^2) + I(pred^3) + I(pred^4))
        
        # alternative code to produce Sc values:
        #        m_0=lm(dep~1)
        #        m_l <- lm(dep ~ pred)
        #        S_NLc <- (AIC(m_l) - AIC(m_0))/2
        #        m_q <- lm(dep ~ pred + I(pred^2))
        #        S_Qc <- (AIC(m_l) - AIC(m_q))/2
        #        m_c <- lm(dep ~ pred + I(pred^2) + I(pred^3))
        #        S_Cc <- (AIC(m_q) - AIC(m_c))/2
        #        m_qt <- lm(dep ~ pred + I(pred^2) + I(pred^3) + I(pred^4))
        #        S_Qtc <- (AIC(m_c) - AIC(m_qt))/2
        
        table <- self$results$lreg
        table$setRow(rowNo=1, values=list(S=S_NLc, 
                                          Param=paste0(c(np,mp), collapse = ', '), r2=summary(m_l)$r.squared, aic=AIC(m_l),
                                          df=paste0(c(m1$Df[1],m1$Df[2]),collapse=', '), p=m1$`Pr(>F)`[1]))
        table$setRow(rowNo=2, values=list(S=S_Qc, 
                                          Param=paste0(c(mp,mp+1), collapse = ', '), r2=summary(m_q)$r.squared, aic=AIC(m_q),
                                          df=paste0(c(m4$Df[1],m4$Df[5]),collapse=', '), p=m4$`Pr(>F)`[2]))
        table$setRow(rowNo=3, values=list(S=S_Cc, 
                                          Param=paste0(c(mp+1,mp+2), collapse = ', '), r2=summary(m_c)$r.squared, aic=AIC(m_c),
                                          df=paste0(c(m4$Df[2],m4$Df[5]),collapse=', '), p=m4$`Pr(>F)`[3]))
        table$setRow(rowNo=4, values=list(S=S_Qtc, 
                                          Param=paste0(c(mp+2,mp+3), collapse = ', '), r2=summary(m_qt)$r.squared, aic=AIC(m_qt),
                                          df=paste0(c(m4$Df[3],m4$Df[5]),collapse=', '), p=m4$`Pr(>F)`[4]))
        
        # stats for summary        
        stats <- list(S1 = S_NLc,
                      S2 = S_Qc,
                      S3 = S_Cc,
                      S4 = S_Qtc,
                      lint = self$options$lint)
        
        
        # Populate Explanation & table
        private$.populateSupportText(stats)
        private$.populateMoreSupportText()
        #
        
        # S and support intervals
        S_Int <- -N/2*log(1 + summary(m_qt)$coefficients[1,3]^2/summary(m_qt)$df[2])
        Int_lo <- summary(m_qt)$coefficients[1,1] - (summary(m_qt)$coefficients[1,2] * 
                                                       sqrt((exp(lint*2/N) - 1) * summary(m_qt)$df[2]))
        Int_hi <- summary(m_qt)$coefficients[1,1] + (summary(m_qt)$coefficients[1,2] * 
                                                       sqrt((exp(lint*2/N) - 1) * summary(m_qt)$df[2]))
        
        S_L <- -N/2*log(1 + summary(m_qt)$coefficients[2,3]^2/summary(m_qt)$df[2])
        L_lo <- summary(m_qt)$coefficients[2,1] - (summary(m_qt)$coefficients[2,2] * 
                                                     sqrt((exp(lint*2/N) - 1) * summary(m_qt)$df[2]))
        L_hi <- summary(m_qt)$coefficients[2,1] + (summary(m_qt)$coefficients[2,2] * 
                                                     sqrt((exp(lint*2/N) - 1) * summary(m_qt)$df[2]))
        
        Q_L <- -N/2*log(1 + summary(m_qt)$coefficients[3,3]^2/summary(m_qt)$df[2])
        Q_lo <- summary(m_qt)$coefficients[3,1] - (summary(m_qt)$coefficients[3,2] * 
                                                     sqrt((exp(lint*2/N) - 1) * summary(m_qt)$df[2]))
        Q_hi <- summary(m_qt)$coefficients[3,1] + (summary(m_qt)$coefficients[3,2] * 
                                                     sqrt((exp(lint*2/N) - 1) * summary(m_qt)$df[2]))
        
        C_L <- -N/2*log(1 + summary(m_qt)$coefficients[4,3]^2/summary(m_qt)$df[2])
        C_lo <- summary(m_qt)$coefficients[4,1] - (summary(m_qt)$coefficients[4,2] * 
                                                     sqrt((exp(lint*2/N) - 1) * summary(m_qt)$df[2]))
        C_hi <- summary(m_qt)$coefficients[4,1] + (summary(m_qt)$coefficients[4,2] * 
                                                     sqrt((exp(lint*2/N) - 1) * summary(m_qt)$df[2]))
        
        Qt_L <- -N/2*log(1 + summary(m_qt)$coefficients[5,3]^2/summary(m_qt)$df[2])
        Qt_lo <- summary(m_qt)$coefficients[5,1] - (summary(m_qt)$coefficients[5,2] * 
                                                      sqrt((exp(lint*2/N) - 1) * summary(m_qt)$df[2]))
        Qt_hi <- summary(m_qt)$coefficients[5,1] + (summary(m_qt)$coefficients[5,2] * 
                                                      sqrt((exp(lint*2/N) - 1) * summary(m_qt)$df[2]))
        
        table <- self$results$coef
        table$setRow(rowNo=1, values=list(Estimate=summary(m_qt)$coefficients[1,1], 
                                          SE=summary(m_qt)$coefficients[1,2], 
                                          S=S_Int, Lower=Int_lo, Upper=Int_hi, p=summary(m_qt)$coefficients[1,4]))
        table$setRow(rowNo=2, values=list(Estimate=summary(m_qt)$coefficients[2,1], 
                                          SE=summary(m_qt)$coefficients[2,2], 
                                          S=S_L, Lower=L_lo, Upper=L_hi, p=summary(m_qt)$coefficients[2,4]))
        table$setRow(rowNo=3, values=list(Estimate=summary(m_qt)$coefficients[3,1], 
                                          SE=summary(m_qt)$coefficients[3,2], 
                                          S=Q_L, Lower=Q_lo, Upper=Q_hi, p=summary(m_qt)$coefficients[3,4]))
        table$setRow(rowNo=4, values=list(Estimate=summary(m_qt)$coefficients[4,1], 
                                          SE=summary(m_qt)$coefficients[4,2], 
                                          S=C_L, Lower=C_lo, Upper=C_hi, p=summary(m_qt)$coefficients[4,4]))
        table$setRow(rowNo=5, values=list(Estimate=summary(m_qt)$coefficients[5,1], 
                                          SE=summary(m_qt)$coefficients[5,2], 
                                          S=Qt_L, Lower=Qt_lo, Upper=Qt_hi, p=summary(m_qt)$coefficients[5,4]))
        
        plotData <- data.frame(y=dep,x=pred)
        image <- self$results$plot
        image$setState(plotData)
        
        if(isTRUE(self$options$plt)) {
          
          plot <- self$results$plot
          
          plot$setVisible(TRUE)
          
        }
        
      },
      
      .plot=function(image, ggtheme, theme, ...) {
        
        plotData <- image$state
        
        p <- ggplot2::ggplot(
          plotData, ggplot2::aes(x=x, y=y)
        )  + 
          ggplot2::geom_point(alpha=.8, size=2.5) + ggtheme +
          ggplot2::labs(
            x=self$options$pred, 
            y=self$options$dep)
        
        if(! isFALSE(self$options$lin)) {
          p <- p + ggplot2::stat_smooth(method="lm", se=FALSE, fill=NA,
                                        formula=y ~ poly(x, 1, raw=TRUE),colour="black")
        }
        if(! isFALSE(self$options$quad)) {
          p <- p + ggplot2::stat_smooth(method="lm", se=FALSE, fill=NA,
                                        formula=y ~ poly(x, 2, raw=TRUE),colour="red")
        }
        if(! isFALSE(self$options$cub)) {
          p <- p + ggplot2::stat_smooth(method="lm", se=FALSE, fill=NA,
                                        formula=y ~ poly(x, 3, raw=TRUE),colour="blue")
        }
        if(! isFALSE(self$options$quart)) {
          p <- p + ggplot2::stat_smooth(method="lm", se=FALSE, fill=NA,
                                        formula=y ~ poly(x, 4, raw=TRUE),colour="green")
        }
        
        print(p)
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
        
        Sxl = list(s=st$S1, "<i>H</i>\u2080", "Linear")                       
        stg1 <- private$.strength(Sxl)
        Sxl = list(s=st$S2, "Quadratic", "Linear")                       
        stg2 <- private$.strength(Sxl)
        Sxl = list(s=st$S3, "Cubic", "Lower Orders")                       
        stg3 <- private$.strength(Sxl)
        Sxl = list(s=st$S4, "Quartic", "Lower Orders")                       
        stg4 <- private$.strength(Sxl)
        if(self$options$correction=="ob") { stg0 <- "<i>Using Occam's Bonus correction, the analysis shows that:</i>"
        } else if(self$options$correction=="aic") { stg0 <- "<i>Using AIC correction, the analysis shows that:</i>"
        } else {
          stg0 <- "<i>Using no correction, the analysis shows that:</i>"
        }
        str = paste0("<br> <h2>Summarizing the evidential analysis</h2>", "<br>",
                     stg0, "<br>", stg1, "<br>", stg2, "<br>", stg3, "<br>", stg4,
                     "<p>
                      Also give the sample size and estimates with likelihood intervals. 
                      The available <i>p</i> values may also be supplied to allow comparison 
                      with a conventional analysis, although those not including the <i>H</i>\u2080 
                      are not testing between specific polynomial hypotheses (see more below).
                      </p> <br> 
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
        str2 <- "Another advantage is that we can select hypotheses that reflect our research interests. "
        str3 <- "For example, we can compare meaningful polynomial hypotheses. These comparisons are shown 
        by the last 3 lines of the main Support table, where the <i>p</i> values cannot be directly calculated 
        but are determined using the residual mean sums of squares from the highest polynomial included in the model. "
        
        str = paste0(str1, str2, str3, "As data accumulates the strength of evidence for one hypothesis over another will tend 
                       to increase.")
        
        
        html$setContent(str)
        
      }
      
      
      
    )
)