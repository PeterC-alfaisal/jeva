
laov1wClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "laov1wClass",
    inherit = laov1wBase,
    private = list(
      #### Init + run functions ----
      .init = function() {
        
        private$.initSupportTab()

        private$.initAnovaTable()
        private$.initDescTable()
        private$.initDescPlot()
        
#        levels <- levels(self$data[[self$options$group]])
#        k <- length(levels)
        
            # contrasts
        if ( self$options$ct1 ){
          contrast1 <- private$.getCt1Values()
          con1txt <- paste(contrast1,collapse=',')
        }
        
        if ( self$options$ct2 ){
          contrast2 <- private$.getCt2Values()
          con2txt <- paste(contrast2,collapse=',')
        }
        
#        conta <- contr.poly(k, scores = 1:k)
        if (isFALSE(self$options$ct1)) {
          con1txt <- 'Linear'
        } 
        
          if (isFALSE(self$options$ct2)) {
            con2txt <- 'Quadratic'
          }

        table <- self$results$anova
        table$setRow(rowNo=1, values=list(var='<i>H</i>\u2080  vs observed means'))
        table$setRow(rowNo=2, values=list(var=paste0('Contrast 1 (',con1txt,') vs observed means')))
        table$setRow(rowNo=3, values=list(var=paste0('Contrast 2 (',con2txt,') vs observed means')))
        table$setRow(rowNo=4, values=list(var="Contrast 1 vs Contrast 2"))
        table$setRow(rowNo=5, values=list(var="Linear vs Non-linear"))
        
        if (is.null(self$options$group) || length(self$options$deps) == 0)
          return()
        
        
      },
      .run = function() {

        if (is.null(self$options$group) || length(self$options$deps) == 0)
          return()

        ready <- TRUE
        
        if (ready) {
          
          data <- private$.cleanData()
          results <- private$.compute(data)
          
          private$.populateAnovaTable(results)
          private$.populateDescTable(results)
          private$.populateLevenesTable(results)
          private$.populateShapiroWilkTable(results)
          private$.prepareDescPlot(results)
          private$.prepareQQPlot(results)
          
        }
      },
      
      #### Compute results ----
      .compute = function(data) {
        
        group <- self$options$group

        r <- list()
        for (dep in self$options$deps) {
          
          dataA <- data.frame(
            dep = jmvcore::toNumeric(data[[dep]]),
            group = data[[group]])
          
          fisher <-  stats::anova(lm(dep ~ group, data=dataA))
          residuals <- rstandard(lm(dep ~ group, data=dataA))
          
          desc <- tapply(dataA$dep, dataA$group, function (x) {
            n <- length(x)
            mean <- mean(x)
            sd <- sd(x)
            se <- sd / sqrt(n)
#            ci <- se * qt(95 / 200 + .5, n - 1)
            si <- se*sqrt((exp(self$options$lint*2/n)-1)*(n-1))     # for S-2 support interval
            return(c(n=n, mean=mean, sd=sd, se=se, si=si))
          })
          
          levene <- car::leveneTest(dep ~ group, dataA, center="mean")
          
          r[[dep]] <- list(fisher=fisher, desc=desc, levene=levene, 
                           residuals=residuals)
        }
        
        return(r)
      },
      
      #### Init tables/plots functions ----
      .initAnovaTable = function() {
        
        table <- self$results$anova
        table$setNote('Note', "<i>S</i>c uses Akaike correction for parameters (Param). 
                      Unless specified, Contrast 1 is linear and Contrast 2 is quadratic")
        table$setTitle(.("Support: One-Way ANOVA (Fisher's assuming equal variances)"))

      },
      .initDescTable = function() {
        
        table <- self$results$desc
        
        group <- self$options$group
        
        if (is.null(group))
          return()
        
        levels <- levels(self$data[[group]])
        
        table$getColumn('group')$setTitle(group)
        
        index <- 1
        for (dep in self$options$deps) {
          
          for (i in seq_along(levels)) {
            table$addRow(paste0(dep,levels[i]), list(dep=dep, group=levels[i]))
            
            if (i == 1)
              table$addFormat(rowKey=paste0(dep,levels[i]), col=1, jmvcore::Cell.BEGIN_GROUP)
          }
        }
      },
      .initDescPlot = function() {
        
        plots <- self$results$plots
        size <- private$.descPlotSize()
        
        for (dep in self$options$deps) {
          image <- plots$get(key=dep)$desc
          image$setSize(size[1], size[2])
        }
      },
      
      #### Populate tables functions ----
      .populateAnovaTable = function(results) {
        
        group <- self$options$group           # added in
        levels <- levels(self$data[[group]])   #
        
        table <- self$results$anova
        for (dep in self$options$deps) {
          
          r <- results[[dep]]
          gp_means <- numeric(); gp_n <- numeric()
          for (i in levels) {
            gp_means[i] <- as.numeric(r$desc[[i]]['mean'])
            gp_n[i] <- as.numeric(r$desc[[i]]['n'])
          }
          
          tss <- sum(r$fisher$`Sum Sq`)  # total SS
          N <- (sum(r$fisher$Df)+1)
          k <- r$fisher$Df[1]+1
          S_12 <- -(-0.5 * N * (log(r$fisher$`Sum Sq`[2]) - log(tss)))
          np <- 2                   # parameter each for variance and grand mean
          mp <- r$fisher$Df[1] + np
          
          # Akaike's correction
          Ac <- function(k1,k2,N) { k2 * N/(N - k2 - 1) - k1 * (N/(N - k1 - 1)) }
          S_12c <- S_12 + Ac(np,mp,N)
          
          if (k < 3){
            jmvcore::reject(.("At least 3 groups are needed"))
          } 
          
          # contrasts
          if ( self$options$ct1 ){
            contrast1 <- private$.getCt1Values()
            if (length(contrast1) != length(levels)){
              jmvcore::reject(.("Contrast 1 has the wrong number of values"))
            }
            
          }
          
          if ( self$options$ct2 ){
            contrast2 <- private$.getCt2Values()
            if (length(contrast2) != length(levels)){
              jmvcore::reject(.("Contrast 2 has the wrong number of values"))
            }
          }
          
          conta <- contr.poly(k, scores = 1:k)
          if (isFALSE(self$options$ct1)) {
            contrast1 <- conta[,1]
          } 

          if (length(gp_means) < 3) {
            contrast2 <- NULL
          } else {
            if (isFALSE(self$options$ct2)) {
             contrast2 <- conta[,2]
            }}
          
          orth <- NULL
          if (abs(sum(contrast1*contrast2)) >= 0.1) orth <- 1
          
          footnote <- NULL
          if (abs(sum(contrast1)) >= 0.03 || abs(sum(contrast2)) >= 0.03)
            footnote <- 1

          contrastL <- conta[,1]  # linear contrast
          
          n <- N/(r$fisher$Df[1]+1)
          
          SS_1 <- sum(contrast1*gp_means)^2/(sum(contrast1^2/(gp_n)))  # allows unequal samples
          SS_2 <- sum(contrast2*gp_means)^2/(sum(contrast2^2/(gp_n)))
          SS_L <- sum(contrastL*gp_means)^2/(sum(contrastL^2/(gp_n)))
          SS_nL <- r$fisher$`Sum Sq`[1] - SS_L
          
          r_SS_1 <- tss - SS_1
          r_SS_2 <- tss - SS_2
          r_SS_L <- tss - SS_L
          r_SS_nL <- tss - SS_nL
          
          S_cont_12 <- -0.5*N*(log(r_SS_1) - log(r_SS_2))  # support contrast1 vs contrast2
          S_cont1_means <- -0.5*N*(log(r_SS_1) - log(r$fisher$`Sum Sq`[2])) # support contrast1
          S_cont2_means <- -0.5*N*(log(r_SS_2) - log(r$fisher$`Sum Sq`[2])) # support contrast1
          S_cont_LnL <- -0.5*N*(log(r_SS_L) - log(r_SS_nL))  # support contrast1 vs contrast2
          
          # vs means model
          
          cp <- np + 1       # parameters for contrast and means model
          S_cont1_meansc <- S_cont1_means + Ac(cp,mp,N)
          S_cont2_meansc <- S_cont2_means + Ac(cp,mp,N)

          dfL <- 1; dfnL <- r$fisher$Df[1]-dfL         # df for linear and non-linear components
          nlp <- np + dfnL               # then parameters
          S_cont_LnLc <- S_cont_LnL + Ac(cp,nlp,N)
            
          Fval_c1 <- SS_1/r$fisher$`Mean Sq`[2]
          Fval_c2 <- SS_2/r$fisher$`Mean Sq`[2]
          Pval_c1 <- pf(Fval_c1, 1, r$fisher$Df[2], lower.tail = FALSE)
          Pval_c2 <- pf(Fval_c2, 1, r$fisher$Df[2], lower.tail = FALSE)

          
          table <- self$results$anova
          if ( ! is.null(orth))
            #            table$addFootnote(rowNo=3, 'var', "The contrasts are not orthogonal")
          table$setNote('Note', "The contrasts are not orthogonal. 
                        <i>S</i>c uses Akaike correction for parameters (Param). 
                        Unless specified, Contrast 1 is linear and Contrast 2 is quadratic")
          if ( ! is.null(footnote))
            table$setNote('Note', "Contrast weights do not sum to zero. 
                        <i>S</i>c uses Akaike correction for parameters (Param). 
                        Unless specified, Contrast 1 is linear and Contrast 2 is quadratic")
          table$setRow(rowNo=1, values=list(S=S_12, Sc=S_12c, Param=paste0(c(np,mp), collapse = ', '),
                                            F=as.numeric(r$fisher$`F value`[1]), 
                                            df=paste(as.numeric(r$fisher$Df),collapse=", "), 
                                            p=as.numeric(r$fisher$`Pr(>F)`[1])))
          table$setRow(rowNo=2, values=list(S=S_cont1_means, Sc=S_cont1_meansc, Param=paste0(c(cp,mp), collapse = ', '),
                                            F=Fval_c1, df=paste0('1, ', r$fisher$Df[2]), p=Pval_c1))
          table$setRow(rowNo=3, values=list(S=S_cont2_means, Sc=S_cont2_meansc, Param=paste0(c(cp,mp), collapse = ', '), 
                                            F=Fval_c2, df=paste0('1, ', r$fisher$Df[2]), p=Pval_c2))
          table$setRow(rowNo=4, values=list(S=S_cont_12, Sc=S_cont_12,
                                            Param=paste0(c(cp,cp), collapse = ', '),
                                            F="", df='1, 1', p=""))
          table$setRow(rowNo=5, values=list(S=S_cont_LnL, Sc=S_cont_LnLc, 
                                            Param=paste0(c(cp,nlp), collapse = ', '),
                                            F="", df=paste0('1, ', dfnL), p=""))
          
        }
        # stats for summary        
        stats <- list(S1 = S_12c,
                      S2 = S_cont1_meansc,
                      S3 = S_cont2_meansc,
                      S4 = S_cont_12,
                      S5 = S_cont_LnLc,
                      lint = self$options$lint)
        
        
        # Populate Explanation & table
        private$.populateSupportText(stats)
        private$.populateMoreSupportText()
        #
        
      },
      .populateDescTable = function(results) {
        
        table <- self$results$desc
        
        group <- self$options$group
        levels <- levels(self$data[[group]])
        
        for (dep in self$options$deps) {
          
          r <- results[[dep]]$desc
          
          for (level in levels) {
            
            row <- list(
              "num" = as.numeric(r[[level]]['n']),
              "mean" = as.numeric(r[[level]]['mean']),
              "sd" = as.numeric(r[[level]]['sd']),
              "se" = as.numeric(r[[level]]['se'])
            )
            
            table$setRow(rowKey=paste0(dep,level), row)
          }
        }
      },
      .populateLevenesTable = function(results) {
        
        table <- self$results$assump$eqv
        for (dep in self$options$deps) {
          
          r <- results[[dep]]$levene
          
          Fval <- as.numeric(r[1,'F value'])
          df1 <- as.numeric(r[1,'Df'])
          df2 <- as.numeric(r[2,'Df'])
          df <- paste(as.numeric(r[,'Df']),collapse=", ")
          
          S <- 0.5 * (df1*log(1 + df2/(df1 * Fval)) + df2*log(1 + df1*Fval/df2) + df1*log(df1)
                      + df2*log(df2) - (df1 + df2) * log(df1 + df2))
          
          row <- list(
            "F" = Fval,
            "S" = S,
            "df" = df,
            "p" = as.numeric(r[1,'Pr(>F)'])
          )
          
          table$setRow(rowKey=dep, row)
          table$setNote('Note', "Unlike <i>F</i>, a large <i>S</i> value indicates that group variances are either 
                        more or less homogenous than expected")
          
        }
        
      },
      .populateShapiroWilkTable = function(results) {
        
        tooFewSamplesMessage <- .('Too few samples to compute statistic (N < {n})')
        tooManySamplesMessage <- .('Too many samples to compute statistic (N > {n})')
        
        table <- self$results$assump$norm
        
        for (dep in self$options$deps) {
          
          r <- results[[dep]]$residuals
          
          row <- list()
          footnote <- NULL
          
          if (length(r) < 3) {
            
            row[['w']] <- NaN
            row[['p']] <- ''
            footnote <- jmvcore::format(tooFewSamplesMessage, n=3)
            
          } else if (length(r) > 5000) {
            
            row[['w']] <- NaN
            row[['p']] <- ''
            footnote <- jmvcore::format(tooManySamplesMessage, n=5000)
            
          } else {
            
            sw <- try(shapiro.test(r), silent=TRUE)
            
            
            if ( ! isError(sw)) {
              row[['w']] <- sw$statistic
              row[['p']] <- sw$p.value
            }
            else {
              row[['w']] <- NaN
              row[['p']] <- ''
            }
          }
          
          table$setRow(rowKey=dep, row)
          if ( ! is.null(footnote))
            table$addFootnote(rowKey=dep, 'w', footnote)
        }
      },

      #### Plot functions ----
      .prepareDescPlot = function(results) {
        
        plots <- self$results$plots
        
        group <- self$options$group
        levels <- levels(self$data[[group]])
        
        for (dep in self$options$deps) {
          
          image <- plots$get(key=dep)$desc
          
          r <- results[[dep]]$desc
          
          df <- as.data.frame(do.call("rbind", r))
          df$levels <- factor(levels, levels=levels)
          
          image$setState(list(df=df, dep=dep))
          
        }
      },
      .desc = function(image, ggtheme, theme, ...) {
        
        if (is.null(image$state))
          return(FALSE)
        
        groupName <- self$options$group
        
        ciLegendTitle<- .(paste0("Means with S-", self$options$lint, " intervals"))
        errorType <- jmvcore::format(ciLegendTitle, ciWidth=95)
        
        p <- ggplot2::ggplot(data=image$state$df, ggplot2::aes(x=levels, y=mean)) +
          ggplot2::geom_errorbar(ggplot2::aes(ymin=mean-si, ymax=mean+si, width=.1), size=.8, color=theme$color[2]) +
          ggplot2::geom_point(ggplot2::aes(color=errorType), fill=theme$fill[1], size=3, shape=21) +
          ggplot2::labs(x=groupName, y=image$state$dep) +
          ggtheme + ggplot2::theme(legend.title = ggplot2::element_blank(),
                                   legend.justification = 0.5, legend.position = 'top')
        
        return(p)
      },
      .prepareQQPlot = function(results) {
        
        plots <- self$results$plots
        group <- self$options$group
        
        for (dep in self$options$deps) {
          
          image <- plots$get(key=dep)$qq
          r <- results[[dep]]$residuals
          df <- as.data.frame(qqnorm(r, plot.it=FALSE))
          
          if (nrow(df) > 10000)
            df <- df[ ! duplicated(round(df$x,2)), ]
          
          image$setState(df)
          
        }
      },
      .qq = function(image, ggtheme, theme, ...) {
        
        if (is.null(image$state))
          return(FALSE)
        
        p <- ggplot2::ggplot(data=image$state, ggplot2::aes(y=y, x=x)) +
          ggplot2::geom_abline(slope=1, intercept=0, colour=theme$color[1]) +
          ggplot2::geom_point(size=2, colour=theme$color[1]) +
          ggplot2::xlab(.("Theoretical Quantiles")) +
          ggplot2::ylab(.("Standardized Residuals")) +
          ggtheme
        
        return(p)
      },
      
      #### Helper functions ----
      .cleanData = function() {
        
        data <- self$data

        data
      },
      .descPlotSize = function() {
        
        group <- self$options$group
        
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
      .sourcifyOption = function(option) {
        if (option$name %in% c('deps', 'group'))
          return('')
        super$.sourcifyOption(option)
      },
      .formula = function() {
        jmvcore:::composeFormula(self$options$deps, self$options$group)
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
          Sxl = list(s=st$S2, "Contrast 1", "observed means")                       
          stg2 <- private$.strength(Sxl)
          Sxl = list(s=st$S3, "Contrast 2", "observed means")                       
          stg3 <- private$.strength(Sxl)
          Sxl = list(s=st$S4, "Contrast 1", "Contrast 2")                       
          stg4 <- private$.strength(Sxl)
          Sxl = list(s=st$S5, "Linear", "Non-linear")                       
          stg5 <- private$.strength(Sxl)
          
          
          str = paste0("<br> <h2>Summarizing the evidential analysis</h2>
                                 <br>  <i>Using the Akaike corrected values, the analysis shows that:</i> <br> 
                                 ", stg1, "<br>", stg2," <br>", stg3,"<br>", stg4,"<br>", stg5,
                             "<p> <i>The Homogeneity test:</i> <br>
                             The <i>S</i> produced by the homogeneity of variance test can be given, 
                             where a large value indicates that the group variances are either less  
                             or <i>more</i> homogenous than expected.
                             <p>
                             Also give the sample size and a graphical plot that includes support 
                             intervals. The available <i>p</i> values may be supplied to allow 
                             comparison with a conventional analysis. 
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
          
          str1 <- "One advantage of the evidential approach is that <i>S</i> quantifies the strength of evidence 
          for or against the null hypothesis. "
          str2 <- "Another advantage is that we can select hypothesis values that reflect our research interests. "
          str3 <- "For example, we could choose a meaningful contrast to compare with another contrast. The results 
                  for such comparisons are shown by the last 2 lines of the main Support table (where the <i>p</i> 
                  values cannot be calculated). "
          
          str = paste0(str1, str2, str3, "As data accumulates the strength of evidence for one hypothesis over another will tend 
                       to increase.")
          
          html$setContent(str)
          
        }

)
)
