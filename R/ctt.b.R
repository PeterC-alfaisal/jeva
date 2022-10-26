
#' @importFrom jmvcore .


cttClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "cttClass",
    inherit = cttBase,
    private = list(
      .init = function() {
        
        private$.initSupportTab()
        
        freqs <- self$results$get('freqs')
        rowVarName <- self$options$rows
        colVarName <- self$options$cols
        
        data <- private$.cleanData()
        
        # add the row column, containing the row variable
        # fill in dots, if no row variable specified
        
        if ( ! is.null(rowVarName))
          title <- rowVarName
        else
          title <- '.'
        
        freqs$addColumn(
          name=title,
          title=title,
          type='text')
        
        # add the column columns (from the column variable)
        # fill in dots, if no column variable specified
        
        if ( ! is.null(colVarName)) {
          superTitle <- colVarName
          levels <- base::levels(data[[colVarName]])
        } else {
          superTitle <- '.'
          levels <- c('.', '.')
        }
        
        hasSubRows <- sum(self$options$pcRow,
                          self$options$pcCol) > 0
        
        subNames  <- c('[count]', '[pcRow]', '[pcCol]')
        subTitles <- c(.('Count'), .('% within row'), .('% within column'))
        visible   <- c('TRUE', '(pcRow)', '(pcCol)')
        types     <- c('integer', 'number', 'number')
        formats   <- c('', 'pc', 'pc')
        
        # iterate over the sub rows
        
        for (j in seq_along(subNames)) {
          subName <- subNames[[j]]
          if (j == 1)
            v <- '(pcRow || pcCol)'
          else
            v <- visible[j]
          
          freqs$addColumn(
            name=paste0('type', subName),
            title='',
            type='text',
            visible=v)
        }
        
        for (i in seq_along(levels)) {
          level <- levels[[i]]
          
          for (j in seq_along(subNames)) {
            subName <- subNames[[j]]
            freqs$addColumn(
              name=paste0(i, subName),
              title=level,
              superTitle=superTitle,
              type=types[j],
              format=formats[j],
              visible=visible[j])
          }
        }
        
        # add the Total column
        
        freqs$addColumn(
          name='.total[count]',
          title=.('Total'),
          type='integer')
        
        # populate the first column with levels of the row variable
        
        values <- list()
        for (i in seq_along(subNames))
          values[[paste0('type', subNames[i])]] <- subTitles[i]
        
        rows <- private$.grid(data, incRows=TRUE)
        
        for (i in seq_len(nrow(rows))) {
          for (name in dimnames(rows)[[2]]) {
            value <- as.character(rows[i, name])
            if (value == '.total')
              value <- .('Total')
            values[[name]] <- value
          }
          key <- paste0(rows[i,], collapse='`')
          freqs$addRow(rowKey=key, values=values)
          
          if (i == 1)
            freqs$addFormat(rowNo=i, 1, Cell.BEGIN_GROUP)
          else if (i == nrow(rows) - 1)
            freqs$addFormat(rowNo=i, 1, Cell.END_GROUP)
          else if (i == nrow(rows))
            freqs$addFormat(rowNo=i, 1, Cell.BEGIN_END_GROUP)
        }
        
        #        test <- self$results$get('test')
        #        test$addRow(rowKey=1, values=list())
        
        int_text <- paste(rowVarName," \u2A2F ", colVarName)
        
        table <- self$results$cttma
        table$setTitle(.("Support: Marginal main effects and interaction analyses, against the Null model"))
        table$setNote('Note', "The interaction and <i>OR</i> (against 1) will have the same <i>S</i> value. Adding  
        the <i>S</i> values for the 3 components will precisely sum to the total <i>S</i>") 
        table$setRow(rowNo=1, values=list(var= rowVarName))
        table$setRow(rowNo=2, values=list(var= colVarName))
        table$setRow(rowNo=3, values=list(var= int_text))
        table$setRow(rowNo=4, values=list(var="Total"))
        
        table <- self$results$ctt
        table$setTitle(.("Support: Odds Ratio analyses"))
        table$setRow(rowNo=1, values=list(var= "H\u2080 vs odds ratio"))
        table$setRow(rowNo=2, values=list(var="H\u2090 vs odds ratio"))
        table$setRow(rowNo=3, values=list(var="H\u2090 vs H\u2080"))
        
        table <- self$results$ctt2
        table$setRow(rowNo=1, values=list(Interval="Support"))
        table$setRow(rowNo=2, values=list(Interval="Likelihood-based"))
        table$addFootnote(rowNo=2, col="Interval", "See reference Pritikin et al (2017) such intervals 
                          are more accurate and are parameterization-invariant compared to conventional 
                          confidence intervals")
        
        
      },
      
      .run = function() {
        
        rowVarName <- self$options$rows
        colVarName <- self$options$cols
        countsName <- self$options$counts
        
        if (is.null(rowVarName) || is.null(colVarName) || is.null(countsName))
          return()

        data <- private$.cleanData()
        
        if (nlevels(data[[rowVarName]]) < 2)
          jmvcore::reject(.("Row variable '{var}' contains fewer than 2 levels"), code='', var=rowVarName)
        if (nlevels(data[[colVarName]]) < 2)
          jmvcore::reject(.("Column variable '{var}' contains fewer than 2 levels"), code='', var=colVarName)
        if (nlevels(data[[rowVarName]]) > 2)
          jmvcore::reject(.("Row variable '{var}' contains more than 2 levels"), code='', var=rowVarName)
        if (nlevels(data[[colVarName]]) > 2)
          jmvcore::reject(.("Column variable '{var}' contains more than 2 levels"), code='', var=colVarName)
        
        if ( ! is.null(countsName)) {
          countCol <- jmvcore::toNumeric(data[[countsName]])
          
          if (any(countCol < 0, na.rm=TRUE))
            jmvcore::reject(.('Counts may not be negative'))
          if (any(is.infinite(countCol)))
            jmvcore::reject(.('Counts may not be infinite'))
        }
        
        rowVar <- data[[rowVarName]]
        colVar <- data[[colVarName]]
        
        freqs <- self$results$freqs
        
        if (! is.null(countsName))
          result <- stats::xtabs(countCol ~ rowVar + colVar)
        else
          result <- base::table(rowVar, colVar)
        
        colTotals <- apply(result, 2, base::sum)
        freqRowNo <- 1
        
        for (rowNo in seq_len(nrow(result))) {
          
          counts <- result[rowNo,]
          
          if (length(counts) > 0) {
            rowTotal <- sum(counts)
            pcRow <- counts / rowTotal
            pcCol <- counts / colTotals
            
            names(counts) <- paste0(seq_len(length(counts)), '[count]')
            names(pcRow)  <- paste0(seq_len(length(counts)), '[pcRow]')
            names(pcCol)  <- paste0(seq_len(length(counts)), '[pcCol]')
            names(rowTotal)  <- '.total[count]'
            
            freqs$setRow(rowNo=rowNo, values=c(counts, pcRow, pcCol, rowTotal))
            freqRowNo <- freqRowNo + 1
          }
        }
        
        nCols <- length(colTotals)
        
        N <- base::sum(colTotals)
        rowTotal <- N
        values <- as.list(colTotals)
        names(values) <- paste0(1:nCols, '[count]')
        values[['.total[count]']] <- rowTotal
        
        pcRow <- colTotals / rowTotal
        pcRow <- as.list(pcRow)
        names(pcRow) <- paste0(1:nCols, '[pcRow]')
        
        pcCol <- rep(1, nCols)
        pcCol <- as.list(pcCol)
        names(pcCol) <- paste0(1:nCols, '[pcCol]')
        
        names(rowTotal)  <- '.total[count]'
        
        values <- c(values, pcRow, pcCol, rowTotal)
        
        freqs$setRow(freqRowNo, values=values)
        
        rowVarName <- self$options$rows
        colVarName <- self$options$cols
        countsName <- self$options$counts
        
        if (is.null(rowVarName) || is.null(colVarName))
          return()
        
        freqs <- self$results$freqs
        
        rlevels <- levels(self$data[[rowVarName]])
        clevels <- levels(self$data[[colVarName]])
        
        
        tabt <- as.matrix(self$data[[self$options$counts]])
        tab <- as.table(rbind(c(tabt[1],tabt[3]),c(tabt[2],tabt[4])))
        dimnames(tab) <- list(c(rlevels[1],rlevels[2]),c(clevels[1],clevels[2]))
        
        a <- tab[1]
        b <- tab[2]
        c <- tab[3]
        d <- tab[4]
        
        r1tot <- sum(a,c) #sum of 1st row
        r2tot <- sum(b,d) #sum of 2nd row
        c1tot <- sum(a,b)
        c2tot <- sum(c,d)
        mintot <- min(r1tot,r2tot,c1tot,c2tot)
        grandtot <- c1tot+c2tot
        toler=0.0001
        
        orv <- (a*d)/(b*c) # actual odds ratio from the contingency table
        
        f <- function(x,a,b,c,d,c1tot,r1tot,r2tot,goal) {
          (-sum(a*log(a/x), b*log(b/(c1tot-x)), c*log(c/(r1tot-x)),
                d*log(d/(r2tot-c1tot+x)))-goal)^2
        }
        
        # likelihood-based % confidence interval
        goal = -qchisq(self$options$ciWidth/100,1)/2
        xmin1 <- optimize(f, c(0, a), tol = toler, a, b, c, d, c1tot, r1tot,
                          r2tot, goal)
        xmin2 <- optimize(f, c(a, mintot), tol = toler, a, b, c, d, c1tot, r1tot,
                          r2tot, goal)
        beg <- xmin1$minimum*(r2tot-c1tot+xmin1$minimum)/((c1tot-xmin1$minimum)*(r1tot-xmin1$minimum))
        end <- xmin2$minimum*(r2tot-c1tot+xmin2$minimum)/((c1tot-xmin2$minimum)*(r1tot-xmin2$minimum))
        
        # likelihood interval
        goalL <- -self$options$lint
        xmin1L <- optimize(f, c(0, a), tol = toler, a, b, c, d, c1tot, r1tot, r2tot, goalL)
        xmin2L <- optimize(f, c(a, mintot), tol = toler, a, b, c, d, c1tot, r1tot, r2tot, goalL)
        begL <- xmin1L$minimum*(r2tot-c1tot+xmin1L$minimum)/((c1tot-xmin1L$minimum)*(r1tot-xmin1L$minimum))
        endL <- xmin2L$minimum*(r2tot-c1tot+xmin2L$minimum)/((c1tot-xmin2L$minimum)*(r1tot-xmin2L$minimum))
        
        lintlev <- toString(self$options$lint); conflev <- paste0(self$options$ciWidth,"%")
        
        # to determine x axis space for plot
        dif <- orv-begL
        lolim <- orv - 3*dif; hilim <- orv + 4*dif
        if (orv < 1 ) { hilim <- orv + 6*dif}
        if (lolim < 0) {lolim <- 0}
        
        # to determine height of self$options$alt on likelihood function
        if (!is.null(self$options$alt)) {
          goal <- self$options$alt
          g <- function(x,c1tot,r1tot,r2tot,goal) {
            ((x*(r2tot-c1tot+x)/((c1tot-x)*(r1tot-x)))-goal)^2
          }
          exa <- optimize(g, c(0, mintot), tol = toler, c1tot, r1tot, r2tot, goal)
          xa <- unname(unlist(exa[1]))
          xah <- exp(-sum(a*log(a/xa), b*log(b/(c1tot-xa)), c*log(c/(r1tot-xa)), d*log(d/(r2tot-c1tot+xa))))
        }
        
        # and likelihood for 1 (null value)
        
        goal <- self$options$nul
        h <- function(x,c1tot,r1tot,r2tot,goal) {
          ((x*(r2tot-c1tot+x)/((c1tot-x)*(r1tot-x)))-goal)^2
        }
        #  exan <- optimize(h, c(1, c1tot), tol = toler, c1tot, r1tot, r2tot, goal) # if else added Oct 2021 to deal with NaNs
        if (r1tot < c1tot) {
          exan <- optimize(h, c(0, r1tot), tol = toler, c1tot, r1tot, r2tot, goal)
        } else {
          exan <- optimize(h, c(0, c1tot), tol = toler, c1tot, r1tot, r2tot, goal)
        }
        xa <- unname(unlist(exan[1]))
        nullh <- exp(-sum(a*log(a/xa), b*log(b/(c1tot-xa)), c*log(c/(r1tot-xa)),
                          d*log(d/(r2tot-c1tot+xa))))
        
        S2way <- log(nullh) # check that this should be the same as S for observed OR
        
        # variance analysis and chi-square
        suppressWarnings(lt <- chisq.test(tab,correct=self$options$cc)) # ignore warning message
        chi.s <- unname(lt$statistic)
        df <- unname(lt$parameter)
        toogood <- df/2*(log(df/chi.s)) - (df - chi.s)/2
        
        # marginal main effects analysis
        exp_row <- (r1tot+r2tot)/2
        Srow <- r1tot*log(r1tot/exp_row)+r2tot*log(r2tot/exp_row)
        exp_col <- (c1tot+c2tot)/2
        Scol <- c1tot*log(c1tot/exp_row)+c2tot*log(c2tot/exp_row)
        Sint <- sum(lt$observed * log(lt$observed/lt$expected))
        exp_int <- grandtot/4
        Sgt <- a*log(a/exp_int)+b*log(b/exp_int)+c*log(c/exp_int)+d*log(d/exp_int) # total S
        
        # support for alt. H
        Salt <- log(xah)
        SexOR_null <- Salt - S2way
        SexOR_obs <- SexOR_null - S2way
        
        gn <- 2*abs(S2way) # likelihood ratio statistic
        gn_p <- 1-pchisq(gn,1)
        ga <- 2*abs(Salt)
        ga_p <- 1-pchisq(ga,1)
        gan <- 2*abs(SexOR_null)
        gan_p <- 1-pchisq(gan,1)
        gt_p <- 1-pchisq(2*Sgt,1)
        gr_p <- 1-pchisq(2*Srow,1)
        gc_p <- 1-pchisq(2*Scol,1)
        gi_p <- 1-pchisq(2*Sint,1)
        
        table <- self$results$cttma
        table$setRow(rowNo=1, values=list(Value=exp_row, S=Srow, G=2*Srow, df=1, p=gr_p))
        table$setRow(rowNo=2, values=list(Value=exp_col, S=Scol, G=2*Scol, df=1, p=gc_p))
        table$setRow(rowNo=3, values=list(Value="", S=Sint, G=2*Sint, df=1, p=gi_p))
        table$setRow(rowNo=4, values=list(Value=exp_int, S=Sgt, G=2*Sgt, df=1, p=gt_p))
        
        table <- self$results$ctt
        table$setRow(rowNo=1, values=list(Value=self$options$nul, 
                                          ordiff= self$options$nul-orv, S=S2way, G=gn, df=df, p=gn_p))
        table$setRow(rowNo=2, values=list(Value=self$options$alt, 
                                          ordiff= self$options$alt-orv, S=Salt, G=ga, df=df, p=ga_p))
        table$setRow(rowNo=3, values=list(Value="", ordiff= self$options$alt-self$options$nul, 
                                          S=SexOR_null, G=gan, df=df, p=gan_p))
        
        table <- self$results$ctt2
        table$setRow(rowNo=1, values=list(Level=lintlev, OR = orv, Lower=begL, Upper=endL))
        table$setRow(rowNo=2, values=list(Level=conflev, OR = orv, Lower=beg, Upper=end))
        
        table <- self$results$ctt3
        table$setNote('Note', "Unlike the \u03C7\u00B2 statistic, a large <i>S</i> value indicates 
          that the proportions are either more different or too similar compared with those expected") 
        if (isTRUE(self$options$cc))
          table$setNote('Note', "Continuity correction applied. Unlike the \u03C7\u00B2 statistic, a large <i>S</i> value indicates 
          that the proportions are either more different or too similar compared with those expected")
        table$setRow(rowNo=1, values=list(var= "For <i>OR</i> = 1", Sv=toogood, X2=chi.s, dfv=df, 
                                          pv=lt$p.value, pv1=1-lt$p.value))
        
        # stats for summary        
        stats <- list(S1 = S2way,
                      S2 = Salt,
                      S3 = SexOR_null,
                      S4 = Srow,
                      S5 = Scol,
                      S7 = Sgt,
                      tg = toogood,
                      chi = chi.s)
        
        # Populate Explanation & table
        private$.populateSupportText(stats)
        private$.populateMoreSupportText()
        #
        
        if(isTRUE(self$options$varA)) { 
          
          table <- self$results$ctt3
          table$setVisible(TRUE)
          
        }
        
        g <- data.frame(orv=orv, a=a, b=b, c=c, d=d, r1tot=r1tot, r2tot=r2tot, c1tot=c1tot, c2tot=c2tot, 
                        goalL=goalL, nullh=nullh, xah=xah, begL=begL,endL=endL)
        imagec <- self$results$plotc
        imagec$setState(g)
        
        if(isTRUE(self$options$pll)) {
          
          plotc <- self$results$plotc
          plotc$setVisible(TRUE)
          
        }
      },
      
.plotc=function(imagec, ...) {
  
  g <- imagec$state
  
  res <- 100            # resolution, increase for greater resolution
  mintot <- min(g$r1tot,g$r2tot,g$c1tot,g$c2tot)
  arrlen <- res*mintot-1
  xs <- 0; ys <- 0
  for (i in 1:arrlen) {     # arrays to plot likelihood vs OR
    dv <- i/res
    ys[i] <- exp(-sum(g$a*log(g$a/dv), g$b*log(g$b/(g$c1tot-dv)),
                      g$c*log(g$c/(g$r1tot-dv)), g$d*log(g$d/(g$r2tot-g$c1tot+dv))))
    xs[i] <- dv*(g$r2tot-g$c1tot+dv)/((g$c1tot-dv)*(g$r1tot-dv))
  }
  
  # to determine x axis space for plot
  seor <- sqrt(1/g$a+1/g$b+1/g$c+1/g$d)
  lolim <- exp(log(g$orv)-3*seor); hilim <- exp(log(g$orv)+3*seor)
  if (lolim < 0) {lolim <- 0}
  
  # do the plot with lines
  plot <- plot(xs, ys, xlim=c(lolim,hilim),type="l", lwd = 1, xlab = "Odds Ratio", ylab = "Likelihood")
  lines(c(g$orv,g$orv),c(0,1),lty=2) # add MLE as dashed line
  segments(g$begL, exp(g$goalL), g$endL, exp(g$goalL), lwd = 1, col = "red")
  lines(c(self$options$nul,self$options$nul),c(0,g$nullh), lty=1, col = "black") # add H prob as black line
  
  if (!is.null(self$options$alt)) {
    lines(c(self$options$alt,self$options$alt), c(0,g$xah), lty=1, col = "blue") # add H prob as blue line
    
    print(plot)
    TRUE
    
  }
      },

      .grid=function(data, incRows=FALSE) {
        
        rowVarName <- self$options$get('rows')
        
        expand <- list()
        
        if (incRows) {
          if (is.null(rowVarName))
            expand[['.']] <- c('.', '. ', .('Total'))
          else
            expand[[rowVarName]] <- c(base::levels(data[[rowVarName]]), '.total')
        }
        
        rows <- rev(expand.grid(expand))
        
        rows
      },
      .sourcifyOption = function(option) {
        if (option$name %in% c('rows', 'cols', 'counts'))
          return('')
        super$.sourcifyOption(option)
      },
      .formula=function() {
        if (is.null(self$options$rows) || is.null(self$options$cols))
          return('~')
        jmvcore:::composeFormula(self$options$counts, list(list(self$options$rows, self$options$cols)))
      },

      .cleanData = function() {
        
        rowVarName <- self$options$rows
        colVarName <- self$options$cols
        countsName <- self$options$counts
        
        data <- jmvcore::select(self$data, c(rowVarName, colVarName, countsName))
        data <- jmvcore::naOmit(data)
        
        if ( ! is.null(rowVarName))
          data[[rowVarName]] <- as.factor(data[[rowVarName]])
        if ( ! is.null(colVarName))
          data[[colVarName]] <- as.factor(data[[colVarName]])
        if ( ! is.null(countsName))
          data[[countsName]]  <- jmvcore::toNumeric(data[[countsName]])
        
        data
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
          
          rowVarName <- self$options$rows
          colVarName <- self$options$cols
          int_text <- paste(rowVarName," \u2A2F ", colVarName)
          
          Sxl = list(s=st$S1, "H\u2080", "the observed <i>OR</i>")                       
          stg1 <- private$.strength(Sxl)
          Sxl = list(s=st$S2, "H\u2090", "the observed <i>OR</i>")                       
          stg2 <- private$.strength(Sxl)
          Sxl = list(s=st$S3, "H\u2090", "H\u2080")  
          stg3 <- private$.strength(Sxl)
          Sxl = list(s=st$S4, rowVarName, "the Null model")  
          stg4 <- private$.strength(Sxl)
          Sxl = list(s=st$S5, colVarName, "the Null model")  
          stg5 <- private$.strength(Sxl)
          Sxl = list(s=st$S7, "Total components", "the Null model")  
          stg7 <- private$.strength(Sxl)
          
          Sxl = list(s=st$tg)                       
          sv <- private$.strength2(Sxl)

          stg8 <- paste0("For the observed <i>OR</i>", sv, ", that it was more different from the H\u2080 = 1 
                         than expected")
          if (2*st$tg > st$chi) {
            stg8 <- paste0("For the observed <i>OR</i>", sv, ", that it was closer to the H\u2080 = 1 than expected")
          }
          
          str = paste0("<br> <h2>Summarizing the evidential analysis</h2>
                               <br>  <i>The main <i>OR</i> analysis shows that:</i> <br>", 
                       stg1, "<br>", stg2, "<br>", stg3, 
                       "<p>
                       <i>The additional analysis of marginal main effects (not necessarily required) shows that:
                       </i> <br>", stg4, "<br>", stg5,"<br>", stg7, 
                       "<p>
                       <i>The variance analysis shows that:</i><br>", 
                       stg8,
                       "<p>Give the <i>OR</i> and the observed frequencies. The support interval for the <i>OR</i> 
                       can be given, along with the likelihood-based % confidence interval (see Pritikin et al, 2017).
                       <br>The available <i>p</i> values for the <i>G</i> test (likelihood ratio test) may also be supplied 
                       to allow comparison with a conventional analysis.
                       </p><br>
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
          str3 <- "For example, we could choose a meaningful <i>H</i>\u2090 <i>OR</i> to compare with a specified <i>H</i>\u2080 <i>OR</i> 
          (default = 1). This is shown by the last line of the main Support table for <i>OR</i> analyses. "
          
          str = paste0(str1, str2, str3, "As data accumulates the strength of evidence for one hypothesis over another will tend 
                       to increase.")
          
          
          html$setContent(str)
          
        }

)
)
