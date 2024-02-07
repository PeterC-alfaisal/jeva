
# This file is a generated template, your changes will not be overwritten

ldiagClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "ldiagClass",
    inherit = ldiagBase,
    #### Active bindings ----
    active = list(
      countsName = function() {
        if (is.null(private$.countsName)) {
          analysisCounts <- self$options$counts
          if ( ! is.null(analysisCounts))
            private$.countsName <- analysisCounts
          else if ( ! is.null(attr(self$data, "jmv-weights"))) {
            private$.countsName <- ".COUNTS"
          }
        }
        
        return(private$.countsName)
      }
    ),
    private=list(
      #### Member variables ----
      .countsName = NULL,
      #### Init + run functions ----
      .init=function() {
        
        private$.initSupportTab()
        
        rowVarName <- self$options$rows
        colVarName <- self$options$cols
        layerNames <- NULL
        countsName <- self$options$counts
        
        freqs <- self$results$freqs
        
        data <- private$.cleanData()
        
        reversed <- rev(layerNames)
        for (i in seq_along(reversed)) {
          layer <- reversed[[i]]
          freqs$addColumn(name=layer, type='text', combineBelow=TRUE)
        }
        
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
        }
        else {
          superTitle <- '.'
          levels <- c('.', '.')
        }
        
        subNames  <- c('[count]', '[expected]', '[pcRow]', '[pcCol]', '[pcTot]')
        subTitles <- c(.('Observed'), .('Expected'), .('% within row'), .('% within column'), .('% of total'))
        visible   <- c('(obs)', '(exp)', '(pcRow)', '(pcCol)', '(pcTot)')
        types     <- c('integer', 'number', 'number', 'number', 'number')
        formats   <- c('', '', 'pc', 'pc', 'pc')
        
        # iterate over the sub rows
        
        for (j in seq_along(subNames)) {
          subName <- subNames[[j]]
          if (subName == '[count]')
            v <- '(obs && (exp || pcRow || pcCol || pcTot))'
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
        
        if (self$options$obs) {
          freqs$addColumn(
            name='.total[count]',
            title=.('Total'),
            type='integer')
        }
        
        if (self$options$exp) {
          freqs$addColumn(
            name='.total[exp]',
            title=.('Total'),
            type='number')
        }
        
        if (self$options$pcRow) {
          freqs$addColumn(
            name='.total[pcRow]',
            title=.('Total'),
            type='number',
            format='pc')
        }
        
        if (self$options$pcCol) {
          freqs$addColumn(
            name='.total[pcCol]',
            title=.('Total'),
            type='number',
            format='pc')
        }
        
        if (self$options$pcTot) {
          freqs$addColumn(
            name='.total[pcTot]',
            title=.('Total'),
            type='number',
            format='pc')
        }
        
        # populate the first column with levels of the row variable
        
        values <- list()
        for (i in seq_along(subNames))
          values[[paste0('type', subNames[i])]] <- subTitles[i]
        
        rows <- private$.grid(data=data, incRows=TRUE)
        
        nextIsNewGroup <- TRUE
        
        for (i in seq_len(nrow(rows))) {
          
          for (name in colnames(rows)) {
            value <- as.character(rows[i, name])
            if (value == '.total')
              value <- .('Total')
            values[[name]] <- value
          }
          
          key <- paste0(rows[i,], collapse='`')
          freqs$addRow(rowKey=key, values=values)
          
          if (nextIsNewGroup) {
            freqs$addFormat(rowNo=i, 1, Cell.BEGIN_GROUP)
            nextIsNewGroup <- FALSE
          }
          
          if (as.character(rows[i, name]) == '.total') {
            freqs$addFormat(rowNo=i, 1, Cell.BEGIN_END_GROUP)
            nextIsNewGroup <- TRUE
            if (i > 1)
              freqs$addFormat(rowNo=i - 1, 1, Cell.END_GROUP)
          }
        }
        
        rows <- private$.grid(data=data, incRows=FALSE)
        values <- list()
        
        if (length(rows) == 0) {
          
          
        } else {
          
          for (i in seq_len(nrow(rows))) {
            
            for (name in dimnames(rows)[[2]]) {
              value <- as.character(rows[i, name])
              if (value == '.total')
                value <- .('Total')
              values[[name]] <- value
            }
            
          }
        }
        
        table <- self$results$stats
        table$setTitle(.("Diagnostic statistics"))
        table$setRow(rowNo=1, values=list(var= "Value"))
        
        
        int_text <- paste(rowVarName," \u2A2F ", colVarName)
        
        if(self$options$correction=="ob") { notext <- "S uses Occam's Bonus correction for parameters (Param). "
        } else if(self$options$correction=="aic") { notext <- "S uses AIC correction for parameters (Param). "
        } else if(self$options$correction=="aicsm") { notext <- "S uses AIC small sample correction for parameters (Param). "
        } else {
          notext <- "S uses no correction for parameters (Param). "
        }
        
        
        table <- self$results$ctt
        table$setNote('Note', notext)
        table$setTitle(.("Support: Diagnostic statistics"))
        table$setRow(rowNo=1, values=list(var= "H\u2080 vs Sensitivity"))
        table$setRow(rowNo=2, values=list(var="H\u2080 vs Specificity"))
        
        table <- self$results$cttsens
        table$setRow(rowNo=1, values=list(Interval="Support"))
        table$setRow(rowNo=2, values=list(Interval="Likelihood-based"))
        
        table <- self$results$cttspec
        table$setRow(rowNo=1, values=list(Interval="Support"))
        table$setRow(rowNo=2, values=list(Interval="Likelihood-based"))
        table$addFootnote(rowNo=2, col="Interval", "See reference Pritikin et al (2017), such intervals 
                          are more accurate and are parameterization-invariant compared to conventional 
                          confidence intervals")
        
        private$.initBarPlot()
        
      },
      .run=function() {
        
        rowVarName <- self$options$rows
        colVarName <- self$options$cols
        countsName <- self$options$counts
        
        if (is.null(rowVarName) || is.null(colVarName))
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
          countCol <- data[[countsName]]
          if (any(countCol < 0, na.rm=TRUE))
            jmvcore::reject(.('Counts may not be negative'))
          if (any(is.infinite(countCol)))
            jmvcore::reject(.('Counts may not be infinite'))
        }
        
        freqs <- self$results$freqs
        
        freqRowNo <- 1
        othRowNo <- 1
        
        
        mats <- private$.matrices(data)
        
        nRows  <- base::nlevels(data[[rowVarName]])
        nCols  <- base::nlevels(data[[colVarName]])
        nCells <- nRows * nCols
        
        ciWidth <- self$options$ciWidth / 100
        
        for (mat in mats) {
          
          suppressWarnings({
            
            test <- try(chisq.test(mat, correct=FALSE))
            n <- sum(mat)
            
            if (base::inherits(test, 'try-error'))
              exp <- mat
            else
              exp <- test$expected
            
            
          }) # suppressWarnings
          
          total <- sum(mat)
          colTotals <- apply(mat, 2, sum)
          rowTotals <- apply(mat, 1, sum)
          
          for (rowNo in seq_len(nRows)) {
            
            values <- mat[rowNo,]
            rowTotal <- sum(values)
            
            pcRow <- values / rowTotal
            
            values <- as.list(values)
            names(values) <- paste0(1:nCols, '[count]')
            values[['.total[count]']] <- rowTotal
            
            expValues <- exp[rowNo,]
            expValues <- as.list(expValues)
            names(expValues) <- paste0(1:nCols, '[expected]')
            expValues[['.total[exp]']] <- sum(exp[rowNo,])
            
            pcRow <- as.list(pcRow)
            names(pcRow) <- paste0(1:nCols, '[pcRow]')
            pcRow[['.total[pcRow]']] <- 1
            
            pcCol <- as.list(mat[rowNo,] / colTotals)
            names(pcCol) <- paste0(1:nCols, '[pcCol]')
            pcCol[['.total[pcCol]']] <- unname(rowTotals[rowNo] / total)
            
            pcTot <- as.list(mat[rowNo,] / total)
            names(pcTot) <- paste0(1:nCols, '[pcTot]')
            pcTot[['.total[pcTot]']] <- sum(mat[rowNo,] / total)
            
            values <- c(values, expValues, pcRow, pcCol, pcTot)
            
            freqs$setRow(rowNo=freqRowNo, values=values)
            freqRowNo <- freqRowNo + 1
          }
          
          values <- apply(mat, 2, sum)
          rowTotal <- sum(values)
          values <- as.list(values)
          names(values) <- paste0(1:nCols, '[count]')
          values[['.total[count]']] <- rowTotal
          
          expValues <- apply(mat, 2, sum)
          expValues <- as.list(expValues)
          names(expValues) <- paste0(1:nCols, '[expected]')
          
          pcRow <- apply(mat, 2, sum) / rowTotal
          pcRow <- as.list(pcRow)
          names(pcRow) <- paste0(1:nCols, '[pcRow]')
          
          pcCol <- rep(1, nCols)
          pcCol <- as.list(pcCol)
          names(pcCol) <- paste0(1:nCols, '[pcCol]')
          
          pcTot <- apply(mat, 2, sum) / total
          pcTot <- as.list(pcTot)
          names(pcTot) <- paste0(1:nCols, '[pcTot]')
          
          expValues[['.total[exp]']] <- total
          pcRow[['.total[pcRow]']] <- 1
          pcCol[['.total[pcCol]']] <- 1
          pcTot[['.total[pcTot]']] <- 1
          
          values <- c(values, expValues, pcRow, pcCol, pcTot)
          
          freqs$setRow(rowNo=freqRowNo, values=values)
          freqRowNo <- freqRowNo + 1
          
          ##########################################################
          tab <- mat
          
          HAc <- FALSE
          a <- tab[1]
          b <- tab[2]
          c <- tab[3]
          d <- tab[4]
          if (a == 0 | b == 0 | c == 0 | d == 0) {
            a=a+0.5;b=b+0.5;c=c+0.5;d=d+0.5;HAc=TRUE       # Haldane-Anscombe correction
          }                   
          r1tot <- sum(a,c) #sum of 1st row
          r2tot <- sum(b,d) #sum of 2nd row
          c1tot <- sum(a,b)
          c2tot <- sum(c,d)
          grandtot <- c1tot+c2tot
          minmarg <- min(r1tot,r2tot,c1tot,c2tot)
          maxmarg <- max(r1tot,r2tot,c1tot,c2tot)
          
          toler=0.0001
          
          # chi-square
          suppressWarnings(lt <- chisq.test(tab,correct=self$options$cc)) # ignore warning message
          chi.s <- unname(lt$statistic)
          df <- unname(lt$parameter)
          # correct if 0 cells
          tabt1=lt$observed
          for (i in 1:length(tab)) {
            tabt1[i] <- lt$observed[i]
            if (lt$observed[i] < 1) tabt1[i]=1   # turn 0s into 1s for one table used for log
          }
          
          # Correction
          Ac <- function(c,k1,k2) { 
            if(c=="nc") { 0
            } else if(c=="ob") { 0.5*(k2-k1) 
            } else { 1*(k2-k1)
            } 
          }
          
          # main marginal totals
          row_sum <- rowSums(tab)
          col_sum <- colSums(tab)
          
          # do not allow 0 marginal totals
          for (i in 1:length(row_sum)) {
            if (row_sum[i] < 1) jmvcore::reject(.("Margin '{var}' has 0 total"), code='', var=rowVarName)
          }
          for (i in 1:length(col_sum)) {
            if (col_sum[i] < 1) jmvcore::reject(.("Margin '{var}' has 0 total"), code='', var=colVarName)
          }
          sens <- a/(a+b)
          spec <- d/(c+d)
          rr <- (a*(b+d))/(b*(a+c)) # RR from the contingency table
          lr <- sens/(1-spec)
          loglr <- -log(lr)
          nlr <- (1-sens)/spec
          acc <- (a+d)/grandtot
          or <- a*d/(b*c)
          prev <- c1tot/grandtot
          Youd <- spec+sens-1
          
          if(self$options$pprob) prev = self$options$ppval
          pretest_odds <- prev/(1-prev)
          ppv <- lr * pretest_odds/(1 + lr * pretest_odds)   # a/r1tot
          npv <- 1 - nlr * pretest_odds/(1 + nlr * pretest_odds) # d/r2tot
          
          if(b == 0.5) {
            npv <- 1
            sens <- 1
          }
          if(c == 0.5) {
            ppv <- 1
            spec <- 1
          }
          table <- self$results$stats
          table$setRow(rowNo=1, values=list(LR=lr, NLR=nlr, Acc=acc, OR=or, Prev=prev, PPV=ppv, NPV=npv, Youd=Youd))
          
          
          f1 <- function(x,a,b,c1tot,goal) {                  # for sensitivity
            (-sum(a*log(a/x), b*log(b/(c1tot-x)))-goal)^2
          }
          f2 <- function(x,d,c,c2tot,goal) {                  # for specificity
            (-sum(d*log(d/x), c*log(c/(c2tot-x)))-goal)^2
          }
          
          # likelihood-based % confidence interval
          
          arry <- numeric(maxmarg)   # finding endpoints for S values
          for(x in 1:maxmarg) {
            arry[x] <- x*(r2tot-c1tot+x)/((c1tot-x)*(r1tot-x))
          }
          arry[!is.finite(arry)] <- 0
          ind <- which(arry > 0)
          aa <- split(ind, cumsum(c(0, diff(ind) > 1)))
          dvs <- min(aa$'0')-1
          dve <- max(aa$'0')+1
          
          # Intervals for sensitivity
          # likelihood interval
          goalL <- -self$options$lint
          suppressWarnings(xmin1L <- optimize(f1, c(0, a), tol = toler,  a, b, c1tot, goalL))
          suppressWarnings(xmin2L <- optimize(f1, c(a, c1tot), tol = toler,  a, b, c1tot, goalL))
          begL <- xmin1L$minimum/c1tot
          endL <- xmin2L$minimum/c1tot
          # Support interval
          goal = -qchisq(self$options$ciWidth/100,1)/2
          suppressWarnings(xmin1 <- optimize(f1, c(0, a), tol = toler, a, b, c1tot, goal))
          suppressWarnings(xmin2 <- optimize(f1, c(a, c1tot), tol = toler, a, b, c1tot, goal))
          beg <- xmin1$minimum/c1tot
          end <- xmin2$minimum/c1tot
          
          # Intervals for specificity
          # likelihood interval
          goalL <- -self$options$lint
          suppressWarnings(xmin1L <- optimize(f2, c(0, d), tol = toler,  d, c, c2tot, goalL))
          suppressWarnings(xmin2L <- optimize(f2, c(d, c2tot), tol = toler,  d, c, c2tot, goalL))
          begL2 <- xmin1L$minimum/c2tot
          endL2 <- xmin2L$minimum/c2tot
          # Support interval
          goal = -qchisq(self$options$ciWidth/100,1)/2
          suppressWarnings(xmin1 <- optimize(f2, c(0, d), tol = toler, d, c, c2tot, goal))
          suppressWarnings(xmin2 <- optimize(f2, c(d, c2tot), tol = toler, d, c, c2tot, goal))
          beg2 <- xmin1$minimum/c2tot
          end2 <- xmin2$minimum/c2tot
          
          lintlev <- toString(self$options$lint); conflev <- paste0(self$options$ciWidth,"%")
          
          # x axis limits - for sensitivity
          goalx <- self$options$supplot   # with e^-10 we get x values for when curve is down to 0.00004539
          suppressWarnings(xmin1x <- optimize(f1, c(0, a), tol = toler, a, b, c1tot, goalx))
          suppressWarnings(xmin2x <- optimize(f1, c(a, c1tot), tol = toler, a, b, c1tot, goalx))
          xmin <- xmin1x$minimum/c1tot
          xmax <- xmin2x$minimum/c1tot
          
          # for specificity
          goalx <- self$options$supplot   # with e^-10 we get x values for when curve is down to 0.00004539
          suppressWarnings(xmin1x <- optimize(f2, c(0, d), tol = toler, d, c, c2tot, goalx))
          suppressWarnings(xmin2x <- optimize(f2, c(d, c2tot), tol = toler, d, c, c2tot, goalx))
          xmin2 <- xmin1x$minimum/c2tot
          xmax2 <- xmin2x$minimum/c2tot
          
          # to determine height of self$options$nulsens and nulspec on likelihood function
          
          goal <- self$options$nulsens
          
          h1 <- function(x,c1tot,goal) {
            ((x/c1tot)-goal)^2
          }
          suppressWarnings(exa2 <- optimize(h1, c(0, c1tot), tol = toler, c1tot, goal))
          xa <- unname(unlist(exa2[1]))
          nul_sens_h <- exp(-sum(a*log(a/xa), b*log(b/(c1tot-xa))))
          
          Ssens <- log(nul_sens_h)
          if(self$options$nulsens == sens) Ssens <- 0
          
          # same for specificity
          
          goal <- self$options$nulspec
          h2 <- function(x,c2tot,goal) {
            ((x/c2tot)-goal)^2
          }
          suppressWarnings(exa2 <- optimize(h2, c(0, c2tot), tol = toler, c2tot, goal))
          xa <- unname(unlist(exa2[1]))
          nul_spec_h <- exp(-sum(d*log(d/xa), c*log(c/(c2tot-xa))))
          
          Sspec <- log(nul_spec_h)
          if(self$options$nulspec == spec) Sspec <- 0
          
          # variance analysis
          toogood <- df/2*(log(df/chi.s)) - (df - chi.s)/2
          
          glr <- 2*abs(loglr) # likelihood ratio statistic
          glr_p <- 1-pchisq(glr,1)
          gn <- 2*abs(Ssens) # sensitivity
          gn_p <- 1-pchisq(gn,1)
          ga <- 2*abs(Sspec) # specificity
          ga_p <- 1-pchisq(ga,1)
          
          table <- self$results$ctt
          table$setRow(rowNo=1, values=list(Value=self$options$nulsens, ordiff= self$options$nulsens-sens, 
                                            S=Ssens + Ac(self$options$correction,1,2), Param=paste0(c(1,2), collapse = ', '),
                                            G=gn, df=df, p=gn_p))
          table$setRow(rowNo=2, values=list(Value=self$options$nulspec, ordiff= self$options$nulspec-spec, 
                                            S=Sspec + Ac(self$options$correction,1,2), Param=paste0(c(1,2), collapse = ', '), 
                                            G=ga, df=df, p=ga_p))
          
          table <- self$results$cttsens
          table$setRow(rowNo=1, values=list(Level=lintlev, sens = sens, Lower=begL, Upper=endL))
          table$setRow(rowNo=2, values=list(Level=conflev, sens = sens, Lower=beg, Upper=end))
          if (HAc)
            table$addFootnote(rowNo=1, col="sens", "Haldane-Anscombe correction applied")      
          
          table <- self$results$cttspec
          table$setRow(rowNo=1, values=list(Level=lintlev, spec = spec, Lower=begL2, Upper=endL2))
          table$setRow(rowNo=2, values=list(Level=conflev, spec = spec, Lower=beg2, Upper=end2))
          if (HAc)
            table$addFootnote(rowNo=1, col="spec", "Haldane-Anscombe correction applied")      
          
          table <- self$results$ctt3
          table$setNote('Note', "Unlike the \u03C7\u00B2 statistic, a large S value indicates 
          that the proportions are either more different or too similar compared with those expected") 
          if (isTRUE(self$options$cc))
            table$setNote('Note', "Continuity correction applied. Unlike the \u03C7\u00B2 statistic, a large S value indicates 
          that the proportions are either more different or too similar compared with those expected")
          table$setRow(rowNo=1, values=list(var= "For LR = 1", Sv=toogood, X2=chi.s, dfv=df, 
                                            pv=lt$p.value, pv1=1-lt$p.value))
          
          # stats for summary        
          stats <- list(S1 = lr,
                        S2 = Ssens,
                        S3 = Sspec,
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
          
          g <- data.frame(sens=sens, spec=spec, nul_sens_h=nul_sens_h, nul_spec_h=nul_spec_h,
                          a=a, b=b, c=c, d=d, r1tot=r1tot, r2tot=r2tot, c1tot=c1tot, c2tot=c2tot, grandtot=grandtot,
                          goalL=goalL, begL=begL,endL=endL, begL2=begL2,endL2=endL2,
                          xmin=xmin, xmax=xmax, xmin2=xmin2, xmax2=xmax2)
          imagec <- self$results$plotc
          imagec$setState(g)
          
          if(isTRUE(self$options$pll)) {
            
            plotc <- self$results$plotc
            plotc$setVisible(TRUE)
            
          }
          
          # Send Data to Plot ----
          
          
          plotData1 <- list(
            "Prevalence" = prev,
            "Sens" = sens,
            "Spec" = spec,
            "Plr" = lr,
            "Nlr" = nlr
          )
          
          image1 <- self$results$plot1
          image1$setState(plotData1)
          
          
          ########################################################
          
        }
      },
      
      #### Plot functions ----
      .initBarPlot = function() {
        image <- self$results$get('barplot')
        
        width <- 450
        height <- 400
        
        layerNames <- NULL
        if (length(layerNames) == 1)
          image$setSize(width * 2, height)
        else if (length(layerNames) >= 2)
          image$setSize(width * 2, height * 2)
      },
      .barPlot = function(image, ggtheme, theme, ...) {
        if (! self$options$barplot)
          return()
        
        rowVarName <- self$options$rows
        colVarName <- self$options$cols
        countsName <- self$options$counts
        layerNames <- NULL
        if (length(layerNames) > 2)
          layerNames <- layerNames[1:2] # max 2
        
        if (is.null(rowVarName) || is.null(colVarName))
          return()
        
        #        data <- private$.cleanData()
        data <- private$.cleanData(B64 = TRUE)
        data <- na.omit(data)
        
        if (! is.null(countsName)){
          untable <- function (df, counts) df[rep(1:nrow(df), counts), ]
          data <- untable(data[, c(rowVarName, colVarName, layerNames)], counts=data[, countsName])
        }
        
        formula <- jmvcore::composeFormula(NULL, c(rowVarName, colVarName, layerNames))
        counts <- xtabs(formula, data)
        d <- dim(counts)
        
        expand <- list()
        for (i in c(rowVarName, colVarName, layerNames))
          expand[[i]] <- base::levels(data[[i]])
        tab <- expand.grid(expand)
        tab$Counts <- as.numeric(counts)
        
        if (self$options$yaxis == "ypc") { # percentages
          props <- counts
          
          if (self$options$yaxisPc == "column_pc") {
            pctVarName <- colVarName
          } else if (self$options$yaxisPc == "row_pc") {
            pctVarName <- rowVarName
          } else { # total
            pctVarName <- NULL
          }
          
          if (length(layerNames) == 0) {
            props <- proportions(counts, pctVarName)
          } else if (length(layerNames) == 1) {
            for (i in seq.int(1, d[3], 1)) {
              props[,,i] <- proportions(counts[,,i], pctVarName)
            }
          } else { # 2 layers
            for (i in seq.int(1, d[3], 1)) {
              for (j in seq.int(1, d[4], 1)) {
                props[,,i,j] <- proportions(counts[,,i,j], pctVarName)
              }
            }
          }
          
          tab$Percentages <- as.numeric(props) * 100
        }
        
        if (self$options$xaxis == "xcols") {
          xVarName <- ensym(colVarName)
          zVarName <- ensym(rowVarName)
        } else {
          xVarName <- ensym(rowVarName)
          zVarName <- ensym(colVarName)
        }
        
        position <- self$options$bartype
        
        if (self$options$yaxis == "ycounts") {
          p <- ggplot(data=tab, aes(y=Counts, x=!!xVarName, fill=!!zVarName)) +
            geom_col(position=position, width = 0.7) +
            labs(y = .("Counts"))
        } else {
          p <- ggplot(data=tab, aes(y=Percentages, x=!!xVarName, fill=!!zVarName)) +
            geom_col(position=position, width = 0.7)
          
          if (self$options$yaxisPc == "total_pc") {
            p <- p + labs(y = .("Percentages of total"))
          } else {
            p <- p + labs(y = jmvcore::format(.("Percentages within {var}"), var=pctVarName))
          }
        }
        
        if (! is.null(layerNames)) {
          if (length(layerNames) == 1)
            layers <- as.formula(jmvcore::composeFormula(NULL, layerNames))
          else
            layers <- as.formula(jmvcore::composeFormula(layerNames[1], layerNames[2]))
          
          p <- p + facet_grid(layers)
        }
        p <- p + ggtheme
        
        return(p)
      },      
      
      .plotc=function(imagec, ...) {
        
        g <- imagec$state
        
        res <- 100    # resolution, increase for greater resolution
        
        if(self$options$choice=="sens_plot") {
          arrlen <- res*g$c1tot
          xs <- 0; ys <- 0
          for (i in 1:arrlen) {     # arrays to plot likelihood vs Sensitivity
            dv <- i/res
            ys[i] <- exp(-sum(g$a*log(g$a/dv), g$b*log(g$b/(g$c1tot-dv))))
            xs[i] <- dv/(g$c1tot)
          }
          
          # do the plot with lines
          if(self$options$plotype=="lplot") {
            plot <- plot(xs, ys, xlim=c(g$xmin,g$xmax),type="l", lwd = 1, xlab = "Sensitivity", ylab = "Likelihood")        
            lines(c(g$sens,g$sens),c(0,1),lty=2) # add MLE as dashed line
            segments(g$begL, exp(g$goalL), g$endL, exp(g$goalL), lwd = 1, col = "red")
            lines(c(self$options$nulsens,self$options$nulsens),c(0,g$nul_sens_h), lty=1, col = "black") # add H prob as black line
          } else {
            plot <- plot(xs, log(ys), xlim=c(g$xmin,g$xmax),type="l", lwd = 1, xlab = "Sensitivity", 
                         ylim=c(self$options$supplot,0), ylab = "Log Likelihood")
            lines(c(g$sens,g$sens),c(self$options$supplot,0),lty=2) # add MLE as dashed line
            segments(g$begL, g$goalL, g$endL, g$goalL, lwd = 1, col = "red")
            lines(c(self$options$nulsens,self$options$nulsens),c(self$options$supplot,log(g$nul_sens_h)), lty=1, col = "black") # add H prob as black line
          }
        } else {
          arrlen <- res*g$c2tot
          xs <- 0; ys <- 0
          for (i in 1:arrlen) {     # arrays to plot likelihood vs Specificity
            dv <- i/res
            ys[i] <- exp(-sum(g$d*log(g$d/dv), g$c*log(g$c/(g$c2tot-dv))))
            xs[i] <- dv/(g$c2tot)
          }
          
          # do the plot with lines
          if(self$options$plotype=="lplot") {
            plot <- plot(xs, ys, xlim=c(g$xmin2,g$xmax2),type="l", lwd = 1, xlab = "Specificity", ylab = "Likelihood")        
            lines(c(g$spec,g$spec),c(0,1),lty=2) # add MLE as dashed line
            segments(g$begL2, exp(g$goalL), g$endL2, exp(g$goalL), lwd = 1, col = "red")
            lines(c(self$options$nulspec,self$options$nulspec),c(0,g$nul_spec_h), lty=1, col = "black") # add H prob as black line
          } else {
            plot <- plot(xs, log(ys), xlim=c(g$xmin2,g$xmax2),type="l", lwd = 1, xlab = "Specificity", 
                         ylim=c(self$options$supplot,0), ylab = "Log Likelihood")
            lines(c(g$spec,g$spec),c(self$options$supplot,0),lty=2) # add MLE as dashed line
            segments(g$begL2, g$goalL, g$endL2, g$goalL, lwd = 1, col = "red")
            lines(c(self$options$nulspec,self$options$nulspec),c(self$options$supplot,log(g$nul_spec_h)), lty=1, col = "black") # add H prob as black line
          }
        }
        TRUE
      },
      .plot1 = function(image1, ggtheme, ...) {
        
        
        plotData1 <- image1$state
        
        plot1 <- nomogrammer(Prevalence = plotData1$Prevalence,
                             Sens = plotData1$Sens,
                             Spec = plotData1$Spec,
                             Plr = plotData1$Plr,
                             Nlr = plotData1$Nlr,
                             Detail = TRUE,
                             NullLine = TRUE,
                             LabelSize = (18/5),
                             Verbose = TRUE
        )
        
        print(plot1)
        TRUE
        
        
      },
      
      #### Helper functions ----
      #      .cleanData = function() {
      .cleanData = function(B64 = FALSE) {
        
        data <- self$data
        
        rowVarName <- self$options$rows
        colVarName <- self$options$cols
        layerNames <- NULL
        countsName <- self$options$counts
        
        #        if ( ! is.null(rowVarName))
        #          data[[rowVarName]] <- as.factor(data[[rowVarName]])
        #        if ( ! is.null(colVarName))
        #          data[[colVarName]] <- as.factor(data[[colVarName]])
        #        for (layerName in layerNames)
        #          data[[layerName]] <- as.factor(data[[layerName]])
        #        if ( ! is.null(countsName))
        #          data[[countsName]] <- toNumeric(data[[countsName]])
        if ( ! is.null(rowVarName)) {
          rowVarNameNew <- ifelse(B64, jmvcore::toB64(rowVarName), rowVarName)
          data[[rowVarNameNew]] <- as.factor(data[[rowVarName]])
        }
        if ( ! is.null(colVarName)) {
          colVarNameNew <- ifelse(B64, jmvcore::toB64(colVarName), colVarName)
          data[[colVarNameNew]] <- as.factor(data[[colVarName]])
        }
        for (layerName in layerNames) {
          layerNameNew <- ifelse(B64, jmvcore::toB64(layerName), layerName)
          data[[layerNameNew]] <- as.factor(data[[layerName]])
        }
        if ( ! is.null(countsName)) {
          countsNameNew <- ifelse(B64, jmvcore::toB64(countsName), countsName)
          data[[countsNameNew]] <- jmvcore::toNumeric(data[[countsName]])
        } else if ( ! is.null(attr(data, "jmv-weights"))) {
          countsNameNew <- ifelse(B64, jmvcore::toB64(".COUNTS"), ".COUNTS")
          data[[countsNameNew]] = jmvcore::toNumeric(attr(data, "jmv-weights"))
        }
        
        
        return(data)
      },
      .matrices=function(data) {
        
        matrices <- list()
        
        rowVarName <- self$options$rows
        colVarName <- self$options$cols
        layerNames <- NULL
        countsName <- self$options$counts
        
        if (length(layerNames) == 0) {
          
          subData <- jmvcore::select(data, c(rowVarName, colVarName))
          
          if (is.null(countsName))
            .COUNTS <- rep(1, nrow(subData))
          else
            .COUNTS <- jmvcore::toNumeric(data[[countsName]])
          
          matrices <- list(ftable(xtabs(.COUNTS ~ ., data=subData)))
          
        } else {
          
          layerData <- jmvcore::select(data, layerNames)
          dataList <- do.call(split, list(data, layerData))
          
          tables <- lapply(dataList, function(x) {
            
            xTemp <- jmvcore::select(x, c(rowVarName, colVarName))
            
            if (is.null(countsName))
              .COUNTS <- rep(1, nrow(xTemp))
            else
              .COUNTS <- jmvcore::toNumeric(x[[countsName]])
            
            ftable(xtabs(.COUNTS ~ ., data=xTemp))
          })
          
          rows <- private$.grid(data=data, incRows=FALSE)
          
          expand <- list()
          
          for (layerName in layerNames)
            expand[[layerName]] <- base::levels(data[[layerName]])
          
          tableNames <- rev(expand.grid(expand))
          
          matrices <- list()
          for (i in seq_along(rows[,1])) {
            
            indices <- c()
            for (j in seq_along(tableNames[,1])) {
              
              row <- as.character(unlist((rows[i,])))
              tableName <- as.character(unlist(tableNames[j,]))
              
              if (all(row == tableName | row == '.total'))
                indices <- c(indices, j)
            }
            
            matrices[[i]] <- Reduce("+", tables[indices])
          }
          
        }
        
        matrices
      },
      .grid=function(data, incRows=FALSE) {
        
        rowVarName <- self$options$rows
        layerNames <- NULL
        
        expand <- list()
        
        if (incRows) {
          if (is.null(rowVarName))
            expand[['.']] <- c('.', '. ', .('Total'))
          else
            expand[[rowVarName]] <- c(base::levels(data[[rowVarName]]), '.total')
        }
        
        for (layerName in layerNames)
          expand[[layerName]] <- c(base::levels(data[[layerName]]), '.total')
        
        rows <- rev(expand.grid(expand))
        
        rows
      },
      .sourcifyOption = function(option) {
        if (option$name %in% c('rows', 'cols', 'counts'))
          return('')
        super$.sourcifyOption(option)
      },
      .formula=function() {
        rhs <- list()
        if ( ! is.null(self$options$rows)) {
          rhs[[1]] <- self$options$rows
          if ( ! is.null(self$options$cols)) {
            rhs[[2]] <- self$options$cols
            rhs <- c(rhs, NULL)
          }
        }
        jmvcore:::composeFormula(self$options$counts, list(rhs))
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
        
        Sxl = list(s=st$S1, "H\u2080 of 1 ", "the observed <i>likelihood ratio</i>")                       
        stg1 <- private$.strength(Sxl)
        Sxl = list(s=st$S2, paste0("H\u2080 of ", self$options$nulsens), "the observed <i>sensitivity</i>")                       
        stg2 <- private$.strength(Sxl)
        Sxl = list(s=st$S3, paste0("H\u2080 of ", self$options$nulspec), "the observed <i>specificity</i>") 
        stg3 <- private$.strength(Sxl)
        
        Sxl = list(s=st$tg)                       
        sv <- private$.strength2(Sxl)
        
        stg8 <- paste0("For the observed <i>LR</i>", sv, ", that it was more different from the H\u2080 = 1 
                         than expected")
        if (2*st$tg > st$chi) {
          stg8 <- paste0("For the observed <i>LR</i>", sv, ", that it was closer to the H\u2080 = 1 than expected")
        }
        if(self$options$correction=="ob") { stg0 <- "<i>Using Occam's Bonus correction, the analysis shows that:</i>"
        } else if(self$options$correction=="aic") { stg0 <- "<i>Using AIC correction, the analysis shows that:</i>"
        } else {
          stg0 <- "<i>Using no correction, the analysis shows that:</i>"
        }
        stg1 <- paste0("The diagnostic <i>likelihood ratio</i> was ", round(st$S1,2), ", see Berkman et al (2015) reference for interpretation.")
        str = paste0("<br> <h2>Summarizing the evidential analysis</h2>", "<br>",
                     stg0, "<br>", stg2, "<br>", stg3, "<br>", stg1, 
                     "<p>
                       <i>The variance analysis shows that:</i><br>", 
                     stg8,
                     "<p>Give the <i>LR</i> and the observed <i>sensitivity</i> and <i>specificity</i>. The support intervals for the <i>sensitivity</i> 
                       and <i>specificity</i> can be given, along with their likelihood-based % confidence intervals (see Pritikin et al, 2017).
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
        str3 <- "For example, we could choose a meaningful <i>H</i>\u2080 value for the <i>sensitivity</i> 
          (default = 0.5), and determine the strength of evidence was for it. "
        
        str = paste0(str1, str3, "As data accumulates the strength of evidence for one hypothesis over another will tend 
                       to increase.")
        
        
        html$setContent(str)
        
      }
      
    )
)
