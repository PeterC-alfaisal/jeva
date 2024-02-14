cttClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "cttClass",
    inherit = cttBase,
    private=list(
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
        
        int_text <- paste(rowVarName," \u2A2F ", colVarName)
        
        if(self$options$correction=="ob") { notext <- "S uses Occam's Bonus correction for parameters (Param). "
        } else if(self$options$correction=="aic") { notext <- "S uses AIC correction for parameters (Param). "
        } else if(self$options$correction=="aicsm") { notext <- "S uses AIC small sample correction for parameters (Param). "
        } else {
          notext <- "S uses no correction for parameters (Param). "
        }
        
        table <- self$results$ctt
        table$setNote('Note', notext)
        table$setTitle(.("Support: Odds Ratio analyses"))
        table$setRow(rowNo=1, values=list(var= "H\u2080 vs odds ratio"))
        table$setRow(rowNo=2, values=list(var="H\u2090 vs odds ratio"))
        table$setRow(rowNo=3, values=list(var="H\u2090 vs H\u2080"))
        
        table <- self$results$cttma
        table$setTitle(.("Support: Marginal main effects and interaction analyses, against the Null model"))
        table$setNote('Note', paste(notext, "The interaction and OR (against 1) will have the same S value. Adding  
        the S values for the 3 components will precisely sum to the total S when no parameter correction is applied.")) 
        table$setRow(rowNo=1, values=list(var= rowVarName))
        table$setRow(rowNo=2, values=list(var= colVarName))
        table$setRow(rowNo=3, values=list(var= int_text))
        table$setRow(rowNo=4, values=list(var="Total"))
        
        table <- self$results$ctt2
        table$setRow(rowNo=1, values=list(Interval="Support"))
        table$setRow(rowNo=2, values=list(Interval="Likelihood-based"))
        table$addFootnote(rowNo=2, col="Interval", "See reference Pritikin et al (2017) such intervals 
                          are more accurate and are parameterization-invariant compared to conventional 
                          confidence intervals")
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
        
        if (any(data$.COUNTS < 0, na.rm=TRUE))
          jmvcore::reject(.('Counts may not be negative'))
        if (any(is.infinite(data$.COUNTS)))
          jmvcore::reject(.('Counts may not be infinite'))
        
        
        freqs <- self$results$freqs
        
        freqRowNo <- 1
        othRowNo <- 1
        
        
        mats <- private$.matrices(data)
        
        nRows  <- base::nlevels(data[[rowVarName]])
        nCols  <- base::nlevels(data[[colVarName]])
        nCells <- nRows * nCols
        
        ciWidth <- self$options$ciWidth / 100
        
        # set state for plot
        if (self$options$barplot) {
          countsDF <- as.data.frame(mats[[1]])
          expand <- list()
          for (v in c(rowVarName, colVarName))
            expand[[v]] <- base::levels(data[[v]])
          tab <- expand.grid(expand)
          tab$Counts <- countsDF$Freq
          self$results$barplot$setState(tab)
        }
        
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
          
          
          orv <- (a*d)/(b*c) # actual odds ratio from the contingency table
          f <- function(x,a,b,c,d,c1tot,r1tot,r2tot,goal) {
            (-sum(a*log(a/x), b*log(b/(c1tot-x)), c*log(c/(r1tot-x)),
                  d*log(d/(r2tot-c1tot+x)))-goal)^2
          }
          
          # likelihood-based % confidence interval
          
          arry <- numeric(maxmarg)   # finding endpoints for S and OR values
          for(x in 1:maxmarg) {
            arry[x] <- x*(r2tot-c1tot+x)/((c1tot-x)*(r1tot-x))
          }
          arry[!is.finite(arry)] <- 0
          ind <- which(arry > 0)
          aa <- split(ind, cumsum(c(0, diff(ind) > 1)))
          dvs <- min(aa$'0')-1
          dve <- max(aa$'0')+1
          
          goal = -qchisq(self$options$ciWidth/100,1)/2
          suppressWarnings(xmin1 <- optimize(f, c(0, a), tol = toler, a, b, c, d, c1tot, r1tot,
                                             r2tot, goal))
          suppressWarnings(xmin2 <- optimize(f, c(a, dve), tol = toler, a, b, c, d, c1tot, r1tot,
                                             r2tot, goal))
          beg <- xmin1$minimum*(r2tot-c1tot+xmin1$minimum)/((c1tot-xmin1$minimum)*(r1tot-xmin1$minimum))
          end <- xmin2$minimum*(r2tot-c1tot+xmin2$minimum)/((c1tot-xmin2$minimum)*(r1tot-xmin2$minimum))
          
          # likelihood interval
          goalL <- -self$options$lint
          suppressWarnings(xmin1L <- optimize(f, c(0, a), tol = toler, a, b, c, d, c1tot, r1tot, r2tot, goalL))
          suppressWarnings(xmin2L <- optimize(f, c(a, dve), tol = toler, a, b, c, d, c1tot, r1tot, r2tot, goalL))
          begL <- xmin1L$minimum*(r2tot-c1tot+xmin1L$minimum)/((c1tot-xmin1L$minimum)*(r1tot-xmin1L$minimum))
          endL <- xmin2L$minimum*(r2tot-c1tot+xmin2L$minimum)/((c1tot-xmin2L$minimum)*(r1tot-xmin2L$minimum))
          
          lintlev <- toString(self$options$lint); conflev <- paste0(self$options$ciWidth,"%")

          # x axis limits
          goalx <- self$options$supplot   # with e^-10 we get x values for when curve is down to 0.00004539
          suppressWarnings(xmin1x <- optimize(f, c(0, a), tol = toler, a, b, c, d, c1tot, r1tot, r2tot, goalx))
          suppressWarnings(xmin2x <- optimize(f, c(a, dve), tol = toler, a, b, c, d, c1tot, r1tot, r2tot, goalx))
          xmin <- xmin1x$minimum*(r2tot-c1tot+xmin1x$minimum)/((c1tot-xmin1x$minimum)*(r1tot-xmin1x$minimum))
          xmax <- xmin2x$minimum*(r2tot-c1tot+xmin2x$minimum)/((c1tot-xmin2x$minimum)*(r1tot-xmin2x$minimum))
          
          # to determine height of self$options$alt and nul on likelihood function
          goal <- self$options$alt
          h <- function(x,c1tot,r1tot,r2tot,goal) {
            (x*(r2tot-c1tot+x)/((c1tot-x)*(r1tot-x))-goal)^2
          }
          suppressWarnings(exa2 <- optimize(h, c(dvs, dve), tol = toler, c1tot, r1tot, r2tot, goal))
          xa <- unname(unlist(exa2[1]))
          xah <- exp(-sum(a*log(a/xa), b*log(b/(c1tot-xa)), c*log(c/(r1tot-xa)), d*log(d/(r2tot-c1tot+xa))))
          
          goal <- self$options$nul
          suppressWarnings(exa2 <- optimize(h, c(dvs, dve), tol = toler, c1tot, r1tot, r2tot, goal))
          xa <- unname(unlist(exa2[1]))
          nullh <- exp(-sum(a*log(a/xa), b*log(b/(c1tot-xa)), c*log(c/(r1tot-xa)), d*log(d/(r2tot-c1tot+xa))))
          
          S2way <- log(nullh) # check that this should be negative but same abs value as S for observed OR
          if(orv == 1) S2way <- 0
          
          # variance analysis
          toogood <- df/2*(log(df/chi.s)) - (df - chi.s)/2
          
          # marginal main effects analysis
          # main marginal totals
          row_sum <- rowSums(tab)
          col_sum <- colSums(tab)
          grandtot <- sum(tab)
          Srow <- sum(row_sum*log(row_sum))-grandtot*log(grandtot) + grandtot*log(length(row_sum))
          if(r1tot == r2tot) Srow <- 0
          Scol <- sum(col_sum*log(col_sum))-grandtot*log(grandtot) + grandtot*log(length(col_sum))
          if(c1tot == c2tot) Scol <- 0
          # interaction
          Sint <- sum(lt$observed * log(tabt1/lt$expected)) 
          # Grand total
          Sgt <- sum(lt$observed*log(tabt1))-sum(lt$observed)*log(sum(lt$observed)/4)
          
          # Sums
          exp_row <- (r1tot+r2tot)/2
          exp_col <- (c1tot+c2tot)/2
          exp_int <- grandtot/4
          
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
          gt_p <- 1-pchisq(2*Sgt,3)
          gr_p <- 1-pchisq(2*Srow,1)
          gc_p <- 1-pchisq(2*Scol,1)
          gi_p <- 1-pchisq(2*Sint,1)
          
          table <- self$results$ctt
          table$setRow(rowNo=1, values=list(Value=self$options$nul, ordiff= self$options$nul-orv, 
                      S=S2way + Ac(self$options$correction,1,2), Param=paste0(c(1,2), collapse = ', '), 
                      G=gn, df=df, p=gn_p))
          table$setRow(rowNo=2, values=list(Value=self$options$alt, ordiff= self$options$alt-orv, 
                      S=Salt + Ac(self$options$correction,2,2), Param=paste0(c(2,2), collapse = ', '), 
                      G=ga, df=df, p=ga_p))
          table$setRow(rowNo=3, values=list(Value="", ordiff= self$options$alt-self$options$nul, 
                      S=SexOR_null + Ac(self$options$correction,2,1), Param=paste0(c(2,1), collapse = ', '), 
                      G=gan, df=df, p=gan_p))
          
          table <- self$results$cttma
          table$setRow(rowNo=1, values=list(Value=exp_row, S=Srow + Ac(self$options$correction,2,1), 
                      G=2*Srow, Param=paste0(c(2,1), collapse = ', '),df=as.integer(df), p=gr_p))
          table$setRow(rowNo=2, values=list(Value=exp_col, S=Scol + Ac(self$options$correction,2,1),
                      G=2*Scol, Param=paste0(c(2,1), collapse = ', '), df=as.integer(df), p=gc_p))
          table$setRow(rowNo=3, values=list(Value="", S=Sint + Ac(self$options$correction,2,1), 
                      G=2*Sint, Param=paste0(c(2,1), collapse = ', '), df=as.integer(df), p=gi_p))
          table$setRow(rowNo=4, values=list(Value=exp_int, S=Sgt + Ac(self$options$correction,4,1), 
                      G=2*Sgt, Param=paste0(c(4,1), collapse = ', '), df=as.integer(3), p=gt_p))
          table <- self$results$ctt2
          table$setRow(rowNo=1, values=list(Level=lintlev, OR = orv, Lower=begL, Upper=endL))
          table$setRow(rowNo=2, values=list(Level=conflev, OR = orv, Lower=beg, Upper=end))
          if (HAc)
            table$addFootnote(rowNo=1, col="OR", "Haldane-Anscombe correction applied")      
          
          table <- self$results$ctt3
          table$setNote('Note', "Unlike the \u03C7\u00B2 statistic, a large S value indicates 
          that the proportions are either more different or too similar compared with those expected") 
          if (isTRUE(self$options$cc))
            table$setNote('Note', "Continuity correction applied. Unlike the \u03C7\u00B2 statistic, a large S value indicates 
          that the proportions are either more different or too similar compared with those expected")
          table$setRow(rowNo=1, values=list(var= "For OR = 1", Sv=toogood, X2=chi.s, dfv=df, 
                                            pv=lt$p.value, pv1=1-lt$p.value))
          
          # stats for summary        
          stats <- list(S1 = S2way+ Ac(self$options$correction,1,2),
                        S2 = Salt + Ac(self$options$correction,2,2),
                        S3 = SexOR_null + Ac(self$options$correction,2,1),
                        S4 = Srow + Ac(self$options$correction,2,1),
                        S5 = Scol + Ac(self$options$correction,2,1),
                        S7 = Sgt + Ac(self$options$correction,4,1),
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
          
          g <- data.frame(orv=orv, a=a, b=b, c=c, d=d, dvs=dvs,
                          r1tot=r1tot, r2tot=r2tot, c1tot=c1tot, c2tot=c2tot, 
                          nullh=nullh, xah=xah, goalL=goalL, begL=begL,endL=endL, xmin=xmin, xmax=xmax)
          imagec <- self$results$plotc
          imagec$setState(g)
          
          if(isTRUE(self$options$pll)) {
            
            plotc <- self$results$plotc
            plotc$setVisible(TRUE)
            
          }
          
          
          ########################################################
          
        }
      },
      
      #### Plot functions ----
      .initBarPlot = function() {
        image <- self$results$get('barplot')
        
        width <- 450
        height <- 400
        
        image$setSize(width * 2, height)
      },
      .barPlot = function(image, ggtheme, theme, ...) {
        
        if (! self$options$barplot)
          return()
        
        rowVarName <- self$options$rows
        colVarName <- self$options$cols
        countsName <- self$options$counts
        
        if (is.null(rowVarName) || is.null(colVarName))
          return()
        
        tab <- image$state
        
        data <- private$.cleanData()
        data <- na.omit(data)
        
        if (self$options$yaxis == "ypc") { # percentages

          if (self$options$yaxisPc == "column_pc") {
            pctVarName <- colVarName
            pctTotals <- tapply(tab$Counts, tab[colVarName], sum)
            props <- tab$Counts / pctTotals[tab[[colVarName]]]
          } else if (self$options$yaxisPc == "row_pc") {
            pctVarName <- rowVarName
            pctTotals <- tapply(tab$Counts, tab[rowVarName], sum)
            props <- tab$Counts / pctTotals[tab[[rowVarName]]]
          } else { # total
            pctVarName <- NULL
            props <- tab$Counts / sum(tab$Counts)
          }
          
          tab$Percentages <- props * 100
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
        
        p <- p + ggtheme
        
        return(p)
      },      
      
      .plotc=function(imagec, ...) {
        
        g <- imagec$state
        
        res <- 100    # resolution, increase for greater resolution
        minmarg <- min(g$r1tot,g$r2tot,g$c1tot,g$c2tot)
        arrlen <- res*minmarg-1
        xs <- 0; ys <- 0
        for (i in 1:arrlen) {     # arrays to plot likelihood vs OR
          dv <- i/res+g$dvs
          ys[i] <- exp(-sum(g$a*log(g$a/dv), g$b*log(g$b/(g$c1tot-dv)),
                            g$c*log(g$c/(g$r1tot-dv)), g$d*log(g$d/(g$r2tot-g$c1tot+dv))))
          xs[i] <- dv*(g$r2tot-g$c1tot+dv)/((g$c1tot-dv)*(g$r1tot-dv))
        }
        
        # to determine x axis space for plot
        seor <- sqrt(1/g$a+1/g$b+1/g$c+1/g$d)
        lolim <- exp(log(g$orv)-3*seor); hilim <- exp(log(g$orv)+3*seor)
        if (lolim < 0) {lolim <- 0}
        
        #-sum(a*log(a/x), b*log(b/(c1tot-x)), c*log(c/(r1tot-x)),
        # d*log(d/(r2tot-c1tot+x)))
        
        # do the plot with lines
        if(self$options$plotype=="lplot") {
          plot <- plot(xs, ys, xlim=c(lolim,hilim),type="l", lwd = 1, xlab = "Odds Ratio", ylab = "Likelihood")        
          lines(c(g$orv,g$orv),c(0,1),lty=2) # add MLE as dashed line
        segments(g$begL, exp(g$goalL), g$endL, exp(g$goalL), lwd = 1, col = "red")
        lines(c(self$options$nul,self$options$nul),c(0,g$nullh), lty=1, col = "black") # add H prob as black line
        lines(c(self$options$alt,self$options$alt), c(0,g$xah), lty=1, col = "blue") # add H prob as blue line
        } else {
          plot <- plot(xs, log(ys), xlim=c(g$xmin,g$xmax),type="l", lwd = 1, xlab = "Odds Ratio", 
                ylim=c(self$options$supplot,0), ylab = "Log Likelihood")
          lines(c(g$orv,g$orv),c(self$options$supplot,0),lty=2) # add MLE as dashed line
          segments(g$begL, g$goalL, g$endL, g$goalL, lwd = 1, col = "red")
          lines(c(self$options$nul,self$options$nul),c(self$options$supplot,log(g$nullh)), lty=1, col = "black") # add H prob as black line
          lines(c(self$options$alt,self$options$alt), c(self$options$supplot,log(g$xah)), lty=1, col = "blue") # add H prob as blue line
        }
          
#        print(plot)
        TRUE
      },
      #### Helper functions ----
      .cleanData = function() {
        
        data <- self$data
        
        rowVarName <- self$options$rows
        colVarName <- self$options$cols
        layerNames <- NULL
        countsName <- self$options$counts
        
        weights <- attr(data, 'jmv-weights')
        
        
        if ( ! is.null(rowVarName))
          data[[rowVarName]] <- as.factor(data[[rowVarName]])
        if ( ! is.null(colVarName))
          data[[colVarName]] <- as.factor(data[[colVarName]])
        for (layerName in layerNames)
          data[[layerName]] <- as.factor(data[[layerName]])
        
        if ( ! is.null(countsName)) {
          data$.COUNTS <- jmvcore::toNumeric(data[[countsName]])
        } else if ( ! is.null(weights)) {
          data$.COUNTS <- weights
        } else {
          data$.COUNTS <- rep(1, nrow(data))
        }
        
        data
      },
      .matrices=function(data) {
        
        matrices <- list()
        
        rowVarName <- self$options$rows
        colVarName <- self$options$cols
        layerNames <- NULL
        countsName <- self$options$counts
        
        if (length(layerNames) == 0) {
          
          subData <- jmvcore::select(data, c('.COUNTS', rowVarName, colVarName))
          
          matrices <- list(ftable(xtabs(.COUNTS ~ ., data=subData)))
          
        } else {
          
          layerData <- jmvcore::select(data, layerNames)
          dataList <- do.call(split, list(data, layerData))
          
          tables <- lapply(dataList, function(x) {
            
            xTemp <- jmvcore::select(x, c(rowVarName, colVarName))
            
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
        if(self$options$correction=="ob") { stg0 <- "<i>Using Occam's Bonus correction, the analysis shows that:</i>"
        } else if(self$options$correction=="aic") { stg0 <- "<i>Using AIC correction, the analysis shows that:</i>"
        } else {
          stg0 <- "<i>Using no correction, the analysis shows that:</i>"
        }
        if(self$options$correction=="ob") { stg0 <- "<i>Using Occam's Bonus correction, the analysis shows that:</i>"
        } else if(self$options$correction=="aic") { stg0 <- "<i>Using AIC correction, the analysis shows that:</i>"
        } else {
          stg0 <- "<i>Using no correction, the analysis shows that:</i>"
        }
        str = paste0("<br> <h2>Summarizing the evidential analysis</h2>", "<br>",
                     stg0, "<br>", stg1, "<br>", stg2, "<br>", stg3, 
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
