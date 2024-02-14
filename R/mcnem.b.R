mcnemClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "mcnemClass",
    inherit = mcnemBase,
    private = list(
      
      .init = function() {
        
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
        
        if(self$options$correction=="ob") { notext <- "S uses Occam's Bonus correction for parameters (Param). "
        } else if(self$options$correction=="aic") { notext <- "S uses AIC correction for parameters (Param). "
        } else if(self$options$correction=="aicsm") { notext <- "S uses AIC small sample correction for parameters (Param). "
        } else {
          notext <- "S uses no correction for parameters (Param). "
        }
        
        table <- self$results$ctt
        table$setNote('Note', notext)
        table$setRow(rowNo=1, values=list(var= "H\u2080 vs observed Odds"))
        table$setRow(rowNo=2, values=list(var="H\u2090 vs observed Odds"))
        table$setRow(rowNo=3, values=list(var="H\u2090 vs H\u2080"))
        
        table <- self$results$ctt2
        siWidthTitle <- jmvcore::format(.('Interval'))
        table$getColumn('Lower')$setSuperTitle(siWidthTitle)
        table$getColumn('Upper')$setSuperTitle(siWidthTitle)
        table$setRow(rowNo=1, values=list(Interval="Support for Odds"))
        table$setRow(rowNo=2, values=list(Interval="Likelihood-based for Odds"))
        table$setRow(rowNo=3, values=list(Interval="Likelihood-based for Logodds"))
        table$addFootnote(rowNo=2, col="Interval", "See reference Pritikin et al (2017) likelihood-based intervals 
                          are more accurate and are parameterization-invariant compared to conventional 
                          confidence intervals")
        
      },
      
      .run = function() {
        
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
        
        rowVar <- data[[rowVarName]]
        colVar <- data[[colVarName]]
        
        freqs <- self$results$freqs
        
        result <- base::table(rowVar, colVar)
        
        
        freqRowNo <- 1
        othRowNo <- 1
        
        
        mats <- private$.matrices(data)
        
        nRows  <- base::nlevels(data[[rowVarName]])
        nCols  <- base::nlevels(data[[colVarName]])
        nCells <- nRows * nCols
        
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
          
          #        tabt <- as.matrix(self$data[[self$options$counts]])
          #        tab <- as.table(rbind(c(tabt[1],tabt[2]),c(tabt[3],tabt[4])))
          tab <- mat
          
          a <- tab[1] #top left
          b <- tab[2] #bottom left
          c <- tab[3] #top right
          d <- tab[4] #bottom right
          
          osp <- c(b,c)
          
          ospT <- osp[1] + osp[2]
          Exos <- ospT/2
          odds <- osp[1]/osp[2]
          lodds <- log(odds)
          
          toler=0.0001
          
          ao = osp[1]; bo = osp[2]
          
          f <- function(x, ao, bo, ospT, goal) {
            (-sum(ao*log(ao/x), bo*log(bo/(ospT-x)))-goal)^2
          }
          
          # likelihood-based % confidence intervals
          goal = -qchisq(self$options$ciWidth/100,1)/2
          xmin1 <- optimize(f, c(0, ao), tol = toler, ao, bo, ospT, goal)
          xmin2 <- optimize(f, c(ao, ospT), tol = toler, ao, bo, ospT, goal)
          beg <- xmin1$minimum/(ospT-xmin1$minimum)
          end <- xmin2$minimum/(ospT-xmin2$minimum)
          begLo <- log(beg)
          endLo <- log(end)
          
          # likelihood interval
          goalL <- -self$options$lint
          xmin1L <- optimize(f, c(0, ao), tol = toler, ao, bo, ospT, goalL)
          xmin2L <- optimize(f, c(ao, ospT), tol = toler, ao, bo, ospT, goalL)
          begL <- xmin1L$minimum/(ospT-xmin1L$minimum)
          endL <- xmin2L$minimum/(ospT-xmin2L$minimum)
          
          lintlev <- toString(self$options$lint); conflev <- paste0(self$options$ciWidth,"%")
          
          # x axis limits
          goalx <- self$options$supplot   # with e^-10 we get x values for when curve is down to 0.00004539
          suppressWarnings(xmin1x <- optimize(f, c(0, ao), tol = toler, ao, bo, ospT, goalx))
          suppressWarnings(xmin2x <- optimize(f, c(ao, ospT), tol = toler, ao, bo, ospT, goalx))
          xmin <- xmin1x$minimum/(ospT-xmin1x$minimum)
          xmax <- xmin2x$minimum/(ospT-xmin2x$minimum)
          
          
          #        # to determine x axis space for plot
          #        dif <- odds-begL
          #        lolim <- odds - 3*dif; hilim <- odds + 4*dif
          #        if (odds < 1 ) { hilim <- odds + 6*dif}
          #        if (lolim < 0) {lolim <- 0}
          
          # to determine height of self$options$alt on likelihood function
          goal <- self$options$alt
          g <- function(x,ospT,goal) {
            (x/(ospT-x)-goal)^2
          }
          exa <- optimize(g, c(0, ospT), tol = toler, ospT, goal)
          xa <- unname(unlist(exa[1]))
          xah <- exp(-sum(osp[1]*log(osp[1]/xa), osp[2]*log(osp[2]/(ospT-xa))))
          
          # and likelihood for 1 (null value)
          goal <- self$options$nul
          exan <- optimize(g, c(0, ospT), tol = toler, ospT, goal)
          xa <- unname(unlist(exan[1]))
          nullh <- exp(-sum(osp[1]*log(osp[1]/xa), osp[2]*log(osp[2]/(ospT-xa))))
          
          # Correction
          Ac <- function(c,k1,k2) { 
            if(c=="nc") { 0
            } else if(c=="ob") { 0.5*(k2-k1) 
            } else { 1*(k2-k1)
            } 
          }
          
          Smc <- log(nullh) + Ac(self$options$correction,1,2)
          
          #         Smc <- -(osp[1]*log(osp[1]/Exos) + osp[2]*log(osp[2]/Exos)) #this for null=1
          
          # variance analysis and chi-square
          wocor <- try(stats::mcnemar.test(result, correct=FALSE), silent=TRUE)
          wcor  <- try(stats::mcnemar.test(result, correct=TRUE), silent=TRUE)
          df=1
          toogood <- df/2*(log(df/wocor$statistic)) - (df - wocor$statistic)/2
          toogoodc <- df/2*(log(df/wcor$statistic)) - (df - wcor$statistic)/2
          mvp <- wocor$p.value; mvpc <- wcor$p.value
          
          xwocor <- try(stats::chisq.test(tab, correct=FALSE), silent=TRUE) # for cross-tabulation
          xwcor  <- try(stats::chisq.test(tab, correct=TRUE), silent=TRUE)
          xtoogood <- df/2*(log(df/xwocor$statistic)) - (df - xwocor$statistic)/2
          xtoogoodc <- df/2*(log(df/xwcor$statistic)) - (df - xwcor$statistic)/2
          xmvp <- xwocor$p.value; xmvpc <- xwcor$p.value
          
          # support for alt. H
          Salt <- log(xah) + Ac(self$options$correction,1,1)
          SexOR_null <- Salt - Smc
          
          gn <- 2*abs(Smc) # likelihood ratio statistic
          gn_p <- 1-pchisq(gn,1)
          ga <- 2*abs(Salt)
          ga_p <- 1-pchisq(ga,1)
          gan <- 2*abs(SexOR_null)
          gan_p <- 1-pchisq(gan,1)
          
          
          table <- self$results$ctt
          table$setRow(rowNo=1, values=list(Value=self$options$nul, ordiff= self$options$nul-odds, 
                                            S=Smc, Param=paste0(c(1,2), collapse = ', '), G=gn, df=1, p=gn_p))
          table$setRow(rowNo=2, values=list(Value=self$options$alt, ordiff= self$options$alt-odds, 
                                            S=Salt, Param=paste0(c(2,2), collapse = ', '), G=ga, df=1, p=ga_p))
          table$setRow(rowNo=3, values=list(Value="", ordiff= self$options$alt-self$options$nul, 
                                            S=SexOR_null, Param=paste0(c(2,1), collapse = ', '), G=gan, df=1, p=gan_p))
          
          table <- self$results$ctt2
          siWidthTitle <- jmvcore::format(.('Interval'))
          table$getColumn('Lower')$setSuperTitle(siWidthTitle)
          table$getColumn('Upper')$setSuperTitle(siWidthTitle)
          table$setRow(rowNo=1, values=list(Level=lintlev, Odds = odds, 
                                            Lower=begL, Upper=endL))
          table$setRow(rowNo=2, values=list(Level=conflev, Odds = odds, 
                                            Lower=beg, Upper=end))
          table$setRow(rowNo=3, values=list(Level=conflev, Odds = lodds, 
                                            Lower=begLo, Upper=endLo))
          
          table <- self$results$ctt3
          table$setNote('Note', "Unlike the \u03C7\u00B2 statistic, a large \U1D446 value indicates 
          that the proportions are either more different or too similar compared with those expected") 
          table$setRow(rowNo=1, values=list(var= "For Odds = 1", Sv=toogood, X2=wocor$statistic, dfv=df, 
                                            pv=mvp, pv1=1-mvp))
          table$setRow(rowNo=2, values=list(var= "For Odds with continuity correction", Sv=toogoodc, X2=wcor$statistic, dfv=df, 
                                            pv=mvpc, pv1=1-mvpc))
          table$setRow(rowNo=3, values=list(var= "For Odds Ratio = 1", Sv=xtoogood, X2=xwocor$statistic, dfv=df, 
                                            pv=xmvp, pv1=1-xmvp))
          table$setRow(rowNo=4, values=list(var= "For OR with continuity correction", Sv=xtoogoodc, X2=xwcor$statistic, dfv=df, 
                                            pv=xmvpc, pv1=1-xmvpc))
          
          # stats for summary        
          stats <- list(S1 = Smc,
                        S2 = Salt,
                        S3 = SexOR_null,
                        S4 = toogoodc,
                        S5 = xtoogoodc,
                        chi1 = wcor$statistic,
                        chi2 = xwcor$statistic)
          
          # Populate Explanation & table
          private$.populateSupportText(stats)
          private$.populateMoreSupportText()
          #
          
          
          if(isTRUE(self$options$varA)) { 
            
            table <- self$results$ctt3
            table$setVisible(TRUE)
            
          }
          
          g <- data.frame(odds=odds, ao, bo, ospT, goalL=goalL, nullh=nullh, 
                          xmin=xmin, xmax=xmax, xah=xah, begL=begL,endL=endL)
          imagec <- self$results$plotc
          imagec$setState(g)
          
          if(isTRUE(self$options$pll)) {
            
            plotc <- self$results$plotc
            plotc$setVisible(TRUE)
            
          }
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
        
        res <- 100            # resolution, increase for greater resolution
        arrlen <- res*g$ospT-1
        xs <- 0; ys <- 0
        for (i in 1:arrlen) {     # arrays to plot likelihood vs OR
          dv <- i/res
          ys[i] <- exp(-sum(g$ao*log(g$ao/dv), g$bo*log(g$bo/(g$ospT-dv))))
          xs[i] <- dv/(g$ospT-dv)
        }
        
        #        # to determine x axis space for plot
        
        #        dif <- g$odds-g$begL
        #        lolim <- g$odds - 2*dif; hilim <- g$odds + 4*dif
        #        if (g$odds < 1 ) { hilim <- g$odds + 3*dif}
        #        if (lolim < 0) {lolim <- 0}
        
        # do the plot with lines
        if(self$options$plotype=="lplot") {
          plot <- plot(xs, ys, xlim=c(g$xmin,g$xmax),type="l", lwd = 1, xlab = "Odds", ylab = "Likelihood")
          lines(c(g$odds,g$odds),c(0,1),lty=2) # add MLE as dashed line
          segments(g$begL, exp(g$goalL), g$endL, exp(g$goalL), lwd = 1, col = "red")
          lines(c(self$options$nul,self$options$nul),c(0,g$nullh), lty=1, col = "black") # add H prob as black line
          if (!is.null(self$options$alt)) {
            lines(c(self$options$alt,self$options$alt), c(0,g$xah), lty=1, col = "blue") # add H prob as blue line
          }
        } else {
          plot <- plot(xs, log(ys), xlim=c(g$xmin,g$xmax),type="l", lwd = 1, xlab = "Odds", 
                       ylim=c(self$options$supplot,0), ylab = "Log Likelihood")
          lines(c(g$odds,g$odds),c(self$options$supplot,0),lty=2) # add MLE as dashed line
          segments(g$begL, g$goalL, g$endL, g$goalL, lwd = 1, col = "red")
          lines(c(self$options$nul,self$options$nul),c(self$options$supplot, log(g$nullh)), lty=1, col = "black") # add H prob as black line
          if (!is.null(self$options$alt)) {
            lines(c(self$options$alt,self$options$alt), c(self$options$supplot,log(g$xah)), lty=1, col = "blue") # add H prob as blue line
          }
        }
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
        
        Sxl = list(s=st$S1, "<i>H</i>\u2080", "the observed Odds")                       
        stg1 <- private$.strength(Sxl)
        Sxl = list(s=st$S2, "<i>H</i>\u2090", "the observed Odds")                       
        stg2 <- private$.strength(Sxl)
        Sxl = list(s=st$S3, "<i>H</i>\u2090", "<i>H</i>\u2080")  
        stg3 <- private$.strength(Sxl)
        
        Sxl = list(s=st$S4)                       
        sv <- private$.strength2(Sxl)
        stg4 <- paste0("For the observed Odds", sv, ", that it was more different from the <i>H</i>\u2080 = 1 
                         than expected")
        if (2*st$S4 > st$chi1) {
          stg8 <- paste0("For the observed Odds", sv, ", that it was closer to the <i>H</i>\u2080 = 1 than expected")
        }
        
        Sxl = list(s=st$S5)                       
        sv <- private$.strength2(Sxl)
        stg5 <- paste0("For the observed <i>OR</i>", sv, ", that it was more different from the <i>H</i>\u2080 = 1 
                         than expected")
        if (2*st$S5 > st$chi2) {
          stg5 <- paste0("For the observed <i>OR</i>", sv, ", that it was closer to the <i>H</i>\u2080 = 1 than expected")
        }
        
        if(self$options$correction=="ob") { stg0 <- "<i>Using Occam's Bonus correction, the main Odds analysis shows that:</i>"
        } else if(self$options$correction=="aic") { stg0 <- "<i>Using AIC correction, the main Odds analysis shows that:</i>"
        } else {
          stg0 <- "<i>Using no correction, the main Odds analysis shows that:</i>"
        }
        str = paste0("<br> <h2>Summarizing the evidential analysis</h2> <br>", 
                     stg0, "<br>", stg1, "<br>", stg2, "<br>", stg3, "<br>
                       <i>The variance analysis, using continuity corrected values, shows that:</i> <br>",
                     stg4, "<br>", stg5,"<br>",
                     "<p>Give the Odds and the observed frequencies. The available <i>p</i> values 
                               for the <i>G</i> test (likelihood ratio test) may also be supplied to allow 
                               comparison with a conventional analysis.</p> 
                               <p>There are no thresholds for <i>S</i> values, just guidelines on 
                               the strength of evidence for one hypothesis versus the other. They range from 
                               \u2212\u221E to +\u221E, with zero representing no evidence either way. 
                               Positive values are evidence for, while negative values are evidence against. 
                               The table below shows the interpretation of <i>S</i> values generally accepted 
                               in science. In contrast, UK law courts regard an <i>S</i> of 4 as 
                               moderate evidence and 8.6 as strong evidence! 
                               <i>S</i> values represent the weight of evidence, and are additive 
                               across independent data.</p>")
        
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
        
        str = paste0("<i>Support Intervals</i> 
          <br> The log likelihood ratio interval identifies a supported range of values which are consistent with the observed statistic. 
          In jeva it is denoted as <i>S</i>-<i>X</i>, where <i>X</i> can be any number between 1 and 100. The <i>S</i>-2 interval is 
          commonly used since it is numerically close to the 95% confidence interval. For the <i>S</i>-2 interval, it means that the values 
          within the interval have likelihood ratios in the range 0.135 to 7.38, corresponding to e\u207B\u00B2 to e\u00B2. 
          Simply put, within an <i>S</i>-2 interval, no likelihoods are more than 7.38 times different from each other. Similarly, for the 
          <i>S</i>-3 interval, likelihood ratios will range from 0.050 to 20.09, corresponding to e\u207B\u00B3 to e\u00B3, and no 
          likelihoods will be more than 20.09 times different from each other.
          <br> <i>Advantages of the Evidential Approach</i> 
          <br> The advantage of the evidential approach is that we 
                can select hypothesis values that reflect our research interests. For example, we could 
                choose a meaningful <i>H</i>\u2090 Odds to compare with a specified <i>H</i>\u2080 Odds (default = 1). 
                This is shown by the last line of the main Support table for Odds analyses. As data accumulates 
                the strength of evidence for one hypothesis over the other will tend to increase.")
        
        html$setContent(str)
        
      }
      
    )
)
