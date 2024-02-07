contabClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "contabClass",
    inherit = contabBase,
    private = list(
      .init = function() {
        
        private$.initSupportTab()
        
        rowVarName <- self$options$rows
        colVarName <- self$options$cols
        countsName <- self$options$counts
        
        freqs <- self$results$freqs
        
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
        }
        else {
          superTitle <- '.'
          levels <- c('.', '.')
        }
        
        hasSubRows <- sum(self$options$pcRow,
                          self$options$pcCol) > 0
        
        subNames  <- c('[count]', '[expected]', '[sred]', '[ssv]', '[pcRow]', '[pcCol]', '[pcTot]')
        subTitles <- c(.('Observed'), .('Expected'), .('St resid'), .('S value'), .('% within row'), .('% within column'), .('% of total'))
        visible   <- c('(obs)', '(exp)', '(sr)', '(ss)', '(pcRow)', '(pcCol)', '(pcTot)')
        types     <- c('integer', 'number', 'number', 'number', 'number', 'number', 'number')
        formats   <- c('', '', '', '', 'pc', 'pc', 'pc')
        
        # iterate over the sub rows
        
        for (j in seq_along(subNames)) {
          subName <- subNames[[j]]
          if (subName == '[count]')
            v <- '(obs && (exp || sr || ss || pcRow || pcCol || pcTot))'
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
        if (self$options$sr) {
          freqs$addColumn(
            name='.total[sr]',
            title=.('Total'),
            type='number')
        }
        if (self$options$ss) {
          freqs$addColumn(
            name='.total[ss]',
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
        
        table <- self$results$cttma
        table$setTitle(.("Support: Marginal main effects and interaction analyses, against the Null model"))
        table$setNote('Note', notext)
        table$setRow(rowNo=1, values=list(var= rowVarName))
        table$setRow(rowNo=2, values=list(var=colVarName))
        table$setRow(rowNo=3, values=list(var=int_text))
        table$setRow(rowNo=4, values=list(var="Total"))
        
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
            srValues <- (values-exp[rowNo,])/sqrt(exp[rowNo,])
            ssValues <- sign(srValues) * 0.5 * srValues^2
            
            values <- as.list(values)
            names(values) <- paste0(1:nCols, '[count]')
            values[['.total[count]']] <- rowTotal
            
            expValues <- exp[rowNo,]
            expValues <- as.list(expValues)
            names(expValues) <- paste0(1:nCols, '[expected]')
            expValues[['.total[exp]']] <- sum(exp[rowNo,])
            
            srValues <- as.list(srValues)
            names(srValues) <- paste0(1:nCols, '[sred]')
            #            srValues[['.total[sr]']] <- sum(exp[rowNo,])
            
            ssValues <- as.list(ssValues)
            names(ssValues) <- paste0(1:nCols, '[ssv]')
            
            pcRow <- as.list(pcRow)
            names(pcRow) <- paste0(1:nCols, '[pcRow]')
            pcRow[['.total[pcRow]']] <- 1
            
            pcCol <- as.list(mat[rowNo,] / colTotals)
            names(pcCol) <- paste0(1:nCols, '[pcCol]')
            pcCol[['.total[pcCol]']] <- unname(rowTotals[rowNo] / total)
            
            pcTot <- as.list(mat[rowNo,] / total)
            names(pcTot) <- paste0(1:nCols, '[pcTot]')
            pcTot[['.total[pcTot]']] <- sum(mat[rowNo,] / total)
            
            values <- c(values, expValues, srValues, ssValues, pcRow, pcCol, pcTot)
            
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
          
          srValues <- apply(mat, 2, sum)
          srValues <- as.list(srValues)
          #          names(srValues) <- paste0(1:nCols, '[sred]')
          
          ssValues <- apply(mat, 2, sum)
          ssValues <- as.list(ssValues)
          
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
          
          values <- c(values, expValues, srValues, ssValues, pcRow, pcCol, pcTot)
          
          freqs$setRow(rowNo=freqRowNo, values=values)
          freqRowNo <- freqRowNo + 1
          
          tabt <- mat
          
          # calculating the interaction support
          S2way <- 0
          suppressWarnings(lt <- try(chisq.test(tabt, correct=FALSE))) # ignore warning message
          
          tabt1=lt$observed
          for (i in 1:length(tabt)) {
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
          
          S2way <- sum(lt$observed * log(tabt1/lt$expected))
          dfi <- lt$parameter
          S2way_c <- S2way - Ac(self$options$correction,1,dfi+1)  # corrected for df
          
          # main marginal totals
          row_sum <- rowSums(tabt)
          col_sum <- colSums(tabt)
          
          # do not allow 0 marginal totals
          for (i in 1:length(row_sum)) {
            if (row_sum[i] < 1) jmvcore::reject(.("Margin '{var}' has 0 total"), code='', var=rowVarName)
          }
          for (i in 1:length(col_sum)) {
            if (col_sum[i] < 1) jmvcore::reject(.("Margin '{var}' has 0 total"), code='', var=colVarName)
          }
          
          # Correction
          Ac <- function(c,k1,k2) { 
            if(c=="nc") { 0
            } else if(c=="ob") { 0.5*(k2-k1) 
            } else { 1*(k2-k1)
            } 
          }
          
          grandtot <- sum(tabt)
          dfr <- length(row_sum)-1; dfc <- length(col_sum)-1; dft <- nRows*nCols-1
          RowMain <- sum(row_sum*log(row_sum))-grandtot*log(grandtot) + grandtot*log(length(row_sum))
          RowMain_c <- RowMain - Ac(self$options$correction,1,dfr+1) # corrected for row df
          
          ColMain <- sum(col_sum*log(col_sum))-grandtot*log(grandtot) + grandtot*log(length(col_sum))
          ColMain_c <- ColMain - Ac(self$options$correction,1,dfc+1) # corrected for column df
          
          exp_row <- grandtot/nRows # expected values
          exp_col <- grandtot/nCols
          exp_cell <- grandtot/(nRows*nCols)
          
          # Total S
          Tot_S <- sum(lt$observed*log(tabt1))-sum(lt$observed)*log(sum(lt$observed)/(nRows*nCols))
          
          # same as components added together (without correction for df)
          Tot_S_c <- Tot_S - Ac(self$options$correction,1,dft+1) # corrected for column df
          
          toogood <- dfi/2*(log(dfi/lt$statistic)) - (dfi - lt$statistic)/2
          suppressWarnings(ltr <- chisq.test(row_sum, correct=FALSE)) # ignore warning message
          toogoodr <- dfr/2*(log(dfr/ltr$statistic)) - (dfr - ltr$statistic)/2
          suppressWarnings(ltc <- chisq.test(col_sum, correct=FALSE)) # ignore warning message
          toogoodc <- dfc/2*(log(dfc/ltc$statistic)) - (dfc - ltc$statistic)/2
          
          # evidence for trend
          #        trX <- NULL
          #        tr <- NULL
          #        trX$p.value <- NULL
          #        if (length(col_sum)>=3) {
          #          table.pos <- tabt[1:length(col_sum)]
          #          trX <- prop.trend.test(tabt[1,], col_sum)
          #          tr <- unname(trX$statistic)/2      # S for trend
          #        }
          
          gt_p <- 1-pchisq(2*Tot_S,dft)
          gr_p <- 1-pchisq(2*RowMain,dfr)
          gc_p <- 1-pchisq(2*ColMain,dfc)
          gi_p <- 1-pchisq(2*S2way,dfi)
          
          int_text <- paste(rowVarName," \u2A2F ", colVarName)
          
          table <- self$results$cttma
          table$setRow(rowNo=1, values=list(Value=exp_row, S=RowMain_c, 
                                            Param=paste0(c(1,dfr+1), collapse = ', '), G=2*RowMain, df=dfr, p=gr_p))
          table$setRow(rowNo=2, values=list(Value=exp_col, S=ColMain_c, 
                                            Param=paste0(c(1,dfc+1), collapse = ', '), G=2*ColMain, df=dfc, p=gc_p))
          table$setRow(rowNo=3, values=list(Value="", S=S2way_c, 
                                            Param=paste0(c(1,dfi+1), collapse = ', '), G=2*S2way, df=dfi, p=gi_p))
          table$setRow(rowNo=4, values=list(Value=exp_cell, S=Tot_S_c, 
                                            Param=paste0(c(1,dft+1), collapse = ', '), G=2*Tot_S, df=dft, p=gt_p))
          
          table <- self$results$ctt3
          table$setNote('Note', "Unlike the \u03C7\u00B2 statistic, a large S value indicates 
          that the proportions are either more different or too similar compared with those expected") 
          table$setRow(rowNo=1, values=list(var= rowVarName, Sv=toogoodr, X2=ltr$statistic, dfv=dfr, 
                                            pv=ltr$p.value, pv1=1-ltr$p.value))
          table$setRow(rowNo=2, values=list(var= colVarName, Sv=toogoodc, X2=ltc$statistic, dfv=dfc, 
                                            pv=ltc$p.value, pv1=1-ltc$p.value))
          table$setRow(rowNo=3, values=list(var= int_text, Sv=toogood, X2=lt$statistic, dfv=dfi, 
                                            pv=lt$p.value, pv1=1-lt$p.value))
          
          # stats for summary        
          stats <- list(S1 = RowMain_c,
                        S2 = ColMain_c,
                        S3 = S2way_c,
                        S4 = Tot_S_c,
                        tgr = toogoodr,
                        tgc = toogoodc,
                        tg = toogood,
                        chi_r = ltr$statistic,
                        chi_c = ltc$statistic,
                        chi = lt$statistic)
          
          # Populate Explanation & table
          private$.populateSupportText(stats)
          private$.populateMoreSupportText()
          #
          
          
          if(isTRUE(self$options$varA)) { 
            
            table <- self$results$ctt3
            table$setVisible(TRUE)
            
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
          props <- counts
          
          if (self$options$yaxisPc == "column_pc") {
            pctVarName <- colVarName
          } else if (self$options$yaxisPc == "row_pc") {
            pctVarName <- rowVarName
          } else { # total
            pctVarName <- NULL
          }
          
          props <- proportions(counts, pctVarName)
          
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
        
        p <- p + ggtheme
        
        return(p)
      },      
      
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
        
        Sxl = list(s=st$S1, rowVarName, "the Null model")                       
        stg1 <- private$.strength(Sxl)
        Sxl = list(s=st$S2, colVarName, "the Null model")                       
        stg2 <- private$.strength(Sxl)
        Sxl = list(s=st$S3, int_text, "the Null model")  
        stg3 <- private$.strength(Sxl)
        Sxl = list(s=st$S4, "Total components", "the Null model")  
        stg4 <- private$.strength(Sxl)
        
        Sxl = list(s=st$tgr)                       
        svr <- private$.strength2(Sxl)
        Sxl = list(s=st$tgc)                       
        svc <- private$.strength2(Sxl)
        Sxl = list(s=st$tg)                   
        sv <- private$.strength2(Sxl)
        
        stg5 <- paste0("For ", rowVarName, svr, ", that the 
                    observed frequencies were more different than the <i>H</i>\u2080 frequencies")
        if (2*st$tgr > st$chi_r) {
          stg5 <- paste0("For ", rowVarName, svr, ", that the 
                    observed frequencies were too close to the <i>H</i>\u2080 frequencies")
        }
        
        stg6 <- paste0("For ", colVarName, svc, ", that the 
                    observed frequencies were more different than the <i>H</i>\u2080 frequencies")
        if (2*st$tgc > st$chi_c) {
          stg6 <- paste0("For ", colVarName, svc, ", that the 
                    observed frequencies were too close to the <i>H</i>\u2080 frequencies")
        }
        stg7 <- paste0("For ", int_text, sv, ", that the 
                    observed frequencies were more different than the <i>H</i>\u2080 frequencies")
        if (2*st$tg > st$chi) {
          stg7 <- paste0("For ", int_text, sv, ", that the 
                    observed frequencies were too close to the <i>H</i>\u2080 frequencies")
        }
        
        if(self$options$correction=="ob") { stg0 <- "<i>Using Occam's Bonus correction, the analysis shows that:</i>"
        } else if(self$options$correction=="aic") { stg0 <- "<i>Using AIC correction, the analysis shows that:</i>"
        } else {
          stg0 <- "<i>Using no correction, the analysis shows that:</i>"
        }
        str = paste0("<br> <h2>Summarizing the evidential analysis</h2>", "<br>",
                     stg0, "<br>", stg1, "<br>", stg2, "<br>", stg3, "<br>", stg4, "<br>Usually only the 
                    interaction term will be of interest. <p>
                    <i>The Variance analysis (not necessarily required) shows that:</i><br>", 
                     stg5,"<br>", stg6, "<br>", stg7,
                     "<p>Give the observed frequencies, and the available <i>p</i> values 
                   for the <i>G</i> test (likelihood ratio test) may also be supplied to allow 
                   comparison with a conventional analysis. The signed S values for individual cells can be given, which 
                   are half of the squared standardized residuals.
                   
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
        str = paste0(str1, "As data accumulates the strength of evidence for one hypothesis over another will tend 
                       to increase.")
        
        html$setContent(str)
        
      }
      
      
    )
)
