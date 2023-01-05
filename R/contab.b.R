
# This file is a generated template, your changes will not be overwritten

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
        
        int_text <- paste(rowVarName," \u2A2F ", colVarName)
        
        table <- self$results$cttma
        table$setTitle(.("Support: Marginal main effects and interaction analyses, against the Null model"))
        table$setNote('Note', "<i>S</i>c is <i>S</i> corrected for degrees of freedom using Edwards's Occam's bonus, see reference")
        table$setRow(rowNo=1, values=list(var= rowVarName))
        table$setRow(rowNo=2, values=list(var=colVarName))
        table$setRow(rowNo=3, values=list(var=int_text))
        table$setRow(rowNo=4, values=list(var="Total"))
        
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
        nRows <- length(rlevels)
        
        subData <- jmvcore::select(data, c(rowVarName, colVarName))
        .COUNTS <- jmvcore::toNumeric(data[[countsName]])
        
        tabt <- xtabs(.COUNTS ~ ., data=subData)

#        tabt <- private$.matrices(data)
        
        # calculating the interaction support
        S2way <- 0
        suppressWarnings(lt <- try(chisq.test(tabt, correct=FALSE))) # ignore warning message

        tabt1=lt$observed
        for (i in 1:length(tabt)) {
          tabt1[i] <- lt$observed[i]
          if (lt$observed[i] < 1) tabt1[i]=1   # turn 0s into 1s for one table used for log
        }
        
        S2way <- sum( lt$observed * log(tabt1/lt$expected))
        dfi <- lt$parameter
        S2way_c <- S2way - (dfi-1)/2  # corrected for df

        # main marginal totals
        row_sum <- rowSums(tabt)
        col_sum <- colSums(tabt)
        grandtot <- sum(tabt)
        dfr <- length(row_sum)-1; dfc <- length(col_sum)-1; dft <- nRows*nCols-1
        RowMain <- sum(row_sum*log(row_sum))-grandtot*log(grandtot) + grandtot*log(length(row_sum))
        RowMain_c <- RowMain - (dfr-1)/2 # corrected for row df
        
        ColMain <- sum(col_sum*log(col_sum))-grandtot*log(grandtot) + grandtot*log(length(col_sum))
        ColMain_c <- ColMain - (dfc-1)/2 # corrected for column df
        
        exp_row <- grandtot/nRows # expected values
        exp_col <- grandtot/nCols
        exp_cell <- grandtot/(nRows*nCols)

        # Total S
        Tot_S <- sum(lt$observed*log(tabt1))-sum(lt$observed)*log(sum(lt$observed)/(nRows*nCols))
        
        # same as components added together (without correction for df)
        Tot_S_c <- Tot_S - (dft-1)/2 # corrected for column df
        
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
        table$setRow(rowNo=1, values=list(Value=exp_row, S=RowMain, Sc=RowMain_c, G=2*RowMain, df=dfr, p=gr_p))
        table$setRow(rowNo=2, values=list(Value=exp_col, S=ColMain, Sc=ColMain_c, G=2*ColMain, df=dfc, p=gc_p))
        table$setRow(rowNo=3, values=list(Value="", S=S2way, Sc=S2way_c, G=2*S2way, df=dfi, p=gi_p))
        table$setRow(rowNo=4, values=list(Value=exp_cell, S=Tot_S, Sc=Tot_S_c, G=2*Tot_S, df=dft, p=gt_p))

        table <- self$results$ctt3
        table$setNote('Note', "Unlike the \u03C7\u00B2 statistic, a large <i>S</i> value indicates 
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
  
  data <- private$.cleanData()
  data <- na.omit(data)
  
  if (! is.null(countsName)){
    untable <- function (df, counts) df[rep(1:nrow(df), counts), ]
    data <- untable(data[, c(rowVarName, colVarName)], counts=data[, countsName])
  }
  
  formula <- jmvcore::composeFormula(NULL, c(rowVarName, colVarName))
  counts <- xtabs(formula, data)
  d <- dim(counts)
  
  expand <- list()
  for (i in c(rowVarName, colVarName))
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

      .matrices=function(data) {
        
        matrices <- list()
        
        rowVarName <- self$options$rows
        colVarName <- self$options$cols
#        layerNames <- self$options$layers
        countsName <- self$options$counts
        

          subData <- jmvcore::select(data, c(rowVarName, colVarName))
          
          if (is.null(countsName))
            .COUNTS <- rep(1, nrow(subData))
          else
            .COUNTS <- jmvcore::toNumeric(data[[countsName]])
          
          matrices <- list(ftable(xtabs(.COUNTS ~ ., data=subData)))
          
        matrices
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
          
          str = paste0("<br> <h2>Summarizing the evidential analysis</h2>
                               <br>  <i>Using the Akaike corrected values, the main analysis shows that:</i> <br>", 
                               stg1, "<br>", stg2, "<br>", stg3, "<br>", stg4, "<br>Usually only the 
                                interaction term will be of interest. <p>
                                <i>The Variance analysis (not necessarily required) shows that:</i><br>", 
                                stg5,"<br>", stg6, "<br>", stg7,
                               "<p>Give the observed frequencies, and the available <i>p</i> values 
                               for the <i>G</i> test (likelihood ratio test) may also be supplied to allow 
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
