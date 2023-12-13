
# This file is a generated template, your changes will not be overwritten

lrm1waovClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "lrm1waovClass",
    inherit = lrm1waovBase,
    private=list(
      #### Member variables ----
      .model = NA,
      .postHocRows = NA,
      emMeans = list(),
      
      #### Init + run functions ----
      .init=function() {
        
        private$.initRMTable()
        private$.initBSTable()
        private$.initSpericityTable()
        private$.initLeveneTable()
        private$.initEmm()
        private$.initEmmTable()
        private$.initGroupSummary()
        private$.initcvals()
        private$.initSupportTab()
        
        measures <- lapply(self$options$rmCells, function(x) x$measure)
        areNull  <- vapply(measures, is.null, FALSE, USE.NAMES=FALSE)
        
        if (length(self$options$cov) > 0)
          jmvcore::reject(.("No covariates allowed"), code='')
        
        if (any(areNull))
          self$setStatus('complete')
      },
      .run=function() {
        
        dataSelected <- ! sapply(lapply(self$options$rmCells, function(x) return(x$measure)), is.null)
        
        ready <- sum(dataSelected) == length(self$options$rmCells) && length(self$options$rmTerms) > 0
        
        if (ready) {
          
          private$.dataCheck()
          data <- private$.wideToLong()
          modelFormula <- private$.modelFormula()
          
          suppressMessages({
            suppressWarnings({
              
              result <- try(afex::aov_car(modelFormula, data, type=self$options$ss, factorize = FALSE), silent=TRUE)
              #             m1.lmer<-lmer(Dat~Grp+(1|Subject),cp,REML=F) #alternative
              #             m0.lmer<-lmer(Dat~1+(1|Subject),cp,REML=F)
              
            }) # suppressWarnings
          }) # suppressMessages
          
          if (isError(result))
            jmvcore::reject(extractErrorMessage(result), code='error')
          
          private$.populateEffectsTables(result)
          private$.populateSpericityTable(result)
          private$.populateLeveneTable()
          private$.prepareQQPlot(result)
          private$.prepareEmmPlots(result, data)
          private$.populateEmmTables()
          private$.populateGroupSummaryTable()
        }
      },
      
      #### Init tables/plots functions ----
      .initRMTable=function() {
        
        if(self$options$correction=="ob") { notext <- " S uses Occam's Bonus correction for parameters (Param). "
        } else if(self$options$correction=="aic") { notext <- " S uses AIC correction for parameters (Param). "
        } else if(self$options$correction=="aicsm") { notext <- " S uses AIC small sample correction for parameters (Param). "
        } else {
          notext <- " S uses no correction for parameters (Param). "
        }
        
        
        ssTypeNote <- .("Type {ssType} Sums of Squares. ")
        
        rmTable <- self$results$rmTable
        rmTable$setNote('Note', paste0(jmvcore::format(ssTypeNote, ssType=self$options$ss),notext))
        
        rm <- private$.rmTerms()
        rmTerms <- rm$terms
        rmSpacing <- rm$spacing
        
        if (length(rmTerms) > 0) {
          for (i in seq_along(rmTerms)) {
            if (rmTerms[i] == 'Residual') {
              key <- unlist(c(rmTerms[[i-1]],'.RES'))
              name <- .("Residual")
            } else {
              key <- unlist(rmTerms[[i]])
              name <- paste0("H\u2080  vs ",stringifyTerm(rmTerms[[i]]))
            }
            values <- list(
              `name[none]`=name,
              `name[GG]`=name,
              `name[HF]`=name
            )
            rmTable$addRow(rowKey=key, values)
          }
        } else {
          name <- '.'
          values <- list(
            `name[none]`=name,
            `name[GG]`=name,
            `name[HF]`=name
          )
          rmTable$addRow(rowKey='.', values)
          rmTable$addRow(rowKey='', list(name=.('Residual')))
        }
        
        for (i in seq_along(rmSpacing)) {
          if ( ! is.null(rmSpacing[[i]])) {
            if (rmSpacing[[i]] == 'both')
              rmTable$addFormat(rowNo=i, col=1, Cell.BEGIN_END_GROUP)
            else if (rmSpacing[[i]] == 'above')
              rmTable$addFormat(rowNo=i, col=1, Cell.BEGIN_GROUP)
            else if (rmSpacing[[i]] == 'below')
              rmTable$addFormat(rowNo=i, col=1, Cell.END_GROUP)
          }
        }
        
        table <- self$results$conts
        table$setNote('Note', notext)
        table$setRow(rowNo=1, values=list(var='H\u2080 versus Contrast 1' ))
        table$setRow(rowNo=2, values=list(var='H\u2080 versus Contrast 2'))
        table$setRow(rowNo=3, values=list(var='Contrast 1 versus Contrast 2'))
        
      },
      
      .initBSTable=function() {
        ssTypeNote <- .("Type {ssType} Sums of Squares. ")
        
        bsTable <- self$results$bsTable
        
        if(self$options$correction=="ob") { notext <- " S uses Occam's Bonus correction for parameters (Param). "
        } else if(self$options$correction=="aic") { notext <- " S uses AIC correction for parameters (Param). "
        } else if(self$options$correction=="aicsm") { notext <- " S uses AIC small sample correction for parameters (Param). "
        } else {
          notext <- " S uses no correction for parameters (Param). "
        }
        
        bsTable$setNote('Note', paste0(jmvcore::format(ssTypeNote, ssType=self$options$ss),notext))
        
        bsTerms <- private$.bsTerms()
        if (length(bsTerms) > 2)
          jmvcore::reject(.("Only one between subject factor allowed"), code='')
        
        if (length(bsTerms) > 0) {
          for (term in bsTerms) {
            if (length(term) == 1 && term == 'Residual') {
              name <- .(' ')
            } else {
              name <- paste0("H\u2080  vs ",stringifyTerm(term))
            }
            
            bsTable$addRow(rowKey=unlist(term), list(name=name))
          }
        } else {
          bsTable$addRow(rowKey='', list(name=.('Residual')))
        }
      },
      .initSpericityTable=function() {
        spherTable <- self$results$get('assump')$get('spherTable')
        for (term in self$options$rmTerms)
          spherTable$addRow(rowKey=term, list(name=stringifyTerm(term)))
      },
      .initLeveneTable=function() {
        leveneTable <- self$results$get('assump')$get('leveneTable')
        rmVars <- sapply(self$options$rmCells, function(x) return(x$measure))
        for (var in rmVars)
          leveneTable$addRow(rowKey=var, list(name=var))
      },
      .initEmm = function() {
        
        emMeans <- self$options$emMeans
        group <- self$results$emm
        
        for (j in seq_along(emMeans)) {
          
          emm <- emMeans[[j]]
          
          if ( ! is.null(emm)) {
            group$addItem(key=j)
            emmGroup <- group$get(key=j)
            emmGroup$setTitle(jmvcore::stringifyTerm(emm))
            
            image <- emmGroup$emmPlot
            size <- private$.emmPlotSize(emm)
            image$setSize(size[1], size[2])
          }
        }
      },
      .initEmmTable = function() {
        
        emMeans <- self$options$emMeans
        rmFactors <- self$options$rm
        
        rmNames <- sapply(rmFactors, function(x) return(x$label))
        rmLevels <- lapply(rmFactors, function(x) return(x$levels))
        
        group <- self$results$emm
        
        emMeansTableTitle <- .('Estimated Marginal Means - {term}')
        ciWidthTitle <- jmvcore::format(.('{ciWidth}% Confidence Interval'), ciWidth=self$options$ciWidthEmm)
        
        for (j in seq_along(emMeans)) {
          
          emm <- emMeans[[j]]
          
          if ( ! is.null(emm)) {
            
            emmGroup <- group$get(key=j)
            
            table <- emmGroup$emmTable
            table$setTitle(jmvcore::format(emMeansTableTitle, term=jmvcore::stringifyTerm(emm)))
            
            nLevels <- numeric(length(emm))
            for (k in rev(seq_along(emm))) {
              table$addColumn(name=emm[k], title=emm[k], type='text', combineBelow=TRUE)
              
              if (emm[k] %in% rmNames) {
                nLevels[k] <- length(rmLevels[[which(emm[k] == rmNames)]])
              } else {
                nLevels[k] <- length(levels(self$data[[ emm[k] ]]))
              }
            }
            
            table$addColumn(name='mean', title=.('Mean'), type='number')
            table$addColumn(name='se', title='SE', type='number')
            table$addColumn(name='lower', title='Lower', type='number', superTitle=ciWidthTitle)
            table$addColumn(name='upper', title='Upper', type='number', superTitle=ciWidthTitle)
            
            nRows <- prod(nLevels)
            
            for (k in 1:nRows) {
              row <- list()
              table$addRow(rowKey=k, row)
            }
          }
        }
      },
      
      .initGroupSummary = function() {
        
        table <- self$results$groupSummary
        bs <- self$options$bs
        
        bs <- lapply(bs, function(x) self$data[[x]])
        levels <- lapply(bs, levels)
        groups <- expand.grid(levels)
        if (nrow(groups) == 0) {
          groups <- data.frame(x='')
          colnames(groups) <- ''
        } else {
          colnames(groups) = self$options$bs
        }
        
        titles = colnames(groups)
        names = paste0('group:', titles)
        
        for (i in seq_len(ncol(groups))) {
          table$addColumn(
            index=1,
            name=names[i],
            title=titles[i],
            type='text',
            combineBelow=TRUE
          )
        }
        
        for (i in seq_len(nrow(groups))) {
          values <- apply(groups[i,,drop=FALSE], 2, paste)
          names(values) <- names
          table$addRow(rowKey=unname(values), values=values)
        }
      },
      
      .initcvals = function() {
        
        table <- self$results$cvals
        table$setRow(rowNo=1, values=list(var=paste0('Contrast 1')))
        table$setRow(rowNo=2, values=list(var=paste0('Contrast 2')))
        table$setRow(rowNo=3, values=list(var='Means'))
      },
      
      #### Populate tables functions ----
      .populateEffectsTables=function(result) {
        
        rmTable <- self$results$get('rmTable')
        bsTable <- self$results$get('bsTable')
        
        suppressWarnings({
          summaryResult <- summary(result)
        })
        model <- summaryResult$univariate.tests
        epsilon <- summaryResult$pval.adjustments
        ges <- result$anova_table
        
        bsRows <- bsTable$rowKeys
        rmRows <- rmTable$rowKeys
        modelRows <- jmvcore::decomposeTerms(as.list(rownames(model)))
        epsilonRows <- jmvcore::decomposeTerms(as.list(rownames(epsilon)))
        gesRows <- jmvcore::decomposeTerms(as.list(rownames(ges)))
        
        SSt <- private$.getSSt(model)
        
        
        # Populate RM table
        
        bs <- self$options$bs
        complete <- complete.cases(self$data)
        if (length(bs) == 0) {
          N <- sum(complete)
        } else {
          by <- lapply(bs, function(x) self$data[[x]])
          rm <- lapply(self$options$rmCells, function(x) x$measure)
          nt <- aggregate(complete, by=by, length)$x
          N <- sum(aggregate(complete, by=by, sum)$x)
        }
        
        wfact_ss <- 0
        wdf <- 0
        bdf <- 0
        for (i in seq_along(rmRows)) {
          
          if (! '.RES' %in% rmRows[[i]]) { # if the row is not a residual
            
            
            index <- which(sapply(modelRows, function(x) setequal(toB64(rmRows[[i]]), x)))
            if (length(rmRows)>1) {
              index2 <- which(sapply(modelRows, function(x) setequal(toB64(rmRows[[2]]), x)))
            }
            
            wfact_ss[i] <- model[index,'Sum Sq']
            werror_ss <- model[index,'Error SS']
            wdf[i] <- model[index,'num Df']
            
            np <- 3  # parameter each for ID, variance and grand mean
            mp <- np + model[index,'num Df']
            wnullm <- wfact_ss[1] + wfact_ss[2] + werror_ss
            wres_ss <- werror_ss
            if (length(rmRows)>1) {
              wres_ss <- werror_ss + model[index2,'Sum Sq']   # adding interaction
            }
            within_ss <- wfact_ss[i] + wres_ss
            
            S_12 <- -0.5 * N * wdf[1] * (log(within_ss) - log(wres_ss))
            S_totw <- -0.5 * N * wdf[1] * (log(within_ss) - log(werror_ss))
            
            # Corrections
            Ac <- function(c,k1,k2,N) { 
              if(c=="nc") { 0
              } else if(c=="ob") { 0.5*(k2-k1) 
              } else if(c=="aic") { 1*(k2-k1)
              } else {
                k2 * N/(N - k2 - 1) - k1 * (N/(N - k1 - 1)) 
              }
            }
            
            if (i > 1) {
              bindex <- which(sapply(modelRows, function(x) setequal(toB64(bsRows[[1]]), x)))
              mp <- np + wdf[1] + wdf[2] + model[bindex,'num Df']
              totme <- werror_ss + wfact_ss[1]
              S_12 <- S_totw - S_12
            } else {
              S_12c1 <- S_12 + Ac(self$options$correction,np,mp,N)
            }
            
            
            S_12c <- S_12 + Ac(self$options$correction,np,mp,N)
            
            row <- list()
            row[['S[none]']] <- S_12c
            row[['Param[none]']] <- paste0(c(np,mp), collapse = ', ')
            row[['ss[none]']] <- row[['ss[GG]']] <- row[['ss[HF]']] <- model[index,'Sum Sq']
            row[['F[none]']] <- row[['F[GG]']] <- row[['F[HF]']] <- model[index,'F value']
            row[['df[none]']] <- paste0(c(model[index,'num Df'],model[index,'den Df']), collapse = ', ')
            #            row[['ms[none]']] <- row[['ss[none]']] / model[index,'num Df']
            row[['p[none]']] <- model[index,'Pr(>F)']
            
            # Add sphericity corrected values
            indexEps <- which(sapply(epsilonRows, function(x) setequal(toB64(rmRows[[i]]), x)))
            dfRes <- model[index,'den Df']
            
            if (length(indexEps) == 0) {
              GG <- 1
              HF <- 1
            } 
            else {
              GG <- if (is.na(epsilon[indexEps,'GG eps'])) 1 else epsilon[indexEps,'GG eps']
              HF <- if (is.na(epsilon[indexEps,'HF eps']) || epsilon[indexEps,'HF eps'] > 1) 1 else epsilon[indexEps,'HF eps']
            }
            
            row[['df[GG]']] <- model[index,'num Df'] * GG
            row[['ms[GG]']] <- row[['ss[GG]']] / row[['df[GG]']]
            dfResGG <- dfRes * GG
            row[['p[GG]']] <- pf(row[['F[GG]']], row[['df[GG]']], dfResGG, lower.tail=FALSE)
            
            row[['df[HF]']] <- model[index,'num Df'] * HF
            row[['ms[HF]']] <- row[['ss[HF]']] / row[['df[HF]']]
            dfResHF <- dfRes * HF
            row[['p[HF]']] <- pf(row[['F[HF]']], row[['df[HF]']], dfResHF, lower.tail=FALSE)
            
            gesIndex <- which(sapply(gesRows, function(x) setequal(toB64(rmRows[[i]]), x)))
            gesValue <- ges[gesIndex, 'ges']
            
            # Add effect sizes
            SSr <- model[index,'Error SS']
            MSr <- SSr/dfRes
            
            row[['eta[none]']] <- row[['eta[GG]']] <- row[['eta[HF]']] <- row[['ss[none]']] / SSt
            row[['ges[none]']] <- row[['ges[GG]']] <- row[['ges[HF]']] <- gesValue
            row[['partEta[none]']] <- row[['partEta[GG]']] <- row[['partEta[HF]']] <- row[['ss[none]']] / (row[['ss[none]']] + SSr)
            
            omega <- (row[['ss[none]']] - (model[index,'num Df'] * MSr)) / (SSt + MSr)
            
            row[['omega[none]']] <- row[['omega[GG]']] <- row[['omega[HF]']] <- if ( ! is.na(omega) && omega < 0) 0 else omega
            
            
            rmTable$setRow(rowNo=i, values=row)
          } else { # if the row is a residual
            
            term <- rmRows[[i]][-length(rmRows[[i]])]
            index <- which(sapply(modelRows, function(x) setequal(toB64(term), x)))
            
            row <- list()
            row[['ss[none]']] <- row[['ss[GG]']] <- row[['ss[HF]']] <- model[index,'Error SS']
            row[['df[none]']] <- model[index,'den Df']
            row[['ms[none]']] <- row[['ss[none]']] / row[['df[none]']]
            row[['F[none]']] <- row[['F[GG]']]  <- row[['F[HF]']] <- ''
            row[['p[none]']] <- row[['p[GG]']] <- row[['p[HF]']] <- ''
            row[['ges[none]']] <- row[['ges[GG]']] <- row[['ges[HF]']] <- ''
            row[['eta[none]']] <- row[['eta[GG]']] <- row[['eta[HF]']] <- ''
            row[['partEta[none]']] <- row[['partEta[GG]']] <- row[['partEta[HF]']] <- ''
            row[['omega[none]']] <- row[['omega[GG]']] <- row[['omega[HF]']] <- ''
            
            # Add sphericity corrected values
            indexEps <- which(sapply(epsilonRows, function(x) setequal(toB64(term), x)))
            dfRes <- model[index,'den Df']
            
            if (length(indexEps) == 0) {
              GG <- 1
              HF <- 1
            } else {
              GG <- if (is.na(epsilon[indexEps,'GG eps'])) 1 else epsilon[indexEps,'GG eps']
              HF <- if (is.na(epsilon[indexEps,'HF eps']) || epsilon[indexEps,'HF eps'] > 1) 1 else epsilon[indexEps,'HF eps']
            }
            
            row[['df[GG]']] <- row[['df[none]']] * GG
            row[['ms[GG]']] <- row[['ss[GG]']] / row[['df[GG]']]
            row[['df[HF]']] <- row[['df[none]']] * HF
            row[['ms[HF]']] <- row[['ss[HF]']] / row[['df[HF]']]
            
            rmTable$setRow(rowNo=i, values=row)            
          }
        }
        
        
        
        # Populate BS table
        
        bsTerms <- lapply(bsRows[which( ! sapply(bsRows, function(x) x[1] == 'Residual'))], toB64)
        bnullm <- 0
        Sb_12c <- ''
        if (length(bsTerms) > 0)
          bsIndices <- sapply(bsTerms, function(x) which(sapply(modelRows, function(y) setequal(x,y))))
        
        for (i in seq_along(bsRows)) {
          
          if (! bsRows[[i]][1] == 'Residual') { # if the row is not a residual
            
            index <- which(sapply(modelRows, function(x) setequal(toB64(bsRows[[i]]), x)))
            gesIndex <- which(sapply(gesRows, function(x) setequal(toB64(bsRows[[i]]), x)))
            
            bfact_ss <- model[index,'Sum Sq']
            berror_ss <- model[index,'Error SS']
            bnullm <- bfact_ss + berror_ss
            bdf[1] <- model[index,'num Df']
            bdf[2] <- model[index,'den Df']
            mp <- np + bdf[1]
            Sb_12 <- -0.5 * N * (log(bnullm) - log(berror_ss))
            # Corrections
            Sb_12c <- Sb_12 + Ac(self$options$correction,np,mp,N)
            
            row <- list()
            row[['S']] <- Sb_12c
            row[['Param']] <- paste0(c(np,mp), collapse = ', ')
            row[['F']] <- model[index,'F value']
            row[['df']] <- paste0(c(model[index,'num Df'],model[index,'den Df']), collapse = ', ')
            rdf <- model[index,'num Df']
            row[['p']] <- model[index,'Pr(>F)']
            
            row[['ss']] <- model[index,'Sum Sq']
            
            # Add effect sizes
            SSr <- model[index,'Error SS']
            MSr <- SSr/model[index,'den Df']
            row[['ges']] <- ges[gesIndex, 'ges']
            row[['eta']] <- row[['ss']] / SSt
            row[['partEta']] <- row[['ss']] / (row[['ss']] + SSr)
            omega <- (row[['ss']] - (rdf * MSr)) / (SSt + MSr)
            row[['omega']] <- if ( ! is.na(omega) && omega < 0) 0 else omega
            row[['omega']] <- 0
            
            bsTable$setRow(rowNo=i, values=row)
            
          } else { # if the row is a residual
            
            row <- list()
            row[['S']] <- ""
            row[['Param']] <- ""
            row[['F']] <- ""
            row[['df']] <- ""
            row[['p']] <- ""
            
            bsTable$setRow(rowNo=i, values=row)
          }
        }       
        # Contrast 1 
        
        datf <- data.frame(private$.wideToLong())  #as.data.frame
        
        if (dim(datf)[2]==4) {
          datf[,1] <- fromB64(datf[,1])
          datf[,4] <- fromB64(datf[,4])
          means <- tapply(datf[,3], c(datf[4],datf[1]), mean)
          numbers <- tapply(datf[,3], c(datf[4], datf[1]), length)
          mean_num <- (wdf[1]+1) * (bdf[1]+1)
          tss <- bnullm + wnullm
        } else {
          datf[,3] <- fromB64(datf[,3])
          means <- tapply(datf[,2], datf[3], mean)
          numbers <- tapply(datf[,2], datf[3], length)
          mean_num <- (wdf[1]+1)
          tss <- within_ss
        }
        
        S_c1Nc <- ''
        Parval_c1N <- ''
        DFval_c1 <- ''
        S_c1c2 <- ''
        Parval_c1c2 <- ''
        SS_cont1 <- ''
        SS_cont2 <- ''
        S_c2Nc <- ''
        Parval_c2N <- ''
        DFval_c2 <- ''
        
        # contrasts
        contrast1 <- 0
        contrast2 <- 0
        
        for (i in 1:10) {
          contrast1[i] <- ''
          contrast2[i] <- ''
        }
        if ( self$options$ct1 ){
          contrast1 <- private$.getCt1Values()
          if (length(contrast1) != mean_num){
            jmvcore::reject(.("Contrast 1 has the wrong number of values"))
          }
        }
        
        if ( self$options$ct2 ){
          contrast2 <- private$.getCt2Values()
          if (length(contrast2) != mean_num){
            jmvcore::reject(.("Contrast 2 has the wrong number of values"))
          }
        }
        
        orth <- NULL
        footnote <- NULL
        LSS_null <- log(tss)
        
        # compare contrast 1 versus null
        
        if ( self$options$ct1 ) {
          if (abs(sum(contrast1)) >= 0.03) footnote <- 1
          
          mp <- np + 1      # parameters for contrasts
          SS_cont1 <- sum(contrast1*means)^2/(sum(contrast1^2/(numbers)))
          LSS_c1 <- log(tss - SS_cont1)
          S_c1N <- -0.5 * N * wdf[1] * (LSS_null - LSS_c1)
          S_c1Nc <- S_c1N + Ac(self$options$correction,mp,np,N)
        }
        
        if ( self$options$ct2 ) {      
          SS_cont2 <- sum(contrast2*means)^2/(sum(contrast2^2/(numbers)))
          LSS_c2 <- log(tss - SS_cont2)
          S_c2N <- -0.5 * N * wdf[1] * (LSS_null - LSS_c2)
          S_c2Nc <- S_c2N  + Ac(self$options$correction,mp,np,N)
          if (abs(sum(contrast2)) >= 0.03) footnote <- 1
          # no correction necessary using contrasts
        }
        
        # compare contrast 1 with null and versus contrast 2
        if ( self$options$ct1 && self$options$ct2) {
          S_c1c2 <- -0.5 * N * wdf[1] * (LSS_c1 - LSS_c2)
          if (abs(sum(contrast1*contrast2)) >= 0.1) orth <- 1
          if (abs(sum(contrast1)) >= 0.03 || abs(sum(contrast2)) >= 0.03) footnote <- 1
          
        }
        
        if ( self$options$ct1 ){
          Parval_c1N <- paste0(c(mp,np), collapse = ', ')
          DFval_c1 <- as.integer(1)
        }
        
        if ( self$options$ct2 ){        
          Parval_c2N <- paste0(c(mp,np), collapse = ', ')
          DFval_c2 <- as.integer(1)
        }
        
        if ( self$options$ct1 && self$options$ct2){        
          Parval_c1c2 <- paste0(c(mp,mp), collapse = ', ')
        }
        
        if(self$options$correction=="ob") { notext <- "S uses Occam's Bonus correction for parameters (Param). "
        } else if(self$options$correction=="aic") { notext <- "S uses AIC correction for parameters (Param). "
        } else if(self$options$correction=="aicsm") { notext <- "S uses AIC small sample correction for parameters (Param). "
        } else {
          notext <- "S uses no correction for parameters (Param). "
        }
        
        table <- self$results$conts
        if ( ! is.null(orth))
          table$setNote('Note', paste0("The contrasts are not orthogonal. ", notext))
        if ( ! is.null(footnote))
          table$setNote('Note', paste0("One or both of the contrast weights do not sum to zero. ", notext))
        if ( ! is.null(orth) && ! is.null(footnote))
          table$setNote('Note', paste0("One or both of the contrast weights do not sum to zero and the contrasts are not orthogonal. ", notext))
        table$setRow(rowNo=1, values=list(S=S_c1Nc, Param=Parval_c1N,
                                          SSq=SS_cont1, df=DFval_c1))
        table$setRow(rowNo=2, values=list(S=S_c2Nc, Param=Parval_c2N,
                                          SSq=SS_cont2, df=DFval_c2))
        table$setRow(rowNo=3, values=list(S=S_c1c2, Param=Parval_c1c2,
                                          SSq="", df=""))
        
        cmn <- 0
        cct <- 0
        cct2 <- 0
        for (i in 1:30) {
          cmn[i] <- ''
          cct[i] <- ''
          cct2[i] <- ''
        }
        
        for (i in 1:mean_num) {
          cmn[i] <- signif(means[i],4)
          cct[i] <- contrast1[i]
          cct2[i] <- contrast2[i]
        }
        
        table <- self$results$cvals
        table$setRow(rowNo=1, values=list(L1=cct[1], L2=cct[2], L3=cct[3], 
                                          L4=cct[4], L5=cct[5], L6=cct[6], L7=cct[7], L8=cct[8], L9=cct[9], L10=cct[10]))
        table$setRow(rowNo=2, values=list(L1=cct2[1], L2=cct2[2], L3=cct2[3], 
                                          L4=cct2[4], L5=cct2[5], L6=cct2[6], L7=cct2[7], L8=cct2[8], L9=cct2[9], L10=cct2[10]))
        table$setRow(rowNo=3, values=list(L1=cmn[1], L2=cmn[2], L3=cmn[3], L4=cmn[4],
                                          L5=cmn[5], L6=cmn[6], L7=cmn[7], L8=cmn[8], L9=cmn[9], L10=cmn[10]))
        
        stats <- list(S1 = S_12c1,
                      S2 = S_12c,
                      S3 = Sb_12c,
                      S4 = S_c1Nc,
                      S5 = S_c2Nc,
                      S6 = S_c1c2)
        
        # Populate Explanation & table
        private$.populateSupportText(stats)
        private$.populateMoreSupportText()
        
        
      },
      .populateSpericityTable=function(result) {
        
        spherTable <- self$results$assump$spherTable
        
        summaryResult <- suppressWarnings({summary(result)})
        epsilon <- summaryResult$pval.adjustments
        mauchly <- summaryResult$sphericity.tests
        
        nLevels <- sapply(self$options$rm, function(x) return(length(x$levels)))
        resultRows <- decomposeTerms(rownames(mauchly))
        
        if (any(nLevels > 2) && length(resultRows) > 0) {
          
          for (term in self$options$rmTerms) {
            
            index <- which(sapply(as.list(resultRows), function(x) setequal(x, toB64(term))))
            
            if (length(index) == 0) {
              
              spherTable$setRow(rowKey=term, values=list('mauch'=1, 'p'=NaN, 'gg'=1, 'hf'=1))
              if (length(spherTable$getRow(rowKey=term)$name$footnotes) == 0)
                spherTable$addFootnote(rowKey=term, 'p', .('The repeated measures has only two levels. The assumption of sphericity is always met when the repeated measures has only two levels.'))
              
            } else {
              
              row <- list()
              row[['mauch']] <- mauchly[index,'Test statistic']
              row[['p']] <- mauchly[index,'p-value']
              row[['gg']] <- epsilon[index, 'GG eps']
              row[['hf']] <- if (epsilon[index, 'HF eps'] > 1) 1 else epsilon[index, 'HF eps']
              
              spherTable$setRow(rowKey=term, values=row)
            }
          }
        } else {
          
          for (term in self$options$rmTerms) {
            
            if (any(nLevels > 2)) {
              spherTable$setRow(rowKey=term, values=list('mauch'=NaN, 'p'=NaN, 'gg'=NaN, 'hf'=NaN))
              if (length(spherTable$getRow(rowKey=term)$name$footnotes) == 0)
                spherTable$addFootnote(rowKey=term, 'name', .('Singularity error. Sphericity tests are not available'))
              
            } else {
              spherTable$setRow(rowKey=term, values=list('mauch'=1, 'p'=NaN, 'gg'=1, 'hf'=1))
              if (length(spherTable$getRow(rowKey=term)$name$footnotes) == 0)
                spherTable$addFootnote(rowKey=term, 'p', .('The repeated measures has only two levels. The assumption of sphericity is always met when the repeated measures has only two levels'))
            }
          }
        }
      },
      .populateLeveneTable=function () {
        
        if (length(self$options$rmCells) == 0)
          return()
        
        leveneTable <- self$results$get('assump')$get('leveneTable')
        
        rmVars <- sapply(self$options$rmCells, function(x) return(x$measure))
        bsVars <- self$options$bs
        
        if (length(bsVars) == 0) {
          for (var in rmVars) {
            leveneTable$setRow(rowKey=var, values=list('F'=NaN, 'df1'='', 'df2'='', 'p'=''))
            leveneTable$addFootnote(rowKey=var, 'F', .('As there are no between subjects factors specified this assumption is always met.'))
          }
          return()
        }
        
        data <- list()
        for (rm in c(rmVars))
          data[[rm]] <- jmvcore::toNumeric(self$data[[rm]])
        
        for (bs in bsVars)
          data[[bs]] <- factor(self$data[[bs]])
        
        attr(data, 'row.names') <- seq_len(length(data[[1]]))
        attr(data, 'class') <- 'data.frame'
        data <- jmvcore::naOmit(data)
        
        group <- interaction(data[bsVars])
        data <- cbind(data, .GROUP=group)
        
        for (var in rmVars) {
          
          formula <- as.formula(paste0(composeTerm(var),'~ .GROUP'))
          
          res <- abs(aov(formula, data=data)$residuals)
          r <- summary(aov(res ~ group))[[1]]
          
          row <- list(F=r[1,'F value'], df1=r[1,'Df'], df2=r[2,'Df'], p=r[1,'Pr(>F)'])
          leveneTable$setRow(rowKey=var, values=row)
        }
      },
      
      .populateEmmTables = function() {
        
        emMeans <- self$options$emMeans
        emmTables <- private$emMeans
        
        group <- self$results$emm
        
        for (j in seq_along(emMeans)) {
          
          emm <- emMeans[[j]]
          
          if ( ! is.null(emm)) {
            
            emmGroup <- group$get(key=j)
            table <- emmGroup$emmTable
            
            emmTable <- emmTables[[j]]
            
            for (k in 1:nrow(emmTable)) {
              row <- list()
              sign <- list()
              
              for (l in seq_along(emm)) {
                value <- emmTable[k, jmvcore::toB64(emm[l])]
                row[[emm[l]]] <- jmvcore::fromB64(value)
              }
              
              row[['mean']] <- emmTable[k, 'emmean']
              row[['se']] <- emmTable[k, 'SE']
              row[['lower']] <- emmTable[k, 'lower.CL']
              row[['upper']] <- emmTable[k, 'upper.CL']
              
              table$setRow(rowNo=k, values=row)
            }
          }
        }
      },
      
      .populateGroupSummaryTable = function() {
        
        table <- self$results$groupSummary
        data <- self$data
        bs <- self$options$bs
        complete <- complete.cases(data)
        
        if (length(bs) == 0) {
          n <- sum(complete)
          ex <- length(complete) - n
          table$setRow(rowNo=1, values=list(n=n, ex=ex))
        } else {
          by <- lapply(bs, function(x) self$data[[x]])
          rm <- lapply(self$options$rmCells, function(x) x$measure)
          nt <- aggregate(complete, by=by, length)$x
          n <- aggregate(complete, by=by, sum)$x
          ex <- nt - n
          for (i in seq_along(n))
            table$setRow(rowNo=i, values=list(n=n[i], ex=ex[i]))
        }
      },
      
      #### Plot functions ----
      .prepareQQPlot = function(model) {
        image <- self$results$assump$qq
        
        suppressMessages({
          suppressWarnings({
            residuals <- scale(residuals(model))
          })
        })
        
        image$setState(residuals)
      },
      .qqPlot=function(image, ggtheme, theme, ...) {
        
        if (is.null(image$state))
          return(FALSE)
        
        df <- as.data.frame(qqnorm(image$state, plot.it=FALSE))
        
        p <- ggplot(data=df, aes(y=y, x=x)) +
          geom_abline(slope=1, intercept=0, colour=theme$color[1]) +
          geom_point(aes(x=x,y=y), size=2, colour=theme$color[1]) +
          xlab(.("Theoretical Quantiles")) +
          ylab(.("Standardized Residuals")) +
          ggtheme
        
        return(p)
      },
      .prepareEmmPlots = function(model, data) {
        
        emMeans <- self$options$emMeans
        
        group <- self$results$emm
        emmTables <- list()
        
        for (j in seq_along(emMeans)) {
          
          term <- emMeans[[j]]
          
          if ( ! is.null(term)) {
            
            image <- group$get(key=j)$emmPlot
            
            termB64 <- jmvcore::toB64(term)
            formula <- formula(paste('~', jmvcore::composeTerm(termB64)))
            
            if (self$options$emmWeights)
              weights <- 'equal'
            else
              weights <- 'cells'
            
            suppressMessages({
              emmeans::emm_options(sep = ",", parens = "a^")
              
              mm <- try(
                emmeans::emmeans(model, formula, options=list(level=self$options$ciWidthEmm / 100),
                                 weights = weights, model = "multivariate"),
                silent = TRUE
              )
            })
            
            d <- as.data.frame(summary(mm))
            emmTables[[ j ]] <- d
            
            for (k in 1:3) {
              if ( ! is.na(termB64[k])) {
                d[[ termB64[k] ]] <- factor(jmvcore::fromB64(d[[ termB64[k] ]]),
                                            jmvcore::fromB64(levels(d[[ termB64[k] ]])))
              }
            }
            
            names <- list('x'=termB64[1], 'y'='emmean', 'lines'=termB64[2], 'plots'=termB64[3], 'lower'='lower.CL', 'upper'='upper.CL')
            names <- lapply(names, function(x) if (is.na(x)) NULL else x)
            
            labels <- list('x'=term[1], 'y'=self$options$depLabel, 'lines'=term[2], 'plots'=term[3])
            labels <- lapply(labels, function(x) if (is.na(x)) NULL else x)
            
            dataNew <- lapply(data, function(x) {
              if (is.factor(x))
                levels(x) <- jmvcore::fromB64(levels(x))
              return(x)
            })
            
            image$setState(list(emm=d, data=dataNew, names=names, labels=labels))
            
          }
        }
        
        private$emMeans <- emmTables
      },
      .emmPlot = function(image, ggtheme, theme, ...) {
        if (is.null(image$state))
          return(FALSE)
        
        data <- as.data.frame(image$state$data)
        emm <- image$state$emm
        names <- image$state$names
        labels <- image$state$labels
        
        emm$lowerSE <- emm[[names$y]] - emm[['SE']]
        emm$upperSE <- emm[[names$y]] + emm[['SE']]
        
        if (theme$bw) {
          lty <- names$lines
        } else {
          lty <- NULL
        }
        
        if (self$options$emmPlotData)
          dodge <- position_dodge(0.7)
        else
          dodge <- position_dodge(0.3)
        
        if (is.null(names$lines))
          jitterdodge <- position_jitter(width = 0.1)
        else
          jitterdodge <- position_jitterdodge(dodge.width = 0.7, jitter.width = 0.4)
        
        p <- ggplot(
          data=emm,
          aes_string(
            x=names$x,
            y=names$y,
            color=names$lines,
            fill=names$lines,
            linetype=lty,
            group=names$lines
          ),
          inherit.aes = FALSE
        )
        
        if (self$options$emmPlotData)
          p <- p + geom_point(data=data, aes_string(y=jmvcore::toB64('.DEPENDENT')), alpha=0.3, position=jitterdodge)
        
        p <- p + geom_line(size=.8, position=dodge)
        
        if (self$options$emmPlotError == 'ci') {
          p <- p + geom_errorbar(
            aes_string(x=names$x, ymin=names$lower, ymax=names$upper, linetype=NULL),
            width=.1, size=.8, position=dodge
          )
        } else if (self$options$emmPlotError == 'se') {
          p <- p + geom_errorbar(
            aes_string(x=names$x, ymin='lowerSE', ymax='upperSE', linetype=NULL),
            width=.1, size=.8, position=dodge
          )
        }
        
        p <- p + geom_point(shape=21, fill='white', size=3, position=dodge)
        
        if ( ! is.null(names$plots)) {
          formula <- as.formula(paste(". ~", names$plots))
          p <- p + facet_grid(formula)
        }
        
        p <- p +
          labs(x=labels$x, y=labels$y, fill=labels$lines, color=labels$lines, linetype=labels$lines) +
          ggtheme + theme(panel.spacing = unit(2, "lines"))
        
        return(p)
      },
      
      #### Helper functions ----
      .dataCheck=function() {
        
        data <- self$data
        
        rm <- sapply(self$options$rmCells, function(x) return(x$measure))
        bs <- unlist(self$options$bs)  # 
        
        varsNumeric <- c(rm)
        
        dataFactors <- list()
        for (i in seq_along(bs))
          dataFactors[[bs[i]]] <- data[[bs[i]]]
        
        dataNumeric <- list()
        for (i in seq_along(varsNumeric))
          dataNumeric[[varsNumeric[i]]] <- jmvcore::toNumeric(data[[varsNumeric[i]]])
        
        # Check all values
        allNAItems <- sapply(c(dataFactors, dataNumeric), function(x) all(is.na(x)))
        if (any(allNAItems)) {
          onlyContainsMissingsMessage <- .("Item '{item}' contains only missing values")
          jmvcore::reject(onlyContainsMissingsMessage, code='error', item=c(bs,varsNumeric)[allNAItems])
        }
        
        # Check factor values
        if (length(dataFactors) > 0) {
          singleLevelItems <- sapply(dataFactors, function(x) length(levels(x)) == 1)
          if (any(singleLevelItems)) {
            oneLevelOnlyMessage <- .("Item '{item}' consists of one level only")
            jmvcore::reject(oneLevelOnlyMessage, code='error', item=bs[singleLevelItems])
          }
          
          factorLevelCounts = table(dataFactors)
          if (any(factorLevelCounts == 0)) {
            jmvcore::reject(
              .("Empty cells in between subject design: at least one combination of between subject factor levels has 0 observations"),
              code=exceptions$dataError
            )
          }
        }
        
        
        # Check numeric values
        factorItems <- sapply(dataNumeric, function(x) class(jmvcore::toNumeric(x)) == "factor")
        infItems <- sapply(dataNumeric, function(x) any(is.infinite(x)))
        noVarItems <- sapply(dataNumeric, function(x) var(x, na.rm = TRUE) == 0)
        if (any(factorItems)) {
          notNumericMessage <- .("Item '{item}' needs to be numeric")
          jmvcore::reject(notNumericMessage, code='error', item=varsNumeric[factorItems])
        }
        
        if (any(infItems)) {
          infiniteValuesMessage <- .("Item '{item}' contains infinite values")
          jmvcore::reject(infiniteValuesMessage, code='error', item=varsNumeric[infItems])
        }
      },
      .rmTerms=function() {
        
        if (length(self$options$rmTerms) == 0) { # if no specific model is specified
          
          rmFactors <- self$options$rm
          bsFactors <- self$options$bs
          
          if (length(rmFactors) == 0)
            rmFactors <- list(list(label='RM Factor 1'))
          
          bsNames <- c(bsFactors)
          
          rmNames <- sapply(rmFactors, function(x) x$label, simplify=TRUE)
          rmFormula <- as.formula(paste('~', paste(paste0('`', rmNames, '`'), collapse='*')))
          rmTerms <- attr(stats::terms(rmFormula), 'term.labels')
          rmTerms <- sapply(rmTerms, function(x) as.list(strsplit(x, ':')), USE.NAMES=FALSE)
          
          if (length(bsFactors) > 0) {
            bsFormula <- as.formula(paste('~', paste(paste0('`', bsFactors, '`'), collapse='*')))
            bsTerms <- attr(stats::terms(bsFormula), 'term.labels')
            bsTerms <- sapply(bsTerms, function(x) as.list(strsplit(x, ':')), USE.NAMES=FALSE)
          } else {
            bsTerms <- NULL
          }
          
          terms <- list()
          spacing <- list()
          
          for (i in seq_along(rmTerms)) {
            
            rmTerm <- rmTerms[[i]]
            terms[[length(terms)+1]] <- rmTerm
            
            for (j in seq_along(bsTerms))
              terms[[length(terms)+1]] <- c(rmTerm, bsTerms[[j]])
            
            spacing[[length(terms)]] <- 'below'
          }
          
        } else { # if the user specifies a model
          
          rmTerms <- self$options$rmTerms
          bsTerms <- self$options$bsTerms
          
          terms <- list()
          spacing <- list()
          
          for (i in seq_along(rmTerms)) {
            
            rmTerm <- rmTerms[[i]]
            terms[[length(terms) + 1]] <- rmTerm
            spacing[[length(terms)]] <- 'above'
            
            for (j in seq_along(bsTerms))
              terms[[length(terms) + 1]] <- c(rmTerm, bsTerms[[j]])
            
            spacing[[length(terms)]] <- 'below'
          }
        }
        
        return(list(terms = terms, spacing = spacing))
      },
      .bsTerms=function() {
        
        if (length(self$options$bsTerms) == 0 && length(self$options$rmTerms) == 0) { # if no specific model is specified
          
          bsFactors <- self$options$bs
          
          if (length(bsFactors) > 0) {
            bsFormula <- as.formula(paste('~', paste(paste0('`', bsFactors, '`'), collapse='*')))
            bsTerms <- attr(stats::terms(bsFormula), 'term.labels')
            bsTerms <- sapply(bsTerms, function(x) as.list(strsplit(x, ':')), USE.NAMES=FALSE)
          } else {
            bsTerms <- list()
          }
          
          terms <- bsTerms
          
        } else { # if the user specifies a model
          
          bsTerms <- self$options$bsTerms
          bsFactors <- self$options$bs
          
          terms <- list()
          
          terms <- c(terms, bsTerms)
          terms[[length(terms) + 1]] <- 'Residual'
          
        }
        
        return(terms)
      },
      .wideToLong=function() {
        
        rmVars <- sapply(self$options$rmCells, function(x) return(x$measure))
        bsVars <- self$options$bs
        
        labels <- sapply(self$options$rm, function(x) return(x$label))
        levels <- lapply(self$options$rm, function(x) return(x$levels))
        rmCells <- lapply(self$options$rmCells, function(x) return(x$cell))
        
        data <- list()
        for (var in c(rmVars))
          data[[var]] <- jmvcore::toNumeric(self$data[[var]])
        
        for (var in bsVars)
          data[[var]] <- factor(self$data[[var]])
        
        attr(data, 'row.names') <- seq_len(length(data[[1]]))
        attr(data, 'class') <- 'data.frame'
        data <- jmvcore::naOmit(data)
        
        data <- cbind(data, '.SUBJECT'=1:nrow(data))
        
        dataLong <- as.list(reshape2:::melt.data.frame(data, id.vars=c(bsVars, '.SUBJECT'), measure.vars=rmVars, value.name='.DEPENDENT'))
        
        col <- dataLong[['variable']]
        temp <- numeric(length(col))
        for (j in seq_along(col))
          temp[j] <- which(rmVars %in% col[j])
        
        for (i in seq_along(labels))
          dataLong[[labels[[i]]]] <- factor(sapply(rmCells[temp], function(x) x[i]), levels[[i]])
        
        dataLong[['variable']] <- NULL
        
        dataLong <- lapply(dataLong, function(x) {
          if (is.factor(x))
            levels(x) <- toB64(levels(x))
          return(x)
        })
        
        attr(dataLong, 'row.names') <- seq_len(length(dataLong[[1]]))
        attr(dataLong, 'names') <- toB64(names(dataLong))
        attr(dataLong, 'class') <- 'data.frame'
        dataLong <- jmvcore::naOmit(dataLong)
        
        return(dataLong)
      },
      .modelFormula=function() {
        
        if (is.null(self$options$rmTerms)) {
          
          
          
        } else {
          
          bsTerms <- lapply(self$options$bsTerms, function(x) toB64(x))
          rmTerms <- lapply(self$options$rmTerms, function(x) toB64(x))
          
          bsItems <- composeTerms(bsTerms)
          bsTerm <- paste0('(', paste0(bsItems, collapse = ' + '), ')')
          
          rmItems <- composeTerms(rmTerms)
          rmTerm <- paste0('Error(', paste0(toB64('.SUBJECT'),'/(', rmItems, ')', collapse=' + '),')')
          
          allTerms <- c(bsTerms, rmTerms)
          for (term1 in rmTerms) {
            for (term2 in bsTerms) {
              allTerms[[length(allTerms) + 1]] <- unlist(c(term1, term2))
            }
          }
          
          allItems <- composeTerms(allTerms)
          mainTerm <- paste0('(', paste0(allItems, collapse = ' + '), ')')
          
          if (length(self$options$bsTerms) == 0) {
            formula <- as.formula(paste(toB64('.DEPENDENT'), '~', paste(mainTerm, rmTerm, sep=' + ')))
          } else {
            formula <- as.formula(paste(toB64('.DEPENDENT'), '~', paste(mainTerm, rmTerm, bsTerm, sep=' + ')))
          }
          
          return(formula)
          
        }
      },
      .emmPlotSize = function(emm) {
        
        data <- self$data
        bsFactors <- self$options$bs
        rmFactors <- self$options$rm
        
        rmNames <- sapply(rmFactors, function(x) return(x$label))
        rmLevels <- lapply(rmFactors, function(x) return(x$levels))
        
        levels <- list()
        for (i in seq_along(emm)) {
          if (emm[i] %in% rmNames) {
            levels[[ emm[i] ]] <- rmLevels[[which(emm[i] == rmNames)]]
          } else {
            levels[[ emm[i] ]] <- levels(data[[ emm[i] ]])
          }
        }
        
        nLevels <- as.numeric(sapply(levels, length))
        nLevels <- ifelse(is.na(nLevels[1:3]), 1, nLevels[1:3])
        nCharLevels <- as.numeric(sapply(lapply(levels, nchar), max))
        nCharLevels <- ifelse(is.na(nCharLevels[1:3]), 0, nCharLevels[1:3])
        nCharNames <- as.numeric(nchar(names(levels)))
        nCharNames <- ifelse(is.na(nCharNames[1:3]), 0, nCharNames[1:3])
        
        xAxis <- 30 + 20
        yAxis <- 30 + 20
        
        width <- max(350, 25 * nLevels[1] * nLevels[2] * nLevels[3])
        height <- 300 + ifelse(nLevels[3] > 1, 20, 0)
        
        legend <- max(25 + 21 + 3.5 + 8.3 * nCharLevels[2] + 28, 25 + 10 * nCharNames[2] + 28)
        width <- yAxis + width + ifelse(nLevels[2] > 1, legend, 0)
        height <- xAxis + height
        
        return(c(width, height))
      },
      .getSSt=function (model) {
        
        rmTerms <- lapply(self$options$rmTerms, jmvcore::toB64)
        bsTerms <- lapply(self$options$bsTerms, jmvcore::toB64)
        
        if (length(bsTerms) == 0)
          bsTerms <- list('(Intercept)')
        
        terms <- c(rmTerms, bsTerms)
        
        modelRows <- jmvcore::decomposeTerms(as.list(rownames(model)))
        
        termSSt <- sum(model[-1, 'Sum Sq'])
        
        errorSSt <- 0
        for (i in seq_along(terms)) {
          for (j in seq_along(modelRows)) {
            if (all(terms[[i]] %in% modelRows[[j]]) && length(terms[[i]]) == length(modelRows[[j]])) {
              errorSSt <- errorSSt + model[j, 'Error SS']
              break
            }
          }
        }
        
        SSt <- termSSt + errorSSt
        
        return(SSt)
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
      
      
      .sourcifyOption = function(option) {
        
        name <- option$name
        value <- option$value
        
        if (name == 'contrasts') {
          if (all(vapply(value, function(x) x$type == 'none', FALSE)))
            return('')
        }
        
        super$.sourcifyOption(option)
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
        rm <- private$.rmTerms()
        rmTerms <- rm$terms
        bsTerms <- private$.bsTerms()
        
        stg2 <- ''; stg3 <- ''; stg4=''; stg5=''; stg6=''
        Sxl = list(s=st$S1, "<i>H</i>\u2080", stringifyTerm(rmTerms[[1]]))                       
        stg1 <- private$.strength(Sxl)
        if (length(rmTerms) > 1) {
          Sxl = list(s=st$S2, "<i>H</i>\u2080", stringifyTerm(rmTerms[[2]]))                       
          stg2 <- private$.strength(Sxl)
          Sxl = list(s=st$S3, "<i>H</i>\u2080", stringifyTerm(bsTerms[[1]]))                       
          stg3 <- private$.strength(Sxl)
        }
        if ( self$options$ct1 ){ 
          Sxl = list(s=st$S4, "<i>H</i>\u2080", "Contrast 1")
          stg4 <- private$.strength(Sxl)
        }
        if ( self$options$ct2 ){
          Sxl = list(s=st$S5, "<i>H</i>\u2080", "Contrast 2")                       
          stg5 <- private$.strength(Sxl)
        }
        if ( self$options$ct1 && self$options$ct2) {
          Sxl = list(s=st$S6, "Contrast 1", "Contrast 2")                       
          stg6 <- private$.strength(Sxl)
        }
        
        if(self$options$correction=="ob") { stg0 <- "<i>Using Occam's Bonus correction, the analysis shows that:</i>"
        } else if(self$options$correction=="aicsm") { stg0 <- "<i>Using AIC small sample correction, the analysis shows that:</i>"
        } else if(self$options$correction=="aic") { stg0 <- "<i>Using AIC correction, the analysis shows that:</i>"
        } else {
          stg0 <- "<i>Using no correction, the analysis shows that:</i>"
        }
        stringmix <- ''; ct1txt <- ''; ct2txt <- ''; ct12txt <- ''
        if (length(rmTerms) > 1) {
          stringmix <- paste0("<br>", stg2," <br>", stg3)
        }
        if ( self$options$ct1 ){ 
          ct1txt <- paste0(stg4,"<br>")
        }
        if ( self$options$ct2 ){ 
          ct2txt <- paste0(stg5,"<br>")
        }
        if ( self$options$ct1 && self$options$ct2 ){ 
          ct12txt <- paste0(stg6,"<br>")
        }
        str = paste0("<br> <h2>Summarizing the evidential analysis</h2>", "<br>",
                     stg0, "<br>", stg1, stringmix, "<br><br>", ct1txt, ct2txt, ct12txt, "<br>",
                     "<br> Also give sample sizes and a graphical plot. Where available, the <i>p</i> values may be provided to allow 
                             comparison with a conventional analysis. 
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
                                            round(Sxl$s,1), ", against ", Sxl[2], " and in favour of ", Sxl[3]),
                       ifelse(Sxl$s < -2.9, paste0("There was strong evidence, <i>S</i> = ",
                                                   round(Sxl$s,1), ", against ", Sxl[2], " and in favour of ", Sxl[3]),
                              ifelse(Sxl$s < -1.9, paste0("There was moderate evidence, <i>S</i> = ",
                                                          round(Sxl$s,1), ", against ", Sxl[2], " and in favour of ", Sxl[3]),
                                     ifelse(Sxl$s < -0.9, paste0("There was weak evidence, <i>S</i> = ",
                                                                 round(Sxl$s,1), ", against ", Sxl[2], " and in favour of ", Sxl[3]),
                                            ifelse(Sxl$s < -0.1, paste0("There was less than weak evidence, <i>S</i> = ",
                                                                        round(Sxl$s,1), ", against ", Sxl[2], " and in favour of ", Sxl[3]),
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
        
        str1 <- "<i>Advantages of the Evidential Approach</i> 
          <br> One advantage of the evidential approach is that <i>S</i> quantifies the strength of evidence for or against the 
          null hypothesis. "
        str2 <- "Another advantage is that we can select hypothesis values that reflect our research interests. "
        str3 <- "For example, we could choose meaningful contrasts, and compare two contrasts with each other. "
        str = paste0(str1, str2, str3, "As data accumulates the strength of evidence for one hypothesis over another will tend to increase.")
        
        html$setContent(str)
        
      }
      
    )
)