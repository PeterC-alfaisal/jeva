llrClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "llrClass",
    inherit = llrBase,
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
        table$setTitle(.("Support: Risk Ratio analyses"))
        table$setRow(rowNo=1, values=list(var= "H\u2080 vs risk ratio"))
        table$setRow(rowNo=2, values=list(var="H\u2090 vs risk ratio"))
        table$setRow(rowNo=3, values=list(var="H\u2090 vs H\u2080"))
        
        table <- self$results$cttma
        table$setTitle(.("Support: Marginal main effects and interaction analyses, against the Null model"))
        table$setNote('Note', paste(notext, "The interaction and RR (against 1) will have the same S value. Adding  
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
          
          ##########################################################
          tab <- mat
          likelihood_2x2_rr <- function(tab,
                                        conf.level = self$options$ciWidth/100,
                                        likelihood.drop = self$options$lint,
                                        theta = self$options$alt,
                                        nul = self$options$nul,
                                        plot.drop = -self$options$supplot,
                                        continuity = 0.5,
                                        root.tol = 1e-12,
                                        n.plot = 1001L) {
            
            stopifnot(is.matrix(tab), identical(dim(tab), c(2L, 2L)))
            
            if (any(!is.finite(tab)) || any(tab < 0))
              stop("All cell counts must be finite and non-negative.")
            if (any(rowSums(tab) == 0) || any(colSums(tab) == 0))
              stop("Neither a row nor a column may have a zero total.")
            if (!is.numeric(conf.level) || length(conf.level) != 1L ||
                !is.finite(conf.level) || conf.level <= 0 || conf.level >= 1)
              stop("conf.level must be a finite value strictly between 0 and 1.")
            if (length(likelihood.drop) != 1L || length(plot.drop) != 1L ||
                !is.finite(likelihood.drop) || !is.finite(plot.drop) ||
                likelihood.drop <= 0 || plot.drop <= 0)
              stop("likelihood.drop and plot.drop must be finite positive numbers.")
            if (length(continuity) != 1L || !is.finite(continuity) || continuity <= 0)
              stop("continuity must be a finite positive number.")
            if (length(n.plot) != 1L || !is.finite(n.plot) || n.plot < 2)
              stop("n.plot must be at least 2.")
            if (length(theta) != 1L || length(nul) != 1L ||
                !is.finite(theta) || !is.finite(nul) || theta <= 0 || nul <= 0)
              stop("theta and nul must be finite positive risk ratios.")
            
            observed <- tab
            haldane_anscombe_applied <- any(tab == 0)
            if (haldane_anscombe_applied)
              tab <- tab + continuity
            
            # Use cc rather than c so base::c() is never masked.
            a <- tab[1, 1]
            b <- tab[1, 2]
            cc <- tab[2, 1]
            d <- tab[2, 2]
            
            r1 <- a + b
            r2 <- cc + d
            c1 <- a + cc
            c2 <- b + d
            n <- r1 + r2
            
            p1_hat <- a / r1
            p2_hat <- cc / r2
            risk_ratio <- p1_hat / p2_hat
            log_risk_ratio <- log(risk_ratio)
            
            # Constant-free unconditional binomial log likelihood. Its limiting values
            # at p = 0 or p = 1 are handled explicitly: 0 * log(0) is defined as 0.
            xlogy <- function(x, y) {
              ifelse(x == 0, 0, x * log(y))
            }
            
            loglik_probabilities <- function(p1, p2) {
              if (!is.finite(p1) || !is.finite(p2) || p1 < 0 || p1 > 1 ||
                  p2 < 0 || p2 > 1)
                return(-Inf)
              
              xlogy(a, p1) + xlogy(b, 1 - p1) +
                xlogy(cc, p2) + xlogy(d, 1 - p2)
            }
            
            ll_max <- loglik_probabilities(p1_hat, p2_hat)
            
            # Under RR = rr, set p1 = rr * p2 and maximise over p2.  Optimise on the
            # logit scale for a numerically stable interior representation:
            # p2 = upper_p2 * plogis(eta), where upper_p2 = min(1, 1/rr).
            # This prevents a fixed absolute tolerance from collapsing the parameter
            # interval for very large RR values, which arises with zero-event tables.
            profile_at_rr <- function(rr) {
              if (!is.finite(rr) || rr <= 0)
                stop("rr must be a finite positive risk ratio.")
              
              upper_p2 <- min(1, 1 / rr)
              if (!is.finite(upper_p2) || upper_p2 <= 0)
                stop("Could not form a valid parameter range for this risk ratio.")
              
              neg_loglik_eta <- function(eta) {
                p2 <- upper_p2 * stats::plogis(eta)
                -loglik_probabilities(rr * p2, p2)
              }
              
              # A finite, broad logit interval is adequate even when upper_p2 is tiny.
              # At +/- 40, plogis() is sufficiently close to 0/1 for profiling here.
              opt <- optimize(
                neg_loglik_eta,
                interval = base::c(-40, 40),
                tol = root.tol
              )
              
              eta <- opt$minimum
              p2 <- upper_p2 * stats::plogis(eta)
              p1 <- rr * p2
              fitted <- matrix(
                base::c(r1 * p1, r1 * (1 - p1), r2 * p2, r2 * (1 - p2)),
                nrow = 2,
                byrow = TRUE
              )
              
              list(
                risk_ratio = rr,
                p1 = p1,
                p2 = p2,
                fitted = fitted,
                loglik = -opt$objective,
                relative_loglik = -opt$objective - ll_max
              )
            }
            
            rel_loglik_at_rr <- function(rr) profile_at_rr(rr)$relative_loglik
            
            # Equivalent of the original like_function_height(). It returns the log of
            # the likelihood ratio relative to the unconstrained RR MLE, at RR = rr.
            like_function_height_rr <- function(rr) {
              profile_at_rr(rr)$relative_loglik
            }
            
            # Retain the original output names: xah is the likelihood height at theta,
            # and nullh is the likelihood height at nul. The log_ versions are the
            # corresponding relative log likelihoods.
            log_xah <- like_function_height_rr(theta)
            log_nullh <- like_function_height_rr(nul)
            xah <- exp(max(log_xah, log(.Machine$double.xmin)))
            nullh <- exp(max(log_nullh, log(.Machine$double.xmin)))
            
            chi <- suppressWarnings(stats::chisq.test(observed, correct = FALSE))
            
            tabt1=observed
            for (i in 1:length(tab)) {
              tabt1[i] <- observed[i]
              if (observed[i] < 1) tabt1[i]=1   # turn 0s into 1s for one table used for log
            }
            
            Sint <- sum(observed * log(tabt1/chi$expected)) 
            Sgt <- sum(observed*log(tabt1))-sum(observed)*log(sum(observed)/4)
            
            # Find a RR endpoint at which profile log likelihood has dropped by `drop`.
            # Work on the log(RR) scale, so ranges are symmetric multiplicatively.
            root_for_drop <- function(drop, side = c("lower", "upper")) {
              side <- match.arg(side)
              eta_hat <- log_risk_ratio
              target <- function(eta) rel_loglik_at_rr(exp(eta)) + drop
              
              step <- 1
              eta_probe <- if (side == "lower") eta_hat - step else eta_hat + step
              f_probe <- target(eta_probe)
              
              for (k in seq_len(100L)) {
                if (is.finite(f_probe) && f_probe <= 0)
                  break
                step <- step * 2
                eta_probe <- if (side == "lower") eta_hat - step else eta_hat + step
                f_probe <- target(eta_probe)
              }
              
              if (!is.finite(f_probe) || f_probe > 0)
                stop(sprintf("Could not bracket the profile-likelihood root for drop = %.8g on the %s side.",
                             drop, side))
              
              interval <- if (side == "lower") {
                base::c(eta_probe, eta_hat)
              } else {
                base::c(eta_hat, eta_probe)
              }
              
              stats::uniroot(target, interval = interval, tol = root.tol)$root
            }
            
            interval_for_drop <- function(drop) {
              log_rr_limits <- base::c(
                root_for_drop(drop, "lower"),
                root_for_drop(drop, "upper")
              )
              
              list(
                log_risk_ratio = log_rr_limits,
                risk_ratio = exp(log_rr_limits)
              )
            }
            
            ci_drop <- stats::qchisq(conf.level, df = 1) / 2
            confidence_interval <- interval_for_drop(ci_drop)
            support_interval <- interval_for_drop(likelihood.drop)
            plot_limits <- interval_for_drop(plot.drop)
            
            rr1_profile <- profile_at_rr(1)
            null_relative_loglik <- rr1_profile$relative_loglik
            likelihood_at_rr1 <- exp(max(null_relative_loglik, log(.Machine$double.xmin)))
            likelihood_ratio_statistic <- -2 * null_relative_loglik
            likelihood_ratio_p_value <- stats::pchisq(
              likelihood_ratio_statistic, df = 1, lower.tail = FALSE
            )
            
            # Plot on the log(RR) scale; equally spaced values therefore have equal
            # multiplicative spacing on the displayed x-axis.
            log_rr_grid <- seq(
              plot_limits$log_risk_ratio[1],
              plot_limits$log_risk_ratio[2],
              length.out = as.integer(n.plot)
            )
            rr_grid <- exp(log_rr_grid)
            profile_grid <- lapply(rr_grid, profile_at_rr)
            relative_loglik_grid <- vapply(profile_grid, `[[`, numeric(1), "relative_loglik")
            likelihood_grid <- exp(pmax(relative_loglik_grid, log(.Machine$double.xmin)))
            
            list(
              table = observed,
              corrected_table = tab,
              HAc = haldane_anscombe_applied,
              counts = base::c(a = a, b = b, c = cc, d = d),
              margins = base::c(row1 = r1, row2 = r2, col1 = c1, col2 = c2, total = n),
              risks = base::c(row1 = p1_hat, row2 = p2_hat),
              risk_ratio = risk_ratio,
              log_risk_ratio = log_risk_ratio,
              log_likelihood_maximum = ll_max,
              relative_log_likelihood_at_rr1 = null_relative_loglik,
              Sint = Sint,
              Sgt = Sgt,
              likelihood_at_rr1 = likelihood_at_rr1,
              likelihood_ratio_statistic = likelihood_ratio_statistic,
              likelihood_ratio_p_value = likelihood_ratio_p_value,
              lint = likelihood.drop,
              confidence_interval = c(list(level = conf.level, drop = ci_drop), confidence_interval),
              support_interval = c(list(drop = likelihood.drop), support_interval),
              plot_limits = c(list(drop = plot.drop), plot_limits),
              log_xah = log_xah,
              log_nullh = log_nullh,
              xah = xah,
              nullh = nullh,
              theta = theta,
              nul = nul,
              theta_profile = profile_at_rr(theta),
              nul_profile = profile_at_rr(nul),
              plot_data = data.frame(
                risk_ratio = rr_grid,
                log_risk_ratio = log_rr_grid,
                p1_profiled = vapply(profile_grid, `[[`, numeric(1), "p1"),
                p2_profiled = vapply(profile_grid, `[[`, numeric(1), "p2"),
                relative_log_likelihood = relative_loglik_grid,
                likelihood = likelihood_grid
              ),
              chi.s = unname(chi$statistic),
              expected = chi$expected,
              observed = chi$observed,
              g_df = chi$parameter,
              pearson_p_value = chi$p.value
              )
          }
          
          res <- likelihood_2x2_rr(mat)
          

          # Correction
          Ac <- function(c,k1,k2) { 
            if(c=="nc") { 0
            } else if(c=="ob") { 0.5*(k2-k1) 
            } else { 1*(k2-k1)
            } 
          }
          
          # don't know why, but get NaN when value for nul or alt is 1, so use the interaction value
          if(self$options$nul==1) {
            S2way <- -res$Sint
          } else {
            S2way <- res$log_nullh # check that this should be negative but same abs value as S for observed OR
          }

          # support for alt. H
          # don't know why, but get NaN when value for nul or alt is 1, so use the interaction value
          if(self$options$alt==1) {
            Salt <- -res$Sint
          } else {
            Salt <- res$log_xah # check that this should be negative but same abs value as S for observed OR
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
          
          # variance analysis
          toogood <- 1/2*(log(1/res$chi.s)) - (1 - res$chi.s)/2
          
          # marginal main effects analysis
          # main marginal totals
          row_sum <- rowSums(tab)
          col_sum <- colSums(tab)
          grandtot <- sum(tab)
          r1tot <- unname(res$margins[1])
          r2tot <- unname(res$margins[2])
          c1tot <- unname(res$margins[3])
          c2tot <- unname(res$margins[4])
          
          Srow <- sum(row_sum*log(row_sum))-grandtot*log(grandtot) + grandtot*log(length(row_sum))
          if(r1tot == r2tot) Srow <- 0
          Scol <- sum(col_sum*log(col_sum))-grandtot*log(grandtot) + grandtot*log(length(col_sum))
          if(c1tot == c2tot) Scol <- 0
          # interaction
          # Grand total
          # Sums
          exp_row <- (r1tot+r2tot)/2
          exp_col <- (c1tot+c2tot)/2
          exp_int <- grandtot/4
          
          # support for alt. H
          SexOR_null <- Salt - S2way
          SexOR_obs <- SexOR_null - S2way
          
          gn <- 2*abs(S2way) # likelihood ratio statistic
          gn_p <- 1-pchisq(gn,1)
          ga <- 2*abs(Salt)
          ga_p <- 1-pchisq(ga,1)
          gan <- 2*abs(SexOR_null)
          gan_p <- 1-pchisq(gan,1)
          gt_p <- 1-pchisq(2*res$Sgt,3)
          gr_p <- 1-pchisq(2*Srow,1)
          gc_p <- 1-pchisq(2*Scol,1)
          gi_p <- 1-pchisq(2*res$Sint,1)

          lintlev <- toString(self$options$lint); conflev <- paste0(self$options$ciWidth,"%")
          
          table <- self$results$ctt
          table$setRow(rowNo=1, values=list(Value=self$options$nul, ordiff= self$options$nul-res$risk_ratio, 
                                            S=S2way + Ac(self$options$correction,1,2), Param=paste0(c(1,2), collapse = ', '), 
                                            G=gn, df=res$g_df, p=gn_p))
          table$setRow(rowNo=2, values=list(Value=self$options$alt, ordiff= self$options$alt-res$risk_ratio, 
                                            S=Salt + Ac(self$options$correction,2,2), Param=paste0(c(2,2), collapse = ', '), 
                                            G=ga, df=res$g_df, p=ga_p))
          table$setRow(rowNo=3, values=list(Value="", ordiff= self$options$alt-self$options$nul, 
                                            S=SexOR_null + Ac(self$options$correction,2,1), Param=paste0(c(2,1), collapse = ', '), 
                                            G=gan, df=res$g_df, p=gan_p))
          
          table <- self$results$cttma
          table$setRow(rowNo=1, values=list(Value=exp_row, S=Srow + Ac(self$options$correction,2,1), 
                                            G=2*Srow, Param=paste0(c(2,1), collapse = ', '),df=as.integer(res$g_df), p=gr_p))
          table$setRow(rowNo=2, values=list(Value=exp_col, S=Scol + Ac(self$options$correction,2,1),
                                            G=2*Scol, Param=paste0(c(2,1), collapse = ', '), df=as.integer(res$g_df), p=gc_p))
          table$setRow(rowNo=3, values=list(Value="", S=res$Sint + Ac(self$options$correction,2,1), 
                                            G=2*res$Sint, Param=paste0(c(2,1), collapse = ', '), df=as.integer(res$g_df), p=gi_p))
          table$setRow(rowNo=4, values=list(Value=exp_int, S=res$Sgt + Ac(self$options$correction,4,1), 
                                            G=2*res$Sgt, Param=paste0(c(4,1), collapse = ', '), df=as.integer(3), p=gt_p))
          table <- self$results$ctt2
          table$setRow(rowNo=1, values=list(Level=lintlev, RR = res$risk_ratio, 
                                            Lower=res$support_interval$risk_ratio[1], Upper=res$support_interval$risk_ratio[2]))
          table$setRow(rowNo=2, values=list(Level=conflev, RR = res$risk_ratio, 
                                            Lower=res$confidence_interval$risk_ratio[1], Upper=res$confidence_interval$risk_ratio[2]))
          if (res$HAc)
            table$addFootnote(rowNo=1, col="RR", "Haldane-Anscombe correction applied")      
          
          table <- self$results$ctt3
          table$setNote('Note', "Unlike the \u03C7\u00B2 statistic, a large S value indicates 
          that the proportions are either more different or too similar compared with those expected") 
          if (isTRUE(self$options$cc))
            table$setNote('Note', "Continuity correction applied. Unlike the \u03C7\u00B2 statistic, a large S value indicates 
          that the proportions are either more different or too similar compared with those expected")
          table$setRow(rowNo=1, values=list(var= "For RR = 1", Sv=toogood, X2=res$chi.s, dfv=res$g_df, 
                                            pv=res$pearson_p_value, pv1=1-res$pearson_p_value))
          
          # stats for summary        
          stats <- list(S1 = S2way+ Ac(self$options$correction,1,2),
                        S2 = Salt + Ac(self$options$correction,2,2),
                        S3 = SexOR_null + Ac(self$options$correction,2,1),
                        S4 = Srow + Ac(self$options$correction,2,1),
                        S5 = Scol + Ac(self$options$correction,2,1),
                        S7 = res$Sgt + Ac(self$options$correction,4,1),
                        tg = toogood,
                        chi = res$chi.s)
          
          # Populate Explanation & table
          private$.populateSupportText(stats)
          private$.populateMoreSupportText()
          #
          
          if(isTRUE(self$options$varA)) { 
            
            table <- self$results$ctt3
            table$setVisible(TRUE)
            
          }
          
          g <- data.frame(a=res$counts[1], b=res$counts[2],
                          c=res$counts[3], d=res$counts[4])
          
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
        
        mat <- matrix(c(
          g$a, g$b,
          g$c, g$d
        ), nrow = 2, byrow = TRUE)
        
        tab <- mat
        likelihood_2x2_rr <- function(tab,
                                      conf.level = self$options$ciWidth/100,
                                      likelihood.drop = self$options$lint,
                                      theta = self$options$alt,
                                      nul = self$options$nul,
                                      plot.drop = -self$options$supplot,
                                      continuity = 0.5,
                                      root.tol = 1e-12,
                                      n.plot = 1001L) {
          
          stopifnot(is.matrix(tab), identical(dim(tab), c(2L, 2L)))
          
          if (any(!is.finite(tab)) || any(tab < 0))
            stop("All cell counts must be finite and non-negative.")
          if (any(rowSums(tab) == 0) || any(colSums(tab) == 0))
            stop("Neither a row nor a column may have a zero total.")
          if (!is.numeric(conf.level) || length(conf.level) != 1L ||
              !is.finite(conf.level) || conf.level <= 0 || conf.level >= 1)
            stop("conf.level must be a finite value strictly between 0 and 1.")
          if (length(likelihood.drop) != 1L || length(plot.drop) != 1L ||
              !is.finite(likelihood.drop) || !is.finite(plot.drop) ||
              likelihood.drop <= 0 || plot.drop <= 0)
            stop("likelihood.drop and plot.drop must be finite positive numbers.")
          if (length(continuity) != 1L || !is.finite(continuity) || continuity <= 0)
            stop("continuity must be a finite positive number.")
          if (length(n.plot) != 1L || !is.finite(n.plot) || n.plot < 2)
            stop("n.plot must be at least 2.")
          if (length(theta) != 1L || length(nul) != 1L ||
              !is.finite(theta) || !is.finite(nul) || theta <= 0 || nul <= 0)
            stop("theta and nul must be finite positive risk ratios.")
          
          observed <- tab
          haldane_anscombe_applied <- any(tab == 0)
          if (haldane_anscombe_applied)
            tab <- tab + continuity
          
          # Use cc rather than c so base::c() is never masked.
          a <- tab[1, 1]
          b <- tab[1, 2]
          cc <- tab[2, 1]
          d <- tab[2, 2]
          
          r1 <- a + b
          r2 <- cc + d
          c1 <- a + cc
          c2 <- b + d
          n <- r1 + r2
          
          p1_hat <- a / r1
          p2_hat <- cc / r2
          risk_ratio <- p1_hat / p2_hat
          log_risk_ratio <- log(risk_ratio)
          
          # Constant-free unconditional binomial log likelihood. Its limiting values
          # at p = 0 or p = 1 are handled explicitly: 0 * log(0) is defined as 0.
          xlogy <- function(x, y) {
            ifelse(x == 0, 0, x * log(y))
          }
          
          loglik_probabilities <- function(p1, p2) {
            if (!is.finite(p1) || !is.finite(p2) || p1 < 0 || p1 > 1 ||
                p2 < 0 || p2 > 1)
              return(-Inf)
            
            xlogy(a, p1) + xlogy(b, 1 - p1) +
              xlogy(cc, p2) + xlogy(d, 1 - p2)
          }
          
          ll_max <- loglik_probabilities(p1_hat, p2_hat)
          
          # Under RR = rr, set p1 = rr * p2 and maximise over p2.  Optimise on the
          # logit scale for a numerically stable interior representation:
          # p2 = upper_p2 * plogis(eta), where upper_p2 = min(1, 1/rr).
          # This prevents a fixed absolute tolerance from collapsing the parameter
          # interval for very large RR values, which arises with zero-event tables.
          profile_at_rr <- function(rr) {
            if (!is.finite(rr) || rr <= 0)
              stop("rr must be a finite positive risk ratio.")
            
            upper_p2 <- min(1, 1 / rr)
            if (!is.finite(upper_p2) || upper_p2 <= 0)
              stop("Could not form a valid parameter range for this risk ratio.")
            
            neg_loglik_eta <- function(eta) {
              p2 <- upper_p2 * stats::plogis(eta)
              -loglik_probabilities(rr * p2, p2)
            }
            
            # A finite, broad logit interval is adequate even when upper_p2 is tiny.
            # At +/- 40, plogis() is sufficiently close to 0/1 for profiling here.
            opt <- optimize(
              neg_loglik_eta,
              interval = base::c(-40, 40),
              tol = root.tol
            )
            
            eta <- opt$minimum
            p2 <- upper_p2 * stats::plogis(eta)
            p1 <- rr * p2
            fitted <- matrix(
              base::c(r1 * p1, r1 * (1 - p1), r2 * p2, r2 * (1 - p2)),
              nrow = 2,
              byrow = TRUE
            )
            
            list(
              risk_ratio = rr,
              p1 = p1,
              p2 = p2,
              fitted = fitted,
              loglik = -opt$objective,
              relative_loglik = -opt$objective - ll_max
            )
          }
          
          rel_loglik_at_rr <- function(rr) profile_at_rr(rr)$relative_loglik
          
          # Equivalent of the original like_function_height(). It returns the log of
          # the likelihood ratio relative to the unconstrained RR MLE, at RR = rr.
          like_function_height_rr <- function(rr) {
            profile_at_rr(rr)$relative_loglik
          }
          
          # Retain the original output names: xah is the likelihood height at theta,
          # and nullh is the likelihood height at nul. The log_ versions are the
          # corresponding relative log likelihoods.
          log_xah <- like_function_height_rr(theta)
          log_nullh <- like_function_height_rr(nul)
          xah <- exp(max(log_xah, log(.Machine$double.xmin)))
          nullh <- exp(max(log_nullh, log(.Machine$double.xmin)))
          
          # Find a RR endpoint at which profile log likelihood has dropped by `drop`.
          # Work on the log(RR) scale, so ranges are symmetric multiplicatively.
          root_for_drop <- function(drop, side = c("lower", "upper")) {
            side <- match.arg(side)
            eta_hat <- log_risk_ratio
            target <- function(eta) rel_loglik_at_rr(exp(eta)) + drop
            
            step <- 1
            eta_probe <- if (side == "lower") eta_hat - step else eta_hat + step
            f_probe <- target(eta_probe)
            
            for (k in seq_len(100L)) {
              if (is.finite(f_probe) && f_probe <= 0)
                break
              step <- step * 2
              eta_probe <- if (side == "lower") eta_hat - step else eta_hat + step
              f_probe <- target(eta_probe)
            }
            
            if (!is.finite(f_probe) || f_probe > 0)
              stop(sprintf("Could not bracket the profile-likelihood root for drop = %.8g on the %s side.",
                           drop, side))
            
            interval <- if (side == "lower") {
              base::c(eta_probe, eta_hat)
            } else {
              base::c(eta_hat, eta_probe)
            }
            
            stats::uniroot(target, interval = interval, tol = root.tol)$root
          }
          
          interval_for_drop <- function(drop) {
            log_rr_limits <- base::c(
              root_for_drop(drop, "lower"),
              root_for_drop(drop, "upper")
            )
            
            list(
              log_risk_ratio = log_rr_limits,
              risk_ratio = exp(log_rr_limits)
            )
          }
          
          ci_drop <- stats::qchisq(conf.level, df = 1) / 2
          confidence_interval <- interval_for_drop(ci_drop)
          support_interval <- interval_for_drop(likelihood.drop)
          plot_limits <- interval_for_drop(plot.drop)
          
          rr1_profile <- profile_at_rr(1)
          null_relative_loglik <- rr1_profile$relative_loglik
          likelihood_at_rr1 <- exp(max(null_relative_loglik, log(.Machine$double.xmin)))
          likelihood_ratio_statistic <- -2 * null_relative_loglik
          likelihood_ratio_p_value <- stats::pchisq(
            likelihood_ratio_statistic, df = 1, lower.tail = FALSE
          )
          
          # Plot on the log(RR) scale; equally spaced values therefore have equal
          # multiplicative spacing on the displayed x-axis.
          log_rr_grid <- seq(
            plot_limits$log_risk_ratio[1],
            plot_limits$log_risk_ratio[2],
            length.out = as.integer(n.plot)
          )
          rr_grid <- exp(log_rr_grid)
          profile_grid <- lapply(rr_grid, profile_at_rr)
          relative_loglik_grid <- vapply(profile_grid, `[[`, numeric(1), "relative_loglik")
          likelihood_grid <- exp(pmax(relative_loglik_grid, log(.Machine$double.xmin)))
          
          list(
            table = observed,
            corrected_table = tab,
            haldane_anscombe_applied = haldane_anscombe_applied,
            counts = base::c(a = a, b = b, c = cc, d = d),
            margins = base::c(row1 = r1, row2 = r2, col1 = c1, col2 = c2, total = n),
            risks = base::c(row1 = p1_hat, row2 = p2_hat),
            risk_ratio = risk_ratio,
            log_risk_ratio = log_risk_ratio,
            log_likelihood_maximum = ll_max,
            relative_log_likelihood_at_rr1 = null_relative_loglik,
            likelihood_at_rr1 = likelihood_at_rr1,
            likelihood_ratio_statistic = likelihood_ratio_statistic,
            likelihood_ratio_p_value = likelihood_ratio_p_value,
            lint = likelihood.drop,
            confidence_interval = c(list(level = conf.level, drop = ci_drop), confidence_interval),
            support_interval = c(list(drop = likelihood.drop), support_interval),
            plot_limits = c(list(drop = plot.drop), plot_limits),
            log_xah = log_xah,
            log_nullh = log_nullh,
            xah = xah,
            nullh = nullh,
            theta = theta,
            nul = nul,
            theta_profile = profile_at_rr(theta),
            nul_profile = profile_at_rr(nul),
            plot_data = data.frame(
              risk_ratio = rr_grid,
              log_risk_ratio = log_rr_grid,
              p1_profiled = vapply(profile_grid, `[[`, numeric(1), "p1"),
              p2_profiled = vapply(profile_grid, `[[`, numeric(1), "p2"),
              relative_log_likelihood = relative_loglik_grid,
              likelihood = likelihood_grid
            )
          )
        }
        
        des <- likelihood_2x2_rr(mat)
        
        # Example base-R likelihood plot.
        if(self$options$plotype=="lplot") {
          with(des$plot_data, {
            plot(risk_ratio, likelihood, type = "l",
                 log = if (self$options$log_x) "x" else "",
                 xlab = if (self$options$log_x) "Risk ratio (Log10 scale)" else "Risk ratio",
                 ylab = "Likelihood")
            segments(des$support_interval$risk_ratio[1], exp(-self$options$lint), 
                     des$support_interval$risk_ratio[2], exp(-self$options$lint), lwd = 1, col = "red")
            lines(c(des$risk_ratio,des$risk_ratio), c(0,1), lty=2) # add OR as dashed line
            lines(c(self$options$alt,self$options$alt), c(0,des$xah), lty=1, col = "blue") # add H prob as blue line
            lines(c(self$options$nul,self$options$nul), c(0,des$nullh), lty=1) # add null H prob as black line
          })
        } else {
          with(des$plot_data, {
            plot(risk_ratio, log(likelihood), type = "l", ylim=c(self$options$supplot,0),
                 log = if (self$options$log_x) "x" else "",
                 xlab = if (self$options$log_x) "Odds ratio (Log10 scale)" else "Odds ratio",
                 ylab = "Likelihood")
            segments(des$support_interval$risk_ratio[1], -self$options$lint, 
                     des$support_interval$risk_ratio[2], -self$options$lint, lwd = 1, col = "red")
            lines(c(des$risk_ratio,des$risk_ratio), c(self$options$supplot,0), lty=2) # add OR as dashed line
            lines(c(self$options$alt,self$options$alt), c(self$options$supplot,des$log_xah), lty=1, col = "blue") # add H prob as blue line
            lines(c(self$options$nul,self$options$nul), c(self$options$supplot,des$log_nullh), lty=1) # add null H prob as black line
          })
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
        
        Sxl = list(s=st$S1, "H\u2080", "the observed <i>RR</i>")                       
        stg1 <- private$.strength(Sxl)
        Sxl = list(s=st$S2, "H\u2090", "the observed <i>RR</i>")                       
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
        
        stg8 <- paste0("For the observed <i>RR</i>", sv, ", that it was more different from the H\u2080 = 1 
                         than expected")
        if (2*st$tg > st$chi) {
          stg8 <- paste0("For the observed <i>RR</i>", sv, ", that it was closer to the H\u2080 = 1 than expected")
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
                     "<p>Give the <i>RR</i> and the observed frequencies. The support interval for the <i>RR</i> 
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
        str3 <- "For example, we could choose a meaningful <i>H</i>\u2090 <i>RR</i> to compare with a specified <i>H</i>\u2080 <i>RR</i> 
          (default = 1). This is shown by the last line of the main Support table for <i>RR</i> analyses. "
        
        str = paste0(str1, str2, str3, "As data accumulates the strength of evidence for one hypothesis over another will tend 
                       to increase.")
        
        
        html$setContent(str)
        
      }
      
    )
)
