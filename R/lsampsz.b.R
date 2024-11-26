
# This file is a generated template, your changes will not be overwritten

lsampszClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "lsampszClass",
    inherit = lsampszBase,
    private = list(
      .init=function() {
        
        ttype <- self$options$ttype
        
        if(ttype == "is") { tabtxt <- "Independent samples test"
        } else { tabtxt <- "Paired samples or one sample test" }
        
        table <- self$results$tab
        table$setTitle(.("Evidential sample size estimate"))
        table$setRow(rowNo=1, values=list(var= tabtxt))
      },
      .run = function() {
        
        ttype <- self$options$ttype
        MW <- self$options$MW
        S <- self$options$S
        d <- self$options$d
        toler <- 0.0001   # for optimize function
        ns <- 0; nul <- 0
        to <- vector(); tmw <- vector(); td <- vector()
        
        if(ttype == "is") { paired = FALSE
        } else { paired = TRUE}
        
        if(paired) {
          
          f <- function(n, MW, nul, d,  S) {
            tmw <- qt(MW,df=n-1, lower.tail =TRUE)
            suppressWarnings(Xu <- nul+(d*(1-sqrt((exp(-(2*S/n))-(1-exp(-(2*S/n)))^2*(n-1)/
                                    (n*d^2))))/((1-exp(-(2*S/n))))))
            to <- (Xu-d)*sqrt(n)
            td <- (to-tmw)^2
          }
          suppressWarnings(xmin1 <- optimize(f, c(3, 10000000), tol = toler, MW, nul, d,  S))
          ns <- ceiling(xmin1$minimum)
          
          table <- self$results$tab
          table$setRow(rowNo=1, values=list(MW=MW, S=S, d=d, N=ns))
          
        } else {
          
          g <- function(n, MW, nul, d,  S) {
            tmw <- qt(MW,df=n-2, lower.tail =TRUE)
            suppressWarnings(Xu <- nul+(d*(1-sqrt((exp(-(2*S/n))-(1-exp(-(2*S/n)))^2*(n-2)/
                                    (n*d^2))))/((1-exp(-(2*S/n))))))
            to <- (Xu-d)*sqrt(n)/(2)
            td <- (to-tmw)^2
          }
          suppressWarnings(xmin1 <- optimize(g, c(3, 10000000), tol = toler, MW, nul, d,  S))
          ns <- ceiling(xmin1$minimum)   # use ceiling rather than round, to get to nearest N
          if((ns %% 2) != 0)  ns <- ns + 1         # make number even for division by 2
          ns <- as.integer(ns)
          
          table <- self$results$tab
          if(self$options$ttype=="is") { 
            table$setNote('Note', "Divide the given sample size by 2 to get the number for each group")
          }
          table$setRow(rowNo=1, values=list(MW=MW, S=S, d=d, N=ns))
          
        }
        
        g <- data.frame(ns=ns, S=S, d=d)
        imagep <- self$results$plotp
        imagep$setState(g)
        
        if(isTRUE(self$options$plwm)) {
          
          plotp <- self$results$plotp
          plotp$setVisible(TRUE)
        }
        
        private$.populatePlotText()
        private$.populateExpText()
        
      },
      
      #### Plot functions ----
      
      .plotp=function(imagep, ...) {
        
        g <- imagep$state
        
        if(self$options$ttype == "is") { 
          dfs <- 2
          sdm <- 2
        } else { 
          dfs <- 1 
          sdm <- 1
        }
        
        d <- g$d
        S10 <- g$S
        null <- 0
        maxsample <- 2*g$ns + g$ns/1.5
        
        if(self$options$tail1 == "onet") { alp <- self$options$alpha
        } else { alp <- self$options$alpha/2}
        
        dp <- 0
        ns <- seq(1,maxsample)
        
        M0line <- function(maxsample,null,d,S10,dfs) { for(x in 1:maxsample) { 
          suppressWarnings(dp[x] <- pt((d-(null+(d*(1-sqrt((exp(-(2*S10/x))
                                                            -(1-exp(-(2*S10/x)))^2*(x-dfs)/(x*d^2))))/((1-exp(-(2*S10/x))))))-d)*sqrt(x)/(sdm),x-dfs))
          if(is.na(dp[x])) { dp[x] <- NA }
        }
          return(dp)
        }
        
        Betaline <- function(maxsample,null,d,S10,dfs) { for(x in 1:maxsample) { 
          suppressWarnings(dp[x] <- pt(qt(1-alp,x-dfs)-d*sqrt(x)/sdm,x-dfs))
          if(is.na(dp[x])) { dp[x] <- NA }
        }
          return(dp)
        }
        
        Weakline <- function(maxsample,null,d,S10,dfs) { for(x in 1:maxsample) { 
          suppressWarnings(dp[x] <- pt((null+(d*(1-sqrt((exp(-(2*S10/x))
                                                         -(1-exp(-(2*S10/x)))^2*(x-dfs)/(x*d^2))))/((1-exp(-(2*S10/x)))))
                                        -d)*sqrt(x)/(sdm),x-dfs)-pt((d-(null+(d*
                                                                                (1-sqrt((exp(-(2*S10/x))-(1-exp(-(2*S10/x)))^2*(x-dfs)/(x*d^2))))
                                                                              /((1-exp(-(2*S10/x))))))-d)*sqrt(x)/(sdm),x-dfs))
          if(is.na(dp[x])) { dp[x] <- NA }
        }
          return(dp)
        }
        
        Weak=Weakline(maxsample,null,d,S10,dfs)
        
        Misldline <- function(maxsample,null,d,S10,dfs) { for(x in 1:maxsample) { 
          suppressWarnings(dp[x] <- pt((d-(null+(d*(1-sqrt((exp(-(2*S10/x))
                                                            -(1-exp(-(2*S10/x)))^2*(x-dfs)/(x*d^2))))/((1-exp(-(2*S10/x))))))
                                        -d)*sqrt(x)/(sdm),x-dfs))
          if(is.na(dp[x])) { dp[x] <- NA }
        }
          return(dp)
        }
        
        M0=M0line(maxsample,null,d,S10,dfs)
        Beta=Betaline(maxsample,null,d,S10,dfs)
        Miss=Misldline(maxsample,null,d,S10,dfs)
        
        MWl=Weak + Miss
        
        Sexp <- bquote(~italic(S) == .(S10))
        dexp <- bquote(~italic(d) == .(d))
        alexp <- bquote(alpha == .(self$options$alpha))

        if (self$options$tail1 == "onet") { tailtx <- "One-tailed test"
        } else { betex <- tailtx <- "Two-tailed test" 
        }
        
        plot(ns,M0, type="l", lwd = 1, ylim=c(0,0.2), xlab = "Sample Size", 
             ylab = "Probability")
        lines(ns, Beta, lty = 2)
        lines(ns, MWl, lty = 1, lwd = 2)
        
        lines(c(0,maxsample),c(self$options$alpha,self$options$alpha),lty=5, col="green") # add alpha
        text(x=0,y=self$options$alpha,pos=3,label = bquote(alpha),cex=1.5)
        text(x=maxsample/1.5,y=0.185,pos=4,label = Sexp,cex=1.5)
        text(x=maxsample/1.5,y=0.17,pos=4,label = dexp,cex=1.5)
        text(x=maxsample/1.5,y=0.155,pos=4,label = alexp,cex=1.5)
        text(x=maxsample/1.5,y=0.140,pos=4,label = tailtx,cex=1.2)
        lines(c(maxsample/1.5, maxsample/1.5+maxsample/10),c(0.11,0.11)) 
        text(x=maxsample/1.5+maxsample/10,y=0.11,pos=4,label = expression("M"[0]),cex=1.5)
        lines(c(maxsample/1.5, maxsample/1.5+maxsample/10),c(0.095,0.095), lwd=2) 
        text(x=maxsample/1.5+maxsample/10,y=0.095,pos=4,label = "M + W",cex=1.5)
        lines(c(maxsample/1.5, maxsample/1.5+maxsample/10),c(0.08,0.08), lty=2) 
        text(x=maxsample/1.5+maxsample/10,y=0.08,pos=4,label = bquote(beta), lty=2, cex=1.5)
        TRUE
        
      },
      
      .populatePlotText = function() {
        
        html <- self$results$plotText
        
        str =   "<h2>Plot Explanation</h2> <br> 
  Probabilities of misleading M and weak W evidence plotted against sample size for specified effect size and strength of evidence. 
  The probability of obtaining misleading evidence against the H\u2080 when it is true is represented by M\u2080 (thin continuous line). 
  The line for M + W (thick solid line) represents the probability of 
  obtaining strong evidence pointing the wrong way and of obtaining insufficiently strong evidence when the alternative H\u2090 
  is true. The Type II error probability (\u03B2) is represented by the small dashed curve, and is plotted for the specified 
  Type I error rate (\u03B1). The top of its curve near probability of .2 (power of .8) is near to the usual value selected 
  for frequentist sample size estimation. The sample size estimated using \u03B2 will typically be less than the evidential 
  analysis estimate (e.g. using M + W probability of .05). The Type I error probability (\u03B1) is represented by the long 
  green dashed horizontal line. Probabilities are undefined below certain sample sizes."
        
        html$setContent(str)
      },
      
      .populateExpText = function() {
        
        
        html <- self$results$exptext
        str =   "<h2>Explanation of Evidential Sample Size Calculation</h2> <br>
  In the evidential framework, Royall provided two different probabilities by which a study may fail (Royall, 1997, 2000). 
  These are the probabilities for <b>weak</b> and for <b>misleading</b> evidence. These differ conceptually from Type I and 
  Type II error probabilities. The evidence is too weak if the evidence fails to reach pre-specified values. For example, 
  the evidence would be too weak if the calculated log likelihood ratio (<i>S</i>) lies within the limits −3 and +3. 
  By contrast, the evidence is regarded as misleading if the evidence supports the wrong hypothesis beyond the pre-specified 
  value of <i>S</i> (e.g. outside the limits −3 and +3).
  <br> In the frequentist approach, the probability of a Type I error is constant regardless of the sample size, since only 
  the null hypothesis is tested. In the evidential approach where two hypotheses are compared, both the probabilities of weak 
  and misleading evidence decrease with sample size, for a specified standardized effect size <i>d</i>.
  <br> <b>Estimating sample size</b> <br>
  A number of issues should be kept in mind when estimating sample sizes. First, by making the sample size large enough, 
  the probabilities of obtaining weak and misleading evidence can be held below any specified level. Second, the probability 
  of misleading evidence is small for any sample size. With a reasonable sample size, this probability becomes negligible. 
  Third, the sample size calculations are driven largely by the need to reduce the probability of obtaining weak evidence. 
  Large samples are needed to have a high probability of obtaining sufficiently strong evidence. Finally, the evidential 
  approach requires us to plan to collect more data than if we were using the power (1 — \u03B2) approach. However, as the study 
  progresses, <i>S</i> can be calculated throughout data acquisition. Once the evidence is believed to be strong enough, 
  e.g. <i>S</i> = 3, then data collection can stop. The Neyman-Pearson approach is conditioned by the number of times the 
  data is tested for statistical significance.
  
  <br> <h2>Examples</h2>
  <b>One sample test:</b> with a large effect size <i>d</i> = 1, M + W = 0.05 and <i>S</i> = 3, a sample size of at least 25 is 
  required if the probability is to fall below .05. By comparison, a frequentist analysis using a two-tailed test with 
  \u03B2 = .05 and \u03B1 = .05 would calculate a sample size of 16.
  <br> <b>Independent samples test:</b> using the same criteria as the previous example, a sample size of least 60 (30 
  in each group) would be needed. The corresponding sample size using the frequentist approach would be 54 (27 in each group). 
  <br>For some calculations the estimated samples using the two methods will coincide. For example, with an independent samples 
  test, using <i>d</i> = 1.2, <i>S</i> = 2, M + W = .05 versus \u03B2 = .05 and \u03B1 = .05 two-tailed test, both give a 
  total sample size of 40.
  
  
  <br> <h2>Final Comment</h2>
  For paired or one sample tests, the evidential approach can demand up to 60% larger sample sizes. The opposite is true
  for independent samples tests, which generally require smaller sample sizes (even up to 50% smaller). These are ballpark
  estimates, which in practice can be modified while data is gathered. Continous monitoring can be done during data 
  collection. Monitoring can involve updating calculations of the strength and direction of the evidence, as well as 
  the calculation of the likelihood function with a suitable log likelihood interval (Cahusac, 2020). Data collection can be 
  stopped once sufficiently strong evidence is obtained, e.g. <i>S</i> = 3 (Royall, 1997). This may even occur before 
  the recommended sample size is reached. If strong evidence fails to be obtained using the allocated sample size, 
  then it is still statistically legitimate to add further observations using the evidential approach (Royall, 2004)."
        
        
        
        html$setContent(str)
        
      }
      
    )
)
