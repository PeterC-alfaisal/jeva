
# This file is automatically generated, you probably don't want to edit this

propOptions <- if (requireNamespace("jmvcore", quietly=TRUE)) R6::R6Class(
    "propOptions",
    inherit = jmvcore::Options,
    public = list(
        initialize = function(
            var = NULL,
            counts = NULL,
            expected = FALSE,
            bi = FALSE,
            ciWidth = 95,
            lint = 2,
            pll = FALSE,
            plotype = "lplot",
            supplot = -10,
            correction = "ob",
            ratio = NULL,
            ratio2 = NULL,
            varA = FALSE,
            text = TRUE, ...) {

            super$initialize(
                package="jeva",
                name="prop",
                requiresData=TRUE,
                ...)

            private$..var <- jmvcore::OptionVariable$new(
                "var",
                var,
                suggested=list(
                    "nominal",
                    "ordinal"),
                permitted=list(
                    "factor"))
            private$..counts <- jmvcore::OptionVariable$new(
                "counts",
                counts,
                suggested=list(
                    "continuous"),
                default=NULL,
                permitted=list(
                    "numeric"))
            private$..expected <- jmvcore::OptionBool$new(
                "expected",
                expected,
                default=FALSE)
            private$..bi <- jmvcore::OptionBool$new(
                "bi",
                bi,
                default=FALSE)
            private$..ciWidth <- jmvcore::OptionNumber$new(
                "ciWidth",
                ciWidth,
                min=50,
                max=99.9,
                default=95)
            private$..lint <- jmvcore::OptionNumber$new(
                "lint",
                lint,
                min=1,
                max=10,
                default=2)
            private$..pll <- jmvcore::OptionBool$new(
                "pll",
                pll,
                default=FALSE)
            private$..plotype <- jmvcore::OptionList$new(
                "plotype",
                plotype,
                options=list(
                    "lplot",
                    "logplot"),
                default="lplot")
            private$..supplot <- jmvcore::OptionNumber$new(
                "supplot",
                supplot,
                min=-100,
                max=-1,
                default=-10)
            private$..correction <- jmvcore::OptionList$new(
                "correction",
                correction,
                options=list(
                    "nc",
                    "ob",
                    "aic"),
                default="ob")
            private$..ratio <- jmvcore::OptionArray$new(
                "ratio",
                ratio,
                template=jmvcore::OptionNumber$new(
                    "ratio",
                    NULL,
                    min=0,
                    default=1),
                default=NULL)
            private$..ratio2 <- jmvcore::OptionArray$new(
                "ratio2",
                ratio2,
                template=jmvcore::OptionNumber$new(
                    "ratio2",
                    NULL,
                    min=0,
                    default=1),
                default=NULL)
            private$..varA <- jmvcore::OptionBool$new(
                "varA",
                varA,
                default=FALSE)
            private$..text <- jmvcore::OptionBool$new(
                "text",
                text,
                default=TRUE)

            self$.addOption(private$..var)
            self$.addOption(private$..counts)
            self$.addOption(private$..expected)
            self$.addOption(private$..bi)
            self$.addOption(private$..ciWidth)
            self$.addOption(private$..lint)
            self$.addOption(private$..pll)
            self$.addOption(private$..plotype)
            self$.addOption(private$..supplot)
            self$.addOption(private$..correction)
            self$.addOption(private$..ratio)
            self$.addOption(private$..ratio2)
            self$.addOption(private$..varA)
            self$.addOption(private$..text)
        }),
    active = list(
        var = function() private$..var$value,
        counts = function() private$..counts$value,
        expected = function() private$..expected$value,
        bi = function() private$..bi$value,
        ciWidth = function() private$..ciWidth$value,
        lint = function() private$..lint$value,
        pll = function() private$..pll$value,
        plotype = function() private$..plotype$value,
        supplot = function() private$..supplot$value,
        correction = function() private$..correction$value,
        ratio = function() private$..ratio$value,
        ratio2 = function() private$..ratio2$value,
        varA = function() private$..varA$value,
        text = function() private$..text$value),
    private = list(
        ..var = NA,
        ..counts = NA,
        ..expected = NA,
        ..bi = NA,
        ..ciWidth = NA,
        ..lint = NA,
        ..pll = NA,
        ..plotype = NA,
        ..supplot = NA,
        ..correction = NA,
        ..ratio = NA,
        ..ratio2 = NA,
        ..varA = NA,
        ..text = NA)
)

propResults <- if (requireNamespace("jmvcore", quietly=TRUE)) R6::R6Class(
    "propResults",
    inherit = jmvcore::Group,
    active = list(
        props = function() private$.items[["props"]],
        text = function() private$.items[["text"]],
        tests = function() private$.items[["tests"]],
        ctt2 = function() private$.items[["ctt2"]],
        ctt3 = function() private$.items[["ctt3"]],
        plotc = function() private$.items[["plotc"]],
        tabText = function() private$.items[["tabText"]],
        SupportTab = function() private$.items[["SupportTab"]],
        MoretabText = function() private$.items[["MoretabText"]]),
    private = list(),
    public=list(
        initialize=function(options) {
            super$initialize(
                options=options,
                name="",
                title="N Outcomes")
            self$add(jmvcore::Table$new(
                options=options,
                name="props",
                title="`Proportions - ${var}`",
                rows="(levels(var))",
                clearWith=list(
                    "var",
                    "ratio",
                    "ratio2",
                    "counts",
                    "data"),
                refs=list(
                    "Book",
                    "Glover_Tut",
                    "Edwards_OR"),
                columns=list(
                    list(
                        `name`="level", 
                        `title`="Level", 
                        `type`="text", 
                        `content`="($key)"),
                    list(
                        `name`="name[obs]", 
                        `title`="", 
                        `type`="text", 
                        `content`="Observed", 
                        `visible`="(expected)"),
                    list(
                        `name`="count[obs]", 
                        `title`="Count", 
                        `type`="integer"),
                    list(
                        `name`="prop[obs]", 
                        `title`="Proportion", 
                        `type`="number"),
                    list(
                        `name`="name[exp]", 
                        `title`="", 
                        `type`="text", 
                        `content`="H\u2080 Expected", 
                        `visible`="(expected)"),
                    list(
                        `name`="count[exp]", 
                        `title`="Count", 
                        `type`="number", 
                        `visible`="(expected)"),
                    list(
                        `name`="prop[exp]", 
                        `title`="Proportion", 
                        `type`="number", 
                        `visible`="(expected)"))))
            self$add(jmvcore::Preformatted$new(
                options=options,
                name="text",
                title="Log likelihood ratio analysis"))
            self$add(jmvcore::Table$new(
                options=options,
                name="tests",
                title="Support",
                rows=3,
                clearWith=list(
                    "var",
                    "ratio",
                    "ratio2",
                    "counts",
                    "data",
                    "correction"),
                columns=list(
                    list(
                        `name`="var", 
                        `title`="Hypotheses", 
                        `type`="text"),
                    list(
                        `name`="rat", 
                        `title`="Ratio", 
                        `type`="text"),
                    list(
                        `name`="Values", 
                        `title`="Expected values", 
                        `type`="text"),
                    list(
                        `name`="S", 
                        `title`="S", 
                        `type`="number"),
                    list(
                        `name`="Param", 
                        `type`="number"),
                    list(
                        `name`="G", 
                        `title`="G", 
                        `type`="number"),
                    list(
                        `name`="df", 
                        `title`="df", 
                        `type`="integer"),
                    list(
                        `name`="p", 
                        `title`="p", 
                        `type`="number", 
                        `format`="zto,pvalue", 
                        `refs`=list(
                            "Edwards_OR")))))
            self$add(jmvcore::Table$new(
                options=options,
                name="ctt2",
                title="Intervals for Proportion",
                clearWith=list(
                    "var",
                    "ratio",
                    "ratio2",
                    "counts",
                    "lint",
                    "ciWidth",
                    "data"),
                visible="(bi)",
                rows=2,
                columns=list(
                    list(
                        `name`="Interval", 
                        `title`="Type of interval", 
                        `type`="text"),
                    list(
                        `name`="Level", 
                        `type`="text"),
                    list(
                        `name`="P", 
                        `type`="number"),
                    list(
                        `name`="Lower", 
                        `type`="number"),
                    list(
                        `name`="Upper", 
                        `type`="number", 
                        `refs`=list(
                            "Pritikin")))))
            self$add(jmvcore::Table$new(
                options=options,
                name="ctt3",
                title="Variance analyses",
                visible="(varA)",
                rows=2,
                clearWith=list(
                    "var",
                    "ratio",
                    "ratio2",
                    "counts"),
                columns=list(
                    list(
                        `name`="var", 
                        `title`="Hypotheses", 
                        `type`="text"),
                    list(
                        `name`="Sv", 
                        `title`="S", 
                        `type`="number"),
                    list(
                        `name`="X2", 
                        `title`="\u03C7\u00B2", 
                        `type`="number"),
                    list(
                        `name`="dfv", 
                        `title`="df", 
                        `type`="integer"),
                    list(
                        `name`="pv", 
                        `title`="p", 
                        `type`="number", 
                        `format`="zto,pvalue"),
                    list(
                        `name`="pv1", 
                        `title`="1 - p", 
                        `type`="number", 
                        `format`="zto,pvalue", 
                        `refs`=list(
                            "EdwardsVA")))))
            self$add(jmvcore::Image$new(
                options=options,
                name="plotc",
                title="`Likelihood curve for Proportion with S-{lint} support interval`",
                width=500,
                height=400,
                renderFun=".plotc",
                visible="(pll)",
                clearWith=list(
                    "data",
                    "var",
                    "ratio",
                    "ratio2",
                    "counts",
                    "lint",
                    "logplot",
                    "lplot",
                    "plotype",
                    "supplot")))
            self$add(jmvcore::Html$new(
                options=options,
                name="tabText",
                title="Summarizing an evidential analysis",
                visible="(text)",
                clearWith=list(
                    "var",
                    "ratio",
                    "ratio2",
                    "counts",
                    "correction")))
            self$add(jmvcore::Table$new(
                options=options,
                name="SupportTab",
                title="Interpreting Support S (log LR)",
                rows=7,
                visible="(text)",
                columns=list(
                    list(
                        `name`="SS", 
                        `title`="S", 
                        `type`="number"),
                    list(
                        `name`="LR", 
                        `title`="LR", 
                        `type`="number"),
                    list(
                        `name`="Interp", 
                        `title`="Interpretation Comparing Hypotheses", 
                        `type`="text"))))
            self$add(jmvcore::Html$new(
                options=options,
                name="MoretabText",
                title="More details about summaries",
                visible="(text)",
                refs=list(
                    "Book")))}))

propBase <- if (requireNamespace("jmvcore", quietly=TRUE)) R6::R6Class(
    "propBase",
    inherit = jmvcore::Analysis,
    public = list(
        initialize = function(options, data=NULL, datasetId="", analysisId="", revision=0) {
            super$initialize(
                package = "jeva",
                name = "prop",
                version = c(1,0,0),
                options = options,
                results = propResults$new(options=options),
                data = data,
                datasetId = datasetId,
                analysisId = analysisId,
                revision = revision,
                pause = NULL,
                completeWhenFilled = TRUE,
                requiresMissings = FALSE,
                weightsSupport = 'integerOnly')
        }))

#' N Outcomes
#'
#' The one-way analysis examines how well the observed proportions match those 
#' expected according to a null hypothesis or a specified hypothesis. 
#' 
#'
#' @examples
#' dat <- data.frame(Sex = c('Female', 'Male'), Students = c(60, 40))
#'
#'
#' jeva::prop(formula = Students ~ Sex, data = dat, ratio = c(1, 1), text = FALSE)
#'
#' # N OUTCOMES
#' #
#' # Proportions - Sex
#' # ---------------------------------
#' #   Level     Count    Proportion
#' # ---------------------------------
#' #   Female       60     0.6000000
#' #   Male         40     0.4000000
#' # ---------------------------------
#' #
#' #
#' # Support
#' # ----------------------------------------------------------------------------------------------------------
#' #   Hypotheses                    Expected values    S             Param    G           df    p
#' # -------------------------------------------------------------------------------------------------------
#' #   H₀ vs observed proportions    50 | 50            -1.5135514     1, 2    4.027103     1    0.0447748
#' #   Ha vs observed proportion     50 | 50            -2.0135514     2, 2    4.027103     1    0.0447748
#' #   Ha vs H₀                                         -0.5000000     2, 1
#' # -------------------------------------------------------------------------------------------------------
#' #   Note. S uses Occam's Bonus correction for parameters (Param).
#' #
#'
#' @param data the data as a data frame
#' @param var categorical or ordinal variable of interest
#' @param counts the counts in \code{data}
#' @param expected \code{TRUE} or \code{FALSE} (default), whether expected
#'   counts should be displayed
#' @param bi \code{TRUE} or \code{FALSE} (default), only if there are 2 levels
#'   of variable
#' @param ciWidth a number between 50 and 99.9 (default: 95), width of the
#'   confidence intervals to provide
#' @param lint a number between 1 and 100 (default: 2) specifying the
#'   likelihood support interval width
#' @param pll for binomial only: plot the likelihood function displaying
#'   observed proportion,  alternative hypothesis and support interval
#' @param plotype choose type of plot, likelihood function (default), support
#'   function
#' @param supplot To set the minimum likelihood display value in plot, in log
#'   units (default = -10)  affects the x-axis range
#' @param correction correction for parameters, none, Occam's bonus (default)
#'   or AIC
#' @param ratio a vector of numbers: the expected proportions
#' @param ratio2 a vector of numbers: the expected proportions
#' @param varA \code{TRUE} or \code{FALSE} (default), perform variance
#'   analysis for null and alternative hypotheses
#' @param text \code{TRUE} (default) or \code{FALSE}, how to report the
#'   results
#' @param formula (optional) the formula to use, see the examples
#' @return A results object containing:
#' \tabular{llllll}{
#'   \code{results$props} \tab \tab \tab \tab \tab a table of the proportions \cr
#'   \code{results$text} \tab \tab \tab \tab \tab a preformatted \cr
#'   \code{results$tests} \tab \tab \tab \tab \tab a table of the test results \cr
#'   \code{results$ctt2} \tab \tab \tab \tab \tab a table \cr
#'   \code{results$ctt3} \tab \tab \tab \tab \tab a table \cr
#'   \code{results$plotc} \tab \tab \tab \tab \tab an image \cr
#'   \code{results$tabText} \tab \tab \tab \tab \tab a html \cr
#'   \code{results$SupportTab} \tab \tab \tab \tab \tab a table \cr
#'   \code{results$MoretabText} \tab \tab \tab \tab \tab a html \cr
#' }
#'
#' Tables can be converted to data frames with \code{asDF} or \code{\link{as.data.frame}}. For example:
#'
#' \code{results$props$asDF}
#'
#' \code{as.data.frame(results$props)}
#'
#' @export
prop <- function(
    data,
    var,
    counts = NULL,
    expected = FALSE,
    bi = FALSE,
    ciWidth = 95,
    lint = 2,
    pll = FALSE,
    plotype = "lplot",
    supplot = -10,
    correction = "ob",
    ratio = NULL,
    ratio2 = NULL,
    varA = FALSE,
    text = TRUE,
    formula) {

    if ( ! requireNamespace("jmvcore", quietly=TRUE))
        stop("prop requires jmvcore to be installed (restart may be required)")

    if ( ! missing(formula)) {
        if (missing(counts))
            counts <- jmvcore::marshalFormula(
                formula=formula,
                data=`if`( ! missing(data), data, NULL),
                from="lhs",
                subset="1")
        if (missing(var))
            var <- jmvcore::marshalFormula(
                formula=formula,
                data=`if`( ! missing(data), data, NULL),
                from="rhs",
                subset="1")
    }

    if ( ! missing(var)) var <- jmvcore::resolveQuo(jmvcore::enquo(var))
    if ( ! missing(counts)) counts <- jmvcore::resolveQuo(jmvcore::enquo(counts))
    if (missing(data))
        data <- jmvcore::marshalData(
            parent.frame(),
            `if`( ! missing(var), var, NULL),
            `if`( ! missing(counts), counts, NULL))

    for (v in var) if (v %in% names(data)) data[[v]] <- as.factor(data[[v]])

    options <- propOptions$new(
        var = var,
        counts = counts,
        expected = expected,
        bi = bi,
        ciWidth = ciWidth,
        lint = lint,
        pll = pll,
        plotype = plotype,
        supplot = supplot,
        correction = correction,
        ratio = ratio,
        ratio2 = ratio2,
        varA = varA,
        text = text)

    analysis <- propClass$new(
        options = options,
        data = data)

    analysis$run()

    analysis$results
}

