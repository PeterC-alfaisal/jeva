
# This file is automatically generated, you probably don't want to edit this

l2waovOptions <- if (requireNamespace("jmvcore", quietly=TRUE)) R6::R6Class(
    "l2waovOptions",
    inherit = jmvcore::Options,
    public = list(
        initialize = function(
            dep = NULL,
            factor1 = NULL,
            factor2 = NULL,
            ct1 = FALSE,
            Ct1Values = "1,-1,...",
            ct2 = FALSE,
            Ct2Values = "1,-1,...",
            correction = "ob",
            plt = FALSE,
            ME = FALSE,
            text = TRUE, ...) {

            super$initialize(
                package="jeva",
                name="l2waov",
                requiresData=TRUE,
                ...)

            private$..dep <- jmvcore::OptionVariable$new(
                "dep",
                dep,
                required=TRUE,
                suggested=list(
                    "continuous"),
                permitted=list(
                    "numeric"),
                rejectInf=FALSE)
            private$..factor1 <- jmvcore::OptionVariable$new(
                "factor1",
                factor1,
                required=TRUE,
                rejectUnusedLevels=TRUE,
                suggested=list(
                    "nominal",
                    "ordinal"),
                permitted=list(
                    "factor"),
                default=NULL)
            private$..factor2 <- jmvcore::OptionVariable$new(
                "factor2",
                factor2,
                required=TRUE,
                rejectUnusedLevels=TRUE,
                suggested=list(
                    "nominal",
                    "ordinal"),
                permitted=list(
                    "factor"),
                default=NULL)
            private$..ct1 <- jmvcore::OptionBool$new(
                "ct1",
                ct1,
                default=FALSE)
            private$..Ct1Values <- jmvcore::OptionString$new(
                "Ct1Values",
                Ct1Values,
                default="1,-1,...")
            private$..ct2 <- jmvcore::OptionBool$new(
                "ct2",
                ct2,
                default=FALSE)
            private$..Ct2Values <- jmvcore::OptionString$new(
                "Ct2Values",
                Ct2Values,
                default="1,-1,...")
            private$..correction <- jmvcore::OptionList$new(
                "correction",
                correction,
                options=list(
                    "nc",
                    "ob",
                    "aic",
                    "aicsm"),
                default="ob")
            private$..plt <- jmvcore::OptionBool$new(
                "plt",
                plt,
                default=FALSE)
            private$..ME <- jmvcore::OptionBool$new(
                "ME",
                ME,
                default=FALSE)
            private$..text <- jmvcore::OptionBool$new(
                "text",
                text,
                default=TRUE)

            self$.addOption(private$..dep)
            self$.addOption(private$..factor1)
            self$.addOption(private$..factor2)
            self$.addOption(private$..ct1)
            self$.addOption(private$..Ct1Values)
            self$.addOption(private$..ct2)
            self$.addOption(private$..Ct2Values)
            self$.addOption(private$..correction)
            self$.addOption(private$..plt)
            self$.addOption(private$..ME)
            self$.addOption(private$..text)
        }),
    active = list(
        dep = function() private$..dep$value,
        factor1 = function() private$..factor1$value,
        factor2 = function() private$..factor2$value,
        ct1 = function() private$..ct1$value,
        Ct1Values = function() private$..Ct1Values$value,
        ct2 = function() private$..ct2$value,
        Ct2Values = function() private$..Ct2Values$value,
        correction = function() private$..correction$value,
        plt = function() private$..plt$value,
        ME = function() private$..ME$value,
        text = function() private$..text$value),
    private = list(
        ..dep = NA,
        ..factor1 = NA,
        ..factor2 = NA,
        ..ct1 = NA,
        ..Ct1Values = NA,
        ..ct2 = NA,
        ..Ct2Values = NA,
        ..correction = NA,
        ..plt = NA,
        ..ME = NA,
        ..text = NA)
)

l2waovResults <- if (requireNamespace("jmvcore", quietly=TRUE)) R6::R6Class(
    "l2waovResults",
    inherit = jmvcore::Group,
    active = list(
        text = function() private$.items[["text"]],
        anova = function() private$.items[["anova"]],
        cvals = function() private$.items[["cvals"]],
        plot = function() private$.items[["plot"]],
        tabText = function() private$.items[["tabText"]],
        SupportTab = function() private$.items[["SupportTab"]],
        MoretabText = function() private$.items[["MoretabText"]]),
    private = list(),
    public=list(
        initialize=function(options) {
            super$initialize(
                options=options,
                name="",
                title="Two-Way Factorial ANOVA")
            self$add(jmvcore::Preformatted$new(
                options=options,
                name="text",
                title="Log likelihood ratio analysis"))
            self$add(jmvcore::Table$new(
                options=options,
                name="anova",
                title="Two-Way ANOVA",
                rows=10,
                clearWith=list(
                    "factor1",
                    "factor2",
                    "data",
                    "dep",
                    "Ct1Values",
                    "Ct2Values",
                    "ct1",
                    "ct2",
                    "correction"),
                columns=list(
                    list(
                        `name`="var", 
                        `title`="Model Comparisons", 
                        `type`="text"),
                    list(
                        `name`="S", 
                        `title`="S", 
                        `type`="number"),
                    list(
                        `name`="Param", 
                        `title`="Param", 
                        `type`="number"),
                    list(
                        `name`="F", 
                        `title`="F", 
                        `type`="number"),
                    list(
                        `name`="df", 
                        `title`="df", 
                        `type`="number"),
                    list(
                        `name`="p", 
                        `title`="p", 
                        `type`="number", 
                        `format`="zto,pvalue", 
                        `refs`=list(
                            "Edwards_OR",
                            "GloverDixon",
                            "DixonPa")))))
            self$add(jmvcore::Table$new(
                options=options,
                name="cvals",
                title="Contrast values and group means",
                visible="(ct1 || ct2)",
                rows=3,
                clearWith=list(
                    "factor1",
                    "factor2",
                    "data",
                    "dep",
                    "Ct1Values",
                    "Ct2Values",
                    "ct1",
                    "ct2",
                    "correction"),
                columns=list(
                    list(
                        `name`="var", 
                        `title`=""),
                    list(
                        `name`="L1", 
                        `title`="1", 
                        `type`="number"),
                    list(
                        `name`="L2", 
                        `title`="2", 
                        `type`="number"),
                    list(
                        `name`="L3", 
                        `title`="3", 
                        `type`="number"),
                    list(
                        `name`="L4", 
                        `title`="4", 
                        `type`="number"),
                    list(
                        `name`="L5", 
                        `title`="5", 
                        `type`="number"),
                    list(
                        `name`="L6", 
                        `title`="6", 
                        `type`="number"),
                    list(
                        `name`="L7", 
                        `title`="7", 
                        `type`="number"),
                    list(
                        `name`="L8", 
                        `title`="8", 
                        `type`="number"),
                    list(
                        `name`="L9", 
                        `title`="9", 
                        `type`="number"),
                    list(
                        `name`="L10", 
                        `title`="10", 
                        `type`="number"))))
            self$add(jmvcore::Image$new(
                options=options,
                name="plot",
                title="Interaction Plot",
                width=550,
                height=400,
                renderFun=".plot",
                clearWith=list(
                    "factor1",
                    "factor2",
                    "dep",
                    "data",
                    "ME"),
                visible="(plt)"))
            self$add(jmvcore::Html$new(
                options=options,
                name="tabText",
                title="Summarizing an evidential analysis",
                visible="(text)",
                clearWith=list(
                    "factor1",
                    "factor2",
                    "data",
                    "dep",
                    "ct1",
                    "ct2",
                    "Ct1Values",
                    "Ct2Values",
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

l2waovBase <- if (requireNamespace("jmvcore", quietly=TRUE)) R6::R6Class(
    "l2waovBase",
    inherit = jmvcore::Analysis,
    public = list(
        initialize = function(options, data=NULL, datasetId="", analysisId="", revision=0) {
            super$initialize(
                package = "jeva",
                name = "l2waov",
                version = c(1,0,0),
                options = options,
                results = l2waovResults$new(options=options),
                data = data,
                datasetId = datasetId,
                analysisId = analysisId,
                revision = revision,
                pause = NULL,
                completeWhenFilled = TRUE,
                requiresMissings = FALSE,
                weightsSupport = 'auto')
        }))

#' Two-Way Factorial ANOVA
#'
#' The Analysis of Variance (ANOVA) is used to explore the relationship 
#' between a continuous dependent variable and two categorical explanatory 
#' variables. Contrasts can be specified. Support values can be corrected 
#' according to the number of parameters in the models being compared.
#' 
#'
#' @examples
#' data('ToothGrowth')
#' jeva::l2waov(data = ToothGrowth,
#' dep = len,
#' factor1 = dose,
#' factor2 = supp,
#' text = FALSE)
#' #
#' # TWO-WAY FACTORIAL ANOVA
#' #
#' # Support
#' # -----------------------------------------------------------------------------------------------
#' #   Model Comparisons                 S               Param    F            df       p
#' # -----------------------------------------------------------------------------------------------
#' #   H₀ vs full model                  -44.85628744     2, 7    41.557178    5, 54    < .0000001
#' #   H₀ versus dose                     35.40697964     4, 2    91.999965    2, 54    < .0000001
#' #   H₀ versus supp                      1.33978727     3, 2    15.571979    1, 54     0.0002312
#' #   H₀ versus both main effects        41.60841579     5, 2    66.523970    3, 54    < .0000001
#' #   H₀ versus Interaction              -0.04361461     4, 2     4.106991    2, 54     0.0218603
#' #   Full model versus main effects      3.24787166     7, 5
#' #   Full model versus interaction      44.89990206     7, 4
#' #   H₀ versus Contrast 1
#' #   H₀ versus Contrast 2
#' #   Contrast 1 versus Contrast 2
#' # -----------------------------------------------------------------------------------------------
#' #   Note. S uses Occam's Bonus correction for parameters (Param).
#' #
#'
#' @param data the data as a data frame
#' @param dep the dependent variable from \code{data}, variable must be
#'   numeric (not necessary when providing a formula, see examples)
#' @param factor1 the 1st grouping or independent variable
#' @param factor2 the 2nd grouping or independent variable
#' @param ct1 \code{TRUE} or \code{FALSE} (default), for contrast 1
#' @param Ct1Values a comma-separated list specifying the contrast
#' @param ct2 \code{TRUE} or \code{FALSE} (default), for contrast 2
#' @param Ct2Values a comma-separated list specifying the contrast
#' @param correction correction for parameters, none, Occam's bonus (default)
#'   or AIC
#' @param plt \code{TRUE} or \code{FALSE} (default), provide interaction plot
#' @param ME \code{TRUE} or \code{FALSE} (default), for showing main effects
#'   model in plot
#' @param text \code{TRUE} (default) or \code{FALSE}, how to report the
#'   results
#' @param formula (optional) the formula to use, see the examples
#' @return A results object containing:
#' \tabular{llllll}{
#'   \code{results$text} \tab \tab \tab \tab \tab a preformatted \cr
#'   \code{results$anova} \tab \tab \tab \tab \tab a table of the test results \cr
#'   \code{results$cvals} \tab \tab \tab \tab \tab a table of the test results \cr
#'   \code{results$plot} \tab \tab \tab \tab \tab an image \cr
#'   \code{results$tabText} \tab \tab \tab \tab \tab a html \cr
#'   \code{results$SupportTab} \tab \tab \tab \tab \tab a table \cr
#'   \code{results$MoretabText} \tab \tab \tab \tab \tab a html \cr
#' }
#'
#' Tables can be converted to data frames with \code{asDF} or \code{\link{as.data.frame}}. For example:
#'
#' \code{results$anova$asDF}
#'
#' \code{as.data.frame(results$anova)}
#'
#' @export
l2waov <- function(
    data,
    dep,
    factor1 = NULL,
    factor2 = NULL,
    ct1 = FALSE,
    Ct1Values = "1,-1,...",
    ct2 = FALSE,
    Ct2Values = "1,-1,...",
    correction = "ob",
    plt = FALSE,
    ME = FALSE,
    text = TRUE,
    formula) {

    if ( ! requireNamespace("jmvcore", quietly=TRUE))
        stop("l2waov requires jmvcore to be installed (restart may be required)")

    if ( ! missing(formula)) {
        if (missing(dep))
            dep <- jmvcore::marshalFormula(
                formula=formula,
                data=`if`( ! missing(data), data, NULL),
                from="lhs",
                subset="1",
                required=TRUE)
        if (missing(factor1))
            factor1 <- jmvcore::marshalFormula(
                formula=formula,
                data=`if`( ! missing(data), data, NULL),
                from="rhs")
        if (missing(factor2))
            factor2 <- jmvcore::marshalFormula(
                formula=formula,
                data=`if`( ! missing(data), data, NULL),
                from="rhs")
    }

    if ( ! missing(dep)) dep <- jmvcore::resolveQuo(jmvcore::enquo(dep))
    if ( ! missing(factor1)) factor1 <- jmvcore::resolveQuo(jmvcore::enquo(factor1))
    if ( ! missing(factor2)) factor2 <- jmvcore::resolveQuo(jmvcore::enquo(factor2))
    if (missing(data))
        data <- jmvcore::marshalData(
            parent.frame(),
            `if`( ! missing(dep), dep, NULL),
            `if`( ! missing(factor1), factor1, NULL),
            `if`( ! missing(factor2), factor2, NULL))

    for (v in factor1) if (v %in% names(data)) data[[v]] <- as.factor(data[[v]])
    for (v in factor2) if (v %in% names(data)) data[[v]] <- as.factor(data[[v]])

    options <- l2waovOptions$new(
        dep = dep,
        factor1 = factor1,
        factor2 = factor2,
        ct1 = ct1,
        Ct1Values = Ct1Values,
        ct2 = ct2,
        Ct2Values = Ct2Values,
        correction = correction,
        plt = plt,
        ME = ME,
        text = text)

    analysis <- l2waovClass$new(
        options = options,
        data = data)

    analysis$run()

    analysis$results
}

