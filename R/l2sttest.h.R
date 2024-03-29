
# This file is automatically generated, you probably don't want to edit this

l2sttestOptions <- if (requireNamespace("jmvcore", quietly=TRUE)) R6::R6Class(
    "l2sttestOptions",
    inherit = jmvcore::Options,
    public = list(
        initialize = function(
            dep = NULL,
            group = NULL,
            welch = TRUE,
            nul = 0,
            alt = 0,
            lint = 2,
            correction = "ob",
            pll = FALSE,
            plotype = "lplot",
            supplot = -10,
            dtab = FALSE,
            plt = FALSE,
            text = TRUE, ...) {

            super$initialize(
                package="jeva",
                name="l2sttest",
                requiresData=TRUE,
                ...)

            private$..dep <- jmvcore::OptionVariable$new(
                "dep",
                dep,
                suggested=list(
                    "continuous"),
                permitted=list(
                    "numeric"),
                rejectInf=FALSE)
            private$..group <- jmvcore::OptionVariable$new(
                "group",
                group,
                suggested=list(
                    "nominal",
                    "ordinal"),
                permitted=list(
                    "factor"))
            private$..welch <- jmvcore::OptionBool$new(
                "welch",
                welch,
                default=TRUE)
            private$..nul <- jmvcore::OptionNumber$new(
                "nul",
                nul,
                min=-100000000000,
                max=100000000000,
                default=0)
            private$..alt <- jmvcore::OptionNumber$new(
                "alt",
                alt,
                min=-100000000000,
                max=100000000000,
                default=0)
            private$..lint <- jmvcore::OptionNumber$new(
                "lint",
                lint,
                min=1,
                max=100,
                default=2)
            private$..correction <- jmvcore::OptionList$new(
                "correction",
                correction,
                options=list(
                    "nc",
                    "ob",
                    "aic"),
                default="ob")
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
            private$..dtab <- jmvcore::OptionBool$new(
                "dtab",
                dtab,
                default=FALSE)
            private$..plt <- jmvcore::OptionBool$new(
                "plt",
                plt,
                default=FALSE)
            private$..text <- jmvcore::OptionBool$new(
                "text",
                text,
                default=TRUE)

            self$.addOption(private$..dep)
            self$.addOption(private$..group)
            self$.addOption(private$..welch)
            self$.addOption(private$..nul)
            self$.addOption(private$..alt)
            self$.addOption(private$..lint)
            self$.addOption(private$..correction)
            self$.addOption(private$..pll)
            self$.addOption(private$..plotype)
            self$.addOption(private$..supplot)
            self$.addOption(private$..dtab)
            self$.addOption(private$..plt)
            self$.addOption(private$..text)
        }),
    active = list(
        dep = function() private$..dep$value,
        group = function() private$..group$value,
        welch = function() private$..welch$value,
        nul = function() private$..nul$value,
        alt = function() private$..alt$value,
        lint = function() private$..lint$value,
        correction = function() private$..correction$value,
        pll = function() private$..pll$value,
        plotype = function() private$..plotype$value,
        supplot = function() private$..supplot$value,
        dtab = function() private$..dtab$value,
        plt = function() private$..plt$value,
        text = function() private$..text$value),
    private = list(
        ..dep = NA,
        ..group = NA,
        ..welch = NA,
        ..nul = NA,
        ..alt = NA,
        ..lint = NA,
        ..correction = NA,
        ..pll = NA,
        ..plotype = NA,
        ..supplot = NA,
        ..dtab = NA,
        ..plt = NA,
        ..text = NA)
)

l2sttestResults <- if (requireNamespace("jmvcore", quietly=TRUE)) R6::R6Class(
    "l2sttestResults",
    inherit = jmvcore::Group,
    active = list(
        text = function() private$.items[["text"]],
        l2sttest = function() private$.items[["l2sttest"]],
        l2sttest2 = function() private$.items[["l2sttest2"]],
        l2sttestd = function() private$.items[["l2sttestd"]],
        plot = function() private$.items[["plot"]],
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
                title="Independent Samples T-Test")
            self$add(jmvcore::Preformatted$new(
                options=options,
                name="text",
                title="Log likelihood ratio analysis"))
            self$add(jmvcore::Table$new(
                options=options,
                name="l2sttest",
                title="Support",
                rows=3,
                clearWith=list(
                    "group",
                    "dep",
                    "nul",
                    "alt",
                    "data",
                    "correction"),
                columns=list(
                    list(
                        `name`="var", 
                        `title`="", 
                        `type`="text"),
                    list(
                        `name`="Value", 
                        `type`="number"),
                    list(
                        `name`="mdiff", 
                        `title`="Difference", 
                        `type`="number"),
                    list(
                        `name`="sed", 
                        `title`="SE difference", 
                        `type`="number"),
                    list(
                        `name`="S", 
                        `title`="S", 
                        `type`="number"),
                    list(
                        `name`="Param", 
                        `title`="Param", 
                        `type`="number"),
                    list(
                        `name`="t", 
                        `title`="t", 
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
                            "Glover_Tut")))))
            self$add(jmvcore::Table$new(
                options=options,
                name="l2sttest2",
                title="Support Interval for mean difference",
                rows=1,
                clearWith=list(
                    "lint",
                    "data",
                    "dep",
                    "group"),
                columns=list(
                    list(
                        `name`="mdiff", 
                        `title`="Difference", 
                        `type`="number"),
                    list(
                        `name`="Lower", 
                        `type`="number"),
                    list(
                        `name`="Upper", 
                        `type`="number"))))
            self$add(jmvcore::Table$new(
                options=options,
                name="l2sttestd",
                title="Group Descriptives",
                visible="(dtab)",
                rows=2,
                clearWith=list(
                    "group",
                    "dep",
                    "data"),
                columns=list(
                    list(
                        `name`="gp", 
                        `title`="", 
                        `type`="text"),
                    list(
                        `name`="N", 
                        `type`="number"),
                    list(
                        `name`="Mean", 
                        `type`="number"),
                    list(
                        `name`="Median", 
                        `type`="number"),
                    list(
                        `name`="SD", 
                        `type`="number"),
                    list(
                        `name`="SE", 
                        `type`="number"))))
            self$add(jmvcore::Image$new(
                options=options,
                name="plot",
                title="`Means with S-{lint} support intervals`",
                width=400,
                height=300,
                renderFun=".plot",
                clearWith=list(
                    "group",
                    "dep",
                    "data",
                    "lint"),
                visible="(plt)"))
            self$add(jmvcore::Image$new(
                options=options,
                name="plotc",
                title="`Likelihood curve with S-{lint} support interval`",
                width=500,
                height=400,
                renderFun=".plotc",
                clearWith=list(
                    "group",
                    "dep",
                    "data",
                    "lint",
                    "nul",
                    "alt",
                    "logplot",
                    "lplot",
                    "plotype",
                    "supplot"),
                visible="(pll)"))
            self$add(jmvcore::Html$new(
                options=options,
                name="tabText",
                title="Summarizing an evidential analysis",
                visible="(text)",
                clearWith=list(
                    "group",
                    "dep",
                    "nul",
                    "alt",
                    "data",
                    "lint")))
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

l2sttestBase <- if (requireNamespace("jmvcore", quietly=TRUE)) R6::R6Class(
    "l2sttestBase",
    inherit = jmvcore::Analysis,
    public = list(
        initialize = function(options, data=NULL, datasetId="", analysisId="", revision=0) {
            super$initialize(
                package = "jeva",
                name = "l2sttest",
                version = c(1,0,0),
                options = options,
                results = l2sttestResults$new(options=options),
                data = data,
                datasetId = datasetId,
                analysisId = analysisId,
                revision = revision,
                pause = NULL,
                completeWhenFilled = TRUE,
                requiresMissings = FALSE,
                weightsSupport = 'auto')
        }))

#' Independent Samples T-Test
#'
#' Independent samples analysis where null and alternative hypotheses canbe 
#' specified. Likelihood interval support level can be specified and plotted 
#' with the means. A likelihood function for the difference in means can be 
#' plotted. 
#' 
#'
#' @examples
#' data('ToothGrowth')
#' jeva::l2sttest(data = ToothGrowth, dep = len, group = supp, text = FALSE)
#'
#' @param data the data as a data frame
#' @param dep a (non-empty) numeric vector of data values
#' @param group an integer vector the same length as data, coding for 2 groups
#' @param welch \code{TRUE} (default) or \code{FALSE}, perform Welch's t-test
#' @param nul difference value for the null hypothesis, default = 0
#' @param alt difference value for an alternative hypothesis, default = 0
#' @param lint likelihood interval given as support value, e.g. 2 or 3,
#'   default = 2
#' @param correction correction for parameters, none, Occam's bonus (default)
#'   or AIC
#' @param pll \code{TRUE} or \code{FALSE} (default), give the likelihood or
#'   log likelihood function showing  null hypothesis (black line), alternative
#'   hypothesis (blue line), mean (dashed line),  and specified support interval
#'   (horizontal red line)
#' @param plotype choose type of plot, likelihood function (default), support
#'   function
#' @param supplot To set the minimum likelihood display value in plot, in log
#'   units (default = -10)  affects the x-axis range
#' @param dtab \code{TRUE} or \code{FALSE} (default), provide descriptive
#'   statistics
#' @param plt \code{TRUE} or \code{FALSE} (default), give a plot of means with
#'   specified support intervals
#' @param text \code{TRUE} (default) or \code{FALSE}, how to report the
#'   results
#' @return A results object containing:
#' \tabular{llllll}{
#'   \code{results$text} \tab \tab \tab \tab \tab a preformatted \cr
#'   \code{results$l2sttest} \tab \tab \tab \tab \tab a table \cr
#'   \code{results$l2sttest2} \tab \tab \tab \tab \tab a table \cr
#'   \code{results$l2sttestd} \tab \tab \tab \tab \tab a table \cr
#'   \code{results$plot} \tab \tab \tab \tab \tab an image \cr
#'   \code{results$plotc} \tab \tab \tab \tab \tab an image \cr
#'   \code{results$tabText} \tab \tab \tab \tab \tab a html \cr
#'   \code{results$SupportTab} \tab \tab \tab \tab \tab a table \cr
#'   \code{results$MoretabText} \tab \tab \tab \tab \tab a html \cr
#' }
#'
#' Tables can be converted to data frames with \code{asDF} or \code{\link{as.data.frame}}. For example:
#'
#' \code{results$l2sttest$asDF}
#'
#' \code{as.data.frame(results$l2sttest)}
#'
#' @export
l2sttest <- function(
    data,
    dep,
    group,
    welch = TRUE,
    nul = 0,
    alt = 0,
    lint = 2,
    correction = "ob",
    pll = FALSE,
    plotype = "lplot",
    supplot = -10,
    dtab = FALSE,
    plt = FALSE,
    text = TRUE) {

    if ( ! requireNamespace("jmvcore", quietly=TRUE))
        stop("l2sttest requires jmvcore to be installed (restart may be required)")

    if ( ! missing(dep)) dep <- jmvcore::resolveQuo(jmvcore::enquo(dep))
    if ( ! missing(group)) group <- jmvcore::resolveQuo(jmvcore::enquo(group))
    if (missing(data))
        data <- jmvcore::marshalData(
            parent.frame(),
            `if`( ! missing(dep), dep, NULL),
            `if`( ! missing(group), group, NULL))

    for (v in group) if (v %in% names(data)) data[[v]] <- as.factor(data[[v]])

    options <- l2sttestOptions$new(
        dep = dep,
        group = group,
        welch = welch,
        nul = nul,
        alt = alt,
        lint = lint,
        correction = correction,
        pll = pll,
        plotype = plotype,
        supplot = supplot,
        dtab = dtab,
        plt = plt,
        text = text)

    analysis <- l2sttestClass$new(
        options = options,
        data = data)

    analysis$run()

    analysis$results
}

