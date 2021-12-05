#' Histogram of selected summary statistic
#'
#' @author Charlotte Soneson
#'
#' @param cbTable \code{data.frame} (such as the \code{cbTable} returned by
#'     \code{readAlevinQC} or \code{readAlevinFryQC}) containing the desired
#'     summary statistic in a column.
#' @param plotVar Character scalar giving the name of a numeric column of
#'     \code{cbTable} to plot.
#' @param axisLabel Character scalar giving the label of the selected
#'     statistic (will be displayed as the axis label in the plot).
#' @param colName Character scalar giving the name of a logical column of
#'     \code{cbTable} to use for filling the bars in the histogram.
#' @param cbName Character scalar giving the name of the set of barcodes
#'     defined by \code{colName}, used for labelling the plot legend.
#' @param firstSelColName Character scalar indicating the name of the logical
#'     column in \code{cbTable} that corresponds to the original selection of
#'     barcodes for quantification.
#'
#' @export
#'
#' @importFrom ggplot2 aes scale_fill_manual theme_bw theme
#'     labs ggplot geom_histogram
#' @import dplyr
#' @import rlang
#'
#' @return A ggplot object
#'
#' @examples
#' alevin <- readAlevinQC(system.file("extdata/alevin_example_v0.14",
#'                                    package = "alevinQC"))
#' plotAlevinHistogram(alevin$cbTable, plotVar = "dedupRate",
#'                     axisLabel = "Deduplication rate",
#'                     colName = "inFinalWhiteList",
#'                     cbName = "final whitelist")
#'
plotAlevinHistogram <- function(cbTable, plotVar = "dedupRate",
                                axisLabel = plotVar,
                                colName = "inFinalWhiteList",
                                cbName = "final whitelist",
                                firstSelColName = "inFirstWhiteList") {
    ## Check input arguments
    .assertVector(x = cbTable, type = "data.frame")
    .assertScalar(x = plotVar, type = "character",
                  validValues = colnames(cbTable))
    .assertVector(x = cbTable[[plotVar]], type = "numeric")
    .assertScalar(x = axisLabel, type = "character")
    .assertScalar(x = colName, type = "character",
                  validValues = colnames(cbTable))
    .assertVector(x = cbTable[[colName]], type = "logical")
    .assertScalar(x = cbName, type = "character")
    .assertScalar(x = firstSelColName, type = "character",
                  validValues = colnames(cbTable))
    .assertVector(x = cbTable[[firstSelColName]], type = "logical")

    ## Filter to only keep the quantified barcodes
    cbTable <- cbTable %>% dplyr::filter(.data[[firstSelColName]]) %>%
        dplyr::filter(!is.na(.data[[plotVar]]))
    nbins <- min(nrow(cbTable)/10, 100)
    ggplot2::ggplot(
        cbTable,
        ggplot2::aes(x = .data[[plotVar]],
                     fill = .data[[colName]])
    ) +
        ggplot2::geom_histogram(bins = nbins, color = "grey") +
        ggplot2::theme_bw() +
        ggplot2::scale_fill_manual(values = c(`TRUE` = "grey35",
                                              `FALSE` = "lightgrey")) +
        ggplot2::theme(legend.position = "bottom") +
        ggplot2::labs(x = axisLabel,
                      fill = paste0("Barcode included in ", cbName))
}
