#' Histogram of selected summary statistic
#'
#' @author Charlotte Soneson
#'
#' @param cbTable \code{data.frame} (such as the \code{cbTable} returned by
#'   \code{readAlevinQC}) containing the desired summary statistic in a
#'   column.
#' @param colName Character scalar giving the name of a numeric column of
#'   \code{cbTable} to plot.
#' @param axisLabel Character scalar giving the label of the selected
#'   statistic (will be displayed as the axis label in the plot).
#'
#' @export
#'
#' @importFrom ggplot2 aes scale_color_manual scale_fill_manual theme_bw theme
#'   labs geom_point ggplot xlab ylab geom_histogram
#' @import dplyr
#' @importFrom cowplot plot_grid get_legend
#' @import rlang
#'
#' @return A ggplot object
#'
#' @examples
#' alevin <- readAlevinQC(system.file("extdata/alevin_example_v0.14",
#'                                    package = "alevinQC"))
#' plotAlevinHistogram(alevin$cbTable, colName = "dedupRate",
#'                     axisLabel = "Deduplication rate")
#'
plotAlevinHistogram <- function(cbTable, colName = "dedupRate",
                                axisLabel = colName) {
    stopifnot(is.numeric(cbTable[[colName]]))

    cbTable <- cbTable %>% dplyr::filter(!is.na(!!rlang::sym(colName)))
    nbins <- min(nrow(cbTable)/10, 100)
    ggplot2::ggplot(
        cbTable,
        ggplot2::aes(x = !!rlang::sym(colName))
    ) +
        ggplot2::geom_histogram(bins = nbins, fill = "lightgrey",
                                color = "grey") +
        ggplot2::theme_bw() +
        ggplot2::xlab(axisLabel)
}
