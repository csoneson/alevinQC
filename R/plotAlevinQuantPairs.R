#' Pairs plot with quantification summary statistics
#'
#' @author Charlotte Soneson
#'
#' @param cbTable \code{data.frame} (such as the \code{cbTable} returned by
#'   \code{readAlevinQC}) with collapsed barcode frequencies, the total UMI
#'   count and the number of detected genes for each cell.
#' @param colName Character scalar giving the name of a column of
#'   \code{cbTable} to use for coloring the points.
#'
#' @export
#'
#' @importFrom GGally ggpairs ggally_cor ggally_points ggally_densityDiag
#' @importFrom ggplot2 aes scale_color_manual scale_fill_manual theme_bw
#' @import dplyr
#' @import rlang
#'
#' @return A ggmatrix object
#'
#' @examples
#' alevin <- readAlevinQC(system.file("extdata/alevin_example_pre0.14",
#'                                    package = "alevinQC"))
#' plotAlevinQuantPairs(alevin$cbTable, colName = "inFinalWhiteList")
#'
plotAlevinQuantPairs <- function(cbTable, colName = "inFinalWhiteList") {
    GGally::ggpairs(
        cbTable %>% dplyr::filter(inFirstWhiteList) %>%
            dplyr::rename(`Barcode frequency` = "collapsedFreq",
                          `Total UMI count` = "totalUMICount",
                          `Nbr detected genes` = "nbrGenesAboveZero"),
        mapping = ggplot2::aes(color = !!rlang::sym(colName)),
        columns = c("Barcode frequency", "Total UMI count",
                    "Nbr detected genes"),
        upper = list(continuous = function(data, mapping, ...) {
            GGally::ggally_cor(data = data, mapping = mapping) +
                ggplot2::scale_color_manual(values = c("darkgreen", "red"))}),
        lower = list(continuous = function(data, mapping, ...) {
            GGally::ggally_points(data = data, mapping = mapping, alpha = 0.5) +
                ggplot2::scale_color_manual(values = c("darkgreen", "red"))}),
        diag = list(continuous = function(data, mapping, ...) {
            GGally::ggally_densityDiag(data = data, mapping = mapping,
                                       alpha = 0.5) +
                ggplot2::scale_fill_manual(values = c("darkgreen", "red"))})
    ) +
        ggplot2::theme_bw()
}
