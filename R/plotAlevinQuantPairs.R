#' Pairs plot with quantification summary statistics
#'
#' @author Charlotte Soneson
#'
#' @param cbTable \code{data.frame} (such as the \code{cbTable} returned by
#'     \code{readAlevinQC} or \code{readAlevinFryQC}) with collapsed barcode
#'     frequencies, the total UMI count and the number of detected genes
#'     for each cell.
#' @param colName Character scalar giving the name of a logical column of
#'     \code{cbTable} to use for coloring the points.
#' @param firstSelColName Character scalar indicating the name of the logical
#'     column in \code{cbTable} that corresponds to the original selection of
#'     barcodes for quantification.
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
#' alevin <- readAlevinQC(system.file("extdata/alevin_example_v0.14",
#'                                    package = "alevinQC"))
#' plotAlevinQuantPairs(alevin$cbTable, colName = "inFinalWhiteList")
#'
plotAlevinQuantPairs <- function(cbTable, colName = "inFinalWhiteList",
                                 firstSelColName = "inFirstWhiteList") {
    ## Check input arguments
    .assertVector(x = cbTable, type = "data.frame")
    .assertScalar(x = colName, type = "character",
                  validValues = colnames(cbTable))
    .assertVector(x = cbTable[[colName]], type = "logical")
    .assertScalar(x = firstSelColName, type = "character",
                  validValues = colnames(cbTable))
    .assertVector(x = cbTable[[firstSelColName]], type = "logical")
    stopifnot(all(c("collapsedFreq", "totalUMICount",
                    "nbrGenesAboveZero") %in% colnames(cbTable)))

    GGally::ggpairs(
        cbTable %>% dplyr::filter(.data[[firstSelColName]]) %>%
            dplyr::rename(`Barcode frequency` = "collapsedFreq",
                          `Total UMI count` = "totalUMICount",
                          `Nbr detected genes` = "nbrGenesAboveZero"),
        mapping = ggplot2::aes(color = .data[[colName]]),
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
