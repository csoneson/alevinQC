#' Plot quantification statistics
#'
#' @author Charlotte Soneson
#'
#' @param quantbcs data.frame with barcode frequencies (as returned by
#'   \code{readAlevinQC}).
#'
#' @export
#'
#' @importFrom GGally ggpairs ggally_cor ggally_points ggally_densityDiag
#' @importFrom ggplot2 aes scale_color_manual scale_fill_manual theme_bw
#' @import dplyr
#'
#' @return A ggplot object
#'
plotAlevinQuantPairs <- function(quantbcs) {
    GGally::ggpairs(
        quantbcs %>%
            dplyr::rename(`Barcode frequency` = "collapsedFreq",
                          `Total UMI count` = "totalUMICount",
                          `Nbr detected genes` = "nbrGenes2"),
        mapping = ggplot2::aes(color = inFinalWhiteList),
        columns = c("Barcode frequency", "Total UMI count",
                    "Nbr detected genes"),
        upper = list(continuous = function(data, mapping, ...) {
            GGally::ggally_cor(data = data, mapping = mapping) +
                ggplot2::scale_color_manual(values = c("darkgreen", "red"))}),
        lower = list(continuous = function(data, mapping, ...) {
            GGally::ggally_points(data = data, mapping = mapping, alpha = 0.5) +
                ggplot2::scale_color_manual(values = c("darkgreen", "red"))}),
        diag = list(continuous = function(data, mapping, ...) {
            GGally::ggally_densityDiag(data = data, mapping = mapping, alpha = 0.5) +
                ggplot2::scale_fill_manual(values = c("darkgreen", "red"))})
    ) +
        ggplot2::theme_bw()
}
