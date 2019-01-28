#' Knee plot of the number of detected genes per cell
#'
#' Plot the number of detected genes per cell in decreasing order.
#'
#' @author Charlotte Soneson
#'
#' @param cbTable \code{data.frame} (such as the \code{cbTable} returned by
#'   \code{readAlevinQC}) with the number of detected genes per cell.
#'
#' @export
#'
#' @importFrom ggplot2 ggplot aes geom_line xlab ylab theme_bw theme
#'   element_text
#' @import dplyr
#'
#' @return A ggplot object
#'
#' @examples
#' alevin <- readAlevinQC(system.file("extdata/alevin/neurons_900",
#'                                    package = "tximportData"))
#' plotAlevinKneeNbrGenes(alevin$cbTable)
#'
plotAlevinKneeNbrGenes <- function(cbTable) {
    ggplot2::ggplot(cbTable %>% dplyr::filter(inFirstWhiteList) %>%
                        dplyr::arrange(desc(nbrGenesAboveZero)) %>%
                        dplyr::mutate(x = seq_along(nbrGenesAboveZero)),
                    ggplot2::aes(x = x, y = nbrGenesAboveZero)) +
        ggplot2::geom_line(size = 2) +
        ggplot2::xlab("Cell barcode rank") +
        ggplot2::ylab("Number of detected genes") +
        ggplot2::theme_bw() +
        ggplot2::theme(legend.position = "none",
                       axis.title = ggplot2::element_text(size = 12))
}
