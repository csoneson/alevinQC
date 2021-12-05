#' Knee plot of the number of detected genes per cell
#'
#' Plot the number of detected genes per cell in decreasing order.
#' Only cells contained in the original whitelist are considered.
#'
#' @author Charlotte Soneson
#'
#' @param cbTable \code{data.frame} (such as the \code{cbTable} returned by
#'     \code{readAlevinQC} or \code{readAlevinFryQC}) with the number of
#'     detected genes per cell.
#' @param firstSelColName Character scalar indicating the name of the logical
#'     column in \code{cbTable} that corresponds to the original selection of
#'     barcodes for quantification.
#'
#' @export
#'
#' @importFrom ggplot2 ggplot aes geom_line xlab ylab theme_bw theme
#'     element_text
#' @import dplyr
#' @importFrom rlang .data
#'
#' @return A ggplot object
#'
#' @examples
#' alevin <- readAlevinQC(system.file("extdata/alevin_example_v0.14",
#'                                    package = "alevinQC"))
#' plotAlevinKneeNbrGenes(alevin$cbTable)
#'
plotAlevinKneeNbrGenes <- function(cbTable,
                                   firstSelColName = "inFirstWhiteList") {
    ## Check input arguments
    .assertVector(x = cbTable, type = "data.frame")
    .assertScalar(x = firstSelColName, type = "character",
                  validValues = colnames(cbTable))
    .assertVector(x = cbTable[[firstSelColName]], type = "logical")
    stopifnot(all(c("nbrGenesAboveZero") %in%
                      colnames(cbTable)))

    ggplot2::ggplot(cbTable %>% dplyr::filter(.data[[firstSelColName]]) %>%
                        dplyr::arrange(dplyr::desc(.data$nbrGenesAboveZero)) %>%
                        dplyr::mutate(x = seq_along(.data$nbrGenesAboveZero)),
                    ggplot2::aes(x = .data$x, y = .data$nbrGenesAboveZero)) +
        ggplot2::geom_line(size = 2) +
        ggplot2::xlab("Cell barcode rank") +
        ggplot2::ylab("Number of detected genes") +
        ggplot2::theme_bw() +
        ggplot2::theme(legend.position = "none",
                       axis.title = ggplot2::element_text(size = 12))
}
