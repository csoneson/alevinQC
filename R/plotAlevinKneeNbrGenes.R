#' Plot nbr genes knee plot
#'
#' @author Charlotte Soneson
#'
#' @param quantbcs data.frame with barcode frequencies (as returned by
#'   \code{readAlevinQC}).
#'
#' @export
#'
#' @importFrom ggplot2 ggplot aes geom_line xlab ylab theme_bw theme
#'   element_text
#' @import dplyr
#'
#' @return A ggplot object
#'
plotAlevinKneeNbrGenes <- function(quantbcs) {
    ggplot2::ggplot(quantbcs %>% dplyr::arrange(desc(nbrGenes2)) %>%
                        dplyr::mutate(x = seq_along(nbrGenes2)),
                    ggplot2::aes(x = x, y = nbrGenes2)) +
        ggplot2::geom_line(size = 2) +
        ggplot2::xlab("Cell barcode rank") +
        ggplot2::ylab("Number of detected genes") +
        ggplot2::theme_bw() +
        ggplot2::theme(legend.position = "none",
                       axis.title = ggplot2::element_text(size = 12))
}
