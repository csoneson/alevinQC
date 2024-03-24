#' alevinQC
#'
#' alevinQC
#'
## usethis namespace: start
#' @useDynLib alevinQC, .registration = TRUE
#' @importFrom Rcpp sourceCpp
## usethis namespace: end
#' @keywords internal
"_PACKAGE"

globalVariables(c("V1", "V2", "V3", "V4", "V5", "CB",
                  "inFirstWhiteList", "inFinalWhiteList",
                  "MappingRate", "CorrectedReads",
                  "DedupRate", "NumGenesOverMean", "MappedReads",
                  "DeduplicatedReads", "NumGenesExpressed",
                  "collapsedFreq", "originalFreq", "nbrGenesAboveZero",
                  "totalUMICount", "ranking", "x"))
