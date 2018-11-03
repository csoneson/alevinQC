#' Read binary Alevin quant matrix
#'
#' @param baseDir Path to the output directory from the Alevin run
#' @param quiet Logical, whether to suppress printing of the number of detected
#'   molecules.
#'
#' @author Avi Srivastava
#'
#' @export
#'
#' @return A matrix with gene counts for the whitelisted barcodes. Rows
#'   represent genes, columns cell barcodes.
#'
readAlevin <- function(baseDir = NULL, quiet = TRUE) {
    if (!dir.exists(baseDir)) {
        stop("Directory provided does not exist")
    }
    barcodeFile <- file.path(baseDir, "alevin/quants_mat_rows.txt")
    geneFile <- file.path(baseDir, "alevin/quants_mat_cols.txt")
    matrixFile <- file.path(baseDir, "alevin/quants_mat.gz")

    if (!file.exists(barcodeFile)) {
        stop("Barcode file missing")
    }
    if (!file.exists(geneFile)) {
        stop("Gene name file missing")
    }
    if (!file.exists(matrixFile)) {
        stop("Expression matrix file missing")
    }

    cellNames <- readLines(barcodeFile)
    geneNames <- readLines(geneFile)
    numCells <- length(cellNames)
    numGenes <- length(geneNames)

    outMatrix <- matrix(NA, nrow = numGenes, ncol = numCells)
    con <- gzcon(file(matrixFile, "rb"))

    totalMolecules <- 0.0
    for (n in seq_len(numCells)) {
        outMatrix[, n] <- readBin(con, double(), endian = "little", n = numGenes)
        totalMolecules <- totalMolecules + sum(outMatrix[, n])
    }

    colnames(outMatrix) <- cellNames
    rownames(outMatrix) <- geneNames

    if (!quiet) {
        print(paste("Found total ", round(totalMolecules), " molecules."))
    }

    return(outMatrix)
}
