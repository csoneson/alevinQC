## Function from Avi Srivastava, to read binary Alevin quant matrix
ReadAlevin <- function(base.path = NULL) {
    if (!dir.exists(base.path)) {
        stop("Directory provided does not exist")
    }
    barcode.loc <- file.path(base.path, "alevin/quants_mat_rows.txt")
    gene.loc <- file.path(base.path, "alevin/quants_mat_cols.txt")
    matrix.loc <- file.path(base.path, "alevin/quants_mat.gz")

    if (!file.exists(barcode.loc)) {
        stop("Barcode file missing")
    }
    if (!file.exists(gene.loc)) {
        stop("Gene name file missing")
    }
    if (!file.exists(matrix.loc)) {
        stop("Expression matrix file missing")
    }

    cell.names <- readLines(barcode.loc)
    gene.names <- readLines(gene.loc)
    num.cells <- length(cell.names)
    num.genes <- length(gene.names)

    out.matrix <- matrix(NA, nrow = num.genes, ncol = num.cells)
    con <- gzcon(file(matrix.loc, "rb"))

    total.molecules <- 0.0
    for (n in seq_len(num.cells)) {
        out.matrix[, n] <- readBin(con, double(), endian = "little", n = num.genes)
        total.molecules <- total.molecules + sum(out.matrix[, n])
    }

    colnames(out.matrix) <- cell.names
    rownames(out.matrix) <- gene.names

    print(paste("Found total ", round(total.molecules), " molecules."))
    return(out.matrix)
}
