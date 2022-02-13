test_that("get_permit_freq works", {
    out <- cpp_get_permit_freq_info(system.file("extdata/permit_freq.bin",
                                                package = "alevinQC"))
    expect_type(out, "list")
    expect_length(out, 2)
    expect_equal(out[[1]], c("TGCTGCTAGACCACCT", "TGCTGCTAGACCACGC",
                             "TGCTGCTAGACCACCC", "TGCTGCTAGACCACGA",
                             "TGCTGCTAGACCACCG"))
    expect_equal(out[[2]], c(5, 5, 4, 5, 5))
})
