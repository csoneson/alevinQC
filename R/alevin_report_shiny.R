#' Generate Alevin summary shiny app
#'
#' Generate a shiny app summarizing the main aspects of an Alevin quantification
#' run. The app generation assumes that Alevin has been run with the
#' --dumpFeatures flag to generate the necessary output files.
#'
#' @param baseDir Path to the output directory from the Alevin run (should be
#'   the directory containing the \code{alevin} directory).
#' @param sampleId Sample ID, will be used set the title for the app.
#'
#' @author Charlotte Soneson
#'
#' @export
#'
#' @import dplyr
#' @importFrom shiny fluidRow plotOutput renderPlot shinyApp
#' @importFrom shinydashboard dashboardPage dashboardHeader dashboardSidebar box
#' @importFrom DT dataTableOutput datatable renderDataTable
#'
#' @return A shiny app.
#'
alevinQCShiny <- function(baseDir, sampleId) {
    alevin <- readAlevinQC(baseDir, quiet = TRUE)

    pLayout <- function() {
        shinydashboard::dashboardPage(
            skin = "red",

            shinydashboard::dashboardHeader(title = paste0("alevinQC, ", sampleId),
                                            titleWidth = (10 + nchar(sampleId)) * 20),

            shinydashboard::dashboardSidebar(disable = TRUE),

            shinydashboard::dashboardBody(
                shiny::fluidRow(
                    shinydashboard::box(
                        title = "Version info",
                        DT::dataTableOutput("versionTable")
                    ),
                    shinydashboard::box(
                        title = "Summary table",
                        DT::dataTableOutput("summaryTable")
                    )
                ),
                shiny::fluidRow(
                    shinydashboard::box(
                        title = "Knee plot I",
                        shiny::plotOutput("rawCBKneePlot")
                    ),
                    shinydashboard::box(
                        title = "Barcode collapsing",
                        shiny::plotOutput("barcodeCollapsePlot")
                    )
                ),
                shiny::fluidRow(
                    shinydashboard::box(
                        title = "Knee plot II",
                        shiny::plotOutput("nbrGenesKneePlot")
                    ),
                    shinydashboard::box(
                        title = "Quantification summary",
                        shiny::plotOutput("quantPairsPlot")
                    )
                )

            )
        )
    }

    server_function <- function(input, output, session) {
        output$versionTable <- DT::renderDataTable(
            DT::datatable(
                alevin$versiontable,
                colnames = "",
                options = list(scrollX = TRUE)
            )
        )

        output$summaryTable <- DT::renderDataTable(
            DT::datatable(
                alevin$summarytable,
                colnames = "",
                options = list(scrollX = TRUE)
            )
        )

        output$rawCBKneePlot <- shiny::renderPlot(
            plotAlevinKneeRaw(alevin$rawcbfreq,
                              threshold = nrow(alevin$quantbcs))
        )

        output$barcodeCollapsePlot <- shiny::renderPlot(
            plotAlevinBarcodeCollapse(alevin$quantbcs)
        )

        output$quantPairsPlot <- shiny::renderPlot(
            plotAlevinQuantPairs(alevin$quantbcs)
        )

        output$nbrGenesKneePlot <- shiny::renderPlot(
            plotAlevinKneeNbrGenes(alevin$quantbcs)
        )
    }

    shiny::shinyApp(ui = pLayout, server = server_function)
}
