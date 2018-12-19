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
#' @examples
#' app <- alevinQCShiny(baseDir = system.file("extdata/alevin_example",
#'                                            package = "alevinQC"),
#'                      sampleId = "example")
#' if (interactive()) {
#'     shiny::runApp(app)
#' }
#'
alevinQCShiny <- function(baseDir, sampleId) {
    alevin <- readAlevinQC(baseDir)

    pLayout <- function() {
        shinydashboard::dashboardPage(
            skin = "red",

            shinydashboard::dashboardHeader(
                title = paste0("alevinQC (v", packageVersion("alevinQC"), "), ",
                               sampleId),
                titleWidth = (10 + nchar(sampleId)) * 20),

            shinydashboard::dashboardSidebar(disable = TRUE),

            shinydashboard::dashboardBody(
                shiny::fluidRow(
                    shinydashboard::box(
                        title = "Version info, alevin run",
                        DT::dataTableOutput("versionTable")
                    ),
                    shinydashboard::box(
                        title = "Summary table",
                        DT::dataTableOutput("summaryTable")
                    )
                ),
                shiny::fluidRow(
                    shinydashboard::box(
                        width = 4,
                        title = "Knee plot, initial whitelist determination",
                        shiny::plotOutput("rawCBKneePlot")
                    ),
                    shinydashboard::box(
                        width = 4,
                        title = "Barcode collapsing",
                        shiny::plotOutput("barcodeCollapsePlot")
                    ),
                    shinydashboard::box(
                        width = 4,
                        title = "Knee plot, number of genes per cell",
                        shiny::plotOutput("nbrGenesKneePlot")
                    )
                ),
                shiny::fluidRow(
                    shinydashboard::box(
                        width = 12,
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
                alevin$versionTable,
                colnames = "",
                options = list(scrollX = TRUE)
            )
        )

        output$summaryTable <- DT::renderDataTable(
            DT::datatable(
                alevin$summaryTable,
                colnames = "",
                options = list(scrollX = TRUE)
            )
        )

        output$rawCBKneePlot <- shiny::renderPlot(
            plotAlevinKneeRaw(alevin$cbTable)
        )

        output$barcodeCollapsePlot <- shiny::renderPlot(
            plotAlevinBarcodeCollapse(alevin$cbTable)
        )

        output$quantPairsPlot <- shiny::renderPlot(
            plotAlevinQuant(alevin$cbTable)
        )

        output$nbrGenesKneePlot <- shiny::renderPlot(
            plotAlevinKneeNbrGenes(alevin$cbTable)
        )
    }

    shiny::shinyApp(ui = pLayout, server = server_function)
}