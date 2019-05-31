#' Generate alevin summary shiny app
#'
#' Generate a shiny app summarizing the main aspects of an alevin quantification
#' run. The app generation assumes that alevin has been run with the
#' --dumpFeatures flag to generate the necessary output files.
#'
#' @param baseDir Path to the output directory from the alevin run (should be
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
#' @importFrom utils packageVersion
#'
#' @return A shiny app.
#'
#' @examples
#' app <- alevinQCShiny(baseDir = system.file("extdata/alevin_example_v0.14",
#'                                            package = "alevinQC"),
#'                      sampleId = "example")
#' if (interactive()) {
#'     shiny::runApp(app)
#' }
#'
alevinQCShiny <- function(baseDir, sampleId) {
    alevin <- readAlevinQC(baseDir)

    pLayout <- shinydashboard::dashboardPage(
        skin = "red",

        shinydashboard::dashboardHeader(
            title = paste0("alevinQC (v",
                           utils::packageVersion("alevinQC"), "), ",
                           sampleId),
            titleWidth = (10 + nchar(sampleId)) * 20),

        shinydashboard::dashboardSidebar(disable = TRUE),

        shinydashboard::dashboardBody(
            shiny::fluidRow(
                shinydashboard::box(
                    width = 6,
                    title = "Version info, alevin run",
                    DT::dataTableOutput("versionTable")
                ),
                shinydashboard::box(
                    width = 6,
                    title = "Summary tables",
                    DT::dataTableOutput("summaryTableFull"),
                    DT::dataTableOutput("summaryTableInitialWl"),
                    DT::dataTableOutput("summaryTableFinalWl")
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
                    shiny::plotOutput("quantPlot")
                )
            )

        )
    )

    server_function <- function(input, output, session) { # nocov start
        output$versionTable <- DT::renderDataTable(
            DT::datatable(
                alevin$versionTable,
                colnames = "",
                options = list(scrollX = TRUE)
            )
        )

        output$summaryTableFull <- DT::renderDataTable(
            DT::datatable(
                alevin$summaryTables$fullDataset,
                colnames = "",
                options = list(scrollX = TRUE)
            )
        )

        output$summaryTableInitialWl <- DT::renderDataTable(
            DT::datatable(
                alevin$summaryTables$initialWhitelist,
                colnames = "",
                options = list(scrollX = TRUE)
            )
        )

        output$summaryTableFinalWl <- DT::renderDataTable(
            DT::datatable(
                alevin$summaryTables$finalWhitelist,
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

        output$quantPlot <- shiny::renderPlot(
            plotAlevinQuant(alevin$cbTable)
        )

        output$nbrGenesKneePlot <- shiny::renderPlot(
            plotAlevinKneeNbrGenes(alevin$cbTable)
        )
    } # nocov end

    shiny::shinyApp(ui = pLayout, server = server_function)
}
