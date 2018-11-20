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
                        DT::dataTableOutput("first.table")
                    ),
                    shinydashboard::box(
                        title = "Summary table",
                        DT::dataTableOutput("second.table")
                    )
                ),
                shiny::fluidRow(
                    shinydashboard::box(
                        title = "Knee plot I",
                        shiny::plotOutput("first.plot")
                    ),
                    shinydashboard::box(
                        title = "Barcode collapsing",
                        shiny::plotOutput("second.plot")
                    )
                ),
                shiny::fluidRow(
                    shinydashboard::box(
                        title = "Knee plot II",
                        shiny::plotOutput("fourth.plot")
                    ),
                    shinydashboard::box(
                        title = "Quantification summary",
                        shiny::plotOutput("third.plot")
                    )
                )

            )
        )
    }

    server_function <- function(input, output, session) {
        output$first.table <- DT::renderDataTable(
            DT::datatable(
                alevin$versiontable,
                colnames = "",
                options = list(scrollX = TRUE)
            )
        )

        output$second.table <- DT::renderDataTable(
            DT::datatable(
                alevin$summarytable,
                colnames = "",
                options = list(scrollX = TRUE)
            )
        )

        output$first.plot <- shiny::renderPlot(
            plotAlevinKneeRaw(alevin$rawcbfreq,
                              threshold = nrow(alevin$quantbcs))
        )

        output$second.plot <- shiny::renderPlot(
            plotAlevinBarcodeCollapse(alevin$quantbcs)
        )

        output$third.plot <- shiny::renderPlot(
            plotAlevinQuantPairs(alevin$quantbcs)
        )

        output$fourth.plot <- shiny::renderPlot(
            plotAlevinKneeNbrGenes(alevin$quantbcs)
        )
    }

    shiny::shinyApp(ui = pLayout, server = server_function)
}
