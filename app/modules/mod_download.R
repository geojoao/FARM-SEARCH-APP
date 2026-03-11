#' Download module - GPKG export
#' @import shiny sf
NULL

mod_download_server <- function(input, output, session, download_data) {
  output$download_gpkg <- downloadHandler(
    filename = function() {
      paste("terras_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".gpkg", sep = "")
    },
    content = function(file) {
      data <- download_data()
      showModal(modalDialog("Preparando arquivo .gpkg...", footer = NULL))
      if (is.null(data)) {
        removeModal()
        showModal(modalDialog("Sem dados para exportar", easyClose = TRUE))
      } else {
        tryCatch({
          sf::st_write(data, file, driver = "GPKG", quiet = TRUE)
          removeModal()
        }, error = function(e) {
          removeModal()
          showModal(modalDialog(paste("Erro exportando o arquivo gpkg:", e$message), easyClose = TRUE))
        })
      }
    }
  )

  output$download_buttons <- renderUI({
    req(download_data())
    div(
      style = "text-align: center;",
      downloadButton("download_gpkg", "Download gpkg", class = "btn-primary")
    )
  })
}
