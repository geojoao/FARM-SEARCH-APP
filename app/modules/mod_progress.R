#' Progress bar module
#' @import shiny shinyWidgets
NULL

mod_progress_server <- function(session, output_progressbar) {
  observeEvent(output_progressbar(), {
    if (identical(output_progressbar()$botao, 'vizinhos')) {
      shinyWidgets::updateProgressBar(session = session, id = "progress",
        value = output_progressbar()$etapa, total = 7, title = output_progressbar()$mensagem)
      if (identical(output_progressbar()$etapa, 7)) {
        shinyWidgets::closeSweetAlert()
      }
    } else if (identical(output_progressbar()$botao, 'pesquisa')) {
      shinyWidgets::updateProgressBar(session = session, id = "progress",
        value = output_progressbar()$etapa, total = 7, title = output_progressbar()$mensagem)
      if (identical(output_progressbar()$etapa, 7)) {
        shinyWidgets::closeSweetAlert()
      }
    }
  })
}
