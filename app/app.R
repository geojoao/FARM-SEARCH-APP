library(shiny)
library(shinydashboard)
library(leaflet)
library(sf)
library(httr)
library(httr2)
library(dplyr)
library(leaflet.extras)
library(data.table)
library(h3)
library(shinyWidgets)
library(shinyjs)
library(ipc)
library(future)
library(promises)
library(htmltools)
library(htmlwidgets)
library(jsonlite)
library(BBMQuant)
library(xml2)

plan(multisession)
httr::set_config(httr::config(ssl_verifypeer = FALSE))

# Source R utilities and modules
for (f in list.files("R", pattern = "\\.R$", full.names = TRUE)) {
  source(f, local = TRUE)
}
for (f in list.files("modules", pattern = "\\.R$", full.names = TRUE)) {
  source(f, local = TRUE)
}

logg('Getting legends from geoserver with dynamic discovery...')

# Global data and periodic refresh
all_data <- NULL
print('abrindo arquivo')
all_data <- load_all_data()
print('Dados carregados inicialmente')

# UI
dashboard_header <- dashboardHeader(
  title = "Farm and Owners Search",
  tags$li(class = "dropdown",
    tags$a(href = 'https://rstudio.bocombbm.com.br/app/quant_agro_doc/apps/app-list.html',
      tags$img(src = 'logo.png', width = 200, align = "right", align = "center")
    )
  )
)

dashboard_sidebar <- dashboardSidebar(
  sidebarMenu(
    selectInput("Grupo_proprietario", "Group", choices = c(''), selected = NULL),
    selectInput("Nome_proprietario", "Nome", choices = c(''), selected = NULL),
    selectInput("Documento_proprietario", "Documento", choices = c(''), selected = NULL),
    selectInput("UF", "Estado", choices = c(''), selected = NULL, multiple = TRUE),
    selectInput("Municipio", "Municipio", choices = c(''), selected = NULL, multiple = TRUE),
    selectInput("Codigo_Imovel_Rural", "Codigo imovel", choices = c(''), selected = NULL, multiple = TRUE),
    selectInput("Nome_Fazenda", "Fazenda", choices = c(''), selected = NULL, multiple = TRUE),
    selectInput("Matricula", "Matricula", selected = NULL, choices = c(''), multiple = TRUE),
    selectInput("ID_Terra", "ID Terra", selected = NULL, choices = c(''), multiple = TRUE),
    actionButton("search_button", "Search"),
    actionButton("load_neighbors_button", "Load Neighbors"),
    actionButton("clear_button", "Clear"),
    hr(),
    uiOutput("download_buttons")
  )
)

dashboard_body <- dashboardBody(
  tags$head(
    includeCSS("https://cdn.jsdelivr.net/npm/leaflet.control.layers.tree@1.1.1/L.Control.Layers.Tree.css"),
    includeScript("https://cdn.jsdelivr.net/npm/leaflet.control.layers.tree@1.1.1/L.Control.Layers.Tree.js"),
    tags$style(HTML("
      .content { min-height: 250px; padding: 0px; }
      @media (min-width: 768px) {
        .navbar-nav>li>a { padding-top: 5px; padding-bottom: 0px; }
      }
      .leaflet-container { height: 95vh !important; }
      .btn-primary { background-color: #91372c; border-color: #000000; }
      .skin-black .sidebar a { color: #eee; }
      .btn-primary:hover { background-color: #dd4b39; }
    "))
  ),
  leafletOutput("mymap")
)

ui <- dashboardPage(skin = 'black', dashboard_header, dashboard_sidebar, dashboard_body)

# Server
server <- function(input, output, session) {
  update_data_observer <- observe({
    invalidateLater(5 * 60 * 1000)
    all_data <<- load_all_data()
  })

  download_data <- reactiveVal(NULL)
  file_source_queue_progressbar <- ipc::TextFileSource$new()
  queue_progressbar <- ipc::shinyQueue(source = file_source_queue_progressbar, session = session)
  queue_progressbar$consumer$start(100)
  output_progressbar <- reactiveVal(list(botao = NULL, etapa = NULL, mensagem = NULL))

  file_source_queue_results <- ipc::TextFileSource$new()
  queue_results <- ipc::shinyQueue(source = file_source_queue_results, session = session)
  queue_results$consumer$start(100)
  output_results <- reactiveVal(list(botao = NULL, erro = NULL, warning = NULL, texto = NULL,
    terras_filtradas = NULL, vizinhos = NULL, vcg = NULL, vcsg = NULL, vnc = NULL, vcg_dissolved_centroids = NULL))

  print('comecou a carregar as labels de pesquisa')
  mod_sidebar_server(session, output, all_data, make_map)

  mod_download_server(input, output, session, download_data)
  mod_map_server(input, output, session, output_results, output_progressbar, download_data, make_map)
  mod_search_server(input, session, output_results, output_progressbar,
    file_source_queue_progressbar, file_source_queue_results)
  mod_neighbors_server(input, session, output_results, output_progressbar,
    file_source_queue_progressbar, file_source_queue_results)
  mod_progress_server(session, output_progressbar)
}

ShinyAppBBM(
  ui = ui,
  server = server,
  tenant = "44d572a6-0370-4d7f-a52c-5c3616252aac",
  app_id = "#{app_id}#",
  app_secret = "#{app_secret}#",
  resource = c("openid"),
  redirect = "https://rstudio.bocombbm.com.br/app/App_farm_owners",
  grantedUsers = c(
    "gabrielvasconcelos@bocombbm.com.br",
    "danielreis@bocombbm.com.br",
    "reneroliveira@bocombbm.com.br",
    "joaoluizneto@bocombbm.com.br",
    "zuilhosegundo@bocombbm.com.br",
    "brunasantos@bocombbm.com.br",
    "thalescosta@bocombbm.com.br",
    "isabelafarina@bocombbm.com.br",
    'alexandrelowenkron@bocombbm.com.br',
    'leonardooliveira@bocombbm.com.br',
    'gabrielmattos@bocombbm.com.br',
    'guilhermemazzoni@bocombbm.com.br',
    'ramiromonarcha@bocombbm.com.br',
    'patriciacoimbra@bocombbm.com.br',
    'sillasrocha@bocombbm.com.br'
  ),
  authEnabled = TRUE
)
