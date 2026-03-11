#' Search module - attribute-based farm search
#' @import shiny sf dplyr httr future promises ipc
NULL

GPKG_PATH <- "~/AFS/Pesquisa_Quantitativa/Compartilhado/rundeck_files_folder/Farm_Owners_Data/farm_owners_data.gpkg"

mod_search_server <- function(input, session, output_results, output_progressbar,
                              file_source_queue_progressbar, file_source_queue_results) {
  observeEvent(input$search_button, {
    output_results(list(botao = 'pesquisa', erro = NULL, warning = NULL, texto = NULL,
                        terras_filtradas = NULL, vizinhos = NULL, vcg = NULL, vcsg = NULL,
                        vnc = NULL, vcg_dissolved_centroids = NULL))
    output_progressbar(list(botao = 'pesquisa', etapa = 0, mensagem = 'Iniciando procura de terras'))
    lista_resultados_output <- list(botao = 'pesquisa', erro = NULL, warning = NULL, texto = NULL,
                                    terras_filtradas = NULL, vizinhos = NULL, vcg = NULL, vcsg = NULL, vnc = NULL)
    shinyWidgets::progressSweetAlert(session = session, id = "progress", title = 'semtitulo',
                                     display_pct = TRUE, value = 0)
    UF <- input$UF
    Municipio <- input$Municipio
    Codigo_Imovel_Rural <- input$Codigo_Imovel_Rural
    Nome_Fazenda <- input$Nome_Fazenda
    Matricula <- input$Matricula
    ID_Terra <- input$ID_Terra
    Grupo_proprietario <- input$Grupo_proprietario
    Nome_proprietario <- input$Nome_proprietario
    Documento_proprietario <- input$Documento_proprietario
    promises::future_promise(
      globals = list(
        file_source_queue_progressbar = file_source_queue_progressbar,
        file_source_queue_results = file_source_queue_results,
        lista_resultados_output = lista_resultados_output,
        UF = UF, Municipio = Municipio, Codigo_Imovel_Rural = Codigo_Imovel_Rural,
        Nome_Fazenda = Nome_Fazenda, Matricula = Matricula, ID_Terra = ID_Terra,
        Grupo_proprietario = Grupo_proprietario, Nome_proprietario = Nome_proprietario,
        Documento_proprietario = Documento_proprietario,
        fetch_spatial_df = fetch_spatial_df,
        GPKG_PATH = GPKG_PATH
      ),
      packages = c("h3", "sf", "ipc", "dplyr", "httr"),
      seed = TRUE,
      {
        httr::set_config(httr::config(ssl_verifypeer = FALSE))
        queue_progressbar <- ipc::shinyQueue(source = file_source_queue_progressbar)
        queue_results <- ipc::shinyQueue(source = file_source_queue_results)
        queue_progressbar$producer$fireAssignReactive("output_progressbar",
          list(botao = 'pesquisa', etapa = 1, mensagem = 'Fazendo query na base de dados completa'))
        base_search_query <- "SELECT * FROM terras_e_proprietarios"
        search_conditions <- list()
        if (!is.null(UF) && length(UF) > 0) {
          if (length(UF) == 1 && UF != "") {
            search_conditions <- append(search_conditions, sprintf("uf = '%s'", UF))
          } else {
            search_conditions <- append(search_conditions, sprintf("uf IN (%s)", paste(sprintf("'%s'", UF), collapse = ", ")))
          }
        }
        if (!is.null(Municipio) && length(Municipio) > 0) {
          if (length(Municipio) == 1 && Municipio != "") {
            search_conditions <- append(search_conditions, sprintf("municipio = '%s'", Municipio))
          } else {
            search_conditions <- append(search_conditions, sprintf("municipio IN (%s)", paste(sprintf("'%s'", Municipio), collapse = ", ")))
          }
        }
        if (!is.null(Codigo_Imovel_Rural) && length(Codigo_Imovel_Rural) > 0) {
          if (length(Codigo_Imovel_Rural) == 1 && Codigo_Imovel_Rural != "") {
            search_conditions <- append(search_conditions, sprintf("codigo_imovel = '%s'", Codigo_Imovel_Rural))
          } else {
            search_conditions <- append(search_conditions, sprintf("codigo_imovel IN (%s)", paste(sprintf("'%s'", Codigo_Imovel_Rural), collapse = ", ")))
          }
        }
        if (!is.null(Nome_Fazenda) && length(Nome_Fazenda) > 0) {
          if (length(Nome_Fazenda) == 1 && Nome_Fazenda != "") {
            search_conditions <- append(search_conditions, sprintf("nome_area LIKE '%s'", Nome_Fazenda))
          } else {
            search_conditions <- append(search_conditions, sprintf("nome_area IN (%s)", paste(sprintf("'%s'", Nome_Fazenda), collapse = ", ")))
          }
        }
        if (!is.null(Matricula) && length(Matricula) > 0) {
          if (length(Matricula) == 1 && Matricula != "") {
            search_conditions <- append(search_conditions, sprintf("matricula = '%s'", Matricula))
          } else {
            search_conditions <- append(search_conditions, sprintf("matricula IN (%s)", paste(sprintf("'%s'", Matricula), collapse = ", ")))
          }
        }
        if (!is.null(ID_Terra) && length(ID_Terra) > 0) {
          if (length(ID_Terra) == 1 && ID_Terra != "") {
            search_conditions <- append(search_conditions, sprintf("id_terra = '%s'", ID_Terra))
          } else {
            search_conditions <- append(search_conditions, sprintf("id_terra IN (%s)", paste(sprintf("'%s'", ID_Terra), collapse = ", ")))
          }
        }
        if (!is.null(Grupo_proprietario) && length(Grupo_proprietario) > 0) {
          if (length(Grupo_proprietario) == 1 && Grupo_proprietario != "") {
            search_conditions <- append(search_conditions, sprintf("grupo = '%s'", Grupo_proprietario))
          } else {
            search_conditions <- append(search_conditions, sprintf("grupo IN (%s)", paste(sprintf("'%s'", Grupo_proprietario), collapse = ", ")))
          }
        }
        if (!is.null(Nome_proprietario) && length(Nome_proprietario) > 0) {
          if (length(Nome_proprietario) == 1 && Nome_proprietario != "") {
            search_conditions <- append(search_conditions, sprintf("nome_proprietario = '%s'", Nome_proprietario))
          } else {
            search_conditions <- append(search_conditions, sprintf("nome_proprietario IN (%s)", paste(sprintf("'%s'", Nome_proprietario), collapse = ", ")))
          }
        }
        if (!is.null(Documento_proprietario) && length(Documento_proprietario) > 0) {
          if (length(Documento_proprietario) == 1 && Documento_proprietario != "") {
            search_conditions <- append(search_conditions, sprintf("documento = '%s'", Documento_proprietario))
          } else {
            search_conditions <- append(search_conditions, sprintf("documento IN (%s)", paste(sprintf("'%s'", Documento_proprietario), collapse = ", ")))
          }
        }
        if (length(search_conditions) == 0) {
          lista_resultados_output$erro <- TRUE
          lista_resultados_output$texto <- "Selecione algum filtro para pesquisar terras"
          queue_results$producer$fireAssignReactive("output_results", lista_resultados_output)
          return(NULL)
        }
        where_clause <- paste("WHERE", paste(search_conditions, collapse = " AND "))
        final_search_query <- paste(base_search_query, where_clause)
        data <- tryCatch({
          sf::st_read(dsn = GPKG_PATH, query = final_search_query, quiet = TRUE) %>% sf::st_set_geometry(NULL)
        }, error = function(e) {
          lista_resultados_output$erro <- TRUE
          lista_resultados_output$texto <- "An error occurred while loading the data. Please check your inputs and try again."
          queue_results$producer$fireAssignReactive("output_results", lista_resultados_output)
          return(NULL)
        })
        if (is.null(data) || nrow(data) == 0) {
          lista_resultados_output$erro <- TRUE
          lista_resultados_output$texto <- "Nenhum dado achado para os filtros selecionados"
          queue_results$producer$fireAssignReactive("output_results", lista_resultados_output)
          return(NULL)
        }
        data <- data %>%
          dplyr::group_by(id_terra) %>%
          dplyr::summarise(
            dplyr::across(c(-grupo, -nome_proprietario, -fonte_proprietario, -data_insercao, -documento), dplyr::first),
            grupo = paste(grupo, collapse = ", "),
            documento = paste(documento, collapse = ", "),
            nome_proprietario = paste(nome_proprietario, collapse = ", "),
            fonte_proprietario = paste(fonte_proprietario, collapse = ", "),
            data_insercao = paste(data_insercao, collapse = ", ")
          ) %>%
          dplyr::ungroup()
        record_limit <- 5000
        if (nrow(data) > record_limit) {
          lista_resultados_output$warning <- TRUE
          lista_resultados_output$texto <- sprintf("Your query returned more than %d records. Only the first %d will be displayed.", record_limit, record_limit)
          queue_results$producer$fireAssignReactive("output_results", lista_resultados_output)
          data <- data[1:record_limit, ]
        }
        queue_progressbar$producer$fireAssignReactive("output_progressbar",
          list(botao = 'pesquisa', etapa = 2, mensagem = 'Adicionando geometrias SNCI'))
        snci_df <- NULL
        if (sum(data$fonte_geo == 'SNCI') > 0) {
          ids_snci <- data$id_terra[data$fonte_geo == 'SNCI']
          snci_cql_string <- paste0("numero_certificado_snci IN ('", paste(ids_snci, collapse = "', '"), "')")
          snci_df <- fetch_spatial_df(url = "https://geoserver.bocombbm.com.br/geoserver/INCRA/ows",
                                      cql_filter = snci_cql_string, camada = "INCRA:SNCI")
          snci_df <- snci_df %>% dplyr::select("numero_certificado_snci", "geometry") %>% dplyr::rename(id_terra = numero_certificado_snci)
        }
        queue_progressbar$producer$fireAssignReactive("output_progressbar",
          list(botao = 'pesquisa', etapa = 3, mensagem = 'Adicionando geometrias SIGEF'))
        sigef_df <- NULL
        if (sum(data$fonte_geo == 'SIGEF') > 0) {
          ids_sigef <- data$id_terra[data$fonte_geo == 'SIGEF']
          sigef_cql_string <- paste0("codigo_parcela_sigef IN ('", paste(ids_sigef, collapse = "', '"), "')")
          sigef_df <- fetch_spatial_df(url = "https://geoserver.bocombbm.com.br/geoserver/INCRA/ows",
                                       cql_filter = sigef_cql_string, camada = "INCRA:SIGEF")
          sigef_df <- sigef_df %>% dplyr::select("codigo_parcela_sigef", "geometry") %>% dplyr::rename(id_terra = codigo_parcela_sigef)
        }
        queue_progressbar$producer$fireAssignReactive("output_progressbar",
          list(botao = 'pesquisa', etapa = 4, mensagem = 'Unindo e validando geometrias'))
        geometrias_wfs <- dplyr::bind_rows(sigef_df, snci_df)
        data_with_geometry <- dplyr::left_join(data, geometrias_wfs, by = 'id_terra')
        data_with_geometry <- data_with_geometry %>%
          dplyr::filter(!sf::st_is_empty(geometry) & !is.na(geometry))
        data_with_geometry <- sf::st_make_valid(sf::st_as_sf(data_with_geometry))
        lista_resultados_output$warning <- NULL
        lista_resultados_output$terras_filtradas <- data_with_geometry
        queue_results$producer$fireAssignReactive("output_results", lista_resultados_output)
        queue_progressbar$producer$fireAssignReactive("output_progressbar",
          list(botao = 'pesquisa', etapa = 5, mensagem = 'Plotando dados no mapa'))
      }
    )
    gc
  })
}
