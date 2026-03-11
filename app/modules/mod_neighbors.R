#' Neighbors module - load neighbors in visible map area
#' @import shiny sf dplyr httr future promises ipc h3
NULL

GPKG_PATH <- "~/AFS/Pesquisa_Quantitativa/Compartilhado/rundeck_files_folder/Farm_Owners_Data/farm_owners_data.gpkg"

mod_neighbors_server <- function(input, session, output_results, output_progressbar,
                                file_source_queue_progressbar, file_source_queue_results) {
  observeEvent(input$load_neighbors_button, {
    output_results(list(botao = 'vizinhos', erro = NULL, warning = NULL, texto = NULL,
                        terras_filtradas = NULL, vizinhos = NULL, vcg = NULL, vcsg = NULL,
                        vnc = NULL, vcg_dissolved_centroids = NULL))
    output_progressbar(list(botao = 'vizinhos', etapa = 0, mensagem = 'Iniciando processamento de vizinhos'))
    lista_resultados_output <- list(botao = 'vizinhos', erro = NULL, warning = NULL, texto = NULL,
                                    terras_filtradas = NULL, vizinhos = NULL, vcg = NULL, vcsg = NULL, vnc = NULL)
    bounds <- input$mymap_bounds
    shinyWidgets::progressSweetAlert(session = session, id = "progress", title = 'semtitulo',
                                     display_pct = TRUE, value = 0)
    promises::future_promise(
      globals = list(
        bounds = bounds,
        file_source_queue_progressbar = file_source_queue_progressbar,
        file_source_queue_results = file_source_queue_results,
        lista_resultados_output = lista_resultados_output,
        fetch_spatial_df = fetch_spatial_df,
        GPKG_PATH = GPKG_PATH
      ),
      packages = c("h3", "sf", "ipc", "dplyr", "httr"),
      seed = TRUE,
      {
        httr::set_config(httr::config(ssl_verifypeer = FALSE))
        queue_progressbar <- ipc::shinyQueue(source = file_source_queue_progressbar)
        queue_results <- ipc::shinyQueue(source = file_source_queue_results)
        if (is.null(bounds)) {
          lista_resultados_output$erro <- TRUE
          lista_resultados_output$texto <- "O mapa ainda nao esta carregado. Tente novamente."
          queue_results$producer$fireAssignReactive("output_results", lista_resultados_output)
          return(NULL)
        }
        queue_progressbar$producer$fireAssignReactive("output_progressbar",
          list(botao = 'vizinhos', etapa = 1, mensagem = 'Extraindo os hexagonos viziveis'))
        lng_min <- bounds$west
        lng_max <- bounds$east
        lat_min <- bounds$south
        lat_max <- bounds$north
        bbox_polygon <- sf::st_as_sf(sf::st_sfc(sf::st_polygon(list(matrix(c(
          lng_min, lat_min, lng_min, lat_max, lng_max, lat_max, lng_max, lat_min, lng_min, lat_min
        ), ncol = 2, byrow = TRUE))), crs = 4326))
        hexagons <- h3::polyfill(bbox_polygon, res = 5)
        if (length(hexagons) >= 50) {
          lista_resultados_output$erro <- TRUE
          lista_resultados_output$texto <- "Sua area de vizinhos e maior do que 50 hexagonos (aprox: 1,25 milhao de ha) reduza a area e pesquise novamente ."
          queue_results$producer$fireAssignReactive("output_results", lista_resultados_output)
          return(NULL)
        } else if (length(hexagons) == 0) {
          lista_resultados_output$erro <- TRUE
          lista_resultados_output$texto <- "Sua area de vizinhos e menor do que do que 1 hexagono (aprox: 25 mil ha) aumente a area e pesquise novamente ."
          queue_results$producer$fireAssignReactive("output_results", lista_resultados_output)
          return(NULL)
        }
        queue_progressbar$producer$fireAssignReactive("output_progressbar",
          list(botao = 'vizinhos', etapa = 2, mensagem = 'Buscando terras no .sqlite'))
        vizinhos_query <- paste0("SELECT * FROM terras_e_proprietarios WHERE id_hexagono IN ('", paste(hexagons, collapse = "', '"), "')")
        vizinhos <- sf::st_read(GPKG_PATH, query = vizinhos_query, quiet = TRUE) %>% sf::st_set_geometry(NULL)
        record_limit <- 10000
        if (nrow(vizinhos) > record_limit) {
          lista_resultados_output$warning <- TRUE
          lista_resultados_output$texto <- "Sua query resultou em muitos resultados, apenas os primeiros 10 mil serao mostrados"
          queue_results$producer$fireAssignReactive("output_results", lista_resultados_output)
          vizinhos <- vizinhos[1:record_limit, ]
        } else if (nrow(vizinhos) == 0) {
          lista_resultados_output$erro <- TRUE
          lista_resultados_output$texto <- "A area pesquisada nao possui terras no SIGEF ou SNCI"
          queue_results$producer$fireAssignReactive("output_results", lista_resultados_output)
          return(NULL)
        }
        vizinhos <- vizinhos %>%
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
        queue_progressbar$producer$fireAssignReactive("output_progressbar",
          list(botao = 'vizinhos', etapa = 3, mensagem = 'Baixando geometrias do SIGEF'))
        sigef_df <- NULL
        if (sum(vizinhos$fonte_geo == 'SIGEF') > 0) {
          ids_sigef <- vizinhos$id_terra[vizinhos$fonte_geo == 'SIGEF']
          sigef_cql_string <- paste0("codigo_parcela_sigef IN ('", paste(ids_sigef, collapse = "', '"), "')")
          sigef_df <- fetch_spatial_df(url = "https://geoserver.bocombbm.com.br/geoserver/INCRA/ows",
                                       cql_filter = sigef_cql_string, camada = "INCRA:SIGEF")
          sigef_df <- sigef_df %>% dplyr::select("codigo_parcela_sigef", "geometry") %>% dplyr::rename(id_terra = codigo_parcela_sigef)
        }
        queue_progressbar$producer$fireAssignReactive("output_progressbar",
          list(botao = 'vizinhos', etapa = 4, mensagem = 'Baixando geometrias do SIGEF'))
        snci_df <- NULL
        if (sum(vizinhos$fonte_geo == 'SNCI') > 0) {
          ids_snci <- vizinhos$id_terra[vizinhos$fonte_geo == 'SNCI']
          snci_cql_string <- paste0("numero_certificado_snci IN ('", paste(ids_snci, collapse = "', '"), "')")
          snci_df <- fetch_spatial_df(url = "https://geoserver.bocombbm.com.br/geoserver/INCRA/ows",
                                      cql_filter = snci_cql_string, camada = "INCRA:SNCI")
          snci_df <- snci_df %>% dplyr::select("numero_certificado_snci", "geometry") %>% dplyr::rename(id_terra = numero_certificado_snci)
        }
        queue_progressbar$producer$fireAssignReactive("output_progressbar",
          list(botao = 'vizinhos', etapa = 5, mensagem = 'Unindo geometrias e os dados do .sqlite'))
        geometrias_wfs <- dplyr::bind_rows(sigef_df, snci_df)
        vizinhos_with_geometry <- dplyr::left_join(vizinhos, geometrias_wfs, by = 'id_terra')
        vizinhos_with_geometry <- vizinhos_with_geometry %>%
          dplyr::filter(!sf::st_is_empty(geometry) & !is.na(geometry))
        vizinhos_with_geometry <- sf::st_make_valid(sf::st_as_sf(vizinhos_with_geometry))
        vizinhos <- vizinhos_with_geometry
        vcg <- vizinhos_with_geometry %>%
          dplyr::filter(
            !is.na(grupo) & !is.null(grupo) &
              sapply(grupo, function(x) !identical(unique(unlist(strsplit(gsub("[^[:alnum:],]", "", x), ","))), "NA"))
          )
        vcsg <- vizinhos_with_geometry %>%
          dplyr::filter(
            !is.na(documento) & !is.null(documento) & documento != "NA",
            sapply(grupo, function(x) is.na(x) || is.null(x) || identical(unique(unlist(strsplit(gsub("[^[:alnum:],]", "", x), ","))), "NA"))
          )
        vnc <- vizinhos_with_geometry %>%
          dplyr::filter(
            (is.na(documento) | is.null(documento) | documento == 'NA') &
              sapply(grupo, function(x) identical(unique(unlist(strsplit(gsub("[^[:alnum:],]", "", x), ","))), "NA"))
          )
        vcg_dissolved_centroids <- NULL
        if (nrow(vcg) > 0) {
          cores_grupos <- c('#e6194b', '#3cb44b', '#ffe119', '#4363d8', '#f58231', '#911eb4', '#46f0f0',
                           '#f032e6', '#bcf60c', '#fabebe', '#008080', '#e6beff', '#9a6324',
                           '#800000', '#aaffc3', '#808000', '#ffd8b1', '#000075', '#808080', '#ffffff')
          vcg <- vcg %>%
            dplyr::mutate(grupo = sapply(strsplit(as.character(grupo), ","), function(x) paste(unique(trimws(x)), collapse = ","))) %>%
            dplyr::mutate(cor = cores_grupos[(as.numeric(factor(grupo)) - 1) %% length(cores_grupos) + 1])
          vcg_buffered <- vcg %>% sf::st_buffer(dist = 0.001) %>% sf::st_make_valid()
          vcg_dissolved <- vcg_buffered %>%
            dplyr::group_by(grupo) %>%
            dplyr::summarise(geometry = sf::st_union(geometry), .groups = "drop") %>%
            sf::st_make_valid()
          vcg_separated <- vcg_dissolved %>%
            sf::st_cast("MULTIPOLYGON") %>%
            sf::st_cast("POLYGON", group_or_split = TRUE)
          vcg_dissolved_centroids <- vcg_separated %>%
            dplyr::mutate(centroid = sf::st_centroid(geometry))
        }
        if (!is.null(vcg) && nrow(vcg) > 0) {
          lista_resultados_output$vcg <- vcg
          lista_resultados_output$vcg_dissolved_centroids <- vcg_dissolved_centroids
        }
        if (!is.null(vcsg)) lista_resultados_output$vcsg <- vcsg
        if (!is.null(vnc)) lista_resultados_output$vnc <- vnc
        if (!is.null(vizinhos)) lista_resultados_output$vizinhos <- vizinhos
        queue_progressbar$producer$fireAssignReactive("output_progressbar",
          list(botao = 'vizinhos', etapa = 6, mensagem = 'plotando os dados no mapa'))
        queue_results$producer$fireAssignReactive("output_results", lista_resultados_output)
        gc()
      }
    )
    gc()
  })
}
