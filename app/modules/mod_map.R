#' Map module - leaflet map rendering and updates
#' @import shiny leaflet leaflet.extras sf
NULL

mod_map_server <- function(input, output, session, output_results, output_progressbar, download_data, make_map) {
  observeEvent(output_results(), {
    if (identical(output_results()$erro, TRUE)) {
      shinyWidgets::closeSweetAlert()
      showModal(modalDialog(title = "Warning", output_results()$texto, easyClose = TRUE, footer = NULL))
    } else if (identical(output_results()$warning, TRUE)) {
      showModal(modalDialog(title = "Warning", output_results()$texto, easyClose = TRUE, footer = NULL))
    } else if (identical(output_results()$botao, 'pesquisa')) {
      terras_filtradas <- output_results()$terras_filtradas
      if (isTRUE(!is.null(terras_filtradas) && nrow(terras_filtradas) > 0)) {
        map <- leaflet::leafletProxy("mymap") %>%
          leaflet::clearShapes() %>%
          leaflet::clearMarkerClusters() %>%
          leaflet::clearMarkers() %>%
          leaflet::clearControls()
        map <- map %>%
          leaflet::addPolygons(
            data = terras_filtradas,
            layerId = ~id_terra,
            popup = ~paste0(
              "<b>ID Terra:</b> ", id_terra, "<br>",
              "<b>Codigo Imovel:</b> ", codigo_imovel, "<br>",
              "<b>Matricula:</b> ", matricula, "<br>",
              "<b>Nome Area:</b> ", nome_area, "<br>",
              "<b>Fonte Geo:</b> ", fonte_geo, "<br>",
              "<b>Area (ha):</b> ", area_ha, "<br>",
              "<b>Documento:</b> ", documento, "<br>",
              "<b>Nome Proprietario:</b> ", nome_proprietario, "<br>",
              "<b>Grupo:</b> ", grupo, "<br>",
              "<b>Fonte Proprietario:</b> ", fonte_proprietario, "<br>",
              "<b>Data Insercao:</b> ", data_insercao, "<br>",
              "<b>Data Insercao:</b> ", uf, "<br>",
              "<b>Municipio/UF:</b> ", municipio
            ),
            color = "#333333", weight = 0.6, opacity = 1,
            fillOpacity = 0.2, fillColor = "blue",
            highlightOptions = leaflet::highlightOptions(color = "white", weight = 2, bringToFront = TRUE),
            group = "Terras"
          ) %>%
          leaflet.extras::addDrawToolbar(
            targetGroup = 'Terras',
            polylineOptions = FALSE,
            circleOptions = FALSE,
            markerOptions = FALSE,
            rectangleOptions = FALSE,
            polygonOptions = FALSE,
            circleMarkerOptions = FALSE,
            editOptions = leaflet.extras::editToolbarOptions()
          )
        if (nrow(terras_filtradas) > 1) {
          map <- map %>%
            leaflet::addMarkers(
              clusterOptions = leaflet::markerClusterOptions(),
              data = sf::st_coordinates(sf::st_centroid(terras_filtradas$geometry))[, c('X', 'Y')]
            )
        }
        map %>%
          leaflet::flyToBounds(
            lng1 = sf::st_bbox(terras_filtradas)$xmin[[1]],
            lat1 = sf::st_bbox(terras_filtradas)$ymin[[1]],
            lng2 = sf::st_bbox(terras_filtradas)$xmax[[1]],
            lat2 = sf::st_bbox(terras_filtradas)$ymax[[1]]
          )
        download_data(terras_filtradas)
        output_progressbar(list(botao = 'pesquisa', etapa = 7, mensagem = 'Finalizada plotagem no mapa'))
      }
    } else if (identical(output_results()$botao, 'vizinhos')) {
      vcg <- output_results()$vcg
      vcsg <- output_results()$vcsg
      vnc <- output_results()$vnc
      vcg_dissolved_centroids <- output_results()$vcg_dissolved_centroids
      vizinhos <- output_results()$vizinhos
      if (any(
        isTRUE(!is.null(vcg) && nrow(vcg) > 0),
        isTRUE(!is.null(vcsg) && nrow(vcsg) > 0),
        isTRUE(!is.null(vnc) && nrow(vnc) > 0)
      )) {
        map <- leaflet::leafletProxy("mymap") %>%
          leaflet::clearShapes() %>%
          leaflet::clearMarkerClusters() %>%
          leaflet::clearMarkers() %>%
          leaflet::clearControls()
        if (isTRUE(!is.null(vcg) && nrow(vcg) > 0)) {
          map <- map %>%
            leaflet::addPolygons(
              data = vcg,
              layerId = ~id_terra,
              popup = ~paste0(
                "<b>ID Terra:</b> ", id_terra, "<br>",
                "<b>Codigo Imovel:</b> ", codigo_imovel, "<br>",
                "<b>Matricula:</b> ", matricula, "<br>",
                "<b>Nome Area:</b> ", nome_area, "<br>",
                "<b>Fonte Geo:</b> ", fonte_geo, "<br>",
                "<b>Area (ha):</b> ", area_ha, "<br>",
                "<b>Documento:</b> ", documento, "<br>",
                "<b>Nome Proprietario:</b> ", nome_proprietario, "<br>",
                "<b>Grupo:</b> ", grupo, "<br>",
                "<b>Fonte Proprietario:</b> ", fonte_proprietario, "<br>",
                "<b>Data Insercao:</b> ", data_insercao, "<br>",
                "<b>UF:</b> ", uf, "<br>",
                "<b>Municipio/UF:</b> ", municipio
              ),
              color = "#333333", weight = 0.6, opacity = 1,
              fillOpacity = 1, fillColor = ~cor,
              highlightOptions = leaflet::highlightOptions(color = "white", weight = 2, bringToFront = TRUE),
              group = "Terras"
            ) %>%
            leaflet.extras::addLabelOnlyMarkers(
              data = vcg_dissolved_centroids,
              lng = ~sf::st_coordinates(centroid)[, 1],
              lat = ~sf::st_coordinates(centroid)[, 2],
              label = ~grupo,
              labelOptions = leaflet::labelOptions(
                noHide = TRUE, direction = "top", textOnly = TRUE,
                style = list("color" = "black", "font-size" = "20px", "font-weight" = "bold")
              ),
              clusterOptions = leaflet::markerClusterOptions(),
              group = "Terras"
            )
        }
        if (isTRUE(!is.null(vcsg) && nrow(vcsg) > 0)) {
          map <- map %>%
            leaflet::addPolygons(
              data = vcsg,
              layerId = ~id_terra,
              popup = ~paste0(
                "<div style='background-color: #ffefcc; padding: 0px; border-radius: 0px;'>",
                "<b>ID Terra:</b> ", id_terra, "<br>",
                "<b>Codigo Imovel:</b> ", codigo_imovel, "<br>",
                "<b>Matricula:</b> ", matricula, "<br>",
                "<b>Nome Area:</b> ", nome_area, "<br>",
                "<b>Fonte Geo:</b> ", fonte_geo, "<br>",
                "<b>Area (ha):</b> ", area_ha, "<br>",
                "<b>Documento:</b> ", documento, "<br>",
                "<b>Nome Proprietario:</b> ", nome_proprietario, "<br>",
                "<b>Grupo:</b> ", grupo, "<br>",
                "<b>Fonte Proprietario:</b> ", fonte_proprietario, "<br>",
                "<b>Data Insercao:</b> ", data_insercao, "<br>",
                "<b>UF:</b> ", uf, "<br>",
                "<b>Municipio/UF:</b> ", municipio, "</div>"
              ),
              color = "#333333", weight = 0.6, opacity = 1,
              fillOpacity = 0.2, fillColor = "orange",
              highlightOptions = leaflet::highlightOptions(color = "white", weight = 2, bringToFront = TRUE),
              group = "Terras"
            )
        }
        if (isTRUE(!is.null(vnc) && nrow(vnc) > 0)) {
          map <- map %>%
            leaflet::addPolygons(
              data = vnc,
              layerId = ~id_terra,
              popup = ~paste0(
                "<div style='background-color:#e6e6e6; padding: 0px; border-radius: 0px;'>",
                "<b>ID Terra:</b> ", id_terra, "<br>",
                "<b>Codigo Imovel:</b> ", codigo_imovel, "<br>",
                "<b>Matricula:</b> ", matricula, "<br>",
                "<b>Nome Area:</b> ", nome_area, "<br>",
                "<b>Fonte Geo:</b> ", fonte_geo, "<br>",
                "<b>Area (ha):</b> ", area_ha, "<br>",
                "<b>Documento:</b> ", documento, "<br>",
                "<b>Nome Proprietario:</b> ", nome_proprietario, "<br>",
                "<b>Grupo:</b> ", grupo, "<br>",
                "<b>Fonte Proprietario:</b> ", fonte_proprietario, "<br>",
                "<b>Data Insercao:</b> ", data_insercao, "<br>",
                "<b>UF:</b> ", uf, "<br>",
                "<b>Municipio/UF:</b> ", municipio, "</div>"
              ),
              color = "#333333", weight = 0.6, opacity = 1,
              fillOpacity = 0.2, fillColor = "gray",
              highlightOptions = leaflet::highlightOptions(color = "white", weight = 2, bringToFront = TRUE),
              group = "Terras"
            )
        }
        download_data(vizinhos)
        output_progressbar(list(botao = 'vizinhos', etapa = 7, mensagem = 'Finalizada plotagem no mapa'))
        shinyWidgets::closeSweetAlert()
      }
    }
  })

  observeEvent(input$clear_button, {
    output$mymap <- leaflet::renderLeaflet(make_map())
  })
}
