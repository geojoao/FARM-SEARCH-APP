#' GeoServer integration: layers, WFS, legends
#' @import httr httr2 xml2 dplyr sf jsonlite
NULL

fetch_spatial_df <- function(url, cql_filter, camada) {
  body <- list(
    service = "WFS",
    version = "1.0.0",
    request = "GetFeature",
    typeName = camada,
    outputFormat = "application/json",
    CQL_FILTER = cql_filter
  )
  wfs_response <- httr::POST(url, body = body, encode = "form", config(ssl_verifypeer = FALSE))
  if (httr::status_code(wfs_response) == 200) {
    geojson_content <- httr::content(wfs_response, as = "text", encoding = "UTF-8")
    wfs_spatial_df <- sf::st_read(geojson_content, quiet = TRUE)
    return(wfs_spatial_df)
  } else {
    stop("Falha ao buscar os dados: ", httr::status_code(wfs_response))
  }
}

get_geoserver_layers <- function(
  url = "https://geoserver.bocombbm.com.br/geoserver/ows?service=WMS&version=1.3.0&request=GetCapabilities",
  ignore_workspaces = c()
) {
  tryCatch(
    {
      logg("Buscando layers do GeoServer...")
      resp <- httr::GET(url, config(ssl_verifypeer = FALSE, ssl_verifyhost = FALSE))
      txt <- httr::content(resp, as = "text", encoding = "UTF-8")
      doc <- xml2::read_xml(txt)
      layer_nodes <- xml2::xml_find_all(doc, "//*[local-name()='Layer' and @queryable='1']")
      layers_list <- lapply(layer_nodes, function(node) {
        name <- xml2::xml_text(xml2::xml_find_first(node, "./*[local-name()='Name']"))
        title <- xml2::xml_text(xml2::xml_find_first(node, "./*[local-name()='Title']"))
        if (is.na(title) || title == "") title <- name
        if (is.na(name)) return(NULL)
        data.frame(full_name = name, title = title, stringsAsFactors = FALSE)
      })
      layers_list <- layers_list[!sapply(layers_list, is.null)]
      if (length(layers_list) == 0) return(NULL)
      layers_df <- dplyr::bind_rows(layers_list) %>%
        dplyr::mutate(
          workspace = sub(":.*", "", full_name),
          layer_name = sub(".*:", "", full_name)
        ) %>%
        dplyr::filter(!workspace %in% ignore_workspaces)
      logg(paste("Encontradas", nrow(layers_df), "layers apos filtragem."))
      return(layers_df)
    },
    error = function(e) {
      logg(paste("Erro ao buscar layers do GeoServer:", e$message))
      return(NULL)
    }
  )
}

build_overlay_tree <- function(layers_df) {
  workspaces <- unique(layers_df$workspace)
  tree_children <- lapply(workspaces, function(ws) {
    ws_layers <- layers_df %>% dplyr::filter(workspace == ws)
    list(
      label = toupper(ws),
      collapsed = TRUE,
      children = lapply(1:nrow(ws_layers), function(i) {
        layer_title <- ws_layers$title[i]
        layer_title <- gsub("_", " ", layer_title)
        layer_title <- tools::toTitleCase(tolower(layer_title))
        list(label = layer_title, layerId = ws_layers$full_name[i])
      })
    )
  })
  list(label = "Camadas GeoServer", children = tree_children)
}

get_legend <- function(url) {
  tryCatch({
    req <- httr2::request(url) %>% httr2::req_options(ssl_verifypeer = 0)
    resp <- httr2::resp_body_json(httr2::req_perform(req))
    entries <- resp$Legend[[1]]$rules[[1]]$symbolizers[[1]]$Raster$colormap$entries
    df <- data.frame()
    for (i in seq_along(entries)) {
      entry <- entries[[i]]
      if (length(entry) == 3) {
        df_entry <- data.frame(
          label = entry$label,
          quantity = entry$quantity,
          color = entry$color
        )
        df <- rbind(df, df_entry)
      } else {
        print(paste0("Skipping entry ", i, ": Unexpected structure\n"))
      }
    }
    rownames(df) <- NULL
    return(df)
  }, error = function(e) {
    logg(paste0("Error: ", e$message, "\n"))
    return(data.frame(label = character(), quantity = numeric(), color = character()))
  })
}

get_polygon_legend <- function(url) {
  tryCatch({
    req <- httr2::request(url) %>% httr2::req_options(ssl_verifypeer = FALSE)
    resp <- httr2::req_perform(req)
    resp_json <- httr2::resp_body_json(resp)
    if (!"Legend" %in% names(resp_json) || length(resp_json$Legend) == 0) {
      stop("Invalid JSON structure: 'Legend' key not found or empty.")
    }
    legend_layers <- resp_json$Legend
    legend_entries <- list()
    for (layer in legend_layers) {
      if (!"rules" %in% names(layer) || length(layer$rules) == 0) next
      for (rule in layer$rules) {
        if (!"symbolizers" %in% names(rule) || length(rule$symbolizers) == 0) next
        polygon_symbolizer <- NULL
        for (symbolizer in rule$symbolizers) {
          if ("Polygon" %in% names(symbolizer)) {
            polygon_symbolizer <- symbolizer$Polygon
            break
          }
        }
        if (is.null(polygon_symbolizer) || !"fill" %in% names(polygon_symbolizer)) next
        fill_color <- polygon_symbolizer$fill
        legend_entries[[length(legend_entries) + 1]] <- list(
          layerName = layer$layerName,
          layerTitle = layer$title,
          ruleName = rule$name,
          ruleTitle = rule$title,
          fillColor = fill_color
        )
      }
    }
    dplyr::bind_rows(legend_entries)
  }, error = function(e) {
    print(paste("Error in get_polygon_legend:", e$message))
    return(NULL)
  })
}

discover_all_legends <- function(layers_df, ignored_layers = c()) {
  logg("Starting automatic legend discovery for all layers...")
  all_legends <- list()
  if (is.null(layers_df) || nrow(layers_df) == 0) {
    logg("No layers found for legend discovery.")
    return(all_legends)
  }
  for (i in seq_len(nrow(layers_df))) {
    full_name <- layers_df$full_name[i]
    title <- layers_df$title[i]
    if (full_name %in% ignored_layers) next
    legend_url <- paste0(
      "https://geoserver.bocombbm.com.br/geoserver/wms?REQUEST=GetLegendGraphic&LAYER=",
      full_name, "&FORMAT=application/json"
    )
    logg(paste("Attempting to discover legend for layer:", full_name))
    legend_data <- tryCatch({
      df <- get_legend(legend_url)
      if (!is.null(df) && nrow(df) > 0 && nrow(df) > 1) {
        list(type = "raster", data = df, layerId = full_name, title = title)
      } else NULL
    }, error = function(e) NULL)
    if (is.null(legend_data)) {
      legend_data <- tryCatch({
        df <- get_polygon_legend(legend_url)
        if (!is.null(df) && nrow(df) > 0 && nrow(df) > 1) {
          list(type = "polygon", data = df, layerId = full_name, title = title)
        } else NULL
      }, error = function(e) NULL)
    }
    if (!is.null(legend_data)) {
      all_legends[[length(all_legends) + 1]] <- legend_data
    } else {
      logg(paste("  -> No legend found for:", full_name))
    }
  }
  filtered_legends <- list()
  layer_groups <- list()
  for (legend in all_legends) {
    layer_id <- legend$layerId
    year_match <- regmatches(layer_id, gregexpr("_\\d{4}$", layer_id))
    if (length(year_match[[1]]) > 0) {
      year <- as.numeric(sub("_", "", year_match[[1]][1]))
      base_layer <- sub("_\\d{4}$", "", layer_id)
      if (!base_layer %in% names(layer_groups)) layer_groups[[base_layer]] <- list()
      layer_groups[[base_layer]][[length(layer_groups[[base_layer]]) + 1]] <- list(legend = legend, year = year)
    } else {
      filtered_legends[[length(filtered_legends) + 1]] <- legend
    }
  }
  for (base_layer in names(layer_groups)) {
    group <- layer_groups[[base_layer]]
    max_year <- max(sapply(group, function(x) x$year))
    for (item in group) {
      if (item$year == max_year) {
        filtered_legends[[length(filtered_legends) + 1]] <- item$legend
        break
      }
    }
  }
  logg(paste("Legend discovery complete. Found", length(filtered_legends), "legends after filtering duplicates."))
  return(filtered_legends)
}
