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


# options(future.globals.maxSize = 2024 * 1024 * 1024)

##### caso va utilizar os poligonos hachurados:
##devtools::install_github("statnmap/HatchedPolygons")
##https://stackoverflow.com/questions/54427190/r-leaflet-addpolygons-how-to-hatch-polygons?__cf_chl_tk=LIUrsoQC2jDbb50qOd6Zrhyq8V4JnPuwCKQ324Up9r4-1749591902-1.0.1.1-2hyinH1kNUvXHPNNWnFygDt2KWg6YunJ.KfANFiJoEo
#library(HatchedPolygons)
##### 


set_config(config(ssl_verifypeer = FALSE))


# Define a logging function
logg <- function(texto) {
  message(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), " | Farm Owners App | ", texto)
}

# Funcao para buscar as layers do GeoServer e agrupar por workspace
get_geoserver_layers <- function(
  url = "https://geoserver.bocombbm.com.br/geoserver/ows?service=WMS&version=1.3.0&request=GetCapabilities",
  ignore_workspaces = c()
) {
  tryCatch(
    {
      logg("Buscando layers do GeoServer...")
      resp <- GET(url, config(ssl_verifypeer = FALSE, ssl_verifyhost = FALSE))
      txt <- content(resp, as = "text", encoding = "UTF-8")
      doc <- read_xml(txt)
      layer_nodes <- xml_find_all(doc, "//*[local-name()='Layer' and @queryable='1']")
      layers_list <- lapply(layer_nodes, function(node) {
        name <- xml_text(xml_find_first(node, "./*[local-name()='Name']"))
        title <- xml_text(xml_find_first(node, "./*[local-name()='Title']"))
        if (is.na(title) || title == "") title <- name
        if (is.na(name)) {
          return(NULL)
        }
        data.frame(full_name = name, title = title, stringsAsFactors = FALSE)
      })
      layers_list <- layers_list[!sapply(layers_list, is.null)]
      if (length(layers_list) == 0) {
        return(NULL)
      }
      layers_df <- bind_rows(layers_list) %>%
        mutate(
          workspace = sub(":.*", "", full_name),
          layer_name = sub(".*:", "", full_name)
        ) %>%
        filter(!workspace %in% ignore_workspaces)
      logg(paste("Encontradas", nrow(layers_df), "layers apos filtragem."))
      return(layers_df)
    },
    error = function(e) {
      logg(paste("Erro ao buscar layers do GeoServer:", e$message))
      return(NULL)
    }
  )
}

# Monta a arvore de overlays agrupando por workspace
build_overlay_tree <- function(layers_df) {
  workspaces <- unique(layers_df$workspace)
  tree_children <- lapply(workspaces, function(ws) {
    ws_layers <- layers_df %>% filter(workspace == ws)
    list(
      label = toupper(ws), # Workspace em CAPSLOCK
      collapsed = TRUE,
      children = lapply(1:nrow(ws_layers), function(i) {
        # Nome do layer capitalizado e _ vira espaco
        layer_title <- ws_layers$title[i]
        layer_title <- gsub("_", " ", layer_title)
        layer_title <- tools::toTitleCase(tolower(layer_title))
        list(label = layer_title, layerId = ws_layers$full_name[i])
      })
    )
  })
  list(
    label = "Camadas GeoServer",
    children = tree_children
  )
}


############################################################# funcao de definicao de mapa #######################################3

# Define an R-friendly function to add a layers control tree.
addLayersControlTree <- function(map, baseTree, overlayTree = NULL, options = list(), hiddenLayers = NULL) {
  
  # Define the htmlDependency for the Leaflet.Control.Layers.Tree plugin.
  layerTreePlugin <- htmlDependency(
    name = "Leaflet.Control.Layers.Tree",
    version = "1.1.1",
    src = c(href = "https://cdn.jsdelivr.net/npm/leaflet.control.layers.tree@1.1.1/"),
    script = "L.Control.Layers.Tree.js",
    stylesheet = "L.Control.Layers.Tree.css"
  )
  
  # Helper function to add a plugin dependency to the leaflet map.
  registerPlugin <- function(map, plugin){
    map$dependencies <- c(map$dependencies, list(plugin))
    map
  }
  
  defaultOptions <- list(
    namedToggle = FALSE,
    collapseAll = "Collapse all",
    expandAll = "Expand all",
    collapsed = FALSE # This refers to the control itself, not the tree nodes
  )
  options <- modifyList(defaultOptions, options)
  
  baseTreeJSON <- toJSON(baseTree, auto_unbox = TRUE, json_verbatim = TRUE)
  overlayTreeJSON <- if (!is.null(overlayTree)) toJSON(overlayTree, auto_unbox = TRUE, json_verbatim = TRUE) else "null"
  optionsJSON <- toJSON(options, auto_unbox = TRUE, json_verbatim = TRUE)
  hiddenLayersJSON <- if (!is.null(hiddenLayers)) toJSON(hiddenLayers, auto_unbox = TRUE) else "[]"
  
  jsCode <- sprintf("function(el, x) {
    var mapInstance = this;

    function getLayerById(id) {
      var found = null;
      mapInstance.eachLayer(function(layer) {
        if(layer.options && layer.options.layerId === id) {
          found = layer;
        }
      });
      return found;
    }

    function assignLayers(tree) {
      if(tree.layerId) { // Check if layerId exists
        tree.layer = getLayerById(tree.layerId);
      }
      if(tree.children) {
        for(var i = 0; i < tree.children.length; i++) {
          assignLayers(tree.children[i]);
        }
      }
    }

    var baseTreeObj = %s;
    var overlayTreeObj = %s;
    assignLayers(baseTreeObj);
    if (overlayTreeObj !== null) {
      assignLayers(overlayTreeObj);
    }
  
    var ctlOptions = %s;
    var controlSpecificOptions = {
        namedToggle: ctlOptions.namedToggle,
        collapseAll: ctlOptions.collapseAll,
        expandAll: ctlOptions.expandAll,
        collapsed: ctlOptions.collapsed,
        labelIsSelector: ctlOptions.labelIsSelector,
        closedSymbol: ctlOptions.closedSymbol,
        openedSymbol: ctlOptions.openedSymbol,
        spaceSymbol: ctlOptions.spaceSymbol,
        selectorAddPoints: ctlOptions.selectorAddPoints,
        groupSelectorParent: ctlOptions.groupSelectorParent,
        position: ctlOptions.position,
        selectorBack: ctlOptions.selectorBack
    };


    var ctl = L.control.layers.tree(baseTreeObj, overlayTreeObj, controlSpecificOptions);
    ctl.addTo(mapInstance).collapseTree(true).expandSelected(false); // collapseTree(true) collapses all, expandSelected(false) means don't auto-expand previously selected. Or use .collapseTree().expandSelected() as in your example.

    var hiddenLayers = %s;
    hiddenLayers.forEach(function(layerId) {
      var layer = getLayerById(layerId);
      if (layer) {
        mapInstance.removeLayer(layer); // This hides the layer by removing it
      }
    });
  }", baseTreeJSON, overlayTreeJSON, optionsJSON, hiddenLayersJSON)
  
  onRender(map %>% registerPlugin(layerTreePlugin), jsCode)
}

adjust_map_controls <- function(m){
  
  # Define the base tree structure.
  baseTree <- list(
    label = "Base Maps",
    # selected = TRUE, # Optional: mark this radio group as selected initially
    children = list(
      list(label = "Satellite", layerId = "Satellite"),
      list(label = "Elevation", layerId = "Elevation"),
      list(label = "OpenStreetMap", layerId = "OpenStreetMap", selected = TRUE) # Mark one as default selected
    )
  )
  
  # Define the overlay tree structure
  overlayTree <- list(
    label = "Overlays",
    # selectAllCheckbox = TRUE, # Adds a checkbox to select/deselect all in this top category
    children = list(
      list(
        label = "Mapbiomas",
        # selectAllCheckbox = TRUE,
        children = list(
          list(
            label = "Land Cover",
            collapsed = TRUE,
            # selectAllCheckbox = TRUE,
            children = list(
              list(label = "Mapbiomas - 2000", layerId = "Mapbiomas - 2000"),
              list(label = "Mapbiomas - 2001", layerId = "Mapbiomas - 2001"),
              list(label = "Mapbiomas - 2002", layerId = "Mapbiomas - 2002"),
              list(label = "Mapbiomas - 2003", layerId = "Mapbiomas - 2003"),
              list(label = "Mapbiomas - 2004", layerId = "Mapbiomas - 2004"),
              list(label = "Mapbiomas - 2005", layerId = "Mapbiomas - 2005"),
              list(label = "Mapbiomas - 2006", layerId = "Mapbiomas - 2006"),
              list(label = "Mapbiomas - 2007", layerId = "Mapbiomas - 2007"),
              list(label = "Mapbiomas - 2008", layerId = "Mapbiomas - 2008"),
              list(label = "Mapbiomas - 2009", layerId = "Mapbiomas - 2009"),
              list(label = "Mapbiomas - 2010", layerId = "Mapbiomas - 2010"),
              list(label = "Mapbiomas - 2011", layerId = "Mapbiomas - 2011"),
              list(label = "Mapbiomas - 2012", layerId = "Mapbiomas - 2012"),
              list(label = "Mapbiomas - 2013", layerId = "Mapbiomas - 2013"),
              list(label = "Mapbiomas - 2014", layerId = "Mapbiomas - 2014"),
              list(label = "Mapbiomas - 2015", layerId = "Mapbiomas - 2015"),
              list(label = "Mapbiomas - 2016", layerId = "Mapbiomas - 2016"),
              list(label = "Mapbiomas - 2017", layerId = "Mapbiomas - 2017"),
              list(label = "Mapbiomas - 2018", layerId = "Mapbiomas - 2018"),
              list(label = "Mapbiomas - 2019", layerId = "Mapbiomas - 2019"),
              list(label = "Mapbiomas - 2020", layerId = "Mapbiomas - 2020"),
              list(label = "Mapbiomas - 2021", layerId = "Mapbiomas - 2021"),
              list(label = "Mapbiomas - 2022", layerId = "Mapbiomas - 2022"),
              list(label = "Mapbiomas - 2023", layerId = "Mapbiomas - 2023"),
              list(label = "Mapbiomas - 2024", layerId = "Mapbiomas - 2024")
            )
          ),
          list(
            label = "Specific Use",
            collapsed = TRUE,
            # selectAllCheckbox = TRUE,
            children = list(
              list(label = "Agriculture - 2023", layerId = "Agriculture - 2023"),
              list(label = "Irrigation - 2022", layerId = "Irrigation - 2022")
            )
          ),
          list(
            label = "Analysis",
            collapsed = TRUE,
            # selectAllCheckbox = TRUE,
            children = list(
              list(label = "Maturity", layerId = "Maturity"),
              list(label = "Consolidation", layerId = "Consolidation")
            )
          )
        )
      ),
      list(
        label = "Land Information",
        # selectAllCheckbox = TRUE,
        children = list(
          list(
            label = "Rural Properties (INCRA)",
            collapsed = TRUE,
            # selectAllCheckbox = TRUE,
            children = list(
              list(label = "SIGEF", layerId = "Rural Properties - SIGEF"),
              list(label = "SNCI", layerId = "Rural Properties - SNCI")
            )
          ),
          list(
            label = "BBM",
            collapsed = TRUE,
            children = list(
              list(label = "Groups Exposure", layerId = "BBM - Groups Exposure")
            )
          )
        )
      ),
      list(
        label = "Environmental Data",
        # selectAllCheckbox = TRUE,
        children = list(
          list(
            label = "Soil",
            collapsed = TRUE,
            # selectAllCheckbox = TRUE,
            children = list(
              list(label = "Clay 0-30cm (Mapbiomas)", layerId = "Clay 0 to 30cm - Mapbiomas"),
              list(label = "Soil - Embrapa", layerId = "Soil - Embrapa")
            )
          ),
          list(
            label = "Biomes",
            collapsed = TRUE,
            children = list(
              list(label = "Biomes - IBGE", layerId = "Biomes - IBGE")
            )
          ),
          list(
            label = "Fire Monitoring (NASA)",
            collapsed = TRUE,
            # selectAllCheckbox = TRUE,
            children = list(
              list(label = "Fire Heatmap (Last 2 Months)", layerId = "Fire Heatmap Last 2 Months - NASA"),
              list(label = "Fire Spots (Last 2 Months)", layerId = "Fire Spots Last 2 Months - NASA")
            )
          )
        )
      ),
      list(
        label = "Administrative Boundaries",
        children = list(
          list(label = "Cities - IBGE", layerId = "Cities - IBGE")
        )
      )
    )
  )
  
  # Layers to be initially hidden (removed from map, but available in control tree)
  # This matches the groups previously hidden using hideGroup()
  hiddenOverlayLayers <- c(
    "Mapbiomas - 2000", "Mapbiomas - 2001", "Mapbiomas - 2002", "Mapbiomas - 2003", "Mapbiomas - 2004", "Mapbiomas - 2005", 
    "Mapbiomas - 2006", "Mapbiomas - 2007", "Mapbiomas - 2008", "Mapbiomas - 2009", "Mapbiomas - 2010", "Mapbiomas - 2011", 
    "Mapbiomas - 2012", "Mapbiomas - 2013", "Mapbiomas - 2014", "Mapbiomas - 2015", "Mapbiomas - 2016", "Mapbiomas - 2017", 
    "Mapbiomas - 2018", "Mapbiomas - 2019", "Mapbiomas - 2020", "Mapbiomas - 2021", "Mapbiomas - 2022", "Mapbiomas - 2023",
    "Mapbiomas - 2024",
    "Agriculture - 2023",
    "Rural Properties - SIGEF", "Rural Properties - SNCI",
    "Irrigation - 2022",
    "Clay 0 to 30cm - Mapbiomas", "Soil - Embrapa",
    "Biomes - IBGE",
    "BBM - Groups Exposure",
    "Fire Heatmap Last 2 Months - NASA", "Fire Spots Last 2 Months - NASA",
    "Cities - IBGE",
    "Maturity", "Consolidation"
  )
  
  hiddenLegendLayers = c("Mapbiomas LandCover Legend", "Agriculture Legend", "Maturity Legend", "Consolidation Legend", "IBGE Biomes Legend")
  
  
  # Add the layers control tree to the map
  # The options list for addLayersControlTree can include specific L.Control.Layers.Tree JS options
  # The 'collapsed' option here refers to the control box itself, not the tree nodes.
  # The JS function handles tree node collapse/expand.
  m <- addLayersControlTree(m,
                            baseTree = baseTree,
                            overlayTree = overlayTree,
                            options = list(
                              collapsed = TRUE, 
                              position = "bottomleft", 
                              namedToggle = T,
                              selectorBack = F,
                              closedSymbol = '&#8862; &#x1f5c0;',
                              openedSymbol = '&#8863; &#x1f5c1;'
                            ), # Control box collapsed, position
                            hiddenLayers = hiddenOverlayLayers
  ) %>%
    addLayersControl(
      overlayGroups = hiddenLegendLayers,
      options = layersControlOptions(collapsed = F),
      position = 'bottomleft'
    ) %>%
    hideGroup(hiddenLegendLayers)
  return(m)
}

discover_all_legends <- function(layers_df, ignored_layers = c()) {
  logg("Starting automatic legend discovery for all layers...")
  
  all_legends <- list()
  
  if (is.null(layers_df) || nrow(layers_df) == 0) {
    logg("No layers found for legend discovery.")
    return(all_legends)
  }
  
  for (i in seq_len(nrow(layers_df))) {
    workspace <- layers_df$workspace[i]
    full_name <- layers_df$full_name[i]
    title <- layers_df$title[i]
    
    # Skip layers in the ignore list
    if (full_name %in% ignored_layers) {
      logg(paste("Skipping legend discovery for ignored layer:", full_name))
      next
    }
    
    legend_url <- paste0(
      "https://geoserver.bocombbm.com.br/geoserver/wms?REQUEST=GetLegendGraphic&LAYER=",
      full_name,
      "&FORMAT=application/json"
    )
    
    logg(paste("Attempting to discover legend for layer:", full_name))
    
    # Try get_legend (Raster/Colormap type)
    legend_data <- tryCatch({
      df <- get_legend(legend_url)
      if (!is.null(df) && nrow(df) > 0) {
        # Skip legends with only one class
        if (nrow(df) == 1) {
          logg(paste("  -> Skipping raster legend for", full_name, "(only 1 class)"))
          NULL
        } else {
          logg(paste("  -> Found raster-type legend for:", full_name))
          list(
            type = "raster",
            data = df,
            layerId = full_name,
            title = title
          )
        }
      } else {
        NULL
      }
    }, error = function(e) {
      NULL
    })
    
    # If raster legend not found, try get_polygon_legend (Polygon type)
    if (is.null(legend_data)) {
      legend_data <- tryCatch({
        df <- get_polygon_legend(legend_url)
        if (!is.null(df) && nrow(df) > 0) {
          # Skip legends with only one class
          if (nrow(df) == 1) {
            logg(paste("  -> Skipping polygon legend for", full_name, "(only 1 class)"))
            NULL
          } else {
            logg(paste("  -> Found polygon-type legend for:", full_name))
            list(
              type = "polygon",
              data = df,
              layerId = full_name,
              title = title
            )
          }
        } else {
          NULL
        }
      }, error = function(e) {
        NULL
      })
    }
    
    if (!is.null(legend_data)) {
      all_legends[[length(all_legends) + 1]] <- legend_data
    } else {
      logg(paste("  -> No legend found for:", full_name))
    }
  }
  
  # Filter out duplicate year-based legends, keeping only the latest year
  filtered_legends <- list()
  layer_groups <- list()
  
  for (legend in all_legends) {
    layer_id <- legend$layerId
    title <- legend$title
    
    # Extract year from layer_id (e.g., "mapbiomas_agriculture_2023" -> 2023)
    year_match <- regmatches(layer_id, gregexpr("_\\d{4}$", layer_id))
    
    if (length(year_match[[1]]) > 0) {
      # Extract the year number
      year <- as.numeric(sub("_", "", year_match[[1]][1]))
      # Get base layer name (remove the year suffix)
      base_layer <- sub("_\\d{4}$", "", layer_id)
      
      # Store legend with its base name and year
      if (!base_layer %in% names(layer_groups)) {
        layer_groups[[base_layer]] <- list()
      }
      
      layer_groups[[base_layer]][[length(layer_groups[[base_layer]]) + 1]] <- list(
        legend = legend,
        year = year
      )
    } else {
      # No year pattern found, keep this legend as is
      filtered_legends[[length(filtered_legends) + 1]] <- legend
    }
  }
  
  # For each base layer, keep only the legend with the highest year
  for (base_layer in names(layer_groups)) {
    group <- layer_groups[[base_layer]]
    
    # Find the legend with the maximum year
    max_year <- max(sapply(group, function(x) x$year))
    max_legend <- NULL
    
    for (item in group) {
      if (item$year == max_year) {
        max_legend <- item$legend
        break
      }
    }
    
    if (!is.null(max_legend)) {
      logg(paste("  -> Keeping legend for year", max_year, "from", base_layer))
      filtered_legends[[length(filtered_legends) + 1]] <- max_legend
    }
  }
  
  logg(paste("Legend discovery complete. Found", length(filtered_legends), "legends after filtering duplicates."))
  return(filtered_legends)
}

make_map <- function(){
  # Fetch layers from GeoServer
  layers_df <- get_geoserver_layers()
  
  # Discover legends for all layers
  discovered_legends <- discover_all_legends(layers_df, ignored_layers = c())
  
  m <- leaflet(options = leafletOptions(attributionControl = FALSE)) %>%
    addProviderTiles(providers$OpenStreetMap, group = "OpenStreetMap", options = providerTileOptions(layerId = "OpenStreetMap")) %>%
    addProviderTiles(providers$Esri.WorldImagery, group = "Satellite", options = providerTileOptions(layerId = "Satellite")) %>%
    addProviderTiles(providers$Esri.WorldTopoMap, group = "Elevation", options = providerTileOptions(layerId = "Elevation"))

  if (!is.null(layers_df)) {
    for (i in seq_len(nrow(layers_df))) {
      m <- m %>% addWMSTiles(
        baseUrl = paste0("https://geoserver.bocombbm.com.br/geoserver/", layers_df$workspace[i], "/wms"),
        layers = layers_df$full_name[i],
        layerId = layers_df$full_name[i],
        options = WMSTileOptions(
          format = "image/png",
          transparent = TRUE,
          layerId = layers_df$full_name[i]
        ),
        group = layers_df$title[i]
      )
    }
  }

  # Add dynamically discovered legends
  if (!is.null(discovered_legends) && length(discovered_legends) > 0) {
    for (legend_item in discovered_legends) {
      tryCatch({
        legend_data <- legend_item$data
        legend_title <- legend_item$title
        legend_group <- legend_item$title
        
        if (legend_item$type == "raster") {
          # Handle raster-type legends with color and label columns
          if ("color" %in% names(legend_data) && "label" %in% names(legend_data)) {
            m <- m %>%
              addLegend(
                "bottomright",
                colors = legend_data$color,
                labels = legend_data$label,
                title = paste0(legend_group,' Legend'),
                opacity = 1,
                group = paste0(legend_group,' Legend'),
                layerId = paste0(legend_group,' Legend')
              )
          }
        } else if (legend_item$type == "polygon") {
          # Handle polygon-type legends with fillColor and ruleTitle columns
          if ("fillColor" %in% names(legend_data) && "ruleTitle" %in% names(legend_data)) {
            m <- m %>%
              addLegend(
                "bottomright",
                colors = legend_data$fillColor,
                labels = legend_data$ruleTitle,
                title = paste0(legend_group,' Legend'),
                opacity = 1,
                group = paste0(legend_group,' Legend'),
                layerId = paste0(legend_group,' Legend')
              )
          }
        }
        logg(paste("Added dynamic legend for:", legend_title))
      }, error = function(e) {
        logg(paste("Error adding legend for", legend_item$title, ":", e$message))
      })
    }
  }

  # Build overlay tree from discovered layers
  overlayTree <- if (!is.null(layers_df)) build_overlay_tree(layers_df) else NULL
  
  baseTree <- list(
    label = "Base Maps",
    children = list(
      list(label = "Satellite", layerId = "Satellite"),
      list(label = "Elevation", name = 'Map Layers', layerId = "Elevation"),
      list(label = "OpenStreetMap", layerId = "OpenStreetMap", selected = TRUE)
    )
  )
  
  # Build legend layer names dynamically from discovered legends
  hiddenLegendLayers <- c()
  
  # Add dynamically discovered legend names
  if (!is.null(discovered_legends) && length(discovered_legends) > 0) {
    for (legend_item in discovered_legends) {
      hiddenLegendLayers <- c(hiddenLegendLayers, paste0(legend_item$title, ' Legend'))
    }
  }
  
  # Remove duplicates
  hiddenLegendLayers <- unique(hiddenLegendLayers)

  # Layers to be initially hidden
  hiddenOverlayLayers <- if (!is.null(layers_df)) layers_df$full_name else NULL
  
  m <- addLayersControlTree(m,
                            baseTree = baseTree,
                            overlayTree = overlayTree,
                            options = list(
                              collapsed = TRUE,
                              position = "bottomleft",
                              namedToggle = TRUE,
                              selectorBack = FALSE,
                              closedSymbol = '&#8862; &#x1f5c0;',
                              openedSymbol = '&#8863; &#x1f5c1;'
                            ),
                            hiddenLayers = hiddenOverlayLayers
  ) %>%
    addLayersControl(
      overlayGroups = hiddenLegendLayers,
      options = layersControlOptions(collapsed = FALSE),
      position = 'bottomleft'
    ) %>%
    hideGroup(hiddenLegendLayers)
  
  # Set initial view
  m <- m %>% setView(lng = -47.9292, lat = -15.7801, zoom = 4)
  
  # Display the map
  return(m)
}

get_legend <- function(url = "https://geoserver.bocombbm.com.br/geoserver/wms?REQUEST=GetLegendGraphic&LAYER=agriculture%3Amapbiomas_agriculture_2022&FORMAT=application/json") {
  tryCatch({
    # Make the HTTP request and ignore SSL certificate verification
    req <- request(url) %>%
      req_options(ssl_verifypeer = 0)
    resp <- resp_body_json(req_perform(req))
    
    # Extract the relevant data
    entries <- resp$Legend[[1]]$rules[[1]]$symbolizers[[1]]$Raster$colormap$entries
    
    # Create an empty dataframe to store the data
    df <- data.frame()
    
    # Iterate through the entries and add them to the dataframe
    for (i in seq_along(entries)) {
      entry <- entries[[i]]
      # Check if the entry has the expected structure
      if (length(entry) == 3) {
        df_entry <- data.frame(
          label = entry$label,
          quantity = entry$quantity,
          color = entry$color
        )
        df <- rbind(df, df_entry)
      } else {
        # Skip entries with unexpected structure
        # logg(paste0("Skipping entry ", i, ": Unexpected structure\n"))
        print(paste0("Skipping entry ", i, ": Unexpected structure\n"))
      }
    }
    
    # Reset row names
    rownames(df) <- NULL
    return(df)
  }, error = function(e) {
    # Print the error message
    logg(paste0("Error: ", e$message, "\n"))
    # Return a dummy dataframe
    return(data.frame(
      label = character(),
      quantity = numeric(),
      color = character()
    ))
  })
}

get_polygon_legend <- function(url) {
  tryCatch({
    req <- request(url) %>%
      req_options(ssl_verifypeer = FALSE)
    resp <- req_perform(req)
    
    resp_json <- resp_body_json(resp)
    
    if (!"Legend" %in% names(resp_json) || length(resp_json$Legend) == 0) {
      stop("Invalid JSON structure: 'Legend' key not found or empty.")
    }
    
    legend_layers <- resp_json$Legend
    legend_entries <- list()
    
    for (layer in legend_layers) {
      layer_name <- layer$layerName
      layer_title <- layer$title
      
      if (!"rules" %in% names(layer) || length(layer$rules) == 0) {
        # logg(paste0("No rules found for layer: ", layer_name))
        print(paste0("No rules found for layer: ", layer_name))
        next
      }
      
      for (rule in layer$rules) {
        rule_name <- rule$name
        rule_title <- rule$title
        
        if (!"symbolizers" %in% names(rule) || length(rule$symbolizers) == 0) {
          # logg(paste0("No symbolizers found for rule: ", rule_name))
          print(paste0("No symbolizers found for rule: ", rule_name))
          next
        }
        
        polygon_symbolizer <- NULL
        for (symbolizer in rule$symbolizers) {
          if ("Polygon" %in% names(symbolizer)) {
            polygon_symbolizer <- symbolizer$Polygon
            break
          }
        }
        
        if (is.null(polygon_symbolizer)) {
          # logg(paste0("No Polygon symbolizer found for rule: ", rule_name))
          print(paste0("No Polygon symbolizer found for rule: ", rule_name))
          next
        }
        
        if (!"fill" %in% names(polygon_symbolizer)) {
          # logg(paste0("No 'fill' color found for rule: ", rule_name))
          print(paste0("No 'fill' color found for rule: ", rule_name))
          next
        }
        
        fill_color <- polygon_symbolizer$fill
        
        legend_entries[[length(legend_entries) + 1]] <- list(
          layerName = layer_name,
          layerTitle = layer_title,
          ruleName = rule_name,
          ruleTitle = rule_title,
          fillColor = fill_color
        )
      }
    }
    
    legend_df <- bind_rows(legend_entries)
    
    return(legend_df)
    
  }, error = function(e) {
    # logg(paste("Error in get_biomas_legend:", e$message))
    print(paste("Error in get_biomas_legend:", e$message))
    return(NULL)
  })
}

logg('Getting legends from geoserver with dynamic discovery...')


################################################################################################################################







##### abrir o arquivo no server geral



# Variavel global para armazenar os dados
all_data <- NULL
print('abrindo arquivo')

# Funcao para carregar os dados
load_all_data <- function() {
  query_all_data <- "SELECT grupo, uf, id_terra, municipio, nome_proprietario, documento, codigo_imovel, matricula, nome_area FROM terras_e_proprietarios"
  x <- st_read('~/AFS/Pesquisa_Quantitativa/Compartilhado/rundeck_files_folder/Farm_Owners_Data/farm_owners_data.gpkg',
               query = query_all_data,
               quiet = TRUE) #%>% st_set_geometry(NULL)
  
  # Adicionar o horario atual ao log com cat()
  cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "- atualizou dado\n")  # Verificacao de atualizacao com horario
  return(x)
}

# Carregar os dados inicialmente
all_data <- load_all_data()
print('Dados carregados inicialmente')

# Atualizar os dados periodicamente no contexto global
update_data_observer <- observe({
  invalidateLater(5 * 60 * 1000)  # Atualiza a cada 30 minutos
  all_data <<- load_all_data()  # Atualiza a Variavel global
})

# UI
ui <- fluidPage(
  titlePanel("Exemplo com Dados Compartilhados"),
  tableOutput("data_table")
)





###### dashboard header
# dashboard_header = dashboardHeader(title = "Farm and Owners App")
# dashboard_header$children[[2]]$children <-  tags$a(href='https://rstudio.bocombbm.com.br/app/quant_agro_doc/apps/app-list.html',
#                                                    tags$img(src = 'logo_h.png', height = "100%"))


dashboard_header = dashboardHeader(
  title = "Farm and Owners Search",
  tags$li(class = "dropdown",
          tags$a(href = 'https://rstudio.bocombbm.com.br/app/quant_agro_doc/apps/app-list.html',
                 tags$img(src = 'logo.png',width= 200, align = "right", align="center")
          )
  ))

###### dashboard sidebar
dashboard_sidebar = dashboardSidebar(
  sidebarMenu(
    selectInput("Grupo_proprietario", "Group", choices = c(''), selected = NULL),
    selectInput("Nome_proprietario", "Nome", choices = c(''), selected = NULL),
    selectInput("Documento_proprietario", "Documento", choices = c(''), selected = NULL),
    selectInput("UF", "Estado", choices = c(''), selected = NULL, multiple = TRUE),
    selectInput("Municipio", "Municipio", choices = c(''), selected = NULL, multiple = TRUE),
    selectInput("Codigo_Imovel_Rural", "Codigo imovel", choices = c(''), selected = NULL, multiple = TRUE),
    selectInput("Nome_Fazenda", "Fazenda", choices = c(''), selected = NULL, multiple = TRUE),
    # textInput("Nome_Fazenda", "Fazenda"),
    selectInput("Matricula", "Matricula", selected = NULL, choices = c(''), multiple = TRUE),
    # textInput("Matricula", "Matricula"),
    selectInput("ID_Terra", "ID Terra", selected = NULL, choices = c(''), multiple = TRUE),
    actionButton("search_button", "Search"),
    actionButton("load_neighbors_button", "Load Neighbors"),
    actionButton("clear_button", "Clear"),
    hr(),
    uiOutput("download_buttons")
  )
)

##### dashboard body
dashboard_body = dashboardBody(
  tags$head(       
    includeCSS("https://cdn.jsdelivr.net/npm/leaflet.control.layers.tree@1.1.1/L.Control.Layers.Tree.css"),
    includeScript("https://cdn.jsdelivr.net/npm/leaflet.control.layers.tree@1.1.1/L.Control.Layers.Tree.js"),
    tags$style(HTML("
          .content {
          min-height: 250px;
          padding: 0px;
      }
      @media (min-width: 768px) {
          .navbar-nav>li>a {
              padding-top: 5px;
              padding-bottom: 0px;
          }
      }
      .leaflet-container {
        height: 95vh !important;  Adjust for the height of the header */
      }
    .btn-primary {
        background-color: #91372c;
        border-color: #000000;
    }
    
    .skin-black .sidebar a {
        color: #eee;
    }
    .btn-primary:hover {
        background-color: #dd4b39;
    }
    "))
  ),
  leafletOutput("mymap")
  # leafletOutput("mymap", height="93vh")
)
# .content-wrapper, .right-side {
#   height: 80vh !important;
# }

# .leaflet-container {
#   height: 92vh !important;  Adjust for the height of the header */
# }

ui <- dashboardPage(skin='black',
                    dashboard_header,
                    dashboard_sidebar,
                    dashboard_body
)


# Define server logic ----
server <- function(input, output, session) {
  
  ########## comeca Variavel reativa de filtered data 
  download_data <- reactiveVal(NULL)
  
  ##### define Variavel para carregamento da barra (tambem universal para os botoes) e tambem o path da queue dessa sessao
  ##### alem do valor reativo de cada chamada da progressbar 
  file_source_queue_progressbar <- TextFileSource$new()
  queue_progressbar <- shinyQueue(source = file_source_queue_progressbar, session = session)
  queue_progressbar$consumer$start(100)
  output_progressbar <- reactiveVal(list(botao=NULL, etapa=NULL, mensagem=NULL))
  
  
  ##### define Variavel para retorno dos resultados (tambem universal para os botoes) e tambem o path da queue dessa sessao
  ##### alem do valor reativo de cada chamada da progressbar 
  file_source_queue_results <- TextFileSource$new()
  queue_results <- shinyQueue(source = file_source_queue_results, session = session)
  queue_results$consumer$start(100)
  output_results <- reactiveVal(list(botao=NULL, erro=NULL, warning=NULL, texto=NULL, terras_filtradas=NULL, vizinhos=NULL,
                                     vcg=NULL, vcsg=NULL, vnc=NULL, vcg_dissolved_centroids=NULL))
  
  
  ########## botao de download
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
          st_write(data, file, driver = "GPKG", quiet = TRUE)
          removeModal()
        }, error = function(e) {
          removeModal()
          showModal(modalDialog(paste("Erro exportando o arquivo gpkg:", e$message), easyClose = TRUE))
        })
      }
    }
  )
  
  output$download_buttons <- renderUI({  # nao consegui fazer o botao ficar "disponivel", acho q a ui ta indo antes do botao mas n consegui resolver ainda
    req(download_data())  # So exibe se houver dados
    # downloadButton("download_gpkg", "Download gpkg", style='')
    div(
      style = "text-align: center;",  # Centraliza o conteudo dentro do div
      downloadButton("download_gpkg", "Download gpkg", class = "btn-primary")
    )
  })    
  
  
  
  
  print('comecou a carregar as labels de pesquisa')
  # withProgress(
  #   min = 1,
  #   max = 12, {
  # grupos_proprietarios_all <- st_read('~/AFS/Pesquisa_Quantitativa/Compartilhado/rundeck_files_folder/Farm_Owners_Data/all_rural_properties.sqlite', query='SELECT DISTINCT(grupo) FROM terras_e_proprietarios', quiet=T)$grupo
  # setProgress(1, message = "Carregando datasets")
  # 
  # ufs <- st_read('~/AFS/Pesquisa_Quantitativa/Compartilhado/rundeck_files_folder/Farm_Owners_Data/all_rural_properties.sqlite', query='SELECT DISTINCT(uf) FROM terras_e_proprietarios', quiet=T)$uf
  # setProgress(2, message = "Carregando datasets")
  # 
  # id_terra_query = '
  #     SELECT DISTINCT id_terra FROM terras_e_proprietarios;'
  # id_terras <- unique(st_read('~/AFS/Pesquisa_Quantitativa/Compartilhado/rundeck_files_folder/Farm_Owners_Data/all_rural_properties.sqlite', query=id_terra_query, quiet=T)$id_terra)
  # setProgress(3, message = "Carregando datasets")
  # 
  # municipios <- st_read('~/AFS/Pesquisa_Quantitativa/Compartilhado/rundeck_files_folder/Farm_Owners_Data/all_rural_properties.sqlite', query='SELECT DISTINCT(municipio) FROM terras_e_proprietarios', quiet=T)$municipio
  # setProgress(4, message = "Carregando datasets")
  # 
  # nomes_proprietarios <- st_read('~/AFS/Pesquisa_Quantitativa/Compartilhado/rundeck_files_folder/Farm_Owners_Data/all_rural_properties.sqlite', query='SELECT DISTINCT(nome_proprietario) FROM terras_e_proprietarios', quiet=T)$nome_proprietario
  # setProgress(5, message = "Carregando datasets")
  # 
  # documentos <- st_read('~/AFS/Pesquisa_Quantitativa/Compartilhado/rundeck_files_folder/Farm_Owners_Data/all_rural_properties.sqlite', query='SELECT DISTINCT(documento) FROM terras_e_proprietarios', quiet=T)$documento
  # setProgress(6, message = "Carregando datasets")
  # 
  # codigos_imoveis <- st_read('~/AFS/Pesquisa_Quantitativa/Compartilhado/rundeck_files_folder/Farm_Owners_Data/all_rural_properties.sqlite', query='SELECT DISTINCT(codigo_imovel) FROM terras_e_proprietarios', quiet=T)$codigo_imovel
  # setProgress(7, message = "Carregando datasets")
  # 
  # nomes_fazendas <- unique(st_read('~/AFS/Pesquisa_Quantitativa/Compartilhado/rundeck_files_folder/Farm_Owners_Data/all_rural_properties.sqlite', query='SELECT DISTINCT(matricula) FROM terras_e_proprietarios', quiet=T)$matricula)
  # setProgress(8, message = "Carregando datasets")
  # 
  # matriculas <- st_read('~/AFS/Pesquisa_Quantitativa/Compartilhado/rundeck_files_folder/Farm_Owners_Data/all_rural_properties.sqlite', query='SELECT DISTINCT(nome_area) FROM terras_e_proprietarios', quiet=T)$nome_area
  # setProgress(8, message = "Carregando datasets")
  
  progressSweetAlert(
    session = session,
    id = "progress",
    title = 'semtitulo',
    display_pct = TRUE,
    value = 0
  )
  
  # Carregar o arquivo inteiro na memoria
  # setProgress(1, message = "Carregando datasets")
  updateProgressBar(session = session, id = "progress", value = 1, total = 12, title='Carregando arquivo sqlite')
  query_all_data <- "SELECT grupo, uf, id_terra, municipio, nome_proprietario, documento, codigo_imovel, matricula, nome_area FROM terras_e_proprietarios"
  # all_data <- st_read('~/AFS/Pesquisa_Quantitativa/Compartilhado/rundeck_files_folder/Farm_Owners_Data/all_rural_properties.sqlite', query=query_all_data, quiet=T)
  
  # Criar as variaveis aplicando `unique` nas colunas
  # setProgress(2, message = "Carregando datasets")
  updateProgressBar(session = session, id = "progress", value = 2, total = 12, title='Carregando valores do formulario')
  grupos_proprietarios_all <- unique(all_data$grupo)
  
  # setProgress(3, message = "Carregando datasets")
  updateProgressBar(session = session, id = "progress", value = 3, total = 12, title='Carregando valores do formulario')
  ufs <- unique(all_data$uf)
  
  # setProgress(4, message = "Carregando datasets")
  updateProgressBar(session = session, id = "progress", value = 4, total = 12, title='Carregando valores do formulario')
  id_terras <- unique(all_data$id_terra)
  
  # setProgress(5, message = "Carregando datasets")
  updateProgressBar(session = session, id = "progress", value = 5, total = 12, title='Carregando valores do formulario')
  municipios <- unique(all_data$municipio)
  
  # setProgress(6, message = "Carregando datasets")
  updateProgressBar(session = session, id = "progress", value = 6, total = 12, title='Carregando valores do formulario')
  nomes_proprietarios <- unique(all_data$nome_proprietario)
  
  # setProgress(7, message = "Carregando datasets")
  updateProgressBar(session = session, id = "progress", value = 7, total = 12, title='Carregando valores do formulario')
  documentos <- unique(all_data$documento)
  
  # setProgress(8, message = "Carregando datasets")
  updateProgressBar(session = session, id = "progress", value = 8, total = 12, title='Carregando valores do formulario')
  codigos_imoveis <- unique(all_data$codigo_imovel)
  
  # setProgress(9, message = "Carregando datasets")
  updateProgressBar(session = session, id = "progress", value = 9, total = 12, title='Carregando valores do formulario')
  nomes_fazendas <- unique(all_data$nome_area)
  
  # setProgress(10, message = "Carregando datasets")
  updateProgressBar(session = session, id = "progress", value = 10, total = 12, title='Carregando valores do formulario')
  matriculas <- unique(all_data$matricula)
  
  # Limpar a Variavel `all_data` para reduzir o uso de memoria
  rm(all_data)
  gc()  # Forcar a coleta de lixo para liberar memoria
  
  
  
  print('terminou de carregar as labels de pesquisa')
  
  # setProgress(11, message = "Carregando datasets")
  updateProgressBar(session = session, id = "progress", value = 11, total = 12, title='Atualizando do formulario')
  # Update the choices for the selectInput based on the filtered data (updateSelectInput)
  updateSelectizeInput(session, "Grupo_proprietario", choices = c('',grupos_proprietarios_all), selected = NULL, server = TRUE)
  updateSelectizeInput(session, "Nome_proprietario", choices = c('',nomes_proprietarios), selected = NULL, server = TRUE)
  updateSelectizeInput(session, "Documento_proprietario", choices = c('',documentos), selected = NULL, server = TRUE)
  updateSelectizeInput(session, "UF", choices = c('',ufs), selected = NULL, server = TRUE)
  updateSelectizeInput(session, "Municipio", choices = c('',municipios), selected = NULL, server = TRUE)
  updateSelectizeInput(session, "Codigo_Imovel_Rural", choices = c('',codigos_imoveis), selected = NULL, server = TRUE)
  updateSelectizeInput(session, "Nome_Fazenda", choices = c('',nomes_fazendas), selected = NULL, server = TRUE)
  updateSelectizeInput(session, "Matricula", choices = c('',matriculas), selected = NULL, server = TRUE)
  updateSelectizeInput(session, "ID_Terra", choices = c('',id_terras), selected = NULL, server = TRUE)
  
  
  
  ###### define meu mapa
  updateProgressBar(session = session, id = "progress", value = 12, total = 12, title='Carregando mapa')
  
  
  # create_map <- function(){
  #   
  # }
  # 
  # output$mymap <- renderLeaflet({
  #   leaflet() %>%
  #     addTiles() %>%
  #     setView(lng = -49.938054175291164, lat = -13.605626011915135, zoom = 5)
  # })
  
  output$mymap <- renderLeaflet({make_map()})
  
  # setProgress(12, message = "Carregando datasets")
  # updateProgressBar(session = session, id = "progress", value = 13, total = 13, title='Carregamento finalizado')
  closeSweetAlert()
  # })
  
  
  
  
  ############################################
  
  
  
  observeEvent(input$search_button, {
    #starto a Variavel de resultado nullada e a de progressbar tambem nullada
    output_results(list(botao='pesquisa', erro=NULL, warning=NULL, texto=NULL, terras_filtradas=NULL, vizinhos=NULL, vcg=NULL, vcsg=NULL, vnc=NULL, vcg_dissolved_centroids=NULL))
    output_progressbar(list(botao='pesquisa', etapa=0, mensagem='Iniciando procura de terras'))
    
    lista_resultados_output <- list(botao='pesquisa', erro=NULL, warning=NULL, texto=NULL, terras_filtradas=NULL, vizinhos=NULL, vcg=NULL, vcsg=NULL, vnc=NULL)
    
    
    progressSweetAlert(
      session = session,
      id = "progress",
      title = 'semtitulo',
      display_pct = TRUE,
      value = 0
    )
    
    
    # logg(texto='antes do future')
    print('antes do future')
    
    
    #### extrai os valores de input, pra adicionar no future (adicionar o input inteiro estoura a memoria)
    UF <- input$UF
    Municipio <- input$Municipio
    Codigo_Imovel_Rural <- input$Codigo_Imovel_Rural
    Nome_Fazenda <- input$Nome_Fazenda
    Matricula <- input$Matricula
    ID_Terra <- input$ID_Terra
    Grupo_proprietario <- input$Grupo_proprietario
    Nome_proprietario <- input$Nome_proprietario
    Documento_proprietario <- input$Documento_proprietario
    
    future_promise(globals=list(file_source_queue_progressbar = file_source_queue_progressbar,
                                file_source_queue_results = file_source_queue_results,
                                lista_resultados_output = lista_resultados_output,
                                UF = UF,
                                Municipio = Municipio,
                                Codigo_Imovel_Rural = Codigo_Imovel_Rural,
                                Nome_Fazenda = Nome_Fazenda,
                                Matricula = Matricula,
                                ID_Terra = ID_Terra,
                                Grupo_proprietario = Grupo_proprietario,
                                Nome_proprietario = Nome_proprietario,
                                Documento_proprietario = Documento_proprietario),
                   packages=c("h3", "sf","ipc", "dplyr", "httr"),
                   seed=TRUE, {
                     
                     
                     #################################### Definicao das funcoes que serao utilizadas dentro do future #####################################
                     #################################### Importar elas como Variavel ocupa muita memoria #################################################
                     #################################### (talvez transformar num modulo e importar ele resolva) ##########################################
                     
                     
                     ##### seta para nao verificar o certificado ssl para as requisicoes (precisa disso pra consultar o geoserver)
                     set_config(config(ssl_verifypeer = FALSE))
                     
                     ##### Define a logging function
                     logg <- function(texto) {
                       message(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), " | Farm Owners App | ", texto)
                     }
                     
                     ##### define Funcao de pegar dados no wfs do geoserver
                     fetch_spatial_df <- function(url, cql_filter, camada) {
                       # base_link = "https://geoserver.bocombbm.com.br/geoserver/INCRA/ows?service=WFS&version=1.0.0&request=GetFeature&typeName=INCRA%3ASNCI&outputFormat=application/json&CQL_FILTER=numero_certificado_snci%20IN%20("
                       # consulta_string = snci_string (que e do formato:  "'050810000035-26', '051206000026-65'" )
                       # Construct the full link using the inputs
                       body <- list(
                         service = "WFS",
                         version = "1.0.0",
                         request = "GetFeature",
                         typeName = camada,
                         outputFormat = "application/json",
                         CQL_FILTER = cql_filter
                       )
                       
                       # Fazer a requisicao POST
                       wfs_response <- POST(url, body = body, encode = "form")
                       
                       # Verificar se a requisicao foi bem-sucedida
                       if (status_code(wfs_response) == 200) {
                         # Extrair o conteudo GeoJSON
                         geojson_content <- content(wfs_response, as = "text", encoding = "UTF-8")
                         # Ler o GeoJSON como um data.frame espacial
                         wfs_spatial_df <- st_read(geojson_content, quiet = TRUE)
                         return (wfs_spatial_df)
                       } else {
                         # Tratar o erro
                         stop("Falha ao buscar os dados: ", status_code(wfs_response))
                       }
                     }
                     
                     
                     
                     
                     ######################################################################################################################################
                     #################################### Conectando as queues dentro do future com  ######################################################
                     #################################### o arquivo de cada uma das queues da sessao ######################################################
                     ######################################################################################################################################
                     
                     # logg(texto='dentro do future, depois de definir as funcoes utilizadas dentro dele e configurar o ssl')
                     print('dentro do future, depois de definir as funcoes utilizadas dentro dele e configurar o ssl')
                     
                     queue_progressbar <- shinyQueue(source = file_source_queue_progressbar)
                     queue_results <- shinyQueue(source = file_source_queue_results)
                     
                     ######################################################################################################################################
                     #################################### Comeca realmente o codigo do future #############################################################
                     ######################################################################################################################################
                     
                     queue_progressbar$producer$fireAssignReactive("output_progressbar", list(botao='pesquisa', etapa=1, mensagem='Fazendo query na base de dados completa'))
                     ################# Initialize query
                     base_search_query <- "SELECT * FROM terras_e_proprietarios"
                     
                     # Initialize list to hold conditions
                     search_conditions <- list()
                     
                     # Add conditions based on user input
                     
                     if (!is.null(UF) && length(UF) > 0) {
                       if (length(UF) == 1) {
                         if (UF != "") {
                           # Caso o usuario selecione apenas uma opcao
                           search_conditions <- append(search_conditions, sprintf("uf = '%s'", UF)) 
                         }
                       } else {
                         # Caso o usuario selecione multiplas opcoes
                         uf_values <- paste(sprintf("'%s'", UF), collapse = ", ")
                         search_conditions <- append(search_conditions, sprintf("uf IN (%s)", uf_values))
                       }
                     }
                     
                     if (!is.null(Municipio) && length(Municipio) > 0) {
                       if (length(Municipio) == 1) {
                         if (Municipio != "") {
                           # Caso o usuario selecione apenas uma opcao
                           search_conditions <- append(search_conditions, sprintf("municipio = '%s'", Municipio)) 
                         }
                       } else {
                         # Caso o usuario selecione multiplas opcoes
                         municipio_values <- paste(sprintf("'%s'", Municipio), collapse = ", ")
                         search_conditions <- append(search_conditions, sprintf("municipio IN (%s)", municipio_values))
                       }
                     }
                     
                     if (!is.null(Codigo_Imovel_Rural) && length(Codigo_Imovel_Rural) > 0) {
                       if (length(Codigo_Imovel_Rural) == 1) {
                         if (Codigo_Imovel_Rural != "") {
                           # Caso o usuario selecione apenas uma opcao
                           search_conditions <- append(search_conditions, sprintf("codigo_imovel = '%s'", Codigo_Imovel_Rural)) 
                         }
                       } else {
                         # Caso o usuario selecione multiplas opcoes
                         codigo_imovel_values <- paste(sprintf("'%s'", Codigo_Imovel_Rural), collapse = ", ")
                         search_conditions <- append(search_conditions, sprintf("codigo_imovel IN (%s)", codigo_imovel_values))
                       }
                     }
                     
                     if (!is.null(Nome_Fazenda) && length(Nome_Fazenda) > 0) {
                       if (length(Nome_Fazenda) == 1) {
                         if (Nome_Fazenda != "") {
                           # Caso o usuario selecione apenas uma opcao
                           search_conditions <- append(search_conditions, sprintf("nome_area LIKE '%s'", Nome_Fazenda)) 
                         }
                       } else {
                         # Caso o usuario selecione multiplas opcoes
                         nome_area_values <- paste(sprintf("'%s'", Nome_Fazenda), collapse = ", ")
                         search_conditions <- append(search_conditions, sprintf("nome_area IN (%s)", nome_area_values))
                       }
                     }
                     
                     if (!is.null(Matricula) && length(Matricula) > 0) {
                       if (length(Matricula) == 1) {
                         if (Matricula != "") {
                           # Caso o usuario selecione apenas uma opcao
                           search_conditions <- append(search_conditions, sprintf("matricula = '%s'", Matricula)) 
                         }
                       } else {
                         # Caso o usuario selecione multiplas opcoes
                         matricula_values <- paste(sprintf("'%s'", Matricula), collapse = ", ")
                         search_conditions <- append(search_conditions, sprintf("matricula IN (%s)", matricula_values))
                       }
                     }
                     
                     if (!is.null(ID_Terra) && length(ID_Terra) > 0) {
                       if (length(ID_Terra) == 1) {
                         if (ID_Terra != "") {
                           # Caso o usuario selecione apenas uma opcao
                           search_conditions <- append(search_conditions, sprintf("id_terra = '%s'", ID_Terra)) 
                         }
                       } else {
                         # Caso o usuario selecione multiplas opcoes
                         id_terra_values <- paste(sprintf("'%s'", ID_Terra), collapse = ", ")
                         search_conditions <- append(search_conditions, sprintf("id_terra IN (%s)", id_terra_values))
                       }
                     }
                     
                     if (!is.null(Grupo_proprietario) && length(Grupo_proprietario) > 0) {
                       if (length(Grupo_proprietario) == 1) {
                         if (Grupo_proprietario != "") {
                           # Caso o usuario selecione apenas uma opcao
                           search_conditions <- append(search_conditions, sprintf("grupo = '%s'", Grupo_proprietario)) 
                         }
                       } else {
                         # Caso o usuario selecione multiplas opcoes
                         grupo_values <- paste(sprintf("'%s'", Grupo_proprietario), collapse = ", ")
                         search_conditions <- append(search_conditions, sprintf("grupo IN (%s)", grupo_values))
                       }
                     }
                     
                     if (!is.null(Nome_proprietario) && length(Nome_proprietario) > 0) {
                       if (length(Nome_proprietario) == 1) {
                         if (Nome_proprietario != "") {
                           # Caso o usuario selecione apenas uma opcao
                           search_conditions <- append(search_conditions, sprintf("nome_proprietario = '%s'", Nome_proprietario)) 
                         }
                       } else {
                         # Caso o usuario selecione multiplas opcoes
                         nome_proprietario_values <- paste(sprintf("'%s'", Nome_proprietario), collapse = ", ")
                         search_conditions <- append(search_conditions, sprintf("nome_proprietario IN (%s)", nome_proprietario_values))
                       }
                     }
                     
                     if (!is.null(Documento_proprietario) && length(Documento_proprietario) > 0) {
                       if (length(Documento_proprietario) == 1) {
                         if (Documento_proprietario != "") {
                           # Caso o usuario selecione apenas uma opcao
                           search_conditions <- append(search_conditions, sprintf("documento = '%s'", Documento_proprietario)) 
                         }
                       } else {
                         # Caso o usuario selecione multiplas opcoes
                         documento_values <- paste(sprintf("'%s'", Documento_proprietario), collapse = ", ")
                         search_conditions <- append(search_conditions, sprintf("documento IN (%s)", documento_values))
                       }
                     }
                     
                     # Combine conditions into WHERE clause
                     if (length(search_conditions) > 0) {
                       where_clause <- paste("WHERE", paste(search_conditions, collapse = " AND "))
                       final_search_query <- paste(base_search_query, where_clause)
                     } else {
                       lista_resultados_output$erro <- TRUE
                       lista_resultados_output$texto <- "Selecione algum filtro para pesquisar terras"
                       queue_results$producer$fireAssignReactive("output_results", lista_resultados_output)
                       return(NULL)
                     }
                     
                     print(final_search_query)
                     data <- tryCatch({
                       st_read(
                         dsn = "~/AFS/Pesquisa_Quantitativa/Compartilhado/rundeck_files_folder/Farm_Owners_Data/farm_owners_data.gpkg",
                         query = final_search_query,
                         quiet = TRUE
                       ) %>% st_set_geometry(NULL)
                     }, error = function(e) {
                       # logg(sprintf('Error reading data: %s', e$message))
                       print(sprintf('Error reading data: %s', e$message))
                       lista_resultados_output$erro <- TRUE
                       lista_resultados_output$texto <- "An error occurred while loading the data. Please check your inputs and try again."
                       queue_results$producer$fireAssignReactive("output_results", lista_resultados_output)
                       return(NULL)
                     })
                     
                     
                     # If data loading failed, exit early
                     if (is.null(data) || nrow(data)==0 )  {
                       lista_resultados_output$erro <- TRUE
                       lista_resultados_output$texto <- "Nenhum dado achado para os filtros selecionados"
                       queue_results$producer$fireAssignReactive("output_results", lista_resultados_output)
                       return(NULL)
                     }
                     
                     data <- data %>%
                       group_by(id_terra) %>%
                       summarise(
                         # Manter a primeira ocorrencia das colunas desejadas
                         across(c(-grupo, -nome_proprietario, -fonte_proprietario, -data_insercao, -documento), first),
                         
                         # Concatenar os valores das colunas especificadas
                         grupo = paste(grupo, collapse = ", "),
                         documento = paste(documento, collapse = ", "),
                         nome_proprietario = paste(nome_proprietario, collapse = ", "),
                         fonte_proprietario = paste(fonte_proprietario, collapse = ", "),
                         data_insercao = paste(data_insercao, collapse = ", ")
                       ) %>%
                       ungroup()
                     
                     # Check if data exceeds 10,000 records
                     record_limit <- 5000
                     if (nrow(data) > record_limit) {
                       lista_resultados_output$warning <- TRUE
                       lista_resultados_output$texto <- sprintf("Your query returned more than %d records. Only the first %d will be displayed.", record_limit, record_limit)
                       queue_results$producer$fireAssignReactive("output_results", lista_resultados_output)
                       data <- data[1:record_limit, ]
                       # logg(sprintf('Attribute-based search limited to %d records.', record_limit))
                       print(sprintf('Attribute-based search limited to %d records.', record_limit))
                     } else {
                       # logg(sprintf('Attribute-based search returned %d records.', nrow(data)))
                       print(sprintf('Attribute-based search returned %d records.', nrow(data)))
                     }
                     
                     
                     
                     ## talvez colocar esse if e o debaixo dele (pro sigef) no if de cima ? (o que limita a quantidade de dados)
                     ## pegando geometrias do snci
                     queue_progressbar$producer$fireAssignReactive("output_progressbar", list(botao='pesquisa', etapa=2, mensagem='Adicionando geometrias SNCI'))
                     if (sum(data$fonte_geo=='SNCI') > 0) {
                       # Filter the values from the data
                       ids_snci <- data$id_terra[data$fonte_geo == 'SNCI']
                       # Convert the values into a comma-separated string
                       snci_cql_string <- paste0("numero_certificado_snci IN ('", paste(ids_snci, collapse = "', '"), "')")
                       #fetch the data
                       snci_df = fetch_spatial_df(url = "https://geoserver.bocombbm.com.br/geoserver/INCRA/ows",
                                                  cql_filter = snci_cql_string,
                                                  camada = "INCRA:SNCI")
                       snci_df <-  snci_df %>% select("numero_certificado_snci", "geometry") %>% rename(id_terra = numero_certificado_snci)
                       print(paste0('snci: ', nrow(snci_df)))
                       
                     } else {
                       snci_df <- NULL
                     }
                     
                     
                     ### pegando as geometrias do sigef
                     queue_progressbar$producer$fireAssignReactive("output_progressbar", list(botao='pesquisa', etapa=3, mensagem='Adicionando geometrias SIGEF'))
                     if (sum(data$fonte_geo=='SIGEF') > 0) {
                       # Filter the values from the data
                       ids_sigef <- data$id_terra[data$fonte_geo == 'SIGEF']
                       # Convert the values into a comma-separated string
                       sigef_cql_string <- paste0("codigo_parcela_sigef IN ('", paste(ids_sigef, collapse = "', '"), "')")
                       #fetch the data
                       sigef_df = fetch_spatial_df(url = "https://geoserver.bocombbm.com.br/geoserver/INCRA/ows",
                                                   cql_filter = sigef_cql_string,
                                                   camada = "INCRA:SIGEF")
                       sigef_df <-  sigef_df %>% select("codigo_parcela_sigef", "geometry") %>% rename(id_terra = codigo_parcela_sigef)
                       print(paste0('sigef: ', nrow(sigef_df)))
                       
                     } else {
                       sigef_df <- NULL
                     }
                     
                     
                     ### unindo as geometrias do snci e sigef e depois unindo com as do .sqlite
                     queue_progressbar$producer$fireAssignReactive("output_progressbar", list(botao='pesquisa', etapa=4, mensagem='Unindo e validando geometrias'))
                     geometrias_wfs <- bind_rows(sigef_df, snci_df) #se uma delas for null, ele vai so manter a outra
                     
                     data_with_geometry <- left_join(data, geometrias_wfs, by='id_terra')
                     data_with_geometry <- data_with_geometry %>%
                       filter(!st_is_empty(geometry) & !is.na(geometry))  ## retira linhas que nao tem geometria (depois e bom investigar isso)
                     data_with_geometry <- st_make_valid(st_as_sf(data_with_geometry))
                     # print(paste0('full: ', nrow(data_with_geometry)))
                     # logg(paste0('dataframe final:', nrow(data_with_geometry)))
                     print(paste0('dataframe final:', nrow(data_with_geometry)))
                     
                     
                     #### retorna o resultado
                     lista_resultados_output$warning <- NULL
                     lista_resultados_output$terras_filtradas <- data_with_geometry
                     queue_results$producer$fireAssignReactive("output_results", lista_resultados_output)
                     
                     #### atualiza a progress bar p/ plotar o mapa
                     queue_progressbar$producer$fireAssignReactive("output_progressbar", list(botao='pesquisa', etapa=5, mensagem='Plotando dados no mapa'))
                     
                   }) #### fecha future_promise
    
    gc
    
    
  }) #final do filtered_data
  
  
  
  
  ##### observe event do resultados ()
  observeEvent(output_results(), {
    print('estamos no output_results')
    if (identical(output_results()$erro, TRUE)) {
      closeSweetAlert()
      showModal(modalDialog(
        title = "Warning",
        output_results()$texto,
        easyClose = TRUE,
        footer = NULL
      ))
      print('deu um erro')
    } 
    else if (identical(output_results()$warning, TRUE)) {
      showModal(modalDialog(
        title = "Warning",
        output_results()$texto,
        easyClose = TRUE,
        footer = NULL
      ))
      print('deu um warning')
    } 
    else if (identical(output_results()$botao,'pesquisa')) {
      print('dentro do resultados do busca')
      terras_filtradas <- output_results()$terras_filtradas
      print(terras_filtradas)
      if  (isTRUE(!is.null(terras_filtradas) && nrow(terras_filtradas) > 0)) {
        
        map <- leafletProxy("mymap") %>%
          clearShapes() %>%
          clearMarkerClusters() %>%
          clearMarkers() %>%
          clearControls()
        # Adicionar poligonos ao mapa
        print('ate aqui foi')
        map <- map %>%
          addPolygons(
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
            highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE),
            group = "Terras"
          ) %>%
          addDrawToolbar(
            targetGroup = 'Terras',
            polylineOptions = FALSE,
            circleOptions = FALSE,
            markerOptions = FALSE,
            rectangleOptions = FALSE,
            polygonOptions = FALSE,
            circleMarkerOptions = FALSE,
            editOptions = editToolbarOptions()
          )
        # Adicionar clusters de marcadores se houver mais de um poligono
        if (nrow(terras_filtradas) > 1) {
          map <- map %>%
            addMarkers(
              clusterOptions = markerClusterOptions(),
              data = st_coordinates(st_centroid(terras_filtradas$geometry))[, c('X', 'Y')]
            )
        }
        # Ajustar o mapa para o bounding box dos dados
        map %>%
          flyToBounds(
            lng1 = st_bbox(terras_filtradas)$xmin[[1]],
            lat1 = st_bbox(terras_filtradas)$ymin[[1]],
            lng2 = st_bbox(terras_filtradas)$xmax[[1]],
            lat2 = st_bbox(terras_filtradas)$ymax[[1]]
          )
        gc()
        
        
        
        #### disponibiliza o dado para download
        download_data(terras_filtradas)
        
        #### atualizado o progress bar (pro usuario) e tambem pra bater o limite e fechar o sweetalert
        output_progressbar(list(botao='pesquisa', etapa=7, mensagem='Finalizada plotagem no mapa'))
      }
    } 
    else if (identical(output_results()$botao,'vizinhos')) {
      print('botao de vizinhos')
      vcg <- output_results()$vcg
      vcsg <- output_results()$vcsg
      vnc <- output_results()$vnc
      vcg_dissolved_centroids <- output_results()$vcg_dissolved_centroids
      vizinhos <- output_results()$vizinhos
      
      #### Criar o mapa base e plota, caso algum dos data.frames principais (vcg, vcsg ou vnc) nao for null
      if (any(
        isTRUE(!is.null(vcg) && nrow(vcg) > 0),
        isTRUE(!is.null(vcsg) && nrow(vcsg) > 0),
        isTRUE(!is.null(vnc) && nrow(vnc) > 0)
      )) {
        map <- leafletProxy("mymap") %>%
          clearShapes() %>%
          clearMarkerClusters() %>%
          clearMarkers() %>%
          clearControls()
        
        
        #### plota cada um dos data.frames se eles nao forem null
        if (isTRUE(!is.null(vcg) && nrow(vcg) > 0)) {
          ## finalmente adiciona o vcg no mapa
          map <- map %>%
            addPolygons(
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
              fillOpacity = 1,  # Ajuste a opacidade do preenchimento conforme necessario
              fillColor = ~cor,   # Usa a coluna "cor" para definir a cor de preenchimento
              highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE),
              group = "Terras"
            ) %>%
            addLabelOnlyMarkers(
              data = vcg_dissolved_centroids,
              lng = ~st_coordinates(centroid)[, 1],  # Longitude do centroide
              lat = ~st_coordinates(centroid)[, 2],  # Latitude do centroide
              label = ~grupo,  # Texto da label baseado no grupo
              labelOptions = labelOptions(
                noHide = TRUE,  # Sempre mostrar as labels
                direction = "top",  # Posicao da label
                textOnly = TRUE,  # Apenas texto, sem marcador
                style = list(
                  "color" = "black",
                  "font-size" = "20px",
                  "font-weight" = "bold"
                )
              )
              ,clusterOptions = markerClusterOptions() ,group = "Terras")
        }
        
        if (isTRUE(!is.null(vcsg) && nrow(vcsg) > 0)) {
          map <- map %>%
            addPolygons(
              data = vcsg,
              layerId = ~id_terra,
              popup = ~paste0(
                "<div style='background-color: #ffefcc; padding: 0px; border-radius: 0px;'>",  # Estilo do popup
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
                "<b>Municipio/UF:</b> ", municipio,
                "</div>"  # Fechando o estilo do popup
              ),
              color = "#333333", weight = 0.6, opacity = 1,
              fillOpacity = 0.2, fillColor = "orange",
              highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE),
              group = "Terras"
            ) 
        }
        
        if (isTRUE(!is.null(vnc) && nrow(vnc) > 0)) {
          map <- map %>%
            addPolygons(
              data = vnc,
              layerId = ~id_terra,
              popup = ~paste0(
                "<div style='background-color:#e6e6e6; padding: 0px; border-radius: 0px;'>",  # Estilo do popup
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
                "<b>Municipio/UF:</b> ", municipio,
                "</div>"  # Fechando o estilo do popup
              ),
              color = "#333333", weight = 0.6, opacity = 1,
              fillOpacity = 0.2, fillColor = "gray",
              highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE),
              group = "Terras"
            ) 
        }
        
        
        
        #### disponibiliza o dado para download
        download_data(vizinhos)
        
        #### atualiza o progressbar (pro usuario e pro observe do progress fechar o sweetalert)
        output_progressbar(list(botao='vizinhos', etapa=7, mensagem='Finalizada plotagem no mapa'))
        closeSweetAlert()
      }
    }
    
  })
  
  
  
  
  #### ObserveEvent para a progressbar (universal para todos os botoes) 
  observeEvent(output_progressbar(), {
    print('estamos no progressbar')
    print(output_progressbar()$etapa)
    if (identical(output_progressbar()$botao, 'vizinhos')) {
      print('e vizinhos')
      updateProgressBar(session=session, id="progress", value=output_progressbar()$etapa, total=7, title=output_progressbar()$mensagem)
      if (identical(output_progressbar()$etapa, 7)) { #primeiro checa se nao e null, depois checa o numero
        closeSweetAlert()
        # Seta o contador pra zero
      }
    } else if (identical(output_progressbar()$botao, 'pesquisa')){
      updateProgressBar(session=session, id="progress", value=output_progressbar()$etapa, total=7, title=output_progressbar()$mensagem)
      if (identical(output_progressbar()$etapa, 7)){
        closeSweetAlert()
        #seta o contador pra zero
      }
    } else {
      print('atualizou o progressbar mas nao foi nem do botao de search nem do de vizinhos')
      print(output_progressbar())
    }
    
  })
  
  
  
  
  observeEvent(input$load_neighbors_button, {
    
    #starto a Variavel de resultado nullada e a de progressbar tambem nullada
    output_results(list(botao='vizinhos', erro=NULL, warning=NULL, texto=NULL, terras_filtradas=NULL, vizinhos=NULL, 
                        vcg=NULL, vcsg=NULL, vnc=NULL, vcg_dissolved_centroids=NULL))
    output_progressbar(list(botao='vizinhos', etapa=0, mensagem='Iniciando processamento de vizinhos'))
    
    lista_resultados_output <- list(botao='vizinhos', erro=NULL, warning=NULL, texto=NULL, terras_filtradas=NULL, vizinhos=NULL, vcg=NULL, vcsg=NULL, vnc=NULL)
    
    
    # Obter os limites atuais do mapa
    bounds <- input$mymap_bounds
    
    progressSweetAlert(
      session = session,
      id = "progress",
      title = 'semtitulo',
      display_pct = TRUE,
      value = 0
    )
    
    
    # logg(texto='antes do future')
    print('antes do future')
    
    future_promise(globals=list(bounds=bounds, 
                                file_source_queue_progressbar = file_source_queue_progressbar,
                                file_source_queue_results = file_source_queue_results,
                                lista_resultados_output = lista_resultados_output),
                   packages=c("h3", "sf","ipc", "dplyr", "httr"),
                   seed=TRUE, {
                     
                     #################################### Definicao das funcoes que serao utilizadas dentro do future #####################################
                     #################################### Importar elas como Variavel ocupa muita memoria #################################################
                     #################################### (talvez transformar num modulo e importar ele resolva) ##########################################
                     
                     
                     ##### seta para nao verificar o certificado ssl para as requisicoes (precisa disso pra consultar o geoserver)
                     set_config(config(ssl_verifypeer = FALSE))
                     
                     ##### Define a logging function
                     logg <- function(texto) {
                       message(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), " | Farm Owners App | ", texto)
                     }
                     
                     ##### define Funcao de pegar dados no wfs do geoserver
                     fetch_spatial_df <- function(url, cql_filter, camada) {
                       # base_link = "https://geoserver.bocombbm.com.br/geoserver/INCRA/ows?service=WFS&version=1.0.0&request=GetFeature&typeName=INCRA%3ASNCI&outputFormat=application/json&CQL_FILTER=numero_certificado_snci%20IN%20("
                       # consulta_string = snci_string (que e do formato:  "'050810000035-26', '051206000026-65'" )
                       # Construct the full link using the inputs
                       body <- list(
                         service = "WFS",
                         version = "1.0.0",
                         request = "GetFeature",
                         typeName = camada,
                         outputFormat = "application/json",
                         CQL_FILTER = cql_filter
                       )
                       
                       # Fazer a requisicao POST
                       wfs_response <- POST(url, body = body, encode = "form")
                       
                       # Verificar se a requisicao foi bem-sucedida
                       if (status_code(wfs_response) == 200) {
                         # Extrair o conteudo GeoJSON
                         geojson_content <- content(wfs_response, as = "text", encoding = "UTF-8")
                         # Ler o GeoJSON como um data.frame espacial
                         wfs_spatial_df <- st_read(geojson_content, quiet = TRUE)
                         return (wfs_spatial_df)
                       } else {
                         # Tratar o erro
                         stop("Falha ao buscar os dados: ", status_code(wfs_response))
                       }
                     }
                     
                     
                     
                     
                     ######################################################################################################################################
                     #################################### Conectando as queues dentro do future com  ######################################################
                     #################################### o arquivo de cada uma das queues da sessao ######################################################
                     ######################################################################################################################################
                     
                     # logg(texto='dentro do future, depois de definir as funcoes utilizadas dentro dele e configurar o ssl')
                     print('dentro do future, depois de definir as funcoes utilizadas dentro dele e configurar o ssl')
                     
                     queue_progressbar <- shinyQueue(source = file_source_queue_progressbar)
                     queue_results <- shinyQueue(source = file_source_queue_results)
                     
                     ######################################################################################################################################
                     #################################### Comeca realmente o codigo do future #############################################################
                     ######################################################################################################################################
                     
                     if (is.null(bounds)) {
                       # showNotification("O mapa ainda nao esta carregado. Tente novamente.", type = "warning")
                       lista_resultados_output$erro <- TRUE
                       lista_resultados_output$texto <- "O mapa ainda nao esta carregado. Tente novamente."
                       queue_results$producer$fireAssignReactive("output_results", lista_resultados_output)
                       return(NULL)
                     }
                     # 
                     
                     queue_progressbar$producer$fireAssignReactive("output_progressbar", list(botao='vizinhos', etapa=1, mensagem='Extraindo os hexagonos viziveis'))
                     # logg('depois do primeiro update')
                     
                     # Extrair os limites (bounding box)
                     lng_min <- bounds$west
                     lng_max <- bounds$east
                     lat_min <- bounds$south
                     lat_max <- bounds$north
                     
                     # logg('depois de definir longlat')
                     
                     # # Gerar hexagonos H3 com resolucao 5 dentro da area visivel
                     # # Primeiro, crie um poligono representando a area visivel
                     bbox_polygon <- st_as_sf(
                       st_sfc(
                         st_polygon(list(matrix(c(
                           lng_min, lat_min,
                           lng_min, lat_max,
                           lng_max, lat_max,
                           lng_max, lat_min,
                           lng_min, lat_min
                         ), ncol = 2, byrow = TRUE))),
                         crs = 4326
                       )
                     )
                     
                     
                     # 
                     # Obter os hexagonos H3 na area visivel
                     hexagons <- h3::polyfill(bbox_polygon, res = 5)
                     # 
                     print(length(hexagons))
                     if (length(hexagons)>=50){
                       # logg('mais do que 50 hexagonos')
                       print('mais do que 50 hexagonos')
                       lista_resultados_output$erro <- TRUE
                       lista_resultados_output$texto <- "Sua area de vizinhos e maior do que 50 hexagonos (aprox: 1,25 milhao de ha) reduza a area e pesquise novamente ."
                       queue_results$producer$fireAssignReactive("output_results", lista_resultados_output)
                       return(NULL)
                     } else if (length(hexagons)==0){
                       # logg('menos do que 1 hexagono')
                       print('menos do que 1 hexagono')
                       lista_resultados_output$erro <- TRUE
                       lista_resultados_output$texto <- "Sua area de vizinhos e menor do que do que 1 hexagono (aprox: 25 mil ha) aumente a area e pesquise novamente ."
                       queue_results$producer$fireAssignReactive("output_results", lista_resultados_output)
                       return(NULL)
                     }
                     
                     # 
                     # # Converter os hexagonos H3 para geometrias (poligonos)
                     # print('convertendo hexagonos')
                     # hexagon_geometries <- lapply(hexagons, h3::h3_to_geo_boundary_sf)
                     # hexagon_sf <- do.call(rbind, hexagon_geometries)
                     # print(hexagon_sf)
                     # 
                     # 
                     ## pega todas as terras dentro dos hexagonos
                     # updateProgressBar(session = session, id = "progress_load_neighbors", value = 1, total = 6, title="Buscando terras no .sqlite")
                     queue_progressbar$producer$fireAssignReactive("output_progressbar", list(botao='vizinhos', etapa=2, mensagem='Buscando terras no .sqlite'))
                     # id_hexagonos <- hexagon_sf$h3_index
                     vizinhos_query <- paste0("SELECT * FROM terras_e_proprietarios WHERE id_hexagono IN ('", paste(hexagons, collapse = "', '"), "')")
                     vizinhos <- st_read('~/AFS/Pesquisa_Quantitativa/Compartilhado/rundeck_files_folder/Farm_Owners_Data/farm_owners_data.gpkg', query=vizinhos_query, quiet=T) %>% st_set_geometry(NULL)
                     ## limita a 10000 terras, se passar retorna null e avisa que tem geometria demais
                     record_limit <- 10000
                     if (nrow(vizinhos) > record_limit) {
                       lista_resultados_output$warning <- TRUE
                       lista_resultados_output$texto <- "Sua query resultou em muitos resultados, apenas os primeiros 10 mil serao mostrados"
                       queue_results$producer$fireAssignReactive("output_results", lista_resultados_output)
                       vizinhos <- vizinhos[1:record_limit, ]
                       # logg(sprintf('Attribute-based search limited to %d records.', record_limit))
                     } else if (nrow(vizinhos) == 0) {
                       lista_resultados_output$erro <- TRUE
                       lista_resultados_output$texto <- "A area pesquisada nao possui terras no SIGEF ou SNCI"
                       queue_results$producer$fireAssignReactive("output_results", lista_resultados_output)
                       return(NULL)
                     } else {
                       # logg(texto=sprintf('Attribute-based search returned %d records.', nrow(vizinhos)))
                       print(sprintf('Attribute-based search returned %d records.', nrow(vizinhos)))
                     }
                     
                     
                     # logg(texto='unindo as linhas do sqlite com mesmo id_terra')
                     print('unindo as linhas do sqlite com mesmo id_terra')
                     
                     
                     ## une as linhas para deixar apenas 1 linha por id_terra (concatenando grupo, nome, fonte, data e documento)
                     vizinhos <- vizinhos %>%
                       group_by(id_terra) %>%
                       summarise(
                         # Manter a primeira ocorrencia das colunas desejadas
                         across(c(-grupo, -nome_proprietario, -fonte_proprietario, -data_insercao, -documento), first),
                         
                         # Concatenar os valores das colunas especificadas
                         grupo = paste(grupo, collapse = ", "),
                         documento = paste(documento, collapse = ", "),
                         nome_proprietario = paste(nome_proprietario, collapse = ", "),
                         fonte_proprietario = paste(fonte_proprietario, collapse = ", "),
                         data_insercao = paste(data_insercao, collapse = ", ")
                       ) %>%
                       ungroup()
                     # logg(texto=sprintf('O numero de terras unicas retornadas e: %d', nrow(vizinhos)))
                     print(sprintf('O numero de terras unicas retornadas e: %d', nrow(vizinhos)))
                     
                     ## pega todas as geometrias do sigef (caso existam)
                     
                     queue_progressbar$producer$fireAssignReactive("output_progressbar", list(botao='vizinhos', etapa=3, mensagem='Baixando geometrias do SIGEF'))
                     if (sum(vizinhos$fonte_geo=='SIGEF') > 0) {
                       # Filter the values from the data
                       ids_sigef <- vizinhos$id_terra[vizinhos$fonte_geo == 'SIGEF']
                       # Convert the values into a comma-separated string
                       sigef_cql_string <- paste0("codigo_parcela_sigef IN ('", paste(ids_sigef, collapse = "', '"), "')")
                       #fetch the data
                       sigef_df = fetch_spatial_df(url = "https://geoserver.bocombbm.com.br/geoserver/INCRA/ows",
                                                   cql_filter = sigef_cql_string,
                                                   camada = "INCRA:SIGEF")
                       sigef_df <-  sigef_df %>% select("codigo_parcela_sigef", "geometry") %>% rename(id_terra = codigo_parcela_sigef)
                       # logg(paste0('sigef: ', nrow(sigef_df)))
                       print(paste0('sigef: ', nrow(sigef_df)))
                       
                     } else {
                       sigef_df <- NULL
                       # logg('geoserver nao retornou terras do sigef para a selecao')
                       print('geoserver nao retornou terras do sigef para a selecao')
                     }
                     
                     ## pega todas as geometrias do snci (caso existam)
                     queue_progressbar$producer$fireAssignReactive("output_progressbar", list(botao='vizinhos', etapa=4, mensagem='Baixando geometrias do SIGEF'))
                     if (sum(vizinhos$fonte_geo=='SNCI') > 0) {
                       # Filter the values from the data
                       ids_snci <- vizinhos$id_terra[vizinhos$fonte_geo == 'SNCI']
                       # Convert the values into a comma-separated string
                       snci_cql_string <- paste0("numero_certificado_snci IN ('", paste(ids_snci, collapse = "', '"), "')")
                       #fetch the data
                       snci_df = fetch_spatial_df(url = "https://geoserver.bocombbm.com.br/geoserver/INCRA/ows",
                                                  cql_filter = snci_cql_string,
                                                  camada = "INCRA:SNCI")
                       snci_df <-  snci_df %>% select("numero_certificado_snci", "geometry") %>% rename(id_terra = numero_certificado_snci)
                       # logg(paste0('snci: ', nrow(snci_df)))
                       print(paste0('snci: ', nrow(snci_df)))
                       
                     } else {
                       snci_df <- NULL
                       # logg('geoserver nao retornou terras do snci para a selecao')
                       print('geoserver nao retornou terras do snci para a selecao')
                     }
                     # 
                     ## unindo as geometrias do snci e sigef e depois unindo com as do .sqlite
                     
                     # logg('unindo as geometrias do sigef e snci com os metadados do .sqlite')
                     print('unindo as geometrias do sigef e snci com os metadados do .sqlite')
                     queue_progressbar$producer$fireAssignReactive("output_progressbar", list(botao='vizinhos', etapa=5, mensagem='Unindo geometrias e os dados do .sqlite'))
                     geometrias_wfs <- bind_rows(sigef_df, snci_df) #se uma delas for null, ele vai so manter a outra
                     
                     vizinhos_with_geometry <- left_join(vizinhos, geometrias_wfs, by='id_terra')
                     vizinhos_with_geometry <- vizinhos_with_geometry %>%
                       filter(!st_is_empty(geometry) & !is.na(geometry))  ## retira linhas que nao tem geometria (depois e bom investigar isso)
                     vizinhos_with_geometry <- st_make_valid(st_as_sf(vizinhos_with_geometry))
                     print(paste0('full: ', nrow(vizinhos_with_geometry)))
                     # logg(paste0('dataframe final: ', nrow(vizinhos_with_geometry)))
                     print(paste0('dataframe final: ', nrow(vizinhos_with_geometry)))
                     
                     
                     
                     ## separa entre vizinhos conhecidos com grupo, vizinhos conhecidos sem grupo e vizinhos nao conhecidos
                     vcg <- NULL
                     vcsg <- NULL
                     vnc <- NULL
                     vizinhos <- vizinhos_with_geometry
                     
                     
                     vcg <- vizinhos_with_geometry %>%
                       filter(
                         !is.na(grupo) &
                           !is.null(grupo) &
                           sapply(grupo, function(x) {
                             !identical(unique(unlist(strsplit(gsub("[^[:alnum:],]", "", x), ","))), "NA")
                           })
                       )
                     
                     vcsg <- vizinhos_with_geometry %>%
                       filter(
                         !is.na(documento) &
                           !is.null(documento) &
                           documento != "NA",
                         sapply(grupo, function(x) {
                           is.na(x) || is.null(x) || identical(unique(unlist(strsplit(gsub("[^[:alnum:],]", "", x), ","))), "NA")
                         })
                       )
                     
                     vnc <- vizinhos_with_geometry %>% filter( (is.na(documento) | is.null(documento) | documento=='NA') &
                                                                 sapply(grupo, function(x) {
                                                                   identical(unique(unlist(strsplit(gsub("[^[:alnum:],]", "", x), ","))), "NA")
                                                                 }
                                                                 )
                     )
                     
                     
                     # logg('calculando labels vcg')
                     print('calculando labels vcg')
                     if (nrow(vcg) > 0) {
                       cores_grupos <- c('#e6194b', '#3cb44b', '#ffe119', '#4363d8', '#f58231', '#911eb4', '#46f0f0',
                                         '#f032e6', '#bcf60c', '#fabebe', '#008080', '#e6beff', '#9a6324',
                                         '#800000', '#aaffc3', '#808000', '#ffd8b1', '#000075', '#808080', '#ffffff') #21cores
                       
                       ## faz um unique por linha nos grupos (separando por virgula)
                       vcg <- vcg %>%
                         mutate(
                           grupo = sapply(strsplit(as.character(grupo), ","), function(x) {
                             paste(unique(trimws(x)), collapse = ",")
                           })
                         )
                       
                       ## add 1 cor pra cada grupo unico (adicao de 2 grupos vira 1 nova cor)
                       vcg <- vcg %>%
                         mutate(
                           cor = cores_grupos[(as.numeric(factor(grupo)) - 1) %% length(cores_grupos) + 1]
                         )
                       
                       ## faz buffer, dissolve pelo grupo e depois desfaz o buffer (pra unir geometrias meio distantes)
                       vcg_buffered <- vcg %>% st_buffer(dist = 0.001)
                       vcg_buffered <- st_make_valid(vcg_buffered)
                       
                       vcg_dissolved <- vcg_buffered %>%
                         group_by(grupo) %>%  # Agrupar por "grupo"
                         summarise(geometry = st_union(geometry), .groups = "drop")  # Dissolver as geometrias
                       
                       vcg_dissolved <- st_make_valid(vcg_dissolved)
                       
                       
                       # Extrai geometrias desconectadas como poligonos separados
                       vcg_separated <- vcg_dissolved %>%
                         st_cast("MULTIPOLYGON") %>%  # Garante que as geometrias sejam multipoligonos
                         st_cast("POLYGON", group_or_split = TRUE)  # Separa os poligonos desconectados
                       
                       # # Opcional: Reatribuir os grupos originais (se necessario)
                       # vcg_separated <- vcg_separated %>%
                       #   mutate(grupo = as.factor(grupo))  # Reatribuir o grupo original, se necessario
                       
                       # vcg_dissolved <- vcg_dissolved %>% st_buffer(dist = -0.001)  # Buffer negativo de -0.001
                       #
                       # vcg_dissolved <- st_make_valid(vcg_dissolved)
                       
                       ## Calcula os centroides das geometrias do dissolvido
                       vcg_dissolved_centroids <- vcg_separated %>%
                         mutate(centroid = st_centroid(geometry))  # Adiciona os centroides como uma nova coluna
                       
                       # vcg_dissolved_centroids <- st_make_valid(vcg_dissolved_centroids)
                     }
                     
                     
                     # salva vcg, vcsg e vnc na Variavel reativa
                     if (!is.null(vcg)){
                       lista_resultados_output$vcg <- vcg
                       lista_resultados_output$vcg_dissolved_centroids <- vcg_dissolved_centroids
                     }
                     if (!is.null(vcsg)) {
                       lista_resultados_output$vcsg <- vcsg
                     }
                     if (!is.null(vnc)) {
                       lista_resultados_output$vnc <- vnc
                     }
                     if (!is.null(vizinhos)){
                       lista_resultados_output$vizinhos <- vizinhos
                     }
                     
                     # logg(lista_resultados_output)
                     queue_progressbar$producer$fireAssignReactive("output_progressbar", list(botao='vizinhos', etapa=6, mensagem='plotando os dados no mapa'))
                     queue_results$producer$fireAssignReactive("output_results", lista_resultados_output)
                     
                     # 
                     # queue_progressbar$producer$fireAssignReactive("output_progressbar", list(botao='vizinhos', etapa=5, mensagem='plotando o mapa (isso e feito no processo principal)'))
                     # queue_resultados$producer$fireAssignReactive("resultados_outputs", list(vcg = vcg, vcsg = vcsg, vnc = vnc))
                     # 
                     gc()
                     
                   }) #fecha futures, ou seja, sai do processo secundario
    
    gc()
    
    
  })
  
  
  
  
  observeEvent(input$clear_button, {
    
    
    # map <- leafletProxy("mymap")
    # print(map$center)
    
    output$mymap <- renderLeaflet({make_map()})
    
  })
  
  
  
  
  
}

# shinyApp(ui = ui, server = server)
# shinyApp(ui=ui, server=server, options = list(host = "0.0.0.0", port = 8889))
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
  ),authEnabled = TRUE  # Set to FALSE to disable authentication for testing  
)
