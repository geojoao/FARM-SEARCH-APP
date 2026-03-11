#' Map creation and layer controls
#' @import leaflet htmltools jsonlite dplyr
NULL

addLayersControlTree <- function(map, baseTree, overlayTree = NULL, options = list(), hiddenLayers = NULL) {
  layerTreePlugin <- htmltools::htmlDependency(
    name = "Leaflet.Control.Layers.Tree",
    version = "1.1.1",
    src = c(href = "https://cdn.jsdelivr.net/npm/leaflet.control.layers.tree@1.1.1/"),
    script = "L.Control.Layers.Tree.js",
    stylesheet = "L.Control.Layers.Tree.css"
  )
  registerPlugin <- function(map, plugin) {
    map$dependencies <- c(map$dependencies, list(plugin))
    map
  }
  defaultOptions <- list(
    namedToggle = FALSE,
    collapseAll = "Collapse all",
    expandAll = "Expand all",
    collapsed = FALSE
  )
  options <- modifyList(defaultOptions, options)
  baseTreeJSON <- jsonlite::toJSON(baseTree, auto_unbox = TRUE, json_verbatim = TRUE)
  overlayTreeJSON <- if (!is.null(overlayTree)) jsonlite::toJSON(overlayTree, auto_unbox = TRUE, json_verbatim = TRUE) else "null"
  optionsJSON <- jsonlite::toJSON(options, auto_unbox = TRUE, json_verbatim = TRUE)
  hiddenLayersJSON <- if (!is.null(hiddenLayers)) jsonlite::toJSON(hiddenLayers, auto_unbox = TRUE) else "[]"
  jsCode <- sprintf("function(el, x) {
    var mapInstance = this;
    function getLayerById(id) {
      var found = null;
      mapInstance.eachLayer(function(layer) {
        if(layer.options && layer.options.layerId === id) found = layer;
      });
      return found;
    }
    function assignLayers(tree) {
      if(tree.layerId) tree.layer = getLayerById(tree.layerId);
      if(tree.children) {
        for(var i = 0; i < tree.children.length; i++) assignLayers(tree.children[i]);
      }
    }
    var baseTreeObj = %s;
    var overlayTreeObj = %s;
    assignLayers(baseTreeObj);
    if (overlayTreeObj !== null) assignLayers(overlayTreeObj);
    var ctlOptions = %s;
    var ctl = L.control.layers.tree(baseTreeObj, overlayTreeObj, {
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
    });
    ctl.addTo(mapInstance).collapseTree(true).expandSelected(false);
    var hiddenLayers = %s;
    hiddenLayers.forEach(function(layerId) {
      var layer = getLayerById(layerId);
      if (layer) mapInstance.removeLayer(layer);
    });
  }", baseTreeJSON, overlayTreeJSON, optionsJSON, hiddenLayersJSON)
  leaflet::onRender(map %>% registerPlugin(layerTreePlugin), jsCode)
}

make_map <- function() {
  layers_df <- get_geoserver_layers()
  discovered_legends <- discover_all_legends(layers_df, ignored_layers = c())
  m <- leaflet::leaflet(options = leaflet::leafletOptions(attributionControl = FALSE)) %>%
    leaflet::addProviderTiles(leaflet::providers$OpenStreetMap, group = "OpenStreetMap", options = leaflet::providerTileOptions(layerId = "OpenStreetMap")) %>%
    leaflet::addProviderTiles(leaflet::providers$Esri.WorldImagery, group = "Satellite", options = leaflet::providerTileOptions(layerId = "Satellite")) %>%
    leaflet::addProviderTiles(leaflet::providers$Esri.WorldTopoMap, group = "Elevation", options = leaflet::providerTileOptions(layerId = "Elevation"))
  if (!is.null(layers_df)) {
    for (i in seq_len(nrow(layers_df))) {
      m <- m %>% leaflet::addWMSTiles(
        baseUrl = paste0("https://geoserver.bocombbm.com.br/geoserver/", layers_df$workspace[i], "/wms"),
        layers = layers_df$full_name[i],
        layerId = layers_df$full_name[i],
        options = leaflet::WMSTileOptions(
          format = "image/png",
          transparent = TRUE,
          layerId = layers_df$full_name[i]
        ),
        group = layers_df$title[i]
      )
    }
  }
  if (!is.null(discovered_legends) && length(discovered_legends) > 0) {
    for (legend_item in discovered_legends) {
      tryCatch({
        legend_data <- legend_item$data
        legend_group <- legend_item$title
        if (legend_item$type == "raster" && "color" %in% names(legend_data) && "label" %in% names(legend_data)) {
          m <- m %>% leaflet::addLegend(
            "bottomright",
            colors = legend_data$color,
            labels = legend_data$label,
            title = paste0(legend_group, ' Legend'),
            opacity = 1,
            group = paste0(legend_group, ' Legend'),
            layerId = paste0(legend_group, ' Legend')
          )
        } else if (legend_item$type == "polygon" && "fillColor" %in% names(legend_data) && "ruleTitle" %in% names(legend_data)) {
          m <- m %>% leaflet::addLegend(
            "bottomright",
            colors = legend_data$fillColor,
            labels = legend_data$ruleTitle,
            title = paste0(legend_group, ' Legend'),
            opacity = 1,
            group = paste0(legend_group, ' Legend'),
            layerId = paste0(legend_group, ' Legend')
          )
        }
        logg(paste("Added dynamic legend for:", legend_group))
      }, error = function(e) {
        logg(paste("Error adding legend for", legend_item$title, ":", e$message))
      })
    }
  }
  overlayTree <- if (!is.null(layers_df)) build_overlay_tree(layers_df) else NULL
  baseTree <- list(
    label = "Base Maps",
    children = list(
      list(label = "Satellite", layerId = "Satellite"),
      list(label = "Elevation", name = 'Map Layers', layerId = "Elevation"),
      list(label = "OpenStreetMap", layerId = "OpenStreetMap", selected = TRUE)
    )
  )
  hiddenLegendLayers <- c()
  if (!is.null(discovered_legends) && length(discovered_legends) > 0) {
    for (legend_item in discovered_legends) {
      hiddenLegendLayers <- c(hiddenLegendLayers, paste0(legend_item$title, ' Legend'))
    }
  }
  hiddenLegendLayers <- unique(hiddenLegendLayers)
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
    leaflet::addLayersControl(
      overlayGroups = hiddenLegendLayers,
      options = leaflet::layersControlOptions(collapsed = FALSE),
      position = 'bottomleft'
    ) %>%
    leaflet::hideGroup(hiddenLegendLayers)
  m <- m %>% leaflet::setView(lng = -47.9292, lat = -15.7801, zoom = 4)
  return(m)
}
