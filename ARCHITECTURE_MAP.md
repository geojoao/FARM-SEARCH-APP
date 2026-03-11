# Mapa Arquitetural - Farm Search App (Shiny)

## 1. Lista de Inputs

| Input ID | Tipo | Descrição |
|----------|------|-----------|
| `Grupo_proprietario` | selectInput | Grupo do proprietário |
| `Nome_proprietario` | selectInput | Nome do proprietário |
| `Documento_proprietario` | selectInput | Documento do proprietário |
| `UF` | selectInput (multiple) | Estado |
| `Municipio` | selectInput (multiple) | Município |
| `Codigo_Imovel_Rural` | selectInput (multiple) | Código do imóvel rural |
| `Nome_Fazenda` | selectInput (multiple) | Nome da fazenda |
| `Matricula` | selectInput (multiple) | Matrícula |
| `ID_Terra` | selectInput (multiple) | ID Terra |
| `search_button` | actionButton | Botão de pesquisa |
| `load_neighbors_button` | actionButton | Botão de carregar vizinhos |
| `clear_button` | actionButton | Botão de limpar mapa |
| `mymap_bounds` | (leaflet) | Limites visíveis do mapa (usado em Load Neighbors) |

---

## 2. Lista de Outputs

| Output ID | Tipo | Descrição |
|-----------|------|-----------|
| `mymap` | renderLeaflet | Mapa Leaflet principal |
| `download_gpkg` | downloadHandler | Download do arquivo GPKG |
| `download_buttons` | renderUI | Botões de download (condicional) |

---

## 3. Expressões Reativas (reactiveVal)

| Nome | Tipo | Estrutura |
|------|------|-----------|
| `download_data` | reactiveVal | sf/data.frame ou NULL — dados disponíveis para download |
| `output_progressbar` | reactiveVal | list(botao, etapa, mensagem) — estado da barra de progresso |
| `output_results` | reactiveVal | list(botao, erro, warning, texto, terras_filtradas, vizinhos, vcg, vcsg, vnc, vcg_dissolved_centroids) — resultados da busca |

**Nota:** Não há `reactive()` explícitos; o app usa `reactiveVal()` para estado mutável.

---

## 4. Observers

| Observer | Gatilho | Descrição |
|----------|---------|-----------|
| `update_data_observer` | `invalidateLater(5*60*1000)` | Recarrega `all_data` a cada 5 minutos (contexto global) |
| `observeEvent(input$search_button)` | Clique em Search | Executa busca por atributos via `future_promise`; popula `output_results` e `output_progressbar` |
| `observeEvent(output_results())` | Mudança em `output_results` | Atualiza mapa (leafletProxy), modais de erro/warning, e `download_data` |
| `observeEvent(output_progressbar())` | Mudança em `output_progressbar` | Atualiza barra de progresso e fecha `progressSweetAlert` ao concluir |
| `observeEvent(input$load_neighbors_button)` | Clique em Load Neighbors | Busca vizinhos na área visível via `future_promise`; popula `output_results` e `output_progressbar` |
| `observeEvent(input$clear_button)` | Clique em Clear | Re-renderiza o mapa com `make_map()` |

---

## 5. Dependências: Outputs → Reactives

```
output$mymap
├── Render inicial: make_map() (sem dependências reativas)
└── Atualização: observeEvent(output_results()) → leafletProxy("mymap")
    └── Depende de: output_results()

output$download_gpkg
└── Depende de: download_data()

output$download_buttons
└── Depende de: download_data() [req(download_data())]
```

---

## 6. Fluxo de Dados (Resumo)

```
[Inputs de busca] ──► input$search_button ──► future_promise (pesquisa)
                                              │
                                              ├──► output_progressbar
                                              └──► output_results
                                                        │
                                                        ▼
                                              observeEvent(output_results())
                                                        │
                                                        ├──► leafletProxy("mymap")
                                                        ├──► download_data()
                                                        └──► Modais (erro/warning)

input$mymap_bounds ──► input$load_neighbors_button ──► future_promise (vizinhos)
                                                      │
                                                      ├──► output_progressbar
                                                      └──► output_results
                                                                │
                                                                ▼
                                                      (mesmo fluxo acima)

input$clear_button ──► output$mymap <- renderLeaflet({make_map()})
```

---

## 7. Estrutura Modular (Implementada)

### Visão Geral dos Módulos

```
app/
├── app.R                    # Orquestração principal
├── modules/
│   ├── mod_search.R         # Módulo de busca
│   ├── mod_neighbors.R      # Módulo de vizinhos
│   ├── mod_map.R            # Módulo do mapa
│   ├── mod_download.R       # Módulo de download
│   ├── mod_sidebar.R        # Módulo da sidebar (filtros + botões)
│   └── mod_progress.R       # Módulo da barra de progresso
├── R/
│   ├── 00_utils.R           # logg
│   ├── geoserver.R          # Funções GeoServer (layers, WFS, legendas)
│   ├── data_loader.R        # Carregamento de dados (load_all_data)
│   └── map_factory.R        # make_map, controles do mapa
└── ARCHITECTURE_MAP.md
```

---

### Módulo 1: `mod_search.R`

| Atributo | Detalhes |
|----------|----------|
| **Arquivo** | `modules/mod_search.R` |
| **Inputs** | `Grupo_proprietario`, `Nome_proprietario`, `Documento_proprietario`, `UF`, `Municipio`, `Codigo_Imovel_Rural`, `Nome_Fazenda`, `Matricula`, `ID_Terra`, `search_button` |
| **Outputs** | Nenhum direto (usa `output_results` e `output_progressbar` via queue) |
| **Reactives consumidos** | Nenhum |
| **Reactives produzidos** | `output_results`, `output_progressbar` (via `fireAssignReactive`) |
| **Responsabilidade** | Montar query SQL, executar busca no gpkg, buscar geometrias WFS (SNCI/SIGEF), retornar `terras_filtradas` |

---

### Módulo 2: `mod_neighbors.R`

| Atributo | Detalhes |
|----------|----------|
| **Arquivo** | `modules/mod_neighbors.R` |
| **Inputs** | `load_neighbors_button`, `mymap_bounds` |
| **Outputs** | Nenhum direto |
| **Reactives consumidos** | Nenhum |
| **Reactives produzidos** | `output_results`, `output_progressbar` |
| **Responsabilidade** | Extrair hexágonos H3 da área visível, buscar terras no gpkg, buscar geometrias WFS, classificar vcg/vcsg/vnc, retornar vizinhos |

---

### Módulo 3: `mod_map.R`

| Atributo | Detalhes |
|----------|----------|
| **Arquivo** | `modules/mod_map.R` |
| **Inputs** | `clear_button` |
| **Outputs** | `mymap` (leafletOutput) |
| **Reactives consumidos** | `output_results` |
| **Reactives produzidos** | Nenhum |
| **Responsabilidade** | Renderizar mapa inicial, atualizar mapa via leafletProxy com `terras_filtradas` ou `vcg/vcsg/vnc` |

---

### Módulo 4: `mod_download.R`

| Atributo | Detalhes |
|----------|----------|
| **Arquivo** | `modules/mod_download.R` |
| **Inputs** | Nenhum |
| **Outputs** | `download_gpkg`, `download_buttons` |
| **Reactives consumidos** | `download_data` |
| **Reactives produzidos** | Nenhum |
| **Responsabilidade** | Exibir botões de download condicionalmente e exportar GPKG |

---

### Módulo 5: `mod_sidebar.R`

| Atributo | Detalhes |
|----------|----------|
| **Arquivo** | `modules/mod_sidebar.R` |
| **Inputs** | Todos os selectInputs e actionButtons da sidebar |
| **Outputs** | `download_buttons` (ou delega ao mod_download) |
| **Reactives consumidos** | `all_data` (ou dados para popular choices) |
| **Reactives produzidos** | Nenhum |
| **Responsabilidade** | UI da sidebar, `updateSelectizeInput` com choices do `all_data` |

---

### Módulo 6: `mod_progress.R` (opcional)

| Atributo | Detalhes |
|----------|----------|
| **Arquivo** | `modules/mod_progress.R` |
| **Inputs** | Nenhum |
| **Outputs** | progressSweetAlert (UI) |
| **Reactives consumidos** | `output_progressbar` |
| **Reactives produzidos** | Nenhum |
| **Responsabilidade** | `observeEvent(output_progressbar())` — atualizar barra e fechar alert |

---

### Funções Auxiliares (R/)

| Arquivo | Funções | Descrição |
|---------|---------|-----------|
| `geoserver.R` | `get_geoserver_layers`, `build_overlay_tree`, `discover_all_legends`, `get_legend`, `get_polygon_legend`, `fetch_spatial_df` | Integração com GeoServer |
| `data_loader.R` | `load_all_data` | Carregamento do gpkg |
| `map_factory.R` | `make_map`, `addLayersControlTree`, `adjust_map_controls` | Criação e configuração do mapa |

---

## 8. Interface entre Módulos

```
                    ┌─────────────────┐
                    │   mod_sidebar   │
                    │  (filtros + UI) │
                    └────────┬────────┘
                             │
         ┌───────────────────┼───────────────────┐
         │                   │                   │
         ▼                   ▼                   ▼
┌─────────────────┐  ┌─────────────────┐  ┌─────────────────┐
│   mod_search    │  │  mod_neighbors   │  │   mod_map       │
│  (search btn)   │  │ (load neighbors) │  │ (clear + proxy)  │
└────────┬────────┘  └────────┬────────┘  └────────┬────────┘
         │                    │                    │
         └────────────────────┼────────────────────┘
                              │
                    output_results, output_progressbar
                    download_data
                              │
         ┌────────────────────┼────────────────────┐
         ▼                    ▼                    ▼
┌─────────────────┐  ┌─────────────────┐  ┌─────────────────┐
│  mod_progress   │  │  mod_download    │  │   mod_map       │
│  (progress bar) │  │  (gpkg export)   │  │ (plotagem)      │
└─────────────────┘  └─────────────────┘  └─────────────────┘
```

---

## 9. Variáveis Globais / Compartilhadas

- `all_data`: carregada por `load_all_data()`, atualizada por `update_data_observer`
- `output_results`, `output_progressbar`, `download_data`: reativos compartilhados entre módulos
- `file_source_queue_progressbar`, `file_source_queue_results`: queues IPC para comunicação com `future_promise`
