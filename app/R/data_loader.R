#' Data loading for farm owners
#' @import sf
NULL

GPKG_PATH <- "~/AFS/Pesquisa_Quantitativa/Compartilhado/rundeck_files_folder/Farm_Owners_Data/farm_owners_data.gpkg"

load_all_data <- function() {
  query_all_data <- "SELECT grupo, uf, id_terra, municipio, nome_proprietario, documento, codigo_imovel, matricula, nome_area FROM terras_e_proprietarios"
  x <- sf::st_read(
    GPKG_PATH,
    query = query_all_data,
    quiet = TRUE
  )
  cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "- atualizou dado\n")
  return(x)
}
