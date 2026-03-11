# Logging utility
logg <- function(texto) {
  message(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), " | Farm Owners App | ", texto)
}
