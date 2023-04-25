.onLoad <- function(...) {
  utils::data("GSEA_hallmarks", "states_shp", package = "TrisomExploreR", envir = parent.env(environment()))
  shiny::addResourcePath('custom-assets', system.file('assets', package = 'TrisomExploreR'))
  shiny::addResourcePath('www', system.file('www', package = 'TrisomExploreR'))
}
