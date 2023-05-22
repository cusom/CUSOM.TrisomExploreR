#' states_shp
#'
#' This geospatial dataset defines the current Census State and Equivalent National geospatial entities. It was generated using data sourced from
#' US Census data, which is publically available here: \url{https://www.census.gov/geographies/mapping-files/2013/geo/carto-boundary-file.html}
#'
#' \itemize{
#'   \item STATEFP, Current state Federal Information Processing Series (FIPS) code
#'   \item STATENS, Current state ANSI code
#'   \item AFFGEOID, American FactFinder summary level code + geovariant code + '00US' + GEOID
#'   \item GEOID, State identifier; state FIPS code
#'   \item STUSPS, Current United States Postal Service state abbreviation
#'   \item NAME, Current state name
#'   \item LSAD, Current legal/statistical area description code for state
#'   \item ALAND, Current land area (square meters)
#'   \item AWATER, Current water area (square meters)
#'   \item geometry, mutlipolygon - list of geographical elements defining shape
#' }
#'
#' @docType data
#' @keywords datasets
#' @name states_shp
#' @usage data("states_shp")
#' @format An sf (simple features) dataset containing spatial data for 52 states within the United states. The dataset contains the following attributes:
NULL

#' GSEA_hallmarks
#'
#' A list of 50 Hallmark gene sets / pathways for Gene Set Enrichment Analysis. Data was sourced from the UC San Diego Gene Set Enrichment Analysis website:
#' \url{https://www.gsea-msigdb.org/gsea/msigdb/human/collections.jsp#H}
#'
#' \itemize{
#'  \item GSEA_hallmarks - list of 50 named gene set / pathway along with a collection of genes listed in a delimited string
#' }
#'
#' @docType data
#' @keywords datasets
#' @name GSEA_hallmarks
#' @usage GSEA_hallmarks
#' @format A list
NULL
