options(box.path = unique(c(
  normalizePath(getwd(), winslash = "/", mustWork = TRUE),
  getOption("box.path") %||% character(0)
)))

box::use(
  config[get],
  glue[glue],
  shiny[moduleServer, NS],
)

box::use(
  app/logic/shared/global_utils[`%||%`]
)

box::use(
  app/logic/app_resources/app_configs[create_app_settings, load_application_config]
)

#load configs
application_id <- config::get(file = "config.yml", "application_id")
app_config <- load_application_config(application_id = application_id)

# dynamically load entrypoint module
eval(parse(text = glue("box::use({app_config$entry_point_path}/{app_config$entry_point})")))

app_settings <- create_app_settings(
  application_id = application_id,
  app_config = app_config
)

#' @export
ui <- function(id) {

  ns <- NS(id)

  #dynamically invoke entrypoint UI
  do.call(
    eval(parse(text = glue("{app_config$entry_point}$ui"))), list(
      id = ns(app_config$parent_namespace)
    )
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    #dynamically invoke entrypoint Server
    do.call(
      eval(parse(text = glue("{app_config$entry_point}$server"))), list(
        id = app_config$parent_namespace,
        app_config = app_settings
      )
    )
  })
}
