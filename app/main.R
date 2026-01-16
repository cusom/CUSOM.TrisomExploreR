box::use(
  config[get],
  glue[glue],
  shiny[moduleServer, NS],
)

box::use(
  app/logic/app_resources/app_configs[TrisomExplorerAppManager]
)

#load configs
application_id <- config::get(file = "config.yml", "application_id")
app_config <- config::get(file = "app/app_configs.yml", config = application_id)

# dynamically load entrypoint module
eval(parse(text = glue("box::use({app_config$entry_point_path}/{app_config$entry_point})")))

app_settings <- TrisomExplorerAppManager$new(
  application_id = application_id
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
