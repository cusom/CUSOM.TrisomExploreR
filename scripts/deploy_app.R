box::use(
    config[get]
)

source("~/source/TrisomExploreR/scripts/deploy_rsconnect.r")

app_id <- get(file = "config.yml", "application_id")

deploy_rsconnect(dry_run = FALSE, app_id = app_id)
