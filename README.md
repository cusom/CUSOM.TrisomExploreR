# CUSOM.TrisomExploreR
Common set of modules / logic / assets used across TrisomExplorer Applications

## Overview 

## Deployment

Use `scripts/deploy_rsconnect.R` for deterministic `rsconnect` bundles.

Note: scripted deploys pass explicit `appFiles`, so `.rscignore` patterns are not used for this flow.
For `shinyapps.io` targets, staged `dependencies.R` excludes `library(odbc)` to avoid unsupported ODBC source builds during deployment.
For `shinyapps.io` targets, staged `app/logic/app_resources/data_services.R` also removes static `odbc` import hints so rsconnect does not infer `odbc` at build time.

- Reads `application_id` from `config.yml`.
- Resolves deployment target from `config.yml -> deploy_targets`.
- Bundles required Rhino app files (`app.R`, `app`, and config/dependency files).
- `renv.lock` is optional (off by default for shinyapps.io reliability).
- Excludes:
	- any file in a path segment containing `__ignore__` (for example `app/logic/___ignore__/...`)
	- everything under `app/logic/legacy_logic/`

Deployment target keys in `config.yml -> deploy_targets` can use either style:

- `appName` / `appTitle` / `account` / `server` / `appId`
- `name` / `title` / `username` / `server` / `appId` (dcf-style)

Dry-run manifest:

`R -q -f scripts/deploy_rsconnect.R --args --dry-run`

Dry-run using `renv.lock` dependency capture:

`R -q -f scripts/deploy_rsconnect.R --args --dry-run --use-renv-lock`

Deploy using current `application_id`:

`R -q -f scripts/deploy_rsconnect.R`

Override app id for one-off deploy:

`R -q -f scripts/deploy_rsconnect.R --args --app-id=BC91AA86-40C5-48CA-BD26-EE2BE88B83C4`

Run from an interactive R console:

`source("scripts/deploy_rsconnect.R")`

`deploy_rsconnect(dry_run = TRUE)`

`deploy_rsconnect(dry_run = TRUE, use_renv_lock = TRUE)`

`deploy_rsconnect(app_id = "BC91AA86-40C5-48CA-BD26-EE2BE88B83C4")`
