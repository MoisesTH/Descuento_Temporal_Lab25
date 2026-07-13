source("scripts/sync_shared_files.R")

rsconnect::deployApp(
  appDir = "apps/app-a-modelos-didacticos",
  appName = "temporal-discount-models"
)
