library(shiny)
library(ggplot2)

source("R/helpers/app_config.R")
source("R/helpers/input_validation.R")
source("R/helpers/formatting.R")

source("R/shared/discount_models.R")
source("R/shared/utility_functions.R")
source("R/shared/ctb_functions.R")
source("R/shared/choice_functions.R")
source("R/shared/visualization_functions.R")

source("R/modules/mod_discount_curves.R")
source("R/modules/mod_binary_choice.R")
source("R/modules/mod_ctb_model.R")
source("R/modules/mod_model_components.R")

ui <- navbarPage(...)
server <- function(input, output, session) {...}

shinyApp(ui, server)
