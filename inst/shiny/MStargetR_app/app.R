# MStargetR Shiny Application
# Main entry point
#
# Shiny's built-in R/ autoloader sources every file under R/ into the app
# environment before ui.R and server.R are evaluated, so no manual
# source() calls are needed here.  The explicit source() calls have been
# removed to prevent double-loading helpers and CWD-sensitive path issues
# when the app is launched via runApp(appDir) without setwd().

# Allow uploads up to 100 MB (default Shiny limit is 5 MB)
options(shiny.maxRequestSize = 100 * 1024^2)

shiny::shinyApp(ui = ui, server = server)
