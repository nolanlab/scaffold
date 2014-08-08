#' @import shiny


#' @export
run_scaffold <- function()
{
    runApp(appDir = file.path(system.file(package = "scaffold"), "shinyGUI"), launch.browser=T, host = "0.0.0.0")
}



