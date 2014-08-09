#' @export
scaffold.run <- function()
{
    runApp(appDir = file.path(system.file(package = "scaffold"), "shinyGUI"), launch.browser=T)
}



