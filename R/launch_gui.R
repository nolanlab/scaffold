#' @export
scaffold.run <- function(launch.browser = TRUE, ...)
{
    runApp(appDir = file.path(system.file(package = "scaffold"), "shinyGUI"), launch.browser = launch.browser, ...)
}



