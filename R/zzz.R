
#' @importFrom utils packageVersion

ProSGPVStartupMessage <- function()
{
  # Startup message obtained as
  # > figlet -f slant ProSGPV
  msg <- c(paste0(
"          ____            _____ __________ _    __
         / __ \\_________ / ___// ____/ __ \\ |  / /
        / /_/ / ___/ __ \\\\__ \\/ / __/ /_/ / | / /
       / ____/ /  / /_/ /__/ / /_/ / ____/| |/ /
      /_/   /_/   \\____/____/\\____/_/     |___/   version ",
    packageVersion("ProSGPV")))
  return(msg)
}


#' @importFrom utils packageVersion

.onAttach <- function(lib, pkg)
{

  # startup message
  msg <- ProSGPVStartupMessage()
  if(!interactive())
    msg[1] <- paste("Package 'ProSGPV' version", packageVersion("ProSGPV"))
  packageStartupMessage(msg)
  invisible()
}
