start_time <- Sys.time()
# Libraries ---------------------------------------------------------------
library(tidyverse)
library(ctv)
# Find_Views --------------------------------------------------------------
CTV_Views <- ctv::available.views()
# Extract names -----------------------------------------------------------
task_view <- function(x){
  tasks <- list()
  for(i in seq_along(1:length(x))){
    tasks[[i]] <- x[[i]][["name"]]
  }
  return(tasks)}

CTV_Views <- task_view(CTV_Views)
# Install Views -----------------------------------------------------------

purrr::map(CTV_Views,ctv::install.views,coreOnly=FALSE)


# END SCRIPT --------------------------------------------------------------
end_time  <- Sys.time()

message(paste0("The script took ", lubridate::as.duration(end_time - start_time), " to run.\n ==============================================END SCRIPT ======================================================"))
