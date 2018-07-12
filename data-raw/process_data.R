## This script processes the yml-files in ./data-raw/-folder and saves them in
## rda-format in data-folder.
## Documentation in ./R/-folder as data.R must be updated manually.

# clean the environment
rm(list = ls())
# read all ymls into data.frame and assign object names corresponding with yml file names
library(dplyr)
flies <- list.files("./data-raw/",pattern = ".yml", full.names = TRUE)
for (i in flies){
  tmp <- yaml::yaml.load_file(i) %>%
    plyr::ldply(., data.frame, stringsAsFactors=FALSE)
  objectname <- gsub("\\./data-raw//|\\.yml", "", i)
  assign(objectname, tmp, envir = globalenv())
}
rm(tmp)

# save each data.frame as .rds into ./data/-folder
dir.create("./data", showWarnings = FALSE)
dfs <- Filter(function(x) is.data.frame(get(x)) , ls())
for(d in dfs) {
  save(list=d, file=paste0("./data/", d, ".rda"))
}

