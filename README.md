Open educational resources for teaching R
===================================================

In this repository, we will introduce specific open infrastructure that we have designed to support community-based knowledge sharing and learning in R. 

This infrastructure gathers modules for constructing **domain spesific** assignments in multiple languages for various phases of data analysis in R. Assignments are coupled with automated evaluation routines and the system can be utilised openly or internally using any file sharing technology such as Github, Sharepoint or plain network drives.


```r
get_modules <- function(topic="read_data", task="reading_csv_data_from_url" ,lang="fi"){
  
  require(dplyr)
  raw <- pref <- yaml::yaml.load_file(paste0("https://raw.githubusercontent.com/rOpenGov/edu/master/modules/",topic,".yml"))
  raw_d <- plyr::ldply (raw, data.frame, stringsAsFactors=FALSE)
  raw_d <- raw_d[grepl(task,raw_d$id),]
  raw_d <- raw_d %>%  select(contains(lang),contains("ans"))
  return(raw_d)
}

get_modules()
```
