
#Preparing Packages
packages = c('tidytext','widyr','wordcloud',
             'DT','ggwordcloud','textplot',
             'lubridate', 'hms', 'tidyverse',
             'tidygraph', 'ggraph', 'igraph')

for (p in packages) {
  if(!require(p, character.only = T)){
    install.packages(p)
  }
  library(p, character.only = T)
}


list_of_files <- "C:/Users/aaron/Documents/Aaron Documents/Semester 3/VA/Assignment/Raw Data/News Articles"

read_folder <- function (infolder) {
  tibble(file = dir(infolder,
                    full.names = TRUE)) %>%
    mutate(text = map(file,
                      read_lines)) %>%
    transmute(id=basename(file),
              text) %>%
    unnest(text)
}

raw_text <- tibble(folder =
                     dir(list_of_files,
                         full.names=TRUE)) %>%
  mutate(folder_out = map(folder,
                          read_folder)) %>%
  unnest(cols = c(folder_out)) %>%
  transmute(newsgroup = basename(folder),
            id, text)
write_rds(raw_text, "C:/Users/aaron/Documents/Aaron Documents/Semester 3/VA/Assignment/Raw Data/News Articles/allfiles.rds")










list_of_files <- list.files(path = "C:/Users/aaron/Documents/Aaron Documents/Semester 3/VA/Assignment/Raw Data/News Articles", recursive = TRUE,
                            pattern = "\\.txt$", 
                            full.names = TRUE)

df <- list_of_files %>%
  set_names(.) %>%
  map_df(read_table, .id = "FileName")


# Put in your actual path where the text files are saved
mypath = "C:/Users/aaron/Documents/Aaron Documents/Semester 3/VA/Assignment/Raw Data/News Articles"
setwd(mypath)

# Create list of text files
txt_files_ls = list.files(path=mypath, pattern="\\.txt", recursive =T) 
# Read the files in, assuming comma separator
txt_files_df <- lapply(txt_files_ls, function(x) {read.table(file = x, header=F, sep = "")})
# Combine them
combined_df <- do.call("rbind", lapply(txt_files_df, as.data.frame)) 

DT <- rbindlist(sapply(list_of_files, fread, simplify = FALSE),
                use.names = TRUE, idcol = "FileName", fill=TRUE)

