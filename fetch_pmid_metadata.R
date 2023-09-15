# Fetch Article Metadata

# Given a newline list of PMIDs, this script will use the NCBI API to fetch
# article metadata and prepare data frames for the shiny-publications app.

library(tidyverse)
library(plyr)
library(httr)
library(jsonlite)

# Read PMIDs
pmid.list <- read.table("PMID_list.txt", stringsAsFactors = F)

# Fetch metadata
ncbi.url <- "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?db=pubmed&id="

pmid.df <- data.frame()
  
for(p in pmid.list$V1[65:length(pmid.list$V1)]){
  print(p)
  p.res <- httr::GET(paste0(ncbi.url,p))
  p.json <- httr::content(p.res)
  # Yikes! This json is atrocious! Let's whack-a-mole...
  p.json <- paste0("{",sub(".*?\\{","",p.json))
  p.json <- sub("\\}\n\n\n","\\}",p.json)
  p.json <- gsub("\\n\\s*","",p.json)
  p.json <- gsub('\\"\\"','',p.json)
  p.json <- gsub('\\{\\s*(\\w+\\s+\\w+)\\s*\\{', '\\{\\"\\1\\":\\{',p.json)
  p.json <- gsub('\\{\\s*(\\w+)\\s*\\{', '\\{\\"\\1\\":\\{',p.json)
  p.json <- gsub('\\}\\,\\s*(\\w+)\\s*\\{', '\\}\\,\\"\\1\\":\\{',p.json)
  p.json <- gsub('\\{\\s*(\\w+) (\\w+|\\w+-\\w+)\\s*([,|}])', '\\{\\"\\1\\":\\"\\2\\"\\3',p.json)
  p.json <- gsub('\\},\\s*(\\w+) (\\w+|\\w+-\\w+)\\s*([,|}])', '\\},\\"\\1\\":\\"\\2\\"\\3',p.json)
  p.json2 <- gsub('\\",\\s*(\\w+) (\\w+|\\w+-\\w+)\\s*([,|}])', '\\",\\"\\1\\":\\"\\2\\"\\3',p.json)
  while(p.json2 != p.json){
    p.json <- p.json2
    p.json2 <- gsub('\\",\\s*(\\w+) (\\w+)\\s*([,|}])', '\\",\\"\\1\\":\\"\\2\\"\\3',p.json)
  }
  p.json <- gsub('\\,\\s*(\\w+)\\s*\\{', '\\,\\"\\1\\":\\{',p.json)
  p.json <- gsub('(\\{|[}\\"],)\\s*(\\w+\\s+\\w+)\\s*\\"', '\\1\\"\\2\\":\\"',p.json)
  p.json <- gsub('(\\{|[}\\"],)\\s*(\\w+-\\w+)\\s*\\"', '\\1\\"\\2\\":\\"',p.json)
  p.json <- gsub('(\\{|[}\\"],)\\s*(\\w+)\\s*\\"(\\w+)', '\\1\\"\\2\\":\\"\\3',p.json)
  p.json <- gsub('(\\{|[}\\"],)\\s*(\\w+\\s+\\w+)\\s*\\{', '\\1\\"\\2\\":\\{',p.json)
  p.json <- gsub('(\\{|[}\\"],)\\s*(\\w+-\\w+)\\s*\\{', '\\1\\"\\2\\":\\{',p.json)
  p.json <- gsub('(\\{|[}\\"],)\\s*(\\w+)\\s*\\{', '\\1\\"\\2\\":\\{',p.json)
  p.json <- gsub('\\{\\s*(\\w+\\s+\\w+)\\s*\\"', '\\{\\"\\1\\":\\"',p.json)
  p.json <- gsub('\\{\\s*(\\w+)\\s*\\"', '\\{\\"\\1\\":\\"',p.json)
  p.json <- gsub('names std\\":\\{(\\{.*?\\})+\\}\\}', 'names std\\":\\[\\1\\]\\}',p.json)
  p.json <- gsub('names ml\\":\\{(.*?)\\}\\}', 'names ml\\":\\[\\1\\]\\}',p.json)
  p.json <- gsub('history\\":\\{(\\{.*?\\})+\\}\\}\\}', 'history\\":\\[\\1\\]\\}\\}',p.json)
  p.json <- gsub('mesh\\":\\{(\\{.*?\\})+\\},\\"', 'mesh\\":\\[\\1\\],\\"',p.json)
  p.json <- gsub('qual\\":\\{(\\{.*?\\})+\\}\\}', 'qual\\":\\[\\1\\]\\}',p.json)
  p.json <- gsub('substance\\":\\{(\\{.*?\\})+\\}', 'substance\\":\\[\\1\\]',p.json)
  p.json <- gsub('xref\\":\\{(\\{.*?\\})+\\}', 'xref\\":\\[\\1\\]',p.json)
  p.json <- gsub('pub-type\\":\\{(.*?)\\}', 'pub-type\\":\\[\\1\\]',p.json)
  p.json <- gsub('idnum\\":\\{(.*?)\\}', 'idnum\\":\\[\\1\\]',p.json)
  p.data <- jsonlite::fromJSON(p.json)
  # p.df <- lapply(p.data, function(x) {
  #   x[sapply(x, is.null)] <- NA
  #   unlist(x)
  # })
  this.df <- data.frame(pmid = p, 
             year = p.data[[2]]$cit$`from journal`$imp$`date std`$year,
             title = p.data[[2]]$cit$title$name,
             journal = p.data[[2]]$cit$`from journal`$title$name,
             author.list = paste(p.data[[2]]$cit$authors$`names std`$`name ml`, collapse = ", "),
             doi = p.data[[2]]$cit$ids$doi,
             mesh.list = paste(p.data$medent$mesh$term[which(p.data$medent$mesh$mp=="TRUE")], collapse = " | "))
  pmid.df <- rbind(pmid.df, this.df)
  Sys.sleep(2)
}

saveRDS(pmid.df, "pmid_df.rds")
