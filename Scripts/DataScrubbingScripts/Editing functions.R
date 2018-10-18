source("Scripts/loadLibraries.r")
library(textreadr)
dir = "/Users/mohriner/Dropbox/Research/In Review/RapWork/Ohriner_flow_initial_manuscript/BookComponents"
f = list.files(dir,pattern="[^\\$]")
f = f[17:length(f)]
setwd(dir)
f %>% llply(.,read_docx) -> book
llply(book,function(i){
  i %>% strsplit(.," ") %>% unlist %>% gsub("[[:punct:]]","",.) %>% 
    laply(.,function(j){
      strsplit(j,"") %>% unlist %>% is_in(.,LETTERS) %>% all
    }) %>% which -> acro
  i %>% strsplit(.," ") %>% unlist %>% .[acro] %>% return
},.progress="text") -> acros
names(acros) = f

# Find words:
target = "A.A.P."
llply(book,function(i){
  laply(i,function(j){
    grepl(target,j)
  }) %>% which
}) -> res
names(res) = f
laply(res,length)
book[[4]][21]
  