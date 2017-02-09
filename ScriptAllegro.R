# Allegro
# Daniel Timponi
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

library(tidyr)
library(dplyr)
library(splitstackshape)


######################################### Import
  #import data 
  data <- read.csv('data/log.csv', sep=";" , header=F, col.names=c('rev', 'author', 'date', 'path'))

###################################### Filter 1

  #add column of module/directory and filter unwanted modules/directory
    data <- data %>%
    mutate(module = gsub('/[^/]+$', '', path)) %>%
    #filter(grepl('([.]c|[.]h)', path))
    filter(module %in% c('src','src/android','src/gp2xwiz','src/iphone','src/linux','src/macosx','src/misc','src/opengl','src/raspberrypi','src/sdl','src/unix','src/win','src/x')) 

  #filter files modified less than 2 times
  #dados2009<-dados2009[dados2009$path %in% names(table(dados2009$path)[table(dados2009$path) >= 2 ]), ]
  #filter files modified more than 12 times
  #dados2009<-dados2009[dados2009$path %in% names(table(dados2009$path)[table(dados2009$path) <= 11 ]), ]

###################################### Tables for clustering
  #split revs into columns, select path + rev
  tab <- as.data.frame.matrix(xtabs(~ path + rev, data=dados2009[c('rev', 'path')]))
  # the same as xtabs
  tab1 <- as.data.frame.matrix(table(dados2009$path, dados2009$rev))
  #select rev+path, merge paths, concat revs
  tab2 <- dados2009 %>% select(rev, path) %>%  group_by(path) %>%  summarise(revs = paste(rev, collapse=","))


###################################### Useful tables 1
  #Number of modules modified in a rev
  modules <- data %>%
    group_by(rev) %>%
    summarise(n_module = n_distinct(module)) %>%
    arrange(rev)
  modules2 <- modules %>%
    group_by(n_module) %>%
    summarise(n_rev= n())%>%
    mutate (porcentage = round(n_rev/sum(n_rev) * 100, 1) ) %>%
    arrange(n_module)

  hist(modules$n_module,xlab="Modules", ylab="Revisons", main="Number of modules modified in a revision")
  boxplot(modules$n_module, ylab="Modules",main="Number of modules modified in a revision" )
  plot(modules2$n_module,modules2$n_rev, type="l", lty=2)

  #number of times a file was modified
  files <- data %>% group_by(path) %>% summarise( qtd = n() ) %>%  arrange(qtd)
  hist(files$qtd,xlab="Revisons", ylab="Files", main="Number of revision a file was modified" )
  boxplot(files$qtd, ylab="Revisions", main="Number of revision a file was modified")

  #Number of files modified in a rev
  revs <- data %>% group_by(rev) %>% summarise( qtd = n() ) %>%  arrange(qtd)
  hist(revs$qtd,xlab="Files", ylab="Revisons", main="Number of files modified in a revision")
  boxplot(revs$qtd, ylab="Files",main="Number of files modified in a revision" )


##################################### Filter 2
  #filter revs that modified more than 4 modules
 #revs_noise <- modules %>% filter(n_module>4)
 #dados2009<-dados2009 %>% filter( !rev %in% revs_noise$rev )

  #only updating modules
  #modules <- modules %>% filter( !rev %in% revs_noise$rev )


###################################### Useful tables
  #module table with the rev number and modules touched
  modules3 <- data %>%
    select(rev, module) %>%
    group_by(rev) %>%
    arrange(module)%>%
    summarise( n_module = n_distinct(module), module = paste(unique(module), collapse=", ")) %>%
    arrange(n_module,module, rev)

######################################## filter 3
  #filter revs that modified a set of packages only once
  revs_noise2<-modules3[modules3$module %in% names(table(modules3$module)[table(modules3$module) < 2 ]), ]
  data<-data %>% filter( !rev %in% revs_noise2$rev )

  #only updating modules
  modules <- modules %>% filter( !rev %in% revs_noise2$rev )
  modules3 <- modules3 %>% filter( !rev %in% revs_noise2$rev )


###################################### Useful tables
  #number of times a file was modified
  files <- data %>% group_by(path) %>% summarise( qtd = n() ) %>%  arrange(qtd)
  hist(files$qtd,xlab="Revisons", ylab="Files", main="Number of revision a file was modified" )
  boxplot(files$qtd, ylab="Revisions", main="Number of revision a file was modified")

  #Number of files modified in a rev
  revs <- data %>% group_by(rev) %>% summarise( n_files = n() ) %>%  arrange(rev)
  hist(revs$n_files,xlab="Files", ylab="Revisons", main="Number of files modified in a revision")
  boxplot(revs$n_files, ylab="Files",main="Number of files modified in a revision" )

  #upgrading module3 table
  modules3 <- modules3 %>% arrange (rev)
  #modules3 [,"n_files"]  <- revs %>% arrange(rev) %>% select (n_files)
  file_list <- data %>%
    select(rev, path) %>%
    group_by(rev) %>%
    arrange(path)%>%
    summarise( n_files = n_distinct(path), path = paste(unique(path), collapse=", "))
  modules3  [,"n_files"] <-  file_list %>% arrange(rev) %>% select (n_files)
  modules3  [,"path"] <-  file_list %>% arrange(rev) %>% select (path)
  modules3 <- modules3 %>% arrange(n_module,module, rev)


 modules4<-modules3 %>% select(n_module, n_files)%>%
   group_by(n_module)%>%
   mutate(n_files=sum(n_files)/n())

#export table
write.table(tab, "D:/Documentos/Pidgin/out_dados2009.txt", sep="\t")
write.table(dados2009, "D:/Documentos/Pidgin/out_dados2009_thi.csv", sep=",", row.names=FALSE, col.names=FALSE)

hg log -v -r 30916 -r  30907 -r  38007 -r  32939 -r  33138 -r  33170 -r  27636 -r  32635 -r  34423 -r  32975 -r  34708 -r  35860 -r  28280 -r  31318 -r  36183 -r  34023
