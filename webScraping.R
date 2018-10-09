##test web scraping
library(rvest)
library(pbapply)

url <- 'https://www.feedipedia.org/content/feeds?category=15967'
#url <- 'https://www.feedipedia.org/content/feeds?category=13587&name=&common-name=&species=All&synonym=&status=All'
webpage <- read_html(url)
webSession <- html_session(url)
getNodes <- html_nodes(webpage, 'a')
getText <- html_text(getNodes)

strtSpp <- "African baobab (Adansonia digitata)"
endSpp <- "Yellow yam (Dioscorea cayenensis)"
sppText <- getText[which(getText %in% strtSpp):which(getText %in% endSpp)]
forbiddenAccess <- c("Wheat shorts, wheat middlings and feed flour")
sppText <- sppText[-c(which(sppText %in% forbiddenAccess))]

##########################################################################
##########################################################################
dataFromTable <- function(cropTx, cropType, tab){
  #################
  #cropTx <- link
  #cropType <- typeCrMeasure[1]
  #tab <- cropTabs[[indValTab[1]]]
  #################
  
  rowNames <- tab$X1
  dryRow <- which(rowNames %in% "Dry matter")
  proteinRow <- which(rowNames %in% "Crude protein")
  phosRow <- which(rowNames %in% "Phosphorus")
  dataRows <- c(dryRow, proteinRow, phosRow)
  
  if(dataRows>0){
    formatOutTab <- rbind.data.frame(tab[dataRows,])
    colnames(formatOutTab) <- c("Measure", "Unit", "Avg", "SD", "Min", "Max", "Nb", "blank")
    formatOutTab$Crop <- cropTx
    formatOutTab$CropVariable <- cropType
    ##reorganize table for output
    reorgTab <- formatOutTab[c("Crop", "Version", "Measure", "Unit", "Avg", "SD", "Min", "Max", "Nb")]
    
    return(reorgTab)
  }
}
##########################################################################
cyclePages <- function(link, sess){
  #################
  #link <- sppText[16]
  #sess <- webSession
  #################
  
  ##navigate to crop page
  sppPage <- read_html(follow_link(sess, link))
  ##get the various type of crop values
  typeCrMeasure <- html_text(html_nodes(sppPage, 'h3 a'))
  
  if(length(typeCrMeasure)>0){
    ##get the crop tables
    cropTabs <- html_table(sppPage, fill=T)
    ##identify the ones with nuitrition values
    valTabs <- sapply(1:length(cropTabs), function(i){if(is.na(cropTabs[[i]][1,1])==F & cropTabs[[i]][1,1]=="Main analysis"){return(TRUE)}else{return(FALSE)}})
    indValTab <- which(valTabs==T)
    
    getData <- lapply(indValTab, function(i){dataFromTable(link, typeCrMeasure[i-(min(indValTab)-1)], cropTabs[[i]])})
    oneOutput <- do.call(rbind, getData)
    
    return(oneOutput)
  }
}
##########################################################################
##########################################################################


scrapData <- pblapply(sppText, cyclePages, sess=webSession)
fullTable <- do.call(rbind, scrapData)

write.csv(fullTable, "D:/mlisk/junk/foodipedia_scrapedData.csv", row.names=F)








