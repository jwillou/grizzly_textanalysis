library(stm)
library(tm)
library(SnowballC)
library(Rtsne)
library(rsvd) 
library(geometry)

setwd("/Users/jannawilloughby/Google Drive/My Drive/Willoughby lab/projects - active/grizzly_text/grizzly_textanalysis/")
setwd("/Users/jrw0107/Google Drive/My Drive/Willoughby lab/projects - active/grizzly_text/grizzly_textanalysis/")

####clean and set up data####
#simplified headers
#empty cells with .
#removed non printing characters in .xlsx with clean()
#replaced , ' " (three types) with blank
#checked dates and removed incomplete and corrected a few with typos (sent for checking and approval)
#replaced 3 instances of plublic with Public to match the other category
#replaced other typos as well, note state groups have few records

#correct data formats, etc
fulldata = read.table("Wyoming Grizzly bear documentsV3.csv", sep=",", header=T, comment.char = "")
data = fulldata[fulldata$type!="NOTRELEVANT",]
monthdays = data.frame(month=1:12, days=c(31,28,31,30,31,30,31,31,30,31,30,31), cdays=c(0,31,59,90,120,151,181,212,243,273,304,334))

for(r in 1:nrow(data)){
  if(is.na(data$Pub_date[r])){next}
  t = strsplit(data$Pub_date[r], split="/")[[1]][3]
  tt = t
  if(t<=24&!is.na(t)){
    tt = paste("20",t,sep="")
  }
  if(t>24&!is.na(t)){
    tt = paste("19",t,sep="")
  }
  data$Year[r]  = tt
  data$Month[r] = strsplit(data$Pub_date[r], split="/")[[1]][1]
  data$Day[r]   = strsplit(data$Pub_date[r], split="/")[[1]][2]
  data$Seqdate[r] = ((as.numeric(data$Year[r])-1)*365) + monthdays$cdays[monthdays$month==as.numeric(data$Month[r])] + as.numeric(data$Day[r])
}
data$Seqdate = data$Seqdate - min(data$Seqdate, na.rm=T)




#set variables
lowerthreshold = c(5,10,20) #words that do not appear in at least this many documents will be removed

####find topics for ideas in focal columns (separately)####
for(c in 12:24){ #these are columns of interest
  for(l in 1:length(lowerthreshold)){
    documents = data.frame(docs = data[,c])
    metadata  = data.frame(level = data$Condensed_gov_actors)
    
    #remove missing data rows
    t = cbind(documents, metadata)
    t = t[t[,1]!="" & t[,2]!="",]
    if(nrow(t)<=l){next}
    documents = data.frame(docs = t[,1])
    metadata  = data.frame(level = t[,2])
    
    #analyze data with levels
    text   = textProcessor(documents = documents$docs, metadata = metadata, customstopwords=c("said", "will", "'re"))
    output = prepDocuments(text$documents, text$vocab, text$meta, lower.thresh = lowerthreshold[l]) #should this be higher?
    if(length(output$vocab)<=5){next}
    output.stm = stm(documents = output$documents, vocab = output$vocab,  prevalence =~ level, K = 5, max.em.its = 10000, data = output$meta, init.type = "Spectral", seed = 2112)
    #summary(output.stm)[[1]]
    
    output.sel = selectModel(output$documents, output$vocab, prevalence =~ level, K = 5, max.em.its = 10000,  data = output$meta, init.type = "Spectral", runs = 20, seed = 2112)
    #plotModels(output.sel, pch=c(1,2,3,4), legend.position="bottomright")
    
    #regressions and plots (output relates topic prevalence in each level)
    sink(paste("output/output_", lowerthreshold[l], "_",colnames(data)[c], ".txt", sep=""), append=F)
    pdf(file=paste("output/output_", lowerthreshold[l], "_",colnames(data)[c], ".pdf", sep=""), width=7, height=7, onefile=T)
    
    print(summary(output.stm))
    for( i in 1:5){
      topic = paste(summary(output.stm)[[1]][i,], collapse=" ")
      level.eff  = estimateEffect(c(i) ~ (level)-1, output.stm, metadata = output$meta, uncertainty = "None")
      print(topic)
      print(summary(level.eff))
      plot(level.eff, "level", model=gadarianFit, method="pointestimate", main=paste("topic ", i, topic))
      print("#########")
    }
    sink()
    dev.off()
  }
}  





####find topic change over time in focal columns (separately)####
for(c in 12:24){ #these are columns of interest
  for(l in 1:length(lowerthreshold)){
    documents = data.frame(docs = data[,c])
    metadata  = data.frame(level = data$Condensed_gov_actors)
    datadata  = data.frame(date = data$Pub_date)
    
    #remove missing data rows
    t = cbind(documents, metadata)
    t = t[t[,1]!="" & t[,2]!="",]
    if(nrow(t)<=l){next}
    documents = data.frame(docs = t[,1])
    metadata  = data.frame(level = t[,2])
    
    #analyze data with levels
    text   = textProcessor(documents = documents$docs, metadata = metadata, customstopwords=c("said", "will", "'re"))
    output = prepDocuments(text$documents, text$vocab, text$meta, lower.thresh = lowerthreshold[l]) #should this be higher?
    if(length(output$vocab)<=5){next}
    output.stm = stm(documents = output$documents, vocab = output$vocab,  prevalence =~ level, K = 5, max.em.its = 10000, data = output$meta, init.type = "Spectral", seed = 2112)
    #summary(output.stm)[[1]]
    
    output.sel = selectModel(output$documents, output$vocab, prevalence =~ level, K = 5, max.em.its = 10000,  data = output$meta, init.type = "Spectral", runs = 20, seed = 2112)
    #plotModels(output.sel, pch=c(1,2,3,4), legend.position="bottomright")
    
    #regressions and plots (output relates topic prevalence in each level)
    sink(paste("output/output_", lowerthreshold[l], "_",colnames(data)[c], ".txt", sep=""), append=F)
    pdf(file=paste("output/output_", lowerthreshold[l], "_",colnames(data)[c], ".pdf", sep=""), width=7, height=7, onefile=T)
    
    print(summary(output.stm))
    for( i in 1:5){
      topic = paste(summary(output.stm)[[1]][i,], collapse=" ")
      level.eff  = estimateEffect(c(i) ~ (level), output.stm, metadata = output$meta, uncertainty = "None")
      date.eff   = estimateEffect(c(i) ~ (level), output.stm, metadata = output$meta, uncertainty = "None")
      print(topic)
      print(summary(level.eff))
      plot(level.eff, "level", model=gadarianFit, method="pointestimate", main=paste("topic ", i, topic))
      print("#########")
    }
    sink()
    dev.off()
  }
}  



