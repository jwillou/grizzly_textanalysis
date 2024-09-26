library(stm)
library(tm)
library(SnowballC)
library(Rtsne)
library(rsvd) 
library(geometry)
library(scales)

setwd("/Users/jannawilloughby/Google Drive/My Drive/Willoughby lab/projects - active/grizzly_text/grizzly_textanalysis/")
#setwd("/Users/jrw0107/Google Drive/My Drive/Willoughby lab/projects - active/grizzly_text/grizzly_textanalysis/")
load.image(file="grizzlyanalysis.RData")

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
#data = fulldata[fulldata$type!="NOTRELEVANT" & fulldata$Condensed_gov_actors!="State Judicial" & fulldata$Condensed_gov_actors!="State Legislature" ,]
monthdays = data.frame(month=1:12, days=c(31,28,31,30,31,30,31,31,30,31,30,31), cdays=c(0,31,59,90,120,151,181,212,243,273,304,334))

data$Seqdate = rep(NA, nrow(data))
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
  data$Seqdate[r] = ((as.numeric(data$Year[r])-1) * 365) + as.numeric(monthdays$cdays[monthdays$month==data$Month[r]]) + as.numeric(data$Day[r])
  month = as.character(data$Month[r])
  if(length(strsplit(month,"")[[1]])==1){month=paste("0",month,sep="")}
  day = data$Day[r]
  if(length(strsplit(day,"")[[1]])==1){day=paste("0",day,sep="")}
  data$SeqdateC[r] = paste(data$Year[r], month, day, sep="")
}
data$SeqdateC = as.numeric(data$SeqdateC)

#set variables and dataset 
lowerthreshold = 10#c(10,20) #words that do not appear in at least this many documents will be removed

#set up and clean analysis data
data$Seqdate = data$Seqdate - min(data$Seqdate)
redata = NULL
for(c in 14:26){
  t = data.frame(docs = data[,c], level = data$Condensed_function, date = data$Seqdate)
  redata = rbind(redata, t)
}
redata = as.data.frame(redata)
redata$docs[redata$docs==""] = NA
redata$level[redata$level==""] = NA
redata$date[redata$date==""] = NA
redata = redata[complete.cases(redata),]
metadata = redata[,2:3]
metadata = as.data.frame(metadata)

#analyze data to identify topics
text   = textProcessor(documents = redata$docs, metadata = metadata, removestopwords=T, customstopwords=c("get","like","one","re","said","say","says","will","grizzly","grizzlies","bear","bears"))
output = prepDocuments(text$documents, text$vocab, text$meta, lower.thresh = lowerthreshold) #should this be higher?
output.stm = stm(documents = output$documents, vocab = output$vocab,  prevalence =~ level, K = 5, max.em.its = 10000, data = output$meta, init.type = "Spectral", seed = 2112)
#summary(output.stm)[[1]]
output.sel = selectModel(output$documents, output$vocab, prevalence =~ level, K = 5, max.em.its = 10000,  data = output$meta, init.type = "Spectral", runs = 20, seed = 2112)
summary(output.stm)
save.image(file="grizzlyanalysis.RData")


#regressions and plots by political actor (level)
sink(paste("output/output_", lowerthreshold, "_combined.txt", sep=""), append=F)
print(text)
print(table(t[,2]))
print(summary(output.stm))
actors = c("Executive", "Judicial", "Legislature", "Indegenous", "Public", "NGO", "Journalist")
colors7 = c("dodgerblue3", "darkorchid4", "firebrick3", "darkorange3", "goldenrod2", "chartreuse4", "navyblue")
for(i in 1:5){
  
  topic = paste(summary(output.stm)[[1]][i,], collapse=" ")
  level.eff  = estimateEffect(c(i) ~ (level)-1, output.stm, metadata = output$meta, uncertainty = "None")
  print(topic)
  print(summary(level.eff))
  
  #make nice looking means plot
  pdf(file=paste("output/output_", lowerthreshold, "_topic", i, "_combined.pdf", sep=""), width=5, height=5, onefile=T)
  reg = summary(level.eff)$tables[[1]]
  reg = as.data.frame(reg)
  reg$cats = rep(0, nrow(reg))
  for(r in 1:nrow(reg)){
    reg$cats[r] = strsplit(rownames(reg)[r], split="level")[[1]][2]
  }
  ytop = 0.6 #upper ylim
  plot(-100,-100, xlim=c(0,(nrow(reg)+0.5)), ylim=c(0,ytop), xlab="political actor", ylab="coefficient estimate", axes=F, main=topic)
  axis(1,at=1:7, labels=actors, pos=0)
  axis(2,at=seq(0,ytop,0.1), labels=seq(0,ytop,0.1), pos=0)
  segments(x0=0,x1=7.5,y0=0,y1=0)
  segments(x0=0,x1=7.5,y0=ytop,y1=ytop)
  segments(x0=7.5,x1=7.5,y0=0,y1=ytop)
  for(a in 1:length(actors)){
    polygon(x=c((a-0.3), (a+0.3), (a+0.3), (a-0.3)), y=c(0,0,reg$Estimate[reg$cats==actors[a]],reg$Estimate[reg$cats==actors[a]]), col=colors7[a], border=T)
    segments(x0=(a-0.1), x1=(a+0.1), y0=(reg$Estimate[reg$cats==actors[a]]-(reg$`Std. Error`[reg$cats==actors[a]]*1.96)), y1=(reg$Estimate[reg$cats==actors[a]]-(reg$`Std. Error`[reg$cats==actors[a]]*1.96)), lwd=2)
    segments(x0=(a-0.1), x1=(a+0.1), y0=(reg$Estimate[reg$cats==actors[a]]+(reg$`Std. Error`[reg$cats==actors[a]]*1.96)), y1=(reg$Estimate[reg$cats==actors[a]]+(reg$`Std. Error`[reg$cats==actors[a]]*1.96)), lwd=2)
    segments(x0=a, x1=a, y0=(reg$Estimate[reg$cats==actors[a]]-(reg$`Std. Error`[reg$cats==actors[a]]*1.96)), y1=(reg$Estimate[reg$cats==actors[a]]+(reg$`Std. Error`[reg$cats==actors[a]]*1.96)), lwd=2)
  }
  #plot(level.eff, "level", model=gadarianFit, method="pointestimate", main=paste("topic ", i, topic))
  dev.off()
  
  #make nice looking change over time plot
  pdf(file=paste("output/output_", lowerthreshold, "_topicchange", i, "_combined.pdf", sep=""), width=5, height=5, onefile=T)
  
  #plot parameters, different for each topic over time figure
  ytops    = c( 0.3, 0.4, 0.8, 0.2, 0.3)
  ybottoms = c(-0.2,-0.3,-0.2,-0.3, 0.0) 
  par(bg=NA)
  plot(-100,-100, xlim=c(0,17000), ylim=c(ybottoms[i],ytops[i]), xlab="date", ylab="change in topic use", axes=F, main=topic)
  axis(1,at=c(0,3650,7300,10950,14600), labels=c(1981,1991,2001,2011,2021), pos=(ybottoms[i]))
  axis(2,at=seq(ybottoms[i],ytops[i],0.1), labels=seq(ybottoms[i],ytops[i],0.1), pos=-500)
  segments(x0=-500,x1=17000,y0=(ybottoms[i]),y1=(ybottoms[i]))
  segments(x0=-500,x1=17000,y0=ytops[i],y1=ytops[i])
  segments(x0=17000,x1=17000,y0=(ybottoms[i]),y1=ytops[i])
  
  date.eff  = estimateEffect(c(i) ~ level*date-1-date, output.stm, metadata = output$meta, uncertainty = "None")
  print(summary(date.eff))
  treg = summary(date.eff)$tables[[1]]
  treg = as.data.frame(treg)
  treg$cats = rep(0, nrow(treg))
  for(r in 1:nrow(treg)){
    treg$cats[r] = strsplit(rownames(treg)[r], split="level")[[1]][2]
  }
  eff = estimateEffect(i ~ level*date-1-date, stmobj = output.stm, metadata = output$meta, uncertainty = "None")
  #treg$cats[is.na(treg$cats)][2] = "Executive:date"
  #treg$cats[is.na(treg$cats)][1] = "Executive"
  treg$upper = treg$Estimate + 1.96 * treg$`Std. Error`
  treg$lower = treg$Estimate - 1.96 * treg$`Std. Error`
  
  for(a in 1:length(actors)){
    if(treg$`Pr(>|t|)`[treg$cats==paste(actors[a],":date", sep="")]<0.05){
      llty=1
      llwd=2
    }else{
      next
      llty=2
      llwd=1
    }
    means.eff = eff[[1]][[1]][[1]]$est
    m=means.eff[(a+7)]
    b=means.eff[a]
    minx=min(redata$date[redata$level==actors[a]])
    maxx=max(redata$date[redata$level==actors[a]])
    x_seq = seq(minx, maxx, length.out = 100)
    y_fit = m*x_seq+b
    y_upper = (treg$upper[a + 7] * x_seq) + treg$upper[a]
    y_lower = (treg$lower[a + 7] * x_seq) + treg$lower[a]
    polygon(c(x_seq, rev(x_seq)), c(y_upper, rev(y_lower)), col = alpha(colors7[a], 0.2), border = NA)
    segments(x0=minx, y0=((m*minx)+b), x1=maxx, y1=((m*maxx)+b), lwd=llwd, lty=llty, col=colors7[a])
  }
  dev.off()
}
sink()

pdf(file=paste("output/output_", lowerthreshold, "_topicchangedates_combined.pdf", sep=""), width=5, height=5, onefile=T)
plot(-100,-100, xlim=c(0,17000), ylim=c(ybottoms[i],ytops[i]), xlab="date", ylab="change in topic use", axes=F, main=topic)
axis(1,at=c(0,3650,7300,10950,14600), labels=c(1981,1991,2001,2011,2021), pos=(ybottoms[i]))
axis(2,at=seq(ybottoms[i],ytops[i],0.1), labels=seq(ybottoms[i],ytops[i],0.1), pos=-500)
segments(x0=-500,x1=17000,y0=(ybottoms[i]),y1=(ybottoms[i]))
segments(x0=-500,x1=17000,y0=ytops[i],y1=ytops[i])
segments(x0=17000,x1=17000,y0=(ybottoms[i]),y1=ytops[i])
abline(v=9855)
abline(v=13505)
dev.off()
