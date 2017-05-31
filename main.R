library(randomForest)
library(rpart)
library(PRROC)


qType="U" #Querying type :N,R,U,M,SR,SU,SM,SE,SRU,SRR,SRM,QFU

top=95 #top
q=5 #exploration budget
m=1000 #SSSL budget
files <- list.files(path = "./syntdf/") # list of files, each file contains transactions for a day
nDayTraining=7 # number of days for the sliding window
nTree=10 #number of trees trained per day
results=data.frame(day=NA,total=NA,auroc=NA,auprc=NA)
modelsList=list()
rankTopTrx=list()
bannedCards=c()
resT=NA
auroc=NA
auprc=NA

for (i in 1:length(files)){
  histTrx=read.csv(paste("./syntdf/",files[i],sep=""))
  print(files[i])
  if(!is.null(bannedCards) & length(is.null(which(histTrx$y %in% bannedCards)))==0){
    histTrx=histTrx[-which(histTrx$y %in% bannedCards),]
  }
  df=histTrx[,-which(names(histTrx) %in% c("CARD_ID"))]
  #fraud set
  dfFraud=df[which(df$y==1),]
  #genuine set
  dfGenuine=df[which(df$y==0),]
  modelsList[[i]]=list() 
  for(j in 1:nTree){
    #undersample genuine
    set=rbind(dfFraud, dfGenuine[sample(dim(dfGenuine)[1],dim(dfFraud)[1]),])
    modelsList[[i]][[j]] = rpart(formula = y~.,data = set, method="class")
  }
  #at the beginning of the streaming (untill day nDayTraining) we train on the whole 
  #daily set of transactions... afterwards we use only the queries
  if(i <= nDayTraining){
    dailyPred=c()
    for( k in 1:length(modelsList)){
      #get predictions from trees of day k
      pred=data.frame(matrix(unlist(lapply(modelsList[[k]],function(x)predict(object = x,newdata = df)[,2])),ncol = nTree))
      if(!is.null(dailyPred)){
        dailyPred=cbind(dailyPred,apply(pred,MARGIN = 1,FUN = mean))
      }else{
        dailyPred=apply(pred,MARGIN = 1,FUN = mean)
      }
    }
    if (i==1) predTot=dailyPred else predTot=apply(dailyPred,MARGIN = 1,FUN = mean)
  }else{ # create a model using only the queries
    totDF=rankTopTrx[[1]]
    for (v in 2:length(rankTopTrx)) totDF=rbind(totDF,rankTopTrx[[v]])
    totDF$y = factor(totDF$y) 
    rf=randomForest(y~.,totDF[,-c(1,2)])
    predTot=predict(rf,newdata = df,type = 'prob')[,2]
  }
  
  ordDel=order(predTot,decreasing = T)
  predTotOrd=histTrx[ordDel,]
  cards=head(unique(predTotOrd[,"CARD_ID"]),top)
  ranked=predTotOrd[which(!duplicated(predTotOrd[,"CARD_ID"]))[1:top],]
  rankedTrx=predTotOrd[which(predTotOrd[,"CARD_ID"] %in% cards),]
  
  queryds<-NULL
  queryds2<-NULL
  #Querying type
  {switch(qType, 
          N={ 
            resT=sum(ranked[,"y"]=="1",na.rm = T)/(top)
          },
          R={
            selectedCards=sample(unique(predTotOrd[,"CARD_ID"]),size = q)
            queryds<-predTotOrd[which(predTotOrd[,"CARD_ID"] %in% selectedCards),]
            resT=sum(rbind(ranked,queryds)[,"y"]=="1",na.rm = T)/(top+q)
          },
          U={ 
            selectedCards=head(unique(predTotOrd[order(abs(predTot - 0.5),decreasing = F),"CARD_ID"]),q)
            queryds<-predTotOrd[which(predTotOrd[,"CARD_ID"] %in% selectedCards),]
            resT=sum(rbind(ranked,queryds)[,"y"]=="1",na.rm = T)/(top+q)
          },
          M={ 
            unc=round(q/2)
            selectedCards1=sample(unique(predTotOrd[,"CARD_ID"]),size = unc)
            selectedCards2=head(unique(predTotOrd[order(abs(predTot - 0.5),decreasing = F),"CARD_ID"]),q-unc)
            queryds<-predTotOrd[which(predTotOrd[,"CARD_ID"] %in% unique(selectedCards1,selectedCards2)),]
            resT=sum(rbind(ranked,queryds)[,"y"]=="1",na.rm = T)/(top+q)
          },
          SR={ 
            selectedCards=head(unique(predTotOrd[,"CARD_ID"]),m)
            queryds2<-predTotOrd[which(predTotOrd[,"CARD_ID"] %in% selectedCards),]
            queryds2$y="0"
            resT=sum(ranked[,"y"]=="1",na.rm = T)/top
          },
          SU={ 
            selectedCards=head(unique(predTotOrd[order(abs(predTot - 0.5),decreasing = F),"CARD_ID"]),m)
            queryds2<-predTotOrd[which(predTotOrd[,"CARD_ID"] %in% selectedCards),]
            queryds2$y="0"
            resT=sum(ranked[,"y"]=="1",na.rm = T)/top
          },
          SM={ 
            selectedCards1=sample(unique(predTotOrd[,"CARD_ID"]),size = 300)
            selectedCards2=head(unique(predTotOrd[order(abs(predTot - 0.5),decreasing = F),"CARD_ID"]),700)
            queryds2<-predTotOrd[which(predTotOrd[,"CARD_ID"] %in% unique(selectedCards1,selectedCards2)),]
            queryds2$y="0"
            resT=sum(ranked[,"y"]=="1",na.rm = T)/top
          },
          SE={ 
            selectedCards=head(unique(predTotOrd[order(predTot,decreasing = F),"CARD_ID"]),m)
            queryds2<-predTotOrd[which(predTotOrd[,"CARD_ID"] %in% selectedCards),]
            queryds2$y="0"
            resT=sum(ranked[,"y"]=="1",na.rm = T)/top
          },
          SRU={ 
            selectedCards=head(unique(predTotOrd[order(abs(predTot - 0.5),decreasing = F),"CARD_ID"]),q)
            queryds<-predTotOrd[which(predTotOrd[,"CARD_ID"] %in% selectedCards),]
            resT=sum(rbind(ranked,queryds)[,"y"]=="1",na.rm = T)/(top+q)
            selectedCards=head(unique(predTotOrd[,"CARD_ID"]),m)
            queryds2<-predTotOrd[which(predTotOrd[,"CARD_ID"] %in% selectedCards),]
            queryds2$y="0"
            resT=sum(ranked[,"y"]=="1",na.rm = T)/top
          },
          SRR={ 
            selectedCards=sample(unique(predTotOrd[,"CARD_ID"]),size = q)
            queryds<-predTotOrd[which(predTotOrd[,"CARD_ID"] %in% selectedCards),]
            resT=sum(rbind(ranked,queryds)[,"y"]=="1",na.rm = T)/(top+q)
            selectedCards=head(unique(predTotOrd[,"CARD_ID"]),m)
            queryds2<-predTotOrd[which(predTotOrd[,"CARD_ID"] %in% selectedCards),]
            queryds2$y="0"
            resT=sum(ranked[,"y"]=="1",na.rm = T)/top
          },
          SRM={ 
            unc=round(q/2)
            selectedCards1=sample(unique(predTotOrd[,"CARD_ID"]),size = unc)
            selectedCards2=head(unique(predTotOrd[order(abs(predTot - 0.5),decreasing = F),"CARD_ID"]),q-unc)
            queryds<-predTotOrd[which(predTotOrd[,"CARD_ID"] %in% unique(selectedCards1,selectedCards2)),]
            resT=sum(rbind(ranked,queryds)[,"y"]=="1",na.rm = T)/(top+q)
            selectedCards=head(unique(predTotOrd[,"CARD_ID"]),m)
            queryds2<-predTotOrd[which(predTotOrd[,"CARD_ID"] %in% selectedCards),]
            queryds2$y="0"
            resT=sum(ranked[,"y"]=="1",na.rm = T)/top
          },
          QFU={
            selectedtr=predTotOrd[which(abs(predTot - 0.5)<0.05),"CARD_ID"]
            tabc=table(selectedtr)
            selectedCards=names(tabc[head(order(tabc,decreasing = T),q)])
            queryds<-predTotOrd[which(predTotOrd[,"CARD_ID"] %in% selectedCards),]
            resT=sum(rbind(ranked,queryds)[,"y"]=="1",na.rm = T)/(top+q)
          }
          
  )}
  
  subsetAlerts=rbind(rankedTrx,queryds) #update of alerted transactions
  rankTopTrx[[i]]=rbind(subsetAlerts,queryds2) #update of labeled transactions
  cat("Total: ")
  print(resT)
  remCards=unique(subsetAlerts[which(subsetAlerts$y==1),'CARD_ID'])
  bannedCards=c(bannedCards,as.character(levels(remCards))[remCards]) #update of banned cards
  auroc=as.numeric(roc.curve(scores.class0 = predTot,weights.class0 = ifelse(as.character(df$y)=="1",1,0))[2])
  auprc=as.numeric(pr.curve(scores.class0 = predTot,weights.class0 = ifelse(as.character(df$y)=="1",1,0))[2])
    
  results=rbind(results,c(i,resT,auroc,auprc)) #update metrics for day i
  if(i>nDayTraining)  rankTopTrx[[i-nDayTraining ]] = NULL #slide the windows of random forests

}

write.csv(results[-1,], file = "results.csv")

