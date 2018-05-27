path<-"D:/Users/CLP124604/Desktop/Accenture" #diretorio onde se encontra as tabelas
dados<-read.csv(paste(path,"/","super_hero_powers.csv",sep=""),sep=",",h=T) #carregando a tabela super_heroes
dadosbayes<-dados #guardando a tabela super_heroes em outro nome

dados1<-read.csv(paste(path,"/","heroes_information.csv",sep=""),sep=",",h=T) #carregando a tabela heroes_information



a<-quantile(dados1$Height,prob=seq(0,1,by=0.2)) #quantil da em 10% da variável Height
a<-a[!duplicated(a)] #removendo valores duplicados se existir

a1<-quantile(na.omit(dados1$Weight),prob=seq(0,1,by=0.2)) #quantil da em 10% da variável Weight
a1<-a1[!duplicated(a1)] #removendo valores duplicados se existir

dados1$Weight<-as.character(cut(dados1$Weight,breaks=as.numeric(a1),include.lowest = TRUE,right=FALSE,dig.lab = 10)) #Gerando os intervalos 
dados1$Weight[which(is.na(dados1$Weight)==TRUE)]<-"-" # substituindo missing por "-"
dados1$Weight<-factor(dados1$Weight) #transformando a variável em factor

dados1$Height<-cut(dados1$Height,breaks=as.numeric(a),include.lowest = TRUE,right=FALSE,dig.lab = 10) #Gerando os intervalos 


### trasnformação de grão (variáveis da heroes_information
p1<-as.data.frame.matrix(table(dados1$name,dados1$Gender)/apply(table(dados1$name,dados1$Gender),1,sum)) 
p2<-as.data.frame.matrix(table(dados1$name,dados1$Eye.color)/apply(table(dados1$name,dados1$Eye.color),1,sum))
p3<-as.data.frame.matrix(table(dados1$name,dados1$Race)/apply(table(dados1$name,dados1$Race),1,sum))
p4<-as.data.frame.matrix(table(dados1$name,dados1$Hair.color)/apply(table(dados1$name,dados1$Hair.color),1,sum))
p5<-as.data.frame.matrix(table(dados1$name,dados1$Publisher)/apply(table(dados1$name,dados1$Publisher),1,sum))
p6<-as.data.frame.matrix(table(dados1$name,dados1$Skin.color)/apply(table(dados1$name,dados1$Skin.color),1,sum))
p7<-as.data.frame.matrix(table(dados1$name,dados1$Alignment)/apply(table(dados1$name,dados1$Alignment),1,sum))
p8<-as.data.frame.matrix(table(dados1$name,dados1$Weight)/apply(table(dados1$name,dados1$Weight),1,sum))
p9<-as.data.frame.matrix(table(dados1$name,dados1$Height)/apply(table(dados1$name,dados1$Height),1,sum))

colnames(p1)<-paste("Gender_",colnames(p1),sep="")
colnames(p2)<-paste("Eye.color_",colnames(p2),sep="")
colnames(p3)<-paste("Race_",colnames(p3),sep="")
colnames(p4)<-paste("Hair.color_",colnames(p4),sep="")
colnames(p5)<-paste("Publisher_",colnames(p5),sep="")
colnames(p6)<-paste("Skin.color_",colnames(p6),sep="")
colnames(p7)<-paste("Alignment_",colnames(p7),sep="")
colnames(p8)<-paste("Weight_",colnames(p8),sep="")
colnames(p9)<-paste("Height_",colnames(p9),sep="")

p1$name<-rownames(p1)
p2$name<-rownames(p2)
p3$name<-rownames(p3)
p4$name<-rownames(p4)
p5$name<-rownames(p5)
p6$name<-rownames(p6)
p7$name<-rownames(p7)
p8$name<-rownames(p8)
p9$name<-rownames(p9)

p1<-merge(p1,p2,by="name")
p1<-merge(p1,p3,by="name")
p1<-merge(p1,p4,by="name")
p1<-merge(p1,p5,by="name")
p1<-merge(p1,p6,by="name")
p1<-merge(p1,p7,by="name")
p1<-merge(p1,p8,by="name")
p1<-merge(p1,p9,by="name")
############# fim da transformação de grão

dadosp<-merge(dados,p1,by.x="hero_names",by.y="name",all.x=T) #left join da tabela super_heroes com a heroes_information transformada com grão unico 
dadosp<-as.data.frame(dadosp) 

#Etapa de pre-processamento de missing value em variaveis numericas e variavel com valor unico (sem variação) é removida
cont<-0
index<-c()
for(i in 1:ncol(dadosp))
{
if(sum(is.na(dadosp[,i])==TRUE)>0 & length(unique(dadosp[,i]))==2)
{
cont<-cont+1
index[cont]<-i
}

if(sum(is.na(dadosp[,i])==TRUE)>0)
{
dadosp[is.na(dadosp[,i])==TRUE,i]<-min(dadosp[,i],na.rm=T)
}

}


#removendo variáveis com valores unicos se existir
if(length(index)>0)
{
dadosp<-dadosp[,-index]

}
##### fim da remoção


dadosp<-model.matrix(~-1+.,data=dadosp[,-1]) #transformando a matrix final em matrix dummy

# remoção de variáveis dicotomicas com valores menores do que 30 em uma de suas classes
cont<-0
idx<-c()
for(i in 1:ncol(dadosp))
{
if(length(unique(dadosp[,i]))==2 & length(which(table(dadosp[,i])<30))>0)
{
cont<-cont+1
idx[cont]<-i
}

}
if(length(idx)>0)
{
dadosp<-dadosp[,-idx]
}
### fim da remoção (remover se existir)

## calculo da técnica Elbow simulando 15 clusters
wss <- (nrow(dadosp)-1)*sum(apply(dadosp,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(dadosp, 
  	centers=i)$withinss)	
#plotando os valores de Elbow dos 15 clusters
plot(1:15, wss, type="b", xlab="Number of Clusters",
  ylab="Within groups sum of squares")

fit <- kmeans(dadosp, 4) # kemeans com 4 clusters

aggregate(dadosp,by=list(fit$cluster),FUN=mean) #claculando a media dos 4 clusters ajustados

dados <- data.frame(dados,cluster= fit$cluster) #adicionando nos dados a variavel cluster


#########Naive Bayes

library(e1071) #carregando o pacote do modelo Naive bayes

dados1<-read.csv(paste(path,"/","heroes_information.csv",sep=""),sep=",",h=T) #carregando novamento os dados heroes_information
dadosbayest<-merge(dados1[,-1],dadosbayes,by.x="name",by.y="hero_names",all.x=T) #left join entre heroes_information e dados super_heroes

## pré_processamento das variaveis numericas e categoricas (missing values)
for(i in 1:ncol(dadosbayest))
{
if(is.numeric(dadosbayest[,i])==TRUE & sum(is.na(dadosbayest[,i])==TRUE)>0)
{
dadosbayest[which(is.na(dadosbayest[,i])==TRUE),i]<-min(dadosbayest[,i],na.rm=T)
}
if(is.numeric(dadosbayest[,i])!=TRUE & sum(is.na(dadosbayest[,i])==TRUE)>0)
{
dadosbayest[,i]<-as.character(dadosbayest[,i])
dadosbayest[which(is.na(dadosbayest[,i])==TRUE),i]<-"False"
dadosbayest[,i]<-factor(dadosbayest[,i])
}

}
##### fim do pré_processamento das variaveis numericas e categoricas (missing values)


dadosbayest<-dadosbayest[-which(dadosbayest$Alignment=="-"),] #removendo valores ausente na variável alvo
dadosbayest$Alignment<-as.character(dadosbayest$Alignment)
dadosbayest$Alignment<-factor(dadosbayest$Alignment) #transformando em factor a variável alvo

ind<-sample(1:nrow(dadosbayest),nrow(dadosbayest)*0.75,replace=F) # selecionando aleatoriamente o publico de treinamento


dados_train<-dadosbayest[ind,-1] #dados de treinamento
dados_test<-dadosbayest[-ind,-1] #dados de teste



Naive_Bayes_Model=naiveBayes(Alignment ~., data=dados_train, usekernel=TRUE) #aplicando o modelo naiveBayes

NB_Predictions=predict(Naive_Bayes_Model,dados_test) #estimando o publico de teste

table(NB_Predictions,dados_test$Alignment) # matriz de confusão entre alvo real e estimado do teste

sum(ifelse(NB_Predictions==dados_test$Alignment,1,0))/nrow(dados_test) # taxa de acerto geral
aggregate(ifelse(NB_Predictions==dados_test$Alignment,1,0),list(dados_test$Alignment),mean) #taxa de acerto por classe


## gerando oversampling da classe alvo para o modelo GBM
indexclass<-c(sample(which(dados_train$Alignment=="bad"),1.5*length(which(dados_train$Alignment=="good")),replace=T),
which(dados_train$Alignment=="good"),
sample(which(dados_train$Alignment=="neutral"),1.5*length(which(dados_train$Alignment=="good")),replace=T))

dados_train1<-dados_train[indexclass,] # dados de treinamento para o modelo GBM
library(gbm) #carregando o pacote do modelo GBM


gbm.model = gbm(Alignment~., data=dados_train1, shrinkage=0.001, distribution = 'multinomial', cv.folds=4, n.trees=12000, verbose=F,interaction.depth = 8) # modelo GBM
best.iter = gbm.perf(gbm.model, method="cv") #selecionando a iteração onde não ocorre overfitting de acordo com o croos validation
pred3 = predict(gbm.model, dados_test, na.action = best.iter,type="response") # estimando a base de teste com a melhor iteração
pred_class <- apply(pred3, 1, which.max) #selecionando a classe de teste com o maior escore estimado pelo modelo
pred_class<-ifelse(pred_class==1,"bad",ifelse(pred_class==2,"good","neutral")) #definindo a classe do alvo teste

table(pred_class,dados_test$Alignment) #matrix de confusão GBM

sum(ifelse(pred_class==dados_test$Alignment,1,0))/nrow(dados_test) #acerto geral
aggregate(ifelse(pred_class==dados_test$Alignment,1,0),list(dados_test$Alignment),mean) #acerto por classe alvo


########################### regression

indext<-which(dados_train$Weight==-99) #selecionando o indice dos registros sem alvo Weight na base de treinamento

library(gbm)

gbm.model = gbm(Weight~., data=dados_train[-indext,], shrinkage=0.001, distribution = 'gaussian', cv.folds=4, n.trees=5000, verbose=F,interaction.depth = 6) #aplicando o modelo GBM com regressão (gaussian)
best.iter = gbm.perf(gbm.model, method="cv") #selecionando a iteração onde não ocorre overfitting de acordo com o croos validation
pred3r = predict(gbm.model, dados_test, na.action = best.iter,type="response") # estimando a base de teste com a melhor iteração
indextt<-which(dados_test$Weight==-99) #selecionando o indice dos registros sem alvo Weight na base de teste
sqrt(sum((dados_test$Weight[-indextt]-pred3r[-indextt])^2)/length(dados_test$Weight[-indextt])) #calculo RMSE na base de teste 


library(e1071)

model <- svm( Weight~., data=dados_train[-indext,]) #aplicando o modelo SVM com regressão (Kernel=gaussian)

predictedY <- predict(model, newdata=dados_test) # estimando a base de teste

indextt<-which(dados_test$Weight==-99) #selecionando o indice dos registros sem alvo Weight na base de teste
sqrt(sum((dados_test$Weight[-indextt]-predictedY[-indextt])^2)/length(dados_test$Weight[-indextt])) #calculo RMSE na base de teste 


### tunando o modelo svm com a variação do suporte e da função custo, assim selecionando de forma iterativa o melhor SVM
tuneResult <- tune(svm, Weight~.,  data = dados_train[-indext,],
              ranges = list(epsilon = seq(0,1,0.1), cost = 2^(2:9))
)
tunedModel <- tuneResult$best.model #selecionando o melhor modelo SVM

predictedY <- predict(tunedModel, newdata=dados_test) # estimando a base de teste com a melhor modelo

sqrt(sum((dados_test$Weight[-indextt]-predictedY[-indextt])^2)/length(dados_test$Weight[-indextt])) #calculo RMSE na base de teste 

####### induct rules

library(CHAID) #carregando o pacote da técnica CHAID

dadosrule<-dadosbayest #criando os dados rules
dadosrule$Alignment<-factor(ifelse(dadosrule$Alignment %in% c("bad","neutral"),"bad","good")) # gerando alvo binario para identificar a melhor regra para a classe good, pode também ser feito para outras classes

a<-quantile(dadosrule$Height,prob=seq(0,1,by=0.2)) # quantile da variável Height
a<-c(a,15.2)
a<-a[!duplicated(a)]

a1<-quantile(na.omit(dadosrule$Weight),prob=seq(0,1,by=0.2)) # quantile da variável Weight
a1<-c(a1,2)
a1<-a1[!duplicated(a1)]

dadosrule$Weight<-as.character(cut(dadosrule$Weight,breaks=as.numeric(a1),include.lowest = TRUE,right=FALSE,dig.lab = 10)) #gerando os intervalos de acordo com os quantis
dadosrule$Weight[which(is.na(dadosrule$Weight)==TRUE)]<-"-"
dadosrule$Weight<-factor(dadosrule$Weight)

dadosrule$Height<-cut(dadosrule$Height,breaks=as.numeric(a),include.lowest = TRUE,right=FALSE,dig.lab = 10) #gerando os intervalos de acordo com os quantis



#aplicando a técnica chaid
dt.chaid  <- chaid(Alignment~Height+Weight+Eye.color+Race+Hair.color+Publisher+Agility+Accelerated.Healing+Durability+Stealth+Flight+Marksmanship
+Longevity+Intelligence+Super.Strength+Energy.Blasts+Stamina+Super.Speed+Invisibility+Reflexes+Invulnerability+Weapon.based.Powers , 
                   control = chaid_control(minprob = 0.0,
                                           minsplit = 200,minbucket = 40,maxheight=6),
                   data=dadosrule)
dt.chaid	#plotando a arvore			   
				   
	
			   
regras<-partykit:::.list.rules.party(dt.chaid) 		#gerando as regras 

attach(dadosrule) #attachando na memoria o nome das variaveis

indice1<-eval(parse(text=regras[4])) #gerando o indice do publico que cairam na regra

table(dadosbayest[indice1,]$Alignment) #contando a classe alvo da regra selecionada

mean(ifelse(dadosbayest[indice1,]$Alignmen=="good",1,0)) #calculando o percentual da classe good na regra selecionada