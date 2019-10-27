
#preparando a base de dados#
library("dplyr")
library("tidyr")
library(readxl)

#abrindo as bases de dados# 
PEI<-read.csv("PEI_COD_CENSO.csv", sep=",") #base retirada da secretária de sp e cruzada por procv)#
#DISPONÍVEL EM: https://docs.google.com/spreadsheets/d/1aWbJ9kzSXYTVM8VvKpEglMNiytGIdYFnqGeb7DxQ-Ag/edit?usp=sharing)#
ETI<-read.csv("ETI_COD_CENSO.csv", sep=",")#base retirada da secretária de sp e cruzada por procv)#
#DISPONÍVEL EM : https://docs.google.com/spreadsheets/d/14qzNM6gb8e0RCxMpKhsxFMbDs3q5uP7V5YNg9XTh0N4/edit?usp=sharing#
CENSO2011<-read.csv("ESCOLAS 2011.csv", sep="|") #base do censo escolar#
CENSO2017<-read.csv("ESCOLAS 2017.csv", sep="|") #base do censo escolar#
Dicionario<-read_excel("Dicionário-2011.xlsx")


#Filtrando só escolas de são paulo#
CENSO2011sp<-filter(CENSO2011,CENSO2011$PK_COD_ENTIDADE>=35000000 & CENSO2011$PK_COD_ENTIDADE<36000000)
CENSO2011sp<-merge(PEI[,c("COD_CENSO","ano")],CENSO2011sp,by.x="COD_CENSO",by.y="PK_COD_ENTIDADE",all.y =TRUE)
CENSO2011sp<-mutate(CENSO2011sp,PEI=ifelse(is.na(CENSO2011sp$ano)==FALSE,"PEI","N"))

CENSO2017sp<-filter(CENSO2017,CENSO2017$CO_ENTIDADE>=35000000 & CENSO2017$CO_ENTIDADE<36000000)
CENSO2017sp<-merge(PEI[,c("COD_CENSO","ano")],CENSO2017sp,by.x="COD_CENSO",by.y="CO_ENTIDADE",all.y =TRUE)
CENSO2017sp<-mutate(CENSO2017sp,PEI=ifelse(is.na(CENSO2017sp$ano)==FALSE,"PEI","N"))

#Descritivas#
table(CENSO2011sp$PEI) #em 2011 faltam 28 escolas no censo# 
table(CENSO2017sp$PEI)  #em 2017 está completo#

CENSO2011sp %>%
  group_by(PEI) %>%
  #summarize(Média_Num_Salas_Uti= mean(NUM_SALAS_UTILIZADAS,na.rm=TRUE),Média_Num_Salas_Exi= mean(NUM_SALAS_EXISTENTES,na.rm=TRUE))
  x<-mean("NUM_SALAS_UTILIZADAS",na.rm=TRUE)

#Calculando médias condicionais#
y<-mean(CENSO2011sp[CENSO2011sp$PEI == "PEI", "NUM_SALAS_UTILIZADAS"],na.rm=TRUE)
x<-mean(CENSO2011sp[CENSO2011sp$PEI == "N", "NUM_SALAS_UTILIZADAS"],na.rm=TRUE)

#teste#
M<-matrix(colnames(CENSO2011sp))
Dicionario<-as.matrix(Dicionario)
vector<-c()
for(i in 1:120){
  for(j in 1:23){
    if(M[i,1]=Dicionario[j,1]{
    vector<-c(vector,i)
    }else{
      j=j+1
    }
  }
}
print(vector)


for(i in 1:10){
  X<-CENSO2011sp[i,]
  Y<-CENSO2011sp$PEI
  t.test(X~Y)
}

CENSO2011sp[,i]=Dicionario[j,1]

class(CENSO2011sp[,3])

lapply(CENSO2011sp[-1], function(x) t.test (x~CENSO2011sp$PEI))
M<-matrix(colnames(CENSO2011sp))
class(M)


#Fazer programa pegando todas variáveis de interesse e fazendo o ttest e fazer uma tabela descritiva#

