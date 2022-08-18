#################################################################
#Bibliotecas
library(dplyr)
library(mirt)
library(ggplot2)
library(CTT)
library(mirtCAT)
library(gridExtra)


# Funções utilizadas
Info.sobreposta <- function(Estimativas){
  
  #Gráfico das informações dos itens, todas sobrepostas.
  r <- rainbow(45)
  theta <- seq(-7,7,.01)
  InfoItSep <- matrix(0,nrow = 45, ncol = length(theta))
  for (i in 1:45) {
    InfoItSep[i,] <- InfoItem(Estimativas[i,1],
                              Estimativas[i,2],
                              Estimativas[i,3],
                              theta)
  }
  g1 <- ggplot()  +  geom_line(mapping = aes(theta,InfoItSep[1,]), col = r[1], alpha = 0.7) +
    geom_line(mapping = aes(theta,InfoItSep[2,]), col = r[2], alpha = 0.7) +
    geom_line(mapping = aes(theta,InfoItSep[3,]), col = r[3], alpha = 0.7) +
    geom_line(mapping = aes(theta,InfoItSep[4,]), col = r[4], alpha = 0.7) +
    geom_line(mapping = aes(theta,InfoItSep[5,]), col = r[5], alpha = 0.7) +
    geom_line(mapping = aes(theta,InfoItSep[6,]), col = r[6], alpha = 0.7) +
    geom_line(mapping = aes(theta,InfoItSep[7,]), col = r[7], alpha = 0.7) +
    geom_line(mapping = aes(theta,InfoItSep[8,]), col = r[8], alpha = 0.7) +
    geom_line(mapping = aes(theta,InfoItSep[9,]), col = r[15], alpha = 0.7) +
    geom_line(mapping = aes(theta,InfoItSep[10,]), col = r[16], alpha = 0.7) +
    geom_line(mapping = aes(theta,InfoItSep[11,]), col = r[11], alpha = 0.7) +
    geom_line(mapping = aes(theta,InfoItSep[12,]), col = r[12], alpha = 0.7) +
    geom_line(mapping = aes(theta,InfoItSep[13,]), col = r[13], alpha = 0.7) +
    geom_line(mapping = aes(theta,InfoItSep[14,]), col = r[14], alpha = 0.7) +
    geom_line(mapping = aes(theta,InfoItSep[15,]), col = r[15], alpha = 0.7) +
    geom_line(mapping = aes(theta,InfoItSep[16,]), col = r[16], alpha = 0.7) +
    geom_line(mapping = aes(theta,InfoItSep[17,]), col = r[17], alpha = 0.7) +
    geom_line(mapping = aes(theta,InfoItSep[18,]), col = r[18], alpha = 0.7) +
    geom_line(mapping = aes(theta,InfoItSep[19,]), col = r[19], alpha = 0.7) +
    geom_line(mapping = aes(theta,InfoItSep[20,]), col = r[20], alpha = 0.7) +
    geom_line(mapping = aes(theta,InfoItSep[21,]), col = r[21], alpha = 0.7) +
    geom_line(mapping = aes(theta,InfoItSep[22,]), col = r[22], alpha = 0.7) +
    geom_line(mapping = aes(theta,InfoItSep[23,]), col = r[23], alpha = 0.7) +
    geom_line(mapping = aes(theta,InfoItSep[24,]), col = r[24], alpha = 0.7) +
    geom_line(mapping = aes(theta,InfoItSep[25,]), col = r[25], alpha = 0.7) +
    geom_line(mapping = aes(theta,InfoItSep[26,]), col = r[26], alpha = 0.7) +
    geom_line(mapping = aes(theta,InfoItSep[27,]), col = r[27], alpha = 0.7) +
    geom_line(mapping = aes(theta,InfoItSep[28,]), col = r[28], alpha = 0.7) +
    geom_line(mapping = aes(theta,InfoItSep[29,]), col = r[29], alpha = 0.7) +
    geom_line(mapping = aes(theta,InfoItSep[30,]), col = r[30], alpha = 0.7) +
    geom_line(mapping = aes(theta,InfoItSep[31,]), col = r[31], alpha = 0.7) +
    geom_line(mapping = aes(theta,InfoItSep[32,]), col = r[32], alpha = 0.7) +
    geom_line(mapping = aes(theta,InfoItSep[33,]), col = r[33], alpha = 0.7) +
    geom_line(mapping = aes(theta,InfoItSep[34,]), col = r[34], alpha = 0.7) +
    geom_line(mapping = aes(theta,InfoItSep[35,]), col = r[35], alpha = 0.7) +
    geom_line(mapping = aes(theta,InfoItSep[36,]), col = r[36], alpha = 0.7) +
    geom_line(mapping = aes(theta,InfoItSep[37,]), col = r[37], alpha = 0.7) +
    geom_line(mapping = aes(theta,InfoItSep[38,]), col = r[38], alpha = 0.7) +
    geom_line(mapping = aes(theta,InfoItSep[39,]), col = r[39], alpha = 0.7) +
    geom_line(mapping = aes(theta,InfoItSep[40,]), col = r[40], alpha = 0.7) +
    geom_line(mapping = aes(theta,InfoItSep[41,]), col = r[41], alpha = 0.7) +
    geom_line(mapping = aes(theta,InfoItSep[42,]), col = r[42], alpha = 0.7) +
    geom_line(mapping = aes(theta,InfoItSep[43,]), col = r[43], alpha = 0.7) +
    geom_line(mapping = aes(theta,InfoItSep[44,]), col = r[44], alpha = 0.7) +
    geom_line(mapping = aes(theta,InfoItSep[45,]), col = r[45], alpha = 0.7) +
    ylab("Informa??o") + xlab("Tra?o Latente") +
    theme_classic() + theme(plot.title = element_text(hjust = 0.5,family="mono"))
  
  return(g1)
}


Extrair.Theta <- function(Estimado){
  
  theta <- seq(-7,7,.01)
  thetas0.5 <- c()
  thetaMInf <- c()
  for (i in 1:45) {
    fim <- TRI(Estimado[i,1],Estimado[i,2],Estimado[i,3],theta)
    fim <- round(fim,2)
    x <- which(fim == 0.5)  
    thetas0.5[i] <- theta[x[1]]
    
    InfoItSep <- InfoItem(Estimado[i,1],Estimado[i,2],
                          Estimado[i,3],theta)
    thetaMInf[i] <- theta[which.max(InfoItSep)]
  }
  thetaMInf <- ifelse(thetaMInf == 7,NA,thetaMInf)
  thetaMInf <- ifelse(thetaMInf == -7,NA,thetaMInf)
  return(list("theta0.5" = thetas0.5, "thetaMInf" = thetaMInf))
}


Extrair.Respostas <- function(am){
  
  #Função para transformar as colunas com as respostas do aluno
  #em certo e errado
  teste <- strsplit(am[[2]],"")
  df1 <- data.frame(matrix(unlist(teste), nrow=nrow(am),
                           byrow=T),stringsAsFactors=FALSE)
  
  teste <- strsplit(am[[3]],"")
  df2 <- data.frame(matrix(unlist(teste), nrow=nrow(am),
                           byrow=T),stringsAsFactors=FALSE)
  
  resp <- df1==df2
  resp <- resp*1
  return(resp)
}


ordenar <- function(Area){
  
  #Função para ordenar as respostas de todas as provas em um mesmo padrão
  itens_2019 <- data.table::fread(input='itens_prova_2019.csv',integer64='character')
  itens_2019 <- itens_2019 %>% filter(CO_PROVA < 530)
  
  Provas <- itens_2019 %>% filter(SG_AREA == Area)
  
  
  Provas$SG_AREA <- NULL
  Provas$TX_GABARITO <- NULL
  Provas$CO_HABILIDADE <- NULL
  
  
  Provas <- Provas[Provas$TX_COR != "Laranja", ]
  Provas <- Provas[Provas$TX_COR != "Verde", ]
  
  Provas <- Provas %>% group_by(CO_PROVA) %>% arrange(CO_ITEM,
                                                      .by_group = T)
  
  Co_Prova <- c(Provas$CO_PROVA[1],Provas$CO_PROVA[46],Provas$CO_PROVA[91],Provas$CO_PROVA[136])
  ordem <- data.frame("X1" = Provas[1:45,]$CO_POSICAO,
                      "X2" = Provas[46:90,]$CO_POSICAO,
                      "X3" = Provas[91:135,]$CO_POSICAO,
                      "X4" = Provas[136:180,]$CO_POSICAO,
                      "Co_item" = Provas$CO_ITEM[1:45],
                      "ord" = 1:45)
  
  ordem <- ordem - min(ordem$X1) +1
  ordem$Ordem <- c(1:45)
  return(list("ordem" = ordem,"Co_Prova" = Co_Prova))
}


Limpar.Colunas <- function(Area,enem_2019){
  
  #Função para pegar as colunas que nos interessam, remover os NAs,
  #e remover as provas que não utilizamos
  Prova_Codigos <- paste("CO_PROVA_",Area,sep="")
  Respostas_Prova <- paste("TX_RESPOSTAS_",Area,sep="")
  Gabarito_Prova <- paste("TX_GABARITO_",Area,sep="")
  Nota_disciplina <- paste("NU_NOTA_",Area,sep="")
  
  y <- c(Prova_Codigos, Respostas_Prova, Gabarito_Prova,Nota_disciplina)
  enem_resp<-enem_2019[,..y]
  enem_resp <- na.omit(enem_resp)
  
  #Excluir maiores de 530(Reaplicação) e excluir laranja e verde
  y <- !(enem_resp[,1] > 518) 
  enem_resp <- enem_resp[y[,1],]
  return(enem_resp)
  
}


Retornar.Respostas <- function(enem_resp, ordem){
  #Retorna as respostas de todos os alunos ordenadas de acordo com o padrão
  Respostas <- c()
  j <- 1
  for (i in ordem$Co_Prova) {
    y <- enem_resp[,1] == i 
    X <- enem_resp[y[,1],]
    
    Resp1 <- Extrair.Respostas(X)
    Resp1 <- Resp1[,ordem$ordem[,j]]
    j <- j +1
    Respostas <- rbind(Respostas,Resp1)
  }  
  return(Respostas)
}

TRI <- function(a,b,c,theta){
  return(c + (1 - c)/(1 + exp(-a*(theta - b))))
}

InfoItem <- function(a,b,c,theta){
  Info <- a^2 * (1 - TRI(a,b,c,theta))/(TRI(a,b,c,theta)) * 
    ((TRI(a,b,c,theta) - c)/(1 - c))^2
  return(Info)
}

Retornar.Estimados <- function(Modelo){
  Estimado <- matrix(0,nrow = 45,ncol = 3)
  for (k in 1:45) {
    Estimado[k,1] <- coef(Modelo)[[k]][[1]]
    Estimado[k,2] <- coef(Modelo)[[k]][[2]]
    Estimado[k,3] <- coef(Modelo)[[k]][[3]]
  }
  
  return(Estimado)
}

#################################################################
#Ler o Enem, e gerar as estima??es usando EM
enem_2019 <- data.table::fread(input='microdados_enem_2019.csv',
                               integer64='character',
                               skip=0,  #Ler do inicio
                               na.strings = "",
                               showProgress = TRUE)



ordem <- ordenar('MT')
enem_resp <- Limpar.Colunas('MT',enem_2019)

# Primeira amostra utilizada
set.seed(1031)
amostra <- sample(1:nrow(enem_resp), 50000)
amostra <- enem_resp[amostra,]
amostra <- as.data.frame(Retornar.Respostas(amostra,ordem))

#rm(ordem);rm(enem_resp)

colnames(amostra) <- paste("X", 1:45,sep = "")
Modelo <- mirt(amostra,model = 1,
               itemtype = '3PL')


#Plots do modelo
g <- plot(Modelo, type = 'trace', main = 'CCI Matem?tica')
print(plot(Modelo, type = 'info',  main = 'Informa??o do Teste'))

ggsave("CCIs.png", g, dpi = 600)

Estimado <- Retornar.Estimados(Modelo)

Estimado <- data.frame("a1" = round(Estimado[,1],2),
                        "d" = round(Estimado[,2],2),
                        "g" = round(Estimado[,3],2))


#Pegar os itens ruins
Itens <- itemAnalysis(amostra)
Itens$itemReport
rm(amostra)
#write.table(round(Itens$itemReport[3],2),"Matematica",row.names = F)


#################################################################
#An?lise da distribui?ao dos parametros
EstimadoTRI <- Estimado
EstimadoTRI$d <- -EstimadoTRI$d/EstimadoTRI$a1   
#write.table(round(EstimadoTRI$g,2),"Matematica",row.names = F,col.names = F)



g1 <- ggplot() + geom_density(mapping = aes(EstimadoTRI$a1))+
  ylab("Densidade") + xlab("Discrimina??o(a)") + 
  theme_bw() + theme(plot.title = element_text(hjust = 0.5,family="mono"))

g2 <- ggplot() + geom_density(mapping = aes(EstimadoTRI$d))+
  ylab("Densidade") + xlab("Dificuldade(b)") + 
  theme_bw() + theme(plot.title = element_text(hjust = 0.5,family="mono"))

g3 <- ggplot() + geom_density(mapping = aes(EstimadoTRI$g))+
  ylab("Densidade") + xlab("Acerto ao acaso(c)") + 
  theme_bw() + theme(plot.title = element_text(hjust = 0.5,family="mono"))



g <- grid.arrange(g1,g2,g3)
ggsave("fig11.png",g, dpi = 600)


print(Info.sobreposta(EstimadoTRI))

ggsave("fig12.png", dpi = 600)


#################################################################
#Separar modulos

ThetasFinal <- Extrair.Theta(EstimadoTRI)



quant <- c(quantile(ThetasFinal$thetaMInf, .25,na.rm = T),
           quantile(ThetasFinal$thetaMInf, .33,na.rm = T),
           quantile(ThetasFinal$thetaMInf, .50,na.rm = T),
           quantile(ThetasFinal$thetaMInf, .67,na.rm = T),
           quantile(ThetasFinal$thetaMInf, .75,na.rm = T))


Informacao <- data.frame("a1"=InfoItem(EstimadoTRI[,1], EstimadoTRI[,2], EstimadoTRI[,3], quant[1]),
                         "a2" = InfoItem(EstimadoTRI[,1], EstimadoTRI[,2], EstimadoTRI[,3], quant[2]),
                         "a3" = InfoItem(EstimadoTRI[,1], EstimadoTRI[,2], EstimadoTRI[,3], quant[3]),
                         "a4" = InfoItem(EstimadoTRI[,1], EstimadoTRI[,2], EstimadoTRI[,3], quant[4]),
                         "a5" = InfoItem(EstimadoTRI[,1], EstimadoTRI[,2], EstimadoTRI[,3], quant[5]),
                         "row" = 1:45)

#Ir para o programa modulos

#Ir para o programa Exaustivo


ggplot() + geom_point(mapping = aes(x = 1:45, y = Itens$itemReport$itemMean))+ 
  xlab("Número Questão")+ylab("ID")+
  theme_classic() +   theme(plot.title = element_text(hjust = 0.5,family="mono"))
ggsave("fig18.png", dpi = 300)


