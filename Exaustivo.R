library(mirtCAT)


InfoMod <- function(modulo,EstimadoTr, Thet){
  
  info <- rep(0, length(Thet)) 
  for (i in modulo) {
    info <- info + InfoItem(EstimadoTr[i,1], EstimadoTr[i,2],
                            EstimadoTr[i,3], Thet)
  }
  return(round(info,1))  
}



# Amostra 2
set.seed(3110)
amostra <- sample(1:nrow(enem_resp), 10000)
amostra <- enem_resp[amostra,]


ResultEnem <- c()
for (i in ordem$Co_Prova) {
  y <- amostra[,1] == i 
  X <- amostra[y[,1],]
  ResultEnem <- c(ResultEnem,X$NU_NOTA_MT)
}  

amostra <- as.data.frame(Retornar.Respostas(amostra,ordem))




ResultTotal <- fscores(generate.mirt_object(parameters = Estimado, 
                                            itemtype = '3PL'), 
                       response.pattern = amostra,
                       method = "EAP")


ggplot()+  geom_density(mapping = aes(ResultTotal[,'F1'])) +
  ylab("Densidade") + xlab("Tra?o Latente")+
  theme_classic() + theme(plot.title = element_text(hjust = 0.5,family="mono"))
ggsave("fig19.png", dpi = 300)


sd(ResultTotal[,'F1'])


ResultMod1 <- fscores(generate.mirt_object(parameters = Estimado[mod.1,], 
                                           itemtype = '3PL'),
                      response.pattern = amostra[,mod.1])


ErroAtual <- 1000

max(ResultMod1[,'F1'])


for (i in seq(min(ResultMod1[,'F1']),max(ResultMod1[,'F1'])-0.1,.1)) {
  print(i)
  AlunosMod2.33 <- which(ResultMod1[,'F1'] <= i)
  AlunosMod2.67 <- which(ResultMod1[,'F1'] > i)

  
  Result1.33 <- fscores(generate.mirt_object(parameters = Estimado[c(mod.1, mod2.33),], 
                                             itemtype = '3PL'),
                        response.pattern = amostra[AlunosMod2.33,c(mod.1,mod2.33)])
  
  
  Result1.67 <- fscores(generate.mirt_object(parameters = Estimado[c(mod.1, mod2.67),],
                                             itemtype = '3PL'),
                        response.pattern = amostra[AlunosMod2.67,c(mod.1,mod2.67)])  
  

  for (j in seq(min(Result1.33[,'F1']),max(Result1.33[,'F1'])-0.1,.1)) {
    for (k in seq(min(Result1.67[,'F1']),max(Result1.67[,'F1'])-0.1,.1)) {
      
      AlunosMod3.33.25 <- which(Result1.33[,'F1'] <= j)
      
      AlunosMod3.33.50 <- which(Result1.33[,'F1'] > j)
      
      AlunosMod3.67.50 <- which(Result1.67[,'F1'] <= k)
      
      AlunosMod3.67.75 <- which(Result1.67[,'F1'] > k)
      
      
      AlunosMod3.33.50 <- AlunosMod2.33[AlunosMod3.33.50]
      
      AlunosMod3.33.25 <- AlunosMod2.33[AlunosMod3.33.25]
      
      AlunosMod3.67.50 <- AlunosMod2.67[AlunosMod3.67.50]
      
      AlunosMod3.67.75 <- AlunosMod2.67[AlunosMod3.67.75]
      
      
      ResultCaminho1 <- fscores(generate.mirt_object(parameters = Estimado[c(mod.1, mod2.33,mod3.25),],
                                                     itemtype = '3PL'),
                                response.pattern = amostra[AlunosMod3.33.25,c(mod.1,mod2.33,mod3.25)])

      ResultCaminho3 <- fscores(generate.mirt_object(parameters = Estimado[c(mod.1, mod2.33,mod3.50),],
                                                     itemtype = '3PL'),
                                response.pattern = amostra[AlunosMod3.33.50,c(mod.1,mod2.33,mod3.50)])
      
      ResultCaminho4 <- fscores(generate.mirt_object(parameters = Estimado[c(mod.1, mod2.67,mod3.50),],
                                                     itemtype = '3PL'),
                                response.pattern = amostra[AlunosMod3.67.50,c(mod.1,mod2.67,mod3.50)])
      
      ResultCaminho6 <- fscores(generate.mirt_object(parameters = Estimado[c(mod.1, mod2.67,mod3.75),],
                                                     itemtype = '3PL'),
                                response.pattern = amostra[AlunosMod3.67.75,c(mod.1,mod2.67,mod3.75)])
      
      ResultMST <- rbind(ResultCaminho1,ResultCaminho3,ResultCaminho4,
                        ResultCaminho6)
      
      Teste <- ResultTotal[c(AlunosMod3.33.25,AlunosMod3.33.50,
                             AlunosMod3.67.50,AlunosMod3.67.75),]
      
      Erro <- mean((ResultMST[,'F1'] - Teste[,'F1'])^2)
      print("Erro: ")
      print(Erro)
      if(ErroAtual > Erro){
        ErroAtual <- Erro
        Roteamento1 <- i
        Roteamento2 <- j
        Roteamento3 <- k
      }
      
    }
  }
}


#Encontramos Rot1 = 1.77
#Rot2 = 1.38
#Rot3 = 2.17

Roteamento1 <- 1.77
Roteamento2 <- 1.38
Roteamento3 <- 2.17

AlunosMod2.33 <- which(ResultMod1[,'F1'] <= Roteamento1)
AlunosMod2.67 <- which(ResultMod1[,'F1'] > Roteamento1)

Result1.33 <- fscores(generate.mirt_object(parameters = Estimado[c(mod.1, mod2.33),], 
                                           itemtype = '3PL'),
                      response.pattern = amostra[AlunosMod2.33,c(mod.1,mod2.33)])


Result1.67 <- fscores(generate.mirt_object(parameters = Estimado[c(mod.1, mod2.67),],
                                           itemtype = '3PL'),
                      response.pattern = amostra[AlunosMod2.67,c(mod.1,mod2.67)])  

AlunosMod3.33.25 <- which(Result1.33[,'F1'] <= Roteamento2)

AlunosMod3.33.50 <- which(Result1.33[,'F1'] > Roteamento2)

AlunosMod3.67.50 <- which(Result1.67[,'F1'] <= Roteamento3)

AlunosMod3.67.75 <- which(Result1.67[,'F1'] > Roteamento3)


AlunosMod3.33.50 <- AlunosMod2.33[AlunosMod3.33.50]

AlunosMod3.33.25 <- AlunosMod2.33[AlunosMod3.33.25]

AlunosMod3.67.50 <- AlunosMod2.67[AlunosMod3.67.50]

AlunosMod3.67.75 <- AlunosMod2.67[AlunosMod3.67.75]


ResultCaminho1 <- fscores(generate.mirt_object(parameters = Estimado[c(mod.1, mod2.33,mod3.25),],
                                               itemtype = '3PL'),
                          response.pattern = amostra[AlunosMod3.33.25,c(mod.1,mod2.33,mod3.25)])

ResultCaminho3 <- fscores(generate.mirt_object(parameters = Estimado[c(mod.1, mod2.33,mod3.50),],
                                               itemtype = '3PL'),
                          response.pattern = amostra[AlunosMod3.33.50,c(mod.1,mod2.33,mod3.50)])

ResultCaminho4 <- fscores(generate.mirt_object(parameters = Estimado[c(mod.1, mod2.67,mod3.50),],
                                               itemtype = '3PL'),
                          response.pattern = amostra[AlunosMod3.67.50,c(mod.1,mod2.67,mod3.50)])

ResultCaminho6 <- fscores(generate.mirt_object(parameters = Estimado[c(mod.1, mod2.67,mod3.75),],
                                               itemtype = '3PL'),
                          response.pattern = amostra[AlunosMod3.67.75,c(mod.1,mod2.67,mod3.75)])

ResultMST <- rbind(ResultCaminho1,ResultCaminho3,ResultCaminho4,
                   ResultCaminho6)

Teste <- ResultTotal[c(AlunosMod3.33.25,AlunosMod3.33.50,
                       AlunosMod3.67.50,AlunosMod3.67.75),]

Erro <- mean((ResultMST[,'F1'] - Teste[,'F1'])^2)
sqrt(Erro)

ggplot()+  geom_density(mapping = aes(Teste[,'F1'])) +
  geom_segment(mapping = aes(x = 0.55, xend = 0.55,
                             y = 0, yend = 0.51)) +
  ylab("Densidade") + xlab("Tra?o Latente")+
  theme_classic() + theme(plot.title = element_text(hjust = 0.5,family="mono"))
ggsave("fig20.png", dpi = 300)



ggplot()+  geom_density(mapping = aes(ResultMST[,'F1'])) +
#  geom_segment(mapping = aes(x = -1.14, xend = -1.14, y = 0, yend = 0.22)) +
  ylab("Densidade") + xlab("Tra?o Latente")+
  theme_classic() + theme(plot.title = element_text(hjust = 0.5,family="mono"))
ggsave("fig21.png", dpi = 300)

summary(ResultMST[,'F1'])
sd(ResultMST[,'F1'])
mean(ResultMST[,'SE_F1'])

summary(Teste[,'F1'])
sd(Teste[,'F1'])
mean(Teste[,'SE_F1'])


cor(ResultMST[,'F1'],Teste[,'F1'], method = "pearson")
cor(ResultMST[,'F1'],Teste[,'F1'], method = "spearman")



ggplot() + geom_point(mapping = aes(x = Teste[,'F1'],
                                    y = ResultMST[,'F1']),col = 'blue') +
  geom_smooth(mapping = aes(x = Teste[,'F1'],
                            y = ResultMST[,'F1']),method = "lm", col = 'green') + 
  ylab("Resultado Prova TAM") + xlab("Resultado Prova Completa") + 
  theme_classic() + theme(plot.title = element_text(hjust = 0.5,family="mono"))
ggsave("fig23.png", dpi = 300)



ggplot() + geom_point(mapping = aes(x = Teste[,'F1'], y = ResultMST[,'F1'] - Teste[,'F1']),
                      col = 'blue') +
  geom_segment(mapping = aes(x = min(Teste[,'F1']), xend = 3.31, y = 0, yend = 0),
               col = rainbow(1),lty=2, size = 1.05) +
  ylab("TAM - Completa") + xlab("Resultado Prova TAM") + 
  theme_classic() + theme(plot.title = element_text(hjust = 0.5,family="mono"))

ggsave("fig22.png", dpi = 300)


# Amostra 3
set.seed(3107)
amostra <- sample(1:nrow(enem_resp), 20000)
amostra <- enem_resp[amostra,]

amostra <- as.data.frame(Retornar.Respostas(amostra,ordem))

ResultTotal <- fscores(generate.mirt_object(parameters = Estimado, 
                                            itemtype = '3PL'), 
                       response.pattern = amostra,
                       method = "EAP")

quantile(ResultTotal[,'F1'])

amostra <- amostra[ResultTotal[,'F1'] > 0.55,]


ResultTotal <- fscores(generate.mirt_object(parameters = Estimado, 
                                            itemtype = '3PL'), 
                       response.pattern = amostra,
                       method = "EAP")

ResultMod1 <- fscores(generate.mirt_object(parameters = Estimado[mod.1,], 
                                           itemtype = '3PL'),
                      response.pattern = amostra[,mod.1])


