################################################################
#Montando os módulos  
#Mais informativos no último estágio
#Montando o modulo 3
Info1 <- Informacao

mod3.25 <- c()
mod3.50 <- c()
mod3.75 <- c()

mod3.25 <- Info1[order(Info1[,1], decreasing = T)[1:5],6]
mod3.50 <- Info1[order(Info1[,3], decreasing = T)[1:5],6]
mod3.75 <- Info1[order(Info1[,5], decreasing = T)[1:5],6]


i <- 5
j25 <- 6
j50 <- 6
while (length(unique(c(mod3.25,mod3.50))) < 8) {
  if(sum(mod3.25[i] == mod3.50) == 1){
    while(sum(Info1[order(Info1[,1], decreasing = T)[j25],6] == mod3.50) == 1){
      j25 <- j25 + 1
    }
    mod3.25[i] <- Info1[order(Info1[,1],decreasing = T)[j25],6]
    j25 <- j25 + 1
  }
  if(sum(mod3.50[i] == mod3.25) == 1){
    while(sum(Info1[order(Info1[,3],decreasing = T)[j50],6] == mod3.25) == 1){
      j50 <- j50 + 1
    }
    mod3.50[i] <- Info1[order(Info1[,3],decreasing = T)[j50],6]
    j50 <- j50 + 1
  }
  i <- i -1
}
rm(i); rm(j25); rm(j50)


i <- 5
j25 <- 6
j75 <- 6
while (length(unique(c(mod3.25,mod3.75))) < 8) {
  if(sum(mod3.25[i] == mod3.75) == 1){
    while(sum(Info1[order(Info1[,1], decreasing = T)[j25],6] == mod3.75) == 1){
      j25 <- j25 + 1
    }
    mod3.25[i] <- Info1[order(Info1[,1],decreasing = T)[j25],6]
    j25 <- j25 + 1
  }
  if(sum(mod3.75[i] == mod3.25) == 1){
    while(sum(Info1[order(Info1[,5],decreasing = T)[j75],6] == mod3.25) == 1){
      j75 <- j75 + 1
    }
    mod3.75[i] <- Info1[order(Info1[,5],decreasing = T)[j75],6]
    j75 <- j75 + 1
  }
  i <- i -1
}
rm(i); rm(j25); rm(j75)


i <- 5
j50 <- 6
j75 <- 6
while (length(unique(c(mod3.50,mod3.75))) < 8) {
  if(sum(mod3.50[i] == mod3.75) == 1){
    while(sum(Info1[order(Info1[,3], decreasing = T)[j50],6] == mod3.75) == 1){
      j50 <- j50 + 1
    }
    mod3.50[i] <- Info1[order(Info1[,3],decreasing = T)[j50],6]
    j50 <- j50 + 1
  }
  if(sum(mod3.75[i] == mod3.50) == 1){
    while(sum(Info1[order(Info1[,5],decreasing = T)[j75],6] == mod3.50) == 1){
      j75 <- j75 + 1
    }
    mod3.75[i] <- Info1[order(Info1[,5],decreasing = T)[j75],6]
    j75 <- j75 + 1
  }
  i <- i -1
}
rm(i); rm(j50); rm(j75)

Info1 <- Informacao[-unique(c(mod3.25,mod3.50,
                              mod3.75)),]


#Montando o modulo 2
mod2.33 <- c()
mod2.67 <- c()

mod2.33 <- Info1[order(Info1[,2], decreasing = T)[1:10],6]
mod2.67 <- Info1[order(Info1[,4], decreasing = T)[1:10],6]

length(unique(c(mod2.33,mod2.67)))

i <- 10
j33 <- 11
j67 <- 11
while (length(unique(c(mod2.33,mod2.67))) < 15) {
  if(sum(mod2.33[i] == mod2.67) == 1){
    while(sum(Info1[order(Info1[,2], decreasing = T)[j33],6] == mod2.67) == 1){
      j33 <- j33 + 1
    }
    mod2.33[i] <- Info1[order(Info1[,2],decreasing = T)[j33],6]
    j33 <- j33 + 1
  }
  if(sum(mod2.67[i] == mod2.33) == 1){
    while(sum(Info1[order(Info1[,4],decreasing = T)[j67],6] == mod2.33) == 1){
      j67 <- j67 + 1
    }
    mod2.67[i] <- Info1[order(Info1[,4],decreasing = T)[j67],6]
    j67 <- j67 + 1
  }
  i <- i -1
}
rm(i); rm(j33); rm(j67)

Info1 <- Informacao[-unique(c(mod3.25,mod3.50,
                              mod3.75, mod2.33, mod2.67)),]

#Montando o modulo 1
mod.1 <- c()
for (i in 1:5) {
  mod.1 <- c(mod.1,Info1[order(Info1[,i],decreasing = T)[1:2],6])
  Info1 <- Informacao[-unique(c(mod3.25,mod3.50,
                                 mod3.75, mod2.33, mod2.67, mod.1)),]
}


 

################################################################
#Gr?fico informa??es dos m?dulos
quant

Graficos.Modulos <- function(Estimado){

  
  theta <- seq(-7,7,.01)

  InfoTot <- rep(0,length(theta))
  for (i in mod.1) {
    InfoTot <- InfoTot +InfoItem(Estimado[i,1], Estimado[i,2], Estimado[i,3], theta)
  }

  InfoTot2 <- rep(0,length(theta))
  for (i in mod2.33) {
    InfoTot2 <- InfoTot2 +InfoItem(Estimado[i,1], Estimado[i,2], Estimado[i,3], theta)
  }

  InfoTot3 <- rep(0,length(theta))
  for (i in mod2.67) {
    InfoTot3 <- InfoTot3 +InfoItem(Estimado[i,1], Estimado[i,2], Estimado[i,3], theta)
  }

  InfoTot4 <- rep(0,length(theta))
  for (i in mod3.25) {
    InfoTot4 <- InfoTot4 +InfoItem(Estimado[i,1], Estimado[i,2], Estimado[i,3], theta)
  }

  InfoTot5 <- rep(0,length(theta))
  for (i in mod3.50) {
    InfoTot5 <- InfoTot5 +InfoItem(Estimado[i,1], Estimado[i,2], Estimado[i,3], theta)
  }

  InfoTot6 <- rep(0,length(theta))
  for (i in mod3.75) {
    InfoTot6 <- InfoTot6 +InfoItem(Estimado[i,1], Estimado[i,2], Estimado[i,3], theta)
  }

  
  g <- ggplot() + geom_line(mapping = aes(x = theta, y = InfoTot))+
  geom_line(mapping = aes(x = theta, y = InfoTot2),lty=2) + 
  geom_line(mapping = aes(x = theta, y = InfoTot3),lty=2) + 
  geom_line(mapping = aes(x = theta, y = InfoTot4),lty=3,col=2, alpha =1.1) +
  geom_line(mapping = aes(x = theta, y = InfoTot5),lty=3,col=5, alpha =1.1) + 
  geom_line(mapping = aes(x = theta, y = InfoTot6),lty=3,col=6, alpha =1.1)+
    theme_classic() + theme(plot.title = element_text(hjust = 0.5,family="mono"))+
    xlab('tra?o latente') + ylab('Informa??o')
  #ggsave("fig16.png", dpi = 300, plot = g)
  print(g)

  
  g <- ggplot() + 
  geom_line(mapping = aes(x = theta, y = InfoTot4),col=2) +
  geom_line(mapping = aes(x = theta, y = InfoTot5),col=3) + 
  geom_line(mapping = aes(x = theta, y = InfoTot6),col=4)+
    theme_classic() + theme(plot.title = element_text(hjust = 0.5,family="mono"))+
    xlab('tra?o latente') + ylab('Informa??o')
  #ggsave("fig15c.png", dpi = 300, plot = g)
  print(g)

  g <- ggplot() + 
  geom_line(mapping = aes(x = theta, y = InfoTot2),lty =1, size =1) +
  geom_line(mapping = aes(x = theta, y = InfoTot3),lty=1, size =1) +
    theme_classic() + theme(plot.title = element_text(hjust = 0.5,family="mono"))+
    xlab('tra?o latente') + ylab('Informa??o')
  #ggsave("fig15b.png",plot=g, dpi = 300)
  print(g)
  
  g <- ggplot() + 
    geom_line(mapping = aes(x = theta, y = InfoTot)) +ylab("InfoTot")+
    theme_classic() + theme(plot.title = element_text(hjust = 0.5,family="mono"))+
    xlab('tra?o latente') + ylab('Informa??o')
  print(g)
  #ggsave("fig15a.png", plot=g,dpi = 300)
###############################################  
}

Graficos.Modulos(EstimadoTRI)

################################################################
#Distribui??o dos par?metros dentro dos modulos

ggplot() +  geom_point(mapping = aes(x = rep(1,10),y = EstimadoTRI$d[mod2.33]),
                       col = 2) +
  geom_point(mapping = aes(x = rep(1.25,10),y = EstimadoTRI$d[mod2.67]),
             col = 3) +
  geom_point(mapping = aes(x = 1:5,y = EstimadoTRI$d[mod3.75]),
             col = 4)


Distr <- data.frame("Modulo" = c(rep("M?dulo 1",5),
                                 rep("M?dulo 2",5),
                                 rep("M?dulo 3",5)),
                    "Dificuldade" = c(EstimadoTRI$d[mod3.25],
                                      EstimadoTRI$d[mod3.50],
                                      EstimadoTRI$d[mod3.75]))

Distr1 <- data.frame("Modulo" = c(rep("M?dulo 1",10),
                                 rep("M?dulo 2",10)),
                    "Dificuldade" = c(EstimadoTRI$d[mod2.33],
                                      EstimadoTRI$d[mod2.67]))

ggplot() +  geom_point(Distr,mapping = aes(x = Modulo,
                                     y = Dificuldade),
                       col = 2)+ ggtitle("Est?gio 3")+
  ylim(0,3)+ ylab("Dificuldade(b)") + xlab("M?dulos")+
  theme_classic() + theme(plot.title = element_text(hjust = 0.5,family="mono"))
ggsave("fig17c.png", dpi = 300)

ggplot() +  geom_point(Distr1,mapping = aes(x = Modulo,
                                           y = Dificuldade),
                       col = 2)+ ggtitle("Est?gio 2")+
  ylim(0,3)+ ylab("Dificuldade(b)") + xlab("M?dulos")+
  theme_classic() + theme(plot.title = element_text(hjust = 0.5,family="mono"))
ggsave("fig17b.png", dpi = 300)


ggplot() +  geom_point(mapping = aes(x = rep("M?dulo 1",10),
                                           y = EstimadoTRI[mod.1,2]),
                       col = 2)+ ggtitle("Est?gio 1")+
  ylim(0,3)+ ylab("Dificuldade(b)") + xlab("M?dulos")+
  theme_classic() + theme(plot.title = element_text(hjust = 0.5,family="mono"))
ggsave("fig17a.png", dpi = 300)





