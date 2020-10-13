#******  Instalar e carregar pacotes necessários *********************

install.packages("ggplot2")
install.packages("gridExtra")
library(gridExtra)
library(ggplot2)

# ***** Função estabiliza_freq ***************************************


estabiliza_freq <- function(prob,exp){
  
        if(exp == 1){
    
        N_vec     <- c(6,10 ,20,50,100,1000)
        freq_cara <- freq_coroa <- matrix(0,6,2)
    
              for(i in 1:6){
                  random_coin     <- numeric(N_vec[i])
                  random_coin     <- rbinom(N_vec[i], 1, prob)
                  freq_coroa[i,]  <- c(sum(random_coin), mean(random_coin))
                  freq_cara[i,]   <- c(N_vec[i]-sum(random_coin), 1- mean(random_coin) ) 
              
              }
    

    
    
    
    cat("\n \n") 
    cat("******************************************************************\n")
    cat("         ESTABILIZAÇÃO DAS FREQUÊNCIAS RELATIVAS                 \n ")
    cat("                   - LANÇAMENTO MOEDA -                           \n ")
    cat("******************************************************************\n")
      
            for (i in 1:6){
                  cat("\n ") 
                  cat("                 -----------\n")
                  cat("                  N = ", N_vec[i]) 
                  cat("                             \n ")
                  cat("                 -----------\n")
                  
                  tab_freq           <- rbind(freq_coroa[i,] ,freq_cara[i,]  )
                  colnames(tab_freq) <- c("Ocorrências","Freq. relativa")
                  rownames(tab_freq) <- c("K","C")
                  
                  cat("----------------------------------------------------------------\n")
                  print(tab_freq)
                  cat("----------------------------------------------------------------\n")
      
            }
    
    
    
# Gráfico de barras -  Frequência lançamento moeda 
    
    
    
    i    <- 1
    data <- data.frame(face=c("C","K"), value=c(freq_cara[i,2],freq_coroa[i,2]))
    g1   <- ggplot(data, aes(x=face, y=value,fill=face)) +  geom_bar(stat = "identity") + xlab("") + ylab("Frequência relativa") +
            ggtitle(paste("N =",N_vec[i])) + ylim(0,1)+  
            theme_bw() + geom_hline(yintercept=prob, colour="red")
    
    
    
    i    <- 2
    data <- data.frame(face=c("C","K"), value=c(freq_cara[i,2],freq_coroa[i,2]))
    g2   <- ggplot(data, aes(x=face, y=value,fill=face)) +  geom_bar(stat = "identity") + xlab("") + ylab("Frequência relativa") +
            ggtitle(paste("N =",N_vec[i])) + ylim(0,1)+  
            theme_bw() + geom_hline(yintercept=prob, colour="red")
    
    
    i    <- 3
    data <- data.frame(face=c("C","K"), value=c(freq_cara[i,2],freq_coroa[i,2]))
    g3   <- ggplot(data, aes(x=face, y=value,fill=face)) +  geom_bar(stat = "identity") + xlab("") + ylab("Frequência relativa") +
            ggtitle(paste("N =",N_vec[i])) + ylim(0,1)+  
            theme_bw() + geom_hline(yintercept=prob, colour="red")
   
    
    
    i    <- 4
    data <- data.frame(face=c("C","K"), value=c(freq_cara[i,2],freq_coroa[i,2]))
    g4   <- ggplot(data, aes(x=face, y=value,fill=face)) +  geom_bar(stat = "identity") + xlab("") + ylab("Frequência relativa") +
            ggtitle(paste("N =",N_vec[i])) + ylim(0,1)+  
            theme_bw() + geom_hline(yintercept=prob, colour="red")
    
    
    i    <- 5
    data <- data.frame(face=c("C","K"), value=c(freq_cara[i,2],freq_coroa[i,2]))
    g5   <- ggplot(data, aes(x=face, y=value,fill=face)) +  geom_bar(stat = "identity") + xlab("") + ylab("Frequência relativa") +
            ggtitle(paste("N =",N_vec[i])) + ylim(0,1)+  
            theme_bw() + geom_hline(yintercept=prob, colour="red")
    
    
    i    <- 6
    data <- data.frame(face=c("C","K"), value=c(freq_cara[i,2],freq_coroa[i,2]))
    g6   <- ggplot(data, aes(x=face, y=value,fill=face)) +  geom_bar(stat = "identity") + xlab("") + ylab("Frequência relativa") +
            ggtitle(paste("N =",N_vec[i])) + ylim(0,1)+  
            theme_bw() + geom_hline(yintercept=prob, colour="red")
    
    grid.arrange(g1,g2,g3,g4,g5,g6)
    
  
  }
  
  
  if(exp == 2){
    
    N_vec     <- c(6,10 ,20,50,100,1000)
    freq_1    <- freq_2 <- freq_3 <- freq_4 <- freq_5 <- freq_6 <- matrix(0,6,2)
    
    
    
          for(i in 1:6){
                random_dado    <- numeric(N_vec[i])
                random_dado    <- sample(c(1,2,3,4,5,6),N_vec[i],replace=T,c(prob,1/6,1/6,1/6,1/6,1/6))
                freq_1[i,] <- c(length(random_dado[random_dado==1]),length(random_dado[random_dado==1])/N_vec[i])
                freq_2[i,] <- c(length(random_dado[random_dado==2]),length(random_dado[random_dado==2])/N_vec[i])
                freq_3[i,] <- c(length(random_dado[random_dado==3]),length(random_dado[random_dado==3])/N_vec[i])
                freq_4[i,] <- c(length(random_dado[random_dado==4]),length(random_dado[random_dado==4])/N_vec[i])
                freq_5[i,] <- c(length(random_dado[random_dado==5]),length(random_dado[random_dado==5])/N_vec[i])
                freq_6[i,] <- c(length(random_dado[random_dado==6]),length(random_dado[random_dado==6])/N_vec[i])
      
          }
    
    

    
    cat("\n \n") 
    cat("******************************************************************\n")
    cat("         ESTABILIZAÇÃO DAS FREQUÊNCIAS RELATIVAS                 \n ")
    cat("                   - LANÇAMENTO DADO -                           \n ")
    cat("******************************************************************\n")
    
    
    
          for (i in 1:6){
               cat("\n ") 
               cat("                 -----------\n")
               cat("                  N = ", N_vec[i]) 
               cat("                             \n ")
               cat("                 -----------\n")
               
               tab_freq     <- rbind(freq_1[i,] ,freq_2[i,],freq_3[i,],freq_4[i,],freq_5[i,],freq_6[i,]  )
               colnames(tab_freq) <- c("Número de ocorrências","Freq. relativa")
               rownames(tab_freq) <- c("1","2","3","4","5","6")
               
               cat("\n ")
               cat("----------------------------------------------------------------\n")
               print(tab_freq)
               cat("----------------------------------------------------------------\n")
           
          }
    
    
    ### Plots   
    
    i    <- 1
    data <- data.frame( face=c("1","2","3","4","5","6"), value=c(freq_1[i,2] ,freq_2[i,2],
            freq_3[i,2],freq_4[i,2],freq_5[i,2],freq_6[i,2]))
    g1   <- ggplot(data, aes(x=face, y=value,fill=face)) +  geom_bar(stat = "identity") +
            xlab("") + ylab("Frequência relativa") +  ggtitle(paste("N =",N_vec[i])) + ylim(0,1)+  
            theme_bw() + geom_hline(yintercept=prob, colour="red")
    
    
    i    <- 2
    data <- data.frame( face=c("1","2","3","4","5","6"), value=c(freq_1[i,2] ,freq_2[i,2],
                                                                 freq_3[i,2],freq_4[i,2],freq_5[i,2],freq_6[i,2]))
    g2   <- ggplot(data, aes(x=face, y=value,fill=face)) +  geom_bar(stat = "identity") +
      xlab("") + ylab("Frequência relativa") +  ggtitle(paste("N =",N_vec[i])) + ylim(0,1)+  
      theme_bw() + geom_hline(yintercept=prob, colour="red")
    
    
    i    <- 3
    data <- data.frame( face=c("1","2","3","4","5","6"), value=c(freq_1[i,2] ,freq_2[i,2],
                                                                 freq_3[i,2],freq_4[i,2],freq_5[i,2],freq_6[i,2]))
    g3   <- ggplot(data, aes(x=face, y=value,fill=face)) +  geom_bar(stat = "identity") +
      xlab("") + ylab("Frequência relativa") +  ggtitle(paste("N =",N_vec[i])) + ylim(0,1)+  
      theme_bw() + geom_hline(yintercept=prob, colour="red")
    
    i    <- 4
    data <- data.frame( face=c("1","2","3","4","5","6"), value=c(freq_1[i,2] ,freq_2[i,2],
                                                                 freq_3[i,2],freq_4[i,2],freq_5[i,2],freq_6[i,2]))
    g4   <- ggplot(data, aes(x=face, y=value,fill=face)) +  geom_bar(stat = "identity") +
      xlab("") + ylab("Frequência relativa") +  ggtitle(paste("N =",N_vec[i])) + ylim(0,1)+  
      theme_bw() + geom_hline(yintercept=prob, colour="red")
    
    i    <- 5
    data <- data.frame( face=c("1","2","3","4","5","6"), value=c(freq_1[i,2] ,freq_2[i,2],
                                                                 freq_3[i,2],freq_4[i,2],freq_5[i,2],freq_6[i,2]))
    g5   <- ggplot(data, aes(x=face, y=value,fill=face)) +  geom_bar(stat = "identity") +
      xlab("") + ylab("Frequência relativa") +  ggtitle(paste("N =",N_vec[i])) + ylim(0,1)+  
      theme_bw() + geom_hline(yintercept=prob, colour="red")
    
    i    <- 6
    data <- data.frame( face=c("1","2","3","4","5","6"), value=c(freq_1[i,2] ,freq_2[i,2],
                                                                 freq_3[i,2],freq_4[i,2],freq_5[i,2],freq_6[i,2]))
    g6   <- ggplot(data, aes(x=face, y=value,fill=face)) +  geom_bar(stat = "identity") +
      xlab("") + ylab("Frequência relativa") +  ggtitle(paste("N =",N_vec[i])) + ylim(0,1)+  
      theme_bw() + geom_hline(yintercept=prob, colour="red")
    grid.arrange(g1,g2,g3,g4,g5,g6)
    
    
  }

  
}

#############################################################################################################################
