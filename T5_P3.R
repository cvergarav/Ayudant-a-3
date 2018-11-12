#pregunta 3 tarea5
library(ggplot2)
library (gridExtra)

set.seed(123)
reps=10000 #repeticiones
betas=matrix(NA,nrow=reps,ncol=8)
beta0=2
beta1=2.5
beta2=1
n=c(50,100,500,1000) #Tamaño Muestral


###--------Caso1--------------------
for (j in 1:length(n)) {
  
  for (i in 1:reps) {
    
    X1 = rnorm(n[j],20,1)
    E = rnorm(n[j],0,1)
    u = rnorm(n[j],0,1)
    X2 = 0.8*X1 + E
    Y = beta0 + beta1*X1 + beta2*X2 + u
    
    model1 = lm(Y~X1)
    betas[i,j]=model1$coef[[1]]
    betas[i,j+4]=model1$coef[[2]]
    
  }
}

betas_df=data.frame(betas)

# pregunta A

#Esperanzas
E_b0_50 = mean(betas_df$X1)
E_b0_100 = mean(betas_df$X2)
E_b0_500 = mean(betas_df$X3)
E_b0_1000 = mean(betas_df$X4)
E_b1_50 = mean(betas_df$X5)
E_b1_100 = mean(betas_df$X6)
E_b1_500 = mean(betas_df$X7)
E_b1_1000 = mean(betas_df$X8)

#Varianzas
V_b0_50 = var(betas_df$X1)
V_b0_100 = var(betas_df$X2)
V_b0_500 = var(betas_df$X3)
V_b0_1000 = var(betas_df$X4)
V_b1_50 = var(betas_df$X5)
V_b1_100 = var(betas_df$X6)
V_b1_500 = var(betas_df$X7)
V_b1_1000 = var(betas_df$X8)


Esp_Var_DF <- data.frame( Tamano_muestral = n,
  E_b0 = c(E_b0_50, E_b0_100,E_b0_500, E_b0_1000),
  E_b1 = c(E_b1_50, E_b1_100,E_b1_500, E_b1_1000),
  V_b0 = c(V_b0_50,V_b0_100,V_b0_500,V_b0_1000),
  V_b1 = c(V_b1_50,V_b1_100,V_b1_500,V_b1_1000))

#Sesgos

Sesgo_b0_50 = E_b0_50 - beta0 
Sesgo_b0_100 = E_b0_100 - beta0
Sesgo_b0_500 = E_b0_500 - beta0
Sesgo_b0_1000 = E_b0_1000 - beta0

Sesgo_b1_50 = E_b1_50 - beta1 
Sesgo_b1_100 = E_b1_100 - beta1
Sesgo_b1_500 = E_b1_500 - beta1
Sesgo_b1_1000 = E_b1_1000 - beta1

Esp_Var_DF["Sesgo_b0"] <- c(Sesgo_b0_50, Sesgo_b0_100, Sesgo_b0_500, Sesgo_b0_1000)
Esp_Var_DF["Sesgo_b1"] <- c(Sesgo_b1_50,Sesgo_b1_100,Sesgo_b1_500,Sesgo_b1_1000)

#Pregunta B - Gráficos

g1 <- ggplot(betas_df)+
  geom_histogram(aes(betas_df[,5], y=..density..), col="red", bins=30)+
  stat_function(fun=dnorm, args=list(mean=2.5, sd=sd(betas_df[,5])),
                geom="line", colour="blue", size=1.2)+
  ylab("Densidad") + ggtitle("Muestra=50") + xlab(expression(beta1))

g2 <- ggplot(betas_df)+
  geom_histogram(aes(betas_df[,6], y=..density..), col="red", bins=30)+
  stat_function(fun=dnorm, args=list(mean=2.5, sd=sd(betas_df[,6])),
                geom="line", colour="blue", size=1.2)+
  ylab("Densidad") + ggtitle("Muestra=100") + xlab(expression(beta1))

g3 <- ggplot(betas_df)+
  geom_histogram(aes(betas_df[,7], y=..density..), col="red", bins=30)+
  stat_function(fun=dnorm, args=list(mean=2.5, sd=sd(betas_df[,7])),
                geom="line", colour="blue", size=1.2)+
  ylab("Densidad") + ggtitle("Muestra=500") + xlab(expression(beta1))

g4 <- ggplot(betas_df)+
  geom_histogram(aes(betas_df[,8], y=..density..), col="red", bins=30)+
  stat_function(fun=dnorm, args=list(mean=2.5, sd=sd(betas_df[,8])),
                geom="line", colour="blue", size=1.2)+
  ylab("Densidad") + ggtitle("Muestra=1000") + xlab(expression(beta1))

grid.arrange(g1, g2, g3, g4, nrow=2, ncol=2, top=caso 1)


#Pregunta C

#Caso 2
betas_c2=matrix(NA,nrow=reps,ncol=8)

for (j in 1:length(n)) {
  
  for (i in 1:reps) {
    
    X1 = rnorm(n[j],20,1)
    E = rnorm(n[j],0,1)
    u = rnorm(n[j],0,1)
    X2 = runif(n[j],0,1)
    Y = beta0 + beta1*X1 + beta2*X2 + u
    
    model1 = lm(Y~X1)
    betas_c2[i,j]=model1$coef[[1]]
    betas_c2[i,j+4]=model1$coef[[2]]
    
  }
}

betas_df2 = data.frame(betas_c2)

#Esperanzas
E_b0_50 = mean(betas_df2$X1)
E_b0_100 = mean(betas_df2$X2)
E_b0_500 = mean(betas_df2$X3)
E_b0_1000 = mean(betas_df2$X4)
E_b1_50 = mean(betas_df2$X5)
E_b1_100 = mean(betas_df2$X6)
E_b1_500 = mean(betas_df2$X7)
E_b1_1000 = mean(betas_df2$X8)

#Varianzas
V_b0_50 = var(betas_df2$X1)
V_b0_100 = var(betas_df2$X2)
V_b0_500 = var(betas_df2$X3)
V_b0_1000 = var(betas_df2$X4)
V_b1_50 = var(betas_df2$X5)
V_b1_100 = var(betas_df2$X6)
V_b1_500 = var(betas_df2$X7)
V_b1_1000 = var(betas_df2$X8)


Esp_Var_DF2 <- data.frame( Tamano_muestral = n,
                          E_b0 = c(E_b0_50, E_b0_100,E_b0_500, E_b0_1000),
                          E_b1 = c(E_b1_50, E_b1_100,E_b1_500, E_b1_1000),
                          V_b0 = c(V_b0_50,V_b0_100,V_b0_500,V_b0_1000),
                          V_b1 = c(V_b1_50,V_b1_100,V_b1_500,V_b1_1000))

#Sesgos

Sesgo_b0_50 = E_b0_50 - beta0 
Sesgo_b0_100 = E_b0_100 - beta0
Sesgo_b0_500 = E_b0_500 - beta0
Sesgo_b0_1000 = E_b0_1000 - beta0

Sesgo_b1_50 = E_b1_50 - beta1 
Sesgo_b1_100 = E_b1_100 - beta1
Sesgo_b1_500 = E_b1_500 - beta1
Sesgo_b1_1000 = E_b1_1000 - beta1

Esp_Var_DF2["Sesgo_b0"] <- c(Sesgo_b0_50, Sesgo_b0_100, Sesgo_b0_500, Sesgo_b0_1000)
Esp_Var_DF2["Sesgo_b1"] <- c(Sesgo_b1_50,Sesgo_b1_100,Sesgo_b1_500,Sesgo_b1_1000)

# Caso 2 pregunta b

g12 <- ggplot(betas_df2)+
  geom_histogram(aes(betas_df2[,5], y=..density..), col="red", bins=30)+
  stat_function(fun=dnorm, args=list(mean=2.5, sd=sd(betas_df2[,5])),
                geom="line", colour="blue", size=1.2)+
  ylab("Densidad") + ggtitle("Muestra=50") + xlab(expression(beta1))

g22 <- ggplot(betas_df2)+
  geom_histogram(aes(betas_df2[,6], y=..density..), col="red", bins=30)+
  stat_function(fun=dnorm, args=list(mean=2.5, sd=sd(betas_df2[,6])),
                geom="line", colour="blue", size=1.2)+
  ylab("Densidad") + ggtitle("Muestra=100") + xlab(expression(beta1))

g32 <- ggplot(betas_df2)+
  geom_histogram(aes(betas_df2[,7], y=..density..), col="red", bins=30)+
  stat_function(fun=dnorm, args=list(mean=2.5, sd=sd(betas_df2[,7])),
                geom="line", colour="blue", size=1.2)+
  ylab("Densidad") + ggtitle("Muestra=500") + xlab(expression(beta1))

g42 <- ggplot(betas_df2)+
  geom_histogram(aes(betas_df2[,8], y=..density..), col="red", bins=30)+
  stat_function(fun=dnorm, args=list(mean=2.5, sd=sd(betas_df2[,8])),
                geom="line", colour="blue", size=1.2)+
  ylab("Densidad") + ggtitle("Muestra=1000") + xlab(expression(beta1))

grid.arrange(g12, g22, g32, g42, nrow=2, ncol=2, top = "Caso 2")
