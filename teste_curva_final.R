
# Testes da curva

library(tidyverse)
library(readxl)

rm(list = ls()) # limpa enviroment

newdata <- read_xlsx("curva.xlsx", sheet = "prev")
newdata %>% head()

ajuste <- read_xlsx("curva.xlsx", sheet = "ajust")

resultado <- newdata
resultado$`Ajuste Luiza` <- ajuste$ajust
resultado %>% head()

## Curva com base nos dados amostrais
data <- read_xlsx("dados_teoricos.xlsx")
data %>% head()
data <- data %>% dplyr::mutate(log2concentration = log2(conc))

# Muito bom esse
library(drc)
drc.fit <- drm(abs ~ conc,
               data = data,
               fct = LL2.4(names = c("n", "Amin", "Amax", "EC50"))
)
summary(drc.fit)

plot(drc.fit)

plot(data$conc %>% log2(), data$abs, type = "line")

# os residuos ficaram ruins
ggplot(as_tibble(drc.fit$predres)) +
  geom_point(aes(`Predicted values`, Residuals),
             pch = 1, size = 3, color = "cornflowerblue") +
  geom_hline(yintercept = 0, lwd = 0.2)

prev.conc = newdata$abs # original

prev.conc = new$conc

### Valores iniciais de DRC

n.est <- coef(drc.fit)[1]
#Amin <- coef(drc.fit)[2]
Amax.est <- coef(drc.fit)[3]
log2EC50.est <- log2(coef(drc.fit)[4])

Amin <- min(newdata$abs)

log2concentration.est <- sapply(prev.conc, function(A){(1/n.est) * (log( ((Amax.est - A)/(A - Amin))) + log2EC50.est*n.est)})

results <- newdata %>% 
  dplyr::mutate(concentration.est = 2^log2concentration.est) %>%
  dplyr::mutate(log2concentration.est = log2concentration.est)

resultado$Conc.LogEst <- results$log2concentration.est
resultado$Conc.Est <- results$concentration.est
resultado

######### Teste grafico

results <- tibble(
  conc =  2^log2concentration.est,
  abs = prev.conc,
) 

results <- results %>% drop_na()

teste <- rbind(dat,results)

plot(teste$conc %>% log2(), teste$abs)
plot(results$conc %>% log2(), results$abs, ylim = )
