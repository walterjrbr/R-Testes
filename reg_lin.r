# Teste de regressão linear

t = read.csv(file = "arquivo_teste.csv", header = TRUE, sep = ";")
print(t)
class(t)
size=(t$size)
tempo=(t$tempo)

data1 <- data.frame(size, tempo)
print(data1)

# Treino do modelo
 
resmodelo<-lm(tempo~size, data=data1)

summary(resmodelo)
shapiro.test(rstudent(resmodelo))

plot(tempo~size)
abline(resmodelo,lty=3)

# Testando valores 

size_300 = data.frame(size = 550)
predict(resmodelo, size_300)

