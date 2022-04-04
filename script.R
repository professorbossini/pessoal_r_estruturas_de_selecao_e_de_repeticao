# 1. Abra o arquivo cancer.txt como um data frame.
arq <- read.table('cancer.txt', header=TRUE)
print(arq)

#2. Usando um loop for, faça o histograma das variáveis LDH, ALB e N. Use cores diferentes.
#nomes que existem no arquivo
campos <- c('LDH', 'ALB', 'N');campos
#titulos para os histogramas
titulos <- c('lactate dehydrogenase (LDH)', 'Albumina(ALB)', 'Nitrogênio na Uréia(N)')
#cores para cada histograma
cores <- c('blue', 'green', 'red')


n <- 1:length(campos)
?par
#par: configura parametros de gráficos que serão feitos a seguir
#mfrow = c(1, 3): figuras serão desenhadas da esquerda para a direita, de cima para baixo
#mfcol = c(1, 3): figuras serão desenhadas de cima para baixo, da esquerda para a direita
#1 linha e 3 colunas (testar primeiro)
par(mfrow = c(1, 3))
#2 linhas e 2 colunas (testar depois)
par(mfcol = c(2, 2))
#lembrando: isso dá a coleção, incluindo os headers
arq[campos[1]]
#já isso, dá a coleção avulsa
arq[[campos[1]]]
#campos[i] = LDH, ALB, N
for (i in n){
  #dados, titulo, cor, rotulo eixo x
  hist(arq[[campos[i]]], main=titulos[i], col=cores[i], xlab=campos[i], ylab="Frequência")
}

#volta parâmetro ao padrão
par(mfrow = c(1,1))




# x <- 3
# if (x %% 2 == 1)
#     print ("É impar")
# 
# x <- 2
# if (x %% 2 == 1)
#     print ("É par")
# 
# 
# x <- 2
# if (x %% 2 == 1)
#   print ("É impar")
# 
# x <- 2
# if (x %% 2 == 1)
#   print ("É impar") else{
#   print ("É par")
# }

