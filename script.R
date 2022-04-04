# 1. Abra o arquivo cancer.txt como um data frame.
arq <- read.table('cancer.txt', header=TRUE)
print(arq)
#####################################################################################
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
#####################################################################################

#3. Qual grupo de diagnósticos tem os pacientes mais novos? Descubra, usando desvio condicional, qual a mediana e escreva o grupo e a mediana.

#pega a variável Grupo
arq$Grupo

#gera uma coleção de booleanos: True onde grupo==1
arq$Grupo==1

#pega todas as idades
arq[TRUE, 'Idade']

#pega idade alternando: primeira sim, segunda não etc...
arq[c(T, F), 'Idade']
#pega as idades em que grupo==1
arq[arq$Grupo==1, 'Idade']

#mediana de falsos negativos
idade_fn <- median (arq[arq$Grupo==1, 'Idade'])
idade_n <- median (arq[arq$Grupo==2, 'Idade'])
idade_p <- median (arq[arq$Grupo==3, 'Idade'])
idade_fp <- median (arq[arq$Grupo==4, 'Idade'])
print (idade_fn)
print (idade_n)
print (idade_p)
print (idade_fp)

#resolvendo com if/else
modelo = 'Os mais novos são do grupo %s. Mediana: %d.\n'
if (idade_fn < idade_n && idade_fn < idade_p && idade_fn && idade_fp){
  #repare como o \n faz parte da string quando usamos sprintf (e variações de print)
  sprintf(modelo, 'FN', idade_fn)
  #para interpretar o \n como newline, use algo como cat
  ?cat
  cat(sprintf(modelo, 'FN', idade_fn))
} else if (idade_n < idade_fn && idade_n < idade_p && idade_n < idade_fp){
  cat(sprintf(modelo, 'N', idade_n))
} else if (idade_p < idade_fn && idade_p < idade_n && idade_p < idade_fp){
  cat(sprintf(modelo, 'P', idade_p))
} else{
  cat(sprintf(modelo, 'FP', idade_fp))
}




#resolvendo com ifelse (tipo o ternário)

#observe como ele pode ser inconveniente neste caso
#por ser vetorizado, o teste precisa ser um vetor de booleanos se desejarmos devolver um vetor
#teste simples antes de resolver o exercício
a <- ifelse(1 < 2, c(1, 2), c(3, 4)); a;
a <- ifelse(c(1 < 2, 1 < 2), c(1, 2), c(3, 4)); a;
res <- ifelse(
  c(idade_fn < idade_n && idade_fn < idade_p && idade_fn && idade_fp, idade_fn < idade_n && idade_fn < idade_p && idade_fn && idade_fp),
  c('FN', idade_fn),
  ifelse(
    c(idade_n < idade_fn && idade_n < idade_p && idade_n < idade_fp, idade_n < idade_fn && idade_n < idade_p && idade_n < idade_fp),
    c('N', idade_n),
    ifelse(
      c(idade_p < idade_fn && idade_p < idade_n && idade_p < idade_fp, idade_p < idade_fn && idade_p < idade_n && idade_p < idade_fp),
      c('P', idade_p),
      c('FP', idade_fp)
    )
  )
)
print(res)

cat(sprintf(modelo, res[1], as.numeric(res[2])))
#resolvendo com um dataframe ordenado
frame = data.frame(nome=c('FN', 'N', 'P', 'FP'), valor=c(idade_fn, idade_n, idade_p, idade_fp))
frame
#ordenamos e pegamos todas as linhas (por isso nada depois da virgula)
frame <- frame[order(frame$valor),]
frame
#paste: concatenação de vetores após conversão para caractere
?paste
#exibimos
print (paste("Os mais novos são do grupo: ", frame[1,1], ". Mediana: ", frame[1,2]))
#######################################################################################
# 4. Crie um gráfico de barras comparando a média de glicose de cada grupo diagnóstico. Suponha que você não saiba de antemão os grupos, por isso monte um algoritmo genérico usando um loop for para acumular.

#unique: remoção de elementos duplicados
?unique
grupos <- unique(arq$Grupo);grupos
#vetor de quatro posições, uma para cada grupo, inicialmente valendo zero em cada posição
glicoses <- vector (mode = 'numeric', length = length(grupos)); glicoses
#Associando os nomes 1, 2, 3 e 4 a cada posição, nesta ordem. cada número é um grupo
names(glicoses) <- grupos; glicoses
#intervalo
n <- 1:length(grupos)
for (i in n){
  #pega somente os valores da coluna GL em que grupo igual ao grupo i
  glicoses[i] <- mean (arq[arq$Grupo==grupos[i], 'GL'])
}
glicoses
barplot(glicoses, main = "Glicemia dos Grupos", ylab="Média Glicemia", xlab='Grupos', col = 'lightblue')
######################################################################
































