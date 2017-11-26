#Análise de regressão é uma técnica estatística utilizada para investigar a relação existente entre variáveis
#através da construção de uma equação (um modelo). De maneira geral, essa técnica pode ser utilizada
#com vários objetivos, dentre os quais se pode destacar: descrever a relação entre variáveis para entender
#um processo ou fenômeno; prever o valor de uma variável a partir do conhecimento dos valores das outras
#variáveis; substituir a medição de uma variável pela observação dos valores de outras variáveis; controlar
#os valores de uma variável em uma faixa de interesse.


#Inicialmente, os dados devem ser organizados como objetos de dados R, nesse caso como um data frame
#(planilha). Para isso, é necessário que a tabela acima se encontre numa estrutura tabular, na qual as
#colunas representam as variáveis e as linhas representam os indivíduos

imoveis <- read.csv(file = "dataset/imoveis-sirgas-xml-bd.csv", header = TRUE, sep = ";")

#ARTHUR - removendo as datas em branco
imoveis <- imoveis[!(is.na(imoveis$dataCadastroPrefeituraSimples) | imoveis$dataCadastroPrefeituraSimples==""), ]
#ARTHUR - calculando a data cadastro prefeitura com a data atual e jogando na variavel teste
vetorSemDataVazio <- imoveis[!(is.na(imoveis$dataCadastroPrefeituraSimples) | imoveis$dataCadastroPrefeituraSimples==""), ]
##imoveis$dataCadastroDiferencaDias <- difftime(as.Date(vetorSemDataVazio$dataCadastroPrefeituraSimples, format = "%d/%m/%Y") ,Sys.Date() , units = c("days"));
imoveis$dataCadastroDiferencaDias <- difftime(Sys.Date(),as.Date(vetorSemDataVazio$dataCadastroPrefeituraSimples, format = "%d/%m/%Y") , units = c("days"));
#ARTHUR - transformando o valor de dias em numerico
imoveis$dataCadastroDiferencaDias <-as.numeric(as.character(imoveis$dataCadastroDiferencaDias))
#ARTHUR - transformando o valor de enderecoImovelBairro, tipoEstruturaSimples, estadoConservacaoSimples e situacaoPavimentacao em numerico
imoveis$enderecoImovelBairroNumerico <- as.numeric(imoveis$enderecoImovelBairro)
imoveis$tipoConstrucaoSimplesNumerico <- as.numeric(imoveis$tipoEstruturaSimples)
imoveis$estadoConservacaoSimplesNumerico <- as.numeric(imoveis$estadoConservacaoSimples)
imoveis$situacaoPavimentacaoNumerico <- as.numeric(imoveis$situacaoPavimentacao)

#ARTHUR - selecionando as colunas
imoveis <- imoveis[imoveis$natureza == "Predial",]
##imoveis <- imoveis[,c(25,30,45:49,64,65)]
imoveis <- imoveis[,c(25,30,45:49,64,65,66,67,68)]
imoveis <- imoveis[imoveis$valorVenal <= 60000, ]

#Uma maneira fácil de obter algumas estatísticas descritivas das variáveis em estudo é através do comando
#summary(), que retorna as estatísticas mínimo, quartis, média e máximo. Para medir a variabilidade,
#utilize as funções var() e sd() para obter a variância e o desvio padrão.

summary(imoveis)
str(imoveis)

#Para verificar a existência de alguma relação entre Salário e Experiência, deve-se construir um Diagrama
#de Dispersão para as duas variáveis:
plot(imoveis)

#Para calcular o Coeficiente de Correlação Linear de Pearson entre as variáveis, utilize a função cor:
cor(imoveis)

cor(imoveis$valorVenal, imoveis$areaTotalConstruidaSimples)
plot(imoveis$valorVenal, imoveis$areaTotalConstruidaSimples)
boxplot(imoveis$valorVenal, imoveis$areaTotalConstruidaSimples)

#Observe que o R retornou o valor 0.9735413 o que evidencia uma forte relação linear entre as variáveis
#em estudo. Para avaliar se esse resultado é significativo, pode-se realizar um Teste de Hipóteses para a
#o Coeficiente de Correlação (supondo que as suposições do teste sejam satisfeitas)

cor.test(imoveis$valorVenal, imoveis$areaTotalConstruidaSimples)

#Como o Valor P do teste (p-value < 2.2e-16) é bem pequeno, conclui-se que o valor do Coeficiente de
#Correlação Linear de Pearson tem significância Estatística.

#Ajuste do Modelo Linear
#Sejam X e Y, respectivamente, as variáveis Experiência (explicativa) e Salário (resposta). Propõe-se um
#modelo de regressão linear de primeira ordem, dado pela equação: Y = β0 + β1X + ², onde β0 e β1 são
#parâmetros desconhecidos e ²é o erro aleatório.
#Para ajustar um modelo de regressão linear no R utiliza-se a função lm:

ajuste=lm(imoveis$valorVenal ~ imoveis$areaTotalConstruidaSimples + imoveis$testadaFicticiaLote)
ajuste

#Note que função lm() é chamada com o formato lm(y ˜ x), ou seja, a variável resposta é y e a preditora
#é x, sempre nessa ordem.

#O R retorna o valor dos coeficientes de β0 e β1 estimados via Método de Mínimos Quadrados. Logo, a
#equação da reta ajustada é dada por Y = -3463.2   + 606.6Xi.
#Com a função summary, diversas medidas descritivas úteis para a análise do ajuste podem ser obtidas:

summary(ajuste)

#Com a função anova, pode-se construir a Tabela da Análise de Variância:

anova(ajuste)

#Para esboçar a reta ajustada no diagrama de dispersão, utilize a função abline:

plot(imoveis$valorVenal, imoveis$areaTotalConstruidaSimples)
abline(lm(imoveis$valorVenal ~ imoveis$areaTotalConstruidaSimples))

#Para construir os Intervalos de Confiança (95%) para os coeficientes da regressão, utiliza-se o seguinte
#comando:

confint(ajuste)

#Para avaliar as suposições de que os erros possuem variância constante e são não correlacionados entre
#si, construa os gráficos de “Resíduos versus Valores Ajustados da Variável Resposta” e “Resíduos versus
#Valores da Variável Explicativa":

plot(fitted(ajuste),residuals(ajuste),xlab="Valores Ajustados",ylab="Resíduos")
abline(h=0)
plot(imoveis$valorVenal,residuals(ajuste),xlab="Experiência",ylab="Resíduos")
abline(h=0)
