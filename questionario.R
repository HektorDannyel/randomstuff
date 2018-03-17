quest <- read_csv("C:/Users/Sueli/Desktop/C+¦pia de Question+írioEstat+¡sticaB+ísica2.csv")



grau <- sample(c("Mestrado", "Doutorado"), 30, TRUE)
ystart <- sample(2013:2018, 30, TRUE)
setor <- sample(c("Agrárias", "Exatas", "Biológicas", "Saúde"), 30, TRUE)
yend <- ystart - sample(1:3, 30, TRUE)
yend2 <- ystart - sample(4:6, 30, TRUE)
yend[which(grau == "Doutorado")] <- yend2[which(grau == "Doutorado")]
local_grd <- sample(c("UFPR", "UFSC", "USP", "UNICAMP", "UFMA"), 30, TRUE)

proggrad <- character(30)

for(i in 1:30){
  proggrad[i] <- paste(sample(c("Iniciação científica", "PET", "Bolsista de algum outro projeto"), sample(1:3), FALSE), collapse = ", ")
}

yend_m <- rep(2017, 30)
yend_m[which(grau != "Doutorado")] <- 0
local_m <- sample(c("UFPR", "UFSC", "USP", "UNICAMP", "UFMA"), 30, TRUE)
local_m[which(grau != "Doutorado")] <- ""
nart <- sample(2:5, 30, TRUE)
bols <- sample(c("capes", "CNPQ", "Não"), 30, TRUE)
estat <- sample(c("Sim", "Não"), 30, TRUE)
estat_p <- sample(c("Sim", "Não"), 30, TRUE)

soft <- character(30)

for(i in 1:30){
  soft[i] <- paste(sample(c("R", "SAS", "SPSS"), sample(1:3), FALSE), collapse = ", ")
}

met <- sample(c("Sim", "Não", "Não sei"), 30, TRUE)
import <- sample(1:10, 30, TRUE)
trab <- sample(c("Sim", "Não"), 30, TRUE)
sexo <- sample(c("Masculino", "Feminino", "Outro"), 30, TRUE, c(.45, .45, .1))
irm <- sample(0:4, 30, TRUE)
orig <- sample(c("Curitiba ou região metropolitana", "São Paulo", "Florianópolis", "Bauru", "Açailândia"),
               30, TRUE)
mora <- sample(c("Pais", "Sozinho", "Cônjuge"), 30, TRUE)
transp <- sample(c("Pé", "Carro", "Ônibus", "Bicicleta"), 30, TRUE)
hab <- sample(c("Não", "A", "B", "AB", "Acima AB"), 30, TRUE)
altura <- sample(c(150:190), 30, TRUE)
peso <- altura/2 - sample(10:20, 30, TRUE)
pet <- sample(c("Sim", "Não"), 30, TRUE)

inst <- character(30)

for(i in 1:30){
  if(sample(8, 1) == 1){
    inst[i] <- "Não"
  }
  else{
    inst[i] <- paste(sample(c("Violão", "Guitarra", "Gaita", "Bateria", "Baixo", "Flauta", "Ocarina"), sample(1:7), FALSE), collapse = ", ")
  }
}

nasc <- paste0(sample(1:31, 30, TRUE), "/", sample(1:12, 30, TRUE), "/", sample(1980:1990, 30, TRUE))

rede <- character(30)

for(i in 1:30){
  rede[i] <- paste(sample(c("Facebook", "Twitter", "LinkedIn", "Instagram", "Outro"), sample(1:5), FALSE), collapse = ", ")
}

quest1 <- cbind(grau, ystart, setor, yend, local_grd, proggrad, yend_m, local_m, nart, bols, estat, estat_p, soft, met, import, trab, sexo, irm, orig, mora,
                transp, hab, altura, peso, pet, inst, nasc, rede)


quest$Grau <- tolower(iconv(quest$Grau, to ='ASCII//TRANSLIT'))

for(i in 1:dim(quest)[1]){
  if(quest$`Ano de início pós-graduação cursando atualmente`[i] <= 18){
    quest$`Ano de início pós-graduação cursando atualmente`[i] <- quest$`Ano de início pós-graduação cursando atualmente` + 2000
  }
  else if(quest$`Ano de início pós-graduação cursando atualmente` < 100){
    quest$`Ano de início pós-graduação cursando atualmente`[i] <- quest$`Ano de início pós-graduação cursando atualmente` + 1900
  }
}

quest$Setor <- tolower(iconv(quest$Setor, to ='ASCII//TRANSLIT'))

for(i in 1:dim(quest)[1]){
  if(quest$`Ano conclusão graduação`[i] <= 18){
    quest$`Ano conclusão graduação`[i] <- quest$`Ano conclusão graduação` + 2000
  }
  else if(quest$`Ano conclusão graduação` < 100){
    quest$`Ano conclusão graduação`[i] <- quest$`Ano conclusão graduação` + 1900
  }
}

quest$`Local da graduação` <- tolower(iconv(quest$`Local da graduação`, to ='ASCII//TRANSLIT'))

for(i in 1:dim(quest)[1]){
  if(quest$`Ano conclusão mestrado (apenas doutorandos)`[i] <= 18 & quest$`Ano conclusão mestrado (apenas doutorandos)`[i] > 0){
    TESTAR COMO O FORMULARIO EXTRAI BLANK
    quest$`Ano conclusão mestrado (apenas doutorandos)`[i] <- quest$`Ano conclusão mestrado (apenas doutorandos)` + 2000
  }
  else if(quest$`Ano conclusão mestrado (apenas doutorandos)` < 100 & quest$`Ano conclusão mestrado (apenas doutorandos)` != 0){
    quest$`Ano conclusão mestrado (apenas doutorandos)`[i] <- quest$`Ano conclusão mestrado (apenas doutorandos)` + 1900
  }
}

quest$`Local mestrado (somente se já concluído)` <- iconv(quest$`Local mestrado (somente se já concluído)`, to ='ASCII//TRANSLIT')

quest$`Você é bolsista remunerado de algum programa?` <- tolower(iconv(quest$`Você é bolsista remunerado de algum programa?`, to ='ASCII//TRANSLIT'))

quest$`Já utilizou algum software estatístico? Qual? (se mais de um separar por ponto-e-vírgula`
PEGAR A FUNÇÃO USADA NO COISA DE DURABILIDADE PRA JOGAR PRA BAIXO

quest$`Seu meio de transporte predominante para ir para Universidade` <- 
  tolower(iconv(quest$`Seu meio de transporte predominante para ir para Universidade`, to ='ASCII//TRANSLIT'))

quest$`Qual a sua altura?` <- gsub(",", "", quest$`Qual a sua altura?`)
quest$`Qual a sua altura?` <- gsub(".", "", quest$`Qual a sua altura?`)
quest$`Qual o seu peso?` <- gsub(",", "", quest$`Qual o seu peso?`)
quest$`Qual o seu peso?` <- gsub(".", "", quest$`Qual o seu peso?`)

quest$`Toca algum instrumento musical?` <- tolower(iconv(quest$`Toca algum instrumento musical?`, to ='ASCII//TRANSLIT'))

