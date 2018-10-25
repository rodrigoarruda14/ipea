
## Carregamento Pacotes
library(pdftools)
library(dplyr)
library(magrittr)
library(stringr)

#############################
# Quebra de PDFs - Modelo 1 #
#############################

## Define o diretório

dir <- "C:/Users/08259760495/Downloads/pdfs/Modelo 1"
setwd(dir)  


## Leitura e quebra do PDF

files <- list.files()

for(i in files){
  
  pdf <- pdf_text(i)
  
  ## Quebra as linhas e remove o espaço a esquerda
  v <- noquote(unlist(strsplit(pdf, "\r\n"))) %>% as.vector() %>% str_trim("left")
  
  ## Expressão para identificar as linhas com observações
  reg_exp <- "^[0-9]"
  reg_cab <- "Curso"
  
  flag  <- grepl(pattern = reg_exp, x = v)
  flag_cab  <- grepl(pattern = reg_cab, x = v)
  df <- cbind(v, flag, flag_cab) %>% as.data.frame()
  df %<>% filter(flag=='TRUE'|flag_cab=='TRUE')
  
  ## Capturando o cabecalho
  df %<>% mutate(Curso = ifelse(flag_cab=='TRUE', v, ""))
  
  while(sum(df$Curso=="")>0){
    
    df <- df %>% mutate(Curso = ifelse(Curso!="", Curso, lag(Curso)))
  
  }
  
  df %<>% mutate(Turno = Curso, Semestre = Curso)
  
  df %<>% mutate(Curso = str_trim(str_extract(string = Curso, pattern = "(?<=Curso:)(.*)(?=Turno)"), "left"),
                 Turno = str_trim(str_extract(string = Turno, pattern = "(?<=Turno:)(.*)(?=Semestre)"), "left"),
                 Semestre = str_trim(str_extract(string = Semestre, pattern = "(?<=Semestre)(.*)"), "left"))
  
  ## Expressões para os diferentes padrões de observações
  reg_exp1 <- "(^[0-9]{1,4})\\s+([0-9]+)\\s+([0-9]{4})\\s+[A-Z]+"
  reg_exp2 <- "(^[0-9]{1,4})\\s+([0-9]+)\\s+([0-9]+)\\s+([0-9]{4})\\s+"
  reg_exp3 <- "(^[0-9]{1,4})\\s+([0-9]+)\\s+(CURSANDO)\\s+"
  reg_exp4 <- "(^[0-9]{1,4})\\s+([0-9]+)\\s+([0-9]+)\\s+(CURSANDO)\\s+"
  
  
  flag1 <- grepl(pattern = reg_exp1, x = df[,1])
  flag2 <- grepl(pattern = reg_exp2, x = df[,1])
  flag3 <- grepl(pattern = reg_exp3, x = df[,1])
  flag4 <- grepl(pattern = reg_exp4, x = df[,1])
  
  df %<>% cbind.data.frame(flag1, flag2, flag3, flag4)
  
  
  ## Construir as expressões regulares para extrair os campos de cada linha
  
  id <- "([0-9]+)"
  inscricao <- "([0-9]+)"
  matricula <- "([0-9]+)"
  ano_reg1 <- "([0-9]{4})"
  ano_reg3 <- "([A-Z]{8})"
  nome <- "(\\D+)"
  cpf <- "([0-9]+[.][0-9]+[.][0-9]+[-][0-9]+)"
  situacao <- "([A-Z]+)"
  tipo <- "([A-Z]+)"
  endereco <- "([^\\(]+)"
  tel <- "([(]\\s*[0-9]+\\s*[)]\\s+[[0-9]\\-]+)"
  tel_con <- "([(]\\s*[0-9]+\\s*[)]\\s+[[0-9]\\-]+)"
  
  regexp1 <- paste(id, inscricao, ano_reg1, nome, cpf, situacao, tipo, endereco, tel, tel_con, sep = "\\s+")
  regexp2 <- paste(id, inscricao, matricula, ano_reg1, nome, cpf, situacao, tipo, endereco, tel, tel_con, sep = "\\s+")
  regexp3 <- paste(id, inscricao, ano_reg3, nome, cpf, situacao, tipo, endereco, tel, tel_con, sep = "\\s+")
  regexp4 <- paste(id, inscricao, matricula, ano_reg3, nome, cpf, situacao, tipo, endereco, tel, tel_con, sep = "\\s+")
  
  ## Existem 4 casos diferentes de preenchimento das linhas
  
  # Caso 1
  st <- df %>% filter(flag1==TRUE)
  st <- str_match_all(string = st$v, pattern = regexp1) 
  df1 <- data.frame(matrix(unlist(st), nrow=length(st), byrow=T),stringsAsFactors=FALSE) 
  names(df1) <- c("obs", "id", "inscricao", "ano", "nome", "cpf", 
                  "situacao", "tipo", "endereco", "tel", "tel_con")
  
  df1 %<>% mutate(matricula="") %>% select(obs, id, inscricao, matricula, ano, nome, cpf, 
                                           situacao, tipo, endereco, tel, tel_con)
  
  # Caso 2
  st <- df %>% filter(flag2==TRUE)
  st <- str_match_all(string = st$v, pattern = regexp2) 
  df2 <- data.frame(matrix(unlist(st), nrow=length(st), byrow=T),stringsAsFactors=FALSE) 
  names(df2) <- c("obs", "id", "inscricao", "matricula", "ano", "nome", "cpf", 
                  "situacao", "tipo", "endereco", "tel", "tel_con")
  
  # Caso 3
  st <- df %>% filter(flag3==TRUE)
  st <- str_match_all(string = st$v, pattern = regexp3) 
  df3 <- data.frame(matrix(unlist(st), nrow=length(st), byrow=T),stringsAsFactors=FALSE) 
  names(df3) <- c("obs", "id", "inscricao", "ano", "nome", "cpf", 
                  "situacao", "tipo", "endereco", "tel", "tel_con")
  
  df3 %<>% mutate(matricula="") %>% select(obs, id, inscricao, matricula, ano, nome, cpf, 
                                           situacao, tipo, endereco, tel, tel_con)
  
  # Caso 4
  st <- df %>% filter(flag4==TRUE)
  st <- str_match_all(string = st$v, pattern = regexp4) 
  df4 <- data.frame(matrix(unlist(st), nrow=length(st), byrow=T),stringsAsFactors=FALSE) 
  names(df4) <- c("obs", "id", "inscricao", "matricula", "ano", "nome", "cpf", 
                  "situacao", "tipo", "endereco", "tel", "tel_con")
  
  df4 %<>% mutate(matricula="") %>% select(obs, id, inscricao, matricula, ano, nome, cpf, 
                                           situacao, tipo, endereco, tel, tel_con)
  
  # Junta os 4 tipos de casos
  df_fim <- rbind(df1,df2,df3, df4)
  
  # Busca as variáveis do cabeçalho
  df_fim %<>% left_join(dplyr::select(df, v, Curso, Turno, Semestre), by = c("obs"="v"))
  
  # Salva o arquivo em csv
  write.csv2(df_fim, paste0(str_extract(i, pattern = "([^\\.pdf]+)"),".csv"))

}


#############################
# Quebra de PDFs - Modelo 2 #
#############################

## Define o diretório

dir <- "C:/Users/08259760495/Downloads/pdfs/Modelo 2"
setwd(dir)  

## Leitura e quebra do PDF

files <- list.files()

for(i in files){
  
  pdf <- pdf_text(i)
  
  # Seleciona a primeira pagina pra teste e remove o espa?o a esquerda
  v <- noquote(unlist(strsplit(pdf, "\n"))) %>% as.vector() %>% str_trim("left")
  df <- as.data.frame(v)
  
  reg_exp5 <- "(^[0-9]{1,4})\\s+([0-9]+)\\s+[A-Z]+"
  
  flag5 <- grepl(pattern = reg_exp5, x = df[,1])
  df %<>% cbind.data.frame(flag5)
  
  regexp5 <- paste(id, inscricao, nome, cpf, situacao, endereco, tel, sep = "\\s+")
  
  st <- df %>% filter(flag5==TRUE)
  st <- str_match_all(string = st$v, pattern = regexp5) 
  df5 <- data.frame(matrix(unlist(st), nrow=length(st), byrow=T),stringsAsFactors=FALSE) 
  names(df5) <- c("obs", "id", "inscricao", "nome", "cpf", 
                  "situacao", "endereco", "tel")
  
  write.csv2(df_fim, paste0(str_extract(i, pattern = "([^\\.pdf]+)"),".csv"))
}

