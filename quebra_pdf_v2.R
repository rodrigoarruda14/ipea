
## Carregamento Pacotes
library(pdftools)
library(dplyr)
library(magrittr)
library(stringr)

# Leitura e quebra do PDF
pdf <- pdf_text("C:/Users/b7215078/Downloads/PDF - Bahia/ALAGOINHAS _SORTEIO.pdf")

# Seleciona a primeira pagina pra teste e remove o espaço a esquerda
v <- noquote(unlist(strsplit(pdf, "\n"))) %>% as.vector() %>% str_trim("left")

# Expressão para identificar as linhas com observações
reg_exp <- "^[0-9]"
flag  <- grepl(pattern = reg_exp, x = v)
df <- cbind(v, flag) %>% as.data.frame()

df2 <- df %>% mutate(v_novo=ifelse(flag==TRUE & lead(flag)==FALSE, paste(v, lead(v), sep = "\n"), v))
df2 %<>% filter(flag=='TRUE')


# Expressões para os diferentes padrões de observações
reg_exp1 <- "(^[0-9]{1,4})\\s+([0-9]+)\\s+([0-9]{4})\\s+[A-Z]+"
reg_exp2 <- "(^[0-9]{1,4})\\s+([0-9]+)\\s+([0-9]+)\\s+([0-9]{4})\\s+"
reg_exp3 <- "(^[0-9]{1,4})\\s+([0-9]+)\\s+(CURSANDO)\\s+"
reg_exp4 <- "(^[0-9]{1,4})\\s+([0-9]+)\\s+([0-9]+)\\s+(CURSANDO)\\s+"


flag1 <- grepl(pattern = reg_exp1, x = df2[,3])
flag2 <- grepl(pattern = reg_exp2, x = df2[,3])
flag3 <- grepl(pattern = reg_exp3, x = df2[,3])
flag4 <- grepl(pattern = reg_exp4, x = df2[,3])

df2 %<>% cbind.data.frame(flag1, flag2, flag3, flag4)


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
complemento <- "([:print:]+)"

regexp1 <- paste(id, inscricao, ano_reg1, nome, cpf, situacao, tipo, endereco, tel, tel_con, complemento, sep = "\\s+")
regexp2 <- paste(id, inscricao, matricula, ano_reg1, nome, cpf, situacao, tipo, endereco, tel, tel_con, complemento, sep = "\\s+")
regexp3 <- paste(id, inscricao, ano_reg3, nome, cpf, situacao, tipo, endereco, tel, tel_con, complemento, sep = "\\s+")
regexp4 <- paste(id, inscricao, matricula, ano_reg3, nome, cpf, situacao, tipo, endereco, tel, tel_con, complemento, sep = "\\s+")

# Caso 1

df2 %<>% select(v_novo, flag1, flag2, flag3, flag4)
st <- df2 %>% filter(flag1==TRUE)
st <- str_match_all(string = st$v_novo, pattern = regexp1) 
df1 <- data.frame(matrix(unlist(st), nrow=length(st), byrow=T),stringsAsFactors=FALSE) 
names(df1) <- c("obs", "id", "inscricao", "ano", "nome", "cpf", "situacao", 
                "tipo", "endereco", "tel", "tel_con", "complemento")

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


df_fim <- rbind(df1,df2,df3, df4)


########
########

# Leitura e quebra do PDF
pdf <- pdf_text("C:/Users/08259760495/Downloads/pdfs/classificadosCompleto - 2014.pdf")

# Seleciona a primeira pagina pra teste e remove o espaço a esquerda
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

write.csv2(df5, "classificados_completo.csv")

df1 <- df[11:20,]
texto <- as.character(df[12,1])
str_view(texto, "(?:(?!ADO).)*")

if(df1[,2]==FALSE)
{
  df1[,1] <- paste(lag(df1[,1]), df1[,1], sep = "\n")
}

df2 <- df %>% mutate(v_novo=ifelse(flag==TRUE & lead(flag)==FALSE, paste(v, lead(v), sep = "\n"), v))


str_trim(str_extract(as.character(df1[5,12]), pattern = "\\D+(?= ADO?)"), "right")
str_extract(as.character(df1[18,12]), pattern = "\\D+(?= O)")


df1 %<>% mutate(compl_nome=ifelse(situacao=="CONTEMPL", str_trim(str_extract(complemento, pattern = "\\D+(?= ADO?)"), "right"), ""))

df1 %<>% mutate(compl_nome2=ifelse(situacao=="RESERVA", str_trim(str_extract(complemento, pattern = "\\D+(?= O?)"), "right"), ""))

            

