library(readxl)
library(dplyr)
library(caret)
library(forecast)
options(scipen = 9999)
# base de casos
base_covid = read_excel("D:/Hitalo/Área de Trabalho/pos/tcc/base/casos_covid09_11_20_fortal_fechado 19jan2021.xlsx",
                        col_types = c("text", "text", "text", 
                                      "numeric", "text", "text", "text", 
                                      "text", "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "text", "text", 
                                      "numeric", "numeric", "text", "text", 
                                      "text", "text", "text", "text", "text", 
                                      "text", "text", "text", "text", "text", 
                                      "text", "text", "text", "text", "text", 
                                      "text", "text", "text", "text", "text", 
                                      "text", "numeric", "text", "text", 
                                      "text", "text"))

#base info adicionais
inform_adicionais <- read_excel("D:/Hitalo/Área de Trabalho/pos/tcc/base/inform_adicionais.xlsx")


base_covid_tratada = base_covid %>% dplyr::mutate(default = case_when(obitoConfirmado == "True" ~ "1",TRUE ~ "0"),
                                                  comorbidadePuerperaSivep = case_when(comorbidadePuerperaSivep == "Sim" ~ "1",TRUE ~ "0"),
                                                  comorbidadeCardiovascularSivep = case_when(comorbidadeCardiovascularSivep == "Sim" ~ "1",TRUE ~ "0"),
                                                  comorbidadeHematologiaSivep = case_when(comorbidadeHematologiaSivep == "Sim" ~ "1",TRUE ~ "0"),
                                                  comorbidadeSindromeDownSivep = case_when(comorbidadeSindromeDownSivep == "Sim" ~ "1",TRUE ~ "0"),
                                                  comorbidadeAsmaSivep = case_when(comorbidadeAsmaSivep == "Sim" ~ "1",TRUE ~ "0"),
                                                  comorbidadeDiabetesSivep = case_when(comorbidadeDiabetesSivep == "Sim" ~ "1",TRUE ~ "0"),
                                                  comorbidadeNeurologiaSivep = case_when(comorbidadeNeurologiaSivep == "Sim" ~ "1",TRUE ~ "0"),
                                                  comorbidadePneumopatiaSivep = case_when(comorbidadePneumopatiaSivep == "Sim" ~ "1",TRUE ~ "0"),
                                                  comorbidadeImunodeficienciaSivep = case_when(comorbidadeImunodeficienciaSivep == "Sim" ~ "1",TRUE ~ "0"),
                                                  comorbidadeRenalSivep = case_when(comorbidadeRenalSivep == "Sim" ~ "1",TRUE ~ "0"),
                                                  comorbidadeObesidadeSivep = case_when(comorbidadeObesidadeSivep == "Sim" ~ "1",TRUE ~ "0"),
                                                  comorbidadeHiv = case_when(comorbidadeObesidadeSivep == "Sim" ~ "1",TRUE ~ "0"),
                                                  comorbidadeNeoplasias = case_when(comorbidadeNeoplasias == "Sim" ~ "1",TRUE ~ "0"),
                                                  racaCorPaciente = case_when(is.na(racaCorPaciente) ~ "Ignorado",TRUE ~ racaCorPaciente),
                                                  resultadoFinalExame = case_when(resultadoFinalExame == "Positivo" ~ "1",TRUE ~ "0"),
                                            bairro = `bairro 1`) %>%
  dplyr::select(default,bairro,sexoPaciente,idadePaciente,resultadoFinalExame,profissionalSaude,racaCorPaciente,starts_with("comorbidade"))

base_default = base_covid_tratada %>% dplyr::left_join(inform_adicionais, by = c("bairro" = "Bairros")) %>% dplyr::filter(resultadoFinalExame == "1") %>% na.omit()
dim(base_default)

table(base_default$default)

str(base_default)

names(base_default)
# exploratoria ------------------------------------------------------------

### quantidade de bairros
length(unique(base_default$bairro))

### Proporção de Sexo

base_default %>% dplyr::group_by(sexoPaciente) %>%
  dplyr::summarise(qtd = n(),
                   prop = n()/nrow(base_default))

### sumario das idades

summary(base_default$idadePaciente)

### profissional de saude

base_default %>% dplyr::group_by(profissionalSaude) %>%
  dplyr::summarise(qtd = n(),
                   prop = n()/nrow(base_default))

### raça do paciente

base_default %>% dplyr::group_by(racaCorPaciente) %>%
  dplyr::summarise(qtd = n(),
                   prop = n()/nrow(base_default))

### Comorbidades

base_default %>% dplyr::group_by(comorbidadeNeoplasias) %>%
  dplyr::summarise(qtd = n(),
                   prop = n()/nrow(base_default))

### Media de habitantes

summary(base_default$Media_habitantes_por_casa)

#### IDH 2012

summary(base_default$IDH2010)

#### Quantidade de habitantes

summary(base_default$qtd_habitantes)

# modelo logistico --------------------------------------------------------



base_0 = base_default %>% dplyr::filter(default == "0")

base_1 = base_default %>% dplyr::filter(default == "1")

amostra_0 = sample(1:length(base_0$default),length(base_1$default)*2)

base_0_fim = base_0[amostra_0,]

base_desenvolvimento_ = rbind(base_1,base_0_fim) %>% as.data.frame() %>% mutate_at(.vars = c("default","comorbidadePuerperaSivep",
                                                                                            "comorbidadeCardiovascularSivep","comorbidadeHematologiaSivep",
                                                                                            "comorbidadeSindromeDownSivep","comorbidadeAsmaSivep",
                                                                                            "comorbidadeDiabetesSivep","comorbidadeNeurologiaSivep",
                                                                                            "comorbidadePneumopatiaSivep","comorbidadeImunodeficienciaSivep",
                                                                                            "comorbidadeRenalSivep","comorbidadeObesidadeSivep"),
                                                                                  as.factor)


base_desenvolvimento = base_desenvolvimento_[,!apply(base_desenvolvimento_,2,is.constant)]

set.seed(123654)
split1<- sample(c(rep(0, 0.7 * nrow(base_desenvolvimento)), rep(1, 0.3 * nrow(base_desenvolvimento))))

train <- base_desenvolvimento[split1 == 0, ]  

test <- base_desenvolvimento[split1== 1, ]   

chisq.test(base_default$default,base_default$sexoPaciente)

mod = glm(data = base_default,formula = as.factor(default) ~ idadePaciente,family = "binomial")
summary(mod)

chisq.test(base_default$default,base_default$resultadoFinalExame)

chisq.test(base_default$default,base_default$profissionalSaude)

chisq.test(base_default$default,base_default$comorbidadeObesidadeSivep)


###### Preparando base treino


base_default_treino = train


### Treinando modelo

mod1 = glm(data = base_default_treino,formula = default ~ .,family = "binomial")

summary(mod1)



base_retreino = base_default_treino %>% dplyr::select(-c(comorbidadePuerperaSivep,comorbidadeHematologiaSivep,
                                                         comorbidadeSindromeDownSivep,
                                                         Media_habitantes_por_casa))

mod2 = glm(data = base_retreino,formula = default ~ .,family = "binomial")

summary(mod2)


#### predizendo o treino

predito = predict(mod2, base_retreino[,-1], type="response")


pred_class <- as.factor(ifelse(predito > .35, "1", "0"))

cmtrx <- confusionMatrix(pred_class,base_retreino$default,positive = "1");cmtrx


library(ROSE)

roc.curve(response = base_retreino$default,predicted = pred_class)


library(DescTools)

DescTools::PseudoR2(mod2)



######### Teste

base_teste = test %>% dplyr::select(-c(comorbidadePuerperaSivep,comorbidadeHematologiaSivep,
                                                         comorbidadeSindromeDownSivep,
                                                         Media_habitantes_por_casa)) %>%
  dplyr::select(names(mod2$data))

predito = predict(mod2, base_teste[,-1], type="response")

exp(mod2$coefficients)

pred_class <- as.factor(ifelse(predito > .35, "1", "0"))

cmtrx <- confusionMatrix(pred_class,base_teste$default,positive = "1");cmtrx




















