#создайте модель множественной линейной регрессии потоков ночных потоков паров воды за период 2013 года 
#по данным измерений методом турбулентной пульсации
# Для выбора нужных суток используйте переменную DOY - день года (1 января - DOY = 1)

#подключаем tidyverse
library(tidyverse)
#читаем данные из файла, пропускаем первую строку, заменяем текстовые 'NA', пустые и сгенерированные пороговые значения на NA, игнорируем строки с "[" 
data = read_csv("eddypro.csv", skip = 1, na =c("","NA","-9999","-9999.0"), comment=c("["))
data = data[-1,]
#убираем ненужные колонки
data = data[, c(-1, -3, -9, -12, -15, -18, -21, -30, -35, -63 , -70, -88:-99) ]
#преобразуем строковые значения в факторные
data = data %>% mutate_if(is.character, factor)
#заменяем конфликтующие знаки колонок
names(data) = names(data) %>% str_replace_all("[!]","_emph_") %>%
  str_replace_all("[?]","_quest_") %>%  
  str_replace_all("[*]","_star_") %>%  
  str_replace_all("[+]","_plus_") %>% 
  str_replace_all("[-]","_minus_") %>% 
  str_replace_all("[@]","_at_") %>% 
  str_replace_all("[$]","_dollar_") %>%
  str_replace_all("[#]","_hash_") %>% 
  str_replace_all("[/]","_div_") %>% 
  str_replace_all("[%]","_perc_") %>% 
  str_replace_all("[&]","_amp_") %>% 
  str_replace_all("[\\^]","_power_") %>% 
  str_replace_all("[()]","_") 
#Посмотрим, что получилось
glimpse(data)
#изменим тип данных колонки daytime 
data$daytime = as.logical(data$daytime)
#оставим данные только по ночным измерениям 2013 года:
data = data[data$daytime == FALSE, c(1:ncol(data))] 
#выберем все переменные типа numeric
data_numeric = data[,sapply(data,is.numeric) ]
#все остальные переменные:
data_non_numeric = data[,!sapply(data,is.numeric) ]
# создадим матрицу для корелляционного анализа и преобразуем ее в таблицу, выбрав нужный столбец (потоки паров воды)
cor_td = cor(drop_na(data_numeric)) %>% as.data.frame %>% select(h2o_flux)
#выберем имена переменных (строк) с коэффициентом детерминации больше 0.1
vars = row.names(cor_td)[cor_td$h2o_flux^2 > .1] %>% na.exclude; vars
#соберем переменные из вектора в одну формулу:
formula = as.formula(paste("h2o_flux~", paste(vars,collapse = "+"), sep="")); formula
#создадим обучающую и тестирующую выборки:
row_numbers = 1:length(data$date)
teach = sample(row_numbers, floor(length(data$date)*.7))
test = row_numbers[-teach]
#непересекающиеся подвыборки:
teaching_tbl_unq = data[teach,]
testing_tbl_unq = data[test,]
# МОДЕЛЬ 1
#создаем модель линейной регрессии
model1 = lm(formula, data = data);model
#коэффициенты
coef(model1)
#остатки
resid(model1)
#доверительный интервал
confint(model1)
#P-значения по модели
summary(model1)
#дисперсионный анализ
anova(model1)
#графическое представление модели:
plot(model1)

# МОДЕЛЬ 2
formula = as.formula(paste("h2o_flux~", "(", paste(vars,collapse = "+"), ")^2", sep="", collapse = NULL));formula
#создаем модель линейной регрессии
model2 = lm(formula, data = data);model
#коэффициенты
coef(model2)
#остатки
resid(model2)
#доверительный интервал
confint(model2)
#P-значения по модели
summary(model2)
#дисперсионный анализ
anova(model2)
#графическое представление модели:


# МОДЕЛЬ 3
formula2 = h2o_flux~rand_err_LE+LE+rand_err_LE+rand_err_h2o_flux+h2o_time_lag+sonic_temperature+air_temperature+air_density+air_molar_volume+es+RH+VPD+u_star_+TKE+un_LE+un_h2o_flux+w_div_h2o_cov
#создаем модель линейной регрессии
model3 = lm(formula2, data = data);model2
#коэффициенты
coef(model3)
#остатки
resid(model3)
#доверительный интервал
confint(model3)
#P-значения по модели
summary(model3)
#дисперсионный анализ
anova(model3)
#графическое представление модели:

# МОДЕЛЬ 4
formula3 = h2o_flux~LE+rand_err_LE+air_density+air_molar_volume+es+RH+VPD+un_LE+un_h2o_flux+w_div_h2o_cov
#создаем модель линейной регрессии
model4 = lm(formula3, data = data);model3
#коэффициенты
coef(model4)
#остатки
resid(model4)
#доверительный интервал
confint(model4)
#P-значения по модели
summary(model4)
#дисперсионный анализ
anova(model4)
#графическое представление модели:

formula4 = h2o_flux~VPD+LE
model5= lm(formula4,data=data)
summary(model5)
