rm(list = ls())

library(AER)
library(ggplot2)
library(dplyr)
data('CPSSWEducation')
install.packages("usethis")
DATA_1 = CPSSWEducation

#model ln wi = B0 + B1educ + ei

model = lm(log(earnings) ~ education, data = DATA_1)
model_sum = summary(model)

Beta_1 = model$coefficients[2] 
#Beta_1 oznacza, że wraz ze wzrostem czasu edukacji o rok, zarobki na godzinę rosną o 8,8%

#istotne statystycznie dla alfa = 0,1%

# (iii) H0 = Beta_1 jest równa 0.1

Beta_1
df = df.residual(model)
Se_1 = model_sum$coefficients[2,2]
t_stat = (Beta_1 - 0.1)/Se_1
p_val <- 2 * pt(abs(t_stat), df = df, lower.tail = FALSE)  
#wartość p mniejsza od 5%, hipoteza obalona 

# (iv) H0 = Beta_1 > 0.08

t_stat_2 = (Beta_1 - 0.08)/Se_1
p_val_2 <- pt(abs(t_stat), df = df, lower.tail = FALSE)  
#wartość p mniejsza od 5%, hipoteza obalona dla przedziału ufności p = 95%

#(v) 

N = 5000
Beta_1_m2 = rep(NA,N)
for (i in 1:N)
  {
DATA_2 = sample_n(DATA_1,2950, replace = T) #alternatywnie sample(nrow(DATA_1,replace =T))
Beta_1_m2[i] = lm(log(earnings) ~ education,data = DATA_2)$coefficients[2]
  }
mean(Beta_1_m2)
sqrt(var(Beta_1_m2))



den <- density(Beta_1_m2)

plot(den,xlab="Wynik egzaminu", main="", col="violet", lwd=2)
typeof(Beta_1_m2)
  
ggplot(Beta_1_m2, aes(x = Beta_1_m2)) +
  geom_density(fill = "skyblue", alpha = 0.7) +
  labs(title = "Density plot for education estimator",
       x = "education estimator",
       y = "Density")
