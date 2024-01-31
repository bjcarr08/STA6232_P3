diabetic status (Diabetes; 0=non-diabetic, 1=diabetic) ~
  age (age; years) + 
  weight (weight; kg) + 
  hypertension status (HTN; 0=normotensive, 1=hypertensive) +
  fasting HDL categorization (hdl3cat; 0=low, 1=normal, 2=high) + 
  weight X HTN + 
  weight X hdl3cat

####


library(tidyverse)
JHS <- haven::read_sas("analysis1.sas7bdat")

JHS1 <- JHS %>% select(Diabetes, age, weight, HTN, hdl3cat) %>% na.omit()

JHS1$hdl3cat <- factor(JHS1$hdl3cat,
                       levels = c(0, 1, 2),
                       labels = c("low", "normal", "high"))

m1 <- glm(Diabetes ~ age + hdl3cat + HTN + weight + weight:HTN + weight:hdl3cat, 
          data=JHS1, 
          family="binomial"(link="logit"))

# tests significance of all terms
summary(m1)

# tests significance of predictors
car::Anova(m1, type=3)

# tests significance of the mi regression line
reduced <- glm(Diabetes ~ 1, data=JHS1, family="binomial"(link="logit"))
anova(reduced, m1, test="LRT")


# 1D
# Create the following models (i.e., plug in the following values and algebraically simplify): 
# weight: 70kg, 90kg, 110kg 

beta = summary(m1)$coeff[,"Estimate"]
x = rownames(summary(m1)$coeff)




# 1E
# Find the odds ratios for the models in 1D


# solve for the prob, which allows us to predict the prob that y_i = 1

JHS1[1,x[2]]
JHS1 <- JHS1 %>% mutate(yhat = 
                          exp



######################



library(tidyverse)
JHS <- haven::read_sas("analysis1.sas7bdat")
#DT::datatable(head(JHS, n=10), filter="top") # first 10 rows of data

# Checked for incomplete/missing cases (11 rows with missing data)
table(complete.cases(JHS %>% select(sbp, age, HSgrad, BMI)))

JHS1 <- JHS %>% select(sbp, age, HSgrad, BMI) %>% na.omit() # removed incomplete cases
m1 <- glm(sbp ~ ., data=JHS1, family="gaussian")
summary(m1) # if categories is >2 then use: car::Anova(m1, type = 3)
# install.packages("car", repos="http://cran.us.r-project.org")

confint(m1)

m0 <- glm(sbp ~ 1, data=JHS1, family="gaussian") # intercept only model
anova(m0, m1, test="F") 

c1 <- data.frame(round(summary(m1)$coeff, 3)) # m1 coefficients rounded to 3 decimal places

colnames(c1)[c(2,4)] <- c("Std.Error", "p.value") # re-named 2 columns

library(formattable)
formattable(
  c1, 
  list(
    t.value = color_tile("#FEFEB0", "#FEFEB0"),
    p.value = color_tile("#CDFFE0", "#CDFFE0")
  )
)

# single estimates for y_hat
# function prompts user to input values for the predictor vars
# then outputs a list including y_hat & the model values
y_hat <- 
  function(model.coeffs = data.frame()){
    beta_k = nrow(model.coeffs)
    beta = as.vector(model.coeffs[,1])
    var.names = rownames(model.coeffs)
    x = c(1)
    for(i in 2:beta_k){
      x[i] = readline(prompt = paste0("Enter value for ", var.names[i], ": "));
      x[i] = ifelse(is.na(x[i]), 0, 
                    ifelse(x[i] == "", 0, x[i])) # treats missing input values as zero's
    }
    df = data.frame("var" = var.names, "beta" = beta, "x" = as.vector(as.numeric(x)))
    df = df %>% mutate(beta.x = beta*x)
    return(list("y_hat" = sum(df$beta.x), "data" = df))
  }
y_hat(c1)



### 1f

## Construct an appropriate data visualization to help with explaining the model results. 
## Systolic blood pressure should be on the y-axis, age should be on the x-axis. You choose what lines to create.**


JHS1 <- JHS1 %>% 
  mutate(yhat_HSgrad.NO_BMI.20 = c1[1,1] + c1[2,1]*age + c1[3,1]*0 + c1[4,1]*20,
         yhat_HSgrad.NO_BMI.40 = c1[1,1] + c1[2,1]*age + c1[3,1]*0 + c1[4,1]*40,
         yhat_HSgrad.NO_BMI.60 = c1[1,1] + c1[2,1]*age + c1[3,1]*0 + c1[4,1]*60)

JHS1$HSgrad2 <- factor(JHS1$HSgrad, levels = c(0, 1), labels = c("No", "Yes"))

JHS1 %>% 
  ggplot(aes(x = age)) +
  geom_point(aes(y = sbp, shape = HSgrad2, fill = HSgrad2, color = HSgrad2), 
             size = 2, 
             alpha = 0.5) +
  scale_shape_manual(values = c(24, 21)) +
  scale_fill_manual(values = c("plum1", "cornsilk2")) +
  scale_color_manual(values = c("orchid4", "cornsilk4")) +
  geom_line(aes(y = yhat_HSgrad.NO_BMI.20), color = "darkorchid4", linewidth = 1.5, linetype = 3) +
  geom_line(aes(y = yhat_HSgrad.NO_BMI.40), color = "darkorchid3", linewidth = 1, linetype = 2) +
  geom_line(aes(y = yhat_HSgrad.NO_BMI.60), color = "darkorchid1", linewidth = 1, linetype = 1) +
  geom_text(aes(x = 85, y = 137, label = "BMI = 20"), color = "darkorchid4", hjust = "left") +
  geom_text(aes(x = 85, y = 142, label = "BMI = 40"), color = "darkorchid3", hjust = "left") +
  geom_text(aes(x = 85, y = 147, label = "BMI = 60"), color = "darkorchid1", hjust = "left") +
  scale_x_continuous(breaks = seq(20,90,10), limits=c(NA,90)) +
  labs(
    x = "Age", 
    y = "Systolic Blood Pressure", 
    title = expression(paste(hat(sbp)," = 95.04 + 0.44(age) - 0.84(HSgrad = 0) + 0.24(BMI: 20,40,60)")),
    color = "High School Graduate", 
    shape = "High School Graduate", 
    fill = "High School Graduate") +
  ylim(NA, 170) +
  theme_bw() +
  theme(
    title = element_text(size=12, face="bold"),
    legend.position = "top", 
    legend.justification = "left",
    plot.background = element_rect(fill="#F9F8F1"),
    legend.background = element_rect(color="cornsilk4"))





# devtools::install_github("ieb2/class_package", repos="http://cran.us.r-project.org")

library(classpackage)

anova_check(m1)

qqplot(m1)



###

(glmtoolbox::adjR2(m1))


JHS1 <- JHS1 %>% mutate(outlier = ifelse(abs(rstandard(m1)) > 2.5, "Suspected", "Not Suspected"))

formattable(
  count(JHS1, outlier) %>% mutate(outlier = factor(outlier)), 
  align = "l",
  list(
    outlier = color_tile("#F9F8F1", "paleturquoise1"),
    n = color_tile("paleturquoise1", "#F9F8F1")
  )
)

JHS1 %>% 
  ggplot(aes(x = age, y = sbp)) +
  geom_point(aes(color = outlier, fill = outlier), shape = 21, size = 2.5) +
  scale_color_manual(values = c("cornsilk3", "mediumturquoise")) +
  scale_fill_manual(values = c("cornsilk2", "paleturquoise1")) +
  labs(
    x = "Age",
    y = "Systolic Blood Pressure",
    color = "Outlier",
    fill = "Outlier") +
  theme_bw() +
  theme(
    title = element_text(size=11, face="bold"),
    legend.position = "top", 
    legend.justification = "left",
    plot.background = element_rect(fill="#F9F8F1"),
    legend.background = element_rect(color="cornsilk4"))




###

new.x = as.numeric(names(cooks.distance(m1)[cooks.distance(m1) > 0.01]))
new.y = as.vector(cooks.distance(m1)[cooks.distance(m1) > 0.01])

formattable(JHS1[new.x, -c(5:8)], align = "l")

classpackage::cooks(m1, show.threshold = T, label = F) +
  geom_point() + 
  ylim(NA, 0.02) +
  
  geom_point(aes(x = new.x[1], y = new.y[1]), size = 3, shape = 21, color = "darkorchid", fill = "darkorchid1") +
  geom_point(aes(x = new.x[2], y = new.y[2]), size = 3, shape = 21, color = "plum3", fill = "plum1") +
  geom_point(aes(x = new.x[3], y = new.y[3]), size = 3, shape = 21, color = "darkorchid", fill = "darkorchid1") +
  geom_point(aes(x = new.x[4], y = new.y[4]), size = 3, shape = 21, color = "plum3", fill = "plum1") +
  geom_point(aes(x = new.x[5], y = new.y[5]), size = 3, shape = 21, color = "darkorchid", fill = "darkorchid1") +
  geom_point(aes(x = new.x[6], y = new.y[6]), size = 3, shape = 21, color = "plum3", fill = "plum1") +
  geom_point(aes(x = new.x[7], y = new.y[7]), size = 3, shape = 21, color = "darkorchid", fill = "darkorchid1") +
  geom_point(aes(x = new.x[8], y = new.y[8]), size = 3, shape = 21, color = "plum3", fill = "plum1") +
  geom_point(aes(x = new.x[9], y = new.y[9]), size = 3, shape = 21, color = "plum3", fill = "plum1") +
  
  geom_text(aes(x = new.x[1], y = new.y[1]+0.0008, label = new.x[1]), color = "darkorchid", hjust = "center") +
  geom_text(aes(x = new.x[2], y = new.y[2]+0.0008, label = new.x[2]), color = "plum3", hjust = "center") +
  geom_text(aes(x = new.x[3], y = new.y[3]+0.0008, label = new.x[3]), color = "darkorchid", hjust = "center") +
  geom_text(aes(x = new.x[4], y = new.y[4]+0.0008, label = new.x[4]), color = "plum3", hjust = "center") +
  geom_text(aes(x = new.x[5], y = new.y[5]+0.0008, label = new.x[5]), color = "darkorchid", hjust = "center") +
  geom_text(aes(x = new.x[6], y = new.y[6]+0.0008, label = new.x[6]), color = "plum3", hjust = "center") +
  geom_text(aes(x = new.x[7], y = new.y[7]+0.0008, label = new.x[7]), color = "darkorchid", hjust = "center") +
  geom_text(aes(x = new.x[8], y = new.y[8]+0.0008, label = new.x[8]), color = "plum3", hjust = "center") +
  geom_text(aes(x = new.x[9], y = new.y[9]+0.0008, label = new.x[9]), color = "plum3", hjust = "center") +
  
  theme(
    title = element_text(size=11, face="bold"),
    plot.background = element_rect(fill="#F9F8F1"))





car::vif(m1)



classpackage::anova_check(m1)

car::qqPlot(
  MASS::stdres(m1),
  col="darkorchid", 
  col.lines="plum2")


#car::qqPlot(m1$residuals)

qqnorm(MASS::stdres(m1))
qqline(MASS::stdres(m1))

m1.diag <- boot::glm.diag(m1)
boot::glm.diag.plots(m1, m1.diag)


############



Model systolic blood pressure (*sbp*; mmHg) as a function of 
age (*age*; years), 
education (*HSgrad*; 0=no, 1=yes), 
and health status as defined by body mass index (*BMI3cat*; 0=poor health, 1=intermediate health, 2=ideal health). 
Remember to report the resulting model.

# library(tidyverse)

# check for incomplete/missing cases (11 rows with missing data)
table(complete.cases(JHS %>% select(sbp, age, HSgrad, BMI3cat)))

# remove incomplete cases
JHS2 <- JHS %>% select(sbp, age, HSgrad, BMI3cat) %>% na.omit() 

# create dummy vars for BMI3cat
JHS2 <- JHS2 %>% fastDummies::dummy_cols(select_columns = "BMI3cat")

# model - reference group: BMI3cat_0 (poor health)
m2 <- glm(sbp ~ BMI3cat_1 + BMI3cat_2 + age + HSgrad, data = JHS2, family="gaussian")
summary(m2)

# if categories is >2 then use: car::Anova(m1, type = 3)
JHS2$Health <- factor(JHS2$BMI3cat, levels = c(0, 1, 2), labels = c("Poor", "Intermediate", "Ideal"))
m2 <- glm(sbp ~ Health + age + HSgrad, data = JHS2, family="gaussian")
car::Anova(m2, type = 3)




#####################



Does the model mpg ~ . exhibit multicollinearity?
  
  The correlation matrix above (and presented again below) has several correlated covariates. disp is strongly correlated with wt (r = 0.89) and hp (r = 0.79).

m <- lm(mpg ~ ., data = mtcars)
corrplot(cor(subset(d, select = c(mpg, disp, hp, drat, wt, qsec))), type = "upper", method = "number")

# https://bookdown.org/mpfoley1973/supervised-ml/ordinary-least-squares.html#linear-regression-model




##############

Construct an appropriate data visualization to help with explaining the model results.
Systolic blood pressure should be on the y-axis, 
age should be on the x-axis. 
You choose what lines to create.


# model coefficients
#b <- summary(m2)$coefficients[,"Estimate"]

#m2_yhat <- function(x_HealthIntermediate=0, x_HealthIdeal=0, x_age=0, x_HSgrad=0){
#  b["(Intercept)"] +
#  b["HealthIntermediate"] * x_HealthIntermediate +
#  b["HealthIdeal"] * x_HealthIdeal +
#  b["age"] * x_age +
#  b["HSgrad"] * x_HSgrad 
#}
#m2_yhat(1,0,JHS2$age,0)



pal <- c("mediumorchid2", "cornsilk4", "turquoise3")
# install.packages("interactions", repos="http://cran.us.r-project.org")
interactions::interact_plot(m2, 
                            pred = age, 
                            modx = Health, 
                            mod2 = HSgrad,
                            plot.points = T, 
                            colors = pal,
                            #line.thickness = 1.5,
                            point.alpha = 0.2) + 
  ylim(NA, 170) +
  labs(title = expression(paste(
    hat(sbp),
    " = 104.36 - 2.17(", 
    Health[Intermediate], 
    ") - 3.14(", 
    Health[Ideal], 
    ") + 0.44(age) - 0.92(HSgrad)"))) +
  theme_bw() +
  theme(
    title = element_text(size = 12, face = "bold", color = colorspace::darken("cornsilk4", 0.4)),
    plot.title = element_text(size = 14, face = "bold"),
    legend.position = "top", 
    legend.justification = "left",
    legend.key.width = unit(1.5, "cm"),
    legend.key = element_rect(fill = "#F9F8F1"),
    legend.title = element_text(size = 12, color = "cornsilk4"),
    legend.text = element_text(size = 11, color = "cornsilk4"),
    legend.background = element_rect(fill = "#F9F8F1"),
    plot.background = element_rect(fill = "#F9F8F1", color = "#F9F8F1"))






###


interactions::interact_plot(m1, 
                            pred = age, 
                            modx = BMI, 
                            modx.values = c(60, 40, 20),
                            mod2 = HSgrad,
                            plot.points = T, 
                            point.size = 2,
                            colors = c("mediumorchid2", "cornsilk4", "turquoise3"),
                            point.alpha = 0.2) + 
  ylim(NA, 170) +
  #labs(title = expression(paste(hat(sbp), " = 104.36 - 2.17(", Health[Intermediate], ") - 3.14(", Health[Ideal], ") + 0.44(age) - 0.92(HSgrad)"))) +
  theme_bw() +
  theme(
    title = element_text(size = 11, face = "bold", color = colorspace::darken("cornsilk4", 0.4)),
    #plot.title = element_text(size = 12, face = "bold"),
    legend.position = "top", 
    legend.justification = "center",
    legend.key.width = unit(1.5, "cm"),
    legend.key = element_rect(fill = "#F9F8F1"),
    #legend.title = element_text(size = 11, color = "cornsilk4"),
    legend.text = element_text(size = 10, colorspace::darken("cornsilk4", 0.4)),
    legend.background = element_rect(fill = "#F9F8F1"),
    plot.background = element_rect(fill = "#F9F8F1", color = "#F9F8F1"))





###############

cbind("BMI" = JHS1$BMI, JHS2) %>% group_by(Health) %>% summarise(avg = mean(BMI), med = median(BMI))


# HSgrad not a sig predictor
# continuous age: for every 1yr increase in age, sbp increases by 0.44
# continuous BMI: for every 1 increase in BMI, sbp increases by 0.24

# did they normalize BMI to get quantiles for the health categories?

quantile(JHS1$BMI)
# 18.52813 27.02398 30.77046 35.56394 59.57043

boxplot(JHS1$BMI)
abline(h = c(22.8, 27.6, 36.6), col=2)

hist(JHS1$BMI)
abline(v = c(22.8, 27.6, 36.6), col=2)

# poor
95.0421 + (0.2399*36.6)
# 103.8224

# Intermediate  
95.0421 + (0.2399*27.6)
# 101.6633

# Ideal         
95.0421 + (0.2399*22.8)
# 100.5118


quantile(JHS1$age)
hist(JHS1$age)
abline(v = c(31, 47, 64), col=4, lwd=2)




BMI = JHS1$BMI[-boxplot.stats(JHS1$BMI)$out]
hist(log(BMI))
abline(v = quantile(log(BMI), c(0.1,0.5,0.9)), lwd=2, col=2)
abline(v = c(log(20), log(40), log(60)), lwd=2, col=4)

quantile(BMI, c(0.1,0.5,0.9))
exp(quantile(log(BMI), c(0.1,0.5,0.9)))


car::powerTransform(BMI)
# lambda = -0.5226561


m3 <- glm(sbp ~ Health + age, data = JHS2, family="gaussian")
summary(m3)


car::avPlot3d(m2, "sbp", "age")

#

y_hat <- 
  function(model.coeffs = data.frame()){
    beta_k = nrow(model.coeffs)
    beta = as.vector(model.coeffs[,1])
    var.names = rownames(model.coeffs)
    x = c(1)
    for(i in 2:beta_k){x[i] = readline(prompt = paste0("Enter value for ", var.names[i], ": "));}
    df = data.frame("var" = var.names, "beta" = beta, "x" = as.vector(as.numeric(x)))
    df = df %>% mutate(beta.x = beta*x)
    return(df)
    sum(df$beta.x)
  }
y_hat(c1)



#
