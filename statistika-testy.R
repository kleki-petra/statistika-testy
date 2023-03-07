library(readxl)
library(ggplot2)
library(tidyverse)
library(dplyr)

tab <- read_xls("vokaly.xls")
tab_cont <- table(tab$Vowel, tab$Sex)

men_a <- filter(tab, Vowel == "a", Sex == "m")
women_a <- filter(tab, Vowel == "a", Sex == "f")

shapiro.test(men_a$F2_B)
shapiro.test(women_a$F2_B)

qqnorm(men_a$F2_B)
qqnorm(women_a$F2_B)

x <- mean(women_a$F2_B)
y <- mean(men_a$F2_B)

t.test(women_a$F2_B, men_a$F2_B, paired = FALSE, conf.level = 0.95)

p <- min(1, 2*pbinom(5, 40, 0.5))
p

r2013 <- c(5, 15, 16) 
r2014 <- c(7, 7, 10)

tabulka <- rbind(r2013, r2014)
cnames <- c("intonace", "hlasky", "IPA")
colnames(tabulka) <- cnames
tabulka

chisq.test(tabulka)
fisher.test(tabulka)

# skupina 1: 88
# skupina 2: 95
# skupina 3: 118
# velikost souboru 240

binom.test(88, 240)$conf.int
binom.test(95, 240)$conf.int
binom.test(118, 240)$conf.int

