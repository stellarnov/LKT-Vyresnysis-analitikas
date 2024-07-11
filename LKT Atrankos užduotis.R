# Funkcijų paketai
library(dplyr)

# Duomenų importavimas (duomenys jau buvo patvarkyti su Excel)
duomenys <- read.csv('LKT_Kultura_w_datamap_ITanalize.csv')

# Sukuriu svoriu kintamąjį dalyvavusiems ir nedalyvavusiems,
# kad vėliau paskaičiuoti populiacijos rodyklius
duomenys <- mutate(duomenys,
                   lank_W = case_when(
                       S6_2==1 ~ W,
                       S6_2==0 ~ 0,
                       TRUE ~ NA))

# Patikrinu, ar viskas gerai su duomenimis paskaičius bendrą dalį
sum(duomenys$lank_W)/sum(duomenys$W)
mean(duomenys$S6_2)
# Viskas atitinka pavyzdžio

# Sustambinu duomenys iki apskričių
duomenys.apskritis <- duomenys %>% 
  group_by(X4) %>%
  summarise(across(ends_with('W'), sum))

# Paskaičiuoju pasvertą besilankančių procentą kiekviename apskrityje
duomenys.apskritis$lank = duomenys.apskritis$lank_W/duomenys.apskritis$W

# Ištrinu nereikalingus stulpelius
duomenys.apskritis <- subset(duomenys.apskritis, select = -c(W, lank_W))

# Ranguoju pagal besilankusių procentą ir rūšiuoju
duomenys.apskritis$rang = rank(-duomenys.apskritis$lank)
duomenys.apskritis <- duomenys.apskritis[order(duomenys.apskritis$rang),]

# Keičiu stulpeliu eiliškumą
duomenys.apskritis <- duomenys.apskritis %>% 
  relocate(rang, .before="X4")






