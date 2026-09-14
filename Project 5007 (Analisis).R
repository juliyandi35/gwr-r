library(dplyr)
library(readxl)
# Import data Excel
Unmeet_Need <- read_excel("Unmeet Need summary.xlsx")
Category_Summary <- read_excel("Category Summary.xlsx")
names(Unmeet_Need)
names(Category_Summary)
df <- data.frame(Category_Summary,Unmeet_Need$Persentase)
colnames(df) <- c(names(Category_Summary),"Persentase.Unmeet.Need")

# Import data Peta
library(sf)
Map <- read_sf("Peta Kabupaten/BATAS KABUPATEN KOTA DESEMBER 2019 DUKCAPIL.shp")
head(Map)
colnames(Map) <- c("Kabupaten","geometry")

Dataset<-merge(df,Map,by="Kabupaten")

library(spgwr)
colex0 <- lm(Persentase.Unmeet.Need~.,data=df[,-1])
summary(colex0)

resid<-residuals(colex0)
par(mfrow=c(2,2))
qqnorm(resid); qqline(resid, col="red"); 
plot(resid~fitted(colex0),xlab = "Predicted Values",ylab = "Residuals")
abline(h=0, col="red")
hist(resid) #histogram utk residual
plot(1:nrow(df[,-1]), resid, pch=20,type="b")
abline(h=0, col="red")

shapiro.test(resid) # Normality test

lmtest::bptest(colex0) # Heteroskedastisity test

Dataset <- st_as_sf(Dataset)

library(spdep)
library(sp)
coords<-data.frame(x = coordinates(as(Dataset,"Spatial"))[,1], y = coordinates(as(Dataset,"Spatial"))[,2])
jarak<-as.matrix(1/dist(coords))

## Basic GWR
# Menentukan bandwidth optimal
library(GWmodel)
# determine the kernel bandwidth

bw <- bw.gwr(Persentase.Unmeet.Need~C1+C2+C3+C4+C5+C6+C7+C8+C9+C10+C11+C12+C13,
             approach = "AIC",
             adaptive = T,
             data=as(Dataset,"Spatial"))

# Modelling
m.gwr <- gwr.basic(Persentase.Unmeet.Need~C1+C2+C3+C4+C5+C6+C7+C8+C9+C10+C11+C12+C13,
                   adaptive = T,
                   data=as(Dataset,"Spatial"),
                   bw = bw)
# Evaluation
summary(m.gwr$SDF)
gwr_sf = st_as_sf(m.gwr$SDF)
writexl::write_xlsx(gwr_sf,"Hasil GWR.xlsx")

Dataset$tval.C1 <- gwr_sf$C1_TV
Dataset$tval.C2<- gwr_sf$C2_TV
Dataset$tval.C3 <- gwr_sf$C3_TV
Dataset$tval.C4 <- gwr_sf$C4_TV
Dataset$tval.C5<- gwr_sf$C5_TV
Dataset$tval.C6 <- gwr_sf$C6_TV
Dataset$tval.C7 <- gwr_sf$C7_TV
Dataset$tval.C8 <- gwr_sf$C8_TV
Dataset$tval.C9 <- gwr_sf$C9_TV
Dataset$tval.C10 <- gwr_sf$C10_TV
Dataset$tval.C11 <- gwr_sf$C11_TV
Dataset$tval.C12 <- gwr_sf$C12_TV
Dataset$tval.C13 <- gwr_sf$C13_TV

library(ggplot2)
#T tabel yang didapat dengan DF=492-14 dan alpha = 0.05 two tailed adalah 1.964981363

#---------------------------------------------------------------#
#    signfikansi variabel (variabel C1)
#---------------------------------------------------------------#
Dataset$signfikansi_C1 <- NA
# Signifikan
Dataset[(Dataset$tval.C1 <= -1.964981363 | Dataset$tval.C1 >= 1.964981363), "signfikansi_C1"] <- "Signifikan"

# Tidak Signifikan
Dataset[(Dataset$tval.C1 > -1.964981363 & Dataset$tval.C1 < 1.964981363), "signfikansi_C1"] <- "Tidak Signifikan"

#------------------------------------------------
#Gabung data GWR dengan SHP
ggplot(data=Dataset) +
  geom_sf(mapping=aes(fill =signfikansi_C1)) +
  scale_fill_manual(values = c("#28E2E5", "#DF536B"))+
  labs(fill="signfikansi")+
  geom_text(
    aes(label = Kabupaten, x = coordinates(as(Dataset,"Spatial"))[,1], y = coordinates(as(Dataset,"Spatial"))[,2]),
    vjust = -0.5,
    color = "black",
    size = 1,
    check_overlap = TRUE
  )+ggtitle("Signifikansi C1")+xlab("Longitude")+ylab("Latitude")

#---------------------------------------------------------------#
#    signfikansi variabel (variabel C2)
#---------------------------------------------------------------#
Dataset$signfikansi_C2 <- NA
# Signifikan
Dataset[(Dataset$tval.C2 <= -1.964981363 | Dataset$tval.C2 >= 1.964981363), "signfikansi_C2"] <- "Signifikan"

# Tidak Signifikan
Dataset[(Dataset$tval.C2 > -1.964981363 & Dataset$tval.C2 < 1.964981363), "signfikansi_C2"] <- "Tidak Signifikan"

#------------------------------------------------
#Gabung data GWR dengan SHP
ggplot(data=Dataset) +
  geom_sf(mapping=aes(fill =signfikansi_C2)) +
  scale_fill_manual(values = c("#28E2E5", "#DF536B"))+
  labs(fill="signfikansi")+
  geom_text(
    aes(label = Kabupaten, x = coordinates(as(Dataset,"Spatial"))[,1], y = coordinates(as(Dataset,"Spatial"))[,2]),
    vjust = -0.5,
    color = "black",
    size = 1,
    check_overlap = TRUE
  )+ggtitle("Signifikansi C2")+xlab("Longitude")+ylab("Latitude")

#---------------------------------------------------------------#
#    signfikansi variabel (variabel C3)
#---------------------------------------------------------------#
Dataset$signfikansi_C3 <- NA
# Signifikan
Dataset[(Dataset$tval.C3 <= -1.964981363 | Dataset$tval.C3 >= 1.964981363), "signfikansi_C3"] <- "Signifikan"

# Tidak Signifikan
Dataset[(Dataset$tval.C3 > -1.964981363 & Dataset$tval.C3 < 1.964981363), "signfikansi_C3"] <- "Tidak Signifikan"

#------------------------------------------------
#Gabung data GWR dengan SHP
ggplot(data=Dataset) +
  geom_sf(mapping=aes(fill =signfikansi_C3)) +
  scale_fill_manual(values = c("#28E2E5", "#DF536B"))+
  labs(fill="signfikansi")+
  geom_text(
    aes(label = Kabupaten, x = coordinates(as(Dataset,"Spatial"))[,1], y = coordinates(as(Dataset,"Spatial"))[,2]),
    vjust = -0.5,
    color = "black",
    size = 1,
    check_overlap = TRUE
  )+ggtitle("Signifikansi C3")+xlab("Longitude")+ylab("Latitude")

#---------------------------------------------------------------#
#    signfikansi variabel (variabel C4)
#---------------------------------------------------------------#
Dataset$signfikansi_C4 <- NA
# Signifikan
Dataset[(Dataset$tval.C4 <= -1.964981363 | Dataset$tval.C4 >= 1.964981363), "signfikansi_C4"] <- "Signifikan"

# Tidak Signifikan
Dataset[(Dataset$tval.C4 > -1.964981363 & Dataset$tval.C4 < 1.964981363), "signfikansi_C4"] <- "Tidak Signifikan"

#------------------------------------------------
#Gabung data GWR dengan SHP
ggplot(data=Dataset) +
  geom_sf(mapping=aes(fill =signfikansi_C4)) +
  scale_fill_manual(values = c("#28E2E5", "#DF536B"))+
  labs(fill="signfikansi")+
  geom_text(
    aes(label = Kabupaten, x = coordinates(as(Dataset,"Spatial"))[,1], y = coordinates(as(Dataset,"Spatial"))[,2]),
    vjust = -0.5,
    color = "black",
    size = 1,
    check_overlap = TRUE
  )+ggtitle("Signifikansi C4")+xlab("Longitude")+ylab("Latitude")

#---------------------------------------------------------------#
#    signfikansi variabel (variabel C5)
#---------------------------------------------------------------#
Dataset$signfikansi_C5 <- NA
# Signifikan
Dataset[(Dataset$tval.C5 <= -1.964981363 | Dataset$tval.C5 >= 1.964981363), "signfikansi_C5"] <- "Signifikan"

# Tidak Signifikan
Dataset[(Dataset$tval.C5 > -1.964981363 & Dataset$tval.C5 < 1.964981363), "signfikansi_C5"] <- "Tidak Signifikan"

#------------------------------------------------
#Gabung data GWR dengan SHP
ggplot(data=Dataset) +
  geom_sf(mapping=aes(fill =signfikansi_C5)) +
  scale_fill_manual(values = c("#28E2E5", "#DF536B"))+
  labs(fill="signfikansi")+
  geom_text(
    aes(label = Kabupaten, x = coordinates(as(Dataset,"Spatial"))[,1], y = coordinates(as(Dataset,"Spatial"))[,2]),
    vjust = -0.5,
    color = "black",
    size = 1,
    check_overlap = TRUE
  )+ggtitle("Signifikansi C5")+xlab("Longitude")+ylab("Latitude")

#---------------------------------------------------------------#
#    signfikansi variabel (variabel C6)
#---------------------------------------------------------------#
Dataset$signfikansi_C6 <- NA
# Signifikan
Dataset[(Dataset$tval.C6 <= -1.964981363 | Dataset$tval.C6 >= 1.964981363), "signfikansi_C6"] <- "Signifikan"

# Tidak Signifikan
Dataset[(Dataset$tval.C6 > -1.964981363 & Dataset$tval.C6 < 1.964981363), "signfikansi_C6"] <- "Tidak Signifikan"

#------------------------------------------------
#Gabung data GWR dengan SHP
ggplot(data=Dataset) +
  geom_sf(mapping=aes(fill =signfikansi_C6)) +
  scale_fill_manual(values = c("#DF536B"))+
  labs(fill="signfikansi")+
  geom_text(
    aes(label = Kabupaten, x = coordinates(as(Dataset,"Spatial"))[,1], y = coordinates(as(Dataset,"Spatial"))[,2]),
    vjust = -0.5,
    color = "black",
    size = 1,
    check_overlap = TRUE
  )+ggtitle("Signifikansi C6")+xlab("Longitude")+ylab("Latitude")

#---------------------------------------------------------------#
#    signfikansi variabel (variabel C7)
#---------------------------------------------------------------#
Dataset$signfikansi_C7 <- NA
# Signifikan
Dataset[(Dataset$tval.C7 <= -1.964981363 | Dataset$tval.C7 >= 1.964981363), "signfikansi_C7"] <- "Signifikan"

# Tidak Signifikan
Dataset[(Dataset$tval.C7 > -1.964981363 & Dataset$tval.C7 < 1.964981363), "signfikansi_C7"] <- "Tidak Signifikan"

#------------------------------------------------
#Gabung data GWR dengan SHP
ggplot(data=Dataset) +
  geom_sf(mapping=aes(fill =signfikansi_C7)) +
  scale_fill_manual(values = c("#28E2E5", "#DF536B"))+
  labs(fill="signfikansi")+
  geom_text(
    aes(label = Kabupaten, x = coordinates(as(Dataset,"Spatial"))[,1], y = coordinates(as(Dataset,"Spatial"))[,2]),
    vjust = -0.5,
    color = "black",
    size = 1,
    check_overlap = TRUE
  )+ggtitle("Signifikansi C7")+xlab("Longitude")+ylab("Latitude")

#---------------------------------------------------------------#
#    signfikansi variabel (variabel C8)
#---------------------------------------------------------------#
Dataset$signfikansi_C8 <- NA
# Signifikan
Dataset[(Dataset$tval.C8 <= -1.964981363 | Dataset$tval.C8 >= 1.964981363), "signfikansi_C8"] <- "Signifikan"

# Tidak Signifikan
Dataset[(Dataset$tval.C8 > -1.964981363 & Dataset$tval.C8 < 1.964981363), "signfikansi_C8"] <- "Tidak Signifikan"

#------------------------------------------------
#Gabung data GWR dengan SHP
ggplot(data=Dataset) +
  geom_sf(mapping=aes(fill =signfikansi_C8)) +
  scale_fill_manual(values = c("#28E2E5", "#DF536B"))+
  labs(fill="signfikansi")+
  geom_text(
    aes(label = Kabupaten, x = coordinates(as(Dataset,"Spatial"))[,1], y = coordinates(as(Dataset,"Spatial"))[,2]),
    vjust = -0.5,
    color = "black",
    size = 1,
    check_overlap = TRUE
  )+ggtitle("Signifikansi C8")+xlab("Longitude")+ylab("Latitude")

#---------------------------------------------------------------#
#    signfikansi variabel (variabel C9)
#---------------------------------------------------------------#
Dataset$signfikansi_C9 <- NA
# Signifikan
Dataset[(Dataset$tval.C9 <= -1.964981363 | Dataset$tval.C9 >= 1.964981363), "signfikansi_C9"] <- "Signifikan"

# Tidak Signifikan
Dataset[(Dataset$tval.C9 > -1.964981363 & Dataset$tval.C9 < 1.964981363), "signfikansi_C9"] <- "Tidak Signifikan"

#------------------------------------------------
#Gabung data GWR dengan SHP
ggplot(data=Dataset) +
  geom_sf(mapping=aes(fill =signfikansi_C9)) +
  scale_fill_manual(values = c("#28E2E5", "#DF536B"))+
  labs(fill="signfikansi")+
  geom_text(
    aes(label = Kabupaten, x = coordinates(as(Dataset,"Spatial"))[,1], y = coordinates(as(Dataset,"Spatial"))[,2]),
    vjust = -0.5,
    color = "black",
    size = 1,
    check_overlap = TRUE
  )+ggtitle("Signifikansi C9")+xlab("Longitude")+ylab("Latitude")

#---------------------------------------------------------------#
#    signfikansi variabel (variabel C10)
#---------------------------------------------------------------#
Dataset$signfikansi_C10 <- NA
# Signifikan
Dataset[(Dataset$tval.C10 <= -1.964981363 | Dataset$tval.C10 >= 1.964981363), "signfikansi_C10"] <- "Signifikan"

# Tidak Signifikan
Dataset[(Dataset$tval.C10 > -1.964981363 & Dataset$tval.C10 < 1.964981363), "signfikansi_C10"] <- "Tidak Signifikan"

#------------------------------------------------
#Gabung data GWR dengan SHP
ggplot(data=Dataset) +
  geom_sf(mapping=aes(fill =signfikansi_C10)) +
  scale_fill_manual(values = c("#28E2E5", "#DF536B"))+
  labs(fill="signfikansi")+
  geom_text(
    aes(label = Kabupaten, x = coordinates(as(Dataset,"Spatial"))[,1], y = coordinates(as(Dataset,"Spatial"))[,2]),
    vjust = -0.5,
    color = "black",
    size = 1,
    check_overlap = TRUE
  )+ggtitle("Signifikansi C10")+xlab("Longitude")+ylab("Latitude")

#---------------------------------------------------------------#
#    signfikansi variabel (variabel C11)
#---------------------------------------------------------------#
Dataset$signfikansi_C11 <- NA
# Signifikan
Dataset[(Dataset$tval.C11 <= -1.964981363 | Dataset$tval.C11 >= 1.964981363), "signfikansi_C11"] <- "Signifikan"

# Tidak Signifikan
Dataset[(Dataset$tval.C11 > -1.964981363 & Dataset$tval.C11 < 1.964981363), "signfikansi_C11"] <- "Tidak Signifikan"

#------------------------------------------------
#Gabung data GWR dengan SHP
ggplot(data=Dataset) +
  geom_sf(mapping=aes(fill =signfikansi_C11)) +
  scale_fill_manual(values = c("#28E2E5", "#DF536B"))+
  labs(fill="signfikansi")+
  geom_text(
    aes(label = Kabupaten, x = coordinates(as(Dataset,"Spatial"))[,1], y = coordinates(as(Dataset,"Spatial"))[,2]),
    vjust = -0.5,
    color = "black",
    size = 1,
    check_overlap = TRUE
  )+ggtitle("Signifikansi C11")+xlab("Longitude")+ylab("Latitude")

#---------------------------------------------------------------#
#    signfikansi variabel (variabel C12)
#---------------------------------------------------------------#
Dataset$signfikansi_C12 <- NA
# Signifikan
Dataset[(Dataset$tval.C12 <= -1.964981363 | Dataset$tval.C12 >= 1.964981363), "signfikansi_C12"] <- "Signifikan"

# Tidak Signifikan
Dataset[(Dataset$tval.C12 > -1.964981363 & Dataset$tval.C12 < 1.964981363), "signfikansi_C12"] <- "Tidak Signifikan"

#------------------------------------------------
#Gabung data GWR dengan SHP
ggplot(data=Dataset) +
  geom_sf(mapping=aes(fill =signfikansi_C12)) +
  scale_fill_manual(values = c("#28E2E5", "#DF536B"))+
  labs(fill="signfikansi")+
  geom_text(
    aes(label = Kabupaten, x = coordinates(as(Dataset,"Spatial"))[,1], y = coordinates(as(Dataset,"Spatial"))[,2]),
    vjust = -0.5,
    color = "black",
    size = 1,
    check_overlap = TRUE
  )+ggtitle("Signifikansi C12")+xlab("Longitude")+ylab("Latitude")

#---------------------------------------------------------------#
#    signfikansi variabel (variabel C13)
#---------------------------------------------------------------#
Dataset$signfikansi_C13 <- NA
# Signifikan
Dataset[(Dataset$tval.C13 <= -1.964981363 | Dataset$tval.C13 >= 1.964981363), "signfikansi_C13"] <- "Signifikan"

# Tidak Signifikan
Dataset[(Dataset$tval.C13 > -1.964981363 & Dataset$tval.C13 < 1.964981363), "signfikansi_C13"] <- "Tidak Signifikan"

#------------------------------------------------
#Gabung data GWR dengan SHP
ggplot(data=Dataset) +
  geom_sf(mapping=aes(fill =signfikansi_C13)) +
  scale_fill_manual(values = c("#28E2E5", "#DF536B"))+
  labs(fill="signfikansi")+
  geom_text(
    aes(label = Kabupaten, x = coordinates(as(Dataset,"Spatial"))[,1], y = coordinates(as(Dataset,"Spatial"))[,2]),
    vjust = -0.5,
    color = "black",
    size = 1,
    check_overlap = TRUE
  )+ggtitle("Signifikansi C13")+xlab("Longitude")+ylab("Latitude")

#---------------------------------------------------------------#
#    Plotting variabel signifikan
#---------------------------------------------------------------#
Significant_Map <- Dataset

# Buat kolom baru untuk kombinasi signfikansi
Significant_Map <- Significant_Map %>%
  mutate(Variabel_Signifikan = case_when(
    # Utuh
    signfikansi_C1 == "Signifikan" & signfikansi_C2 == "Signifikan" &
      signfikansi_C3 == "Signifikan" & signfikansi_C4 == "Signifikan" &
      signfikansi_C5 == "Signifikan" & signfikansi_C6 == "Signifikan" &
      signfikansi_C7 == "Signifikan" & signfikansi_C8 == "Signifikan" &
      signfikansi_C9 == "Signifikan" & signfikansi_C10 == "Signifikan" &
      signfikansi_C11 == "Signifikan" & signfikansi_C12 == "Signifikan" &
      signfikansi_C13 == "Signifikan" ~ "Seluruh Kategori",
    
    # Eliminasi 1
    signfikansi_C1 == "Signifikan" & signfikansi_C2 == "Signifikan" &
      signfikansi_C3 == "Signifikan" & signfikansi_C4 == "Signifikan" &
      signfikansi_C5 == "Signifikan" & signfikansi_C6 == "Signifikan" &
      signfikansi_C7 == "Signifikan" & signfikansi_C8 == "Signifikan" &
      signfikansi_C9 == "Signifikan" & signfikansi_C10 == "Signifikan" &
      signfikansi_C11 == "Signifikan" & signfikansi_C12 == "Signifikan" ~ "C1,C2,C3,C4,C5,C6,C7,C8,C9,C10,C11,C12",
    
    signfikansi_C1 == "Signifikan" & signfikansi_C2 == "Signifikan" &
      signfikansi_C3 == "Signifikan" & signfikansi_C4 == "Signifikan" &
      signfikansi_C5 == "Signifikan" & signfikansi_C6 == "Signifikan" &
      signfikansi_C7 == "Signifikan" & signfikansi_C8 == "Signifikan" &
      signfikansi_C9 == "Signifikan" & signfikansi_C10 == "Signifikan" &
      signfikansi_C11 == "Signifikan" &
      signfikansi_C13 == "Signifikan" ~ "C1,C2,C3,C4,C5,C6,C7,C8,C9,C10,C11,C13",
    
    signfikansi_C1 == "Signifikan" & signfikansi_C2 == "Signifikan" &
      signfikansi_C3 == "Signifikan" & signfikansi_C4 == "Signifikan" &
      signfikansi_C5 == "Signifikan" & signfikansi_C6 == "Signifikan" &
      signfikansi_C7 == "Signifikan" & signfikansi_C8 == "Signifikan" &
      signfikansi_C9 == "Signifikan" & signfikansi_C10 == "Signifikan" &
      signfikansi_C12 == "Signifikan" &
      signfikansi_C13 == "Signifikan" ~ "C1,C2,C3,C4,C5,C6,C7,C8,C9,C10,C12,C13",
    
    signfikansi_C1 == "Signifikan" & signfikansi_C2 == "Signifikan" &
      signfikansi_C3 == "Signifikan" & signfikansi_C4 == "Signifikan" &
      signfikansi_C5 == "Signifikan" & signfikansi_C6 == "Signifikan" &
      signfikansi_C7 == "Signifikan" & signfikansi_C8 == "Signifikan" &
      signfikansi_C9 == "Signifikan" &
      signfikansi_C11 == "Signifikan" & signfikansi_C12 == "Signifikan" &
      signfikansi_C13 == "Signifikan" ~ "C1,C2,C3,C4,C5,C6,C7,C8,C9,C11,C12,C13",
    
    signfikansi_C1 == "Signifikan" & signfikansi_C2 == "Signifikan" &
      signfikansi_C3 == "Signifikan" & signfikansi_C4 == "Signifikan" &
      signfikansi_C5 == "Signifikan" & signfikansi_C6 == "Signifikan" &
      signfikansi_C7 == "Signifikan" & signfikansi_C8 == "Signifikan" &
      signfikansi_C10 == "Signifikan" &
      signfikansi_C11 == "Signifikan" & signfikansi_C12 == "Signifikan" &
      signfikansi_C13 == "Signifikan" ~ "C1,C2,C3,C4,C5,C6,C7,C8,C10,C11,C12,C13",
    
    signfikansi_C1 == "Signifikan" & signfikansi_C2 == "Signifikan" &
      signfikansi_C3 == "Signifikan" & signfikansi_C4 == "Signifikan" &
      signfikansi_C5 == "Signifikan" & signfikansi_C6 == "Signifikan" &
      signfikansi_C7 == "Signifikan" &
      signfikansi_C9 == "Signifikan" & signfikansi_C10 == "Signifikan" &
      signfikansi_C11 == "Signifikan" & signfikansi_C12 == "Signifikan" &
      signfikansi_C13 == "Signifikan" ~ "C1,C2,C3,C4,C5,C6,C7,C9,C10,C11,C12,C13",
    
    signfikansi_C1 == "Signifikan" & signfikansi_C2 == "Signifikan" &
      signfikansi_C3 == "Signifikan" & signfikansi_C4 == "Signifikan" &
      signfikansi_C5 == "Signifikan" & signfikansi_C6 == "Signifikan" &
      signfikansi_C8 == "Signifikan" &
      signfikansi_C9 == "Signifikan" & signfikansi_C10 == "Signifikan" &
      signfikansi_C11 == "Signifikan" & signfikansi_C12 == "Signifikan" &
      signfikansi_C13 == "Signifikan" ~ "C1,C2,C3,C4,C5,C6,C8,C9,C10,C11,C12,C13",
    
    signfikansi_C1 == "Signifikan" & signfikansi_C2 == "Signifikan" &
      signfikansi_C3 == "Signifikan" & signfikansi_C4 == "Signifikan" &
      signfikansi_C5 == "Signifikan" & 
      signfikansi_C7 == "Signifikan" & signfikansi_C8 == "Signifikan" &
      signfikansi_C9 == "Signifikan" & signfikansi_C10 == "Signifikan" &
      signfikansi_C11 == "Signifikan" & signfikansi_C12 == "Signifikan" &
      signfikansi_C13 == "Signifikan" ~ "C1,C2,C3,C4,C5,C7,C8,C9,C10,C11,C12,C13",
    
    signfikansi_C1 == "Signifikan" & signfikansi_C2 == "Signifikan" &
      signfikansi_C3 == "Signifikan" & signfikansi_C4 == "Signifikan" &
      signfikansi_C6 == "Signifikan" &
      signfikansi_C7 == "Signifikan" & signfikansi_C8 == "Signifikan" &
      signfikansi_C9 == "Signifikan" & signfikansi_C10 == "Signifikan" &
      signfikansi_C11 == "Signifikan" & signfikansi_C12 == "Signifikan" &
      signfikansi_C13 == "Signifikan" ~ "C1,C2,C3,C4,C6,C7,C8,C9,C10,C11,C12,C13",
    
    signfikansi_C1 == "Signifikan" & signfikansi_C2 == "Signifikan" &
      signfikansi_C3 == "Signifikan" &
      signfikansi_C5 == "Signifikan" & signfikansi_C6 == "Signifikan" &
      signfikansi_C7 == "Signifikan" & signfikansi_C8 == "Signifikan" &
      signfikansi_C9 == "Signifikan" & signfikansi_C10 == "Signifikan" &
      signfikansi_C11 == "Signifikan" & signfikansi_C12 == "Signifikan" &
      signfikansi_C13 == "Signifikan" ~ "C1,C2,C3,C5,C6,C7,C8,C9,C10,C11,C12,C13",
    
    signfikansi_C1 == "Signifikan" & signfikansi_C2 == "Signifikan" &
      signfikansi_C4 == "Signifikan" &
      signfikansi_C5 == "Signifikan" & signfikansi_C6 == "Signifikan" &
      signfikansi_C7 == "Signifikan" & signfikansi_C8 == "Signifikan" &
      signfikansi_C9 == "Signifikan" & signfikansi_C10 == "Signifikan" &
      signfikansi_C11 == "Signifikan" & signfikansi_C12 == "Signifikan" &
      signfikansi_C13 == "Signifikan" ~ "C1,C2,C4,C5,C6,C7,C8,C9,C10,C11,C12,C13",
    
    signfikansi_C1 == "Signifikan" & 
      signfikansi_C3 == "Signifikan" & signfikansi_C4 == "Signifikan" &
      signfikansi_C5 == "Signifikan" & signfikansi_C6 == "Signifikan" &
      signfikansi_C7 == "Signifikan" & signfikansi_C8 == "Signifikan" &
      signfikansi_C9 == "Signifikan" & signfikansi_C10 == "Signifikan" &
      signfikansi_C11 == "Signifikan" & signfikansi_C12 == "Signifikan" &
      signfikansi_C13 == "Signifikan" ~ "C1,C3,C4,C5,C6,C7,C8,C9,C10,C11,C12,C13",
    
    signfikansi_C2 == "Signifikan" &
      signfikansi_C3 == "Signifikan" & signfikansi_C4 == "Signifikan" &
      signfikansi_C5 == "Signifikan" & signfikansi_C6 == "Signifikan" &
      signfikansi_C7 == "Signifikan" & signfikansi_C8 == "Signifikan" &
      signfikansi_C9 == "Signifikan" & signfikansi_C10 == "Signifikan" &
      signfikansi_C11 == "Signifikan" & signfikansi_C12 == "Signifikan" &
      signfikansi_C13 == "Signifikan" ~ "C2,C3,C4,C5,C6,C7,C8,C9,C10,C11,C12,C13",
    
    # Eliminasi 2
    signfikansi_C1 == "Signifikan" & signfikansi_C2 == "Signifikan" &
      signfikansi_C3 == "Signifikan" & signfikansi_C4 == "Signifikan" &
      signfikansi_C5 == "Signifikan" & signfikansi_C6 == "Signifikan" &
      signfikansi_C7 == "Signifikan" & signfikansi_C8 == "Signifikan" &
      signfikansi_C9 == "Signifikan" & signfikansi_C10 == "Signifikan" &
      signfikansi_C11 == "Signifikan" ~ "C1,C2,C3,C4,C5,C6,C7,C8,C9,C10,C11",
    
    signfikansi_C1 == "Signifikan" & signfikansi_C2 == "Signifikan" &
      signfikansi_C3 == "Signifikan" & signfikansi_C4 == "Signifikan" &
      signfikansi_C5 == "Signifikan" & signfikansi_C6 == "Signifikan" &
      signfikansi_C7 == "Signifikan" & signfikansi_C8 == "Signifikan" &
      signfikansi_C9 == "Signifikan" & signfikansi_C10 == "Signifikan" &
      signfikansi_C13 == "Signifikan" ~ "C1,C2,C3,C4,C5,C6,C7,C8,C9,C10,C13",
    
    signfikansi_C1 == "Signifikan" & signfikansi_C2 == "Signifikan" &
      signfikansi_C3 == "Signifikan" & signfikansi_C4 == "Signifikan" &
      signfikansi_C5 == "Signifikan" & signfikansi_C6 == "Signifikan" &
      signfikansi_C7 == "Signifikan" & signfikansi_C8 == "Signifikan" &
      signfikansi_C9 == "Signifikan" & signfikansi_C12 == "Signifikan" &
      signfikansi_C13 == "Signifikan" ~ "C1,C2,C3,C4,C5,C6,C7,C8,C9,C12,C13",
    
    signfikansi_C1 == "Signifikan" & signfikansi_C2 == "Signifikan" &
      signfikansi_C3 == "Signifikan" & signfikansi_C4 == "Signifikan" &
      signfikansi_C5 == "Signifikan" & signfikansi_C6 == "Signifikan" &
      signfikansi_C7 == "Signifikan" & signfikansi_C8 == "Signifikan" &
      signfikansi_C11 == "Signifikan" & signfikansi_C12 == "Signifikan" &
      signfikansi_C13 == "Signifikan" ~ "C1,C2,C3,C4,C5,C6,C7,C8,C11,C12,C13",
    
    signfikansi_C1 == "Signifikan" & signfikansi_C2 == "Signifikan" &
      signfikansi_C3 == "Signifikan" & signfikansi_C4 == "Signifikan" &
      signfikansi_C5 == "Signifikan" & signfikansi_C6 == "Signifikan" &
      signfikansi_C7 == "Signifikan" & signfikansi_C10 == "Signifikan" &
      signfikansi_C11 == "Signifikan" & signfikansi_C12 == "Signifikan" &
      signfikansi_C13 == "Signifikan" ~ "C1,C2,C3,C4,C5,C6,C7,C10,C11,C12,C13",
    
    signfikansi_C1 == "Signifikan" & signfikansi_C2 == "Signifikan" &
      signfikansi_C3 == "Signifikan" & signfikansi_C4 == "Signifikan" &
      signfikansi_C5 == "Signifikan" & signfikansi_C6 == "Signifikan" &
      signfikansi_C9 == "Signifikan" & signfikansi_C10 == "Signifikan" &
      signfikansi_C11 == "Signifikan" & signfikansi_C12 == "Signifikan" &
      signfikansi_C13 == "Signifikan" ~ "C1,C2,C3,C4,C5,C6,C9,C10,C11,C12,C13",
    
    signfikansi_C1 == "Signifikan" & signfikansi_C2 == "Signifikan" &
      signfikansi_C3 == "Signifikan" & signfikansi_C4 == "Signifikan" &
      signfikansi_C5 == "Signifikan" & signfikansi_C8 == "Signifikan" &
      signfikansi_C9 == "Signifikan" & signfikansi_C10 == "Signifikan" &
      signfikansi_C11 == "Signifikan" & signfikansi_C12 == "Signifikan" &
      signfikansi_C13 == "Signifikan" ~ "C1,C2,C3,C4,C5,C8,C9,C10,C11,C12,C13",
    
    signfikansi_C1 == "Signifikan" & signfikansi_C2 == "Signifikan" &
      signfikansi_C3 == "Signifikan" & signfikansi_C4 == "Signifikan" &
      signfikansi_C7 == "Signifikan" & signfikansi_C8 == "Signifikan" &
      signfikansi_C9 == "Signifikan" & signfikansi_C10 == "Signifikan" &
      signfikansi_C11 == "Signifikan" & signfikansi_C12 == "Signifikan" &
      signfikansi_C13 == "Signifikan" ~ "C1,C2,C3,C4,C7,C8,C9,C10,C11,C12,C13",
    
    signfikansi_C1 == "Signifikan" & signfikansi_C2 == "Signifikan" &
      signfikansi_C3 == "Signifikan" & signfikansi_C6 == "Signifikan" &
      signfikansi_C7 == "Signifikan" & signfikansi_C8 == "Signifikan" &
      signfikansi_C9 == "Signifikan" & signfikansi_C10 == "Signifikan" &
      signfikansi_C11 == "Signifikan" & signfikansi_C12 == "Signifikan" &
      signfikansi_C13 == "Signifikan" ~ "C1,C2,C3,C6,C7,C8,C9,C10,C11,C12,C13",
    
    signfikansi_C1 == "Signifikan" & signfikansi_C2 == "Signifikan" &
      signfikansi_C5 == "Signifikan" & signfikansi_C6 == "Signifikan" &
      signfikansi_C7 == "Signifikan" & signfikansi_C8 == "Signifikan" &
      signfikansi_C9 == "Signifikan" & signfikansi_C10 == "Signifikan" &
      signfikansi_C11 == "Signifikan" & signfikansi_C12 == "Signifikan" &
      signfikansi_C13 == "Signifikan" ~ "C1,C2,C5,C6,C7,C8,C9,C10,C11,C12,C13",
    
    signfikansi_C1 == "Signifikan" & signfikansi_C4 == "Signifikan" &
      signfikansi_C5 == "Signifikan" & signfikansi_C6 == "Signifikan" &
      signfikansi_C7 == "Signifikan" & signfikansi_C8 == "Signifikan" &
      signfikansi_C9 == "Signifikan" & signfikansi_C10 == "Signifikan" &
      signfikansi_C11 == "Signifikan" & signfikansi_C12 == "Signifikan" &
      signfikansi_C13 == "Signifikan" ~ "C1,C4,C5,C6,C7,C8,C9,C10,C11,C12,C13",
    
    signfikansi_C3 == "Signifikan" & signfikansi_C4 == "Signifikan" &
      signfikansi_C5 == "Signifikan" & signfikansi_C6 == "Signifikan" &
      signfikansi_C7 == "Signifikan" & signfikansi_C8 == "Signifikan" &
      signfikansi_C9 == "Signifikan" & signfikansi_C10 == "Signifikan" &
      signfikansi_C11 == "Signifikan" & signfikansi_C12 == "Signifikan" &
      signfikansi_C13 == "Signifikan" ~ "C3,C4,C5,C6,C7,C8,C9,C10,C11,C12,C13",
    
    signfikansi_C2 == "Signifikan" &
      signfikansi_C3 == "Signifikan" & signfikansi_C4 == "Signifikan" &
      signfikansi_C5 == "Signifikan" & signfikansi_C6 == "Signifikan" &
      signfikansi_C7 == "Signifikan" & signfikansi_C8 == "Signifikan" &
      signfikansi_C9 == "Signifikan" & signfikansi_C10 == "Signifikan" &
      signfikansi_C11 == "Signifikan" & signfikansi_C12 == "Signifikan" ~ "C2,C3,C4,C5,C6,C7,C8,C9,C10,C11,C12",
    
    # Eliminasi 3
    signfikansi_C1 == "Signifikan" & signfikansi_C2 == "Signifikan" &
      signfikansi_C3 == "Signifikan" & signfikansi_C4 == "Signifikan" &
      signfikansi_C5 == "Signifikan" & signfikansi_C6 == "Signifikan" &
      signfikansi_C7 == "Signifikan" & signfikansi_C8 == "Signifikan" &
      signfikansi_C9 == "Signifikan" & signfikansi_C10 == "Signifikan" ~ "C1,C2,C3,C4,C5,C6,C7,C8,C9,C10",
    
    signfikansi_C1 == "Signifikan" & signfikansi_C2 == "Signifikan" &
      signfikansi_C3 == "Signifikan" & signfikansi_C4 == "Signifikan" &
      signfikansi_C5 == "Signifikan" & signfikansi_C6 == "Signifikan" &
      signfikansi_C7 == "Signifikan" & signfikansi_C8 == "Signifikan" &
      signfikansi_C9 == "Signifikan" &
      signfikansi_C13 == "Signifikan" ~ "C1,C2,C3,C4,C5,C6,C7,C8,C9,C13",
    
    signfikansi_C1 == "Signifikan" & signfikansi_C2 == "Signifikan" &
      signfikansi_C3 == "Signifikan" & signfikansi_C4 == "Signifikan" &
      signfikansi_C5 == "Signifikan" & signfikansi_C6 == "Signifikan" &
      signfikansi_C7 == "Signifikan" & signfikansi_C8 == "Signifikan" &
      signfikansi_C12 == "Signifikan" &
      signfikansi_C13 == "Signifikan" ~ "C1,C2,C3,C4,C5,C6,C7,C8,C12,C13",
    
    signfikansi_C1 == "Signifikan" & signfikansi_C2 == "Signifikan" &
      signfikansi_C3 == "Signifikan" & signfikansi_C4 == "Signifikan" &
      signfikansi_C5 == "Signifikan" & signfikansi_C6 == "Signifikan" &
      signfikansi_C7 == "Signifikan" & 
      signfikansi_C11 == "Signifikan" & signfikansi_C12 == "Signifikan" &
      signfikansi_C13 == "Signifikan" ~ "C1,C2,C3,C4,C5,C6,C7,C11,C12,C13",
    
    signfikansi_C1 == "Signifikan" & signfikansi_C2 == "Signifikan" &
      signfikansi_C3 == "Signifikan" & signfikansi_C4 == "Signifikan" &
      signfikansi_C5 == "Signifikan" & signfikansi_C6 == "Signifikan" &
      signfikansi_C10 == "Signifikan" &
      signfikansi_C11 == "Signifikan" & signfikansi_C12 == "Signifikan" &
      signfikansi_C13 == "Signifikan" ~ "C1,C2,C3,C4,C5,C6,C10,C11,C12,C13",
    
    signfikansi_C1 == "Signifikan" & signfikansi_C2 == "Signifikan" &
      signfikansi_C3 == "Signifikan" & signfikansi_C4 == "Signifikan" &
      signfikansi_C5 == "Signifikan" &
      signfikansi_C9 == "Signifikan" & signfikansi_C10 == "Signifikan" &
      signfikansi_C11 == "Signifikan" & signfikansi_C12 == "Signifikan" &
      signfikansi_C13 == "Signifikan" ~ "C1,C2,C3,C4,C5,C9,C10,C11,C12,C13",
    
    signfikansi_C1 == "Signifikan" & signfikansi_C2 == "Signifikan" &
      signfikansi_C3 == "Signifikan" & signfikansi_C4 == "Signifikan" &
      signfikansi_C8 == "Signifikan" &
      signfikansi_C9 == "Signifikan" & signfikansi_C10 == "Signifikan" &
      signfikansi_C11 == "Signifikan" & signfikansi_C12 == "Signifikan" &
      signfikansi_C13 == "Signifikan" ~ "C1,C2,C3,C4,C8,C9,C10,C11,C12,C13",
    
    signfikansi_C1 == "Signifikan" & signfikansi_C2 == "Signifikan" &
      signfikansi_C3 == "Signifikan" & 
      signfikansi_C7 == "Signifikan" & signfikansi_C8 == "Signifikan" &
      signfikansi_C9 == "Signifikan" & signfikansi_C10 == "Signifikan" &
      signfikansi_C11 == "Signifikan" & signfikansi_C12 == "Signifikan" &
      signfikansi_C13 == "Signifikan" ~ "C1,C2,C3,C7,C8,C9,C10,C11,C12,C13",
    
    signfikansi_C1 == "Signifikan" & signfikansi_C2 == "Signifikan" &
      signfikansi_C6 == "Signifikan" &
      signfikansi_C7 == "Signifikan" & signfikansi_C8 == "Signifikan" &
      signfikansi_C9 == "Signifikan" & signfikansi_C10 == "Signifikan" &
      signfikansi_C11 == "Signifikan" & signfikansi_C12 == "Signifikan" &
      signfikansi_C13 == "Signifikan" ~ "C1,C2,C6,C7,C8,C9,C10,C11,C12,C13",
    
    signfikansi_C1 == "Signifikan" &
      signfikansi_C5 == "Signifikan" & signfikansi_C6 == "Signifikan" &
      signfikansi_C7 == "Signifikan" & signfikansi_C8 == "Signifikan" &
      signfikansi_C9 == "Signifikan" & signfikansi_C10 == "Signifikan" &
      signfikansi_C11 == "Signifikan" & signfikansi_C12 == "Signifikan" &
      signfikansi_C13 == "Signifikan" ~ "C1,C5,C6,C7,C8,C9,C10,C11,C12,C13",
    
    signfikansi_C4 == "Signifikan" &
      signfikansi_C5 == "Signifikan" & signfikansi_C6 == "Signifikan" &
      signfikansi_C7 == "Signifikan" & signfikansi_C8 == "Signifikan" &
      signfikansi_C9 == "Signifikan" & signfikansi_C10 == "Signifikan" &
      signfikansi_C11 == "Signifikan" & signfikansi_C12 == "Signifikan" &
      signfikansi_C13 == "Signifikan" ~ "C4,C5,C6,C7,C8,C9,C10,C11,C12,C13",
    
    signfikansi_C3 == "Signifikan" & signfikansi_C4 == "Signifikan" &
      signfikansi_C5 == "Signifikan" & signfikansi_C6 == "Signifikan" &
      signfikansi_C7 == "Signifikan" & signfikansi_C8 == "Signifikan" &
      signfikansi_C9 == "Signifikan" & signfikansi_C10 == "Signifikan" &
      signfikansi_C11 == "Signifikan" & signfikansi_C12 == "Signifikan" ~ "C3,C4,C5,C6,C7,C8,C9,C10,C11,C12",
    
    signfikansi_C2 == "Signifikan" &
      signfikansi_C3 == "Signifikan" & signfikansi_C4 == "Signifikan" &
      signfikansi_C5 == "Signifikan" & signfikansi_C6 == "Signifikan" &
      signfikansi_C7 == "Signifikan" & signfikansi_C8 == "Signifikan" &
      signfikansi_C9 == "Signifikan" & signfikansi_C10 == "Signifikan" &
      signfikansi_C11 == "Signifikan" ~ "C2,C3,C4,C5,C6,C7,C8,C9,C10,C11",
    
    # Eliminasi 4
    signfikansi_C1 == "Signifikan" & signfikansi_C2 == "Signifikan" &
      signfikansi_C3 == "Signifikan" & signfikansi_C4 == "Signifikan" &
      signfikansi_C5 == "Signifikan" & signfikansi_C6 == "Signifikan" &
      signfikansi_C7 == "Signifikan" & signfikansi_C8 == "Signifikan" &
      signfikansi_C9 == "Signifikan" ~ "C1,C2,C3,C4,C5,C6,C7,C8,C9",
    
    signfikansi_C1 == "Signifikan" & signfikansi_C2 == "Signifikan" &
      signfikansi_C3 == "Signifikan" & signfikansi_C4 == "Signifikan" &
      signfikansi_C5 == "Signifikan" & signfikansi_C6 == "Signifikan" &
      signfikansi_C7 == "Signifikan" & signfikansi_C8 == "Signifikan" &
      signfikansi_C13 == "Signifikan" ~ "C1,C2,C3,C4,C5,C6,C7,C8,C13",
    
    signfikansi_C1 == "Signifikan" & signfikansi_C2 == "Signifikan" &
      signfikansi_C3 == "Signifikan" & signfikansi_C4 == "Signifikan" &
      signfikansi_C5 == "Signifikan" & signfikansi_C6 == "Signifikan" &
      signfikansi_C7 == "Signifikan" & signfikansi_C12 == "Signifikan" &
      signfikansi_C13 == "Signifikan" ~ "C1,C2,C3,C4,C5,C6,C7,C12,C13",
    
    signfikansi_C1 == "Signifikan" & signfikansi_C2 == "Signifikan" &
      signfikansi_C3 == "Signifikan" & signfikansi_C4 == "Signifikan" &
      signfikansi_C5 == "Signifikan" & signfikansi_C6 == "Signifikan" &
      signfikansi_C11 == "Signifikan" & signfikansi_C12 == "Signifikan" &
      signfikansi_C13 == "Signifikan" ~ "C1,C2,C3,C4,C5,C6,C11,C12,C13",
    
    signfikansi_C1 == "Signifikan" & signfikansi_C2 == "Signifikan" &
      signfikansi_C3 == "Signifikan" & signfikansi_C4 == "Signifikan" &
      signfikansi_C5 == "Signifikan" & signfikansi_C10 == "Signifikan" &
      signfikansi_C11 == "Signifikan" & signfikansi_C12 == "Signifikan" &
      signfikansi_C13 == "Signifikan" ~ "C1,C2,C3,C4,C5,C10,C11,C12,C13",
    
    signfikansi_C1 == "Signifikan" & signfikansi_C2 == "Signifikan" &
      signfikansi_C3 == "Signifikan" & signfikansi_C4 == "Signifikan" &
      signfikansi_C9 == "Signifikan" & signfikansi_C10 == "Signifikan" &
      signfikansi_C11 == "Signifikan" & signfikansi_C12 == "Signifikan" &
      signfikansi_C13 == "Signifikan" ~ "C1,C2,C3,C4,C9,C10,C11,C12,C13",
    
    signfikansi_C1 == "Signifikan" & signfikansi_C2 == "Signifikan" &
      signfikansi_C3 == "Signifikan" & signfikansi_C8 == "Signifikan" &
      signfikansi_C9 == "Signifikan" & signfikansi_C10 == "Signifikan" &
      signfikansi_C11 == "Signifikan" & signfikansi_C12 == "Signifikan" &
      signfikansi_C13 == "Signifikan" ~ "C1,C2,C3,C8,C9,C10,C11,C12,C13",
    
    signfikansi_C1 == "Signifikan" & signfikansi_C2 == "Signifikan" &
      signfikansi_C7 == "Signifikan" & signfikansi_C8 == "Signifikan" &
      signfikansi_C9 == "Signifikan" & signfikansi_C10 == "Signifikan" &
      signfikansi_C11 == "Signifikan" & signfikansi_C12 == "Signifikan" &
      signfikansi_C13 == "Signifikan" ~ "C1,C2,C7,C8,C9,C10,C11,C12,C13",
    
    signfikansi_C1 == "Signifikan" & signfikansi_C6 == "Signifikan" &
      signfikansi_C7 == "Signifikan" & signfikansi_C8 == "Signifikan" &
      signfikansi_C9 == "Signifikan" & signfikansi_C10 == "Signifikan" &
      signfikansi_C11 == "Signifikan" & signfikansi_C12 == "Signifikan" &
      signfikansi_C13 == "Signifikan" ~ "C1,C6,C7,C8,C9,C10,C11,C12,C13",
    
    signfikansi_C5 == "Signifikan" & signfikansi_C6 == "Signifikan" &
      signfikansi_C7 == "Signifikan" & signfikansi_C8 == "Signifikan" &
      signfikansi_C9 == "Signifikan" & signfikansi_C10 == "Signifikan" &
      signfikansi_C11 == "Signifikan" & signfikansi_C12 == "Signifikan" &
      signfikansi_C13 == "Signifikan" ~ "C5,C6,C7,C8,C9,C10,C11,C12,C13",
    
    signfikansi_C4 == "Signifikan" &
      signfikansi_C5 == "Signifikan" & signfikansi_C6 == "Signifikan" &
      signfikansi_C7 == "Signifikan" & signfikansi_C8 == "Signifikan" &
      signfikansi_C9 == "Signifikan" & signfikansi_C10 == "Signifikan" &
      signfikansi_C11 == "Signifikan" & signfikansi_C12 == "Signifikan" ~ "C4,C5,C6,C7,C8,C9,C10,C11,C12",
    
    signfikansi_C3 == "Signifikan" & signfikansi_C4 == "Signifikan" &
      signfikansi_C5 == "Signifikan" & signfikansi_C6 == "Signifikan" &
      signfikansi_C7 == "Signifikan" & signfikansi_C8 == "Signifikan" &
      signfikansi_C9 == "Signifikan" & signfikansi_C10 == "Signifikan" &
      signfikansi_C11 == "Signifikan" ~ "C3,C4,C5,C6,C7,C8,C9,C10,C11",
    
    signfikansi_C2 == "Signifikan" &
      signfikansi_C3 == "Signifikan" & signfikansi_C4 == "Signifikan" &
      signfikansi_C5 == "Signifikan" & signfikansi_C6 == "Signifikan" &
      signfikansi_C7 == "Signifikan" & signfikansi_C8 == "Signifikan" &
      signfikansi_C9 == "Signifikan" & signfikansi_C10 == "Signifikan" ~ "C2,C3,C4,C5,C6,C7,C8,C9,C10",
    
    # Eliminasi 5
    signfikansi_C1 == "Signifikan" & signfikansi_C2 == "Signifikan" &
      signfikansi_C3 == "Signifikan" & signfikansi_C4 == "Signifikan" &
      signfikansi_C5 == "Signifikan" & signfikansi_C6 == "Signifikan" &
      signfikansi_C7 == "Signifikan" & signfikansi_C8 == "Signifikan" ~ "C1,C2,C3,C4,C5,C6,C7,C8",
    
    signfikansi_C1 == "Signifikan" & signfikansi_C2 == "Signifikan" &
      signfikansi_C3 == "Signifikan" & signfikansi_C4 == "Signifikan" &
      signfikansi_C5 == "Signifikan" & signfikansi_C6 == "Signifikan" &
      signfikansi_C7 == "Signifikan" &
      signfikansi_C13 == "Signifikan" ~ "C1,C2,C3,C4,C5,C6,C7,C13",
    
    signfikansi_C1 == "Signifikan" & signfikansi_C2 == "Signifikan" &
      signfikansi_C3 == "Signifikan" & signfikansi_C4 == "Signifikan" &
      signfikansi_C5 == "Signifikan" & signfikansi_C6 == "Signifikan" &
      signfikansi_C12 == "Signifikan" &
      signfikansi_C13 == "Signifikan" ~ "C1,C2,C3,C4,C5,C6,C12,C13",
    
    signfikansi_C1 == "Signifikan" & signfikansi_C2 == "Signifikan" &
      signfikansi_C3 == "Signifikan" & signfikansi_C4 == "Signifikan" &
      signfikansi_C5 == "Signifikan" & 
      signfikansi_C11 == "Signifikan" & signfikansi_C12 == "Signifikan" &
      signfikansi_C13 == "Signifikan" ~ "C1,C2,C3,C4,C5,C11,C12,C13",
    
    signfikansi_C1 == "Signifikan" & signfikansi_C2 == "Signifikan" &
      signfikansi_C3 == "Signifikan" & signfikansi_C4 == "Signifikan" &
      signfikansi_C10 == "Signifikan" &
      signfikansi_C11 == "Signifikan" & signfikansi_C12 == "Signifikan" &
      signfikansi_C13 == "Signifikan" ~ "C1,C2,C3,C4,C10,C11,C12,C13",
    
    signfikansi_C1 == "Signifikan" & signfikansi_C2 == "Signifikan" &
      signfikansi_C3 == "Signifikan" & 
      signfikansi_C9 == "Signifikan" & signfikansi_C10 == "Signifikan" &
      signfikansi_C11 == "Signifikan" & signfikansi_C12 == "Signifikan" &
      signfikansi_C13 == "Signifikan" ~ "C1,C2,C3,C9,C10,C11,C12,C13",
    
    signfikansi_C1 == "Signifikan" & signfikansi_C2 == "Signifikan" &
      signfikansi_C8 == "Signifikan" &
      signfikansi_C9 == "Signifikan" & signfikansi_C10 == "Signifikan" &
      signfikansi_C11 == "Signifikan" & signfikansi_C12 == "Signifikan" &
      signfikansi_C13 == "Signifikan" ~ "C1,C2,C8,C9,C10,C11,C12,C13",
    
    signfikansi_C1 == "Signifikan" &
      signfikansi_C7 == "Signifikan" & signfikansi_C8 == "Signifikan" &
      signfikansi_C9 == "Signifikan" & signfikansi_C10 == "Signifikan" &
      signfikansi_C11 == "Signifikan" & signfikansi_C12 == "Signifikan" &
      signfikansi_C13 == "Signifikan" ~ "C1,C7,C8,C9,C10,C11,C12,C13",
    
    signfikansi_C6 == "Signifikan" &
      signfikansi_C7 == "Signifikan" & signfikansi_C8 == "Signifikan" &
      signfikansi_C9 == "Signifikan" & signfikansi_C10 == "Signifikan" &
      signfikansi_C11 == "Signifikan" & signfikansi_C12 == "Signifikan" &
      signfikansi_C13 == "Signifikan" ~ "C6,C7,C8,C9,C10,C11,C12,C13",
    
    signfikansi_C5 == "Signifikan" & signfikansi_C6 == "Signifikan" &
      signfikansi_C7 == "Signifikan" & signfikansi_C8 == "Signifikan" &
      signfikansi_C9 == "Signifikan" & signfikansi_C10 == "Signifikan" &
      signfikansi_C11 == "Signifikan" & signfikansi_C12 == "Signifikan" ~ "C5,C6,C7,C8,C9,C10,C11,C12",
    
    signfikansi_C4 == "Signifikan" &
      signfikansi_C5 == "Signifikan" & signfikansi_C6 == "Signifikan" &
      signfikansi_C7 == "Signifikan" & signfikansi_C8 == "Signifikan" &
      signfikansi_C9 == "Signifikan" & signfikansi_C10 == "Signifikan" &
      signfikansi_C11 == "Signifikan" ~ "C4,C5,C6,C7,C8,C9,C10,C11",
    
    signfikansi_C3 == "Signifikan" & signfikansi_C4 == "Signifikan" &
      signfikansi_C5 == "Signifikan" & signfikansi_C6 == "Signifikan" &
      signfikansi_C7 == "Signifikan" & signfikansi_C8 == "Signifikan" &
      signfikansi_C9 == "Signifikan" & signfikansi_C10 == "Signifikan" ~ "C3,C4,C5,C6,C7,C8,C9,C10",
    
    signfikansi_C2 == "Signifikan" &
      signfikansi_C3 == "Signifikan" & signfikansi_C4 == "Signifikan" &
      signfikansi_C5 == "Signifikan" & signfikansi_C6 == "Signifikan" &
      signfikansi_C7 == "Signifikan" & signfikansi_C8 == "Signifikan" &
      signfikansi_C9 == "Signifikan" ~ "C2,C3,C4,C5,C6,C7,C8,C9",
    
    # Eliminasi 6
    signfikansi_C1 == "Signifikan" & signfikansi_C2 == "Signifikan" &
      signfikansi_C3 == "Signifikan" & signfikansi_C4 == "Signifikan" &
      signfikansi_C5 == "Signifikan" & signfikansi_C6 == "Signifikan" &
      signfikansi_C7 == "Signifikan" ~ "C1,C2,C3,C4,C5,C6,C7",
    
    signfikansi_C1 == "Signifikan" & signfikansi_C2 == "Signifikan" &
      signfikansi_C3 == "Signifikan" & signfikansi_C4 == "Signifikan" &
      signfikansi_C5 == "Signifikan" & signfikansi_C6 == "Signifikan" &
      signfikansi_C13 == "Signifikan" ~ "C1,C2,C3,C4,C5,C6,C13",
    
    signfikansi_C1 == "Signifikan" & signfikansi_C2 == "Signifikan" &
      signfikansi_C3 == "Signifikan" & signfikansi_C4 == "Signifikan" &
      signfikansi_C5 == "Signifikan" & signfikansi_C12 == "Signifikan" &
      signfikansi_C13 == "Signifikan" ~ "C1,C2,C3,C4,C5,C12,C13",
    
    signfikansi_C1 == "Signifikan" & signfikansi_C2 == "Signifikan" &
      signfikansi_C3 == "Signifikan" & signfikansi_C4 == "Signifikan" &
      signfikansi_C11 == "Signifikan" & signfikansi_C12 == "Signifikan" &
      signfikansi_C13 == "Signifikan" ~ "C1,C2,C3,C4,C11,C12,C13",
    
    signfikansi_C1 == "Signifikan" & signfikansi_C2 == "Signifikan" &
      signfikansi_C3 == "Signifikan" & signfikansi_C10 == "Signifikan" &
      signfikansi_C11 == "Signifikan" & signfikansi_C12 == "Signifikan" &
      signfikansi_C13 == "Signifikan" ~ "C1,C2,C3,C10,C11,C12,C13",
    
    signfikansi_C1 == "Signifikan" & signfikansi_C2 == "Signifikan" &
      signfikansi_C9 == "Signifikan" & signfikansi_C10 == "Signifikan" &
      signfikansi_C11 == "Signifikan" & signfikansi_C12 == "Signifikan" &
      signfikansi_C13 == "Signifikan" ~ "C1,C2,C9,C10,C11,C12,C13",
    
    signfikansi_C1 == "Signifikan" & signfikansi_C8 == "Signifikan" &
      signfikansi_C9 == "Signifikan" & signfikansi_C10 == "Signifikan" &
      signfikansi_C11 == "Signifikan" & signfikansi_C12 == "Signifikan" &
      signfikansi_C13 == "Signifikan" ~ "C1,C8,C9,C10,C11,C12,C13",
    
    signfikansi_C7 == "Signifikan" & signfikansi_C8 == "Signifikan" &
      signfikansi_C9 == "Signifikan" & signfikansi_C10 == "Signifikan" &
      signfikansi_C11 == "Signifikan" & signfikansi_C12 == "Signifikan" &
      signfikansi_C13 == "Signifikan" ~ "C7,C8,C9,C10,C11,C12,C13",
    
    signfikansi_C6 == "Signifikan" &
      signfikansi_C7 == "Signifikan" & signfikansi_C8 == "Signifikan" &
      signfikansi_C9 == "Signifikan" & signfikansi_C10 == "Signifikan" &
      signfikansi_C11 == "Signifikan" & signfikansi_C12 == "Signifikan" ~ "C6,C7,C8,C9,C10,C11,C12",
    
    signfikansi_C5 == "Signifikan" & signfikansi_C6 == "Signifikan" &
      signfikansi_C7 == "Signifikan" & signfikansi_C8 == "Signifikan" &
      signfikansi_C9 == "Signifikan" & signfikansi_C10 == "Signifikan" &
      signfikansi_C11 == "Signifikan" ~ "C5,C6,C7,C8,C9,C10,C11",
    
    signfikansi_C4 == "Signifikan" &
      signfikansi_C5 == "Signifikan" & signfikansi_C6 == "Signifikan" &
      signfikansi_C7 == "Signifikan" & signfikansi_C8 == "Signifikan" &
      signfikansi_C9 == "Signifikan" & signfikansi_C10 == "Signifikan" ~ "C4,C5,C6,C7,C8,C9,C10",
    
    signfikansi_C3 == "Signifikan" & signfikansi_C4 == "Signifikan" &
      signfikansi_C5 == "Signifikan" & signfikansi_C6 == "Signifikan" &
      signfikansi_C7 == "Signifikan" & signfikansi_C8 == "Signifikan" &
      signfikansi_C9 == "Signifikan" ~ "C3,C4,C5,C6,C7,C8,C9",
    
    signfikansi_C2 == "Signifikan" &
      signfikansi_C3 == "Signifikan" & signfikansi_C4 == "Signifikan" &
      signfikansi_C5 == "Signifikan" & signfikansi_C6 == "Signifikan" &
      signfikansi_C7 == "Signifikan" & signfikansi_C8 == "Signifikan" ~ "C2,C3,C4,C5,C6,C7,C8",
    
    # Eliminasi 7
    signfikansi_C1 == "Signifikan" & signfikansi_C2 == "Signifikan" &
      signfikansi_C3 == "Signifikan" & signfikansi_C4 == "Signifikan" &
      signfikansi_C5 == "Signifikan" & signfikansi_C6 == "Signifikan" ~ "C1,C2,C3,C4,C5,C6",
    
    signfikansi_C1 == "Signifikan" & signfikansi_C2 == "Signifikan" &
      signfikansi_C3 == "Signifikan" & signfikansi_C4 == "Signifikan" &
      signfikansi_C5 == "Signifikan" & signfikansi_C13 == "Signifikan" ~ "C1,C2,C3,C4,C5,C13",
    
    signfikansi_C1 == "Signifikan" & signfikansi_C2 == "Signifikan" &
      signfikansi_C3 == "Signifikan" & signfikansi_C4 == "Signifikan" &
      signfikansi_C12 == "Signifikan" & signfikansi_C13 == "Signifikan" ~ "C1,C2,C3,C4,C12,C13",
    
    signfikansi_C1 == "Signifikan" & signfikansi_C2 == "Signifikan" &
      signfikansi_C3 == "Signifikan" & signfikansi_C11 == "Signifikan" & 
      signfikansi_C12 == "Signifikan" & signfikansi_C13 == "Signifikan" ~ "C1,C2,C3,C11,C12,C13",
    
    signfikansi_C1 == "Signifikan" & signfikansi_C2 == "Signifikan" &
      signfikansi_C10 == "Signifikan" &
      signfikansi_C11 == "Signifikan" & signfikansi_C12 == "Signifikan" &
      signfikansi_C13 == "Signifikan" ~ "C1,C2,C10,C11,C12,C13",
    
    signfikansi_C1 == "Signifikan" & 
      signfikansi_C9 == "Signifikan" & signfikansi_C10 == "Signifikan" &
      signfikansi_C11 == "Signifikan" & signfikansi_C12 == "Signifikan" &
      signfikansi_C13 == "Signifikan" ~ "C1,C9,C10,C11,C12,C13",
    
    signfikansi_C8 == "Signifikan" &
      signfikansi_C9 == "Signifikan" & signfikansi_C10 == "Signifikan" &
      signfikansi_C11 == "Signifikan" & signfikansi_C12 == "Signifikan" &
      signfikansi_C13 == "Signifikan" ~ "C8,C9,C10,C11,C12,C13",
    
    signfikansi_C7 == "Signifikan" & signfikansi_C8 == "Signifikan" &
      signfikansi_C9 == "Signifikan" & signfikansi_C10 == "Signifikan" &
      signfikansi_C11 == "Signifikan" & signfikansi_C12 == "Signifikan" ~ "C7,C8,C9,C10,C11,C12",
    
    signfikansi_C6 == "Signifikan" &
      signfikansi_C7 == "Signifikan" & signfikansi_C8 == "Signifikan" &
      signfikansi_C9 == "Signifikan" & signfikansi_C10 == "Signifikan" &
      signfikansi_C11 == "Signifikan" ~ "C6,C7,C8,C9,C10,C11",
    
    signfikansi_C5 == "Signifikan" & signfikansi_C6 == "Signifikan" &
      signfikansi_C7 == "Signifikan" & signfikansi_C8 == "Signifikan" &
      signfikansi_C9 == "Signifikan" & signfikansi_C10 == "Signifikan" ~ "C5,C6,C7,C8,C9,C10",
    
    signfikansi_C4 == "Signifikan" &
      signfikansi_C5 == "Signifikan" & signfikansi_C6 == "Signifikan" &
      signfikansi_C7 == "Signifikan" & signfikansi_C8 == "Signifikan" &
      signfikansi_C9 == "Signifikan" ~ "C4,C5,C6,C7,C8,C9",
    
    signfikansi_C3 == "Signifikan" & signfikansi_C4 == "Signifikan" &
      signfikansi_C5 == "Signifikan" & signfikansi_C6 == "Signifikan" &
      signfikansi_C7 == "Signifikan" & signfikansi_C8 == "Signifikan" ~ "C3,C4,C5,C6,C7,C8",
    
    signfikansi_C2 == "Signifikan" &
      signfikansi_C3 == "Signifikan" & signfikansi_C4 == "Signifikan" &
      signfikansi_C5 == "Signifikan" & signfikansi_C6 == "Signifikan" &
      signfikansi_C7 == "Signifikan" ~ "C2,C3,C4,C5,C6,C7",
    
    # Eliminasi 8
    signfikansi_C1 == "Signifikan" & signfikansi_C2 == "Signifikan" &
      signfikansi_C3 == "Signifikan" & signfikansi_C4 == "Signifikan" &
      signfikansi_C5 == "Signifikan" ~ "C1,C2,C3,C4,C5",
    
    signfikansi_C1 == "Signifikan" & signfikansi_C2 == "Signifikan" &
      signfikansi_C3 == "Signifikan" & signfikansi_C4 == "Signifikan" &
      signfikansi_C13 == "Signifikan" ~ "C1,C2,C3,C4,C13",
    
    signfikansi_C1 == "Signifikan" & signfikansi_C2 == "Signifikan" &
      signfikansi_C3 == "Signifikan" & 
      signfikansi_C12 == "Signifikan" & signfikansi_C13 == "Signifikan" ~ "C1,C2,C3,C12,C13",
    
    signfikansi_C1 == "Signifikan" & signfikansi_C2 == "Signifikan" &
      signfikansi_C11 == "Signifikan" & 
      signfikansi_C12 == "Signifikan" & signfikansi_C13 == "Signifikan" ~ "C1,C2,C11,C12,C13",
    
    signfikansi_C1 == "Signifikan" & 
      signfikansi_C10 == "Signifikan" &
      signfikansi_C11 == "Signifikan" & signfikansi_C12 == "Signifikan" &
      signfikansi_C13 == "Signifikan" ~ "C1,C10,C11,C12,C13",
    
    signfikansi_C9 == "Signifikan" & signfikansi_C10 == "Signifikan" &
      signfikansi_C11 == "Signifikan" & signfikansi_C12 == "Signifikan" &
      signfikansi_C13 == "Signifikan" ~ "C9,C10,C11,C12,C13",
    
    signfikansi_C8 == "Signifikan" &
      signfikansi_C9 == "Signifikan" & signfikansi_C10 == "Signifikan" &
      signfikansi_C11 == "Signifikan" & signfikansi_C12 == "Signifikan" ~ "C8,C9,C10,C11,C12",
    
    signfikansi_C7 == "Signifikan" & signfikansi_C8 == "Signifikan" &
      signfikansi_C9 == "Signifikan" & signfikansi_C10 == "Signifikan" &
      signfikansi_C11 == "Signifikan" ~ "C7,C8,C9,C10,C11",
    
    signfikansi_C6 == "Signifikan" &
      signfikansi_C7 == "Signifikan" & signfikansi_C8 == "Signifikan" &
      signfikansi_C9 == "Signifikan" & signfikansi_C10 == "Signifikan" ~ "C6,C7,C8,C9,C10",
    
    signfikansi_C5 == "Signifikan" & signfikansi_C6 == "Signifikan" &
      signfikansi_C7 == "Signifikan" & signfikansi_C8 == "Signifikan" &
      signfikansi_C9 == "Signifikan" ~ "C5,C6,C7,C8,C9",
    
    signfikansi_C4 == "Signifikan" &
      signfikansi_C5 == "Signifikan" & signfikansi_C6 == "Signifikan" &
      signfikansi_C7 == "Signifikan" & signfikansi_C8 == "Signifikan" ~ "C4,C5,C6,C7,C8",
    
    signfikansi_C3 == "Signifikan" & signfikansi_C4 == "Signifikan" &
      signfikansi_C5 == "Signifikan" & signfikansi_C6 == "Signifikan" &
      signfikansi_C7 == "Signifikan" ~ "C3,C4,C5,C6,C7",
    
    signfikansi_C2 == "Signifikan" &
      signfikansi_C3 == "Signifikan" & signfikansi_C4 == "Signifikan" &
      signfikansi_C5 == "Signifikan" & signfikansi_C6 == "Signifikan" ~ "C2,C3,C4,C5,C6",
    
    # Eliminasi 9
    signfikansi_C1 == "Signifikan" & signfikansi_C2 == "Signifikan" &
      signfikansi_C3 == "Signifikan" & signfikansi_C4 == "Signifikan" ~ "C1,C2,C3,C4",
    
    signfikansi_C1 == "Signifikan" & signfikansi_C2 == "Signifikan" &
      signfikansi_C3 == "Signifikan" &
      signfikansi_C13 == "Signifikan" ~ "C1,C2,C3,C13",
    
    signfikansi_C1 == "Signifikan" & signfikansi_C2 == "Signifikan" & 
      signfikansi_C12 == "Signifikan" & signfikansi_C13 == "Signifikan" ~ "C1,C2,C12,C13",
    
    signfikansi_C1 == "Signifikan" & signfikansi_C11 == "Signifikan" & 
      signfikansi_C12 == "Signifikan" & signfikansi_C13 == "Signifikan" ~ "C1,C11,C12,C13",
    
    signfikansi_C10 == "Signifikan" & signfikansi_C11 == "Signifikan" & 
      signfikansi_C12 == "Signifikan" &
      signfikansi_C13 == "Signifikan" ~ "C10,C11,C12,C13",
    
    signfikansi_C9 == "Signifikan" & signfikansi_C10 == "Signifikan" &
      signfikansi_C11 == "Signifikan" & signfikansi_C12 == "Signifikan" ~ "C9,C10,C11,C12",
    
    signfikansi_C8 == "Signifikan" &
      signfikansi_C9 == "Signifikan" & signfikansi_C10 == "Signifikan" &
      signfikansi_C11 == "Signifikan" ~ "C8,C9,C10,C11",
    
    signfikansi_C7 == "Signifikan" & signfikansi_C8 == "Signifikan" &
      signfikansi_C9 == "Signifikan" & signfikansi_C10 == "Signifikan" ~ "C7,C8,C9,C10",
    
    signfikansi_C6 == "Signifikan" &
      signfikansi_C7 == "Signifikan" & signfikansi_C8 == "Signifikan" &
      signfikansi_C9 == "Signifikan" ~ "C6,C7,C8,C9",
    
    signfikansi_C5 == "Signifikan" & signfikansi_C6 == "Signifikan" &
      signfikansi_C7 == "Signifikan" & signfikansi_C8 == "Signifikan" ~ "C5,C6,C7,C8",
    
    signfikansi_C4 == "Signifikan" &
      signfikansi_C5 == "Signifikan" & signfikansi_C6 == "Signifikan" &
      signfikansi_C7 == "Signifikan" ~ "C4,C5,C6,C7",
    
    signfikansi_C3 == "Signifikan" & signfikansi_C4 == "Signifikan" &
      signfikansi_C5 == "Signifikan" & signfikansi_C6 == "Signifikan" ~ "C3,C4,C5,C6",
    
    signfikansi_C2 == "Signifikan" &
      signfikansi_C3 == "Signifikan" & signfikansi_C4 == "Signifikan" &
      signfikansi_C5 == "Signifikan" ~ "C2,C3,C4,C5",
    
    # Eliminasi 10
    signfikansi_C1 == "Signifikan" & signfikansi_C2 == "Signifikan" &
      signfikansi_C3 == "Signifikan" ~ "C1,C2,C3",
    
    signfikansi_C1 == "Signifikan" & signfikansi_C2 == "Signifikan" &
      signfikansi_C13 == "Signifikan" ~ "C1,C2,C13",
    
    signfikansi_C1 == "Signifikan" & 
      signfikansi_C12 == "Signifikan" & signfikansi_C13 == "Signifikan" ~ "C1,C12,C13",
    
    signfikansi_C11 == "Signifikan" & 
      signfikansi_C12 == "Signifikan" & signfikansi_C13 == "Signifikan" ~ "C11,C12,C13",
    
    signfikansi_C10 == "Signifikan" & signfikansi_C11 == "Signifikan" & 
      signfikansi_C12 == "Signifikan" ~ "C10,C11,C12",
    
    signfikansi_C9 == "Signifikan" & signfikansi_C10 == "Signifikan" &
      signfikansi_C11 == "Signifikan" ~ "C9,C10,C11",
    
    signfikansi_C8 == "Signifikan" &
      signfikansi_C9 == "Signifikan" & signfikansi_C10 == "Signifikan" ~ "C8,C9,C10",
    
    signfikansi_C7 == "Signifikan" & signfikansi_C8 == "Signifikan" &
      signfikansi_C9 == "Signifikan" ~ "C7,C8,C9",
    
    signfikansi_C6 == "Signifikan" &
      signfikansi_C7 == "Signifikan" & signfikansi_C8 == "Signifikan" ~ "C6,C7,C8",
    
    signfikansi_C5 == "Signifikan" & signfikansi_C6 == "Signifikan" &
      signfikansi_C7 == "Signifikan" ~ "C5,C6,C7",
    
    signfikansi_C4 == "Signifikan" &
      signfikansi_C5 == "Signifikan" & signfikansi_C6 == "Signifikan" ~ "C4,C5,C6",
    
    signfikansi_C3 == "Signifikan" & signfikansi_C4 == "Signifikan" &
      signfikansi_C5 == "Signifikan" ~ "C3,C4,C5",
    
    signfikansi_C2 == "Signifikan" &
      signfikansi_C3 == "Signifikan" & signfikansi_C4 == "Signifikan" ~ "C2,C3,C4",
    
    # Eliminasi 11
    signfikansi_C1 == "Signifikan" & signfikansi_C2 == "Signifikan" ~ "C1,C2",
    
    signfikansi_C1 == "Signifikan" & signfikansi_C13 == "Signifikan" ~ "C1,C13",
    
    signfikansi_C12 == "Signifikan" & signfikansi_C13 == "Signifikan" ~ "C12,C13",
    
    signfikansi_C11 == "Signifikan" & signfikansi_C12 == "Signifikan" ~ "C11,C12",
    
    signfikansi_C10 == "Signifikan" & signfikansi_C11 == "Signifikan" ~ "C10,C11",
    
    signfikansi_C9 == "Signifikan" & signfikansi_C10 == "Signifikan" ~ "C9,C10",
    
    signfikansi_C8 == "Signifikan" & signfikansi_C9 == "Signifikan" ~ "C8,C9",
    
    signfikansi_C7 == "Signifikan" & signfikansi_C8 == "Signifikan" ~ "C7,C8",
    
    signfikansi_C6 == "Signifikan" & signfikansi_C7 == "Signifikan" ~ "C6,C7",
    
    signfikansi_C5 == "Signifikan" & signfikansi_C6 == "Signifikan" ~ "C5,C6",
    
    signfikansi_C4 == "Signifikan" & signfikansi_C5 == "Signifikan" ~ "C4,C5",
    
    signfikansi_C3 == "Signifikan" & signfikansi_C4 == "Signifikan" ~ "C3,C4",
    
    signfikansi_C2 == "Signifikan" & signfikansi_C3 == "Signifikan" ~ "C2,C3",
    
    # Eliminasi 12
    signfikansi_C1 == "Signifikan" ~ "C1",
    
    signfikansi_C13 == "Signifikan" ~ "C13",
    
    signfikansi_C12 == "Signifikan" ~ "C12",
    
    signfikansi_C11 == "Signifikan" ~ "C11",
    
    signfikansi_C10 == "Signifikan" ~ "C10",
    
    signfikansi_C9 == "Signifikan" ~ "C9",
    
    signfikansi_C8 == "Signifikan" ~ "C8",
    
    signfikansi_C7 == "Signifikan" ~ "C7",
    
    signfikansi_C6 == "Signifikan" ~ "C6",
    
    signfikansi_C5 == "Signifikan" ~ "C5",
    
    signfikansi_C4 == "Signifikan" ~ "C4",
    
    signfikansi_C3 == "Signifikan" ~ "C3",
    
    signfikansi_C2 == "Signifikan" ~ "C2",
    
    TRUE ~ "Tidak Signifikan"
  ))

# Buat skema warna kustom
Kategori <- c(
  "Seluruh Kategori",
  # Eliminasi 1
  "C1,C2,C3,C4,C5,C6,C7,C8,C9,C10,C11,C12",
  "C1,C2,C3,C4,C5,C6,C7,C8,C9,C10,C11,C13",
  "C1,C2,C3,C4,C5,C6,C7,C8,C9,C10,C12,C13",
  "C1,C2,C3,C4,C5,C6,C7,C8,C9,C11,C12,C13",
  "C1,C2,C3,C4,C5,C6,C7,C8,C10,C11,C12,C13",
  "C1,C2,C3,C4,C5,C6,C7,C9,C10,C11,C12,C13",
  "C1,C2,C3,C4,C5,C6,C8,C9,C10,C11,C12,C13",
  "C1,C2,C3,C4,C5,C7,C8,C9,C10,C11,C12,C13",
  "C1,C2,C3,C4,C6,C7,C8,C9,C10,C11,C12,C13",
  "C1,C2,C3,C5,C6,C7,C8,C9,C10,C11,C12,C13",
  "C1,C2,C4,C5,C6,C7,C8,C9,C10,C11,C12,C13",
  "C1,C3,C4,C5,C6,C7,C8,C9,C10,C11,C12,C13",
  "C2,C3,C4,C5,C6,C7,C8,C9,C10,C11,C12,C13",
  
  # Eliminasi 2
  "C1,C2,C3,C4,C5,C6,C7,C8,C9,C10,C11",
  "C1,C2,C3,C4,C5,C6,C7,C8,C9,C10,C13",
  "C1,C2,C3,C4,C5,C6,C7,C8,C9,C12,C13",
  "C1,C2,C3,C4,C5,C6,C7,C8,C11,C12,C13",
  "C1,C2,C3,C4,C5,C6,C7,C10,C11,C12,C13",
  "C1,C2,C3,C4,C5,C6,C9,C10,C11,C12,C13",
  "C1,C2,C3,C4,C5,C8,C9,C10,C11,C12,C13",
  "C1,C2,C3,C4,C7,C8,C9,C10,C11,C12,C13",
  "C1,C2,C3,C6,C7,C8,C9,C10,C11,C12,C13",
  "C1,C2,C5,C6,C7,C8,C9,C10,C11,C12,C13",
  "C1,C4,C5,C6,C7,C8,C9,C10,C11,C12,C13",
  "C3,C4,C5,C6,C7,C8,C9,C10,C11,C12,C13",
  "C2,C3,C4,C5,C6,C7,C8,C9,C10,C11,C12",
  
  # Eliminasi 3
  "C1,C2,C3,C4,C5,C6,C7,C8,C9,C10",
  "C1,C2,C3,C4,C5,C6,C7,C8,C9,C13",
  "C1,C2,C3,C4,C5,C6,C7,C8,C12,C13",
  "C1,C2,C3,C4,C5,C6,C7,C11,C12,C13",
  "C1,C2,C3,C4,C5,C6,C10,C11,C12,C13",
  "C1,C2,C3,C4,C5,C9,C10,C11,C12,C13",
  "C1,C2,C3,C4,C8,C9,C10,C11,C12,C13",
  "C1,C2,C3,C7,C8,C9,C10,C11,C12,C13",
  "C1,C2,C6,C7,C8,C9,C10,C11,C12,C13",
  "C1,C5,C6,C7,C8,C9,C10,C11,C12,C13",
  "C4,C5,C6,C7,C8,C9,C10,C11,C12,C13",
  "C3,C4,C5,C6,C7,C8,C9,C10,C11,C12",
  "C2,C3,C4,C5,C6,C7,C8,C9,C10,C11",
  
  # Eliminasi 4
  "C1,C2,C3,C4,C5,C6,C7,C8,C9",
  "C1,C2,C3,C4,C5,C6,C7,C8,C13",
  "C1,C2,C3,C4,C5,C6,C7,C12,C13",
  "C1,C2,C3,C4,C5,C6,C11,C12,C13",
  "C1,C2,C3,C4,C5,C10,C11,C12,C13",
  "C1,C2,C3,C4,C9,C10,C11,C12,C13",
  "C1,C2,C3,C8,C9,C10,C11,C12,C13",
  "C1,C2,C7,C8,C9,C10,C11,C12,C13",
  "C1,C6,C7,C8,C9,C10,C11,C12,C13",
  "C5,C6,C7,C8,C9,C10,C11,C12,C13",
  "C4,C5,C6,C7,C8,C9,C10,C11,C12",
  "C3,C4,C5,C6,C7,C8,C9,C10,C11",
  "C2,C3,C4,C5,C6,C7,C8,C9,C10",
  
  # Eliminasi 5
  "C1,C2,C3,C4,C5,C6,C7,C8",
  "C1,C2,C3,C4,C5,C6,C7,C13",
  "C1,C2,C3,C4,C5,C6,C12,C13",
  "C1,C2,C3,C4,C5,C11,C12,C13",
  "C1,C2,C3,C4,C10,C11,C12,C13",
  "C1,C2,C3,C9,C10,C11,C12,C13",
  "C1,C2,C8,C9,C10,C11,C12,C13",
  "C1,C7,C8,C9,C10,C11,C12,C13",
  "C6,C7,C8,C9,C10,C11,C12,C13",
  "C5,C6,C7,C8,C9,C10,C11,C12",
  "C4,C5,C6,C7,C8,C9,C10,C11",
  "C3,C4,C5,C6,C7,C8,C9,C10",
  "C2,C3,C4,C5,C6,C7,C8,C9",
  
  # Eliminasi 6
  "C1,C2,C3,C4,C5,C6,C7",
  "C1,C2,C3,C4,C5,C6,C13",
  "C1,C2,C3,C4,C5,C12,C13",
  "C1,C2,C3,C4,C11,C12,C13",
  "C1,C2,C3,C10,C11,C12,C13",
  "C1,C2,C9,C10,C11,C12,C13",
  "C1,C8,C9,C10,C11,C12,C13",
  "C7,C8,C9,C10,C11,C12,C13",
  "C6,C7,C8,C9,C10,C11,C12",
  "C5,C6,C7,C8,C9,C10,C11",
  "C4,C5,C6,C7,C8,C9,C10",
  "C3,C4,C5,C6,C7,C8,C9",
  "C2,C3,C4,C5,C6,C7,C8",
  
  # Eliminasi 7
  "C1,C2,C3,C4,C5,C6",
  "C1,C2,C3,C4,C5,C13",
  "C1,C2,C3,C4,C12,C13",
  "C1,C2,C3,C11,C12,C13",
  "C1,C2,C10,C11,C12,C13",
  "C1,C9,C10,C11,C12,C13",
  "C8,C9,C10,C11,C12,C13",
  "C7,C8,C9,C10,C11,C12",
  "C6,C7,C8,C9,C10,C11",
  "C5,C6,C7,C8,C9,C10",
  "C4,C5,C6,C7,C8,C9",
  "C3,C4,C5,C6,C7,C8",
  "C2,C3,C4,C5,C6,C7",
  
  # Eliminasi 8
  "C1,C2,C3,C4,C5",
  "C1,C2,C3,C4,C13",
  "C1,C2,C3,C12,C13",
  "C1,C2,C11,C12,C13",
  "C1,C10,C11,C12,C13",
  "C9,C10,C11,C12,C13",
  "C8,C9,C10,C11,C12",
  "C7,C8,C9,C10,C11",
  "C6,C7,C8,C9,C10",
  "C5,C6,C7,C8,C9",
  "C4,C5,C6,C7,C8",
  "C3,C4,C5,C6,C7",
  "C2,C3,C4,C5,C6",
  
  # Eliminasi 9
  "C1,C2,C3,C4",
  "C1,C2,C3,C13",
  "C1,C2,C12,C13",
  "C1,C11,C12,C13",
  "C10,C11,C12,C13",
  "C9,C10,C11,C12",
  "C8,C9,C10,C11",
  "C7,C8,C9,C10",
  "C6,C7,C8,C9",
  "C5,C6,C7,C8",
  "C4,C5,C6,C7",
  "C3,C4,C5,C6",
  "C2,C3,C4,C5",
  
  # Eliminasi 10
  "C1,C2,C3",
  "C1,C2,C13",
  "C1,C12,C13",
  "C11,C12,C13",
  "C10,C11,C12",
  "C9,C10,C11",
  "C8,C9,C10",
  "C7,C8,C9",
  "C6,C7,C8",
  "C5,C6,C7",
  "C4,C5,C6",
  "C3,C4,C5",
  "C2,C3,C4",
  
  # Eliminasi 11
  "C1,C2",
  "C1,C13",
  "C12,C13",
  "C11,C12",
  "C10,C11",
  "C9,C10",
  "C8,C9",
  "C7,C8",
  "C6,C7",
  "C5,C6",
  "C4,C5",
  "C3,C4",
  "C2,C3",
  
  # Eliminasi 12
  "C1",
  "C13",
  "C12",
  "C11",
  "C10",
  "C9",
  "C8",
  "C7",
  "C6",
  "C5",
  "C4",
  "C3",
  "C2")

warna_custom<-c(
"Seluruh Kategori"="#FF0000",
"C1,C2,C3,C4,C5,C6,C7,C8,C9,C10,C11,C12"="#FB0300",
"C1,C2,C3,C4,C5,C6,C7,C8,C9,C10,C11,C13"="#F80600",
"C1,C2,C3,C4,C5,C6,C7,C8,C9,C10,C12,C13"="#F50900",
"C1,C2,C3,C4,C5,C6,C7,C8,C9,C11,C12,C13"="#F20C00",
"C1,C2,C3,C4,C5,C6,C7,C8,C10,C11,C12,C13"="#EF0F00",
"C1,C2,C3,C4,C5,C6,C7,C9,C10,C11,C12,C13"="#EC1200",
"C1,C2,C3,C4,C5,C6,C8,C9,C10,C11,C12,C13"="#E91500",
"C1,C2,C3,C4,C5,C7,C8,C9,C10,C11,C12,C13"="#E61800",
"C1,C2,C3,C4,C6,C7,C8,C9,C10,C11,C12,C13"="#E31B00",
"C1,C2,C3,C5,C6,C7,C8,C9,C10,C11,C12,C13"="#E01E00",
"C1,C2,C4,C5,C6,C7,C8,C9,C10,C11,C12,C13"="#DD2100",
"C1,C3,C4,C5,C6,C7,C8,C9,C10,C11,C12,C13"="#DA2400",
"C2,C3,C4,C5,C6,C7,C8,C9,C10,C11,C12,C13"="#D72700",
"C1,C2,C3,C4,C5,C6,C7,C8,C9,C10,C11"="#D42A00",
"C1,C2,C3,C4,C5,C6,C7,C8,C9,C10,C13"="#D12D00",
"C1,C2,C3,C4,C5,C6,C7,C8,C9,C12,C13"="#CE3000",
"C1,C2,C3,C4,C5,C6,C7,C8,C11,C12,C13"="#CB3300",
"C1,C2,C3,C4,C5,C6,C7,C10,C11,C12,C13"="#C83600",
"C1,C2,C3,C4,C5,C6,C9,C10,C11,C12,C13"="#C53900",
"C1,C2,C3,C4,C5,C8,C9,C10,C11,C12,C13"="#C23C00",
"C1,C2,C3,C4,C7,C8,C9,C10,C11,C12,C13"="#BF3F00",
"C1,C2,C3,C6,C7,C8,C9,C10,C11,C12,C13"="#BC4200",
"C1,C2,C5,C6,C7,C8,C9,C10,C11,C12,C13"="#B94500",
"C1,C4,C5,C6,C7,C8,C9,C10,C11,C12,C13"="#B64800",
"C3,C4,C5,C6,C7,C8,C9,C10,C11,C12,C13"="#B34B00",
"C2,C3,C4,C5,C6,C7,C8,C9,C10,C11,C12"="#B04E00",
"C1,C2,C3,C4,C5,C6,C7,C8,C9,C10"="#AD5100",
"C1,C2,C3,C4,C5,C6,C7,C8,C9,C13"="#AA5500",
"C1,C2,C3,C4,C5,C6,C7,C8,C12,C13"="#A65800",
"C1,C2,C3,C4,C5,C6,C7,C11,C12,C13"="#A35B00",
"C1,C2,C3,C4,C5,C6,C10,C11,C12,C13"="#A05E00",
"C1,C2,C3,C4,C5,C9,C10,C11,C12,C13"="#9D6100",
"C1,C2,C3,C4,C8,C9,C10,C11,C12,C13"="#9A6400",
"C1,C2,C3,C7,C8,C9,C10,C11,C12,C13"="#976700",
"C1,C2,C6,C7,C8,C9,C10,C11,C12,C13"="#946A00",
"C1,C5,C6,C7,C8,C9,C10,C11,C12,C13"="#916D00",
"C4,C5,C6,C7,C8,C9,C10,C11,C12,C13"="#8E7000",
"C3,C4,C5,C6,C7,C8,C9,C10,C11,C12"="#8B7300",
"C2,C3,C4,C5,C6,C7,C8,C9,C10,C11"="#887600",
"C1,C2,C3,C4,C5,C6,C7,C8,C9"="#857900",
"C1,C2,C3,C4,C5,C6,C7,C8,C13"="#827C00",
"C1,C2,C3,C4,C5,C6,C7,C12,C13"="#7F7F00",
"C1,C2,C3,C4,C5,C6,C11,C12,C13"="#7C8200",
"C1,C2,C3,C4,C5,C10,C11,C12,C13"="#798500",
"C1,C2,C3,C4,C9,C10,C11,C12,C13"="#768800",
"C1,C2,C3,C8,C9,C10,C11,C12,C13"="#738B00",
"C1,C2,C7,C8,C9,C10,C11,C12,C13"="#708E00",
"C1,C6,C7,C8,C9,C10,C11,C12,C13"="#6D9100",
"C5,C6,C7,C8,C9,C10,C11,C12,C13"="#6A9400",
"C4,C5,C6,C7,C8,C9,C10,C11,C12"="#679700",
"C3,C4,C5,C6,C7,C8,C9,C10,C11"="#649A00",
"C2,C3,C4,C5,C6,C7,C8,C9,C10"="#619D00",
"C1,C2,C3,C4,C5,C6,C7,C8"="#5EA000",
"C1,C2,C3,C4,C5,C6,C7,C13"="#5BA300",
"C1,C2,C3,C4,C5,C6,C12,C13"="#58A600",
"C1,C2,C3,C4,C5,C11,C12,C13"="#55AA00",
"C1,C2,C3,C4,C10,C11,C12,C13"="#51AD00",
"C1,C2,C3,C9,C10,C11,C12,C13"="#4EB000",
"C1,C2,C8,C9,C10,C11,C12,C13"="#4BB300",
"C1,C7,C8,C9,C10,C11,C12,C13"="#48B600",
"C6,C7,C8,C9,C10,C11,C12,C13"="#45B900",
"C5,C6,C7,C8,C9,C10,C11,C12"="#42BC00",
"C4,C5,C6,C7,C8,C9,C10,C11"="#3FBF00",
"C3,C4,C5,C6,C7,C8,C9,C10"="#3CC200",
"C2,C3,C4,C5,C6,C7,C8,C9"="#39C500",
"C1,C2,C3,C4,C5,C6,C7"="#36C800",
"C1,C2,C3,C4,C5,C6,C13"="#33CB00",
"C1,C2,C3,C4,C5,C12,C13"="#30CE00",
"C1,C2,C3,C4,C11,C12,C13"="#2DD100",
"C1,C2,C3,C10,C11,C12,C13"="#2AD400",
"C1,C2,C9,C10,C11,C12,C13"="#27D700",
"C1,C8,C9,C10,C11,C12,C13"="#24DA00",
"C7,C8,C9,C10,C11,C12,C13"="#21DD00",
"C6,C7,C8,C9,C10,C11,C12"="#1EE000",
"C5,C6,C7,C8,C9,C10,C11"="#1BE300",
"C4,C5,C6,C7,C8,C9,C10"="#18E600",
"C3,C4,C5,C6,C7,C8,C9"="#15E900",
"C2,C3,C4,C5,C6,C7,C8"="#12EC00",
"C1,C2,C3,C4,C5,C6"="#0FEF00",
"C1,C2,C3,C4,C5,C13"="#0CF200",
"C1,C2,C3,C4,C12,C13"="#09F500",
"C1,C2,C3,C11,C12,C13"="#06F800",
"C1,C2,C10,C11,C12,C13"="#03FB00",
"C1,C9,C10,C11,C12,C13"="#00FF00",
"C8,C9,C10,C11,C12,C13"="#00FB03",
"C7,C8,C9,C10,C11,C12"="#00F806",
"C6,C7,C8,C9,C10,C11"="#00F509",
"C5,C6,C7,C8,C9,C10"="#00F20C",
"C4,C5,C6,C7,C8,C9"="#00EF0F",
"C3,C4,C5,C6,C7,C8"="#00EC12",
"C2,C3,C4,C5,C6,C7"="#00E915",
"C1,C2,C3,C4,C5"="#00E618",
"C1,C2,C3,C4,C13"="#00E31B",
"C1,C2,C3,C12,C13"="#00E01E",
"C1,C2,C11,C12,C13"="#00DD21",
"C1,C10,C11,C12,C13"="#00DA24",
"C9,C10,C11,C12,C13"="#00D727",
"C8,C9,C10,C11,C12"="#00D42A",
"C7,C8,C9,C10,C11"="#00D12D",
"C6,C7,C8,C9,C10"="#00CE30",
"C5,C6,C7,C8,C9"="#00CB33",
"C4,C5,C6,C7,C8"="#00C836",
"C3,C4,C5,C6,C7"="#00C539",
"C2,C3,C4,C5,C6"="#00C23C",
"C1,C2,C3,C4"="#00BF3F",
"C1,C2,C3,C13"="#00BC42",
"C1,C2,C12,C13"="#00B945",
"C1,C11,C12,C13"="#00B648",
"C10,C11,C12,C13"="#00B34B",
"C9,C10,C11,C12"="#00B04E",
"C8,C9,C10,C11"="#00AD51",
"C7,C8,C9,C10"="#00AA54",
"C6,C7,C8,C9"="#00A658",
"C5,C6,C7,C8"="#00A35B",
"C4,C5,C6,C7"="#00A05E",
"C3,C4,C5,C6"="#009D61",
"C2,C3,C4,C5"="#009A64",
"C1,C2,C3"="#009767",
"C1,C2,C13"="#00946A",
"C1,C12,C13"="#00916D",
"C11,C12,C13"="#008E70",
"C10,C11,C12"="#008B73",
"C9,C10,C11"="#008876",
"C8,C9,C10"="#008579",
"C7,C8,C9"="#00827C",
"C6,C7,C8"="#007F7F",
"C5,C6,C7"="#007C82",
"C4,C5,C6"="#007985",
"C3,C4,C5"="#007688",
"C2,C3,C4"="#00738B",
"C1,C2"="#00708E",
"C1,C13"="#006D91",
"C12,C13"="#006A94",
"C11,C12"="#006797",
"C10,C11"="#00649A",
"C9,C10"="#00619D",
"C8,C9"="#005EA0",
"C7,C8"="#005BA3",
"C6,C7"="#0058A6",
"C5,C6"="#0055A9",
"C4,C5"="#0051AD",
"C3,C4"="#004EB0",
"C2,C3"="#004BB3",
"C1"="#0048B6",
"C13"="#0045B9",
"C12"="#0042BC",
"C11"="#003FBF",
"C10"="#003CC2",
"C9"="#0039C5",
"C8"="#0036C8",
"C7"="#0033CB",
"C6"="#0030CE",
"C5"="#002DD1",
"C4"="#002AD4",
"C3"="#0027D7",
"C2"="#0024DA")

ggplot(data = Significant_Map) +
  geom_sf(mapping=aes(geometry = geometry,fill = Variabel_Signifikan)) +
  scale_fill_manual(values = warna_custom)+
  labs(fill="Variabel Signifikan")+
  geom_text(
    aes(label = Kabupaten, x = coordinates(as(Dataset,"Spatial"))[,1], y = coordinates(as(Dataset,"Spatial"))[,2]),
    vjust = -0.5,
    color = "black",
    size = 1,
    check_overlap = TRUE
  )+ggtitle("Signifikansi Tiap Kategori")+xlab("Longitude")+ylab("Latitude")
