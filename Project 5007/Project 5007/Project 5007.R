# Import data
# library(haven)
# Dataset <- read_sav("blok_kb_pk21_brin.sav")
# str(Dataset)

## Mengubah ID kabupaten menjadi nama kabupaten.
# Dataset$id_kabupaten_rev <- haven::as_factor(Dataset$id_kabupaten_rev)

## Hapus kolom nama responden, ID provinsi, ID kecamatan dan kolom filter
# Dataset <- Dataset[,-c(1,2,4,14)]
# colnames(Dataset) <- c("Kabupaten","kb1","kb1_lahir_hidup",
#                       "kb1_masih_hidup","kb3","kb3a_alasan",
#                       "kb3b_alasan","kb4","kb5","kb6")
# Kabupaten <- unique(Dataset$Kabupaten)

library(dplyr)
## Summary
# kb3a dan kb5 = 1 tidak masuk
Subset_data11_summary <- Subset_data11%>%
  group_by(Kabupaten) %>%
  summarise(Jumlah.PUS = length(kb1),
            Hamil.Ingin.Anak.Lagi = length(kb3a_alasan[which(kb3a_alasan==2)]),
            Hamil.Tidak.Ingin.Anak.Lagi = length(kb3a_alasan[which(kb3a_alasan==3)]),
            Tidak.Hamil.Ingin.Anak.Lagi = length(kb3b_alasan[which(kb3b_alasan==2)]),
            Tidak.Hamil.Tidak.Ingin.Anak.Lagi = length(kb3b_alasan[which(kb3b_alasan==3)]))
Subset_data11_summary$Unmet.Need.Spacing = Subset_data11_summary$Hamil.Ingin.Anak.Lagi+Subset_data11_summary$Tidak.Hamil.Ingin.Anak.Lagi
Subset_data11_summary$Unmet.Need.Limiting = Subset_data11_summary$Hamil.Tidak.Ingin.Anak.Lagi+Subset_data11_summary$Tidak.Hamil.Tidak.Ingin.Anak.Lagi
Subset_data11_summary$Jumlah = Subset_data11_summary$Unmet.Need.Spacing+Subset_data11_summary$Unmet.Need.Limiting
Subset_data11_summary$Persentase = Subset_data11_summary$Jumlah/Subset_data11_summary$Jumlah.PUS*100
writexl::write_xlsx(Subset_data11_summary,"Subset data11 summary.xlsx")

# + kb6 respon 1 tidak masuk
Subset_data2_summary <- Subset_data2%>%
  group_by(Kabupaten) %>%
  summarise(Jumlah.PUS = length(kb1),
            Hamil.Ingin.Anak.Lagi = length(kb3a_alasan[which(kb3a_alasan==2)]),
            Hamil.Tidak.Ingin.Anak.Lagi = length(kb3a_alasan[which(kb3a_alasan==3)]),
            Tidak.Hamil.Ingin.Anak.Lagi = length(kb3b_alasan[which(kb3b_alasan==2)]),
            Tidak.Hamil.Tidak.Ingin.Anak.Lagi = length(kb3b_alasan[which(kb3b_alasan==3)]))
Subset_data2_summary$Unmet.Need.Spacing = Subset_data2_summary$Hamil.Ingin.Anak.Lagi+Subset_data2_summary$Tidak.Hamil.Ingin.Anak.Lagi
Subset_data2_summary$Unmet.Need.Limiting = Subset_data2_summary$Hamil.Tidak.Ingin.Anak.Lagi+Subset_data2_summary$Tidak.Hamil.Tidak.Ingin.Anak.Lagi
Subset_data2_summary$Jumlah = Subset_data2_summary$Unmet.Need.Spacing+Subset_data2_summary$Unmet.Need.Limiting
Subset_data2_summary$Persentase = Subset_data2_summary$Jumlah/Subset_data2_summary$Jumlah.PUS*100
writexl::write_xlsx(Subset_data2_summary,"Subset data no kb6 = 1 summary.xlsx")

# kb6 respon 13 tidak masuk
Subset_data3_summary <- Subset_data3%>%
  group_by(Kabupaten) %>%
  summarise(Jumlah.PUS = length(kb1),
            Hamil.Ingin.Anak.Lagi = length(kb3a_alasan[which(kb3a_alasan==2)]),
            Hamil.Tidak.Ingin.Anak.Lagi = length(kb3a_alasan[which(kb3a_alasan==3)]),
            Tidak.Hamil.Ingin.Anak.Lagi = length(kb3b_alasan[which(kb3b_alasan==2)]),
            Tidak.Hamil.Tidak.Ingin.Anak.Lagi = length(kb3b_alasan[which(kb3b_alasan==3)]))
Subset_data3_summary$Unmet.Need.Spacing = Subset_data3_summary$Hamil.Ingin.Anak.Lagi+Subset_data3_summary$Tidak.Hamil.Ingin.Anak.Lagi
Subset_data3_summary$Unmet.Need.Limiting = Subset_data3_summary$Hamil.Tidak.Ingin.Anak.Lagi+Subset_data3_summary$Tidak.Hamil.Tidak.Ingin.Anak.Lagi
Subset_data3_summary$Jumlah = Subset_data3_summary$Unmet.Need.Spacing+Subset_data3_summary$Unmet.Need.Limiting
Subset_data3_summary$Persentase = Subset_data3_summary$Jumlah/Subset_data3_summary$Jumlah.PUS*100
writexl::write_xlsx(Subset_data3_summary,"Subset data no kb6 = 13 summary.xlsx")

# kb6 respon 1 dan 13 tidak masuk
Subset_data4_summary <- Subset_data4%>%
  group_by(Kabupaten) %>%
  summarise(Jumlah.PUS = length(kb1),
            Hamil.Ingin.Anak.Lagi = length(kb3a_alasan[which(kb3a_alasan==2)]),
            Hamil.Tidak.Ingin.Anak.Lagi = length(kb3a_alasan[which(kb3a_alasan==3)]),
            Tidak.Hamil.Ingin.Anak.Lagi = length(kb3b_alasan[which(kb3b_alasan==2)]),
            Tidak.Hamil.Tidak.Ingin.Anak.Lagi = length(kb3b_alasan[which(kb3b_alasan==3)]))
Subset_data4_summary$Unmet.Need.Spacing = Subset_data4_summary$Hamil.Ingin.Anak.Lagi+Subset_data4_summary$Tidak.Hamil.Ingin.Anak.Lagi
Subset_data4_summary$Unmet.Need.Limiting = Subset_data4_summary$Hamil.Tidak.Ingin.Anak.Lagi+Subset_data4_summary$Tidak.Hamil.Tidak.Ingin.Anak.Lagi
Subset_data4_summary$Jumlah = Subset_data4_summary$Unmet.Need.Spacing+Subset_data4_summary$Unmet.Need.Limiting
Subset_data4_summary$Persentase = Subset_data4_summary$Jumlah/Subset_data4_summary$Jumlah.PUS*100
writexl::write_xlsx(Subset_data4_summary,"Subset data no kb6 = 1 & 13 summary.xlsx")

library(readxl)
Subset_data11 <- read_excel("Subset data 1 (kb3a & kb5 !=1).xlsx")
Subset_data11_summary <- read_excel("Subset data no kb6 = 1 & 13 summary.xlsx")
Subset_data2 <- read_excel("Subset data (kb3a,kb5,kb6 !=1).xlsx")
Subset_data2_summary <- read_excel("Subset data no kb6 = 1 summary.xlsx")
Subset_data3 <- read_excel("Subset data (kb3a,kb5 != 1,kb6 !=13).xlsx")
Subset_data3_summary <- read_excel("Subset data no kb6 = 13 summary.xlsx")
Subset_data4 <- read_excel("Subset data (kb3a,kb5,kb6 != 1,kb6 !=13).xlsx")
Subset_data4_summary <- read_excel("Subset data no kb6 = 1 & 13 summary.xlsx")

Subset_data11_summary.sum <- read_excel("Subset data no kb6 = 1 & 13 summary sum.xlsx")
Subset_data2_summary.sum <- read_excel("Subset data no kb6 = 1 summary sum.xlsx")
Subset_data3_summary.sum <- read_excel("Subset data no kb6 = 13 summary sum.xlsx")
Subset_data4_summary.sum <- read_excel("Subset data no kb6 = 1 & 13 summary sum.xlsx")


Dataset.new <- read_excel("Dataset filtered.xlsx")
head(Dataset.new)
names(Dataset.new)
Dataset.new<-Dataset.new[,-2]
Dataset.new <- data.frame(Dataset.new)

# Import Unmet Need data
Unmet_need <- read_excel('Persentase Unmet Need.xlsx')
head(Unmet_need)

# Gabungkan kedua data
All_data <- merge(Dataset.new,Unmet_need,by="KABUPATEN")
head(All_data)

# Import data Peta
library(sf)
Map <- read_sf("Peta Kabupaten/BATAS KABUPATEN KOTA DESEMBER 2019 DUKCAPIL.shp")
head(Map)
colnames(Map) <- c("KABUPATEN","geometry")

Mapdata<-merge(All_data,Map,by="KABUPATEN")
Mapdata$`Persentasi Unmet Need`

library(spgwr)
colex0 <- lm(`Persentasi Unmet Need` ~ kb1 + kb1_lahir_hidup + kb1_masih_hidup+
               kb3 + kb3a_alasan + kb5 + kb6,data=Mapdata)
summary(colex0)

resid<-residuals(colex0)
par(mfrow=c(2,2))
qqnorm(resid); qqline(resid, col="red"); 
plot(resid~fitted(colex0),xlab = "Predicted Values",ylab = "Residuals")
abline(h=0, col="red")
hist(resid) #histogram utk residual
plot(1:nrow(Mapdata), resid, pch=20,type="b")
abline(h=0, col="red")

shapiro.test(resid) # Normality test

lmtest::bptest(colex0) # Heteroskedastisity test

Mapdata <- st_as_sf(Mapdata)
colnames(Mapdata) <- c("KABUPATEN","kb1","kb1_lahir_hidup","kb1_masih_hidup","kb3","kb3a_alasan","kb5","kb6","PUN","geometry")
library(spdep)
library(sp)
coords<-data.frame(x = coordinates(as(Mapdata,"Spatial"))[,1], y = coordinates(as(Mapdata,"Spatial"))[,2])
jarak<-as.matrix(1/dist(coords))
lm.morantest(colex0,listw=mat2listw(jarak), alternative="two.sided") # Moran Index

## Basic GWR
# Menentukan bandwidth optimal
library(GWmodel)
# determine the kernel bandwidth

bw <- bw.gwr(PUN ~ kb1 + kb1_lahir_hidup + kb1_masih_hidup+
               kb3 + kb3a_alasan + kb5 + kb6,
             approach = "AIC",
             adaptive = T,
             data=as(Mapdata,"Spatial"))

# Modelling
m.gwr <- gwr.basic(PUN ~ kb1 + kb1_lahir_hidup + kb1_masih_hidup+
                     kb3 + kb3a_alasan + kb5 + kb6,
                   adaptive = T,
                   data=as(Mapdata,"Spatial"),
                   bw = bw)
# Evaluation
summary(m.gwr$SDF)

tab.gwr.bisquare <- rbind(apply(m.gwr.bisquare$SDF@data[, 1:7], 2, summary), coef(m))
rownames(tab.gwr.bisquare)[7] <- "Global"
tab.gwr.bisquare <- round(tab.gwr.bisquare, 1)
t(tab.gwr.bisquare)


gwr_sf.bisquare = st_as_sf(m.gwr.bisquare$SDF)

tm_shape(gwr_sf.bisquare) +
  tm_fill(c("PctBach", "PctPov"), palette = "viridis", style = "kmeans") +
  tm_layout(legend.position = c("right","top"), frame = F)
tm_shape(gwr_sf.bisquare) +
  tm_fill(c("PctFB", "PctBlack"),midpoint = 0, style = "kmeans") +
  tm_style("col_blind")+
  tm_layout(legend.position = c("right","top"), frame = F)