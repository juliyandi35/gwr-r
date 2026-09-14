# Import data
library(haven)
Dataset <- read_sav("blok_kb_pk21_brin.sav")
str(Dataset)

# Mengubah ID kabupaten menjadi nama kabupaten.
Dataset$id_kabupaten_rev <- haven::as_factor(Dataset$id_kabupaten_rev)

# Hapus kolom nama responden, ID provinsi, ID kecamatan dan kolom filter
Dataset <- Dataset[,-c(1,2,4,14)]
colnames(Dataset) <- c("Kabupaten","kb1","kb1_lahir_hidup",
                       "kb1_masih_hidup","kb3","kb3a_alasan",
                       "kb3b_alasan","kb4","kb5","kb6")
Kabupaten <- unique(Dataset$Kabupaten)

# Menghapus baris data di mana kb6 = 1
Dataset <- Dataset[which(Dataset$kb1 <= 14),]
Dataset <- Dataset[which(Dataset$kb1_lahir_hidup <= 14),]
Dataset <- Dataset[which(Dataset$kb1_masih_hidup <= 14),]

# Syarat 1
Dataset1 <- Dataset[which(Dataset$kb3a_alasan != 1),]

library(dplyr)
# Unmeet Need Summary New
Unmeet_Need_summary1 <- Dataset1%>%
  group_by(Kabupaten) %>%
  summarise(Jumlah.PUS = length(kb1),
            Hamil.Ingin.Anak.Lagi = length(kb3a_alasan[which(kb3a_alasan==2)]),
            Hamil.Tidak.Ingin.Anak.Lagi = length(kb3a_alasan[which(kb3a_alasan==3)]),
            Tidak.Hamil.Ingin.Anak.Lagi = length(kb3b_alasan[which(kb3b_alasan==2)]),
            Tidak.Hamil.Tidak.Ingin.Anak.Lagi = length(kb3b_alasan[which(kb3b_alasan==3)]))

Unmeet_Need_summary1$Unmet.Need.Spacing = Unmeet_Need_summary1$Hamil.Ingin.Anak.Lagi+Unmeet_Need_summary1$Tidak.Hamil.Ingin.Anak.Lagi
Unmeet_Need_summary1$Unmet.Need.Limiting = Unmeet_Need_summary1$Hamil.Tidak.Ingin.Anak.Lagi+Unmeet_Need_summary1$Tidak.Hamil.Tidak.Ingin.Anak.Lagi
Unmeet_Need_summary1$Jumlah = Unmeet_Need_summary1$Unmet.Need.Spacing+Unmeet_Need_summary1$Unmet.Need.Limiting
Unmeet_Need_summary1$Persentase = (Unmeet_Need_summary1$Jumlah/Unmeet_Need_summary1$Jumlah.PUS)*100
writexl::write_xlsx(Unmeet_Need_summary1,"Unmeet Need summary Syarat 1.xlsx")

# Syarat 2
Dataset2 <- Dataset[which(Dataset$kb3b_alasan != 1),]
Dataset2 <- Dataset2[which(Dataset2$kb4 == 2),]
Dataset2 <- Dataset2[which(Dataset2$kb6 != 1),]
Dataset2 <- Dataset2[which(Dataset2$kb6 != 13),]

Unmeet_Need_summary2 <- Dataset2%>%
  group_by(Kabupaten) %>%
  summarise(Jumlah.PUS = length(kb1),
            Hamil.Ingin.Anak.Lagi = length(kb3a_alasan[which(kb3a_alasan==2)]),
            Hamil.Tidak.Ingin.Anak.Lagi = length(kb3a_alasan[which(kb3a_alasan==3)]),
            Tidak.Hamil.Ingin.Anak.Lagi = length(kb3b_alasan[which(kb3b_alasan==2)]),
            Tidak.Hamil.Tidak.Ingin.Anak.Lagi = length(kb3b_alasan[which(kb3b_alasan==3)]))

Unmeet_Need_summary2$Unmet.Need.Spacing = Unmeet_Need_summary2$Hamil.Ingin.Anak.Lagi+Unmeet_Need_summary2$Tidak.Hamil.Ingin.Anak.Lagi
Unmeet_Need_summary2$Unmet.Need.Limiting = Unmeet_Need_summary2$Hamil.Tidak.Ingin.Anak.Lagi+Unmeet_Need_summary2$Tidak.Hamil.Tidak.Ingin.Anak.Lagi
Unmeet_Need_summary2$Jumlah = Unmeet_Need_summary2$Unmet.Need.Spacing+Unmeet_Need_summary2$Unmet.Need.Limiting
Unmeet_Need_summary2$Persentase = (Unmeet_Need_summary2$Jumlah/Unmeet_Need_summary2$Jumlah.PUS)*100
writexl::write_xlsx(Unmeet_Need_summary2,"Unmeet Need summary Syarat 2.xlsx")

library(dplyr)
Category_1 <- Dataset1 %>%
  group_by(Kabupaten) %>%
  summarise(C1 = length(kb6[which(kb6==1)]),
            C2 = length(kb6[which(kb6==2)]),
            C3 = length(kb6[which(kb6==3)]),
            C4 = length(kb6[which(kb6==4)]),
            C5 = length(kb6[which(kb6==5)]),
            C6 = length(kb6[which(kb6==6)]),
            C7 = length(kb6[which(kb6==7)]),
            C8 = length(kb6[which(kb6==8)]),
            C9 = length(kb6[which(kb6==9)]),
            C10 = length(kb6[which(kb6==10)]),
            C11 = length(kb6[which(kb6==11)]),
            C12 = length(kb6[which(kb6==12)]),
            C13 = length(kb6[which(kb6==13)]))
writexl::write_xlsx(Category_1,"Category Summary Syarat 1.xlsx")

Category_2 <- Dataset2 %>%
  group_by(Kabupaten) %>%
  summarise(C1 = length(kb6[which(kb6==1)]),
            C2 = length(kb6[which(kb6==2)]),
            C3 = length(kb6[which(kb6==3)]),
            C4 = length(kb6[which(kb6==4)]),
            C5 = length(kb6[which(kb6==5)]),
            C6 = length(kb6[which(kb6==6)]),
            C7 = length(kb6[which(kb6==7)]),
            C8 = length(kb6[which(kb6==8)]),
            C9 = length(kb6[which(kb6==9)]),
            C10 = length(kb6[which(kb6==10)]),
            C11 = length(kb6[which(kb6==11)]),
            C12 = length(kb6[which(kb6==12)]),
            C13 = length(kb6[which(kb6==13)]))
writexl::write_xlsx(Category_2,"Category Summary Syarat 2.xlsx")
