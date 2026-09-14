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
Dataset_No_kb6_1 <- Dataset[which(Dataset$kb6 != 1),]

# Menghapus baris data di mana kb6 = 13
Dataset_No_kb6_13 <- Dataset[which(Dataset$kb6 != 13),]

# Menghapus baris data di mana kb6 = 1 & 13
Dataset_No_kb6_1_and_13 <- Dataset[which(Dataset$kb6 != c(1,13)),]

library(dplyr)
# Unmeet Neet Summary All
#Unmeet_Need_summary <- Dataset%>%
#  group_by(Kabupaten) %>%
#  summarise(Jumlah.PUS = length(kb1),
#            Hamil.Ingin.Anak.Lagi = length(kb3a_alasan[which(kb3a_alasan==2)]),
#            Hamil.Tidak.Ingin.Anak.Lagi = length(kb3a_alasan[which(kb3a_alasan==3)]),
#            Tidak.Hamil.Ingin.Anak.Lagi = length(kb3b_alasan[which(kb3b_alasan==2)]),
#            Tidak.Hamil.Tidak.Ingin.Anak.Lagi = length(kb3b_alasan[which(kb3b_alasan==3)]))

#Unmeet_Need_summary$Unmet.Need.Spacing = Unmeet_Need_summary$Hamil.Ingin.Anak.Lagi+Unmeet_Need_summary$Tidak.Hamil.Ingin.Anak.Lagi
#Unmeet_Need_summary$Unmet.Need.Limiting = Unmeet_Need_summary$Hamil.Tidak.Ingin.Anak.Lagi+Unmeet_Need_summary$Tidak.Hamil.Tidak.Ingin.Anak.Lagi
#Unmeet_Need_summary$Jumlah = Unmeet_Need_summary$Unmet.Need.Spacing+Unmeet_Need_summary$Unmet.Need.Limiting
#Unmeet_Need_summary$Persentase = (Unmeet_Need_summary$Jumlah/Unmeet_Need_summary$Jumlah.PUS)*100
#writexl::write_xlsx(Unmeet_Need_summary,"Unmeet Need summary.xlsx")

# Unmeet Need Summary No kb6 = 1
Unmeet_Need_summary_No_kb6_1 <- Dataset_No_kb6_1%>%
  group_by(Kabupaten) %>%
  summarise(Jumlah.PUS = length(kb1),
            Hamil.Ingin.Anak.Lagi = length(kb3a_alasan[which(kb3a_alasan==2)]),
            Hamil.Tidak.Ingin.Anak.Lagi = length(kb3a_alasan[which(kb3a_alasan==3)]),
            Tidak.Hamil.Ingin.Anak.Lagi = length(kb3b_alasan[which(kb3b_alasan==2)]),
            Tidak.Hamil.Tidak.Ingin.Anak.Lagi = length(kb3b_alasan[which(kb3b_alasan==3)]))

Unmeet_Need_summary_No_kb6_1$Unmet.Need.Spacing = Unmeet_Need_summary_No_kb6_1$Hamil.Ingin.Anak.Lagi+Unmeet_Need_summary_No_kb6_1$Tidak.Hamil.Ingin.Anak.Lagi
Unmeet_Need_summary_No_kb6_1$Unmet.Need.Limiting = Unmeet_Need_summary_No_kb6_1$Hamil.Tidak.Ingin.Anak.Lagi+Unmeet_Need_summary_No_kb6_1$Tidak.Hamil.Tidak.Ingin.Anak.Lagi
Unmeet_Need_summary_No_kb6_1$Jumlah = Unmeet_Need_summary_No_kb6_1$Unmet.Need.Spacing+Unmeet_Need_summary_No_kb6_1$Unmet.Need.Limiting
Unmeet_Need_summary_No_kb6_1$Persentase = (Unmeet_Need_summary_No_kb6_1$Jumlah/Unmeet_Need_summary_No_kb6_1$Jumlah.PUS)*100
writexl::write_xlsx(Unmeet_Need_summary_No_kb6_1,"Unmeet Need summary No kb6 = 1.xlsx")

# Unmeet Neet Summary No kb6 = 13
Unmeet_Need_summary_No_kb6_13 <- Dataset_No_kb6_13%>%
  group_by(Kabupaten) %>%
  summarise(Jumlah.PUS = length(kb1),
            Hamil.Ingin.Anak.Lagi = length(kb3a_alasan[which(kb3a_alasan==2)]),
            Hamil.Tidak.Ingin.Anak.Lagi = length(kb3a_alasan[which(kb3a_alasan==3)]),
            Tidak.Hamil.Ingin.Anak.Lagi = length(kb3b_alasan[which(kb3b_alasan==2)]),
            Tidak.Hamil.Tidak.Ingin.Anak.Lagi = length(kb3b_alasan[which(kb3b_alasan==3)]))

Unmeet_Need_summary_No_kb6_13$Unmet.Need.Spacing = Unmeet_Need_summary_No_kb6_13$Hamil.Ingin.Anak.Lagi+Unmeet_Need_summary_No_kb6_13$Tidak.Hamil.Ingin.Anak.Lagi
Unmeet_Need_summary_No_kb6_13$Unmet.Need.Limiting = Unmeet_Need_summary_No_kb6_13$Hamil.Tidak.Ingin.Anak.Lagi+Unmeet_Need_summary_No_kb6_13$Tidak.Hamil.Tidak.Ingin.Anak.Lagi
Unmeet_Need_summary_No_kb6_13$Jumlah = Unmeet_Need_summary_No_kb6_13$Unmet.Need.Spacing+Unmeet_Need_summary_No_kb6_13$Unmet.Need.Limiting
Unmeet_Need_summary_No_kb6_13$Persentase = (Unmeet_Need_summary_No_kb6_13$Jumlah/Unmeet_Need_summary_No_kb6_13$Jumlah.PUS)*100
writexl::write_xlsx(Unmeet_Need_summary_No_kb6_13,"Unmeet Need summary No kb6 = 13.xlsx")

# Unmeet Neet Summary No kb6 = 1 and 13
Unmeet_Need_summary_No_kb6_1_and_13 <- Dataset_No_kb6_1_and_13%>%
  group_by(Kabupaten) %>%
  summarise(Jumlah.PUS = length(kb1),
            Hamil.Ingin.Anak.Lagi = length(kb3a_alasan[which(kb3a_alasan==2)]),
            Hamil.Tidak.Ingin.Anak.Lagi = length(kb3a_alasan[which(kb3a_alasan==3)]),
            Tidak.Hamil.Ingin.Anak.Lagi = length(kb3b_alasan[which(kb3b_alasan==2)]),
            Tidak.Hamil.Tidak.Ingin.Anak.Lagi = length(kb3b_alasan[which(kb3b_alasan==3)]))

Unmeet_Need_summary_No_kb6_1_and_13$Unmet.Need.Spacing = Unmeet_Need_summary_No_kb6_1_and_13$Hamil.Ingin.Anak.Lagi+Unmeet_Need_summary_No_kb6_1_and_13$Tidak.Hamil.Ingin.Anak.Lagi
Unmeet_Need_summary_No_kb6_1_and_13$Unmet.Need.Limiting = Unmeet_Need_summary_No_kb6_1_and_13$Hamil.Tidak.Ingin.Anak.Lagi+Unmeet_Need_summary_No_kb6_1_and_13$Tidak.Hamil.Tidak.Ingin.Anak.Lagi
Unmeet_Need_summary_No_kb6_1_and_13$Jumlah = Unmeet_Need_summary_No_kb6_1_and_13$Unmet.Need.Spacing+Unmeet_Need_summary_No_kb6_1_and_13$Unmet.Need.Limiting
Unmeet_Need_summary_No_kb6_1_and_13$Persentase = (Unmeet_Need_summary_No_kb6_1_and_13$Jumlah/Unmeet_Need_summary_No_kb6_1_and_13$Jumlah.PUS)*100
writexl::write_xlsx(Unmeet_Need_summary_No_kb6_1_and_13,"Unmeet Need summary No kb6 = 1 & 13.xlsx")

# Unmeet Need Summary No kb4 = 1
Unmeet_Need_summary_No_kb4_1 <- Dataset_No_kb4_1%>%
  group_by(Kabupaten) %>%
  summarise(Jumlah.PUS = length(kb1),
            Hamil.Ingin.Anak.Lagi = length(kb3a_alasan[which(kb3a_alasan==2)]),
            Hamil.Tidak.Ingin.Anak.Lagi = length(kb3a_alasan[which(kb3a_alasan==3)]),
            Tidak.Hamil.Ingin.Anak.Lagi = length(kb3b_alasan[which(kb3b_alasan==2)]),
            Tidak.Hamil.Tidak.Ingin.Anak.Lagi = length(kb3b_alasan[which(kb3b_alasan==3)]))

Unmeet_Need_summary_No_kb4_1$Unmet.Need.Spacing = Unmeet_Need_summary_No_kb4_1$Hamil.Ingin.Anak.Lagi+Unmeet_Need_summary_No_kb4_1$Tidak.Hamil.Ingin.Anak.Lagi
Unmeet_Need_summary_No_kb4_1$Unmet.Need.Limiting = Unmeet_Need_summary_No_kb4_1$Hamil.Tidak.Ingin.Anak.Lagi+Unmeet_Need_summary_No_kb4_1$Tidak.Hamil.Tidak.Ingin.Anak.Lagi
Unmeet_Need_summary_No_kb4_1$Jumlah = Unmeet_Need_summary_No_kb4_1$Unmet.Need.Spacing+Unmeet_Need_summary_No_kb4_1$Unmet.Need.Limiting
Unmeet_Need_summary_No_kb4_1$Persentase = (Unmeet_Need_summary_No_kb4_1$Jumlah/Unmeet_Need_summary_No_kb4_1$Jumlah.PUS)*100
writexl::write_xlsx(Unmeet_Need_summary_No_kb4_1,"Unmeet Need summary No kb4 = 1.xlsx")

# Unmeet Neet Summary No kb5 = 1
Unmeet_Need_summary_No_kb5_1 <- Dataset_No_kb5_1%>%
  group_by(Kabupaten) %>%
  summarise(Jumlah.PUS = length(kb1),
            Hamil.Ingin.Anak.Lagi = length(kb3a_alasan[which(kb3a_alasan==2)]),
            Hamil.Tidak.Ingin.Anak.Lagi = length(kb3a_alasan[which(kb3a_alasan==3)]),
            Tidak.Hamil.Ingin.Anak.Lagi = length(kb3b_alasan[which(kb3b_alasan==2)]),
            Tidak.Hamil.Tidak.Ingin.Anak.Lagi = length(kb3b_alasan[which(kb3b_alasan==3)]))

Unmeet_Need_summary_No_kb5_1$Unmet.Need.Spacing = Unmeet_Need_summary_No_kb5_1$Hamil.Ingin.Anak.Lagi+Unmeet_Need_summary_No_kb5_1$Tidak.Hamil.Ingin.Anak.Lagi
Unmeet_Need_summary_No_kb5_1$Unmet.Need.Limiting = Unmeet_Need_summary_No_kb5_1$Hamil.Tidak.Ingin.Anak.Lagi+Unmeet_Need_summary_No_kb5_1$Tidak.Hamil.Tidak.Ingin.Anak.Lagi
Unmeet_Need_summary_No_kb5_1$Jumlah = Unmeet_Need_summary_No_kb5_1$Unmet.Need.Spacing+Unmeet_Need_summary_No_kb5_1$Unmet.Need.Limiting
Unmeet_Need_summary_No_kb5_1$Persentase = (Unmeet_Need_summary_No_kb5_1$Jumlah/Unmeet_Need_summary_No_kb5_1$Jumlah.PUS)*100
writexl::write_xlsx(Unmeet_Need_summary_No_kb5_1,"Unmeet Need summary No kb5 = 1.xlsx")

# Unmeet Neet Summary No kb4 = 1 and kb5 = 1
Unmeet_Need_summary_No_kb4_1_and_kb5_1 <- Dataset_No_kb4_1_and_kb5_1%>%
  group_by(Kabupaten) %>%
  summarise(Jumlah.PUS = length(kb1),
            Hamil.Ingin.Anak.Lagi = length(kb3a_alasan[which(kb3a_alasan==2)]),
            Hamil.Tidak.Ingin.Anak.Lagi = length(kb3a_alasan[which(kb3a_alasan==3)]),
            Tidak.Hamil.Ingin.Anak.Lagi = length(kb3b_alasan[which(kb3b_alasan==2)]),
            Tidak.Hamil.Tidak.Ingin.Anak.Lagi = length(kb3b_alasan[which(kb3b_alasan==3)]))

Unmeet_Need_summary_No_kb4_1_and_kb5_1$Unmet.Need.Spacing = Unmeet_Need_summary_No_kb4_1_and_kb5_1$Hamil.Ingin.Anak.Lagi+Unmeet_Need_summary_No_kb4_1_and_kb5_1$Tidak.Hamil.Ingin.Anak.Lagi
Unmeet_Need_summary_No_kb4_1_and_kb5_1$Unmet.Need.Limiting = Unmeet_Need_summary_No_kb4_1_and_kb5_1$Hamil.Tidak.Ingin.Anak.Lagi+Unmeet_Need_summary_No_kb4_1_and_kb5_1$Tidak.Hamil.Tidak.Ingin.Anak.Lagi
Unmeet_Need_summary_No_kb4_1_and_kb5_1$Jumlah = Unmeet_Need_summary_No_kb4_1_and_kb5_1$Unmet.Need.Spacing+Unmeet_Need_summary_No_kb4_1_and_kb5_1$Unmet.Need.Limiting
Unmeet_Need_summary_No_kb4_1_and_kb5_1$Persentase = (Unmeet_Need_summary_No_kb4_1_and_kb5_1$Jumlah/Unmeet_Need_summary_No_kb4_1_and_kb5_1$Jumlah.PUS)*100
writexl::write_xlsx(Unmeet_Need_summary_No_kb4_1_and_kb5_1,"Unmeet Need summary No kb4 = 1 & kb5 = 1.xlsx")

# Category Summary
#Category_Summary <- Dataset%>%
#  group_by(Kabupaten) %>%
#  summarise(C1 = length(kb6[which(kb6==1)]),
#            C2 = length(kb6[which(kb6==2)]),
#            C3 = length(kb6[which(kb6==3)]),
#            C4 = length(kb6[which(kb6==4)]),
#            C5 = length(kb6[which(kb6==5)]),
#            C6 = length(kb6[which(kb6==6)]),
#            C7 = length(kb6[which(kb6==7)]),
#            C8 = length(kb6[which(kb6==8)]),
#            C9 = length(kb6[which(kb6==9)]),
#            C10 = length(kb6[which(kb6==10)]),
#            C11 = length(kb6[which(kb6==11)]),
#            C12 = length(kb6[which(kb6==12)]),
#            C13 = length(kb6[which(kb6==13)]))
#writexl::write_xlsx(Category_Summary,"Category Summary.xlsx")

Category_Summary_No_kb6_1 <- Dataset_No_kb6_1%>%
  group_by(Kabupaten) %>%
  summarise(C2 = length(kb6[which(kb6==2)]),
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
writexl::write_xlsx(Category_Summary_No_kb6_1,"Category Summary No kb6 = 1.xlsx")

Category_Summary_No_kb6_13 <- Dataset_No_kb6_13%>%
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
            C12 = length(kb6[which(kb6==12)]))
writexl::write_xlsx(Category_Summary_No_kb6_13,"Category Summary No kb6 = 13.xlsx")

Category_Summary_No_kb6_1_and_13 <- Dataset_No_kb6_1_and_13%>%
  group_by(Kabupaten) %>%
  summarise(C2 = length(kb6[which(kb6==2)]),
            C3 = length(kb6[which(kb6==3)]),
            C4 = length(kb6[which(kb6==4)]),
            C5 = length(kb6[which(kb6==5)]),
            C6 = length(kb6[which(kb6==6)]),
            C7 = length(kb6[which(kb6==7)]),
            C8 = length(kb6[which(kb6==8)]),
            C9 = length(kb6[which(kb6==9)]),
            C10 = length(kb6[which(kb6==10)]),
            C11 = length(kb6[which(kb6==11)]),
            C12 = length(kb6[which(kb6==12)]))
writexl::write_xlsx(Category_Summary_No_kb6_1_and_13,"Category Summary No kb6 = 1 and 13.xlsx")

Category_Summary_No_kb4_1 <- Dataset_No_kb4_1%>%
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
writexl::write_xlsx(Category_Summary_No_kb4_1,"Category Summary No kb4 = 1.xlsx")

Category_Summary_No_kb5_1 <- Dataset_No_kb5_1%>%
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
writexl::write_xlsx(Category_Summary_No_kb5_1,"Category Summary No kb5 = 1.xlsx")

Category_Summary_No_kb4_1_and_kb5_1 <- Dataset_No_kb4_1_and_kb5_1 %>%
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
writexl::write_xlsx(Category_Summary_No_kb4_1_and_kb5_1,"Category Summary No kb4 = 1 and kb5 = 1.xlsx")
