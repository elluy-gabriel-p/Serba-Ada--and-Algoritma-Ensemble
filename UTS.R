# Langkah 1: Pengumpulan dan Pemuatan Data
  # Diasumsikan data disimpan dalam file CSV bernama 'dataset_UTS.csv'
  data_ecommerce <- read.csv("dataset_UTS.csv")

  # Langkah 2: Eksplorasi Data
  str(data_ecommerce)  # Periksa struktur dataset

  library(tidyverse)
  ecom <- mutate(data_ecommerce[, c(-6)], # mendapatkan hanya kolom yang kita butuhkan
                PurchaseDate = as.Date(PurchaseDate, format = '%m/%d/%Y'), # mengubah format tanggal
                TransactionAmount = as.numeric(TransactionAmount)) # mengubah ke numerik

  head(ecom) # Tampilkan beberapa baris pertama
  summary(ecom)  # Statistik ringkasan

  # memeriksa apakah semua kasus lengkap
  table(complete.cases(ecom))
  complete.cases(ecom)

  # Langkah 3: Pembersihan Data
  ecom$TransactionAmount[is.na(ecom$TransactionAmount)] <- median(ecom$TransactionAmount, na.rm = TRUE)
  ecom_new=na.omit(ecom)

  table(complete.cases(ecom_new))
  summary(ecom_new) # Statistik ringkasan

  ecom_new = ecom_new %>%
    filter(TransactionAmount > 0)

  summary(ecom_new) 
  str(ecom_new)  # Periksa struktur dataset

  library(VIM)
  aggr(ecom_new, numbers=TRUE, prop=FALSE)
  marginplot(ecom_new[c("PurchaseDate","TransactionAmount")],pch=c(18),col=c("blue","red"))

  ecom_new %>% 
    arrange(desc(TransactionAmount)) %>%  
    # head(10)
    tail(10)

# No 2 a
  # Langkah 4: Analisis Data
  library(rfm)

  # Asumsikan tanggal analisis adalah satu hari setelah tanggal transaksi terakhir dalam dataset
  analysis_date <- max(ecom_new$PurchaseDate) + 1

  # Hitung nilai Recency, Frequency, dan Monetary
  rfm_data <- ecom_new %>% 
    group_by(CustomerID) %>%
    summarise(
      Recency = as.numeric(analysis_date - max(as.Date(PurchaseDate, "%m/%d/%Y"))),
      Frequency = n(),
      Monetary = sum(TransactionAmount)
    )

  # Skor setiap variabel RFM dari 1-5
  rfm_data <- rfm_data %>%
    mutate(
      R_score = ntile(-Recency, 5),
      F_score = ntile(Frequency, 5),
      M_score = ntile(Monetary, 5)
    )

  # Tentukan segmen berdasarkan skor RFM
  rfm_data <- rfm_data %>%
    mutate(
      R_level = ifelse(R_score %in% 4:5, "High", "Low"),
      F_level = ifelse(F_score %in% 4:5, "High", "Low"),
      M_level = ifelse(M_score %in% 4:5, "High", "Low"),
      Segment = paste(R_level, F_level, M_level, sep = "-")
    )

# Ubah nama segmen
rfm_data$Segment <- recode(rfm_data$Segment,
                           "High-High-High" = "Champions",
                           "High-High-Low" = "Loyal Customers",
                           "High-Low-High" = "Potential Loyalist",
                           "High-Low-Low" = "New Customers",
                           "Low-High-High" = "Promising",
                           "Low-High-Low" = "Need Attention",
                           "Low-Low-High" = "About To Sleep",
                           "Low-Low-Low" = "At Risk"
)

  # Tampilkan distribusi pelanggan di segmen
  segment_counts <- rfm_data %>%
    group_by(Segment) %>%
    summarise(Count = n())

  print(segment_counts)

  # Tentukan nama segmen dan karakteristiknya
  segment_names <- c("Champions", "Loyal Customers", "Potential Loyalists", "New Customers", "Promising", "Needs Attention", "About To Sleep", "At Risk")
  segment_characteristics <- c(
    "Pelanggan ini sering melakukan pembelian, menghabiskan banyak, dan merespon dengan baik terhadap promosi. Mereka adalah pelanggan paling berharga.",
    "Pelanggan ini sering melakukan pembelian dan menghabiskan banyak, tetapi tidak merespon dengan baik terhadap promosi.",
    "Pelanggan ini sering melakukan pembelian, tidak menghabiskan banyak, tetapi merespon dengan baik terhadap promosi.",
    "Pelanggan ini sering melakukan pembelian tetapi tidak menghabiskan banyak dan tidak merespon dengan baik terhadap promosi.",
    "Pelanggan ini tidak sering melakukan pembelian, tetapi ketika mereka melakukannya, mereka menghabiskan banyak dan merespon dengan baik terhadap promosi.",
    "Pelanggan ini tidak sering melakukan pembelian, tetapi ketika mereka melakukannya, mereka menghabiskan banyak. Namun, mereka tidak merespon dengan baik terhadap promosi.",
    "Pelanggan ini jarang melakukan pembelian dan tidak menghabiskan banyak, tetapi mereka merespon dengan baik terhadap promosi.",
    "Pelanggan ini jarang melakukan pembelian, tidak menghabiskan banyak, dan tidak merespon dengan baik terhadap promosi. Mereka mungkin adalah pelanggan yang paling sedikit terlibat."
  )

  # Segmen prioritas dan strategi promosi
  priority_segments <- c("Champions", "Loyal Customers", "Potential Loyalists")
  promotional_strategies <- c(
    "Exclusive discounts, loyalty rewards program, akses awal ke produk baru",
    "Personalized recommendations berdasarkan kategori pengeluaran tinggi, diskon yang ditargetkan",
    "Bundle deals termasuk item kategori pengeluaran tinggi, special offers untuk kategori pengeluaran menengah"
  )

  # Cetak nama segmen dan karakteristiknya
  for (i in 1:length(segment_names)) {
    print(paste(segment_names[i], ":", segment_characteristics[i]))
  }

  # Cetak segmen prioritas dan strategi promosi
  for (i in 1:length(priority_segments)) {
    print(paste(priority_segments[i], ":", promotional_strategies[i]))
  }



# NO 2 b
# Muat library yang diperlukan
library(tidyverse)
library(cluster)
library(ggplot2)

# Pilih variabel RFM untuk pengelompokan
rfm_cluster <- rfm_data[, c("R_score", "F_score", "M_score")]

# Skala variabel RFM
scaled_rfm <- scale(rfm_cluster)

# Lakukan pengelompokan K-means dengan jumlah kluster yang bervariasi
wss <- (nrow(scaled_rfm)-1)*sum(apply(scaled_rfm,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(scaled_rfm, centers=i)$withinss)

# Plot metode Elbow
plot(1:15, wss, type="b", xlab="Jumlah Kluster", ylab="Jumlah kuadrat dalam grup")

# Terapkan pengelompokan K-means dengan jumlah kluster yang dipilih
k <- 7  # Pilih jumlah kluster optimal berdasarkan plot Elbow
kmeans_model <- kmeans(scaled_rfm, centers = k)

# Tambahkan informasi kluster ke data RFM asli
kmeans_data <- rfm_data
kmeans_data$Cluster <- as.factor(kmeans_model$cluster)

# Ringkas karakteristik setiap kluster
cluster_summary <- kmeans_data %>%
  group_by(Cluster) %>%
  summarise(
    Count = n(),
    Avg_Recency = mean(Recency),
    Avg_Frequency = mean(Frequency),
    Avg_Monetary = mean(Monetary)
  )

print(cluster_summary) # Cetak ringkasan kluster

# Visualisasikan kluster
ggplot(kmeans_data, aes(x = Recency, y = Frequency, color = Cluster)) +
  geom_point() +
  labs(x = "Recency", y = "Frequency", color = "Cluster") +
  ggtitle("Kluster RFM")

  # Define segment names and characteristics
  segment_names <- c("High-Spenders, Recent (41)", "Low-Spenders, Recent (55)", "Medium Spenders, Less Recent (54)", "High-Spenders, Less Recent (43)", "Low-Spenders, Least Recent (73)", "Medium Spenders, Average Recency (50)", "High-Spenders, Dormant (55)")
  segment_characteristics <- c(
    "Pelanggan ini memiliki recency rendah (sering membeli) dan nilai pesanan rata-rata yang tinggi. Mereka mungkin pelanggan setia yang secara teratur melakukan pembelian signifikan.",
    "Pelanggan ini memiliki recency rendah (sering membeli) tetapi nilai pesanan rata-rata rendah. Mereka bisa jadi pelanggan baru atau sadar anggaran yang sering membeli tetapi dalam jumlah kecil.",
    "Pelanggan ini memiliki recency rata-rata (kadang-kadang membeli) dan nilai pesanan rata-rata sedang. Mereka bisa jadi pelanggan yang kembali yang membeli dengan beberapa regularitas tetapi tidak sering.",
    "Pelanggan ini memiliki recency lebih rendah (kurang sering membeli) tetapi nilai pesanan rata-rata tertinggi. Mereka bisa jadi pelanggan yang melakukan pembelian berharga tinggi yang jarang.",
    "Pelanggan ini memiliki recency tertinggi (paling jarang membeli) dan nilai pesanan rata-rata rendah. Mereka bisa jadi pelanggan yang jarang, sadar anggaran.",
    "Pelanggan ini memiliki recency rata-rata dan nilai pesanan rata-rata rendah. Mereka mungkin pelanggan yang peka harga yang membeli sesekali.",
    "Pelanggan ini memiliki recency tertinggi (belum membeli baru-baru ini) tetapi nilai pesanan rata-rata tinggi. Mereka bisa jadi pelanggan setia sebelumnya yang belum kembali dalam beberapa waktu tetapi memiliki riwayat pengeluaran tinggi."
  )

# Prioritaskan kluster berdasarkan nilai moneter dan aktivitas terbaru
priority_clusters <- c("Cluster 4", "Cluster 6")
# Tentukan strategi promosi untuk setiap kluster
promotional_strategies <- list(
  "Cluster 4" = c(
    "Personalized recommendations: Sarankan produk pelengkap berdasarkan riwayat pembelian mereka.",
    "Exclusive benefits: Tawarkan mereka pengiriman cepat gratis, jendela pengembalian yang diperpanjang, atau akses ke jalur layanan pelanggan khusus.",
    "Early bird offers Izinkan mereka akses awal ke produk baru atau barang edisi terbatas."
  ),
  "Cluster 6" = c(
    "Win-back campaigns: Kembalikan keterlibatan mereka dengan diskon personal atau penawaran khusus berdasarkan perilaku pembelian masa lalu mereka.",
    "Abandoned cart reminders: Ingatkan mereka tentang barang yang terlupakan di keranjang mereka dan tawarkan insentif untuk menyelesaikan pembelian.",
    "Personalized product recommendations: Rekomendasikan item yang mungkin mereka minati berdasarkan pembelian dan riwayat penelusuran sebelumnya."
  )
)

# Cetak nama segmen dan karakteristiknya
for (i in 1:length(segment_names)) {
  print(paste(segment_names[i], ":", segment_characteristics[i]))
}

# Cetak kluster yang diprioritaskan dan strategi yang direkomendasikan
for (cluster in priority_clusters) {
  print(paste0(cluster, ":"))
  for (strategy in promotional_strategies[[cluster]]) {
    print(strategy)
  }
}

# NO 2 c

# Muat pustaka yang diperlukan
library(dendextend)

# Pilih variabel RFM untuk pengelompokan
rfm_cluster <- rfm_data[, c("R_score", "F_score", "M_score")]

# Skala variabel RFM
scaled_rfm <- scale(rfm_cluster)

# Lakukan Pengelompokan Hierarki
hc <- hclust(dist(scaled_rfm), method = "ward.D2")

plot(hc, main = "Dendrogram Pengelompokan Hierarki")
rect.hclust(hc, k = 7, border = "black", lty = "dashed")   # Potong dendrogram pada k=7 kluster

# Potong pohon menjadi kluster
k <- 7  # Pilih jumlah kluster optimal berdasarkan dendrogram
clusters <- cutree(hc, k)

# Tambahkan informasi kluster ke data RFM asli
rfm_data$Cluster <- as.factor(clusters)

# Ringkasan karakteristik setiap kluster
cluster_summary <- rfm_data %>%
  group_by(Cluster) %>%
  summarise(
    Count = n(),
    Avg_Recency = mean(Recency),
    Avg_Frequency = mean(Frequency),
    Avg_Monetary = mean(Monetary)
  )

print(cluster_summary)

# Visualisasikan dendrogram
dend <- as.dendrogram(hc)
plot(dend)
  # Define segment names and characteristics
  segment_names <- c("Kluster 1 (60 anggota)", "Kluster 2 (50 anggota)", "Kluster 3 (66 anggota)", "Pengeluar Tinggi, Kurang Baru (43)", "Kluster 4 (50 anggota)", "Kluster 5 (37 anggota)", "Kluster 6 (52 anggota)")
  segment_characteristics <- c(
    "Low Frequency, Low Monetary: Pelanggan ini jarang melakukan pembelian dan menghabiskan jumlah yang relatif rendah per pembelian.",
    "Moderate Recency, Moderate Frequency, Moderate Monetary: Pelanggan ini menunjukkan perilaku yang agak seimbang dalam hal frekuensi pembelian dan jumlahnya.",
    "High Recency (Paling Tidak Baru), High Frequency, Low Monetary: Pelanggan ini sering melakukan pembelian tetapi dengan pengeluaran rata-rata yang rendah per pembelian, dan pembelian terakhir mereka mungkin sudah lama.",
    "Moderate Recency, High Frequency, Moderate Monetary: Mirip dengan Kluster 3, pelanggan ini sering melakukan pembelian tetapi dengan pengeluaran rata-rata yang moderat.",
    "Low Recency (Paling Baru), High Frequency, High Monetary: Pelanggan ini melakukan pembelian dengan frekuensi tinggi dengan pengeluaran rata-rata yang tinggi, dan pembelian terakhir mereka adalah baru-baru ini.",
    "Moderate Recency, Very High Frequency, High Monetary (Sedikit Lebih Rendah dari Kluster 5): Pelanggan ini melakukan pembelian dengan frekuensi tinggi dengan pengeluaran rata-rata yang tinggi, dan pembelian terakhir mereka adalah baru-baru ini.",
    "Moderate Recency, Moderate Frequency, Low Monetary (Sedikit Lebih Tinggi dari Kluster 3): Pelanggan ini menunjukkan pola yang mirip dengan Kluster 3 tetapi dengan pengeluaran rata-rata yang sedikit lebih tinggi per pembelian."
  )
# Priority segments and promotional strategies
priority_clusters <- c("Cluster 1", "Cluster 2")
promotional_strategies <- c(
  "pertimbangkan program loyalty atau hadiah untuk pembelian yang sering karena mereka sudah memiliki frekuensi pembelian yang tinggi. Ini dapat mendorong mereka untuk mempertahankan atau meningkatkan pengeluaran mereka.",
  "pertimbangkan campaigns keterlibatan kembali seperti penawaran eksklusif atau rekomendasi personalisasi untuk mendorong mereka melakukan pembelian lebih sering.",
)

# Print segment names and characteristics
for (i in 1:length(segment_names)) {
  print(paste(segment_names[i], ":", segment_characteristics[i]))
}

  # Print priority segments and promotional strategies
  for (i in 1:length(priority_clusters)) {
    print(paste(priority_clusters[i], ":", promotional_strategies[i]))
  }


# NO 2 d
# Analisis RFM: Metode ini sederhana dan mudah diinterpretasikan. Ini mengelompokkan pelanggan berdasarkan tiga faktor: Ketepatan Waktu, Frekuensi, dan Nilai Moneter. Namun, metode ini tidak mempertimbangkan karakteristik pelanggan lainnya yang mungkin penting.
# Pengelompokan K-means: Metode ini dapat mengelompokkan pelanggan ke dalam sejumlah kluster yang telah ditentukan berdasarkan semua variabel yang tersedia, bukan hanya RFM. Metode ini lebih fleksibel daripada analisis RFM dan dapat mengungkap segmen pelanggan yang lebih kompleks. Namun, memilih jumlah kluster yang tepat bisa menjadi tantangan, dan hasilnya dapat berubah dengan pusat kluster awal yang berbeda.
# Pengelompokan Hierarki (Agglomerative): Metode ini menyediakan hierarki kluster, yang dapat berguna jika ingin memahami hubungan antara segmen pelanggan yang berbeda. Metode ini tidak memerlukan penentuan jumlah kluster di awal. Namun, metode ini bisa menjadi intensif secara komputasi dengan dataset yang besar dan mungkin tidak bekerja dengan baik dengan data berdimensi tinggi.

# Berdasarkan pertimbangan ini, jika interpretasi dan kemudahan implementasi adalah pertimbangan utama, Analisis RFM mungkin menjadi pilihan yang disukai. Namun, jika mencari segmentasi yang lebih halus dan bersedia untuk berinvestasi dalam memahami dan menyetel algoritma pengelompokan, K-means atau Pengelompokan Hierarki dapat memberikan wawasan yang lebih dalam tentang karakteristik pelanggan.
# Untuk mengeksplorasi karakteristik pelanggan e-commerce, dengan mempertimbangkan keseimbangan antara interpretasi dan granularitas, saya akan merekomendasikan untuk memulai dengan Analisis RFM. Jika segmentasi yang lebih rinci diperlukan, maka pengelompokan K-means dengan pertimbangan yang hati-hati tentang jumlah kluster optimal menggunakan metode Elbow bisa menjadi langkah berikutnya yang baik.

# Untuk eksplorasi awal karakteristik pelanggan e-commerce, Pengelompokan Hierarki dengan analisis RFM umumnya menjadi pilihan yang baik karena fleksibilitas dan pendekatan yang berbasis data. Metode ini memungkinkan untuk menemukan pengelompokan alami dalam basis pelanggan berdasarkan perilaku pembelian mereka.
# Namun, jika sudah memiliki pemahaman yang kuat tentang segmen pelanggan potensial, K-means dengan Metode Elbow bisa menjadi cara yang lebih cepat dan efisien untuk menetapkan pelanggan ke grup yang telah ditentukan sebelumnya untuk analisis lebih lanjut atau kampanye pemasaran yang ditargetkan.

# NO 3
# step 5: Data Visualization

# Load required libraries
library(ggplot2)
library(dplyr)

# Visualize the distribution of Recency, Frequency, and Monetary values
ggplot(rfm_data, aes(x = Recency, y = Frequency, color = M_score)) +
  geom_point() +
  labs(x = "Recency", y = "Frequency", color = "Monetary Score") +
  ggtitle("RFM Analysis: Recency, Frequency, and Monetary Distribution")



# Visualize the K-means clusters based on RFM values
ggplot(kmeans_data, aes(x = Recency, y = Frequency, color = Cluster)) +
  geom_point() +
  labs(x = "Recency", y = "Frequency", color = "Cluster") +
  ggtitle("K-means Clustering: RFM Clusters")

# Visualize the Hierarchical Clustering dendrogram
dend <- as.dendrogram(hc)
plot(dend, main = "Hierarchical Clustering Dendrogram")

