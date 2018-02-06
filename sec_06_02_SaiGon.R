

#=============================================
#   Missing Data and  Imputation Method 
#============================================

#------------------
#     Method 1
#------------------

# Import pima_na.csv: 
rm(list = ls())
path <- dir("F:/usth/data", full.names = TRUE)
path
pima <- read.csv(path[4])

# Import some packages: 
library(tidyverse)
library(magrittr)

pima %>% head()

# Convert zero to NA and preprocessing data: 
convert_NA <- function(x) {case_when(x == 0 ~ NA_integer_, 
                                     x != 0 ~ x)}

# Use our function: 
pima_na <- pima %>% 
  mutate_at(.vars = c("glucose", "diastolic", "triceps", "insulin"), 
            .funs = convert_NA)

pima_na %>% head()

# A function for calculating NA rate:  
na_rate <- function(x) {
  return(100*sum(is.na(x)) / length(x))
}


# Use our function: 
pima_na %>% 
  summarise_all(na_rate)

# A function for imputing NA by mean: 

imputing_by_mean <- function(x) {
  tb <- mean(x, na.rm = TRUE)
  x[is.na(x)] <- tb
  return(x)
}

pima_imp <- pima_na %>% mutate_all(imputing_by_mean)
pima_imp %>% head()

pima_imp %>% summarise_all(na_rate)

# Compare: 

pima_na %>% 
  lm(glucose ~ bmi, data = .) %>% 
  summary()

pima_imp %>% 
  lm(glucose ~ bmi, data = .) %>% 
  summary()

#-----------------
#   Method 2
#-----------------


library(VIM)

plot_na <- aggr(pima_na, 
                col = c("navyblue", "yellow"),
                numbers = TRUE, 
                sortVars = TRUE, 
                labels = names(pima_na), 
                cex.axis = .7, gap = 3, 
                ylab = c("Missing Data Rate", ""))


library(mice)
df_impu <- mice(pima_na,
                m = 2, 
                method = "pmm", 
                seed = 100)

data1 <- complete(df_impu, 1) 
data2 <- complete(df_impu, 2) 
summary(data1)

ols1 <- lm(glucose ~ bmi, data = data1)
ols2 <- lm(glucose ~ bmi, data = data2)

library(stargazer)
stargazer(ols1, ols2, title = "So sánh OLS1 và OLS2", type = "text")


fit <- with(data = df_impu, exp = lm(glucose ~ bmi))
summary(fit)

tonghop <- pool(fit)
tonghop

(ols1$coefficients[2] + ols2$coefficients[2]) / 2

# Write a function for visualizing missing data: 

missing_vis <- function(x){
  x %>% 
    is.na() %>% 
    melt() %>% 
    ggplot(aes(x = Var2, fill = value)) +
    geom_bar(aes(y = ..count..), alpha = 0.5) + 
    coord_flip() + theme_bw() + 
    scale_fill_manual(values = c("blue", "red"), 
                      name = NULL, 
                      labels = c("Available", "Missing")) + 
    labs(x = NULL, y = NULL, title = "Missing Data Rate in our Data Set")
}

# Use our function: 
missing_vis(pima_na)


#===============================
#    A practical project
#===============================

rm(list = ls())
path <- dir("F:/R_project/VHLSS2016/data", full.names = TRUE)

library(readstata13)
ho3 <- read.dta13("F:/R_project/VHLSS2016/data/Ho3.dta", encoding = "UTF-8")

ho3 %<>% mutate_if(is.factor, as.character)

muc2ab <- read.dta13("F:/R_project/VHLSS2016/data/Muc2AB.dta", encoding = "UTF-8")
muc2ab %<>% mutate_if(is.factor, as.character)

muc2ab$tinh %>% head()

library(raster)
# Lấy dữ liệu địa lí cho VN ở cấp tỉnh: 
vietnam <- getData("GADM", country = "Vietnam", level = 1)

detach(package:raster)

# Chuyển  hóa vietnam về dạng data frame quen thuộc: 
vietnam_df_province <- vietnam %>% fortify(region = "NAME_1")
head(vietnam_df_province)

# 63 tỉnh thành: 
vietnam_df_province$id %>% unique()


vietnam_df_province$id %>% unique() -> vv
muc2ab$tinh %>% unique() -> uu
uu <- uu[order(uu)]

# Viết hàm thay tên: 
rename_province <- function(x) {
  case_when(x == uu[1] ~ vv[15], 
            x == uu[2] ~ vv[19], 
            x == uu[3] ~ vv[16], 
            x == uu[4] ~ vv[17],  
            x == uu[5] ~ vv[20], 
            x == uu[6] ~ vv[1], 
            x == uu[7] ~ vv[3], 
            x == uu[8] ~ vv[4], 
            x == uu[9] ~ vv[2], 
            x == uu[10] ~ vv[5], 
            x == uu[11] ~ vv[6], 
            x == uu[12] ~ vv[7], 
            x == uu[13] ~ vv[8], 
            x == uu[14] ~ vv[9],
            x == uu[15] ~ vv[10], 
            x == uu[16] ~ vv[11], 
            x == uu[17] ~ vv[13], 
            x == uu[18] ~ vv[14], 
            x == uu[19] ~ vv[21],
            x == uu[20] ~ vv[22], 
            x == uu[21] ~ vv[31], 
            x == uu[22] ~ vv[24], 
            x == uu[23] ~ vv[26], 
            x == uu[24] ~ vv[28],  
            x == uu[25] ~ vv[29], 
            x == uu[26] ~ vv[30], 
            x == uu[27] ~ vv[32], 
            x == uu[28] ~ vv[33],  
            x == uu[29] ~ vv[34],  
            x == uu[30] ~ vv[35], 
            x == uu[31] ~ vv[36], 
            x == uu[32] ~ vv[37], 
            x == uu[33] ~ vv[38], 
            x == uu[34] ~ vv[39], 
            x == uu[35] ~ vv[40], 
            x == uu[36] ~ vv[41], 
            x == uu[37] ~ vv[42], 
            x == uu[38] ~ vv[43], 
            x == uu[39] ~ vv[44], 
            x == uu[40] ~ vv[45], 
            x == uu[41] ~ vv[46], 
            x == uu[42] ~ vv[47], 
            x == uu[43] ~ vv[48], 
            x == uu[44] ~ vv[49], 
            x == uu[45] ~ vv[50], 
            x == uu[46] ~ vv[52], 
            x == uu[47] ~ vv[51], 
            x == uu[48] ~ vv[53], 
            x == uu[49] ~ vv[54], 
            x == uu[50] ~ vv[55], 
            x == uu[51] ~ vv[56], 
            x == uu[52] ~ vv[57], 
            x == uu[53] ~ vv[58], 
            x == uu[54] ~ vv[59], 
            x == uu[55] ~ vv[60], 
            x == uu[56] ~ vv[61], 
            x == uu[57] ~ vv[62],  
            x == uu[58] ~ vv[63], 
            x == uu[59] ~ vv[18], 
            x == uu[60] ~ vv[12], 
            x == uu[61] ~ vv[25], 
            x == uu[62] ~ vv[23], 
            x == uu[63] ~ vv[27])
} 

#  Áp dụng hàm: 
muc2ab %<>% mutate(tinh = rename_province(tinh)) 
ho3 %<>% mutate(tinh = rename_province(tinh))
# Xem qua: 
muc2ab$tinh %>% unique()

# Tên chung: 
dplyr::intersect(muc2ab %>% names(), ho3 %>% names())


# Chú ý sự chênh lệch về số lượng: 
ho3 %>% nrow()
muc2ab %>% nrow()

# Viết hàm tạo mã ID: 
library(stringr)
ID_household <- function(tinh, huyen, xa, diaban, hoso) {
  u <- paste(tinh, paste(paste(xa, paste(diaban, hoso, sep = "_"), sep = "_"), huyen, sep = "_"), sep = "_")
  u %>% str_replace_all(" ", "")
}

# Sử dụng hàm: 
muc2ab %<>% mutate(ID = ID_household(tinh, huyen, xa, diaban, hoso))
ho3 %<>% mutate(ID = ID_household(tinh, huyen, xa, diaban, hoso))

# Kiểm tra sự trùng nhau của case: 
sum(duplicated(ho3$ID))

# Nhưng thằng này thì trùng nhiều:  
sum(duplicated(muc2ab$ID))

# Lọc ra case không trùng hợp: 
khong_trung <- muc2ab %>% dplyr::filter(!duplicated(ID))

khong_trung %>% nrow()

ho3 %>% nrow()

total_df <- inner_join(khong_trung, ho3, by = "ID")
total_df %<>% dplyr::select(-ends_with("y"))

# Hàm kiểm tra dữ liệu thiếu: 
ti_le_na <- function(x) {
  100*sum(is.na(x)) / length(x)
}

total_df %>% summarise_all(ti_le_na)

#  Hàm kiểm tra dữ liệu âm: 

kiem_tra_am <- function(x) {
  sum(x < 0,  na.rm = TRUE)
}


total_df %>% summarise_all(kiem_tra_am)


#-------------------------------------
#     Bằng cấp - thu nhập
#-------------------------------------

total_df$m2ac2a %>% unique() -> k
k

# Viết hàm dán  lại nhãn cho bằng cấp: 

rename_educ_level <- function(x) {
  case_when(x %in% c(k[1], k[7]) ~ "College & University", 
            x == k[6] ~ "None", 
            x %in% c(k[c(5, 8)]) ~ "Master & PhD", 
            x %in% c(k[c(1, 2)]) ~ "Elementary & Higher", 
            x == k[4] ~ "Primary", 
            x == k[10] ~ "Other")
}

df_educ_level <- total_df %>% 
  dplyr::filter(!is.na(m2ac2a)) %>% 
  mutate(Educ_level = rename_educ_level(m2ac2a))


# Phân bố trình độ học vấn theo tỉnh thành: 

df_educ_level %>% 
  dplyr::filter(Educ_level != "Other") %>% 
  group_by(tinh.x, Educ_level) %>% 
  count() %>% 
  ggplot(aes(tinh.x, n, fill = Educ_level)) + 
  geom_col(position = "fill") + 
  coord_flip() + 
  theme_minimal() + 
  labs(x = "", y = "") + 
  scale_fill_manual(values = c('#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00'), 
                    name = "Education Level")

nghe <- df_educ_level$m2ac2b %>% unique()

# Dán lại nhãn cho nghề. Chú ý mức độ mất cân bằng của m2ac2b: 
recode_nghe <- function(x) {
  case_when(x %in% nghe[c(2:5)] ~ "Vocational", 
            x == nghe[1] ~ "None")
}

# Sử dụng hàm: 
df_educ_level %<>% mutate(Vocat_level = recode_nghe(m2ac2b))

df_educ_level %>% 
  dplyr::filter(!is.na(Vocat_level)) %>% 
  group_by(Vocat_level, tinh.x) %>% 
  count() %>% 
  ggplot(aes(tinh.x, n, fill = Vocat_level)) + 
  geom_col(position = "fill") + 
  coord_flip() + 
  theme_minimal() + 
  labs(x = "", y = "") + 
  scale_fill_manual(values = c('#4daf4a','#984ea3','#ff7f00'), 
                    name = "Vocational Education")

# Tổng thu nhập của chủ hộ theo học vấn: 

thu_nhap <- df_educ_level %>% 
  dplyr::select(tinh.x, thunhap, Educ_level)

# Có thể thấy mức học vấn càng cào thì thu nhập càng cao: 

thu_nhap %>% 
  dplyr::filter(thunhap > 0, Educ_level != "Other") %>% 
  group_by(Educ_level) %>% 
  summarise_each(funs(min, max, median, mean, n()), thunhap) %>% 
  arrange(-thunhap_mean) %>% 
  dplyr::select(Educ_level, thunhap_mean, everything()) %>% 
  knitr::kable(col.names = c("Education Level", "Mean", "Min", "Max", "Median", "N"))

# Cẩn thận: có 5 case mà thu nhập bình quân thì cao hơn con  số tổng: 
sum(df_educ_level$thunhap < df_educ_level$thubq)


# Tạo cột biến tổng thu và tổng chi cho SXKD: 
df_educ_level %<>% mutate(tong_thu = 
                            tongthu_01 + 
                            tongthu_02 + 
                            tongthu_03 + 
                            tongthu_04 + 
                            tongthu_05 + 
                            tongthu_06 + 
                            tongthu_07 + 
                            tongthu_08 + 
                            tongthu_09 + 
                            tongthu_10 + 
                            tongthu_11 + 
                            tongthu_12 + 
                            tongthu_13 + 
                            tongthu_14, 
                          tong_chi_sx = 
                            chisxkd_1 + 
                            chisxkd_2 + 
                            chisxkd_3 + 
                            chisxkd_4 + 
                            chisxkd_5 + 
                            chisxkd_6 + 
                            chisxkd_7 + 
                            chisxkd_8, 
                          tong_chi_khac = 
                            chikhac_1 + 
                            chikhac_2 + 
                            chikhac_3 + 
                            chikhac_4 + 
                            chikhac_5 + 
                            chikhac_6 + 
                            chikhac_7 + 
                            chisxkd_8 + 
                            chikhac_9)

# Tổng thu nhập của hộ gia đình theo học vấn:  
df_educ_level %>% 
  dplyr::filter(Educ_level != "Other", tong_thu > 0) %>% 
  group_by(Educ_level) %>% 
  summarise_each(funs(min, max, median, mean, n()), tong_thu) %>% 
  arrange(-tong_thu_mean) %>% 
  dplyr::select(Educ_level, tong_thu_mean, everything()) %>% 
  knitr::kable(col.names = c("Education Level", "Mean", "Min", "Max", "Median", "N"))


# Tổng chi cho SXKD có thể thấy bức tranh đa dạng: nhóm từ bậc đại học đến cấp 
# hai là nhóm ham hố kinh doanh nhất. Các bác học cao từ thạc sĩ trở lên 
# không ham hố gì  kinh doanh mà có lẽ  chuyên sâu học: 
df_educ_level %>% 
  dplyr::filter(Educ_level != "Other") %>% 
  group_by(Educ_level) %>% 
  summarise_each(funs(min, max, median, mean, n()), tong_chi_sx) %>% 
  arrange(-tong_chi_sx_mean) %>% 
  dplyr::select(Educ_level, tong_chi_sx_mean, everything()) %>% 
  knitr::kable(col.names = c("Education Level", "Mean", "Min", "Max", "Median", "N"))


# Chúng ta quan tâm đến chi cho SXKD trung bình theo tỉnh để coi
# tỉnh nào có tinh thần khởi nghiệp nhất: 

df_educ_level %>% 
  group_by(tinh.x) %>% 
  summarise_each(funs(mean, median, max, min, n()), tong_chi_sx) %>% 
  ungroup() %>% 
  arrange(-tong_chi_sx_mean) -> u

# Đây là 10 tình chi nhiều nhất và ít  cho đầu tư kinh doanh: 

u %>% slice(c(1:10, 54:63))


# Chúng ta thử nghiên cứu mối liên hệ giữa tổng thu nhập của chủ hộ
# và chi cho sản xuất kinh doanh ra sao: 


thu_chi_sx <- df_educ_level %>% 
  dplyr::select(tinh.x, tong_thu, tong_chi_sx)

thu_chi_sx %>% summarise_all(ti_le_na)


theme_set(theme_minimal())
thu_chi_sx %>% 
  ggplot(aes(tong_chi_sx / 1000, tong_thu / 1000)) + 
  geom_point(alpha = 0.2) + 
  geom_smooth(method = "lm") + 
  labs(x = NULL,  y = NULL, 
       title = "Relationship between total Income and Expenditure for Business", 
       caption = "Data Source: VHLSS 2016 provided by GSO")

# Hệ số hồi quy của mô hình OLS này chỉ ra rằng 
# cứ đầu tư thêm 100 đồng thì tổng thu nhập hộ sẽ tăng chừng 9.5%: 
thu_chi_sx %>% 
  lm(tong_thu ~ tong_chi_sx, data = .) %>% 
  summary()


# Chúng ta trở lại danh sách các tỉnh chi cho đầu  tư SXKD cao nhất ở 
# quy mô gia đình: 

df_educ_level %>% 
  group_by(tinh.x) %>% 
  summarise_each(funs(mean, median, max, min, n()), tong_chi_sx) %>% 
  ungroup() %>% 
  arrange(-tong_chi_sx_mean) -> u

# Lọc ra dữ liệu của  12  chi  nhiều nhất cho đầu tư : 

df_educ_level %>% 
  dplyr::filter(tinh.x %in% u$tinh.x[1:12]) -> top12_for_business

# Có vẻ như cơ hội sinh lãi từ đầu tư đa phần tương  tự nhau ngoại  
# nếu căn cứ độ dốc đường hồi quy: 
top12_for_business %>% 
  ggplot(aes(tong_chi_sx / 1000, tong_thu / 1000)) + 
  geom_point(alpha = 0.2) + 
  geom_smooth(method = "lm") + 
  facet_wrap(~ tinh.x, scales = "free") + 
  labs(x = NULL,  y = NULL, 
       title = "Relationship between total Income and Expenditure for Business by top 12", 
       caption = "Data Source: VHLSS 2016 provided by GSO")


# Ta có thể có mô hình hồi quy cho 12 tình thành này như sau: 
library(purrr)

top12_for_business %>% 
  split(.$tinh.x) %>% 
  map(function(df) lm(tong_thu ~ tong_chi_sx, data = df)) %>% 
  map(summary)


# Hoặc hệ số góc của đường OLS: 
top12_for_business %>% 
  split(.$tinh.x) %>% 
  map(function(df) lm(tong_thu ~ tong_chi_sx, data = df)) %>% 
  map("coefficients")


# Nên làm đẹp hơn: 
top12_for_business %>% 
  split(.$tinh.x) %>% 
  map(function(df) lm(tong_thu ~ tong_chi_sx, data = df)) %>% 
  map("coefficients") -> he_so

he_so <- do.call("rbind", he_so) %>% 
  as.data.frame() %>% 
  mutate(Province = row.names(.))

# Sắp xếp theo hệ số chặn giảm dần và đổi tên: 
names(he_so) <- c("Intercept", "Coefficient", "Province")

he_so %>% 
  arrange(-Coefficient) %>% 
  knitr::kable()

#--------------------------------------------
# Minh họa tinh thần  khởi nghiệp ở các tỉnh
#--------------------------------------------

u %<>% rename(id = tinh.x) # đổi lại cái tên tỉnh trước khi merge. 
vietnam_df_province %>% head()

start_up <- inner_join(u, vietnam_df_province, by = "id")

# Vẽ chơi: 
m2 <- start_up %>% 
  ggplot(aes(x = long, y = lat, group = group))+
  geom_polygon(aes(fill = tong_chi_sx_mean), color = "grey30") +  
  labs(x = NULL, y = NULL)

m2


library(viridis)
m2 + scale_fill_viridis(direction = -1, 
                        option = "B", 
                        "Expenditure for\nBusiness") -> m3

m3


# Điều chỉnh vị trí của legend: 
m3 + 
  theme(legend.position = c(0.22, 0.40), 
        # Bỏ các  chỉ dẫn trên hai trục: 
        axis.text = element_blank(),
        # Bỏ lưới: 
        panel.grid = element_blank()) + 
  labs(title = "Mean Expenditure for Business by Household", 
       caption = "Data Sourse: VHLSS 2016, GSO")
