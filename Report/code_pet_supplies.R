install.packages("dplyr")
library(dplyr)
pet_supplies <- read.csv('pet_supplies_2212.csv')
#Chuyển đổi kiểu dữ liệu sang từ chr sang fct để phân loại
pet_supplies$category <-as.factor(pet_supplies$category)
pet_supplies$size <-as.factor(pet_supplies$size)
pet_supplies$animal <-as.factor(pet_supplies$animal)
pet_supplies$price <-as.numeric(pet_supplies$price)

#Xử lý dữ liệu theo yêu cầu của bảng
pet_supplies_fix <- pet_supplies %>% mutate(
  category=recode(category,"-"="Unknow"), size=recode(size,"small"="Small","SMALL"="Small","medium"="Medium","MEDIUM"="Medium","large"="Large", "LARGE"="Large"),
  rating = ifelse(is.na(rating),0,rating),
  price = ifelse(is.na(price),mean(pet_supplies$price,na.rm=TRUE), price)) 

#Lấy 2 số thập phân của price
pet_supplies_fix$price <- round(pet_supplies_fix$price,2)

#Tạo table đếm repeat_purchase
pet_supplies_count <- pet_supplies_fix %>% group_by(category) %>% summarize(total_repeat=sum(repeat_purchase)) %>% arrange(desc(total_repeat))

#Tạo table giá trung bình của category
pet_supplies_avg <- pet_supplies_fix %>% group_by(category) %>% summarize(avg_price=mean(price))

install.packages("ggplot2")
library(ggplot2)
ggplot(pet_supplies_count,aes(x = reorder(category, total_repeat), y = total_repeat)) +
  geom_bar(stat = "identity") +
  labs(title="Count of Category in the market - Equipment are the most common category",x = "Category", y = "Count")