###########################################################################################################
#
# Kaggle Instacart competition
# Fabien Vavrand, June 2017
# Simple xgboost starter, score 0.3791 on LB
# Products selection is based on product by product binary classification, with a global threshold (0.21)
#
###########################################################################################################

library(data.table)
library(dplyr)
library(tidyr)


# Load Data 
path <- "/home/rstudio/AWS"

aisles <- fread(file.path(path, "aisles.csv"))
departments <- fread(file.path(path, "departments.csv"))
orderp <- fread(file.path(path, "order_products__prior.csv"))
ordert <- fread(file.path(path, "order_products__train.csv"))
orders <- fread(file.path(path, "orders.csv"))
products <- fread(file.path(path, "products.csv"))

join_tbl <- 
  inner_join( orderp %>% select( order_id, product_id) ,
              orders %>% select( order_id, user_id) , 
              by = c('order_id'="order_id") ) %>% as_tibble()


# 有多少個客人
join_tbl$user_id %>% unique() %>% length
join_tbl$order_id %>% unique() %>% length
# 有多少個商品
join_tbl$product_id %>% unique() %>% length


products <- products %>% 
  inner_join(aisles) %>% inner_join(departments)

tmp <- products %>% group_by(department) %>% summarise(n = n()) %>% arrange(desc(n))
ggplot(data=tmp, aes(x=reorder(department,n), y=n)) +
  labs(x = "", y = "") +
  coord_flip() +
  geom_bar(position="dodge", stat="identity")+
  theme_bw() 
  
tmp <- products %>% select( aisle ,department) %>% distinct() %>% group_by(department) %>% summarise(n = n()) %>% arrange(desc(n))
ggplot(data=tmp, aes(x=reorder(department,n), y=n)) +
  labs(x = "", y = "") +
  coord_flip() +
  geom_bar(position="dodge", stat="identity")+
  theme_bw() 

 

# 為何要分群？
# 提出客人數量與商品總類多, 要整個下去看嗎？
# top 10 放進去
# 每個人的習慣(不喝酒的人就是不會喝, 有些人喜歡一次買很多, 有些人是常常買每次買不多)或是
# 商品相似度本身就差異很大(酒和不是酒本來就有差, 生鮮和非生鮮)？
# 因此希望先進一步細分出不同的客群與消費習慣, 再下去針對不同族群的人提出不同的促銷或推薦

# 第一步分群
# 有些客人的貢獻度(會員等級)比較大, 以結果論(消費金額(數量)跟次數)先區分出客人的重要性與目標

# 二次分群
# 有相同重要性的客人他們購買的東西分布可能不一樣, 因此在依照購買的東西種類區分出他們的購物習慣

# 為何不先分購物習慣在分重要性？
# 最主要的原因是計算上的負擔, 或許可以, 甚至是以商品種類為出發點下去分析, 不過目前的目標是以客人為單位做推薦商品和促銷組合所以就先用客人為單位下去跑

# 針對分群結果
# 比較兩個群體購物習慣分佈 or 跟top10 直接比, 顯示分群是有效的
# 圓餅圖

# 促銷組合 (客戶管理)
# 針對購物習慣不同的族群使用購物籃分析找出專屬的促銷組合
# 提出要用A/B test檢驗

# 推薦系統
# 先用新客人做測試, 為何要用新客人？
# 相較於老客人, 新客人多半對系統不熟悉, 無法快速找到自己想要的東西, 或是沒有耐心, 一旦覺得不好用很容易流失,
# 而且對於這些人也沒有過去的購物資料可以推薦或是幫忙了解這個人的習慣, 如果可以快速推薦出他想要的東西, 可以幫助留下這個顧客
# 推薦系統的驗證
# NMF 解說, 並解釋沒有做優化, 推薦商品也是以最簡單的方式選出
# 這樣的結果和 直接推以前買過的比較來有沒有比較好
# 比較的指標是什麼？ 推薦給它10個, 最後這10個佔他這筆訂單中的百分之幾, 舉例出來
 
# 優化方向與附加價值
# 主軸
# 1+1 > 2



# 每個客人會買多少商品的分佈 # 加上商品部門的分佈, 推到每個切點的商品組成都有點細微差異, 所以要在分群

ggplot(customer_summary_tbl, aes(x=n_product_bought)) + 
  geom_histogram(color="black", fill="white") + 
  geom_vline(aes(xintercept = quantile(n_product_bought, probs = 0.75) ),
             color="blue", linetype="dashed", size=1 ) +
  geom_vline(aes(xintercept = quantile(n_product_bought, probs = 0.25) ),
             color="blue", linetype="dashed", size=1 ) +
  geom_vline(aes(xintercept = quantile(n_product_bought, probs = 0.50) ),
             color="blue", linetype="dashed", size=1 ) +
  # labs(x = "購買商品總數", y = "消費者數") +
  theme_bw() 

# # 每個客人會消費次數的分佈
ggplot(customer_summary_tbl, aes(x=order_times)) + 
  geom_histogram(color="black", fill="white") + 
  theme_classic() +
  geom_vline(aes(xintercept = quantile(order_times, probs = 0.75) ),
             color="blue", linetype="dashed", size=1 ) +
  geom_vline(aes(xintercept = quantile(order_times, probs = 0.25) ),
             color="blue", linetype="dashed", size=1 ) +
  geom_vline(aes(xintercept = quantile(order_times, probs = 0.50) ),
             color="blue", linetype="dashed", size=1 ) +
  # labs(x = "消費次數", y = "消費者數") +
  theme_bw() 



# 不分群時客人的分佈
base_summary_tbl <- join_tbl %>% left_join(.,products, by = "product_id") %>% select(-product_name)

# department top 10 
base_summary_tbl %>% group_by(department_id) %>% summarise(n = n()) %>% arrange(desc(n)) %>% left_join(.,departments)
# aisle top 10
base_summary_tbl %>% group_by(aisle_id) %>% summarise(n = n()) %>% arrange(desc(n)) %>% left_join(.,aisles)
# product top 10
base_summary_tbl %>% group_by(product_id) %>% summarise(n = n()) %>% arrange(desc(n))%>% left_join(.,products %>% select(product_id, product_name))


top10_plot <- function(tmp){
  
  add_text <- (tmp$n*100/sum(tmp$n)) %>% .[1:10] %>% round(.,1) %>% paste(.,"%", sep = " ")
  
  plot_data <- tmp[1:10,]
  colnames(plot_data) <- c("product_id", "n", "product_name")
  
  plot_data$product_name <- factor(plot_data$product_name,                                    # Factor levels in decreasing order
                                   levels = plot_data$product_name[order(plot_data$n, decreasing = F)])
  
  ggplot(data=plot_data , aes(x=product_name, y=n, fill = n)) +
    geom_bar(position="dodge", stat="identity")+
    theme(axis.text.x = element_text(angle = 75, hjust = 1))+
    coord_flip() +
    geom_text(aes(label=add_text), position=position_dodge(width=0.9), hjust=1.1, colour = "white",  fontface = "bold") +
    labs(x = "", y = "Sales", fill = "Sales") +
    theme_bw() +
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.text.y = element_text(size=12, face="bold", colour = "black") # bold
    )+
    scale_fill_gradient(low="blue", high="red")
  
}
base_summary_tbl %>% group_by(aisle_id) %>% 
  summarise(n = n()) %>% arrange(desc(n)) %>% 
  left_join(.,aisles) %>% #.[1:10,] %>% 
  left_join(., products %>% select(aisle_id, department_id)) %>% 
  distinct() %>% left_join(., departments ) %>% mutate( asile_d = paste(aisle," (", department, ")", sep = "")) %>% 
  select(aisle_id, n, asile_d ) %>% top10_plot()
# department top 10 
base_summary_tbl %>% group_by(department_id) %>% summarise(n = n()) %>% arrange(desc(n)) %>% left_join(.,departments) %>% top10_plot()
# aisle top 10
base_summary_tbl %>% group_by(aisle_id) %>% summarise(n = n()) %>% arrange(desc(n)) %>% left_join(.,aisles) %>% top10_plot()
# product top 10
base_summary_tbl %>% group_by(product_id) %>% summarise(n = n()) %>% arrange(desc(n))%>% left_join(.,products %>% select(product_id, product_name)) %>% top10_plot()

  


product_dis_tbl %>% 
  select(-user_id) %>%
  melt(., id="km_cluster") %>% 
  group_by(km_cluster, variable) %>% 
  summarise(n = sum(value)) %>% 
  ungroup() %>% 
  group_by(km_cluster) %>%
  mutate( value = round(n*100/sum(n), 2) ) %>% 
  rename(department_id = variable) %>% 
  mutate( department_id = as.numeric(department_id)) %>% 
  left_join(.,departments) %>% 
  ggplot(., aes(x= "", y = value, fill=department )) + 
  geom_col(color="white") +
  facet_wrap(~km_cluster)+
  ggtitle(" ") +
  coord_polar("y", start=0) +
  theme_void()  +
  scale_fill_hue(h = c(0,375),l=50, c = 100) +
  theme(legend.position="top")
  # scale_fill_brewer(palette="Set1")
  
# 色相（hue）飽和度（chroma）亮度（luminosity）來調整顏色


# 關聯性的圖
  
plot_data <- rules_res[,c(1,3,4,5,6)] 
# plot_data <- plot_data  %>% .[1:10,]
ggplot(plot_data, aes(rhs, lhs, colour = confidence, size = support)) +
  geom_point()+
  scale_color_gradient(low="pink", high="red")+ 
  theme_minimal()+
  theme(axis.text.x = element_text(face="bold",
                                   size=15),
        axis.text.y = element_text(face="bold",
                                   size=15),
        text = element_text(face="bold",
                            size = 15 ))
df <- 
tibble(
  Group = rep(LETTERS[1:5],4),
  Method = rep(c("top10","re-order", "cluster model", "NMF model"),5)
) %>% arrange(Group,desc(Method) )

acc_value <- c(
  0.1617516,
  0.4134506,
  0.4246966,
  0.4706311,
  0.041147,
  0.4065898,
  0.4346594,
  0.4236767,
  0.041129,
  0.5635387,
  0.5863329,
  0.5635387,
  0.01564925,
  0.6206251,
  0.6369295,
  0.6496106,
  0.009976135,
  0.5178571,
  0.5178571,
  0.5178571
)

df <- df %>% mutate(acc = acc_value)



ggplot(df, aes(x=Group, y=acc, group=Method)) +
  geom_line(aes(linetype=Method))+
  geom_point()+
  scale_linetype_manual(values=c("twodash", "dotted"))



ggplot(df, aes(x=Group, y=acc, group=Method, color = Method)) +
  geom_line(aes(linetype=Method))+
  geom_point(aes(shape=Method))


ggplot(data=df, aes(x=Group, y=acc, fill=Method)) +
  geom_bar(position="dodge", stat="identity")+ theme_bw()+ theme(legend.position="top")+
  theme(axis.text.x = element_text(face="bold",
                                   size=15),
        axis.text.y = element_text(face="bold",
                                   size=15),
        text = element_text(face="bold",
                            size = 15 ))



