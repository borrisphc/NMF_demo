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

# 先區分出哪些是熟客, 跟不常買的
order_times_tbl <- join_tbl %>% select(order_id, user_id) %>% distinct() %>% group_by(user_id) %>% summarise( order_times = n() )
n_product_bought_tbl <- join_tbl %>% select(order_id, user_id) %>% group_by(user_id) %>% summarise( n_product_bought = n() )

# 從買的量跟買的次數來區分出客人的種類
customer_summary_tbl <- inner_join( order_times_tbl, n_product_bought_tbl, by = "user_id" )
rm(order_times_tbl, n_product_bought_tbl)

# 利用數字上的經驗或是業務上的專業知識區分
# plot_d <- 
#   full_join(
#   tibble(n_product_bought = c(1:(customer_summary_tbl$n_product_bought %>% max)) %>% as.character()  ),
#   customer_summary_tbl$n_product_bought %>% table %>% as_tibble() %>% rename("n_p"="."), 
#   by = c("n_product_bought"= "n_p")
# ) %>% replace(., is.na(.), 0) %>% mutate(n_product_bought = as.integer(n_product_bought))


# 各自依照4分位數分類, 總共可分為16(4*4)組

ggplot(customer_summary_tbl, aes(x=n_product_bought)) + 
  geom_histogram(color="black", fill="white") + 
  geom_vline(aes(xintercept = quantile(n_product_bought, probs = 0.75) ),
             color="blue", linetype="dashed", size=1 ) +
  geom_vline(aes(xintercept = quantile(n_product_bought, probs = 0.25) ),
             color="blue", linetype="dashed", size=1 ) +
  geom_vline(aes(xintercept = quantile(n_product_bought, probs = 0.50) ),
             color="blue", linetype="dashed", size=1 ) 

ggplot(customer_summary_tbl, aes(x=order_times)) + 
  geom_histogram(color="black", fill="white") + 
  geom_vline(aes(xintercept = quantile(order_times, probs = 0.75) ),
             color="blue", linetype="dashed", size=1 ) +
  geom_vline(aes(xintercept = quantile(order_times, probs = 0.25) ),
             color="blue", linetype="dashed", size=1 ) +
  geom_vline(aes(xintercept = quantile(order_times, probs = 0.50) ),
             color="blue", linetype="dashed", size=1 ) 

q75 <-  quantile(customer_summary_tbl$order_times, probs = 0.75)
q50 <-  quantile(customer_summary_tbl$order_times, probs = 0.50)
q25 <-  quantile(customer_summary_tbl$order_times, probs = 0.25)

type_get <- function(x, q_75, q_50, q_25){
  if(x >= q_75 ){ 4 } else 
    if( x>= q50 ){ 3 } else
      if( x>= q25 ){ 2 } else 
      {1} 
}

# 整合分組的結果, 新增col 基本的16組 customer_type

customer_summary_tbl <- 
  customer_summary_tbl %>% 
  mutate( order_type =
            sapply(customer_summary_tbl$order_times, type_get,
                   q_75 = quantile(customer_summary_tbl$order_times, probs = 0.75),
                   q_50 = quantile(customer_summary_tbl$order_times, probs = 0.50),
                   q_25 = quantile(customer_summary_tbl$order_times, probs = 0.25)
            ) ) %>% 
  mutate( product_bought_type =
            sapply(customer_summary_tbl$n_product_bought, type_get,
                   q_75 = quantile(customer_summary_tbl$n_product_bought, probs = 0.75),
                   q_50 = quantile(customer_summary_tbl$n_product_bought, probs = 0.50),
                   q_25 = quantile(customer_summary_tbl$n_product_bought, probs = 0.25)
            ) ) %>% 
  mutate( customer_type = paste(order_type,product_bought_type, sep = '-'  ))


customer_summary_tbl$customer_type %>% table

#######################################
#######################################
# 提取特定的customer type, 做後續的分析
#######################################
#######################################

# 前面是消費次數, 後面是消費數量
ana_customer_type <- c("2-3")


#######################################

specify_cust <-  customer_summary_tbl %>% filter(customer_type %in% ana_customer_type) %>% .$user_id



products <- products %>% 
  inner_join(aisles) %>% inner_join(departments)


specify_join_tbl <- 
  left_join(
    join_tbl %>% filter(user_id %in% specify_cust),
    products %>% select(product_id, department_id, aisle_id) %>% as_tibble(),
    by = c("product_id" = "product_id")
  )

# 選擇用什麼東西作為基準看k-means適合分幾群 department_id, aisle_id, product_id
user_ids <- unique( specify_join_tbl$user_id ) 
cols <-   unique( specify_join_tbl$department_id ) %>% sort
product_dis_tbl  <- tibble(user_id = user_ids)

for( ii in 1:length(cols) ){
  product_dis_tbl <- full_join(product_dis_tbl,
                               specify_join_tbl %>% filter(department_id == ii ) %>% group_by(user_id) %>% summarise(n = n()),
                               by = c('user_id'="user_id") ) %>%
    replace(., is.na(.), 0)
}

colnames(product_dis_tbl)[-1] <- cols
# https://www.cnblogs.com/nxld/p/6376496.html
library("factoextra")
product_dis_tbl[,-1]
# 用k means 看4-4當中的消費者適合分作幾群, 在下去看每一群的department分佈
get_best_k <- fviz_nbclust( product_dis_tbl[,-1] %>% scale, kmeans, method = "silhouette",  k.max = 8 )
# best_k <- which.max(get_best_k$data$y)
# best_k <- 6
# best_k <- 2
best_k 

km_res <- kmeans(product_dis_tbl[,-1], best_k, nstart =1)


fviz_cluster(km_res, data = product_dis_tbl[,-1],
             ellipse.type = "convex",
             palette = "jco",
             ggtheme = theme_minimal())


product_dis_tbl <- product_dis_tbl %>% mutate(km_cluster = km_res$cluster)

km_summary <- km_res$centers %>% t() %>% as_tibble() %>% mutate(departments = departments$department)
km_summary %>% as.data.frame()


# 常客(4-4)裡面就有一群是酒鬼

plot_data <- melt(km_summary, id="departments")
colnames(plot_data) <- c("departments", "cluster", "sales")
ggplot(data=plot_data, aes(x=departments, y=sales, fill=cluster)) +
   geom_bar(position="dodge", stat="identity")+
   theme(axis.text.x = element_text(angle = 75, hjust = 1))    

km_res$cluster %>% table
#######
#######
# 選擇哪一個 cluster !!!!! ### 跑後續的分析
#######
#######

#############################################################



k_cluster <- c(1)



#############################################################

cluster_customer <- product_dis_tbl %>% filter( km_cluster %in% k_cluster ) %>% .$user_id

#############################################################



library(arules)

transactions_tbl <-
  specify_join_tbl %>%
  filter( user_id  %in% cluster_customer) %>%
  select(order_id, aisle_id)  %>% # left_join( ., aisles) %>%
  group_by(order_id) %>%
  summarise( products = paste( aisle_id, collapse = ", ") )

data <- paste(transactions_tbl$products, collapse ="\n" )
write(data, file = "transaction")
tr <- read.transactions("transaction", format = "basket", sep=",")


summary(tr)
tr
# inspect(tr)

# sort(itemFrequency(tr), decreasing = T)
rules <- apriori(tr,
                 parameter = list(minlen = 2, supp = 0.03, conf = 0.65) )
# rules <- apriori(tr)
rules_res <- inspect(rules)
aisle_trans <- function(x){
  x %>% gsub("\\{","",.) %>%
    gsub("\\}","",.) %>%
    strsplit(.,split = ",") %>%
    lapply( ., function(x){aisles$aisle[x %>% as.numeric()] %>%
        paste(.,collapse = ", ")} ) %>%
    unlist
}
LHS <- aisle_trans(rules_res$lhs)
RHS <- aisle_trans(rules_res$rhs)
rules_res$lhs <- LHS
rules_res$rhs <- RHS
rules_res

# 3-3 , k = 1 or 2, 沒有越南
# 2-3, k = 2, 有越南
# 1-3, k = 1, 有越南
# write.csv( rules_res, "44_k2_rules.csv")



RMF_long_tbl <- specify_join_tbl %>% 
  filter( user_id  %in% cluster_customer) %>% 
  # filter( department_id %in%  c(18) ) %>% 
  select(user_id, product_id, aisle_id) 

# gg_group <- c(16,83)
# used_user <- 
#   specify_join_tbl %>% 
#   filter( user_id  %in% cluster_customer) %>% 
#   # filter( department_id %in%  c(18) ) %>% 
#   select(user_id, aisle_id) %>% 
#   filter(aisle_id %in% gg_group ) %>% 
#   distinct() %>% group_by(user_id) %>% summarise(n = n()) %>% filter( n ==length(gg_group) ) %>% .$user_id
# 
# RMF_long_tbl <- RMF_long_tbl %>% filter(user_id %in% c(used_user))
# specify_join_tbl %>% 
#   filter( user_id  %in% cluster_customer) %>% select(department_id) %>% table

# 選擇用什麼東西作為基準看k-means適合分幾群 department_id, aisle_id, product_id
user_ids <- unique( RMF_long_tbl$user_id ) 
cols <-   unique( RMF_long_tbl$product_id ) %>% sort

RMF_tbl  <- matrix(0, ncol = length(cols), nrow = length(user_ids))
length(cols)
length(user_ids)
for( ii in 1:length(cols) ){
  n_summary <- RMF_long_tbl %>% filter(product_id == cols[ii] ) %>% group_by(user_id) %>% summarise(n = n()) %>% as.matrix
  RMF_tbl[match( n_summary[,1], user_ids),ii] <- n_summary[,2]
  # print(ii)
}

colnames(RMF_tbl) <- cols





library(NMF)
# RMF_tbl %>% scale
res <- nmf(RMF_tbl, 3,"lee") 



p_matrix <- coef(res) # H  movie feature matrix
dim(p_matrix) #  r x p (r = 4 p = 4
user_matrix <- basis(res) #  W  user feature matrix matrix
dim(user_matrix) # n x r (n= 5  r = 4)
rownames(user_matrix) <- user_ids

top10 <- c(24852, 13176, 21137, 21903, 47209, 47766, 47626, 16797, 26209, 27845)
ans_sum <- c()
recom_hit <- c()
freq_hit <- c()
freq_his <- c()
freq_only <- c()
only_freq_length  <- c() 

recom_num <- 10
for ( xxx in 1:length(user_ids) ){
  
  ans_order_id  <- orders %>% filter(eval_set == "train" & user_id == user_ids[xxx]) %>% .$order_id
  if( length(ans_order_id) == 0 ){
    recom_hit[xxx] <- 0
    ans_sum[xxx] <- 0
    freq_hit[xxx] <- 0
    freq_his[xxx] <- 0
    freq_only[xxx] <- 0
  } else {
    anser <- ordert %>% filter( order_id %in% ans_order_id ) %>% .$product_id
    # dist_res <- apply(p_matrix , 2, function(x){ dist(rbind(user_matrix[rownames(user_matrix) == user_ids[xxx],],
    #                                                         x) ) } )
    # recom <- dist_res %>% sort(.,decreasing = T) %>% names %>% .[1:recom_num] %>% as.numeric()
    # 
    max_feature <- which.max(user_matrix[rownames(user_matrix) == user_ids[xxx],])
    recom <- p_matrix[max_feature,] %>% sort(.,decreasing = T) %>% as.data.frame() %>%  rownames() %>% .[1:recom_num] %>% as.numeric()

    freq_buy <- RMF_long_tbl %>% filter(user_id == user_ids[xxx]) %>% group_by(product_id) %>% summarise(n = n()) %>% arrange(desc(n)) %>% .$product_id
    
    if ( length(freq_buy)>recom_num ){freq_buy <- freq_buy[1:recom_num]}
    # recom <- intersect(recom, freq_buy)
    # recom <- c(recom,freq_buy[ !freq_buy %in% recom ])
    only_freq <- freq_buy
    recom <- recom[! recom %in% freq_buy]# 只有在買的商品數量低於 recom_num時有用
    recom <- c( freq_buy, recom)
    freq_buy <- c(freq_buy,  RMF_long_tbl %>% filter( !product_id  %in% freq_buy) %>% group_by(product_id) %>% summarise(n = n()) %>% arrange(desc(n)) %>% .$product_id)
    
    if ( length(freq_buy)>recom_num ){ freq_buy <- freq_buy[1:recom_num] }
    if ( length(recom)>recom_num ){ recom <- recom[1:recom_num] }
    if ( length(freq_top10)>recom_num ){ freq_top10 <- freq_top10[1:recom_num] }
    recom_hit[xxx] <- length(recom[recom %in% anser] )
    freq_hit[xxx] <- length(freq_buy[freq_buy %in% anser])
    freq_his[xxx] <- length(top10[top10 %in% anser])
    freq_only[xxx] <- length(only_freq[only_freq %in% anser])
    ans_sum[xxx] <- length(anser)
    only_freq_length[xxx] <- length(only_freq)
  }
  #print(xxx)
}

sum(ans_sum) 
sum(recom_hit) 
sum(freq_hit)
sum(freq_his)
na.exclude(recom_hit/ans_sum) %>% mean # 一張訂單裡面的商品有多少是被我推薦的
na.exclude(freq_hit/ans_sum) %>% mean # 一張訂單裡面的商品有多少是被我推薦的
na.exclude(freq_only/ans_sum) %>% mean 
na.exclude(freq_his/ans_sum) %>% mean 
# recom_hit/ans_sum



only_freq_length %>% na.exclude()%>% mean()




# 1-1, k = 5, same
# 1-1, k = 4, not good 
# 1-1, k = 3, good 
# 1-1, k = 2, good 
# 1-1, k = 1, not good 


# 2-3 促銷組合, 12,可以
# 2-3 促銷組合 5 群裡面的4可以更精確地定