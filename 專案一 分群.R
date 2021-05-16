##資料清洗####
library(dplyr)
library(data.table) #fread dcast 
library(naniar) #chk na
library(ggplot2)
library(ggeasy)#置中

#2016-1-1~2018-9-7，區間穩定
#完全空白的資料已刪除
elec_stock_raw <- fread(file="D:\\elec_stock.csv",header=TRUE,stringsAsFactors = FALSE)
#空值匯入變na，補0
any_na(elec_stock_raw)
elec_stock <- replace(elec_stock_raw,is.na(elec_stock_raw),0)
any_na(elec_stock)
elec_stock <- elec_stock %>% 
  mutate(年=substr(年月日,start=1,stop=4)) %>% 
  subset(年>=2016)

##資料探索####
#檢查數量，共402支股票
stock_num <- elec_stock %>% 
             group_by(名稱) %>% 
             summarise(次數=n())
boxplot(stock_num$次數) #出現次數不相等

stock_num %>% #chk沒有被藍色填滿
  ggplot(aes(x=名稱, y=次數)) + 
  geom_bar(stat = "identity", width=0.01,
           color="blue", fill=rgb(0.1,0.4,0.5,0.7))+
  #coord_flip()+
  ggtitle("數量檢查")+
  ggeasy::easy_center_title()#標題置中
#股票出現次數不同>>時間出現次數不同
#>>unbalanced data

##集群####
#指標選擇: 盡可能讓每間公司起跑點相同&其他角度
#所以抓比值或百分比
#因若抓股本、股票，其實直接分類就好，分群意義變小

#GMM，因為可以看到每群的機率，每支股票取平均，抓大的
#若用k-means之類的方法，看不到權重，群一邊邊=群一中心
elec_stock_numeric <- elec_stock[,4:7]
#單位相同不去單位
library(ClusterR)
opt_gmm = Optimal_Clusters_GMM(elec_stock_numeric, max_clusters = 10, criterion = "BIC", 
                               dist_mode = "eucl_dist", seed_mode = "random_subset",
                               km_iter = 10, em_iter = 10, var_floor = 1e-10, 
                               plot_data = T) 
#挑角度小的>>轉折>>不太好選
elec_stock_scale <- as.data.frame(center_scale(elec_stock_numeric))
opt_gmm = Optimal_Clusters_GMM(elec_stock_scale, max_clusters = 10, criterion = "BIC", 
                               dist_mode = "eucl_dist", seed_mode = "random_subset",
                               km_iter = 10, em_iter = 10, var_floor = 1e-10, 
                               plot_data = T) 
#標準化後下降速度快，選3群>>3支電子類股票 分類預測

#因每支股票多個樣本，故分群後取平均，看哪群機率大就丟那
#最後把每群機率最大的股票抓出
gmm = GMM(elec_stock_scale, 3, dist_mode = "eucl_dist", seed_mode = "random_subset", km_iter = 10,
          em_iter = 10, verbose = F)    
gmm$covariance_matrices #特徵篩選，接近0的不好，不刪
gmm_out <- as.data.frame(gmm$Log_likelihood) #大好
elec_stock_gmm <- cbind(elec_stock,gmm_out)
elec_stock_gmm <- elec_stock_gmm %>% 
                  group_by(名稱) %>% 
                  summarise(群一機率=mean(V1),群二機率=mean(V2),群三機率=mean(V3)) %>% 
                  mutate(最大=pmax(群一機率,群二機率,群三機率)) %>% 
                  mutate(分群=ifelse(最大==群一機率,"c1",
                              ifelse(最大==群二機率,"c2","c3")))
num_cluster <- elec_stock_gmm %>% 
               group_by(分群) %>%
               summarise(數量=n())
num_cluster %>%
  ggplot(aes(x=分群, y=數量)) + 
  geom_bar(stat = "identity", width=0.2,
           color="blue", fill=rgb(0.1,0.4,0.5,0.7))+
  #coord_flip()+
  ggtitle("分群結果")+
  ggeasy::easy_center_title()#標題置中


elec_stock_c1 <- elec_stock_gmm %>% 
                 subset(分群=="c1") %>% 
                 mutate(the_rank  = rank(最大,ties.method = "random")) %>%
                 filter(the_rank == max(the_rank))

elec_stock_c2 <- elec_stock_gmm %>% 
                 subset(分群=="c2") %>% 
                 mutate(the_rank  = rank(最大,ties.method = "random")) %>%
                 filter(the_rank == max(the_rank))

elec_stock_c3 <- elec_stock_gmm %>% 
                 subset(分群=="c3") %>% 
                 mutate(the_rank  = rank(最大,ties.method = "random")) %>%
                 filter(the_rank == max(the_rank))

elec_stock_c4 <- elec_stock_gmm %>% 
                 subset(分群=="c4") %>% 
                 mutate(the_rank  = rank(最大,ties.method = "random")) %>%
                 filter(the_rank == max(the_rank))
##no use####
elec_stock_c5 <- elec_stock_gmm %>% 
                 subset(分群=="c5") %>% 
                 mutate(the_rank  = rank(最大,ties.method = "random")) %>%
                 filter(the_rank == max(the_rank))
