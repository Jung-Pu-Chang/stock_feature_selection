##資料清洗####
library(dplyr)
library(data.table) #fread dcast 
library(naniar) #chk na
library(ggplot2)
library(ggeasy)#置中

#2016-1-1~2018-9-7，區間穩定
elec_stock_raw <- fread(file="D:\\1.分群.csv",header=TRUE,stringsAsFactors = FALSE)
#na踢掉
any_na(elec_stock_raw)
elec_stock <- replace(elec_stock_raw,is.na(elec_stock_raw),"幹")
any_na(elec_stock)
elec_stock <- elec_stock %>% 
              subset(`ROA－綜合損益`!="幹"&常續性EPS!="幹"&稅後淨利率!="幹"&營業利益率!="幹"&營業毛利率!="幹"&`每股淨值(F)－TSE公告數`!="幹"&`ROE－綜合損益`!="幹")
#挑ROA總和>0，402>>315
elec_stock_need <- elec_stock %>% 
                   group_by(代號) %>% 
                   summarise(ROA=sum(`ROA－綜合損益`)) %>% 
                   subset(ROA>0)
elec_stock <- left_join(elec_stock,elec_stock_need,by="代號")
elec_stock$ROA[is.na(elec_stock$ROA)] <- 0
elec_stock <- subset(elec_stock,ROA>0)

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
elec_stock$`每股淨值(F)－TSE公告數` <- as.numeric(elec_stock$`每股淨值(F)－TSE公告數`)
elec_stock$`ROE－綜合損益` <- as.numeric(elec_stock$`ROE－綜合損益`)
elec_stock_numeric <- elec_stock[,4:10]

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
#標準化後下降速度快，選4群>>4支電子類股票 分類預測

#因每支股票多個樣本，故分群後取平均，看哪群機率大就丟那
#最後把每群機率最大的股票抓出
gmm = GMM(elec_stock_scale, 2, dist_mode = "eucl_dist", seed_mode = "random_subset", km_iter = 10,
          em_iter = 10, verbose = F)    
gmm$covariance_matrices #特徵篩選，接近0的不好，不刪
gmm_out <- as.data.frame(gmm$Log_likelihood) #大好
elec_stock_gmm <- cbind(elec_stock,gmm_out)
elec_stock_gmm <- elec_stock_gmm %>% 
                  group_by(名稱) %>% 
                  summarise(群一機率=mean(V1),群二機率=mean(V2)) %>% 
                  mutate(最大=pmax(群一機率,群二機率)) %>% 
                  mutate(分群=ifelse(最大==群一機率,"c1","c2"))
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
elec_stock_c1
elec_stock_c2
elec_stock_c3
elec_stock_c4
final <- left_join(elec_stock_raw,elec_stock_gmm,by="名稱")
write.table(final, file="D:\\分群結果.csv", sep = ",", na = "", row.names=FALSE, col.names = TRUE)

##no use####
elec_stock_c5 <- elec_stock_gmm %>% 
                 subset(分群=="c5") %>% 
                 mutate(the_rank  = rank(最大,ties.method = "random")) %>%
                 filter(the_rank == max(the_rank))
##版本2 >6/11####
elec_stock <- elec_stock %>% 
  mutate(同負=ifelse(ROA<0&ROE<0,"滾","留"))
elec_stock_rate <- elec_stock %>% 
  group_by(代號,名稱) %>% 
  summarise(次數=n())                   
elec_stock <- left_join(elec_stock,elec_stock_rate,by="代號")
elec_stock <- elec_stock %>% 
  subset(同負=="留") %>% #先刪35全負的
  group_by(代號,名稱.x,次數) %>% 
  summarise(有正=n()) %>% 
  mutate(正比率=有正/次數) %>% 
  subset(正比率>=6/11)
elec_stock <- left_join(elec_stock_raw,elec_stock,by="代號")
#不要的 <- left_join(elec_stock_rate,elec_stock,by="代號")
elec_stock$正比率[is.na(elec_stock$正比率)] <- 0
不要的 <- subset(elec_stock,正比率==0)
elec_stock <- subset(elec_stock,正比率!=0)

write.table(不要的, file="D:\\拉基.csv", sep = ",", na = "", row.names=FALSE, col.names = TRUE)
