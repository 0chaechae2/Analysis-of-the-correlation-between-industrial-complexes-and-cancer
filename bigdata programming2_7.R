#######선그래프/막대그래프 - 연령별 암발생자 수########
age <- read.csv('c:/Rworks/project/암종_연령별.csv',
                header=T, fileEncoding="euc-kr")
str(age)
age1 <- subset(age, sex=='계'& cancertype == '모든 암(C00-C96)')
age2 <- age1[2:19,]  #필요한 부분만 추출
head(age2)

agelen <- c()        #벡터 선언         
for (i in 1:length(age2$agefeel)) {
  agelen[i] <- age2$agefeel[i]
}               #비어있는 벡터에 agefeel열의 값 저장
print(agelen)

class(age2$occurenes19)           
age2$occurenes19 <- as.numeric(age2$occurenes19) #문자형 숫자형으로 변환

par(mfrow=c(1,2), mar=c(5,4,4,2)) #화면 분할(1x2)
#선그래프
plot(agelen,
     age2$occurenes19,
     main='연령별 암발생자 수',
     type='b',
     lty=1,
     lwd=1,
     xlab= '연령',
     ylab = '발생자 수',
     col='blue')

#막대그래프
color <- rep('#a8dadc',18)
color[13] <- '#1d3557'    #최대값 색깔 다르게 지정
length <- age2$occurenes19
barplot(length, main = '연령별 암발생자 수',xlab = '연령',
        ylab = '발생자 수',
        names= agelen,
        col = color ,border = '#457b9d')
par(mfrow=c(1,1), mar=c(5,4,4,2)+0.1)  #화면 분할 취소

#####결론: 연령마다 차이가 커 연령구조가 발생률에 미치는 영향을 제거하기 위해 앞으로는 연령표준화발생률을 기준으로 연구를 진행

##########지도 – 시군구별 연령표준화발생률###########
#지도 그릴 때 필요한 패키지 install 후 순서대로 import
install.packages("ggmap")
install.packages("ggplot2")
install.packages("raster")
install.packages("rgeos")
install.packages("maptools")
install.packages("rgdal")
#하나씩 순서대로 불러오기
library(ggmap)
library(ggplot2)
library(raster)
library(rgeos)
library(maptools)
library(rgdal)

setwd('C:/Rworks/project/map/2021')  #데이터 경로 지정
map <- shapefile('TL_SCCO_SIG2.shp') #2021-01 지리 정보 데이터셋 불러오기 #오류나면 다시 실행해주세요
map <- spTransform(map,CRS("+proj=longlat")) #위도,경도 좌표계 설정

df_map = fortify(map)    #map을 data frame으로 변환
slotNames(map)     
df_map_data = map@data    #map에서 data슬롯 정보 불러오기
df_map_data$SIG_KOR_NM <- iconv(df_map_data$SIG_KOR_NM, from = "CP949", to = "UTF-8", sub = NA, mark = TRUE, toRaw = FALSE) #한글 깨짐 방지
head(df_map_data)

df_map_data <- (df_map_data[order(df_map_data$SIG_CD),]) #행정구역 코드 오름차순으로 정리
head(df_map_data)
map@data <- df_map_data  #data정보 다시 map에 저장

cn_data <- read.csv('C:/Rworks/project/cancer(eng).csv',
                  header=T, fileEncoding="euc-kr")  #시각화할 데이터셋
##id 통일을 위해 df_map과 행정구역 동일하게 데이터 전처리##
head(cn_data)

cancer1 <- data.frame(cn_data)
cancer2 <- subset(cancer1, sex == '계')  #성별-'계' 정보만 저장
cancer2[, "id"] = (1:nrow(df_map_data))  #cancer2에 250개 행정구역에 대응하는 id변수생성

head(df_map)   #id정보 확인-행정구역 순서대로 id가 부여되어있다는 것을 알 수 있음
merge_result <- merge(df_map, cancer2, by='id')  #id변수로 두 데이터 합치기
head(merge_result)

#지도 그리기
ggplot() + geom_polygon(data = merge_result, aes(x=long, y=lat, group=group, fill = standariztion13)) +labs(fill='연령표준화발생률(명/10만명)')

#####결론: 암발생률 그래프와 산업단지 분포도와의 유사도로 보아 산업단지 인근과 암발생률이 상관관계가 있음.

############막대 그래프 – 전국/석유화학/철강/반도체 산단의 암발생률###########
cancer <- read.csv('C:/Rworks/project/시군구_암종_성별.csv',
                  header=T, fileEncoding="euc-kr")
cancer_df <- data.frame(cancer)
str(cancer_df)
filter <- function(state1,city1,sex1 = '계',cancertype1 = 'all') {
  cancer_vec <- as.numeric(c(subset(cancer_df, state == state1 & cancertype == cancertype1 & city == city1 & sex == sex1)$std2003,
                            subset(cancer_df, state == state1 & cancertype == cancertype1 &city == city1 & sex == sex1)$std2008,
                            subset(cancer_df, state == state1 & cancertype == cancertype1 &city == city1 & sex == sex1)$std2013))
  return(cancer_vec)  #광역단체, 시군구, 성별을 받아 조건에 맞는 값을 벡터로 변환
}

#전국 암발생률
nationwide_can <- filter('전국','전국')

#석유화학 산단의 암발생률
ulsan_can <- filter('울산광역시','울산광역시')
susan_can <- filter('충청남도','서산시')
yeosu_can <- filter('전라남도','여수시')
ulsansou_can <- filter('울산광역시','남구')
  
#반도체 산단의 암발생률
pyeongtaek_can <- filter('경기도','평택시')
yongin_can <- filter('경기도','용인시')
asan_can <- filter('충청남도','아산시')
hwaseong_can <- filter('경기도','화성시')
chungju_can <- filter('충청북도','청주시')
  
#철강 산단의 암발생률
pohang_can <- filter('경상북도', '포항시 남구')
gwangyang_can <- filter('전라남도', '광양시')

#각 벡터를 행 방향을 결합
ptc <- rbind(ulsan_can,susan_can,yeosu_can,ulsansou_can)
sec <- rbind(pyeongtaek_can,yongin_can,asan_can,hwaseong_can,chungju_can)
stl <- rbind(pohang_can,gwangyang_can)

#각 벡터의 이름 입력
colnames(ptc) <- c('std2003','std2008','std2013')
colnames(sec) <- c('std2003','std2008','std2013')
colnames(stl) <- c('std2003','std2008','std2013')

#연도별 평균을 구하여 저장
ptc_avg <- colSums(ptc)/nrow(ptc)
sec_avg <- colSums(sec)/nrow(sec)
stl_avg <- colSums(stl)/nrow(stl)

ds <- rbind(nationwide_can,ptc_avg, sec_avg, stl_avg)
ds

#색 지정하는 패키지
install.packages("RColorBrewer")
library(RColorBrewer)
mycol=brewer.pal(4,"Blues")

#막대그래프 작성
par(mfrow=c(1,1), mar=c(5,5,5,7))
barplot(ds, col = c(mycol),
        beside = TRUE,
        main = '산단유형별 암발생률 비교',
        legend.text= c('전국','석유화학','반도체','철강'),
        args.legend = list(x='topright', bty= 'n', inset = c(-0.3, 0)))
par(mfrow=c(1,1), mar=c(5,5,5,7)+0.1)

#####결론 : 반도체 산업단지는 다른 산단에 비해 암발생률에 영향이 적음.

#####################추가 연구############################
##########원그래프- 여수/광양의 암 종류 비율##########
str(cancer_df)

# 데이터 정리+ 암 발병률 상위 5개 추출
filter2 <- function(st, ci, sx = '계'){
  data <- subset(cancer_df, state == st & city == ci
                 & sex == sx & cancertype != 'all')
  data$std2013[is.na(data$std2013)] <- 0
  data$std2013 <- as.numeric(data$std2013)
  data.new <- data[order(data$std2013, decreasing=TRUE),]
  data_top5 <- data.new[1:5,]
  return(data_top5)
}

#원그래프 색 저장
col.5 <- rep('#a8dadc', 6)
col.5[1] <- '#1d3557'

par(mfrow=c(1,3), mar=c(5,5,5,7))  #화면 분할(1x3)
# 전국 원그래프
nationwide <- filter2('전국', '전국')   #필터를 통해 전국 데이터 추출
pie(nationwide$std2013, main = '전국 암발병률 TOP 5',
    labels = nationwide$cancertype,
    radius = 1, col = col.5)

# 여수 원그래프
yeosu <- filter2('전라남도', '여수시')    #필터를 통해 여수시 데이터 추출
pie(yeosu$std2013, main = '여수 암발병률 TOP 5',
    labels = yeosu$cancertype,
    radius = 1, col = col.5)

# 광양 원그래프
gwangyang <- filter2('전라남도', '광양시')  #필터를 통해 광양시 데이터 추출
pie(gwangyang$std2013, main = '광양 암발병률 TOP 5',
    labels = gwangyang$cancertype,
    radius = 1, col = col.5)
par(mfrow=c(1,1), mar=c(5,4,4,2)+0.1)  #화면 분할 취소

nationwide$std2013 / sum(nationwide$std2013)  # 전국 암 종류 비율
yeosu$std2013 / sum(yeosu$std2013)  # 여수 암 종류 비율
gwangyang$std2013 / sum(gwangyang$std2013)  # 광양 암 종류 비율

#####결론: 암 발병률이 높은 5종의 암 중에서 갑상선이 차지하는 비율이 여수와 광양에서 훨씬 높음을 확인할 수 있음.

##########여수/광양의 갑상선암 발병률 전국 순위 확인#########
thyroid <- read.csv('C:/Rworks/project/시군구_지역별_갑상선암.csv', 
                    header = T,
                    fileEncoding = "euc-kr",
                    skip = 2)
thyroid_df <- data.frame(thyroid)
str(thyroid_df)

thyroid <- subset(thyroid_df, sex == '계')
thyroid$std2013[is.na(thyroid$std2013)] <- 0   #결측값 0으로 치환
thyroid$std2013 <- as.numeric(thyroid$std2013)    #숫자로 변환
thyroid.new <- thyroid[order(thyroid$std2013, decreasing=TRUE),]  #내림차순 정렬
thyroid.new[1:5,]    #상위 5개의 값 추출

#####결론: 2013년 연령표준화발생률은 광양시가 1위, 여수시가 4위임을 확인할 수 있음

########여수 산단으로부터의 거리와 암발생률의 상관관계 분석########

library(geosphere)  #거리계산 패키지
library(xlsx)        #.xlsx 확장자를 불러오기 위한 패키지
library(readxl)      #.xlsx 확장자를 불러오기 위한 패키지

#엑셀파일의 모든 시트를 한번에 불러오는 함수작성
read_excel_allsheets <- function(filename, tibble = FALSE) {
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
  if(!tibble) x <- lapply(x, as.data.frame)
  names(x) <- sheets
  x
}

#함수 호출
setwd('c:/Rworks/project')
crd <- read_excel_allsheets('coordinate.xlsx')
str(crd)

#하나의 데이터프레임으로 결합
crd_df <- data.frame(do.call(rbind,crd))
str(crd_df)

#광역자치단체에 속한 도시이름을 반환하는 함수
name_city <- function(x){
  name <- unique(subset(thyroid, state == x)[,2])[-1]
  return(name)
}
JN_city <- name_city('전라남도')
KN_city <- name_city('경상남도')[-2][-3]

#광역자치단체와 시군구를 받아 위도와 경도를 출력하는 함수
crd_filter <- function(x,y){
  crd_df_vec <- as.numeric(c(subset(crd_df, state == x & city == y & is.na(crd_df$town))$longtitude,
                             subset(crd_df, state == x & city == y & is.na(crd_df$town))$latitude))
  return(crd_df_vec)
}
ys_cord <- crd_filter('전라남도','여수시') #여수의 위경도 좌표

JN_ys_dist <- c()
for(i in 1:length(JN_city)){
  print(JN_city[i])  #작동표지
  JN_ys_dist <- c(JN_ys_dist, distGeo(ys_cord,crd_filter('전라남도', JN_city[i])))
}     #전라남도의 시도와 여수사이의 거리
names(JN_ys_dist) <- JN_city
JN_ys_dist

KN_ys_dist <- c()
for(i in 1:length(KN_city)){
  print(KN_city[i])
  KN_ys_dist <- c(KN_ys_dist, distGeo(ys_cord,crd_filter('경상남도', KN_city[i])))
}     #경상남도의 시도와 여수사이의 거리
names(KN_ys_dist) <- KN_city
dist <- c(JN_ys_dist, KN_ys_dist)
dist

#thyroid 데이터를 자치단체, 시도, 연도, 성별에 따라 필터링하는 함수
filter_thy <- function(st, ci ,year, sx = '계'){
  cancer_vec <-as.numeric(subset(thyroid, state == st  & city == ci & sex == sx)[year])
  return(cancer_vec)
}

#데이터 프레임을 벡터로 변환하여 받기위해 작성한 함수
filter_thy_year <- function(stat, year,stat_name) {
  std_thy <- c()
  for(i in 1:length(stat_name)){
    std_thy <- c(std_thy, filter_thy(stat, stat_name[i],year))
  }
  return(std_thy)
}

#연도별
std_2003 <- c(filter_thy_year('전라남도','std2003',JN_city), 
              filter_thy_year('경상남도','std2003',KN_city))
std_2008 <- c(filter_thy_year('전라남도','std2008',JN_city), 
              filter_thy_year('경상남도','std2008',KN_city))
std_2013 <- c(filter_thy_year('전라남도','std2013',JN_city), 
              filter_thy_year('경상남도','std2013',KN_city))
thy_res <- data.frame(cbind(dist,std_2003,std_2008,std_2013))
str(thy_res)

#산점도 & 상관계수
par(mfrow=c(1,3), mar=c(5,5,5,7))
plot(thy_res$dist,
     thy_res$std_2003,
     main = '여수산단으로부터의 거리와 암발생률(2003)',
     xlab = '여수산단으로부터의 거리',
     ylab = '연령표준화')
plot(thy_res$dist,
     thy_res$std_2008,
     main = '여수산단으로부터의 거리와 암발생률(2008)',
     xlab = '여수산단으로부터의 거리',
     ylab = '연령표준화')
plot(thy_res$dist,
     thy_res$std_2013,
     main = '여수산단으로부터의 거리와 암발생률(2013)',
     xlab = '여수산단으로부터의 거리',
     ylab = '연령표준화')
par(mfrow=c(1,1), mar=c(5,5,5,7)+0.1)

cor(thy_res$dist, thy_res$std_2003)
cor(thy_res$dist, thy_res$std_2008)
cor(thy_res$dist, thy_res$std_2013)

#####결론: 출력된 상관계수의 값이 모두 0.4이하로 매우 약한 상관관계를 가졌다는 결과를 얻을 수 있음.
##################################################################