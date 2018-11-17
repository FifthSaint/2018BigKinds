# 여성 출력 결과물 women.xlsx를 
# Gapminder 시각화에 원하는 포맷으로 변환

#현재의 디렉토리주소 알아보기
getwd()
#디렉토리 주소 재설정
setwd("C:/Users/hannews/Documents/GitHub/2018BigKinds/women")
#폴더 안의 파일들 확인하기
list.files()

install.packages("readxl")
library(readxl)

# 변환할 파일 읽어들이기
d <- read_excel("women.xlsx")

# 칼럼의 수
nc <-ncol(d)
# 3개씩 분할
m = nc / 3

# 분할
df<-lapply(split(as.list(d), cut(1:nc, m, labels = FALSE)), as.data.frame)

#################
# stopwords 제거
#################
#빌드할 데이터 프레임 정의
df2 <- data.frame("Word"=character(),
                  "Freq"=integer(), 
                  "Year"=as.Date(character(), "%Y"))

library(dplyr) #filter
#여성용 스탑워드
stopwords <- c( "여성", "문제", "우리", "이번", "때문", "사람", "이날", "이상", "가운데", 
                "자신", "지난해", "생각", "사실", "관련", "정도", "여자", "경우", "이후", 
                "시간", "만원", "결과", "기자", "사진")
# 칼럼 이름 확인
colnames(as.data.frame(df[1]))

# 루프
for (i in 1:29) {
  colname <- paste0("X", i, ".vectordata", i)
  df1 <- filter(as.data.frame(df[i]), !grepl(paste(stopwords, collapse="|"), eval(parse(text=colname)))) #스탑워드 제거
  df1 <- df1[-(21:nrow(df1)),] #20개만 남김
  colnames(df1) <-c("Word", "Freq", "Year")
  df2 <- rbind(df2, df1)
}

#출고
write.table(df2, file="women_gap.csv", sep=",", row.names=FALSE)
