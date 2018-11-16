# 출력 결과물 handicap.xlsx를 Gapminder 시각화에 원하는 포맷으로 변환

#현재의 디렉토리주소 알아보기
getwd()
#디렌토리 주소 재설정
setwd("C:/Users/hannews/Documents/GitHub/2018BigKinds/handicap")
#폴더 안의 파일들 확인하기
list.files()

install.packages("readxl")
library(readxl)

# 변환할 파일 읽어들이기
d <- read_excel("handicap2.xlsx")

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
stopwords <- c("장애인", "장애자", "경우", "백억", "경우", "내년", "때문", "올해",
               "이날", "가운데", "관련", "지난해", "우리", "이상", "만원", "시간")
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
write.table(df2, file="handi_gap.csv", sep=",", row.names=FALSE)
