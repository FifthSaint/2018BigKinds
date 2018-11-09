#2018 언론진흥재단 뉴스빅데이터 '소수자 키워드' 프로젝
#현재의 디렉토리주소 알아보기
getwd()
#디렌토리 주소 재설정
setwd("C:/Users/hannews/Documents/GitHub/2018BigKinds/gay")
#디렌토리 주소 재설정
getwd()

#폴더 안의 파일들 확인하기
list.files()

install.packages("readxl")
library(readxl)

# 저장된 엑셀데이터 불러오기 
data <- read_excel("동성애.xlsx")

# 중복 기사 제거
install.packages("dplyr")
library(dplyr)
udata <- data[((data$"분석제외 여부" != "중복") & (data$"분석제외 여부" !="중복, 예외"))|is.na(data$"분석제외 여부"),]
#  subset(data, (data&"분석제외 여부" != "중복") & (data&"분석제외 여부" !="중복, 예외"))
#udata <- data %>% 
#  select(일자, 본문, "분석제외 여부") %>% 
#  filter("분석제외 여부" != "중복")

# 연도별로 데이터 나누기
year <- c(1990:2018)

for (val in year)
{
  nam <- paste("data", val, sep = "")
  assign(nam, subset(udata, grepl(paste0("^",val), 일자)))
}

install.packages("tm") #Corpus f.
library(tm)

install.packages("KoNLP") #extractNoun f.
library(KoNLP)
## 사전 선택 택1
# useSejongDic()
useNIADic()

install.packages("stringr") #str_replace_all f.
library(stringr)

### tempdata에서 콘텐츠(본문) 부분만 따오기 이후의 코드를 
### ext50 펑션으로 정의 그러나 톱 30으로 바꾸었음
ext50 <- function(data, year) {
contents <- data$'본문' 
# head(contents)

#영문표현삭제
newcontents <- str_replace_all(contents, "[[:lower:]]", "")
#제어문자 삭제
newcontents <- str_replace_all(newcontents, "[[:cntrl:]]", "")
#특수기호 삭제
newcontents <- str_replace_all(newcontents, "[[:punct:]]", "")
#숫자 = 삭제
newcontents <- str_replace_all(newcontents, "[[:digit:]]", "")
#괄호삭제
newcontents <- str_replace_all(newcontents, "\\(", "")
newcontents <- str_replace_all(newcontents, "\\)", "")

#따옴표 삭제
newcontents <- str_replace_all(newcontents, "'", "")
newcontents <- str_replace_all(newcontents, "'", "")

noun <- extractNoun(newcontents)

##불용어처리
txt_data <- gsub("//d+","",noun)
txt_data <- gsub("[[:cntrl:]]","",txt_data)
txt_data <- gsub("[[:punct:]]","",txt_data)
#이녀석이 숫자를 삭제해줍니다. 
txt_data <- gsub("[[:digit:]]","",txt_data)
txt_data <- gsub("[[:lower:]]","",txt_data)
txt_data <- gsub("[[:upper:]]","",txt_data)
txt_data <- gsub("[A-z]","",txt_data)
txt_data <- gsub("'","",txt_data)
txt_data <- gsub("'","",txt_data)
txt_data <- gsub("‘","",txt_data)
txt_data <- gsub("’","",txt_data)
# head(txt_data)

myCorpus <- Corpus(VectorSource(txt_data))

myCorpus <- tm_map(myCorpus, removePunctuation)
myCorpus <- tm_map(myCorpus, removeNumbers)
myCorpus <- tm_map(myCorpus, tolower)
myCorpus <- tm_map(myCorpus, stripWhitespace)

WordList <- sapply(myCorpus, extractNoun, USE.NAMES=FALSE)
vectordata <- unlist(WordList)
vectordata <- Filter(function(x){nchar(x)>1}, vectordata)

# preview<- sort(table(vectordata), decreasing=TRUE,100)
# View(preview)

#빈도추출 
wordcount <- table(vectordata)
# write.csv(wordcount,file="freq.csv")


#상위빈도로 정렬해서 result2로 명명
#result2 <- sort(wordcount, decreasing=TRUE)
# View(result2)

#누적빈도 알아보기
#cumsum.word.freq <-cumsum(result2)
#cumsum.word.freq[1:50]

#전체합이1이되는비율알아보기
#prop.word.freq <-cumsum.word.freq/cumsum.word.freq[length(cumsum.word.freq)]
#prop.word.freq[1:100]

#상위빈도 30단어 저장
result <- head(sort(wordcount, decreasing=TRUE), n=30)

#데이터프레임으로 변환
#df.result <-data.frame(result)
#연도 추가
#df.result$year <-year
#순위 추가
#df.result$rank <-rank(df.result$Freq)
return(result)
} # ext50 펑션 정의 끝

# 1990~2018 연도별 단어 추출
words <- vector("list", 29)
for (val in 1:29)
{
  nam1 <- paste("data", val+1989, sep ="")
  #nam2 <- paste("words", val, sep ="")
  assign("words[val]", ext50(eval(parse(text=nam1)), val+1989))
  #print(nam2$일자)
}

#합치기
t <- Reduce(function(x,y) rbind(x, y), list(words1990:words2018))

# csv로 저장하기
for (val in year)
{
  nam1 <- paste("words", val, sep ="")
  write.table(eval(parse(text=nam1)), file = paste(nam1, ".csv", sep=""), row.names=FALSE, sep=",")
}



