#### 패키지 설치 및 로드하기

install.packages("foreign") # foreign 패키지 설치

library(foreign) # SPSS 파일 불러오기
library(dplyr) # 전처리
library(ggplot2) #데이터 시각화
library(readxl) # 엑셀 파일 불러오기

# 한글이 깨져서 폰트 임포트 맥이라서?
install.packages("extrafont") 
library(extrafont) 
font_import()

theme_set(theme_grey(base_family='NanumGothic'))


# 데이터 불러오기

raw_welfare <- read.spss(file="../data/Koweps_hpc10_2015_beta1.sav", to.data.frame = T) 
# to.data.frame = T 는 SPSS 파일을 데이터 프레임 형태로 변환, 설정안하면 리스트로 불러옴

# 복사본 만들기

welfare <- raw_welfare

# 데이터 검토하기

head(welfare)
tail(welfare)
View(welfare)

#변수명 바꾸기

welfare <- rename(welfare,
                  sex = h10_g3, 
                  birth = h10_g4,
                  marriage = h10_g10,
                  religion = h10_g11,
                  income = p1002_8aq1,
                  code_job = h10_eco9,
                  code_region = h10_reg7
                  )

######## 예제 1 성별 월급  ########### 
# 성별 변수 검토및전처리
class(welfare$sex)
table(welfare$sex)

# 이상치 결측 처리
welfare$sex <- ifelse(welfare$sex==9, NA, welfare$sex )

# 결측치 확인

table(is.na(welfare$sex))


# 성별 항목 이름 부여

welfare$sex <- ifelse(welfare$sex==1, "male", "female")
table(welfare$sex)
qplot(welfare$sex)

# 월급 변수 검토 및 전처리
class(welfare$income)

summary(welfare$income)
qplot(welfare$income)
qplot(welfare$income) + xlim(0, 1000)


# 이상치 결측처리
welfare$income <- ifelse(welfare$income %in% c(0,9999), NA, welfare$income)
table(is.na(welfare$income))

# 분석하기

sex_income <- welfare %>% filter(!is.na(income)) %>% 
  group_by(sex) %>% 
  summarise(mean_income=mean(income))

sex_income

ggplot(data=sex_income, aes(x=sex, y=mean_income))+geom_col()

# 남성의 평균월급이 2배정도 높다.

######### 예제 2  나이 월급 ########### 

# 변수검토

class(welfare$birth)
summary(welfare$birth)

qplot(welfare$birth)

# 이상치 확인

table(is.na(welfare$birth))

# 파생변수 만들기
welfare$age <- 2015 - welfare$birth + 1
summary(welfare$age)
qplot(welfare$age)

# 분석하기
age_income <- welfare %>% filter(!is.na(income))%>%
  group_by(age)%>%
  summarise(mean_income=mean(income))

age_income

ggplot(data =age_income, aes(x=age, y=mean_income)) + geom_line()

# 40중반 50초중반이 평균소득이 높다.  중간 중간 감소되는 부분이 보이는데 직업별 정년 기간을 나타내는게 아닌가 싶다.

######### 예제 3  연령대 월급 ########### 
# 파생변수 만들기#
welfare <- welfare %>% mutate(ageg = ifelse(age<30, "young", ifelse(age <= 59, "middle", "old")))
table(welfare$ageg)
qplot(welfare$ageg)

#분석하기
ageg_income <- welfare %>% filter(!is.na(income)) %>%
  group_by(ageg) %>%
  summarise(mean_income = mean(income))
ageg_income

ggplot(data=ageg_income, aes(x=ageg, y=mean_income)) +geom_col() + scale_x_discrete(limits=c("young","middle","old"))
# 예상 과 같이 중년층이 가장의역할을 하는만큼 연봉이 가장 높게 형성되어있다.

######### 예제 4  성별 연령대 월급 ########### 

#분석하기

sexageg_income <-welfare %>% filter(!is.na(income)) %>%
  group_by(sex,ageg) %>%
  summarise(mean_income =mean(income))
sexageg_income

ggplot(data = sexageg_income, aes(x=ageg, y=mean_income, fill=sex))+ geom_col() + scale_x_discrete(limits=c("young", "middle", "old")) 
ggplot(data = sexageg_income, aes(x=ageg, y=mean_income, fill=sex))+ geom_col(position="dodge") + scale_x_discrete(limits=c("young", "middle", "old")) 

# 중년층에서 가장 크게 차이가보이는 듯하나 노년층도 모두 2배정도 차이나 보인다. 사회초년생은 다소 차이가 적다. 대부분 가장의 역할을 남자가 하는 것과 일맥 상통하다

##### 예제 5 나이 성별 월급 #########
# 분석하기

sexage_income <- welfare %>% filter(!is.na(income)) %>%
  group_by(sex, age)%>%
  summarise(mean_income= mean(income))
sexage_income

ggplot(data = sexage_income, aes(x=age,y=mean_income, col=sex))+geom_line()
# 여성은 30부터 점차 감소 하는 경향을 보이고 남성은 60세부터 감소한다. 결혼과 출산 에 영향이 있어 보인다.

##### 예제 6 직업 월급 #########

# 변수 검토
class(welfare$code_job)
table(welfare$code_job)

# 파생 변수 만들기
list_job<- read_excel("../data/Koweps_Codebook.xlsx", col_names = T, sheet=2)
list_job

dim(list_job)


# job 변수를 welfare 에 결합 공통으로 들어있는 code_job을 기준으로 결합
welfare <- left_join(welfare, list_job, by="code_job")

# 잘 결합되었는지 확인
welfare %>% filter(!is.na(income)) %>% select(code_job, job)%>% head(10)

# 분석하기

job_income <- welfare %>% filter(!is.na(income)) %>%
  group_by(job) %>% summarise(mean_income=mean(income))
job_income

# 양이 많은 관계로 순위를 보기위한 연봉기준 내림차순으로 5개만 뽑아보기
job_income5 <- job_income %>% arrange(desc(mean_income)) %>% head(5)
job_income5  


ggplot(data= job_income5, aes(x=job, y=mean_income))+geom_col()

# x 축 값이 길어서 90도 회전 시켜서 보자

ggplot(data= job_income5, aes(x=reorder(job, mean_income), y=mean_income))+geom_col() + coord_flip()
ggplot(data=job_income5, aes(x=mean_income, y=reorder(job,mean_income))) +geom_col()

# 한글이 깨져서 폰트 임포트
install.packages("extrafont") 
library(extrafont) 
font_import()

theme_set(theme_grey(base_family='NanumGothic'))

##### 예제 7 성별 직업 #########

# 분석하기

job_male <- welfare %>% filter(!is.na(job) & sex=="male") %>% group_by(job)%>%
  summarise(count=n()) %>%
  arrange(desc(count)) %>% head(10)
job_male

job_female <- welfare %>% filter(!is.na(job)& sex=="female") %>% group_by(job)%>%
  summarise(count=n()) %>%
  arrange(desc(count)) %>% head(10)
job_female

ggplot(data=job_male, aes(x=count, y=reorder(job, count)))+geom_col()
ggplot(data=job_female, aes(x=count, y=reorder(job,count)))+geom_col()

####### 예제 8 종교 혼인 #########
#  종교 변수 분석
class(welfare$religion)
table(welfare$religion)

#컬럼명을 부여하자
welfare$religion <- ifelse(welfare$religion==1, "yes", "no")
table(welfare$religion)

# 혼인 변수 분석

class(welfare$marriage)
table(welfare$marriage)

# 이혼 여부 파생 변수 만들기

welfare$group_marriage <- ifelse(welfare$marriage == 1, "marriage", ifelse(welfare$marriage == 3, "divorce", NA))
table(welfare$group_marriage)

table(is.na(welfare$group_marriage))
qplot(welfare$group_marriage) 

# 분석하기

# 종교 이혼여부 퍼센트 표 생성
religion_marriage <- welfare %>% filter(!is.na(group_marriage)) %>%
  group_by(religion, group_marriage) %>%
  summarise(count=n())%>%
  mutate(total_group=sum(count))%>%
  mutate(percent = (count/total_group*100))

religion_marriage

## 이혼 값만 추출

divorce <- religion_marriage %>% filter(group_marriage=="divorce")%>%
  select(religion, percent)

divorce

ggplot(data = divorce, aes(x=religion, y=percent))+geom_col()

# 종교가 있으면 좀더 안정적인가.. 이혼율이 종교있는 경우가 더 낮다. 의미있는 차이인지 모르겠다 수치상 근소한 차이이다.

####### 예제 9 연령대  종교 혼인 #########

# 분석
# 연령대별로 이혼율 표

ageg_marriage <- welfare %>% filter(!is.na(group_marriage)) %>%
  group_by(ageg, group_marriage) %>%
  summarise(count=n()) %>%
  mutate(total_group=sum(count))%>%
  mutate(percent = (count/total_group*100))

ageg_marriage

## 이혼 퍼센트 추출 및 young 집단 제외
ageg_divorce <- ageg_marriage %>% filter(group_marriage=="divorce"&ageg!="young")%>%
  select(ageg,percent)
ageg_divorce
ggplot(data=ageg_divorce, aes(x=ageg, y=percent))+geom_col()


# 연령대 및 종교 유무에 따른 이혼율표
ageg_religion_marriage <- welfare %>% filter(!is.na(group_marriage)) %>%
  group_by(ageg,religion, group_marriage) %>%
  summarise(count=n()) %>%
  mutate(total_group=sum(count))%>%
  mutate(percent = (count/total_group*100))

ageg_religion_marriage

## 이혼 퍼센트 추출 및 young 집단 제외

ageg_religion_divorce <- ageg_religion_marriage%>% filter(group_marriage=="divorce"&ageg!="young")%>%
  select(ageg,religion,percent)

ageg_religion_divorce

ggplot(data=ageg_religion_divorce, aes(x=ageg, y=percent, fill=religion))+geom_col(position = "dodge")

# 종교유무에 따른 이혼율이 노년층에게는 큰 의미가 없는 것으로 보인다.  관록이 느껴진다.

####### 예제 10 지역별 연령대 비율#########

# 지역 변수 검토하기
class(welfare$code_region)
table(welfare$code_region)

# 지역 코드 목록 만들기
list_region <- data.frame(code_region=c(1:7),
                          region=c("서울",
                                   "수도권(인천/경기)",
                                   "부산/경남/울산",
                                   "대구/경북",
                                   "대전/충남",
                                   "강원/충북",
                                   "광주/전남/전북/제주도"))
list_region

# 합치기
welfare <-left_join(welfare, list_region, by="code_region")

#확인하기
welfare %>% select(code_region, region)%>% head(5)

# 분석하기
ageg_region <- welfare %>% group_by(region,ageg)%>%
  summarise(count=n())%>%
  mutate(total_ageg = sum(count))%>%
  mutate(percent_ageg = (count/total_ageg*100))
ageg_region

ggplot(data=ageg_region, aes(x=percent_ageg,y=region,fill=ageg))+geom_col()

# 보기가 힘들다 노년층 내림차순 정렬한 변수를 만들기
list_order_old <- ageg_region %>% filter(ageg=="old")%>% arrange(percent_ageg) 
list_order_old
# 지역명 순서 변수 만들기
order <- list_order_old$region
order

ggplot(data=ageg_region, aes(x=percent_ageg, y=region, fill=ageg))+geom_col()+scale_y_discrete(limits=order)


# 막대색깔 순서 바꾸기
class(ageg_region$ageg)
levels(ageg_region$ageg)

ageg_region$ageg <- factor(ageg_region$ageg,
                           level = c("old", "middle","young"))
class(ageg_region$ageg)
levels(ageg_region$ageg)

ggplot(data=ageg_region, aes(x=percent_ageg, y=region, fill=ageg))+geom_col()+scale_y_discrete(limits=order)

#사견 :  나이가 들수록 서울 수도권에 빡빡한곳에서 벗어나고 싶어 하는 것 같다.