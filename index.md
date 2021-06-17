# 程式設計與資料科學導論
第十一組
# 組員&工作&投入分數
社會一 陳星丞 蒐集資料+分析+網站建置＋md撰寫(6)
公衛一 鄧禮頡 整理+報告    (6)
公衛一 張震奕 統計分析+海報    (6)
公衛一 丁子翔 海報        (6)
## 實驗動機
- 以八卦版(爆掛+問卦+新聞)當日發文數以及發文的標題，
              去分析"情緒"對於發文數是否有影響
## 套件&程式
Python: Snownlp ,Jieba
R: dplyr,tidy,ggplot2
SAS: 假設檢定
## 操作流程
### 資料爬取
```python=
#載入套件
import requests,re,threading
from bs4 import BeautifulSoup as bs
#設定cookie
cookies={
    "__cf_bm":"b061f4d11494eb98c01e49950891fa97dc746683-1620901046-1800-AauhlnO0M9nCanP6vCyG9DD1VZlcWfe1fBc7L0xv+BO6F/ISc3qfjbHCfZCXvkGUiVTFZhJEjHSYoLmDYsSy9vk",
    "_ga":"GA1.2.197183732.1620494134",
    "_gid":"GA1.2.986367917.1620886017",
    'over18':1,
    "__cfduid":"dda0debaa78515d636afe69ca1066d3871618801296"
}
#取得為18歲的認證和設定檔所在的資料夾
r = requests.Session()
payload ={
    "from":"/bbs/Gossiping/index.html",
    "yes":"yes"
}
f=open("ptt_data_mult",'w')
temp="0/0"
r = requests.Session()
backup=[]
r1 = r.post("https://www.ptt.cc/ask/over18?from=%2Fbbs%2FGossiping%2Findex.html",payload)
#定義爬蟲程式
def process(i):
    place="https://www.ptt.cc/bbs/Gossiping/index"+str(i)+".html"
    text=r.get(place)
    content=bs(text.text,features='html.parser')
    titles=content.find_all('div',attrs={'class':'title'})
    date=content.find_all('div',attrs={'class','date'})
    print(i)
    try:
        for k in range(len(titles)):
            """
            if str(date[k].text) == ' 1/01' and str(temp) == ' 12/31':
                j = j + 1
            temp = str(date[k].text)
            """
            lock.acquire()
            #以csv格式輸出
            f.writelines(str(i)+","+'"'+re.sub('"',"",re.sub(",","/",str(re.sub("\n","",titles[k].text))))+'",'+str(re.sub(" ","",date[k].text)+"\n"))
            lock.release()
    except:
        print("errrrrrrrow")
#以threads加快爬取2014~2021的ptt標題
if __name__ == '__main__':
    lock=threading.Lock()
    thread=[None for i in range(40000)]
    for i in range(341,39429):
        thread[i]=threading.Thread(target=process,args=(i,))
        thread[i].start()
    for i in range(341,39429):
        thread[i].join()
```
## 資料篩選
```r=
library(tidyr)
library(dplyr)
library(ggplot2)
library(utils)
library(lubridate)
ptt_2014to2021_year<-read_csv("2014to2021_year.csv")

ptt_2014to2021_year%>%filter(cata %in% c("問卦","新聞","爆卦"))%>%group_by(year(time))%>%summarise(n())->tt
mutate(ptt_2014to2021_year,year=year(time),month=month(time))->tt
mutate(tt,ym=paste(year,month,"1",sep="-"))->tt
tt%>%group_by(ym)%>%summarise(freq=n())->tt
tt$y_m<-as.Date(tt$ym)
ggplot(data=tt)+
  geom_point(aes(x=y_m,y=freq))
```

###### ![](https://i.imgur.com/SJXw7TC.png)

```r=
#透過上圖，發現大多資料點集中在2020-03之後，因此開始篩選2020-03到2021-05的資料
select(ptt_2014to2021_year,time,cata,title)->dt
filter(dt,cata %in% c("問卦","爆卦","新聞"))->dt
filter(dt,as.Date(time)>as.Date("2020-03-31") & as.Date(time)<as.Date("2021-06-01"))->dt
dt%>%group_by(time,cata)%>%summarise(titles=paste(title,collapse = " "),freq=n())->ptt_2020to2021_daily
write.csv(ptt_2020to2021_daily,'./ptt_20202021_daily.csv')
```
### 進行情感正面程度分析
```bash=
pip install snownlp #下載snownlp
```
```python=
import csv
from snownlp import SnowNLP,sentiment

csvfile=open('ptt_20202021_daily.csv', newline='')
rows=csv.DictReader(csvfile)
data=[]
for i in rows:
     data.append(i)

x=0
for i in data:
     x=x+1
     lis=i['titles'].split()
     soc=0
     for j in lis:
          snow=SnowNLP(j)
          soc=soc+snow.sentiments
     i['sentiment_value']=(soc/len(lis))
     print(x/len(data))
     print((soc/len(lis)))
with open('sentiment_ptt.csv',"w",newline='') as csvfile:
     tname=['','time','cata','titles','freq','sentiment_value']
     writer = csv.DictWriter(csvfile, fieldnames=tname)
     writer.writeheader()
     for i in data:
          writer.writerow(i)
```
### 進行資料清理、分析
```r=
sentiment<-read.csv(sentiment_ptt.csv)
#重新命名
sentiment_ptt$cata[sentiment_ptt$cata=='問卦']<-"ask"
sentiment_ptt$cata[sentiment_ptt$cata=='爆卦']<-"hit"
sentiment_ptt$cata[sentiment_ptt$cata=='新聞']<-"news"
```

### 對情緒正面程度與發文數做分析
```r=
ggplot(data=sentiment_ptt)+
geom_point(mapping = aes(x=sentiment_value,y=freq,color=cata))+
geom_smooth(method = "lm",mapping = aes(x=sentiment_value,y=freq,color=cata))
```
![](https://i.imgur.com/7buakvu.png)
情緒正面程度由0~1，越大代表越正面
### 各類別的平均情緒
```r=
sentiment_ptt%>%group_by(cata)%>%summarise(sentiment=mean(sentiment_value))->cata_sent
ggplot(data=cata_sent,aes(cata,sentiment,fill=cata))+
geom_bar(stat='identity',width = 0.5)
```
![](https://i.imgur.com/DLHBHaI.png)

```r=
bartlett.test(sentiment_ptt$sentiment_value~sentiment_ptt$cata)
```
![](https://i.imgur.com/XWexElZ.png)
```r=
Ft<-aov(sentiment_ptt$sentiment_value~sentiment_ptt$cata,sentiment_ptt)
summary(Ft)
```
![](https://i.imgur.com/RHwDtpc.png)
```r=
pairwise.t.test(sentiment_ptt$sentiment_value,sentiment_ptt$cata, p.adjust.method="bonferroni")
```
![](https://i.imgur.com/X1BgUtl.png)
### 對時間與情緒正面程度做分析
```r=
ggplot(data=sentiment_ptt,mapping = aes(x=time,y=sentiment_value,color=cata))+
geom_line()
```
![](https://i.imgur.com/dUGGJ62.png)

### 合併類別進行分析
```r=
entiment_ptt->ttt
ttt$cata<-NULL
head(ttt)
ttt%>%select(time,freq,sentiment_value)%>%group_by(time)%>%summarise(freq=sum(freq),sentiment=(freq*sentiment_value)/(sum(freq)))->nocata_value
ggplot(data=nocata_value,aes(sentiment,freq))+
  geom_point()+
  geom_smooth(method = "lm")
```

![](https://i.imgur.com/voVg7HP.jpeg)
### 檢驗情緒正面程度與發文頻率的關係
```r=
model_1=lm(freq~sentiment,data = nocata_value)
summary(model_1)

#output:
#lm(formula = freq ~ sentiment, data = nocata_value)
#
#Residuals:
#     Min       1Q   Median       3Q      Max 
#-1925.35  -224.07    10.25   177.83  2114.04 
#
#Coefficients:
#            Estimate Std. Error t value Pr(>|t|)    
#(Intercept)   1954.5       82.7  23.634   <2e-16 ***
#sentiment     -167.2      139.8  -1.196    0.232    
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Residual standard error: 357.4 on 1231 degrees of freedom
#Multiple R-squared:  0.00116,	Adjusted R-squared:  0.0003487 
#F-statistic:  1.43 on 1 and 1231 DF,  p-value: 0.232
```
透過此一檢定，可知在標題的情緒正面程度與發文頻率並無顯著的相關性，即便有，其可解釋的變異也極小，因此可以推測，情緒並無顯著的影響發文量
## 結論
- 在情緒正面程度與發文量上並無明顯的相關
- 在爆卦、問卦及新聞三個類別中，情緒正面程度依序為爆卦、新聞、問卦
- 其中爆卦的情緒變異性較大

