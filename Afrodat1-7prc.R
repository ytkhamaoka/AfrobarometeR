#--------
#----Afrodatae wave 1-7を読み込んだデータを使って各種処理
#----2019/12/10 hamaoka@fbc.keio.ac.jp
#--------
#Rについては例えば　濱岡の下記ページ参照
#　http://news.fbc.keio.ac.jp/~hamaoka/cgi-bin/fswiki/wiki.cgi?page=R　


#install.packages(c("dplyr","haven","memisc","biglm","nlme","ICC","lavaan","semTools","biglm"))	#としてインストールしておく
options(width=150)
library(dplyr)	#国別集計用
library(haven)	#SPSSデータ読み込み
library(memisc)	#lmの出力整形
library(biglm)	#bigdata lm,glm
library(nlme)	#multi-level
library(lattice)	#lattice plot
library(ICC)	#級内相関
library(lavaan)	
library(semTools)	
library(biglm)

#ファイルのあるディレクトリ指定　　自分のAfroデータのあるところに変更  下記を書き換えるか､Rのメニューで指定
setwd("/Users/yh/Dropbox/Files2019/研究2019/AfroData")
#save.image("0Afrodat.img")

#load("0Afrodat.img");ls()

#Afrodat1-Afrodat7　やそれらの共通の変数をまとめたAfrodatAllなどが入っている

#データ
#http://afrobarometer.org/data/merged-data
#http://afrobarometer.org/data/merged-round-6-data-36-countries-2016
#Merged Round 6 data (36 countries) (2016) (last updated: 2016)

# Round7
#https://www.afrobarometer.org/data/merged-round-7-data-34-countries-2019
#　code bookは未公開


#NAを除いて平均計算
mean2<-function(x){
	m<-mean(x,na.rm=T)
	return(m)
	}

max2<-function(x){
	m<-max(x,na.rm=T)
	return(m)
	}

min2<-function(x){
	m<-min(x,na.rm=T)
	return(m)
	}

# naの数をカウント
sum.NA<-function(x){
	m<-sum(is.na(x))
	return(m)
	}

table2<-function(x1,x2,...){ 
	st<-table(x1,x2,...) 
	Ic<-matrix(1,nrow(st),1) 
	Ir<-matrix(1,ncol(st),1) 
		sr<-st%*%Ir					#行和 
		sc<-t(st)%*%Ic			#列和 
		s0<-t(sr)%*%Ic			#総和 
	#----------------------- 
	ss0<-st 
 	sst<-rbind(st,t(sc)) 		 #行和、列和を入れた行列に 
	sc<-rbind(sc,s0) 		#列和にさらに総計を入れておく 
	sr<-rbind(sr,s0) 		#行和にさらに総計を入れておく
	 sst<-cbind(sst,sr)   #もとの表に列和、行和、総和がついたもの
	 print(sst)
 #-------------------------------- 行和を100% 
 pr<-sst 
	 for (j in seq(1:ncol(sst))){ 
 	pr[,j]<-pr[,j]/sr*100 
 	} 
 #小数点1桁までで丸めて出力 
 pr<-round( pr,1) 
	 pr<-cbind(pr,sr)		#和のベクトルをつけて返す
			colnames(pr)[length(colnames(pr))-1]<-"%"
			colnames(pr)[length(colnames(pr))]<-"N"
 #列和を100% 
 pc<-sst 
 for (i in seq(1:nrow(sst))){ 
	 pc[i,]<-pc[i,]/t(sc)*100 
	 } 
 pc<-round( pc,1) 
 #総和を100% 
 p0<-sst 
 for (i in seq(1:nrow(sst))){ 
 	for (j in seq(1:ncol(sst))){ 
		 p0[i,j]<-p0[i,j]/s0*100 
	 } 
	 } 
 p0<-round( p0,1) 
	chisq<-chisq.test(st)
	return(list(pr,chisq))
 } 
 
#-------------------------------
dwaw.line2<-function(dat){		#datの中にあるtを横軸､yを縦軸にとってプロット｡左端に国記号　変数名は固定　dat$t:year, dat$y, dat$cnam: country name
	d<-dim(dat)[1]
	lines(dat$t,dat$y)
		text(dat$t[d],dat$y[d],dat$cnam[d],pos=4,cex=0.5)		#最後のtのところにcountry nameを
		print(dat[,c("t","y","cnam")])		#最後のtのところにcountry nameを
	}

#datの中にあるcnam毎にトレンドをプロット
group_trend_plot<-function(dat,lab){
par(mfrow=c(1,1))
	ymin<-min2(dat$y)
	ymax<-max2(dat$y)
	tmax<-max2(dat$t)
	tmin<-min2(dat$t)
	plot(dat$t,dat$y,xlim=c(tmin,tmax+1),ylim=c(ymin,ymax),main=lab,xlab="Year",ylab="Mean")
		by(dat,dat$cnam,dwaw.line2)
	}

head(m)
#Radio
#dat<-m[,c(1:3)];names(dat)<-c("cnam","t","y")
#	group_trend_plot(dat,lab="Radio")



load(file="0AfrodatAllN.rda");names(AfrodatAllN)	#afro1-7の使う変数データにWbankの国レベルデータ(1次らぐつき)をつけたもの


#-----
#AfrodatAllN		"Own_Radio","Own_TV","Own_Auto","Own_Mbphone",
AfrodatAllNg<-group_by(AfrodatAllN, AfrodatAllN$COUNTRY2,AfrodatAllN$year)	#国､年別に集計することを指定

#平均値		国別､年別
(mdat<-summarise(AfrodatAllNg,N=n(),
mean2(wave),mean2(year),mean2(COUNTRY2),
mean2(dUrban),mean2(Age),mean2(Gender_f),mean2(Language),mean2(Race),
mean2(Cond_econ),mean2(Cond_your_liv),mean2(Relative_live),
mean2(gone_food),mean2(gone_water),mean2(gone_med),mean2(gone_fuel),mean2(gone_cash),mean2(gone_electricity),
mean2(Interest_pubaff),
mean2(Discuss_politics),mean2(dDiscuss_politics),
mean2(Mem_religious),mean2(Mem_voluntary),
mean2(Cit_action_Attend_meeting),mean2(Cit_action_raise_issue),
mean2(Diss_request_government),mean2(Diss_Contact_official),mean2(Diss_Attend_demonstration),
mean2(Democ_pref),mean2(dDemoc_pref),
mean2(Democ_nation),mean2(Democ_satis),
mean2(Trust_president),mean2(Trust_parliament),mean2(Trust_police),mean2(Trust_traditional_leaders),mean2(Trust_religious_leaders),
mean2(Use_Inet),mean2(Use_Mbphone),
#mean2(Employment_status),
#mean2(Occupation),
mean2(Education),
mNews_Radio=mean2(News_Radio),mNews_Television=mean2(News_Television),mNews_Newspaper=mean2(News_Newspaper),mNews_Internet=mean2(News_Internet),mNews_Social_media=mean2(News_Social_media),
	mOwn_Radio=mean2(Own_Radio),mOwn_TV=mean2(Own_TV),mOwn_Auto=mean2(Own_Auto),mOwn_Mbphone=mean2(Own_Mbphone)
))

edit(mdat)

mdat<-as.data.frame(mdat)
	names(mdat)
# [1] "AfrodatAllN$COUNTRY2"             "AfrodatAllN$year"                 "N"                                "mean2(wave)"                     
# [5] "mean2(year)"                      "mean2(COUNTRY2)"                  "mean2(dUrban)"                    "mean2(Age)"                      
# [9] "mean2(Gender_f)"                  "mean2(Language)"                  "mean2(Race)"                      "mean2(Cond_econ)"                
#[13] "mean2(Cond_your_liv)"             "mean2(Relative_live)"             "mean2(gone_food)"                 "mean2(gone_water)"               
#[17] "mean2(gone_med)"                  "mean2(gone_fuel)"                 "mean2(gone_cash)"                 "mean2(gone_electricity)"         
#[21] "mean2(Interest_pubaff)"           "mean2(Discuss_politics)"          "mean2(dDiscuss_politics)"         "mean2(Mem_religious)"            
#[25] "mean2(Mem_voluntary)"             "mean2(Cit_action_Attend_meeting)" "mean2(Cit_action_raise_issue)"    "mean2(Diss_request_government)"  
#[29] "mean2(Diss_Contact_official)"     "mean2(Diss_Attend_demonstration)" "mean2(Democ_pref)"                "mean2(dDemoc_pref)"              
#[33] "mean2(Democ_nation)"              "mean2(Democ_satis)"               "mean2(Trust_president)"           "mean2(Trust_parliament)"         
#[37] "mean2(Trust_police)"              "mean2(Trust_traditional_leaders)" "mean2(Trust_religious_leaders)"   "mean2(Use_Inet)"                 
#[41] "mean2(Use_Mbphone)"               "mean2(Education)"                 "mNews_Radio"                      "mNews_Television"                
#[45] "mNews_Newspaper"                  "mNews_Internet"                   "mNews_Social_media"               "mOwn_Radio"                      
#[49] "mOwn_TV"                          "mOwn_Auto"                        "mOwn_Mbphone"                    


names(mdat)[1:2]<-c("COUNTRY2","year")

dat<-mdat[,c("COUNTRY2","year","mOwn_Radio")];names(dat)<-c("cnam","t","y")	##40-90%   w3から大きくはかわらず｡　w7では低下の国が多い(が質問方法がかわった)
	group_trend_plot(dat,lab="own Radio")

dat<-mdat[,c("COUNTRY2","year","mOwn_TV")];names(dat)<-c("cnam","t","y")	#10-90%   w3から微増傾向　w7では低下の国が多い(が質問方法がかわった)
	group_trend_plot(dat,lab="own TV")

dat<-mdat[,c("COUNTRY2","year","mOwn_Auto")];names(dat)<-c("cnam","t","y")	#0-40%s
	group_trend_plot(dat,lab="own Auto")

dat<-mdat[,c("COUNTRY2","year","mOwn_Mbphone")];names(dat)<-c("cnam","t","y")	#w4　だと30%程度の国もあるが､多くは50%以上｡　W6だと､多くの国で8割を超えている
	group_trend_plot(dat,lab="own Mbphone")


#	ニュースソースとしての利用状況
dat<-mdat[,c("COUNTRY2","year","mNews_Radio")];names(dat)<-c("cnam","t","y")		#低下傾向
	group_trend_plot(dat,lab="Radio")
dat<-mdat[,c("COUNTRY2","year","mNews_Television")];names(dat)<-c("cnam","t","y")	#ほぼ横ばい　7から低下国も
	group_trend_plot(dat,lab="TV")

dat<-mdat[,c("COUNTRY2","year","mNews_Newspaper")];names(dat)<-c("cnam","t","y")	#低下傾向
	group_trend_plot(dat,lab="Newspaper")

dat<-mdat[,c("COUNTRY2","year","mNews_Internet")];names(dat)<-c("cnam","t","y")		#増加
	group_trend_plot(dat,lab="Internet")

dat<-mdat[,c("COUNTRY2","year","mNews_Social_media")];names(dat)<-c("cnam","t","y")	#増加
	group_trend_plot(dat,lab="Social_media ")




##--year毎に
v<-as.formula('~Own_TV+Age+Gender_f+Education+	#dUrban+
Mem_religious+
gone_water+
dEmployment_status_no+   #これらはNA2.3万dEmployment_status_looking+dEmployment_status_part_time+dEmployment_status_full_time+
dRace_BAf+dRace_Wh+dRace_Col+dRace_Arab+dRace_SAs+dRace_EAs	+
dlang_English+dlang_French+dlang_Portuguese+dlang_Swahili+dlang_Arabic+dlang_Afrikaans+dlang_Chichewa+dlang_Akan+dlang_Other+dlang_Egyptian_Arabic+dlang_Crioulo+dlang_Kirund+dlang_Sesotho+dlang_Sudanese_Arabic+dlang_Creole+dlang_siSwati+dlang_Shona+dlang_Algerian_Arabic+
dRace_BAf+dRace_Wh+dRace_Col+dRace_Arab+dRace_SAs+dRace_EAs+
dReligion_Muslim+dReligion_RomanCatholic+dReligion_Christian+dReligion_Pentecostal+dReligion_Anglican+dReligion_Evangelical+dReligion_none+dReligion_Lutheran+dReligion_Methodist+dReligion_Independent+dReligion_SeventhDay+
as.factor(COUNTRY2)')	#dRace_Oth Religion+as.factor(Education)+

res_oOwn_Mbphone<-glm(formula(paste("Own_Mbphone~",v,sep="")[2]),family= binomial(link = "logit"), data=AfrodatAllN)
	summary(res_oOwn_Mbphone)

res_oOwn_Mbphone_w4<-glm(formula(paste("Own_Mbphone~",v,sep="")[2]),family= binomial(link = "logit"), data=AfrodatAllN[AfrodatAllN$wave==4,])
	summary(res_oOwn_Mbphone_w4)
res_oOwn_Mbphone_w5<-update(res_oOwn_Mbphone_w4,.~.,data=AfrodatAllN[AfrodatAllN$wave==5,])
	summary(res_oOwn_Mbphone_w5)
res_oOwn_Mbphone_w6<-update(res_oOwn_Mbphone_w4,.~.,data=AfrodatAllN[AfrodatAllN$wave==6,])
	summary(res_oOwn_Mbphone_w6)
res_oOwn_Mbphone_w7<-update(res_oOwn_Mbphone_w4,.~.,data=AfrodatAllN[AfrodatAllN$wave==7,])
	summary(res_oOwn_Mbphone_w7)

mtable(res_oOwn_Mbphone,res_oOwn_Mbphone_w4,res_oOwn_Mbphone_w5,res_oOwn_Mbphone_w6,res_oOwn_Mbphone_w7,
	summary.stats=c("p","N","AIC"))


##--year  国レベル
v<-as.formula('~Own_TV+Age+Gender_f+Education+	#dUrban+
Mem_religious+
gone_water+
dEmployment_status_no+   #これらはNA2.3万dEmployment_status_looking+dEmployment_status_part_time+dEmployment_status_full_time+
dRace_BAf+dRace_Wh+dRace_Col+dRace_Arab+dRace_SAs+dRace_EAs	+
dlang_English+dlang_French+dlang_Portuguese+dlang_Swahili+dlang_Arabic+dlang_Afrikaans+dlang_Chichewa+dlang_Akan+dlang_Other+dlang_Egyptian_Arabic+dlang_Crioulo+dlang_Kirund+dlang_Sesotho+dlang_Sudanese_Arabic+dlang_Creole+dlang_siSwati+dlang_Shona+dlang_Algerian_Arabic+
dRace_BAf+dRace_Wh+dRace_Col+dRace_Arab+dRace_SAs+dRace_EAs+
dReligion_Muslim+dReligion_RomanCatholic+dReligion_Christian+dReligion_Pentecostal+dReligion_Anglican+dReligion_Evangelical+dReligion_none+dReligion_Lutheran+dReligion_Methodist+dReligion_Independent+dReligion_SeventhDay+
as.factor(COUNTRY2)+
year+GDP_per_capita2+Mobile_cellular_subscriptions2+Access_to_electricity2
')	#dRace_Oth Religion+as.factor(Education)+


#人口密度
AfrodatAllN$PopDens<-AfrodatAllN$Population__total2/AfrodatAllN$Surface_area2

#中心化
m<-mean2(AfrodatAllN$year)
	AfrodatAllN$yearc<-AfrodatAllN$year-m
m<-mean2(AfrodatAllN$GDP_per_capita2)
	AfrodatAllN$GDP_per_capita2c<-AfrodatAllN$GDP_per_capita2-m
m<-mean2(AfrodatAllN$Access_to_electricity2)
	AfrodatAllN$Access_to_electricity2c<-AfrodatAllN$Access_to_electricity2-m
m<-mean2(AfrodatAllN$Mobile_cellular_subscriptions2)
	AfrodatAllN$Mobile_cellular_subscriptions2c<-AfrodatAllN$Mobile_cellular_subscriptions2-m

attach(AfrodatAllN,warn=F)
cor(data.frame(year,yearc,GDP_per_capita2,GDP_per_capita2c,GDP_per_capita2*year,GDP_per_capita2c*yearc),use="complete")

res_oOwn_Mbphone_2<-glm(Own_Mbphone~year+GDP_per_capita2c+Access_to_electricity2c+
Own_TV+Age+Gender_f+Education+	#dUrban+
Mem_religious+
gone_water+
dEmployment_status_no+   #これらはNA2.3万dEmployment_status_looking+dEmployment_status_part_time+dEmployment_status_full_time+
dRace_BAf+dRace_Wh+dRace_Col+dRace_Arab+dRace_SAs+dRace_EAs	+
dlang_English+dlang_French+dlang_Portuguese+dlang_Swahili+dlang_Arabic+dlang_Afrikaans+dlang_Chichewa+dlang_Akan+dlang_Other+dlang_Egyptian_Arabic+dlang_Crioulo+dlang_Kirund+dlang_Sesotho+dlang_Sudanese_Arabic+dlang_Creole+dlang_siSwati+dlang_Shona+dlang_Algerian_Arabic+
dRace_BAf+dRace_Wh+dRace_Col+dRace_Arab+dRace_SAs+dRace_EAs+
dReligion_Muslim+dReligion_RomanCatholic+dReligion_Christian+dReligion_Pentecostal+dReligion_Anglican+dReligion_Evangelical+dReligion_none+dReligion_Lutheran+dReligion_Methodist+dReligion_Independent+dReligion_SeventhDay+
as.factor(COUNTRY2),family= binomial(link = "logit"), data=AfrodatAllN)	#+Mobile_cellular_subscriptions2
	summary(res_oOwn_Mbphone_2)


res_oOwn_Mbphone_2.1<-update(res_oOwn_Mbphone_2,.~.+GDP_per_capita2c:yearc)
	summary(res_oOwn_Mbphone_2.1)
#year:GDP_per_capita2 がマイナスなので
res_oOwn_Mbphone_2.1.2<-update(res_oOwn_Mbphone_2.1,.~.-GDP_per_capita2)
	summary(res_oOwn_Mbphone_2.1.2)


res_oOwn_Mbphone_w4_2<-glm(formula(paste("Own_Mbphone~",v,sep="")[2]),family= binomial(link = "logit"), data=AfrodatAllN[AfrodatAllN$wave==4,])
	summary(res_oOwn_Mbphone_w4_2)
res_oOwn_Mbphone_w5_2<-update(res_oOwn_Mbphone_w4,.~.,data=AfrodatAllN[AfrodatAllN$wave==5,])
	summary(res_oOwn_Mbphone_w5_2)
res_oOwn_Mbphone_w6_2<-update(res_oOwn_Mbphone_w4,.~.,data=AfrodatAllN[AfrodatAllN$wave==6,])
	summary(res_oOwn_Mbphone_w6_2)
res_oOwn_Mbphone_w7_2<-update(res_oOwn_Mbphone_w4,.~.,data=AfrodatAllN[AfrodatAllN$wave==7,])
	summary(res_oOwn_Mbphone_w7_2)

mtable(res_oOwn_Mbphone_2,res_oOwn_Mbphone_w4_2,res_oOwn_Mbphone_w5_2,res_oOwn_Mbphone_w6_2,res_oOwn_Mbphone_w7_2,
	summary.stats=c("p","N","AIC"))


#---
names(AfrodatAllNg)
summary(AfrodatAll)

cor(dat<-Afrodat6[,c("gone_food","gone_water","gone_med","gone_cash","gone_fuel","Electric_connection","Mem_religious","Mem_voluntary","Cit_action_Attend_meeting","Cit_action_raise_issue","Ele_campaign_rally","Ele_campaign_meeting","Ele_Attend_persuade","Ele_Attend_Work")],use="complete")

factanal(dat[complete.cases(dat),],2,rotation="promax")
factanal(dat[complete.cases(dat),],3,rotation="promax")
factanal(dat[complete.cases(dat),],4,rotation="promax")
factanal(dat[complete.cases(dat),],5,rotation="promax")




Model.cfa0<- '
	f1lack_food=~gone_food +gone_water+gone_med+gone_cash+gone_fuel #
	f2connect_Elec=~Electric_connection #
	f3community=~Mem_religious+Mem_voluntary #
	f4action=~Cit_action_Attend_meeting +Cit_action_raise_issue #
	f5Election=~Ele_campaign_rally +Ele_campaign_meeting+Ele_Attend_persuade+ Ele_Attend_Work#
	'
res.cfa0<-lavaan(Model.cfa0, data=Afrodat6,auto.var=TRUE,  auto.fix.first=TRUE,auto.fix.single=T,auto.cov.lv.x=TRUE)
	summary(res.cfa0, fit.measures=TRUE)		
	standardizedSolution(res.cfa0, type = "std.all")

#多母集団
res.cfa0g<-lavaan(Model.cfa0, data=Afrodat6,auto.var=TRUE,  auto.fix.first=TRUE,auto.fix.single=T,auto.cov.lv.x=TRUE,group="COUNTRY2")
	summary(res.cfa0g, fit.measures=TRUE)		












#-------Round 6で遊ぶ
#国別に集計してみる
Afrodat6g<-group_by(Afrodat6,Afrodat6$COUNTRY2)

m<-summarise(Afrodat6g,mNews_Radio=mean2(News_Radio),mNews_Television=mean2(News_Television),mNews_Newspaper=mean2(News_Newspaper),mNews_Internet=mean2(News_Internet),mNews_Social_media=mean2(News_Social_media),
mOwn_Radio=mean2(Own_Radio), mOwn_TV=mean2(Own_TV), mOwn_Auto=mean2(Own_Auto), mOwn_Mbphone=mean2(Own_Mbphone))
as.data.frame(m)
#edit(as.data.frame(m))


#"dlang_English"                   "dlang_French"                    "dlang_Portuguese"               "dlang_Swahili"                   "dlang_Arabic"                    "dlang_Afrikaans"                 "dlang_Chichewa"                 "dlang_Akan"                      "dlang_Other"                     "dlang_Egyptian_Arabic"           "dlang_Crioulo"                   "dlang_Kirund"                    "dlang_Sesotho"                   "dlang_Sudanese_Arabic"           "dlang_Creole"                   "dlang_siSwati"                   "dlang_Shona"                     "dlang_Algerian_Arabic" 

#"dOccupation_Never"               "dOccupation_Student"            "dOccupation_Housewife_homemaker" "dOccupation_primary"             "dOccupation_Trader"              "dOccupation_Retail"              "dOccupation_Unskilled"           "dOccupation_skilled"             "dOccupation_Clerical"            "dOccupation_Supervisor"         "dOccupation_police"              "dOccupation_Mid_level"           "dOccupation_Upper_level"         "dOccupation_Other" 

#"dRace_BAf"                       "dRace_Wh"                        "dRace_Col"                       "dRace_Arab"                      "dRace_SAs"                       "dRace_EAs"                       "dRace_Oth"                      


#単純にfactorだと変数が多い
#res<-glm(Own_TV~Age+Gender_f+Education+as.factor(Employment_status)+as.factor(Race)+as.factor(Occupation)+Mem_religious+Mem_voluntary+gone_food+gone_cash+as.factor(Language)+COUNTRY2,family="binomial",data=Afrodat6)
	summary(res)

table2(Afrodat6$Mem_religious,Afrodat6$Mem_voluntary,exclude=NULL)
#        0    1    2    3 <NA>   %     N
#0    78.6  8.8  9.4  3.0  0.2 100 28141
#1    49.8 34.1 12.4  3.2  0.5 100  8986
#2    46.2 11.7 35.9  5.6  0.6 100 13739
#3    37.9  8.5 26.7 26.4  0.5 100  2718
#<NA> 11.7  3.4  5.7  1.7 77.5 100   351
#     63.1 13.7 17.5  4.9  0.9 100 53935


table2(Afrodat6$Own_Radio,Afrodat6$News_Radio,exclude=NULL)
#radioなしでもニュースソースにしている者も
#        0   1   2    3    4 <NA>   %     N
#0    45.5 5.9 9.8 19.7 18.8  0.2 100 15825
#1     7.0 2.5 5.9 22.9 61.5  0.1 100 38038
#<NA> 13.9 4.2 8.3 16.7 44.4 12.5 100    72
#     18.3 3.5 7.1 21.9 49.0  0.2 100 53935
table2(Afrodat6$Own_TV,Afrodat6$News_Television,exclude=NULL)
#        0   1   2    3    4 <NA>   %     N
#0    70.9 5.3 5.7  8.3  9.5  0.2 100 27034
#1     7.0 2.3 5.1 18.2 67.3  0.2 100 26812
#<NA> 37.1 5.6 3.4 10.1 29.2 14.6 100    89
#     39.1 3.8 5.4 13.2 38.3  0.2 100 53935

cor(Afrodat6[,c("Own_Auto","Own_Mbphone","Own_Radio","Own_TV","News_Radio","News_Television","News_Internet","News_Social_media")],use="complete")
	 
#ダミー変数で
v<-as.formula('~gone_food+gone_cash+
Mem_religious+Mem_voluntary+
Age+Gender_f+Education+
dEmployment_status_looking+dEmployment_status_part_time+dEmployment_status_full_time+
dOccupation_Student+dOccupation_Housewife_homemaker+dOccupation_primary+dOccupation_Trader+dOccupation_Retail+dOccupation_Unskilled+dOccupation_skilled+dOccupation_Clerical+dOccupation_Supervisor+dOccupation_police+dOccupation_Mid_level+dOccupation_Upper_level+	
dRace_BAf+dRace_Wh+dRace_Col+dRace_Arab+dRace_SAs+dRace_EAs	+
dlang_English+dlang_French+dlang_Portuguese+dlang_Swahili+dlang_Arabic+dlang_Afrikaans+dlang_Chichewa+dlang_Akan+dlang_Other+dlang_Egyptian_Arabic+dlang_Crioulo+dlang_Kirund+dlang_Sesotho+dlang_Sudanese_Arabic+dlang_Creole+dlang_siSwati+dlang_Shona+dlang_Algerian_Arabic+
dCOUNTRY_ALG+dCOUNTRY_BDI+dCOUNTRY_BFO+dCOUNTRY_CAM+dCOUNTRY_CDI+dCOUNTRY_EGY+dCOUNTRY_GAB+dCOUNTRY_GHA+dCOUNTRY_GUI+dCOUNTRY_KEN+dCOUNTRY_LES+dCOUNTRY_LIB+dCOUNTRY_MAD+dCOUNTRY_MAU+dCOUNTRY_MLI+dCOUNTRY_MLW+dCOUNTRY_MOR+dCOUNTRY_MOZ+dCOUNTRY_NAM+dCOUNTRY_NGR+dCOUNTRY_NIG+dCOUNTRY_SAF+dCOUNTRY_SEN+dCOUNTRY_SRL+dCOUNTRY_STP+dCOUNTRY_SUD+dCOUNTRY_SWZ+dCOUNTRY_TAN+dCOUNTRY_TOG+dCOUNTRY_TUN+dCOUNTRY_UGA+dCOUNTRY_ZAM')
##dOccupation_Never+ dOccupation_Other +dRace_Oth 



res_oAuto<-glm(formula(paste("Own_Auto~",v,sep="")[2]),family="binomial",data=Afrodat6)
	summary(res_oAuto)
res_oMbphone<-glm(formula(paste("Own_Mbphone~",v,sep="")[2]),family="binomial",data=Afrodat6)
	summary(res_oMbphone)

res_oRadio<-glm(formula(paste("Own_Radio~",v,sep="")[2]),family="binomial",data=Afrodat6)
	summary(res_oRadio)
res_nRadio<-lm(formula(paste("News_Radio~",v,sep="")[2]),data=Afrodat6)
	summary(res_nRadio)

res_oTelevision<-glm(formula(paste("Own_TV~",v,sep="")[2]),family="binomial",data=Afrodat6)
	summary(res_oTelevision)
res_nTelevision<-lm(formula(paste("News_Television~",v,sep="")[2]),data=Afrodat6)
	summary(res_nTelevision)

res_nInternet<-lm(formula(paste("News_Internet~",v,sep="")[2]),data=Afrodat6)
	summary(res_nInternet)
res_nSocial_media<-lm(formula(paste("News_Social_media~",v,sep="")[2]),data=Afrodat6)
	summary(res_nSocial_media)

mtable(res_oAuto,res_oMbphone,res_oRadio,res_nRadio,res_oTelevision,res_nTelevision,res_nInternet,res_nSocial_media)


#---
cor(dat<-Afrodat6[,c("gone_food","gone_water","gone_med","gone_cash","gone_fuel","Electric_connection","Mem_religious","Mem_voluntary","Cit_action_Attend_meeting","Cit_action_raise_issue","Ele_campaign_rally","Ele_campaign_meeting","Ele_Attend_persuade","Ele_Attend_Work")],use="complete")

factanal(dat[complete.cases(dat),],2,rotation="promax")
factanal(dat[complete.cases(dat),],3,rotation="promax")
factanal(dat[complete.cases(dat),],4,rotation="promax")
factanal(dat[complete.cases(dat),],5,rotation="promax")

#                          Factor1 Factor2 Factor3 Factor4 Factor5
#gone_food                  0.637                          -0.132 
#gone_water                 0.657                                 
#gone_med                   0.742                                 
#gone_cash                  0.594                          -0.280 
#gone_fuel                  0.679                           0.171 
#Electric_connection       -0.132                           0.547 
#Mem_religious                                      0.555  -0.106 
#Mem_voluntary                                      0.583         
#Cit_action_Attend_meeting                  0.822                 
#Cit_action_raise_issue                     0.753                 
#Ele_campaign_rally                 0.658                  -0.116 
#Ele_campaign_meeting               0.773                         
#Ele_Attend_persuade                0.638                         
#Ele_Attend_Work                    0.664                         
#
#               Factor1 Factor2 Factor3 Factor4 Factor5
#SS loadings      2.225   1.882   1.268   0.689   0.474
#Proportion Var   0.159   0.134   0.091   0.049   0.034
#Cumulative Var   0.159   0.293   0.384   0.433   0.467
#
#Factor Correlations:
#        Factor1 Factor2 Factor3 Factor4 Factor5
#Factor1   1.000 -0.1270  0.3666  0.5700  0.2879
#Factor2  -0.127  1.0000 -0.0769 -0.0925 -0.3653
#Factor3   0.367 -0.0769  1.0000  0.3790  0.0651
#Factor4   0.570 -0.0925  0.3790  1.0000  0.1447
#Factor5   0.288 -0.3653  0.0651  0.1447  1.0000
#
#Test of the hypothesis that 5 factors are sufficient.
#The chi square statistic is 2846.64 on 31 degrees of freedom.
#The p-value is 0 


Model.cfa0<- '
	f1lack_food=~gone_food +gone_water+gone_med+gone_cash+gone_fuel #
	f2connect_Elec=~Electric_connection #
	f3community=~Mem_religious+Mem_voluntary #
	f4action=~Cit_action_Attend_meeting +Cit_action_raise_issue #
	f5Election=~Ele_campaign_rally +Ele_campaign_meeting+Ele_Attend_persuade+ Ele_Attend_Work#
	'
res.cfa0<-lavaan(Model.cfa0, data=Afrodat6,auto.var=TRUE,  auto.fix.first=TRUE,auto.fix.single=T,auto.cov.lv.x=TRUE)
	summary(res.cfa0, fit.measures=TRUE)		
	standardizedSolution(res.cfa0, type = "std.all")

#多母集団
res.cfa0g<-lavaan(Model.cfa0, data=Afrodat6,auto.var=TRUE,  auto.fix.first=TRUE,auto.fix.single=T,auto.cov.lv.x=TRUE,group="COUNTRY2")
	summary(res.cfa0g, fit.measures=TRUE)		
#	standardizedSolution(res.cfa0g, type = "std.all")	#時間がかかるのでやめる
#lavaan 0.6-5 ended normally after 2234 iterations
#
#  Estimator                                         ML
#  Optimization method                           NLMINB
#  Number of free parameters                       1332
#                                                      
#  Number of observations per group:               Used       Total
#    ALG                                            962        1200
#    BDI                                            845        1200
#    BEN                                            862        1200
#    BFO                                            769        1200
#    BOT                                            967        1200
#    CAM                                            743        1182
#    CDI                                            817        1199
#    CVE                                           1058        1200
#    EGY                                           1044        1198
#    GAB                                            929        1198
#    GHA                                           2004        2400
#    GUI                                            750        1200
#    KEN                                           2140        2397
#    LES                                            602        1200
#    LIB                                            884        1199
#    MAD                                           1030        1200
#    MAU                                           1132        1200
#    MLI                                            922        1200
#    MLW                                            871        2400
#    MOR                                            995        1200
#    MOZ                                           1724        2400
#    NAM                                           1108        1200
#    NGR                                            886        1200
#    NIG                                           2020        2400
#    SAF                                           2153        2390
#    SEN                                            831        1200
#    SRL                                            877        1191
#    STP                                            989        1196
#    SUD                                            904        1200
#    SWZ                                            983        1200
#    TAN                                           1739        2386
#    TOG                                            771        1200
#    TUN                                           1078        1200
#    UGA                                           2011        2400
#    ZAM                                           1015        1199
#    ZIM                                           2083        2400
#                                                                  
#Model Test User Model:
#                                                        
#  Test statistic                              129170.680
#  Degrees of freedom                                2952
#  P-value (Chi-square)                             0.000
#  Test statistic for each group:
#    ALG                                       3637.671
#    BDI                                       2853.361
#    BEN                                       2697.105
#    BFO                                       2287.498
#    BOT                                       2645.085
#    CAM                                       2220.190
#    CDI                                       2669.851
#    CVE                                       3377.988
#    EGY                                       4457.356
#    GAB                                       3140.012
#    GHA                                       5091.285
#    GUI                                       2596.040
#    KEN                                       5875.835
#    LES                                       1615.200
#    LIB                                       2388.104
#    MAD                                       2671.431
#    MAU                                       6738.084
#    MLI                                       2489.523
#    MLW                                       2665.149
#    MOR                                       4941.983
#    MOZ                                       4352.193
#    NAM                                       2730.346
#    NGR                                       2770.312
#    NIG                                       5620.855
#    SAF                                       6504.080
#    SEN                                       2660.956
#    SRL                                       2405.757
#    STP                                       3083.422
#    SUD                                       2422.518
#    SWZ                                       2540.061
#    TAN                                       4846.536
#    TOG                                       2686.043
#    TUN                                       3620.984
#    UGA                                       5987.098
#    ZAM                                       2712.112
#    ZIM                                       7168.656
#
#Model Test Baseline Model:
#
#  Test statistic                            157501.037
#  Degrees of freedom                              3276
#  P-value                                        0.000
#
#User Model versus Baseline Model:
#
#  Comparative Fit Index (CFI)                    0.182
#  Tucker-Lewis Index (TLI)                       0.092
#
#Loglikelihood and Information Criteria:
#
#  Loglikelihood user model (H0)            -659189.336
#  Loglikelihood unrestricted model (H1)             NA
#                                                      
#  Akaike (AIC)                             1321042.672
#  Bayesian (BIC)                           1332542.362
#  Sample-size adjusted Bayesian (BIC)      1328309.258
#
#Root Mean Square Error of Approximation:
#
#  RMSEA                                          0.193
#  90 Percent confidence interval - lower         0.192
#  90 Percent confidence interval - upper         0.193
#  P-value RMSEA <= 0.05                          0.000
#
#Standardized Root Mean Square Residual:
#
#  SRMR                                           1.806
#
#Parameter Estimates:
#
#  Information                                 Expected
#  Information saturated (h1) model          Structured
#  Standard errors                             Standard


res.cfa0gmi<-measurementInvariance(model=Model.cfa0, data=Afrodat6,group="COUNTRY2")

#Measurement invariance models:#
#Chi-Squared Difference Test
#                 Df     AIC     BIC Chisq Chisq diff Df diff Pr(>Chisq)    
#fit.configural 2448 1205049 1220900 12169                                  
#fit.loadings   2763 1210476 1223608 18226       6057     315  < 2.2e-16 ***
#fit.intercepts 3078 1233377 1243789 41758      23531     315  < 2.2e-16 ***
#fit.means      3253 1272171 1281072 80901      39144     175  < 2.2e-16 ***
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Fit measures:
#                 cfi rmsea cfi.delta rmsea.delta
#fit.configural 0.937 0.059        NA          NA
#fit.loadings   0.900 0.070     0.037       0.011
#fit.intercepts 0.749 0.104     0.151       0.035
#fit.means      0.497 0.144     0.253       0.039



#----
attach(Afrodat6,warn=F)
Afrodat6$f1lack_food<-(gone_food +gone_water+gone_med+gone_cash+gone_fuel)/5 #
Afrodat6$f2connect_Elec<-Electric_connection #
Afrodat6$f3community<-(Mem_religious+Mem_voluntary )/2#
Afrodat6$f4action<-(Cit_action_Attend_meeting +Cit_action_raise_issue)/2 #
Afrodat6$f5Election<-(Ele_campaign_rally +Ele_campaign_meeting+Ele_Attend_persuade+ Ele_Attend_Work)/4 	#

	summary(Afrodat6)	#  f4action は　　NA　11373サンプル

#ダミー変数と蒸気で定義した変数で
v<-as.formula('~f1lack_food+f2connect_Elec+
f3community+f4action+
Age+Gender_f+Education+
dEmployment_status_looking+dEmployment_status_part_time+dEmployment_status_full_time+
dOccupation_Student+dOccupation_Housewife_homemaker+dOccupation_primary+dOccupation_Trader+dOccupation_Retail+dOccupation_Unskilled+dOccupation_skilled+dOccupation_Clerical+dOccupation_Supervisor+dOccupation_police+dOccupation_Mid_level+dOccupation_Upper_level+	
dRace_BAf+dRace_Wh+dRace_Col+dRace_Arab+dRace_SAs+dRace_EAs	+
dlang_English+dlang_French+dlang_Portuguese+dlang_Other+dlang_Crioulo+dlang_Kirund+dlang_Sesotho+dlang_Sudanese_Arabic+dlang_Creole+dlang_siSwati+dlang_Shona+dlang_Algerian_Arabic+
dCOUNTRY_ALG+dCOUNTRY_BDI+dCOUNTRY_BFO+dCOUNTRY_CAM+dCOUNTRY_CDI+dCOUNTRY_EGY+dCOUNTRY_GAB+dCOUNTRY_GHA+dCOUNTRY_GUI+dCOUNTRY_KEN+dCOUNTRY_LES+dCOUNTRY_LIB+dCOUNTRY_MAD+dCOUNTRY_MAU+dCOUNTRY_MLI+dCOUNTRY_MLW+dCOUNTRY_MOR+dCOUNTRY_MOZ+dCOUNTRY_NAM+dCOUNTRY_NGR+dCOUNTRY_NIG+dCOUNTRY_SAF+dCOUNTRY_SEN+dCOUNTRY_SRL+dCOUNTRY_STP+dCOUNTRY_SUD+dCOUNTRY_SWZ+dCOUNTRY_TAN+dCOUNTRY_TOG+dCOUNTRY_TUN+dCOUNTRY_UGA+dCOUNTRY_ZAM')
##dOccupation_Never+ dOccupation_Other +dRace_Oth 



res_oAuto<-glm(formula(paste("Own_Auto~",v,sep="")[2]),family="binomial",data=Afrodat6)
	summary(res_oAuto)
res_oMbphone<-glm(formula(paste("Own_Mbphone~",v,sep="")[2]),family="binomial",data=Afrodat6)
	summary(res_oMbphone)

res_oRadio<-glm(formula(paste("Own_Radio~",v,sep="")[2]),family="binomial",data=Afrodat6)
	summary(res_oRadio)
res_nRadio<-lm(formula(paste("News_Radio~",v,sep="")[2]),data=Afrodat6)
	summary(res_nRadio)

res_oTelevision<-glm(formula(paste("Own_TV~",v,sep="")[2]),family="binomial",data=Afrodat6)
	summary(res_oTelevision)
res_nTelevision<-lm(formula(paste("News_Television~",v,sep="")[2]),data=Afrodat6)
	summary(res_nTelevision)

res_nInternet<-lm(formula(paste("News_Internet~",v,sep="")[2]),data=Afrodat6)
	summary(res_nInternet)
res_nSocial_media<-lm(formula(paste("News_Social_media~",v,sep="")[2]),data=Afrodat6)
	summary(res_nSocial_media)

mtable(res_oAuto,res_oMbphone,res_oRadio,res_nRadio,res_oTelevision,res_nTelevision,res_nInternet,res_nSocial_media)










#ーーーーーーー保有状況でクラスタ
set.seed(12345)
d<-complete.cases(Afrodat6[,c("Own_Auto","Own_Mbphone","Own_Radio","Own_TV","News_Radio","News_Television","News_Internet","News_Social_media")])
	sum(d);dim(Afrodat6)[1]	#52496  53935
ocl3<-kmeans(Afrodat6[d,c("Own_Auto","Own_Mbphone","Own_Radio","Own_TV")],3)
	ocl3$size
	ocl3$centers

ocl4<-kmeans(Afrodat6[d,c("Own_Auto","Own_Mbphone","Own_Radio","Own_TV")],4)
	ocl4$size	# 18870  9615  1639 22372
	ocl4$centers
data.frame(ocl4$centers,ocl4$size)
#    Own_Auto Own_Mbphone Own_Radio    Own_TV ocl4.size
#1 0.56550079           1 0.5002650 0.6043455     18870	(4)全体的に高い
#2 0.02953718           0 0.4284971 0.0000000      9615	(1)ラジオ
#3 0.12751678           0 0.7156803 1.0000000      1639	(3)ラジオ､TV､自動車
#4 0.00000000           1 1.0000000 0.5910066     22372	(2)携帯､ラジオ､TV

ocl5<-kmeans(Afrodat6[d,c("Own_Auto","Own_Mbphone","Own_Radio","Own_TV")],5)
	data.frame(ocl5$centers,ocl5$size)
#    Own_Auto Own_Mbphone Own_Radio     Own_TV ocl5.size
#1 0.02054329   0.0000000 0.0000000 0.06706282      5890	(1)全体的に低い
#2 0.01228335   0.6895252 1.0000000 0.00000000     13270	(2)携帯とラジオ
#3 0.00000000   1.0000000 0.6172448 0.74230895     21421	(3)携帯とラジオ､TV
#4 0.16800643   0.0000000 0.9429260 1.00000000      1244	(4)クルマ､ラジオ､TV
#5 1.00000000   1.0000000 0.8846406 0.81763659     10671	(5)全体的に高い

#ニュースソース
ucl3<-kmeans(Afrodat6[d,c("News_Radio","News_Television","News_Internet","News_Social_media")],3)
	ucl3$size
	ucl3$centers

ucl4<-kmeans(Afrodat6[d,c("News_Radio","News_Television","News_Internet","News_Social_media")],4)
	ucl4$size
	ucl4$centers

cor(Afrodat6[d,c("News_Radio","News_Television","News_Internet","News_Social_media")])
ucl5<-kmeans(Afrodat6[d,c("News_Radio","News_Television","News_Internet","News_Social_media")],5)
data.frame(ucl5$centers,ucl5$size)
#  News_Radio News_Television News_Internet News_Social_media ucl5.size
#1  0.1755152      1.05501979     0.1099381        0.08750381      9851	(1)全体的に利用せず
#2  3.5655794      3.51169832     0.1138866        0.05229838     14532	(3)ラジオ､TVのみ
#3  3.4360568      3.49609236     2.5367673        2.28952043      5630	(4)利用するが　ネットは若干低い
#4  2.9646811      3.58895625     3.8278861        3.85859251      7588	(5)全体的に利用
#5  3.4563948      0.09405841     0.0537093        0.05176234     14895	(2)ラジオのみ

dat<-data.frame(ocl5$cluster,ucl5$cluster)
names(dat)<-c("own","usage")
	dat$usage2<-ifelse(dat$usage==2,3,	#usageの番号うちなおし
				ifelse(dat$usage==3,4,
				ifelse(dat$usage==4,5,
				ifelse(dat$usage==5,2,dat$usage))))
				
	table(dat$own,dat$usage2,exclude=NULL)
	table2(dat$own,dat$usage2,exclude=NULL)
#     1    2    3    4    5   %     N
#1 60.6 27.3  8.8  1.4  1.8 100  5890
#2  7.3 70.8 14.3  4.1  3.5 100 13270
#3 19.5 10.7 36.3 14.4 19.2 100 21421
#4 12.5  9.5 65.5  8.0  4.5 100  1244
#5  9.1 14.0 33.1 17.1 26.7 100 10671
#  18.8 28.4 27.7 10.7 14.5 100 52496





#multivariate probit  #処理遅いので
#resmP<-mvProbit(formula(paste("cbind(Own_Auto,Own_Mbphone,Own_Radio,Own_TV)~",v,sep="")[2]),data=Afrodat6[d,])
#resmP<-mvProbit(cbind(Own_Auto,Own_Mbphone,Own_Radio,Own_TV)~gone_food+gone_cash+Mem_religious+Mem_voluntary+Age+Gender_f+Education+dEmployment_status_looking+dEmployment_status_part_time+dEmployment_status_full_time+dOccupation_Never+dOccupation_Student+dOccupation_Housewife_homemaker+dOccupation_primary+dOccupation_Trader+dOccupation_Retail+dOccupation_Unskilled+dOccupation_skilled+dOccupation_Clerical+dOccupation_Supervisor+dOccupation_police+dOccupation_Mid_level+dOccupation_Upper_level+	dRace_BAf+dRace_Wh+dRace_Col+dRace_Arab+dRace_SAs+dRace_EAs	+dlang_English+dlang_French+dlang_Portuguese+dlang_Swahili+dlang_Arabic+dlang_Afrikaans+dlang_Chichewa+dlang_Akan+dlang_Other+dlang_Crioulo+dlang_Kirund+dlang_Sesotho+dlang_Sudanese_Arabic+dlang_Creole+dlang_siSwati+dlang_Shona+dlang_Algerian_Arabic+dCOUNTRY_ALG+dCOUNTRY_BDI+dCOUNTRY_BFO+dCOUNTRY_CAM+dCOUNTRY_CDI+dCOUNTRY_EGY+dCOUNTRY_GAB+dCOUNTRY_GHA+dCOUNTRY_GUI+dCOUNTRY_KEN+dCOUNTRY_LES+dCOUNTRY_LIB+dCOUNTRY_MAD+dCOUNTRY_MAU+dCOUNTRY_MLI+dCOUNTRY_MLW+dCOUNTRY_MOR+dCOUNTRY_MOZ+dCOUNTRY_NAM+dCOUNTRY_NGR+dCOUNTRY_NIG+dCOUNTRY_SAF+dCOUNTRY_SEN+dCOUNTRY_SRL+dCOUNTRY_STP+dCOUNTRY_SUD+dCOUNTRY_SWZ+dCOUNTRY_TAN+dCOUNTRY_TOG+dCOUNTRY_TUN+dCOUNTRY_UGA+dCOUNTRY_ZAM,data=Afrodat6[d,])	#dlang_Egyptian_Arabic+	とdCOUNTRY_EGYが相関
#resmP

#---------------
#			7
#---------------
Afrodat7 <- read_sav("r7_merged_data_34ctry.release.sav")
#View(Afrodat7)
summary(Afrodat7)
names(Afrodat7)	
dim(Afrodat7)	#[1] 53935   364
	Afrodat7$wave<-7
	Afrodat7$year<-2018

table(Afrodat7$COUNTRY)
Afrodat7$COUNTRY2<-substr(Afrodat7$RESPNO,1,3)	#はじめの3文字が国名
	table(Afrodat7$COUNTRY2)
# BEN  BFO  BOT  CAM  CDI  CVE  GAB  GAM  GHA  GUI  KEN  LES  LIB  MAD  MAU  MLI  MLW  MOR  MOZ  NAM  NGR  NIG  SAF  SEN  SRL  STP  SUD  SWZ  TAN  TOG  TUN  UGA  ZAM  ZIM 
#1200 1200 1198 1202 1200 1200 1199 1200 2400 1194 1599 1200 1200 1200 1200 1200 1200 1200 2392 1200 1200 1600 1840 1200 1200 1200 1200 1200 2400 1200 1199 1200 1200 1200 


#2019/11/現在　codebookがないので､ボツワナの調査票を参照　https://www.afrobarometer.org/sites/default/files/questionnaires/Round%207/bot_r7_questionnaire_062018.pdf
#これを使って欠損値処理
#12. How often do you get news from the following sources? 
#A.Radio
#B. Television
#C. Newspapers
#D.Internet
#E. Social media such as Facebook or Twitter

Afrodat7$News_Radio<-repNA04(Afrodat7$Q12A)
Afrodat7$News_Television<-repNA04(Afrodat7$Q12B)
Afrodat7$News_Newspaper<-repNA04(Afrodat7$Q12C)
Afrodat7$News_Internet<-repNA04(Afrodat7$Q12D)
Afrodat7$News_Social_media<-repNA04(Afrodat7$Q12E)

#保存
save(Afrodat7,file=“0Afrodat7.rda”)







#------全ラウンド必要部分をまとめる　　とりあえずニュースの利用
v<-c("wave","year","COUNTRY2","News_Radio","News_Television","News_Newspaper","News_Internet","News_Social_media")
AfrodatAll<-rbind(Afrodat1[,v],Afrodat2[,v],Afrodat3[,v],Afrodat4[,v],Afrodat5[,v],Afrodat6[,v],Afrodat7[,v])
	dim(AfrodatAll)	#[1] 204464      8

table(AfrodatAll$COUNTRY2,AfrodatAll$wave,exclude=NULL)
#AfrodatAllg<-group_by(AfrodatAll,c("year","COUNTRY2"))
AfrodatAllg<-group_by(AfrodatAll, AfrodatAll$COUNTRY2,AfrodatAll$year)	#国､年別に集計することを指定

(m<-as.data.frame(summarise(AfrodatAllg,mNews_Radio<-mean2(News_Radio),mNews_Television<-mean2(News_Television),mNews_Newspaper<-mean2(News_Newspaper),mNews_Internet<-mean2(News_Internet),mNews_Social_media<-mean2(News_Social_media))))
names(m)<-c("COUNTRY2","year","mNews_Radio","mNews_Television","mNews_Newspaper","mNews_Internet","mNews_Social_media")
edit(m)

m2<-m
	dd<-rep(NA,dim(m)[2])
m2<-rbind(dd,m2)		#1行下にずらす
m2<-m2[-dim(m2)[1],]	#最後の行は除く
	edit(m2)
names(m2)<-paste("l",names(m2),sep="_")
m2<-cbind(m,m2)

m2$l_COUNTRY2[1]<-""	#NAだと次の処理がエラーになるので
m2[m2$COUNTRY2!=m2$l_COUNTRY2,c("l_COUNTRY2","l_year","l_mNews_Radio","l_mNews_Television","l_mNews_Newspaper","l_mNews_Internet","l_mNews_Social_media")]<-NA
	edit(m2)

dim(AfrodatAll)
AfrodatAll2<-merge(AfrodatAll,m2,by=c("COUNTRY2","year"))	#各国の平均をつける
dim(AfrodatAll2)
names(AfrodatAll2)
	save(AfrodatAll2,file="0AfrodatAll2.rda")


#-------
#a<-xyplot(News_Radio~year|COUNTRY2,data=AfrodatAll,
#           panel=function(x,y){
#                      panel.xyplot(x,y)
#                      panel.abline(lsfit(x,y))
#					  }
#		)
#
#b<-xyplot(News_Television~year|COUNTRY2,data=AfrodatAll,
#           panel=function(x,y){
#                      panel.xyplot(x,y)
#                      panel.abline(lsfit(x,y))
#					  }
#		)
#plot(a+b)


#----メディア間の比較 時系列プロット
plot_media_usage<-function(dat0,country_nam,...){
#datの中にCOUNTRY2
dat<-dat0[dat0$COUNTRY2==country_nam,]
	print(list(country_nam,dim(dat)))
plot(dat$year,dat$mNews_Radio,type="l",xlim=c(2000,2018),ylim=c(0,4),...)	#ylab="Usage as News Source"
	lines(dat$year,dat$mNews_Television,type="l",col="Red")
	lines(dat$year,dat$mNews_Newspaper,type="l",col="Purple")
	lines(dat$year,dat$mNews_Internet,type="l",col="Blue")
	lines(dat$year,dat$mNews_Social_media,type="l",col="Green")
#	legend("topleft",c(dat$COUNTRY2[1],"Radio","TV","Newspaper","Internet","Social_media"),text.col=c("Black","Black","Red","Purple","Blue","Green"))
	legend("topleft",c(country_nam))	#,"Radio","TV","Newspaper","Internet","Social_media"),text.col=c("Black","Black","Red","Purple","Blue","Green"))
}

(cnam<-unique(m2$COUNTRY2))
length(cnam)	#37
	par(mar=c(0.,0.,0.,0.),mfrow=c(5,8))	#c(0.5,0.5,0.5,0.5)
	for(i in seq(1,length(cnam))){
		plot_media_usage(m2,cnam[i],xaxt="n",yaxt="n")	#軸の目盛り消す
		}

#6wave 以上ある国のみ
(d<-as.data.frame(table(m2$COUNTRY2)))
sum(d$Freq>=6)	#[1] 12
	cnam<-as.character(d[,1])
	par(mar=c(0.,0.,0.,0.),mfrow=c(4,4))	#c(0.5,0.5,0.5,0.5)
	jj<-0
	for(ii in seq(1,length(cnam))){
		if(d$Freq[ii]>=6){
			if(jj==1){
					plot_media_usage(m2,cnam[ii])	#軸の目盛り消さず
				}else{
					plot_media_usage(m2,cnam[ii],xaxt="n",yaxt="n")	#軸の目盛り消す
					}
		if(jj==1){
		legend("bottomleft",c("Radio","TV","Newspaper","Internet","Social_media"),text.col=c("Black","Red","Purple","Blue","Green"))
			}
			jj<-jj+1
		}
		}


#----
res0<-lme(News_Radio~1,random=~1|COUNTRY2,na.action=na.omit,data=AfrodatAll2)

AfrodatAll2g<-groupedData(News_Radio~year|COUNTRY2,data=AfrodatAll2)
	plot(AfrodatAll2g)
	reslm.g<-lmList(News_Radio~year|COUNTRY2,data=AfrodatAll2,na.action=na.omit)
	summary(reslm.g)
	plot(intervals(reslm.g))


	dat0<-AfrodatAll2[AfrodatAll2$COUNTRY2=="BEN",]
	table(dat0$year,dat0$News_Internet,exclude=NULL)	#	2012=wave5		それぞれ測定前については0にしてダミー定義
	table(dat0$year,dat0$News_Social_media,exclude=NULL)	#2014= wave 6
		dat0$News_Internet[dat0$year<2012]<-0	
			dat0$fg.News_Internet<-ifelse(dat0$year<2012,0,1)		
		dat0$News_Social_media[dat0$year<2014]<-0
			dat0$fg.News_Social_media<-ifelse(dat0$year<2014,0,1)		
	
	d<-complete.cases(dat0[,c("year","News_Radio","News_Television","News_Newspaper","News_Internet","News_Social_media")])
	dat<-dat0[d,];dim(dat0);dim(dat)
	summary(dat)
res<-lm(News_Radio~year,data=dat)
	summary(res)
	plot(News_Radio~year,data=dat)
	lines(res$fitted.values~year,col="red",data=dat)

res2<-lm(News_Radio~year+News_Television+News_Newspaper+News_Internet+News_Social_media+fg.News_Internet+fg.News_Social_media,data=dat)
	summary(res2)
	plot(News_Radio~year,data=dat)
	lines(res2$fitted.values~year,col="red",data=dat)



library(ICC)
ICCest(as.factor(COUNTRY2),News_Radio,data=AfrodatAll2[AfrodatAll2$wave==1,],alpha=0.05,CI.type="Smith")
#[1] 0.0517132と低いが


#国による違いの有無､
res0<-lme(News_Radio~1,random=~1|COUNTRY2,na.action=na.omit,data=AfrodatAll2)
	summary(res0)
	VarCorr(res0)
#        AIC       BIC   logLik
#  884816.3 884847.6 -442405.2

#(Intercept) 2.735488 0.07191879 250250 38.0358       0

#            Variance   StdDev   
#(Intercept) 0.1909259 0.4369507
#Residual    2.0192435 1.4210009


res1<-update(res0,.~.+year)	#時間に比例､ランダム切片
	summary(res1)
#        AIC       BIC   logLik
#  883515.7 883557.4 -441753.9
#               Value Std.Error     DF   t-value p-value
#(Intercept) 42.56237 0.15894351 250249  267.7830       0
#year        -0.01980 0.00007129 250249 -277.6865       0

res2<-update(res1,.~.,random=~year|COUNTRY2)	#傾きランダム
	summary(res2)
#        AIC     BIC   logLik
#  883515.1 883577.7 -441751.6
#               Value Std.Error     DF   t-value p-value
#(Intercept) 42.76453 1.0962271 249800  39.01065       0
#year        -0.01990 0.0005459 249800 -36.44408       0

res2c<-update(res2,.~.,correlation=corAR1())	#系列相関 このデータは個人が同一ではないのでできない
	summary(res2c)
#Error: 'sumLenSq := sum(table(groups)^2)' = 2.37085e+09 is too large.
# Too large or no groups in your correlation structure?

res2.1<-update(res2,.~.+l_mNews_Radio+News_Television,random=~year|COUNTRY2)	
	summary(res2.1)


#--TV
res0tv<-update(res0,News_Television~.)
	summary(res0tv)
#       AIC      BIC    logLik
#  915710.2 915741.5 -457852.1

res1tv<-update(res1,News_Television~.)
	summary(res1tv)
#       AIC      BIC    logLik
#  914765.4 914807.1 -457378.7
#                Value Std.Error     DF   t-value p-value
#(Intercept) -35.67483 1.2268400 247074 -29.07863       0
#year          0.01874 0.0006042 247074  31.01131       0

res2tv<-update(res2,News_Television~.)
	summary(res2tv)
#       AIC      BIC    logLik
#  914766.6 914829.1 -457377.3




#-------------------------------
dwaw.line2<-function(dat){		#datの中にあるtを横軸､yを縦軸にとってプロット｡左端に国記号　dat$t:year, dat$y, dat$cnam: country name
	d<-dim(dat)[1]
	lines(dat$t,dat$y)
		text(dat$t[d],dat$y[d],dat$cnam[d],pos=4,cex=0.5)		#最後のtのところにcountry nameを
	}

#datの中にあるcnam毎にトレンドをプロット
group_trend_plot<-function(dat,lab){
par(mfrow=c(1,1))
	ymin<-min2(dat$y)
	ymax<-max2(dat$y)
	tmax<-max2(dat$t)
	tmin<-min2(dat$t)
	plot(dat$t,dat$y,xlim=c(tmin,tmax),ylim=c(ymin,ymax),main=lab,xlab="Year",ylab="Mean")
		by(dat,dat$cnam,dwaw.line2)
	}

head(m)
#Radio
dat<-m[,c(1:3)];names(dat)<-c("cnam","t","y")	#40-90%   w3から大きくはかわらず｡　w7では低下の国が多い(が質問方法がかわった)
	group_trend_plot(dat,lab="Radio")
dat<-m[,c(1:2,4)];names(dat)<-c("cnam","t","y")
	group_trend_plot(dat,lab="TV")

dat<-m[,c(1:2,5)];names(dat)<-c("cnam","t","y")
	group_trend_plot(dat,lab="Newspaper")

dat<-m[,c(1:2,6)];names(dat)<-c("cnam","t","y")
	group_trend_plot(dat,lab="Internet")

dat<-m[,c(1:2,7)];names(dat)<-c("cnam","t","y")
	group_trend_plot(dat,lab="Social_media ")


#-------



save.image("0Afrodat.img")
