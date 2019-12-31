#--------
#----Afrodatae wave 1-7の処理例  読み込んで欠損値をNA(Rでの欠損値)として国別集計､回帰分析
#----2019/11/6 hamaoka@fbc.keio.ac.jp
#--------
#Rについては例えば　濱岡の下記ページ参照
#　http://news.fbc.keio.ac.jp/~hamaoka/cgi-bin/fswiki/wiki.cgi?page=R　

options(stringsAsFactors=FALSE)

#install.packages(c("dplyr","haven","memisc","openxlsx","simputation"))	#としてインストールしておく
options(width=150)
library(dplyr)	#国別集計用
library(haven)	#SPSSデータ読み込み
library(memisc)	#lmの出力整形
library(openxlsx)
library(simputation)
#ファイルのあるディレクトリ指定　　自分のAfroデータのあるところに変更  下記を書き換えるか､Rのメニューで指定
setwd("/Users/yh/Dropbox/Files2019/研究2019/AfroData")
#save.image("0Afrodat.img")
#load("0Afrodat.img");ls()
#unique(AfrodatAll$COUNTRY2)
#table(AfrodatAll$COUNTRY2)

#データ
#http://afrobarometer.org/data/merged-data
#http://afrobarometer.org/data/merged-round-6-data-36-countries-2016
#Merged Round 6 data (36 countries) (2016) (last updated: 2016)

# Round7
#https://www.afrobarometer.org/data/merged-round-7-data-34-countries-2019
#　code bookは未公開

#----------------サブルーチン
repNA01<-function(x){
	x[x<0|x>1]<-NA
	print(table(x,exclude=NULL))
	return(x)
	}
repNA02<-function(x){
	x[x<0|x>2]<-NA
	print(table(x,exclude=NULL))
	return(x)
	}
repNA03<-function(x){
	x[x<0|x>3]<-NA
	print(table(x,exclude=NULL))
	return(x)
	}
#0-4　以外をNAに　
repNA04<-function(x){
	x[x<0|x>4]<-NA
	print(table(x,exclude=NULL))
	return(x)
	}
repNA05<-function(x){
	x[x<0|x>5]<-NA
	print(table(x,exclude=NULL))
	return(x)
	}

#0-1　以外をNAに　
repNA01<-function(x){
	x[x<0|x>1]<-NA
	print(table(x,exclude=NULL))
	return(x)
	}

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


#----------1列目に国記号､行方向に各国(の平均)がはいったデータについて、国別に折れ線グラフを作成
plot.by.group<-function(x, ...){
	#x<-as.data.frame(m)
	plot(x[1,-1],type="l")#,...)
	par(srt=90,adj=0)
	text(x[1,1],x[,1],cex=2)
	for (i in seq(2,dim(x)[1])) {lines(x[i,],col=i)}
}


#---World Bankの国データから必要な時点のみ取り出す→すべて使うことにしたので使わない
toLong0<-function(dat,vnam){
	#vnam="Population, total"
	dd<-dat[dat$Series.Name==vnam,c(1,4,5,7,10,13,17,19,20)]
	names(dd);dim(dd)	#[1] 219  19
# [1] "Country.Name"  "Country.Code"  "2000.[YR2000]" "2002.[YR2002]" "2005.[YR2005]" "2008.[YR2008]" "2012.[YR2012]" "2014.[YR2014]" "2015.[YR2015]"
	names(dd)[3:9]<-c("y.2000","y.2002","y.2005","y.2008","y.2012","y.2014","y.2015")
#	print(head(dd))

	d0<-reshape(dd,direction = "long",varying=c("y.2000","y.2002","y.2005","y.2008","y.2012","y.2014","y.2015"),
				idvar=c("Country.Name","Country.Code"))	#,timevar=c(2000,2002,2005,2008,2012,2014,2015)
	vnam2<-strsplit(vnam," (",fixed=T)	# (以降の単位部分は除く
	vnam2<-gsub(" |,|%","_",vnam2[[1]])	#　,や%､スペースは_に
	names(d0)[3:4]<-c("year",vnam2)
		d0[,4]<-as.numeric(d0[,4])	#characterになっているので数値に
	print(head(d0))
	return(d0)
	}

#---World Bankの国データにある時点データをすべてとりだす
	n_NA<-function(x){
		sum(is.na(x))
		}

toLong<-function(dat,vnam){
	#vnam<-"Population, total";dat<-WBdat
	dd<-dat[dat$Series.Name==vnam,c(1,2,5,6:35)]
	names(dd);dim(dd)
# [1] "WB_Code"       "Afro_Code"     "Country.Name"  "1990.[YR1990]" "1991.[YR1991]" "1992.[YR1992]" "1993.[YR1993]" "1994.[YR1994]" "1995.[YR1995]"
#[10] "1996.[YR1996]" "1997.[YR1997]" "1998.[YR1998]" "1999.[YR1999]" "2000.[YR2000]" "2001.[YR2001]" "2002.[YR2002]" "2003.[YR2003]" "2004.[YR2004]"
#[19] "2005.[YR2005]" "2006.[YR2006]" "2007.[YR2007]" "2008.[YR2008]" "2009.[YR2009]" "2010.[YR2010]" "2011.[YR2011]" "2012.[YR2012]" "2013.[YR2013]"
#[28] "2014.[YR2014]" "2015.[YR2015]" "2016.[YR2016]" "2017.[YR2017]" "2018.[YR2018]" "2019.[YR2019]"
#[1] 37 33

	names(dd)[4:33]<-paste("y",seq(1990,2019),sep=".")		#変数名 .数字の後からtimeを類推する
	d0<-reshape(dd,direction = "long",varying=names(dd)[4:33],	
				idvar=c("Country.Name","WB_Code","Afro_Code"))		#long形式に
				head(d0)
#                           WB_Code Afro_Code  Country.Name time        y
#Burundi.BDI.BDI.1990           BDI       BDI       Burundi 1990  5438957
#Benin.BEN.BEN.1990             BEN       BEN         Benin 1990  4978496
#Burkina Faso.BFA.BFO.1990      BFA       BFO  Burkina Faso 1990  8811034

	vnam2<-strsplit(vnam," (",fixed=T)	# (以降の単位部分は除く
	vnam2<-gsub(" |,|%","_",vnam2[[1]])	#　,や%､スペースは_に
	names(d0)[4:5]<-c("year",vnam2)
		d0[,5]<-as.numeric(d0[,5])	#characterになっているので数値に
#		d0<-d0[order(d0[,c("Country.Name","year")]),]
	#trend plot
	dd<-d0[,c(3:5)]
	names(dd)<-c("cnam","t","y")
	group_trend_plot(dd,vnam)

	print(by(d0[,5],d0$Country.Name,n_NA))
	print(head(d0))
	return(d0)
	}

#-----yとtをあたえてトレンド推定　　t2について内挿値を算出
fit_trend<-function(t,y,tnew){
	#y<-WBdat[WBdat$Country.Name=="Algeria","Access_to_electricity.x"];t<-WBdat[WBdat$Country.Name=="Algeria","year"];tnew<-seq(2000,2018)
	#y<-WBdat[WBdat$Country.Name=="Algeria","Literacy_rate__adult_total"]
	#y<-WBdat[WBdat$Country.Name=="Algeria","Income_share_held_by_lowest_20_"]
	
	t2<-t^2
	dt<-data.frame(t,t2,y)
	dt$no<-seq(1,dim(dt)[1])
	dt$fg<-complete.cases(dt)
			print(sum(dt$fg))
	dt2<-dt[dt$fg,]
	print(summary(data.frame(t,y)))

	res<-lm(y~t+t2,data=dt2)
#	res<-lm(y~t,data=dt2)		#2乗を入れると"Algeria","Access_to_electricity.xがおかしくなるので1乗に
		print(summary(res))

#	tnew2<-tnew^2
	newdat<-data.frame(tnew,tnew2)
#	newdat<-data.frame(tnew)
	names(newdat)<-c("t","t2")
#	names(newdat)<-c("t")
	yhat<-predict(res,newdat)

	dtnew<-data.frame(newdat,yhat)
	(dtnew<-merge(dtnew,dt2[,c("t","y")],by.x="t",by.y="t",all=T))
	dtnew$y2<-ifelse(is.na(dtnew$y),dtnew$yhat,dtnew$y)	#データがある場合はそれを､欠損の場合は内挿値を
	dtnew$fg.interpolate<-ifelse(is.na(dtnew$y),1,0)	#データがある場合はそれを､欠損の場合は内挿値を
	
	plot(t,y,xlim=c(min(t),max(tnew)),ylim=c(min(y),max(yhat)))
	lines(tnew,yhat,col="red")
	return(dtnew[,c("t","y2","fg.interpolate")])	#y2　観測値もしくは内挿値､fg.interpolate=内挿値の場合に1
	}




#------------------
#		国コード対応表　
#		下記からDL　ただし､国名やコードの最後にスペースが入っているので要注意
#		https://wits.worldbank.org/wits/wits/witshelp/content/codes/country_codes.htm
#------------------
Cnam<-read.xlsx("Afro_countryCode.xlsx",sheet=1)
	names(Cnam);dim(Cnam)	#[1] "Country.Name" "Afro_Code"   [1] 37  2
dat<-read.xlsx("WBcountry_code.xlsx",sheet=1)
	names(dat);dim(dat)	#[1] "Country.Name0" "Country.Name"  "WB_Code"       "WB_nCode"    [1] 264   3
dat<-dat[,2:3]

Cnam<-merge(Cnam,dat,by.x="Country.Name","Country.Name",all=T)
 names(Cnam)
#[1] "Country.Name"  "Afro_Code"     "Country.Name0" "WB_Code"       "WB_nCode"   
Cnam<-Cnam[order(Cnam$Afro_Code),]
	edit(Cnam)
#対応できたことがわかったので不要部分を捨てる
(Cnam<-Cnam[!is.na(Cnam$Afro_Code),])
	dim(Cnam)	#[1] 37  5

#------------------
#		国レベルデータ  2000 2002 2005 2008   2012  2014  2015のみを残す  Afroは最新2018だがないので､2015を使う
#------------------
#Data from database: World Development Indicators Last Updated: 12/20/2019
#https://databank.worldbank.org/indicator/NY.GDP.MKTP.KD.ZG/1ff4a498/Popular-Indicators
#にいくつか加えた   データは1990年からに

dat0<-read.xlsx("WBPopular Indicators2.xlsx",sheet=1)	#, stringsAsFactors=FALSE
	summary(dat0)
	head(dat0)
	names(dat0);dim(dat0)	#[1] 11937    201		Country.CodeがWBの国略号3文字
# [1] "Series.Name"   "Series.Code"   "Country.Name"  "Country.Code"  "1990.[YR1990]" "1991.[YR1991]" "1992.[YR1992]" "1993.[YR1993]" "1994.[YR1994]"
#[10] "1995.[YR1995]" "1996.[YR1996]" "1997.[YR1997]" "1998.[YR1998]" "1999.[YR1999]" "2000.[YR2000]" "2001.[YR2001]" "2002.[YR2002]" "2003.[YR2003]"
#[19] "2004.[YR2004]" "2005.[YR2005]" "2006.[YR2006]" "2007.[YR2007]" "2008.[YR2008]" "2009.[YR2009]" "2010.[YR2010]" "2011.[YR2011]" "2012.[YR2012]"
#[28] "2013.[YR2013]" "2014.[YR2014]" "2015.[YR2015]" "2016.[YR2016]" "2017.[YR2017]" "2018.[YR2018]" "2019.[YR2019]"
#[1] 13456    34
	unique(dat0$Country.Code)
	unique(dat0$Country.Name)

#コード対応表とマージして対象国のみに
WBdat<-merge(Cnam[,c( "Afro_Code","WB_Code")],dat0,by.x="WB_Code",by.y="Country.Code",all=T)
	dim(WBdat[is.na(WBdat$Afro_Code),])
	WBdat<-WBdat[!is.na(WBdat$Afro_Code),]
	unique(WBdat$WB_Code)
	length(unique(WBdat$WB_Code))
	unique(WBdat$Afro_Code)		#37ヵ国
	length(unique(WBdat$Afro_Code))
head(WBdat)
	names(WBdat);dim(WBdat)
edit(WBdat)
# [1] "WB_Code"       "Afro_Code"     "Series.Name"   "Series.Code"   "Country.Name"  "1990.[YR1990]" "1991.[YR1991]" "1992.[YR1992]" "1993.[YR1993]"
#[10] "1994.[YR1994]" "1995.[YR1995]" "1996.[YR1996]" "1997.[YR1997]" "1998.[YR1998]" "1999.[YR1999]" "2000.[YR2000]" "2001.[YR2001]" "2002.[YR2002]"
#[19] "2003.[YR2003]" "2004.[YR2004]" "2005.[YR2005]" "2006.[YR2006]" "2007.[YR2007]" "2008.[YR2008]" "2009.[YR2009]" "2010.[YR2010]" "2011.[YR2011]"
#[28] "2012.[YR2012]" "2013.[YR2013]" "2014.[YR2014]" "2015.[YR2015]" "2016.[YR2016]" "2017.[YR2017]" "2018.[YR2018]" "2019.[YR2019]"
#[1] 2294   35


#	基礎
WBdatL<-toLong(WBdat,"Population, total")
	dd<-toLong(WBdat,"Population growth (annual %)")
	names(dd)
#[1] "WB_Code"           "Afro_Code"         "Country.Name"      "year"              "Population_growth"
		WBdatL<-merge(WBdatL,dd[,-c(1,2)],by.x=c("Country.Name","year"),by.y=c("Country.Name","year"))
	dd<-toLong(WBdat,"Surface area (sq. km)")
		WBdatL<-merge(WBdatL,dd[,-c(1,2)],by.x=c("Country.Name","year"),by.y=c("Country.Name","year"))
#	GNP　GDP
	dd<-toLong(WBdat,"GDP (current US$)")
		WBdatL<-merge(WBdatL,dd[,-c(1,2)],by.x=c("Country.Name","year"),by.y=c("Country.Name","year"))
	dd<-toLong(WBdat,"GDP growth (annual %)")
		WBdatL<-merge(WBdatL,dd[,-c(1,2)],by.x=c("Country.Name","year"),by.y=c("Country.Name","year"))
	dd<-toLong(WBdat,"GNI per capita, PPP (current international $)")
		WBdatL<-merge(WBdatL,dd[,-c(1,2)],by.x=c("Country.Name","year"),by.y=c("Country.Name","year"))
	dd<-toLong(WBdat,"GDP per capita (current US$)")
		WBdatL<-merge(WBdatL,dd[,-c(1,2)],by.x=c("Country.Name","year"),by.y=c("Country.Name","year"))

#	格差
	dd<-toLong(WBdat,"Income share held by lowest 20%")		#欠損多い
		WBdatL<-merge(WBdatL,dd[,-c(1,2)],by.x=c("Country.Name","year"),by.y=c("Country.Name","year"))
	dd<-toLong(WBdat,"GINI index (World Bank estimate)")
		WBdatL<-merge(WBdatL,dd[,-c(1,2)],by.x=c("Country.Name","year"),by.y=c("Country.Name","year"))
#	教育
	dd<-toLong(WBdat,"Primary completion rate, total (% of relevant age group)")
		WBdatL<-merge(WBdatL,dd[,-c(1,2)],by.x=c("Country.Name","year"),by.y=c("Country.Name","year"))
	dd<-toLong(WBdat,"School enrollment, secondary (% gross)")
		WBdatL<-merge(WBdatL,dd[,-c(1,2)],by.x=c("Country.Name","year"),by.y=c("Country.Name","year"))
	dd<-toLong(WBdat,"Literacy rate, adult total (% of people ages 15 and above)")
		WBdatL<-merge(WBdatL,dd[,-c(1,2)],by.x=c("Country.Name","year"),by.y=c("Country.Name","year"))
#	インフラ
	dd<-toLong(WBdat,"Access to electricity (% of population)")
		WBdatL<-merge(WBdatL,dd[,-c(1,2)],by.x=c("Country.Name","year"),by.y=c("Country.Name","year"))
#通信
#	dd<-toLong(WBdat,"Fixed telephone subscriptions")
#		WBdatL<-merge(WBdatL,dd[,-c(1,2)],by.x=c("Country.Name","year"),by.y=c("Country.Name","year"))
#	dd<-toLong(WBdat,"Fixed telephone subscriptions (per 100 people)")
#		WBdatL<-merge(WBdatL,dd[,-c(1,2)],by.x=c("Country.Name","year"),by.y=c("Country.Name","year"))
	dd<-toLong(WBdat,"Internet users (per 100 people)")
		WBdatL<-merge(WBdatL,dd[,-1],by.x=c("Country.Name","year"),by.y=c("Country.Name","year"))
	dd<-toLong(WBdat,"Individuals using the Internet (% of population)")
		WBdatL<-merge(WBdatL,dd[,-1],by.x=c("Country.Name","year"),by.y=c("Country.Name","year"))
	dd<-toLong(WBdat,"Mobile cellular subscriptions")
		WBdatL<-merge(WBdatL,dd[,-c(1,2)],by.x=c("Country.Name","year"),by.y=c("Country.Name","year"))

	summary(WBdatL)
	head(WBdatL)
	names(WBdatL);dim(WBdatL)
	edit(WBdatL)


#----------		cnam毎にyの欠損値を線形モデルで補完
t0<-WBdatL$year
cnam<-WBdatL$Country.Name
	clist<-unique(cnam)

for(jj in seq(5,dim(WBdatL)[2])){	#変数についてループ
	#jj<-5
	y0<-WBdatL[,jj]
	(vnam2<-paste(names(WBdatL)[jj],"2",sep=""))
	print(names(WBdatL[jj]))


	jpeg(file=paste(vnam2,".jpeg",sep=""),width = 960, height = 480)
	par(mfrow=c(5,8),mar=c(0,0,0,0))

	dd<-data.frame(cnam,y0,t0)

		for(ii in seq(1,length(clist))){	#国についてループ
		#ii<-1	
			print(cl<-clist[ii])
			dd2<-dd[dd$cnam==clist[ii],]
			
			if(sum(!is.na(dd2$y0))==0){
					print(list("**********No Data",clist[ii],vnam2))
					}else{
						ydat0<-fit_trend(dd2$t0,dd2$y0,tnew=seq(2000,2018))	#y0の欠損を線形補完しつつ　2018までも外挿
						legend("topleft",cl)
						ydat0$cnam<-cl
							if(ii==1) {
							ydat_all<-ydat0
							}else{
							ydat_all<-rbind(ydat_all,ydat0)
							}
					}
			}
#      t       y2 fg.interpolate       cnam
#1  2000 45.68376              1 Mozambique
#2  2001 46.34044              1 Mozambique
			names(ydat_all)[1]<-"year"
			names(ydat_all)[2]<-vnam2
			names(ydat_all)[3]<-paste(vnam2,"fg.interpolat",sep=".")
			names(ydat_all)[4]<-"Country.Name"
			dev.off()
			x<- readline(list(vnam2,"waiting for pushing a key"))

	if(jj==5) {
		WBdatL2<-ydat_all
			}else{
		WBdatL2<-merge(WBdatL2,ydat_all,by.x=c("Country.Name","year"),by.y=c("Country.Name","year"),all=T)
			}
	}
	
	summary(WBdatL2)
	head(WBdatL2)
	names(WBdatL2);	dim(WBdatL2)

edit(WBdatL2)
	
names(WBdatL);dim(WBdatL)
names(WBdatL2);dim(WBdatL2)

WBdatL<-merge(WBdatL,WBdatL2,by.x=c("Country.Name","year"),by.y=c("Country.Name","year"),all=T)
summary(WBdatL)
	names(WBdatL);dim(WBdatL)
#[1] "Country.Name"                                   "year"                                          
# [3] "WB_Code"                                        "Afro_Code"                                     
# [5] "Population__total"                              "Population_growth"                             
# [7] "Income_share_held_by_lowest_20_"                "GDP"                                           
# [9] "GDP_growth"                                     "Literacy_rate__adult_total"                    
#[11] "Access_to_electricity"                          "Mobile_cellular_subscriptions"                 
#[13] "Population__total2"                             "Population__total2.fg.interpolat"              
#[15] "Population_growth2"                             "Population_growth2.fg.interpolat"              
#[17] "Income_share_held_by_lowest_20_2"               "Income_share_held_by_lowest_20_2.fg.interpolat"
#[19] "GDP2"                                           "GDP2.fg.interpolat"                            
#[21] "GDP_growth2"                                    "GDP_growth2.fg.interpolat"                     
#[23] "Literacy_rate__adult_total2"                    "Literacy_rate__adult_total2.fg.interpolat"     
#[25] "Access_to_electricity2"                         "Access_to_electricity2.fg.interpolat"          
#[27] "Mobile_cellular_subscriptions2"                 "Mobile_cellular_subscriptions2.fg.interpolat"  
#[1] 703  28
save(WBdatL,file="0WBdatL.rda")







#---------------
#			1
#---------------
Afrodat1 <- read_sav("merged_r1_data.sav")
head(Afrodat1)
summary(Afrodat1)
names(Afrodat1)	
dim(Afrodat1)	#
	Afrodat1$wave<-1
	Afrodat1$year<-2000
edit(Afrodat1[,1:2])
table(Afrodat1$country)
#Afrodat1$COUNTRY2<-substr(Afrodat1$casenumb,1,3)	#他のroundだと､はじめの3文字が国名+回答者番号だが､1stは3文字+数字のところと数字から始まるものが混在
(lb<-names(attributes(Afrodat1$country)$labels))
# [1] "Botswana"     "Ghana"        "Lesotho"      "Malawi"       "Mali"         "Namibia"      "Nigeria"      "South Africa" "Tanzania"     "Uganda"      
#[11] "Zambia"       "Zimbabwe"    
lb[4]<-"MLW"	#Malawi	他のwaveと統一
lb[5]<-"MLI"	#Mali	他のwaveと統一
lb[8]<-"SAF"	# "South Africa"はSAF
	(lb<-substr(toupper(lb),1,3))	#大文字にしてはじめの3文字

Afrodat1$COUNTRY2<-lb[Afrodat1$country]	#こちらに入っている
	table(Afrodat1$COUNTRY2)

# BOT  GHA  LES  MAL  NAM  NIG  SAF  TAN  UGA  ZAM  ZIM 
#1200 2004 1177 3297 1183 3603 2200 2198 2271 1198 1200 


#"Variable name: medtv
#Variable label: TV news
#Values: 0-5, 9, 98, 99
#Value Labels: 0=Never, 1=Less than once a month, 2=About once a month, 3=About once a week/few times a month, 4=Several times a week/a few times a week, 5=Every day, 9=Don’t Know, 98=Refused to Answer, 99=Missing Data
#Notes: Not asked in Ghana.
#Question text - SAB

#これを使って欠損値処理
Afrodat1$News_Radio<-repNA05(Afrodat1$medrad)
#    0     1     2     3     4     5  <NA> 
# 2817   746   417  1716  4356 11372   107 

Afrodat1$News_Television<-repNA05(Afrodat1$medtv)
#   0    1    2    3    4    5 <NA> 
#9966 1032  375 1162 2054 4663 2279 


#他の回は0=Never, 1=Less than once a month, 2=A few times a month, 3=A few times a week, 4=Every day, 9=Don’t Know, 98=Refused to Answer, -1=Missing Data なので､5→4　　4→3　　3→2　　とする｡
Afrodat1$News_Radio0<-Afrodat1$News_Radio
Afrodat1$News_Radio<-ifelse(Afrodat1$News_Radio>2,Afrodat1$News_Radio-1,Afrodat1$News_Radio)
	table(Afrodat1$News_Radio0,Afrodat1$News_Radio,exclude=NULL)

Afrodat1$News_Television0<-Afrodat1$News_Television
Afrodat1$News_Television<-ifelse(Afrodat1$News_Television>2,Afrodat1$News_Television-1,Afrodat1$News_Television)
	table(Afrodat1$News_Television0,Afrodat1$News_Television,exclude=NULL)

Afrodat1$News_Newspaper<-NA	#repNA04(Afrodat1$Q12C)	#設問がない場合はNAに
Afrodat1$News_Internet<-NA	#repNA04(Afrodat1$Q12D)
Afrodat1$News_Social_media<-NA	#repNA04(Afrodat1$Q12E)

table(Afrodat1$COUNTRY2,Afrodat1$News_Radio,exclude=NULL)
table(Afrodat1$COUNTRY2,Afrodat1$News_Television,exclude=NULL)


#保存
save(Afrodat1,file=“0Afrodat1.rda”)


#---------------
#			2
#---------------
Afrodat2 <- read_sav("merged_r2_data.sav")
#View(Afrodat2)
summary(Afrodat2)
names(Afrodat2)	
dim(Afrodat2)	#[1] 53935   364
	Afrodat2$wave<-2
	Afrodat2$year<-2002

table(Afrodat2$country)
Afrodat2$COUNTRY2<-substr(Afrodat2$respno,1,3)	#はじめの3文字が国名
	table(Afrodat2$COUNTRY2)
# BOT  cve  CVE  GHA  KEN  LES  MLI  MOZ  MWI  NAM  NIG  SAF  SEN  TNZ  UGA  ZAM  ZIM 
#1200 1250   18 1200 2398 1200 1283 1400 1200 1199 2428 2400 1200 1223 2400 1198 1104 

Afrodat2$COUNTRY2[Afrodat2$COUNTRY2=="cve"]<-"CVE"	#大文字に
Afrodat2$COUNTRY2[Afrodat2$COUNTRY2=="TNZ"]<-"TAN"	#他のwaveと統一
Afrodat2$COUNTRY2[Afrodat2$COUNTRY2=="MWI"]<-"MLW"	#他のwaveと統一


#これを使って欠損値処理
Afrodat2$News_Radio<-repNA04(Afrodat2$q26a)
Afrodat2$News_Television<-repNA04(Afrodat2$q26b)
Afrodat2$News_Newspaper<-repNA04(Afrodat2$q26c)
Afrodat2$News_Internet<-NA	#repNA04(Afrodat2$Q12D)
Afrodat2$News_Social_media<-NA	#repNA04(Afrodat2$Q12E)

#保存
save(Afrodat2,file=“0Afrodat2.rda”)

#---------------
#			3
#---------------
Afrodat3 <- read_sav("merged_r3_data.sav")
#View(Afrodat3)
summary(Afrodat3)
names(Afrodat3)	
dim(Afrodat3)	#[1]25397   302
	Afrodat3$wave<-3
	Afrodat3$year<-2005

table(Afrodat3$country)
Afrodat3$COUNTRY2<-substr(Afrodat3$respno,1,3)	#はじめの3文字が国名
	table(Afrodat3$COUNTRY2)
# BEN  BOT  CVE  GHA  KEN  LES  MAD  MLI  MOZ  MWI  NAM  NIG  SAF  SEN  TAN  UGA  ZAM  ZIM 
#1198 1200 1256 1197 1278 1161 1350 1244 1198 1200 1200 2363 2400 1200 1304 2400 1200 1048 
#これを使って欠損値処理
Afrodat3$COUNTRY2[Afrodat3$COUNTRY2=="MWI"]<-"MLW"	#他のwaveと統一

Afrodat3$News_Radio<-repNA04(Afrodat3$q15a)
Afrodat3$News_Television<-repNA04(Afrodat3$q15b)
Afrodat3$News_Newspaper<-repNA04(Afrodat3$q15c)
Afrodat3$News_Internet<-NA	#repNA04(Afrodat3$q15d)
Afrodat3$News_Social_media<-NA	#repNA04(Afrodat3$q15e)

#保存
save(Afrodat3,file=“0Afrodat3.rda”)

#---------------
#			4
#---------------
Afrodat4 <- read_sav("merged_r4_data.sav")
#View(Afrodat4)
summary(Afrodat4)
names(Afrodat4)	
dim(Afrodat4)	#[1] 53935   364
	Afrodat4$wave<-4
	Afrodat4$year<-2008

table(Afrodat4$COUNTRY)
Afrodat4$COUNTRY2<-substr(Afrodat4$RESPNO,1,3)	#はじめの3文字が国名
	table(Afrodat4$COUNTRY2)
# BEN  BFO  BOT  CVE  GHA  KEN  LES  LIb  LIB  MAD  MLI  MLW  MOZ  NAM  NIG  SAF  SEN  TAN  UGA  ZAM  ZIM 
#1200 1200 1200 1264 1200 1104 1200    1 1199 1350 1232 1200 1200 1200 2324 2400 1200 1208 2431 1200 1200
Afrodat4$COUNTRY2[Afrodat4$COUNTRY2=="LIb"]<-"LIB"	#大文字に

#これを使って欠損値処理
Afrodat4$News_Radio<-repNA04(Afrodat4$Q12A)
Afrodat4$News_Television<-repNA04(Afrodat4$Q12B)
Afrodat4$News_Newspaper<-repNA04(Afrodat4$Q12C)
Afrodat4$News_Internet<-NA	#repNA04(Afrodat4$Q12D)
Afrodat4$News_Social_media<-NA	#repNA04(Afrodat4$Q12E)

#保存
save(Afrodat4,file=“0Afrodat4.rda”)

#---------------
#			5
#---------------
Afrodat5 <- read_sav("merged-round-5-data-34-countries-2011-2013-last-update-july-2015.sav")
##View(Afrodat5)
summary(Afrodat5)
names(Afrodat5)	
dim(Afrodat5)	#[1] 53935   364
	Afrodat5$wave<-5
	Afrodat5$year<-2012

table(Afrodat5$COUNTRY)

Afrodat5$COUNTRY2<-substr(Afrodat5$RESPNO,1,3)	#はじめの3文字が国名
	table(Afrodat5$COUNTRY2)
# ALG  BDI  BEN  BFO  BOT  CAM  CDI  CVE  EGY  GHA  GUI  KEN  LES  LIB  MAD  MAU  MLI  MLW  MOZ  MRC  NAM  NGR  NIG  SAF  SEN  SRL  SUD  SWZ  TAN  TOG  TUN 
#1204 1200 1200 1200 1200 1200 1200 1208 1190 2400 1200 2399 1197 1199 1200 1200 1200 2407 2400 1196 1200 1199 2400 2399 1200 1190 1199 1200 2400 1200 1200 
# UGA  ZAM  ZIM 
#2400 1200 2400 
Afrodat5$COUNTRY2[Afrodat5$COUNTRY2=="MRC"]<-"MOR"	#他のwaveと統一

#これを使って欠損値処理
Afrodat5$News_Radio<-repNA04(Afrodat5$Q13A)
Afrodat5$News_Television<-repNA04(Afrodat5$Q13B)
Afrodat5$News_Newspaper<-repNA04(Afrodat5$Q13C)
Afrodat5$News_Internet<-repNA04(Afrodat5$Q13D)
Afrodat5$News_Social_media<-NA	#repNA04(Afrodat5$Q13E)

#保存
save(Afrodat5,file=“0Afrodat5.rda”)


#---------------
#			6
#---------------
Afrodat6 <- read_sav("merged_r6_data_2016_36countries2.sav")
#View(Afrodat6)
summary(Afrodat6)
names(Afrodat6)	
dim(Afrodat6)	#[1] 53935   364
	Afrodat6$wave<-6
	Afrodat6$year<-2014

table(Afrodat6$COUNTRY)
Afrodat6$COUNTRY2<-substr(Afrodat6$RESPNO,1,3)	#はじめの3文字が国名
	table(Afrodat6$COUNTRY2)
# ALG  BDI  BEN  BFO  BOT  CAM  CDI  CVE  EGY  GAB  GHA  GUI  KEN  LES  LIB  MAD  MAU  MLI  MLW  MOR  MOZ  NAM  NGR  NIG  SAF  SEN  SRL  STP  SUD  SWZ  TAN  TOG  TUN  UGA  ZAM  ZIM 
#1200 1200 1200 1200 1200 1182 1199 1200 1198 1198 2400 1200 2397 1200 1199 1200 1200 1200 2400 1200 2400 1200 1200 2400 2390 1200 1191 1196 1200 1200 2386 1200 1200 2400 1199 2400 

#これを使って欠損値処理


#    0     1  <NA> 
#22089 31246   600 

#----年齢
#Question Number: Q1
#Question: How old are you?
#Variable Label: Q1. Age
#Values: 18-105, 998-999, -1
#Value Labels: 98=Refused to answer, 999=Don’t know, -1=Missing
Afrodat6$Age<-ifelse(Afrodat6$Q1<0|Afrodat6$Q1>105|Afrodat6$Q1==98,NA,Afrodat6$Q1)
	table(Afrodat6$Employment_status,exclude=NULL)

#"Question Number: Q2
#Question: Which language is your home language?
#Variable Label: Q2. Language of respondent
#Values: 1-35, 101- 107, 141-149, 180- 197, 220-221, 260- 278, 300-315, 340-342, 381-396, 420-421,460-471, 502- 518, 540-553, 581-591, 621- 653, 660-668, 702-710, 740-800, 820- 872, 900, 930- 943, 1100-1105, 1141- 1160, 1180, 1220 -1282, 1300-1305, 1420, 1460,1501,1540,1541,1620,1621,1660,1661,1662, 1700-1707, 2200-2222, 2740-2748, 9998-9999"
#Value Labels: -1 =Missing, 1 =English, 2 =French, 3 =Portuguese, 4 =Swahili, 5 =Arabic, 6 =Adja, 7 =Afrikaans, 8 =Arabe, 9 =Bambara, 10 =Bassa, 11

Afrodat6$Language<-ifelse(Afrodat6$Q2<0|Afrodat6$Q2>=9998,NA,Afrodat6$Q2)
table(Afrodat6$Language,exclude=NULL)

	d<-data.frame(table(Afrodat6$Language,exclude=NULL))
	d[order(d[,2],decreasing=T),]
#    Var1 Freq
#5      5 2199
#3      3 1400
#117  463 1258	Chichewa
#64   260 1210	Akan
#425 9995 1206	Other
#396 1460 1198	Egyptian Arabic
#62   220 1195	Crioulo
#333 1180 1192	Kirund
#96   340 1182	Sesotho
#398 1540 1179	Sudanese Arabic
#303  900 1166	Creole
#400 1620 1166	siSwati
#294  861 1156	Shona
#395 1420 1108	Algerian Arabic
#

Afrodat6$dlang_English<-ifelse(Afrodat6$Language==1,1,0)
Afrodat6$dlang_French<-ifelse(Afrodat6$Language==2,1,0)
Afrodat6$dlang_Portuguese<-ifelse(Afrodat6$Language==3,1,0)
Afrodat6$dlang_Swahili<-ifelse(Afrodat6$Language==4,1,0)
Afrodat6$dlang_Arabic<-ifelse(Afrodat6$Language==5,1,0)
Afrodat6$dlang_Swahili<-ifelse(Afrodat6$Language==6,1,0)
Afrodat6$dlang_Afrikaans<-ifelse(Afrodat6$Language==7,1,0)

Afrodat6$dlang_Chichewa<-ifelse(Afrodat6$Language==463,1,0)
Afrodat6$dlang_Akan<-ifelse(Afrodat6$Language==260,1,0)
Afrodat6$dlang_Other<-ifelse(Afrodat6$Language==9995,1,0)
Afrodat6$dlang_Egyptian_Arabic<-ifelse(Afrodat6$Language==1460,1,0)
Afrodat6$dlang_Crioulo<-ifelse(Afrodat6$Language==220,1,0)
Afrodat6$dlang_Kirund<-ifelse(Afrodat6$Language==1180,1,0)
Afrodat6$dlang_Sesotho<-ifelse(Afrodat6$Language==340,1,0)
Afrodat6$dlang_Sudanese_Arabic<-ifelse(Afrodat6$Language==1540,1,0)
Afrodat6$dlang_Creole<-ifelse(Afrodat6$Language==900,1,0)
Afrodat6$dlang_siSwati<-ifelse(Afrodat6$Language==1620,1,0)
Afrodat6$dlang_Shona<-ifelse(Afrodat6$Language==861,1,0)
Afrodat6$dlang_Algerian_Arabic<-ifelse(Afrodat6$Language==1420,1,0)


#"Question Number: Q4A
#Question: In general, how would you describe: The present economic condition of this country?
#Variable Label: Q4A. Country’s present economic condition
#Values: 1-5, 9, 98, -1
#Value Labels: 1=Very bad, 2=Fairly bad, 3=Neither good nor bad, 4=Fairly good, 5=Very good, 9=Don’t know, 98=Refused to answer, -1=Missing
#Source: NDB, Zambia96"
#"Question Number: Q4B
#Variable Label: Q4B. Your present living conditions
Afrodat6$Cond_econ<-repNA05(Afrodat6$Q4A)
Afrodat6$Cond_your_liv<-repNA05(Afrodat6$Q4B)


#"Question Number: Q5
#Question: In general, how do you rate your living conditions compared to those of other [ENTER NATIONALITY]?
#Variable Label: Q5. Your living conditions vs. others
#Values: 1-5, 9, 98, -1
#Value Labels: 1=Much worse, 2=Worse, 3=Same, 4=Better, 5=Much better, 9=Don’t know, 98=Refused to answer, -1=Missing
#Source: SAB"
Afrodat6$Relative_live<-ifelse(Afrodat6$Q5<0|Afrodat6$Q5>=9,NA,Afrodat6$Q5)
	table(Afrodat6$Relative_live,exclude=NULL)		#


#---不足　　
#Variable Label: Q8a. How often gone without food
#Values: 0-4, 9, 98, -1
#Value Labels: 0=Never, 1=Just once or twice, 2=Several times, 3=Many times, 4=Always, 9=Don’t know, 98=Refused to answer, -1=Missing
#Source: NDB
#Variable Label: Q8b. How often gone without water
#Variable Label: Q8c.How often gone without medical care
#Variable Label: Q8d. How often gone without cooking fuel
#Variable Label: Q8e. How often gone without a cash income#Source: SAB"
Afrodat6$gone_food<-repNA04(Afrodat6$Q8A)
Afrodat6$gone_water<-repNA04(Afrodat6$Q8B)
Afrodat6$gone_med<-repNA04(Afrodat6$Q8C)
Afrodat6$gone_fuel<-repNA04(Afrodat6$Q8D)
Afrodat6$gone_cash<-repNA04(Afrodat6$Q8E)

# news source
#"Question Number: Q12A 
#Question: How often do you get news from the following sources: Radio? 
#Variable Label: Q12a. Radio news 
#Values: 0-4, 9, 98, -1 
#Value Labels: 0=Never, 1=Less than once a month, 2=A few times a month, 3=A few times a week, 4=Every day, 9=Don’t know, 98=Refused to answer, -1=Missing 
#Source: Zambia96 "
Afrodat6$News_Radio<-repNA04(Afrodat6$Q12A)
Afrodat6$News_Television<-repNA04(Afrodat6$Q12B)
Afrodat6$News_Newspaper<-repNA04(Afrodat6$Q12C)
Afrodat6$News_Internet<-repNA04(Afrodat6$Q12D)
Afrodat6$News_Social_media<-repNA04(Afrodat6$Q12E)


#----電気　　R6にはなし
# Gone without electricity
# "Question Number: Q94
#Question: Do you have an electric connection to your home from the mains? [If yes] How often is the electricity actually available?
#Variable Label: Q94. Electric connection from mains
#Values: 0-5, 9, 98, -1
#Value Labels: 0= No mains electric supply or connection to the home, [If yes], 1=Never, 2=Occasionally, 3= About half of the time, 4= Most of the time, 5= All of the time, 9=Don’t know , 98=Refused to answer, -1=Missing
#Source: Afrobarometer Round 5"

 
# "Question Number: Q94
#Question: Do you have an electric connection to your home from the mains? [If yes] How often is the electricity actually available?
#Variable Label: Q94. Electric connection from mains
#Values: 0-5, 9, 98, -1
#Value Labels: 0= No mains electric supply or connection to the home, [If yes], 1=Never, 2=Occasionally, 3= About half of the time, 4= Most of the time, 5= All of the time, 9=Don’t know , 98=Refused to answer, -1=Missing
#Source: Afrobarometer Round 5"
Afrodat6$Electric_connection<-repNA05(Afrodat6$Q94)




#---Public affairへの興味
#"Question Number: Q13 
#Question: How interested would you say you are in public affairs? 
#Variable Label: Q13. Interest in public affairs 
#Values: 0-3, 9, 98, -1 
#Value Labels: 0=Not at all interested, 1=Not very interested, 2=Somewhat interested, 3=Very interested, 9=Don’t know, 98=Refused to answer, -1=Missing 
#Source: SAB 
#Note : Interviewer was instructed to prompt if necessary with “You know, in politics and government.” "

Afrodat6$Interest_pubaff<-repNA03(Afrodat6$Q13)


#政治についての会話
#"Question Number: Q14
#Question: When you get together with your friends or family, would you say you discuss political matters:
#Variable Label: Q14. Discuss politics
#Values: 0-2, 9, 98, -1
#Value Labels: 0=Never, 1=Occasionally, 2=Frequently, 9=Don’t know, 98=Refused to answer, -1=Missing
#Source: Adapted from Zambia96."
Afrodat6$Discuss_politics<-repNA02(Afrodat6$Q14)



#"Question Number: Q19A
#Question: Let’s turn to your role in the community. Now I am going to read out a list of groups that people join or attend. For each one, could you tell me whether you are an official leader, an active member, an inactive member, or not a member: A religious group that meets outside of regular worship services? Variable Label: Q19a. Member of religious group
#Values: 0-3, 9, 98, -1
#Value Labels: 0=Not a Member, 1=Inactive member, 2=Active member, 3=Official leader, 9=Don’t know, 98=Refused to answer, -1=Missing
#Source: SAB"
Afrodat6$Mem_religious<-repNA03(Afrodat6$Q19A)

#"Question Number: Q19B
#Variable Label: Q19b. Member of voluntary association or community group
#Source: Afrobarometer Round 4"
Afrodat6$Mem_voluntary<-repNA03(Afrodat6$Q19B)	#


#"Question Number: Q20A
#Question: Here is a list of actions that people sometimes take as citizens. For each of these, please tell me whether you, personally, have done any of these things during the past year. If not, would you do this if you had the chance: Attended a community meeting?
#Variable Label: Q20a. Attend a community meeting
#Values: 0-4, 9, 98, -1
#Value Labels: 0=No, would never do this, 1=No, but would do if had the chance, 2=Yes, once or twice, 3=Yes, several times, 4=Yes, often, 9=Don’t know, 98=Refused to answer, -1=Missing
#Source: SAB"

#"Question Number: Q20B
#Variable Label: Q20b. Join the others to raise an issue

Afrodat6$Cit_action_Attend_meeting<-repNA03(Afrodat6$Q20A)	#
Afrodat6$Cit_action_raise_issue<-repNA03(Afrodat6$Q20B)	#



#"Question Number: Q23A
#Question: Thinking about the last national election in [20xx], did you: Attend a campaign rally? 
#Variable Label: Q23a. Last national election: attend a campaign rally
#Values: 0, 1, 9, 98, -1
#Value Labels: 0=No, 1=Yes, 9=Don’t know, 98=Refused to answer, -1=Missing
#Source: Afrobarometer Round 5"
#"Question Number: Q23B
#Variable Label: Q23b. Last national election: Attend a campaign meeting?
#Source: Afrobarometer Round 5"
#"Question Number: Q23C
#Variable Label: Q23c. Last national election: persuade others to vote for a certain candidate or party Values: 0, 1, 9, 98, -1
#Source: Afrobarometer Round 5"
#"Question Number: Q23D
#Question: Thinking about the last national election in [20xx], did you: Work for a candidate or party? 
#Variable Label: Q23d. Last national election: work for a candidate or party
#Source: Afrobarometer Round 5"

Afrodat6$Ele_campaign_rally<-repNA01(Afrodat6$Q23A)
Afrodat6$Ele_campaign_meeting<-repNA01(Afrodat6$Q23B)
Afrodat6$Ele_Attend_persuade<-repNA01(Afrodat6$Q23C)
Afrodat6$Ele_Attend_Work<-repNA01(Afrodat6$Q23D)


#"Question Number: Q27A
#Question: Here is a list of actions that people sometimes take as citizens when they are dissatisfied with government performance. For each of these, please tell me whether you, personally, have done any of these things during the past year. If not, would you do this if you had the chance: Joined others in your community to request action from government
#Variable Label: Q27a. Join others to request government action
#Values: 0-4, 9, 98, -1
#Value Labels: 0=No, would never do this, 1=No, but would do if had the chance, 2=Yes, once or twice, 3=Yes, several times, 4=Yes, often, 9=Don’t know, 98=Refused to answer, -1=Missing
#Source: Zambia96"
#"Question Number: Q27B
#Variable Label: Q27b. Contact media
#Source: Afrobarometer Round5"
#"Question Number: Q27C
#Variable Label: Q27c. Contact official for help
#Source: Afrobarometer Round5"
#"Question Number: Q27D
#Variable Label: Q27d. Refuse to pay a tax or fee to government
#Source: Zambia96"
#Variable Label: Q27e. Attend a demonstration or protest march
#Source: Afrobarometer Round5"

Afrodat6$Diss_request_government<-repNA04(Afrodat6$Q27A)
Afrodat6$Diss_Contact_media<-repNA04(Afrodat6$Q27B)
Afrodat6$Diss_Contact_official<-repNA04(Afrodat6$Q27C)
Afrodat6$Diss_Refuse2pay<-repNA04(Afrodat6$Q27D)
Afrodat6$Diss_Attend_demonstration<-repNA04(Afrodat6$Q27E)


#"Question Number: Q30
#Question: Which of these three statements is closest to your own opinion?
#Statement 1: Democracy is preferable to any other kind of government.
#Statement 2: In some circumstances, a non-democratic government can be preferable. Statement 3: For someone like me, it doesn’t matter what kind of government we have. Variable Label: Q30. Support for democracy
#Values: 1-3, 9, 98, -1
#  Copyright Afrobarometer
#Value Labels: 1=Statement 3: Doesn’t matter, 2=Statement 2: Sometimes non-democratic preferable, 3=Statement 1: Democracy preferable, 9=Don’t know, 98=Refused to answer, -1=Missing
#Source: Latinobarometer (LB)
Afrodat6$Democ_pref<-repNA03(Afrodat6$Q30)


#Variable Label: Q40. Extent of democracy
#Values: 1-4, 8, 9, 98, -1
#Value Labels: 1=Not a democracy, 2=A democracy, with major problems, 3=A democracy, but with minor problems, 4=A full democracy, 8=Do not understand question/ do not understand what ‘democracy’ is, 9=Don’t know, 98=Refused to answer, -1=Missing
#Source: Ghana 97
Afrodat6$Democ_nation<-ifelse(Afrodat6$Q40<1|Afrodat6$Q40>4,NA,Afrodat6$Q40)
	table(Afrodat6$Democ_nation,exclude=NULL)		#


#"Question Number: Q41
#Question: Overall, how satisfied are you with the way democracy works in [ENTER COUNTRY]? Are you: Variable Label: Q41. Satisfaction with democracy
#Values: 0-4, 9, 98, -1
#Value Labels: 0=[COUNTRY] is not a democracy, 1=Not at all satisfied, 2=Not very satisfied, 3=Fairly satisfied, 4=Very satisfied, 9=Don’t know, 98=Refused to answer, -1=Missing
#Source: Eurobarometer
Afrodat6$Democ_satis<-repNA04(Afrodat6$Q41)


#"Question Number: Q52A
#Question: How much do you trust each of the following, or haven’t you heard enough about them to say: The President?
#Variable Label: Q52a. Trust president
#Values: 0-3, 9, 98, -1
#Value Labels: 0=Not at all, 1=Just a little, 2=Somewhat, 3=A lot, 9=Don’t know/Haven’t heard enough, 98=Refused to answer, -1=Missing
#Source: Zambia96

#* The question asked about the most powerful leadership role, whether the President or the Prime Minister. If there was a secondary leader, those are included in country-specific data sets.
#* The following countries asked about their President: ALG, BDI, BEN, BFO, BOT, CAM, CDI, CVE, EGY, GHA, GUI, KEN, LIB, MAD, MLI, MLW, MOZ, NAM, NGR, NIG, SAF, SEN, SRL, SUD, TAN, TOG, UGA, ZAM, ZIM
#* The following countries asked about their Prime Minister: LES, MAU, MRC, TUN
#* Not asked in SWZ"

#Variable Label: Q52b. Trust parliament/national assembly1#Source: Adapted from Zambia96	#*Not asked in EGY"
#Variable Label: Q52h. Trust police#Source: Zambia 96"
#Variable Label: Q52k. Trust traditional leaders	#Source: Zambia 96	#*Not asked in STP, CVE, MAU"
#Variable Label: Q52l. Trust religious leaders	#Source: Zambia 96"
Afrodat6$Trust_president<-repNA04(Afrodat6$Q52A)
Afrodat6$Trust_parliament<-repNA04(Afrodat6$Q52B)
Afrodat6$Trust_police<-repNA04(Afrodat6$Q52H)
Afrodat6$Trust_traditional_leaders<-repNA04(Afrodat6$Q52K)
Afrodat6$Trust_religious_leaders<-repNA04(Afrodat6$Q52L)


#"Question Number: Q54
#Question: In your opinion, over the past year, has the level of corruption in this country increased, decreased, or stayed the same?
#Variable Label: Q54. Level of corruption
#Values: 1-5, 9, 98, -1
#Value Labels: 1=Increased a lot, 2=Increased somewhat, 3=Stayed the same, 4=Decreased somewhat, 5=Decreased a lot, 9=Don’t know, 98=Refused to answer, -1=Missing
#Source: Transparency International
Afrodat6$corruption<-6-repNA05(Afrodat6$Q54)		#逆転しておく

#"Question Number: Q56
#Question: If you ever paid a bribe for any of the services discussed above, did you report any of the incidents you mentioned to a government official or someone in authority?
#Variable Label: Q56. Reported payment of bribes to government
#Values: 0, 1, 7, 9, 98, -1
#Value Labels: 0=No, 1=Yes, 7=Not Applicable, 9=Don`t know, 98=Refused to answer, -1=Missing Source: Afrobarometer Round 6 and Transparency International
#Note: Interviewer asks the question if respondent ever reported paying a bribe on Q55B, Q55D, Q55F, Q55H,Q 55J or-Q55L"
Afrodat6$report_bribe<-ifelse(Afrodat6$Q56<0|Afrodat6$Q56>7,NA,Afrodat6$Q56)
	table(Afrodat6$report_bribe,exclude=NULL)	#bribeしたことがないと7になる


#-------------所有もあるので濱岡はこれを分析予定
#Source: Afrobarometer Rouund 3
#Value Labels: 0=No (Don’t own), 1=Yes (Do own), 9=Don’t know, 98=Refused to answer, -1=Missing
#Question: Which of these things do you personally own: Radio?
#Variable Label: 
#Q91a. Own radio
#Q91b. Own television
#Q91c. Own motor vehicle, car, or motorcycle
#Q91d. Own mobile phone

Afrodat6$Own_Radio <-repNA01(Afrodat6$Q91A)
Afrodat6$Own_TV <-repNA01(Afrodat6$Q91B)
Afrodat6$Own_Auto <-repNA01(Afrodat6$Q91C)
Afrodat6$Own_Mbphone <-repNA01(Afrodat6$Q91D)

#    0     1  <NA> 
#15825 38038    72 
#27034 26812    89 
#42363 11348   224 
#11698 42129   108 

#Q92A　Question: How often do you use: A mobile phone?
#Values: 0-4, 9, 98, -1
#Value Labels: 0=Never, 1=Less than once a month, 2=A few times a month, 3=A few times a week, 4=Every day, 9=Don’t know, 98=Refused to answer, -1=Missing
#Source: Afrobarometer Round 4
#Q92a. How often use a mobile phone
#Q92b. How often use the internet

Afrodat6$Use_Mbphone <-repNA04(Afrodat6$Q92A)
Afrodat6$Use_Inet <-repNA04(Afrodat6$Q92B)
#    0     1     2     3     4  <NA> 
# 8978   667  1221  4669 38216   184 
#36352  1545  2312  5087  7733   906 



#----雇用状況
#Question Number: Q95
#Question: Do you have a job that pays a cash income? If yes, is it full-time or part-time? If no, are you presently looking for a job?
#Variable Label: Q95. Employment status
#Values: 0-3, 9, 98, -1
#Value Labels: 0=No (not looking), 1=No (looking), 2=Yes, part time, 3= Yes, full time, 9=Don’t know, 98=Refused to answer, -1=Missing
#Source: SAB
Afrodat6$Employment_status<-ifelse(Afrodat6$Q95<0|Afrodat6$Q95>3,NA,Afrodat6$Q95)
	table(Afrodat6$Employment_status,exclude=NULL)

#----職業
#Question Number: Q96A
#Question: What is your main occupation? (If unemployed, retired or disabled, what was your last main occupation?)
#Variable Label: Q96a. Occupation of respondent
#Values: 0-12 95, 99, 98, -1
#Value Labels: 0=Never had a job, 1=Student, 2=Housewife / homemaker, 3=Agriculture / farming / fishing / forestry, 4=Trader / hawker / vendor, 5=Retail / Shop , 6=Unskilled manual worker (e.g., cleaner, laborer, domestic help, unskilled manufacturing worker), 7=Artisan or skilled manual worker (e.g., trades like electrician, mechanic, machinist or skilled manufacturing worker), 8=Clerical or secretarial, 9=Supervisor / Foreman / Senior Manager, 10=Security services (police, army, private security), 11=Mid-level professional (e.g., teacher, nurse, mid-level government officer), 12=Upper-level professional (e.g., banker/finance, doctor, lawyer, engineer, accountant, professor, senior-level government officer), 95=Other , 99=Don’t know, 98=Refused to answer, -1=Missing
#Source: Afrobarometer Round 6

Afrodat6$Occupation<-ifelse(Afrodat6$Q96A<0|Afrodat6$Q96A>95,NA,Afrodat6$Q96A)
	table(Afrodat6$Occupation,exclude=NULL)
#   0     1     2     3     4     5     6     7     8     9    10    11    12    95  <NA> 
# 5953  5344  5540 12375  4864  2195  4850  4074   947   660   931  3080  1150  1766   206 


Afrodat6$dOccupation_Never<-ifelse(Afrodat6$Occupation==0,1,0)
Afrodat6$dOccupation_Student<-ifelse(Afrodat6$Occupation==1,1,0)
Afrodat6$dOccupation_Housewife_homemaker<-ifelse(Afrodat6$Occupation==2,1,0)
Afrodat6$dOccupation_primary<-ifelse(Afrodat6$Occupation==3,1,0)
Afrodat6$dOccupation_Trader<-ifelse(Afrodat6$Occupation==4,1,0)
Afrodat6$dOccupation_Retail<-ifelse(Afrodat6$Occupation==5,1,0)
Afrodat6$dOccupation_Unskilled<-ifelse(Afrodat6$Occupation==6,1,0)
Afrodat6$dOccupation_skilled<-ifelse(Afrodat6$Occupation==7,1,0)
Afrodat6$dOccupation_Clerical<-ifelse(Afrodat6$Occupation==8,1,0)
Afrodat6$dOccupation_Supervisor<-ifelse(Afrodat6$Occupation==9,1,0)
Afrodat6$dOccupation_police<-ifelse(Afrodat6$Occupation==10,1,0)
Afrodat6$dOccupation_Mid_level<-ifelse(Afrodat6$Occupation==11,1,0)
Afrodat6$dOccupation_Upper_level<-ifelse(Afrodat6$Occupation==12,1,0)
Afrodat6$dOccupation_Other<-ifelse(Afrodat6$Occupation==95,1,0)


#----学歴
#Question Number: Q97
#Question: What is your highest level of education?
#Variable Label: Q97. Education of respondent
#Values: 0-9, 99, 98, -1
#Value Labels: 0=No formal schooling, 1=Informal schooling only (including Koranic schooling), 2=Some primary schooling, 3=Primary school completed, 4=Intermediate school or Some secondary school / high school, 5=Secondary school / high school completed , 6=Post-secondary qualifications, other than university e.g. a diploma or degree from a polytechnic or college, 7=Some university, 8=University completed, 9=Post-graduate, 99=Don’t know [Do not read], 98=Refused to answer, -1=Missing
#Source: SAB

Afrodat6$Education<-ifelse(Afrodat6$Q97<0|Afrodat6$Q97>9,NA,Afrodat6$Q97)
	table(Afrodat6$Education,exclude=NULL)


#Question Number: Q101
#Question: Respondent’s gender
#Variable Label: Q101. Gender of respondent
#Values: 1, 2
#Value Labels: 1=Male, 2=Female
#Source: SAB
#Note: Answered by interviewer
#fe_maleダミーに
Afrodat6$Gender_f<-ifelse(Afrodat6$Q101<0|Afrodat6$Q101>2,NA,Afrodat6$Q101-1)
	table(Afrodat6$Gender_f,exclude=NULL)
#   0     1 
#26801 27134 


#-------人種
#Question Number: Q102
#Question: Respondent’s race
#Variable Label: Q102. Race of respondent
#Values: 1-6, 95, -1
#Value Labels: 1=Black/African, 2=White/European, 3=Colored/Mixed Race, 4=Arab/Lebanese/North African, 5=South Asian (Indian, Pakistani, etc.), 6=Eat Asian (Chinese, Korean, Indonesian, etc.), Other=95, -1=Missing
#Source: SAB
#Note: Answered by interviewer
Afrodat6$Race<-ifelse(Afrodat6$Q102<0|Afrodat6$Q102>95,NA,Afrodat6$Q102)
	table(Afrodat6$Race,exclude=NULL)
#    1     2     3     4     5     6    95  <NA> 
#45981   361  1094  4856  1031     8   572    32 
#人種ダミー
Afrodat6$dRace_BAf<-ifelse(Afrodat6$Race==1,1,0)
Afrodat6$dRace_Wh<-ifelse(Afrodat6$Race==2,1,0)
Afrodat6$dRace_Col<-ifelse(Afrodat6$Race==3,1,0)
Afrodat6$dRace_Arab<-ifelse(Afrodat6$Race==4,1,0)
Afrodat6$dRace_SAs<-ifelse(Afrodat6$Race==5,1,0)
Afrodat6$dRace_EAs<-ifelse(Afrodat6$Race==6,1,0)
Afrodat6$dRace_Oth<-ifelse(Afrodat6$Race==95,1,0)



#保存
save(Afrodat6,file=“0Afrodat6.rda”)

#国別に集計してみる
Afrodat6g<-group_by(Afrodat6,Afrodat6$COUNTRY2)

m<-summarise(Afrodat6g,mNews_Radio=mean2(News_Radio),mNews_Television=mean2(News_Television),mNews_Newspaper=mean2(News_Newspaper),mNews_Internet=mean2(News_Internet),mNews_Social_media=mean2(News_Social_media),
mOwn_Radio=mean2(Own_Radio), mOwn_TV=mean2(Own_TV), mOwn_Auto=mean2(Own_Auto), mOwn_Mbphone=mean2(Own_Mbphone))
as.data.frame(m)
#edit(as.data.frame(m))

	
#edit(m)
write.table(m,file="0m.txt",row.names=F,sep="\t")

#plot.by.group(m)

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

(m<-summarise(AfrodatAllg,mNews_Radio<-mean2(News_Radio),mNews_Television<-mean2(News_Television),mNews_Newspaper<-mean2(News_Newspaper),mNews_Internet<-mean2(News_Internet),mNews_Social_media<-mean2(News_Social_media)))

edit(m)
head(m)
#Radio
dat<-m[,c(1:3)];names(dat)<-c("cnam","t","y")
	group_trend_plot(dat,lab="Radio")
dat<-m[,c(1:2,4)];names(dat)<-c("cnam","t","y")
	group_trend_plot(dat,lab="TV")

dat<-m[,c(1:2,5)];names(dat)<-c("cnam","t","y")
	group_trend_plot(dat,lab="Newspaper")

dat<-m[,c(1:2,6)];names(dat)<-c("cnam","t","y")
	group_trend_plot(dat,lab="Internet")

dat<-m[,c(1:2,7)];names(dat)<-c("cnam","t","y")
	group_trend_plot(dat,lab="Social_media ")



save.image("0Afrodat.img")
