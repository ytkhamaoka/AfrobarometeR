#--------
#----Afrodatae wave 1-7にマージする世銀データの処理
#----2020/1/3 hamaoka@fbc.keio.ac.jp
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
		print(dat[,c("t","y","cnam")])		#最後のtのところにcountry nameを
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


#---World Bankの国データから必要な時点のみ取り出す→すべて使うことにしたのでこのルーチンは使わない
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

#	NAの数をカウント
n_NA<-function(x){
		sum(is.na(x))
		}

#---World Bankの国データにある時点データをすべてとりだす  
toLong<-function(dat,vnam){
	#vnam<-"Population, total";dat<-WBdat
	dd<-dat[dat$Series.Name==vnam,c(1,2,5,6:34)]	#2019まで列はあるが､ほとんどデータはないので除外
	names(dd);dim(dd)
# [1] "WB_Code"       "Afro_Code"     "Country.Name"  "1990.[YR1990]" "1991.[YR1991]" "1992.[YR1992]" "1993.[YR1993]" "1994.[YR1994]" "1995.[YR1995]"
#[10] "1996.[YR1996]" "1997.[YR1997]" "1998.[YR1998]" "1999.[YR1999]" "2000.[YR2000]" "2001.[YR2001]" "2002.[YR2002]" "2003.[YR2003]" "2004.[YR2004]"
#[19] "2005.[YR2005]" "2006.[YR2006]" "2007.[YR2007]" "2008.[YR2008]" "2009.[YR2009]" "2010.[YR2010]" "2011.[YR2011]" "2012.[YR2012]" "2013.[YR2013]"
#[28] "2014.[YR2014]" "2015.[YR2015]" "2016.[YR2016]" "2017.[YR2017]" "2018.[YR2018]" "2019.[YR2019]"
#[1] 37 33

	names(dd)[4:32]<-paste("y",seq(1990,2018),sep=".")		#変数名 .数字の後からtimeを類推する
	d0<-reshape(dd,direction = "long",varying=names(dd)[4:32],		
				idvar=c("Country.Name","WB_Code","Afro_Code"))		#long形式に
				head(d0)
#                           WB_Code Afro_Code  Country.Name time        y
#Burundi.BDI.BDI.1990           BDI       BDI       Burundi 1990  5438957
#Benin.BEN.BEN.1990             BEN       BEN         Benin 1990  4978496
#Burkina Faso.BFA.BFO.1990      BFA       BFO  Burkina Faso 1990  8811034

	vnam2<-strsplit(vnam," (",fixed=T)	# (以降の単位部分は除く
	vnam2<-gsub(" |,|%","_",vnam2[[1]])	#　,や%､スペースは_に
	names(d0)[4:5]<-c("year",vnam2)
#		d0[,5]<-as.numeric(d0[,5])	#characterになっているので数値に　　既にそうしてある
#		d0<-d0[order(d0[,c("Country.Name","year")]),]
	#trend plot
	dd<-d0[,c(3:5)]
	names(dd)<-c("cnam","t","y")
	group_trend_plot(dd,vnam)

#	print(by(d0[,5],d0$Country.Name,n_NA))
datg<-group_by(dd,dd$cnam)
	print(sm<-summarize(datg,N=n(),n_NA=sum(is.na(y)),mn=min2(y),mx=max2(y)))	#

	print(head(d0))
	return(d0)
	}

#-----yとtをあたえてトレンド推定　　tnew=seq(a,b)について内挿値を算出
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

	tnew2<-tnew^2
	newdat<-data.frame(tnew,tnew2)
#	newdat<-data.frame(tnew)
	names(newdat)<-c("t","t2")
#	names(newdat)<-c("t")
	yhat<-predict(res,newdat)

	dtnew<-data.frame(newdat,yhat)
	(dtnew<-merge(dtnew,dt2[,c("t","y")],by.x="t",by.y="t",all=T))
	dtnew$y2<-ifelse(is.na(dtnew$y),dtnew$yhat,dtnew$y)	#データがある場合はそれを､欠損の場合は内挿値を
	dtnew$fg.interpolate<-ifelse(is.na(dtnew$y),1,0)	#データがある場合はそれを､欠損の場合は内挿値を
	
#	plot(t,y,xlim=c(min(t),max(tnew)),ylim=c(min(y),max(yhat)))
	plot(t,y,xlim=c(1990,2018),ylim=c(min2(y),max2(y)))
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

#characterになっているので数値に
for (i in seq(5,34)){
	dat0[,i]<-as.numeric(dat0[,i])
	}
summary(dat0)

	

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

#----指標名を指定してとりだし　ロング形式に(欠損がおおい変数は除外)
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
#	dd<-toLong(WBdat,"GNI per capita, PPP (current international $)")
#		WBdatL<-merge(WBdatL,dd[,-c(1,2)],by.x=c("Country.Name","year"),by.y=c("Country.Name","year"))
	dd<-toLong(WBdat,"GDP per capita (current US$)")
		WBdatL<-merge(WBdatL,dd[,-c(1,2)],by.x=c("Country.Name","year"),by.y=c("Country.Name","year"))

#通信
	dd<-toLong(WBdat,"Individuals using the Internet (% of population)")
		WBdatL<-merge(WBdatL,dd[,-c(1,2)],by.x=c("Country.Name","year"),by.y=c("Country.Name","year"))
	dd<-toLong(WBdat,"Mobile cellular subscriptions")
		WBdatL<-merge(WBdatL,dd[,-c(1,2)],by.x=c("Country.Name","year"),by.y=c("Country.Name","year"))
	dd<-toLong(WBdat,"Fixed telephone subscriptions")
		WBdatL<-merge(WBdatL,dd[,-c(1,2)],by.x=c("Country.Name","year"),by.y=c("Country.Name","year"))
#	dd<-toLong(WBdat,"Fixed telephone subscriptions (per 100 people)")		#toLongルーチンでは　(以降を除外するので､上のと同じ変数名になるため除外
#		WBdatL<-merge(WBdatL,dd[,-c(1,2)],by.x=c("Country.Name","year"),by.y=c("Country.Name","year"))
#	dd<-toLong(WBdat,"Internet users (per 100 people)")
#		WBdatL<-merge(WBdatL,dd[,-c(1,2)],by.x=c("Country.Name","year"),by.y=c("Country.Name","year"))
#	インフラ
	dd<-toLong(WBdat,"Access to electricity (% of population)")
		WBdatL<-merge(WBdatL,dd[,-c(1,2)],by.x=c("Country.Name","year"),by.y=c("Country.Name","year"))
#	教育
	dd<-toLong(WBdat,"Primary completion rate, total (% of relevant age group)")			#こちらの方がNAが少ない
		WBdatL<-merge(WBdatL,dd[,-c(1,2)],by.x=c("Country.Name","year"),by.y=c("Country.Name","year"))
#	dd<-toLong(WBdat,"School enrollment, secondary (% gross)")
#		WBdatL<-merge(WBdatL,dd[,-c(1,2)],by.x=c("Country.Name","year"),by.y=c("Country.Name","year"))
#	dd<-toLong(WBdat,"Literacy rate, adult total (% of people ages 15 and above)")		#欠損が非常に多い国あり
#		WBdatL<-merge(WBdatL,dd[,-c(1,2)],by.x=c("Country.Name","year"),by.y=c("Country.Name","year"))
#	格差   いずれもNAが多いので除外
#	dd<-toLong(WBdat,"Income share held by lowest 20%")		#欠損多いが最低一つはデータあり
#		WBdatL<-merge(WBdatL,dd[,-c(1,2)],by.x=c("Country.Name","year"),by.y=c("Country.Name","year"))
#	dd<-toLong(WBdat,"GINI index (World Bank estimate)")		#欠損多いが
#		WBdatL<-merge(WBdatL,dd[,-c(1,2)],by.x=c("Country.Name","year"),by.y=c("Country.Name","year"))

	summary(WBdatL)
	head(WBdatL)
	names(WBdatL);dim(WBdatL)

# [1] "Country.Name"                   "year"                           "WB_Code"                        "Afro_Code"                     
# [5] "Population__total"              "Population_growth"              "Surface_area"                   "GDP"                           
# [9] "GDP_growth"                     "GDP_per_capita"                 "Individuals_using_the_Internet" "Mobile_cellular_subscriptions" 
#[13] "Fixed_telephone_subscriptions"  "Access_to_electricity"          "Primary_completion_rate__total"
#[1] 1073   15

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
						ydat0<-fit_trend(dd2$t0,dd2$y0,tnew=seq(1990,2018))	#y0の欠損を線形補完しつつ　2018までも外挿
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

	
summary(WBdatL2)
summary(WBdatL2[WBdatL2$year>=2000,])

names(WBdatL);dim(WBdatL)
names(WBdatL2);dim(WBdatL2)

WBdatL2<-merge(WBdatL[,c( "Country.Name","year","WB_Code","Afro_Code")],WBdatL2,by.x=c("Country.Name","year"),by.y=c("Country.Name","year"),all=T)
summary(WBdatL2)
names(WBdatL2);dim(WBdatL2)


save(WBdatL2,file="0WBdatL2.rda")		#long形式のデータ
edit(WBdatL2)

#NamibiaはPrimary_completion_rate__totalのデータがまったくないので内挿もできない

# [1] "Country.Name"                                  "year"                                          "WB_Code"                                      
# [4] "Afro_Code"                                     "Population__total2"                            "Population__total2.fg.interpolat"             
# [7] "Population_growth2"                            "Population_growth2.fg.interpolat"              "Surface_area2"                                
#[10] "Surface_area2.fg.interpolat"                   "GDP2"                                          "GDP2.fg.interpolat"                           
#[13] "GDP_growth2"                                   "GDP_growth2.fg.interpolat"                     "GDP_per_capita2"                              
#[16] "GDP_per_capita2.fg.interpolat"                 "Individuals_using_the_Internet2"               "Individuals_using_the_Internet2.fg.interpolat"
#[19] "Mobile_cellular_subscriptions2"                "Mobile_cellular_subscriptions2.fg.interpolat"  "Fixed_telephone_subscriptions2"               
#[22] "Fixed_telephone_subscriptions2.fg.interpolat"  "Access_to_electricity2"                        "Access_to_electricity2.fg.interpolat"         
#[25] "Primary_completion_rate__total2"               "Primary_completion_rate__total2.fg.interpolat"
#[1] 1073   26


#ラグ変数つくる
#国→年の順にならんでいる
#WBdatL3<-WBdatL2[order(c(WBdatL2$Country.Name,WBdatL2$year)),]	
#	head(WBdatL3)
	
m2<-WBdatL2[,5:26]
	dd<-rep(NA,dim(m2)[2])
m2<-rbind(dd,m2)		#1行下にずらす
m2<-m2[-dim(m2)[1],]	#最後の行は除く
#	edit(m2)
names(m2)<-paste("l",names(m2),sep="_")

WBdatL3<-cbind(WBdatL2,m2)
	names(WBdatL3)
	for(i in seq(2,dim(WBdatL3)[1])){
		if(WBdatL3$Country.Name[i]!=WBdatL3$Country.Name[i-1]){
			WBdatL3[i,27:48]<-NA
			}
		}

	edit(WBdatL3)
save(WBdatL3,file="0WBdatL3.rda")	##long形式のデータ　1年ラグ付き

