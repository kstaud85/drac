#all of the words from dracula(not just top 10)

drac<-gutenberg_download(345)

drac_words<-drac%>%
  unnest_tokens(word,text)

bing<-get_sentiments('bing')

drac_words<-inner_join(drac_words,bing)

drac_words$gutenberg_id<-NULL


drac_pos<-drac_words%>%
  filter(sentiment=='positive')%>%
  group_by(word)%>% 
  summarize(count=n(),sentiment=first(sentiment))%>% 
  arrange(count)%>%
 

tail(drac_pos,n=20)
class(drac_pos$word)
head(drac_pos)
tail(drac_pos,n=20)

drac_pos$word<-factor(drac_pos$word,levels=drac_pos$word)
ggplot()+
  geom_bar(data=drac_pos,aes(x=word,y=count), stat='identity')+
  coord_flip()

#-----------------------------------------
head(drac_neg)
count(drac_neg)

drac_neg<-drac_words%>%
  filter(sentiment=='negative')%>%
  group_by(word)%>% 
  summarize(count=n(),sentiment=first(sentiment))%>% 
  arrange(count)%>%#order
  filter(word!='miss')%>%
  top_n(25,wt=count)
 
 

drac_neg$word<-factor(drac_neg$word,levels=drac_neg$word)

ggplot()+
  geom_bar(data=drac_neg,aes(x=word,y=count), stat='identity')+
  coord_flip()
#----------------------------------------------

drac_comp<-rbind(drac_pos,drac_neg)

ggplot()+
  geom_bar(data=drac_comp,aes(x=word,y=count,fill=sentiment,color=sentiment), stat ='identity')+
  coord_flip()+
  facet_wrap(~sentiment,scales='free_y')+ #what column
  scale_fill_manual(values=c('#0174DF','#088A08'))+
  scale_color_manual(values=c('#FF8000','#FF0000'))


