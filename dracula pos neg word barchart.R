drac<-gutenberg_download(345)

drac_words<-drac%>%
  unnest_tokens(word,text)

bing<-get_sentiments('bing')

drac_words<-inner_join(drac_words,bing)

drac_words$gutenberg_id<-NULL

drac_pos<-drac_words%>%
  filter(sentiment=='positive')%>%
  group_by(word)%>% 
  summarize(count=n(),sentiment=first(sentiment))%>% #take first sentiment and store into col "sentiment"
  arrange(count)%>%#order
  top_n(10,wt=count) #returns top 10 records in col "count"
 
tail(drac_pos,n=20)
class(drac_pos$word) #see its a character, want a factor

drac_pos$word<-factor(drac_pos$word,levels=drac_pos$word)#levels are how words are appearing as they are

ggplot()+
  geom_bar(data=drac_pos,aes(x=word,y=count), stat='identity')+
  coord_flip()
 #-------------------------------------
  #top neg words
 drac_neg<-drac_words%>%
   filter(sentiment=='negative')%>%
   group_by(word)%>% 
   summarize(count=n(),sentiment=first(sentiment))%>% 
   arrange(count)%>%#order
   filter(word!='miss')%>%
   top_n(10,wt=count)
 
 drac_neg$word<-factor(drac_neg$word,levels=drac_neg$word)
 
 ggplot()+
   geom_bar(data=drac_neg,aes(x=word,y=count), stat='identity')+
   coord_flip()
  
  
#-----------------------------------------------------
 
#combine two dataframes
drac_comp<-rbind(drac_pos,drac_neg)
 
ggplot()+
  geom_bar(data=drac_comp,aes(x=word,y=count,fill=sentiment,color=sentiment), stat ='identity')+
  coord_flip()+
  facet_wrap(~sentiment,scales='free_y')+ #what column
  scale_fill_manual(values=c('#0174DF','#088A08'))+
  scale_color_manual(values=c('#FF8000','#FF0000'))
  
 
