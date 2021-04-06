server <- function(input, output, session) {
    
    observe({
        
        #Processing the file importation
        progress <- shiny::Progress$new()
        progress$set(message = "File processing", value = 0)
        on.exit(progress$close())
        progress$set(detail = "2 minutes...", value = 0.5)
        data <- input$fichier
        if(is.null(data)){return(NULL)} 
        if(grepl("\\.zip$", data)=="TRUE"){
            df_data<-rwa_read(unzip(data$datapath,files="_chat.txt"))
        } else {
            df_data<-rwa_read(data$datapath)
        }
        
        
        
        
        #Processing the valuebox informations
        ########################################################################
        output$n_messages <- renderInfoBox({
            nrow <-NROW(df_data)
            valueBox(prettyNum(nrow, big.mark = ","), subtitle = tags$p(strong("Total number of messages"),
                                                                        style="font-family:cambria;font-size: 100%;font-weight: bold"),
                     icon = icon("envelope"),
                     color = "green"
            )
        })
        
        ##########################################################################
        #Processing this kind of information for french and english languages
        output$n_sup_mess <- renderInfoBox({
            msgdeleted="This message was deleted" 
            msg_you="You deleted this message"
            msg_eff="Ce message a été supprimé"
            msg_vous="Vous avez supprimé ce message"
            msg1=grep(msgdeleted, df_data$text,value=T)
            msg2=grep(msg_eff, df_data$text,value=T)
            msg3=grep(msg_vous, df_data$text,value=T)
            msg4=grep(msg_you, df_data$text,value=T)
            
            if(length(msg2)!=0){
                
                msg2=grep(msg_eff, df_data$text,value=T)
                print(length(msg2)+length(msg3))%>%
                    valueBox(subtitle = tags$p(strong("Number of deleted messages"),
                                               style="font-family:cambria;font-size: 100%;font-weight: bold"),
                             icon = icon("trash"),
                             color = "yellow"
                    )
            } else {
                msg1=grep(msgdeleted, df_data$text,value=T)
                print(length(msg1)+length(msg4))%>%
                    valueBox(subtitle = tags$p(strong("Number of deleted messages"),
                                              style="font-family:cambria;font-size: 100%;font-weight: bold"),
                             icon = icon("trash"),
                             color = "yellow"
                    )
            }
            
            
        })
        ######################################################################### 
        
        
        
        output$n_author <- renderInfoBox({
            df_data$author %>% 
                nlevels() %>% 
                valueBox(subtitle = tags$p(strong("Number of participants in discussions"),
                                          style="font-family:cambria;font-size: 100%;font-weight: bold"),
                         icon = icon("users"),
                         color = "purple"
                )
        })
        
        
        ##########################################################################
        
        #Plotting the series (time series graph) of messages per day
        output$plot_seri_chrono <- renderPlotly({
            l_year<-df_data%>%
                mutate(day = as.Date.factor(time)) %>%
                mutate(year=year(day))%>%
                select(year)
            l_year<-min(l_year[,1])
            
            shiny::validate(
                need(input$limit_year >= l_year,
                     paste0("Please, select a higher value for the minimal year. The minimal year detected in your data is", l_year,"!")))
                   
            ggplotly(df_data %>%
                mutate(day = as.Date.factor(time)) %>%
                mutate(year=year(day))%>%
                filter(!is.na(year))%>%
                filter(year>=input$limit_year)%>%
                count(day) %>%
                ggplot(aes(x = day, y = n),fill="#00AFBB")+
                geom_line(stat = "identity", 
                          color= "#00AFBB",size=0.5) +
                ylab("Message frequency per day") +
                xlab("Date") +
                  labs(caption = "@Lucainson RAYMOND")+
                    theme_classic()+
                    theme(
                        axis.title.x = element_text(family="Cambria",size=9),
                        axis.title.y = element_text(family="Cambria",size=9))
            )
            
        })
        
        
        ############################################################################
        
        #Plotting the domain name of site web/link shared
        url_pattern <- "http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+"
        df_data$ContentURL <- str_extract(df_data$text, url_pattern)
        df_data$url<-suffix_extract(domain(df_data$ContentURL))$host 
        
        output$plot_link_shared <- renderPlot({
          df_data %>%
                mutate(day = as.Date.factor(time)) %>%
                count(url)%>%
                filter(!is.na(url))%>%
                top_n(n = input$max_link, n) %>%
                ggplot(aes(fill=url,x = reorder( url, n), y = n)) +
                geom_bar(stat = "identity",color="black",show.legend = F) +
            scale_fill_manual(values=c("black","#FF5D00","#C8102E", 
                                       "#A2A569","#38170B","#BF1B0B",
                                       "#8B5B29", "#B9975B", "#EED484",
                                       "#6E6259", "#707372", "#ACA39A",
                                       "#F1BE48", "#524727", "#76881D",
                                       "#9B945F", "#CAC7A7","#3E4827",  
                                       "#003D4C", "#006BA6", "#7A99AC",
                                       "#7C2529", "#9A3324", "#BE531C",
                                       "#999999", "#E69F00", "#56B4E9",
                                       "#FFC465", "#66ADE5", "#252A52"))+
                ylab("Frequency") + xlab("") +
                coord_flip() +
                ggtitle("")+
                labs(caption = "@Lucainson RAYMOND")+
                theme(
                    axis.title.x = element_text(family="Cambria",size=9),
                    axis.title.y = element_text(family="Cambria",size=9))+
            theme_classic()
            
        })
        
        
        
        #######################################################################################
        
        #Plotting the statistics of multimedia files shared
        output$plot_stat <- renderPlotly({
            
            if(input$tel_type=="ios"){
                
                image1=".*image omitted$"
                video1=".*video omitted$"
                document1=".*document omitted$"
                contact1=".*Contact card omitted$"
                audio1=".*audio omitted$"
                gif1=".*GIF omitted$"
                sticker1=".*sticker omitted$"
                
                
                image2=".*image absente$"
                video2=".*vidéo absente"
                document2=".*document manquant$"
                contact2=".*Fiche contact manquante$"
                audio2=".*audio omis$"
                gif2=".*GIF retiré$"
                sticker2=".*sticker omis$"
                
                
                #En utilisant maintenant la fonction grep() qui est une fonction du paquet "base" de R, nous allons extraire l'ensemble desdites traces comme suit
                im1=grep(image1, df_data$text)
                vid1=grep(video1, df_data$text)
                doc1=grep(document1, df_data$text)
                cont1=grep(contact1, df_data$text)
                aud1=grep(audio1, df_data$text)
                gf1=grep(gif1, df_data$text)
                stick1=grep(sticker1, df_data$text)
                
                im2=grep(image2, df_data$text)
                vid2=grep(video2, df_data$text)
                doc2=grep(document2, df_data$text)
                cont2=grep(contact2, df_data$text)
                aud2=grep(audio2, df_data$text)
                gf2=grep(gif2, df_data$text)
                stick2=grep(sticker2, df_data$text)
                
                #Next, we build a vector of data with the frequency of sharing each category of files while formally labeling them for ease of reading
                media1<-c(length(im1),length(vid1),
                          length(doc1),length(cont1),length(aud1),
                          length(gf1),length(stick1))
                
                media2<-c(length(im2),length(vid2),
                          length(doc2),length(cont2),length(aud2),
                          length(gf2),length(stick2))
                
                labels<-c("Images","Videos",
                          "Documents","Contact cards",
                          "Audios","GIF","Stickers")
                
                med1<-tibble(media1,labels)
                med1 <-med1[order(med1$media1, decreasing = T),]
                med2<-tibble(media2,labels)
                med2 <-med2[order(med2$media2, decreasing = T),]
                
                #Let's project the graphs according to the selection conditions
                
                if(sum(med1[,1])!=0){
                  
                  med1<-med1%>%
                    filter(!media1 %in% 0)
                  fig <- plot_ly(med1, labels = ~labels, values = ~media1, type = 'pie',textinfo='label+percent',
                                 insidetextorientation='radial')
                  
                  fig 
                    
                } else{

                  med2<-med2%>%
                    filter(!media2 %in% 0)
                  fig <- plot_ly(med2, labels = ~labels, values = ~media2, type = 'pie',textinfo='label+percent',
                                 insidetextorientation='radial')
                  
                  fig 
                    
                }
            } else{
              
              shiny::validate(
                need(input$tel_type=="ios", "Sorry! This option isn't available for android phone."))
              
            }
            
            
            
        })
        
        ################################################################################
        
        #Plotting the top most active users
        
        output$plot_top_users <- renderPlot({
            
            if(input$num_users<9){
            df_data %>%
                mutate(day = as.Date.factor(time)) %>%
                count(author)%>%
                filter(!is.na(author))%>%
                top_n(n = input$num_users, n) %>%
                ggplot(aes(x = reorder(author, n), y = n,fill=author)) +
                geom_bar(stat = "identity",show.legend = F) +
                scale_fill_brewer(palette = "Dark2", 
                                     direction = 1) +
                ylab("Frequency") + 
                xlab("") +
                coord_flip() +
                ggtitle("")+
                labs(caption = "@Lucainson RAYMOND")+
                theme(plot.title = element_text((hjust=0)))+
            theme_bw()
                
            } else {
                
                getpalette=colorRampPalette(brewer.pal(8,'Dark2'))
                
                df_data %>%
                    mutate(day = as.Date.factor(time)) %>%
                    count(author)%>%
                    filter(!is.na(author))%>%
                    top_n(n = input$num_users, n) %>%
                    ggplot(aes(x = reorder(author, n), y = n,fill=getpalette(input$num_users))) +
                    geom_bar(stat = "identity",show.legend = F) +
                    ylab("") + 
                    xlab("") +
                    coord_flip() +
                    ggtitle("")+
                    labs(caption = "@Lucainson RAYMOND")+
                    theme(plot.title = element_text((hjust=0)))+
                    theme_bw()
                
            }
            
        })
        
        ###############################################################
        
        
        #Plotting User activity by time of day
        
        output$plot_act_heure<-renderPlot({
            
            if(input$freq_ch=="Median"){
            
                d<-df_data %>%
                mutate(hour = hour(time)) %>%
                mutate(day = as.Date(time)) %>%
                select(hour,day)%>%
                group_by(hour,day)%>%
                count()%>%
                ungroup()%>%
                select(-day)%>%
                group_by(hour)%>%
                summarise(n=round(median(n),0))
            
            } else if(input$freq_ch=="Average"){
                
                d<-df_data %>%
                    mutate(hour = hour(time)) %>%
                    mutate(day = as.Date(time)) %>%
                    select(hour,day)%>%
                    group_by(hour,day)%>%
                    count()%>%
                    ungroup()%>%
                    select(-day)%>%
                    group_by(hour)%>%
                    summarise(n=round(mean(n),0))
                
            } else {
                
            d<-df_data %>%
                    mutate(hour = hour(time)) %>%
                group_by(hour)%>%
                    count(hour) 
            }
            
           d$hour[d$hour=="0"]<-"24h"
           d$hour[d$hour=="1"]<-"1h"
           d$hour[d$hour=="2"]<-"2h"
           d$hour[d$hour=="3"]<-"3h"
           d$hour[d$hour=="4"]<-"4h"
           d$hour[d$hour=="5"]<-"5h"
           d$hour[d$hour=="6"]<-"6h"
           d$hour[d$hour=="7"]<-"7h"
           d$hour[d$hour=="8"]<-"8h"
           d$hour[d$hour=="9"]<-"9h"
           d$hour[d$hour=="10"]<-"10h"
           d$hour[d$hour=="11"]<-"11h"
           d$hour[d$hour=="12"]<-"12h"
           d$hour[d$hour=="13"]<-"13h"
           d$hour[d$hour=="14"]<-"14h"
           d$hour[d$hour=="15"]<-"15h"
           d$hour[d$hour=="16"]<-"16h"
           d$hour[d$hour=="17"]<-"17h"
           d$hour[d$hour=="18"]<-"18h"
           d$hour[d$hour=="19"]<-"19h"
           d$hour[d$hour=="20"]<-"20h"
           d$hour[d$hour=="21"]<-"21h"
           d$hour[d$hour=="22"]<-"22h"
           d$hour[d$hour=="23"]<-"23h"
            
            d%>%
                ggplot(aes(x = reorder(hour, -n),y=n))+
                geom_bar(stat = "identity", color="black",
                         fill="#00AFBB") +
              labs(caption = "@Lucainson RAYMOND")+
                ylab("") + 
                xlab("") +
                theme(plot.title = element_text((hjust=0.5)))+
            theme_classic()
        })
        
        #########################################################################
        
        output$plot_act_jour<-renderPlot({
            
            if(input$freq_ch=="Median"){
              #Distribution of messages by day of the week (median number)
            df_data %>%
                mutate(x = as.Date(time)) %>%
                select(x)%>%
                group_by(x)%>%
                count()%>%
                mutate(day=weekdays(x))%>%
                mutate(dayx=weekdays(x))%>%
                ungroup()%>%
                select(-x)%>%
                group_by(day)%>%
                summarise(n=round(median(n),0))%>%
                ggplot(aes(x = reorder(day, n), y = n))+
                geom_bar(stat = "identity", color="black",
                         fill="#4A9260") +
                labs(caption = "@Lucainson RAYMOND")+
                ylab("") +
                xlab("") +
                theme(plot.title = element_text((hjust=0.5)))+
                theme_classic()
                
            } else if (input$freq_ch=="Average"){
                
              #Distribution of messages by day of the week (average number)
                df_data %>%
                    mutate(x = as.Date(time)) %>%
                    select(x)%>%
                    group_by(x)%>%
                    count()%>%
                    mutate(day=weekdays(x))%>%
                    mutate(dayx=weekdays(x))%>%
                    ungroup()%>%
                    select(-x)%>%
                    group_by(day)%>%
                    summarise(n=round(mean(n),0))%>%
                    ggplot(aes(x = reorder(day, n), y = n))+
                    geom_bar(stat = "identity", color="black",
                             fill="#4A9260") +
                    labs(caption = "@Lucainson RAYMOND")+
                    ylab("") +
                    xlab("") +
                    theme(plot.title = element_text((hjust=0.5)))+
                    theme_classic()
                
            } else {
                #Overall
                df_data %>%
                    mutate(day = weekdays(time)) %>%
                    count(day) %>%
                    ggplot(aes(x = reorder(day, n), y = n))+
                    geom_bar(stat = "identity", color="black",
                             fill="#4A9260") +
                    labs(caption = "@Lucainson RAYMOND")+
                    ylab("") +
                    xlab("") +
                    theme(plot.title = element_text((hjust=0.5)))+
                    theme_classic()
            }
                
                
        })
        
        ##############################################################################
        
        #We will create a Heatmap for easy viewing of a bivariate distribution (message per day and per hour)
        # Creation of two (2) additional columns in the original dataset
        
        output$plot_heure_jour<-renderPlot({
            
            df_data$hour<-hour(df_data$time)
            df_data$weekdays<-weekdays(df_data$time, abbreviate=F)
      
            df <- df_data %>% 
                    group_by(weekdays, hour) %>% 
                    summarise(n=n()) %>% 
                    rename(`message flow`=n)
            
#Transforming numbers in hour format
            df$hour[df$hour=="0"]<-"24h"
            df$hour[df$hour=="1"]<-"1h"
            df$hour[df$hour=="2"]<-"2h"
            df$hour[df$hour=="3"]<-"3h"
            df$hour[df$hour=="4"]<-"4h"
            df$hour[df$hour=="5"]<-"5h"
            df$hour[df$hour=="6"]<-"6h"
            df$hour[df$hour=="7"]<-"7h"
            df$hour[df$hour=="8"]<-"8h"
            df$hour[df$hour=="9"]<-"9h"
            df$hour[df$hour=="10"]<-"10h"
            df$hour[df$hour=="11"]<-"11h"
            df$hour[df$hour=="12"]<-"12h"
            df$hour[df$hour=="13"]<-"13h"
            df$hour[df$hour=="14"]<-"14h"
            df$hour[df$hour=="15"]<-"15h"
            df$hour[df$hour=="16"]<-"16h"
            df$hour[df$hour=="17"]<-"17h"
            df$hour[df$hour=="18"]<-"18h"
            df$hour[df$hour=="19"]<-"19h"
            df$hour[df$hour=="20"]<-"20h"
            df$hour[df$hour=="21"]<-"21h"
            df$hour[df$hour=="22"]<-"22h"
            df$hour[df$hour=="23"]<-"23h"
            
            # So we display the data using the graphical grammar from the ggplot2 package
            ggplot(df, aes(hour,weekdays)) + 
                geom_tile(aes(fill = `message flow`),
                          colour = "white") + 
                geom_text(aes(label=`message flow`),show.legend = F)+
                scale_fill_distiller(palette = "Dark2", 
                                     direction = 1) + 
                scale_x_discrete(breaks = df$hour,
                                 labels = df$hour) + 
                labs(caption = "@Lucainson RAYMOND")+
                theme(legend.position = "None",
                      panel.grid = element_blank()) + 
                coord_equal()+
                theme_bw()
            
            
        })
        
        
        
        ################################################################################### 
        #By a simple transformation of the "time" column of the original dataset,
        #we are going to create the 4 moments, in this case,
        #Morning (6h-12h), Afternoon (12h-16h), Evening (Night) & Mid-night (0h-6h)
        #Next, we will highlight which of the Users
        #are most active during said moments
        
        output$plot_moment_jour<-renderPlot({
            
            times=df_data%>%mutate(time=hour(time))
            times=times %>% mutate(Moment = case_when(
                time > 6 & time <= 12~ 'Morning',
                time > 12 & time <= 16 ~ 'Afternoon', 
                time > 16 & time <= 21 ~ 'Evening',
                time > 21 & time <= 24 ~ 'Night',
                time > 0 & time <= 6~ 'Mid Night'))
            
            #Next, we run the algorithm by combining the functions in
            #several packages of R, including dplyr [count (), filter (), ungroup (), top_n ()],
            # then the layers of ggplot2 [ggplot (), geom_bar (), facet_wrap () ...]
            
            times%>%count(author,Moment)%>%
                
                filter(!is.na(author))%>%
                group_by(Moment)%>%filter(!is.na(Moment))%>%
                top_n(n = input$num_ajust, n) %>%
                ggplot(aes(x=reorder(author,n),y=n,fill=Moment))+
                geom_bar(stat="identity",show.legend = F)+
                facet_wrap(~Moment,ncol=2,scales="free")+
                coord_flip()+
                labs(x="",y="",caption = "@Lucainson RAYMOND")+
                theme(plot.title = element_text((hjust=0)))+
                theme_bw()
            
        })
        ####################################################################################################
        
        
        
        #So now, we are going the transform our text in a corpus
        #Let's start by cleaning it, by drop some useless words (stopwords) in french, english and haitian creole (our native language)
        to_remove1 <- c(stopwords(kind = "fr"), stopwords(kind="en"),
                        "media",
                        "omitted","image omitted",
                        "ref","manquant","vidéo","remplacé","amp","groupe","group","très",
                        "dass","lap","appuyez","intégré","téléphone","numéro","bout","en",
                        "schon","me","dak","met","fok","aprs","anne","chiffrés","chiffré","changé","ajouter",
                        "mal", "janvier","marchs", "age","send","pages","participants","Participants",
                        "android.s.wt","message","document","image","nn","Oserlecrire", "messages",
                        "sticker","yo", "de","pa","la","se","ki","c","gen","mw","nou","add","envoyer",
                        "ou","li","pou","w","nan","yon","nou","k","a","men","f","ke","retiré",
                        "ak","an","sou","tout","menm","si","e","https","ap","tou","paske","settings","dirk","omis",
                        "epi","epwi","lan","mpa","non","mwen","ka","we","fè","tt","fe","stp","gon","gen","moun","ion",
                        "konn","jan","s'on","anpil","oui","wap","épi","plus","pi","c'est","ion","yon","youn","sa",
                        "lot","lè","oubyen","tap","oswa","ds","too","ave","etre","ui","mte","pouw","t","absente","sent",
                        "tj","u","jus","jis","ete","w'ap","oserlecrire","g'on","lol",'poun',"infos","sak","fè","supprim",
                        "video","deleted","add","changed","pdf","page","phone","number","take","new","waiting",
                        "que","qui","pap","pat","nan","toujours","it","i","of",'you',"very","and","donk","san","son",
                        "chak","ns","etc","fé","saaa","laa","laaa","mgen","nap","htg","usd","2350","2019",
                        "kap",'al',"this","the","supprim","supprimé","for","l","while","may","peut","peuvent","yap","pour","toujou",
                        "mar","to","ti","di","wi","ye","2","have","are","dil","masse","fel","map","ok","fon","anh","svp",                    "dim","2500","my","forms.gle","that","siw","d'i", "meme","oserlecrire","2020","25","2","50933976027","o",
                        "greg","in","sonw","sak","deux","deuxime","deuxième","faire",'fait','fait faire',"wimax",
                        "wifi", "Marchs","allow","allows","Clavens","clavens","formulaire","formulaires",
                        "group","Wilbens","wilbens","association","Association","atwood","salutations","left","Marchs","Administration","administration",
                        "added","untitled","annuel","collects", "p","io","ui","uii","uiii","pral","509","poum","poun",
                        "konsa","bn","GIF","gif","audio","Audio","bout","en","chiffrement","ann","hein","via","one","two",
                        "mage","Omitted","video","Video","dirk","2400")
        
        
        
       #Let's clean up the text again, removing other non-informative "regex" (url, numbers, useless white spaces, punctuation ...)
        whatsapp <- data.frame(text=df_data$text)
        whatsapp$text <- as.character(whatsapp$text)
        whatsapp$text <- gsub('\\p{So}|\\p{Cn}', '', whatsapp$text, perl = TRUE)
        whatsapp$text <- gsub('http\\S+\\s*', '', whatsapp$text)
        whatsapp$text <- gsub("[[:digit:]]", '', whatsapp$text)
        whatsapp$text <- gsub('\\b+RT', '', whatsapp$text)
        whatsapp$text <- gsub('#\\S+', '', whatsapp$text)
        whatsapp$text <- gsub('@\\S+', '', whatsapp$text)
        whatsapp$text <- gsub("+509\\S+", '', whatsapp$text)
        whatsapp$text <- gsub('[[:cntrl:]]', '', whatsapp$text)
        whatsapp$text <- gsub("\\d", '', whatsapp$text)
        whatsapp$text <- gsub("[:graph:]]", '', whatsapp$text)
        whatsapp$text <- gsub("<(.+)>", '', whatsapp$text)
        whatsapp$text <- gsub('<p{So}|>p{Cn}', '', whatsapp$text, perl = TRUE)
        whatsapp$text <- gsub("\uFFFD", "", whatsapp$text, fixed=TRUE)

        #pattern<-"[[:xdigit:]]"
        
        #whatsapp1<-whatsapp%>%
            #mutate(aa=grep(pattern, whatsapp$text, value = TRUE))
        

        
        # Specification of a function to lower case all tokens (1-gram) of the text
        tryTolower <- function(x){
          # return NA when there is an error
            y=NA
            # tryCatch error
            try_error= tryCatch(tolower(x),error=function(e) e)
            if (!inherits(try_error, 'error'))
                y=tolower(x)
            
            return(y)
        }
        
        #Cleaning (continued)
        custom.stopwords <- c(stopwords('english'),to_remove1)
        clean.corpus <- function(corpus) {
            corpus <- tm_map(corpus,
                             content_transformer(tryTolower))
            corpus <- tm_map(corpus,removeWords,
                             custom.stopwords)
            corpus<-tm_map(corpus,removePunctuation)
            corpus <- tm_map(corpus,stripWhitespace)
            corpus <- tm_map(corpus,removeNumbers)
            return(corpus)
        }
        corpus <- Corpus(VectorSource(whatsapp$text))
        corpus <- clean.corpus(corpus)
        #corpus<-tm_map(corpus, function(x) iconv(x, "latin1", "ASCII", sub=""))
        
        
        
        #Conversion now of the corpus into a Document-Term-Matrix object
        dtm <- DocumentTermMatrix(corpus)
        #Removing some sparse terms in our matrix
        dtm<-removeSparseTerms(dtm, sparse=0.994)
        
        #Preparing the text corpus for plotting a wordcloud
        pattern<-"[[:xdigit:]]"
        
        wh_td <- tidy(dtm)
        
        w<-grep(pattern, wh_td$term, value = TRUE)%>%
            as.data.frame()
         w$x<-1
        output$word_cloud_plot <- renderWordcloud2({

            wh_words <- w %>%
                group_by(as.factor(.))%>%
                count(sort=TRUE)%>%
                filter(n >= input$w_cloud_freq)
            
            shiny::validate(
                need(input$w_cloud_freq < wh_words[1,2], "Please, select a lower value for the minimal frequency."))
            #Wordcloud
            set.seed(28)
            wordcloud2(data=wh_words,size=0.8,fontFamily = "Cambria",
                       color = "random-light",backgroundColor = "black")
            
        })
        
        #######################################################################################   
        
        #NGRAM ANALYSIS
        
        
        #Bag-of-word barplot
        output$word_freq_plot <- renderPlot({ 
            if(input$hist_gram=="Uni-gram"){
                
              progress <- shiny::Progress$new()
              progress$set(message = "Ngram analysis", value = 0.2)
              on.exit(progress$close())
              progress$set(message = "Computing...", value = 0.4)
              
                data.frame(text = sapply(corpus, as.character), stringsAsFactors = FALSE) %>% 
                    unnest_tokens(output = ngram, input = text, token="ngrams",n=1) %>% 
                    count(ngram, sort = TRUE) %>% 
                    arrange(desc(n))%>%
                  filter(!is.na(ngram))%>%
                    slice(1:20) %>% 
                    ggplot(aes(fct_reorder(ngram, n), n)) + 
                    geom_col(aes(fill=n), show.legend = FALSE, width = .1) +
                    geom_point(aes(color=n), show.legend = FALSE, size = 3)+
                    coord_flip()+
                    theme_minimal() +
                    labs(x="N-gram", y="Frequence",title = "Bag-of-Words anlysis of the corpus",
                         caption = "@Lucainson RAYMOND")+
                    scale_fill_gradient(low="#2b83ba",high="darkred") +
                    scale_color_gradient(low="#2b83ba",high="#d7191c") +
                  theme_bw()
                
            } else if(input$hist_gram=="Bi-gram"){
                  #Bi-gram barplot
              progress <- shiny::Progress$new()
              progress$set(message = "Ngram analysis", value = 0.2)
              on.exit(progress$close())
              progress$set(message = "Computing...", value = 0.4)
              
                        
                data.frame(text = sapply(corpus, as.character), stringsAsFactors = FALSE) %>% 
                    unnest_tokens(output = ngram, input = text, token="ngrams",n=2) %>% 
                    count(ngram, sort = TRUE) %>% 
                    arrange(desc(n))%>%
                  filter(!is.na(ngram))%>%
                  slice(1:20) %>% 
                    ggplot(aes(fct_reorder(ngram, n), n)) + 
                    geom_col(aes(fill=n), show.legend = FALSE, width = .1) +
                    geom_point(aes(color=n), show.legend = FALSE, size = 3)+
                    coord_flip()+
                    theme_minimal() +
                    labs(x="N-gram", y="Frequency",title = "Bigram anlysis of the corpus",
                         caption = "@Lucainson RAYMOND")+
                    scale_fill_gradient(low="#2b83ba",high="darkred") +
                    scale_color_gradient(low="#2b83ba",high="#d7191c") +
                  theme_bw()
            } else {
              #Tri-gram barplot
                        progress <- shiny::Progress$new()
                        progress$set(message = "Ngram analysis", value = 0.2)
                        on.exit(progress$close())
                        progress$set(message = "Computing...", value = 0.4)
                        
                data.frame(text = sapply(corpus, as.character), stringsAsFactors = FALSE) %>% 
                    unnest_tokens(output = ngram, input = text, token="ngrams",n=3) %>% 
                    count(ngram, sort = TRUE) %>% 
                    arrange(desc(n))%>%
                  filter(!is.na(ngram))%>%
                  slice(1:20) %>% 
                    ggplot(aes(fct_reorder(ngram, n), n)) + 
                    geom_col(aes(fill=n), show.legend = FALSE, width = .1) +
                    geom_point(aes(color=n), show.legend = FALSE, size = 3)+
                    coord_flip()+
                    theme_minimal() +
                    labs(x="N-gram", y="Frequency",title = "Trigram anlysis of the corpus",
                         caption = "@Lucainson RAYMOND")+
                    scale_fill_gradient(low="#2b83ba",high="darkred") +
                    scale_color_gradient(low="#2b83ba",high="#d7191c") +
                  theme_bw()
            }  
            
        })
        
        #########################################################################################    
        
        #SENTIMENT ANALYSIS
        
        # HTML page fetch emoji sentiment ranking 1.0
        url_base <- "http://kt.ijs.si/data/Emoji_sentiment_ranking/index.html"
        doc <- read_html(url_base)
        # Search emoji table and process
        tab_emojis <- doc %>% 
          html_node("#myTable") %>% 
          html_table() %>% 
          as_tibble()
        
        # Get Feeling score and clean up
        sentiment_emoji <- tab_emojis %>% 
          select(1,6:9) %>% 
          set_names("char", "négatif","neutre","positif","sent.score")
        # EXTRACT EMOJI AND UNITE WITH FEELING
        emoji_chat <- df_data %>% 
          unnest(emoji, emoji_name) %>% 
          mutate( emoji = str_sub(emoji, end = 1)) %>% 
          inner_join(sentiment_emoji, by=c("emoji"="char"))

        
        #Plotting the sentiment analysis graph
        output$plot_pol_sent <- renderPlotly({ 
            
            #OCCURRENCES OF FEELINGS BY EMOJIS, BY USER
            emoji_sentiment_user <- emoji_chat %>% 
                group_by(author) %>% 
                filter(!is.na(author))%>%
                summarise(
                    positive=mean(positif),
                    negative=mean(négatif),
                    neutral=mean(neutre),
                    balance=mean(sent.score)
                ) %>% 
                arrange(desc(balance))
           
            # DATA FORMAT FOR PLOTING
            b<-emoji_sentiment_user %>% 
                mutate( negative  = -negative,
                        neutral.positive =  neutral/2,
                        neutral.negative = -neutral/2) %>% 
                select(-neutral) %>% 
                gather("sentiment","mean", -author, -balance) %>% 
                mutate(sentiment = factor(sentiment, levels = c("negative", "neutral.negative", "positive", "neutral.positive"), ordered = T)) %>% 
                ggplot(aes(x=reorder(author,balance), y=mean, fill=sentiment)) +
                geom_bar(position="stack", stat="identity", show.legend = F, width = .5) +
                scale_fill_manual(values = brewer.pal(4,"RdYlGn")[c(1,2,4,2)]) +
                coord_flip() +
                ylab(" - Negative / Neutral/ Positive +") + xlab("") +
                ggtitle("Based on Emoji Expressions") +
                labs(caption = "@Lucainson RAYMOND")+
                theme(plot.title = element_text(family="cambria", size = 7,hjust=0.5,vjust=0.5),
                      axis.title.x = element_text(family = "Cambria",size=7),
                      axis.title.y = element_text(family = "Cambria",size=7)
                      )+
                theme_bw()
            
            ggplotly(b, tooltip=c("author","mean","sentiment"))
        })
        
        

        
        
        #######################################################################################
        
        #Plotting user lexical diversity
        
        output$plot_div_lex<-renderPlot({
            df_data %>%
                unnest_tokens(input = text,
                              output = word) %>%
                filter(!word %in% to_remove1) %>%
                group_by(author) %>%
                filter(!is.na(author))%>%
                summarise(lex_diversity = n_distinct(word)) %>%
                arrange(desc(lex_diversity)) %>%
                top_n(n = input$max_div, lex_diversity)%>%
                ggplot(aes(x = reorder(author, lex_diversity),
                           y = lex_diversity,
                           fill = author)) +
                geom_col(show.legend = FALSE) +
                scale_y_continuous(expand = (mult = c(0, 0, 0, 500))) +
                #geom_text(aes(label = scales::comma(lex_diversity)), hjust = -0.1) +
                ylab("") +
                xlab("") +
                labs(title="Richness of user's vocabulary",caption="@Lucainson RAYMOND") +
                theme(plot.title = element_text((hjust=0.5)))+
                coord_flip()+
                theme_classic()
            
        })
        
        ##############################################################################
        
        #Plotting Predilection words of users
        output$plot_predilec<-renderPlot({
          #Model-based approach: tf-idf
            if(input$word_div_lex=="TF-IDF"){
                
                auth<- df_data %>%
                    mutate(day = as.Date.factor(time)) %>%
                    count(author)%>%
                    filter(!is.na(author))%>%
                    arrange(-n)%>%
                    top_n(n = 6, n)
                
                
                df_data %>%
                    unnest_tokens(input = text,
                                  output = word) %>%
                    select(word, author) %>%
                    filter(!word %in% to_remove1) %>%
                    mutate(word = gsub(".com", "", word)) %>%
                    mutate(word = gsub("^gag", "9gag", word)) %>%
                    mutate(word = gsub("[[:digit:]]", "", word))%>%
                    filter(!word %in% "")%>%
                    count(author, word, sort = TRUE) %>%
                    filter(!is.na(author))%>%
                    bind_tf_idf(term = word, document = author, n = n) %>%
                    group_by(author) %>%
                    filter(author %in% auth$author) %>%
                    top_n(n = 5, tf_idf) %>%
                    ggplot(aes(x = reorder_within(word, n, author), y = n, fill = author)) +
                    geom_col(show.legend = FALSE) +
                    ylab("") +
                    xlab("") +
                    labs(caption = "@Lucainson RAYMOND")+
                    coord_flip() +
                    facet_wrap(~author, ncol = 2, scales = "free_y") +
                    scale_x_reordered() +
                    ggtitle("Model-based approach: tf-idf")+
                    theme(plot.title = element_text((hjust=0.5)))+
                    theme_bw()
                
            } else if (input$word_div_lex=="Unweighted Frequency"){
                
                #Unweighted frequency
                auth<- df_data %>%
                    mutate(day = as.Date.factor(time)) %>%
                    count(author)%>%
                    filter(!is.na(author))%>%
                    arrange(-n)%>%
                    top_n(n = 6, n)
                
                df_data %>%
                    unnest_tokens(input = text,
                                  output = word) %>%
                    filter(!word %in% to_remove1) %>%
                    mutate(word = gsub(".com", "", word)) %>%
                    mutate(word = gsub("^gag", "9gag", word)) %>%
                    mutate(word = gsub("[[:digit:]]", "", word))%>%
                    filter(!word %in% "")%>%
                    count(author, word, sort = TRUE) %>%
                    group_by(author) %>%
                    top_n(n = 5, n) %>%
                    filter(!is.na(author))%>%
                    filter(author %in% auth$author)%>%
                    ggplot(aes(x = reorder_within(word, n, author), y = n, fill = author)) +
                    geom_col(show.legend = FALSE) +
                    ylab("") +
                    xlab("") +
                    labs(caption = "@Lucainson RAYMOND")+
                    coord_flip() + 
                    facet_wrap(~author, ncol = 2, scales = "free_y") +
                    scale_x_reordered() +
                    ggtitle("")+
                    theme_bw()

            } else {
              #Model-based approach: Weighted log odds ratio
              auth<- df_data %>%
                mutate(day = as.Date.factor(time)) %>%
                count(author)%>%
                filter(!is.na(author))%>%
                arrange(-n)%>%
                top_n(n = 6, n)
              
              
              df_data %>%
                unnest_tokens(input = text,
                              output = word) %>%
                select(word, author) %>%
                filter(!word %in% to_remove1) %>%
                mutate(word = gsub(".com", "", word)) %>%
                mutate(word = gsub("^gag", "9gag", word)) %>%
                mutate(word = gsub("[[:digit:]]", "", word))%>%
                filter(!word %in% "")%>%
                count(author, word, sort = TRUE) %>%
                filter(!is.na(author))%>%
                bind_log_odds(feature = word, set = author, n = n) %>%
                group_by(author) %>%
                filter(author %in% auth$author) %>%
                top_n(n = 5, log_odds_weighted) %>%
                ggplot(aes(x = reorder_within(word, n, author), y = n, fill = author)) +
                geom_col(show.legend = FALSE) +
                ylab("") +
                xlab("") +
                labs(caption = "@Lucainson RAYMOND")+
                coord_flip() +
                facet_wrap(~author, ncol = 2, scales = "free_y") +
                scale_x_reordered() +
                ggtitle("Model-based approach: Weighted log odds ratio")+
                theme(plot.title = element_text((hjust=0.5)))+
                theme_bw()
              
            }
            
        })
 
        
        ###########################################################################################  
        
        #word correlation plot
        
        tdm<-TermDocumentMatrix(corpus)
        tdm<-removeSparseTerms(tdm,sparse=0.994)
        
        ###################################################################
        getConditionedDataFrame <- function(Corpus) {
            # calculate the frequency of words and sort it by frequency
            word.freq <- sort(rowSums(as.matrix(tdm)), decreasing = T)
            word.freq <- subset(word.freq, word.freq >=1)
            df <- data.frame(term = names(word.freq), freq = word.freq)
            return(df)
        }
        
        
        what.df <- getConditionedDataFrame(Corpus)
        what.df.t <- what.df[,-2]
        
        output$corrTableText <- renderText ({
            paste("Correlation table of words")
        })
        
        #word correlation table
        output$plot_corr_table <- renderPrint({
            as.data.frame(findAssocs(tdm, input$word, corlimit=input$corLimit))
        })
        
        #random word gathering from dataframe
        observeEvent(input$randWord, handlerExpr = {
            word <- sample(what.df.t, size = 1, replace = F)
            updateTextInput(session, "word", value=word)
        })
        
        ##################################################################   
        
        #NETWORK ANALYSIS
        
        output$network_plot <- renderPlot({
            
            corpus1<-corpus
            progress <- shiny::Progress$new()
            progress$set(message = "Network Graph", value = 0.2)
            on.exit(progress$close())
            progress$set(detail = "Creating Bigrams...", value = 0.6)
            
            bigr <- data.frame(text = sapply(corpus1, as.character), stringsAsFactors = FALSE) %>% 
                unnest_tokens(bigram, text, token = "ngrams", n = 2)
            
            two_word <- bigr %>% count(bigram, sort = TRUE)%>%
                drop_na()
            
            bigrams_separated <- two_word %>%
                separate(bigram, c("word1", "word2"), sep = " ")
            
            progress$set(detail = "Plotting..", value = 0.8)
            bigram_graph <- head(bigrams_separated %>% arrange(desc(n)),input$limit_gram) %>% graph_from_data_frame()
            
            #bigram_graph
            
            set.seed(1996)
            ggraph(bigram_graph, layout = "stress") +
                geom_edge_link() +
                geom_node_point() +
                geom_node_text(aes(label = name), vjust = 1, hjust = 1)
            
            a <- grid::arrow(type = "closed", length = unit(.08, "inches"))
            ggraph(bigram_graph, layout = "fr") +
                geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                               arrow = a, end_cap = circle(.08, 'inches')) +
                geom_node_point(color = "lightblue", size = 4) +
                geom_node_text(aes(label = name), vjust = 1, hjust = 0.5, size=5) +
                theme_void()
            
        })
        
        ####################################
        
        #Correlation plot
        output$corr_plot <- renderPlot({
            
            progress <- shiny::Progress$new()
            progress$set(message = "Correlation Plot", value = 0.2)
            on.exit(progress$close())
            progress$set(message = "Creating Pairwise words", value = 0.4)
            corpus2<-corpus
            raym <- data.frame(text = sapply(corpus2, as.character), stringsAsFactors = FALSE)
            raym$what_nbr <- 1:nrow(raym)
            what_word <- raym %>% unnest_tokens(word, text)
            what_word1 <- raym %>% unnest_tokens(word, text)%>%
                group_by(word)%>%
                count(sort=T)
                
            shiny::validate(
            need(input$limit_freq_word <= what_word1[1,2], "Please, select lower values for frequency and correlation threshold."))
            
            what_word_correl <- what_word %>% group_by(word) %>% 
                filter(n() >= input$limit_freq_word) %>% 
                pairwise_cor(word, what_nbr, sort = TRUE, upper = FALSE)
            
            shiny::validate(
                need(input$limit_cor <= what_word_correl[1,3], "Please, select lower values for frequency and correlation threshold."))
            
            progress$set(detail = "Plotting..", value = 0.8)
            set.seed(1996)
            what_word_correl %>%
                filter(correlation > input$limit_cor) %>%
                graph_from_data_frame() %>%
                ggraph(layout = "fr") +
                geom_edge_link(aes(edge_alpha = correlation, edge_width = correlation), edge_colour = "blue") +
                geom_node_point(size = 5) +
                geom_node_text(aes(label = name), repel = TRUE,
                               point.padding = unit(0.2, "lines"))+
                theme_void()
            
            
            
            
            
        })
        
        ################################################################################# 
        
        #topic modeling 
        observeEvent(input$newTopics, handlerExpr = {
            
            progress <- shiny::Progress$new()
            progress$set(message = "Topic modeling", value = 0.2)
            on.exit(progress$close())
            
            
            #a kind of average to judge log-liklihood by
            harmonicMean <- function(logLikelihoods, precision = 2000L) {
                llMed <- median(logLikelihoods)
                as.double(llMed - log(mean(exp(-mpfr(logLikelihoods,
                                                     prec = precision) + llMed))))
            }
            

            corpus_x<-tm_map(corpus, function(x) iconv(x, "latin1", "ASCII", sub=""))
            dtm<-DocumentTermMatrix(corpus_x)
            dtm<-removeSparseTerms(dtm,sparse=0.994)
            
            #TDM/DTM for topic modeling 
            #remove empty values from DTM and original dataframe copy
            rowTotals <- apply(dtm , 1, sum) 
            dtm<- dtm[rowTotals> 0, ]
            df_data <- df_data[rowTotals> 0,]

            
            #run LDA model n times
            seqk <- seq(2, input$times_r, 1)
            burnin <- 500
            iter <- 500
            keep <- 50
            
            progress$set(message = "Training the model...", value = 0.4)
            fitted_many <- lapply(seqk, function(k) LDA(dtm, k = k, method = "Gibbs",
                                                        control = list(burnin = burnin, iter = iter, keep = keep)))
            
            #Extract logliks from each topic
            logLiks_many <- lapply(fitted_many, function(L)  L@logLiks[-c(1:(burnin/keep))])
            
            #Compute harmonic means
            hm_many <- sapply(logLiks_many, function(h) harmonicMean(h))
            
            
            #Create model with the GIBBS sampling method
            lda.mod <- LDA(dtm, seqk[which.max(hm_many)], method = "Gibbs", control = list(iter=2000))
            
            #Gather topics
            topics <- topics(lda.mod, 1)
            
            output$tpcs_number <- renderText ({
                paste("Optimal number of topics is: ",sep="", seqk[which.max(hm_many)],".")
            })
            progress$set(detail = "Printing the results..", value = 0.8)
            
            
            #topic table  
            tmResult <- posterior(lda.mod)
            theta <- tmResult$topics
            
observe({
  
  shiny::validate(
    need(input$n_topic<=seqk[which.max(hm_many)], "Sorry! Can't execute this instruction. Please, select a lower number."))
  
            exampleTermData <- terms(lda.mod, input$terms)
            exampleTermData<-as.data.frame(exampleTermData[, 1:input$n_topic])
          
            
            output$plot_topic_table = DT::renderDataTable({
                
                as.data.frame(exampleTermData)
            })
            data_t_matrix<-reactive(as.data.frame(exampleTermData))
            output$plot_topic_table <- renderDataTable(data_t_matrix(), server=FALSE, extensions = 'Buttons', 
                                              options = list(dom = 'Bfrtip',
                                                             columnDefs = list(list(className = 'dt-center', targets = "_all")),
                                                             initComplete = JS(
                                                                 "function(settings, json) {",
                                                                 "$(this.api().table().header()).css({'background-color': '#8D272A', 'color': '#fff'});",
                                                                 "}"),
                                                             buttons = c('excel', 'pdf'),
                                                             pageLength = 10))  
            
})
             #########
                
                t_lda<-tidy(lda.mod,matrix="beta") #PER TOPIC
            t_tab<-as.data.frame(t_lda)%>%
                rename(`beta(Probability)`=beta)
            
            
                d_lda<-tidy(lda.mod,matrix="gamma") #PER DOCUMENT
                d_tab<-as.data.frame(d_lda)%>%
                    rename(`gamma(Probability)`=gamma)
                
                
                #TOPIC TABLE
                output$t_table = DT::renderDataTable({

                    t_tab
                })
                data_t_tab<-reactive(t_tab)
                output$t_table <- renderDataTable(data_t_tab(), server=FALSE, extensions = 'Buttons', 
                                                    options = list(dom = 'Bfrtip',
                                                                   columnDefs = list(list(className = 'dt-center', targets = "_all")),
                                                                   initComplete = JS(
                                                                       "function(settings, json) {",
                                                                       "$(this.api().table().header()).css({'background-color': '#003562', 'color': '#fff'});",
                                                                       "}"),
                                                                   buttons = c('excel', 'pdf'),
                                                                   pageLength = 10))  
                
                
                
                ################
                
                #Document table
                output$d_table = DT::renderDataTable({
                    d_tab
                })
                data_d_tab<-reactive(d_tab)
                output$d_table <- renderDataTable(data_d_tab(), server=FALSE, extensions = 'Buttons', 
                                                  options = list(dom = 'Bfrtip',
                                                                 columnDefs = list(list(className = 'dt-center', targets = "_all")),
                                                                 initComplete = JS(
                                                                     "function(settings, json) {",
                                                                     "$(this.api().table().header()).css({'background-color': '#3E4827', 'color': '#fff'});",
                                                                     "}"),
                                                                 buttons = c('excel', 'pdf'),
                                                                 pageLength = 10))     
                
                
         
                
                #####HEATMAT TOPIC MODELING
                
                output$plot_topic<-renderPlotly({
                  
                  #APPROCH 2
                  shiny::validate(
                    need(input$n_topic<=seqk[which.max(hm_many)], "Sorry! Can't execute this instruction. Please, select a lower number."))
                  
                  toptermsPerTopic <- terms(lda.mod, input$terms)
                  topicNames <- apply(toptermsPerTopic, 2, paste, collapse=" ")
                  
                  countsOfPrimaryTopics <- rep(0,seqk[which.max(hm_many)])
                  names(countsOfPrimaryTopics) <- topicNames
                  for (i in 1:nDocs(dtm)) {
                    topicsPerDoc <- theta[i, ] # select topic distribution for document i
                    # get first element position from ordered list
                    primaryTopic <- order(topicsPerDoc, decreasing = TRUE)[1] 
                    countsOfPrimaryTopics[primaryTopic] <- countsOfPrimaryTopics[primaryTopic] + 1
                  }
                  
                  #Topic proportions over time
                  #In a last step, we provide a distant view on the topics in the data over time. 
                  #For this, we aggregate mean topic proportions per month of all SOTU speeches. 
                  #These aggregated topic proportions can then be visualized as a bar plot.
                  
                  #Append month information for aggregation
                  df_data$month <- substr(df_data$time, 0, 7)
                  
                  #Get mean topic proportions per month
                  topic_proportion_per_month <- aggregate(theta, by = list(month = df_data$month), mean)
                  
                  #Set topic names to aggregated columns
                  colnames(topic_proportion_per_month)[2:(seqk[which.max(hm_many)]+1)] <- topicNames
                  
                  #Reshape data frame
                  vizDataFrame <- melt(topic_proportion_per_month, id.vars = "month")%>%
                    rename(topic=variable)
                  vizDataFrame$value<-round(vizDataFrame$value,4)
                  
                  #Plotting topic proportions per month as bar plot
                  g<-ggplot(vizDataFrame, aes(x=month, y=value, fill=topic)) + 
                    geom_bar(stat = "identity") + ylab("proportion") + 
                    scale_fill_viridis(discrete = T,option="B")+ 
                    theme(axis.text.x = element_text(angle = 30, hjust = 1))
                  
                  ggplotly(g)  
                  
                })
                
                
            
        })
        
        
        
        
            
        
        
        
        
        ##########################################################################################
        
        #Network Analysis for chat group
        output$plot_net <- renderVisNetwork({
          
          shiny::validate(
            need(nlevels(df_data$author)>2, "Sorry, the user network analysis is only available for group chat with more than two users ."))
          
          progress <- shiny::Progress$new()
          progress$set(message = "Network analysis", value = 0.2)
          on.exit(progress$close())
          progress$set(message = "Printing the results...", value = 0.6)
          
          nodes <- df_data %>%
            group_by(id = author) %>%
            summarise() %>%
            mutate(label = id)
          edges <- df_data %>%
            select(author, text) %>%
            rename(from = author) %>%
            mutate(to = str_extract(text, paste(unique(df_data$author), collapse = "|"))) %>%
            select(from, to) %>%
            na.omit() %>%
            group_by(from,to) %>%
            summarise(value = n())
          
          visNetwork(nodes, edges, main="User network analysis", 
                     submain = "Based on tag", font="white") %>% 
            visOptions(highlightNearest = TRUE)%>% 
            visEdges(arrows = 'from',font='white')%>%
            visInteraction(navigationButtons = TRUE,tooltipStyle = 'position: fixed;visibility:hidden;padding: 5px;white-space: nowrap;
 font-family: cursive;font-size:18px;font-color:purple;background-color: red')%>%
            visNodes(font= '14px arial white')
          
        })
        
        ###########################################################################################
        ##############FORECASTING###########################################

        #Time series analysis of messages exchanged by day
        time_series<-as_tibble(df_data) %>%
            mutate(day=as.Date(time))%>%
            group_by(day)%>%
            count()%>%
            ungroup()
        
        
        output$summary_table <- renderText ({
            paste("Selecting the Optimal Model")
        })
        
        output$print_table <- renderPrint({
            #Training the model for printing the process
            set.seed(1996)
          progress <- shiny::Progress$new()
          progress$set(message = "Time series forecasting", value = 0.2)
          on.exit(progress$close())
          progress$set(message = "Training...", value = 0.6)
          
            sarima_model <- baggedModel(time_series$n,bootstrapped_series = bld.mbb.bootstrap(time_series$n, 50),fn = auto.arima,trace=T)
        })
        
        #Training the bagged model by a boostrap process
        set.seed(1996)
        sarima_model <- baggedModel(time_series$n,bootstrapped_series = bld.mbb.bootstrap(time_series$n, 50),fn = auto.arima,trace=T)
        arima_fitted <- round(as.vector(sarima_model$fitted),0)
        
        time<-seq_along(time_series$n)
        sarimaFitted <- as.data.frame(cbind(time,arima_fitted, time_series$n))
        
        #Plotting the results
        output$plot_comp<-renderPlotly({
            plot_ly(sarimaFitted, x = ~time) %>%
                add_lines(y = ~time_series$n, mode = "lines", text=~time_series$day, name = "real data") %>%
                add_lines(y = ~arima_fitted, mode = "lines", text=~time_series$day, name = "model output") %>%
                layout(title = "ARIMA bagged model",
                       xaxis=list(autorange=TRUE),
                       yaxis=list(title="",autorange=TRUE))
            
        })
        
        
        ###FORECASTING PLOT
        
        fcast <- forecast(sarima_model,h=10)
        
        output$plot_pred<-renderPlot({
            
            plot(fcast)
            
        })
        
        
        
        ###TITLE ABOVE THE TABLE
        output$prev_ponc_int <- renderText ({
            paste("Point and Confidence Interval Forecasts")
        })
        
        fcast1<-as_tibble(fcast)
        fcast1$`Point Forecast`<-round(fcast1$`Point Forecast`,0)
        #fcast1$`Lo 80`<-round(fcast1$`Lo 80`,0)
        #fcast1$`Hi 80`<-round(fcast1$`Hi 80`,0)
        #fcast1$`Lo 95`<-round(fcast1$`Lo 95`,0)
        #fcast1$`Hi 95`<-round(fcast1$`Hi 95`,0)
        
        #####table pred
        output$print_table_pred<-DT::renderDataTable({
            
            as_tibble(fcast1)
        })
        data_pred<-reactive(as_tibble(fcast1))
        
        output$print_table_pred <- DT::renderDataTable(data_pred(), server=FALSE, extensions = 'Buttons', 
                                                       options = list(dom = 'Bfrtip',
                                                                      columnDefs = list(list(className = 'dt-center', targets = "_all")),
                                                                      initComplete = JS(
                                                                          "function(settings, json) {",
                                                                          "$(this.api().table().header()).css({'background-color': '#1B9E77', 'color': '#fff'});",
                                                                          "}"),
                                                                      buttons = c('excel', 'pdf'),
                                                                      pageLength = 10))
        
        
        
        sarima_tab<- as_tibble(cbind(arima_fitted, time_series$n))
        sarima_tab<-cbind(sarima_tab,as.Date(time_series$day))%>%
            as_tibble()%>%
            rename(Day=`as.Date(time_series$day)`)%>%
            rename(`real data`=V2)
        
        #Arrange the tibble data by decreasing order
        sarima_tab<-sarima_tab[order(sarima_tab$Day,decreasing = T),]
        
        #sarima_tab$`real data`<-paste(sarima_tab$`real data`,sep="","%")
        #sarima_tab$`model output`<-paste(sarima_tab$`model output`,sep="","%")
        
        output$summary_comp <- renderText ({
            paste("Comparison of actual data and model data")
        })
        
        output$print_table_comp <- DT::renderDataTable({
            sarima_tab
        })
        
        data_sarima<-reactive(sarima_tab)
        
        output$print_table_comp<- DT::renderDataTable(data_sarima(), server=FALSE, extensions = 'Buttons', 
                                                      options = list(dom = 'Bfrtip',
                                                                     columnDefs = list(list(className = 'dt-center', targets = "_all")),
                                                                     initComplete = JS(
                                                                         "function(settings, json) {",
                                                                         "$(this.api().table().header()).css({'background-color': '#D95F02', 'color': '#fff'});",
                                                                         "}"),
                                                                     buttons = c('excel', 'pdf'),
                                                                     pageLength = 10))
        
        
        

            
            
        
        
        ######################################################################################
  
        
    })
    
    
    
}
