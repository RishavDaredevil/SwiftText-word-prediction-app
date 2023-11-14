library(tidytext)
library(widyr)
path <- ("C:/Users/Risha/Desktop/R_course/Captstone/Coursera-SwiftKey/final/en_US")

vec_twit <- read_lines(list.files(path = path,pattern = "twitter",full.names = T),lazy = T)
vec_blog <- read_lines(list.files(path = path,pattern = "blog",full.names = T),lazy = T)
vec_news <- read_lines(list.files(path = path,pattern = "news",full.names = T),lazy = T)

combi_vec <- c(vec_twit,vec_blog,vec_news)

max(str_length(vec_blog))
length(vec_twit)
add_rn <- as_mapper(~tibble(text = .x) |> 
                        mutate(id = row_number()) |>
                        slice_sample(n = 10000))
df_twit <- add_rn(vec_twit)
df_blog <- add_rn(vec_blog)
df_news <- add_rn(vec_news)

all_samp_df <- list(df_twit = df_twit,df_blog = df_blog,df_news = df_news)

library(wordcloud)
df_news |> 
    unnest_tokens(word,text) |> 
    anti_join(stop_words) |> 
    inner_join(get_sentiments("bing")) |> 
    count(word,sentiment,sort = T) |> 
    slice_max(n,n = 40) |> 
    select(-sentiment) |> 
    wordcloud2(color = "black")
    group_by(sentiment) |> 
    summarise(n = sum(n))

frequency <- bind_rows(mutate(df_twit, type = "twitter"),
                      mutate(df_blog, type = "blogs"), 
                      mutate(df_news, type = "news")) |> 
    mutate(word = str_extract(text, "[a-z']+")) |>
    count(type, word) |>
    group_by(type) |>
    mutate(proportion = n / sum(n)) |> 
    select(-n) |> 
    pivot_wider(names_from = type, values_from = proportion) |>
    pivot_longer(blogs:news,
                 names_to = "type", values_to = "proportion")


library(scales)
source(file = "C:/Users/Risha/Desktop/R_course/Captstone/count_ngrams_&_visualize_ngrams.r")

# expect a warning about rows with missing values being removed
tempplot <- ggplot(frequency, aes(x = proportion, y = twitter, 
                      color = abs(twitter - proportion))) +
    geom_abline(color = "gray40", lty = 2) +
    geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
    geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
    scale_x_log10(labels = percent_format()) +
    scale_y_log10(labels = percent_format()) +
    scale_color_gradient(limits = c(0, 0.001), 
                         low = "darkslategray4", high = "gray75") +
    facet_wrap(~type, ncol = 2) +
    theme(legend.position="none") +
    labs(y = "twitter", x = NULL)

plotly::ggplotly(tempplot)

cor.test(data = frequency[frequency$type == "blogs",],
         ~ proportion + twitter)
cor.test(data = frequency[frequency$type == "news",], 
         ~ proportion + twitter)

# using ngrams to see relationship
col_name_df <- c(col_name_df,"word")

add_rn <- as_mapper(~tibble(text = .x) |> 
                        mutate(id = row_number()) |>
                        slice_sample(n = 10000) |> 
                        unnest_tokens(output = "word",input = text) |> 
                        anti_join(stop_words)
                        )
df_twit_ngrams2 <- add_rn(vec_twit)
df_blog_ngrams2 <- add_rn(vec_blog)
df_news_ngrams2 <- add_rn(vec_news)

col_name_df <- c("word1","word2")
df_news_ngrams2 |> 
    separate("word", col_name_df, sep = " ")  |> 
    list() |> 
    map(~filter(.x,if_all(contains("word"),
                             ~ !.  %in% stop_words$word)),.progress = TRUE) |> 
    list_rbind() |> 
    count(word1,word2,sort = T)

prep <- c("above, are, a, the, of, is, has, have, had, was, were, will, across, against, along, among, around, at, before, behind, below, beneath, beside, between, by, down, from, in, into, near, of, off, on, to, toward, under, upon, with, within")
prep |> str_split_1(pattern = ", ") |> tibble(text = _) ->prep 



cust <- tibble(word = "rt",lexicon = "custom")
stop_words <- bind_rows(stop_words,cust)

twit_bigram <- count_ngrams_tidytable(vec_twit,n = 2)

twit_bigram |> 
    filter(n > 25,
           !str_detect(word1, "\\d"),
           !str_detect(word2, "\\d")) |> 
    visualize_ngrams() + ggtitle("bigram")

twit_trigram |> 
    filter(n>5,!str_detect(word1, "\\d"),
           !str_detect(word2, "\\d"),!str_detect(word3, "\\d")) |> 
    visualize_ngrams() +  ggtitle("Trigram n>5") -> temp

plotly::ggplotly(temp)

twit_bigram |> as_tibble() |>  filter(n > 40,
                                      !str_detect(word1, "\\d"),
                                      !str_detect(word2, "\\d")) |> 
    unite(bigram,word1,word2,sep = " ") |> 
    ggplot(aes(reorder(bigram,n),n)) + geom_col() + coord_flip()




combi_df_tri_lab <- bind_rows(mutate(count_ngrams_tidytable(vec_twit,n=3), type = "twitter"),
                      mutate(count_ngrams_tidytable(vec_blog,n=3), type = "blogs"), 
                      mutate(count_ngrams_tidytable(vec_news,n=3), type = "news")) 

# how many words needed in frequency sorted dictionary to cover 50% of all word instances in the language? 90%? 

combi_vec <- c(vec_twit,vec_blog,vec_news)
combi_df_uni <- count_ngrams_tidytable(combi_vec,1)

combi_df_uni |> mutate(id = row_number() ,prop = n/sum(n)*100,cumprop = cumsum(prop)) |>  
    filter(cumprop>50,cumprop<50.1)

for 50% = 1655/87000*100
for 90% = 20700/87000*100

combi_df_uni |> filter(n > 1200,!str_detect(word1, "\\d")) |>
    ggplot(aes(reorder(word1,n),n)) + geom_col() + coord_flip() + ggtitle("word freq unigram n>1200")+
    xlab("word")

# What are the frequencies of 2-grams and 3-grams in the dataset? 

combi_df_bi <- count_ngrams_tidytable(combi_vec,2)
combi_df_bi_lab |> as_tibble() |>  filter(!str_detect(word1, "\\d"),!str_detect(word2, "\\d")) |> 
    unite(bigram,word1,word2,sep = " ") |> group_by(type) |> slice_max(n,n = 15) |> ungroup() |>  
    ggplot(aes(reorder(bigram,n),n,fill = type)) + geom_col() + coord_flip()+ ggtitle("Word Freq bigram Top 10 from each Corpus")+
    xlab("word") + facet_wrap(~type,scales = "free")

combi_df_tri <- count_ngrams_tidytable(combi_vec,3)
combi_df_tri |> as_tibble() |>  filter(n>7,!str_detect(word1, "\\d"),
                                       !str_detect(word2, "\\d"),
                                       !str_detect(word3, "\\d")) |> 
    unite(trigram,word1,word2,word3,sep = " ") |> 
    ggplot(aes(reorder(trigram,n),n)) + geom_col() + 
    coord_flip()+ ggtitle("word freq trigram n > 7")+
    xlab("word")
combi_df_tri_lab |> as_tibble() |>  filter(!str_detect(word1, "\\d"),
                                           !str_detect(word2, "\\d"),
                                           !str_detect(word3, "\\d")) |> 
    unite(trigram,word1,word2,word3,sep = " ") |> group_by(type) |> slice_max(n,n = 15) |> 
    ggplot(aes(reorder(trigram,n),n,fill = type)) + geom_col() + 
    coord_flip()+ ggtitle("Word Freq trigram Top 10 from each Corpus")+
    xlab("word") + facet_wrap(~type,scales = "free")




# How do you evaluate how many of the words come from foreign languages?

# Dictionary

diction_path <- c("C:/Users/Risha/Desktop/R_course/Captstone/Word lists in csv/")

eng_dict <- read_csv(file = list.files(diction_path,full.names = T),lazy = T,col_names = c("word"))
class(eng_dict)
eng_dict <- eng_dict |> 
    distinct(word)

combi_df_1word <- add_rn(combi_vec)

combi_vec |> tibble(text = _) |> mutate(id = row_number()) |> slice_sample(n = 10000) |> 
    mutate(langua = map(text,~cld2::detect_language_mixed(.x),.progress = "lang_detec")) |> 
    mutate(langua = future_map(langua,~.x$classification |> select(language,proportion),
                        .progress = T)) |>   unnest(langua) 

lang_classi |> filter(!language %in% c("ENGLISH","UNKNOWN"),!str_detect(text, "^\\d")) |> count(text,language,sort = T) |> 
    print(n = Inf)

# Counting and correlating pairs of words with the widyr package

combi_type_df_1word <- bind_rows(mutate(add_rn(vec_twit), type = "twitter"),
                              mutate(add_rn(vec_blog), type = "blogs"), 
                              mutate(add_rn(vec_news), type = "news"))  

combi_type_df_1word |> filter(!str_detect(word, "\\d")) |> group_by(word) |> 
    filter(n() >= 50,n() <= 80) |> ungroup() |>  pairwise_cor(word,id,sort = TRUE) -> correl_combi_type_df_1word

correl_combi_type_df_1word |> filter(correlation > .11) |>
    graph_from_data_frame() |>
    ggraph(layout = "fr") +
    geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
    geom_node_point(color = "lightblue", size = 5) +
    geom_node_text(aes(label = name), repel = TRUE) +
    theme_void() + ggtitle("correlation > .11")

# making a dictionaary of bad words

badwords <- read_lines(list.files(pattern = "bad-words_"))

badwords[[1]] |> str_split(pattern = ",") |> tibble(text = _) |> unnest(text)-> badwords

badwords |> distinct(text)
badwords_kaggle <- read_csv(list.files(pattern = "bad-words.csv"),col_names = "text")

badwords <- bind_rows(badwords,badwords_kaggle) |> distinct(text)

correl_combi_type_df_1word |>  filter(item1 %in% badwords$text) |>
    group_by(item1) |>
    slice_max(correlation, n = 6) |>
    ungroup() |>
    mutate(item2 = reorder(item2, correlation)) |>
    ggplot(aes(item2, correlation,fill = item1)) +
    geom_bar(stat = "identity") +
    facet_wrap(~ item1, scales = "free") +
    coord_flip()  


