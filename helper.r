library(tidytext)
library(stringdist)
library(furrr)
library(tidytable)

nam <- c('combi_df_1word','bi','tri','poly_4',"GradyAugmented")
dt <- future_map(list.files(path = "data/",pattern = "word",full.names = T),~read_rds(.x))
names(dt) <- nam
list2env(x = dt,envir = globalenv())

distances_1 <- integer(length = 122806)
distances_2 <- integer(length = 122806)
# Create a function to find the closest match CAN CHANGE METHOD TO TEST WHICH ONE IS MOST SUITABLE
find_closest_match <- function(name) {
    distances_1 <<- stringdist(name, GradyAugmented, method = "osa")
    closest_index_1 <- which.min(distances_1)
    combi_df_1word  |> filter(word == GradyAugmented[closest_index_1]) |> pull(n) -> n1
    distances_2 <<- stringdist(name, GradyAugmented, method = "jaccard")
    closest_index_2 <- which.min(distances_2)
    combi_df_1word  |> filter(word == GradyAugmented[closest_index_2]) |> pull(n) -> n2
    if_else(length(n1)==0|n1<n2,return(GradyAugmented[closest_index_2]),return(GradyAugmented[closest_index_1]))
}

split_clean_usr_txt <- function(text){
    text <- str_split_1(text,pattern = "\\b[\\s]+\\b")
    text <- str_remove_all(text,pattern = "[0-9!@#\\s[:punct:]+=%$&_]")
    text <- str_to_lower(text)
    return(text)
}

case4 <- function(){poly_4 |> filter(word1 == dist_closest[length(dist_closest)-2],word2 == dist_closest[length(dist_closest)-1],
                                     word3 == dist_closest[length(dist_closest)]) |> 
        slice_head(n=3) |> pull(word4) -> word_poly_4
    comb_word <- c(word_poly_4)
    if(length(comb_word)<3){tri |> filter(word1 == dist_closest[length(dist_closest)-1],word2 == dist_closest[length(dist_closest)]) |> 
            slice_head(n=3-length(comb_word)) |> pull(word3) -> word_tri
        comb_word <- c(word_tri,comb_word)
        if (length(comb_word)<3) {
            bi |> filter(word1 == dist_closest[length(dist_closest)]) |> 
                slice_head(n=3-length(comb_word)) |> pull(word2) -> word_bi
            comb_word <- c(word_bi,comb_word) 
            ifelse(length(comb_word)<3,{
                combi_df_1word  |> slice_head(n=3-length(comb_word)) |> pull(word) -> word_uni
                comb_word <- c(word_bi,word_uni) 
                comb_word},comb_word)
            return(comb_word)
        }
        else {comb_word}}
    else {comb_word}}

case3 <- function(){tri |> filter(word1 == dist_closest[length(dist_closest)-1],word2 == dist_closest[length(dist_closest)]) |> 
        slice_head(n=3) |> pull(word3) -> word_tri
    if (length(word_tri)<3) {
        bi |> filter(word1 == dist_closest[length(dist_closest)]) |> 
            slice_head(n=3-length(word_tri)) |> pull(word2) -> word_bi
        comb_word <- c(word_tri,word_bi) 
        ifelse(length(comb_word)<3,{
            combi_df_1word  |> slice_head(n=3-length(comb_word)) |> pull(word) -> word_uni
            comb_word <- c(word_bi,word_uni) 
            comb_word},comb_word)
        return(comb_word)
    }
    else {word_tri}}

case2 <- function(){bi |> filter(word1 == dist_closest[length(dist_closest)]) |> 
        slice_head(n=3) |> pull(word2) -> word_bi
    comb_word <- c(word_bi) 
    ifelse(length(comb_word)<3,function(){
        combi_df_1word  |> slice_head(n=3-length(comb_word)) |> pull(word) -> word_uni
        comb_word <- c(word_bi,word_uni) 
        comb_word},comb_word)
    return(comb_word)}

case1 <- function(){combi_df_1word  |> slice_head(n=3) |> pull(word) -> word_uni
    comb_word <- word_uni}

clean_match <- function(x){ifelse(x %in% qdapDictionaries::GradyAugmented,x,find_closest_match(x))}

predicting <- function(x){
    predictions <- character(length = 3)
    clean_txt <- split_clean_usr_txt(x)
    dist_closest <<- map_chr(tail(clean_txt,3),clean_match) 
    # for 4 gram
    if (length(dist_closest)>=3 ) {
        case4()-> predictions
    }
    # for tri gram
    else if (length(dist_closest)==2 ) {
        case3()-> predictions
    }
    # for bi gram
    else if (length(dist_closest)==1 ) {
        case2()-> predictions
    }
    # for uni gram
    else {
        case1()-> predictions
    }
    return(predictions)
}





