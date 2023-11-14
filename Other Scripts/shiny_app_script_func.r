library(tidytext)
library(stringdist)
library(furrr)
library(tidytable)
plan(multisession)
rm(list = ls())

load(file = "data/all_req_obj")
# Create a function to find the closest match CAN CHANGE METHOD TO TEST WHICH ONE IS MOST SUITABLE
find_closest_match <- function(name) {
    distances_1 <- stringdistmatrix(name, GradyAugmented, method = "osa")
    closest_index_1 <- which.min(distances_1)
    combi_df_1word  |> filter(word == GradyAugmented[closest_index_1]) |> _$n -> n1
    distances_2 <- stringdistmatrix(name, GradyAugmented, method = "jaccard")
    closest_index_2 <- which.min(distances_2)
    combi_df_1word  |> filter(word == GradyAugmented[closest_index_2]) |> _$n -> n2
    if_else(length(n1)==0|n1<n2,return(GradyAugmented[closest_index_2]),return(GradyAugmented[closest_index_1]))
}

split_clean_usr_txt <- function(text){
    text <- str_split_1(text,pattern = "\\b[\\s]+\\b")
    text <- str_remove_all(text,pattern = "[0-9!@#\\s[:punct:]+=%$&_]")
    text <- str_to_lower(text)
    return(text)
}


case5 <- function(){poly_5 |> filter(word1 == dist_closest[length(dist_closest)-3],word2 == dist_closest[length(dist_closest)-2],
                                     word3 == dist_closest[length(dist_closest)-1],word4 == dist_closest[length(dist_closest)]) |> 
        slice_head(n=3) |> _$word5 -> word_poly_5
    comb_word <- c(word_poly_5)
    if(length(comb_word)<3){poly_4 |> filter(word1 == dist_closest[length(dist_closest)-2],word2 == dist_closest[length(dist_closest)-1],
                                             word3 == dist_closest[length(dist_closest)]) |> 
            slice_head(n=3-length(comb_word)) |> _$word4 -> word_poly_4
        comb_word <- c(word_poly_4,comb_word)
        if(length(comb_word)<3){tri |> filter(word1 == dist_closest[length(dist_closest)-1],word2 == dist_closest[length(dist_closest)]) |> 
                slice_head(n=3-length(comb_word)) |> _$word3 -> word_tri
            comb_word <- c(word_tri,comb_word)
            if (length(comb_word)<3) {
                bi |> filter(word1 == dist_closest[length(dist_closest)]) |> 
                    slice_head(n=3-length(comb_word)) |> _$word2 -> word_bi
                comb_word <- c(word_bi,comb_word) 
                if (length(comb_word)<3) {
                    combi_df_1word  |> slice_head(n=3-length(comb_word)) |> _$word -> word_uni
                    comb_word <- c(comb_word,word_uni) 
                    comb_word
                }
                else {comb_word}
            }
            else {comb_word}}
        else {comb_word}}
    else {comb_word}}

case4 <- function(){poly_4 |> filter(word1 == dist_closest[length(dist_closest)-2],word2 == dist_closest[length(dist_closest)-1],
                                     word3 == dist_closest[length(dist_closest)]) |> 
        slice_head(n=3) |> _$word4 -> word_poly_4
    comb_word <- c(word_poly_4)
    if(length(comb_word)<3){tri |> filter(word1 == dist_closest[length(dist_closest)-1],word2 == dist_closest[length(dist_closest)]) |> 
            slice_head(n=3-length(comb_word)) |> _$word3 -> word_tri
        comb_word <- c(word_tri,comb_word)
        if (length(comb_word)<3) {
            bi |> filter(word1 == dist_closest[length(dist_closest)]) |> 
                slice_head(n=3-length(comb_word)) |> _$word2 -> word_bi
            comb_word <- c(word_bi,comb_word) 
            if (length(comb_word)<3) {
                combi_df_1word  |> slice_head(n=3-length(comb_word)) |> _$word -> word_uni
                comb_word <- c(comb_word,word_uni) 
                comb_word
            }
            else {comb_word}
        }
        else {comb_word}}
    else {comb_word}}

case3 <- function(){tri |> filter(word1 == dist_closest[length(dist_closest)-1],word2 == dist_closest[length(dist_closest)]) |> 
        slice_head(n=3) |> _$word3 -> word_tri
    if (length(word_tri)<3) {
        bi |> filter(word1 == dist_closest[length(dist_closest)]) |> 
            slice_head(n=3-length(word_tri)) |> _$word2 -> word_bi
        comb_word <- c(word_tri,word_bi) 
        if (length(comb_word)<3) {
            combi_df_1word  |> slice_head(n=3-length(comb_word)) |> _$word -> word_uni
            comb_word <- c(comb_word,word_uni) 
            comb_word
        }
        else {comb_word}
    }
    else {word_tri}}

case2 <- function(){bi |> filter(word1 == dist_closest[length(dist_closest)]) |> 
        slice_head(n=3) |> _$word2 -> word_bi
    comb_word <- c(word_bi) 
    if (length(comb_word)<3) {
        combi_df_1word  |> slice_head(n=3-length(comb_word)) |> _$word -> word_uni
        comb_word <- c(word_bi,word_uni) 
        comb_word
    }
    else {comb_word}}

case1 <- function(){combi_df_1word  |> slice_head(n=3) |> _$word -> word_uni
    comb_word <- word_uni}

predicting <- function(x){
    cleaned_txt <- split_clean_usr_txt(x) 
    dist_closest <<- furrr::future_map_chr(cleaned_txt,~if_else(.x %in% qdapDictionaries::GradyAugmented,.x,find_closest_match(.x)))
    # for bi gram
    if (length(dist_closest) >= 4) {
        case5()-> predictions
    }
    else if (length(dist_closest)==3 ) {
        case4()-> predictions
    }
    else if (length(dist_closest)==2 ) {
        case3()-> predictions
    }
    else if (length(dist_closest)==1 ) {
        case2()-> predictions
    }
    else {
        case1()-> predictions
    }
    return(predictions)
}



