library(tidytext)
library(stringdist)
library(furrr)
library(tidytable)
library(qdapDictionaries)
plan(multisession)

add_rn <- as_mapper(~tibble(text = .x) |> 
                        mutate(id = row_number()) |>
                        slice_sample(n = 100000) |> 
                        unnest_tokens(output = "word",input = text) |> 
                        anti_join(stop_words) |> 
                        filter(if_all(contains("word"),
                                      ~ !str_detect(., "\\d"))) |> 
                        count(word,sort = T)
)

count_ngrams_tidytable <- function(dataset,n) {
    library(tidytable)
    coll_name_gram <- case_when(
        n == 2 ~ "bigram",
        n == 3 ~ "trigram",
        .default = "poly_4gram"
    )
    col_name_df <- furrr::future_map_chr(1:n,~paste0("word",.x),.progress = TRUE)
    dataset |>
        tidytable(text = _) |> 
        mutate(id = row_number()) |>
        slice_sample(n = 100000*2) |> 
        unnest_tokens(!!coll_name_gram, text, token = "ngrams", n = n)|>
        separate(eval(coll_name_gram), eval(col_name_df), sep = " ") |> 
        drop_na() |> 
        count(contains("word"),sort = T) -> counter
    return(counter)
}
bi <- count_ngrams_tidytable(combi_vec,2)
tri <- count_ngrams_tidytable(combi_vec,3)
poly_4 <- count_ngrams_tidytable(combi_vec,4)
poly_5 <- count_ngrams_tidytable(combi_vec,5)
combi_df_1word <- add_rn(combi_vec)

# Define the desired factor levels
desired_levels <- c(qdapDictionaries::GradyAugmented)



# Create a function to find the closest match CAN CHANGE METHOD TO TEST WHICH ONE IS MOST SUITABLE
find_closest_match <- function(name) {
    distances_1 <- stringdistmatrix(name, desired_levels, method = "osa")
    closest_index_1 <- which.min(distances_1)
    combi_df_1word  |> filter(word == desired_levels[closest_index_1]) |> _$n -> n1
    distances_2 <- stringdistmatrix(name, desired_levels, method = "jaccard")
    closest_index_2 <- which.min(distances_2)
    combi_df_1word  |> filter(word == desired_levels[closest_index_2]) |> _$n -> n2
    if_else(length(n1)==0|n1<n2,return(desired_levels[closest_index_2]),return(desired_levels[closest_index_1]))
}

split_clean_usr_txt <- function(text){
    text <- str_split_1(text,pattern = "\\s")
    str_remove_all(text,pattern = "[0-9!@#[:punct:]+=%$&_]")
}

predicting <- function(x){
    gc()
    cleaned_txt <- split_clean_usr_txt(x) 
    dist_closest <- furrr::future_map_chr(cleaned_txt,~if_else(.x %in% qdapDictionaries::GradyAugmented,.x,find_closest_match(.x)))
    detach("package:tidytable", unload = TRUE)
    unloadNamespace("tidytable")
    # for bigram
    case_when(length(dist_closest)>=4 ~ {poly_5 |> filter(word1 == dist_closest[length(dist_closest)-3],word2 == dist_closest[length(dist_closest)-2],
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
                        slice_head(n=3-length(word_tri)) |> _$word2 -> word_bi
                    comb_word <- c(word_bi,comb_word) 
                    if (length(comb_word)<3) {
                        combi_df_1word  |> slice_head(n=3-length(comb_word)) |> _$word -> word_uni
                        comb_word <- c(word_tri,word_bi,word_uni) 
                        comb_word
                    }
                    else {comb_word}
                }
                else {comb_word}}
            else {comb_word}}
        else {comb_word}},
        length(dist_closest)==3 ~ {poly_4 |> filter(word1 == dist_closest[length(dist_closest)-2],word2 == dist_closest[length(dist_closest)-1],
                                                    word3 == dist_closest[length(dist_closest)]) |> 
                slice_head(n=3) |> _$word4 -> word_poly_4
            comb_word <- c(word_poly_4)
            if(length(comb_word)<3){tri |> filter(word1 == dist_closest[length(dist_closest)-1],word2 == dist_closest[length(dist_closest)]) |> 
                    slice_head(n=3-length(comb_word)) |> _$word3 -> word_tri
                comb_word <- c(word_tri,comb_word)
                if (length(comb_word)<3) {
                    bi |> filter(word1 == dist_closest[length(dist_closest)]) |> 
                        slice_head(n=3-length(word_tri)) |> _$word2 -> word_bi
                    comb_word <- c(word_bi,comb_word) 
                    if (length(comb_word)<3) {
                        combi_df_1word  |> slice_head(n=3-length(comb_word)) |> _$word -> word_uni
                        comb_word <- c(word_tri,word_bi,word_uni) 
                        comb_word
                    }
                    else {comb_word}
                }
                else {comb_word}}
            else {comb_word}},
        length(dist_closest)==2 ~ {tri |> filter(word1 == dist_closest[length(dist_closest)-1],word2 == dist_closest[length(dist_closest)]) |> 
                slice_head(n=3) |> _$word3 -> word_tri
            if (length(word_tri)<3) {
                bi |> filter(word1 == dist_closest[length(dist_closest)]) |> 
                    slice_head(n=3-length(word_tri)) |> _$word2 -> word_bi
                comb_word <- c(word_tri,word_bi) 
                if (length(comb_word)<3) {
                    combi_df_1word  |> slice_head(n=3-length(comb_word)) |> _$word -> word_uni
                    comb_word <- c(word_tri,word_bi,word_uni) 
                    comb_word
                }
                else {comb_word}
            }
            else {word_tri}},
        length(dist_closest)==1 ~ {bi |> filter(word1 == dist_closest[length(dist_closest)]) |> 
                slice_head(n=3) |> _$word2 -> word_bi
            comb_word <- c(word_bi) 
            if (length(comb_word)<3) {
                combi_df_1word  |> slice_head(n=3-length(comb_word)) |> _$word -> word_uni
                comb_word <- c(word_bi,word_uni) 
                comb_word
            }
            else {comb_word}},
        TRUE ~ {combi_df_1word  |> slice_head(n=3) |> _$word -> word_uni
            comb_word <- word_uni}) -> predictions
    library(tidytable)
    gc()
    return(predictions)
}

tri |> filter(word1 == "are",word2 == "strong") |> 
    slice_head(n=3)

predicting("the waves are strong")

object_size(bi,tri,poly_4,poly_5,combi_df_1word,GradyAugmented)

obj_nam <- c("combi_df_1word", "bi","tri","poly_4","poly_5","GradyAugmented")
obj_nam_sml <- c("combi_df_1word", "bi","tri","GradyAugmented")
save(combi_df_1word, bi,tri,GradyAugmented,list = obj_nam_sml,compress = F,file = "all_req_obj_lr")
saveRDS(GradyAugmented,file = "word_dic")




