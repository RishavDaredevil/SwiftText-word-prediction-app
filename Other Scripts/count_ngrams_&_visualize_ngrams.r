library(ggraph)
library(igraph)
library(tidytext)
library(furrr)

plan(multisession)

count_ngrams_tibble <- function(dataset,n) {
    coll_name_gram <- case_when(
        n == 2 ~ "bigram",
        n == 3 ~ "trigram",
        .default = "polygram"
    )
    col_name_df <- furrr::future_map_chr(1:n,~paste0("word",.x),.progress = TRUE)
    dataset |>
        tibble(text = _) |> 
        mutate(id = row_number()) |>
        slice_sample(n = 100000) |> 
        unnest_tokens(!!coll_name_gram, text, token = "ngrams", n = n)|>
        separate(eval(coll_name_gram), eval(col_name_df), sep = " ") |> 
        list() |> 
        furrr::future_map(~filter(.x,if_all(contains("word"),
                              ~ !.  %in% stop_words$word)),.progress = TRUE) |> 
        list_rbind() |> 
        drop_na()|> 
        count(pick(contains("word")),sort = T)  
}

count_ngrams_tidytable <- function(dataset,n) {
    library(tidytable)
    coll_name_gram <- case_when(
        n == 2 ~ "bigram",
        n == 3 ~ "trigram",
        .default = "polygram"
    )
    col_name_df <- furrr::future_map_chr(1:n,~paste0("word",.x),.progress = TRUE)
    dataset |>
        tidytable(text = _) |> 
        mutate(id = row_number()) |>
        slice_sample(n = 100000) |> 
        unnest_tokens(!!coll_name_gram, text, token = "ngrams", n = n)|>
        separate(eval(coll_name_gram), eval(col_name_df), sep = " ") |> 
        list() |> 
        furrr::future_map(~filter(.x,if_all(contains("word"),
                                            ~ !.  %in% stop_words$word)) |> 
                              filter(.df = _,if_all(contains("word"),
                                               ~ !str_detect(., "\\d"))) ,.progress = TRUE) |> 
        list_rbind() |> 
        drop_na() |> 
        count(contains("word"),sort = T) -> counter
    detach("package:tidytable", unload = TRUE)
    unloadNamespace("tidytable")
    return(counter)
}

# TEST
twit_trigram <- count_ngrams_tidytable(dataset = vec_twit,n = 3)

visualize_ngrams <- function(bigrams) {
    set.seed(2016)
    a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
    
    bigrams  |> 
        graph_from_data_frame()  |> 
        ggraph(layout = "fr") +
        geom_edge_link(aes(edge_alpha = n), show.legend = FALSE, arrow = a) +
        geom_node_point(color = "lightblue", size = 5) +
        geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
        theme_void()  
}






