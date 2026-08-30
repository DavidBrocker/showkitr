fx_showcase(str_replace_all("what are you thinking of for dinner?", "[aeiou]","_"))

fx_showcase(str_to_camel("hey there pal"))

fx_showcase(tibble(
  words,
  length = str_length(words)
) |> 
  count(length) |> 
  ggplot(aes(length,n)) + 
  geom_col() + 
  theme_minimal())

fx_showcase(str_replace_all(st,"\\s","{\\\\s}"),handle = "DataVizWithDave")
fx_showcase(str_replace_all(st,"\\b","{boundary}"))

str_count(st,pattern = boundary("word"))

fx_showcase(str_split_1(st,boundary("word")) |> str_length())

fx_showcase(str_extract_all("Where are the vowels?", "[aeiou]"))

cl <- "Claude, thank you for putting up with me!"

fx_showcase(str_replace_all(cl,"\\s","{\\\\s}"))
