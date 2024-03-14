library(rvest)
library(tidyverse)
library(dplyr)

men100m_html <- read_html("http://www.alltime-athletics.com/m_100ok.htm")
men100m_html |> html_nodes(xpath = "//pre") |> html_text() -> men100m_list
men100m_tbl <- read_fwf(men100m_list)
men100m_tbl <- men100m_tbl[-(3900:4025),]			#cancel the last rows

setNames(men100m_tbl,c("position","time","idk","name","nationality","birth","idk","olympics","date"))		#name the columns

men100m_tbl <- as_tibble(men100m_tbl)				#write as a tibble

str(men100m_tbl)
