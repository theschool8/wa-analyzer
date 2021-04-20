library("rwhatsapp")
chat <- rwa_read("D:/Project Iseng/WA.txt")
chat

library("dplyr")
chat <- chat %>%
  filter(!is.na(author)) # remove messages without author
chat

library("ggplot2"); theme_set(theme_minimal())
library("lubridate")

perday <- chat %>%
          mutate(day = date(time)) %>%
          count(day)

perjam <- chat %>%
  mutate(jam = hour(time)) %>%
  count(jam) 

perjam2 <- data.frame(rbind(max(perjam$n), min(perjam$n), perjam$n))
colnames(perjam2) <- seq(0,23,1)
rownames(perjam2) <- c("max", "min", "overall")


rata2chat <- mean(perday$n)
rata2chat

ggplot(data=perday, aes(x = day, y = n)) +
  geom_bar(stat = "identity") +
  ylab("") + xlab("") +
  ggtitle("Chat per hari (Rata2 = 67 chat per hari)")

chat %>%
  mutate(day = date(time)) %>%
  count(author) %>%
  ggplot(aes(x = reorder(author, n), y = n)) +
  geom_bar(stat = "identity") +
  ylab("") + xlab("") +
  coord_flip() +
  ggtitle("Banyak chat per orang")



