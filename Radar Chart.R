# Radar chart with fmsb package
library(fmsb)
library(reshape)


# ALL Chat 
perjam <- chat %>%
  mutate(jam = hour(time)) %>%
  count(jam) %>%
  arrange(desc(jam))

n <- perjam$n
perjam2 <- data.frame(rbind(max=max(n), min=min(n), overall=n))
colnames(perjam2) = seq(23,0,-1)

par(mfrow=c(2,4),new=TRUE)
# Plot All chat
radarchart(perjam2, 
           seg = 10,  # Number of axis segments
           title = "Activity by time of day",
           pfcol = scales::alpha("grey", 0.9),
           plwd = 2)

# Group by author
data2 <- chat %>%
  mutate(jam = hour(time)) %>%
  group_by(author) %>%
  count(jam)  %>%
  cast(author~jam)

rownames(data2) <- data2[,1]
data2 <- data2[,-1]
data2[is.na(data2)] <- 0

data3 = data.frame(rbind(max=max(data2), min=min(data2), data2))
colnames(data3) = seq(0,23,1)
data3 = data3[,seq(24,1,-1)]     

# Create plot Galih
radarchart(data3[c(1,2,5),], 
           seg = 10,  # Number of axis segments
           title = "Galih",
           pfcol = scales::alpha("darkblue", 0.9),
           plwd = 2)

# Create plot Hilmi
radarchart(data3[c(1,2,7),], 
           seg = 10,  # Number of axis segments
           title = "Hilmi",
           pfcol = scales::alpha("Green", 0.9),
           plwd = 2)

# Create plot mustofa
radarchart(data3[c(1,2,9),], 
           seg = 10,  # Number of axis segments
           title = "mustofa",
           pfcol = scales::alpha("yellow", 0.9),
           plwd = 2)

# Create plot Lintang
radarchart(data3[c(1,2,8),], 
           seg = 10,  # Number of axis segments
           title = "Lintang",
           pfcol = scales::alpha("skyblue", 0.9),
           plwd = 2)

# Create plot Dimas
radarchart(data3[c(1,2,4),], 
           seg = 10,  # Number of axis segments
           title = "Dimas",
           pfcol = scales::alpha("Maroon", 0.9),
           plwd = 2)

# Create plot Irul
radarchart(data3[c(1,2,3),], 
           seg = 10,  # Number of axis segments
           title = "Irul",
           pfcol = scales::alpha("orange", 0.9),
           plwd = 2)

# Create plot Haikal
radarchart(data3[c(1,2,6),], 
           seg = 10,  # Number of axis segments
           title = "Haikal",
           pfcol = scales::alpha("lightgreen", 0.9),
           plwd = 2)

