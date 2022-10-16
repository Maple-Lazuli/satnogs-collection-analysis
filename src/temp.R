bad <- read.csv("bad.csv", nrows = 1000)
bad$Signal = FALSE
good  <- read.csv("good.csv", nrows = 1000)
good$Signal = TRUE
df <- rbind(bad, good)
df <- df %>% mutate(Frequency = as.numeric(gsub(r"[\D]", "", Frequency))) %>%
  mutate(Frequency = Frequency/1000000)

label_band <- function(freq) {
  if ((freq <= 300) & (freq >= 30))
    return("VHF")
  if ((freq <= 1000 ) & (freq > 30))
    return("UHF")
  if ((freq <= 2000) & (freq > 1000))
    return("L-Band")
  if ((freq <= 4000) & (freq > 2000))
    return("S-Band")
  return("Unk")
}
df$Band <- sapply(df$Frequency, FUN = label_band)

ggplot(counts, aes(x = Band, y = n, color = Band, fill = Band)) + geom_col() + labs(title = "Frequency Bands", x ="Band", y = "Counts")

ggplot(df %>% filter(Band == "UHF"), aes(x = Frequency)) + geom_histogram(bins = 1000) +  labs(title = "Histogram of UHF Frequencies", x ="Frequency", y = "Counts")
ggplot(df %>% filter(Band == "UHF"), aes(x = Frequency, color = Signal, fill = Signal)) + geom_histogram(bins = 1000, alpha = 0.4) +  labs(title = "Histogram of UHF Frequencies", x ="Frequency", y = "Counts")

                                                                                                         
ggplot(df %>% filter(Band == "VHF"), aes(x = Frequency)) + geom_histogram(bins = 1000) +  labs(title = "Histogram of VHF Frequencies", x ="Frequency", y = "Counts")
ggplot(df %>% filter(Band == "VHF"), aes(x = Frequency, color = Signal, fill = Signal)) + geom_histogram(bins = 1000) +  labs(title = "Histogram of VHF Frequencies", x ="Frequency", y = "Counts")



ggplot(df %>% filter(Band == "L-Band"), aes(x = Frequency)) + geom_histogram() +  labs(title = "Histogram of L-Band Frequencies", x ="Frequency", y = "Counts")
ggplot(df %>% filter(Band == "S-Band"), aes(x = Frequency)) + geom_histogram() +  labs(title = "Histogram of S-Band Frequencies", x ="Frequency", y = "Counts")

