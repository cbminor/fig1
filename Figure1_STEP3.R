require(ggplot2)
require(gridExtra)
library(tidyverse)
all_data <- read_csv('all_data.csv')
up_or_down <- read_csv('up_or_down.csv')
cutoffs <- read_csv('cutoffs_table.csv')
brca_counts <- read_csv('brca_counts.csv')

print(all_data)
print(up_or_down)
print(cutoffs)
print(brca_counts)


# Plot 1 
all_data = all_data %>%
  as_tibble() %>%
  gather(key="Y", value="Z", -1) %>%
  group_by(X1, Z) %>%
  summarise(
    counts = n()
  )

print(all_data)

plt1 = ggplot(total, aes(fill=factor(Z), y=counts, x=X1)) +
  geom_bar(position="dodge", stat="identity") +
  theme(axis.text.x = element_text(angle=90, hjust=1)) +
  labs(fill="Copy Number") +
  scale_fill_discrete(breaks=c("high", "low", "normal"), labels=c("Significant Increase", "Significant Decrease", "Average"))


plt1

# Plot 2

plt2 = ggplot(brca_counts, aes(high)) +
  geom_histogram() +
  geom_vline(xintercept=54.5, color='red')
plt2


# Plot 3
plt3 = up_or_down %>%
  as_tibble() %>%
  gather(key="Y", value="Z", -1, -9) %>%
  group_by(chromosome) %>%
  ggplot(aes(X1, factor(Y), fill=factor(Z))) +
  geom_tile(mapping=aes(group=factor(chromosome))) +
  theme(axis.text.x=element_blank()) +
  xlab("Gene") +
  ylab("Cancer Type") +
  labs(fill="Copy Number") +
  scale_fill_discrete(breaks=c("high", "low", "normal"), labels=c("Significant Increase", "Significant Decrease", "Average"))

plt3

plt3 = ggplot(up_or_down1, aes(X1, factor(Y), fill=factor(Z))) +
  geom_tile(mapping=aes(group=factor(chromosome))) +
  theme(axis.text.x=element_blank()) +
  xlab("Gene") +
  ylab("Cancer Type")


grid.arrange(
  plt2, plt1, plt3,
  widths = c(1,1),
  layout_matrix = rbind(c(1, 2),
                        c(3, 3))
)
