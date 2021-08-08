# visualise.R
# Part of R_template by Tuomas Eerola, https://github.com/tuomaseerola/R_template/

#### 1. histogram of responses by Track and Scale ----------------------------------
g1 <- ggplot(pivoted, aes(x=Rating))+
  geom_histogram(fill='red',colour="black",bins = 5)+
  facet_wrap(.~MAS)+
  #  scale_y_continuous(limits = c(0.8,5))+
  theme_bw()
print(g1)

#### 1. histogram of responses by Track and Scale ----------------------------------
g2 <- ggplot(pivoted2, aes(x=Rating,fill=Gender))+
  geom_histogram(colour="black",bins = 5,position = position_dodge())+
  facet_wrap(.~HUMS)+
  #  scale_y_continuous(limits = c(0.8,5))+
  theme_bw()
print(g2)

