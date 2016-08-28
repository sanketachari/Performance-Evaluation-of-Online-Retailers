
library(xlsx)
library(dplyr)
library(ggplot2)
library(gridExtra)
data <- read.xlsx("Data Analytics case study (2).xlsx",sheetName = "Sheet1")
names(data)[8] <- paste("CTR")
columns <- colnames(data)

webA_data <- data[which(data$Website == "Website A"),]
webB_data <- data[which(data$Website == "Website B"),]
webC_data <- data[which(data$Website == "Website C"),]

# Trends- Likes

g <- ggplot(data, aes(Post.about, Post.Likes, fill = Website)) + 
     geom_bar(stat = "identity",col = "black") + facet_grid(Website~.) +
      theme(axis.text.x = element_text(angle = 60,hjust = 1)) +
      xlab("Mobile Phones") + ylab("Number of Likes") +
      ggtitle("Number of Likes Vs Mobile Phones")
g

# Total Number of Posts
total<-  as.vector(c(dim(webA_data)[1],dim(webB_data)[1],dim(webC_data)[1]))
websites <- unique(data[,"Website"])

posts_data <- data.frame(websites,total)

g1 <- ggplot(posts_data, aes(websites, total, fill = websites, , ymax = 100)) + 
     geom_bar(stat = "identity",col = "black", position = "dodge") + 
     geom_text(aes(label = total), position = position_dodge(width = 0.9),vjust = -.025) +
     theme(axis.text.x = element_text(angle = 60,hjust = 1)) +
     xlab("Websites") + ylab("Number of Posts") +
     ggtitle("Total Posts of the websites")
g1

# Total Number of Likes
total_Likes <- as.vector(c(sum(webA_data[,"Post.Likes"]),sum(webB_data[,"Post.Likes"]),sum(webC_data[,"Post.Likes"])))
Likes_data <- data.frame(websites,total_Likes)

g2 <- ggplot(Likes_data, aes(websites, total_Likes, fill = websites, ,ymax = 300000)) + 
  geom_bar(stat = "identity",col = "black", position = "dodge") + 
  geom_text(aes(label = total_Likes), position = position_dodge(width = 0.9),vjust = -.025) +
  theme(axis.text.x = element_text(angle = 60,hjust = 1)) +
  xlab("Websites") + ylab("Total Likes ") +
  ggtitle("Total Likes to the posts of websites")


# Average Likes to the posts of Websites

avg_Likes <- round(total_Likes/total)

avg_Likes_data <- data.frame(websites,avg_Likes)

g3 <- ggplot(Likes_data, aes(websites, avg_Likes, fill = websites,, ymax = 5000)) + 
  geom_bar(stat = "identity",col = "black", position = "dodge") + 
  geom_text(aes(label = avg_Likes), position = position_dodge(width = 0.9),vjust = -.025) +
  theme(axis.text.x = element_text(angle = 60,hjust = 1)) +
  xlab("Websites") + ylab("Average Likes ") +
  ggtitle("Average Likes to the posts of websites")

grid.arrange(g2,g3,ncol = 2)

# Comments

# Number of comments
total_Comments <- as.vector(c(sum(webA_data[,"Post.Comments"]),sum(webB_data[,"Post.Comments"]),sum(webC_data[,"Post.Comments"])))
Comments_data <- data.frame(websites,total_Comments)

g4 <- ggplot(Comments_data, aes(websites, total_Comments, fill = websites, ,ymax = 1200)) + 
  geom_bar(stat = "identity",col = "black", position = "dodge") + 
  geom_text(aes(label = total_Comments), position = position_dodge(width = 0.9),vjust = -.025) +
  theme(axis.text.x = element_text(angle = 60,hjust = 1)) +
  xlab("Websites") + ylab("Total Comments ") +
  ggtitle("Total Comments to the posts of websites")


# Average Comments to the posts of Websites

avg_Comments<- round(total_Comments/total)

avg_Comments_data <- data.frame(websites,avg_Comments)

g5 <- ggplot(avg_Comments_data, aes(websites, avg_Comments, fill = websites,, ymax = 15)) + 
  geom_bar(stat = "identity",col = "black", position = "dodge") + 
  geom_text(aes(label = avg_Comments), position = position_dodge(width = 0.9),vjust = -.025) +
  theme(axis.text.x = element_text(angle = 60,hjust = 1)) +
  xlab("Websites") + ylab("Average Comments ") +
  ggtitle("Average Comments to the posts of websites")

grid.arrange(g4,g5,ncol = 2)



# Shares


# Number of Shares
total_Shares <- as.vector(c(sum(webA_data[,"Post.Shares"]),sum(webB_data[,"Post.Shares"]),sum(webC_data[,"Post.Shares"])))
Shares_data <- data.frame(websites,total_Shares)

g6 <- ggplot(Shares_data, aes(websites, total_Shares, fill = websites, ,ymax = 1200)) + 
  geom_bar(stat = "identity",col = "black", position = "dodge") + 
  geom_text(aes(label = total_Shares), position = position_dodge(width = 0.9),vjust = -.025) +
  theme(axis.text.x = element_text(angle = 60,hjust = 1)) +
  xlab("Websites") + ylab("Total Shares ") +
  ggtitle("Total Shares to the posts of websites")


# Average Shares to the posts of Websites

avg_Shares<- round(total_Shares/total)

avg_Sharess_data <- data.frame(websites,avg_Shares)

g7 <- ggplot(avg_Sharess_data, aes(websites, avg_Shares, fill = websites,, ymax = 15)) + 
  geom_bar(stat = "identity",col = "black", position = "dodge") + 
  geom_text(aes(label = avg_Shares), position = position_dodge(width = 0.9),vjust = -.025) +
  theme(axis.text.x = element_text(angle = 60,hjust = 1)) +
  xlab("Websites") + ylab("Average Shares ") +
  ggtitle("Average Shares to the posts of websites")

grid.arrange(g6,g7,ncol = 2)



### From the above graphs we can see that Posts from 
### Website C has got maximum Likes & Website A and B have got nearly same likes
### Website A has got maximum Comments Website B has got minimum comments
### Website A has got maximum Shares & Website C has got minimum shares


# Reach

total_Reach <- as.vector(c(sum(webA_data[,"Reach"]),sum(webB_data[,"Reach"]),sum(webC_data[,"Reach"])))
Reach_data <- data.frame(websites,total_Reach)

Reach_data <- cbind(Reach_data,pos = cumsum(total_Reach)- total_Reach/2)

g8 <- ggplot(Reach_data, aes(x = factor(1), y = total_Reach, fill = websites)) +
      geom_bar(stat = "identity",col = "black",width =1) +
      coord_polar(theta = "y") +
      geom_text(aes(y = pos,label = total_Reach)) +   xlab("Websites") + ylab("Reach") +
      ggtitle("Reach of the websites")
g8


# Average Clicks & Average Click Through Rate(CTR)

total_Clicks<- as.vector(c(sum(webA_data[,"Clicks"]),sum(webB_data[,"Clicks"]),sum(webC_data[,"Clicks"])))
total_Clicks <- round(total_Clicks/total)
Clicks_data <- data.frame(websites,total_Clicks)

Clicks_data <- cbind(Clicks_data,pos = cumsum(total_Clicks)- total_Clicks/2)

g9 <- ggplot(Clicks_data, aes(x = factor(1), y = total_Clicks, fill = websites)) +
  geom_bar(stat = "identity",col = "black",width =1) +
  coord_polar(theta = "y") +
  geom_text(aes(y = pos,label = total_Clicks)) +   xlab("Websites") + ylab("Clicks") +
  ggtitle("Average Clicks on the post of the websites")



total_CTR<- as.vector(c(sum(webA_data[,"CTR"]),sum(webB_data[,"CTR"]),sum(webC_data[,"CTR"])))
total_CTR <- round(total_CTR/total)
CTR_data <- data.frame(websites,total_CTR)

CTR_data <- cbind(CTR_data,pos = cumsum(total_CTR)- total_CTR/2)

g10 <- ggplot(CTR_data, aes(x = factor(1), y = total_CTR, fill = websites)) +
  geom_bar(stat = "identity",col = "black",width =1) +
  coord_polar(theta = "y") +
  geom_text(aes(y = pos,label = total_CTR)) +   xlab("Websites") + ylab("Click Through Rate(CTR)") +
  ggtitle("Average Click Through Rate(CTR) of a post from websites")

grid.arrange(g9,g10, ncol =2)