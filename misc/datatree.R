library(data.tree)
data(acme)
library(treemap)
data(GNI2014)

#create a pathString
GNI2014 <- GNI2014 %>% 
  mutate(pathString = paste("world", continent, country,
                            sep = "/"))
#create tree
population <- as.Node(GNI2014)
print(population, "iso3", "population", "GNI", limit = 20)

#Actives look and feel like fields, but are dynamically evaluated
population$isRoot
population$height
population$fieldsAll
population$path

#OO-style methods
sum(population$Get("population", filterFun = isLeaf))

#Traditional R Methods
popClone <- Clone(population)
as.data.frame(popClone)

#each ... argument refers to a level to climb
population$Climb(position = 1, name = "Canada")

#Custom Fields
population$`North America`$Canada$HDI <- 9999
population$`North America`$AddChild("New California", war=TRUE)
NODE_RESERVED_NAMES_CONST

func <- function() {
  return (list(1,2))
}
