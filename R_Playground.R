python_data <- read.csv('My-Shiny-App/600_Python_Packages.csv')
head(python_data)
python_data <- python_data %>% rename(license_type = license)
colnames(python_data)


#python_packages <- read.csv("My-Shiny-App", "600_Python_Packages.csv")
python_packages <- python_packages %>% rename(license_type = license, distribution = metadata_source)


license_type_list <- unique(python_data[c("license")])
license_type_list
list.count(license_type_list)
# 
# python_data <- python_data %>% arrange(desc(version))
# head(python_data)

python_license_distinct <- python_data %>% distinct(license())
head(python_license_distinct)


# Observe layers being added with the + sign
viz <- ggplot(data=python_data, aes(x=package_name, y=version)) +
  #geom_point(aes(color=nrOfGenre), alpha=0.5) + 
  labs(title="Python Package", subtitle="my test app", y="Package Name", x="Package Version", color = "Number of Genre")


# Prints the plot
viz