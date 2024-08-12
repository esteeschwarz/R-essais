library(XML)

# Your XML data
xml_data <- '<node1 type="url">sample-url<child1 type="title">sample-txt</child1></node1><node1 type="url">sample-url<child1 type="title">sample-txt</child1></node1>'
#xml_data <- '<node1 type="url">sample-url0</node1><node1 type="url">sample-url2</node1><node1 type="url">sample-url3</node1>

xml_data
#xml_data <-''
# Parse the XML
doc <- xmlParse(xml_data)
#?node
# Extract relevant information
#xpat
nodes <- xpathApply(doc, "//node()")  # Get all nodes
nodes
csv_rows <- lapply(nodes, function(node) {
  att <- xmlGetAttr(node, "type")
  value <- xmlValue(node)
  text <- ifelse(is.null(xmlChildren(node)), "", xmlValue(xmlChildren(node)))
  paste(att, value, text, sep = ";")
})

# Combine rows and add header
csv_content <- paste("x;node;att;value;text", unlist(csv_rows), sep = "\n")

# Print the CSV content
cat(csv_content)

####

# Read the CSV (assuming it's in a file named 'data.csv')
csv_data <- read.csv("yudale.desc.csv", sep = ";", header = TRUE)

# Create an empty XML document
xml_doc <- newXMLDoc()

# Create the root node
root_node <- newXMLNode("root", doc = xml_doc)

# Add child nodes based on CSV data
for (i in 1:nrow(csv_data)) {
  node_name <- csv_data$node[i]
  att <- csv_data$att[i]
  value <- csv_data$value[i]
  text <- csv_data$text[i]
  
  node <- newXMLNode(node_name, attrs = c(type = att), doc = xml_doc)
  if (text != "") {
    child_node <- newXMLNode("child1", doc = xml_doc)
    xmlValue(child_node) <- text
    xmlAddChild(node, child_node)
  }
  xmlValue(node) <- value
  xmlAddChild(root_node, node)
}

# Print the reconstructed XML
cat(saveXML(xml_doc))

cat(as(toHTML(rnorm(10)), "character"))
cat(as(toHTML(csv_data), "character"))
xmlHashTree(title)

