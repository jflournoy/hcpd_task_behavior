library(data.table)
library(igraph)

#We need to get info on family composition and relationship type (e.g., twin,
#sib, etc)

rel_dat <- data.table::fread('HCPD_relationships.csv')


#First, I want to stack all the relationships on top of one another, so each row
#codes just one relationship.
rel_dat_pieces <- list(rel_dat[, c(2, 3, 4)], 
                       rel_dat[, c(4, 5, 6)][`Relationship 2` != ""],
                       rel_dat[, c(6, 7, 8)][`Relationship 3` != ""])
rel_dat_pieces <- lapply(rel_dat_pieces, setnames, c('from', 'relationship', 'to'))
rel_dat_l <- rbindlist(rel_dat_pieces)[, c('from', 'to', 'relationship')]
#This keeps only HCD participants
rel_dat_l <- unique(rel_dat_l[grepl('^HCD.*', from) & grepl('^HCD.*', to)])

#There are still some duplicates where one row's "from" "to" is another row's
#"to" "from". We can do this by melting the data so each row becomes two rows
#with a "from" row and a "to" row. We can then sort each of these pairs by the
#ID (the order doesn't matter), and recode the to-from rows. When we widen this
#we will be able to capture the duplicates as the from-to order will no longer
#be swapped.
rel_dat_l[, row := 1:.N]
rel_dat_l_l <- melt(rel_dat_l, id.vars = c('row', 'relationship'))
setkey(rel_dat_l_l, row, value)
rel_dat_l_l[, variable := rep(c('from', 'to'), .N/2)]
rel_dat_l_l_w <- dcast(rel_dat_l_l, ... ~ variable)
setkey(rel_dat_l_l_w, from, to)

rel_dat_f <- unique(rel_dat_l_l_w, by = c('from', 'to'))[, row := NULL]

#Let's see what kind of relations we have here
rel_dat_f[, .N, by = 'relationship']

#hmmmmmmmm
rel_dat_f[relationship == 'parent-child']

rel_dat_n <- copy(rel_dat)
setnames(rel_dat_n, c('site', 'id1', 'r1', 'id2', 'r2', 'id3', 'r3', 'id4')) 
rel_dat_n[id1 %in%  c('HCD1632244', 'HCD1857066', 'HCD2703751') | 
            id2 %in%  c('HCD1632244', 'HCD1857066', 'HCD2703751') | 
            id3 %in%  c('HCD1632244', 'HCD1857066', 'HCD2703751') |
            id4 %in%  c('HCD1632244', 'HCD1857066', 'HCD2703751') ]

#do the demos make sense?
demos <- data.table::fread('HCPD_COMBINED20200608.csv',
                           select = c('id', 'age'))

#They do not...the child ages are there but the other IDs are not in the demos.
#They also don't exist on intradb.
merge(demos, 
      merge(demos, rel_dat_f[relationship == 'parent-child'], by.x = 'id', by.y = 'from'),
      by.x = 'id', by.y = 'to', all.y = TRUE)

#so we drop them
rel_dat_f <- rel_dat_f[relationship != 'parent-child']
rel_dat_f[, .N, by = 'relationship']

#drop the first cousin, fraternal twin is genetically similar to full sib
rel_dat_f <- rel_dat_f[relationship != 'first cousin']
rel_dat_f[, relationship := fifelse(relationship == 'half-sibling', 'half sibling',
                                   fifelse(relationship == 'fraternal twin', 'full sibling',
                                           relationship))]
rel_dat_f[, .N, by = 'relationship']

g <- graph_from_data_frame(rel_dat_f[, c(2,3,1)], directed = FALSE)
plot(g, vertex.size = 2, vertex.label = NA)

d_g <- decompose(g)

get.edge.attribute(d_g[[1]])

get_relationship_counts <- function(g){
  t <- table(get.edge.attribute(g)[['relationship']])
  return(t)
}
make_relcount_labels <- function(rc){
  paste(paste(abbreviate(names(rc)), rc, sep = '-'), collapse = '_')
}
get_vertices_names <- function(g){
  igraph::get.vertex.attribute(g)$name
}
make_family_dt <- function(g){
  ids <- get_vertices_names(g)
  fam_lab <- make_relcount_labels(get_relationship_counts(g))
  data.table(id = ids, family_type = fam_lab)
}

family_sets <- lapply(d_g, make_family_dt)
