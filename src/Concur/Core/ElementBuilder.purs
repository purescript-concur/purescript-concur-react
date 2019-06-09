module Concur.Core.ElementBuilder where

-- | A View backend that can build nodes
-- | e, f, p, v, are all raw types, highly specific to the backend
-- | e  == type of element (e.g. 'div')
-- | f  == props container (for example, an array)
-- | p  == a single prop
-- | v  == the monoidal view
class ElementBuilder e f p v | v -> e, v -> f, v -> p where
  buildNode :: e -> f p -> v -> v
  buildLeaf :: e -> f p -> v

-- | A "Freeish" implementation of element builder
-- | That fixes the type of view
data Element e f p
  = NodeElement e (f p) (Element e f p)
  | LeafElement e (f p)

instance elementBuilderElement :: ElementBuilder e f p (Element e f p) where
  buildNode e fp v = NodeElement e fp v
  buildLeaf e fp = LeafElement e fp
