import Data.List

--data Arbol a = Hoja 
--             | Nodo a (Arbol a) (Arbol a)
--             deriving (Show, Eq)

--arbol = Nodo 0
--               (Nodo 1 
--                     (Nodo 3 Hoja 9 Hoja 11) 
--                     (Nodo 4 Hoja 7 Hoja 12)) 
--               (Nodo 2 Hoja 5 Hoja 6)
			   
data Arbol a = Hoja a
             | Nodo a (Arbol a) (Arbol a)
             deriving (Show, Eq)

arbol= Nodo 9 (Nodo 3 (Hoja 2) (Hoja 4)) (Hoja 7)

preorden :: Arbol a -> [a]
preorden (Hoja x)     = [x]
preorden (Nodo x i d) = x : (preorden i ++ preorden d)

postorden::Arbol a->[a]
postorden (Hoja x)  =[x]
postorden (Nodo x i d) = postorden d ++ postorden i++ [x]


ordenarLista :: Ord a => [a] -> [a]
ordenarLista [] = []
ordenarLista xs = m : ordenarLista (delete m xs)
    where m = minimum xs                                      

--orden::(Arbol a, Ord b)=> a->[b] 
orden x =ordenarLista (postorden x )



