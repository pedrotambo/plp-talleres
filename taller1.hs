import Test.HUnit

-- Definiciones de tipos

data AB a = Nil | Bin (AB a) a (AB a) deriving Eq
-- data AB a = Nil | Bin (AB a) a (AB a) deriving (Show, Eq)

instance Show a => Show (AB a) where
  show t = padAB t 0 0

-- Funciones auxiliares

pad :: Int -> String
pad i = replicate i ' '

padAB :: Show a => AB a -> Int -> Int -> String
padAB = foldAB (const $ const "") (\ri x rd n base ->let l = length $ show x in pad n ++ show x ++ ri 4 (base+l) ++ "\n" ++ rd (n+4+base+l) base)

-- Crea una hoja de un árbol binario AB
abHoja :: a -> AB a
abHoja x = Bin Nil x Nil

-- Devuelve una lista con los elementos de los nodos de un árbol binario AB recorridos en profundidad de izquierda a derecha
inorder :: AB a -> [a]    
inorder = foldAB [] (\i r d -> i ++ (r:d))

-- Estructuras para tests

-- Heap (<) completo
ab1 = Bin (abHoja 4) 2 (abHoja 5)
-- Heap (<) completo
ab2 = Bin (abHoja 6) 3 (abHoja 7)
-- Heap (>) completo
ab3 = Bin (Bin (abHoja 4) 5 (abHoja 2)) 7 (Bin (abHoja 3) 6 (abHoja 1))
-- Heap (<)
ab4 = Bin ab1 1 (abHoja 3)
-- ABB completo
ab5 = Bin (Bin (abHoja 1) 2 (abHoja 3)) 4 (Bin (abHoja 5) 6 (abHoja 7))
-- Heap (<)
ab6 = Bin ab1 0 (abHoja 6)
-- ABB
ab7 = Bin (Bin (abHoja 1) 2 (abHoja 4)) 5 (abHoja 7)
-- Heap (<) infinito, probar truncando
ab8 = Bin (mapAB (*2) ab8) 1 (mapAB ((+1) . (*2)) ab8)

-- Ejercicios


--recAB ::
--recAB = undefined
recAB :: b -> (AB a -> a -> AB a -> b -> b -> b) -> (AB a -> b)
recAB z _ Nil = z
recAB z f (Bin izq v der) = f izq v der (recAB z f izq) (recAB z f der)
-- recAB :: (a -> b -> b -> b) -> b -> (AB a -> b)
-- recAB _ z Nil = z
-- recAB f z (Bin izq v der) = f v (recAB f z izq) (recAB f z der)


foldAB :: b -> (b -> a -> b -> b) -> (AB a -> b)
foldAB z f = recAB z (\i r d ri rd -> f ri r rd)
-- foldAB _ z Nil = z
-- foldAB f z (Bin izq v der) = f v (recAB f z izq) (recAB f z der)


mapAB :: (a -> b) -> AB a -> AB b
mapAB f = foldAB Nil (\i r d -> Bin i (f r) d)
-- mapAB f Nil = Nil
-- mapAB f (Bin i r d) = Bin (mapAB f i) (f r) (mapAB f d)
-- mapAB = undefined

nilOCumple :: (a -> a -> Bool) -> a -> AB a -> Bool
nilOCumple p x = foldAB True (\i r d -> p x r)
-- nilOCumple p x Nil = True
-- nilOCumple p x (Bin i r d) = if p x r then True else False  

algunoCumple :: (a -> a -> Bool) -> a -> AB a -> Bool
algunoCumple p x = foldAB False (\i r d -> p x r || i || d)


maximoAB :: Ord a => AB a -> a
maximoAB = maximum . inorder
-- maximoAB l = maximum (inorder l)

esABB :: Ord a => AB a -> Bool
esABB = recAB True (\i r d esABBIzq esABBDer -> (esMayorATodos r i)  && (esMenorATodos r d)  && esABBIzq && esABBDer )
    where
        esMayorATodos = nilOCumple (>=)
        esMenorATodos = nilOCumple (<)

altura :: AB a -> Int
altura = foldAB 0 (\hi r hd -> 1 + (max hi hd))

esHeap :: (a -> a -> Bool)  -> AB a -> Bool
esHeap p = recAB True (\i r d esHeapIzq esHeapDer -> 
        (valeParaTodos r i) && (valeParaTodos r d) && esHeapIzq && esHeapDer && alturaNoDifiereMasDeUno i d)
    where
        valeParaTodos = nilOCumple p
        alturaNoDifiereMasDeUno = (\i d -> abs(altura i - altura d) <= 1)
-- esHeap = undefined

esMaxHeap :: Ord a => AB a -> Bool
esMaxHeap = esHeap (>=) 

esMinHeap :: Ord a => AB a -> Bool
esMinHeap = esHeap (<=)

cantidadDeNodos :: AB a -> Int
cantidadDeNodos = foldAB 0 (\nodosi r nodosd -> 1 + nodosi + nodosd)

completo :: AB a -> Bool
completo ab = (2 ^ (altura ab)) - 1 == (cantidadDeNodos ab)  

insertarABB :: Ord a => AB a -> a -> AB a
insertarABB ab e = 
    recAB (Bin Nil e Nil) (\i r d ri rd ->
            if e <= r 
                then (Bin (insertarABB i e) r d)
                else (Bin i r (insertarABB d e))
                ) ab



insertarHeap :: Ord a => (a -> a -> Bool) -> AB a -> a -> AB a
insertarHeap p ab e =
    recAB (Bin Nil e Nil) (\i r d ri rd ->
            if p e r 
                then 
                    if (completo i) && (cantidadDeNodos i) > (cantidadDeNodos d)
                        then (Bin i e (insertarHeap p d r))
                        else (Bin (insertarHeap p i r) e d)
                else
                    if (completo i) && (cantidadDeNodos i) > (cantidadDeNodos d)
                        then (Bin i r (insertarHeap p d e))
                        else (Bin (insertarHeap p i e) r d)
            ) ab


truncar :: AB a -> Int -> AB a
truncar ab t =
      recAB Nil (\i r d ri rd ->
          if t == 0
            then Nil
            else (Bin (truncar i (t-1)) r (truncar d (t-1)) )
        ) ab

-- Ejecución de los tests
main :: IO Counts
main = do runTestTT allTests

-- Variables para tests
raizMayorADiez = Bin (Bin Nil 15 Nil) 19 (Bin Nil 3 Nil)
conUnTres = Bin (Bin Nil 15 Nil) 19 (Bin Nil 3 Nil)

arbolVacio = Nil

a = Bin Nil 3 Nil
b = Bin a 10 Nil
c = Bin a 500 b

unABBYNoHeap = Bin Nil 3 (Bin Nil 10 Nil)

unMaxHeapYNoABB = Bin (abHoja 3) 500 (abHoja 10)

unMinHeap = Bin (abHoja 1000) 2 (abHoja 15000)

unNoHeap = Bin (Bin (Bin (Bin Nil 1 Nil) 2 Nil) 5 Nil) 10 (abHoja 20)

unCompleto = Bin a 1000 a

unaHoja = abHoja 5

unArbolConCuatroNodos = insertarHeap (<) unMinHeap 10

unNoCompleto = unMaxHeapYNoABB

unHeapPiola = Bin (abHoja 5) 20 (abHoja 10)

unMaxHeapSimple = Bin (abHoja 5) 20 (Bin Nil 10 (abHoja 7))

unMinHeapSimple = Bin (abHoja 5) 2 (Bin Nil 7 (abHoja 20))

unCasoHeapAlturaMal = Bin (abHoja 5) 20 (Bin Nil 10 (Bin Nil 7 (abHoja 2)))


completoConTresNodos :: a -> a -> a -> AB a
completoConTresNodos i r d = Bin (abHoja i) r (abHoja d)

unArbolCompleto = Bin (completoConTresNodos 10 100 1) 1000 (completoConTresNodos 5 500 10)

-- cambiamos el tipo de truncar de integer a int
truncarHastaAnteUltimoNivel :: AB a -> AB a
truncarHastaAnteUltimoNivel = (\ab -> truncar ab (altura ab - 1) )

listaToHeap :: Ord a => [a] -> AB a
listaToHeap = foldl (insertarHeap (<=)) Nil 

allTests = test [
  "ejercicio1" ~: testsEj1,
  "ejercicio2" ~: testsEj2,
  "ejercicio3" ~: testsEj3,
  "ejercicio4" ~: testsEj4,
  "ejercicio5" ~: testsEj5,
  "ejercicio6" ~: testsEj6,
  "ejercicio7" ~: testsEj7
  ]

testsEj1 = test [
  [1,2,4,5,7] ~=? inorder ab7,
  [1,2,3,4,5,6,7] ~=? inorder ab5
  ]
  
testsEj2 = test [
  [5,3,6,1,7] ~=? inorder (mapAB (+1) ab6)
  ]

testsEj3 = test [
  True ~=? nilOCumple (<) 10 raizMayorADiez,
  True ~=? nilOCumple (<) 10 arbolVacio,
  False ~=? nilOCumple (>) 10 raizMayorADiez,
  True ~=? algunoCumple (==) 3 conUnTres
  ]

testsEj4 = test [
  True ~=? esMaxHeap unMaxHeapYNoABB,
  False ~=? esMaxHeap unMinHeap,
  True ~=? esHeap (<=) unMinHeap
  ]

testsEj5 = test [
  0 ~=? cantidadDeNodos arbolVacio,
  1 ~=? cantidadDeNodos unaHoja,
  4 ~=? cantidadDeNodos unArbolConCuatroNodos,
  True ~=? completo unArbolCompleto,
  True ~=? esMaxHeap unMaxHeapSimple,
  True ~=? esMinHeap unMinHeapSimple,
  False ~=? esMaxHeap unCasoHeapAlturaMal
  ]

testsEj6 = test [
  True ~=? esHeap (<) (insertarHeap (<) (insertarHeap (<) ab6 3) 1),
  True ~=? esABB (insertarABB (insertarABB ab7 6) 9),
  True ~=? esABB (insertarABB (insertarABB unABBYNoHeap 10) 50),
  True ~=? esMaxHeap (insertarHeap (>=) unMaxHeapSimple 2),
  True ~=? esMinHeap (insertarHeap (<=) unMinHeapSimple 500)
  ]

testsEj7 = test [
  [8,4,12,2,10,6,14,1,9,5,13,3,11,7,15] ~=? inorder (truncar ab8 4),
  True ~=? esHeap (<) (truncar ab8 5)
  ]

testsEj8 = test [
  True ~=? completo (truncar ab8 10),
  True ~=? esHeap (<) (listaToHeap [1,2,3,4,5,6]),
  True ~=? esHeap (<) (truncar ab8 20),
  True ~=? (completo . truncarHastaAnteUltimoNivel) (listaToHeap [1,2,3,4,5,6,135135,134123,12312,312313,1235,-10124,1350135,12310,23123,123])
  ]
