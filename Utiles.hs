-- Archivo con las funciones útiles que necesitamos en el desarrollo del resto de archivos.

-- Especificaciones del módulo
-- ------------------------------------------------------------------------
module Utiles
    (Matriz,
     matrizUnitaria,
     listaFilas,
     listaColumnas,
     diagonalesMatriz
    ) where
-- ------------------------------------------------------------------------

-- Módulos importados
-- ------------------------------------------------------------------------
import Data.Array
-- ------------------------------------------------------------------------

-- Definimos el tipo Matriz que vamos a usar para los estados de nuestro juego. En este caso, las matrices tendrán
-- como índices números enteros tanto para filas como para columnas.
-- ------------------------------------------------------------------------
type Matriz a = Array (Int,Int) a
-- ------------------------------------------------------------------------

-- Funciones
-- ------------------------------------------------------------------------

-- Función para crear una matriz de tamaño n, en la que todos los valores son el mismo. Ej: Matriz de ceros,
-- o de unos, o de doses, etc...
matrizUnitaria :: Int -> a -> Matriz a
matrizUnitaria n v = array ((0,0),(n-1,n-1)) [((i,j),v) | i<-[0..n-1],j<-[0..n-1]]

-- Función para crear una Matriz a partir de una lista de listas.
listaMatriz :: [[a]] -> Matriz a
listaMatriz xss = array ((0,0),(f-1,c-1)) [((i,j),k) | ((i,j),k)<-zip indices des]
    where f = length xss
          c = length (head xss)
          indices = [(i,j) | i<-[0..f-1],j<-[0..c-1]]
          des = concat xss

-- Función para conseguir el número de columnas de una Matriz.
numColumnas:: Matriz a -> Int
numColumnas m = c where (_,c) = last (indices m)

-- Función para conseguir el número de filas de una Matriz.
numFilas :: Matriz a -> Int
numFilas m = f where (f,_) = last (indices m)

-- Función que separa una lista en listas de tamaño n.
separa :: Int -> [a] -> [[a]]
separa n xs
    | null xs = []
    | length xs < n = xs:(separa n [])
    | otherwise = ys:(separa n rs)
        where ys = take n xs
              rs = drop n xs

-- Traspuesta de una Matriz.
traspuesta :: Matriz a -> Matriz a
traspuesta p = array((0,0),(nf-1,nc-1)) [((i,j), p!(j,i)) | i <- [0..nf-1], j <- [0..nc-1]]
    where 
        nf = numColumnas p
        nc = numFilas p

-- Función para separar una Matriz en una lista con sus filas.
listaFilas :: Matriz a -> [[a]]
listaFilas p = separa (n+1) elm
    where n = numColumnas p
          elm = elems p

-- Función para separar una Matriz en una lista con sus columnas.
listaColumnas :: Matriz a -> [[a]]
listaColumnas p = listaFilas $ traspuesta p

-- Funciones para conseguir los n primeros valores de las diagonales principal y secundaria de una Matriz.
diagonalPMatriz :: Int -> Matriz a -> [a]
diagonalPMatriz n p = [p ! (x,x) | x<-[0..n-1]]

diagonalSMatriz :: Int -> Matriz a -> [a]
diagonalSMatriz n p = [p ! (x,n-x-1) | x<-[0..n-1]]

-- Función para conseguir ambas diagonales en una lista de listas.
diagonalesMatriz :: Matriz a -> [[a]]
diagonalesMatriz p = (diagonalPMatriz n p):[diagonalSMatriz n p]
    where n = numFilas p

-- Función para cambiar el valor de una posición de la Matriz entrante.
actualizaValor :: (Int, Int) -> a -> Matriz a -> Matriz a
actualizaValor (i,j) v p = listaMatriz ass
    where xss = listaFilas p
          xs = xss !! i
          as = [if ind == j then v else x | (x,ind)<-zip xs [0..]]
          ass = [if ind == i then as else ss | (ss,ind)<-zip xss [0..]]

-- ------------------------------------------------------------------------