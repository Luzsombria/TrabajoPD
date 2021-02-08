-- Archivo con las funciones útiles que necesitamos en el desarrollo del resto de archivos.

-- Especificaciones del módulo
-- ------------------------------------------------------------------------
module Utiles
    (Matriz,
     matrizUnitaria,
     listaFilas,
     listaColumnas,
     diagonalesMatriz,
     actualizaValor,
     valido,
     escribeCuadricula,
     traduceCadena
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

-- ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
-- Funciones sobre matrices, arrays y todo lo tocante al desarrollo básico del programa interactivo
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
traspuesta p = array((0,0),(nf,nc)) [((i,j), p!(j,i)) | i <- [0..nf], j <- [0..nc]]
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
diagonalPMatriz n p = [p ! (x,x) | x<-[0..n]]

diagonalSMatriz :: Int -> Matriz a -> [a]
diagonalSMatriz n p = [p ! (x,n-x) | x<-[0..n]]

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

-- La siguiente función es para validar un movimiento en cualquiera de los dos juegos. Básicamente, comprueba
-- que la casilla a ocupar por el movimiento no esté ya cogida, y comprueba que el movimiento no esté fuera de rango
-- de la Matriz usada.
valido :: (Int,Int) -> Matriz Char -> Bool
valido (i,j) p = (v==' ') && (i>=imi && i<=ima) && (j>=jmi && j<=jma)
    where v = p ! (i,j)
          rangos = bounds p
          (imi,jmi) = fst rangos
          (ima,jma) = snd rangos

-- Funciones pintar las cuadrículas que necesitemos.
-- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
escribeCuadricula :: [String] -> String
escribeCuadricula [] = []
escribeCuadricula (xs:xss) = ((escribeFila xs)++"\n"++guiones++"\n")++(escribeCuadricula xss)
    where guiones = escribeGuiones (length xs)

escribeFila :: [Char] -> String
escribeFila [] = []
escribeFila (x:xs) = " "++(x:" |")++(escribeFila xs)

escribeGuiones :: Int -> String
escribeGuiones n
    | n == 0 = []
    | otherwise = "----"++(escribeGuiones (n-1))
-- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

-- Función para crear una Matriz de tipo Char a partir de un String dado.
traduceCadena :: String -> Int -> Matriz Char
traduceCadena cs n = array ((0,0),(n-1,n-1)) [(is,c) | (is,c)<-zip ind cs]
    where ind = [(i,j) | i<-[0..n-1],j<-[0..n-1]]

-- ------------------------------------------------------------------------
-- ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

-- ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
-- Funciones sobre aprendizaje automático
-- ------------------------------------------------------------------------

-- ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||