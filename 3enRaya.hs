-- Este archivo contiene el main final del juego interactivo 3 en raya

-- Módulos importados
-- ------------------------------------------------------------------------
import Utiles
import System.Directory
import Data.Array
-- ------------------------------------------------------------------------


-- Función main
main :: IO()
main = undefined

-- También definimos el tipo Cuadricula que va a ser la referencia para el correcto desarrollo del juego. El tipo 'a' de la
-- Matriz lo vamos a sustituir por Char ya que vamos a usar 'X' y 'O' para representar los círculos y cruces del juego.
type Cuadricula = Matriz Char

-- Creamos la cuadrícula vacía para comenzar un nuevo juego.
inicial :: Cuadricula
inicial = matrizUnitaria 3 ' '

-- Vamos a crear una función para conocer si un juego se encuentra en el estado final o no.
finalizado :: Cuadricula -> Bool
finalizado c = (llena c) || (hay3EnRaya c)

-- Función para saber si la Cuadrícula del juego que nos pasan ya está llena.
llena :: Cuadricula -> Bool
llena = undefined

-- Función para saber si hay un jugador con 3 en raya.
hay3EnRaya :: Cuadricula -> Bool
hay3EnRaya = undefined