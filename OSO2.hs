-- Módulos importados
-- ------------------------------------------------------------------------
import Utiles
import System.Directory
import Data.Array
import Data.Char
import Data.List
import System.IO
-- ------------------------------------------------------------------------

-- Vamos a crear una lista con los movimientos para replicar la pseudoaleatoriedad conseguida con el 3 en raya.
listaAleatoria = [(4,4),(2,1),(0,0),(2,5),(0,2),(2,7),(0,3),(3,0),(0,5),(1,0),(6,0),(1,2),(3,5),(1,3),(1,4),(1,6),(1,7),(2,0),(7,1),(4,5),(2,2),(0,1),(2,6),(5,0),(5,3),(3,1),(3,2),(3,3),(3,4),(0,7),(1,1),(6,1),(3,6),(3,7),(4,0),(4,2),(4,3),(7,0),(4,6),(7,5),(4,7),(2,3),(2,4),(5,2),(5,4),(5,5),(5,6),(5,7),(0,6),(6,2),(6,3),(6,4),(6,5),(6,6),(1,5),(6,7),(4,1),(7,2),(7,3),(7,4),(7,6),(5,1),(0,4)]

-- Función main
main :: IO()
main = do
    modo <- leeDigito "Escoja. Modo 1 jugador o 2 jugadores. Para escoger simplemente ponga el número (1 ó 2): "
    if modo == 1
        then iniciaJuego2
        else iniciaJuego

-- Definimos el tipo Cuadricula que va a ser la referencia para el correcto desarrollo del juego. El tipo 'a' de la
-- Matriz lo vamos a sustituir por Char ya que vamos a usar 'O' y 'S' para formar la palabra OSO.
type Cuadricula = Matriz Char

-- Necesitaremos una función que nos cree cuadrículas con los valores que necesitemos más adelante.
creaCuadricula :: (Int,Int) -> [Char] -> Cuadricula
creaCuadricula is vs = matrizNueva is vs

-- Creamos la Cuadrícula vacía del tamaño escogido para comenzar un nuevo juego.
inicial :: Int -> Cuadricula
inicial tam = matrizUnitaria tam ' '

-- Vamos a crear una función para conocer si un juego se encuentra en el estado final o no.
-- ------------------------------------------------------------------------
finalizado :: Cuadricula -> Bool
finalizado c = (llena c)

-- Función auxiliar para saber si la Cuadrícula del juego que nos pasan ya está llena.
llena :: Cuadricula -> Bool
llena c = (length es) == suma
    where es = elems c
          os = [o | o<-es,o=='O']
          ss = [s | s<-es,s=='S']
          suma = (length ss)+(length os)

-- ------------------------------------------------------------------------

-- Función para conseguir los dígitos que vamos a necesitar en nuestra interacción.
leeDigito :: String -> IO Int
leeDigito c = do
    putStr c
    digito <- getLine
    if (length digito == 1) && isDigit (head digito)
        then return (read digito)
        else do 
            putStrLn "ERROR: No has introducido un dígito"
            d <- leeDigito c
            return d

-- Función para gestionar que entren índices que estén dentro de la Cuadrícula.
revisaIn :: (Int,Int) -> IO (Int,Int)
revisaIn (i,j) = do
    f <- leeDigito "-Primero indica la fila: "
    c <- leeDigito "-Ahora indica la columna: "
    if (f>=i && f<=j) && (c>=i && c<=j)
        then return (f, c)
        else do
            putStrLn "¡Fila o columna fuera de la cuadrícula. Vuelva a escoger!"
            revisaIn (i,j)

-- Función para realizar una jugada.
jugada :: Cuadricula -> (Int,Int) -> Char -> IO Cuadricula
jugada c (i,j) v
    | valido (i,j) c = do
        let cn = actualizaValor v c (i,j)
        return cn
    | otherwise = do
        putStrLn "Jugada no válida, vuelva a intentarlo."
        putStrLn "-Recuerde que no puede pintar casillas ya ocupadas."
        let rangosC = bounds c
        let par1 = fst rangosC
        let par2 = snd rangosC
        let menor = fst par1
        let mayor = snd par2
        putStr "-Para escoger casilla recuerda que los números que puedes escoger oscilan entre "
        putStrLn $ (show menor) ++ " y " ++ (show mayor) ++ "."
        (fil,col) <- revisaIn (menor,mayor)
        jugada c (fil,col) v

-- Función para representar el estado del juego en consola.
representaCuadricula :: Cuadricula -> IO()
representaCuadricula c = do
    let fs = listaFilas c
    let cuadricula = escribeCuadricula fs
    putStrLn $ "\n" ++ cuadricula

-- Función para cambiar de jugador 1 a jugador 2.
siguiente :: Int -> Int
siguiente j = if j == 1 then 2 else 1

-- Función para conseguir el carácter pedido en la interacción.
devuelveChar :: String -> IO Char
devuelveChar c = do
    putStrLn c
    car <- getChar
    if (car=='O' || car=='S')
        then return car
        else do 
            putStrLn "ERROR: Sólo son admisibles la 'O' o la 'S'. Inténtelo de nuevo."
            d <- devuelveChar c
            return d

-- Funciones para ayudar con los cálculos de las puntuaciones.
-- ------------------------------------------------------------------------

-- Primero vamos a dividir en dos los cálculos para reducir su dificultad.
calculaPuntuacion :: Cuadricula -> (Int,Int) -> Char -> Int
calculaPuntuacion c (i,j) v
    | v=='O' = calculaPuntuacionO c (i,j)
    | otherwise = calculaPuntuacionS c (i,j)

-- De los dos cálculos posibles, el de 'S' es más corto. Ya que de las 8 posibles formas de puntuar sólo tenemos que mirar
-- la mitad gracias a la naturaleza propia del juego.
calculaPuntuacionS :: Cuadricula -> (Int,Int) -> Int
calculaPuntuacionS c (i,j) = sum [miraOS c (i,j) ind alrededor | ind<-contienenO]
    where lc = snd $ snd $ bounds c
          alrededor = [(m,n) | m<-[i-1..i+1],n<-[j-1..j+1], m>=0,n>=0,m<=lc,n<=lc,(m,n)/=(i,j)]
          contienenO = [ind | ind<-alrededor,(c ! ind)=='O']

-- Esta será la función auxiliar que ayude a calcular los puntos con las 'S'.
miraOS :: Cuadricula -> (Int,Int) -> (Int,Int) -> [(Int,Int)] -> Int
miraOS c (i,j) (m,n) is
    | (m<i) && (n<j) = if length ([1 | (x,y)<-is,x>i,y>j,(c ! (x,y))=='O'])>0 then 1 else 0
    | (m<i) && (n==j) = if length ([1 | (x,y)<-is,x>i,y==j,(c ! (x,y))=='O'])>0 then 1 else 0
    | (m<i) && (n>j) = if length ([1 | (x,y)<-is,x>i,y<j,(c ! (x,y))=='O'])>0 then 1 else 0
    | (m==i) && (n<j) = if length ([1 | (x,y)<-is,x==i,y>j,(c ! (x,y))=='O'])>0 then 1 else 0
    | otherwise = 0

-- Con esta vamos a hacer lo mismo que la anterior pero teniendo en cuenta que estamos tratando con las 'O'
-- no con las 'S'.
calculaPuntuacionO :: Cuadricula -> (Int,Int) -> Int
calculaPuntuacionO c (i,j) = sum [miraOO c (i,j) (m,n) lc | (m,n)<-contienenS]
    where lc = snd $ snd $ bounds c
          alrededor = [(m,n) | m<-[i-1..i+1],n<-[j-1..j+1], m>=0,n>=0,m<=lc,n<=lc,(m,n)/=(i,j)]
          contienenS = [ind | ind<-alrededor,(c ! ind)=='S']

-- Esta será la función auxiliar que ayude a calcular los puntos con las 'O'.
miraOO :: Cuadricula -> (Int,Int) -> (Int,Int) -> Int -> Int
miraOO c (i,j) (m,n) lc
    | m-1<0 = compruebaLSu c (i,j) (m,n)
    | n-1<0 = compruebaLIz c (i,j) (m,n)
    | m+1>lc = compruebaLIn c (i,j) (m,n)
    | n+1>lc = compruebaLDe c (i,j) (m,n)
    | (m<i) && (n<j) = if (c ! (m-1,n-1))=='O' then 1 else 0
    | (m<i) && (n==j) = if (c ! (m-1,n))=='O' then 1 else 0
    | (m<i) && (n>j) = if (c ! (m-1,n+1))=='O' then 1 else 0
    | (m==i) && (n<j) = if (c ! (m,n-1))=='O' then 1 else 0
    | (m==i) && (n>j) = if (c ! (m,n+1))=='O' then 1 else 0
    | (m>i) && (n<j) = if (c ! (m+1,n-1))=='O' then 1 else 0
    | (m>i) && (n==j) = if (c ! (m+1,n))=='O' then 1 else 0
    | (m>i) && (n>j) = if (c ! (m+1,n+1))=='O' then 1 else 0
    | otherwise = 0

-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
compruebaLSu :: Cuadricula -> (Int,Int) -> (Int,Int) -> Int
compruebaLSu c (i,j) (m,n)
    | (m==i) && (n<j) = if (c ! (m,n-1))=='O' then 1 else 0
    | (m==i) && (n>j) = if (c ! (m,n+1))=='O' then 1 else 0
    | (m>i) && (n<j) = if (c ! (m+1,n-1))=='O' then 1 else 0
    | (m>i) && (n==j) = if (c ! (m+1,n))=='O' then 1 else 0
    | (m>i) && (n>j) = if (c ! (m+1,n+1))=='O' then 1 else 0
    | otherwise = 0

compruebaLIz :: Cuadricula -> (Int,Int) -> (Int,Int) -> Int
compruebaLIz c (i,j) (m,n)
    | (m<i) && (n==j) = if (c ! (m-1,n))=='O' then 1 else 0
    | (m<i) && (n>j) = if (c ! (m-1,n+1))=='O' then 1 else 0
    | (m==i) && (n>j) = if (c ! (m,n+1))=='O' then 1 else 0
    | (m>i) && (n==j) = if (c ! (m+1,n))=='O' then 1 else 0
    | (m>i) && (n>j) = if (c ! (m+1,n+1))=='O' then 1 else 0
    | otherwise = 0

compruebaLIn :: Cuadricula -> (Int,Int) -> (Int,Int) -> Int
compruebaLIn c (i,j) (m,n)
    | (m<i) && (n<j) = if (c ! (m-1,n-1))=='O' then 1 else 0
    | (m<i) && (n==j) = if (c ! (m-1,n))=='O' then 1 else 0
    | (m<i) && (n>j) = if (c ! (m-1,n+1))=='O' then 1 else 0
    | (m==i) && (n<j) = if (c ! (m,n-1))=='O' then 1 else 0
    | (m==i) && (n>j) = if (c ! (m,n+1))=='O' then 1 else 0
    | otherwise = 0

compruebaLDe :: Cuadricula -> (Int,Int) -> (Int,Int) -> Int
compruebaLDe c (i,j) (m,n)
    | (m<i) && (n<j) = if (c ! (m-1,n-1))=='O' then 1 else 0
    | (m<i) && (n==j) = if (c ! (m-1,n))=='O' then 1 else 0
    | (m==i) && (n<j) = if (c ! (m,n-1))=='O' then 1 else 0
    | (m>i) && (n<j) = if (c ! (m+1,n-1))=='O' then 1 else 0
    | (m>i) && (n==j) = if (c ! (m+1,n))=='O' then 1 else 0
    | otherwise = 0

-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

-- ------------------------------------------------------------------------
-- Función para continuar una partida. Recibe la Cuadrícula estado del juego y el jugador que tiene turno.
juegoMedio :: Cuadricula -> Int -> (Int,Int) -> IO()
juegoMedio c j puntuaciones = do
    putStrLn "Estado del juego:\n"
    representaCuadricula c
    putStrLn $ "Puntuación Jugador 1 = " ++ (show (fst puntuaciones)) ++ ", puntuación Jugador 2 = " ++ (show (snd puntuaciones))
    putStrLn $ "-Le toca al jugador " ++ (show j)
    let rangosC = bounds c
    let par1 = fst rangosC
    let par2 = snd rangosC
    let menor = fst par1
    let mayor = snd par2
    putStr "-Para escoger casilla recuerda que los números que puedes escoger oscilan entre "
    putStrLn $ (show menor) ++ " y " ++ (show mayor) ++ "."
    (fil,col) <- revisaIn (menor,mayor)
    v <- devuelveChar "-Indique si quiere colocar una 'O' o una 'S'"
    cn <- jugada c (fil,col) v
    let calculo = calculaPuntuacion cn (fil,col) v
    if j == 1
        then do
            let nuevasP = ((fst puntuaciones) + calculo, snd puntuaciones)
            gestionaTurno cn j nuevasP
        else do
            let nuevasP = (fst puntuaciones, (snd puntuaciones) + calculo)
            gestionaTurno cn j nuevasP

-- Función para continuar una partida con la máquina.
juegoMedio2 :: Cuadricula -> Int -> (Int,Int) -> Int -> IO()
juegoMedio2 c j puntuaciones dif = do
    putStrLn "Estado del juego:\n"
    representaCuadricula c
    let rangosC = bounds c
    let par1 = fst rangosC
    let par2 = snd rangosC
    let menor = fst par1
    let mayor = snd par2
    putStrLn $ "Puntuación Jugador = " ++ (show (fst puntuaciones)) ++ ", puntuación Máquina = " ++ (show (snd puntuaciones))
    if j == 1
        then do        
            putStrLn "-Le toca al jugador"
            putStr "-Para escoger casilla recuerda que los números que puedes escoger oscilan entre "
            putStrLn $ (show menor) ++ " y " ++ (show mayor) ++ "."
            (fil,col) <- revisaIn (menor,mayor)
            v <- devuelveChar "-Indique si quiere colocar una 'O' o una 'S'"
            cn <- jugada c (fil,col) v
            let calculo = calculaPuntuacion cn (fil,col) v
            let nuevasP = ((fst puntuaciones) + calculo, snd puntuaciones)
            gestionaTurno2 cn j nuevasP dif
        else do
            putStrLn "-Le toca a la máquina"
            let (cn,pos,v) = trataDificultad (c,puntuaciones) dif
            let calculo = calculaPuntuacion cn pos v
            let nuevasP = (fst puntuaciones, (snd puntuaciones) + calculo)
            gestionaTurno2 cn j nuevasP dif

-- Función para manejar lo que le ocurre al juego entre turno y turno. Esta función se usará también para
-- gestionar el comienzo de un juego nuevo en el main.
gestionaTurno :: Cuadricula -> Int -> (Int,Int) -> IO()
gestionaTurno c j puntuaciones = do
    if (finalizado c)
        then if (fst puntuaciones)==(snd puntuaciones)
            then putStrLn "Empate..."
            else if (fst puntuaciones)>(snd puntuaciones)
                then do
                    representaCuadricula c
                    putStrLn "¡El jugador 1 ha ganado!"
                else do
                    representaCuadricula c
                    putStrLn "¡El jugador 2 ha ganado!"
        else do
            let jn = siguiente j
            representaCuadricula c
            putStrLn "¿Desea guardar partida?"
            putStrLn "Si desea guardar partida escriba <<SI>> por favor"
            seguridad <- getLine
            deseo <- getLine
            if deseo == "SI"
                then do
                    putStrLn "En ese caso escriba un nombre para el archivo de guardado"
                    nombre <- getLine
                    guardarPartida c jn puntuaciones nombre
                    putStrLn "¿Desea seguir jugando?"
                    putStrLn "Si desea seguir jugando escriba <<SI>> por favor. En caso contrario se entenderá como que no."
                    deseo2 <- getLine
                    if deseo2=="SI"
                        then juegoMedio c jn puntuaciones
                        else return ()
                else juegoMedio c jn puntuaciones

-- Función para manejar lo que le ocurre al juego entre turno y turno en el modo de 1 jugador.
gestionaTurno2 :: Cuadricula -> Int -> (Int,Int) -> Int -> IO()
gestionaTurno2 c j puntuaciones dif = do
    if (finalizado c)
        then if (fst puntuaciones)==(snd puntuaciones)
            then putStrLn "Empate..."
            else if (fst puntuaciones)>(snd puntuaciones)
                then do
                    representaCuadricula c
                    putStrLn "¡El jugador ha ganado!"
                else do
                    representaCuadricula c
                    putStrLn "La máquina ha ganado..."
        else do
            let jn = siguiente j
            representaCuadricula c
            putStrLn "¿Desea guardar partida?"
            putStrLn "Si desea guardar partida escriba <<SI>> por favor"
            seguridad <- getLine
            deseo <- getLine
            if deseo == "SI"
                then do
                    putStrLn "En ese caso escriba un nombre para el archivo de guardado"
                    nombre <- getLine
                    guardarPartida c jn puntuaciones nombre
                    putStrLn "¿Desea seguir jugando?"
                    putStrLn "Si desea seguir jugando escriba <<SI>> por favor. En caso contrario se entenderá como que no."
                    deseo2 <- getLine
                    if deseo2=="SI"
                        then juegoMedio2 c jn puntuaciones dif
                        else return ()
                else juegoMedio2 c jn puntuaciones dif

-- Función para generar una partida nueva.
partidaNueva :: IO ()
partidaNueva = do juegoMedio (inicial 8) 1 (0,0)

-- Función para cargar partida. Supondremos que los datos de los guardados son siempre correctos.
cargarPartida :: IO [String]
cargarPartida = do
    putStrLn "-Escriba el nombre del fichero que guarda la partida"
    fichero <- getLine
    existe <- doesFileExist fichero
    if existe
        then do
            contenido <- readFile fichero
            let lineas = [l | l<-lines contenido, length l > 0]
            return lineas
        else do
            putStrLn "Error, este fichero no existe en el directorio actual."
            cargarPartida

-- Función para crear un guardado del juego.
guardarPartida :: Cuadricula -> Int -> (Int,Int) -> FilePath -> IO ()
guardarPartida c j (p1,p2) nombre = do
    let estado = elems c
    let jugador = show j
    let puntuaciones = (show p1) ++ "\n" ++ (show p2)
    let texto = estado ++ "\n"++ jugador ++ "\n" ++ puntuaciones
    writeFile nombre texto

-- Funciones para iniciar o cargar el juego. A estas funciones son a las que acabará llamando el main.
-- ------------------------------------------------------------------------
iniciaJuego :: IO ()
iniciaJuego = do
    putStrLn "¿Quieres empezar un juego nuevo o cargar una partida?"
    putStrLn "Escribe 'nuevo' o 'cargar' por favor"
    respuesta <- getLine
    trataR respuesta

-- Función para tratar con la respuesta del usuario.
trataR :: String -> IO ()
trataR r
    | r == "nuevo" = partidaNueva
    | r == "cargar" = do
        datos <- cargarPartida
        let c = traduceCadena (datos !! 0) 8
        let j = digitToInt $ head $ datos !! 1
        let p1 = digitToInt $ head $ datos !! 2
        let p2 = digitToInt $ head $ datos !! 3
        if (finalizado c)
        then if (p1)==(p2)
            then putStrLn "Empate..."
            else if (p1)>(p2)
                then do
                    representaCuadricula c
                    putStrLn "¡El jugador 1 ha ganado!"
                else do
                    representaCuadricula c
                    putStrLn "¡El jugador 2 ha ganado!"
            else do
                juegoMedio c j (p1,p2)
    | otherwise = do
        putStrLn "Entrada no válida. Inténtelo de nuevo"
        respuesta <- getLine
        trataR respuesta
-- ------------------------------------------------------------------------

-- Funciones para iniciar o cargar el juego con la máquina.
-- ------------------------------------------------------------------------
iniciaJuego2 :: IO ()
iniciaJuego2 = do
    putStrLn "¿Quieres empezar un juego nuevo o cargar una partida?"
    putStrLn "Escribe 'nuevo' o 'cargar' por favor"
    respuesta <- getLine
    trataR2 respuesta

-- Función para tratar con la respuesta del usuario.
trataR2 :: String -> IO ()
trataR2 r
    | r == "nuevo" = partidaNueva2
    | r == "cargar" = do
        datos <- cargarPartida
        let c = traduceCadena (datos !! 0) 8
        let j = digitToInt $ head $ datos !! 1
        let p1 = digitToInt $ head $ datos !! 2
        let p2 = digitToInt $ head $ datos !! 3
        if (finalizado c)
        then if (p1)==(p2)
            then putStrLn "Empate..."
            else if (p1)>(p2)
                then do
                    representaCuadricula c
                    putStrLn "¡El jugador ha ganado!"
                else do
                    representaCuadricula c
                    putStrLn "La máquina ha ganado..."
            else do
                dif <- leeDigito "Escoja la dificultad con la que quiere reanudar la partida por favor. Simple(1) o Complejo(2): "
                juegoMedio2 c j (p1,p2) dif
    | otherwise = do
        putStrLn "Entrada no válida. Inténtelo de nuevo"
        respuesta <- getLine
        trataR2 respuesta

-- Función para generar una partida nueva para 1 jugador.
partidaNueva2 :: IO ()
partidaNueva2 = do
    dif <- leeDigito "Escoja dificultad por favor. Simple(1) o Complejo(2): "
    juegoMedio2 (inicial 8) 1 (0,0) dif
-- ------------------------------------------------------------------------

-- Función para dar pie a la dificultad ya elegida para la máquina.
trataDificultad :: (Cuadricula, (Int, Int)) -> Int -> (Cuadricula,(Int,Int),Char)
trataDificultad c dif | dif == 1 = ponAleatorio c
					  | otherwise = ponNoAleatorio c

-- Función para el nivel más simple de la máquina.
ponAleatorio :: (Cuadricula, (Int, Int)) -> (Cuadricula,(Int,Int),Char)
ponAleatorio (c,_) = ((actualizaValor v c pos),pos,v)
    where validos = [x | x<-listaAleatoria, valido x c]
          pos = head validos
          numA = last $ generaAleatorios $ snd $ head validos
          v = if (mod numA 1)==0 then 'O' else 'S'
		  
-- Funciones para que la máquina juegue con aprendizaje automático

data Arbol a = N a [Arbol a]


-- Valor por defecto para la poda
profBus :: Int
profBus = 14

-- Función para determinar las casillas libres
libres :: Cuadricula -> [(Int, Int)]
libres c = [p | p <- lista, valido p c]
	where lista = [(a,b) | a <- [0..7], b <- [0..7]]
	
-- Función de creacion de arbol
creaArbol :: (Cuadricula,(Int,Int)) -> Arbol (Cuadricula,(Int,Int),(Int,Int),Char)
creaArbol (c,pts) = creaArbol' (c,pts,(0,0),'O') 0
creaArbol' (c,pts,indexs,letra) ac
            | finalizado c = (N (c,pts,indexs,letra) [])
            | otherwise = (N (c,pts,indexs,letra) as)
             where s = movimientos c ac pts
                   as = [creaArbol' a (ac+1) | a<-s]
				
-- Función de jugadas futuras segun su disponibilidad
movimientos :: Cuadricula -> Int -> (Int,Int) -> [(Cuadricula,(Int,Int),(Int,Int),Char)]
movimientos c pr pts
              | finalizado c = []
			  | even pr = map (auxiliarM c pts) (libres c)
              | otherwise = map (auxiliarH c pts) (libres c)
			  
auxiliarM :: Cuadricula -> (Int,Int) -> (Int,Int) -> (Cuadricula,(Int,Int),(Int,Int),Char)
auxiliarM c pts p
    | cPO >= cPS = ((actualizaValor 'O' c p), ((fst pts),(snd pts) + cPO),p,'O')
	| otherwise = ((actualizaValor 'S' c p), ((fst pts),(snd pts) + cPS),p,'S')
        where cPO = calculaPuntuacion c p 'O'
              cPS = calculaPuntuacion c p 'S'
			  
auxiliarH :: Cuadricula -> (Int,Int) -> (Int,Int) -> (Cuadricula,(Int,Int),(Int,Int),Char)
auxiliarH c pts p
    | cPO >= cPS = ((actualizaValor 'O' c p), ((fst pts) + cPO,(snd pts)),p,'O')
	| otherwise = ((actualizaValor 'S' c p), ((fst pts) + cPS,(snd pts)),p,'S')
        where cPO = calculaPuntuacion c p 'O'
              cPS = calculaPuntuacion c p 'S'

-- Funciones propias del algoritmo minimax con poda
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- Función para el nivel avanzado de la máquina
ponNoAleatorio :: (Cuadricula, (Int, Int)) -> (Cuadricula,(Int,Int),Char)
ponNoAleatorio c = mejorMov c

-- Función para determinar el mejor movimiento posible para ganar.
mejorMov :: (Cuadricula, (Int, Int)) -> (Cuadricula,(Int,Int),Char)
mejorMov = seleccion . maximiza . poda profBus . creaArbol

-- Definimos tipo Puntuación para valorar las cuadrículas
type Puntuacion = Int

-- Función para puntuar las cuadrículas dentro del arbol de decisiones
puntuaciones :: [Arbol (Puntuacion, Cuadricula,(Int,Int),Char)] -> [Puntuacion]
puntuaciones arboles = [p | N (p,_,_,_) _ <- arboles]

-- Función que selecciona la cuadrícula que tenga asignada la puntuación mas favorable
seleccion :: Arbol (Puntuacion, Cuadricula,(Int,Int),Char) -> (Cuadricula,(Int,Int),Char)
seleccion (N (p,_,_,_) cs) = head [(c,pos,letra) | N (puntos, c, pos, letra) _ <- cs, puntos == p]

-- Función que actualiza el valor de una cuadrícula a mejor si resulta en un movimiento favorable
maximiza :: Arbol (Cuadricula,(Int,Int),(Int,Int),Char) -> Arbol (Puntuacion, Cuadricula,(Int,Int),Char)
maximiza (N (c,puntAct,pos,letra) []) | finalizado c || (fst puntAct) > (snd puntAct) = N (-1,c,pos,letra) [] 
						  | otherwise = N (0,c,pos,letra) []
maximiza (N (c,puntAct,pos,letra) cs) = N (maximum (puntuaciones ps),c,pos,letra) ps
	where ps = map minimiza cs

-- Función que actualiza el valor de una cuadrícula a peor si resulta en un movimiento no favorable
minimiza :: Arbol (Cuadricula,(Int,Int),(Int,Int),Char) -> Arbol (Puntuacion, Cuadricula,(Int,Int),Char)
minimiza (N (c,puntAct,pos,letra) []) | finalizado c || (fst puntAct) > (snd puntAct) = N (1,c,pos,letra) [] 
						  | otherwise = N (0,c,pos,letra) []
minimiza (N (c,puntAct,pos,letra) cs) = N (minimum (puntuaciones ps),c,pos,letra) ps
	where ps = map maximiza cs

-- Función que elimina caminos posibles en el arbol de decisiones según la profundidad de busqueda
poda :: Int -> Arbol a -> Arbol a
poda prof (N a as) | prof == 0 = N a [] | otherwise = N a (map (poda (prof-1)) as)