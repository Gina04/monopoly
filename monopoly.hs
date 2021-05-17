import Text.Show.Functions()
import Data.List()
data Persona = Persona {
    nombre :: String ,
    dinero :: Int,
    tactica:: String,
    propiedades :: [Propiedad],
    acciones ::  Acciones
}deriving Show

data Propiedad = Propiedad{
    nombrePropiedad:: String,
    precio:: Int
}deriving Show

type Acciones = [Accion]
type Accion = Persona -> Persona

----Accesssors-------------------------------------------------------------------------
---------------------------------------------------------------------------------------
mapNombre:: (String -> String)->Persona ->Persona
mapNombre f unJugador = unJugador{nombre = f.nombre$unJugador}

mapDinero:: (Int -> Int)-> Persona ->Persona 
mapDinero f unJugador= unJugador{dinero = f.dinero$ unJugador}

cambiarTactica::String-> Persona -> Persona 
cambiarTactica otroValor unJugador = unJugador{tactica = otroValor}


mapPropiedades:: ([Propiedad]->[Propiedad])->Persona ->Persona
mapPropiedades f unJugador = unJugador{propiedades = f. propiedades $ unJugador}

mapAcciones:: (Acciones -> Acciones)->Persona -> Persona 
mapAcciones f unJugador = unJugador{acciones = f.acciones $ unJugador}

-----------------------FIN ACCESSORS------------------------------------------------
------------------------------------------------------------------------------------
{-
Cada participante comienza con $500, ninguna propiedad a su nombre y con la acción 
pasarPorElBanco, ya que es lo primero que hacen. Además, cada participante 
tiene su propio estilo: 
La táctica infalible de Carolina es ser una “Accionista” y, por esa razón, también tiene la acción pagarAAccionistas. 
Manuel es un “Oferente singular” y su acción inicial, además de la del banco, es enojarse.

-}

manuel::Persona 
manuel= Persona{
    nombre = "Juan",
    dinero = 500, 
    tactica = "Oferente singular", 
    propiedades=[],
    acciones= [pasarPorElBanco , enojarse]

}

carolina::Persona 
carolina = Persona{
    nombre = "Carolina", 
    dinero = 500, 
    tactica = "Accionista", 
    propiedades = [], 
    acciones = []

}

--Acciones---------------------------------------------------------------------------
-------------------------------------------------------------------------------------
{-
pasarPorElBanco: aumenta el dinero del jugador en $40 y cambia su táctica 
a “Comprador compulsivo”.
-}

pasarPorElBanco:: Accion
pasarPorElBanco unJugador = cambiarTactica "Comprador compulsivo". mapDinero (+40) $ unJugador
    
   

--aumentarDinero:: Int-> Int -> Int
--aumentarDinero unValor otroValor= (+unValor) otroValor



{-
enojarse: suma $50 y agrega gritar a sus acciones.
-}

enojarse:: Accion
enojarse unJugador = mapDinero (+50).mapAcciones (gritar : )$unJugador



--agregarAcciones:: Accion ->Acciones-> Acciones
--agregarAcciones unaAccion unasAcciones = unaAccion : unasAcciones   

{-
gritar: agrega “AHHHH” al principio de su nombre.

-}
gritar::Accion
gritar unJugador = mapNombre (++ "AHH") unJugador

--agregarPalabra:: String ->String->String
--agregarPalabra unaPalabra otraPalabra = (++unaPalabra) otraPalabra     

{-
subastar: al momento de una subasta solo quienes tengan como tácticas “Oferente singular”
o “Accionista” podrán ganar la propiedad. Ganar implica restar el precio de la propiedad 
de su dinero y sumar la nueva adquisición a sus propiedades.-} 


subastar:: Propiedad ->Accion
subastar unaPropiedad unJugador
   |tieneComoTactica unJugador =
     mapDinero (restarDinero (precio unaPropiedad)).mapPropiedades(unaPropiedad :) $ unJugador
   |otherwise = unJugador



--agregarPropiedad:: Propiedad ->[Propiedad]->[Propiedad]
--agregarPropiedad unaPropiedad propiedades = unaPropiedad : propiedades     

restarDinero:: Int -> Int -> Int
restarDinero unValor otroValor = unValor - otroValor

tieneComoTactica:: Persona -> Bool 
tieneComoTactica unJugador = tactica unJugador == "Oferente singular" ||  tactica unJugador == "Accionista"



{-
cobrarAlquileres: suma $10 por cada propiedad barata y $20 por cada propiedad cara obtenida.
Las propiedades baratas son aquellas cuyo precio es menor a $150.
-}

cobrarAlquileres:: [Propiedad]-> Accion
cobrarAlquileres  propiedades unJugador = mapDinero  (+sumarAlquileres propiedades) unJugador

sumarAlquileres::[Propiedad] -> Int
sumarAlquileres  propiedades= sum.map precioAlquileres$ propiedades 

precioAlquileres:: Propiedad-> Int
precioAlquileres unaPropiedad
    | esPropiedadCara unaPropiedad = 20  
    | otherwise = 10
 


esPropiedadCara:: Propiedad -> Bool 
esPropiedadCara unaPropiedad = (>150).precio $ unaPropiedad


{-
pagarAAccionistas: resta $100 para todos los casos excepto que la táctica sea “Accionista”, 
en ese caso suma $200.-}

pagarAccionistas:: Accion
pagarAccionistas unJugador 
    | esTacticaAccionista unJugador = mapDinero(+200) unJugador
    | otherwise = mapDinero(flip restarDinero 100) unJugador

esTacticaAccionista:: Persona -> Bool
esTacticaAccionista (Persona _ _"Accionista"_ _) = True
esTacticaAccionista _ = False    


{-
hacerBerrinchePor: cuando una persona hace un berrinche por una propiedad se le suman $10 y se la hace gritar,
la persona sigue haciendo berrinche hasta que llegue a comprar la propiedad que quiere.

-}

hacerBerrinchePor:: Propiedad->Accion
hacerBerrinchePor unaPropiedad unJugador 
    | puedeComprarPropiedad unJugador unaPropiedad = comprarPropiedad unaPropiedad unJugador       
    | otherwise = hacerBerrinchePor unaPropiedad (mapDinero(+10).gritar $ unJugador)


puedeComprarPropiedad:: Persona -> Propiedad -> Bool 
puedeComprarPropiedad unJugador unaPropiedad = precio unaPropiedad <= dinero unJugador 



comprarPropiedad:: Propiedad -> Persona ->Persona
comprarPropiedad unaPropiedad unJugador = mapPropiedades(unaPropiedad :) unJugador


{-
Finalmente llega la tan ansiada ronda final: Carolina vs Manuel.
Quien gane el juego será aquella persona que, luego de realizar todas sus acciones,
tenga más dinero. 
Para ello, modelar la función últimaRonda, que dado un participante retorna una acción equivalente
a todas sus acciones. 

-}

ultimaRonda:: Persona->Accion
ultimaRonda unJugador= foldl (.) id (acciones unJugador)

 
juegoFinal:: Persona -> Persona -> Persona
juegoFinal unJugador otroJugador 
     |dinero unJugador > dinero otroJugador = unJugador
     |otherwise = otroJugador

