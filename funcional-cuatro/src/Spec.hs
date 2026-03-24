module Spec where
import PdePreludat
import Library
import Test.Hspec

correrTests :: IO ()
correrTests = hspec $ do
  describe "Test de ejemplo" $ do
    it "El pdepreludat se instaló correctamente" $ do
      doble 1 `shouldBe` 2

  describe "Ejercicios Lista |" $ do
    it "sumarListaNumeros/1" $ do
      sumarListaNumeros [1,2,3,4]  `shouldBe` 10
      sumarListaNumeros [22,1,17,60] `shouldBe` 100

    it "promedioFrecuenciaCardiaca/1" $ do
      promedioFrecuenciaCardiaca frecuenciaCardiaca `shouldBe` 115

    it "frecuenciaCardiacaMinuto/1" $ do
      frecuenciaCardiacaMinuto 30 `shouldBe` 127

    it "frecuenciaHastaMomento/1" $ do
      frecuenciaHastaMomento 30 `shouldBe` [80, 100, 120, 127]
    
    it "esCapicua" $ do
      esCapicua ["ne", "uqu", "en"] `shouldBe` True
      esCapicua ["me", "n", "em"] `shouldBe` True
      esCapicua ["tr", "a", "fi", "co"] `shouldBe` False

    it "cuandoHabloMasMinutos" $ do
      cuandoHabloMasMinutos `shouldBe` "horarioReducido"

    it "existsAny/2" $ do
      existsAny even (1,3,5) `shouldBe` False
      existsAny even (1,4,7) `shouldBe` True
      existsAny (0>) (1,-3,7) `shouldBe` True

    it "mejor/3" $ do
      mejor (^2) (*3) 1 `shouldBe` 3
      mejor (^2) (*3) 5 `shouldBe` 25

    it "aplicarPar" $ do
      aplicarPar doble (3,12) `shouldBe` (6,24)
      aplicarPar even (3,12) `shouldBe` (False, True)
      aplicarPar (even . doble) (3,12) `shouldBe` (True, True)

    it "parDeFns/3" $ do
      parDeFns even doble 12 `shouldBe` (True,24)

    it "esMultiploDeAlgunos/2" $ do
      esMultiploDeAlguno 15 [2,3,4] `shouldBe` True
      esMultiploDeAlguno 34 [3,4,5] `shouldBe` False

    it "promedios/1" $ do
      promedios [[8,6],[7,9,5],[6,2,4],[9,6]] `shouldBe` [7,7,4,7.5] 

    it "promediosSinAplazos/1" $ do
      promediosSinAplazos [[8,6],[6,2,6]] `shouldBe` [7,6]

    it "mejoresNotas/1" $ do
      mejoresNotas [[8,6,2,4],[7,9,4,5],[6,2,4,2],[9,6,7,10]] `shouldBe` [8,9,6,10]
    
    it "aprobo/1" $ do
      aprobo [8,6,2,4] `shouldBe` False
      aprobo [7,9,6,8] `shouldBe` True 

    it "divisores/1" $ do
      divisores 60 `shouldBe` [1,2,3,4,5,6,10,12,15,20,30,60]

    it "exists/2" $ do
      exists even [1,3,5] `shouldBe`False
      exists even [1,4,7] `shouldBe` True
    
    it "hayAlgunNegativo/2" $ do
      hayAlgunNegativo [2,-3,9] 214545 `shouldBe` True 

    it "aplicarFunciones/2" $ do
      aplicarFunciones [(*4),(+3),abs] (-8) `shouldBe` [-32,-5,8]

    it "sumaF/2" $ do
      sumaF [(*4),(+3),abs] (-8) `shouldBe` -29 
    
    it "subirHabilidades/2" $ do
      subirHabilidad 2 [3,6,9,10,11,12] `shouldBe` [5,8,11,12,12,12] 
      
    it "flimitada/2" $ do
      flimitada (*2) 9 `shouldBe` 12
      flimitada (+(-4)) 3 `shouldBe` 0
      flimitada (*2) 5 `shouldBe` 10

    it "cambiarHabilidad/2" $ do
      cambiarHabilidad (*2) [2,4,6,8,10] `shouldBe` [4,8,12,12,12]
      -- 13 b
      cambiarHabilidad (max 4) [2,4,5,3,8] `shouldBe` [4,4,5,4,8] 

    -- 14 investigar takeWhile - > toma cuando cumple la condicion 
    -- si no cumple la condicion corta la ejecucuion por eso (> 9) es vacio
    it "takeWhile/2" $ do
      takeWhile (< 3) [1..12] `shouldBe` [1,2]
      takeWhile (< 9) [1..12] `shouldBe` [1..8]
      takeWhile (9 <) [1..12] `shouldBe` []

    it "primerosPares" $ do
      primerosPares [4,12,3,8,2,9,6] `shouldBe` [4,12]

    it "primerosDivisores/2" $ do
      primerosDivisores 60 [4,12,3,8,2,9,6] `shouldBe` [4,12,3]

    it "primerosNoDivisores/2" $ do
      primerosNoDivisores 60 [8,9,4,12,3,8,2,9,6] `shouldBe` [8,9]
    
    it "huboMesMejorDe/3" $ do
      huboMesMejorDe [1..12] [12,11..1] 10 `shouldBe` True 

    it "crecimientoAnual/1" $ do
      crecimientoAnual 12 `shouldBe` 4
    
    it "crecimientoEntreEdades/2" $ do
      crecimientoEntreEdades 8 12 `shouldBe` 22 

    it "alturasEn1Anio/2" $ do
      alturasEn1Anio 7 [120,108,89] `shouldBe` [130,118,99]

    it "alturaEnEdades/3" $ do
      alturaEnEdades 120 8 [12,15,18] `shouldBe` [142,154,164]

    it "rachasLluvias/1" $ do
      rachasLluvia lluviasEnero `shouldBe` [[2,5,1,34,2],[21],[5,9,18,4]]

    it "mayorRachasDeLluvias/1" $ do
      mayorRachasDeLluvia lluviasEnero `shouldBe` 5