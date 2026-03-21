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