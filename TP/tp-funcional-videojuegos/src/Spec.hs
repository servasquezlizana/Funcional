module Spec where
import PdePreludat
import Library
import Test.Hspec

correrTests :: IO ()
correrTests = hspec $ do
  describe "Casos de prueba para la función impactoVJ" $ do
      it "Impacto del juego Zelda (antes 1990, > 3 expansiones)" $ do
        impactoVJ zelda `shouldBe` 12000
        
      it "Impacto del juego Pacman (antes 1990, <= 3 expansiones)" $ do
        impactoVJ pacman `shouldBe` 2200
        
      it "Impacto del juego Hollow Knight (sin expansiones)" $ do
        impactoVJ hollowKnight `shouldBe` 250
        
      it "Impacto del juego Cyberpunk (post 1990, con expansiones)" $ do
        impactoVJ cyberpunk `shouldBe` 4050

  describe "Casos de prueba para expansionDeCulto" $ do
      it "Alan Wake tiene una expansión de culto (contiene 'Director')" $ do
        expansionDeCulto alanWake `shouldBe` True
        
      it "Death Stranding NO tiene (es case-sensitive o la palabra es distinta)" $ do
        expansionDeCulto deathStranding `shouldBe` False
        
      it "Control NO tiene expansión de culto" $ do
        expansionDeCulto control `shouldBe` False
        
      it "Hollow Knight NO tiene expansión de culto" $ do
        expansionDeCulto hollowKnight `shouldBe` False
  
  describe "Casos de prueba para lanzarExpansion" $ do
    it "Lanzar 'Revenge' sobre Hades aumenta el precio a 1100 y agrega la expansión" $ do
      let juegoNuevo = lanzarExpansion "Revenge" hades
      precioBase juegoNuevo `shouldBe` 1100
      (length . listaExpanciones) juegoNuevo `shouldBe` 1
      listaExpanciones juegoNuevo `shouldBe` ["Revenge"]
      
    it "Lanzar 'Blood and Wine' sobre Witcher 3 aumenta el precio a 2130 y suma la expansión" $ do
      let juegoNuevo = lanzarExpansion "Blood and Wine" witcher3
      precioBase juegoNuevo `shouldBe` 2130
      (length . listaExpanciones) juegoNuevo `shouldBe` 2
      listaExpanciones juegoNuevo `shouldBe` ["Hearts of Stone", "Blood and Wine"]

  describe "Casos de prueba para la función parchear" $ do
    
    it "Parchear 8 sobre Skyrim: sobreviven las 2 y el precio sube a 1200" $ do
      let juegoParcheado = parchear 8 skyrim
      precioBase juegoParcheado `shouldBe` 1200
      listaExpanciones juegoParcheado `shouldBe` ["Dragonborn", "Dawnguard"]
      
    it "Parchear 10 sobre Skyrim: sobrevive solo Dragonborn y el precio sube a 1100" $ do
      let juegoParcheado = parchear 10 skyrim
      precioBase juegoParcheado `shouldBe` 1100
      listaExpanciones juegoParcheado `shouldBe` ["Dragonborn"]
      
    it "Parchear 15 sobre Skyrim: no sobrevive ninguna y el precio queda en 1000" $ do
      let juegoParcheado = parchear 15 skyrim
      precioBase juegoParcheado `shouldBe` 1000
      (null . listaExpanciones) juegoParcheado `shouldBe` True
      
    it "Parchear 5 sobre ageOfEmpires: no hay cambios ya que la lista estaba vacía" $ do
      let juegoParcheado = parchear 5 ageOfEmpires
      precioBase juegoParcheado `shouldBe` 500
      listaExpanciones juegoParcheado `shouldBe` []

















-- VideoJuegos cargados
zelda :: VideoJuego
zelda = VideoJuego {
    titulo = "The Legend of Zelda",
    anioLanzamiento = 1986,
    listaExpanciones = ["Breath", "Tears", "Echoes", "Awakening"],
    precioBase = 3000
}

pacman :: VideoJuego
pacman = VideoJuego {
    titulo = "Pac-Man",
    anioLanzamiento = 1980,
    listaExpanciones = ["Championship"],
    precioBase = 200
}

hollowKnight :: VideoJuego
hollowKnight = VideoJuego {
    titulo = "Hollow Knight",
    anioLanzamiento = 2017,
    listaExpanciones = [],
    precioBase = 500
}

cyberpunk :: VideoJuego
cyberpunk = VideoJuego {
    titulo = "Cyberpunk 2077",
    anioLanzamiento = 2020,
    listaExpanciones = ["Phantom Liberty"],
    precioBase = 4000
}

hades :: VideoJuego
hades = VideoJuego {
    titulo = "Hades",
    anioLanzamiento = 2020,
    listaExpanciones = [],
    precioBase = 1000
}

witcher3 :: VideoJuego
witcher3 = VideoJuego {
    titulo = "The Witcher 3",
    anioLanzamiento = 2015,
    listaExpanciones = ["Hearts of Stone"],
    precioBase = 2000
}

skyrim :: VideoJuego
skyrim = VideoJuego {
    titulo = "Skyrim",
    anioLanzamiento = 2011,
    listaExpanciones = ["Dragonborn", "Dawnguard"],
    precioBase = 1000
}

ageOfEmpires :: VideoJuego
ageOfEmpires = VideoJuego {
    titulo = "Age of Empires II",
    anioLanzamiento = 1999,
    listaExpanciones = [],
    precioBase = 500
}

alanWake :: VideoJuego
alanWake = VideoJuego {
    titulo = "Alan Wake",
    anioLanzamiento = 2010,
    listaExpanciones = ["The Writer", "The Director Edition"],
    precioBase = 800
}

deathStranding :: VideoJuego
deathStranding = VideoJuego {
    titulo = "Death Stranding",
    anioLanzamiento = 2019,
    listaExpanciones = ["director cut", "Directors"],
    precioBase = 3000
}

control :: VideoJuego
control = VideoJuego {
    titulo = "Control",
    anioLanzamiento = 2019,
    listaExpanciones = ["Foundation", "AWE"],
    precioBase = 2000
}

rimworld :: VideoJuego
rimworld = VideoJuego {
    titulo = "Rimworld",
    anioLanzamiento = 2018,
    listaExpanciones = ["Ideology", "Biotech", "Royalty"],
    precioBase = 1500
}

stellaris :: VideoJuego
stellaris = VideoJuego {
    titulo = "Stellaris",
    anioLanzamiento = 2016,
    listaExpanciones = ["Utopia", "Megacorp", "Federations"],
    precioBase = 2000
}