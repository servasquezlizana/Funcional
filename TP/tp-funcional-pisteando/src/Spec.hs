module Spec where
import PdePreludat
import Library
import Test.Hspec

correrTests :: IO ()
correrTests = hspec $ do
  describe "2.a - Estado de salud del auto" $ do
    it "Un auto marca Peugeot no está en buen estado" $ do
      estaEnBuenEstado peugeot `shouldBe` False
      
    it "Un auto marca Lamborghini, con tiempo en pista de 99 segundos y desgaste de chasis 7, está en buen estado" $ do
      estaEnBuenEstado (lamborghini { tiempoCarrera = 99 }) `shouldBe` True
      
    it "Un auto marca Fiat, con tiempo en pista de 99 segundos y desgaste de chasis 33, no está en buen estado" $ do
      estaEnBuenEstado (fiat { tiempoCarrera = 99 }) `shouldBe` False
      
    it "Un auto marca Ferrari que tiene 130 segundos de tiempo en pista con desgaste de ruedas 50 y chasis 30 está en buen estado" $ do
      estaEnBuenEstado (ferrari { tiempoCarrera = 130, desgasteRuedas = 50, desgasteChasis = 30 }) `shouldBe` True
      
    it "Un auto marca Ferrari que tiene 15 segundos de tiempo en pista con desgaste de ruedas 50 y chasis 45 no está en buen estado" $ do
      estaEnBuenEstado (ferrari { tiempoCarrera = 15, desgasteRuedas = 50, desgasteChasis = 45 }) `shouldBe` False
      
    it "Un auto marca Ferrari que tiene 150 segundos de tiempo en pista con desgaste de ruedas 70 y chasis 30 no está en buen estado" $ do
      estaEnBuenEstado (ferrari { tiempoCarrera = 150, desgasteRuedas = 70, desgasteChasis = 30 }) `shouldBe` False

  describe "2.b - Saber si un auto no da más" $ do
    it "Un auto de marca Ferrari con desgaste de ruedas 20 y chasis 90 no da más (primer apodo empieza con 'La ')" $ do
      noDaMas (ferrari { desgasteRuedas = 20, desgasteChasis = 90 }) `shouldBe` True
      
    it "Un auto de marca Ferrari con desgaste de ruedas 90 y chasis 20 da para más" $ do
      noDaMas (ferrari { desgasteRuedas = 90, desgasteChasis = 20 }) `shouldBe` False
      
    it "Un auto de marca Lamborghini con desgaste de ruedas 90 y chasis 20 no da más" $ do
      noDaMas (lamborghini { desgasteRuedas = 90, desgasteChasis = 20 }) `shouldBe` True
      
    it "Un auto de marca Lamborghini base da para más" $ do
      noDaMas lamborghini `shouldBe` False

  describe "2.c - Saber si un auto es un chiche" $ do
    it "Un auto de marca Lamborghini es un chiche (apodos pares, chasis 7)" $ do
      esChiche lamborghini `shouldBe` True
      
    it "Un auto de marca Lamborghini con desgaste de ruedas 90 y chasis 20 no es un chiche" $ do
      esChiche (lamborghini { desgasteRuedas = 90, desgasteChasis = 20 }) `shouldBe` False
      
    it "Un auto de marca Ferrari con desgaste de ruedas 20 y chasis 90 no es un chiche (impar y chasis 90)" $ do
      esChiche (ferrari { desgasteRuedas = 20, desgasteChasis = 90 }) `shouldBe` False
      
    it "Un auto de marca Ferrari es un chiche (apodos impares y cero desgaste de chasis)" $ do
      esChiche ferrari `shouldBe` True

  describe "2.d - Saber si un auto es una joya" $ do
    it "Un auto de marca Peugeot es una joya (cero desgaste y un apodo)" $ do
      esJoya peugeot `shouldBe` True
      
    it "Un auto de marca Ferrari no es una joya (no tiene desgaste pero tiene más de un apodo)" $ do
      esJoya ferrari `shouldBe` False

  describe "2.e - Conocer el nivel de chetez" $ do
    it "Un auto de marca Ferrari tiene un valor de 180 (20 * 3 apodos * 3 letras del modelo)" $ do
      nivelDeChetez ferrari `shouldBe` 180

  describe "2.f - Capacidad supercalifragilisticaespialidosa" $ do
    it "Un auto de marca Ferrari tiene un valor de 7 (cantidad de letras contando el espacio en 'La nave')" $ do
      capacidadSupercalifragilisticaespialidosa ferrari `shouldBe` 7

  describe "2.g - Qué tan riesgoso es un auto" $ do
    it "Un auto de marca Lamborghini tiene valor 29.2 (está en buen estado)" $ do
      riesgoso lamborghini `shouldBe` 29.2
      
    it "Un auto de marca Fiat tiene valor 237.6 (no está en buen estado, multiplica por 2)" $ do
      riesgoso fiat `shouldBe` 237.6

  describe "3. Manos a la obra" $ do
    describe "3.a - Reparar un auto" $ do
      it "Reparar un auto de marca Fiat deja 0 en ruedas y 4.95 en chasis" $ do
        let fiatReparado = reparar fiat
        desgasteRuedas fiatReparado `shouldBe` 0 -- [cite: 236]
        desgasteChasis fiatReparado `shouldBe` 4.95 -- [cite: 236]
        
      it "Reparar un auto de marca Ferrari mantiene sus desgastes en 0 y 0" $ do
        let ferrariReparado = reparar ferrari
        desgasteRuedas ferrariReparado `shouldBe` 0 -- [cite: 236]
        desgasteChasis ferrariReparado `shouldBe` 0 -- [cite: 236]

    describe "3.b - Aplicar penalidad" $ do
      it "Aplicar penalidad de 20 segundos a un auto Ferrari con tiempo 10 segundos en pista lo deja en 30" $ do
        tiempoCarrera (penalizar 20 (ferrari { tiempoCarrera = 10 })) `shouldBe` 30 -- [cite: 237]
        
      it "Aplicar penalidad de 0 segundos a un auto Ferrari con tiempo 10 lo mantiene igual" $ do
        tiempoCarrera (penalizar 0 (ferrari { tiempoCarrera = 10 })) `shouldBe` 10 -- [cite: 237]

    describe "3.c - Poner nitro" $ do
      it "Poner nitro a un Fiat le deja una velocidad máxima de 52.8" $ do
        velocidadMaxima (ponerNitro fiat) `shouldBe` 52.8 -- [cite: 242]
        
      it "Poner nitro a un Fiat con velocidad máxima 0 mantiene su velocidad" $ do
        velocidadMaxima (ponerNitro (fiat { velocidadMaxima = 0 })) `shouldBe` 0 -- [cite: 242]

    describe "3.d - Bautizar un auto" $ do
      it "Bautizar 'El diablo' a un Lamborghini hace que contenga ese apodo" $ do
        apodos (bautizar "El diablo" lamborghini) `shouldContain` ["El diablo"] -- [cite: 246]
        
      it "Bautizar 'El diablo' a un Lamborghini sin apodos hace que sólo tenga ese apodo" $ do
        apodos (bautizar "El diablo" (lamborghini { apodos = [] })) `shouldBe` ["El diablo"] -- [cite: 246]

    describe "3.e - Desarmadero" $ do
      it "Llevar un Fiat al desarmadero para cambiar por Tesla X cambia la marca a Tesla" $ do
        marca (desarmadero "Tesla" "X" fiat) `shouldBe` "Tesla" -- [cite: 247]
        
      it "Llevar un Fiat al desarmadero para cambiar por Tesla X cambia el modelo a X" $ do
        modelo (desarmadero "Tesla" "X" fiat) `shouldBe` "X" -- [cite: 247]
        
      it "Llevar un Fiat al desarmadero para cambiar por Tesla X lo deja sólo con el apodo 'Nunca Taxi'" $ do
        apodos (desarmadero "Tesla" "X" fiat) `shouldBe` ["Nunca Taxi"] -- [cite: 247, 249]

  describe "4. ¡Pistas!" $ do
    describe "4.a - Tramo Curva" $ do
      it "Transitar curva peligrosa con Ferrari deja ruedas en 15" $ do
        desgasteRuedas (curvaPeligrosa ferrari) `shouldBe` 15 -- [cite: 260]
      it "Transitar curva peligrosa con Ferrari mantiene chasis en 0" $ do
        desgasteChasis (curvaPeligrosa ferrari) `shouldBe` 0 -- [cite: 260]
      it "Transitar curva peligrosa con Ferrari deja tiempo en 23.5" $ do
        redondearA2Decimales (tiempoCarrera (curvaPeligrosa ferrari)) `shouldBe` 9.23 -- [cite: 260]
        
      it "Transitar curva tranca con Ferrari deja ruedas en 15" $ do
        desgasteRuedas (curvaTranca ferrari) `shouldBe` 15 -- [cite: 260]
      it "Transitar curva tranca con Ferrari mantiene chasis en 0" $ do
        desgasteChasis (curvaTranca ferrari) `shouldBe` 0 -- [cite: 260]
      it "Transitar curva tranca con Ferrari deja tiempo en 48.5" $ do
        redondearA2Decimales (tiempoCarrera (curvaTranca ferrari)) `shouldBe` 16.92 -- [cite: 260]

    describe "4.b - Tramo Recto" $ do
      it "Transitar tramo recto classic con Ferrari deja chasis en 7.15 y tiempo en 11" $ do
        let ferrariPost = tramoRectoClassic ferrari
        desgasteChasis ferrariPost `shouldBe` 7.15 -- [cite: 267]
        tiempoCarrera ferrariPost `shouldBe` 11 -- [cite: 267]
        
      it "Transitar tramito con Ferrari deja chasis en 2.6 y tiempo en 4" $ do
        let ferrariPost =  tramito ferrari
        desgasteChasis ferrariPost `shouldBe` 2.6 -- [cite: 267]
        tiempoCarrera ferrariPost `shouldBe` 4 -- [cite: 267]

    describe "4.c - Tramo ZigZag" $ do
      it "Transitar zigZagLoco con Ferrari deja chasis en 5, ruedas en 32.5 y tiempo en 15" $ do
        let ferrariPost =  zigzagLoco ferrari
        desgasteChasis ferrariPost `shouldBe` 5 -- [cite: 276]
        desgasteRuedas ferrariPost `shouldBe` 32.5 -- [cite: 276]
        tiempoCarrera ferrariPost `shouldBe` 15 -- [cite: 276]
        
      it "Transitar casiCurva con Ferrari deja chasis en 2.6, ruedas en 5.0 y tiempo en 3" $ do
        let ferrariPost =  casiCurva ferrari
        desgasteChasis ferrariPost `shouldBe` 5.0 -- [cite: 276]
        desgasteRuedas ferrariPost `shouldBe` 6.5 -- [cite: 276]
        tiempoCarrera ferrariPost `shouldBe` 3 -- [cite: 276]

    describe "4.d - Tramo Rulo" $ do
      it "Transitar ruloClasico con Ferrari deja chasis en 0, ruedas en 19.5 y tiempo en 1" $ do
        let ferrariPost =  ruloClasico ferrari
        desgasteChasis ferrariPost `shouldBe` 0 -- [cite: 282]
        desgasteRuedas ferrariPost `shouldBe` 19.5 -- [cite: 282]
        tiempoCarrera ferrariPost `shouldBe` 1 -- [cite: 284]
        
      it "Transitar deseoDeMuerte con Ferrari deja chasis en 0, ruedas en 39.0 y tiempo en 2" $ do
        let ferrariPost =  deseoDeMuerte ferrari
        desgasteChasis ferrariPost `shouldBe` 0 -- [cite: 284]
        desgasteRuedas ferrariPost `shouldBe` 39.0 -- [cite: 284]
        tiempoCarrera ferrariPost `shouldBe` 2 -- [cite: 284]

  describe "5. Opción de Recursión" $ do
    describe "5.a - Nivel de Joyez" $ do
      it "Ferrari con tiempo 49 y Peugeot con tiempo 50 dan un nivel de joyez total de 3" $ do
        let autos = [ferrari { tiempoCarrera = 49 }, peugeot { tiempoCarrera = 50 }]
        nivelDeJoyez autos `shouldBe` 3 -- [cite: 292]

    describe "5.b - Grupo para Entendidos" $ do
      it "Ferrari con tiempo 201 y Ferrari con tiempo 200 no es para entendidos (supera el tiempo)" $ do
        let autos = [ferrari { tiempoCarrera = 201 }, ferrari { tiempoCarrera = 200 }]
        paraEntendidos autos `shouldBe` False -- [cite: 298]
        
      it "Ferrari con tiempo 200 y un Peugeot no es para entendidos (Peugeot no está en buen estado)" $ do
        let autos = [ferrari { tiempoCarrera = 200 }, peugeot]
        paraEntendidos autos `shouldBe` False -- [cite: 298]
        
      it "Ferrari con tiempo 200 y Lamborghini con tiempo 200 es para entendidos" $ do
        let autos = [ferrari { tiempoCarrera = 200 }, lamborghini { tiempoCarrera = 200 }]
        paraEntendidos autos `shouldBe` True -- [cite: 298]

-- a. Auto Ferrari [cite: 130, 131]
ferrari :: Auto
ferrari = Auto {
    marca           = "Ferrari",
    modelo          = "F50",
    desgasteChasis  = 0,
    desgasteRuedas  = 0,
    velocidadMaxima = 65,
    tiempoCarrera   = 0,
    apodos          = ["La nave", "El fierro", "Ferrucho"]
}

-- b. Auto Lamborghini [cite: 132]
lamborghini :: Auto
lamborghini = Auto {
    marca           = "Lamborghini",
    modelo          = "Diablo",
    desgasteChasis  = 7,
    desgasteRuedas  = 4,
    velocidadMaxima = 73,
    tiempoCarrera   = 0,
    apodos          = ["Lambo", "La bestia"]
}

-- c. Auto Fiat [cite: 133, 134]
fiat :: Auto
fiat = Auto {
    marca           = "Fiat",
    modelo          = "600",
    desgasteChasis  = 33,
    desgasteRuedas  = 27,
    velocidadMaxima = 44,
    tiempoCarrera   = 0,
    apodos          = ["La Bocha", "La bolita", "Fitito"]
}

-- d. Auto Peugeot [cite: 137, 138]
peugeot :: Auto
peugeot = Auto {
    marca           = "Peugeot",
    modelo          = "504",
    desgasteChasis  = 0,
    desgasteRuedas  = 0,
    velocidadMaxima = 40,
    tiempoCarrera   = 0,
    apodos          = ["El rey del desierto"]
}