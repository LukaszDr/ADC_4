'program testowy komunikacji na magistarli RS485
'znaki nadane w terminalu na jednym komputerze s¹ wysy³ane gdy adres odbiorcy 0
'wpisuj¹c znaki najpierw wpisaæ dwa znaki adresu, potem tekst do wys³ania i enter

'zadania:
'1. w procedurze odbioru wprowadziæ weryfikacjê adresu zawartego w BOF,
'2. wprowadziæ EOF,
'3. wprowadziæ automatyczne odpowiedzi ramk¹ potwierdzenia odbioru,
'4. zastosowaæ przerwanie UDRE1,
'5. wprowadziæ dwudzielnoœæ bufora tabin(), jedna po³owa s³uzy do nadawania,
'a druga do odbioru znaków lub wprowadziæ bufor pierœcieniowy.

'by Marcin Kowalczyk

'obliczenia parametrów konfiguracyjnych
Const Prescfc = 0                                           'potêga dzielnika czêstotliwoœci taktowania procesora
Const Fcrystal =(14745600 /(2 ^ Prescfc))                   'czêstotliwoœæ po przeskalowaniu
'sta³e konfiguracujne USARTów
Const Baundrs0 = 115200                                     'prêdkoœæ transmisji po RS [bps] USART0
Const _ubrr0 =(((fcrystal / Baundrs0) / 16) - 1)            'potrzebne w nastêpnych zadaniach
Const Baundrs1 = Baundrs0                                   'prêdkoœæ transmisji po RS [bps] USART1
Const _ubrr1 =(((fcrystal / Baundrs1) / 16) - 1)            'potrzebne w nastêpnych zadaniach

'konfigurowanie mikrokontrolera
$regfile = "m644pdef.dat"                                   'plik konfiguracyjny z literk¹ "p" w nazwie
$crystal = Fcrystal
'$baud = Baundrs0    'zbêdne gdy inicjalizacja w zdefiniowanej procedurze


'aliasy rejestrów procesora
Temp Alias R16
Temph Alias R17
Rstemp Alias R18
Rsdata Alias R19
'LICZNIK  ile bylo konwersji
Count Alias R25
Flag Alias R26
'pozosta³e aliasy
Te_pin Alias 4
Te Alias Portd.te_pin                                       'sterowanie przep³ywem w nadajniku/odbiorniku linii

'Config ADC
Config Adc = Single , Prescaler = Auto , Reference = Avcc

'USTAWIENIE TIMERA
Config Timer1 = Timer , Prescale = 64 , Compare A = Disconnect , Clear Timer = 1
Stop Timer1
Timer1 = 0                                                  'wARTOSC POCZ, RACZEJ NIEPOTRZEBNE
Ocr1a = 14400                                               'WARTOSC DO ZLICZENIA

On Oc1a Oblicz_adc Nosave                                   'wlaczenie przerwania timera
Enable Oc1a
                                                             'start timera
Start Timer1

On Urxc1 Usart_rx Nosave                                    'deklaracja przerwania URXC (odbiór znaku USART)
On Utxc1 Usart_tx_end Nosave                                'deklaracja przerwania UTXC, koniec nadawania

'UStawienia obrabiania danych
Dim Prescadc As Word
Dim Offset As Word

Prescadc = 5
Offset = 0


'WL i WH
Dim Wl As Long
Dim Wh As Long
Dim Wartdziel As Word
Dim Tempbyte As Byte

Wl = 200
Wh = 800
Wartdziel = Wh - Wl
Wartdziel = Wartdziel / 100
Wl = Wl / Wartdziel
Wh = Wh / Wartdziel


'SUMA - do niej zliczam kolejne odczyty
Dim Suma As Long
Suma = 0

'Adrw = 1    niepotrzebne
                                             'adres odbiorcy 0...15

'ZMIENNE TYSIACE ITD
Dim S As Byte
Dim D As Byte
Dim J As Byte
Dim Asuma As Word

'CZY PRZYSZLO BOF
Dim Czekamnaeof As Byte
Czekamnaeof = 0


''RAMKI

Const Bof_bit = &B11000000
Const Bofm_bit = &B10000001
Const Bofmaster_bit = &B11000010
Const Bofs_bit = &B11000001

Const Eofs_bit = &B10000001
Const Eofm_bit = &B10100010

'ZNAKI ASCII

Const Znaku = &B01010101
Const Znaka = &B01000001
Const Znakrowne = &B00111101
Const Znakm = &B01101101



rcall usart_init                                            'inicjalizacja USARTów i w³¹czenie przerwañ
Sei                                                         'w³¹czenie globalnie przerwañ


Do

   cpi flag,1
   sbic sreg,1
      rcall obliczenia
Loop

!obliczenia:
'   Print "Przed wszystkim:" ; Suma
'   Suma = Suma / 16                                         'usrednienie
'   Print "Na poczatku: " ; Suma
   Suma = Suma / Wartdziel                                  '//przeskalowanie
   Print "po wartdziel: " ; Suma


'   Tempbyte = Suma
   LDS rstemp, {Suma}
'   Tempbyte = Wh
   LDS rsdata ,{Wh}
   !SUB rstemp, rsdata
   'BRLO wieksze
   SBIS SREG, 0
       RJMP wieksze



   Tempbyte = Suma
   LDS rstemp, {Suma}
   Tempbyte = Wl
   LDS rsdata ,{Wl}
   !SUB rstemp, rsdata
'   BRLO mniejsze
   SBIC SREG, 2
      RJMP mniejsze




   Suma = Suma - Wl
   S = Suma / 100
   Asuma = S * 100
   Suma = Suma - Asuma
   D = Suma / 10
   Asuma = D * 10
   Suma = Suma - Asuma
   J = Suma
   Kont:
   LDI flag, 0
   Suma = 0
   Print "po obliczeniach: " ; S ; D ; J
   Writeeeprom J , 30
   Writeeeprom D , 31
   Writeeeprom S , 32
   Ret

!mniejsze:
 '  Print "mniejsze"
    Suma = 0
    S = 0
    D = 0
    J = 0
    RJMP Kont

!wieksze:
   Print "wieksze"
   S = 1
   D = 0
   J = 0
   Suma = 0
   RJMP Kont


Oblicz_adc:
   push rstemp                                              'o ile potrzeba - sprawdziæ
   in rstemp,sreg                                           'o ile potrzeba  - sprawdziæ
   push rstemp                                              'o ile potrzeba - sprawdziæ
   push rsdata                                              'o ile potrzeba  - sprawdziæ
   push yl                                                  'o ile potrzeba  - sprawdziæ
   push yh                                                  'o ile potrzeba  - sprawdziæ
   push r1                                                  'o ile potrzeba  - sprawdziæ
   push r0                                                  'o ile potrzeba  - sprawdziæ
   !cli


   INC Count

   SBI adcsra,6
   !czekaj_adc:
   SBiC ADCSRA, 4
      RJMP czekaj_adc
   Asuma = Adc / 16
   Suma = Suma + Asuma
   cpi Count,16
   sbic sreg,1
      rcall jest


   sei
   pop r0
   pop r1
   pop yh
   pop yl
   pop rsdata
   pop rstemp
   !out sreg,rstemp
   pop rstemp
Return

!jest:
   ldi flag,1
   ldi count, 0
   ret

Usart_rx:                                                   'etykieta bascomowa koniecznie bez !
   push rstemp                                              'o ile potrzeba - sprawdziæ
   in rstemp,sreg                                           'o ile potrzeba  - sprawdziæ
   push rstemp                                              'o ile potrzeba - sprawdziæ
   push rsdata                                              'o ile potrzeba  - sprawdziæ
   push yl                                                  'o ile potrzeba  - sprawdziæ
   push yh                                                  'o ile potrzeba  - sprawdziæ
   push r1                                                  'o ile potrzeba  - sprawdziæ
   push r0                                                  'o ile potrzeba  - sprawdziæ
   push count
   push flag

   !cli
   rcall rs_rx                                              'kod mo¿e byæ bezpoœrenio w usart_rx
   sei

   'odtworzenie stanu jak przed przerwanie
   pop flag
   pop count
   pop r0
   pop r1
   pop yh
   pop yl
   pop rsdata
   pop rstemp
   !out sreg,rstemp
   pop rstemp
Return

!czekajUDR1:
      sbiS ucsr1a,udre1                                     'czekaj na udr1
      rjmp czekajUDR1
      RET

!rs_rx:
   in rsdata,udr1
   lDs rstemp, {Czekamnaeof}
   cpi rstemp,1
      breq koniec_ramki
   cpi rsdata,bofm_bit
      sbis sreg,1
   ret

   ldi rstemp,1
   sts {Czekamnaeof},rstemp
  ret

   !koniec_ramki:
     cpi rsdata, eofm_bit
      sbis sreg,1
     Ret
      ldi rstemp,0
      sts {Czekamnaeof},rstemp

 '        Print "przed nad: " ; S ; D ; J

      Te = 1
      ldi rstemp,bofmaster_bit                              'BOF
      !out udr1,rstemp

      RCALL czekajUDR1                                      'ZNAK U
      LDI rstemp, znaku
      !out udr1, rstemp

      RCALL czekajUDR1                                      'ZNAK A
      LDI rstemp, znaka
      !out udr1, rstemp

      RCALL czekajUDR1                                      'ZNAK =
      LDI rstemp, znakrowne
      !out udr1, rstemp

      Readeeprom S , 32
      RCALL czekajUDR1
      LDS rstemp, {S}
      subi rstemp, -48                                      'Setki
      !OUT UDR1, rstemp


      Readeeprom D , 31
      RCALL czekajUDR1
      LDS rstemp, {D}
      subi rstemp, -48                                      'dziesiatki
      !OUT UDR1, rstemp


      Readeeprom J , 30
      RCALL czekajUDR1
      LDS rstemp, {J}
      subi rstemp, -48                                      'jednosci
      !OUT UDR1, rstemp

      RCALL czekajUDR1                                      'ZNAK m
      LDI rstemp, znakm
      !out udr1, rstemp

      RCALL czekajUDR1                                      'ZNAK m
      !out udr1, rstemp

      ldi rstemp,eofs_bit
      !out udr1,rstemp

      RCALL czekajUDR1

      Te = 0


            'KONTROLNIE
   ret




Usart_tx_end:                                               'przerwanie wyst¹pi gdy USART wyœle znak i UDR bêdzie pusty
   Te = 0                                                   'wy³¹czenie nadajnika, w³¹czenie odbiornika
   'to samo co CBI PORTD,TE_pin, brak zmian w SREG
Return



!usart_init:
'procedura inicjalizacji USARTów
   ldi temp,0
   !out ubrr0h,temp                                         'bardziej znacz¹cy bajt UBRR USART0
   !out ubrr1h,temp
   ldi temp,_ubrr0
   !out ubrr0l,temp                                         'mniej znacz¹cy bajt UBRR USART0
   ldi temp,_ubrr1
   !out ubrr1l,temp                                         'mniej znacz¹cy bajt UBRR USART1
   ldi temp,24                                              'w³¹czone odbiorniki i nadajniki USARTów
   !out ucsr0b,temp
   !out ucsr1b,temp
   ldi temp,6                                               'N8bit
   !out ucsr0C,temp
   !out ucsr1C,temp
   'ustawienia RS485
   Te = 0                                                   'domyœlnie stan odbioru
   sbi ddrd,Te_pin                                          'wyjœcie TE silnopr¹dowe
   'w³¹czenie przerwañ
   Enable Urxc
   Enable Urxc1
   Enable Utxc1
ret