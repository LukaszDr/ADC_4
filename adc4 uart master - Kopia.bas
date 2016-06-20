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
'OBLICZENIA
Jednostki Alias R26
Dziesiatki Alias R27
Setki Alias R28
'LICZNIK  ile bylo konwersji
Count Alias R25
Flag Alias R29
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
Dim Wl As Integer
Dim Wh As Integer
Dim Wartdziel As Integer
Dim Tempbyte As Byte

Wl = 200
Wh = 800
Wartdziel = Wh - Wl
Wartdziel = Wartdziel / 100
Wl = Wl / Wartdziel
Wh = Wh / Wartdziel


'SUMA - do niej zliczam kolejne odczyty
Dim Suma As Word
Suma = 0

'Adrw = 1    niepotrzebne
                                             'adres odbiorcy 0...15

'ZMIENNE TYSIACE ITD
Dim T As Integer
Dim S As Integer
Dim D As Integer
Dim J As Integer
Dim Asuma As Integer

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
   CPI flag,1
      Rcall obliczenia

Loop

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
'   Print "ADC:" ; Adc    'KONTROLNIE
   Suma = Suma + Adc
   cpi Count,16
      ldi Flag, 1


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

!obliczenia:
'   Print "WL: " ; Wl
'   Print "WH: " ; Wh
   Suma = Suma / 16
   Suma = Suma / Wartdziel                                  '//przeskalowanie

'   Print "SUMA po dzielenie: " ; Suma


   Tempbyte = Suma
   LDS rstemp, {Tempbyte}
   Tempbyte = Wl
   LDS rsdata, {Tempbyte}
   !SUB rstemp, rsdata
   SBIC SREG, 2
      RJMP mniejsze




   Tempbyte = Suma
   LDS rstemp, {Tempbyte}
   Tempbyte = Wh
   LDS rsdata ,{Tempbyte}
   !SUB rsdata, rstemp
   SBIC SREG, 2
       RJMP wieksze

   Suma = Suma - Wl

   Kont:
'   Print "Suma finalnie " ; Suma
'   Suma = Suma * Prescadc
'   Suma = Suma + Offset
'   Print "Srednia: " ; Suma  'KONTROLNIE
'   T = Suma / 1000
'   Asuma = T * 1000
'   Suma = Suma - Asuma

   S = Suma / 100
   Asuma = S * 100
   Suma = Suma - Asuma

   D = Suma / 10
   Asuma = D * 10
   Suma = Suma - Asuma

   J = Suma
   LDS jednostki, {J}
   LDS dziesiatki, {d}
   LDS setki, {s}

         !OUT UDR0,setki
      RCALL czekajUDR0

      !OUT UDR0,dziesiatki
      RCALL czekajUDR0

      !OUT UDR0,dziesiatki
      RCALL czekajUDR0



   'ZAPIS DO EEPROM
 '  Writeeeprom J , 40
 '  Writeeeprom D , 41
 '  Writeeeprom S , 42

' KONTROLNIE
'   Print "T: " ; T
   Print Suma
'   RCALL wyslij                                             'NIE WYSYLAM DO KOMPUTERA!!!!!!
   CLR Count
   Suma = 0
   Ldi flag, 0
   Ret


!mniejsze:
    Suma = 0
    RJMP kont

!wieksze:
   Suma = 100
   RJMP kont

Usart_rx:
   PUSH jednostki
   PUSH dziesiatki
   PUSH setki                                               'etykieta bascomowa koniecznie bez !
   push rstemp                                              'o ile potrzeba - sprawdziæ
   in rstemp,sreg                                           'o ile potrzeba  - sprawdziæ
   push rstemp                                              'o ile potrzeba - sprawdziæ
   push rsdata                                              'o ile potrzeba  - sprawdziæ
   push yl                                                  'o ile potrzeba  - sprawdziæ
   push yh                                                  'o ile potrzeba  - sprawdziæ
   push r1                                                  'o ile potrzeba  - sprawdziæ
   push r0                                                  'o ile potrzeba  - sprawdziæ
   !cli
   rcall rs_rx                                              'kod mo¿e byæ bezpoœrenio w usart_rx
   sei
   'odtworzenie stanu jak przed przerwanie
   pop r0
   pop r1
   pop yh
   pop yl
   pop rsdata
   pop rstemp
   !out sreg,rstemp
   pop rstemp
   POP setki
   POP dziesiatki
   POP jednostki
Return


                                                '
!czekajUDR0:
      sbiS ucsr0a,udre0                                     'czekaj na udr0
      rjmp czekajUDR0
      RET

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

       'KONTROLNIE!!!

 '     Readeeprom J , 40
 '     Readeeprom D , 41
 '     Readeeprom S , 42
 '           Print "PRZED NADANIEM DO MASTERA: " ; S ; D ; J

      Te = 1
      ldi rstemp,bofmaster_bit
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

      RCALL czekajUDR1
'      LDS rstemp, {T}
'      subi rstemp, -48                                      'TYSIACE
'      !OUT UDR1, rstemp

      RCALL czekajUDR1
      'LDS rstemp, {S}
      subi setki, -48                                       'Setki
      !OUT UDR1, setki

      RCALL czekajUDR1
      'LDS rstemp, {D}
      subi dziesiatki, -48                                  'dziesiatki
      !OUT UDR1, dziesiatki


      RCALL czekajUDR1
 '     LDS rstemp, {J}
      subi jednostki, -48                                   'jednosci
      !OUT UDR1, jednostki

      RCALL czekajUDR1                                      'ZNAK m
      LDI rstemp, znakm
      !out udr1, rstemp

      RCALL czekajUDR1                                      'ZNAK m
      LDI rstemp, znakm
      !out udr1, rstemp

      ldi rstemp,eofs_bit
      !out udr1,rstemp

      RCALL czekajUDR1

      Te = 0
      STS {s}, setki
      STS {d}, dziesiatki
      STS {j}, jednostki
      Print S ; D ; J

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