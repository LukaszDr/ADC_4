'program testowy komunikacji na magistarli RS485
'znaki nadane w terminalu na jednym komputerze s� wysy�ane gdy adres odbiorcy 0
'wpisuj�c znaki najpierw wpisa� dwa znaki adresu, potem tekst do wys�ania i enter

'zadania:
'1. w procedurze odbioru wprowadzi� weryfikacj� adresu zawartego w BOF,
'2. wprowadzi� EOF,
'3. wprowadzi� automatyczne odpowiedzi ramk� potwierdzenia odbioru,
'4. zastosowa� przerwanie UDRE1,
'5. wprowadzi� dwudzielno�� bufora tabin(), jedna po�owa s�uzy do nadawania,
'a druga do odbioru znak�w lub wprowadzi� bufor pier�cieniowy.

'by Marcin Kowalczyk

'obliczenia parametr�w konfiguracyjnych
Const Prescfc = 0                                           'pot�ga dzielnika cz�stotliwo�ci taktowania procesora
Const Fcrystal =(14745600 /(2 ^ Prescfc))                   'cz�stotliwo�� po przeskalowaniu
'sta�e konfiguracujne USART�w
Const Baundrs0 = 115200                                     'pr�dko�� transmisji po RS [bps] USART0
Const _ubrr0 =(((fcrystal / Baundrs0) / 16) - 1)            'potrzebne w nast�pnych zadaniach
Const Baundrs1 = Baundrs0                                   'pr�dko�� transmisji po RS [bps] USART1
Const _ubrr1 =(((fcrystal / Baundrs1) / 16) - 1)            'potrzebne w nast�pnych zadaniach

'konfigurowanie mikrokontrolera
$regfile = "m644pdef.dat"                                   'plik konfiguracyjny z literk� "p" w nazwie
$crystal = Fcrystal
'$baud = Baundrs0    'zb�dne gdy inicjalizacja w zdefiniowanej procedurze

$eeprom
'WLsetki, WLjednostki, WHsetki, WHjednostki, Offset setki, Offset jednostki
Data 0 , 0 , 10 , 8 , 0 , 0

$data

'aliasy rejestr�w procesora
Temp Alias R16
Temph Alias R17
Rstemp Alias R18
Rsdata Alias R19

Wniski Alias R27
Wwysoki Alias R20
'LICZNIK  ile bylo konwersji
Count Alias R25
Flag Alias R26
'pozosta�e aliasy
Te_pin Alias 4
Te Alias Portd.te_pin

                                               'sterowanie przep�ywem w nadajniku/odbiorniku linii

'Config ADC
Config Adc = Single , Prescaler = Auto , Reference = Avcc

'USTAWIENIE TIMERA
Config Timer1 = Timer , Prescale = 64 , Compare A = Disconnect , Clear Timer = 1
Stop Timer1
Timer1 = 0                                                  'wARTOSC POCZ, RACZEJ NIEPOTRZEBNE
Ocr1a = 14399                                               'WARTOSC DO ZLICZENIA

On Oc1a Oblicz_adc Nosave                                   'wlaczenie przerwania timera
Enable Oc1a
                                                             'start timera
Start Timer1

On Urxc1 Usart_rx Nosave                                    'deklaracja przerwania URXC (odbi�r znaku USART)
On Utxc1 Usart_tx_end Nosave                                'deklaracja przerwania UTXC, koniec nadawania

'UStawienia obrabiania danych
'Dim Prescadc As Word
Dim Offset As Long

'WL i WH
Dim Wl As Single
Dim Wh As Single
Dim Wartdziel As Single
Dim Tempbyte As Byte

''Odczyt Wh, Wl i offset z eeprom

Readeeprom Tempbyte , 0
Wl = Tempbyte * 100
Readeeprom Tempbyte , 1
Wl = Wl + Tempbyte
Readeeprom Tempbyte , 2
Wh = Tempbyte * 100
Readeeprom Tempbyte , 3
Wh = Wh + Tempbyte
Readeeprom Tempbyte , 4
Offset = Tempbyte * 100
Readeeprom Tempbyte , 5
Offset = Offset + Tempbyte

'Print "Wartosci: !!!!!!!!!!!!!!!!!!" ; Wh ; Wl ; Offset

Wartdziel = Wh - Wl
'Wartdziel = Wartdziel / 1000
'Wl = Wl / Wartdziel
'Wh = Wh / Wartdziel


'SUMA - do niej zliczam kolejne odczyty
Dim Suma As Long
Dim Wynik As Single
Suma = 0
Wynik = 0

'Adrw = 1    niepotrzebne
                                             'adres odbiorcy 0...15

'ZMIENNE TYSIACE ITD
Dim T As Byte
Dim S As Byte
Dim D As Byte
Dim J As Byte
Dim Asuma As Word
Dim Tempsingle As Single

'CZY PRZYSZLO BOF
Dim Czekamnaeof As Byte
Czekamnaeof = 0


''RAMKI

Const Bof_bit = &B11000000
Const Bofm_bit = &B11000001
Const Bofmaster_bit = &B11000010
Const Bofs_bit = &B11000001

Const Eofs_bit = &B10000001
Const Eofm_bit = &B10100010

'ZNAKI ASCII

Const Znaku = &B01010101
Const Znaka = &B01000001
Const Znakrowne = &B00111101
Const Znakm = &B01101101



rcall usart_init                                            'inicjalizacja USART�w i w��czenie przerwa�
Sei                                                         'w��czenie globalnie przerwa�


Do
   cpi flag,1
   sbic sreg,1
      rcall obliczenia
Loop

!obliczenia:
'   Print "Przed wszystkim:" ; Suma
'   Suma = Suma / 16                                         'usrednienie
'   Print "Na poczatku: " ; Suma
'   If Wynik < Offset Then
'      Wynik = 0
'   Else
'      Wynik = Wynik - Offset
'      Wynik = Wynik / Wartdziel                             '//przeskalowanie
'   End If
 '  Print "po wartdziel: " ; Wynik


'   Tempbyte = Suma
'   LDS rstemp, {Wynik}
'   Tempbyte = Wh
'   LDS rsdata ,{Wh}
'   !SUB rstemp, rsdata
   'BRLO wieksze
'   SBIS SREG, 0
'       RJMP wieksze



 '  Tempbyte = Suma
 '  LDS rstemp, {Wynik}
'   Tempbyte = Wl
'   LDS rsdata ,{Wl}
'   !SUB rstemp, rsdata
'   BRLO mniejsze
'   SBIC SREG, 2
'      RJMP mniejsze
'   Print "Wynik: " ; Wynik ; "Wh: " ; Wh
   If Wynik > Wh Then
      T = 1
      S = 0
      D = 0
      J = 0
   Else
      T = 0
      Wynik = Wynik - Wl
      Wynik = Wynik * 1000
      Wynik = Wynik / Wartdziel
 '     Print " Po w-wl: " ; Wynik
      Asuma = Wynik / 1000
      T = Asuma
      Asuma = T * 1000
      Wynik = Wynik - Asuma
      Asuma = Wynik / 100
      S = Asuma
      Asuma = S * 100
      Wynik = Wynik - Asuma
      D = Wynik / 10
      Asuma = D * 10
      Wynik = Wynik - Asuma
      J = Wynik
   End If
   'Kont:
   LDI flag, 0
   Wynik = 0
'   Print "po obliczeniach: " ; T ; S ; D ; J
   Writeeeprom J , 30
   Writeeeprom D , 31
   Writeeeprom S , 32
   Writeeeprom T , 33
   Ret

'!mniejsze:
 '  Print "mniejsze"
 '   Wynik = 0
 '   S = 0
  '  D = 0
   ' J = 0
    'RJMP Kont

'!wieksze:
'   Print "wieksze"
 '  S = 1
  ' D = 0
   'J = 0
'   Wynik = 0
   'RJMP Kont


Oblicz_adc:
   push rstemp                                              'o ile potrzeba - sprawdzi�
   in rstemp,sreg                                           'o ile potrzeba  - sprawdzi�
   push rstemp                                              'o ile potrzeba - sprawdzi�
   push rsdata                                              'o ile potrzeba  - sprawdzi�
   push yl                                                  'o ile potrzeba  - sprawdzi�
   push yh                                                  'o ile potrzeba  - sprawdzi�
   push r1                                                  'o ile potrzeba  - sprawdzi�
   push r0                                                  'o ile potrzeba  - sprawdzi�
   !cli


   INC Count

   SBI adcsra,6
   !czekaj_adc:
   SBiC ADCSRA, 4
      RJMP czekaj_adc

   !in rstemp,ADCL
   !ADD wniski, rstemp

   SBIc sreg,0
      inc wwysoki

 '  !out UDR0, wniski

   !in rstemp, ADCH
   andi rstemp, &B00000011
 '  sts {tempbyte}, wwysoki
  ' Print "przed dodaniem: " ; Tempbyte
 '  sts {tempbyte}, rstemp
 '  Print "aktuany h: " ; Tempbyte
   !ADD wwysoki, rstemp
   STS {tempbyte}, wwysoki
   Print "Suma: " ; Tempbyte


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
   STS {Tempbyte},wniski
'   Print " niski: " ; Tempbyte
   STS {tempbyte},wwysoki
'   Print "wysoki: " ; Tempbyte
   Suma = Suma * 256
   Suma = Suma + Tempbyte
   Suma = Suma / 16
 '  Print "Po zsumowaniu: " ; Suma
   Suma = 0
   Tempbyte = 0
'   Print Suma
   ldi flag,1
   ldi count, 0
   ldi Wniski ,0
   ldi Wwysoki ,0
   ret

Usart_rx:                                                   'etykieta bascomowa koniecznie bez !
   push rstemp                                              'o ile potrzeba - sprawdzi�
   in rstemp,sreg                                           'o ile potrzeba  - sprawdzi�
   push rstemp                                              'o ile potrzeba - sprawdzi�
   push rsdata                                              'o ile potrzeba  - sprawdzi�
   push yl                                                  'o ile potrzeba  - sprawdzi�
   push yh                                                  'o ile potrzeba  - sprawdzi�
   push r1                                                  'o ile potrzeba  - sprawdzi�
   push r0                                                  'o ile potrzeba  - sprawdzi�
   push count
   push flag

   !cli
   rcall rs_rx                                              'kod mo�e by� bezpo�renio w usart_rx
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

     ' RCALL czekajUDR1                                      'ZNAK U
      LDI rstemp, znaku
      !out udr1, rstemp

      LDI rstemp, znaka
      RCALL czekajUDR1                                      'ZNAK A
      !out udr1, rstemp

      LDI rstemp, znakrowne
      RCALL czekajUDR1                                      'ZNAK =
      !out udr1, rstemp


      Readeeprom T , 33
      Rcall czekajUDR1
      LDS rsdata, {T}
      SUBI rsdata, -48
      CPI rsdata, 48
      SBIC SREG, 1
         LdI rsdata, 32
      !OUT UDR1, rsdata

      Readeeprom S , 32
      RCALL czekajUDR1
      LDS rstemp, {S}
      subi rstemp, -48                                      'Setki
      CPI rsdata,32
      SBIC SREG,1
         RCALL tysiace0
      !OUT UDR1, rstemp


      Readeeprom D , 31
      RCALL czekajUDR1
      LDS rsdata, {D}
      subi rsdata, -48                                      'dziesiatki
      CPI rstemp,32
      SBIC SREG,1
         RCALL setki0
      Te = 1
      !OUT UDR1, rsdata


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

      'Te = 0


            'KONTROLNIE
   ret




Usart_tx_end:                                               'przerwanie wyst�pi gdy USART wy�le znak i UDR b�dzie pusty
   Te = 0                                                   'wy��czenie nadajnika, w��czenie odbiornika
   'to samo co CBI PORTD,TE_pin, brak zmian w SREG
Return

!setki0:
      CPI rsdata,48
      SBIC SREG,1
         ldi rsdata, 32
      ret


!tysiace0:
   CPI rstemp, 48
   SBIC SREG, 1
      LDI rstemp, 32
   RET

!usart_init:
'procedura inicjalizacji USART�w
   ldi temp,0
   !out ubrr0h,temp                                         'bardziej znacz�cy bajt UBRR USART0
   !out ubrr1h,temp
   ldi temp,_ubrr0
   !out ubrr0l,temp                                         'mniej znacz�cy bajt UBRR USART0
   ldi temp,_ubrr1
   !out ubrr1l,temp                                         'mniej znacz�cy bajt UBRR USART1
   ldi temp,24                                              'w��czone odbiorniki i nadajniki USART�w
   !out ucsr0b,temp
   !out ucsr1b,temp
   ldi temp,6                                               'N8bit
   !out ucsr0C,temp
   !out ucsr1C,temp
   'ustawienia RS485
   Te = 0                                                   'domy�lnie stan odbioru
   sbi ddrd,Te_pin                                          'wyj�cie TE silnopr�dowe
   'w��czenie przerwa�
   Enable Urxc
   Enable Urxc1
   Enable Utxc1
ret