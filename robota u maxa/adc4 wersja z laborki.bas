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


'aliasy rejestr�w procesora
Temp Alias R16
Temph Alias R17
Rstemp Alias R18
Rsdata Alias R19
'LICZNIK  ile bylo konwersji
Count Alias R25
'pozosta�e aliasy
Te_pin Alias 4
Te Alias Portd.te_pin                                       'sterowanie przep�ywem w nadajniku/odbiorniku linii

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

On Urxc1 Usart_rx Nosave                                    'deklaracja przerwania URXC (odbi�r znaku USART)
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



rcall usart_init                                            'inicjalizacja USART�w i w��czenie przerwa�
Sei                                                         'w��czenie globalnie przerwa�


Do

Loop

Oblicz_adc:
   push rstemp                                              'o ile potrzeba - sprawdzi�
   in rstemp,sreg                                           'o ile potrzeba  - sprawdzi�
   push rstemp                                              'o ile potrzeba - sprawdzi�
   push rsdata                                              'o ile potrzeba  - sprawdzi�
   push yl                                                  'o ile potrzeba  - sprawdzi�
   push yh                                                  'o ile potrzeba  - sprawdzi�
   push r1                                                  'o ile potrzeba  - sprawdzi�
   push r0                                                  'o ile potrzeba  - sprawdzi�
 '  !cli


   INC Count

   SBI adcsra,6
   !czekaj_adc:
   SBiC ADCSRA, 4
      RJMP czekaj_adc
   Suma = Suma + Adc
   cpi Count,16
      brEQ obliczenia


 '  sei
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
   Suma = Suma / 16                                         'usrednienie
   Suma = Suma / Wartdziel                                  '//przeskalowanie



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


   S = Suma / 100
   Asuma = S * 100
   Suma = Suma - Asuma

   D = Suma / 10
   Asuma = D * 10
   Suma = Suma - Asuma

   J = Suma
   CLR Count
   Suma = 0
   Print "po obliczeniach: " ; S ; D ; J
   Writeeeprom J , 30
   Writeeeprom D , 31
   Writeeeprom S , 32
   Ret
!mniejsze:
    Suma = 0
    RJMP Kont

!wieksze:
   Suma = 100
   RJMP Kont

Usart_rx:                                                   'etykieta bascomowa koniecznie bez !
   push rstemp                                              'o ile potrzeba - sprawdzi�
   in rstemp,sreg                                           'o ile potrzeba  - sprawdzi�
   push rstemp                                              'o ile potrzeba - sprawdzi�
   push rsdata                                              'o ile potrzeba  - sprawdzi�
   push yl                                                  'o ile potrzeba  - sprawdzi�
   push yh                                                  'o ile potrzeba  - sprawdzi�
   push r1                                                  'o ile potrzeba  - sprawdzi�
   push r0                                                  'o ile potrzeba  - sprawdzi�

   !cli
   rcall rs_rx                                              'kod mo�e by� bezpo�renio w usart_rx
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




Usart_tx_end:                                               'przerwanie wyst�pi gdy USART wy�le znak i UDR b�dzie pusty
   Te = 0                                                   'wy��czenie nadajnika, w��czenie odbiornika
   'to samo co CBI PORTD,TE_pin, brak zmian w SREG
Return



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