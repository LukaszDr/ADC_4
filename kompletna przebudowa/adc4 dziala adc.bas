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

'$eeprom
'WLsetki, WLjednostki, WHsetki, WHjednostki, Offset setki, Offset jednostki
'Data 0 , 0 , 10 , 8 , 0 , 0

$data

'aliasy rejestrów procesora
Temp Alias R16
Temph Alias R17
Rstemp Alias R18
Rsdata Alias R19

Wniski Alias R20
Wwysoki Alias R21
Wynikl Alias R23
Wynikh Alias R23
Tysiace Alias R24
Setki Alias R25
Dziesiatki Alias R26
Jednostki Alias R27
'LICZNIK  ile bylo konwersji
Count Alias R25
Te_pin Alias 4
Te Alias Portd.te_pin


                                               'sterowanie przep³ywem w nadajniku/odbiorniku linii

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

On Urxc1 Usart_rx Nosave                                    'deklaracja przerwania URXC (odbiór znaku USART)
On Utxc1 Usart_tx_end Nosave                                'deklaracja przerwania UTXC, koniec nadawania

Dim Tempbyte As Byte


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



rcall usart_init                                            'inicjalizacja USARTów i w³¹czenie przerwañ


Print "Tutaj"
clr wniski
clr wwysoki
clr count



Sei                                                         'w

Do
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


   inc Count



   SBI adcsra,6
   !czekaj_adc:
   SBiC ADCSRA, 4
      RJMP czekaj_adc

   in rstemp, adcl
   add wniski, rstemp
   sbic sreg,0
      SUBI wwysoki, -1


   in rstemp, adch
   add Wwysoki, rstemp

   cpi Count,16
   sbic sreg,1
      rcall skaluj


   mov wynikl,wniski
   mov wynikh,wwysoki

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

!skaluj:
   'usrednianie
   rcall podziell
   RCALL podzielh




   'mnozenie przez 250  i dzieleni prez 256
   LDI rstemp,250
   mul Wniski,rstemp
   mov Wniski,R1
   MUL Wwysoki,rstemp
   mov Wwysoki, R1
   ADD wniski, R0
   SBIC sreg,0
      subi wwysoki,-1


   mov wynikh, wwysoki
   mov wynikl, wniski
   ldi wwysoki,0
   ldi wniski,0
   ldi Count, 0
   ret

Podziell:
   lsr Wniski
   lsr Wniski
   LSR Wniski
   LSR Wniski
   ret

Podzielh:
   LSR Wwysoki
   SBIC SREG,0
      inc Wniski
   LSR Wwysoki
   SBIC SREG,0
      inc Wniski
   LSR Wwysoki
   SBIC SREG,0
      inc Wniski
   LSR Wwysoki
   SBIC SREG,0
      inc Wniski
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
   push wwysoki
   push wniski

   !cli
   rcall rs_rx                                              'kod mo¿e byæ bezpoœrenio w usart_rx
   sei

   'odtworzenie stanu jak przed przerwanie
   pop wniski
   pop wwysoki
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

 '    Print "przed nad: " ; S ; D ; J

      Te = 1
      ldi rstemp,bofmaster_bit                              'BOF
      !out udr1,rstemp
                                                            'ZNAK U
      LDI rstemp, znaku
      !out udr1, rstemp

      rcall dziesietnyl
      RCALL dziesietnyh


      LDI rstemp, znaka
      RCALL czekajUDR1                                      'ZNAK A
      !out udr1, rstemp

      LDI rstemp, znakrowne
      RCALL czekajUDR1                                      'ZNAK =
      !out udr1, rstemp


      ldi rstemp,eofs_bit
      !out udr1,rstemp


      ldi tysiace, 0
      ldi setki, 0
      ldi dziesiatki, 0
      ldi jednostki, 9

      RCALL czekajUDR1

      'Te = 0


            'KONTROLNIE
      ret

!dziesietnyl:
   subi wynikl, 100
   SBIC sreg,0
      rjmp dalej
   ldi setki,1
   subi wynikl,100
   sbic sreg,0
      RJMP dalej
   ldi setki,2
   subi wynikl,100

   !dalej:
   subi wynikl, -100

   RCALL liczdziesiatki
   subi wynikl, -10

   rcall liczjednostki


   ret
!Liczdziesiatki:
   SUBI wynikl,10
   SBIC SREG,0
      RET
   inc dziesiatki
   rcall liczdziesiatki


!Liczjednostki:
   SUBI wynikl,1
   SBIC SREG,0
      RET
   inc jednostki
   rcall liczjednostki

!dziesietnyh:
   SBRC wynikh,0
      rcall bit0
   SBRC wynikh,1
      rcall bit1

   LDI rsdata,0
   mov rstemp, jednostki
   rcall sprawdz
   ADD dziesiatki, rsdata

   LDI rsdata,0
   mov rstemp, dziesiatki
   rcall sprawdz
   ADD setki, rsdata

   LDI rsdata,0
   mov rstemp, setki
   rcall sprawdz
   ADD tysiace, rsdata


   ret

!bit0:
   Subi Setki , -2
   subi dziesiatki, -5
   subi jednostki, -6
   ret

!bit1:
   Subi Setki , -5
   subi dziesiatki, -1
   subi jednostki, -2
   ret

!sprawdz:
   Subi Rstemp , 10
   SBIs sreg,0
      LDI wynikl,1
   ret


!czekajUDR1:
      sbiS ucsr1a,udre1                                     'czekaj na udr1
      rjmp czekajUDR1
      RET

!czekajUDR0:
      sbiS ucsr0a,udre0                                     'czekaj na udr1
      rjmp czekajUDR0
      RET



Usart_tx_end:                                               'przerwanie wyst¹pi gdy USART wyœle znak i UDR bêdzie pusty
   Te = 0                                                   'wy³¹czenie nadajnika, w³¹czenie odbiornika
   'to samo co CBI PORTD,TE_pin, brak zmian w SREG
Return