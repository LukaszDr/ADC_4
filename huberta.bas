Const Prescfc = 0                                           'potêga dzielnika czêstotliwoœci taktowania procesora
Const Fcrystal =(14745600 /(2 ^ Prescfc))                   'czêstotliwoœæ po przeskalowaniu
'sta³e konfiguracujne USARTów
Const Baundrs0 = 115200                                     'prêdkoœæ transmisji po RS [bps] USART0
Const _ubrr0 =(((fcrystal / Baundrs0) / 16) - 1)            'potrzebne w nastêpnych zadaniach
Const Baundrs1 = Baundrs0                                   'prêdkoœæ transmisji po RS [bps] USART1
Const _ubrr1 =(((fcrystal / Baundrs1) / 16) - 1)            'potrzebne w nastêpnych zadaniach

'konfigurowanie mikrokontrolera
$regfile = "m644pdef.dat"                                   'plik konfiguracyjny z literk¹ "p" w nazwie
$crystal = Fcrystal                                         ' informuje program jaka jest czêstotliwoœæ taktowania


'aliasy rejestrów procesora
Temp Alias R16
Temph Alias R17
Rstemp Alias R18
Rsdata Alias R19
'pozosta³e aliasy
Te_pin Alias 4
Te Alias Portd.te_pin                                       'sterowanie przep³ywem w nadajniku/odbiorniku linii


Licznik Alias R20                                           'doliczanie do 16 operacji
Config Adc = Single , Prescaler = Auto , Reference = Avcc   'Reference off - z mikro (chyba), Avcc - napiêcie zasilania

Config Timer1 = Timer , Prescale = 1024 , Compare A = Disconnect , Clear Timer = 1       'konfiguracja timera
Stop Timer1
Ocr1a = 900
'sprawdziæ czy TIMER1=0 jest potrzebne, w tym rejestrze zmienia siê wartoœæ

On Oc1a Odczytadc Nosave                                    'przy przerwaniu Oc1a (od licznika) skocz do ADC

Enable Oc1a                                                 'w³¹czenie konkretnego przerwania Oc1a

On Urxc1 Usart_rx Nosave                                    'deklaracja przerwania URXC (odbiór znaku USART)
On Utxc1 Usart_tx_end Nosave                                'deklaracja przerwania UTXC, koniec nadawania


'deklarowanie zmiennych                                      'adres w³asny
Dim Adrw As Byte                                            'adres odbiorcy 0...15
Dim Adro As Byte                                            ' word bo 2 bity
Dim Srednia As Word
Dim Wartosc As Word
                                                             'ka¿dy wysy³a na pocz¹tku transmisji, je¿eli pojawi siê beggin of frame mastera to nas³uchuje
Const Bof_bit = &B11000000                                  'KOWALCZYK - PDF
Const Bofm_bit = &B10001101
Const Bofmaster_bit = &B11000010
Const Bofs_bit = &B11001101

Const Eofs_bit = &B10001101
Const Eofm_bit = &B10100010

Dim Odbior As Byte
Odbior = 0

rcall usart_init                                            'funkcja która inicjalizuje usarta
Start Timer1                                               'w³¹czenie timera

Sei                                                         'w³¹czenie globalnie przerwañ

Do

'SBI ADCsra, 6
'
'   !czekaj1:
'                                                            'je¿eli =0, przeskoczê linijke
'   SBIC ADCSRA, 4
'   RJMP czekaj1
'   Wartosc = Adc / 16
'   Print Adc
'   Waitms 1000


Loop



Odczytadc:
   push rstemp                                              'o ile potrzeba - sprawdziæ
   in rstemp,sreg                                           'o ile potrzeba  - sprawdziæ
   push rstemp                                              'o ile potrzeba - sprawdziæ
   push rsdata                                              'o ile potrzeba  - sprawdziæ
   push yl                                                  'o ile potrzeba  - sprawdziæ
   push yh                                                  'o ile potrzeba  - sprawdziæ
   push r1                                                  'o ile potrzeba  - sprawdziæ
   push r0                                                  'o ile potrzeba  - sprawdziæ
   !cli

   INC Licznik                                              'inkrementacja licznila
   SBI ADCsra, 6

   !czekaj:
                                                            'je¿eli =0, przeskoczê linijke
   SBIC ADCSRA, 4
   RJMP czekaj
   Wartosc = Adc / 16
   Print Adc

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

Usart_rx:                                                   'etykieta bascomowa koniecznie bez !
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
Return


!rs_rx:
   in rsdata,udr1
   lDs rstemp, {Odbior}
   cpi rstemp,1
      breq koniec_ramki
   cpi rsdata,bofm_bit
      sbis sreg,1
   ret

   ldi rstemp,1
   sts {Odbior},rstemp
  ret

   !koniec_ramki:
     cpi rsdata, eofm_bit
      sbis sreg,1
     Ret
      ldi rstemp,0
      sts {Odbior},rstemp

      Te = 1
      ldi rstemp,bofmaster_bit
      !out udr1,rstemp

      ldi rstemp,40
      !wyslij_liczby:

      !pusty_UDR:
      sbiS ucsr1a,udre1                                     'petla gdy udre1 jest zajety
      rjmp pusty_UDR

      !out udr1,rstemp
      inc rstemp
      cpi rstemp,120
         brne wyslij_liczby

      !pusty_UDR1:
      sbiS ucsr1a,udre1                                     'petla gdy udre1 jest zajety
      rjmp pusty_UDR1

      ldi rstemp,eofs_bit
      !out udr1,rstemp
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