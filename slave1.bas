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
'pozosta³e aliasy
Te_pin Alias 4
Te Alias Portd.te_pin                                       'sterowanie przep³ywem w nadajniku/odbiorniku linii

Led_pin Alias 5
Led Alias Portd.led_pin


'Led_reg Alias Ddrd  'rejestr kontrolki nadawania, gdy anoda LED -> Ucc
'Led_reg Alias Portd 'rejestr kontrolki nadawania, gdy katoda LED -> GND
'Led_pin Alias 7     'numer wyprowadzenia portu dla kontrolki


'DEFINICJA ADC
'Ldi rstemp,0
'!out ddrA,rstemp
'Ldi rstemp, 2
'!out porta,rstemp

'LDI rstemp, &B11100001
'!OUT ADMUX, rstemp

'LDI rstemp, &B10000000
'!OUT ADCSRA, rstemp



'auto ADC
Config Adc = Single , Prescaler = Auto , Reference = Avcc

'Dim W As Word , Channel As Byte
'Channel = 0


'odpalenie timera
Config Timer1 = Timer , Prescale = 64

On Timer1 Oblicz_adc
Timer1 = 100

Enable  TIMER1







On Urxc1 Usart_rx Nosave                                    'deklaracja przerwania URXC (odbiór znaku USART)
On Utxc1 Usart_tx_end Nosave                                'deklaracja przerwania UTXC, koniec nadawania

'deklarowanie zmiennych
Dim Adrw As Byte                                            'adres w³asny
Dim Adro As Byte
Dim Bajt As Byte
Adrw = 1                                                    'adres odbiorcy 0...15
Const Bof_bit = &B11000000
Const Bofm_bit = &B10000001
Const Bofmaster_bit = &B11000010
Const Bofs_bit = &B11000001

Const Eofs_bit = &B10000001
Const Eofm_bit = &B10100010

Dim Stanodbioru As Byte
Stanodbioru = 0

SBI Ddrd,Led_pin

rcall usart_init                                            'inicjalizacja USARTów i w³¹czenie przerwañ
Sei                                                         'w³¹czenie globalnie przerwañ


Do
   sbi adcsra,6
'   W = Getadc(channel)
'   Waitms 1000
'   Print "Channel " ; Channel ; " value " ; W

   !petla_adc:
   SBIC ADCSRA, 4
      RJMP petla_ADC
   Waitms 1500
   rcall dioda
   Print "ADC: " ; Adc


Loop

Oblicz_adc:
   Print "LICZNIK"
Return


'ZMIANA ZMIANA

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
                                             '

!rs_rx:
   in rsdata,udr1
   lDs rstemp, {stanodbioru}
   cpi rstemp,1
      breq koniec_ramki
   cpi rsdata,bofm_bit
      sbis sreg,1
   ret

   ldi rstemp,1
   sts {stanodbioru},rstemp
  ret

   !koniec_ramki:
     cpi rsdata, eofm_bit
      sbis sreg,1
     Ret
      ldi rstemp,0
      sts {stanodbioru},rstemp

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

!dioda:
            Sbis portd,led_pin
      rjmp zapal_led
   sbic portd,led_pin
      rjmp zgas_led
   !zapal_led:
      Led = 1
      rjmp wyslij
   !zgas_led:
      Led = 0
      !wyslij:
      ret