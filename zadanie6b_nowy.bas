

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

$eeprom                                                     'zawartoœæ eeprom wgrywana na zasadzie programowania
Data 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0       '0...15
Data 10 , 0 , 1 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0       '16...31, 16 - adres wlasny (dec.)
$data

'aliasy rejestrów procesora
Temp Alias R16
Temph Alias R17
Rstemp Alias R18
Rsdata Alias R19
'pozosta³e aliasy
Te_pin Alias 5
Te Alias Portd.te_pin                                       'sterowanie przep³ywem w nadajniku/odbiorniku linii


On Urxc Usart0_rx Nosave                                    'deklaracja przerwania URXC (odbiór znaku USART0)
On Urxc1 Usart1_rx Nosave                                   'deklaracja przerwania URXC1 (odbiór znaku USART1)
On Utxc1 Usart1_tx_end Nosave                               'deklaracja przerwania UTXC1, koniec nadawania

'deklarowanie zmiennych
Dim Adrw As Byte                                            'adres w³asny
Dim Adro As Byte                                            'adres odbiorcy 0...15
Dim Tabin(50) As Byte                                       'tabela znaków odebranych
Const Lstrmax = 24                                          'maksymalna liczba znaków w tabin
Dim Lstr As Byte                                            'liczba odebranych znaków z USART0
Const Bof_bit = &B11000000
Const Eof_bit = &B10000000


'zmniejszenie czêstotliwoœci taktowania procesora
ldi temp,128
!Out clkpr,temp                                             'ustawienie bitu 7, CLKPR = 128
ldi temp,prescfc                                            'aktualizacja CLKPR dopiero po uprzednim ustawienu bitu 7
!Out clkpr,temp                                             'CLKPR = Prescfc


rcall usart_init                                            'inicjalizacja USARTów i w³¹czenie przerwañ
Sei                                                         'w³¹czenie globalnie przerwañ

Readeeprom Adrw , 16


Do
   'inne procedury
Loop

Usart0_rx:                                                  'etykieta bascomowa koniecznie bez !
   push rstemp                                              'o ile potrzeba - sprawdziæ
   in rstemp,sreg                                           'o ile potrzeba  - sprawdziæ
   push rstemp                                              'o ile potrzeba - sprawdziæ
   push rsdata                                              'o ile potrzeba  - sprawdziæ
   push yl                                                  'o ile potrzeba  - sprawdziæ
   push yh                                                  'o ile potrzeba  - sprawdziæ
   push r1                                                  'o ile potrzeba  - sprawdziæ
   push r0                                                  'o ile potrzeba  - sprawdziæ

   rcall rs_rx                                              'kod mo¿e byæ bezpoœrenio w usart_rx

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

Usart1_rx:                                                  'etykieta bascomowa koniecznie bez !
   push rstemp                                              'o ile potrzeba - sprawdziæ
   in rstemp,sreg                                           'o ile potrzeba  - sprawdziæ
   push rstemp                                              'o ile potrzeba - sprawdziæ
'   push yl          'o ile potrzeba  - sprawdziæ
'   push yh          'o ile potrzeba  - sprawdziæ

   in rstemp,udr1
   !out udr0,rstemp                                         'wys³anie znaku do kumputera bez przetwarzania

   'odtworzenie stanu jak przed przerwanie
'   pop yh
'   pop yl
   pop rstemp
   !out sreg,rstemp
   pop rstemp
Return                                                      '

!rs_rx:
   in rsdata,udr1
   mov rstemp, rsdata                                       'odebranie danych z linii
   subi rstemp, 127
   sbis sreg,2
   rjmp sprawdz_status

   rcall sprawdz_adres
    'cokolowiek

!rec13:
   ldi rstemp,0
   sts {lstr},rstemp                                        'wyzerowanie licznika bajtów, rozpoznanie koñca przez 0
   'weryfikacja zakresu adresu 0...15, adres w dwóch pierwszych znakach w tabin
   'najpier dziesi¹tki potem jednostki
   Loadadr Tabin(1) , Y
   ld rsdata,y+                                             'dziesi¹tki +48
   subi rsdata,48
   ldi rstemp,10
   mul rsdata,rstemp                                        'w r0 liczba 10 razy wiêksza
   ld rsdata,y+                                             'jednostki +48
   subi rsdata,48
   add rsdata,r0
   cpi rsdata,16                                            'wersyfikacj adresu - ma byæ adres 0...15
   sbis sreg,2                                              'obejœcie gdy adres prawid³owy
      Ret

   sts {adro},rsdata                                        'do testu
   ori rsdata,bof_bit
   !out udr1,rsdata                                         'wys³anie BOF

   !rs485_tx:
      sbis ucsr1a,udre1
         rjmp rs485_tx                                      'czekaj a¿ UDR1 pusty
      ld rsdata,y+
      tst rsdata                                            'kontrola koñca
      breq frame_end
      Te = 1                                                'w³¹czenie nadajnika
      !out udr1,rsdata                                      'wys³anie znaku na magistarlê RS485 bez przetwarzania
   rjmp rs485_tx:

   !frame_end:
   Te = 1
   ldi rsdata,13
   !out udr1,rsdata                                         'wys³anie znaku koñcz¹cego ramkê

   'Print Adro       'do testu, uwaga na u¿yte zasoby w przerwaniu i wyd³u¿enie procedury
ret

Usart1_tx_end:                                              'przerwanie wyst¹pi gdy USART wyœle znak i UDR bêdzie pusty
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