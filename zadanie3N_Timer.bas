'program bufora danych wejœciowych bez u¿ycia przerwañ
'rozmiar bufora 16 znaków, bufor w formie stringu
'za wpisywanym znakiem jest dopisywany znak koñcz¹cy string
'odbiór znaki 13 powoduje wys³anie zawartoœci bufora

'kontynuacja zadania:
'1. u¿yæ przerwania URXC
'2. zmieniæ bufor na pierœcieniowy (najwczeœniej zapisane znaki s¹ zastêpowane
'nowymi znakami, a odbiór znaku 13 ma powodowaæ zwrócenie odebranych znaków)
'rozmiar bufora 16 lub 64 lub 256

'wgranie bootloadera wymaga u¿ycia programatora STK500 native driver
'po wgraniu bootloadera nale¿y zmieniæ typ programatopra i od³¹czyæ programator

'u¿yæ bootloadera modyfikowanego MCS bez kodu dostêpu
'W Options->Programmer:
'1. ma byæ wybrany MCS Bootloader
'2. poni¿ej w zak³adce MCS Loader: Reset: DTR

'wymagane po³¹czenia:
'USB_RS.TxD -> PD.0
'USB_RS.Rxd -> PD.1
'USB_RS.DTR -> Reset (pojedynczy ko³ek ko³o przycisku resetu)

'by Marcin Kowalczyk

'obliczenia parametrów konfiguracyjnych
Const Prescfc = 3                                           'potêga dzielnika czêstotliwoœci taktowania procesora
Const Fcrystal =(14745600 /(2 ^ Prescfc))                   'czêstotliwoœæ po przeskalowaniu
'Const Fcrystal =(3686400 /(2 ^ Prescfc))       'czêstotliwoœæ po przeskalowaniu
Const Baundrs = 115200                                      'prêdkoœæ transmisji po RS [bps]
Const _ubrr =(((fcrystal / Baundrs) / 16) - 1)              'potrzebne w nastêpnych zadaniach
'konfigurowanie mikrokontrolera
$regfile = "m644pdef.dat"                                   'plik konfiguracyjny z literk¹ "p" w nazwie
$crystal = Fcrystal
$baud = Baundrs

Const _presctimer = 8
Const _fpr = 1                                              'czestotliwosc probkowania
Const _ocrtimer = Fcrystal / _presctimer / _fpr -1

Config Timer1 = Timer , Prescale = _presctimer , Compare A = Disconnect , Clear Timer = 1
Stop Timer1
Timer1 = 0
Ocr1a = _ocrtimer

On Oc1a Przerwanie Nosave




Temp Alias R16                                              'aliasy rejestrów procesora
Temph Alias R17
Rstemp Alias R18
Rsdata Alias R19

'zmniejszenie czêstotliwoœci taktowania procesora
ldi temp,128
!Out clkpr,temp                                             'ustawienie bitu 7, CLKPR = 128
ldi temp,prescfc                                            'aktualizacja CLKPR dopiero po uprzednim ustawienu bitu 7
!Out clkpr,temp                                             'CLKPR = Prescfc

'deklarowanie zmiennych
Dim Aw As Byte                                              'offset zapisu
Dim Ar As Byte                                              'offset odczytu
Dim Ftx As Byte                                             'flaga nadawania, gdy nieze5rowa, to nakaz nadawania
Const Rb = 32                                               'rozmiar bufora
Dim Bufor(rb) As Byte                                       ' bufor danych wejsciowych


Enable Oc1a
Start Timer1
sei

Do
   sbic ucsr0a,rxc0                                         'obejœcie gdy nie obebrano znaku
      rcall rs_rx

   lds rstemp,{ar}
   lds rsdata,{aw}
   cpse rstemp,rsdata
      rcall rs_tx

Loop


Przerwanie:
   push rstemp
   in rstemp,sreg
   push rstemp
   push yl                                                  'o ile potrzeba  - sprawdziæ
   push yh                                                  'o ile potrzeba  - sprawdziæ

   in yl,portd
   ldi rstemp,128
   eor yl,rstemp
   !out portd,yl

   rcall rs_tx_init                                         'kod mo¿e byæ bezpoœrenio w usart_rx

   'odtworzenie stanu jak przed przerwanie
   pop yh                                                   'o ile potrzeba - sprawdziæ
   pop yl                                                   'o ile potrzeba - sprawdziæ
   pop rstemp                                               'o ile potrzeba - sprawdziæ
   !out sreg,rstemp                                         'o ile potrzeba - sprawdziæ
   pop rstemp                                               'o ile potrzeba - sprawdziæ



Return

!rs_rx:
   in rsdata,udr0                                           'przepianie znaku zeruje RXC0
   'sprawdzenie czy polecenie wys³ania zawartoœci bufora
   cpi rsdata,13
   sbic sreg,1
     STS {ftx},rsdata
      Ret

   !rs_tx_init:

   ldi rsdata,48

   LDS rstemp,{Aw}                                          'ofset zapisu
   Loadadr Bufor(1) , Y                                     'za³adowanie adresu zmiennaj S do pary adrespowej Y (r28,r29)
   'dodanie do adresu pocz¹tku offsetu
   add yl,rstemp
   ldi rstemp,0                                             'LDI nie zmienia SREG
   adc yh,rstemp
   st y,rsdata

   LDS rstemp,{Aw}                                          'ofset zapisu
   inc rstemp
   Andi rstemp,(rb-1)                                       'maska rozmiaru bufora

   sts {Aw},rstemp                                          'zachowanie ofsetu zapisu

   LDS rsdata,{Ar}                                          'ofset odczytu
   cpse rstemp,rsdata
      ret

   inc rsdata
   Andi rsdata,(rb-1)                                       'maska rozmiaru bufora
   sts {Ar},rsdata                                          'zachowanie ofsetu odczytu

ret


!rs_tx:
    sbis ucsr0a,udre0                                       'obejœcie udre0 jest zajety
      ret

' w rstemp offset odczytu
   lds rsdata,{ftx}
   tst rsdata
   sbic sreg,1                                              'obejscie gdy z=0, niezerowy rstemp
      ret


   Loadadr Bufor(1) , Y                                     'za³adowanie adresu zmiennaj S do pary adrespowej Y (r28,r29)
   'dodanie do adresu pocz¹tku offsetu
   add yl,rstemp
   ldi rstemp,0                                             'LDI nie zmienia SREG
   adc yh,rstemp

   ld rstemp,y
   !out udr0,rstemp                                         'zainicjowanie nadawania

   LDS rsdata,{Ar}                                          'ofset odczytu
   inc rsdata
   Andi rsdata,(rb-1)                                       'maska rozmiaru bufora
   sts {Ar},rsdata                                          'zachowanie ofsetu odczytu

      LDS rstemp,{Aw}                                       'ofset zapisu
   cpse rstemp,rsdata
      ret

   ldi rstemp,0                                             'wyzerowanie flagi gdy ar=aw
   sts {ftx},rstemp

'   sbis ucsr0a,udre0       'obejœcie rjmp gdy UDR0 pusty - bit UDRE0 = 1
'      rjmp rs_tx    'czakanie w pêtli na zwolnienie UDR0
'   !out udr0,rsdata 'wpisanie nowego znaku do UDR0 - nie oznacza rozpoczêcia transmicji
ret