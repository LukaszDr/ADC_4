'program bufora danych wej�ciowych bez u�ycia przerwa�
'rozmiar bufora 16 znak�w, bufor w formie stringu
'za wpisywanym znakiem jest dopisywany znak ko�cz�cy string
'odbi�r znaki 13 powoduje wys�anie zawarto�ci bufora

'kontynuacja zadania:
'1. u�y� przerwania URXC
'2. zmieni� bufor na pier�cieniowy (najwcze�niej zapisane znaki s� zast�powane
'nowymi znakami, a odbi�r znaku 13 ma powodowa� zwr�cenie odebranych znak�w)
'rozmiar bufora 16 lub 64 lub 256

'wgranie bootloadera wymaga u�ycia programatora STK500 native driver
'po wgraniu bootloadera nale�y zmieni� typ programatopra i od��czy� programator

'u�y� bootloadera modyfikowanego MCS bez kodu dost�pu
'W Options->Programmer:
'1. ma by� wybrany MCS Bootloader
'2. poni�ej w zak�adce MCS Loader: Reset: DTR

'wymagane po��czenia:
'USB_RS.TxD -> PD.0
'USB_RS.Rxd -> PD.1
'USB_RS.DTR -> Reset (pojedynczy ko�ek ko�o przycisku resetu)

'by Marcin Kowalczyk

'obliczenia parametr�w konfiguracyjnych
Const Prescfc = 3                                           'pot�ga dzielnika cz�stotliwo�ci taktowania procesora
Const Fcrystal =(14745600 /(2 ^ Prescfc))                   'cz�stotliwo�� po przeskalowaniu
'Const Fcrystal =(3686400 /(2 ^ Prescfc))       'cz�stotliwo�� po przeskalowaniu
Const Baundrs = 115200                                      'pr�dko�� transmisji po RS [bps]
Const _ubrr =(((fcrystal / Baundrs) / 16) - 1)              'potrzebne w nast�pnych zadaniach
'konfigurowanie mikrokontrolera
$regfile = "m644pdef.dat"                                   'plik konfiguracyjny z literk� "p" w nazwie
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




Temp Alias R16                                              'aliasy rejestr�w procesora
Temph Alias R17
Rstemp Alias R18
Rsdata Alias R19

'zmniejszenie cz�stotliwo�ci taktowania procesora
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
   sbic ucsr0a,rxc0                                         'obej�cie gdy nie obebrano znaku
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
   push yl                                                  'o ile potrzeba  - sprawdzi�
   push yh                                                  'o ile potrzeba  - sprawdzi�

   in yl,portd
   ldi rstemp,128
   eor yl,rstemp
   !out portd,yl

   rcall rs_tx_init                                         'kod mo�e by� bezpo�renio w usart_rx

   'odtworzenie stanu jak przed przerwanie
   pop yh                                                   'o ile potrzeba - sprawdzi�
   pop yl                                                   'o ile potrzeba - sprawdzi�
   pop rstemp                                               'o ile potrzeba - sprawdzi�
   !out sreg,rstemp                                         'o ile potrzeba - sprawdzi�
   pop rstemp                                               'o ile potrzeba - sprawdzi�



Return

!rs_rx:
   in rsdata,udr0                                           'przepianie znaku zeruje RXC0
   'sprawdzenie czy polecenie wys�ania zawarto�ci bufora
   cpi rsdata,13
   sbic sreg,1
     STS {ftx},rsdata
      Ret

   !rs_tx_init:

   ldi rsdata,48

   LDS rstemp,{Aw}                                          'ofset zapisu
   Loadadr Bufor(1) , Y                                     'za�adowanie adresu zmiennaj S do pary adrespowej Y (r28,r29)
   'dodanie do adresu pocz�tku offsetu
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
    sbis ucsr0a,udre0                                       'obej�cie udre0 jest zajety
      ret

' w rstemp offset odczytu
   lds rsdata,{ftx}
   tst rsdata
   sbic sreg,1                                              'obejscie gdy z=0, niezerowy rstemp
      ret


   Loadadr Bufor(1) , Y                                     'za�adowanie adresu zmiennaj S do pary adrespowej Y (r28,r29)
   'dodanie do adresu pocz�tku offsetu
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

'   sbis ucsr0a,udre0       'obej�cie rjmp gdy UDR0 pusty - bit UDRE0 = 1
'      rjmp rs_tx    'czakanie w p�tli na zwolnienie UDR0
'   !out udr0,rsdata 'wpisanie nowego znaku do UDR0 - nie oznacza rozpocz�cia transmicji
ret