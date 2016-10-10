$regfile = "m328pdef.dat"                                   ' SIMCOM_SIM900D   s proshivkoy, podderjivayuschey DTMF-dekodirovanie
$crystal = 8000000                                          '     Revision:1137B09SIM900D64_ST_DTMF_JD_MMS
$hwstack = 50                                               ' Versiya s hraneniem vseh dannyh v EEPROM kontrollera. Na SIM - nichego net!
$swstack = 50
$framesize = 120                                            ' 1 raz SMS ob otkl 220v, i 1 raz SMS pri narujenii Temperatury (<min ili F>max)

$baud = 19200                                               ' Vyshe 19200 pri kvarce 4/8/16 MHz nel'zya! Ili nado s drobnymi znacheniyami!
               ' Dlya otladki  zamenit' "''  Print #1" na "'  Print #1" i obratno pri normal'nom rejime raboty BAZILIO

   Open "comd.4:19200,8,n,1" For Output As #1               'proggaz
   Open "comd.2:19200,8,n,1" For Input As #2                'proggaz
                                     'ubrat'   stroku Out_3 Alias Portd.4
Dim A As Byte , Inmessage$ As String * 4 , Time_error As Long
Dim Out_5 As Bit                                            'proggaz
Dim Startbit As Byte , Error_line As Byte                   'servisgaz
Dim Bin_vhodi As Byte , Kotli(3) As String * 1              ' , Kot_2 As String * 1 , Kot_3 As String * 1 ,       '  'servisgaz

'Vse dlya termodatchikov
Dim T_status(5) As Byte , Modem_s_counter As Byte , Power_akb As Byte
Dim Start_ok As Byte , Time_tm As Byte , Tm_counter As Byte
Dim Nom_ter As Byte
Dim T_d_a(5) As Integer , T_vih_a(5) As Byte                ' temperatura i znak, Datchik 1 - 5
Dim Adress_rom_termostata As Integer
Dim Termostat_v_a(5) As Byte , Termostat_min_a(5) As Integer , Termostat_max_a(5) As Integer       ' , vyhod na kajdom datchike + temperatura upravleniya vyhoda
Dim Sms_t_otpravil_a(5) As Byte                             ' SMS uje otpravlena: byl vyhod Temperatury datchika H za predely min ili max.
Dim T_min_sms_a(5) As Integer , T_max_sms_a(5) As Integer
'Vremennye peremennye
Dim Temp As Byte , Temp2 As Byte , Temp3 As Byte , Adc_data As Word , B As Byte
Dim Byte0 As Byte , Byte1 As Byte , Integ1 As Integer , Integ2 As Integer , T1 As Byte , T2 As Byte , Simvol$ As String * 1 , Tstr$ As String * 14
'Vse dlya vhodov
Dim Numbern$ As String * 13
Dim Otvetn$ As String * 280
Dim Vhod_d_a(5) As Byte
Dim Vhod_w_a(5) As Byte
Dim Vhod_m_a(5) As Byte
Dim Vhod_v_a(5) As Byte
Dim Vhod_min_a(5) As Byte
Dim Vhod_max_a(5) As Byte
Dim Vhod_otpravil_a(5) As Byte
Dim Vhod_w_p_a(5) As Byte
Dim Pauza_do_sireny_v(5) As Byte
Dim In_0 As Byte , Vhod_otpravil As Byte , Vhod_m As Byte
Dim Vhod_w As Byte , Vhod_min As Byte , Vhod_max As Byte
Dim Vhod_w_p As Byte , Pauza_do_sireny_0 As Byte , Vhod_d As Byte , Nomer_vhoda As Byte

Dim Rejim_vihoda_a(5) As Byte , Rejim_vihoda As Byte
Dim Zaderjka_postanovki As Byte , Zaderjka_postanovki_guard As Byte
Dim Sirena_timer As Byte , Pauza_do_sireny As Byte , Vhod_adc As Word
Dim Sirena_timer_p As Byte
Dim Nomer_termostata As Byte , Nomer_vihoda As Byte
Dim Sms_net220v As Byte , Timer_pow As Byte , Dozvon_po_trevoge As Byte
Dim Alarm_in As Byte , Error_gsm_modem As Byte , Error_line_tm As Byte
Dim Counter_c As Byte , Begunok As Byte , Ohrana As Byte , Admin As Byte , Begunok2 As Byte

Dim Phone_$_a(5) As String * 14 , Number$ As String * 14 , Phoner(5) As Byte

Dim Komanda$ As String * 70 , Parametri_komandy$ As String * 41
Dim Otvet$ As String * 280                                  'soobschenie, kotoroe modul' budet otpravlyat'
Dim Sms$ As String * 280                                    ' Vhodyaschee SMS soobschenie

Dim T_znak_temp As Integer , T_znak_temp2 As Integer        ' vremennaya peremennaya dlya hraneniya znacheniya temperatury so znakom

Dim Temp_massiv_9bait(9) As Byte                            ' vremennoe hranilische dlya chteniya\zapisi nomera ustroystva 1wire
Dim Ds18b20_1(8) As Byte , Ds18b20_2(8) As Byte , Ds18b20_3(8) As Byte , Ds18b20_4(8) As Byte , Ds18b20_5(8) As Byte
Dim Kolvo_18b20 As Byte , Kolvo_t_mem As Byte               ' Skol'ko zaregistrirovano termometrov i "tabletok"
Dim Kolichestvo_1wire As Word                               ' word peremennaya dlya poscheta kolichestva datchikov na linii

Dim Poslednij_tm As Byte                                    ' Nomer poslednego ispol'zuemogo klyucha TM
Dim Config_device As Byte                                   ' bity-flagi, opredelyayut rejimy raboty: otpravka SMS, dozvon i prochee
Dim Vremya_dozvona As Byte                                  '   skol'ko vremeni pytat'sya dozvonit'sya, x * 0.75 sekundy   15=12sek

Config Portd = Output : Portd = 0
Config Portd.7 = Input : Portd.7 = 1                        ' = Knopka Nomer_Hozyaina + rezistor podtyajki k +pitaniya
Button Alias Pind.7                                         ' podklyuchenie knopki vhoda v nastroyku
L_no_220 Alias Portd.5                                      ' podklyuchenie svetodioda
L_gsm Alias Portd.6                                         ' podklyuchenie svetodioda
Out_3 Alias Portd.3                                         ' vyhod 4     'proggaz
Config Portd.0 = Input                                      ' = RX uC     'proggaz
Config Portd.2 = Input                                      'proggaz
'                                                                          'proggaz
Config Portb = Output : Portb = 0                           'proggaz
Out_2 Alias Portb.0                                         ' vyhod 1    'proggaz
Out_1 Alias Portb.1                                         ' vyhod 2    'proggaz
Out_4 Alias Portb.2                                         ' vyhod 3    'proggaz

Pwr_key_gsm Alias Portb.4                                   ' upravlenie pitaniem GSM-modema SIM900d

L_guard Alias Portb.3                                       ' podklyuchenie svetodioda
L_tm Alias Portb.5                                          ' podklyuchenie svetodioda


''  Print #1 , "START PROGRAMM"
Config Portc = Input : Portc = 0                            ' Vhody ot datchikov
In_5 Alias 0                                                'Pinc.0        ' vhod 5
In_4 Alias 1                                                'Pinc.1        ' vhod 4
In_3 Alias 2                                                'Pinc.2        ' vhod 3
In_2 Alias 3                                                'Pinc.3        ' vhod 2
In_1 Alias 4                                                'Pinc.4        ' vhod 1

Tm_in Alias Pinc.5                                          ' vhod Datchik Memory Touch
Config 1wire = Tm_in

Config Adc = Single , Prescaler = Auto , Reference = Avcc : Start Adc

''  Print #1 , ""
''  Print #1 , "Start"


' Adresa yacheek, gde hranyatsya nastroyki
$eeprom                                                     ' Schitaem adresa iz yacheek EEPROM

Gosub Chtenir_nastroek
                                        ' Chitaem nomera telefonov iz EEPROM kontrollera
Gosub Chtenie_nomerov_iz_eeprom                             ' Zanosim v massivy nomera klyuchey i termometrov

' Nastroyki termostata: min= temperatura vklyucheniya, max= temperatura otklyucheniya

' 2 bayta   Pridet SMS, esli T <= ukazannoy     s 144-


' Nastroyki vhodov (shleyfov). ZadaetFsya rabochiy predel min i max. Esli v predelah - norma, inache - oshibka

''  Print #1 , "Config=" ; Bin(config_device)

Config Serialin = Buffered , Size = 254                     ' Bufer dlya prinyatyh po UART simvolov
Enable Interrupts                                           ' Obyazatel'no, chtoby rabotal bufer priema!!
If Tm_in = 0 Then Gosub Install_mode                        ''  Print #1 , "Analiz linii TM"                                ' Esli minus na linii TM, to v Nastroyka
Gosub Wait_5sec
Gosub Test_power

Gosub Init_modem                                            ' Zapusk GSM-modema
Startbit = 0 : Kotli(1) = "0" : Kotli(2) = "0" : Kotli(3) = "0"
Bin_vhodi = 0
Out_5 = 0                                                   'proggaz
Zaderjka_postanovki_guard = 0
Vremya_dozvona = 50
T_status(1) = 2
T_status(2) = 2
T_status(3) = 2
T_status(4) = 2
T_status(5) = 2
Phoner(1) = 9
Error_gsm_modem = 0                                         ' flagi oshibok obmena s GSM-modemom i liniey TM 1wire
Ohrana = 0                                                  ' flag rejima ohrany: 1-Vklyucheno, 0-Otklyucheno
Admin = 0                                                   ' Otklyuchim rejim vvoda nastroek
Poslednij_tm = 0                                            ' Nomer poslednego ispol'zuemogo klyucha TM
Start_ok = 10
Pauza_do_sireny = 0                                         ' Pauza pered vklyucheniem trevogi. Chtoby uspet' doyti i otklyuchit'
Sirena_timer = 0                                            ' Vremya zvuchaniya trevogi - sireny
Zaderjka_postanovki_guard = 0
Time_tm = 0
Tm_counter = 0
Gosub Chitaem_nomera
Config Watchdog = 2048
On Wdt Wd_isr
                                              ' Vklyuchaem preryvanie ot perepolneniya Storojevogo taymera WDT
Wdtcsr = &B0100_0000


'------------------------------------------------------------------------------------------------------------------------------------------------------------
'  Otvet$ = "Ver: 10-09-2014 "
''  Print #1 , Otvet$                                           ' otobrazim versiyu
''  Print #1 , ""

'  Print #1 , "Start"
 ''  Print #1 , Config_device
'Print #1 , "Start"
If Config_device.1 = 1 Then                                 '  Flag "G".      AvtoPostanovka na Ohranu pri vkl.ustroystva
 ''  Print #1 , "AvtoPostanovka na ohranu. Analiz shleyfov..."
  ' Alarm_in = 0
   ' Opros vhodov (shleyfov)  Esli shleyfy v norme (zakryty), to stanet na Ohranu!
  ' Vhod_adc = Getadc(in_1) : Vhod_adc = Vhod_adc / 4
   ''  Print #1 , "Vhod 1= " ; Vhod_adc ; ", min:" ; Vhod_min_a(1) ; ", max:" ; Vhod_max_a(1)
  ' If Vhod_adc < Vhod_min_a(1) Or Vhod_adc > Vhod_max_a(1) Then Alarm_in = 1       ' esli znacheniya vyshli za predely, to oshibka

  ' Vhod_adc = Getadc(in_2) : Vhod_adc = Vhod_adc / 4
   ''  Print #1 , "Vhod 2= " ; Vhod_adc ; ", min:" ; Vhod_min_a(2) ; ", max:" ; Vhod_max_a(2)
  ' If Vhod_adc < Vhod_min_a(2) Or Vhod_adc > Vhod_max_a(2) Then Alarm_in = 2       ' esli znacheniya vyshli za predely, to oshibka

  ' Vhod_adc = Getadc(in_3) : Vhod_adc = Vhod_adc / 4
   ''  Print #1 , "Vhod 3= " ; Vhod_adc ; ", min:" ; Vhod_min_a(3) ; ", max:" ; Vhod_max_a(3)
  ' If Vhod_adc < Vhod_min_a(3) Or Vhod_adc > Vhod_max_a(3) Then Alarm_in = 3       ' esli znacheniya vyshli za predely, to oshibka

  ' Vhod_adc = Getadc(in_4) : Vhod_adc = Vhod_adc / 4
   ''  Print #1 , "Vhod 4= " ; Vhod_adc ; ", min:" ; Vhod_min_a(4) ; ", max:" ; Vhod_max_a(4)
  ' If Vhod_adc < Vhod_min_a(4) Or Vhod_adc > Vhod_max_a(4) Then Alarm_in = 4       ' esli znacheniya vyshli za predely, to oshibka

  ' Vhod_adc = Getadc(in_5) : Vhod_adc = Vhod_adc / 4
   ''  Print #1 , "Vhod 5= " ; Vhod_adc ; ", min:" ; Vhod_min_a(5) ; ", max:" ; Vhod_max_a(5)
  ' If Vhod_adc < Vhod_min_a(5) Or Vhod_adc > Vhod_max_a(5) Then Alarm_in = 5       ' esli znacheniya vyshli za predely, to oshibka

      Ohrana = 3                                            ' shleyfy v norme = postavim na ohranu
End If


Do                                                          'proggaz
   Nomer_vhoda = 254
   Gosub Service_gas_input                                  'Servisgaz
Loop Until Startbit = 1 Or Startbit = 2
Nomer_vihoda = 0                                            'proggaz


If Error_line = 3 Then                                      'proggaz
   Otvet$ = "h "
End If

' <<<<<<<<<<<   Chtoby pri vklyuchenii ustroystva prihodilo soobschenie  <<<<<<<<<<<<<<<<<<<<<<<
If Config_device.7 = 1 Then                                 '        Flag "A".       Esli zadano nastroykami - soobschit' o vklyuchenii
   Start_ok = 0
   Otvet$ = Otvet$ + "ver:3.5 zapusk"
End If                                                      '   If Config_device.7 = 1
'  Print #1 , "Start PROGRAMM"                               'FOR TESTING


'  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
Start_prg:
   Reset Watchdog : Gosub Waitms_100
   'Toggle L_tm
      If T_znak_temp2 = 250 Then
      Gosub Sendsms
      T_znak_temp2 = 0
      End If

      If Zaderjka_postanovki_guard > 0 Then
         Toggle L_guard
         If Begunok = 1 Then Decr Zaderjka_postanovki_guard
          If Zaderjka_postanovki_guard = 0 Then
              Ohrana = 3
              If Poslednij_tm > 0 And Poslednij_tm < 15 Then Otvet$ = "tm=" + Str(poslednij_tm)
          End If
      Else
             If Ohrana = 0 Then L_guard = 0
             If Ohrana = 1 Then L_guard = 1
      End If

' If Start_ok > 110 Then
'   Otvet$ = "malBva tel 8 8422 539563 ili 942324"
'   Rejim_vihoda = 1
'   Gosub Sendsms
'   Start_ok = 10
'   Otvet$ = ""
' End If



 If Start_ok >= 0 And Start_ok < 10 Then
   Start_ok = Start_ok + 1
   If Start_ok = 1 Then
      Tstr$ = "r"
      Komanda$ = "r"
      Admin = 1
      Gosub Test_001
   End If
   If Start_ok = 2 Then Start_ok = 10
 End If

 If Ohrana > 1 And Ohrana < 5 And Start_ok > 9 Then Gosub R_ohrani
                                                  ' Ohrana = 0

   Gosub Opros_vhodov                                       ' Opros vhodov (shleyfov)

   If Pauza_do_sireny > 0 Then
      Toggle L_guard                                        ' sv.diod Ohrana migaet
      Decr Pauza_do_sireny
      ''  Print #1 , "Do sireny: " ; Pauza_do_sireny
      If Pauza_do_sireny = 0 Then
       ''  Print #1 , "Sirena!!!!!!"
        If Vhod_m_a(alarm_in) <> 4 Then
         Rejim_vihoda = 1
         Gosub Perebor_na_vkl
        End If                                              ' Vklyuchim sirenu
        ' L_gsm = 1                                          ' sv.diod GSM
         L_guard = 1                                        ' sv.diod Ohrana

         If Config_device.2 = 1 Then                        ' otpravim SMS
          ''  Print #1 , "SMS - Alarm! " ; Alarm_in
            Vhod_adc = Alarm_in * 16
            Vhod_adc = Vhod_adc + 320

            Otvet$ = ""                                     ' chitaem soobschenie iz EEPROM mikrokontrollera
            For Temp = 0 To 15
               Adc_data = Vhod_adc + Temp                   ' Po adresu Vhod_adc + X
               Readeeprom T1 , Adc_data                     ' Peremennaya, adres
               If T1 = 255 Then Exit For                    ' Doshli do konca soobscheniya
               Otvet$ = Otvet$ + Chr(t1)
            Next

            ' esli v pamyati net nastroek
            Otvet$ = "trevoga " + Str(alarm_in) + ": " + Otvet$
            Rejim_vihoda = 40
            Gosub Sendsms                                   ' otpravim soobschenie

          ''  Print #1 , "SMS o trevoge otpravil"
         End If                                             '  If Config_device.2 = 1    ' otpravim SMS

         Gosub Nabor_nomera                                 '  delaem dozvon iz-za narusheniya odnogo iz vhodov (shleyfov)
         Gosub Flushbuf                                     'ochistka bufera
      End If                                                '   If Pauza_do_sireny = 0
   End If                                                   ' If Pauza_do_sireny > 0 Then


   If Begunok2 = 90 Then                                    'proggaz
       If Startbit = 1 Then
            Gosub Service_gas_input                         'cervisgaz ""
       End If
       Begunok2 = 0
   End If
      Incr Begunok2                                         'proggaz



   If Sirena_timer > 0 And Pauza_do_sireny = 0 Then         ' Pischit zadannoe vremya
     If Dozvon_po_trevoge = 0 And Alarm_in > 0 Then Dozvon_po_trevoge = 123
     If Begunok = 20 Then
     Decr Sirena_timer
      ''  Print #1 , "Pischim esche: " ; Sirena_timer
     End If
   Else
      Rejim_vihoda = 1
      Gosub Perebor_na_vikl                                 ' Otklyuchim sirenu
   End If                                                   '  If Sirena_timer > 0


   If Error_line_tm = 255 Then
      Goto Modem
   End If
   Incr Begunok

 ''  Print #1 , Begunok

   If Dozvon_po_trevoge > 121 Then                          ' v proshlyy raz ne dozvonilis'.Flag trevogi, v soedinenii sbrasyvaetsya

      Decr Dozvon_po_trevoge
      Gosub Nabor_nomera                                    '  delaem dozvon iz-za narusheniya odnogo iz vhodov (shleyfov)
      Alarm_in = 0                                          ' Sbros flaga Trevogi i nomera povrejdennogo shleyfa

   End If

   If Begunok = 30 Then                                     ' Raz v X sek zameryaet parametry gsm-signala
      If Dozvon_po_trevoge > 90 Then Decr Dozvon_po_trevoge
      If Dozvon_po_trevoge = 116 Then Dozvon_po_trevoge = 123
      Gosub Flushbuf                                        'ochistka bufera
      Print "AT+CSQ" : Gosub Waitms_100                     ' vydast parametry priema, dva parametra
      Gosub Getline                                         ' smotrim chto prishlo ot modema v bufer
      Gosub Flushbuf                                        'ochistka bufera
      Tstr$ = Mid(sms$ , 7 , 2)                             ' vydelim dve cifry -  "moschnost'"
      Temp2 = Val(tstr$)
      If Temp2 = 0 Or Temp2 > 33 Then
           Incr Error_gsm_modem
           L_gsm = 0
           If Error_gsm_modem > 5 Then Gosub Init_modem     ' Esli dolgo net signala - perezagruzim modem
         Else
            Error_gsm_modem = 0
            L_gsm = 1
            Temp2 = Temp2 * 3
          ''  Print #1 , "SQ:" ; Temp2
      End If
   End If                                                   '    If Begunok = 60

   If Begunok = 60 Then
   'Adr_t_mem = 50              ' 8 bayt * 5 klyuchey =  944...1024
   ''  Print #1 , "TEST POWER"                                ' Raz v XX sek  zameryaet T i napryajenie pitaniya
      Gosub Test_power
      If Kolvo_18b20 <> 0 Then                              '  chitaem pokazaniya termometrov, esli oni vneseny v nastroyki
         If Error_line_tm <> 255 Then
           Gosub 18b20                                      '  chitaem pokazaniya termometrov
           Gosub Analiz_pokazanij_termometrov               ' Analiz pokazaniy termometrov
         End If
      End If
         Begunok = 0                                        '  If Kolvo_18b20 <> 0
   End If                                                   ' If Begunok = 110


 If Time_tm = 0 And Error_line_tm <> 255 Then
  Gosub Opros_tm
 Else
   If Time_tm > 0 Then Decr Time_tm
 End If

   ' ---  Opros modema ---
  Modem:
   Gosub Getline                                            ' smotrim chto prishlo ot modema v bufer  - Sms$
   Tstr$ = Left(sms$ , 5)                                   ' vydelim pervye 6 simvolov
 ''  Print #1 , Sms$

   If Tstr$ = "+CMT:" Then                                  ' Nam SMS
      Gosub Waitms_500
      Gosub Opros_sms                                       ' Rasshifruem poluchennoe
      Print "AT+CMGD=1"                                     ' Udalim SMS ¹1 - chtoby telefonu bylo kuda sohranyat' poluchenuyu SMS
   End If                                                   '   If Tstr$ = "+CMT: "
   If Tstr$ = "+CUSD" Then
      Otvetn$ = Mid(sms$ , 11 , 160)
      For Temp = 1 To 160 Step 4
         Sms$ = Mid(otvetn$ , Temp , 4)
         Gosub Convert_to_eng
      Next
      If Otvet$ = "" Then Otvet$ = "owibka"
      Gosub Sendsms
   End If
   If Tstr$ = "+CLIP" Then                                  '  nam zvonyat   Vhodyaschiy zvonok   +CLIP: "+79308284748",145,"",,"",0

      Sms$ = Mid(sms$ , 9 , 13)                             ' dostaem nomer zvonyaschego
      Gosub Vydelim_nomer_iz_stroki                         ' Vydelenie # iz stroki v kovychkah i s "+" vnachale v peremennuyu Number$

      Gosub Nomet_svoj_chuzhij
    ''  Print #1 , "1=Admin,  0=User   " ; Admin              ' Etot nomer svoy? Admin ili Pol'zovatel'?
      If Counter_c <> 0 Then                                ' Eto izvestnyy nomer
       ''  Print #1 , "Nomer dostoveren"                      ' esli svoy nomer, to vypolnit' komandu
         Adc_data = 0 : Temp = 0 : Sms$ = "" : Alarm_in = 0 ' ochistim peremennye
         Gosub Nam_pozvonili
      Else
            Gosub Nam_pozvonili_end

      End If                                                '   If Counter_c <> 0
   End If                                                   '   If Tstr$ = "+CLIP:" Then

   '   Waitms 200                                               ' Pauza v osnovnom cikle. Uvelichivat' nejelatel'no.
   Gosub Waitms_100
   Goto Start_prg



   '======================================================================
Nam_pozvonili:                                              ' Obrabotka Vhodyaschego golosovogo vyzova ----

   Toggle L_gsm : Gosub Waitms_100 : Reset Watchdog

   Incr Adc_data                                            ' Schitaem vremya seansa svyazi
   If Adc_data > 1150 Then                                  ' Prevyshen limit vremeni - 2 minuty = 120 * 10
    ''  Print #1 , "Prevyshen limit vremeni!"
      Temp2 = 0                                             ' vremya vyshlo
      Goto Nam_pozvonili_end
   End If


   If Sms$ = "RING" Then                                    ' Zvonyat
      Incr Temp                                             ' schitaem kol-v gudkov
      If Temp = 4 Then                                      ' Skol'ko gudkov ne lojili trubku
         Print "ATA"                                        ' otvetit' na zvonok
         Gosub Waitms_750                                   ' Pauza obyazatel'na!!!
       ''  Print #1 , "Svyaz' ustanovlena"
         Komanda$ = ""                                      ' obyazatel'no vychistit' peremennuyu
         If Ohrana = 0 Then                                 ' 1 zv.signal, esli otklyucheno, 2 - esli vklyucheno
            Print "AT+VTS=" ; Chr(34) ; "0" ; Chr(34)       '     1 zvukovoy signal zvonyaschemu v liniyu
         Else
            Print "AT+VTS=" ; Chr(34) ; "1,1" ; Chr(34)     '    2  zvukovyh signala zvonyaschemu v liniyu
         End If                                             '   If Ohrana = 0
      End If                                                '  If Temp = 4
   End If                                                   ' If Sms$ = "RING"

   If Alarm_in <> 0 Then                                    ' esli srabotala trevoga, to peredaem gudkami ¹ srabotavshego shleyfa
      Incr T1
      If T1 > 45 Then
         T1 = 0
         For T2 = 1 To Alarm_in
            Print "AT+VTS=" ; Chr(34) ; "0" ; Chr(34)       '     1 zvukovoy signal zvonyaschemu v liniyu
            Gosub Waitms_500
         Next
      End If
   End If


   If Sms$ = "NO CARRIER" Or Sms$ = "NO DIALTONE" Then      ' polojili trubku / propala svyaz' - razorvat'
    ''  Print #1 , "Zakonchili vyzov"
      Temp2 = 2                                             ' Abonent prerval svyaz'
      ' Esli VKLYuChEN rejim upravlenie dozvonom s nomerov Hozyaev, bez podnyatiya trubki
      If Config_device.3 = 1 And Admin = 1 Then             ' Upravlenie Ohranoy dozvonom s nomerov Hozyaev 1 i 2
         If Temp <= 3 Then                                  ' Bystro polojili trubku posle zvonka
          ''  Print #1 , "Pereklyuchim sostoyanie signalizacii "
           Otvet$ = Number$
            If Ohrana = 0 Then                              ' Pereklyuchim sostoyanie signalizacii
               Ohrana = 3
            Else
               Ohrana = 2
               Pauza_do_sireny = 0 : Sirena_timer = 0
               Rejim_vihoda = 1                             'Otklyuchenie sireny
               Gosub Perebor_na_vikl
            End If                                          '   If Ohrana = 0
         End If                                             '  If temp <=3 Then
      End If                                                ' If Config_device.3 = 1
      Nam_pozvonili_end:
         Print "ATH0"                                       '  polojit' trubku
         Gosub Waitms_750                                   ' Pauza obyazatel'na!!!
         Admin = 0
       Return                                               ' vyhodim
   End If                                                   '   If Sms$ = "NO CARRIER"


   Tstr$ = Left(sms$ , 5)
 '  Print #1 , Tstr$
                                                   '        +DTMF:*      +DTMF:#     +DTMF:1      +DTMF:0

  '  If Tstr$ = "+DTMF" Then
'      Tstr$ = Mid(sms$ , 7 , 1)                             ' Vydelim simvol, peredannyy signalom DTMF
'      If Admin = 1 Then Komanda$ = Komanda$ + Tstr$
'   '  Print #1 , Komanda$
'      If Len(komanda$) > 5 Then                             ' Esli dlina bol'shaya, to yavno oshibka
'         Komanda$ = ""                                      '  - ne to najali, obnulyaem
'         Print "AT+VTS=" ; Chr(34) ; "1,1,1" ; Chr(34)      '    3 signala v liniyu = Oshibka!
'      End If
'    ''  Print #1 , "$=" ; Komanda$ ; "."

'      If Komanda$ = "1" Then                                '  Vklyuchit' Ohranu
'         Ohrana = 3 : L_guard = 1
'         Print "AT+VTS=" ; Chr(34) ; "1,1" ; Chr(34)        '    2  zvukovyh signala zvonyaschemu v liniyu
'         Goto Nam_pozvonili_konec
'      End If
'      If Komanda$ = "0" Then                                '  Otklyuchit' Ohranu
'         Ohrana = 2 : L_guard = 0
'         Rejim_vihoda = 1
'         Gosub Perebor_na_vikl                              ' Otklyuchim sirenu
'         Print "AT+VTS=" ; Chr(34) ; "0" ; Chr(34)          '     1 zvukovoy signal zvonyaschemu v liniyu
'         Goto Nam_pozvonili_konec
'      End If

'      If Komanda$ = "*####" Then                            '   ' Polnyy perezapusk ustroystva!!
'         'Print "ATH0"     '  polojit' trubku
'         Goto Reset_device                                  ' Polnyy perezapusk ustroystva!!
'      End If

'      If Komanda$ = "*4" Then                               ' menyaem Chuvstvitel'nost' mikrofona, rejim 1
'         Print "AT+CMIC=0,5"                                ' minimal'naya
'         Goto Nam_pozvonili_konec
'      End If
'      If Komanda$ = "*5" Then                               ' menyaem Chuvstvitel'nost' mikrofona, rejim 2
'         Print "AT+CMIC=0,10"                               ' srednyaya
'         Goto Nam_pozvonili_konec
'      End If
'      If Komanda$ = "*6" Then                               ' menyaem Chuvstvitel'nost' mikrofona, rejim 3
'         Print "AT+CMIC=0,15"                               ' maksimal'naya
'         Goto Nam_pozvonili_konec
'      End If


'      If Komanda$ = "#11" Then
'         Out_1 = 1                                          ' Vklyuchit' vyhod 1
'         Goto Nam_pozvonili_konec
'      End If
'      If Komanda$ = "#10" Then
'         Out_1 = 0                                          ' Otklyuchit' vyhod 1
'         Goto Nam_pozvonili_konec
'      End If

'      If Komanda$ = "#21" Then
'         Out_2 = 1                                          ' Vklyuchit' vyhod 2
'         Goto Nam_pozvonili_konec
'      End If
'      If Komanda$ = "#20" Then
'         Out_2 = 0                                          ' Otklyuchit' vyhod 2
'         Goto Nam_pozvonili_konec
'      End If

'      If Komanda$ = "#31" Then
'         Out_3 = 1                                          ' Vklyuchit' vyhod 3
'         Goto Nam_pozvonili_konec
'      End If
'      If Komanda$ = "#30" Then
'         Out_3 = 0                                          ' Otklyuchit' vyhod 3
'         Goto Nam_pozvonili_konec
'      End If

'      If Komanda$ = "#41" Then
'         Out_4 = 1                                          ' Vklyuchit' vyhod 4
'         Goto Nam_pozvonili_konec
'      End If
'      If Komanda$ = "#40" Then
'         Out_4 = 0                                          ' Otklyuchit' vyhod 4
'         Goto Nam_pozvonili_konec
'      End If
'      If Komanda$ = "#51" Then
'         Out_5 = 1                                          ' Vklyuchit' vyhod 5
'         Goto Nam_pozvonili_konec
'      End If
'      If Komanda$ = "#50" Then
'         Out_5 = 0                                          ' Otklyuchit' vyhod 5
'         Goto Nam_pozvonili_konec
'      End If

'   End If                                                   '   If Tstr$ = "+DTMF:"

   Gosub Getline                                            ' smotrim chto prishlo ot modema v bufer

   Goto Nam_pozvonili

Nam_pozvonili_konec:
   Gosub Waitms_500
   Print "AT+VTS=" ; Chr(34) ; "#" ; Chr(34)                ' zv.signal zvonyaschemu
   Komanda$ = ""                                            ' obyazatel'no vychistit' peremennuyu
   Sms$ = ""                                                ' obyazatel'no vychistit' peremennuyu
   Adc_data = 0                                             ' Sbrosim schetchik - Uvelichim vremya do otklyucheniya svyazi
   Goto Nam_pozvonili





   '======================================================================
Nabor_nomera:                                               ' Zvonim
 '  Print #1 , "Dozvon... Dozvon = " ; Dozvon_po_trevoge
 If Config_device.4 = 1 Then Return
   Gosub Flushbuf                                           'ochistka bufera
   Print "AT+CLCC=1" : Gosub Waitms_500                     ' vklyuchit' otobrajenie izmeneniy vyzovov
   ' pri dozvone abonentu GSM vydaet:  +CLCC: 1,0,3,0,0,"+79051004257",145,""
   ' kogda podnyali trubku:                          +CLCC: 1,0,0,0,0,"+79051004257",145,""
   ' kogda polojili:                                    +CLCC: 1,0,6,0,0,"+79051004257",145,""


   If Dozvon_po_trevoge > 121 Then                          ' Ne dozvonilis'! Probuem zvonit' na nomer vtorogo Admina (esli est')
     For T2 = 1 To 5
      If Phoner(t2) = 9 Then
         If Len(phone_$_a(t2)) > 3 Then                     '   esli takoy est'
            Number$ = Phone_$_a(t2) : Gosub Nabor_nomera_2
            If Dozvon_po_trevoge = 0 Then Exit For
         End If
      End If
     Gosub Opros_tm                                         'TEST TM
     Next
   End If
   If Dozvon_po_trevoge < 100 And Dozvon_po_trevoge > 90 Then Dozvon_po_trevoge = 123       ' Flag trevogi, soedinenie ego sbrasyvaet

   L_gsm = 1                                                ' sv.diod GSM
   Print "AT+CLCC=0" : Gosub Waitms_100                     ' Otklyuchit' otobrajenie izmeneniy vyzovov
Return


Nabor_nomera_2:
 ''  Print #1 , "Nabiraem " ; Number$

   Print "ATD+" ; Number$ ; ";"                             ' obyazatel'no tochka s zapyatoy v konce
   Gosub Flushbuf                                           'ochistka bufera

   ' poshel gudok? nomer nabran verno?
   For Temp3 = 0 To 200                                     ' v techenie 10 sekund jdem podtverjdeniya bazoy ish.vyzova
      Waitms 50 : Gosub Opros_tm : Vremya_dozvona = 0 : Toggle L_gsm       ' sv.diod GSM
      Reset Watchdog
      Gosub Getline                                         ' smotrim chto prishlo ot modema v bufer
      Sms$ = Left(sms$ , 12)

      If Sms$ = "+CLCC: 1,0,3" Then                         ' pri dozvone abonentu  - gudok poshel
       ''  Print #1 , "Gudok poshel..."
         Gosub Waitms_750 : Reset Watchdog

         ' Jdem ustanovki svyazi s vyzyvaemym abonentom
         For Vremya_dozvona = 0 To 30                       ' skol'ko vremeni pytat'sya dozvonit'sya
            Gosub Opros_tm                                  'TEST TM
            Gosub Waitms_750 : Toggle L_gsm                 ' sv.diod GSM
            Reset Watchdog                                  '
            ''  Print #1 , Dozvon_po_trevoge
            Gosub Getline                                   ' smotrim chto prishlo ot modema v bufer
            Sms$ = Left(sms$ , 12)

            If Sms$ = "+CLCC: 1,0,0" Then                   ' podnyali trubku
             ''  Print #1 , "Dozvonilis'"
               Adc_data = 980 : Temp = 4 : Sms$ = "RING" : Admin = 1       ' chtoby srazu ustanovilas' svyaz', na 15-18 sek
               Gosub Nam_pozvonili
               ' vyshli iz seansa svyazi s abonentom, ili zakonchilos' vremya seansa
               Dozvon_po_trevoge = 0                        ' chtoby srazu vyyti iz cikla I sbros flaga "ne dozvonilsya"
            End If
         Next

      End If
   Next
   Vremya_dozvona = 50
   Print "ATH0"                                             '  polojit' trubku
   Gosub Waitms_750                                         ' Pauza obyazatel'na!!!
Return






'======================================================================
Opros_sms:
   Error_line_tm = 0                                        ' obrabotka novoy SMS        '  vnachale vydaetsya nomer otpravitelya, zatem samo soobschenie
  Time_tm = 0
''  Print #1 , "Polucheno SMS: " ; Sms$                       ' +CMT: "+79308284748","Boss1","14/03/31,12:43:29+16"
    Otvet$ = ""
   'Sms$ = Mid(sms$ , 8 , 13)                                ' dostaem nomer otpravitelya SMS
 ''  Print #1 , "1>>  " ; Sms$
   'Gosub Vydelim_nomer_iz_stroki                            ' Vydelenie # iz stroki v kovychkah i s "+" vnachale v peremennuyu Number$
   Print "AT+CMGR=1,0"
   Rejim_vihoda = 55
   Gosub Getline
   Sms$ = Otvet$
'   Print #1 , "SMS = " ; Sms$
     Otvet$ = ""
   Rejim_vihoda = 0
 '  Print #1 , "nomer = " ; Number$
   Gosub Nomet_svoj_chuzhij                                 ' Etot nomer svoy? Admin ili Pol'zovatel'?
 ''  Print #1 , "1=Admin,  0=User   " ; Admin

   If Counter_c <> 0 And Admin = 1 Then                     ' esli SMS s nastroennyh nomerov, to vypolnit' komandu
    ''  Print #1 , "Nomer dostoveren"
    '  Gosub Getline                                         'zdes' samo SMS v formate:  komanda_parametry

     ' Sms$ = Lcase(sms$)                                    'zdes' samo SMS v formate:  komanda_parametry
     '  Print #1 , "Dannye: " ; Sms$                         ' Vse simvoly SMS - v nijniy registr!    TEST

        Test_001:
         '  Print #1 , Sms$                                  'TESTING
        '  Print #1 , "OTVET = " ; Otvet$                    'TESTING
      Tstr$ = Left(sms$ , 4)
        If Tstr$ = "otkl" Then
                 Tstr$ = Mid(sms$ , 6 , 1)
                 Nomer_vihoda = Val(tstr$)
                  Nomer_vihoda = Nomer_vihoda + 10
                  Gosub Vikl_vihoda
                  Goto Opros_sms_end
        End If
       ' If Tstr$ = "akb " Then
'           Power_akb = Getadc(7)
'           Writeeeprom Power_akb , 832
'        End If
        If Tstr$ = "vkl " Then                              'servisgaz
                T_vih_a(1) = 2
                T_vih_a(2) = 2
                T_vih_a(3) = 2
                T_vih_a(4) = 2
                T_vih_a(5) = 2
                Tstr$ = Mid(sms$ , 5 , 1)
                Nomer_vihoda = Val(tstr$)
                Nomer_vihoda = Nomer_vihoda + 10
                Gosub Vkl_vihoda
                Goto Opros_sms_end
          End If

Tstr$ = Mid(sms$ , 1 , 2)

        If Tstr$ = "ss" Then
          Parametri_komandy$ = Mid(sms$ , 5)
              Tstr$ = Mid(sms$ , 3 , 1)
              Counter_c = Val(tstr$)
              If Len(parametri_komandy$) >= 1 Then          ' Esli est' tekst (hotya by 1-2 simvolov) = pravil'no
                   ''  Print #1 , "Sohranim tekst '" ; Parametri_komandy$ ; "' dlya vhoda ¹" ; Counter_c
                     Reset Watchdog
                     If Len(parametri_komandy$) > 15 Then   ' Esli tekst dlinee 15 simvolov -
                        Parametri_komandy$ = Left(parametri_komandy$ , 15) + Str(255)       ' -  obrejem do 15 i marker Konec teksta
                     End If
              End If
                If Counter_c > 0 And Counter_c < 6 Then
                    Vhod_adc = Counter_c * 16
                    Vhod_adc = Vhod_adc + 320
                  T1 = Vhod_adc + 15
                  For Integ1 = Vhod_adc To T1
                  T2 = 255
                     Writeeeprom T2 , Integ1
                  Next
                  For Temp = 1 To 15
                     Tstr$ = Mid(parametri_komandy$ , Temp , 1)
                     If Tstr$ <> "" Then
                        T1 = Asc(tstr$)
                     Else
                        T1 = 255
                     End If
                     Adc_data = Temp + Vhod_adc             ' Po adresu Vhod_adc + X
                     Decr Adc_data                          ' -1, chtoby popalo vse po adresu!!!
                     Writeeeprom T1 , Adc_data              ' Peremennaya, adres
                  Next
                ''  Print #1 , "OK"
                  Otvet$ = "prinqto ss"
                End If
                Tstr$ = "1"
                Goto Opros_sms_end
        End If                                              'if nss

         If Tstr$ = "nr" Then
                     Tstr$ = Mid(sms$ , 4 , 1)
                     Rejim_vihoda_a(1) = Val(tstr$) : Writeeeprom Rejim_vihoda_a(1) , 830
                     Tstr$ = Mid(sms$ , 5 , 1)
                     Rejim_vihoda_a(2) = Val(tstr$) : Writeeeprom Rejim_vihoda_a(2) , 831
                     Tstr$ = Mid(sms$ , 6 , 1)
                     Rejim_vihoda_a(3) = Val(tstr$) : Writeeeprom Rejim_vihoda_a(3) , 832
                     Tstr$ = Mid(sms$ , 7 , 1)
                     Rejim_vihoda_a(4) = Val(tstr$) : Writeeeprom Rejim_vihoda_a(4) , 833
                     Otvet$ = "prinqto nr"
                     Tstr$ = ""
                     Nomer_vhoda = 9
                     Goto Opros_sms_end
           End If

        If Tstr$ = "sm" Then
                    Parametri_komandy$ = Mid(sms$ , 5)
                    Tstr$ = Mid(sms$ , 3 , 1)
                    Counter_c = Val(tstr$)
                    If Len(parametri_komandy$) >= 1 Then    ' Esli est' tekst (hotya by 1-2 simvolov) = pravil'no
                         ''  Print #1 , "Sohranim tekst '" ; Parametri_komandy$ ; "' dlya vhoda ¹" ; Counter_c
                           Reset Watchdog
                           If Len(parametri_komandy$) > 40 Then       ' Esli tekst dlinee 15 simvolov -
                              Parametri_komandy$ = Left(parametri_komandy$ , 40) + Str(255)       ' -  obrejem do 15 i marker Konec teksta
                           End If
                    End If
                      If Counter_c > 0 And Counter_c < 9 Then
                          Vhod_adc = Counter_c * 41
                          Vhod_adc = Vhod_adc + 375
                        T1 = Vhod_adc + 40
                        For Integ1 = Vhod_adc To T1
                        T2 = 255
                           Writeeeprom T2 , Integ1
                        Next
                        For Temp = 1 To 40
                           Tstr$ = Mid(parametri_komandy$ , Temp , 1)
                           If Tstr$ <> "" Then
                              T1 = Asc(tstr$)
                           Else
                              T1 = 255
                           End If
                           Adc_data = Temp + Vhod_adc       ' Po adresu Vhod_adc + X
                           Decr Adc_data                    ' -1, chtoby popalo vse po adresu!!!
                           Writeeeprom T1 , Adc_data        ' Peremennaya, adres
                        Next
                      ''  Print #1 , "OK"
                        Otvet$ = "prinqto sm"
                     End If
                     Tstr$ = "1"
                     Goto Opros_sms_end
        End If                                              'if nsm

        If Tstr$ = "vs" Then
                    Tstr$ = Mid(sms$ , 4 , 1)
                    Counter_c = Val(tstr$)
                    Parametri_komandy$ = Mid(sms$ , 5)
                    If Len(parametri_komandy$) >= 7 Then
                    Parametri_komandy$ = Mid(sms$ , 5 , 1) : T1 = Val(parametri_komandy$)       'min porog
                    Parametri_komandy$ = Mid(sms$ , 6 , 1) : T2 = Val(parametri_komandy$)       'max porog
                    Shift T1 , Left , 4 : Shift T2 , Left , 4       ' preobrazuem v znachenie 1v = 16 edinic ADC posle delitelya i preobrazovaniy
                    Parametri_komandy$ = Mid(sms$ , 7 , 1) : Byte0 = Val(parametri_komandy$) * 2       ' Vydelyaem  simvol =  D reakciya
                    Parametri_komandy$ = Mid(sms$ , 8 , 1) : Byte1 = Val(parametri_komandy$) * 20       ' Vydelyaem  simvol =  W neaktivnost'
                    Parametri_komandy$ = Mid(sms$ , 9 , 1) : Pauza_do_sireny_v(counter_c) = Val(parametri_komandy$) * 24       ' Vydelyaem  simvol =  P pauza do
                    Parametri_komandy$ = Mid(sms$ , 10 , 1) : Temp2 = Val(parametri_komandy$)       '  posledniy,  simvol =  rejim raboty vhoda
                    Parametri_komandy$ = Mid(sms$ , 11 , 1) : Nomer_vihoda = Val(parametri_komandy$)
                     If Counter_c = 1 Then                  ' sohranim vvedennye dannye  1
                        Vhod_min_a(1) = T1 : Vhod_max_a(1) = T2 : Writeeeprom Vhod_min_a(1) , 176 : Writeeeprom Vhod_max_a(1) , 177
                        Vhod_d_a(1) = Byte0 : Writeeeprom Vhod_d_a(1) , 186
                        Writeeeprom Byte1 , 191 : Writeeeprom Pauza_do_sireny_v(counter_c) , 196
                        Vhod_m_a(1) = Temp2 : Writeeeprom Vhod_m_a(1) , 201
                        Vhod_v_a(1) = Nomer_vihoda : Writeeeprom Vhod_v_a(1) , 825
                     End If

                     If Counter_c = 2 Then                  ' sohranim vvedennye dannye  2
                        Vhod_min_a(2) = T1 : Vhod_max_a(2) = T2 : Writeeeprom Vhod_min_a(2) , 178 : Writeeeprom Vhod_max_a(2) , 179
                        Vhod_d_a(2) = Byte0 : Writeeeprom Vhod_d_a(2) , 187
                        Writeeeprom Byte1 , 192 : Writeeeprom Pauza_do_sireny_v(counter_c) , 197
                        Vhod_m_a(2) = Temp2 : Writeeeprom Vhod_m_a(2) , 202
                        Vhod_v_a(2) = Nomer_vihoda : Writeeeprom Vhod_v_a(2) , 826
                     End If

                     If Counter_c = 3 Then                  ' sohranim vvedennye dannye  3
                        Vhod_min_a(3) = T1 : Vhod_max_a(3) = T2 : Writeeeprom Vhod_min_a(3) , 180 : Writeeeprom Vhod_max_a(3) , 181
                        Vhod_d_a(3) = Byte0 : Writeeeprom Vhod_d_a(3) , 188
                        Writeeeprom Byte1 , 193 : Writeeeprom Pauza_do_sireny_v(counter_c) , 198
                        Vhod_m_a(3) = Temp2 : Writeeeprom Vhod_m_a(3) , 203
                        Vhod_v_a(3) = Nomer_vihoda : Writeeeprom Vhod_v_a(3) , 827
                     End If

                     If Counter_c = 4 Then                  ' sohranim vvedennye dannye  4
                        Vhod_min_a(4) = T1 : Vhod_max_a(4) = T2 : Writeeeprom Vhod_min_a(4) , 182 : Writeeeprom Vhod_max_a(4) , 183
                        Vhod_d_a(4) = Byte0 : Writeeeprom Vhod_d_a(4) , 189
                        Writeeeprom Byte1 , 194 : Writeeeprom Pauza_do_sireny_v(counter_c) , 199
                        Vhod_m_a(4) = Temp2 : Writeeeprom Vhod_m_a(4) , 204
                        Vhod_v_a(4) = Nomer_vihoda : Writeeeprom Vhod_v_a(4) , 828
                     End If

                     If Counter_c = 5 Then                  ' sohranim vvedennye dannye  5
                        Vhod_min_a(5) = T1 : Vhod_max_a(5) = T2 : Writeeeprom Vhod_min_a(5) , 184 : Writeeeprom Vhod_max_a(5) , 185
                        Vhod_d_a(5) = Byte0 : Writeeeprom Vhod_d_a(5) , 190
                        Writeeeprom Byte1 , 195 : Writeeeprom Pauza_do_sireny_v(counter_c) , 200
                        Vhod_m_a(5) = Temp2 : Writeeeprom Vhod_m_a(5) , 205
                        Vhod_v_a(5) = Nomer_vihoda : Writeeeprom Vhod_v_a(5) , 829
                     End If
                    Otvet$ = "prinqto vs"
                    Tstr$ = ""
                    Nomer_vhoda = 13
                  End If
                  Goto Opros_sms_end
        End If                                              'if nvs

        If Tstr$ = "vm" Then
                         Tstr$ = Mid(sms$ , 4 , 6 )
                         Gosub Wait_2sec
                         Print #1 , Tstr$
                         Tstr$ = ""
                         Nomer_vhoda = 11
                         Goto Opros_sms_end
        End If                                              'if nvm

        If Tstr$ = "nn" Then
              Tstr$ = Mid(sms$ , 4 , 1)
              Parametri_komandy$ = Mid(sms$ , 6 , 12)
                  Otvet$ = Sms$
                  Sms$ = Parametri_komandy$
                  ''  Print #1 , "P>  " ; Sms$                ' dostaem nomer otpravitelya SMS
                  Gosub Vydelim_nomer_iz_stroki             ' Vydelenie nomera iz stroki v kovychkah i s "+" vnachale  v peremennuyu Number$
                  Sms$ = Otvet$
                  Otvet$ = ""
                 If Tstr$ = "0" Then                        ' Zapominanie nomera kak Boss_1   ' Komanda: 1234 boss 1 NomerTelefona
                     Phone_$_a(1) = Number$                 ' Sohranim etot nomer kak nomer Bossa1
                     Vhod_adc = 256 : Gosub Sohranim_nomer  ' Sohranim etot nomer kak nomer Boss_1
                     Otvet$ = "0"

                 Elseif Tstr$ = "1" Then                    ' Zapominanie nomera kak Boss_2   ' Komanda: 1234 boss 2 NomerTelefona
                     Phone_$_a(2) = Number$                 ' Sohranim etot nomer kak nomer Bossa1
                     Vhod_adc = 272 : Gosub Sohranim_nomer  ' Sohranim etot nomer
                     Otvet$ = "1"
                  Elseif Tstr$ = "2" Then                   ' Zapominanie nomera kak user_1  ' Komanda: 1234 user 1 NomerTelefona
                     Phone_$_a(3) = Number$                 ' Sohranim etot nomer kak nomer Bossa1
                     Vhod_adc = 288 : Gosub Sohranim_nomer  ' Sohranim etot nomer
                     Otvet$ = "2"
                  Elseif Tstr$ = "3" Then                   ' Zapominanie nomera kak user_2  ' Komanda: 1234 user 2 NomerTelefona
                     Phone_$_a(4) = Number$                 ' Sohranim etot nomer kak nomer Bossa1
                     Vhod_adc = 304 : Gosub Sohranim_nomer  ' Sohranim etot nomer
                     Otvet$ = "3"
                  Elseif Tstr$ = "4" Then                   ' Zapominanie nomera kak user_3  ' Komanda: 1234 user 3 NomerTelefona
                     Phone_$_a(5) = Number$                 ' Sohranim etot nomer kak nomer Bossa1
                     Vhod_adc = 320 : Gosub Sohranim_nomer  ' Sohranim etot nomer
                     Otvet$ = "4"
                  End If
               If Otvet$ <> "" Then
                  Otvet$ = "n " + Otvet$ + " prinqt"
                  Tstr$ = ""
                  Nomer_vhoda = 19
               End If
               Goto Opros_sms_end
           End If

          If Tstr$ = "nd" Then
            Sms$ = Mid(sms$ , 2)
             For Temp = 2 To 5
               Integ1 = Temp + 1
               Tstr$ = Mid(sms$ , Integ1 , 1)
               Phoner(temp) = Val(tstr$)
               Integ1 = 834 + Temp
               Writeeeprom Phoner(temp) , Integ1
             '  Print #1 , "Pnoher " ; Temp ; " = " ; Phoner(temp)
             Next
            Otvet$ = "prinqto nd"
            Nomer_vhoda = 9
            Tstr$ = ""
            Goto Opros_sms_end
          End If


                  If Tstr$ = "sk" Then
                      T1 = 50
                      T2 = 89
                      Integ1 = 944
                      Integ2 = 1023
                      Kolvo_t_mem = 0 : Writeeeprom Kolvo_t_mem , 2
                  End If
                  If Tstr$ = "st" Then
                      T1 = 10
                      T2 = 49
                      Kolvo_18b20 = 0 : Writeeeprom Kolvo_18b20 , 1
                  End If
                  If Tstr$ = "sv" Then
                      T1 = 10
                      T2 = 89
                      Integ1 = 944
                      Integ2 = 1023
                      Kolvo_t_mem = 0 : Writeeeprom Kolvo_t_mem , 2
                      Kolvo_18b20 = 0 : Writeeeprom Kolvo_18b20 , 1
                  End If
                  If Tstr$ = "sv" Or Tstr$ = "st" Or Tstr$ = "sk" Then
                     Temp = 255
                     ''  Print #1 , "Udalyayu klyuchi TM..."
                     For Byte0 = T1 To T2                   ' Zatiraem nomera klyuchey TM i termostaty          8 bayt na kajdyy klyuch TM
                        Writeeeprom Temp , Byte0            ' Peremennaya, Adres
                     Next
                    If Integ1 <> 0 Then
                     For Adress_rom_termostata = Integ1 To Integ2       ' Zatiraem nomera klyuchey TM i termostaty          8 bayt na kajdyy klyuch TM
                        Writeeeprom Temp , Adress_rom_termostata       ' Peremennaya, Adres
                     Next
                    End If
                    Otvet$ = "prinqto " + Tstr$
                    Goto Opros_sms_end
                  End If


               If Tstr$ = "ns" Then
                  Parametri_komandy$ = Mid(sms$ , 4)
                ''  Print #1 , Parametri_komandy$
                  If Len(parametri_komandy$) >= 10 Then     ' esli imeyutsya vse dannye rejimov raboty

                     Config_device = 0                      ' ochischaem vse nastroyki

                     Tstr$ = Mid(parametri_komandy$ , 1 , 1)       ' Flag "A" = Soobschenie o vklyuchenii ili perezagruzke ustroystva
                     If Tstr$ = "1" Then
                        Config_device.7 = 1
                     End If

                     Tstr$ = Mid(parametri_komandy$ , 2 , 1)       ' Flag "B" = Soobschenie pri upravlenii posredstvom klyucha TM
                     If Tstr$ = "1" Then
                        Config_device.6 = 1
                     End If

                     Tstr$ = Mid(parametri_komandy$ , 3 , 1)       ' Flag "C" = Prisylat' otchety o vypolnenii komand
                     If Tstr$ = "1" Then
                        Config_device.5 = 1
                     End If

                     Tstr$ = Mid(parametri_komandy$ , 4 , 1)       ' Flag "D" = zapret ishodiashih vizovovv
                     If Tstr$ = "1" Then
                        Config_device.4 = 1
                     End If

                     Tstr$ = Mid(parametri_komandy$ , 5 , 1)       ' Flag "E"  = Snyatie/Postanovka zvonkom s nomerov Hozyain 1 i 2,
                     If Tstr$ = "1" Then                    '  bez ustanovki svyazi.
                        Config_device.3 = 1
                     End If

                     Tstr$ = Mid(parametri_komandy$ , 6 , 1)       ' Flag "F" =  0 - tol'ko dozvon, 1 - SMS-uvedomlenie + dozvon
                     If Tstr$ = "1" Then
                        Config_device.2 = 1
                     End If

                     Tstr$ = Mid(parametri_komandy$ , 7 , 1)       ' Flag "G"
                     If Tstr$ = "1" Then
                        Config_device.1 = 1
                     End If

                     Tstr$ = Mid(parametri_komandy$ , 8 , 1)       ' Flag "H"
                     If Tstr$ = "1" Then
                        Config_device.0 = 1
                     End If

                   ''  Print #1 , "new=" ; Bin(config_device)
                     Writeeeprom Config_device , 255        ' Sohranim nastroyki raboty ustroystva

                     Tstr$ = Mid(parametri_komandy$ , 9 , 1) : Temp = Val(tstr$)       '  Flag "K" - dlitel'nost' raboty sireny
                   ''  Print #1 , "Dlina trevogi " ; Temp
                     Writeeeprom Temp , 220                 ' Sirena_timer
                     Tstr$ = Mid(parametri_komandy$ , 10 , 1) : Zaderjka_postanovki = Val(tstr$) * 2
                      Writeeeprom Zaderjka_postanovki , 835
                     Otvet$ = "prinqto ns"
                     Tstr$ = ""
                     Nomer_vhoda = 15
                  End If                                    '  If Len(parametri_komandy$) = 8
                  Goto Opros_sms_end
               End If

            Tstr$ = Mid(sms$ , 1 , 1)

          If Tstr$ = "o" Then
               Parametri_komandy$ = Mid(sms$ , 2 , 1 )
               Otvet$ = Number$
               If Parametri_komandy$ = "" Then
                  If Ohrana = 1 Then Parametri_komandy$ = "0"
                  If Ohrana = 0 Then Parametri_komandy$ = "1"
               End If
               If Parametri_komandy$ = "1" Then             ' Komanda: g1
                       Ohrana = 3
                   ''  Print #1 , "Ohrana Vklyuchena!"
                  End If
                  If Parametri_komandy$ = "0" Then          ' Komanda: g0
                     Ohrana = 2
                   ''  Print #1 , "Ohrana Otklyuchena!"
                     Pauza_do_sireny = 0 : Sirena_timer = 0
                     Rejim_vihoda = 1
                      Gosub Perebor_na_vikl                 ' otklyuchim sirenu, esli pischala
                  End If
               Return
           End If

           If Tstr$ = "b" Then
               Balans:

               Parametri_komandy$ = Mid(sms$ , 3 )          ' Komanda: 1234 balans #100#
               Gosub Wait_5sec
               Print "AT+CUSD=1,{034}" ; Parametri_komandy$ ; "{034}"       ' ; Chr(34) ; Parametri_komandy$ ; Chr(34) ; ",15"
               Gosub Flushbuf                               'ochistka bufera
               'Print #1 , "temp24 = " ; Temp24 ; " USD = " ; "AT+CUSD=1,{034}*102#{034}"
                  Gosub Wait_5sec                           'Otvet$ = Mid(sms$ , 11 )                     ' uberem nachalo s ukazaniem komandy, ostal'noe - v otvet
               Return
            End If

         If Tstr$ = "v" Then
                  Parametri_komandy$ = Mid(sms$ , 2 , 1)
                  Tstr$ = Mid(sms$ , 4 , 1)
                  Byte0 = Val(tstr$)
                  Counter_c = Val(parametri_komandy$)

                      If Byte0 = 2 Then                     '
                         Temp2 = 123                        '
                         Byte0 = 1                          '
                      End If
                      If Byte0 = 1 Then                     ' Vklyuchit' rele
                         Nomer_vihoda = Counter_c           ' vybor po nomeru rele
                         Rejim_vihoda = 255
                         Gosub Vkl_vihoda
                         Otvet$ = "prinqto"
                      End If

                      If Temp2 = 123 Then                   ' Chtoby sdelat' impul's na nujnom vyhode
                         Gosub Wait_5sec                    ' pauza            vnachale vklyuchit', sdelat' pauzu
                         Byte0 = 0                          '                                                 i otklyuchit' vyhod
                      End If

                      If Byte0 = 0 Then                     ' Otklyuchit' rele
                         Nomer_vihoda = Counter_c           ' vybor po nomeru rele
                         Rejim_vihoda = 255
                         Gosub Vikl_vihoda
                         Otvet$ = "prinqto"
                       End If
                  If Temp2 = 123 Then
                      Otvet$ = "prinqto"
                      Temp2 = 0
              End If                                        '   If Komanda$ = "v "
          Goto Opros_sms_end
           End If


           If Tstr$ = "t" Then
              Tstr$ = Mid(sms$ , 3 , 1)
              If Tstr$ = "s" Then                           ' nastroyka sms dlya termostatov
                 Tstr$ = Mid(sms$ , 2 , 1)
                 Counter_c = Val(tstr$)
                 Parametri_komandy$ = Mid(sms$ , 5 , 6)
                 If Len(parametri_komandy$) >= 6 Then       ' Esli 7 simvolov = pravil'no:  "-01 +35"
                   ''  Print #1 , "SMS T" ; Counter_c ; "  " ; Parametri_komandy$

                     ' Vydelyaem temperaturu min (2 znaka)
                     Simvol$ = Mid(parametri_komandy$ , 1 , 1 )       ' Vydelyaem znak temperatury
                     Tstr$ = Mid(parametri_komandy$ , 2 , 2 ) : T_znak_temp = Val(tstr$)
                     If Simvol$ = "-" Then T_znak_temp = -1 * T_znak_temp       ' esli temperatura - otricatel'naya, to menyaem znak
                   ''  Print #1 , "min=" ; T_znak_temp

                     ' Vydelyaem temperaturu max (2 znaka)
                     Simvol$ = Mid(parametri_komandy$ , 4 , 1 )       ' Vydelyaem znak temperatury
                     Tstr$ = Mid(parametri_komandy$ , 5 , 2 ) : T_znak_temp2 = Val(tstr$)
                     If Simvol$ = "-" Then T_znak_temp2 = -1 * T_znak_temp2       ' esli temperatura - otricatel'naya, to menyaem znak
                   ''  Print #1 , "max=" ; T_znak_temp2
                     T_min_sms_a(counter_c) = T_znak_temp
                     T_max_sms_a(counter_c) = T_znak_temp2
                     T1 = Counter_c * 4
                     T1 = 140 + T1
                     T2 = T1 + 2
                     Writeeeprom T_min_sms_a(counter_c) , T1       ' min 1
                     Writeeeprom T_max_sms_a(counter_c) , T2       ' max 1
                     Otvet$ = "prinqto ts"
                     Nomer_vhoda = 12
                     Tstr$ = ""
                  End If
                  Else
              If Tstr$ = "v" Then                           ' nastroyka vyhoda dlya termostata
               Parametri_komandy$ = Mid(sms$ , 6 , 6)
                  If Len(parametri_komandy$) = 6 Then       ' Esli 7 simvolov = pravil'no:  "-12 +02"
                      ''  Print #1 , Parametri_komandy$

                        ' Vydelyaem temperaturu vklyucheniya (2 znaka)
                        Simvol$ = Left(parametri_komandy$ , 1 )       ' Vydelyaem znak temperatury
                        Tstr$ = Mid(parametri_komandy$ , 2 , 2 ) : T_znak_temp = Val(tstr$)
                        If Simvol$ = "-" Then T_znak_temp = -1 * T_znak_temp       ' esli temperatura - otricatel'naya, to menyaem znak
                      ''  Print #1 , "TRele min=" ; T_znak_temp

                        ' Vydelyaem temperaturu otklyucheniya (2 znaka)
                        Simvol$ = Mid(parametri_komandy$ , 5 , 1 )       ' Vydelyaem znak temperatury
                        Tstr$ = Right(parametri_komandy$ , 2 ) : T_znak_temp2 = Val(tstr$)       ' T max = 2 simvola sprava
                        If Simvol$ = "-" Then T_znak_temp2 = -1 * T_znak_temp2       ' esli temperatura - otricatel'naya, to menyaem znak
                      ''  Print #1 , "TRele max=" ; T_znak_temp2
                         Simvol$ = Mid(sms$ , 2 , 1 )
                         Nomer_termostata = Val(simvol$)
                         If Nomer_termostata > 0 And Nomer_termostata < 6 Then
                             Simvol$ = Mid(sms$ , 4 , 1 )
                             Nomer_termostata = Nomer_termostata * 5
                             Nomer_vihoda = Val(simvol$)
                             If Nomer_vihoda > 4 And Nomer_vihoda < 8 Then Nomer_vihoda = Nomer_vihoda + 10
                             Adress_rom_termostata = 795 + Nomer_termostata
                             Nomer_termostata = Nomer_termostata / 5
                             Writeeeprom Nomer_vihoda , Adress_rom_termostata       ' sohranim nastroyki v EEPROM  96-97
                             Adress_rom_termostata = Adress_rom_termostata + 1
                             Writeeeprom T_znak_temp , Adress_rom_termostata       ' sohranim nastroyki v EEPROM  96-97
                             Adress_rom_termostata = Adress_rom_termostata + 2
                             Writeeeprom T_znak_temp2 , Adress_rom_termostata       ' sohranim nastroyki v EEPROM  96-97
                             Termostat_v_a(nomer_termostata) = Nomer_vihoda
                             Termostat_min_a(nomer_termostata) = T_znak_temp
                             Termostat_max_a(nomer_termostata) = T_znak_temp2
                              Otvet$ = "prinqto tv"
                              Nomer_vhoda = 13
                              Tstr$ = ""
                        End If
                  End If
                 End If                                     '  If Len(parametri_komandy$) = 7
              End If
              Goto Opros_sms_end
           End If

           If Tstr$ = "r" Or Komanda$ = "r"then
              Tstr$ = Mid(sms$ , 2 , 1)
              If Tstr$ = " " Or Tstr$ = "" Or Komanda$ = "r"then
                  Komanda$ = ""
                  Error_line_tm = 0                         ' sbros flaga oshibki linii TM
            If Startbit = 1 Then                            'proggaz
                Nomer_vhoda = 254
                Kotli(1) = "X"
                Kotli(2) = "X"
                Kotli(3) = "X"
                Number$ = ""
                Startbit = 0
                Parametri_komandy$ = ""
                   Gosub Wait_2sec
                   Print #1 , 969999
                   Gosub Wait_2sec
                   Print #1 , 979999
                   Gosub Wait_2sec
                    Print #1 , 989999
                   Komanda$ = Otvet$
                   While Startbit = 0
                      Gosub Service_gas_input
                   Wend
                   Komanda$ = Komanda$ + " kot:" + Kotli(1) + Kotli(2) + Kotli(3)
                   Komanda$ = Komanda$ + " vh:" + Bin(bin_vhodi)
                   Komanda$ = Komanda$ + " vQh:" + Parametri_komandy$
                   Otvet$ = Otvet$ + Komanda$
                   Gosub Sendsms
                   Otvet$ = ""
                   Nomer_vhoda = 0
               End If                                       'proggaz
'               ' izmerim i otpravim temperaturu
               If Ohrana = 0 Then Otvet$ = Otvet$ + " snqt s ohranQ"
               If Ohrana = 1 Then Otvet$ = Otvet$ + " na ohrane"
               If Kolvo_18b20 <> 0 Then                     ' esli est' hot' 1 termometr
                  Gosub 18b20
                  Otvet$ = Otvet$ + " t:" + Str(t_d_a(1))   '   #1
                  If Kolvo_18b20 > 1 Then Otvet$ = Otvet$ + ";" + Str(t_d_a(2))       '   #2
                  If Kolvo_18b20 > 2 Then Otvet$ = Otvet$ + ";" + Str(t_d_a(3))       '   #3
                  If Kolvo_18b20 > 3 Then Otvet$ = Otvet$ + ";" + Str(t_d_a(4))       '   #4
                  If Kolvo_18b20 > 4 Then Otvet$ = Otvet$ + ";" + Str(t_d_a(5))       '   #5
               End If

               If Kolvo_t_mem = 0 Then
                                ' klyuchi TM ne zaregistrirovany
               Else
                  Otvet$ = Otvet$ + " tm=" + Str(kolvo_t_mem)
               End If

               If Len(otvet$) > 35 Then
                  Gosub Sendsms
                  Otvet$ = ""
               End If
               Otvet$ = Otvet$ + " vh:"

               Vhod_adc = Getadc(in_1) : Vhod_adc = Vhod_adc / 4
               If Vhod_adc <= Vhod_min_a(1) Or Vhod_adc >= Vhod_max_a(1) Then
                  Otvet$ = Otvet$ + "+"                     ' esli znacheniya vyshli za predely, to  "+"
               Else
                  Otvet$ = Otvet$ + "-"                     ' esli norma, to "-"
               End If

               Vhod_adc = Getadc(in_2) : Vhod_adc = Vhod_adc / 4
               If Vhod_adc <= Vhod_min_a(2) Or Vhod_adc >= Vhod_max_a(2) Then
                  Otvet$ = Otvet$ + "+"                     ' esli znacheniya vyshli za predely, to  "+"
               Else
                  Otvet$ = Otvet$ + "-"                     ' esli norma, to "-"
               End If

               Vhod_adc = Getadc(in_3) : Vhod_adc = Vhod_adc / 4
               If Vhod_adc <= Vhod_min_a(3) Or Vhod_adc >= Vhod_max_a(3) Then
                  Otvet$ = Otvet$ + "+"                     ' esli znacheniya vyshli za predely, to  "+"
               Else
                  Otvet$ = Otvet$ + "-"                     ' esli norma, to "-"
               End If

               Vhod_adc = Getadc(in_4) : Vhod_adc = Vhod_adc / 4
               If Vhod_adc <= Vhod_min_a(4) Or Vhod_adc >= Vhod_max_a(4) Then
                  Otvet$ = Otvet$ + "+"                     ' esli znacheniya vyshli za predely, to  "+"
               Else
                  Otvet$ = Otvet$ + "-"                     ' esli norma, to "-"
               End If

               Vhod_adc = Getadc(in_5) : Vhod_adc = Vhod_adc / 4
               If Vhod_adc <= Vhod_min_a(5) Or Vhod_adc >= Vhod_max_a(5) Then
                  Otvet$ = Otvet$ + "+"                     ' esli znacheniya vyshli za predely, to  "+"
               Else
                  Otvet$ = Otvet$ + "-"                     ' esli norma, to "-"
               End If

               Otvet$ = Otvet$ + " vQh:"

               If Out_1 = 1 Then
                  Otvet$ = Otvet$ + "1"
               Else
                  Otvet$ = Otvet$ + "0"
               End If

               If Out_2 = 1 Then
                  Otvet$ = Otvet$ + "1"
               Else
                  Otvet$ = Otvet$ + "0"
               End If

               If Out_3 = 1 Then
                  Otvet$ = Otvet$ + "1"
               Else
                  Otvet$ = Otvet$ + "0"
               End If

               If Out_4 = 1 Then
                  Otvet$ = Otvet$ + "1"
               Else
                  Otvet$ = Otvet$ + "0"
               End If


              Adc_data = Getadc(7)
              If Adc_data > 560 Then                        '  Esli pitanie ot 220v
               Otvet$ = Otvet$ + " 220v "
               Else                                         '                Esli akkumulyator razryajen
               Otvet$ = Otvet$ + " akk!"
               'If Adc_data < 520 Then Otvet$ = Otvet$ + "Hu3Kuu 3ap9g"
               End If
              Else
               Tstr$ = Mid(sms$ , 1 , 5)
                     If Tstr$ = "reset" Then
                        Goto Reset_device
                     End If
              End If
           End If
      Opros_sms_end:
      '   If Tstr$ = "" And Nomer_vhoda <> 0 Then
'           ' Print #1 , "OTVET = " ; Otvet$                  'TESTING
'            Time_tm = Time_tm + 1
'            Sms$ = Mid(sms$ , Nomer_vhoda)
'            Nomer_vhoda = 0
'           Tstr$ = Mid(sms$ , 1 , 1)
'           If Tstr$ <> "" Then Goto Test_001
'           If Time_tm > 1 Then Otvet$ = "prinqto komand:" + Str(time_tm)
'         End If

         If Config_device.5 = 1 Then                        'Flag "C"       Esli zadano - vydavat' podtverjdayuschie SMS
              If Otvet$ = "" And Config_device.0 = 1 Then Otvet$ = "owibka:nevernaq komanda"
            '  Print #1 , "OTVET FINISH= " ; Otvet$          'TESTING
              Rejim_vihoda = 2
              If Otvet$ <> "" Then Gosub Sendsms            ' Otpravim soobschenie
            End If

   End If
  Otvet$ = ""
  Gosub Flushbuf
Return




'======================================================================
Nomet_svoj_chuzhij:                                         ' Etot nomer svoy? Admin ili Pol'zovatel'?
   Counter_c = 0                                            ' Flag: esli etot nomer est' v pamyati, to stanet ukazatelem nomera
   Admin = 0                                                ' Flag:  0=Pol'zovatel',  1=Admin

   '  analiz nomera: est' takoy v pamyati ili net. Esli net - ignorirovanie SMS

   For Temp = 1 To 5
      If Number$ = Phone_$_a(temp) Then
        If Phoner(temp) = 9 Or Phoner(temp) = 6 Then Admin = 1       ' Boss_2
        Counter_c = Temp                                    ' nomer dostoveren - est' v nashem spiske
      '  Print #1 , Number$ ; " admin = " ; Admin
     End If
   Next

   If Counter_c <> 0 Then
   ''  Print #1 , "Nomer dostoveren"
   End If

    Return




'======================================================================
Vydelim_nomer_iz_stroki:                                    ' Vydelenie Nomera Iz Stroki V Kovychkah I S "+" vnachale
   Number$ = ""
   For Counter_c = 1 To Len(sms$)                           '  Ot kovychki do kovychki
      Simvol$ = Mid(sms$ , Counter_c , 1)                   ' simvol iz nomera
      If Simvol$ = Chr(34) Then Exit For                    ' kak tol'ko vstretili "kavychki" = vyhod, tak kak nomer zakonchilsya
      If Simvol$ <> Chr(43) Then                            ' esli ne +
         If Simvol$ >= "0" Then
            If Simvol$ =< "9" Then
               Number$ = Number$ + Simvol$
            End If                                          ' Simvol$ < "9"
         End If                                             '  If Simvol$ > "0"
      End If                                                '  if Simvol$ <> Chr(43)
   Next                                                     '  For Counter_c
 ''  Print #1 , "!>  " ; Number$
Return




'======================================================================
18b20:
   T_d_a(1) = 0 : T_d_a(2) = 0 : T_d_a(3) = 0 : T_d_a(4) = 0 : T_d_a(5) = 0

 ''  Print #1 , "Opros T..."

   If Tm_in = 1 Then                                        ' Esli liniya TM ispravna
      1wreset                                               ' Sbros linii 1 Ware
      Error_line_tm = 0
      Start Watchdog : Reset Watchdog

      1wwrite &HCC                                          ' Obraschenie ko vsem datchikam na linii
      1wwrite &H44 : Gosub Waitms_750                       ' Zapusk izmereniya t
      1wwrite &H55                                          ' Komanda ispol'zuetsya dlya raboty s konkretnym nomerov Ds18b20

      ' Opros termometra ¹1

      Counter_c = 1 : Gosub Kopirovka_id                    ' perenosim vo vremennyy bufer nomer DS18b20
      Gosub Chitaem_temperaturu                             ' primem temperaturu
      If Counter_c <> 255 And T_znak_temp <> 85 Then        ' esli T schitana uspeshno
         T_d_a(1) = T_znak_temp
         ''  Print #1 , "t`1 " ; T_d_a(1)                         ' Sohranim schitannoe znachenie v termometr-1
      Else                                                  ' Esli dannye termometra #1  schitany neverno ili na shine net etogo termometra,
         T_d_a(1) = -255                                    ' Problemy s oprosom etogo termometra!
      End If                                                '   If Counter_c <> 255 Then

      If Kolvo_18b20 > 1 Then                               ' Opros termometra  #2
         Counter_c = 2 : Gosub Kopirovka_id                 ' perenosim vo vremennyy bufer nomer DS18b20
         Gosub Chitaem_temperaturu                          ' primem temperaturu
         If Counter_c <> 255 And T_znak_temp <> 85 Then     ' esli T schitana uspeshno
            T_d_a(2) = T_znak_temp
           ''  Print #1 , "t`2 " ; T_d_a(2)   ' Sohranim schitannoe znachenie v termometr-2
         Else                                               ' Esli dannye termometra schitany neverno ili na shine net etogo termometra,
            T_d_a(2) = -255                                 ' Problemy s oprosom etogo termometra!
         End If                                             '   If Counter_c <> 255 Then
      End If                                                '   #2

      If Kolvo_18b20 > 2 Then                               ' Opros termometra  #3
         Counter_c = 3 : Gosub Kopirovka_id                 ' perenosim vo vremennyy bufer nomer DS18b20
         Gosub Chitaem_temperaturu                          ' primem temperaturu
         If Counter_c <> 255 And T_znak_temp <> 85 Then     ' esli T schitana uspeshno
            T_d_a(3) = T_znak_temp
            ''  Print #1 , "t`3 " ; T_d_a(3)                      ' Sohranim schitannoe znachenie v termometr-3
         Else                                               ' Esli dannye termometra schitany neverno ili na shine net etogo termometra,
            T_d_a(3) = -255                                 ' Problemy s oprosom etogo termometra!
         End If                                             '   If Counter_c <> 255 Then
      End If                                                '   #3

      If Kolvo_18b20 > 3 Then                               ' Opros termometra  #4
         Counter_c = 4 : Gosub Kopirovka_id                 ' perenosim vo vremennyy bufer nomer DS18b20
         Gosub Chitaem_temperaturu                          ' primem temperaturu
         If Counter_c <> 255 And T_znak_temp <> 85 Then     ' esli T schitana uspeshno
            T_d_a(4) = T_znak_temp
            ''  Print #1 , "t`4 " ; T_d_a(4)                      ' Sohranim schitannoe znachenie v termometr-4
         Else                                               ' Esli dannye termometra schitany neverno ili na shine net etogo termometra,
            T_d_a(4) = -255                                 ' Problemy s oprosom etogo termometra!
         End If                                             '   If Counter_c <> 255 Then
      End If                                                '   #4

      If Kolvo_18b20 > 4 Then                               ' Opros termometra  #5
         Counter_c = 5 : Gosub Kopirovka_id                 ' perenosim vo vremennyy bufer nomer DS18b20
         Gosub Chitaem_temperaturu                          ' primem temperaturu
         If Counter_c <> 255 And T_znak_temp <> 85 Then     ' esli T schitana uspeshno
            T_d_a(5) = T_znak_temp
            ''  Print #1 , "t`5 " ; T_d_a(2)                      ' Sohranim schitannoe znachenie v termometr-5
         Else                                               ' Esli dannye termometra schitany neverno ili na shine net etogo termometra,
            T_d_a(5) = -255                                 ' Problemy s oprosom etogo termometra!
         End If                                             '   If Counter_c <> 255 Then
      End If                                                '   #5

      Stop Watchdog
   Else
      If Error_line_tm <> 200 Then Incr Error_line_tm

         If Error_line_tm = 100 Then                        ' Esli liniya TM dolgo zamknuta, to peredaem SMS o probleme
            'Otvet$ = "HEuCnPABHOCTb TM"
            'Gosub Sendsms                                         ' Otpravim podtverjdenie vypolneniya komandy
         End If

   End If                                                   '  If Tm_in = 1
Return


Kopirovka_id:
   For Temp = 1 To 8
      Select Case Counter_c
         Case 1 : Temp_massiv_9bait(temp) = Ds18b20_1(temp)
         Case 2 : Temp_massiv_9bait(temp) = Ds18b20_2(temp)
         Case 3 : Temp_massiv_9bait(temp) = Ds18b20_3(temp)
         Case 4 : Temp_massiv_9bait(temp) = Ds18b20_4(temp)
         Case 5 : Temp_massiv_9bait(temp) = Ds18b20_5(temp)
      End Select
   Next
Return



Chitaem_temperaturu:
   Counter_c = 255                                          '  ispol'zuyu kak priznak oshibki (255= oshibka )

   1wverify Temp_massiv_9bait(1)                            ' v massive - adres oprashivaemogo termometra

   If Err = 0 Then
      1wwrite &HBE                                          ' Komanda ispol'zuetsya dlya chteniya Ds18b20

      Temp_massiv_9bait(1) = 1wread(9)                      ' Prinimaem 8 bayt dannyh  vo vremennyy bufer
      If Err = 0 Then                                       ' Esli poluchili otvet datchika

         If Temp_massiv_9bait(9) = Crc8(temp_massiv_9bait(1) , 8) Then       '   Proveryaem CRC poluchennyh dannyh
            T1 = Temp_massiv_9bait(1)                       '  pervyy bayt
            T2 = Temp_massiv_9bait(2)                       '  vtoroy bayt
            If T2 >= 248 Then                               ' Proveryaem na otricatel'nuyu temperaturu. 248 / 11111000
               T1 = &HFF - T1 : T2 = &HFF - T2              ' Esli temperatura otricatel'naya – vychitaem iz &HFF
               Counter_c = 1                                ' temperatura otricatel'naya
            Else
               Counter_c = 2                                ' temperatura polojitel'naya
            End If

            Shift T1 , Right , 4                            ' Sdvigaem pervyy bayt vpravo na 4 bita
            Shift T2 , Left , 4                             ' Sdvigaem vtoroy bayt vlevo na 4 bita
            T_znak_temp = T1 + T2                           ' Formiruem rezul'tam

            If Counter_c = 1 Then T_znak_temp = -1 * T_znak_temp       ' dobavlyaem otricatel'nyy flag

          ''  Print #1 , T_znak_temp

         End If                                             '  If Temp_massiv_9bait(9) = Crc8
      End If                                                '  If Err = 0
   End If                                                   '  If Err = 0 Then

Return





'======================================================================
Test_power:                                                 ' Zamer napryajeniya
   Adc_data = Getadc(7)
 '  Print #1 , "U=" ; Adc_data
   If Adc_data < 540 Then                                   '480 dlya malen'ki 580 dlya 2ki                                ' Esli net 220v i rabotaem na akkumulyatore   U<12.5v
                                                            '600 - 13.8
                                                            '592 - 13.6
                                                            '583 - 13.4
                                                            '555 - 13
     If Sms_net220v <> 2 Then                               '551 - 12.8
      L_no_220 = 1
     End If

      If Sms_net220v = 0 Then                               ' esli Propalo 220v I ni razu ne otpravlyali SMS o propadanii el-va
         Incr Timer_pow
         If Timer_pow >= 1 Then                             ' 220v net bol'she 1 chasa!
            Timer_pow = 0                                   ' Sbros vremeni otsutstviya 220v
            Otvet$ = "otkaz 220v"
            Rejim_vihoda = 2
            Gosub Sendsms                                   ' Otpravim soobschenie "net 220 Vol't"
            Sms_net220v = 1                                 ' chtoby povtorno ne slat' SMS o propadanii el-va
         End If                                             '  If Timer_pow > Minut_120
      End If                                                '  If Sms_net220v = 0
   Elseif Adc_data > 545 Then                               ' Esli est' 220v
      L_no_220 = 0
      Timer_pow = 0
      If Sms_net220v = 1 Or Sms_net220v = 2 Then            'BAZILIO vosstanovlenie pitaniya 220V
            Gosub Wait_5sec
            Otvet$ = "vosstanovlenie 220v"
            Rejim_vihoda = 2
            Gosub Sendsms                                   ' esli otpravlyalos' soobschenie o propaje 220
            Sms_net220v = 0                                 'otpravit' soobschenie o vosstanovlenii
      End If
   End If
Return




'======================================================================
Sendsms:                                                    '  podprogramma otpravki sms
  If Start_ok > 9 Then Start_ok = Start_ok + 1              'dlya reklamy

  For T1 = 1 To 5
        If Len(phone_$_a(t1)) > 3 And Phoner(t1) <> 0 Then  ' esli nastroen nomer Hozyain 2
               Number$ = Phone_$_a(t1)
               If Alarm_in = Phoner(t1) Then Gosub Send_sms_message
               If Phoner(t1) = 8 Or Phoner(t1) = 9 Then Gosub Send_sms_message
               If Phoner(t1) = 6 Then
                If Rejim_vihoda = 40 Or Rejim_vihoda = 88 Then Gosub Send_sms_message
               End If
               If Phoner(t1) = 7 Then
                  Komanda$ = Otvet$
                  If Rejim_vihoda = 20 Then Otvet$ = "snqt s ohranQ"
                  If Rejim_vihoda = 21 Then Otvet$ = "na ohrane"
                  If Rejim_vihoda = 40 Then Otvet$ = "trevoga"       '   If Number$ <> Phone_$_a(2)
                  If Rejim_vihoda = 40 Or Rejim_vihoda = 21 Or Rejim_vihoda = 20 Then Gosub Send_sms_message
                  Otvet$ = Komanda$
               End If
         End If
  Next

   Otvet$ = ""
Return

'Send_sms_message:
 '  Print #1 , " CMC HA HOMEP  " ; Number$ ; " :::::: " ; Otvet$       'FOR TESTING
'Return

Send_sms_message:
   Start Watchdog
'   Print #1 , "Poluchatel': " ; Number$
'   Print #1 , "SMS: " ; Otvet$
   Reset Watchdog
   Gosub Flushbuf
   'Otvet$ = "privet"                                        'ochistka bufera
    Gosub Convert_to_ucs
   Temp = Len(otvetn$) / 2
      Rejim_vihoda = Temp
   Temp = Temp + 13
   Print "AT+CMGS=" ; Str(temp)                             '; Len(otvetn$)                          'Chr(34) ; "+" ; Number$ ; Chr(34) ; ","       ' ; Chr(13);       ' AT+CMGS="+79003611601", #010
   'Print #1 , "AT+CMGS=" ; Str(temp)
   'Gosub Waitms_750
   'Print Otvet$ ; Chr(26)                                   ' Teper' samo soobschenie +  Zavershit'
   Integ1 = 0
   Do
      Integ1 = Integ1 + 1
      'Print #1 , "Wait >"
      Print "AT+CMGS=" ; Str(temp)
      Toggle L_gsm : Gosub Waitms_100                       ' : Reset Watchdog
      If Integ1 < 100 Then Reset Watchdog
      Gosub Getline
      If Integ1 = 100 Then Goto Sendsms_end                 'Sendsms
      If Sms$ = "ERROR" Then                                ' esli oshibka - popischim  6 raz
         '         For Temp = 0 To 5
         '            Sound L_tm , 300 , 400
         '            Gosub Waitms_500
         '         Next
       ''  Print #1 , "SMS ERROR!"
       Toggle L_tm
         Goto Sendsms_end
      End If
Loop Until Sms$ = "> "
                                     'ne vyydet iz cikla poka modul' ne otvetit OK
Gosub Number_modem
 '  Print #1 , "CMC = " ; Otvet$
 '  Print #1 , "HOMEP = " ; Numbern$
   'Print #1 , "0001000B91" ; Numbern$ ; "0008" ; Hex(rejim_vihoda) ; Otvetn$ ; Chr(26)
   Print "0001000B91" ; Numbern$ ; "0008" ; Hex(rejim_vihoda) ; Otvetn$ ; Chr(26)
Sendsms_end:
   For Temp = 0 To 100
      Gosub Getline
      Tstr$ = Mid(sms$ , 1 , 6)                             ' Pauza 3 sek, chtoby operator ne glyuchil, osobenno vajno dlya MTS
      Reset Watchdog
      Toggle L_gsm : Gosub Waitms_500
      Reset Watchdog
      If Tstr$ = "+CMGS:" Then Exit For
   Next
   Gosub Flushbuf                                           'ochistka bufera
Return


'======================================================================

Waitms_50:
   Waitms 50
Return

Waitms_100:
   Waitms 100
Return

Waitms_500:
   Waitms 500
Return


Waitms_750:
   Waitms 750 : Reset Watchdog
Return

Wait_2sec:
For Temp3 = 1 To 4                                          ' 2 sek
      Gosub Waitms_500 : Reset Watchdog
   Next
Return

Wait_5sec:
   For Temp3 = 0 To 9                                       ' 5 sek
      Gosub Waitms_500 : Reset Watchdog
   Next
Return



'======================================================================
Reset_device:
   Stop Watchdog                                            ' ----  Perezapusk ustroystva!!  ----
 ''  Print #1 , " __ Perezapusk ustroystva __"
   Print #1 , 799999
   Pwr_key_gsm = 1                                          ' ne menee 1.5 sek dlya upravleniya pitaniem !!!
   Wait 2
   Config Watchdog = 2048
   Start Watchdog
   Do : Loop                                                ' Zavisnem, chtoby perezapustit' po storojevomu taymeru



   '======================================================================
Wd_isr:
   'Print #1 , "int WDT! " ; Error_line_tm
   'If Err = 1 Then Config 1wire = Tm_in

   'If Error_line_tm > 100 Then Goto Wd_isr_01


   Incr Error_line_tm

   If Error_line_tm = 5 Then                                ' Esli liniya TM dolgo zamknuta, to peredaem SMS o probleme
      Otvet$ = "owibka:neispravnostB"
      Rejim_vihoda = 2
      Gosub Sendsms                                         ' Otpravim podtverjdenie vypolneniya komandy
      Error_line_tm = 200                                   ' chtoby povtorno ne otpravlyat' SMS o sboe v linii TM
      '1wreset
   End If

Wd_isr_01:
   If Error_line_tm = 205 Then
       Otvet$ = "owibka  255"
      Rejim_vihoda = 2
      Gosub Sendsms
      Error_line_tm = 255
      Goto Start_prg
   End If
   Wdtcsr = &B0100_0000

   'If Error_line_tm = 255 Then Gosub Start_prg
Return





'======================================================================
Flushbuf:                                                   ' ochistka bufera
   Gosub Waitms_100
   Do
      B = Inkey()                                           'zabiraem vse iz bufera
   Loop Until B = 0
Return



'======================================================================
Getline:                                                    ' podprogramma dlya razbora soobscheniya modulya
   Sms$ = ""
   Do
      B = Inkey()                                           'berem simvol iz bufera v formate ASCII
      Select Case B
         Case 0 : Exit Do                                   ' esli v bufere net simvola - vyhod!
         Case 13                                            ' vozvrat karetki (Enter)                  'vozvrat karetki (Enter)
         Case 10 : If Sms$ <> "" Then Exit Do               'konec stroki,  vyhodit iz podprogrammy
         Case Else : If B > 31 Then Sms$ = Sms$ + Chr(b)    ' sostavlyaem stroku
      End Select
      If Rejim_vihoda = 55 And Len(sms$) > 53 Then Gosub Sms_upd
      If Rejim_vihoda = 56 And Len(sms$) > 3 Then Gosub Convert_to_eng
   Loop
          If Sms$ = "> " Then
            Modem_s_counter = Modem_s_counter + 1
            If Modem_s_counter = 4 Then Print "0001000B91" ; Chr(26)
          Elseif Sms$ <> "" Then
               Modem_s_counter = 0
          End If
          ' If Sms$ <> "" Then Print #1 , Sms$ ; "modem_s = " ; Modem_s_counter       '   Print #1            'FOR TESTING

Return




'======================================================================
Chtenie_nomerov_iz_eeprom:
   Counter_c = 10                                           ' nachalo pamyati, gde hranyatsya nomera Ds18b20.  8 bayt na nomer 1wire
 ''  Print #1 , "Chitayu v pamyat' iz eeprom nomera 18b20..." ; Counter_c

   For Temp = 1 To 8
      Readeeprom Temp2 , Counter_c                          ' Peremennaya, adres
      Ds18b20_1(temp) = Temp2
      Incr Counter_c
   Next

   For Temp = 1 To 8
      Readeeprom Temp2 , Counter_c
      Ds18b20_2(temp) = Temp2
      Incr Counter_c
   Next

   For Temp = 1 To 8
      Readeeprom Temp2 , Counter_c
      Ds18b20_3(temp) = Temp2
      Incr Counter_c
   Next

   For Temp = 1 To 8
      Readeeprom Temp2 , Counter_c
      Ds18b20_4(temp) = Temp2
      Incr Counter_c
   Next

   For Temp = 1 To 8
      Readeeprom Temp2 , Counter_c
      Ds18b20_5(temp) = Temp2
      Incr Counter_c
   Next


   Counter_c = 50                                           ' nachalo pamyati, gde hranyatsya nomera klyuchey TM.  8 bayt na nomer 1wire
 ''  Print #1 , "Chitayu v pamyat' iz eeprom nomera TouchMemory..." ; Counter_c

 ''  Print #1 , "Cchital iz EEPROM ## klyuchey i termometrov"
Return








'======================================================================
Zhdu_kluch_tm:                                              ' Jdem prikosnovenie klyuchom TochMemori
   'Start Watchdog :
    Reset Watchdog
        ''  Print #1 , "Start watchdog"

     1wreset

        ''  Print #1 , "1wreser"
   Toggle L_tm

   Kolichestvo_1wire = 1wirecount()

   ''  Print #1 , "1wwirecount"
   If Kolichestvo_1wire = 0 Then
      Gosub Waitms_50
    ''  Print #1 , "Zhdu_1wire"
      Goto Zhdu_kluch_tm
   End If                                                   ' Esli net ustroystv na linii TM

   If T2 = Kolichestvo_1wire Then
      Goto Zhdu_kluch_tm                                    ' oprashivat' kogda izmenyaetsya kol-vo ustroystv na linii
   End If


   '  Print #1 , "Kol-vo 1wire: " ; Kolichestvo_1wire
   '  Print #1 , "Kol-vo T2: " ; T2
   For T1 = 1 To Kolichestvo_1wire
      If T1 = 1 Then                                        ' esli pervyy raz chitaem,
         Temp_massiv_9bait(1) = 1wsearchfirst()             ' chitaem ID semeystva + 6 bayt nomera + CRC8 vo vremennyy massiv
               '  Print #1 , "Proverka 1"
      Else                                                  ' Inache
         Temp_massiv_9bait(1) = 1wsearchnext()              '  chitaem nomer sleduyuschego ustroystva 1wire vo vremennyy massiv
               '  Print #1 , "Proverka 2"
      End If

      Temp2 = Crc8(temp_massiv_9bait(1) , 8)                ' Proverka CRC8
      If Temp2 <> 0 Then
         '  Print #1 , "Owu6ka CRC"
         Goto Zhdu_kluch_tm                                 ' Esli oshibochno - vyhod iz analiza
      End If


      T2 = Kolichestvo_1wire
      '  Print #1 , "Temp_massiv_9bait = " ; Temp_massiv_9bait(1)
      If Temp_massiv_9bait(1) = 1 Or Temp_massiv_9bait(1) = 137 Then       ' ID semeystva  1= TouchMemory
         L_tm = 1 : Gosub Waitms_750 : L_tm = 0
         '  Print #1 , "TM #" ;
         For Temp2 = 7 To 1 Step -1                         ' Ne otobrajaem CRC8, no pokazyvaem ID semeystva
            '  Print #1 , Hex(temp_massiv_9bait(temp2));       ' otobrazim ROM-nomer klyucha
         Next
         '  Print #1 , "Podoshel!!"
         Return                                             ' Vyhod
      End If                                                '  cikl   TouchMemory
   Next

   Goto Zhdu_kluch_tm





   '======================================================================
Proverka_sovpadeniya_ds18b20:
   Temp_massiv_9bait(9) = 0                                 ' tut hranim flag - nomer zapisi klyucha v EEPROM (esli takoy est')
   For Temp = 1 To 5
   Integ1 = Temp * 8
   Integ1 = Integ1 + 2
    Gosub Poisk_1wire                                       ' est' li takoy termometr v pamyati?
    If B = 0 Then
       Temp_massiv_9bait(9) = Temp
       Goto Proverka_sovpadeniya_ds18b20_end                ' Vyhodim, kogda sovpal nomer
    End If
   Next
Proverka_sovpadeniya_ds18b20_end:
 ''  Print #1 , "DS18b20 #" ; Temp_massiv_9bait(9)
Return                                                      ' Vyhod



'======================================================================
Proverka_sovpadeniya_tm:
   'Print #1 , "Proverka sovpadeniia TM"
   Temp_massiv_9bait(9) = 0                                 ' tut hranim flag - nomer zapisi klyucha v EEPROM (esli takoy est')
   Poslednij_tm = 0
  For Integ1 = 50 To 82 Step 8
      'Print #1 , "Proverka adresa = " ; Integ1
      Gosub Poisk_1wire                                     ' est' li takoy TM v pamyati?
      Incr Temp_massiv_9bait(9)
      If B = 0 Then
        Poslednij_tm = Temp_massiv_9bait(9)
      Goto Proverka_sovpadeniya_tm_end
      End If
  Next
  For Integ1 = 944 To 1016 Step 8
      'Print #1 , "Proverka adresa = " ; Integ1
      Gosub Poisk_1wire                                     ' est' li takoy TM v pamyati?
      Incr Temp_massiv_9bait(9)
      If B = 0 Then
        Poslednij_tm = Temp_massiv_9bait(9)
      Goto Proverka_sovpadeniya_tm_end
      End If
  Next

Proverka_sovpadeniya_tm_end:
 ''  Print #1 , "Klyuch TM #" ; Temp_massiv_9bait(9)
Return                                                      ' Vyhod


'======================================================================
Poisk_1wire:                                                ' est' li ustroystvo 1wire s takim nomerov v pamyati?

   Counter_c = 1                                            ' schetchik-ukazatel'
   B = 0                                                    ' Flag,
   Integ2 = Integ1 + 7                                      ' 8 yacheek pamyati na ustroystvo

   For Adress_rom_termostata = Integ1 To Integ2             ' Adres ustroystvo
      Readeeprom Temp2 , Adress_rom_termostata              ' Peremennaya, adres
      If Temp2 <> Temp_massiv_9bait(counter_c) Then B = 1   ' Esli ne znaem takoe ustroystvo
      Incr Counter_c
   Next

Return



'======================================================================
Sohranim_1wire_v_eeprom:                                    ' Sohranim nomer ustroystva 1 wire v EEPROM
   Integ2 = Integ1 + 7                                      ' 8 bayt nomera
   Counter_c = 1                                            ' dannye # (pervyy = ID semeystva, posledniy = CRC8)
   For Adress_rom_termostata = Integ1 To Integ2             ' Adres v EEPROM
      Temp2 = Temp_massiv_9bait(counter_c)
      Writeeeprom Temp2 , Adress_rom_termostata             ' Peremennaya, adres
      Incr Counter_c
   Next

   L_tm = 1 : L_gsm = 1 : Gosub Waitms_750 : L_tm = 0 : L_gsm = 0
Return



'======================================================================



' esli v pamyati est' klyuchi, to nado kosnut'sya klyuchom "Hozyaina" i togda v rejim "NASTROYKA"
' esli net klyuchey, to v rejim "NASTROYKA"
' Nastroyka: klyuchi vvodyatsya pervymi, zatem termometry
' Dlya vyhoda iz rejima nastroyki sleduet vyklyuchit' pitanie.
' V rejime raboty pri kasanii rabochim klyuchom svetodiod  L_tm =1  zagoraetsya na 1 sekundu.

'======================================================================
Install_mode:                                               ' Rejim Nastroyki
   ' Vvod klyuchey iButton, termometrov DS18b20

   L_tm = 1 : Wait 3

   While Tm_in = 0
      Toggle L_tm : Gosub Waitms_50 : Reset Watchdog
   Wend                                                     ' jdem otpuskaniya linii.
   ' Eto uvelichivaet nadejnost': esli bylo KZ v linii TM i propalo pitanie. Chtoby ne steret' nastroyki
   L_tm = 0

   Wait 3
   L_tm = 1

 ''  Print #1 , ""
 ''  Print #1 , "Vvod 1Wire"
   Admin = 1                                                ' Flag rejima vvoda nastroek


   If Tm_in = 0 Then                                        ' esli liniya TM zakorochena, to
      Counter_c = 0

      While Tm_in = 0                                       ' Schitaem vremya uderjaniya linii zamknutoy
         Incr Counter_c : Gosub Waitms_50 : Reset Watchdog
         If Counter_c > 60 Then Toggle L_tm
      Wend
      ' Doljny otpustit'!
      ' Eto uvelichivaet nadejnost': esli bylo KZ v linii TM i propalo pitanie. Chtoby ne steret' nastroyki
      L_tm = 0

      If Counter_c > 60 Then                                ' esli zakorochena bolee 3 sekund, to
       ''  Print #1 , "Stirayu parol' i vse nomera 1wire..."
         For Temp = 10 To 89                                ' stiraem EEPROM, gde hranyatsya nomera ustroystv 1wire
            Temp2 = 255 : Writeeeprom Temp2 , Temp          ' stiraem nomera termometrov i "tabletok"
         Next
         For Integ1 = 944 To 1023                           ' stiraem EEPROM, gde hranyatsya nomera ustroystv 1wire
            Temp2 = 255 : Writeeeprom Temp2 , Integ1        ' stiraem nomera termometrov i "tabletok"
         Next
         Kolvo_t_mem = 0 : Writeeeprom Kolvo_t_mem , 2      ' kol-vo zaregistrirovannyh klyuchey TM
         Kolvo_18b20 = 0 : Writeeeprom Kolvo_18b20 , 1
          L_no_220 = 1
          Gosub Waitms_50
          L_no_220 = 0
          L_gsm = 1
          Gosub Waitms_50
          L_gsm = 0
          L_guard = 1
          Gosub Waitms_50
          L_guard = 0

         ' Adr_18b20 = 10                ' 8 bayt * 5 klyuchey =  10...49
         ' Adr_t_mem = 50              ' 8 bayt * 5 klyuchey =  50...89
       ''  Print #1 , "Udalenie nomerov 1wire zaversheno!"
      End If
   End If



   If Kolvo_t_mem <> 0 Then
      Temp3 = 0                                             ' Schetchik popytok vvoda klyucha Hozyaina
      Do                                                    ' Esli uje est' klyuchi, to jdem klyuch Hozyaina, i tol'ko posle etogo Vvod novyh klyuchey
      Vvod_ustroistv_1wire_a:
         If Temp3 < 6 Then                                  '  Schetchik popytok vvoda klyucha Hozyaina
          '  Print #1 , "Gdem TM hoziaina Popinka = " ; Temp3
            Incr Temp3                                      ' Uvelichim schetchik popytok
            'Stop Watchdog
            Gosub Zhdu_kluch_tm                             ' Chitaem klyuch TM
          ''  Print #1 , "Proverka tm na4alas"
            Gosub Proverka_sovpadeniya_tm                   ' Sravnim, s kakim iz imeyuschihsya TM sovpadaet vvedennyy TM
           ''  Print #1 , "Proverka tm proshla"
            If Poslednij_tm <> 1 Then                       '  esli =1, prikosnulis' klyuchom Hozyaina
             ''  Print #1 , " Nevernyy klyuch!"
               Goto Vvod_ustroistv_1wire_a                  ' Esche popytka
              ''  Print #1 , "Vvod_a"
            Else
               L_guard = 1
               Goto Vvod_ustroistv_1wire_b                  ' v rejim vvoda novyh 1wire
             ''  Print #1 , "Vvod_b"
            End If

         End If                                             '   If Data_1 < 4 Then
       ''  Print #1 , "Previsheno kol-vo popitok"
         L_tm = 1
   Loop                                                     ' Zavisaem do otklyucheniya pitaniya
   Else
     L_guard = 1
     Goto Vvod_ustroistv_1wire_b
   End If                                                   '  If Kolvo_t_mem <> 0



Vvod_ustroistv_1wire_b:                                     ' prinyat' # ustroystva na linii 1wire
   Reset Watchdog

   1wreset                                                  'reset the device
   Toggle L_tm : Gosub Waitms_50

   Kolichestvo_1wire = 1wirecount()
   If Kolichestvo_1wire = 0 Then                            ' Esli net ustroystv na linii TM
      Gosub Waitms_50
      Goto Vvod_ustroistv_1wire_b
   End If

 ''  Print #1 , "Kol-vo 1wire: " ; Kolichestvo_1wire
   Gosub Waitms_50

   For T1 = 1 To Kolichestvo_1wire
      If T1 = 1 Then                                        ' esli pervyy raz chitaem,
         Temp_massiv_9bait(1) = 1wsearchfirst()             ' chitaem ID semeystva + 6 bayt nomera + CRC8 vo vremennyy massiv
       '  Print #1 , "1::::     " ; Temp_massiv_9bait(1)
      Else                                                  ' Inache
         Temp_massiv_9bait(1) = 1wsearchnext()              '  chitaem nomer sleduyuschego ustroystva 1wire vo vremennyy massiv
       '  Print #1 , "2::::     " ; Temp_massiv_9bait(1)
      End If

      If Crc8(temp_massiv_9bait(1) , 8) = 0 Then            ' Net oshibki v CRC8
         ' prinyali nomer ustroystva, proverim: eto TM ili DS18b20?
         ' zatem proverim: est' li takoy nomer v pamyati?
         ' esli net - zapomnit' v sootvetstvuyuschih yacheykah (TM i ds18b20 pishutsya v raznye mesta)

         If Temp_massiv_9bait(1) = 1 Or Temp_massiv_9bait(1) = 137 Then       ' ID semeystva  1= TouchMemory
            Gosub Proverka_sovpadeniya_tm                   ' Sravnim, s kakim iz imeyuschihsya TM sovpadaet vvedennyy TM
            If Poslednij_tm = 0 Then                        ' dannyy klyuch TM - novyy
             ''  Print #1 , "Schitali klyuch TM #" ;
               For Temp2 = 7 To 1 Step -1
                '  Print #1 , Hex(temp_massiv_9bait(temp2)); ' otobrazim ROM-nomer klyucha
               Next
             '  Print #1 , ""
               Incr Kolvo_t_mem
               If Kolvo_t_mem < 16 Then                     ' tol'ko 5 klyuchey mojet byt'
                ''  Print #1 , "Sohranim pod nomerom " ; Kolvo_t_mem
                  Select Case Kolvo_t_mem
                     Case 1 : Integ1 = 50                   ' 50 Nachalo pamyati dlya hraneniya 1 nomera klyucha TM
                     Case 2 : Integ1 = 58                   ' 58 Nachalo pamyati dlya hraneniya 2 nomera klyucha TM
                     Case 3 : Integ1 = 66                   ' 66 Nachalo pamyati dlya hraneniya 3 nomera klyucha TM
                     Case 4 : Integ1 = 74                   ' 74 Nachalo pamyati dlya hraneniya 4 nomera klyucha TM
                     Case 5 : Integ1 = 82                   ' 82 Nachalo pamyati dlya hraneniya 5 nomera klyucha TM
                     Case 6 : Integ1 = 944
                     Case 7 : Integ1 = 952
                     Case 8 : Integ1 = 960
                     Case 9 : Integ1 = 968
                     Case 10 : Integ1 = 976
                     Case 11 : Integ1 = 984
                     Case 12 : Integ1 = 992
                     Case 13 : Integ1 = 1000
                     Case 14 : Integ1 = 1008
                     Case 15 : Integ1 = 1016
                  End Select
                  Gosub Sohranim_1wire_v_eeprom:            ' Sohranim nomer ustroystva 1 wire v EEPROM
                  Writeeeprom Kolvo_t_mem , 2               ' kol-vo klyuchey
                ''  Print #1 , "Sohranili novyy klyuch TM v EEPROM"
               End If                                       '    If Kolvo_t_mem < 6
            Else                                            '    If Temp_massiv_9bait(9) = 0
               L_no_220 = 1 : Gosub Waitms_750 : L_no_220 = 0
            End If                                          '    If Temp_massiv_9bait(9) = 0
         End If                                             '    If Temp_massiv_9bait(1) = 1


         If Temp_massiv_9bait(1) = 40 Then                  ' ID semeystva  28h = DS18b20
            Gosub Proverka_sovpadeniya_ds18b20              ' Sravnim, s kakim iz imeyuschihsya # sovpadaet
            If Temp_massiv_9bait(9) = 0 Then                ' dannyy klyuch TM - novyy
             ''  Print #1 , "Schitali DS18b20 #" ;
               For Temp2 = 7 To 1 Step -1
                ''  Print #1 , Hex(temp_massiv_9bait(temp2)); ' otobrazim ROM-nomer termometra
               Next
             ''  Print #1 , ""
               Incr Kolvo_18b20
               If Kolvo_18b20 < 6 Then                      ' tol'ko 3 termometra mojet byt'
                ''  Print #1 , "Sohranim pod nomerom " ; Kolvo_18b20
                  Select Case Kolvo_18b20
                     Case 1 : Integ1 = 10                   ' 10 Nachalo pamyati dlya hraneniya nomera termometra
                     Case 2 : Integ1 = 18                   ' 18 Nachalo pamyati dlya hraneniya nomera termometra
                     Case 3 : Integ1 = 26                   ' 26 Nachalo pamyati dlya hraneniya nomera termometra
                     Case 4 : Integ1 = 34                   ' 34 Nachalo pamyati dlya hraneniya nomera termometra
                     Case 5 : Integ1 = 42                   ' 42 Nachalo pamyati dlya hraneniya nomera termometra
                  End Select
                  Gosub Sohranim_1wire_v_eeprom:            ' Sohranim nomer ustroystva 1 wire v EEPROM
                  Writeeeprom Kolvo_18b20 , 1               ' kol-vo termodatchikov
                ''  Print #1 , "Sohranili novyy termometr v EEPROM" :     ''  Print #1 , ""
               End If                                       '    If Kolvo_18b20 < 6
            End If                                          '    If Temp_massiv_9bait(9) = 0
         End If                                             '    If Temp_massiv_9bait(1) = 40

      End If                                                '   If Crc8(Temp_massiv_9bait(1) , 8) = 0

   Next

   Goto Vvod_ustroistv_1wire_b                              ' I tak do otklyucheniya pitaniya ...







   '======================================================================
Analiz_pokazanij_termometrov:                               ' analiz pokazaniy termometrov
   'Termostat_vne_predela = 0                                ' flag vyhoda temperatury za predely, dlya lyubogo datchika
   Otvet$ = ""
   Print #1 , "analyz start";
   ' Opros termometra ¹1
For Nom_ter = 1 To 5
  'Print #1 , "T" ; Nom_ter ; "=" ; T_d_a(nom_ter) ; " min=" ; Termostat_min_a(nom_ter) ; " max=" ; Termostat_max_a(nom_ter) ;
If Kolvo_18b20 >= Nom_ter Then                              ' Opros termometra  #1
      Rejim_vihoda = 3
      Nomer_vihoda = Termostat_v_a(nom_ter)
 If T_d_a(nom_ter) <> -255 Then
         'If T_status(nom_ter) = 0 Then
'            Otvet$ = "vosstanovlenie termodatCika:" + Str(nom_ter) + "(" + Str(t_d_a(nom_ter)) + ")"
'         End If
         T_status(nom_ter) = 10
    'Print #1 , "Analyz " ; Nom_ter ; "=" ; T_d ; " min=" ; T_min_sms ; " max=" ; T_max_sms       ' esli T schitana uspeshno
          If T_min_sms_a(nom_ter) > -99 And T_max_sms_a(nom_ter) < 99 Then
            If T_d_a(nom_ter) < T_min_sms_a(nom_ter) Then   ' Temperatura nije poroga
               If Sms_t_otpravil_a(nom_ter) = 0 Then
                  Sms_t_otpravil_a(nom_ter) = 1             ' Ustanovim  flag "uje znaem pro etu problemu"
                  Otvet$ = Otvet$ + "t" + Str(nom_ter) + "(" + Str(t_d_a(nom_ter)) + ") <" + Str(t_min_sms_a(nom_ter))
               End If                                       ' T v predelah normy - sbros flaga oshibki
            End If

            If T_d_a(nom_ter) > T_max_sms_a(nom_ter) Then   ' Temperatura vyshe poroga
               If Sms_t_otpravil_a(nom_ter) = 0 Then
                  Sms_t_otpravil_a(nom_ter) = 1
               ''  Print #1 , "BOLSHE" ;                        ' Ustanovim  flag "uje znaem pro etu problemu"
                  Otvet$ = Otvet$ + "t" + Str(nom_ter) + "(" + Str(t_d_a(nom_ter)) + ") >" + Str(t_max_sms_a(nom_ter))
               End If                                       ' T v predelah normy - sbros flaga oshibki
            End If
          End If
      If Termostat_min_a(nom_ter) > -99 And Termostat_max_a(nom_ter) < 99 Then
       ' Print #1 , "T" ; Nom_ter ; "=" ; T_d_a(nom_ter) ; " min=" ; Termostat_min_a(nom_ter) ; " max=" ; Termostat_max_a(nom_ter) ;
         If Termostat_min_a(nom_ter) > Termostat_max_a(nom_ter) Then
            If T_d_a(nom_ter) > Termostat_min_a(nom_ter) And T_vih_a(nom_ter) <> 1 Then       ' esli tekuschaya T nije min - vklyuchit' vyhod
                T_vih_a(nom_ter) = 1
                Gosub Vkl_vihoda
             End If                                         '   if T_d_a(1) <= T_znak_temp

            If T_d_a(nom_ter) < Termostat_max_a(nom_ter) And T_vih_a(nom_ter) <> 0 Then       ' esli tekuschaya T vyshe max - otklyuchit' vyhod
                T_vih_a(nom_ter) = 0
                Gosub Vikl_vihoda
             End If
         Else
            If T_d_a(nom_ter) < Termostat_min_a(nom_ter) And T_vih_a(nom_ter) <> 1 Then       ' esli tekuschaya T nije min - vklyuchit' vyhod
                T_vih_a(nom_ter) = 1
                Gosub Vkl_vihoda
             End If                                         '   if T_d_a(1) <= T_znak_temp

            If T_d_a(nom_ter) > Termostat_max_a(nom_ter) And T_vih_a(nom_ter) <> 0 Then       ' esli tekuschaya T vyshe max - otklyuchit' vyhod
                T_vih_a(nom_ter) = 0
                Gosub Vikl_vihoda
             End If
         End If
      Else
             Gosub Vikl_vihoda
      End If
   Else
      'If T_status(nom_ter) = 1 Then
'         T_status(nom_ter) = 0
'         Otvet$ = "otkaz termodatCika:" + Str(nom_ter)
'      Else
'         T_status(nom_ter) = T_status(nom_ter) - 1          '   If Counter_c <> 255 Then
'      End If
   End If                                                   '   If Counter_c <> 255 Then
     If T_d_a(nom_ter) > T_min_sms_a(nom_ter) And T_d_a(nom_ter) < T_max_sms_a(nom_ter) And Sms_t_otpravil_a(nom_ter) = 1 Then
          Sms_t_otpravil_a(nom_ter) = 0
          Otvet$ = Otvet$ + "t" + Str(nom_ter) + "(" + Str(t_d_a(nom_ter)) + ") v norme"
     End If
   End If                                                   'If Kolvo_18b20 >= Nom_ter Then
  Next
   Rejim_vihoda = 3
   If Otvet$ <> "" Then Gosub Sendsms                       'DEBUG                      ' Otpravim soobschenie
Return

'======================================================================
Opros_vhodov:                                               ' Opros vhodov (shleyfov)

For Nomer_vhoda = 1 To 4
    Select Case Nomer_vhoda
       Case 1 : In_0 = In_1
       Case 2 : In_0 = In_2
       Case 3 : In_0 = In_3
       Case 4 : In_0 = In_4
       Case 5 : In_0 = In_5
    End Select
    Vhod_m = Vhod_m_a(nomer_vhoda)
    Vhod_otpravil = Vhod_otpravil_a(nomer_vhoda)
    Vhod_w = Vhod_w_a(nomer_vhoda)
    Vhod_d = Vhod_d_a(nomer_vhoda)
    Vhod_min = Vhod_min_a(nomer_vhoda)
    Vhod_max = Vhod_max_a(nomer_vhoda)
    Nomer_vihoda = Vhod_v_a(nomer_vhoda)
    Rejim_vihoda = 3
    Vhod_w_p = Vhod_w_p_a(nomer_vhoda)
    Pauza_do_sireny_0 = Pauza_do_sireny_v(nomer_vhoda)
    Gosub Analyz_vhodov
    Vhod_otpravil_a(nomer_vhoda) = Vhod_otpravil
    Vhod_w_a(nomer_vhoda) = Vhod_w
Next

   If Vhod_m_a(5) <> 4 Then
    Vhod_m = Vhod_m_a(5)
    Nomer_vhoda = 5
    In_0 = In_5
    Vhod_otpravil = Vhod_otpravil_a(5)
    Vhod_w = Vhod_w_a(5)
    Vhod_d = Vhod_d_a(5)
    Vhod_min = Vhod_min_a(5)
    Vhod_max = Vhod_max_a(5)
    Nomer_vihoda = Vhod_v_a(5)
    Rejim_vihoda = 3
    Vhod_w_p = Vhod_w_p_a(5)
    Pauza_do_sireny_0 = Pauza_do_sireny_v(5)
    Gosub Analyz_vhodov
    Vhod_otpravil_a(5) = Vhod_otpravil
    Vhod_w_a(5) = Vhod_w
  Else
    Vhod_adc = Getadc(in_5) : Vhod_adc = Vhod_adc / 4
     If Vhod_adc <= Vhod_min_a(5) Or Vhod_adc >= Vhod_max_a(5) Then
        If Ohrana = 0 And Zaderjka_postanovki_guard = 0 Then
          If Zaderjka_postanovki > 0 Then
              Zaderjka_postanovki_guard = Zaderjka_postanovki
           Else
             Ohrana = 3
         End If
        End If
     Else
        If Zaderjka_postanovki_guard > 0 Then
            Zaderjka_postanovki_guard = 0
            Ohrana = 0
            L_guard = 0
        End If

        If Ohrana = 1 Then
         Ohrana = 2                                         '    If Vhod_otpravil_a(5) = 0 Then                       ' shleyf vostanovlen
         Nomer_vihoda = Vhod_v_a(5)
         Rejim_vihoda = 3
         Gosub Vikl_vihoda
        End If
     End If
  End If



 ''  Print #1 , ""

Return




'##############################################################################################
'##############################################################################################
Init_modem:                                                 ' Inicializaciya modema,  chtenie nomerov s SIM karty
 ''  Print #1 , "Inicializaciya modema..."

   Pwr_key_gsm = 1                                          ' pitanie modema
   For Temp = 0 To 59                                       ' 3 sek
      Toggle L_gsm
      Gosub Waitms_50
   Next
   Pwr_key_gsm = 0                                          ' pitanie modema


   For Temp = 0 To 129                                      ' jdem 13 sek poka modem zaregistriruetsya....
      Toggle L_gsm                                          ' Sv.diod GSM-svyaz'
      Gosub Waitms_100
   Next


   Print "AT" : Gosub Flushbuf                              'ochistka bufera Vnutri est' waitms 100
   Print "ATE0" : Gosub Flushbuf                            'ochistka bufera Vnutri est' waitms 100

   ' Print "AT+GSV" : Gosub Waitms_750          ' Zapros modeli i versii proshivki    Vydaet 3 stroki
   '' SIMCOM_Ltd
   '' SIMCOM_SIM900D
   '' Revision:1137B09SIM900D64_ST_DTMF_JD_MMS
   '   Gosub Getline' ': Print #1 , Sms$                          ' otvet modema
   '   Gosub Getline' ': Print #1 , Sms$                          ' otvet modema
   '   Gosub Getline' ': Print #1 , Sms$                          ' otvet modema
   '   Gosub Getline' ': Print #1 , Sms$                          ' otvet modema

   Error_gsm_modem = 0                                      '  esli net svyazi s operatorom - perezagruzit' modem
   Do
      Incr Error_gsm_modem
      If Error_gsm_modem >= 9 Then                          '9
         L_gsm = 0                                          ' net svyazi s bazoy
         '         For Temp = 0 To 1                                                           ' 2 signala - net svyazi s bazoy
         '            Sound L_tm , 500 , 500 : Gosub Waitms_500
         '         Next
         Goto Init_modem                                    ' - perezagruzim modem
      End If
      Gosub Flushbuf                                        'ochistka bufera
      Print "AT+COPS?" : Gosub Waitms_750                   ' vydast imya operatora svyazi
      Gosub Getline                                         '
      ''  Print #1 , Sms$ ; "."                               ' otvet modema
   Loop Until Len(sms$) > 15                                ' jdem kogda modul' dast imya operatora, a ne pustotu

   L_no_220 = 1 : L_gsm = 1 : L_guard = 1 : L_tm = 1        ' GSM-modem otvetil

   For Temp = 1 To 5                                        ' udalyaet SMS 1 - 5
      Print "AT+CMGD=" ; Str(temp)
      Gosub Waitms_100
   Next
   Gosub Flushbuf                                           'ochistka bufera


   Print "AT+CMGF=0" : Gosub Waitms_100                     ' vklyuchit' tekstovyy rejim SMS
   'Print "AT+CSCS=" ; Chr(34) ; "GSM" ; Chr(34) : Gosub Waitms_100       ' Kodirovka teksta GSM (tol'ko latinskie)

   Print "AT+GSMBUSY=0" : Gosub Waitms_100                  ' 0 = razreshit' vhodyaschie
   Print "AT+CLIP=1" : Gosub Waitms_100                     ' vklyuchit' AON

   Print "AT+CPBS=" ; Chr(34) ; "SM" ; Chr(34) : Gosub Waitms_100       ' Vybrat' Kak Osnovnuyu Pamyat' Sim -kartu

   Print "AT+CNMI=1,2,2,1,0" : Gosub Waitms_100             ' Vklyuchaem opoveschenie o novyh SMS
   '+CMT: "<nomer telefona>", "", "<data, vremya>", a na sleduyuschey strochke - tekst soobscheniya
   ' +CMT: "+79308284748","Boss-1","14/03/31,12:43:29+16"
   '123456

   Print "AT+DDET=1"                                        ' Vklyuchit' detektor DTMF

   '   For Temp = 105 To 255 Step 10                                                     ' trel' - vse normal'no
   '      Sound L_tm , 300 , Temp
   '   Next

   Error_gsm_modem = 0                                      ' vse v poryadke!
   L_no_220 = 0 : L_gsm = 0 : L_guard = 0 : L_tm = 0

   Gosub Waitms_500 : Gosub Flushbuf                        'ochistka bufera
 ''  Print #1 , "Inicializaciyu zavershil!"
Return



'======================================================================
Chitaem_nomera:                                             ' Chitaem nomera
 ''  Print #1 , "Chitaem nomera"
   Reset Watchdog                                           '  Number$ = nomer otpravitelya SMS

   Phone_$_a(1) = "" : Phone_$_a(2) = "" : Phone_$_a(3) = "" : Phone_$_a(4) = "" : Phone_$_a(5) = ""

   If Button = 0 Then
    '  Print #1 , "Button = 0 "
      For Vhod_adc = 256 To 335
       Temp2 = 255 : Writeeeprom Temp2 , Vhod_adc
      Next
      Gosub Setup_phone_boss1                               '   Ustanovlena peremychka = vvod nomera Phone_$_a(1)

   End If

   Readeeprom T1 , 256                                      ' Peremennaya, adres
   If T1 > 9 Then                                           ' Esli nomer Bossa1 neveren,
    ''  Print #1 , "! Net # Boss-1"
     '  Print #1 , "NOt number"
      Gosub Setup_phone_boss1                               '   to na vvod nomera Phone_$_a(1)
   Else

      For Vhod_adc = 256 To 271                             ' Phone_$_a(1)
         Readeeprom T1 , Vhod_adc                           ' Peremennaya, adres
         If T1 <= 9 Then
            Phone_$_a(1) = Phone_$_a(1) + Str(t1)
         Else
            Exit For
         End If
      Next
    ''  Print #1 , "Nomer 1 " ; Phone_$_a(1)

      For Vhod_adc = 272 To 287                             ' Phone_$_a(2)
         Readeeprom T1 , Vhod_adc                           ' Peremennaya, adres
         If T1 <= 9 Then
            Phone_$_a(2) = Phone_$_a(2) + Str(t1)
         Else
            Exit For
         End If
      Next
    ''  Print #1 , "Nomer 2 " ; Phone_$_a(2)

      For Vhod_adc = 288 To 303                             ' Phone_$_a(3)
         Readeeprom T1 , Vhod_adc                           ' Peremennaya, adres
         If T1 <= 9 Then
            Phone_$_a(3) = Phone_$_a(3) + Str(t1)
         Else
            Exit For
         End If
      Next
    ''  Print #1 , "Nomer 3 " ; Phone_$_a(3)

      For Vhod_adc = 304 To 319                             ' Phone_$_a(4)
         Readeeprom T1 , Vhod_adc                           ' Peremennaya, adres
         If T1 <= 9 Then
            Phone_$_a(4) = Phone_$_a(4) + Str(t1)
         Else
            Exit For
         End If
      Next
    ''  Print #1 , "Nomer 4 " ; Phone_$_a(4)

      For Vhod_adc = 320 To 335                             ' Phone_$_a(5)
         Readeeprom T1 , Vhod_adc                           ' Peremennaya, adres
         If T1 <= 9 Then
            Phone_$_a(5) = Phone_$_a(5) + Str(t1)
         Else
            Exit For
         End If
      Next
    ''  Print #1 , "Nomer 5 " ; Phone_$_a(5)

   End If

Return




'======================================================================
Setup_phone_boss1:                                          ' Ustanovka nomera Boss-1

   ' Zaschita
   Vhod_adc = 0
   For Adc_data = &H68B0 To &H6910
      Temp = Cpeek(adc_data)                                'get byte from internal memory
      Vhod_adc = Vhod_adc + Temp
   Next
 ''  Print #1 , "% " ; Vhod_adc

   For Temp = 0 To 2
      L_no_220 = 1 : Gosub Waitms_100
      L_gsm = 1 : Gosub Waitms_100
      L_guard = 1 : Gosub Waitms_100
      L_tm = 1 : Gosub Waitms_100

      L_no_220 = 0 : Gosub Waitms_100
      L_gsm = 0 : Gosub Waitms_100
      L_guard = 0 : Gosub Waitms_100
      L_tm = 0 : Gosub Waitms_500
   Next


 ''  Print #1 , "Stirayu nomera Boss-1"
   For Vhod_adc = 256 To 271                                ' adres v EEPROM, gde hranitsya nomer
      Temp = 255
      Writeeeprom Temp , Vhod_adc                           ' Peremennaya, adres
   Next


 ''  Print #1 , "Vvod nomera Boss-1 ..."
   Do
      ' ---  Opros modema ---
      Gosub Getline                                         ' smotrim chto prishlo ot modema v bufer  - Sms$
      Tstr$ = Left(sms$ , 5)                                ' vydelim pervye 5 simvolov
    ''  Print #1 , Sms$

      If Tstr$ = "+CLIP" Then                               '  nam zvonyat   Vhodyaschiy zvonok   +CLIP: "+79308284748",145,"",,"",0
         Sms$ = Mid(sms$ , 9 , 13)                          ' dostaem nomer zvonyaschego
         Gosub Vydelim_nomer_iz_stroki                      ' Vydelenie # iz stroki v kovychkah i s "+" vnachale v peremennuyu Number$
       ''  Print #1 , "Prinyat nomer: " ; Number$
         Vhod_adc = 256 : Gosub Sohranim_nomer              ' Sohranim etot nomer kak Boss_1
       ''  Print #1 , "--- OK ---"
         For Temp = 0 To 2
            L_no_220 = 1 : Gosub Waitms_100 : L_no_220 = 0
            L_gsm = 1 : Gosub Waitms_100 : L_gsm = 0
            L_guard = 1 : Gosub Waitms_100 : L_guard = 0
            L_tm = 1 : Gosub Waitms_100 : L_tm = 0
         Next
         Goto Reset_device                                  ' ----  Perezapusk ustroystva!!  ----
      End If                                                '   If Tstr$ = "+CLIP:"

      Toggle L_gsm : Toggle L_tm                            ' Dlya indikacii rejima vvoda nomera Boss-1
      Gosub Waitms_500
   Loop





   '======================================================================
Sohranim_nomer:                                             ' Sohranim etot nomer
 ''  Print #1 , "Sohranim nomer"
   Reset Watchdog                                           '  Number$ = nomer otpravitelya SMS

   For Temp = 1 To Len(number$)
      Tstr$ = Mid(number$ , Temp , 1)
      T1 = Val(tstr$)
      Adc_data = Temp + Vhod_adc                            ' Po adresu Counter_c + X
      Decr Adc_data                                         ' -1, chtoby popalo vse po adresu!!!
      Writeeeprom T1 , Adc_data                             ' Peremennaya, adres
   Next

Return

Chtenir_nastroek:

      Readeeprom Kolvo_18b20 , 1                            ' skol'ko zaregistrirovano termometrov DS18b20
   If Kolvo_18b20 = 255 Then
      Kolvo_18b20 = 0
      Writeeeprom Kolvo_18b20 , 1
   End If
   ''  Print #1 , "Termometrov: " ; Kolvo_18b20


   ' Adr_kolvo_t_mem = 2       ' 1 bayt
   Readeeprom Kolvo_t_mem , 2                               ' skol'ko zaregistrirovano "tabletok"  TouchMemory
   If Kolvo_t_mem = 255 Then
      Kolvo_t_mem = 0
      Writeeeprom Kolvo_t_mem , 2
   End If
' Adr_18b20 = 10                ' 8 bayt * 5 Termostatov =  10...49
' Adr_t_mem = 50              ' 8 bayt * 5 klyuchey =  50...89
   Readeeprom T_min_sms_a(1) , 144 : Readeeprom T_max_sms_a(1) , 146
   Readeeprom T_min_sms_a(2) , 148 : Readeeprom T_max_sms_a(2) , 150
   Readeeprom T_min_sms_a(3) , 152 : Readeeprom T_max_sms_a(3) , 154
   Readeeprom T_min_sms_a(4) , 156 : Readeeprom T_max_sms_a(4) , 158
   Readeeprom T_min_sms_a(5) , 160 : Readeeprom T_max_sms_a(5) , 162

    Readeeprom Vhod_min_a(1) , 176 : Readeeprom Vhod_max_a(1) , 177
    Readeeprom Vhod_min_a(2) , 178 : Readeeprom Vhod_max_a(2) , 179
    Readeeprom Vhod_min_a(3) , 180 : Readeeprom Vhod_max_a(3) , 181
    Readeeprom Vhod_min_a(4) , 182 : Readeeprom Vhod_max_a(4) , 183
    Readeeprom Vhod_min_a(5) , 184 : Readeeprom Vhod_max_a(5) , 185

    Readeeprom Vhod_d_a(1) , 186 : Readeeprom Vhod_d_a(2) , 187 : Readeeprom Vhod_d_a(3) , 188
    Readeeprom Vhod_d_a(4) , 189 : Readeeprom Vhod_d_a(5) , 190
            ' dlina oprosa zaschity ot drebezga kontaktov
    Readeeprom Vhod_w_p_a(1) , 191 : Readeeprom Vhod_w_p_a(2) , 192 : Readeeprom Vhod_w_p_a(3) , 193
    Readeeprom Vhod_w_p_a(4) , 194 : Readeeprom Vhod_w_p_a(5) , 195

    Readeeprom Pauza_do_sireny_v(1) , 196 : Readeeprom Pauza_do_sireny_v(2) , 197 : Readeeprom Pauza_do_sireny_v(4) , 198
    Readeeprom Pauza_do_sireny_v(4) , 199 : Readeeprom Pauza_do_sireny_v(5) , 200

    Readeeprom Vhod_m_a(1) , 201 : Readeeprom Vhod_m_a(2) , 202 : Readeeprom Vhod_m_a(3) , 203
    Readeeprom Vhod_m_a(4) , 204 : Readeeprom Vhod_m_a(5) , 205       ' rejim raboty vhoda
    Readeeprom Sirena_timer_p , 220                         '  Adr  220 -  dlitel'nost' raboty sireny
    Readeeprom Config_device , 255                          ' rejimy raboty ustroystva (8 flagov)
   ' 256 - nomer telefona Boss-1 (16 simvolov)
   ' 272 - nomer telefona Boss-2 (16 simvolov)
   ' 288 - nomer telefona User-1 (16 simvolov)
   ' 304 - nomer telefona User-2 (16 simvolov)
   ' 320 - nomer telefona User-3 (16 simvolov)  do 335

   ' 336 - SMS-1  (16 simvolov)
   ' 352 - SMS-2  (16 simvolov)
   ' 368 - SMS-3  (16 simvolov)
   ' 384 - SMS-4  (16 simvolov)
   ' 400 - SMS-5  (16 simvolov)  do 415
   ' 416 -> 461   - sms-g1
   ' 462 -> 507   - sms-g2
   ' 508 -> 553   - sms-g3
   ' 554 -> 599   - sms-g4
   ' 600 -> 645   - sms-g5
   ' 646 -> 691   - sms-g6
   ' 692 -> 737   - Sms-g7
   ' 738 -> 783   - sms-g8
   '
   Readeeprom Termostat_v_a(1) , 800 : Readeeprom Termostat_min_a(1) , 801 : Readeeprom Termostat_max_a(1) , 803
   Readeeprom Termostat_v_a(2) , 805 : Readeeprom Termostat_min_a(2) , 806 : Readeeprom Termostat_max_a(2) , 808
   Readeeprom Termostat_v_a(3) , 810 : Readeeprom Termostat_min_a(3) , 811 : Readeeprom Termostat_max_a(3) , 813
   Readeeprom Termostat_v_a(4) , 815 : Readeeprom Termostat_min_a(4) , 816 : Readeeprom Termostat_max_a(4) , 818
   Readeeprom Termostat_v_a(5) , 820 : Readeeprom Termostat_min_a(5) , 821 : Readeeprom Termostat_max_a(5) , 823

    Readeeprom Vhod_v_a(1) , 825 : Readeeprom Vhod_v_a(2) , 826 : Readeeprom Vhod_v_a(3) , 827
    Readeeprom Vhod_v_a(4) , 828 : Readeeprom Vhod_v_a(5) , 829       ' nomer privyazannogo vyhoda ko vhodu

   Readeeprom Rejim_vihoda_a(1) , 830 : Readeeprom Rejim_vihoda_a(2) , 831 : Readeeprom Rejim_vihoda_a(3) , 832
   Readeeprom Rejim_vihoda_a(4) , 833 : Readeeprom Rejim_vihoda_a(5) , 834

   Readeeprom Zaderjka_postanovki , 835

   Readeeprom Phoner(2) , 836 : Readeeprom Phoner(3) , 837 : Readeeprom Phoner(4) , 838 : Readeeprom Phoner(5) , 839
   'Adr_t_mem = 50              ' 8 bayt * 5 klyuchey =  944...1024

Return

Vkl_vihoda:
            Select Case Nomer_vihoda
            Case 1 : If Rejim_vihoda_a(1) = Rejim_vihoda Or Rejim_vihoda = 255 Then Out_1 = 1
            Case 2 : If Rejim_vihoda_a(2) = Rejim_vihoda Or Rejim_vihoda = 255 Then Out_2 = 1
            Case 3 : If Rejim_vihoda_a(3) = Rejim_vihoda Or Rejim_vihoda = 255 Then Out_3 = 1
            Case 4 : If Rejim_vihoda_a(4) = Rejim_vihoda Or Rejim_vihoda = 255 Then Out_4 = 1
'            Case 10 : Do                                    'proggaz
'                        Print #1 , 999977                   'otkluchenie platy
'                        Startbit = 1
'                        Otvet$ = "zapusk dopolnitelBnogo modulq"
'                        Gosub Sendsms
'                      Exit Do
'                      Loop                                  'vkluchenie platy
            Case 11 : Print #1 , 119999
            Case 12 : Print #1 , 219999
            Case 13 : Print #1 , 319999
            Case 15 : Print #1 , 419999                     'Term1
            Case 16 : Print #1 , 519999                     'Term2
            Case 17 : Print #1 , 619999                     'Term3
            Case 18 : Print #1 , 719999                     'Vih 1
            Case 19 : Print #1 , 819999                     'Vih 2
            Case 199 : Print #1 , 919999
         End Select

         Nomer_vihoda = 0
Return
Vikl_vihoda:
            Select Case Nomer_vihoda
            Case 1 : If Rejim_vihoda_a(1) = Rejim_vihoda Or Rejim_vihoda = 255 Then Out_1 = 0
            Case 2 : If Rejim_vihoda_a(2) = Rejim_vihoda Or Rejim_vihoda = 255 Then Out_2 = 0
            Case 3 : If Rejim_vihoda_a(3) = Rejim_vihoda Or Rejim_vihoda = 255 Then Out_3 = 0
            Case 4 : If Rejim_vihoda_a(4) = Rejim_vihoda Or Rejim_vihoda = 255 Then Out_4 = 0
'            Case 10 : Do                                    'proggaz
'                        Print #1 , 999966                   'otkluchenie platy
'                        Startbit = 0
'                        Otvet$ = "otklUCenie dopolnitelBnogo modulq"
'                        Gosub Sendsms
'                      Exit Do
'                      Loop
            Case 11 : Print #1 , 109999
            Case 12 : Print #1 , 209999
            Case 13 : Print #1 , 309999
            Case 15 : Print #1 , 409999                     'Term1  Dlya term eto 5 vyhod
            Case 16 : Print #1 , 509999                     'Term2  Dlya term eto 6 vyhod
            Case 17 : Print #1 , 609999                     'Term3  Dlya term eto 7 vyhod
            Case 18 : Print #1 , 709999                     'Vih 1
            Case 19 : Print #1 , 809999                     'Vih 2
            Case 199 : Print #1 , 909999
         End Select
         Nomer_vihoda = 0
Return

Perebor_na_vkl:
   For Temp = 1 To 5
           Nomer_vihoda = Temp
           Gosub Vkl_vihoda
   Next
Return

Perebor_na_vikl:
   For Temp = 1 To 5
           Nomer_vihoda = Temp
           Gosub Vikl_vihoda
   Next
Return



Analyz_vhodov:

   Vhod_adc = Getadc(in_0) : Vhod_adc = Vhod_adc / 4
   If Vhod_adc <= Vhod_min Or Vhod_adc >= Vhod_max Then     ' esli znacheniya vyshli za predely, to oshibka
     If Vhod_m = 3 Or Vhod_m = 2 Or Vhod_m = 4 Then Goto Analyz_v1
     Analyz_v2:
      If Vhod_otpravil = 0 Then                             ' Esli Ne Bylo Narusheniya , I Trevogu Po Etomu Shleyfu Ne Ob "yavlyali"
         If Ohrana = 1 Or Vhod_m = 1 Or Vhod_m = 3 Or Vhod_m = 4 Then       ' esli vklyuchena Ohrana ili dannyy vhod - kruglosutochnyy
            If Vhod_w > 0 Then                              ' Esli vklyuchena neaktivnost' vhoda posle poslednego srabatyvaniya
               Decr Vhod_w
            Else                                            '  esli Vhod_w_a(1) = 0  , T.e., net zaderjki v oprose vhoda
               For Temp = 1 To Vhod_d                       ' pauza - ustranenie drebezga
                  Gosub Waitms_500 : Reset Watchdog
               Next

               Vhod_adc = Getadc(in_0) : Vhod_adc = Vhod_adc / 4       ' povtoryy opros posle pauzy, narushenie v nalichii?
               If Vhod_adc <= Vhod_min Or Vhod_adc >= Vhod_max Then       ' esli znacheniya vyshli za predely, to Trevoga
                 If Vhod_m = 3 Or Vhod_m = 2 Then Goto Analyz_v1       ''  Print #1 , "Trevoga 1"
                  Analyz_v3:
                  Vhod_w = Vhod_w_p                         ' vnosim v peremennuyu znachenie zaderjki aktivnosti vhoda posle srabatyvaniya
                  Alarm_in = Nomer_vhoda
                  Gosub Vkl_vihoda
                  If Pauza_do_sireny < 1 And Sirena_timer < 1 Then
                   Pauza_do_sireny = Pauza_do_sireny_0 * 2
                   If Pauza_do_sireny_0 = 0 Then Pauza_do_sireny = 1
                   Toggle L_guard
                   Sirena_timer = Sirena_timer_p
                   If Sirena_timer_p = 0 Then Sirena_timer = 1
                  End If
                  Vhod_otpravil = 1
               Else
                 If Vhod_m = 3 Or Vhod_m = 2 Or Vhod_m = 4 Then Goto Analyz_v3
               End If                                       '          If Vhod_1 <= Vhod_min_a(1) Or Vhod_1 >= Vhod_max_a(1)
            End If                                          '      If Vhod_w_a(1) = 0 Then
         End If                                             '      If Ohrana = 1 Or Vhod_m_a(1) >= 1 Then
      End If                                                '    If Vhod_otpravil_a(1) = 0 Then
   Else
      If Vhod_m = 3 Or Vhod_m = 2 Or Vhod_m = 4 Then Goto Analyz_v2
      Analyz_v1:
      Vhod_otpravil = 0 : Vhod_w = 0                        ' shleyf vostanovlen
      Gosub Vikl_vihoda
   End If

Return

R_ohrani:
      If Ohrana = 2 Then
           Dozvon_po_trevoge = 0
           Ohrana = 0                                       '  OHRANA
           L_guard = 0
           Pauza_do_sireny = 0
           For T1 = 1 To 5
            Vhod_otpravil_a(t1) = 0
           Next
           Rejim_vihoda = 2
           Gosub Perebor_na_vikl
           Rejim_vihoda = 1
           Gosub Perebor_na_vikl                            ' Otklyuchim sirenu
           Otvet$ = Otvet$ + " snqt s ohranQ"
           If Config_device.6 = 1 Then
              Rejim_vihoda = 20
              If Vremya_dozvona > 30 Then
               Gosub Sendsms
              Else
               T_znak_temp2 = 250
              End If
           Else
              Otvet$ = ""
           End If
          Return
         End If
      If Ohrana = 3 Then
            Ohrana = 1
            L_guard = 1
            Rejim_vihoda = 2
            Gosub Perebor_na_vkl
            Otvet$ = Otvet$ + " na ohrane"
           If Config_device.6 = 1 Then
             Rejim_vihoda = 21
             Gosub Sendsms
           Else
              Otvet$ = ""
           End If
           Return
   End If
Return

Opros_tm:
   ' Opros linii TM na fakt podklyucheniya klyucha
   If Kolvo_t_mem > 0 Then                                  ' Esli vvedeny klyuchi 1wire, to oprashivaem shinu
      If Tm_in = 1 Then                                     ' Esli liniya TM ispravna
         Error_line_tm = 0
         Start Watchdog : Reset Watchdog

         Kolichestvo_1wire = 1wirecount()
         ''  Print #1 , "?=" ; Kolichestvo_1wire
         If Kolichestvo_1wire <> 0 Then
            For T1 = 1 To Kolichestvo_1wire
               If T1 = 1 Then                               ' esli pervyy raz chitaem,
                  Temp_massiv_9bait(1) = 1wsearchfirst()    ' chitaem ID semeystva + 6 bayt nomera + CRC8 vo vremennyy massiv
               Else                                         ' Inache
                  Temp_massiv_9bait(1) = 1wsearchnext()     '  chitaem nomer sleduyuschego ustroystva 1wire vo vremennyy massiv
               End If

               ' prinyali nomer ustroystva, proverim: eto TM?
               If Temp_massiv_9bait(1) = 1 Or Temp_massiv_9bait(1) = 137 Then       ' ID semeystva  1= TouchMemory
                  If Crc8(temp_massiv_9bait(1) , 8) = 0 Then       ' Net oshibki v CRC8
                     Gosub Proverka_sovpadeniya_tm          ' Sravnim, s kakim iz imeyuschihsya TM sovpadaet vvedennyy TM
                     If Poslednij_tm <> 0 Then              ' dannyy klyuch TM iz nashego spiska
                        Tm_counter = 0
                        L_tm = 1
                        Time_tm = 20
                      '  Poslednij_tm = Temp_massiv_9bait(9) ' Zapomnim nomer poslednego ispol'zuemogo klyucha TM (dlya otcheta)
                      ''  Print #1 , "Svoy, klyuch #" ; Temp_massiv_9bait(9)

                        If Ohrana = 0 Then                  ' Pomenyaem sostoyanie Ohrany
                         If Zaderjka_postanovki_guard > 0 Then
                           L_guard = 0
                           Ohrana = 0
                            Zaderjka_postanovki_guard = 0
                         Else
                         If Zaderjka_postanovki > 0 Then
                                 Zaderjka_postanovki_guard = Zaderjka_postanovki
                           Else
                                 Ohrana = 3
                           End If
                         End If
                         ''  Print #1 , "Ohrana Vklyuchena!"
                        Else
                           Ohrana = 2
                         ''  Print #1 , "Ohrana Otklyuchena"
                           Pauza_do_sireny = 0 : Sirena_timer = 0 : Dozvon_po_trevoge = 0
                           Rejim_vihoda = 1
                           Gosub Perebor_na_vikl            ' otklyuchim sirenu, esli pischala
                        End If

                        Gosub Waitms_750 : L_tm = 0
                        Temp_massiv_9bait(1) = 0
                           Otvet$ = Otvet$ + " tm=" + Str(poslednij_tm)
                           Gosub R_ohrani                   ' Otpravim soobschenie ob upravlenii klyuchem TM
                        Temp_massiv_9bait(9) = 0            ' sbros, chtoby povtorno ne srabotalo

                     Else
                        If Tm_counter <> 200 Then Incr Tm_counter
                        If Tm_counter = 15 Then
                           Otvet$ = "owibka:podbor tm"
                           Rejim_vihoda = 2
                           Gosub Sendsms
                        End If
                     End If                                 '  If Temp_massiv_9bait(9) <> 0
                  End If                                    '  If Crc8(Temp_massiv_9bait(1) , 8) = 0
               End If                                       '  If Temp_massiv_9bait(1) = 1
            Next

         Else
            Gosub Waitms_100                                ' esli na shine net ustroystv 1wire - nebol'shaya pauza
         End If                                             '  If Kolichestvo_1wire <> 0 Then
         Stop Watchdog
      Else                                                  '  If Tm_in = 1 Then            ' Esli liniya TM ispravna
         'If Error_line_tm = 255 Then Goto Wd_isr_01

         If Error_line_tm <> 200 Then Incr Error_line_tm

         If Error_line_tm = 15 Then                         ' Esli liniya TM dolgo zamknuta, to peredaem SMS o probleme
            Otvet$ = "owibka:tm neispravno"
            Rejim_vihoda = 2
            Gosub Sendsms                                   ' Otpravim podtverjdenie vypolneniya komandy
         '   Error_line_tm = 255                                   ' chtoby povtorno ne otpravlyat' SMS o sboe v linii TM
         End If
      End If
   Else
      Gosub Waitms_100                                      ' esli ne vvedeny ustroystva 1wire - nebol'shaya pauza
   End If                                                   '  If Kolvo_t_mem > 0 Then         ' Esli vvedeny klyuchi 1wire, to oprashivaem shinu
Return

 Convert_to_ucs:
 Otvetn$ = ""
For Temp = 1 To Len(otvet$)
Integ1 = 0
Simvol$ = Mid(otvet$ , Temp , 1)
Parametri_komandy$ = "abvgdejziIklmnoprstufhcCwWDQBEUq"
For Temp2 = 1 To 32
   Tstr$ = Mid(parametri_komandy$ , Temp2 , 1)
   If Tstr$ = Simvol$ Then
      Integ1 = 1071 + Temp2
      Otvetn$ = Otvetn$ + Hex(integ1)
      Exit For
   End If
Next
Parametri_komandy$ = " !{034}#$%&'()*+,-./0123456789:;<=>?"
If Integ1 = 0 Then
   For Temp2 = 1 To 32
      Tstr$ = Mid(parametri_komandy$ , Temp2 , 1)
      If Tstr$ = Simvol$ Then
         Integ1 = 31 + Temp2
         Otvetn$ = Otvetn$ + Hex(integ1)
         Exit For
      End If
   Next
End If
Next
Return

Convert_to_eng:
Integ1 = Hexval(sms$)
 '  Print #1 , "Integ1 =  " ; Integ1
   If Integ1 > 1039 And Integ1 <= 1071 Then Integ1 = Integ1 + 32
   If Integ1 > 1071 And Integ1 < 1104 Then
   Parametri_komandy$ = "abvgdejziIklmnoprstufhcCwWDQBEUq"
      Integ1 = Integ1 - 1071
      Simvol$ = Mid(parametri_komandy$ , Integ1 , 1)
      Otvet$ = Otvet$ + Simvol$
   Else
   Parametri_komandy$ = " !{034}#$%&'()*+,-./0123456789:;<=>?"
      If Integ1 > 31 And Integ1 < 64 Then
         Integ1 = Integ1 - 31
         Simvol$ = Mid(parametri_komandy$ , Integ1 , 1)
         Otvet$ = Otvet$ + Simvol$
      End If
   End If
Sms$ = ""
'Print #1 , "SMS = " ; Otvet$
Return

Number_modem:
   Number$ = Number$ + "F"
Number_b:
      Numbern$ = ""
    For Temp = 1 To 11 Step 2
    Temp2 = Temp + 1
    Simvol$ = Mid(number$ , Temp2 , 1)
    Numbern$ = Numbern$ + Simvol$
    Simvol$ = Mid(number$ , Temp , 1)
    Numbern$ = Numbern$ + Simvol$
   Next
Return

Sms_upd:
 Number$ = Mid(sms$ , 23 , 12)
 Gosub Number_b
 Number$ = Left(numbern$ , 11)
 Rejim_vihoda = 56
Return

Service_gas_input:                                          'proggaz
  If Nomer_vhoda = 254 Then Gosub Wait_2sec
    A = 0
   Inmessage$ = ""
   Time_error = 0
   Reset Watchdog
   Otvet$ = ""
   Print #1 , 999999
      Do
          Reset Watchdog
         A = Inkey(#2)
         Select Case A
           Case 0 :
           Case 35 : Exit Do
           Case Else : If A > 47 And A < 58 Then Inmessage$ = Inmessage$ + Chr(a)
         End Select
         Time_error = Time_error + 1
         If A <> 0 Then Time_error = 0
         If Time_error = 7000 Then
            Error_line = Error_line + 1
            If Error_line = 3 And Startbit = 1 Then
               Otvet$ = "otkaz dopolnitelBnogo modulq"
               Gosub Sendsms
            End If
            If Error_line = 3 Then
               Startbit = 2
               Exit Do
            End If
            Time_error = 0
             Reset Watchdog
             Exit Do
         End If                                             ' If Time_error = 7000
      Loop
      If Inmessage$ <> "" Then
         Time_error = Val(inmessage$)
         If Time_error <= 255 Then
             If Startbit = 0 And Inmessage$ = "000" Then
                 Startbit = 1
             Else
                  If Inmessage$ <> "000" Then
                     Bin_vhodi = Val(inmessage$)
                  End If
         End If
      Else                                                  'If Time_error <= 255 Then
        Tstr$ = Mid(inmessage$ , 1 , 1)
        Temp = Val(tstr$)
        Tstr$ = Mid(inmessage$ , 2 , 1)
        Temp2 = Val(tstr$)=
        Tstr$ = Mid(inmessage$ , 3 , 1)
        Temp3 = Val(tstr$)
        If Temp = 3 Then
         Kotli(temp2) = Str(temp3)
           Select Case Temp3
              Case 9 : Otvet$ = "ostanovka:"
              Case 1 : Otvet$ = "zapusk:"
              Case 2 : Otvet$ = "net plameni:"
              Case 3 : Otvet$ = "peregrev teplonositelq 1:"
              Case 4 : Otvet$ = "net tqgi:"
              Case 5 : Otvet$ = "peregrev teplonositelq 2:"
              Case 6 : Otvet$ = "nizkoe naprqjenie:"
              Case 7 : Otvet$ = "net nagreva teplonositelq:"
              Case 8 : Otvet$ = "net otveta:"
           End Select
            If Temp3 = 9 Then Kotli(temp2) = "0"
            Otvet$ = Otvet$ + " kotel " + Str(temp2)
        End If                                              'If Temp = 3 Then

        If Temp = 4 Then
           Parametri_komandy$ = Str(temp2)
           Parametri_komandy$ = Parametri_komandy$ + Str(temp3)
        End If

         If Temp = 8 Then
            'Print #1 , 989999
            Vhod_adc = Temp2 * 41
               Vhod_adc = Vhod_adc + 375

               Otvet$ = ""                                  ' chitaem soobschenie iz EEPROM mikrokontrollera
               For Temp = 0 To 40
                  Adc_data = Vhod_adc + Temp                ' Po adresu Vhod_adc + X
                  Readeeprom T1 , Adc_data                  ' Peremennaya, adres
                  If T1 = 255 Then Exit For                 ' Doshli do konca soobscheniya
                  Otvet$ = Otvet$ + Chr(t1)
               Next

               ' esli v pamyati net nastroek
               Rejim_vihoda = 41                            'sms gas

              Select Case Temp3
               Case 1 : Otvet$ = "srabotal " + Str(temp2) + ":" + Otvet$
               Case 0 : Otvet$ = "vosstanovlenie " + Str(temp2) + ":" + Otvet$
              End Select
         End If                                             'If Temp = 8
      If Temp = 9 Then
        Otvet$ = "prinqto"
      End If
   End If                                                   'If Time_error <= 255 Then


      If Startbit = 0 Then
         Time_error = 0
         Goto Service_gas_input
      End If
      If Otvet$ <> "" And Startbit = 1 And Nomer_vhoda <> 254 Then
         Rejim_vihoda = 88
         Gosub Sendsms
      End If
      Otvet$ = ""
      Error_line = 0
  Else
    If Startbit = 0 Then
        Goto Service_gas_input
    End If
  End If
Return