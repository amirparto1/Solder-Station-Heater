$regfile = "m8def.dat"
$crystal = 16000000
$hwstack = 70
$swstack = 40
$framesize = 40                                             '#########IMPOERTANT#####################
'******************************************************
Config Portd = Output : 7seg_anodes Alias Portd
Config Portc.5 = Output : Heater Alias Portc.5
Config Portb.0 = Output : 7seg0 Alias Portb.0
Config Portb.2 = Output : 7seg1 Alias Portb.2
Config Portb.3 = Output : 7seg2 Alias Portb.3
Config Portb.4 = Output : 7seg3 Alias Portb.4
Config Portb.5 = Output : 7seg4 Alias Portb.5
Config Portc.1 = Output : 7seg5 Alias Portc.1
Config Portc.2 = Output : 7seg6 Alias Portc.2
Config Portc.3 = Output : 7seg7 Alias Portc.3
Config Portc.4 = Input : Set Portc.4 : Readrelay Alias Pinc.4
'******************************************************
Config Adc = Single , Prescaler = 16 , Reference = Avcc
Config Timer1 = Pwm , Prescale = 1 , Noise_cancel = 0 , _
Pwm = 10 , Compare_a_pwm = Clear_up
Config Timer2 = Timer , Prescale = 64
Config Watchdog = 128
'******************************************************
Dim Temp As Word , Last_temp As Single , Last_temp_plus As Single , Last_temp_minus As Single
Dim Disp_fan_speed As Single , Last_fan_speed As Single , Real_fan_speed As Single
Dim Last_fan_speed_plus As Single , Last_fan_speed_minus As Single
Dim Temp_avg As Long
Dim Tcount As Byte
Dim F As String * 1
Dim G As String * 1
Dim J As String * 1
Dim L As Byte
Dim M As Byte
Dim N As Byte
Dim L2 As Byte
Dim M2 As Byte
Dim N2 As Byte
Dim Ad1 As Word
Dim Vol_fan As Word
Dim Ads0 As String * 4
Dim A As Word
Dim Setheat As Word , Setheat500 As Single , Setheat_int As Word
'********************************************************
Wa Alias 30
Fan Alias Pwm1a
Ff Alias 120
'*******************************************************
On Ovf2 Ti2
Enable Interrupts
Enable Ovf2
Start Timer2
Start Watchdog
'******************************************************
Fan = 1023
Reset Heater
Declare Sub 7seg_reset
Do
   Reset Watchdog


'*********************************
   For A = 0 To Ff
      7seg0 = 1
      7seg_anodes = &H77
      Waitus Wa
      Call 7seg_reset
      7seg1 = 1
      7seg_anodes = Lookup(l , 7seg)
      Waitus Wa
      Call 7seg_reset
      7seg2 = 1
      7seg_anodes = Lookup(m , 7seg)
      Waitus Wa
      Call 7seg_reset
      7seg3 = 1
      7seg_anodes = Lookup(n , 7seg)
      Waitus Wa
      Call 7seg_reset
      7seg4 = 1
      7seg_anodes = &H78
      Waitus Wa
      Call 7seg_reset
      7seg5 = 1
      7seg_anodes = Lookup(l2 , 7seg)
      Waitus Wa
      Call 7seg_reset
      7seg6 = 1
      7seg_anodes = Lookup(m2 , 7seg)
      Waitus Wa
      Call 7seg_reset
      7seg7 = 1
      7seg_anodes = Lookup(n2 , 7seg)
      Waitus Wa
      Call 7seg_reset
   Next A
Loop
End                                                         'end program

7seg:
   Data &H3F , &H06 , &H5B , &H4F , &H66
   Data &H6D , &H7D , &H07 , &H7F , &H6F




Ti2:
   Stop Timer2
   Disable Ovf2
   Incr Tcount
   Start Adc
   Ad1 = Getadc(0)
   Setheat = Getadc(6)
   Vol_fan = Getadc(7)
   Stop Adc
   Setheat500 = Setheat / 2.04

   Disp_fan_speed = Vol_fan / 1.022
   Disp_fan_speed = Disp_fan_speed / 10
   If Disp_fan_speed < 2 Then Disp_fan_speed = 1
   If Disp_fan_speed > 100 Then Disp_fan_speed = 100
   Real_fan_speed = Vol_fan / 1.5
   Real_fan_speed = Real_fan_speed + 341
  ' If Real_fan_speed > 1010 Then Real_fan_speed = 1023
   If Readrelay = 1 Then
      Fan = Int(real_fan_speed)
   Else
      If Temp > 80 Then
         Fan = 512
      Elseif Temp < 60 Then
         Fan = 0
      End If
   End If


   If Disp_fan_speed > Last_fan_speed_plus Or Disp_fan_speed < Last_fan_speed_minus Then
      Last_fan_speed = Disp_fan_speed
      Last_fan_speed_plus = Last_fan_speed + 0.5
      Last_fan_speed_minus = Last_fan_speed - 0.5
      Ads0 = Str(disp_fan_speed)
      If Disp_fan_speed >= 100 Then
         F = Mid(ads0 , 3 , 1)
         G = Mid(ads0 , 2 , 1)
         J = Mid(ads0 , 1 , 1)
      End If
      If Disp_fan_speed < 100 And Disp_fan_speed >= 10 Then
         F = Mid(ads0 , 2 , 1)
         G = Mid(ads0 , 1 , 1)
         J = "0"
      End If
      If Disp_fan_speed < 10 Then
         F = Mid(ads0 , 1 , 1)
         G = "0"
         J = "0"
      End If
      L = Val(j)
      M = Val(g)
      N = Val(f)
   End If





   Temp_avg = Temp_avg + Ad1

   If Tcount = 20 Then
      Temp = Temp_avg / Tcount
      Tcount = 0
      Temp_avg = 0
    '*********************************
      If Temp < Setheat And Readrelay = 1 Then
         Set Heater
      Else
         Reset Heater
      End If
    '*********************************

      If Setheat500 > Last_temp_plus Or Setheat500 < Last_temp_minus Then
         Last_temp = Setheat500
         Last_temp_plus = Last_temp + 0.8
         Last_temp_minus = Last_temp - 0.8
         Setheat_int = Int(setheat500)
         Ads0 = Str(setheat_int)
         If Setheat_int >= 100 Then
            F = Mid(ads0 , 3 , 1)
            G = Mid(ads0 , 2 , 1)
            J = Mid(ads0 , 1 , 1)
         End If
         If Setheat_int >= 10 And Setheat_int < 100 Then
            F = Mid(ads0 , 2 , 1)
            G = Mid(ads0 , 1 , 1)
            J = "0"
         End If
         If Setheat_int < 10 Then
            F = Mid(ads0 , 1 , 1)
            G = "0"
            J = "0"
         End If
         L2 = Val(j)
         M2 = Val(g)
         N2 = Val(f)
      End If
   '***********************************
   End If
   Enable Ovf2
   Timer2 = 20
   Start Timer2

Return




Sub 7seg_reset
   7seg_anodes = 0
   7seg0 = 0
   7seg1 = 0
   7seg2 = 0
   7seg3 = 0
   7seg4 = 0
   7seg5 = 0
   7seg6 = 0
   7seg7 = 0
End Sub