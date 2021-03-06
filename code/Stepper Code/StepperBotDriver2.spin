



{{ **************************************************************************************
   *                                                                                    *     
   *  Stepper Motor Robot Driver Object                                                 *
   *                                                                                    *
   *  Using 28BYJ48-12-300 Motor ULN2003 Motor Controller                               * 
   *                                                                                    *
   *  Each step is 5.626 degrees / 64 (gear reduction) or 0.087890625 degrees           *
   *  The coils are energized in 8 steps or 0.703125 degrees (8*0.087890625 degrees)    *
   *  Each revolution is 360 degrees/0.087890625 degrees/rev or 4096 steps or           *
   *  512 8-steps cycles.  This code uses a 4 step coil sequence which is slightly      *
   *  faster with less torque by skipping 1/2 steps.                                    *
   *                                                                                    *
   *  This code launches a control method for each motor in an independent cog.  The    *
   *  main code commands the motors by updating parameters of speed, distance, direction*
   *  distance to the target, and brakes.  The motor updates the main code and allows   *
   *  coordination by passing parameters back of remaining distance, remaining distance *
   *  to target and motor in motion.  The main code using a control loop to read        *
   *  sensors, determine status, define needed action and coordinate commands between   *
   *  motors.                                                                           *    
   *                                                                                    *
   *  Gregg Erickson - 2013 (MIT License)                                               *
   *                                                                                    *
   **************************************************************************************
}}



CON     '----------------------------Define Constants ------------------------------------------------------
        _clkmode = xtal1 + pll16x   ' Set Prop to Maximum Clock Speed
        _XinFREQ = 5_000_000

                                    ' Case Constants for Motor Commands of Move Methods
                                    
        RightTurn=1                 ' Pivot to the Right by Stopping the Right Wheel and Forward on the Left
        LeftTurn=0                  ' Pivot to the Left by Stopping the Left Wheel and Forward on the Right
        Straight=2                  ' Both Wheel Same Direction and Speed
        RightTwist=3                ' Twist Right by Reversing Right Wheel and Forward on the Left Wheel
        LeftTwist=4                 ' Twist Left by Reversing Left Wheel and Forward on the Right Wheel
        LeftCurve=5                 ' Curve Left by Running Left Wheel Proportionally Slower than the Right
        RightCurve=6                ' Curve Right by Running Right Wheel Proportionally Slower than the Left
        DumpRight=7                 ' Dump Right by Rotating Dispensing Motor
        DumpLeft=8                  ' Dump Left by Rotating Dispensing Motor 

                                    ' Stepper Motor Sequence to Distance Conversion Ratios
    
        InchTicks=53                ' Motor Sequences to Move a Wheel 1 inch
        CentiTicks=21               ' Motor Sequences to Move a Wheel 1 Centimeter
        RevTicks=512                ' Motor Sequences to Rotate a Wheel 360 Degrees
        TwistDegTicks=3             ' Motor Sequences to Rotate 1 Degree Using 2 Wheels
        TurnDegTicks=6              ' Motor Sequences to Pivot 1 Degree Using 1 Wheel  


                                    ' Stepper Sequence Constants
        Full=0
        Half=1
        Wave=3                            
        
        TO_CM = 29_034              ' Microseconds to go 1 Centimeters
                           
 
VAR    '----------------------------- Define Variables -------------------------------------------------------
                                

Long LeftStack[100]                            ' Set Aside Stack Space for Motor Cogs
Long RightStack[100]
Long FeedStack[100]                            ' Set Aside Stack Space for Ping & IR Sensors
Long PingStack[100]
Long IRStack[100]


Long LeftSpeedAddr, RightSpeedAddr,FeedSpeedAddr           ' Speed of Each Motor, 0 to 500 (Max Limit for Specific Motor)
Long LeftDistAddr, RightDistAddr,FeedDistAddr              ' Primary Count for Maximum Distance for Motor to Drive, each step is ~1/4 inches
Long LeftTargetAddr,RightTargetAddr,FeedTargetAddr         ' Secondary Count of Distance for Motor Drive to Target, Secondary Maximum
Long LeftBrakeAddr, RightBrakeAddr,FeedBrakeAddr           ' Set Brakes When Not in Motion by Energizing All Coils, 1=Energized, 0=Off
Long LeftOdometerAddr,RightOdometerAddr,FeedOdometerAddr   ' Odometer
Long LeftTripAddr, RightTripAddr,FeedTripAddr              ' Trip Odometer 
Long LeftLockAddr,RightLockAddr,FeedLockAddr               ' In Motion Flags, True or False
Long LeftStepsAddr,RightStepsAddr,FeedStepsAddr
Long LeftPin,RightPin,FeedPin


      '---------------------------- Start Methods and Launch Cogs ----------------------------------------------

PUB StartPing(PingPin,RangePtr)     '' Launch Ping Sensor Loop in a Separate Cog   


  cognew(PingRange(PingPin,RangePtr),@PingStack)  

PUB StartIrLineSensor(LeftIRPin,RightIRPin,LeftLedPtr,RightLedPtr,IRStatusPtr)   '' Launch IR Sensor Method in a Separate Cog

    cognew(LineSensor(LeftIRPin,RightIRPin,LeftLedPtr,RightLedPtr,IRStatusPtr),@IRstack)


PUB StartMotor(LeftStepsPtr,LeftPinPtr,LeftSpeedPtr,LeftDistPtr,LeftTargetPtr,LeftOdometerPtr,LeftTripPtr,LeftBrakePtr,LeftLockPtr)
'' Launch Control Code in a Separate Cog for Each Motor and Pass Parameters from Hub Memory

' Retrieve Hub Variable Memory Locations Based upon Left Motor Parameters Assuming the Order is Left, Right, Feed

' Retrieve Values

LeftPin:=Long[LeftPinPtr]           ' First Pin of Four for Each Motor
RightPin:=Long[LeftPinPtr+4]
FeedPin:=Long[LeftPinPtr+8]

' Retrieve Hub Memory Addresses

LeftStepsAddr:=LeftStepsPtr          '   Step Sequence for Each Motor
RightStepsAddr:=LeftStepsPtr+4
FeedStepsAddr:=LeftStepsPtr+8



LeftSpeedAddr:=LeftSpeedPtr            ' Speed of Each Motor, 0 to 500 (Max Limit for Specific Motor) 
RightSpeedAddr:=LeftSpeedPtr+4 
FeedSpeedAddr:=LeftSpeedPtr+8            
LeftDistAddr:=LeftDistPtr              ' Primary Count for Maximum Distance for Motor to Drive, each step is ~1/4 inches   
RightDistAddr:=LeftDistPtr+4 
FeedDistAddr:=LeftDistPtr+8               '
LeftTargetAddr:=LeftTargetPtr          ' Secondary Count of Distance for Motor Drive to Target, Secondary Maximum
RightTargetAddr:=LeftTargetPtr+4
FeedTargetAddr:=LeftTargetPtr+8         
LeftBrakeAddr:=LeftBrakePtr            ' Set Brakes When Not in Motion by Energizing All Coils, 1=Energized, 0=Off  
RightBrakeAddr:=LeftBrakePtr+4 
FeedBrakeAddr:=LeftBrakePtr+8            
LeftOdometerAddr:=LeftOdometerPtr      ' Odometer 
RightOdometerAddr:=LeftOdometerPtr+4
FeedOdometerAddr:=LeftOdometerPtr+8   
LeftTripAddr:=LeftTripPtr              ' Trip Odometer
RightTripAddr:=LeftTripPtr+4   
FeedTripAddr:=LeftTripPtr+8                  
LeftLockAddr:=LeftLockPtr               ' In Motion Flags, True or False
RightLockAddr:=LeftLockPtr+4 
FeedLockAddr:=LeftLockPtr+8                 
                               
' Start Stepper Motor Controls with Values and Pass Hub Memory Addresses  
                                                                              
cognew(Motor(LeftStepsAddr,LeftPin,LeftSpeedAddr,LeftDistAddr,LeftTargetAddr,LeftOdometerAddr,LeftTripAddr,LeftBrakeAddr,LeftLockAddr),@Leftstack)  
cognew(Motor(RightStepsAddr,RightPin,RightSpeedAddr,RightDistAddr,RightTargetAddr,RightOdometerAddr,RightTripAddr,RightBrakeAddr,RightLockAddr),@Rightstack)  
cognew(Motor(FeedStepsAddr,FeedPin,FeedSpeedAddr,FeedDistAddr,FeedTargetAddr,FeedOdometerAddr,FeedTripAddr,FeedBrakeAddr,FeedLockAddr),@Feedstack)  
                               
'--------------------------- Methods ----------------------------------------------------------------------------------


Pub Autobrake(_on)                                      '' Set Parameters for Both Driver Motor to Freeze by Turning on All Coils
   long[LeftBrakeAddr]:=_On                             ' Actual Change Occurs in Motor Driver Sequence
   long[RightBrakeAddr]:=_On

Pub FlashBrakes(_LeftPin,_RightPin,Times,Frac)          '' Flash Brake Lights (Motor Indicator LEDs)       

   Dira[_LeftPin.._LeftPin+3]~~                          ' Set Pins to Output
   Dira[_RightPin.._RightPin+3]~~
   
   repeat times                                          ' Turn Sides On and Off Defined Number of Times
      outa[_LeftPin.._LeftPin+3]~~                       ' Left On
      outa[_RightPin.._RightPin+3]~                      ' Right Off                    
      waitcnt(clkfreq/Frac/2+cnt)                        ' Pause Using Frac Input as a Portion of a Second       
      outa[_LeftPin.._LeftPin+3]~                        ' Left Off
      outa[_RightPin.._RightPin+3]~~                     ' Right On                                         
      waitcnt(clkfreq/Frac/2+cnt)                        ' Pause again then Repeat

  outa[_LeftPin.._LeftPin+3]~                            ' Turn Both Off After Flashing
  outa[_RightPin.._RightPin+3]~
  

Pub Move(_Move,_Speed,_Distance,_Ratio)                  '' Control Motors by Setting Speed and Distance Parameters

  ' Case Constants for Motor Commands are in the Constants Section 

  case _Move
    LeftTurn:                       '     LeftTurn=0, Pivot to the Left by Stopping the Left Wheel and Forward on the Right
      Long[RightSpeedAddr]:=_Speed
      Long[RightDistAddr]:=_Distance
      Long[LeftSpeedAddr]:=0
      Long[LeftDistAddr]:=0
    RightTurn:                      '     RightTurn=1, Pivot to the Right by Stopping the Right Wheel and Forward on the Left
      Long[RightSpeedAddr]:=0
      Long[RightDistAddr]:=0
      Long[LeftSpeedAddr]:=_Speed
      Long[LeftDistAddr]:=_Distance
    Straight:                         '   Straight=2, Both Wheel Same Direction and Speed
      Long[RightSpeedAddr]:=_Speed
      Long[RightDistAddr]:=_Distance
      Long[LeftSpeedAddr]:=_Speed
      Long[LeftDistAddr]:=_Distance
    LeftTwist:                       '    LeftTwist=4, Twist Left by Reversing Left Wheel and Forward on the Right Wheel
      Long[RightSpeedAddr]:=_Speed
      Long[RightDistAddr]:=_Distance
      Long[LeftSpeedAddr]:=-_Speed*_Ratio/100
      Long[LeftDistAddr]:=_Distance*_Ratio/100
    RightTwist: '                    '    RightTwist=3,Twist Right by Reversing Right Wheel and Forward on the Left Wheel
      Long[RightSpeedAddr]:=-_Speed*_Ratio/100
      Long[RightDistAddr]:=_Distance*_Ratio/100
      Long[LeftSpeedAddr]:=_Speed
      Long[LeftDistAddr]:=_Distance
    LeftCurve:                       '    LeftCurve=5, Curve Left by Running Left Wheel Proportionally Slower than the Right
      Long[RightSpeedAddr]:=_Speed
      Long[RightDistAddr]:=_Distance
      Long[LeftSpeedAddr]:=_Speed*_Ratio/100
      Long[LeftDistAddr]:=_Distance*_Ratio/100
    RightCurve: '                          RightCurve=6, Curve Right by Running Right Wheel Proportionally Slower than the Left
      Long[RightSpeedAddr]:=_Speed*_Ratio/100
      Long[RightDistAddr]:=_Distance*_Ratio/100
      Long[LeftSpeedAddr]:=_Speed
      Long[LeftDistAddr]:=_Distance
    DumpRight:                         '   DumpRight=7, Dump Right by Rotating Dispensing Motor Forward
     Long[FeedSpeedAddr]:=600
     Long[FeedDistAddr]:=600
     Long[FeedTargetAddr]:=600     
    DumpLeft:
     Long[FeedSpeedAddr]:=-600      '   DumpLeft=8, Dump Left by Rotating Dispensing Motor Backward
     Long[FeedDistAddr]:=600
     Long[FeedTargetAddr]:=600




'------------------------- Motor Control Method for Launching in Separate Cog ------------------------------------

Pub Motor(_SequencePtr,_pin,_SpeedPtr,_DistPtr,_TargetPtr,_OdoPtr,_TripPtr,_BrakePtr,_LockPtr)| _Sequence,i,j,k,_Speed,_Distance,_PDistance,StartPin, EndPin,BrakeOn,MaxPin
'' Driver code for a single motor


  
Repeat
  'Receive Parameters

  _Sequence:=long[_SequencePtr]
  _Speed:=long[_SpeedPtr]                                        ' Retrieve Desired Speed from Hub Memory

  ' Process Parameters and Implement
  If _speed>0                                                     ' Define Start and Ending Pin for Sequences
    StartPin:=_pin
    EndPin:=_pin+3
  Else                                                            ' Reverse Pins if Speed is Negative
    StartPin:=_pin+3
    EndPin:=_pin
  dira[StartPin..EndPin]~~                                        ' Set Pins to Output

  ||_speed                                                        ' Make Speed Absolute Since Pins Set Direction
  _speed#>=0                                                      ' Verify Speed is Positive

  Case _Sequence                                                    ' Maximum Motor Speed Based on Sequence  
   0: _speed<#=500
   1: _speed<#=500
   2: _speed<#=500
  
                                                  

  If _Speed>0 and Long[_DistPtr]>0 and Long[_TargetPtr]>0         ' If Speed, Distance and Target are Positive then Run Motor
           long[_LockPtr]:=true                                   ' If Running Motor, Set a Flag as Busy So Calling Program Knows to Wait

           case _Sequence

            0:  Repeat i from 0 to 3                                  
                   outa[StartPin..EndPin]:=Byte[@FullSeq+i]                ' Output Full-Step Sequence in 4 Steps
                   waitcnt(clkfreq/_speed+cnt)                          ' Pause between Changes in Sequence Steps to Control Speed
     
            1:  Repeat i from 0 to 7
                   outa[StartPin..EndPin]:=Byte[@HalfSeq+i]                ' Output Half-Step Sequence in 4 Steps 
                   waitcnt(clkfreq/_speed+cnt)                          ' Pause between Changes in Sequence Steps to Control Speed
     
            2:  Repeat i from 0 to 4
                   outa[StartPin..EndPin]:=Byte[@WaveSeq+i]                ' Output Wave-Step Sequence in 8 Steps 
                   waitcnt(clkfreq/_speed+cnt)                          ' Pause between Changes in Sequence Steps to Control Speed
     
             
           Long[_DistPtr]:=Long[_DistPtr]-1                       ' Report Distance Back to Calling Program for Navigating
           Long[_TargetPtr]:=Long[_TargetPtr]-1                   ' Report Target Distance Back to Calling Program for Navigating 
           Long[_OdoPtr]:=Long[_OdoPtr]+1                         ' Report Odometer Back to Calling Program for Navigating 
           Long[_TripPtr]:=Long[_TripPtr]+1                       ' Report Trip Odometer Back to Calling Program for Navigating 
  else 
        long[_LockPtr]:=false                                     ' Clear the Busy Flag so Calling Program Knows to Proceed
 


    
  outa[StartPin..EndPin]~                                         ' Set Brakes after Sequences if Set
  if long[_BrakePtr]==true
     outa[StartPin..EndPin]~~

PUB PingRange(_Pin,_RangePtr) : Microseconds | cnt1, cnt2         '' Use Ping to Measure Distance in Centimeters
                                                                   ' This code is from the Ping Object
                                                                                 
Repeat
  outa[_Pin]~                                                      ' Clear I/O Pin
  dira[_Pin]~~                                                     ' Make Pin Output
  outa[_Pin]~~                                                     ' Set I/O Pin
  outa[_Pin]~                                                      ' Clear I/O Pin (> 2 µs pulse)
  dira[_Pin]~                                                      ' Make I/O Pin Input
  waitpne(0, |< _Pin, 0)                                           ' Wait For Pin To Go HIGH
  cnt1 := cnt                                                      ' Store Current Counter Value
  waitpeq(0, |< _Pin, 0)                                           ' Wait For Pin To Go LOW 
  cnt2 := cnt                                                      ' Store New Counter Value
  Microseconds := (||(cnt1 - cnt2) / (clkfreq / 1_000_000)) >> 1   ' Return Time in µs

  Long[_RangePtr]:=Microseconds* 10_00 / TO_CM                     ' Return Distance In Millimeters
  
  waitcnt(clkfreq/10+cnt)                                          ' Pause to Reduce Sampling Frequencies

  
PUB LineSensor(_LeftIr,_RightIr,_LeftLed,_RightLed,_StatusIrPtr) | InputPin    '' Monitor Reflective Infrared Sensors and Report

Dira[_LeftIr.._RightIr]~                                          ' Set Pins to Input for IR Sensors
Dira[_RightLed.._LeftLed]~~                                       ' Set Pins 12 & 13 to Output for Indicator LEDs

 repeat
    
    InputPin:=ina[_RightIR.._LeftIR]                              ' Read IR Sensors to Determine the Postion of a Line
    Long[_StatusIrPtr] :=InputPin                                 ' Post Status Back to Main Program
    outa[_RightLed.._LeftLed]:=InputPin                           ' Mirror IR Sensor on Indicator LEDs


DAT

FullSeq      byte   %1001, %1100, %0110,   %0011                             ' Full step    
WaveSeq      byte   %1000, %0100, %0010, %0001                               ' Wave    
HalfSeq      byte   %1001, %1000, %1100, %0100, %0110, %0010, %0011, %0001   ' Half step

                               
                   
{{
┌──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┐
│                                                   TERMS OF USE: MIT License                                                  │                                                            
├──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┤
│Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation    │ 
│files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy,    │
│modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software│
│is furnished to do so, subject to the following conditions:                                                                   │
│                                                                                                                              │
│The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.│
│                                                                                                                              │
│THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE          │
│WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR         │
│COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,   │
│ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.                         │
└──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┘
}}   
  