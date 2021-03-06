{{ 
┌─────────────────────────────────────┬────────────────┬─────────────────────┬───────────────┐
│ dualPing Demo                       │ BR             │ (C)2008             │  13Sept09     │
├─────────────────────────────────────┴────────────────┴─────────────────────┴───────────────┤
│                                                                                            │
│ Update/extension of the original Savage/Martin object.                                     │
│ This object is a dual-channel driver for Ping))) ultrasonic ranging units (Channels A & B).│
│ A single cog is used to monitor both ping units (via counters A & B).                      │
│                                                                                            │
│ Features: •counters used for time-of-flight measurement (fire & forget)                    │
│           •dual ping driver capable of 2 simultaneous measurements                         │
│           •simple ambient temperature compensation functionality                           │
│           •drop-in replacement for original object                                         │
│                                                                                            │
│ Miscellaneous notes:                                                                       │
│ •Although this object can drive 2 Ping))) units, it works perfectly fine for single        │
│  channel applications, too...just leave the AorB selector set to A and use channel A.      │
│ •If you are using two Ping))) units simultaneously, there is significant possibility       │
│  for mutual interference.  To avoid this, it is recommended that the two Ping))) units     │
│  be placed AT LEAST a foot apart and that each be angled at least 15-20 deg out from each  │
│  other.                                                                                    │
│ •It is strongly recommended that the Ping))) units be mounted inside small open-ended      │
│  boxes (like project boxes, for example) to help isolate them from noise.  Why?            │
│  A room is typically a confined space filled with many hard surfaces and lots of good      │
│  sound reflectors that tend to accentuate noise. In addition, the Ping))) unit apparently  │
│  has significant ringdown and/or sidelobes that can add to this interference. As such,     │
│  it can sometimes be difficult to get consistent/reliable range measurements. A box helps  │
│  to: a) block extraneous ambeint noise b) channel energy coming from sidelobes out toward  │
│  the front of the sensor where it does no harm, and c) adds directivity to the reciever.   │
│  For example: the back wall of my garage is made of cinder blocks. If the bare Ping)))     │
│  unit is placed against this wall facing out into the garage, I cannot get a correct range │
│  measurement as long as the Ping))) unit is within about 8" or less of this block wall,    │
│  presumably because energy from sidelobes bounces off the many scatterers in the rough     │
│  surface of the cinder blocks.  If the Ping))) unit is placed in a small box, the unit     │
│  can be mounted against the wall without trouble.  Chris' installation appears to be       │
│  optimal: http://forums.parallax.com/forums/default.aspx?f=21&m=176721                     │
│ •A note on temperature compensation...a temperature variation of 0 deg F to 100 deg F will │
│  change the resultant distance measurement by √(560/460) or ~10%...not a big deal in most  │
│  applications, but perhaps worth considering for those cases where a considerable range    │
│  of ambient temperature is expected.                                                       │
│                                                                                            │
│ See end of file for terms of use.                                                          │
└────────────────────────────────────────────────────────────────────────────────────────────┘
SCHEMATIC                                                                                            
───────────────────────────────  
2X: ┌───────────────────┐        
    │┌───┐         ┌───┐│        
    ││ ‣ │ PING))) │ ‣ ││            
    │└───┘         └───┘│        
    │    GND +5V SIG    │        
    └─────┬───┬───┬─────┘        
          │  │    1K           
            └┘   └ to Prop     
───────────────────────────────  
                                                                           
}}                                                                         
                                                                           
CON                                                                            
  _clkmode        = xtal1 + pll16x    ' System clock → 80 MHz                  
  _xinfreq        = 6_000_000
'hardware constants
  pingA           = 27         'pin-- ping))) A

'software constants
  updaterate      = 15        'range update rate when active (measurement cycles per second) ~15 max
'machine states
  #0, a, b
  

VAR


OBJ   
  debug: "SerialMirror"        'Same as fullDuplexSerial, but can also call from subroutines
  ping: "dualPing"             'Updated ping object (dual channel fire & forget)

  
PUB Init

  'waitcnt(clkfreq * 5 + cnt)                      'Start FullDuplexSerial
  Debug.start(31, 30, 0, 57600)
  Debug.Str(String("MSG,Initializing...",13))     'Note: debug formatting is set up for use with plx-daq
  ping.calibrate(68)                              'calibrate ping for ambient temperature=68 F
  main


Pub Main| mark, distA, distB
  mark := cnt                                       'process initialization time
  
  repeat
  
    ping.selectAB(a)                                'Select channel A
    distA := ping.ReadPingIn                        'read Ping))) channel A                   
    ping.fireping(pingA)                            'restart measurement cycle on channel A

    distA := kalman1c(distA,0)                      'Mild filtering to supress msmnt jitter

    Debug.Dec(distA)
    Debug.Str(String(13))

    mark += clkfreq/updaterate
    waitcnt(mark)                                   'wait for next measurement frame





pri kalman1c(x_meas,AorB):x_ret
''Simple 1-D kalman filter for scalar random constant with simplified covariance model.
''Uses a constant covariance (and therefore hard-coded kalman gain). Simplifies/speeds
''up calculations. User must manually select gain, where gain is given by:
''gain = const / k_scale (high gainbelieve msmnts)
''Max filter update rate ~9000 samples/sec for 1 cog @ clkfreq=80
'Note: as implemented here, this is actually 2 filters, one for channel A and one for B
'Can easily delete the AorB functionality if only a single channel is desired

  x_prev[AorB] := x_cur[AorB]                        
  x_cur[AorB] := (x_prev[AorB] * k_scale + (k * (x_meas - x_prev[AorB]))) / k_scale
'   Debug.Str(String("  x_meas, "))
'   Debug.dec(x_meas)
'   Debug.Str(String("  x_prev, "))
'   Debug.Dec(x_prev[AorB])
'   Debug.Str(String("  x_cur, "))
'   Debug.dec(x_cur[AorB])
  return x_cur[AorB]


dat
'-----------[ Predefined variables and constants ]-----------------------------
k_scale        long      65_536            '2^16 (scaling varible on K, needed because K is of order 1)                      
k              long      32_768            'Kalman filter gain, in this case 32_768 / 65_536 = 0.5
x_prev         long      0,0               'Kalman filter estimate of x at previous timestep
x_cur          long      0,0               'Kalman filter estimate of x at current timestep


DAT

{{

┌─────────────────────────────────────────────────────────────────────────────────────────────────────┐
│                                     TERMS OF USE: MIT License                                       │                                                            
├─────────────────────────────────────────────────────────────────────────────────────────────────────┤
│Permission is hereby granted, free of charge, to any person obtaining a copy of this software and    │
│associated documentation files (the "Software"), to deal in the Software without restriction,        │
│including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense,│
│and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so,│
│subject to the following conditions:                                                                 │
│                                                                                                     │                        │
│The above copyright notice and this permission notice shall be included in all copies or substantial │
│portions of the Software.                                                                            │
│                                                                                                     │                        │
│THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT│
│LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  │
│IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER         │
│LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION│
│WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.                                      │
└─────────────────────────────────────────────────────────────────────────────────────────────────────┘
}}       