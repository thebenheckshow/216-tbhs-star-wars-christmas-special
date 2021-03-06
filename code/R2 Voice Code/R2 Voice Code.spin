{
NEW OPTIMIZED EXPERIMENTAL VERSION
PINHECK SYSTEM - Parallax Propeller Audio / DMD System Driver
2009-2015 Benjamin J Heckendorn
www.benheck.com
}
CON

  _clkmode = xtal1 + pll16x
  _xinfreq = 6_000_000     
  '_xinfreq = 5_000_000

'hardware constants
  pingA           = 27        'pin-- ping))) A

'software constants
  updaterate      = 15        'range update rate when active (measurement cycles per second) ~15 max
'machine states
  #0, a, b

dir  = 17
stp = 16
en   = 26
limit = 4

open = 0
close = 1

  
'------------------------------------Propeller Cog Usage---------------------------------------Langauge---------
                                '(0) Setup / Interpreter / Kernel Main                          (SPIN)
                                '1 FREE COG
                                '2 FREE COG
sd0_Cog         = 3             'SD File System for SFX                                         (ML)
audioDACCog     = 4             'Machine language DAC Audio Player                              (ML)
audioLoopCog    = 5
stepperCog      = 6
serialCog       = 7 

eepromBus       =               $A0                     'Where the Propeller's EEPROM is on the I2C bus
eepromBase      =               $8000                   'Where free EEPROM starts (32k)

ACK      = 0                        ' I2C Acknowledge
NAK      = 1                        ' I2C No Acknowledge
Xmit     = 0                        ' I2C Direction Transmit
Recv     = 1                        ' I2C Direction Receive
BootPin  = 28                       ' I2C Boot EEPROM SCL Pin
EEPROM   = $A0                      ' I2C EEPROM Device Address

atnSpin = 24
clkSpin = 25
datSpin = 26

commBuffSize = 512                                      'Size of the command buffer (32 commands)
status_LED  =  27                                       'Where the blinky LED is. Very important!

volumeMultiplier = 10                                   'How much to reduce audio volume for each additional track playing concurrently

'Video Attribute Bits:

loopVideo       =               %1000_0000              'Should video start over after it ends?
preventRestart  =               %0100_0000              'If video called is already playing, don't restart it (just let it keep playing)
noEntryFlush    =               %0010_0000              'Do not flush graphics on video start (for instance, to have a number appear on an enqueued video)
noExitFlush     =               %0001_0000              'Do not flush graphics on video end
allowSmall      =               %0000_0001              'Can show small numbers on the video? 
allowLarge      =               %0000_0010              'Can show large numbers on the video?
allowBar        =               %0000_0100              'Can show a Progress Bar?
manualStep      =               %0000_1000              'Video loads first frame, but requires manual advancement

doubleHeight    =               %0100_0000              'To make taller numerals
tripleHeight    =               %1000_0000
quadHeight      =               %1100_0000

'Video Play Control Bits

videoDirection  =               %1000_0000              'Bit that sets which direction the video plays
videoPlay       =               %0000_0100              'Bit that tells the video to advance
videoStepper    =               %0000_1100              'OR'ing the vidPlay variable with this will advance 1 step
videoOneShot    =               %0000_1000              'If this bit is set, we clear the one shot and the stepper, so video won't advance until we OR in "videoStepper" again
videoStepMask   =               %1111_0011              'After a one-shot advance, AND vidPlay with this to clear the stepper bits

'graphicAtt Attribute Bits:

returnPixels    =               %0010_0000              'Before drawing this character, place the existing left and rightmost pixels in the Outbuffer data return buffer
numberStay   =                  %0001_0000              'If we see this bit on a Numbers command, we make the incoming number a Timers number
totalNumbersAllowed = 16                                'Total number of numbers allowed (includes Timer) Setting as 8 means 7 normal numbers (0 - 6) + the timer (7)

'Graphic Command Bits:

clearScreen     =               %1000_0000              'Allows the PIC32 to take command of screen drawing routines
loadScreen      =               %0100_0000


screenBase      =               448                     'The byte # where the screen actually starts (need to put settings offset here instead of zero as well)



VAR     'Display buffer data, SD card read buffer data, audio buffers
 
  long audioBuffer[1280]                                '1K buffer per stereo channel
  byte dataBlock0[512]                                  'For reading sectors off SD card



VAR                                                   'PIC32 to PROP command processor variables

  byte outBuffer[16]                                  '16 bytes we can shift back out the PIC32 during a command transition   
  byte command[commBuffSize]                          '32 (16 byte long) commands that are coming in from the PIC32
  word Pointer                                        'What command line SPIN is currently looking at
    
  long eepromAddress                                  'Address of where we are reading on the EEPROM
  long eepromData                                     'Data to be written to, or read from, the EEPROM

  byte shortName[4]                                      'The 3 characters that identify a game AMH, RZO, JET, etc  

  
VAR                                                     'SD Card Variables for audio video filesystem

  long card0DirectorySector                             'Sector that the SD card's root directory begins at
  long folderSector0[26]                                'Starting sector # for each folder's directory _FA - _FZ in SFX folder (audio)     
  long folderSector1[26]                                'Starting sector # for each folder's directory _DA - _DZ in DMD folder (graphics)
  word sectorScan                                       'Used to count through a sector finding data. Global variable so any function can access it
                          

  byte DMDSearch                                        'Flag (1 or 0) to indicate if we're looking for a DMD video. Allows us to preserve DMDToFind[x] variables for Video Looping
  byte DMDToFind[16]                                    'char0, char1, char2, attributes, play bits, xPos, yPos, transparency color, priority. (0-8) DMD layer 0 search, (9-17) DMD layer search
  byte scoreVideo[16]                                   'What video to run to build a Customized Score, including its Attribute Byte and layers   

  long vidStartingSector[2]                             'Where video starts at in the SD card. 8 sectors per 128x32 color frame. File location + 1 (skips the dummy sector) Vector back to this to loop video
  long vidCurrentSector[2]                              'What sector we're currently on. Increments by 8
  long vidTotalSectors[2]                               'Total # of sectors

  byte vidAttributes[2]                                 'Settings for the frame, looping, no repeat, allow numbers, etc
  byte vidXpos[2]                                       '0 to 128. 64 = centered horizontal (normal)
  byte vidYpos[2]                                       '0 to 64. 32 = centered vertical (normal)
  byte vidPlay[2]                                       'Which way to advance the frames (bitwise operations)

  byte vidWhichLoading                                  'Which video the file system is currently searching for 0 or 1 (loads 0 first, then searches for 1 if asked to)

  long DMDframeTimer                                    'Uses the system timer to determine when to load a new frame. 30 FPS with single video, goes to 15 FPS with double videos to preserve bandwidth
  byte vidFrameCounter                                  'Increments with each new frame. Used only for number flashing effects

  
  byte requestSector                                    'How many sectors the DMD kernel would like loaded off the SD card.
  long DMDReadSector                                    'Which sector we're asking system to fetch    
  long targetAddress                                    'Where in memory the requested sectors should be loaded to (auto-incremented each cycle by 512 bytes which is 1 sector)

 

VAR


  long currentDirectorySector0[5]                       'Which sector of the directory we're currently searching (could possibly be searching for all 5 channels at once)

  byte sfxToFind[12]                                    'Which SFX filename XXX.wav to search for. Channel 0 is 0-2, Channel 1 is 3-5, Channel 2 is 6-8, Channel 3 (music) is 9-11. 
 
  byte leftCurrentV[4]                                  'What the current volume is for left side of each channel (user sets default but if multiple sounds playing volumes are auto-reduced to avoid clipping)
  byte rightCurrentV[4]                                 'What the current volume is for right side of each channel (user sets default but if multiple sounds playing volumes are auto-reduced to avoid clipping)   
  byte leftDefaultV[4]                                  'What the default volume is for left
  byte rightDefaultV[4]                                 'What the default volume is for right
  
  byte sfxPriority[4]                                   'Priority level of sound currently playing on that channel
  byte nextPriority[4]                                  'If we find and load the file, set its priority to this
  long sfxSamples[4]                                    'How many samples to play

  long sfxStartingSector[5]                             'Beginning sector of file       
  long sfxSector[5]                                     'Which sector the SFX is currently playing from
  long loopStart[6]                                     'Which sector offset the audio loop starts at (defaults to 0)
  long loopEnd[6]                                       'Which sector offset  the audio loop ends (defaults to file length / 512 minus 1) 
  long sfxTotalSectors[5]                               'Figure this out upon load, and keep it in case we want to completely turn off an audio loop (go back to default) 
  long musicStartSector                                 'If a music command has anything other than 0 for startPoint, store it here until file is loaded, then start playback at this sector
  
  byte sfxStereoSFX[4]                                  'Stereo FX override for that channel?

  byte musicRepeat                                      '1 = Music repeats when file's done 0 = Music ends when file's done  
  byte musicOneShot                                     'Flag if we should resume previous playing music or not
  byte currentMusic[2]                                  '0,1 what IS playing, 2,3 what WAS playing
  word fadeSpeed                
  word fadeTimer
  byte musicVolumeTarget                                'The target volume of a fade command



VAR                   'DMD & Game Indicator Variables

  long PlayerScore[8]                                   'The 4 player's scores. We use 1-4 for players. Position 0, 5, 6 and 7 can be used to store jackpots, values, etc.
  byte CurrentPlayer                                    'Which player is up
  byte NumberPlayers                                    'How many player game it is (1-4)
  byte Ball                                             'Which ball we are one
  byte credits                                          'How many credits we have. MSB is Freeplay (1) or Not (0)
  long highScores[6]                                    'Stores the top 6 scores
  byte topPlayers[24]                                   'Stores 6 sets of 3 letter initials

  byte nextVid[12]                                       'The next video that plays once current one finishes
  byte nextSound[10]                                     'The next sound to play
  byte syncNext                                         'Flag that says the enqueued video and sound should start together

  byte VidPriority                                      'The priority of the running mode. New mode must be equal or greater to be shown. Score mode is always ZERO. 
  byte videoAtt                                         'Attributes of currently playing video
  byte videoStep                                        'Flag to advance video
 
  byte drawingFrame                                     'Flag that a frame is being drawn


  byte lastScoresFlag                                   'If the last game's scores should be shown in Attract Mode
  byte attractMode                                      'Which part of Attract Mode we're in
  byte mode                                             'What mode the display is in
                                                        '1 = Score Display. Set to 255 within a routine to force Score Display.
                                                        '2 = Progress Display

  byte replayFlag                                       'Flag for system to play REPLAY sound once everything else has finished
  byte doubleZero                                       'Universal flag. If 1, numbers displayed as double zeros (scores) else, single zero (anything else)                        

  long numberFXtimer                                    'Overall timer for number FX (right now we just have "blink" - lame!
  long frameTimer
  
  'You can add up to ([totalNumbersAllowed] numbers on a video. Certain numbers can stay active during a score display as well

  byte graphicType[totalNumbersAllowed]                 'What type of graphic this is
                                                        '00 - No graphics
                                                        '01 - Numbers Command
                                                        '02 - Sprite Command
                                                        
  byte graphicAtt[totalNumbersAllowed]                  'Flag to place numbers on still frames or video  
                                                        '0 = None (off)
                                                        '1 = Large number, XY position
                                                        '2 = Small number, XY position
                                                        '3 = (2) small numbers, upper left and right corners
                                                        '4 = (2) small numbers, lower left and right corners
                                                        '5 = (4) small numbers, all four corners
                                                        '6 = (1) small number, allow double zeros (such as a score)
                                                        '7 = Show a large number after current video finishes
                                                        '8 = Show all 4 fours scores on right side of screen for Match animation
  byte graphicX[totalNumbersAllowed]                    'Position of the number
  byte graphicY[totalNumbersAllowed]                    'Position of the number  
  long graphicValue[totalNumbersAllowed]                'A number to place on stills or video for countdown timers, score bonus, etc. Clear flag when it's done.
  byte graphicPriMatch[totalNumbersAllowed]             'Set a priority for the number. If current video is playing, and does not match this number, number will not display

  byte timerNumberActive                                'If permanent # is found, alert the Score Display so it can accomodate
  long numberFlash                                      'What number value to flash
  word numberFlashTimer                                 'How long to flash it before reverting to Mode 255

  byte numberString[15]                                 'Converts numbers into strings
  byte switchDisplay[10]                                'For viewing the switch matrix + cab switches

  
'Variables for Initial Entry Display


  long numberSectorStart                                'What sector the first frame of number animation is on
  long numberSectorCurrent                              'The current sector we're looking at
  word numberFillPointer                                'Which 512 byte cluster we are filling in memory (0, 512, 1024)
  byte numberCounter                                    'Which animated number frame we are on
  byte numberTotalFrames                                'How many total animated number frames there are
  
  byte blitterOrNot

  'long musicStartSector                                 'Used to calculate the looping offset  
  'long loopStart[4]                                     'Start of music loop, in ms
  'long loopEnd[4]                                       'When music reaches this ms point, it jumps back to loopStart


  byte settingsRequest                                  'Flag that tells the DMD kernel to pause and allow settings to be uploaded to the FPGA
  

VAR

'R2D2

  byte talkFlag

  byte stepperState
  word stepperPosition
  word openTimer

  byte averageCount
  byte lastDistance
  byte averageDistance
  
  long audioLoopStack[50]

  long stepperLoopStack[50]
  
  byte checksum

  word dataPointer

  word commandNumber

  word messLength

  long fileSize
  
  
  
OBJ

  sd0           :               "SD_Engine_0"                             'SD Card Reader 0 (AUDIO)
  audio         :               "roy_DAC_4"                               'NEW DAC engine for Audio

  debug         : "SerialMirror"        'Same as fullDuplexSerial, but can also call from subroutines
  ping          : "dualPing"             'Updated ping object (dual channel fire & forget)
  
  
PUB AVKernel | g, tester, mark, distA, distB, averageCycle


{------------- Start Audio SD Card & DAC Drivers --------------------------}  

  sd0.sdStart(3, 2, 1, 0, -1, -1, sd0_Cog, @dataBlock0)
  sd0.mountPartition
  card0DirectorySector := sd0.GetRootDirectory   

  audio.DACEngineStart(@audioBuffer, 14, 15, 22050, audioDACCog)

  setVolume(0, 100, 100)
  setVolume(1, 100, 100)  
  setVolume(2, 100, 100)
  setVolume(3, 100, 100)   

{----------------- Begin Main Loop ---------------------------------------}  


  BuildFolderList("SFX", @folderSector0)                'In the SFX folder, make a sector listing for all _Fx folders     

  cogINIT(audioLoopCog, audioLoop, @audioLoopStack)

  stepperState := 99
  
  cogINIT(stepperCog, stepperLoop, @stepperLoopStack)
 
  'waitcnt(clkfreq * 5 + cnt)                      'Start FullDuplexSeria

  Debug.start(31, 30, 0, 57600)
 
  ping.calibrate(68)                              'calibrate ping for ambient temperature=68 F

  tester := 0

  mark := cnt                                       'process initialization time

  openTimer := 0


  
  ping.selectAB(a)                                'Select channel A
  distA := ping.ReadPingIn                        'read Ping))) channel A                   
  ping.fireping(pingA)                            'restart measurement cycle on channel A
   
  distA := kalman1c(distA,0)                      'Mild filtering to supress msmnt jitter

  averageCount := 0  
  averageDistance := distA                              'Use these as the baseline
  lastDistance := distA
  
  repeat

    ping.selectAB(a)                                'Select channel A
    distA := ping.ReadPingIn                        'read Ping))) channel A                   
    ping.fireping(pingA)                            'restart measurement cycle on channel A

    distA := kalman1c(distA,0)                      'Mild filtering to supress msmnt jitter

    Debug.Dec(distA)
    Debug.Tx(32)
    Debug.Dec(averageDistance)
    


    mark += clkfreq/updaterate
    waitcnt(mark)                                   'wait for next measurement frame

  
    if lastDistance > (distA - 5) and lastDistance < (distA + 5)              'Look for consistency 
     
      averageCount += 1
      Debug.Tx(32)
      Debug.Dec(averageCount)
     
      if averageCount > 15                            '2 seconds of not much changing?
     
        averageDistance := distA                      'Set this as new average
        
        averageCount := 0                             'Reset counter
     
    else
     
      averageCount := 0                               'Reset counter. We'll only consider reading "average" if it's stable for 2 seconds    


    if distA < (averageDistance / 3) and stepperState == 0                      'Wave can only trigger when lid isn't moving

      Debug.Str(String("WAVE OPEN"))
    
      stepperState := 1

      talkFlag := 1
  
    if stepperState == 20

      openTimer += 1

      if openTimer > 60

        Debug.Str(String("NOW CLOSING..."))
      
        openTimer := 0

        stepperState := 99      'Close it up!

        talkFlag := 1 
        
    Debug.Str(String(13)) 
    lastDistance := distA
            


PUB stepperLoop

  dira[dir]~~
  dira[stp]~~
  dira[en]~~
  dira[limit]~                  'Limit switch is an input

  
  outa[en]~~                    'Default is DISABLED


repeat

  case stepperState

    0   : 'Inert?

      outa[en]~~                'Disable driver

    1   : 'Open?

      if stepperPosition == 0   'OK to open?

        outa[en]~               'Enable stepper  

        stepperState := 10       'Advance to state 10

    10 :  'Opening lid?

      stepperStep(open, 1000)

      stepperPosition += 1

      if stepperPosition == 1600                        '90 degrees?

        outa[en]~~              'Disable steppers

        stepperState := 20      'Set state to OPEN (now it can only close)      

        openTimer := 0          'Reset open timer
        
    99 :  'Home the stepper?  

      outa[en]~
    
      repeat while ina[limit] == 1

        stepperStep(close, 3000)

      PauseUS(500_000)          'Pause half a second...
      
      repeat while ina[limit] == 0

        stepperStep(open, 10_000)

      outa[en]~~                'Disable driver
    
      stepperPosition := 0      'Homed
      stepperState := 0



PUB stepperStep(theDirection, theSpeed)


  outa[dir] := theDirection                                             'Set direction
  outa[stp] := 1                                                            'Pulse stepper
  outa[stp] := 0
  PauseUS(theSpeed)                                                     'Wait for speed
   



PUB AudioLoop | clip, g, tester


  clip := 49

  repeat

    if talkFlag

      talkFlag := 0
    
      playSFX(0, "AA", clip, 255)

      clip += 1

      if clip > 53

        clip := 49

    repeat g from 0 to 3                                                                                'Cycle through the 4 audio SFX channels
  
      if sfxToFind[g * 3]                                                                                   'Doing a search on that channel?
        FindSFXEntry(g)  

      audioFill(g, 127, 0)                                                      'Fill first half of buffer, if needed

      audioFill(g, 255, 128)                                                    'Fill second half of buffer, if needed






PUB audioFill(g, offset0, offset1)

  if sfxSector[g]                                                                                     'Audio file still active?
    
    if (audioBuffer[(g << 8) + offset0] == 0)                                                         'This half just about empty?

      sfxSector[g] := sd0.readSector(sfxSector[g], @audioBuffer[(g << 8) + offset1], 1, 0)          'Fill it!   
     
      if sfxSector[g] > ((sfxStartingSector[g] + loopEnd[g]) - 1)                                          'File done or hit the loop point?
     
         finishSFX(g, 1)
                            



PUB PauseUS(time)

'Pause execution for TIME milliseconds

waitcnt((time * 96) + cnt)




PUB playSFX(whichChannel, char0, char1, char2, incomingPriority) | g, temp, sortFlag     'Play a sound, if a channel is available

  if incomingPriority < sfxPriority[whichChannel]                               'If new file has a lower priority than something already playing on this channel...
    return                                                                      'Don't play it                      

  nextPriority[whichChannel] := incomingPriority                                'Once we find the file, set its priority

  sfxToFind[(whichChannel * 3)] := char0
  sfxToFind[(whichChannel * 3) + 1] := char1 
  sfxToFind[(whichChannel * 3) + 2] := char2 
    
  currentDirectorySector0[whichChannel] :=  folderSector0[char0 - 65]            'Start our search at beginning of the directory of the first letter (such as _FA, _FB)

  'currentDirectorySector0[whichChannel] :=  8384 'folderSector0[0] 'folderSector0[char0 - 65]            'Start our search at beginning of the directory of the first letter (such as _FA, _FB)
  


  
PUB FindSFXEntry(whichChannel)

  'Look for the file in the current Directory Sector...
  sfxStartingSector[whichChannel] := getFileSector(sfxToFind[(whichChannel * 3)], sfxToFind[(whichChannel * 3) + 1], sfxToFind[(whichChannel * 3) + 2], currentDirectorySector0[whichChannel])

  if sfxStartingSector[whichChannel] == 0                                                               'Didn't find the file? (returned a zero but remember we added a ONE)

    if sectorScan == 0                                                                                  'Did it find nothing at start of sector?
      sfxToFind[whichChannel * 3] := 0                                                                  'That means file isn't there - end search. Clearly user error!
      
    else
    
      currentDirectorySector0[whichChannel] += 1                                                        'Advance to next directory sector it will search on the next cycle  

    return       

  if whichChannel == 3 and musicOneShot == 0                                                            'Music channel, and not a one-shot music clip?

    currentMusic[0] := sfxToFind[(whichChannel * 3) + 1]                                                 'Set as current, so we can prevent it from restarting if called again
    currentMusic[1] := sfxToFind[(whichChannel * 3) + 2]

  sfxPriority[whichChannel] := nextPriority[whichChannel]                                               'Set the new priority for this channel
  sfxToFind[whichChannel * 3] := 0                                                                      'Found it, set this to 0 so it stops looking  
  sfxClear(whichChannel)                                                                                'Clear the SFX buffer    
  audio.stopPlayer(whichChannel)                                                                        'Stop the player

  'Determine total sectors from the file's length in case this is a music file. If not a music file, this value will be overwritten
  sfxTotalSectors[whichChannel] := (blockToLong0(sectorScan + 28) >> 9) - 8   '((dataBlock0[sectorScan + 31] << 24 + dataBlock0[sectorScan + 30] << 16 + dataBlock0[sectorScan + 29] << 8 + dataBlock0[sectorScan + 28]) >> 9) - 8
  
  sd0.readSector(sfxStartingSector[whichChannel], @dataBlock0, 1, 0)                                  'Load the first sector into memory
  
  sectorScan := 15                                                                                       'Where to start looking for the DATA indicator 
  sfxSector[whichChannel] := 0
  
  repeat while sfxSector[whichChannel] == 0                                                              'Go past the metadata and find out where the REAL data starts (after the word "data")

    sectorScan += 1                                                                                     'Increment the pointer
  
    if (dataBlock0[sectorScan] == "d" and dataBlock0[sectorScan + 1] == "a") or whichChannel == 3       'Found a lowercase 'd' followed by an 'a'? (Music files are headerless so don't search)
    
      'Set the starting sector of the file
      sfxSector[whichChannel] := sfxStartingSector[whichChannel]
       
      'Fill the DAC buffer 
      sfxSector[whichChannel] := sd0.readSector(sfxSector[whichChannel], @audioBuffer[whichChannel << 8], 1, 0)                         'Fill Buffer 0
      sfxSector[whichChannel] := sd0.readSector(sfxSector[whichChannel], @audioBuffer[(whichChannel << 8) + 128], 1, 0)                 'Fill Buffer 1

      'Calculate # of sectors. It's the size of the sample data / 512 and then we lob 8 sectors off the end just to be sure
      'We save this in its own variable so if you want to turn all ALL audio looping, we still remember where the end of the file is

      if whichChannel == 3                                                                              'If music channel, loop points were sent with the command and stored (position 5)

        loopStart[3] := loopStart[5]                                                                    'Copy starting point

        if loopEnd[5] == 0                                                                              'If end point not set...

          loopEnd[3] := sfxTotalSectors[whichChannel]                                                   'use end of file as default

        else

          loopEnd[3] := loopEnd[5]                                                                      'Else, load the end point that was requested

        if musicStartSector                                                                             'If a starting sector other than 0 was requested, jump to that and clear the flag

          sfxSector[3] := sfxStartingSector[whichChannel] + musicStartSector
          musicStartSector := 0
 
      else                                                                                              'A standard SFX wave file? channel 0-2 Get # of data sectors from the metadata

        sfxTotalSectors[whichChannel] := ((dataBlock0[sectorScan + 7] << 24 + dataBlock0[sectorScan + 6] << 16 + dataBlock0[sectorScan + 5] << 8 + dataBlock0[sectorScan + 4]) >> 9) - 8
      
        longFill(@audioBuffer[whichChannel << 8], 0, (sectorScan + 7) >> 2)                             'Erase the metadata from start of buffer 

        loopStart[whichChannel] := 0                                                                    'Where we jump to when file is over (or we hit a loop point)          
        loopEnd[whichChannel] := sfxTotalSectors[whichChannel]                                          'If no, the default is to set loopEnd to the end of the file

      'Get how many channels are playing (not counting this one that's about to)  
      sectorScan := audio.getActiveChannels * volumeMultiplier                                          'Use the sectorscan variable since we're done with it for now
                                                                 
      'Make this new track quieter based off that      
      if sfxStereoSFX[whichChannel]                                                                     'If stereo SFX set, don't manually set volume here
  
        sfxStereoSFX[whichChannel] := 0
        
      else
  
        if whichChannel < 3                                                                             'Also don't auto-correct volume for music either
      
          setVolumeCurrent(whichChannel, leftDefaultV - sectorScan, rightDefaultV - sectorScan)         'Auto-level the audio if multiple clips are playing                            
      
      'Start the player, skipping the beginning of the buffer which is WAV format data
      audio.startPlayer(whichChannel, sfxSamples[whichChannel])



   

PUB finishSFX(whichChannel, allowRepeat)                                                                             'Stuff to check when a SFX clip ends

  if whichChannel == 3                                                                                  'Music file? Then there's special conditions

    fadeSpeed := 0                                                                                      'If a fade timer was active, disable it

    if musicOneShot == 1                                                                                'Resume music that was playing before one shot clip?
     
      musicOneShot := 0                                                                                 'Turn off music resume flag
      musicResume                                                                                       'Resume old music

      return                                                                                            'Return out so next function won't execute

    if musicRepeat and allowRepeat                                                                      'Repeat music that is playing?

      sfxSector[whichChannel] := loopStart[whichChannel] + sfxStartingSector[whichChannel]              'Set sector pointer back to beginning of file

      return                                                                                            'Return so file keeps playing

    currentMusic[0] := 0                                                                                'Music isn't going to repeat, so clear current music variable in case we start same song over
    currentMusic[1] := 0

  audio.stopPlayer(whichChannel)                                                                        'Stop the DAC player
  sfxPriority[whichChannel] := 0
  sfxClear(whichChannel)

  sfxSector[whichChannel] := 0                                                                          'Prevents kernel from trying to load audio on this channel
  loopEnd[whichChannel] := 0                                                                            'Set to 0 so next time we load this channel it will calculate loopEnd point
  
  audio.Volume(whichChannel, leftDefaultV[whichChannel], rightDefaultV[whichChannel])                   'Set channel to default audio levels

  leftCurrentV[whichChannel] := leftDefaultV[whichChannel]                                              'Set current volume levels back to default
  rightCurrentV[whichChannel] := rightDefaultV[whichChannel]

  if nextSound[4] and nextSound[0] == whichChannel                                                       'SFX in queue, and it's same channel as SFX that just finished?

    if syncNext == 10                                                                                   'Waiting for SFX to finish so we can do an A/V sync?

      'playVideo(nextVid[0], nextVid[1], nextVid[2], nextVid[3], nextVid[4], nextVid[5], 3)                 'Run video
      playSFX(nextSound[0], nextSound[1], nextSound[2], nextSound[3], nextSound[4])                     'Run audio                   
      syncNext := 0                                                                                     'Clear flags
                  
      byteFill(@nextVid[0], 0, 6)                                                                       'Erase the enqueued video  
      byteFill(@nextSound[0], 0, 5)                                                                   'Erase the SFX queue  
                    
      if nextVid[11]                                                                                    'BUT if there's another video in the secondary queue...
               
        byteMove(@nextVid[0], @nextVid[6], 6)                                                           'Copy the second enqueued video into the next enqueued video spot
        byteFill(@nextVid[6], 0, 6)                                                                     'and clear out the secondary queue
 
    else                                                                                                'No sync? Then just play the SFX

      if syncNext == 0                                                                                  'We aren't waiting for the DMD to finish?
    
        playSFX(nextSound[0], nextSound[1], nextSound[2], nextSound[3], nextSound[4])                   'Play that SFX

        byteFill(@nextSound[0], 0, 5)                                                                   'Erase the SFX queue




PUB sfxClear(whichChannel)

  longFill(@audioBuffer[whichChannel << 8], 0, 256)                          'Erase that buffer



  
PUB musicResume

  setVolumeCurrent(3, 0, 0)                                                     'Set music volume to 0

  fadeSpeed := 25                                                               'Set faster fade-in than AMH had
  fadeTimer := fadeSpeed

  musicVolumeTarget := leftDefaultV[3]                                          'Default volume we're going to fade up to  

  sfxStartingSector[3] := sfxStartingSector[4]                             'Beginning sector of file       
  sfxSector[3] := sfxSector[4]                                     'Which sector the SFX is currently playing from
  loopStart[3] := loopStart[4]                                     'Which sector offset the audio loop starts at (defaults to 0)
  loopEnd[3] := loopEnd[4]                                       'Which sector offset  the audio loop ends (defaults to file length / 512 minus 1) 
  sfxTotalSectors[3] := sfxTotalSectors[4]                               'Figure this out upon load, and keep it in case we want to completely turn off an audio loop (go back to default) 



PUB setVolume(whichChannel, leftVol, rightVol)

  leftDefaultV[whichChannel] := leftVol
  rightDefaultV[whichChannel] := rightVol    

  leftCurrentV[whichChannel] := leftVol 
  rightCurrentV[whichChannel] := rightVol 
  
  audio.Volume(whichChannel, leftVol, rightVol)
  

PUB setVolumeCurrent(whichChannel, leftVol, rightVol)

  leftCurrentV[whichChannel] := leftVol 
  rightCurrentV[whichChannel] := rightVol 
  
  audio.Volume(whichChannel, leftVol, rightVol)



PUB blockToWord0(index) ' 4 Stack Longs

  bytemove(@result, @dataBlock0[(index & $1_FF)], 2)
 

PUB blockToLong0(index) ' 4 Stack Longs

  bytemove(@result, @dataBlock0[(index & $1_FF)], 4)







PUB Pause(time)

'Pause execution for TIME milliseconds

waitcnt((time * 104_000) + cnt)




      
    
  


PUB BuildFolderList(fChar0, fChar1, fChar2, arrayPointer) | g, currentDirectorySector                                                      'Read the root directory and make a list of starting sectors for all folders


  currentDirectorySector := getFileSector(fChar0, fChar1, fChar2, card0DirectorySector)                 'Start at the beginning of the Root Directory

  repeat
  
    currentDirectorySector := sd0.readSector(currentDirectorySector, @dataBlock0, 1, 0)                    'Read in a sector of the directory, advance the sector #

    repeat sectorScan from 0 to 480 step 32                                                                   'Check all 16 entries in this block

      if dataBlock0[sectorScan] == 0                                                                          'No more entries - done!
        return

      if dataBlock0[sectorScan] == "_"                                                                  'Valid SFX or SFX Folder Name?

        long[arrayPointer][dataBlock0[sectorScan + 2] - 65] := ((((blockToWord0(sectorScan + 20) << 16) | blockToWord0(sectorScan + 26)) - 2) << 6 ) + card0DirectorySector



PUB getFileSector(char0, char1, char2, searchSector)                                                    'Returns the starting sector of the requested 3 character filename

  

  repeat
  
    searchSector := sd0.readSector(searchSector, @dataBlock0, 1, 0)                                        'Read in a sector of the directory, advance the sector #

    repeat sectorScan from 0 to 480 step 32                                                                   'Check all 16 entries in this block. Root directory probably won't have more than 3-4 files

      if dataBlock0[sectorScan] == 0                                                                          'No more entries - done!
        return 0
    
      if dataBlock0[sectorScan] == char0 and dataBlock0[sectorScan + 1] == char1 and dataBlock0[sectorScan + 2] == char2                                      'Matching file name?

        fileSize := blockToLong0(sectorScan + 28)                                                                               'Put this file size in the standard variable
        
        return ((((blockToWord0(sectorScan + 20) << 16) | blockToWord0(sectorScan + 26)) - 2) << 6 ) + card0DirectorySector     'Return what sector file starts at


PUB getVersionNumber                                    'Call this right after a getFileSector to obtain 3 digit version # from a code update BIN filename XXX_V123.bin

  return ((dataBlock0[sectorScan + 5] - 48) * 100) + ((dataBlock0[sectorScan + 6] - 48) * 10) + (dataBlock0[sectorScan + 7] - 48)





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

numeralLength byte 9, 5  
commaLength   byte 3, 2      
numberHeight  byte 13, 5
zeroChar      byte 0, 5
commaChar     byte 10, 1

numeralLength2 byte 10, 6  
commaLength2   byte 4, 3


DAT                                                     

Message 

        file "error_message.DAT"                        '47x5 pixel message that reads "SD CARD ERROR" - Stored in RAM in case SD card can't load  

ORG 0                                                   'This machine language codes looks for serial data commands from the PIC32  

IOSetup

        rdlong dataBufferStart, par                     'We pass along location of Byte 0 of the Data Out buffer

        mov dataOutPosition, dataBufferStart            'This is our starting byte as well
        
        mov commBufferStart, dataBufferStart            'We copy that value into Comm Buffer Start
        add commBufferStart, #16                        'and add 16 to get Comm Buffer Start position

        mov dData, commBufferStart                      'Our starting command read byte
        
        mov commBufferTop, commBufferStart              'Load starting address into commBufferTop
        add commBufferTop, #256                         'Add commBuffSize to find the Top of Memory (256 bytes = 16 commands) 
        add commBufferTop, #256                         'Add commBuffSize to find the Top of Memory (256 bytes = 16 commands) 
                                                        'We do this twice to get a 512 byte buffer

        mov dataOut, #1                                'Setup masking bits for I/O data
        rol dataOut, #24
        mov Clock, #1
        rol Clock, #25
        mov dataIn, #1
        rol dataIn, #26
        mov dira, dataOut                               'There is one output line, set it
           
        rdbyte dataOutByte, dataOutPosition             'Get the first Data Out Byte from memory 
        mov DataMask, #1                                'Reset the bit mask 
        mov dataByte, #0                                'Clear the data in byte
        
Wait4ClockHigh
        test Clock, ina wz                              'See if the clock is HIGH yet
        if_z jmp #Wait4ClockHigh                        'If not, keep waiting...

FillData
        test dataIn, ina wc                             'See if there's a data bit on the line
        if_c or Databyte, DataMask                      'If so, OR Datamask onto Databyte (else its a zero)

        test dataOutByte, DataMask wc                   'If the current bit of DataOutByte is high...
        muxc outa, dataOut                              'Assert that value on the bus
        
        rol DataMask, #1                                'Shift to the left to make room for next bit
        
Wait4ClockLow
        test Clock, ina wc                              'See if Clock line is still high
        if_c jmp #Wait4ClockLow                         'Keep waiting if it is

        cmp DataMask, #256 wc                           'See if DataMask has shifted an entire byte (is this mask on Bit 8?)
        if_c jmp #Wait4ClockHigh                        'If hasn't, keep getting more bits - else, move on to main memory byte write

WriteByte
        wrbyte Databyte, dData                          'Write the byte we received to main memory
        mov DataMask, #1                                'Reset the bit mask 
        mov dataByte, #0                                'Clear the data in byte
        
        add dData, #1                                   'Increment the Command Buffer Pointer
        add dataOutPosition, #1                         'Increment the Data Output pointer
        
        cmp dataOutPosition, commBufferStart wz         'Did we reach the top? (16 bytes)
        if_z mov dataOutPosition, dataBufferStart       'Reset it!
         
        rdbyte dataOutByte, dataOutPosition             'Get the next (or if it reset, the first) data out byte from memory
        
        cmp dData, commBufferTop wz                     'Did the data buffer reach the top?
        if_z mov dData, commBufferStart                 'Reset it, hopefully SPIN has interpreted 32 commands by now

        jmp #Wait4ClockHigh                             'Wait for more bits



zero                    long    0

'Cog RAM Resident Variables

'Data lines
dataOut                 res       1       'long %00000001_00000000_00000000_00000000  
Clock                   res       1       'long %00000010_00000000_00000000_00000000
dataIn                  res       1       'long %00000100_00000000_00000000_00000000

Databyte                res       1       'byte 0
DataMask                res       1

dataBufferStart         res       1
dataOutPosition         res       1
dataOutByte             res       1       'Read this byte from main memory, then shift it out as new bits are shifted in

commBufferStart         res       1       'Address of first data table
dData                   res       1
commBufferTop           res       1       'Where the top of command line memory is