{
NEW OPTIMIZED EXPERIMENTAL VERSION
PINHECK SYSTEM - Parallax Propeller Audio / DMD System Driver
2009-2015 Benjamin J Heckendorn
www.benheck.com
}
CON

  _clkmode = xtal1 + pll16x
  _xinfreq = 6_500_000     
  '_xinfreq = 5_000_000

AVversion         =              0                      'July 2015                  

'------------------------------------Propeller Cog Usage---------------------------------------Langauge---------
                                '(0) Setup / Interpreter / Kernel Main                          (SPIN)
                                '1 FREE COG
                                '2 FREE COG
sd0_Cog         = 3             'SD File System for SFX                                         (ML)
audioDACCog     = 4             'Machine language DAC Audio Player                              (ML)
Graphic_Cog     = 5             'Runs video, score display, etc                                 (SPIN)
Raster_Cog      = 6             'DMD shift register PWM driver                                  (ML) 
Comm_Cog        = 7             'Get commands from PIC32                                        (ML)  


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
  byte bufferD[4992]                                    'Lower 2K is frame buffer, upper 2K is what is put on display 
  byte dataBlock0[512]                                  'For reading sectors off SD card
  byte font[2944]                                       'RAM that holds the bitmap font data. First 3 sectors are the score (so it's easy to animate off SD) rest is the 5x5 character font (small numbers and letters)

  long CogstackV[75]                                   'Stack for launching a video cog. Keep it high!     



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

'STK500 Programmer

  byte checksum

  word dataPointer

  word commandNumber

  word messLength

  long fileSize
  
  
  
OBJ

  sd0           :               "SD_Engine_0"                             'SD Card Reader 0 (AUDIO)
  dmd           :               "colorDisplay_driver"                     'FPGA color DMD driver      
  audio         :               "roy_DAC_4"                               'NEW DAC engine for Audio
  com           :               "COM_Engine.spin"                         'For communicating with PIC32 bootloader
  
  
PUB AVKernel | g, tester

                                     

{------------------------ Start DMD ---------------------------------------}  

  dmd.Launch(@bufferD[448], Raster_Cog)
  dira[16]~~                                                                    'Set register as an output 
  outa[16]~                                                                     'Default is LOW 
   
  settingsLoad(250, 460, 0, 200, 0)                                             'Settings for Spooky LCD with AMH-style cabinet backbox (zero at end means send immediately, DMD loop isn't running yet)
  
  Mode := 10                                                                    'Don't do anything until we get the all-clear from SD cards, and the PIC32 sends a command to change mode


{------------- Start Audio SD Card & DAC Drivers --------------------------}  

  sd0.sdStart(0, 1, 2, 3, -1, -1, sd0_Cog, @dataBlock0)
  sd0.mountPartition
  card0DirectorySector := sd0.GetRootDirectory   

  Blink(2, 50)

  audio.DACEngineStart(@audioBuffer, 14, 15, 22050, audioDACCog)

  setVolume(0, 80, 80)
  setVolume(1, 80, 80)
  setVolume(2, 80, 80)
  setVolume(3, 35, 100)


{----------------- Begin Main Loop ---------------------------------------}  

  I2Cinitialize                                         'Get the I2C routine booted up

  BuildFolderList("SFX", @folderSector0)                'In the SFX folder, make a sector listing for all _Fx folders  
  BuildFolderList("DMD", @folderSector1)                'In the DMD folder, make a sector listing for all _Dx folders 
  
  LoadNumbers                                           'Load bitmap fonts from SD card into RAM

  showVersions                                          'Get game name + version numbers from EEPROM

  PlayerScore[0] := getFileSector(shortName[0], shortName[1], shortName[2], card0DirectorySector)
  
  if PlayerScore[0]                                     'Does a XXX_Vnnn.BIN file exist in the root directory that matches game's short name?

    PlayerScore[1] :=  getVersionNumber
  
    'if PlayerScore[1]  > eepromData                    'Filename is higher version # than what is installed per the EEPROM?

      'updateGame

  Load
  
  Pause(500)                                           'Slight pause, then proceed with setup


{-------------- Start DMD Kernel Loop ------------------------------------}  
  
  coginit(Graphic_Cog, DMDLoop, @CogStackV)             'We're done with manual file loading, so start DMD loop (video, scores, etc) It will get commands from the interpreter and run them


{-------------- Start ML I/O communication routine -----------------------}  

  eepromData := @outBuffer                                                      'Pass buffer address to ML so it can fill it (use a random LONG to pass the value to save RAM!)
  coginit(Comm_Cog, @IOSetup, @eepromData)                                      'Start command processor, ready to rock!
  Pointer := 0                                                                  'Set command pointer to start and wait for command   


  attractMode := 0                                      'We may not need this in the future
  musicRepeat := 1                                      'By default music should repeat
  numberTotalFrames := 0                                'Default to 1 frame of number animation until we're told how many there are
  
  PlayerScore[1] := 123_456_789
  PlayerScore[2] := 234_678_970
  PlayerScore[3] := 222_333_444
  PlayerScore[4] := 222_333_444
      
  CurrentPlayer := 1
  numberPlayers := 1

  Ball := 1

  Credits := 0                                          'If MSB set, then display will say "Freeplay"
  'timerNumberActive := 1

  
  'playVideo("DMC", loopVideo, 0, 255)

  'DMA DMB DMC DMG DMH DMI DMZ DRA
  'AA1
  'AA2
  'AA3
  

  
  'playVideo("EX1", 0, 0, 255)
  
  'playVideo("AQQ", 0, 0, 255)

  'playSFX(3, "ZLE", 255)  
  'playVideo("AQQ", loopVideo, 0, 255, 3)
  
  'playSFX(2, "AA1", 255)
  
  'Mode := 0

  repeat

    if command[pointer + 15]                                                                            'A command has filled a line of the buffer with Op Code at end?
      Interpret
      LineDone

    if DMDsearch                                                                                        'Searching for a DMD file?
    
      FindDMDEntry                                                                                      'Keep searching directory for it. Don't do ANYTHING else!

    if requestSector                                                                                    'DMD wants a sector?

      DMDReadSector := sd0.readSector(DMDReadSector, targetAddress, 1, blitterOrNot)      'Copy frame from SD card to buffer memory. We do this 1 sector at a time so it won't affect audio playback

      targetAddress += 512                                                                              'Increment the memory pointer the sectors are being loaded to
      
      requestSector -= 1                                                                                'Decrement how many sectors are left to load
    
    repeat g from 0 to 3                                                                                'Cycle through the 4 audio SFX channels
  
      if sfxToFind[g * 3]                                                                                   'Doing a search on that channel?
        FindSFXEntry(g)  

      audioFill(g, 127, 0)                                                      'Fill first half of buffer, if needed

      audioFill(g, 255, 128)                                                    'Fill second half of buffer, if needed

    if fadeSpeed                                                                'Music fade enabled? 

      fadeTimer -= 1                                                            'Decrement timer     

      if fadeTimer < 1                                                          'Timer done?
      
        fadeTimer := fadeSpeed                                                  'Reset timer

        if leftCurrentV[3] > musicVolumeTarget                                      'Move towards target
          leftCurrentV[3] -= 1
          rightCurrentV[3] -= 1

        if leftCurrentV[3] < musicVolumeTarget
          leftCurrentV[3] += 1
          rightCurrentV[3] += 1

        audio.Volume(3, leftCurrentV[3], rightCurrentV[3])                      'Set current volume
        
        if leftCurrentV[3] == musicVolumeTarget                                 'Did we reach the target?
          fadeSpeed := 0                                                        'Stop fade routine
          if musicVolumeTarget == 0                                             'Was it a fade out?
             musicOneShot := 0
             finishSFX(3, 0)                                                    'Terminate music player




PUB audioFill(g, offset0, offset1)

  if sfxSector[g]                                                                                     'Audio file still active?
    
    if (audioBuffer[(g << 8) + offset0] == 0)                                                         'This half just about empty?

      sfxSector[g] := sd0.readSector(sfxSector[g], @audioBuffer[(g << 8) + offset1], 1, 0)          'Fill it!   
     
      if sfxSector[g] > ((sfxStartingSector[g] + loopEnd[g]) - 1)                                          'File done or hit the loop point?
     
         finishSFX(g, 1)
                            

      

PUB Interpret | g, xPos, yPos

 'Look at the opcode, and do what it says!
 
  case command[pointer + 15]

    $01 : 'Play an SFX file?

      playSFX(command[pointer + 0], command[pointer + 1], command[pointer + 2] & %0111_1111, command[pointer + 3], command[pointer + 4])     'Run the sound!

    $02 : 'Play a video file?   

      if command[pointer] == 0 and Mode == 3                                                                                    'Send a "0" as the first character in a video command to STOP VIDEO

        stopVideo
        return

      if command[pointer] == 255                                                                                                'Video Priority manual override? (usually to 0)
      
        VidPriority := command[pointer + 5]                                                                                     'Set new priority, and return
        return
        
      if command[pointer + 5] => VidPriority                                                                                    'Make sure video has priority control before we let it change anything 

        if vidAttributes[0] & preventRestart                                                                                    'Prevent this video from restarting?

          if command[pointer + 0] == DMDToFind[0] and command[pointer + 1] == DMDToFind[1] and command[pointer + 2] == DMDToFind[2] 

             return                                                                                                                'Abort                   

        if numberFlash
          numberFlash := 0

        if (vidAttributes[0] & noExitFlush) and (nextVid[3] & noEntryFlush)                                                      'Current video has noExitFlush, and a queued video has noEntryFlush? Must be a number waiting!
   
          command[pointer + 3] |= %0011_0000                                                                                     'Make sure this video popping in doesn't flush at either end!

        vidStartingSector[1] := 0                                                                                               'Kill second video in case there was one running
    
        playVideo(command[pointer + 0], command[pointer + 1], command[pointer + 2], command[pointer + 3], command[pointer + 4], command[pointer + 5], 3)
    
    $03 : 'Update a player's score? (typically used for adding)
           
      PlayerScore[command[pointer + 0]] := commandToLong(1)

    $04 : 'Show a graphic onscreen

      'serial.Str(STRING("Graphic Command!"))
    
      if command[pointer + 1] == 255                                                                                            'Command to kill graphics?

        GraphicFlush(0, 7)                                                                                                      'Kill ALL ze numbers!

        return                                                                                                                  'Don't do the other stuff

      if command[pointer + 1] == 254                                                                                            'Command to kill score graphics?

        GraphicFlush(8, 15)                                                                                                     'Kill ALL ze numbers!

        return                                                                                                                  'Don't do anything else

      case command[pointer + 0]                                                                                                 'What to do based off Graphic Type

        1 : 'A number graphic? command[pointer + 2] is which graphic # have been chosen. Wisely. 

          if command[pointer + 2] == numberStay                                                                                  'Manual termination of a Timer Number? (send just the bit no number type)
            graphicType[command[pointer + 1]]  := 0
            return 
           
          if command[pointer + 1] > totalNumbersAllowed - 1                                                                         'Invalid graphic #?
           
            return
   
          graphicType[command[pointer + 1]] := 1                                                                                    'Set this graphic type as Number      
          graphicAtt[command[pointer + 1]]  := command[pointer + 2]
          graphicX[command[pointer + 1]]  := command[pointer + 3]
          graphicY[command[pointer + 1]]  := command[pointer + 4]
          graphicValue[command[pointer + 1]] := commandToLong(5)                '(command[pointer + 8] << 24) + (command[pointer + 7] << 16) + (command[pointer + 6] << 8) + command[pointer + 5]

          graphicPriMatch[command[pointer + 1]] := command[pointer + 9]         'Late feature add. Assigns a priority - if number priority matches video priority then it is drawn. 0 = Don't Care.
        
        2 : 'A Progress graphic bar?

          graphicType[command[pointer + 1]] := 2                                                                                    'Set this graphic type as Number      
          graphicAtt[command[pointer + 1]]  := command[pointer + 2]
          graphicX[command[pointer + 1]]  := command[pointer + 3]
          graphicY[command[pointer + 1]]  := command[pointer + 4]
          graphicValue[command[pointer + 1]] := commandToLong(5)                '(command[pointer + 8] << 24) + (command[pointer + 7] << 16) + (command[pointer + 6] << 8) + command[pointer + 5]
          
          graphicPriMatch[command[pointer + 1]] := command[pointer + 9]         'Late feature add. Assigns a priority - if number priority matches video priority then it is drawn. 0 = Don't Care.
          
        3 : 'A Character Sprite?

          graphicType[command[pointer + 1]] := 3                                                                                    'Set this graphic type as Number      
          graphicAtt[command[pointer + 1]]  := command[pointer + 2]
          graphicX[command[pointer + 1]]  := command[pointer + 3]
          graphicY[command[pointer + 1]]  := command[pointer + 4]
          graphicValue[command[pointer + 1]] := commandToLong(5)                         '(command[pointer + 8] << 24) + (command[pointer + 7] << 16) + (command[pointer + 6] << 8) + command[pointer + 5]

          graphicPriMatch[command[pointer + 1]] := command[pointer + 9]         'Late feature add. Assigns a priority - if number priority matches video priority then it is drawn. 0 = Don't Care.

    $05 : 'Video layer control

      g := command[pointer] & %0000_0001                                        'Get the video layer # (0 or 1) other bits could be used for stuff later

      if vidCurrentSector[g] == 0                                               'No video on this layer?

        return                                                                  'Abort

      vidPlay[g] := command[pointer + 1]
      vidXpos[g] := command[pointer + 2]
      vidYpos[g] := command[pointer + 3]  
      
      if command[pointer] & %1000_0000                                          'Frame jump bit set?

        vidCurrentSector[g] := vidStartingSector[g] + commandToLong(4) << 3     'Mutliple frame number by 8 to get sector count, compute offset from start of file
 
    $06 : 'Enqueue a video to play after current one finishes

      g := 0                                                                    'Pointer of where we'll put this enqueued video
    
      if nextVid[5]                                                             'Already a video enqueued?
        g := 6                                                                  'Set pointer to the Second Enqueued video spot.

      if command[pointer] == 0 and command[pointer + 5] == 0                    'If this is actually a KILL Q command (all zeros, like my salesmen)
        syncNext := 0
        byteFill(@nextVid[0], 0, 12)                                            'Wipe out both video queues
        byteFill(@nextSound[0], 0, 5)
        return   

      byteMove(@nextVid[g], @command[pointer], 6)                               'Copy it over to Next Video


      if command[pointer + 10]                                                  'More than 6 bytes, meaning it's a VideoQ + AudioQ command?

        byteMove(@nextSound[0], @command[pointer + 6], 5)                       'Copy the sound filename     
        syncNext := 1                                                           'Set flag that we need to sync the next Video and Audio together

    $07 : 'Make a large number flash after video ends

      numberFlash := commandToLong(0)                                            '(command[pointer + 3] << 24) + (command[pointer + 2] << 16) + (command[pointer + 1] << 8) + (command[pointer + 0])
      numberFlashTimer := command[pointer + 4]

      if numberFlashTimer == 0                          'Old setting?
        numberFlashTimer := 40                          'This is a good default amount (slightly over 2 seconds)

    $08 : 'Play a SFX file after current one finishes?

      byteMove(@nextSound[0], @command[pointer], 5)

    $09 : 'Set starting and ending loop points for the currently playing music file

      if currentMusic[0] == (command[pointer + 2] & %0111_1111) and currentMusic[1] == command[pointer + 3]                     'Is the requested file already playing? Then we just use this as jump / loop control

        loopStart[command[pointer]] := commandToLong(3)                                                                         'Get new starting point
        loopEnd[command[pointer]] := commandToLong(7)                                                                           'Get new ending point
        g := commandToLong(11)                                                                                                  'Jump command?
         
        if g                                                                                                                    'If there is a jump command...
         
          sfxSector[command[pointer]] := sfxStartingSector[command[pointer]] + g                                                'Jump to it immediately!
         
        if loopEnd[command[pointer]] == 0                                                                                       'Flag to reset this to default value?
         
          loopEnd[command[pointer]] := sfxTotalSectors[command[pointer]]                                                        'Set the end point to the value we got when we first loaded the file. Music will still loop, but not til it reaches the end

      else                                                                                                                      'File not already playing, so let's start it!

        if command[pointer + 1] & %1000_0000                                                                                    'One Shot bit enabled?

          if sfxSector[3]                                                                                                       'Only set one-shot if music actually playing (probably is, but you never know)

                                                                                                                                'Copy the parameters for the current music that is playing into an extra buffer
             sfxStartingSector[4] := sfxStartingSector[3]                                                                       'Beginning sector of file       
             sfxSector[4] := sfxSector[3]                                                                                       'Which sector the SFX is currently playing from
             loopStart[4] := loopStart[3]                                                                                       'Which sector offset the audio loop starts at (defaults to 0)
             loopEnd[4] := loopEnd[3]                                                                                           'Which sector offset  the audio loop ends (defaults to file length / 512 minus 1) 
             sfxTotalSectors[4] := sfxTotalSectors[3]                                                                           'Figure this out upon load, and keep it in case we want to completely turn off an audio loop (go back to default)
 
             sfxSector[3] := 0                                                                                                  'These must be cleared for a new file load to work correctly
             loopEnd[3] := 0
             
             musicOneShot := 1                                                                                                  'Set flag
             
        else                                                                                                                    'If we got music command, and bit NOT set, kill the flag

          musicOneShot := 0        

        loopStart[5] := commandToLong(3)                                                                                        'Get loop start for new music file, put in temp storage
        loopEnd[5] := commandToLong(7)                                                                                          'Get loop end for new music file, put in temp storage 
        musicStartSector := commandToLong(11)                                                                                   'Jump command?
                                                                                                                                'If there's a requested start point, save it for when the file starts        
        playSFX(3, command[pointer + 0], command[pointer + 1] & %0111_1111, command[pointer + 2], 255)                          'Run the sound!

 
    $0A : 'Video layer load

      if command[pointer] & %1000_0000                                            'MSB set? Then we're loading a 2 layer CustomScore video
    
        byteMove(@scoreVideo[(command[pointer] & %0111_1111) * 8], @command[pointer + 1], 8)   'Copy the parameters to the Score Display default (send all 0's to kill custom score display) 

      else
      
        byteMove(@DMDToFind[command[pointer] * 8], @command[pointer + 1], 8)      'Copy the parameters 

        if command[pointer + 9]                                                   'Last parameter is the DMD search flag. If not 0, set flags to start loading videos
         
          currentDirectorySector0[4] := folderSector1[DMDToFind[0] - 65]          'Get the directoy sector for the first video we're looking for     
          vidWhichLoading := 0                                                    'Set that we're loading video #0 first   
         
        DMDsearch := command[pointer + 9]                                         'Set the actual flag last so the DMD kernel has all the info it needs

    $0C : 'Set current player #, ball #, total players, free play and # of frames in animated score

      CurrentPlayer := command[pointer + 0]
      Ball := command[pointer + 1]
      NumberPlayers := command[pointer + 2]
      credits := command[pointer + 3]
      lastScoresFlag := command[pointer + 5]
      numberTotalFrames := command[pointer + 6]
      
      if command[pointer + 4] == 255                    'Set replay flag?
        replayFlag := 1
        return                                          'Abort so Attract Mode doesn't start
      
      if command[pointer + 4] > 0
                                       
        attractMode := command[pointer + 4]
        
        clearQueue
        replayFlag := 0
        frameTimer := 5
        
      else
      
        attractMode := 0
        frameTimer := 0

      return

    $0D : 'Play a single sound effect with stereo override?

      sfxStereoSFX[command[pointer + 0]] := 1                                                           'Set stereo FX flag

      setVolumeCurrent(command[pointer + 0], command[pointer + 5], command[pointer + 6])
       
      playSFX(command[pointer + 0], command[pointer + 1], command[pointer + 2], command[pointer + 3], command[pointer + 4])

    $0E : 'Get high scores from main CPU so they can be displayed during attract mode

      highScores[command[pointer + 0]] := commandToLong(1)                      '(command[pointer + 4] << 24) + (command[pointer + 3] << 16) + (command[pointer + 2] << 8) + (command[pointer + 1])                    'Get the score
      topPlayers[(command[pointer + 0] * 3) + 0] := command[pointer + 5]        'Get the initials in ASCII (we use @ as a blank)
      topPlayers[(command[pointer + 0] * 3) + 1] := command[pointer + 6]       
      topPlayers[(command[pointer + 0] * 3) + 2] := command[pointer + 7] 

    $10 : 'Volume control command for SFX and Music. Contains sub command for fading music in and out

      if command[pointer + 0] == "f"                                            'Change SFX volume? (f)
        setVolume(command[pointer + 1], command[pointer + 2], command[pointer + 3])                        'Store new default values 

      if command[pointer + 0] == "r"                                            'Set Repeat or End?
        musicRepeat := command[pointer + 1]

      if command[pointer + 0] == "z"                                            'Fade out music?

        if sfxSector[3] == 0                                                   'Music not playing? Abort.
          return

        if command[pointer + 1] == 0                                            'Just up and kill the music?
          finishSFX(3, 0)                                                       'Stop music, don't bother with any of the fading stuff
          return

        fadeSpeed := command[pointer + 1] << 7                                  'Enable fadeSpeed, which also acts as the active flag for fading (thus can never be zero)
        musicVolumeTarget := command[pointer + 2]                               'What we're fading to 

        if musicVolumeTarget > 100
          musicVolumeTarget := 100

        if fadeSpeed > 2550                                                     '10 seconds * 255 cycles. No fade needs to be longer than 10 seconds!
          fadeSpeed := 2550
        
        fadeTimer := fadeSpeed                                                  'Rougly "fadeSpeed" seconds for the fadeout   
 
    $12 : 'Put a string of text on the screen?

      g := 1

      xPos := (command[pointer + 0] >> 3) * 6                                   'First 5 bits are X position (by horizontal character)
      yPos := (command[pointer + 0] & %00000111) * 6                            'Last 3 bits are Y position (by vertical character)
      
      repeat while command[pointer + g] > 0 and command[pointer + g] < 255       'Will terminate either with a 0 (string terminator) or 255 (end of command line)

        LoadAlpha(command[pointer + g++], xPos, yPos, 5)
        xPos += 6

        if xPos > 127
          xPos := 0
          yPos += 6
          if yPos > 27
             quit

    $13 : 'A graphics command?

      Mode := command[pointer + 0]
      frameTimer := 0
      
      repeat while drawingFrame                                               'Don't abort yet if system is drawing a frame 
      
      if command[pointer + 1] == clearScreen

        Clear

      if command[pointer + 1] == loadScreen

        Load       

    $14 : 'Load a sprite into memory, manually?

      '3 character filename, plus flag if a LOAD command should execute afterwards

      repeat while drawingFrame                                               'Don't abort yet if system is drawing a frame 

      loadSprite(command[pointer + 0], command[pointer + 1], command[pointer + 2]) ', command[pointer + 3])

 
    $15 : 'Get switch matrix, draw it onscreen

      byteMove(@switchDisplay[0], @command[pointer], 10)                      'Copy switches into memory 

      drawSwitches

    $20 : 'Write a long to EEPROM?

      'The PIC32 can access longs 0 - 8191. We take the address from the PIC32, multiply by 4, and add the base address past the BOOT EEPROM
      
      eepromAddress := commandToLong(0)                          '(command[pointer + 3] << 24) + (command[pointer + 2] << 16) + (command[pointer + 1] << 8) + command[pointer + 0]
      'eepromAddress := (eepromAddress << 2) + eepromBase
      eepromData := commandToLong(4)                             '(command[pointer + 7] << 24) + (command[pointer + 6] << 16) + (command[pointer + 5] << 8) + command[pointer + 4] 

      writeLong($A0, eepromAddress, eepromData)

    $21 : 'Read a long from EEPROM?

      eepromAddress := commandToLong(0)                          '(command[pointer + 3] << 24) + (command[pointer + 2] << 16) + (command[pointer + 1] << 8) + command[pointer + 0]                 'Get 0-8191
      'eepromAddress := (eepromAddress << 2) + eepromBase                                                                                                'Convert
      eepromData := readLong($A0, eepromAddress)     

      outBuffer[0] := eepromData
      outBuffer[1] := eepromData >> 8
      outBuffer[2] := eepromData >> 16
      outBuffer[3] := eepromData >> 24 
 



PUB LineDone
   
  bytefill(@command[0] + pointer, 0, 16)                                        'Erase this line of the buffer
   
  pointer += 16                                                                 'Increment pointer
  if pointer == commBuffSize                                                    'Reached the top?
    pointer := 0                                                                'Reset



PUB commandToLong(startingLongByte)

  startingLongByte += pointer

  return command[startingLongByte++] + (command[startingLongByte++] << 8) + (command[startingLongByte++] << 16) + (command[startingLongByte++] << 24) 



PUB DMDLoop | g                                             'This runs in its own cog. Controls what goes on the DMD display, streams video, does text


repeat

  if settingsRequest == 1
   
    settingsRequest := 2    
    repeat while settingsRequest == 2

  if DMDsearch == 0                                     'If searching for videos, do nothing. This prevents Score Screen flash between queued videos
    
    case Mode                                           'Figure out what we should be doing
     
      0 :                                               'Nothing going on? Then draw the default score or goto Attract Mode
   
        Score
     
      3 :                                               'A video is playing?
   
        videoStream                                     'Load more frames 

      8 :                                               'A video is playing?
   
        videoStream   
   
      255 :                                             'A video just ended itself?

          AnimateScoreReset
      
          if nextVid[5]                                                                                          'A video is waiting?
       
            if syncNext == 0                                                                              'Not a A/V sync, just a video?
            
               playVideo(nextVid[0], nextVid[1], nextVid[2], nextVid[3], nextVid[4], nextVid[5], 3)          'Run it like normal
     
               byteFill(@nextVid[0], 0, 6)                                                              'Erase the enqueued video  
               
               if nextVid[11]                                                                           'BUT if there's another video in the secondary queue...
     
                  byteMove(@nextVid[0], @nextVid[6], 6)                                                 'Copy the second enqueued video into the next enqueued video spot
                  byteFill(@nextVid[6], 0, 6)                                                           'and clear out the secondary queue
     
            else                                                                                        'Play audio and video together? syncNext = 1
            
               if sfxSamples[nextSound[0]] == 0                                                            'The SFX channel the queue wants to use is clear?
                  playVideo(nextVid[0], nextVid[1], nextVid[2], nextVid[3], nextVid[4], nextVid[5], 3)         'Run video
                  playSFX(nextSound[0], nextSound[1], nextSound[2], nextSound[3], nextSound[4])           'Run audio                   
                  syncNext := 0                                                                           'Clear flags
                  
                  byteFill(@nextVid[0], 0, 6)                                                              'Erase the enqueued video  
               
                  if nextVid[11]                                                                           'BUT if there's another video in the secondary queue...
               
                     byteMove(@nextVid[0], @nextVid[6], 6)                                                 'Copy the second enqueued video into the next enqueued video spot
                     byteFill(@nextVid[6], 0, 6)                                                           'and clear out the secondary queue
                  
                  byteFill(@nextSound[0], 0, 5)                                                           'Erase the SFX queue                
       
               else                                                                                       'A sound is still playing?
               
                  syncNext := 10                                                                          'Set flag that when channel finishes, the A/V sync will be started
                  Mode := 0                                                                               'Allow DMD to show default score display until then    
     
          else
     
             if numberFlash                             'Flash a value after video ends?
             
                Mode := 9
                
             else
   
                               
                                if scoreVideo[0]                                                        'Run a video + numbers to show a customized score?

                                   if scoreVideo[8]                                                     '2 layers of score display?

                                      byteMove(@DMDToFind[0], @scoreVideo[0], 16)                       'Copy 16 parameters into DMD search
                                      currentDirectorySector0[4] := folderSector1[DMDToFind[0] - 65]          'Get the directoy sector for the first video we're looking for     
                                      vidWhichLoading := 0                                                    'Set that we're loading video #0 first   
                                      DMDsearch := 8   

                                   else
                                   
                                       playVideo(scoreVideo[0], scoreVideo[1], scoreVideo[2], scoreVideo[3] | loopVideo, 0, 0, 8)  'Run single layer Score Custom Video with attribute bits set but priority 0 (anything can override)
                                 
                                else

                                   vidFrameCounter := 0                    'Reset what is shown if numbers taking up corners  
                                   Mode := 0                               'Display scores or attract mode          
                                 
{-
  byteMove(@scoreVideo[command[pointer] * 8], @command[pointer + 1], 8)   'Copy the parameters to the Score Display default 
 
else
 
  byteMove(@DMDToFind[command[pointer] * 8], @command[pointer + 1], 8)      'Copy the parameters 
 
  if command[pointer + 9]                                                   'Last parameter is the DMD search flag. If not 0, set flags to start loading videos
   
    currentDirectorySector0[4] := folderSector1[DMDToFind[0] - 65]          'Get the directoy sector for the first video we're looking for     
    vidWhichLoading := 0                                                    'Set that we're loading video #0 first   
   
  DMDsearch := command[pointer + 9]                                         'Set the actual flag last so the DMD kernel has all the info it needs
 


DMDToFind[0] := char0                                 'This is the file/droid we're looking for
DMDToFind[1] := char1
DMDToFind[2] := char2
DMDToFind[3] := vidAtt                                'We store these so they'll only be activated once DMD file is found
DMDToFind[4] := %10000101                             'Standard play forward
DMDToFind[5] := 64                                    'Centered video
DMDToFind[6] := 32
DMDToFind[7] := newPriority                           'If the video is found, this will be its new priority                  
 
 
-}                                  

PUB FindDMDEntry | startingSector                                                                       'This runs during the kernel to find the specified DMD file

  'bufferClear

  startingSector := getFileSector(DMDToFind[0], DMDToFind[1], DMDToFind[2], currentDirectorySector0[4]) + 1                     'Look for this filename, and if we get a valid sector number, skip the Single Sector File Header

  if startingSector == 1                                                                                'Didn't find the file? (returned a zero but remember we added a ONE)

    if sectorScan == 0                                                                                  'Did it find nothing at start of sector?
    
      DMDsearch := 0                                                                                    'That means file isn't there - end search. Clearly user error!
      
    else
    
      currentDirectorySector0[4] += 1                                                                   'Advance to next directory sector it will search on the next cycle     

    return
                                                                                                       '-1 the number so count is logical, ie 30 frames is frames 0-29                                                                                                          
  vidAttributes[vidWhichLoading] := DMDToFind[3]                                                                               'Set new attributes 
  vidPlay[vidWhichLoading] := DMDToFind[4]
  vidXpos[vidWhichLoading] := DMDToFind[5]
  vidYpos[vidWhichLoading] := DMDToFind[6]
  vidPriority := DMDToFind[7]

  vidStartingSector[vidWhichLoading] := startingSector                                                                  'If video loops, we can immediately start it again (no file search)
  vidCurrentSector[vidWhichLoading] := startingSector
  vidTotalSectors[vidWhichLoading] := (blockToLong0(sectorScan + 28) >> 9) - 9                          'File size / 512 = total sectors (remember to subtract 1 because of the data sector)

  if DMDToFind[8]                                                                                       'Trying to load 2 videos?

    vidWhichLoading := 1                                                                                'Now load the next video

    byteMove(@DMDToFind[0], @DMDToFind[8], 8)                                                           'Copy the second half down the first half
    byteFill(@DMDToFind[8], 0, 8)                                                                       'Erase second half

    currentDirectorySector0[4] := folderSector1[DMDToFind[0] - 65]                                      'Start our search at beginning of the directory of the first letter (such as _DA, _DB...)
 
  else                                                                                                  'Only one video, start platyin

    AnimateScoreReset
  
    Mode := DMDsearch                                                                                   'Set Video Mode active 
  
    DMDsearch := 0                                                                                        'Clear flag since we've found the file. We do this last so score doesn't overwrite it

    DMDframeTimer := cnt                                                                                  'FRAME RATE DEPENDENT!!!!  




PUB clearQueue                                                                  'Flush the video queue

  byteFill(@nextVid[0], 0, 12) 



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

      playVideo(nextVid[0], nextVid[1], nextVid[2], nextVid[3], nextVid[4], nextVid[5], 3)                 'Run video
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


PUB settingsLoad(endingWhite, startingDMD, pixelShape, brightness, DMDinterrupt)              'ADD WHITE STRIPE BRIGHTNESS!!!!!!!!

  if (DMDinterrupt)

    settingsRequest := 1
     
    repeat while settingsRequest == 1

  outa[16]~~

  bufferD[0 + screenBase] := endingWhite >> 8
  bufferD[1 + screenBase] := endingWhite & $FF
  
  bufferD[2 + screenBase] := startingDMD >> 8
  bufferD[3 + screenBase] := startingDMD & $FF

  bufferD[4 + screenBase] := pixelShape >> 8
  bufferD[5 + screenBase] := pixelShape & $FF

  bufferD[6 + screenBase] := brightness >> 8
  bufferD[7 + screenBase] := brightness & $FF  

  dmd.Load(8)

  outa[16]~
  
  settingsRequest := 0

  

PUB Load

  'SmallNum(audio.getActiveChannels, 0, 27)
  
  dmd.Load(4096)
  
  'byteMove(@bufferD + 2048, @bufferD, 2048)                       'Copy lower half of buffer into top half


PUB Clear

  byteFill(@bufferD + screenBase, 0, 4096)


PUB bufferClear

  byteFill(@dataBlock0, 0, 512)
  

PUB videoSearchClear

  'vidStartingSector[0] := 0
  'vidStartingSector[1] := 0
    
  byteFill(@DMDToFind[0], 0, 16)                        'Clear the Video Load parameters  


PUB Blink(b_times, b_speed)

dira[status_LED]~~

repeat b_times

  OUTA[status_LED]~~
  Pause(b_speed)
  OUTA[status_LED]~
  Pause(b_speed)       

  

PUB Pause(time)

'Pause execution for TIME milliseconds

waitcnt((time * 104_000) + cnt)



PUB Plot(xPos, yPos, plotColor)

  bufferD[screenBase + xPos + (ypos << 7)] := plotColor




PUB Graphics | totalNumbers, g                                                  'See if anything should be overlaid on the video or score display

  timerNumberActive := 0                                                        'Assume it isn't unless we find a number that says it is

  if Mode == 8                                                                  'Need to build a display using the 4 persistent Score Display Numbers?

    totalNumbers := 11                                                          'From 0-11

  else

    totalNumbers := 7

  repeat g from 0 to totalNumbers                                                         'Display any/all active numbers, including Timer Number

    if graphicPriMatch[g] == 0 or graphicPriMatch[g] == VidPriority             'If 0 (what almost everything will have) or equal to priority
                                                                                'Pops will use 249 and actually set this value, meaning those numbers will only work on pops. I hope.
      case graphicType[g]
       
        1 : 'Numbers command?
       
          Numbers(g)
       
        2 : 'Progress bar?
       
          if vidAttributes[0] & allowBar                                                    'Can show a progress bar?
           
            drawProgressBar(g)
       
        3 : 'Character Sprite?
       
          drawSprite(g)  




PUB drawSprite(which) | g, buffPos, numPos, startingRow, startingByte, whichCharacter  

  whichCharacter := graphicValue[which] & %1111_1111

  if whichCharacter > 95
    whichCharacter -= 32

  if whichCharacter < 32
    return 0

  buffpos := (graphicX[which] >> 1) + (graphicY[which] << 6) 

  numPos := 0

  whichCharacter -= 32                                                          'Lobb off first 32 bytes of ASCII

  startingRow := whichCharacter >> 4                                            'Find startingRow (0 - 3)

  startingByte := (startingRow << 9) + ((whichCharacter - (startingRow * 16)) << 2)

  g := 0
  
  repeat graphicValue[which] >> 24

    if (graphicAtt[which] & returnPixels) and (g < 8)                           'Flag to return the pixels that the character draws over?

      outBuffer[g] := bufferD[buffPos + 1]                                      'Copy center left pixel pair
      outBuffer[g + 8] := bufferD[buffPos + 2]                                  'Copy center right pixel pair
      g += 1                                                                    'Advance counter

    byteMove(@bufferD[buffPos], @font[startingByte + numPos], 4)                'Each alphabet character is 2 bytes long
    buffPos += 64
    numPos += 64
    
    if buffPos > 2048                                                           'Avoid scrolling off bottom of screen
      quit    




PUB drawProgressBar(which) | gg


  gg := screenBase + (graphicY[which] << 7) + graphicX[which]                              'Left to right byte offset 

  repeat 5 '(graphicValue[which] & %1111_1111)                                                                         'LSB is the height of the bar
  
    byteFill(@bufferD + gg, graphicAtt[which], graphicValue[which] >> 24)                                       'MSB is the length, fill that many across
    
    gg += 128                                                                                         'Jump to next line

    if gg > (screenBase + 4095)                                                             'Don't draw past bottom of screen

      quit


PUB Numbers(g) | subCommand, FXcommand, gg, numberTemp

    numberTemp := graphicValue[g]                                                 'What number to display

    case graphicAtt[g] >> 5                                                     'Analyze the top 3 bits to find Attributes 
     
      %001 : 'Blink the number?
     
        if vidFrameCounter & %00000001                                         'Flash on odd video frame numbers
          return                                                                  'Return, don't draw numbers this frame

      %010 : 'Display a Player's score as the current number?

        numberTemp := PlayerScore[graphicValue[g]]                               'Set the NumberTemp to that player's score

    if vidAttributes[0] & allowLarge                                                    'If we are allowing large numbers, check for those...
    
      case graphicAtt[g] & %00001111                                            'Lower 4 bits is the type of number
       
        1 : 'Large number?

          drawNumber(0, numberTemp, graphicX[g], graphicY[g])
        
          'LargeNum(numberTemp, graphicX[g], graphicY[g])

    if vidAttributes[0] & allowSmall                                                    'If we are allowing small numbers, check for those... 

      case graphicAtt[g] & %00001111                                            'A lot of ways to use small numbers. Check for them all

        2 : 'Small number?   

          drawNumber(1, numberTemp, graphicX[g], graphicY[g])

          timerNumberActive := 1

        3 : 'Small numbers, upper left and right corners?   

          drawNumber(1, numberTemp, 0, 0)                      'Left side number, always at X = 0
          drawNumber(1, numberTemp, 128, 0)

          timerNumberActive := 1       

        4 : 'Small numbers, lower left and right corners?   

          drawNumber(1, numberTemp, 0, 27)                      'Left side number, always at X = 0
          drawNumber(1, numberTemp, 128, 27)

          timerNumberActive := 1     

        5 : 'Small numbers, all four corners?   

          drawNumber(1, numberTemp, 0, 0)                      'Left side number, always at X = 0
          drawNumber(1, numberTemp, 128, 0)
         
          drawNumber(1, numberTemp, 0, 27)                      'Left side number, always at X = 0
          drawNumber(1, numberTemp, 128, 27)

          timerNumberActive := 1     

        6 : 'Small number, with Double Zeros for a score?   
          
          doubleZero := 1           
          drawNumber(1, numberTemp, graphicX[g], graphicY[g])
          doubleZero := 0   

        8 : 'Show all 4 player numbers on right side for Match?
          
          doubleZero := 1
         
          repeat gg from 1 to NumberPlayers  
         
            drawNumber(1, PlayerScore[gg], graphicX[g], graphicY[g] + (graphicValue[g] * (gg - 1)))
          
          doubleZero := 0
         
          if vidFrameCounter & %00000001                             'Flash the last 2 characters of each score (every other frame)
         
            numberTemp := screenBase + (116 - graphicX[g])            'Go 2 small numbers LEFT of the ending digitis

            repeat 32
              byteFill(@bufferD + numberTemp, 0, 12)
              numberTemp += 128

        9 : 'Draw Ball # on display? 

          showBall(graphicX[g], graphicY[g])
          
        10 : 'Draw Credits / Freeplay on display? 

          showCredits(graphicX[g], graphicY[g])

          


PUB showNumberFlash

  Clear

  if numberFlashTimer & %0000_0010
    drawNumber(1, numberFlash, 255, 6)

  Load

  Pause(50)
  
  numberFlashTimer -= 1

  if numberFlashTimer == 0                                   'Done?

    numberFlash := 0                                    'Clear Numberflash, revert to default mode
    mode := 255




PUB GraphicFlush(startFlush, endFlush) | g                                                                                     'Clears out all numbers (except timer numbers)

  repeat g from startFlush to endFlush

    if (graphicAtt[g] & numberStay) == 0

      graphicType[g] := 0
      graphicAtt[g] := 0   
      graphicX[g] := 0
      graphicY[g] := 0
      graphicValue[g] := 0
      graphicPriMatch[g] := 0
 


PUB Score | numDigits, columnSize, pCount, sX, sY, g

  Clear                                                 'Clear bufferD

  vidAttributes[0] := allowSmall                        'Only numbers allowed in Score display are small ones (like in the corners)

  Graphics                                              'Do this first to get status of current numbers

  g := 0                                                'Flag to show up to 4 normal scores UNLESS...

  if attractMode                                        'Don't show # of balls in attract mode
  
    showCredits(75, 27)    

  else
       
    if timerNumberActive                                  'Timer numbers in the corners?
    
      g := 1                                              'Flag to draw single score

      vidFrameCounter += 1

      if vidFrameCounter < 20

        showBall(46, 27)                                  'Show current Ball # and Credits normally

      if vidFrameCounter > 19 and vidFrameCounter < 40

        text(string("PLAYER"), 40, 27)
        drawNumber(1, CurrentPlayer, 82, 27)   

      if vidFrameCounter > 39

        showCredits(37, 27)  

      if vidFrameCounter > 59
        
        vidFrameCounter := 0  

    else
    
      g := 0
      
      showBall(0, 27)                               'Show current Ball # and Credits normally 
      showCredits(75, 27)


  doubleZero := 1                                       'Drawing scores, allow DOUBLE ZEROS 
  
  repeat pCount from 1 to NumberPlayers
    if PlayerScore[pCount] > 999_999_999 and attractMode == 0 or NumberPlayers == 1                   'If any player has a 10 digit score, only show 1 score at a time
      g := 1                                                                    'In attract mode we show 4 small scores, so they'll fit no problem
 
  if g == 0                                                                          'For large scores, single player or during a countdown, show current player score only
     
    repeat pCount from 1 to NumberPlayers                                       'Put all player scores in corners (like Baby)   
   
      sX := 0
      sY := 0                                                                   'Show LARGE number for active player

      if pCount == CurrentPlayer

        sY := 6

        if pCount == 2 or pCount == 4                                           'Player 2 or player 4 scores that need to be on the Right?
          sX := 128                                                             'Set score to right justify
        if pCount == 3 or pCount == 4                                           'Player 3 or Player 4 scores that need to be on the Bottom?
          sY := 7        

        drawNumber(0, PlayerScore[CurrentPlayer], sX, sY)

      else
     
        if pCount == 2 or pCount == 4                                           'Player 2 or player 4 scores that need to be on the Right?
          sX := 128                                                             'Set score to right justify
        if pCount == 3 or pCount == 4                                           'Player 3 or Player 4 scores that need to be on the Bottom?
          sY := 21
           
        drawNumber(1, PlayerScore[pCount], sX, sY)

  else

    drawNumber(0, PlayerScore[CurrentPlayer], 255, 9)  
     
  doubleZero := 0 

  AnimateScore(3)                                                               'Animate the larger numbers
  
  Pause(33)                                                                     'Locks the Score update to 30 FPS, like video

  Load                                                                          'Load bufferD onto display


PUB AnimateScore(numSectors)


  DMDReadSector := numberSectorCurrent

  blitterOrNot := 0
  
  targetAddress := @font[numberFillPointer]
  requestSector := numSectors                                                   'Request that the SD card loop loads a video frame

  repeat while requestSector                                                    'Wait until a frame loads

  if numSectors == 3                                                            'Full frame load? (during score display)

    numberSectorCurrent += 8                                                    'Increment to next frame
     
    numberCounter += 1                                                          'Increment counter

  else                                                                          '1/3 load speed while a video is playing?

    numberFillPointer += 512                                                    'Advance one sector ahead in memory
    numberSectorCurrent += 1                                                    'Advance to next sector
    
    if numberFillPointer > 1535                                                 'Did we load 3 sectors?

      numberFillPointer := 0                                                    'Reset the memory pointer
      numberSectorCurrent += 5                                                  'Increment to next frame       
      numberCounter += 1                                                        'Increment frame counter  
   
  if numberCounter => numberTotalFrames
   
    numberCounter := 0
   
    numberSectorCurrent := numberSectorStart


PUB AnimateScoreReset                                                           'Call this when starting Score or a video, so it doesn't get lost on a intraframe

  numberFillPointer := 0                                                        'Reset fill pointer
  numberSectorCurrent := numberSectorStart                                      'Reset sector pointer to beginning of file
   


PUB LoadAlpha(whichCharacter, xPos, yPos, height) | buffPos, numPos, startingByte

  if whichCharacter == 32                                                       'Space? Do nothing!

    return
  
  buffpos := screenBase + xpos + (ypos << 7)                                                 'Figure out where text starts in memory 

  numPos := 0

  if whichCharacter > 67
   
    startingByte := 640 + ((whichCharacter - 68) * 5) 
   
  else
   
    startingByte := ((whichCharacter - 43) * 5)

  startingByte += 1664                                                          'Hard-cord this in when everything works
  
  repeat height

    byteMove(@bufferD[buffPos], @font[startingByte + numPos], 5)                'Each alphabet character is 2 bytes long
    buffPos += 128
    numPos += 128



PUB text(strPointer, xPos, yPos)

  repeat while byte[strPointer]

    LoadAlpha(byte[strPointer++], xPos, yPos, 5)

    xPos += 6


PUB hex(value, digits, xPos, yPos)

  value <<= (8 - digits) << 2
  
  repeat digits
  
    LoadAlpha(lookupz((value <-= 4) & $F : "0".."9", "A".."F"), xPos, yPos, 5)

    xPos += 6



PUB drawSwitches | xPos, yPos, whichSwitch, checkBit

  xPos := 29
  yPos := 1

  repeat whichSwitch from 0 to 7

    checkBit := %00000001
    
    repeat 8
  
      if switchDisplay[whichSwitch] & checkBit
       
        Plot(xPos, yPos, 255)
        Plot(xPos + 1, yPos, 255)
        Plot(xPos, yPos + 1, 255)
        Plot(xPos + 1, yPos + 1, 255)

      checkBit <<= 1

      yPos += 4

    xPos -= 4
    yPos := 1

  xPos := 37
  
  repeat whichSwitch from 8 to 9

    checkBit := %00000001
    
    repeat 8
  
      if switchDisplay[whichSwitch] & checkBit
       
        Plot(xPos, yPos, %11100000)
        Plot(xPos + 1, yPos, %11100000)
        Plot(xPos, yPos + 1, %11100000)
        Plot(xPos + 1, yPos + 1, %11100000)     

      checkBit <<= 1

      yPos += 4

    xPos -= 4
    yPos := 1



    
PUB drawNumber(whatSize, whatNumber, xPos, yPos) | x, xx, info, numerals, commas, buffpos, numPos           '0 = big score number 1 = small text sized number

  info := SuperNum2String(whatSize, whatNumber)                      'Convert the number to a string, get # of numerals and commas

  numerals := info & %00001111                          'LSB's hold number of numerals
  commas := (info & %1111000) >> 4                      'MSB's hold number of commas

  if xPos == 255                                                                                        'Flag to auto-center the number?
    xPos := (128 - ((numerals * numeralLength2[whatSize]) + (commas * commaLength2[whatSize]))) >> 1    'Center onscreen. Number of bytes wide - (width size of number in bytes) / 2
   
  if xPos > 127 and xPos < 255 ' == 128                                                                                        'Justify right?
    xPos := 129 - (xPos - 128) - ((numerals * numeralLength2[whatSize]) + (commas * commaLength2[whatSize]))

  repeat x from 0 to (numerals + commas) - 1

    xx := (numberString[x] >> 4) + 1                                                                    'Use to find commas. We do the +1 so XX can also be used to increment the X position (adds pixel space between numerals)
    
    buffpos := screenBase + ((ypos & %111111) << 7) + xPos                                                                       'Find starting byte of current character                      

    if (xx == 4 or xx == 3) and whatSize == 0                                                           'Spacing of a comma detected? On large numbers, drop the comma down a line so it looks better.

      buffPos += 128

    numPos := whatSize * 1664

    info := numberString[x] & %1111
    
    repeat numberHeight[whatSize]                                                                       'Repeat # of lines tall the character is 

      repeat (yPos >> 6) + 1
    
        byteMove(@bufferD[buffPos], @font + (info * numeralLength[whatSize]) + numPos, numeralLength[whatSize])                 'Each small character file is 7 bytes long, including the 2 X Y size header bytes
       
        buffPos += 128                                                                                    'Increase bufferD one 128

      numPos += 128

    xPos += xx




PUB SuperNum2String(whatSize, numConvert) | div, z_pad, idx, size, column, actualCommas 

  div := 1_000_000_000                                  ' initialize divisor
  z_pad~                                                ' clear zero-pad flag
  bytefill(@numberString[0], 0, 14)                        ' clear string to zeros
  idx~
  size := 0                                             ' reset index
  actualCommas := 0                                     'How many commas we put in
  column := 10                                          'Used for finding commas

  if numConvert == 0 and doubleZero == 1                                                                'Number is a zero, and Double Zeros enabled?  (usually for scores)
    numberString[0] := zeroChar[whatSize] | (numeralLength[whatSize] << 4)                              'Double zeros
    numberString[1] := zeroChar[whatSize] | (numeralLength[whatSize] << 4)                              'Double zeros   
    return 2                                                                                            'Return that we have 2 zeros and no commas   

  repeat 10                                                                                             'Max 9,999,999,999
  
    if (numConvert => div)                                                                              ' printable character?
      numberString[idx++] := ((numConvert / div) + zeroChar[whatSize]) | (numeralLength[whatSize] << 4)                  'yes, print ASCII digit
      numConvert //= div                                                                                ' update value
      z_pad~~
      size += 1

      if column == 10 or column == 7 or column == 4                                                    'Should we add a comma?
        numberString[idx++] := commaChar[whatSize] | commaLength[whatSize] << 4                                           'Add a column 
        actualCommas += 1
      
    elseif z_pad or (div == 1)                                                                          'printing or last column?
      numberString[idx++] := zeroChar[whatSize] | numeralLength[whatSize] << 4 
      size += 1
      if column == 10 or column == 7 or column == 4                                                     'Should we add a comma?
        numberString[idx++] := commaChar[whatSize] | commaLength[whatSize] << 4                                           'Add a column
        actualCommas += 1

    column -= 1                                                                                         'Column decrements no matter what, so we can find commas  
    div /= 10
 
  return size | (actualCommas << 4)                                                                     'Combine size and # of commas into 1 returned value




PUB showBall(xpos, ypos)


  if ball                                               'To hide Ball #, set Ball to 0

    text(string("BALL"), xPos, yPos)  

    drawNumber(1, Ball, xPos + 30, yPos)



PUB showCredits(xPos, yPos)                                         'Put either "Free Play" or "Credits: X" on lower right side of display


  if credits & %1000_0000                                                       'Freeplay bit set?

    text(string("FREE PLAY"), xPos, yPos)
 
  else

    if credits & %0111_1111 > 9                                                 'Two digits? 

      xPos -= 6                                                                 'Shift to the left to make room  

    text(string("CREDITS"), xPos, yPos)
    drawNumber(1, credits & %0111_1111, xPos + 48, yPos)                         'Show number of credits, masking off Freeplay bit



PUB showError                                                                   'Load a bitmap message from RAM if SD card can't boot

  PlayerScore[0] := screenBase
  PlayerScore[1] := 0
  
  repeat 5
  
    byteMove(@bufferD + PlayerScore[0], @message + PlayerScore[1], 47)                   'Read in one line of the image (20 bytes)   
    PlayerScore[1] += 47
    PlayerScore[0] += 128                                                               'Go down one scaneline

  Load   
  repeat
    

PUB playVideo(char0, char1, char2, vidAtt, unused, newPriority, targetMode)        'This is called by the main cog.

  if newPriority < VidPriority                          'New video lower priority than what's already playing?
    return 0                                            'Abort

  if char0 < 65 or char0 > 90                           'Only A-Z are allowed
    return 0                                            '...RETURN 0, sound can't be played

  videoSearchClear                                      'Clear the previous search
  
  if (vidAtt & noEntryFlush) == 0                       'Don't flush graphics on entry to video?

    GraphicFlush(0, 7)

  DMDToFind[0] := char0                                 'This is the file/droid we're looking for
  DMDToFind[1] := char1
  DMDToFind[2] := char2
  DMDToFind[3] := vidAtt                                'We store these so they'll only be activated once DMD file is found
  DMDToFind[4] := %10000101                             'Standard play forward
  DMDToFind[5] := 64                                    'Centered video
  DMDToFind[6] := 32
  DMDToFind[7] := newPriority                           'If the video is found, this will be its new priority                  
      
  currentDirectorySector0[4] := folderSector1[char0 - 65]  'Start our search at beginning of the directory of the first letter (such as _DA, _DB...)

  vidWhichLoading := 0
  
  DMDsearch :=  targetMode '3                                        'Set flag that we're looking for a DMD on the SD card. It's also the mode the game should switch to once DMD loads



PUB stopVideo

  'ALSO NEEDS TO KILL VIDEO QUEUE, ONCE WE GET THAT BACK IN

  byteFill(@nextVid[0], 0, 12)                          'Erase both video queues
  byteFill(@nextSound[0], 0, 5)
  syncNext := 0 

  videoSearchClear                                      'Clear the Video Load parameters 

  if (vidAttributes[0] & noExitFlush) == 0
    GraphicFlush(0, 7)  
       
  vidAttributes[0] := %0000_0011                                'Default is ALLOW ALL NUMBERS, no repeats
  VidPriority := 0                                      'Allow any video to start up

  vidStartingSector[1] := 0                              'Kill second video in case there was one running

  Mode := 255                                            'Let DMD kernel revert back to whatever it was doing   

  

PUB getFrame(sectorAddress, xPos, yPos, blitterWhatState) | sectorsToLoad, yRemainder

  'if yPos < 1 or yPos > 63                                                      'If scrolled this far it's just blank so do nothing

    'return sectorAddress + 8                                                    'No matter how many sectors we actually loaded, jump ahead 8 sectors in the counter (full frame)

  yRemainder := yPos & %11                                                      'Used for positions off the top of the screen < 32

  if yPos > 31                                                                  'Image centered on screen, or lower?

    sectorsToLoad := ((63 - yPos) >> 2) + 1                                     'Only load as many sectors as will actually appear onscreen
    targetAddress := @bufferD + 384 + xPos + ((yPos - 32) << 7)                 'Compute location of where the image starts

    DMDReadSector := sectorAddress                                              'Start at beginning of sector frame

  else                                                                          'Imagine is higher on the screen

    sectorsToLoad := (yPos + 3) >> 2                                            'Must always load at least 1 sector (add 3 for remainder)

    DMDReadSector := sectorAddress + (8 - sectorsToLoad)                        'How many sectors to offset (don't need to load them because we aren't drawing them, they're off the top of the screen)

    if yRemainder                                                               'Data loaded above the start of the frame? Offset by remainder

      targetAddress := @bufferD + xPos + ((yRemainder - 1) << 7)

    else                                                                        'If it's even on 4's, load at start of frame.
      
      targetAddress := @bufferD + xPos + 384


  blitterOrNot := blitterWhatState                          'Set blitter as requested

  requestSector := sectorsToLoad                        'Request that the SD card loop loads a video frame

  repeat while requestSector                            'Wait until a frame loads

  blitterOrNot := 0                                     'Reset blitter in case we forget to elsewhere

  return sectorAddress + 8                              'No matter how many sectors we actually loaded, jump ahead 8 sectors in the counter (full frame)

  
  

PUB videoStream | numberVideos, g, whichDirection, loopTarget       'Streams video directly from the SD card to display buffer

  loopTarget := 0

  if cnt < DMDframeTimer                                'This runs in-line with the rest of the DMD kernel
    return Mode                                         'Return back with the current status of Mode (so we don't override it)
 
  if vidStartingSector[1]                               'Two videos playing at same time?

    DMDframeTimer := cnt + 6_933_333                    '104MHZ / 15 FPS
    numberVideos := 1

  else
  
    DMDframeTimer := cnt + 3_466_666                    '104MHz / 30 FPS
    numberVideos := 0     

  vidFrameCounter += 1

  AnimateScore(1)
  
  Clear

  Graphics
  
  'DRAW SCORES AND THINGS HERE

  'drawNumber(1, loopStart[3] + sfxStartingSector[3], 0, 0)
  
  'drawNumber(1, sfxSector[3], 0, 6)

  'drawNumber(1, loopEnd[3] + sfxStartingSector[3], 0, 12)  

  'drawNumber(1, fadeSpeed, 128, 0)


  'drawNumber(1, vidStartingSector[0], 5, 0)  
  'drawNumber(1, vidCurrentSector[0], 5, 6) 
  'drawNumber(1, vidStartingSector[0] + vidTotalSectors[0], 5, 12)

  'drawNumber(1, sfxSector[0], 0, 0)
  'drawNumber(1, loopEnd[0] , 64, 0)   

  'drawNumber(1, currentMusic[0], 0, 6)
  'drawNumber(1, currentMusic[1], 20, 6) 
  'drawNumber(1, musicOneShot, 40, 6)
      
  'drawNumber(1, sfxSector[2], 0, 12)
  'drawNumber(1, sfxSector[3], 0, 18)
  'drawNumber(1, loopEnd[3], 64, 18) 
  
  'drawNumber(1, currentDirectorySector0[4], 0, 24)
   

  'drawNumber(1, vidCurrentFrame[0], 0, 0) 
  'drawNumber(1, vidTotalFrames[0], 0, 6)

'videoDirection  =               %1000_0000              'Bit that sets which direction the video plays
'videoPlay       =               %0000_0100              'Bit that tells the video to advance
'videoStepper    =               %0000_1100              'OR'ing the vidPlay variable with this will advance 1 step
'videoOneShot    =               %0000_1000              'If this bit is set, we clear the one shot and the stepper, so video won't advance until we OR in "videoStepper" again
'videoStepMask   =               %1111_0011              'After a one-shot advance, AND vidPlay with this to clear the stepper bits


  'numberVideos := 0
  
  repeat g from 0 to numberVideos

    getFrame(vidCurrentSector[g], vidXpos[g], vidYPos[g], 1)

    if g == numberVideos                                                        'Last layer? Send it to the display

      Load

    if vidPlay[g] & videoPlay                                                   'Video should advance?

      if vidPlay[g] & %1000_0000                                                'Forward?

        vidCurrentSector[g] += (vidPlay[g] & %0000_0011) << 3                   'Advance sectors by that amount * 8

        if vidCurrentSector[g] > vidStartingSector[g] + vidTotalSectors[g]      'Did we go past the end of the file?

          loopTarget := vidStartingSector[g]                                    'Set target to the beginning of the file (if we choose to loop)
        
      else

        vidCurrentSector[g] -= (vidPlay[g] & %0000_0011) << 3                   'Advance sectors by that amount * 8
      
        if vidCurrentSector[g] < vidStartingSector[g]                           'Did we go past the end of the file? 

          loopTarget := vidStartingSector[g] + vidTotalSectors[g]               'Set target to the beginning of the file (if we choose to loop) 

      if vidPlay[g] & videoOneShot                                              'Was the one-shot bit set?

        vidPlay[g] &= videoStepMask 

      if loopTarget                                                             'Did we go past the beginning or end of file?

        VidPriority := 0                                                        'Set No Priority. If a video loops, it will have 0 priority, anything can override it                
       
        if vidAttributes[g] & loopVideo                                         'Supposed to loop video?
                                                          '                     Run same video again, same parameters except Video Priority 0, so ANYTHING can override it
          vidCurrentSector[g] := loopTarget                                     'vidStartingSector[g]           'Reload starting sector

          return                                                                'Return that a video is still playing
       
        if (vidAttributes[g] & noExitFlush) == 0
       
          GraphicFlush(0, 7)                                                    'Video ALWAYS flushes graphics on video end

        videoSearchClear                                                        'Clear the Video Load parameters  
        
        vidAttributes[g] := %0000_0011                                          'Default is ALLOW ALL NUMBERS, no repeats

        Mode := 255                                                             'Video is done!




PUB loadSprite(char0, char1, char2) | offset, startingSector                     'This is called by the main cog.

  currentDirectorySector0[4] := folderSector1[char0 - 65]          'Start our search at beginning of the "_DZ" directory

  startingSector := 0   

  repeat while startingSector == 0
  
    sd0.readSector(currentDirectorySector0[4]++, @dataBlock0, 1, 0)                                               'Read in a sector of the directory
    
    if dataBlock0[0] == 0                                                                                 'Empty sector?
      return 255                                                                                          'Return MERCY FILE NOT FOUND code
      
    repeat offset from 0 to 480 step 32                                                                   'Check all 16 entries in this block
     
      if dataBlock0[offset] == char0 and dataBlock0[offset + 1] == char1 and dataBlock0[offset + 2] == char2 
        startingSector := ((((blockToWord0(offset + 20) << 16) | blockToWord0(offset + 26)) - 2) << 6 ) + card0DirectorySector + 1

  return sd0.readSector(startingSector, @bufferD, 8, 0)                          'Load 8 sector color Sprite into memory and return the location of the next sector




PUB LoadNumbers | g                                                             'Load large numerals from DMD SD card into memory

  numberSectorStart := loadSprite("ZMB") - 8             'Load Bitmap fonts into memory and get the sector pointer for Animated Numbers
  numberSectorCurrent := numberSectorStart
  numberFillPointer := 0
  numberCounter := 0
 
  'dmd.Load(4096)                                        'Put it into the display (for testing purposes only)

  'Pause(1000)
  
  byteMove(@font, @bufferD, 2944)                       'Copy it into the numberLarge variable (RAM) (23 lines by 128 bytes wide)
 
  'Clear


PUB showVersions | xPos                                'Gets the CODE version from EEPROM and displays them on startup

  Clear
  
  PlayerScore[4] := 0                                   'Use this for name cursor position

  repeat eepromAddress from 128 to 131

    eepromData := readLong($A0, eepromAddress) <- 8     'Get 4 bytes, shift position 

    repeat xPos from 0 to 3
    
      LoadAlpha(eepromData & $FF, PlayerScore[4], 0, 5)

      PlayerScore[4] += 6                               'Increment cursor
      
      if eepromAddress == 128                           'First 4 characters? Copy them to the SHORT NAME BUFFER (we only use first 3 but this is easier to code)

        shortName[xPos] := eepromData & $FF             'We use this code to make sure we load the correct BIN file for updating (first 3 characters must match!)

      eepromData <-= 8                                  'Shift the long to get the next byte

  shortName[3] := 0
  
  eepromData := readLong($A0, $0000) >> 24              'Always contains the checksum and code version. Rotate to leave just the leftmost byte (game code version #)

  'text(string("+,-./012345678"), 0, 6)
  'text(string("A CDEFGHIJK"), 0, 12)     
  
  text(string("CODE VERSION:"), 0, 12)
  drawNumber(1, eepromData, 78, 12)
  text(string("A/V VERSION:"), 6, 18)
  drawNumber(1, AVversion, 78, 18)

  'Draw all this onscreen but we may not load it if there's a file update...

  


PUB BuildFolderList(fChar0, fChar1, fChar2, arrayPointer) | g, currentDirectorySector                                                      'Read the root directory and make a list of starting sectors for all folders


  currentDirectorySector := getFileSector(fChar0, fChar1, fChar2, card0DirectorySector)                 'Start at the beginning of the Root Directory

  if currentDirectorySector == 0                                                                        'No root directory was found? SD failed to mount!

    showError  
 
  repeat
  
    currentDirectorySector := sd0.readSector(currentDirectorySector, @dataBlock0, 1, 0)                    'Read in a sector of the directory, advance the sector #

    repeat sectorScan from 0 to 480 step 32                                                                   'Check all 16 entries in this block

      if dataBlock0[sectorScan] == 0                                                                          'No more entries - done!
        return

      if dataBlock0[sectorScan] == "_"                                                                  'Valid SFX or SFX Folder Name?

        long[arrayPointer][dataBlock0[sectorScan + 2] - 65] := ((((blockToWord0(sectorScan + 20) << 16) | blockToWord0(sectorScan + 26)) - 2) << 6 ) + card0DirectorySector



PUB getFileSector(char0, char1, char2, searchSector)                                                    'Returns the starting sector of the requested 3 character filename

  bufferClear

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




PUB updateGame | g, gg, x, xx

  commandNumber := 0                                    'Use some random BYTE for this to save memory

  Clear                                                                         'Erase the Version # splash screen

  shortName[3] := 0                                                             'Add zero termination 
  text(@shortName, 0, 0)                                                        'Print name  
  text(string("V    FOUND"), 24, 0)                                             'and version# that we found on SD card root
  drawNumber(1, PlayerScore[1], 30, 0)

  text(string("CONNECT PIC32:"), 0, 6) 
  
  Load

  com.COMEngineStart(atnSpin, clkSpin, 115_200, Comm_Cog)                   'Start up the Serial Driver

  stkSignOn                                                                 'Log into the STK bootloader, which the PIC32 should still be in at boot (we let the Prop boot faster for this very reason) 

  com.receiverFlush                                                         'Flush garbage that we may have gotten waiting for the response

  if command[3] <> "S" and command[4] <> "T"

    text(string("FAIL"), 84, 6)
    text(string("PLEASE RESTART"), 0, 12) 
    Load
    
    repeat

  text(string("OK"), 84, 6)
  text(string("UPADTING IN 5 SECONDS"), 0, 18)

  repeat g from 0 to 5                                  '5 second countdown

    drawNumber(1, 5 - g, 72, 18)
    
    Load 

    Pause(100)

  PlayerScore[7] := (fileSize / 512) + 1                'Total sectors to load

  PlayerScore[4] := PlayerScore[0]                      'Copy the starting sector # into a temp
  
  stkLoadAddress($00)

  repeat g from 0 to PlayerScore[7]

    Clear
    text(string("PROGAMMING GAME CODE"), 0, 0)
    text(string("SECTOR 0   OF"), 0, 6)

    drawNumber(1, g, 42, 6)
    drawNumber(1, PlayerScore[7], 84, 6)  

    PlayerScore[4] := sd0.readSector(PlayerScore[4], @dataBlock0, 1, 0)                'Read in 1 sector 


    stkWrite(@dataBlock0[0], 128, 0)
    stkWrite(@dataBlock0[128], 128, 0)
    stkWrite(@dataBlock0[256], 256, 0)
  
    Load

  byteFill(@dataBlock0[0], $FF, 512)                                            'Fill a ways past the end of our file

  repeat 2

    stkWrite(@dataBlock0[0], 256, 0) 

  PlayerScore[4] := PlayerScore[0]                                              'Copy the starting sector # into a temp
  
  stkLoadAddress($00)                                                           'Reset the flash memory pointer

  PlayerScore[5] := 0
  
  repeat g from 0 to PlayerScore[7]

    Clear
    
    text(string("VERIFYING GAME CODE"), 0, 0)
    text(string("SECTOR 0   OF"), 0, 6)
    text(string("ERRORS"), 0, 27)
    
    drawNumber(1, g, 42, 6)
    drawNumber(1, PlayerScore[7], 84, 6)
    drawNumber(1, PlayerScore[5], 42, 27)      

    Load
    
    PlayerScore[4] := sd0.readSector(PlayerScore[4], @dataBlock0, 1, 0)                'Read in 1 sector 

    xx := 0

    repeat 2                                                                    'Compare 1 sector at a time

      stkRead(256)                                                                'Get 1 page from EEPROM flash

      x := 2                                                                    'Pointer for the response we get
       
      repeat 256

        'text(string("BYTE"), 0, 18)
        'drawNumber(1, xx, 30, 18)     

         if dataBlock0[xx++] <> command[x++]

            PlayerScore[5] += 1
      
        'hex(dataBlock0[xx++], 2, 60, 18)
       
        'hex(command[x++], 2, 78, 18)

        'Load

  Clear

  if PlayerScore[5]

    text(string("VERIFY ERROR"), 0, 0)
    text(string("PLEASE RESTART"), 0, 12)
    Load

    repeat
 
  PlayerScore[0] := getFileSector("PRP", card0DirectorySector)
  
  if PlayerScore[0]                                     'Does a XXX_Vnnn.BIN file exist in the root directory that matches game's short name?

    PlayerScore[1] :=  getVersionNumber
  
    if PlayerScore[1]  > AVversion                    'Filename is higher version # than what is installed per the EEPROM?

      flashEEPROM    

  Clear
  
  text(string("CODE UPDATE COMPLETE"), 0, 0)
  text(string("PLEASE RESTART"), 0, 12)  

  Load

  stkSignOff                                            'Sign off, which will start the PIC32 code

  repeat                                                'Pause the A/V so the user will restart game  



PUB stkSignOn


  clearComBuffer                             'Clear command buffer to see what we get back! 

  command[0] := $01
  
  return sendCommand(1)



PUB stkSignOff


  clearComBuffer                             'Clear command buffer to see what we get back! 
  
  command[0] := $11
  command[1] := $10
  command[2] := $10
 
  return sendCommand(3)

 

PUB stkLoadAddress(whatAddress)


  clearComBuffer                             'Clear command buffer to see what we get back! 


  'text(string("LOAD:"), 0, 0)

  'SmallNum(whatAddress, 64, 1)
  
  command[0] := $06
  command[1] := (whatAddress >> 24) & $FF
  command[2] := (whatAddress >> 16) & $FF
  command[3] := (whatAddress >> 8) & $FF
  command[4] := whatAddress & $FF  
  
  return sendCommand(5)



PUB stkWrite(fromAddress, howManyBytes, testicles) | dummyCounter, g

  'Clear
  'text(string("PRGM:"), 0, 0)

  dummyCounter := 1

  byteFill(@command, 0, 512)                            'Clear command buffer to see what we get back! 

  command[0] := $13
  command[1] := (howManyBytes >> 8) & $FF
  command[2] := howManyBytes & $FF                
  command[3] := $C1
  command[4] := $0A   
  command[5] := $40
  command[6] := $4C 
  command[7] := $20
  command[8] := $FF 
  command[9] := $FF

  if testicles
  
    repeat g from 0 to 255
     
      command[10 + g] := dummyCounter++

  else
  
    byteMove(@command[10], fromAddress, howManyBytes)

  sendCommand(10 + howManyBytes) '266)

  Load


  
PUB stkRead(howManyBytes) | xPos, yPos, g, gg, x

  clearComBuffer                                        'Clear command buffer to see what we get back! 

  'Clear
  'text(string("READ:"), 0, 0)

  command[0] := $14
  command[1] := (howManyBytes >> 8) & $FF
  command[2] := howManyBytes & $FF               
  command[3] := 0
  
  sendCommand(4)

  xPos := 0
  yPos := 8

  g := 2                                                'Skip the answer ID and the Status Command

  Load

  return
  
  Pause(500)
  
  repeat gg from 0 to 7

    Clear
  
    'SmallNum(gg, 55, 1)

    xPos := 0
    yPos := 8
    
    repeat x from g to g + 31
       
      'SmallNum(command[x], xPos, yPos)
 
      xPos += 16
       
      if xPos > 112
       
        yPos += 6
       
        xPos := 0
          
    Load
    
    Pause(1000)
    
    g += 32



PUB sendCommand(numBytes) | commandCheck, g, xPos, yPos


  xPos := 0
  yPos := 0

  commandCheck := command[0]                            'Make a copy now for after the buffer gets cleared for re-entry

  text(string("COM"), 78, 27) 
  hex(commandCheck, 2, 96, 27)                        'Print what command we sent  

  Load
  
  checkSum := 0                                         'Clear checksum

  sendByte($1B)                                         'Always 1B
  sendByte(commandNumber++)                             'Increment command number

  sendByte((numBytes >> 8) & $FF)                       'How many bytes we are sending (in high and low bytes though I doubt we'll send 65535 at once...)
  sendByte(numBytes & $FF)                              'Send token
  sendByte($0E)

  pointer := 0                                          'Reset pointer

  repeat numBytes                                       'Send all the data bytes

    sendByte(command[pointer++])                        'Send byte, increment pointer

  com.writeByte(checkSum)                               'Finally, checksum XOR of all bytes sent, message complete!

  repeat

    if com.receivedNumber > 5                           'Wait until the response comes in, will always have at least 6 bytes. Byte 6 is the command returned back

      quit

  clearComBuffer                             'Clear command buffer to see what we get back! 

  com.readByte                                          'Eat token $1B
  com.readByte                                          'Eat message #
  messLength := (com.readByte << 8) + com.readByte      'Get length of return message (2 BYTES)
  com.readByte                                          'Eat token $OE
  com.readData(@command, messLength + 1)                'Eat up the data and final checksum (thus the plus 1)

  'SmallNum(messLength, 116, 1)
   
  if command[0] == commandCheck and command[1] == $00   'Did we get the response we wanted?

    text(string("OK"), 114, 27)
    Load
    return 1

  else

    text(string("XX"), 114, 27)
    Load
    return 0
     

PUB sendByte(whatByte)                                  'A separate routine so we can XOR the bytes we send

  com.writeByte(whatByte)

  checkSum ^= whatByte


PUB clearComBuffer

  byteFill(@command, 0, 512)                            'Clear command buffer to see what we get back!


PUB flashEEPROM                                                                'Loads a HEX file off the SD card to reprogram the Propeller's EERPOM

  Clear

  text(string("PRP V    FOUND"), 0, 0)                                             'and version# that we found on SD card root
  drawNumber(1, PlayerScore[1], 30, 0)
  
  text(string("PROGAMMING A/V EEPROM"), 0, 6)

  eepromAddress := 0                                                            'Starting page of the EEPROM

  repeat 64                                                                     'Load 64 sectors of data (512 bytes per sector)

    PlayerScore[0] := sd0.readSector(PlayerScore[0], @dataBlock0, 1, 0)                'Read in 1 sector

    eepromData := 0                                                             'The offset of where we are in the dataBlock0
    
    repeat 16                                                                   '32 bytes per page is a very safe assumption of what the EEPROM can do (most can do 64 bytes per page)

      WritePage($A0, eepromAddress, @dataBlock0[0] + eepromData, 32)            'Write page

      Pause(5)                                                                  'Give EEPROM page buffer time to finish write cycle (just to be safe)
      
      eepromData += 32                                                          'Advance 32 bytes into the buffer
      eepromAddress += 32                                                       'Advance 32 bytes into the EEPROM

      frameTimer := 1536 + screenBase
      
      repeat 4                                                                  'Draw progress bar onscreen

        byteFill(@bufferD + frameTimer, $FF, eepromAddress >> 8)

        frameTimer += 128

      Load    
      
  


PUB I2Cinitialize

   outa[28] := 1                       '   reinitialized.  Drive SCL high.
   dira[28] := 1
   dira[29] := 0                       ' Set SDA as input
   repeat 9
      outa[28] := 0                    ' Put out up to 9 clock pulses
      outa[28] := 1
      if ina[29]                      ' Repeat if SDA not driven high
         quit                          '  by the EEPROM


PUB I2Cstart

   outa[28]~~                         ' Initially drive SCL HIGH
   dira[28]~~
   outa[29]~~                         ' Initially drive SDA HIGH
   dira[29]~~
   outa[29]~                          ' Now drive SDA LOW
   outa[28]~                          ' Leave SCL LOW
  
PUB I2CStop

   outa[28]~~                         ' Drive SCL HIGH
   outa[29]~~                         '  then SDA HIGH
   dira[28]~                          ' Now let them float
   dira[29]~                          ' If pullups present, they'll stay HIGH


PUB Write(dataX) : ackbit
'' Write i2c data.  Data byte is output MSB first, SDA data line is valid
'' only while the SCL line is HIGH.  Data is always 8 bits (+ ACK/NAK).
'' SDA is assumed LOW and SCL and SDA are both left in the LOW state.

   ackbit := 0 
   dataX <<= 24
   repeat 8                            ' Output data to SDA
      outa[29] := (dataX <-= 1) & 1
      outa[28]~~                      ' Toggle SCL from LOW to HIGH to LOW
      outa[28]~
   dira[29]~                          ' Set SDA to input for ACK/NAK
   outa[28]~~
   ackbit := ina[29]                  ' Sample SDA when SCL is HIGH
   outa[28]~
   outa[29]~                          ' Leave SDA driven LOW
   dira[29]~~


PUB Read(ackbit): dataX
'' Read in i2c data, Data byte is output MSB first, SDA data line is
'' valid only while the SCL line is HIGH.  SCL and SDA left in LOW state.

   dataX := 0
   dira[29]~                          ' Make SDA an input
   repeat 8                            ' Receive data from SDA
      outa[28]~~                      ' Sample SDA when SCL is HIGH
      dataX := (dataX << 1) | ina[29]
      outa[28]~
   outa[29] := ackbit                 ' Output ACK/NAK to SDA
   dira[29]~~
   outa[28]~~                         ' Toggle SCL from LOW to HIGH to LOW
   outa[28]~
   outa[29]~                          ' Leave SDA driven LOW


PUB ReadPage(devSel, addrReg, dataPtr, count) : ackbit
'' Read in a block of i2c data.  Device select code is devSel.  Device starting
'' address is addrReg.  Data address is at dataPtr.  Number of bytes is count.
'' The device select code is modified using the upper 3 bits of the 19 bit addrReg.
'' Return zero if no errors or the acknowledge bits if an error occurred.

   devSel |= addrReg >> 15 & %1110
   I2Cstart
   ackbit := Write(devSel | Xmit)
   ackbit := (ackbit << 1) | Write(addrReg >> 8 & $FF)
   ackbit := (ackbit << 1) | Write(addrReg & $FF)          
   I2Cstart
   ackbit := (ackbit << 1) | Write(devSel | Recv)
   
   repeat count - 1
      byte[dataPtr++] := Read(ACK)
   byte[dataPtr++] := Read(NAK)
   I2Cstop
   return ackbit
   


PUB ReadLong(devSel, addrReg) : dataX
'' Read in a single long of i2c data.  Device select code is devSel.  Device
'' starting address is addrReg.  The device select code is modified using the
'' upper 3 bits of the 19 bit addrReg.  This returns true if an error occurred.
'' Note that you can't distinguish between a return value of -1 and true error.

   if ReadPage(devSel, (addrReg << 2) + eepromBase, @dataX, 4)
      return -1
      

PUB WritePage(devSel, addrReg, dataPtr, count) : ackbit
'' Write out a block of i2c data.  Device select code is devSel.  Device starting
'' address is addrReg.  Data address is at dataPtr.  Number of bytes is count.
'' The device select code is modified using the upper 3 bits of the 19 bit addrReg.
'' Most devices have a page size of at least 32 bytes, some as large as 256 bytes.
'' Return zero if no errors or the acknowledge bits if an error occurred.  If
'' more than 31 bytes are transmitted, the sign bit is "sticky" and is the
'' logical "or" of the acknowledge bits of any bytes past the 31st.
   devSel |= addrReg >> 15 & %1110
   I2CStart
   ackbit := Write(devSel | Xmit)
   ackbit := (ackbit << 1) | Write(addrReg >> 8 & $FF)
   ackbit := (ackbit << 1) | Write(addrReg & $FF)          
   repeat count                        ' Now send the data
      ackbit := ackbit << 1 | ackbit & $80000000 ' "Sticky" sign bit         
      ackbit |= Write(byte[dataPtr++])
   I2CStop
   return ackbit
   

PUB WriteLong(devSel, addrReg, dataX)
'' Write out a single long of i2c data.  Device select code is devSel.  Device
'' starting address is addrReg.  The device select code is modified using the
'' upper 3 bits of the 19 bit addrReg.  This returns true if an error occurred.
'' Note that the long word value may not span an EEPROM page boundary.

   if WritePage(devSel, (addrReg << 2) + eepromBase, @dataX, 4)
      return true
   ' james edit - wait for 5ms for page write to complete (80_000 * 5 = 400_000)      
   waitcnt(800_000 + cnt)      
   return false




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