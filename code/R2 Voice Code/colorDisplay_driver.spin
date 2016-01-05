VAR

  byte loadFlag
  word numberBytes
  long parameterX
  
PUB Launch(display_memory, DMD_cog) | g


  loadFlag := 0
  numberBytes := 0

  loadFlagAddress := @loadFlag
  numberBytesAddress := @numberBytes

  parameterX := display_memory                         'Make sure memory location is long-aligned
  coginit(DMD_cog, @Loader, @parameterX)                'Start cog 7 and pass the screen memory location into it.   


PUB Load(howManyBytes)

  numberBytes := howManyBytes
    
  loadFlag := 1
  repeat while loadFlag



DAT     org

Loader

        mov     ctra,                  clockCounterSetup            ' Setup counter modules.
        mov     ctrb,                  dataInCounterSetup           '

        rdlong memStart, par                            'Get location of the beginning of screen memory

        mov cclockML, #1                                '
        rol cclockML, #21                               '

        mov cdataML, #1                                 '
        rol cdataML, #20                                '
           
        mov clatchML, #1                                'set up latch, data, and clock pins
        rol clatchML, #19                               '

        or dira, cdataML                                'Set all ouput pins to OUT direction (=1)
        or dira, cclockML                               '
        or dira, clatchML                               '

        cmp zero, #0 wz                                 'Latch the data onto the registers - we do this at the start to sync with the LCD
     
        muxz outa, clatchML                             'Set latch HIGH
        
        muxz outa, cclockML                             'Pulse clock                    
        muxnz outa, cclockML                            '
        muxnz outa, clatchML                            'Set latch low - frame loaded!

StartFrame

        rdbyte dataTemp, loadFlagAddress                'read the load flag
        tjz dataTemp, #StartFrame                       'if the load flag is zero, then just loop waiting 

        rdword toDoBytes, numberBytesAddress            'read in how many bytes to send         
        mov pointer, memStart                           'Set pointer to start of frame
                
DoByte

        rdbyte phsb, pointer                            'Read in the byte to send

        ror     phsb,                  #8

        mov     phsa,                  #0                           ' Start clock low.

        movi    frqa,                  #%0100_0000_0                ' Write out data.
         
        shl     phsb,                  #1                           '
        shl     phsb,                  #1                           '
        shl     phsb,                  #1                           '
        shl     phsb,                  #1                           '
        shl     phsb,                  #1                           '
        shl     phsb,                  #1                           '
        shl     phsb,                  #1                           '

        'shl     phsb,                  #0                           'Extra clock to tell FPGA to load a byte                   
 
        mov     frqa,                  #0                           'Stop the clock.
   

        add pointer, #1                                 'Increment memory pointer 1 byte

        djnz toDoBytes, #DoByte                         'Keep going until out of bytes
 
        cmp zero, #0 wz                                 'Latch the data onto the registers
        muxz outa, clatchML                             'Set latch HIGH
        muxz outa, cclockML                             'Pulse clock                    
        muxnz outa, cclockML                            '
        muxnz outa, clatchML                            'Set latch low - frame loaded!

        wrbyte zero, loadFlagAddress                    'signal that the frame is done

        jmp #StartFrame





'Constants for PASM code
one                     long    1
zero                    long    0

loadFlagAddress         long    0               'address of loadFlag var in hub memory (set this from spin before starting the cog with the PASM)
numberBytesAddress      long    0               'address of numberBytes var in hub memory (set this from spin before starting the cog with the PASM)



clockCounterSetup       long    $1000_0015                                              ' Clock control.
dataInCounterSetup      long    $1000_0014                                                   'Data in control.

SPIcounter              res     1

'Variables for PASM code
clatchML                res     1               'Column Latch  
cclockML                res     1               'Dot Clock
cdataML                 res     1               'Serial Data  
cEnableML               res     1               'Enable line
                  
dataTemp                res     1               'Used for temp data storage and bitwise operations

memStart                res     1               'Start of screen memory
pointer                 res     1               'Current location in screen memory
toDoBits                res     1               'How many bits left to shift out of current byte
toDoBytes               res     1               'How many bytes to clock out