-----------------------------------------
-- DATA PROCESOR CODE
-- Lady Nicole Payan Cepeda, wv22318@bristol.ac.uk
-- Angelia Nudi, cm20884@bristol.ac.uk
-----------------------------------------

LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE ieee.numeric_std.ALL;
use work.common_pack.all; 

entity dataConsume is
  port(
        clk:in std_logic; 
		reset: in std_logic; 
		start: in std_logic; -- goes high to signal data transfer
		numWords_bcd: in BCD_ARRAY_TYPE(2 downto 0);  --Amount of words to be read from the Data Generator
		ctrlIn: in std_logic;  -- Signal recieved from the Data Generator signaling the data is ready in the 8-bit line
		ctrlOut: out std_logic; -- Signal sent to the Data Generator requesting a byte
		data: in std_logic_vector(7 downto 0); --The latest 8-bit wide data word retrieved from Data Generator
		dataReady: out std_logic; --Signaling Data is ready on the 8-bit byte line
		byte: out std_logic_vector(7 downto 0); --The latest 8-bit wide data word to send to Command Procesesor
		seqDone: out std_logic; --Number of bytes in numWords has been processed and results are in dataResults 
		maxIndex: out BCD_ARRAY_TYPE(2 downto 0); --Index of peak byte in BCD format
		dataResults: out CHAR_ARRAY_TYPE(0 to RESULT_BYTE_NUM-1) -- 7 bytes comprising of peak byte and the 3 bytes on either side of it
    );
end dataConsume;


architecture behav of dataConsume is
  
  --Initialising the state type 
   type state_type IS (INIT, INIT_BYTE_REQUEST,WAIT_CTRLIN_DATAGEN, BYTE_READY_FOR_CP, PEAK_DETECTOR, REPEAT_BYTE_REQUEST, CHECK_SLOT_EMPTY,CHECK_CORNER_CASE, EXTRA_BYTES_REQUEST,EXTRA_BYTES_RECEIVED,SEQDONE_CHANGE,OUTPUT);
   signal currentState, nextState: state_type;
   
   --Use of boolean algebra for type of logic
   type logic_type is (TRUE, FALSE);
   signal Enable_PD : logic_type; --Signal used to enable the peak detector state
   signal Ctrl_out_Sent: logic_type; --Signal used to check that the control signal has beend sent 
   signal Reset_signal: logic_type;
   
   -- Signals used to identy the slots to be filled for the corner case
   signal slot5_Enable: logic_type;  
   signal slot6_Enable : logic_type;
   signal slot7_Enable : logic_type;
   
   signal Enable_SeqDone : logic_type; --Only once all of numWords have passed through th peak detecor and corner case has been filled
   signal index_byte : integer; --Index of the input byte received.
   signal converted_byte_signed: integer; --Input byte receive conversion to signed integer
   signal maxIndex_integer : integer; --Input byte with max value in integer
   signal maxByte_integer : integer; --Index of input byte with max value in integer format.
   signal ctrlIn_delayed, ctrlIn_detected, ctrlOut_reg: std_logic; --Signals used to detect communication with Data Generator
   signal numWords_integer: integer range 0 to 999; --The integer version of numWords_bcd
   
   signal slot1,slot2,slot3,slot4,slot5,slot6,slot7,slot8,slot9,slot10: integer; -- Slots used in the peak detector implementation
   
begin
----------------------
--delay_Ctrln Process
--Delays the signal by one clock cycle 
-----------------------
delay_CtrlIn: process(clk)
begin 
  if rising_edge(clk) then
    ctrlIn_delayed <= ctrlIn;  --Delays the signal by one clock cycle 
  end if;
end process;

ctrlIn_detected <= ctrlIn xor ctrlIn_delayed; --Xor to check wether a signal has been detected from the Data Generator
-----------------------
--next_state Process
-----------------------
next_state: PROCESS (currentState, start, Ctrl_out_Sent, ctrlIn_detected, Enable_PD)
BEGIN
  CASE currentState IS
    --State used to initilase values
    WHEN INIT =>
      --Initialising all variables
      ctrlOut <= '0';
      ctrlOut_reg <= '0';
      dataReady <= '0'; 
      seqDone <= '0';
      Enable_SeqDone <= FALSE;
      Enable_PD <= FALSE;
      Ctrl_out_Sent <= FALSE;
      slot5_Enable <= FALSE;
      slot6_Enable <= FALSE;
      slot7_Enable <= FALSE;
      Reset_signal <= FALSE;
      
      -- initialising the slots for peak detector array
      slot1 <= 0; 
      slot2 <= 0;
      slot3 <= 0;
      slot4 <= 0;
      slot5 <= 0;
      slot6 <= 0;
      slot7 <= 0;
      
      -- initialising the slots of placeholder for another array of peak
      slot8 <= 0; 
      slot9 <= 0;
      slot10 <= 0;
     
      
      maxByte_integer <= -231; --reseting the maxByte after each iteration done, taking the lowest value possible from the integer range
      maxIndex_integer <= 0;
      
      IF start = '1' THEN  -- When start = 1, it is a signal for data retrieval to take place;
        ctrlOut_reg <= not ctrlOut_reg; -- ctrlOut is changed in a different state to give it time to be changed
        nextState <= INIT_BYTE_REQUEST; --First byte is requested
      ELSE
        nextState <= INIT; --Waits for start signal
      END IF;
      
    -- Control out is sent to Data Generator to Request the first byte
    WHEN INIT_BYTE_REQUEST =>
      Reset_signal <= FALSE;
      ctrlOut <= ctrlOut_reg;
      Ctrl_out_Sent <= TRUE;
      IF Ctrl_out_Sent = TRUE THEN  
        nextState <= WAIT_CTRLIN_DATAGEN;
      ELSE
        nextState <= INIT_BYTE_REQUEST;
      END IF;
    
    -- State waits for CntrlIn to be detected 
    WHEN WAIT_CTRLIN_DATAGEN => 
      IF ctrlIn_detected = '1' THEN
        Ctrl_out_Sent <= FALSE; 
        nextState <= BYTE_READY_FOR_CP;
      ELSE 
        nextState <= WAIT_CTRLIN_DATAGEN;
      END IF;
    
    --Once the bye is in the 8-bit wide byte line, it can be sent to the CP
    WHEN BYTE_READY_FOR_CP =>
      byte <= data;
      dataReady <= '1';
      Enable_PD <= TRUE;    --Once we have sent out byte and data ready to CP we can pass through peak detector
      IF Enable_PD = TRUE THEN
        nextState <= PEAK_DETECTOR;
      ELSE
        nextState <= BYTE_READY_FOR_CP;
      END IF;
      
    --This state is used to retrieve the remaining bytes based on the start signal from CP
    WHEN REPEAT_BYTE_REQUEST =>
      IF start = '1' THEN
        ctrlOut <= ctrlOut_reg;  --The peak detector would have already changed ctrlOut_reg
        Ctrl_out_Sent <= TRUE;
      ELSE
        nextState <= REPEAT_BYTE_REQUEST; --Wait until start signal to request next byte
      END IF;
      IF Ctrl_out_Sent = TRUE THEN  --Once start is 1 and ctrlOut has changed, move to cntrlIn waiting stage 
          nextState <= WAIT_CTRLIN_DATAGEN;
      ELSE
          nextState <= REPEAT_BYTE_REQUEST;
      END IF;
  
    WHEN PEAK_DETECTOR =>
     -- 1st Condition from the schematic figure
     IF index_byte < numWords_integer OR index_byte = numWords_integer THEN 
      
      -- IF Condition to compare the Received byte with the current Max Byte
      IF converted_byte_signed > maxByte_integer THEN
         maxByte_integer <= converted_byte_signed;
         maxIndex_integer <= index_Byte;
         
         --  IF Condition for Slots conditions, ensuring the respective byte gets assigned to the correct slot
         IF slot1 = 0 and slot2 = 0 and slot3 = 0 and slot4 = 0 THEN      
            maxByte_integer <= converted_byte_signed;
            slot4 <= converted_byte_signed;
            
            -- Nested IF Condition to make sure the correct byte gets assign and either request for another byte or go to check byte
            -- This Nested IF Condition will be repeated for each time when the byte gets assign
            IF index_byte < numWords_integer  THEN    
               dataReady <= '0';  
               ctrlOut_reg <= not ctrlOut_reg;             
               nextState <= REPEAT_BYTE_REQUEST;
            ELSIF index_byte = numWords_integer THEN
               dataReady <= '0';  
               nextState <= CHECK_SLOT_EMPTY;
            END IF;
         
         ELSIF slot1 = 0 and slot2 = 0 and slot3 = 0 and slot4 /= 0 and slot5 = 0 THEN          
            slot3 <= slot4;
            slot4 <= converted_byte_signed;
            IF index_byte < numWords_integer  THEN    
               dataReady <= '0';  
               ctrlOut_reg <= not ctrlOut_reg;             
               nextState <= REPEAT_BYTE_REQUEST;
            ELSIF index_byte = numWords_integer THEN
               dataReady <= '0';  
               nextState <= CHECK_SLOT_EMPTY;
            END IF;
         
         ELSIF slot1 = 0 and slot2 = 0 and slot3 /= 0 and slot4 /= 0 and slot5 = 0 THEN           
            slot2 <= slot3;
            slot3 <= slot4;
            slot4 <= converted_byte_signed;
            IF index_byte < numWords_integer  THEN    
               dataReady <= '0';  
               ctrlOut_reg <= not ctrlOut_reg;             
               nextState <= REPEAT_BYTE_REQUEST;
            ELSIF index_byte = numWords_integer THEN
               dataReady <= '0';  
               nextState <= CHECK_SLOT_EMPTY;
            END IF;
            
         ELSIF slot1 = 0 and slot2 /= 0 and slot3 /= 0 and slot4 /= 0 and slot5 = 0 THEN           
            slot1 <= slot2;
            slot2 <= slot3;
            slot3 <= slot4;
            slot4 <= converted_byte_signed;
            IF index_byte < numWords_integer  THEN    
               dataReady <= '0';  
               ctrlOut_reg <= not ctrlOut_reg;             
               nextState <= REPEAT_BYTE_REQUEST;
            ELSIF index_byte = numWords_integer THEN
               dataReady <= '0';  
               nextState <= CHECK_SLOT_EMPTY;
            END IF;
            
         ELSIF slot1 /= 0 and slot2 /= 0 and slot3 /= 0 and slot4 /= 0 and slot5 = 0 THEN           
            slot1 <= slot2;
            slot2 <= slot3;
            slot3 <= slot4;
            slot4 <= converted_byte_signed;
            IF index_byte < numWords_integer  THEN    
               dataReady <= '0';  
               ctrlOut_reg <= not ctrlOut_reg;             
               nextState <= REPEAT_BYTE_REQUEST;
            ELSIF index_byte = numWords_integer THEN
               dataReady <= '0';  
               nextState <= CHECK_SLOT_EMPTY;
            END IF;
                  
         ELSIF slot4 /= 0 and slot5 /= 0 and slot6 = 0 THEN
            slot1 <= slot3;
            slot2 <= slot4;
            slot3 <= slot5;
            slot4 <= converted_byte_signed;
            slot5 <= 0;
            IF index_byte < numWords_integer  THEN    
               dataReady <= '0';  
               ctrlOut_reg <= not ctrlOut_reg;             
               nextState <= REPEAT_BYTE_REQUEST;
            ELSIF index_byte = numWords_integer THEN
               dataReady <= '0';  
               nextState <= CHECK_SLOT_EMPTY;
            END IF;
         
         ELSIF slot4 /= 0 and slot5 /= 0 and slot6 /= 0 and slot7 = 0 THEN
            slot1 <= slot5;
            slot2 <= slot6;
            slot3 <= slot7;
            slot4 <= converted_byte_signed;
            slot5 <= 0;
            slot6 <= 0;
            IF index_byte < numWords_integer  THEN    
               dataReady <= '0';  
               ctrlOut_reg <= not ctrlOut_reg;             
               nextState <= REPEAT_BYTE_REQUEST;
            ELSIF index_byte = numWords_integer THEN
               dataReady <= '0';  
               nextState <= CHECK_SLOT_EMPTY;
            END IF;
            
         ELSIF slot4 /= 0 and slot5 /= 0 and slot6 /= 0 and slot7 /= 0 and slot8 = 0 THEN
            slot1 <= slot5;
            slot2 <= slot6;
            slot3 <= slot7;
            slot4 <= converted_byte_signed;
            slot5 <= 0;
            slot6 <= 0;
            slot7 <= 0;
            IF index_byte < numWords_integer  THEN    
               dataReady <= '0';  
               ctrlOut_reg <= not ctrlOut_reg;             
               nextState <= REPEAT_BYTE_REQUEST;
            ELSIF index_byte = numWords_integer THEN
               dataReady <= '0';  
               nextState <= CHECK_SLOT_EMPTY;
            END IF;    
             
         ELSIF slot5 /= 0 and slot6 /= 0 and slot7 /= 0 and slot8 /= 0 and slot9 = 0 THEN
            slot1 <= slot6;
            slot2 <= slot7;
            slot3 <= slot8;
            slot4 <= converted_byte_signed;
            slot5 <= 0;
            slot6 <= 0;
            slot7 <= 0;
            IF index_byte < numWords_integer  THEN    
               dataReady <= '0';  
               ctrlOut_reg <= not ctrlOut_reg;             
               nextState <= REPEAT_BYTE_REQUEST;
            ELSIF index_byte = numWords_integer THEN
               dataReady <= '0';  
               nextState <= CHECK_SLOT_EMPTY;
            END IF;
            
         ELSIF slot5 /= 0 and slot6 /= 0 and slot7 /= 0 and slot8 /= 0 and slot9 /= 0 and slot10 = 0 THEN
            slot1 <= slot7;
            slot2 <= slot8;
            slot3 <= slot9;
            slot4 <= converted_byte_signed;
            slot5 <= 0;
            slot6 <= 0;
            slot7 <= 0;
            IF index_byte < numWords_integer  THEN    
               dataReady <= '0';  
               ctrlOut_reg <= not ctrlOut_reg;             
               nextState <= REPEAT_BYTE_REQUEST;
            ELSIF index_byte = numWords_integer THEN
               dataReady <= '0';  
               nextState <= CHECK_SLOT_EMPTY;
            END IF;
            
         ELSIF slot5 /= 0 and slot6 /= 0 and slot7 /= 0 and slot8 /= 0 and slot9 /= 0 and slot10 /= 0 THEN
            slot1 <= slot8;
            slot2 <= slot9;
            slot3 <= slot10;
            slot4 <= converted_byte_signed;
            slot5 <= 0;
            slot6 <= 0;
            slot7 <= 0;
            IF index_byte < numWords_integer  THEN    
               dataReady <= '0';  
               ctrlOut_reg <= not ctrlOut_reg;             
               nextState <= REPEAT_BYTE_REQUEST;
            ELSIF index_byte = numWords_integer THEN
               dataReady <= '0';  
               nextState <= CHECK_SLOT_EMPTY;
            END IF;
         END IF;
      
      ELSIF converted_byte_signed < maxByte_integer or converted_byte_signed = maxByte_integer THEN
      
        IF slot5 = 0 THEN
           slot5 <= converted_byte_signed;
            IF index_byte < numWords_integer  THEN    
               dataReady <= '0';  
               ctrlOut_reg <= not ctrlOut_reg;             
               nextState <= REPEAT_BYTE_REQUEST;
            ELSIF index_byte = numWords_integer THEN
               dataReady <= '0';  
               nextState <= CHECK_SLOT_EMPTY;
            END IF;
                 
        ELSIF slot5 /= 0 and slot6 = 0 THEN
            slot6 <= converted_byte_signed;
            IF index_byte < numWords_integer  THEN    
               dataReady <= '0';  
               ctrlOut_reg <= not ctrlOut_reg;             
               nextState <= REPEAT_BYTE_REQUEST;
            ELSIF index_byte = numWords_integer THEN
               dataReady <= '0';  
               nextState <= CHECK_SLOT_EMPTY;
            END IF;
            
        ELSIF slot5 /= 0 and slot6 /= 0 and slot7=0 THEN
            slot7 <= converted_byte_signed;
            IF index_byte < numWords_integer  THEN    
               dataReady <= '0';  
               ctrlOut_reg <= not ctrlOut_reg;             
               nextState <= REPEAT_BYTE_REQUEST;
            ELSIF index_byte = numWords_integer THEN
               dataReady <= '0';  
               nextState <= CHECK_SLOT_EMPTY;
            END IF;
            
        ELSIF slot5 /= 0 and slot6 /= 0 and slot7 /= 0 and slot8 = 0 THEN
            slot8 <= converted_byte_signed;
            IF index_byte < numWords_integer  THEN    
               dataReady <= '0';  
               ctrlOut_reg <= not ctrlOut_reg;             
               nextState <= REPEAT_BYTE_REQUEST;
            ELSIF index_byte = numWords_integer THEN
               dataReady <= '0';  
               nextState <= CHECK_SLOT_EMPTY;
            END IF;
            
        ELSIF slot5 /= 0 and slot6 /= 0 and slot7 /= 0 and slot8 /= 0 and slot9 = 0 THEN
            slot9 <= converted_byte_signed;
            IF index_byte < numWords_integer  THEN    
               dataReady <= '0';  
               ctrlOut_reg <= not ctrlOut_reg;             
               nextState <= REPEAT_BYTE_REQUEST;
            ELSIF index_byte = numWords_integer THEN
               dataReady <= '0';  
               nextState <= CHECK_SLOT_EMPTY;
            END IF;
            
        ELSIF slot5 /= 0 and slot6 /= 0 and slot7 /= 0 and slot8 /= 0 and slot9 /= 0 and slot10 = 0 THEN
            slot10 <= converted_byte_signed;
            IF index_byte < numWords_integer  THEN    
               dataReady <= '0';  
               ctrlOut_reg <= not ctrlOut_reg;             
               nextState <= REPEAT_BYTE_REQUEST;
            ELSIF index_byte = numWords_integer THEN
               dataReady <= '0';  
               nextState <= CHECK_SLOT_EMPTY;
            END IF;
            
        ELSIF slot5 /= 0 and slot6 /= 0 and slot7 /= 0 and slot8 /= 0 and slot9 /= 0 and slot10 /= 0 THEN
            slot8 <= slot9;
            slot9 <= slot10;                 
            slot10 <= converted_byte_signed;
            IF index_byte < numWords_integer  THEN    
               dataReady <= '0';  
               ctrlOut_reg <= not ctrlOut_reg;             
               nextState <= REPEAT_BYTE_REQUEST;
            ELSIF index_byte = numWords_integer THEN
               dataReady <= '0';  
               nextState <= CHECK_SLOT_EMPTY;
            END IF;             
	    END IF;
      END IF;
      
     -- 2nd Condition from the schematic figure
     ELSIF index_byte > numWords_integer THEN   
       dataReady <= '0';  
       IF slot5 = 0 AND slot6 = 0 AND slot7 = 0 THEN
          slot5 <= converted_byte_signed;
          nextState <= CHECK_SLOT_EMPTY;
       ELSIF  slot5 /= 0 AND slot6 /= 0 AND slot7 = 0  THEN 
          slot7 <= converted_byte_signed;
          nextState <= CHECK_SLOT_EMPTY;
       ELSIF  slot5 /= 0 AND slot6 /= 0 AND slot7 /= 0  THEN 
          Enable_SeqDone <= True;
          nextState <= SEQDONE_CHANGE;         
       END IF;
     END IF; 
    
    --State to check once all numWords have been processed if any of the slots are still empty
    --This is to account for the corner case
    WHEN CHECK_SLOT_EMPTY =>
     IF slot5 = 0 THEN
       slot5_Enable <= True;
     ELSE 
       slot5_Enable <= FALSE;
     END IF;
     
     IF slot6 = 0  THEN
       slot6_Enable <= True;
     ELSE 
       slot6_Enable <= FALSE;
     END IF;
     
     IF slot7 = 0 THEN
       slot7_Enable <= True;
     ELSE 
       slot7_Enable <= FALSE;
     END IF; 
     
     --Once the check is complete and signals have been assigend to last 3 bytes
     --The signals assigned are checked to see if any slot is 0
     nextState <= CHECK_CORNER_CASE;
     
     --This state either moves to requesting extra bytes so that the slots that are empty are filled to print in dataSequence
     --Or straight to output
     WHEN CHECK_CORNER_CASE =>
       IF slot5_Enable = True or slot6_Enable = True or slot7_Enable = True THEN
          ctrlOut_reg <= not ctrlOut_reg; --ctrlOut_reg is a state before to allow time for change
          nextState <= EXTRA_BYTES_REQUEST;
       END IF;
       IF slot5_Enable = False and slot6_Enable = False and slot7_Enable = False THEN
          Enable_SeqDone <= True;
          nextState <= SEQDONE_CHANGE;
       END IF;
    
    --ctrlOut is sent to request for the remaining bytes of data
    WHEN EXTRA_BYTES_REQUEST => 
      ctrlOut <= ctrlOut_reg;
      Ctrl_out_Sent <= TRUE;
      IF Ctrl_out_Sent = TRUE THEN
        nextState <= EXTRA_BYTES_RECEIVED;
      ELSE
        nextState <= EXTRA_BYTES_REQUEST;
      END IF;
     
    --ctrlIn with ctrlIn_detected is checked from the Data Generated if not wait for it
    --The slots are filled in order and return to check if there are any remaining empty slots
    WHEN EXTRA_BYTES_RECEIVED => 
      IF ctrlIn_detected = '1' THEN
        Ctrl_out_Sent <= FALSE;
        IF slot5_Enable = True THEN
          slot5 <= converted_byte_signed;
          nextState <= CHECK_SLOT_EMPTY;
        ELSE
          IF slot6_Enable = True THEN
            slot6 <= converted_byte_signed;
            nextState <= CHECK_SLOT_EMPTY;
          ElSE
            IF slot7_Enable = True THEN
              slot7 <= converted_byte_signed;
              nextState <= CHECK_SLOT_EMPTY;
            END IF;
          END IF;
        END IF;
        
        --This process is looped until all slots are filled and we can moce to output stage
        IF slot5_Enable = FALSE and slot6_Enable = FALSE and slot7_Enable = FALSE THEN
          Enable_SeqDone <= True;
          nextState <= SEQDONE_CHANGE;
        END IF; 
     ELSE
       nextState <= EXTRA_BYTES_RECEIVED;
     END IF;
    
    -- This state is used to change seqDone for one clock cycle
    WHEN SEQDONE_CHANGE =>
      IF Enable_SeqDone <= True THEN
        seqDone <= '1';
        nextState <= OUTPUT;
      END IF;
    
    --Once all the word is processed,wait for start signal to start process again for new word from the INIT_BYTE_REQUEST state
    WHEN OUTPUT =>
     seqDone <= '0';
     Enable_SeqDone <= False;
     Reset_signal <= TRUE;
     IF Start = '0' THEN
       nextState <= OUTPUT;
     ELSE 
       slot1 <= 0; -- initialising the slots for peak detector
       slot2 <= 0;
       slot3 <= 0;
       slot4 <= 0;
       slot5 <= 0;
       slot6 <= 0;
       slot7 <= 0;
       slot8 <= 0; -- placeholder for another array of peak
       slot9 <= 0;
       slot10 <= 0;
       
       maxByte_integer <= -231; --reseting the maxByte after each iteration done taking the lowest value possible from the integer range
       maxIndex_integer <= 0;
       
       Enable_SeqDone <= False;
       slot5_Enable <= FALSE;
       slot6_Enable <= FALSE;
       slot7_Enable <= FALSE;
       --Reset_signal <= TRUE;
       
       ctrlOut_reg <= not ctrlOut_reg; -- ctrlOut is changed in a different state to give it time to be changed
       nextState <= INIT_BYTE_REQUEST;
     END IF;
  END CASE;
END PROCESS;

--------------------------------------------------------------------
--data_sequence_output process
--Data results is initiliased to 0 at the beginning 
--Updating the Data Results every time a byte is fetch
--Data results is reset once the words and corner case is complete
----------------------------------------------------------------------
data_sequence_output: process(clk)
BEGIN
  IF rising_edge(clk) THEN
    IF ctrlIn_detected = '1' or Enable_SeqDone = True THEN  
      dataResults(6) <= std_logic_vector(to_unsigned(slot7,8));
      dataResults(5) <= std_logic_vector(to_unsigned(slot6,8));
      dataResults(4) <= std_logic_vector(to_unsigned(slot5,8));
      dataResults(3) <= std_logic_vector(to_unsigned(slot4,8));
      dataResults(2) <= std_logic_vector(to_unsigned(slot3,8));
      dataResults(1) <= std_logic_vector(to_unsigned(slot2,8));
      dataResults(0) <= std_logic_vector(to_unsigned(slot1,8));
    ELSIF Enable_SeqDone = false and Reset_signal= true THEN  
      dataResults(6) <= std_logic_vector(to_unsigned(0,8));
      dataResults(5) <= std_logic_vector(to_unsigned(0,8));
      dataResults(4) <= std_logic_vector(to_unsigned(0,8));
      dataResults(3) <= std_logic_vector(to_unsigned(0,8));
      dataResults(2) <= std_logic_vector(to_unsigned(0,8));
      dataResults(1) <= std_logic_vector(to_unsigned(0,8));
      dataResults(0) <= std_logic_vector(to_unsigned(0,8));
    END IF;
  END IF;
END PROCESS;
--------------------------------------------------------------
--index_byte_count
-- The index is reset zero once numWords have been processed
--Index does not count the extra bytes added in the corner case
--Index increments when ctrlIn is detected
-------------------------------------------------------------
index_byte_count: process (clk)
BEGIN
  IF rising_edge(clk) THEN
    if reset = '1' or Reset_signal= true then
        index_byte <=0;
    ELSE 
      IF ctrlIn_detected = '1' THEN
        IF Enable_SeqDone = False THEN
          index_byte <= index_byte + 1;
        ELSIF Enable_SeqDone = True THEN
          index_byte <= 0;
        END IF;
      END IF;
    END IF;
  END IF;
END PROCESS;

-----------------------------------------------------------------------
---bcd_to_integer process
---Changes the signal recieved numWords_bcd to unsigned integer for ease of manipulation
-----------------------------------------------------------------------
bcd_to_integer: process (numWords_bcd)
Variable add: integer range 0 to 999 :=0;
BEGIN
  add := 0;
  for i in 0 to 2 loop
    add := add + (to_integer(unsigned(numWords_bcd(i)))*(10**(i)));
  end loop;
  numWords_integer <= add;
END PROCESS;
--------------------------------------------------
--integer_to_bcd
-- Convers the maxIndex_integer to bcd 
--------------------------------------------------
integer_to_bcd: process (maxIndex_integer)
variable temp1, temp2,temp3 : integer range 0 to 9 ;
BEGIN
  temp1 := 0;
  temp2 := 0;
  temp3 := 0;
  maxIndex(2) <= std_logic_vector(to_unsigned(maxIndex_integer/100, 4));
  temp1 := maxIndex_integer - ((maxIndex_integer/100)*100);
  temp2 := temp1 - (temp1 mod 10) ;
  maxIndex(1) <= std_logic_vector(to_unsigned(temp2/10, 4));
  temp3 := temp1 mod 10;
  maxIndex(0) <= std_logic_vector(to_unsigned(temp3, 4));
END PROCESS;
---------------------------------------------------------------------------------------------------
--data_to_integer
--Convers the data from the Commnd Process to signed integer for peak detection manipulation
-------------------------------------------------------------------------------------------------
data_to_integer: process (data)
BEGIN
  converted_byte_signed <= to_integer(signed(data));
END PROCESS;

------------------------------------------
--State register
-- If the rest button on the FPGA is pressed then go back to initial state
-- Else we move on to the next state
--------------------------------------------
stateRegister: process (clk)
BEGIN
  IF rising_edge(clk) THEN
    IF (reset = '1') THEN 
      currentState <= INIT;   
    ELSE 
      currentState <= nextState;
    END IF;
  END IF;
END PROCESS;

END behav; --end of Data Consume

