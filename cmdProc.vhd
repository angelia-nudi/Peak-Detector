------------------------------------------------
-- cmdProc Code
-- Ali Alturaifi, oy21115@bristol.ac.uk
-- Ibhar Hajjam, pu21037@bristol.ac.uk
-- Yvan Dalumpines, xh21408@bristol.ac.uk
------------------------------------------------
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use ieee.numeric_std.all;
use work.common_pack.all;
use ieee.std_logic_signed.all;

library UNISIM;
use UNISIM.VComponents.all;

entity cmdProc is
port (
    clk:		in std_logic;
    reset:        in std_logic;
    rxnow:        in std_logic; -- dataReady (Receiver)
    rxData:            in std_logic_vector (7 downto 0); 
    txData:            out std_logic_vector (7 downto 0);-- A register that contains the whole data packet to be sent, including start and stop bits. 
    rxdone:        out std_logic;  -- Tell receiver to send next data packet (active high)
    ovErr:        in std_logic; -- Too much data (Receiver)
    framErr:    in std_logic; -- Wrong data (Receiver)
    txnow:        out std_logic; -- Ready to send data to transmitter (sends hight for 1 clock cycle)
    txdone:        in std_logic; -- Transmitter done transmitting
    start: out std_logic; -- Send for dataConsume to iterate through the next data packet
    numWords_bcd: out BCD_ARRAY_TYPE(2 downto 0); -- 12 bits
    dataReady: in std_logic; -- from dataConsumer
    byte: in std_logic_vector(7 downto 0); -- the latest 8 bits , valid when dataready=1
    maxIndex: in BCD_ARRAY_TYPE(2 downto 0); -- 12 bits
    dataResults: in CHAR_ARRAY_TYPE(0 to RESULT_BYTE_NUM-1); -- 6 bytes
    seqDone: in std_logic --from dataconsumer
    );
end cmdProc;

architecture Behavioral of cmdProc is
    type state_type is (
        reset_state, -- 
        data_echo, --
        check_cmd, --
        send_start, -- 
        wait_dataReady, --
        print_first_byte, --
        print_second_byte, --
        print_space, --
        check_seq_completion,
        print_LF,
        print_CR,
        check_P_L_cmd, --
        peak_first_letter, 
        peak_second_letter, 
        peak_space, 
        peak_index,
        check_peak, 
        list_first_letter, 
        list_second_letter, 
        list_space,
        check_list,
        LF, 
        CR  --
        );

    signal curState, nextState: state_type;

    -- Define a type for the BCD-to-ASCII lookup table
    type BCD_ASCII_Table is array (0 to 15) of std_logic_vector(7 downto 0);
    
    signal rxData_reg : std_logic_vector(7 downto 0); -- Holds data for checking (NOT USED)
    
    -- new added signals
    -- Register signals to sync with clock.
    signal dataResults_reg: CHAR_ARRAY_TYPE(0 to RESULT_BYTE_NUM-1):= (others => "00000000");
    signal maxIndex_reg: BCD_ARRAY_TYPE(2 downto 0):= (others => "0000");
    signal en_shift, en_numWords: std_logic := '0'; -- For use in receiving commands, treat like flags.
    type ASCII_ARRAY_TYPE is array(integer range <>)of std_logic_vector(7 downto 0);
    signal rcv_cmd: ASCII_ARRAY_TYPE(3 downto 0):= (others => "00000000"); -- Vector for storing commands from the receiver.
    signal reset_seqDone: std_logic := '0'; -- When high, set seqDone to 0
    signal reg_seqDone:  std_logic := '0'; -- Store current value of seqDone.
       
    -- Counter signals.
    signal peak_counter: integer :=0;
    signal en_P_counter: std_logic := '0';
    signal reset_P_counter: std_logic := '0';
    signal list_counter: integer :=0 ;
    signal reset_L_counter: std_logic := '0';
    signal en_L_counter: std_logic := '0';
    
    -- Define the lookup table for BCD-to-ASCII conversion
function BCD_to_ASCII(BCD_input : in std_logic_vector(3 downto 0)) return std_logic_vector is
    variable ASCII_output : std_logic_vector(7 downto 0);
begin
    case BCD_input is
        when "0000" =>
            ASCII_output := "00110000"; -- '0' in ASCII
        when "0001" =>
            ASCII_output := "00110001"; -- '1' in ASCII
        when "0010" =>
            ASCII_output := "00110010"; -- '2' in ASCII
        when "0011" =>
            ASCII_output := "00110011"; -- '3' in ASCII
        when "0100" =>
            ASCII_output := "00110100"; -- '4' in ASCII
        when "0101" =>
            ASCII_output := "00110101"; -- '5' in ASCII
        when "0110" =>
            ASCII_output := "00110110"; -- '6' in ASCII
        when "0111" =>
            ASCII_output := "00110111"; -- '7' in ASCII
        when "1000" =>
            ASCII_output := "00111000"; -- '8' in ASCII
        when "1001" =>
            ASCII_output := "00111001"; -- '9' in ASCII
        when "1010" =>
            ASCII_output := "01000001"; -- 'A' in ASCII
        when "1011" =>
            ASCII_output := "01000010"; -- 'B' in ASCII
        when "1100" =>
            ASCII_output := "01000011"; -- 'C' in ASCII
        when "1101" =>
            ASCII_output := "01000100"; -- 'D' in ASCII
        when "1110" =>
            ASCII_output := "01000101"; -- 'E' in ASCII
        when "1111" =>
            ASCII_output := "01000110"; -- 'F' in ASCII
        when others =>
            ASCII_output := "00110000"; -- Default to '0' in ASCII for invalid BCD
    end case;
    return ASCII_output;
end function;
 
begin

-- Process for holding data from the receiver
rx_hold : process(clk, rxnow)
begin
    if reset = '1' then
        rxData_reg <= "00000000";
    elsif rising_edge(clk) then
        rxData_reg <= rxData;
    end if;
end process;

-- new added processes:
-- Process to record the returned data from the data processor upon asserting seqDone
Data_Register: process(seqDone, reset, clk)
begin
    if reset = '1' then
        dataResults_reg <= (others => "00000000");
        maxIndex_reg <= (others => "0000");
    elsif rising_edge(clk) and seqDone = '1' then
        dataResults_reg <= dataResults;
        maxIndex_reg <= maxIndex;
    end if;
end process;


-- Process for registering and shifting the entered commands
-- en_shift is high when data_echo process in effect.
shift_command: process(reset, en_shift, dataReady, clk)
begin
    if reset = '1' or dataReady = '1' then
        rcv_cmd <= (others => "11111111");    
    elsif rising_edge(clk)and en_shift = '1' then
        rcv_cmd <= rcv_cmd(2 downto 0) & "00000000";
        rcv_cmd(0) <= rxData;
    end if;
end process;

-- Process for recording numWords
-- Use a register to store the information and constantly update it.
-- Only changes when a new command is received.
numWords_record: process(clk, en_numWords, rcv_cmd, dataReady)
    variable numWords_bcd_temp: BCD_ARRAY_TYPE(2 downto 0);
begin
    if rising_edge(clk) and en_numWords = '1' then
        numWords_bcd_temp(2) := rcv_cmd(2)(3 downto 0);
        numWords_bcd_temp(1) := rcv_cmd(1)(3 downto 0);
        numWords_bcd_temp(0) := rcv_cmd(0)(3 downto 0);     
    end if;
        numWords_bcd <= numWords_bcd_temp;
end process;

-- Process for registering seqDone
seqDone_Reg: process(clk, reset, reset_seqDone)
begin
    if reset = '1' or reset_seqDone = '1' then
        reg_seqDone <= '0';
    elsif rising_edge(clk) and seqDone = '1' then
        reg_seqDone <= '1';
    end if;
end process; 

-- Process for the peak index operation 
peak_index_counter: process(clk, reset, reset_P_counter , en_P_counter)
variable count: integer;
begin
    if reset = '1' or reset_P_counter = '1' then
        count:= 3; -- Initialize the counter to middle (3 is equivalent to 0)
    elsif rising_edge(clk) then
        if en_P_counter = '1' then -- Check if counter should be enabled
                count := count - 1; -- Decrement the counter
        end if;
    end if;
    peak_counter <= count;
end process;

-- Process for recoding the number of data that has been listed
list_index_counter: process(clk, reset, reset_L_counter, en_L_counter)
variable count: integer; 
begin
    if reset = '1' or reset_L_counter = '1' then
        count := 7; -- Initialize the counter to 0
    elsif rising_edge(clk) then
        if en_L_counter = '1' then -- Check if counter should be enabled
                count := count - 1; -- Decrement the counter
        end if;
    end if;
    list_counter <= count;
end process;

-- Process to handle state transitions
logic_nextState: process(curState, txdone, rxnow, rcv_cmd, dataReady)

-- Define boolean conditions for commands
variable valid_command_condition : boolean; 
variable valid_P_command_condition : boolean;
variable valid_L_command_condition : boolean;

begin    
      case curState is
        -- Inital state of the machine
        when reset_state =>
            if rxnow = '1' then 
                nextState <= data_echo;
            else
                nextState <= reset_state;
            end if;
            
        -- Sending the received data to the transmitter.
        when data_echo =>
            if txdone = '1' and rxnow = '1' then
                nextState <= check_cmd;
            else 
                nextState <= data_echo;
            end if;
        
        -- Check which commands are being sent to the 
        when check_cmd =>
            if txdone = '1' then
                valid_command_condition := (rcv_cmd(3) = "01100001" or rcv_cmd(3) = "01000001") and
                       (rcv_cmd(0) >= "00110000" and rcv_cmd(0) <= "00111001") and
                       (rcv_cmd(1) >= "00110000" and rcv_cmd(1) <= "00111001") and
                       (rcv_cmd(2) >= "00110000" and rcv_cmd(2) <= "00111001") and
                       not (rcv_cmd(0) = "00110000" and rcv_cmd(1) = "00110000" and rcv_cmd(2) = "00110000");

                valid_P_command_condition := (rcv_cmd(0) = "011100000" or rcv_cmd(0) = "01010000");
                valid_L_command_condition := (rcv_cmd(0) = "01101100" or rcv_cmd(0) = "01001100");

                   if valid_command_condition then
                       nextState <= send_start;
                       
                    elsif valid_P_command_condition then
                                nextState <= peak_first_letter;
                    elsif valid_L_command_condition then
                                nextState <= list_first_letter;
                    else
                                nextState <= reset_state;
                    end if;
                   else
                       nextState <= check_cmd;
                   end if;

        -- Send start signal when aNNN command is received.                  
        when send_start =>
            if txdone = '1' then
                nextState <= wait_dataReady;
            else
                nextState <= send_start;
            end if;
        
        -- Wait for the data processor to send current byte to print.
        when wait_dataReady =>
            if dataReady = '1' then
                nextState <= print_first_byte;
            else
                nextState <= wait_dataReady;
            end if;
            
         -- Data echoing process.   
         when print_first_byte =>
            if txdone = '1' then
                nextState <= print_second_byte;
            else
                nextState <= print_first_byte;
            end if;
            
         when print_second_byte =>
            if txdone = '1' then
                nextState <= print_space;
            else
                nextState <= print_second_byte;
            end if;
            
         when print_space =>
           if txdone = '1' then
                nextState <= check_seq_completion;
           else
                nextState <= print_space;
           end if;
        
        -- Check for seqDone
        when check_seq_completion =>
    
                if reg_seqDone ='1' then
                    nextState <= print_LF;
                else
                    nextState <= wait_dataReady;
                end if;
                
        -- Send Line Feed and Carriage Return characters to end statement.
        when print_LF =>
            if txdone = '1' then
                nextState <= print_CR;
            else
                nextState <= print_LF;
            end if;
            
        when print_CR => 
            if txdone = '1' then
                nextState <= check_cmd;
            else
                nextState <= print_CR;
            end if;
                
       -- Peak printing sequence
       when peak_first_letter =>
            if txdone ='1' then
                nextState <= peak_second_letter;
            else
                nextState <= peak_first_letter;
            end if;
            
        when peak_second_letter => 
            if txdone ='1' then

                nextState <= peak_space;
            else
                nextState <= peak_second_letter;
            end if;
        
        when peak_space =>
            if txdone ='1' then
                nextState <= peak_index;
            else
                nextState <= peak_space;
            end if;
            
        when peak_index =>
            if txdone ='1' then

                nextState <= check_peak;
            else
                 nextState <= peak_index;
            end if;
            
        when check_peak =>
            if txdone = '1' then
                if peak_counter <= 0 then
                   nextState <= print_LF;
                else
                   nextState <= peak_index;
                end if;
                
            else
                nextState <= check_peak;
            end if;
        
        -- List sequence
        when list_first_letter =>
            if txdone ='1' then
                nextState <= list_second_letter;
            else 
                nextState <= list_first_letter;
            end if;
            
        when list_second_letter =>
            if txdone ='1' then
                nextState <= list_space;
            else 
                nextState <= list_second_letter;
            end if;
            
        when list_space =>
            if txdone ='1' then  
                nextState <= check_list;
            else
                nextState <= list_space;
            end if;
            
        when check_list =>
            if txdone = '1' then
                if list_counter <= 0 then
                    nextState <= print_LF;
                else 
                    nextState <= list_first_letter;
                end if;
                
            else
                nextState <= check_list;
            end if;
                  
        when LF =>
            if txdone ='1' then
                nextState <= CR;
            else
                nextState <= LF;
            end if;
                   
        when CR =>
            if txdone ='1' then
                nextState <= reset_state;
                else
                nextState <= CR;
                end if;
 
        when others => 
           nextState <= reset_state;                        
            
    end case;
end process;



-- State outputs here.
logic_StateOut: process(curState, txDone, rxnow, rxdata, rcv_cmd, dataReady, dataResults_reg, reg_seqDone, maxIndex_reg, byte)

variable valid_command_condition : boolean;

begin
    txnow <= '0';
    start <= '0';
    rxDone <= '0';
    en_shift <= '0';
    en_numWords <= '0';
    reset_P_counter <= '0';
    reset_L_counter <= '0';
    en_P_counter <= '0';
    en_L_counter <= '0';
    reset_seqDone <= '0';
    
    
    case curState is
    
        when reset_state =>
            reset_P_counter <= '1';
            reset_L_counter <= '1';
            reset_seqDone <= '1';
            
            
        when data_echo =>
            if txDone = '1' and rxnow = '1' then
                rxdone <= '1';
                en_shift <= '1';
                txData <= rxData_reg;
                txnow <= '1';
            end if;
        
        -- Store numWords after the command is finished.
        when check_cmd =>
            if txdone = '1' then
                    valid_command_condition := (rcv_cmd(3) = "01100001" or rcv_cmd(3) = "01000001") and
                           (rcv_cmd(0) >= "00110000" and rcv_cmd(0) <= "00111001") and
                           (rcv_cmd(1) >= "00110000" and rcv_cmd(1) <= "00111001") and
                           (rcv_cmd(2) >= "00110000" and rcv_cmd(2) <= "00111001") and
                           not (rcv_cmd(0) = "00110000" and rcv_cmd(1) = "00110000" and rcv_cmd(2) = "00110000");
                    
                           if valid_command_condition then
                               en_numWords <= '1';
                           end if;
            end if;
       
       -- Clear seqDone status and send start signal to the data processor.
       when send_start =>
            if txDone = '1' then
                reset_seqDone <= '1';
                start <= '1';
           end if;
       
       when print_first_byte =>
            if txDone = '1' then
            txnow <= '1';
            txData <= BCD_to_ASCII(byte(7 downto 4));
            end if;
            
       when print_second_byte =>
            if txDone = '1' then
                txnow <= '1';
                txData <= BCD_to_ASCII(byte(3 downto 0));
               
            end if;
            
       when print_space =>
            if txDone = '1' then
                txnow <= '1';
                txData <= "00100000"; -- ASCII Space Character 
            end if;
        
        when check_seq_completion =>
 
                if reg_seqDone = '0' then
                    start <= '1';
                end if;

        when print_LF =>
            if txDone = '1' then
                txData <= "00001010"; -- the ASCII representation of line feed
                txnow <= '1';
            end if;
       
        when print_CR =>
            if txDone = '1' then
                txData <= "00001101"; -- the ASCII representation of carriage return
                txnow <= '1';
            end if;
           
        when peak_first_letter =>
             if txDone = '1' then
                txData <= BCD_to_ASCII(dataResults_reg(3)(7 downto 4)); -- the ASCII representation of the first four bit of the data received
                txnow <= '1';
             end if;
             
        when peak_second_letter => 
             if txDone = '1' then
                txData <= BCD_to_ASCII(dataResults_reg(3)(3 downto 0));-- the ASCII representation of the second four bit of the data received
                txnow <= '1';
              end if;
              
        when peak_space => 
             if txDone = '1' then                
                txData <= "00100000"; -- the ASCII representation of space
                txnow <= '1';
             end if;
                
        when peak_index =>
             if txDone = '1' then
                txData <= BCD_to_ASCII(maxIndex_reg(peak_counter-1));-- The index value according to the counter
                txnow <= '1';
                en_P_counter <= '1';
             end if; 
                
        when check_peak =>
            if txdone = '1' then
                if peak_counter <= 0 then
                    en_P_counter <= '0';
                else
                    en_P_counter <= '1';
                end if;
            end if;
     
        when list_first_letter =>
             if txDone = '1' then                
                txData <= BCD_to_ASCII(dataResults_reg(list_counter)(7 downto 4));-- the ASCII representation of the first four bit of the intended byte
                txnow <= '1';
              end if;
                
        when list_second_letter =>
             if txDone = '1' then                
                txData <= BCD_to_ASCII(dataResults_reg(list_counter-1)(3 downto 0)); -- the ASCII representation of the second four bit of the intended byte
                txnow <= '1';
             end if;  
            
        when list_space =>
             if txDone = '1' then                
                txData <= "00100000"; -- the ASCII representation of space
                txnow <= '1';
                en_L_counter <= '1';
             end if;
             
        when check_list =>
            if txdone = '1' then
                if list_counter <= 0 then
                    en_L_counter <= '0';
                else
                    en_L_counter <= '1';
                end if;
            end if;   
             
        when LF =>
            if txDone = '1' then
                txnow <= '1';
                txData <= "00001010"; -- the ASCII representation of line feed
            end if;
       
        when CR =>
            if txDone = '1' then
                txnow <= '1';
                txData <= "00001101"; -- the ASCII representation of carriage return
                rxDone <= '1';
            end if;
           
        when others =>
            null;
                   
    end case;
end process;

sequential_state: process(clk, reset)
begin
    if rising_edge(clk) then
        if reset = '1' then
            -- Reset state machine
            curState <= reset_state;
        else
            -- Update state.
            curState <= nextState;

        end if;
    end if;
end process;

end Behavioral;
    
