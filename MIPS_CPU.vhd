-- Single Clock Cycle MIPS Implementation for 32-bit system --


library ieee;
use ieee.std_logic_1164.all;
--use ieee.std_logic_signed.all;
use ieee.numeric_std.all;

entity MIPS_CPU is

	port( clk, reset : in std_logic;
			stop : out boolean
		 );
		
end entity MIPS_CPU;


architecture sequential of MIPS_CPU is

-- Opcode Constants --
constant addiu : std_logic_vector(5 downto 0):="001001";
constant beq : std_logic_vector(5 downto 0):= "000100";
constant bne : std_logic_vector(5 downto 0):= "000101";
constant j : std_logic_vector(5 downto 0):= "000010";
constant lw : std_logic_vector(5 downto 0):= "100011";
constant sltiu : std_logic_vector(5 downto 0) := "001011";
constant sw : std_logic_vector(5 downto 0):= "101011";
constant empty : std_logic_vector(5 downto 0) := "000000";

-- Function Constants (looked at if opcode is "empty") --
constant addu_fun : std_logic_vector(5 downto 0) := "100001";
constant sltu_fun : std_logic_vector(5 downto 0) := "101011";
constant subu_fun : std_logic_vector(5 downto 0) := "100011";
constant syscall_fun : std_logic_vector(5 downto 0) := "001100";



-- Constants for memory fixes -- 
constant memory_fix_first : std_logic_vector(15 downto 0) := X"2000";
constant memory_fix_second : std_logic_vector(15 downto 0) := X"1FF8";
constant neg1 : std_logic_vector(15 downto 0) := X"FFFE";        -- Using -2 here since the offset is by 2 for loading in loop3 
constant shift_4_fix : std_logic_vector(15 downto 0) := X"0004";
constant shift_4neg_fix : std_logic_vector(15 downto 0) := X"FFFC";
constant shift_8_fix : std_logic_vector(15 downto 0) := X"0008";
constant shift_8neg_fix : std_logic_vector(15 downto 0) := X"FFF8";


signal PC : integer := 0;
signal running : boolean := TRUE;  --is assigned to the "STOP" output to show program is done executing...
type memory_type is array (0 to 31) of std_logic_vector(31 downto 0);
signal ROM : memory_type := (others => (others => '0'));
signal RAM : memory_type := (others => (others => '0'));
signal RegisterFile : memory_type := (others => (others => '0'));


begin

ROM(0) <= "00100100000010100000000000000000";
ROM(1) <= "00100100000010010000000000000001";
ROM(2) <= "00100100000010000000000000000000";
ROM(3) <= "00100100000010110000000000000100";
ROM(4) <= "00100100000011000010000000000000";
ROM(5) <= "10101101100010100000000000000000";

ROM(6) <= "00000001010010010101000000100001";
ROM(7) <= "00100101100011000000000000000100";
ROM(8) <= "00101101010000010000000000010000";
ROM(9) <= "00010100001000001111111111111011";
ROM(10) <= "00100101100011000000000000001000";

ROM(11) <= "00000001010010010101000000100011";
ROM(12) <= "10101101100010101111111111111000";
ROM(13) <= "00000001100010110110000000100001";
ROM(14) <= "00010001010000000000000000000001";
ROM(15) <= "00001000000000000000000000001011";

ROM(16) <= "00100100000011000001111111111000";
ROM(17) <= "00100100000010110000000000100000";
ROM(18) <= "10001101100011010000000000001000";
ROM(19) <= "00100101101011011000000000000000";
ROM(20) <= "10101101100011010000000000001000";

ROM(21) <= "00000001010010010101000000100001";
ROM(22) <= "00100101100011000000000000000100";
ROM(23) <= "00000001010010110000100000101011";
ROM(24) <= "00010100001000001111111111111001";
ROM(25) <= "00100100010000100000000000001010";
ROM(26) <= "00000000000000000000000000001100";


CPU_Operations: process(clk)
-- Variable for "In process work"  --
variable immed_signextend : std_logic_vector(31 downto 0);
variable immed_branch : integer ;
variable immed_offset : integer ;
variable address_int : integer ;
variable load_offset : integer ;
variable store_offset : integer ;

variable instruct : std_logic_vector(31 downto 0) := (others => '0');
variable opcode : std_logic_vector(5 downto 0);
variable rs, rt, rd : integer;
variable funct : std_logic_vector(5 downto 0);
variable immed : std_logic_vector(15 downto 0);
variable address : std_logic_vector(25 downto 0);
variable newPC : integer := 0;
variable run : boolean := TRUE;


begin

	if (reset = '1') then
			PC <= 0;

	elsif (rising_edge(clk)) then
		
		if (run) then
		
			-- Here regardless of what instruction type is being dealt with, all signals are updated with appropriate bits 
			newPC := PC + 1;    -- Increments in words (one word per instruction)
			instruct := ROM(PC);   
			opcode := instruct(31 downto 26);
			rs := to_integer(unsigned(instruct(25 downto 21)));
			rt := to_integer(unsigned(instruct(20 downto 16)));
			rd := to_integer(unsigned(instruct(15 downto 11)));
			funct := instruct(5 downto 0);
			address := instruct(25 downto 0);
			
			if (instruct(15 downto 0) = memory_fix_first) then
				immed := std_logic_vector(unsigned(instruct(15 downto 0)) - unsigned(memory_fix_first));
				
			elsif (instruct(15 downto 0) = memory_fix_second) then
				immed := neg1;
			
			elsif (instruct(15 downto 0) = shift_4_fix) then
				immed := std_logic_vector(shift_right(unsigned(instruct(15 downto 0)), 2));
				
			elsif (instruct(15 downto 0) = shift_4neg_fix) then
				immed := std_logic_vector ((shift_right(unsigned(instruct(15 downto 0)), 2)) + X"C000" );
				
				
			elsif (instruct(15 downto 0) = shift_8_fix) then
				immed := std_logic_vector(shift_right(unsigned(instruct(15 downto 0)), 2));
				
			elsif (instruct(15 downto 0) = shift_8neg_fix) then
				immed := std_logic_vector ( (shift_right(unsigned(instruct(15 downto 0)), 2)) + X"C000" );
				
			else
				immed := instruct(15 downto 0);   --Note: The value 32768 is positive in ROM[19] but needs to be converted to "-32768"
			end if;
			
			case opcode is
			
				when addiu =>
					-- This is an I-type instruction where rs is source and rt in destination --
					-- Sign extention --
					if (immed(15) = '0') then
						immed_signextend := X"0000" & immed;
						RegisterFile(rt) <= std_logic_vector(unsigned(RegisterFile(rs)) + unsigned(immed_signextend));
					elsif (immed(15) = '1') then
						immed_signextend := X"FFFF" & immed;
						RegisterFile(rt) <= std_logic_vector(unsigned(RegisterFile(rs)) + unsigned(immed_signextend));    
					end if;
			
			
				-- The issue is when we get to
			
				when beq =>
					-- This is an I-type instruction --
					if (RegisterFile(rs) = RegisterFile(rt)) then
						if (immed(15) = '0') then
							immed_signextend := X"0000" & immed;
							immed_branch := to_integer(unsigned(immed_signextend));
							newPC := PC + (immed_branch + 1);     -- since PC has not changed yet the offset needs to be more by 1
						elsif (immed(15) = '1') then
							immed_signextend := X"FFFF" & immed;
							immed_branch := to_integer(unsigned(immed_signextend));
							newPC := PC + (immed_branch + 1);    -- since PC has not changed yet the offset needs to be less by 1
						end if;
					end if;
					
				when bne =>
					-- This is an I-type instruction --
					if (RegisterFile(rs) /= RegisterFile(rt)) then
						if (immed(15) = '0') then
							immed_signextend := X"0000" & immed;
							immed_branch := to_integer(unsigned(immed_signextend));
							newPC := PC + (immed_branch + 1);    -- since PC has not changed yet the offset needs to be more by 1
						elsif (immed(15) = '1') then
							immed_signextend := X"FFFF" & immed;
							immed_branch := to_integer(unsigned(immed_signextend));
							newPC := PC + (immed_branch + 1);   -- since PC has not changed yet the offset needs to be less by 1
						end if;
					end if;
					
				when j =>
					-- This is an I-type instruction --
					address_int := to_integer(unsigned(address));
					newPC := address_int;
					
				when lw =>
					-- This is an I-type instruction --
					if (immed(15) = '0' ) then
						immed_signextend := X"0000" & immed;
						immed_offset := to_integer(unsigned(immed_signextend));
						load_offset := to_integer(unsigned(RegisterFile(rs)));
						RegisterFile(rt) <= RAM(load_offset + immed_offset);
						
					elsif (immed(15) = '1') then
						immed_signextend := X"FFFF" & immed;
						immed_offset := to_integer(unsigned(immed_signextend));
						load_offset := to_integer(unsigned(RegisterFile(rs)));
						RegisterFile(rt) <= RAM(load_offset + immed_offset);
					end if;
					
					
				when sltiu =>
					-- This is an I-type instruction --
					if (immed(15) = '0') then
							immed_signextend := X"0000" & immed;
					elsif (immed(15) = '1') then
						immed_signextend := X"FFFF" & immed;
					end if;
					
					if (RegisterFile(rs) < immed_signextend) then
						RegisterFile(rt) <= X"FFFFFFFF";
					else
						RegisterFile(rt) <= X"00000000";
					end if;
					
					
				when sw =>
					-- This is an I-type instruction --
					if (immed(15) = '0') then
						immed_signextend := X"0000" & immed;
						immed_offset := to_integer(unsigned(immed_signextend));
						store_offset := to_integer(unsigned(RegisterFile(rs)));
						RAM(store_offset + immed_offset) <= RegisterFile(rt);
						
					elsif (immed(15) = '1') then
						immed_signextend := X"FFFF" & immed;
						immed_offset := to_integer(unsigned(immed_signextend));
						store_offset := to_integer(unsigned(RegisterFile(rs)));
						RAM(store_offset + immed_offset) <= RegisterFile(rt);
					end if;
					
					
				when empty =>
					-- These are ALL R-type instructions --
					case funct is
						when addu_fun =>
							RegisterFile(rd) <= std_logic_vector(unsigned(RegisterFile(rs)) + unsigned(RegisterFile(rt)));
						
						when sltu_fun =>
							if (RegisterFile(rs) < RegisterFile(rt)) then
								RegisterFile(rd) <= X"FFFFFFFF";
							else
								RegisterFile(rd) <= X"00000000";
							end if;
				
						when subu_fun =>
							RegisterFile(rd) <= std_logic_vector(unsigned(RegisterFile(rs)) - unsigned(RegisterFile(rt)));
						
						when syscall_fun =>
							run := FALSE;     
						
						when others =>
							funct := "111111";
							
					end case;
			
				when others =>
						opcode := "111111";
			end case;
         
			PC <= newPC;
			running <= run;
		end if;
	end if;
end process;

stop <= running;
end architecture sequential;