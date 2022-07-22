library ieee;
use ieee.std_logic_1164.all;
package Gates is
  component INVERTER is
   port (A: in std_logic; Y: out std_logic);
  end component INVERTER;

  component AND_2 is
   port (A, B: in std_logic; Y: out std_logic);
  end component AND_2;

  component NAND_2 is
   port (A, B: in std_logic; Y: out std_logic);
  end component NAND_2;

  component OR_2 is
   port (A, B: in std_logic; Y: out std_logic);
  end component OR_2;

  component NOR_2 is
   port (A, B: in std_logic; Y: out std_logic);
  end component NOR_2;

  component XOR_2 is
   port (A, B: in std_logic; Y: out std_logic);
  end component XOR_2;

  component XNOR_2 is
   port (A, B: in std_logic; Y: out std_logic);
  end component XNOR_2;

  component HALF_ADDER is
   port (A, B: in std_logic; S, C: out std_logic);
  end component HALF_ADDER;
  
  component AND_3 is 
   port (A, B, C: in std_logic; Y: out std_logic);
  end component AND_3;
 
  component AND_4 is 
   port (A, B, C, D: in std_logic; Y: out std_logic);
  end component AND_4; 
  
  component OR_3 is
   port (A, B, C: in std_logic; Y: out std_logic);
  end component OR_3;  

  component FULL_ADDER is
   port ( A, B, Cin: in std_logic; S, C: out std_logic);
  end component FULL_ADDER;
  
  component mux2to1 is
port(zero,one:in std_logic_vector(15 downto 0);
     output:out std_logic_vector(15 downto 0); 
     sel:in std_logic);
  end component mux2to1;
  
  component div is
         generic(
        N : integer:=8; -- operand width
        NN : integer:=16 -- result width
        );
    port (
        Nu: in std_logic_vector(N-1 downto 0);-- Nu (read numerator) is dividend
        D: in std_logic_vector(N-1 downto 0);-- D (read Denominator) is divisor
        RQ: out std_logic_vector((NN)-1 downto 0)--upper N bits of RQ will have remainder and lower N bits will have quotient
    ) ;
  end component div;
  
  component AND_1bit is
     port ( A: in std_logic_vector( 0 downto 0);
	          B: in std_logic_vector( 0 downto 0);
			 O: out std_logic_vector (0 downto 0));
	end component AND_1bit;
	
  component OR_1bit is
     port ( A: in std_logic_vector( 0 downto 0);
	          B: in std_logic_vector( 0 downto 0);
			 O: out std_logic_vector (0 downto 0));
	end component OR_1bit;
	
  component Add16 is
	port ( A,B : in std_logic_vector(15 downto 0);
		carry:out std_logic;
		X   : out std_logic_vector(15 downto 0)); 
	end component Add16;	
	
  component mux4to1 is
port(three,two,one,zero:in std_logic_vector(15 downto 0);
     output:out std_logic_vector(15 downto 0); 
     sel:in std_logic_vector(1 downto 0));
  end component mux4to1;
  
	component register_1 is 
		port ( data_in: in std_logic;
			data_out: out std_logic;
			LD_reg: in std_logic;
			Reset,clk: in std_logic);
	end component register_1;
	
   component checkEquality is
		port ( A,B : in std_logic_vector(15 downto 0);------inputs to be checked if equal or not
		X   : out std_logic_vector(15 downto 0)------output 1/0 reflecting equality
		);
	end component checkEquality;
	
	component Nand16 is
		port ( 
		A,B : in std_logic_vector(15 downto 0);
		X   : out std_logic_vector(15 downto 0));
	end component Nand16;

	component register_8 is 
	port ( 
		data_in: in std_logic_vector(7 downto 0);
		data_out: out std_logic_vector(7 downto 0);
		LD_reg: in std_logic;
		Reset,clk: in std_logic
		);
	end component register_8; 


	
	component register_16 is 
		port ( 
			data_in: in std_logic_vector(15 downto 0);
			data_out: out std_logic_vector(15 downto 0);
			LD_reg: in std_logic;
			Reset,clk: in std_logic
			);
	end component register_16;  
	
	 component PriorityEncoder is
		port (x7 ,x6 ,x5 ,x4 ,x3 ,x2 ,x1 ,x0:in std_logic;
		 s2 ,s1 ,s0 ,N:out std_logic);
		end component PriorityEncoder ;
		
	component Index_decode is	
		port
		(index_input	: in std_logic_vector(2 downto 0);
			all_zero_input	: in std_logic;
			reset_out	: 	out std_logic_vector(7 downto 0));
	end component Index_decode;
	
	component zero_hard is	
	port ( data_in: in std_logic_vector(15 downto 0);
		zero_out	: out std_logic);
	end component zero_hard;
	
	component decode_3to8 is
	port(address 	: in std_logic_vector(2 downto 0);
		output	: out std_logic_vector(7 downto 0));
   end component decode_3to8;
  
  
--//////////////////////////////////////////////////////////////  
--///////////////end of gates///////////////////////////////////
--//////////////////////////////////////////////////////////////  
end package Gates;



library ieee;
use ieee.std_logic_1164.all;
entity INVERTER is
   port (A: in std_logic; Y: out std_logic);
end entity INVERTER;

architecture Equations of INVERTER is
begin
   Y <= not A;
end Equations;
  

library ieee;
use ieee.std_logic_1164.all;
entity AND_2 is
   port (A, B: in std_logic; Y: out std_logic);
end entity AND_2;

architecture Equations of AND_2 is
begin
   Y <= A and B;
end Equations;
  
library ieee;
use ieee.std_logic_1164.all;
entity NAND_2 is
   port (A, B: in std_logic; Y: out std_logic);
end entity NAND_2;

architecture Equations of NAND_2 is
begin
   Y <= not (A and B);
end Equations;
  
library ieee;
use ieee.std_logic_1164.all;
entity OR_2 is
   port (A, B: in std_logic; Y: out std_logic);
end entity OR_2;

architecture Equations of OR_2 is
begin
   Y <= A or B;
end Equations;
  
library ieee;
use ieee.std_logic_1164.all;
entity NOR_2 is
   port (A, B: in std_logic; Y: out std_logic);
end entity NOR_2;

architecture Equations of NOR_2 is
begin
   Y <= not (A or B);
end Equations;
  

library ieee;
use ieee.std_logic_1164.all;
entity XOR_2 is
   port (A, B: in std_logic; Y: out std_logic);
end entity XOR_2;

architecture Equations of XOR_2 is
begin
   Y <= A xor B;
end Equations;
  
library ieee;
use ieee.std_logic_1164.all;
entity XNOR_2 is
   port (A, B: in std_logic; Y: out std_logic);
end entity XNOR_2;

architecture Equations of XNOR_2 is
begin
   Y <= not (A xor B);
end Equations;
  
library ieee;
use ieee.std_logic_1164.all;
entity HALF_ADDER is
   port (A, B: in std_logic; S, C: out std_logic);
end entity HALF_ADDER;

architecture Equations of HALF_ADDER is
begin
   S <= (A xor B);
   C <= (A and B);
end Equations;

library ieee;
use ieee.std_logic_1164.all;
entity AND_3 is
   port (A, B, C: in std_logic; Y: out std_logic);
end entity AND_3;

architecture Equations of AND_3 is
begin
   Y <= A and B and C;
end Equations;  

library ieee;
use ieee.std_logic_1164.all;
entity AND_4 is
   port (A, B, C, D: in std_logic; Y: out std_logic);
end entity AND_4;

architecture Equations of AND_4 is
begin
   Y <= A and B and C and D;
end Equations;

library ieee;
use ieee.std_logic_1164.all;
entity OR_3 is
   port (A, B, C: in std_logic; Y: out std_logic);
end entity OR_3;

architecture Equations of OR_3 is
begin
   Y <= A or B or C;
end Equations;


library ieee;
use ieee.std_logic_1164.all;
entity FULL_ADDER is
   port ( A, B, Cin: in std_logic; S, C: out std_logic);
end entity FULL_ADDER;

architecture Equations of Full_Adder is
  signal tC, tS, V: std_logic;
begin
  
  tS <= (A xor B);
  tC <= (A and B);
  V <= (Cin and tS);
  S <= (tS xor Cin);
  C <= (V xor tC);
end Equations;
---------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;

entity mux2to1 is
port(zero,one:in std_logic_vector(15 downto 0);
     output:out std_logic_vector(15 downto 0); 
     sel:in std_logic);
end entity;

architecture Equations of mux2to1 is


begin
	
process(zero,one,sel)
variable temp: std_logic_vector(15 downto 0); 
begin

case sel is 
	when '0' =>
        temp:=zero;
    when '1' =>
        temp:=one;
    when others =>
        temp:="XXXXXXXXXXXXXXXX";
end case;

output<= temp;

end process;

end Equations;

------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;

entity div is
    generic(
        N : integer:=8; -- operand width
        NN : integer:=16 -- result width
        );
    port (
        Nu: in std_logic_vector(N-1 downto 0);-- Nu (read numerator) is dividend
        D: in std_logic_vector(N-1 downto 0);-- D (read Denominator) is divisor
        RQ: out std_logic_vector((NN)-1 downto 0)--upper N bits of RQ will have remainder and lower N bits will have quotient
    ) ;
end div;

architecture beh of div is
    -- unconstrained 1D x 1D array
    type pr_type is array (natural range<>) of std_logic_vector(NN-1 downto 0);
    
    -- subtractor function. [Usage: var := sub(X,Y) where var is a variable
    --                                                  and X,Y are two 4-bit inputs for subtractor]
    function sub(A: in std_logic_vector; B: in std_logic_vector)
        return std_logic_vector is
            -- variable declaration
            variable W : integer := A'length;
            variable diff : std_logic_vector(W downto 0):= (others=>'0');
            variable borrow : std_logic_vector(W downto 0):= (0 => '1', others=>'0');
            variable B_sign: std_logic_vector(A'length-1 downto 0):=(others=>'0');
        begin
            B_sign(B'length-1 downto 0) := not B;
            for i in 0 to W-1 loop
                diff(i) := A(i) xor B_sign(i) xor borrow(i);
                borrow(i+1) := (A(i) and B_sign(i)) or (borrow(i) and (A(i) xor B_sign(i)));
            end loop;
            diff(W) := not borrow(W);
            return diff;
    end sub;

begin


division : process(Nu, D)
-- Here Nu (read numerator) is dividend and D (read denominator) is divisor
-- variable k holds length of dividend
variable k : integer := Nu'length;

-- declare variable to hold partial remainder for subsequent iteration  
variable pr : pr_type(0 to k) := (others=>(others=>'0'));

-- declare variable to hold difference from subtractor
variable diff : std_logic_vector(k downto 0) := (others=>'0');

-- declare temporary variable to hold prior partial product in case difference is negative
variable temp : std_logic_vector(k-1 downto 0) := (others=>'0');
begin
    -- initializing partial remainder
    pr(0)(k-1 downto 0):= Nu;

    for i in 0 to k-1 loop
        pr(i+1) := pr(i)(k*2-2 downto 0) & '0';-- shift left
        temp := pr(i+1)(k*2-1 downto k);-- preserve prior partial remainder
        diff := sub(temp, D);-- subtraction of divisor from partial remainder

        if diff(k) = '0' then-- difference is positive
            pr(i+1)(k*2-1 downto k) := diff(k-1 downto 0);
            pr(i+1)(0) := '1';
        else-- difference is negative
            pr(i+1)(0) := '0';
        end if;
    end loop;
    RQ <= pr(k);-- final result assignment
end process ; -- division
end beh ; -- beh



library ieee;
use ieee.std_logic_1164.all;
entity AND_1bit is
     port ( A: in std_logic_vector( 0 downto 0);
	          B: in std_logic_vector( 0 downto 0);
			 O: out std_logic_vector (0 downto 0));
end entity AND_1bit;

architecture Equations of AND_1bit is
begin
   O(0) <= A(0) and B(0);
end Equations;


library ieee;
use ieee.std_logic_1164.all;
entity OR_1bit is
     port ( A: in std_logic_vector( 0 downto 0);
	          B: in std_logic_vector( 0 downto 0);
			 O: out std_logic_vector (0 downto 0));
end entity OR_1bit;

architecture Equations of OR_1bit is
begin
   O(0) <= A(0) or B(0);
end Equations;

--AKSJDFJKASNV

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity Add16 is 
	port ( 
		A,B : in std_logic_vector(15 downto 0);----inputs to be added
		carry:out std_logic;		       ----carry flag to be set
		X   : out std_logic_vector(15 downto 0)-----output after addition
		);
end entity Add16;

architecture Equations of Add16 is
signal inputa,inputb,output : signed(16 downto 0);
signal in_vector_a, in_vector_b,out_vector	: std_logic_vector(16 downto 0);
begin
		in_vector_a <= a(15) & a;----inputs with sign
		in_vector_b <= b(15) & b;
		inputa <= signed(in_vector_a);
		inputb <= signed(in_vector_b);
		output <= inputa + inputb;------signed addition
		out_vector <= std_logic_vector(output);
		X <= out_vector(16) & out_vector(14 downto 0);
		carry <= out_vector(15);-----carry to be set as flag
	end Equations;
-------------------------------------------------------------------------------------	
	
library ieee;
use ieee.std_logic_1164.all;

entity mux4to1 is
port(three,two,one,zero:in std_logic_vector(15 downto 0);
     output:out std_logic_vector(15 downto 0); 
     sel:in std_logic_vector(1 downto 0));
end entity;

architecture Equations of mux4to1 is


begin
	
process(zero,one,sel,three,two)
variable temp: std_logic_vector(15 downto 0); 
begin

case sel is 
	when "00" =>
        temp:=zero;
    when "01" =>
        temp:=one;
    when "10" =>
        temp:=two;
    when "11" =>
        temp:=three;
    when others =>
        temp:="XXXXXXXXXXXXXXXX";
end case;

output<= temp;

end process;

end Equations;
-------------------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;

entity register_1 is 
	port ( 
		data_in: in std_logic;
		data_out: out std_logic;
		LD_reg: in std_logic;
		Reset,clk: in std_logic
		);
end entity register_1;

architecture Equations of register_1 is 
	begin
		process(Reset,clk,data_in,LD_reg)
			variable data: std_logic;
			begin
				if(Reset = '1') then
						data := '0';
				elsif(clk'event and (clk = '1')) then
					if (LD_reg = '1') then
						data := data_in;
					else 
						data := data;
					end if;
				else 
					data := data;
				end if;
				data_out <= data;
			end process;
	end Equations;

--------------------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;

entity checkEquality is 
	port ( 
		A,B : in std_logic_vector(15 downto 0);------inputs to be checked if equal or not
		X   : out std_logic_vector(15 downto 0)------output 1/0 reflecting equality
		);
end entity checkEquality;


architecture Equations of checkEquality is 
	begin
		process(A, B)
			variable tempC : std_logic_vector( 15 downto 0 );
            variable equal : std_logic;  ----flag generating output
            variable Xtemp : std_logic_vector(15 downto 0);
			begin	
			equal := '1';
				for i in 0 to 15 loop
				tempC(i):=A(i) xor B(i); ----bit wise XOR to check equality(if equal o/p=0,if not o/p=1)
				if(tempC(i) = '1') then-----(if previous one is not equal,final o/p=0,i.e. inputs are not equal)
					equal := '0';		
				end if;  
				end loop;
				
				if(equal = '1') then
					Xtemp := "0000000000000001";----final output
                else Xtemp := "0000000000000000";    
				end if;
					X <= Xtemp;
		end process;
	end Equations;
	
-------------------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;

entity Nand16 is 
	port ( 
		A,B : in std_logic_vector(15 downto 0);----inputs
		X   : out std_logic_vector(15 downto 0)----output of nand operation
		);
end entity Nand16;

architecture Equations of Nand16 is 
	begin
		X(0) <= A(0) nand B(0);---bitwise nand operation
		X(1) <= A(1) nand B(1);
		X(2) <= A(2) nand B(2);
		X(3) <= A(3) nand B(3);
		X(4) <= A(4) nand B(4);
		X(5) <= A(5) nand B(5);
		X(6) <= A(6) nand B(6);
		X(7) <= A(7) nand B(7);
		X(8) <= A(8) nand B(8);
		X(9) <= A(9) nand B(9);
		X(10) <= A(10) nand B(10);
		X(11) <= A(11) nand B(11);
		X(12) <= A(12) nand B(12);
		X(13) <= A(13) nand B(13);
		X(14) <= A(14) nand B(14);
		X(15) <= A(15) nand B(15);

	end Equations;
	
----------------------------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;

entity register_8 is 
	port ( 
		data_in: in std_logic_vector(7 downto 0);
		data_out: out std_logic_vector(7 downto 0);
		LD_reg: in std_logic;
		Reset,clk: in std_logic
		);
end entity register_8;

architecture Equations of register_8 is 
	begin
		process(Reset,clk,data_in,LD_reg)
			variable data: std_logic_vector(7 downto 0);
			begin
				if(Reset = '1') then								--FIRST CHECK IF RESET IS EQUAL TO 1
						data := "00000000";						-- IF RESET 1 SET VALUE = 0
				elsif(clk'event and (clk = '1')) then		-- WHEN CLOCK IS HIGH AND WRITE ENABLE IS ON
					if (LD_reg = '1') then
						data := data_in;							-- WRITE DATA
					else 
						data := data;
					end if;
				else 
					data := data;
				end if;
				data_out <= data;									-- OUTPUT DEFINATION
			end process;
	end Equations;
----------------------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;

entity register_16 is 
	port ( 
		data_in: in std_logic_vector(15 downto 0);
		data_out: out std_logic_vector(15 downto 0);
		LD_reg: in std_logic;
		Reset,clk: in std_logic
		);
end entity register_16;

architecture Equations of register_16 is 
	begin
		process(Reset,clk,data_in,LD_reg)
			variable data: std_logic_vector(15 downto 0);
			begin
				if(Reset = '1') then
						data := "0000000000000000";
				elsif(clk'event and (clk = '1')) then
					if (LD_reg = '1') then
						data := data_in;
					else 
						data := data;
					end if;
				else 
					data := data;
				end if;
				data_out <= data;
			end process;
	end Equations;
-------------------------------------------------------------------------------------------

library ieee ;
use ieee . std_logic_1164 .all ;
 entity PriorityEncoder is
port (x7 ,x6 ,x5 ,x4 ,x3 ,x2 ,x1 ,x0:in std_logic;
 s2 ,s1 ,s0 ,N:out std_logic);
end PriorityEncoder ;
 architecture Equations of PriorityEncoder is
begin
 N <= not(x7 or x6 or x5 or x4 or x3 or x2 or x1 or x0);
s0 <= (x1 and not x0) or
 (x3 and not x2 and not x1 and not x0) or
(x5 and not x4 and not x3 and not x2 and
 not x1 and not x0) or
(x7 and not x6 and not x5 and not x4
 and not x3 and not x2 and not x1
and not x0);
 s1 <= (x2 and not x1 and not x0) or
(x3 and not x2 and not x1 and not x0) or
 (x6 and not x5 and not x4 and not x3 and
not x2 and not x1 and not x0) or
 (x7 and not x6 and not x5 and not x4 and
not x3 and not x2 and not x1 and not x0);
 s2 <= (x4 and not x3 and not x2 and
not x1 and not x0) or
 (x5 and not x4 and not x3 and not x2 and
not x1 and not x0) or
 (x6 and not x5 and not x4 and not x3
and not x2 and not x1 and not x0) or
 (x7 and not x6 and not x5 and not x4 and not x3
and not x2 and not x1 and not x0);
 end Equations ;
 
 ----------------------------------------------------------------------------------------

 library ieee;
use ieee.std_logic_1164.all;

entity Index_decode is
	port
	(
		index_input	: in std_logic_vector(2 downto 0);
		all_zero_input	: in std_logic;
		reset_out	: 	out std_logic_vector(7 downto 0)
	);
end entity Index_decode;


architecture Equations of Index_decode is
begin
	reset_out(0) <=   (not all_zero_input) and (not index_input(0)) and (not index_input(1)) and (not index_input(2));
	reset_out(1) <=   (not all_zero_input) and (index_input(0)) and (not index_input(1)) and (not index_input(2));
	reset_out(2) <=   (not all_zero_input) and (not index_input(0)) and (index_input(1)) and (not index_input(2));
	reset_out(3) <=   (not all_zero_input) and (index_input(0)) and (index_input(1)) and (not index_input(2));
	reset_out(4) <=   (not all_zero_input) and (not index_input(0)) and (not index_input(1)) and (index_input(2));
	reset_out(5) <=   (not all_zero_input) and (index_input(0)) and (not index_input(1)) and (index_input(2));
	reset_out(6) <=   (not all_zero_input) and (not index_input(0)) and (index_input(1)) and (index_input(2));
	reset_out(7) <=   (not all_zero_input) and (index_input(0)) and (index_input(1)) and (index_input(2));
end Equations;

---------------------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;

entity zero_hard is 
	port ( 
		data_in: in std_logic_vector(15 downto 0);
		zero_out	: out std_logic
		);
end entity zero_hard;

architecture Equations of zero_hard is
begin 
	zero_out <= not(data_in(0) or data_in(1) or data_in(2) or data_in(3) or data_in(4) or data_in(5) or data_in(6) or data_in(7) or data_in(8) or data_in(9)
					 or data_in(10) or data_in(11) or data_in(12) or data_in(13) or data_in(14) or data_in(15));
end Equations;
----------------------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;

entity decode_3to8 is
	port(address 	: in std_logic_vector(2 downto 0);
		output	: out std_logic_vector(7 downto 0));
end entity decode_3to8;

architecture Equations of decode_3to8 is
begin 
	process(address)
		variable temp: std_logic_vector(7 downto 0) := "00000000";
		begin
		temp := "00000000";							--TEMPORARY VARIABLE DEFINED
			case address is 
						when "000" =>
							temp(0) := '1';
						when "001" =>
							temp(1) := '1';
						when "010" =>
							temp(2) := '1';
						when "011" =>
							temp(3) := '1';
						when "100" =>
							temp(4) := '1';
						when "101" =>
							temp(5) := '1';
						when "110" =>
							temp(6) := '1';
						when "111" =>
							temp(7) := '1';
						when others =>
							temp(0) := '0';
					end case;
		
		output <= temp;							--OUTPUT DEFINITION
		end process;
end Equations;
--------------------------------------------------------------------------------------------
