library ieee;
use ieee.std_logic_1164.all;
library work;
use work.Gates.all;

entity Priority_logic is												-- ENTITY DEFINITION
	port
	(	IR_input	: in std_logic_vector(7 downto 0);				-- INPUT OF 8 INSTRUCTION BITS SHOWING ADDRESS
		address_out: 	out std_logic_vector(2 downto 0);		-- REGISTER ADDRSS TO WHICH DATA IS TO BE WRITTEN
		all_zero							: out std_logic;				-- ALL ZERO INDICATING NO ADDRESS IS TO WRITTEN
		LD_Index,LD_Pre_priority	: in std_logic;				-- LD_Pre_priority WRITE ENABLE TO WRITE TO REGISTER WHICH STORES ADDRESS FROM INSTRUCTION REGISTER
		clk,reset						: in std_logic
	);
end entity Priority_logic;

architecture d_priority of Priority_logic is

-------------------------------------------------------
--------------------------------------------------------
signal Pre_Priority_reg_out,reset_pre_priority_reg,index_reg_in	: std_logic_vector(7 downto 0);
signal input_from_PE	: std_logic_vector(2 downto 0);
signal temp_all_zero	: std_logic;
signal reset_reg_reset	: std_logic;
begin
	Pre_Priority_reg0:	register_1 port map (IR_input(0),Pre_Priority_reg_out(0),LD_Pre_priority,reset_pre_priority_reg(0),clk);
	Pre_Priority_reg1:	register_1 port map (IR_input(1),Pre_Priority_reg_out(1),LD_Pre_priority,reset_pre_priority_reg(1),clk);
	Pre_Priority_reg2:	register_1 port map (IR_input(2),Pre_Priority_reg_out(2),LD_Pre_priority,reset_pre_priority_reg(2),clk);
	Pre_Priority_reg3:	register_1 port map (IR_input(3),Pre_Priority_reg_out(3),LD_Pre_priority,reset_pre_priority_reg(3),clk);
	Pre_Priority_reg4:	register_1 port map (IR_input(4),Pre_Priority_reg_out(4),LD_Pre_priority,reset_pre_priority_reg(4),clk);
	Pre_Priority_reg5:	register_1 port map (IR_input(5),Pre_Priority_reg_out(5),LD_Pre_priority,reset_pre_priority_reg(5),clk);
	Pre_Priority_reg6:	register_1 port map (IR_input(6),Pre_Priority_reg_out(6),LD_Pre_priority,reset_pre_priority_reg(6),clk);
	Pre_Priority_reg7:	register_1 port map (IR_input(7),Pre_Priority_reg_out(7),LD_Pre_priority,reset_pre_priority_reg(7),clk);
	
	priority_encoder: PriorityEncoder port map (Pre_Priority_reg_out(7),Pre_Priority_reg_out(6),Pre_Priority_reg_out(5),Pre_Priority_reg_out(4),
																Pre_Priority_reg_out(3),Pre_Priority_reg_out(2),Pre_Priority_reg_out(1),Pre_Priority_reg_out(0),
																input_from_PE(2),input_from_PE(1),input_from_PE(0),temp_all_zero);
																
	index : register_8 port map (index_reg_in,reset_pre_priority_reg,LD_Index,Reset_reg_reset,Clk);
	
	reset_reg_reset <= reset or temp_all_zero;
	decoder				: 	index_decode port map (input_from_PE,temp_all_zero,index_reg_in);
	address_out <= input_from_PE;
	all_zero <= temp_all_zero;
end;