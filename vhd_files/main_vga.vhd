library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

entity top_vga is
    port (
        clk20_in : in  std_logic;  -- 20 MHz external clock
        red      : out std_logic;
        green    : out std_logic;
        blue     : out std_logic;
        hs_out   : out std_logic;
        vs_out   : out std_logic;
        fpga_btn : in  std_logic;
        led      : out std_logic;
		  led2      : out std_logic;
        big_btn  : in  std_logic;
        reward   : inout std_logic_vector(2 downto 0);
		  MSB		  : out std_logic;
		  b2		  : out std_logic;
		  LSB		  : out std_logic;
		  insert_coin : in std_logic;
		  sw1 : in std_logic;
		  sw2 : in std_logic;
		  sw3 : in std_logic;
		  ready : inout std_logic;
		  buzzer : out std_logic

    );
end top_vga;

architecture Behavioral of top_vga is
    signal clk25  : std_logic;
    signal locked : std_logic;
    signal btn    : std_logic;
    signal d_btn  : std_logic;
	 signal cheat_mode  : std_logic;
	 signal var_coin : std_logic;

begin
    -- Combine buttons
    btn <= big_btn or fpga_btn;
	 cheat_mode <= sw1 and sw2 and sw3;
    -- Clock generator (20 MHz -> 25 MHz)
    clk_gen_inst : entity work.clk25_gen
        port map (
            CLK_IN1  => clk20_in,
            CLK_OUT1 => clk25,
            LOCKED   => locked,
            RESET    => '0'
        );

    -- Debounce
    debounce_inst : entity work.debounce
        port map (
            CLK   => clk25,
            RST_N => '1',
            D     => btn,
            Q     => d_btn
        );
		  
	 debounce_inst2 : entity work.debounce
        port map (
            CLK   => clk25,
            RST_N => '1',
            D     => insert_coin,
            Q     => var_coin
        );

    -- VGA controller
    vga_inst : entity work.VGA
        port map (
            clk25_in    => clk25,
            red         => red,
            green       => green,
            blue        => blue,
            hs_out      => hs_out,
            vs_out      => vs_out,
            -- Optional:
            btn        => d_btn,
            reward_out => reward,
				insert_coin => var_coin,
				cheat_mode_on => cheat_mode,
				ready => ready,
				buzzer => buzzer
        );

    -- Debug LED shows button press
    led <= d_btn;
	 led2 <= ready;
	 LSB <= reward(0);
	 b2 <= reward(1);
	 MSB <= reward(2);

end Behavioral;