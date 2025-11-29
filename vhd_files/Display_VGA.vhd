library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_ARITH.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;

entity VGA is
    port (
        clk25_in : in std_logic;
        btn : in std_logic;
        red : out std_logic;
        green : out std_logic;
        blue : out std_logic;
        hs_out : out std_logic;
        vs_out : out std_logic;
        
        reward_out : out std_logic_vector(2 downto 0);
        insert_coin : in std_logic;
		  cheat_mode_on : in std_logic;
		  ready : out std_logic;
		  buzzer : out std_logic

    );
end VGA;

 architecture Behavioral of VGA is

    -- Timing Signals
    signal h_count : integer range 0 to 799 := 0;
    signal v_count : integer range 0 to 524 := 0;
    signal h_sync, v_sync : std_logic;
    -- Pipelined Color Signals
    signal r, g, b : std_logic := '0';
    signal r_reg, g_reg, b_reg : std_logic := '0';

    --------------------------------------------------------
    -- Grid Constants
    --------------------------------------------------------
    constant LINE_WIDTH : integer := 2;
    constant SCREEN_WIDTH : integer := 640;
    constant SCREEN_HEIGHT : integer := 480;
    -- UPDATED CONSTANTS FOR REEL SIZE
    constant VISIBLE_ROWS : integer := 5;
    -- The number of symbols visible on screen (5 rows)
    constant REEL_SIZE : integer := 7;
    -- The total number of symbols in the data structure (7 rows deep)

    constant NUM_ROWS : integer := VISIBLE_ROWS;
    -- Used for calculating display size
    constant NUM_COLS : integer := 5;
    constant TITLE_HEIGHT : integer := 80;
    constant GRID_AREA_HEIGHT : integer := SCREEN_HEIGHT - TITLE_HEIGHT;
    constant CELL_WIDTH : integer := SCREEN_WIDTH / NUM_COLS;
    constant CELL_HEIGHT : integer := GRID_AREA_HEIGHT / NUM_ROWS;
    --------------------------------------------------------

    -- Clock/Mode Signals
    -- 10 Hz Rolling Speed
    constant COUNT_MAX : INTEGER := 630000;
    SIGNAL counter_reg : NATURAL RANGE 0 TO COUNT_MAX := 0;
    SIGNAL clkRoll : STD_LOGIC := '0';
    SIGNAL clkRoll_prev : STD_LOGIC := '0';
    
    -- Game State Machine
    -- 0 = IDLE (or Insert Coin)
    -- 1 = ROLLING
    -- 2 = WIN_ANIMATION
    Signal mode : Integer range 0 to 2 := 0;

    -- 4 Hz Blinking Signals for Next Column Highlight
    constant BLINK_COUNT_MAX : INTEGER := 3124999;
    SIGNAL blink_counter : NATURAL RANGE 0 TO BLINK_COUNT_MAX := 0;
    SIGNAL blink_4hz : STD_LOGIC := '0';
	 
	 -- Timer constant for 0.25 seconds at 25 MHz (25,000,000 * 0.25)
	 constant READY_PULSE_CYCLES : natural := 6250000;
	 -- Counter for the 0.25s duration
	 signal ready_timer_count : natural range 0 to READY_PULSE_CYCLES := 0;
	 -- Flag to show the timer is running
	 signal ready_timer_active : std_logic := '0';

    -- 5-Second Win Animation Timer
    constant WIN_ANIMATION_CYCLES : natural := 125000000; -- 5 seconds * 25,000,000 Hz
    signal win_timer_count : natural range 0 to WIN_ANIMATION_CYCLES := 0;

    signal column_to_blink : integer range 0 to NUM_COLS := 1;
    
    -- Symbol Types and Grid State
    type T_SYMBOL is (
        SYMBOL_NONE, SYMBOL_APPLE, SYMBOL_CHERRY, SYMBOL_CREEPER,
        SYMBOL_DIAMOND, SYMBOL_SEVEN, SYMBOL_EIGHT, SYMBOL_TOP_HAT
    );
    -- T_GRID_ARRAY uses REEL_SIZE (7) for the row count
    type T_GRID_ARRAY is array (1 to REEL_SIZE, 1 to NUM_COLS) of T_SYMBOL;

    -- Boolean grid to store *which* cells are part of a win
    type T_GRID_ARRAY_BOOL is array (1 to VISIBLE_ROWS, 1 to NUM_COLS) of boolean;
    signal winning_cells : T_GRID_ARRAY_BOOL := (others => (others => false));
    
    -- Grid State Signals
    signal current_grid_state : T_GRID_ARRAY;
    constant CHEAT_INIT : T_GRID_ARRAY := (
        (1 => SYMBOL_SEVEN, 2 => SYMBOL_EIGHT, 3 => SYMBOL_SEVEN, 4 => SYMBOL_SEVEN, 5 => SYMBOL_SEVEN),
        (1 => SYMBOL_TOP_HAT, 2 => SYMBOL_TOP_HAT, 3 => SYMBOL_EIGHT, 4 => SYMBOL_DIAMOND, 5 => SYMBOL_SEVEN),
        (1 => SYMBOL_DIAMOND, 2 => SYMBOL_SEVEN, 3 => SYMBOL_CREEPER, 4 => SYMBOL_EIGHT, 5 => SYMBOL_CHERRY),
        (1 => SYMBOL_APPLE, 2 => SYMBOL_APPLE, 3 => SYMBOL_SEVEN, 4 => SYMBOL_SEVEN, 5 => SYMBOL_SEVEN),
        (1 => SYMBOL_SEVEN, 2 => SYMBOL_APPLE, 3 => SYMBOL_SEVEN, 4 => SYMBOL_TOP_HAT, 5 => SYMBOL_DIAMOND),
        (1 => SYMBOL_CREEPER, 2 => SYMBOL_SEVEN, 3 => SYMBOL_SEVEN, 4 => SYMBOL_SEVEN, 5 => SYMBOL_SEVEN),
        (1 => SYMBOL_SEVEN, 2 => SYMBOL_SEVEN, 3 => SYMBOL_SEVEN, 4 => SYMBOL_SEVEN, 5 => SYMBOL_SEVEN)
    );
    
    constant REAL_INIT : T_GRID_ARRAY := (
		  (1 => SYMBOL_SEVEN, 2 => SYMBOL_EIGHT, 3 => SYMBOL_DIAMOND, 4 => SYMBOL_CREEPER, 5 => SYMBOL_APPLE),
        (1 => SYMBOL_APPLE, 2 => SYMBOL_SEVEN, 3 => SYMBOL_EIGHT, 4 => SYMBOL_DIAMOND, 5 => SYMBOL_TOP_HAT),
        (1 => SYMBOL_DIAMOND, 2 => SYMBOL_TOP_HAT, 3 => SYMBOL_CREEPER, 4 => SYMBOL_EIGHT, 5 => SYMBOL_CHERRY),
        (1 => SYMBOL_EIGHT, 2 => SYMBOL_DIAMOND, 3 => SYMBOL_CHERRY, 4 => SYMBOL_APPLE, 5 => SYMBOL_SEVEN),
        (1 => SYMBOL_CHERRY, 2 => SYMBOL_APPLE, 3 => SYMBOL_SEVEN, 4 => SYMBOL_TOP_HAT, 5 => SYMBOL_DIAMOND),
        (1 => SYMBOL_CREEPER, 2 => SYMBOL_CHERRY, 3 => SYMBOL_TOP_HAT, 4 => SYMBOL_SEVEN, 5 => SYMBOL_EIGHT),
        (1 => SYMBOL_TOP_HAT, 2 => SYMBOL_CREEPER, 3 => SYMBOL_APPLE, 4 => SYMBOL_CHERRY, 5 => SYMBOL_CREEPER)
	 );
    
    signal cheat : T_GRID_ARRAY := CHEAT_INIT;
    signal real_grid_state : T_GRID_ARRAY := REAL_INIT;
    
    -- Game State Signals
    signal btn_prev : std_logic := '0';
    signal insert_coin_prev : std_logic := '0';
    signal wheel_now : integer range 0 to NUM_COLS := 0;
    -- Last wheel that finished rolling
    signal first_run : std_logic := '1';
    -- Variables to limit play and track rewards
    signal rolls_remaining : integer range 0 to NUM_COLS := NUM_COLS;
    -- Starts at 5
    signal reward : integer range 0 to 5 := 0;
	 constant NOTE_OFF : integer := 0;
    constant NOTE_Bb4 : integer := 26811; -- Rolling Note 1 (466 Hz)
    constant NOTE_D5  : integer := 21281; -- Rolling Note 2 (587 Hz)
    constant NOTE_A5  : integer := 14205; -- Rolling Note 3 (880 Hz)
    constant NOTE_Bb5 : integer := 13405; -- Ka-ching Note (932 Hz)
    
    -- Duration for the "ka-ching" sound (0.1 seconds)
    constant KACHING_DURATION : integer := 2500000; 

    -- Signals to control the sound
    signal note_freq_max : integer range 0 to 26811 := 0;
    signal buzzer_out : std_logic := '0';
    signal rolling_note_state : integer range 0 to 2 := 0;
    signal kaching_timer : integer range 0 to KACHING_DURATION := 0;

    ----------------------------------------------------------------
    -- Win Check Function
    ----------------------------------------------------------------
    function check_win (grid : T_GRID_ARRAY) return integer is
        variable symbol : T_SYMBOL;
    begin
        if (grid(2, 2) = grid(1, 1)) and (grid(3, 3) = grid(1, 1)) and
           (grid(4, 4) = grid(1, 1)) and (grid(5, 5) = grid(1, 1)) then
            return 5;
        end if;
            
        if (grid(2, 4) = grid(1, 5)) and (grid(3, 3) = grid(1, 5)) and
           (grid(4, 2) = grid(1, 5)) and (grid(5, 1) = grid(1, 5)) then
            return 5;
        end if;
        
        rows_5: for r_idx in 1 to 5 loop
            symbol := grid(r_idx, 1);
            if (grid(r_idx, 2) = symbol) and (grid(r_idx, 3) = symbol) and
               (grid(r_idx, 4) = symbol) and (grid(r_idx, 5) = symbol) then
                return 5;
            end if;
        end loop rows_5;

        rows_3: for r_idx in 1 to 5 loop
            cols_3: for c_idx in 1 to 3 loop
                symbol := grid(r_idx, c_idx);
                if (grid(r_idx, c_idx + 1) = symbol) and
                   (grid(r_idx, c_idx + 2) = symbol) then
                    return 2;
                end if;
            end loop cols_3;
        end loop rows_3;
        
        return 0;
    end function check_win;

    -- Function to find winning cells
    function find_winning_cells (grid : T_GRID_ARRAY) return T_GRID_ARRAY_BOOL is
        variable symbol : T_SYMBOL;
        variable v_win_grid : T_GRID_ARRAY_BOOL := (others => (others => false));
    begin
        -- (Top-Left to Bottom-Right)
        if (grid(2, 2) = grid(1, 1)) and (grid(3, 3) = grid(1, 1)) and
           (grid(4, 4) = grid(1, 1)) and (grid(5, 5) = grid(1, 1)) then
            v_win_grid(1, 1) := true;
            v_win_grid(2, 2) := true;
            v_win_grid(3, 3) := true;
            v_win_grid(4, 4) := true;
            v_win_grid(5, 5) := true;
            return v_win_grid;
        end if;
            
        -- Top-Right to Bottom-Left)
        if (grid(2, 4) = grid(1, 5)) and (grid(3, 3) = grid(1, 5)) and
           (grid(4, 2) = grid(1, 5)) and (grid(5, 1) = grid(1, 5)) then
            v_win_grid(1, 5) := true;
            v_win_grid(2, 4) := true;
            v_win_grid(3, 3) := true;
            v_win_grid(4, 2) := true;
            v_win_grid(5, 1) := true;
            return v_win_grid;
        end if;

        -- 5-in-a-row (Horizontal)
        rows_5: for r_idx in 1 to 5 loop -- Rows 1 to 5 (visible)
            symbol := grid(r_idx, 1);
            if (grid(r_idx, 2) = symbol) and (grid(r_idx, 3) = symbol) and
               (grid(r_idx, 4) = symbol) and (grid(r_idx, 5) = symbol) then
                v_win_grid(r_idx, 1) := true;
                v_win_grid(r_idx, 2) := true;
                v_win_grid(r_idx, 3) := true;
                v_win_grid(r_idx, 4) := true;
                v_win_grid(r_idx, 5) := true;
                return v_win_grid;
            end if;
        end loop rows_5;

        -- 3-in-a-row (Horizontal)
        rows_3: for r_idx in 1 to 5 loop
            cols_3: for c_idx in 1 to 3 loop
                symbol := grid(r_idx, c_idx);
                if (grid(r_idx, c_idx + 1) = symbol) and
                   (grid(r_idx, c_idx + 2) = symbol) then
                    v_win_grid(r_idx, c_idx) := true;
                    v_win_grid(r_idx, c_idx + 1) := true;
                    v_win_grid(r_idx, c_idx + 2) := true;
                    return v_win_grid;
                end if;
            end loop cols_3;
        end loop rows_3;
        
        return v_win_grid;
    end function find_winning_cells;
   
begin

    -- Map internal reward signal to output port
    reward_out <= CONV_STD_LOGIC_VECTOR(reward, 3);
    ready <= ready_timer_active;
	 
	 ----------------------------------------------------------------
    -- Music Maker
    ----------------------------------------------------------------
	 SOUND_GENERATOR_PROCESS: process(clk25_in)
        variable note_counter : integer range 0 to 26811 := 0;
    begin
        if rising_edge(clk25_in) then
            if note_freq_max = NOTE_OFF then
                note_counter := 0;
                buzzer_out <= '0';
            else
                if note_counter = note_freq_max then
                    note_counter := 0;
                    buzzer_out <= not buzzer_out;
                else
                    note_counter := note_counter + 1;
                end if;
            end if;
        end if;
    end process;

    -- Connect the internal buzzer signal to the output port
    buzzer <= buzzer_out;
    ----------------------------------------------------------------
    -- VGA Timing Process
    ----------------------------------------------------------------
    VGA_TIMING_PROCESS: process(clk25_in)
    begin
        if rising_edge(clk25_in) then
            if h_count = 799 then
                h_count <= 0;
                if v_count = 524 then
                    v_count <= 0;
                else
                    v_count <= v_count + 1;
                end if;
            else
                h_count <= h_count + 1;
            end if;
        end if;
    end process;

    ----------------------------------------------------------------
    -- 4Hz Blinker Clock Process (Generates blink_4hz)
    ----------------------------------------------------------------
    BLINKER_PROCESS: process(clk25_in)
    begin
        if rising_edge(clk25_in) then
            if blink_counter = BLINK_COUNT_MAX then
                blink_counter <= 0;
                blink_4hz <= NOT blink_4hz;
            else
                blink_counter <= blink_counter + 1;
            end if;
        end if;
    end process BLINKER_PROCESS;

    ----------------------------------------------------------------
    -- Blinking Column Index Calculation (Concurrent)
    ----------------------------------------------------------------
    BLINK_TARGET_PROCESS : process(mode, wheel_now, rolls_remaining)
    begin
        if mode = 0 then 
            if rolls_remaining = 0 or wheel_now = NUM_COLS or wheel_now = 0then
                column_to_blink <= 1;
            else
                column_to_blink <= wheel_now + 1;
            end if;
				
        else -- System is ROLLING (mode = 1) or WIN (mode = 2)
            column_to_blink <= 0;
            -- Stop blinking when a wheel is actively rolling or winning
        end if;
    end process BLINK_TARGET_PROCESS;

    ----------------------------------------------------------------
    -- Output Pipelining Process
    ----------------------------------------------------------------
    OUTPUT_PIPELINE_PROCESS: process(clk25_in)
    begin
        if rising_edge(clk25_in) then
            r_reg <= r;
            g_reg <= g;
            b_reg <= b;
        end if;
    end process;
    ----------------------------------------------------------------
    -- Game Logic Process (with Insert Coin logic)
    ----------------------------------------------------------------
    GAME_LOGIC_PROCESS: process(clk25_in)
        variable next_mode : integer range 0 to 2;
        variable temp_symbol : T_SYMBOL;
        variable v_wheel_now : integer range 0 to NUM_COLS := 0;
        variable v_grid_copy : T_GRID_ARRAY;
        variable v_reward : integer range 0 to 5;
        
    begin
        if rising_edge(clk25_in) then
				if kaching_timer > 0 then
                kaching_timer <= kaching_timer - 1;
                note_freq_max <= NOTE_Bb5; -- Play high Bb
            
            elsif mode = 1 or mode = 2 then
                -- On the rising edge of the 10Hz roll clock...
                if (clkRoll = '1' and clkRoll_prev = '0') then
                    -- ...cycle to the next note
                    if rolling_note_state = 2 then
                        rolling_note_state <= 0;
                    else
                        rolling_note_state <= rolling_note_state + 1;
                    end if;
                end if;
                
                case rolling_note_state is
                    when 0 =>
                        note_freq_max <= NOTE_Bb4;
                    when 1 =>
                        note_freq_max <= NOTE_D5;
                    when others =>
                        note_freq_max <= NOTE_A5;
                end case;
            
            else
                note_freq_max <= NOTE_OFF;
            end if;
				
		  
				if cheat_mode_on = '1' and first_run = '1' then
						v_grid_copy := cheat;
						real_grid_state <= cheat;
				else
					-- Load the signal into the variable at the start of every clock
					v_grid_copy := real_grid_state;
				end if;

            -- Load states into local variables
            v_wheel_now := wheel_now;
            next_mode := mode; -- Default: stay in current mode
            
            -- Load initial display state on first clock edge
            if first_run = '1' then
                current_grid_state <= v_grid_copy;
                first_run <= '0';
                reward <= 0;
                
				-- Start with 0 rolls, need a coin insert
                rolls_remaining <= 0;
            end if;
            
            -- 1. Create the 10Hz roll clock (clkRoll)
            if counter_reg = COUNT_MAX then
                counter_reg <= 0;
                clkRoll <= not clkRoll;
            else
                counter_reg <= counter_reg + 1;
            end if;

            -- Register the current clkRoll state
            clkRoll_prev <= clkRoll;
				
            -- Handle Insert Coin
            -- Check for a rising edge on insert_coin
            if insert_coin = '1' and insert_coin_prev = '0' then
                if rolls_remaining = 0 and mode = 0 then
                    rolls_remaining <= NUM_COLS;
                    -- Give 5 rolls
                    v_wheel_now := 0;
                    -- Reset wheel
                    reward <= 0;
                    -- Clear old reward
                end if;
            end if;
            insert_coin_prev <= insert_coin; -- Save current coin state1

            -- 2. State machine logic
            if btn = '1' and btn_prev = '0' then

                -- A. If currently ROLLING (mode=1), this press STOPS the roll.
                if mode = 1 then
						kaching_timer <= KACHING_DURATION;
						for r_idx in 1 to REEL_SIZE loop
							 v_grid_copy(r_idx, v_wheel_now) := current_grid_state(r_idx, v_wheel_now);
						end loop;
						
						real_grid_state <= v_grid_copy;
						rolls_remaining <= rolls_remaining - 1;
						reward <= 0;

                        -- Check for win condition on final roll
                        if rolls_remaining = 1 then
                        
                            -- Use the (now-fixed) v_reward variable
                            v_reward := check_win(v_grid_copy);
                            reward <= v_reward;
                            rolls_remaining <= 0;
									 ready_timer_active <= '1'; 
									 ready_timer_count  <= 0;
                            
                            if v_reward > 0 then
                                -- A WIN OCCURRED
                                winning_cells <= find_winning_cells(v_grid_copy);
                                win_timer_count <= 0; -- Reset/start 5-sec timer
                                next_mode := 2; -- GO TO WIN_ANIMATION STATE
                                
                                -- Trigger the 0.25s 'ready' pulse for payout
                            else
                                -- NO WIN
                                next_mode := 0; -- Go to insert coin
                                winning_cells <= (others => (others => false));
				
                            end if;
                            
						else
                            -- Not the final roll, just go back to idle
                            next_mode := 0;
                        end if;
				  
				  
                elsif mode = 0 then -- B. If IDLE (mode=0), this press STARTS a roll.
                    if rolls_remaining > 0 then
								rolling_note_state <= 0;
                        reward <= 0;
                        if v_wheel_now = NUM_COLS then v_wheel_now := 1;
                        else v_wheel_now := v_wheel_now + 1; end if;
                        
                        for r_idx in 1 to REEL_SIZE loop
                            current_grid_state(r_idx, v_wheel_now) <= v_grid_copy(r_idx, v_wheel_now);
                        end loop;

                        next_mode := 1; -- Start rolling
								
                    end if;
                end if;
            
            -- C. If in WIN_ANIMATION (mode=2), run the 5-second timer
            -- This runs when NO button is pressed
            elsif mode = 2 then
                if win_timer_count < WIN_ANIMATION_CYCLES - 1 then
                    win_timer_count <= win_timer_count + 1;
                    next_mode := 2; -- Stay in win mode
                else
                    win_timer_count <= 0;
                    next_mode := 0;
                    reward <= 0;
                    winning_cells <= (others => (others => false)); -- Clear winning cells
                end if;

            end if;

               -- 0.25s Ready Timer (for payout signal)
               if ready_timer_active = '1' then
						if ready_timer_count < READY_PULSE_CYCLES - 1 then
							 ready_timer_count <= ready_timer_count + 1;
						else
							 ready_timer_active <= '0'; -- Stop the timer
							 ready_timer_count  <= 0;
						end if;
				  end if;

            btn_prev <= btn;
            mode <= next_mode;
            wheel_now <= v_wheel_now;
            
            -- 3. Continuous Rolling Logic (Unchanged)
            if mode = 1 then
                if (clkRoll = '1' and clkRoll_prev = '0') then
                    temp_symbol := current_grid_state(REEL_SIZE, v_wheel_now);
                    for i in REEL_SIZE downto 2 loop
                        current_grid_state(i, v_wheel_now) <= current_grid_state(i-1, v_wheel_now);
                    end loop;
                    current_grid_state(1, v_wheel_now) <= temp_symbol;
                end if;
            end if;

        end if;
    end process;
    ---

    -- Sync Signals (Combinational)
    h_sync <= '0' when (h_count >= 656 and h_count < 752) else '1';
    v_sync <= '0' when (v_count >= 490 and v_count < 492) else '1';
    hs_out <= h_sync;
    vs_out <= v_sync;
    ---

    -- Drive Final Output Pins (Uses registered signals)
    red <= r_reg;
    green <= g_reg;
    blue <= b_reg;

    ----------------------------------------------------------------
    -- Drawing Logic Process (OPTIMIZED)
    ----------------------------------------------------------------
    -- Sensitivity list is correct
    process(h_count, v_count, current_grid_state, blink_4hz, column_to_blink, rolls_remaining, mode, winning_cells)
        variable rr, gg, bb : std_logic;
        variable x_area, y_area : integer;
        variable cx, cy : integer;
        variable row, col : integer;
        variable is_grid_h, is_grid_v : boolean;
        variable is_symbol_pixel : boolean := false;
        constant CENTER_H : integer := CELL_WIDTH / 2;
        constant CENTER_V : integer := CELL_HEIGHT / 2;
        constant SPRITE_SCALE : integer := 4;
        constant SPRITE_SIZE : integer := 8;
        variable px, py : integer;
        variable pixel_on : boolean;
        constant FONT_SCALE : integer := 2;
        constant FONT_WIDTH : integer := 8;
        constant FONT_HEIGHT : integer := 8;
        constant H_START : integer := 192;
        constant V_START : integer := 32;
        constant CHAR_SPACING : integer := 20;
        constant PUNCT_SPACING : integer := 10;
        variable char_px, char_py : integer;
        variable char_on : boolean;

        variable logical_h_count : integer;
        
        constant POPUP_H_START : integer := 120;
        constant POPUP_H_END   : integer := 520; 
        constant POPUP_V_START : integer := 150;
        constant POPUP_V_END   : integer := 250;
        constant H_START_L1 : integer := 170;
        constant H_START_L2 : integer := 250;
        constant V_START_L1 : integer := 170; 
        constant V_START_L2 : integer := 200;

        variable is_winning_cell : boolean;
    begin
    
        logical_h_count := h_count + 2;
        rr := '0'; gg := '0'; bb := '0'; -- Default background (black)
        is_symbol_pixel := false;
        is_winning_cell := false; 
        
        -- Check if we are outside the visible display area
        if logical_h_count >= SCREEN_WIDTH or v_count >= SCREEN_HEIGHT then
             rr := '0';
             gg := '0'; bb := '0';
             r <= rr; g <= gg; b <= bb;
            
        elsif v_count < TITLE_HEIGHT then
            rr := '0';
            gg := '0'; bb := '1';
            if v_count > 20 and v_count < TITLE_HEIGHT - 20 then rr := '1';
            gg := '1'; bb := '0'; end if;
            if (logical_h_count < 100 or logical_h_count > 540) then rr := '0';
            gg := '0'; bb := '1'; end if;
            if (v_count >= V_START and v_count < V_START + (FONT_HEIGHT * FONT_SCALE)) then
                 char_py := (v_count - V_START) / FONT_SCALE;
                 char_on := false;
                 if (logical_h_count >= (H_START + 0) and logical_h_count < (H_START + 0 + 16)) then char_px := (logical_h_count - (H_START + 0)) / FONT_SCALE;
                 if ((char_py=1 or char_py=6) and (char_px=1 or char_px=6)) or (char_py=2 and char_px=1) or ((char_py=3 or char_py=7 or char_py=0) and (char_px>=2 and char_px<=5)) or ((char_py>=4 and char_py<=6) and char_px=6) then char_on := true;
                 end if;
                 elsif (logical_h_count >= (H_START + 20) and logical_h_count < (H_START + 20 + 16)) then char_px := (logical_h_count - (H_START + 20)) / FONT_SCALE;
                 if ((char_px=1) or (char_py=0 and (char_px>=2 and char_px<=5)) or ((char_py=2 or char_py=1) and char_px=6) or (char_py=3 and (char_px>=2 and char_px<=5)) or (char_py=4 and char_px=1) or (char_py=5 and char_px=1)) then char_on := true;
                 end if;
                 elsif (logical_h_count >= (H_START + 40) and logical_h_count < (H_START + 40 + 16)) then char_px := (logical_h_count - (H_START + 40)) / FONT_SCALE;
                 if ((char_py=0 or char_py=7) and (char_px>=2 and char_px<=4)) or char_px=3 then char_on := true; end if;
                 elsif (logical_h_count >= (H_START + 60) and logical_h_count < (H_START + 60 + 16)) then char_px := (logical_h_count - (H_START + 60)) / FONT_SCALE;
                 if ((char_px=1 or char_px=6) or (char_py=2 and char_px=2) or (char_py=3 and char_px=3) or (char_py=4 and char_px=4) or (char_py=5 and char_px=5)) then char_on := true;
                 end if;
                 elsif (logical_h_count >= (H_START + 100) and logical_h_count < (H_START + 100 + 16)) then char_px := (logical_h_count - (H_START + 100)) / FONT_SCALE;
                 if (char_py=0 and (char_px>=1 and char_px<=5)) or char_px=3 then char_on := true; end if;
                 elsif (logical_h_count >= (H_START + 120) and logical_h_count < (H_START + 120 + 16)) then char_px := (logical_h_count - (H_START + 120)) / FONT_SCALE;
                 if (((char_py=0 or char_py=7) and (char_px>=2 and char_px<=5)) or ((char_px=1 or char_px=6) and (char_py>=1 and char_py<=6))) then char_on := true;
                 end if;
                 elsif (logical_h_count >= (H_START + 160) and logical_h_count < (H_START + 160 + 16)) then char_px := (logical_h_count - (H_START + 160)) / FONT_SCALE;
                 if ((char_px=0 or char_px=6) or (char_py=5 and (char_px=1 or char_px=5)) or (char_py=4 and (char_px=2 or char_px=4)) or (char_py=3 and char_px=3)) then char_on := true;
                 end if;
                 elsif (logical_h_count >= (H_START + 180) and logical_h_count < (H_START + 180 + 16)) then char_px := (logical_h_count - (H_START + 180)) / FONT_SCALE;
                 if ((char_py=0 or char_py=7) and (char_px>=2 and char_px<=4)) or char_px=3 then char_on := true; end if;
                 elsif (logical_h_count >= (H_START + 200) and logical_h_count < (H_START + 200 + 16)) then char_px := (logical_h_count - (H_START + 200)) / FONT_SCALE;
                 if ((char_px=1 or char_px=6) or (char_py=2 and char_px=2) or (char_py=3 and char_px=3) or (char_py=4 and char_px=4) or (char_py=5 and char_px=5)) then char_on := true;
                 end if;
                 elsif (logical_h_count >= (H_START + 220) and logical_h_count < (H_START + 220 + 16)) then char_px := (logical_h_count - (H_START + 220)) / FONT_SCALE;
                 if ((char_px=3 or char_px=4) and (char_py>=0 and char_py<=5)) or ((char_px=3 or char_px=4) and char_py=7) then char_on := true; end if;
                 elsif (logical_h_count >= (H_START + 220 + PUNCT_SPACING) and logical_h_count < (H_START + 220 + PUNCT_SPACING + 16)) then char_px := (logical_h_count - (H_START + 220 + PUNCT_SPACING)) / FONT_SCALE;
                 if ((char_px=3 or char_px=4) and (char_py>=0 and char_py<=5)) or ((char_px=3 or char_px=4) and char_py=7) then char_on := true; end if;
                 elsif (logical_h_count >= (H_START + 220 + 2*PUNCT_SPACING) and logical_h_count < (H_START + 220 + 2*PUNCT_SPACING + 16)) then char_px := (logical_h_count - (H_START + 220 + 2*PUNCT_SPACING)) / FONT_SCALE;
                 if ((char_px=3 or char_px=4) and (char_py>=0 and char_py<=5)) or ((char_px=3 or char_px=4) and char_py=7) then char_on := true; end if;
                 end if;
                 if char_on then rr := '1'; gg := '0'; bb := '0'; end if;
            end if;
            
        -- ==========================================================
        -- GRID AREA DRAWING...
        -- ==========================================================
        elsif v_count < SCREEN_HEIGHT then

            x_area := logical_h_count;
            y_area := v_count - TITLE_HEIGHT;

            -- Set default background color for the grid area (Blue)
            rr := '0'; gg := '0'; bb := '1';

            -- Calculate the current cell's row and col
            -- We add a check later to ensure they are in the 1-5 range
            row := (y_area / CELL_HEIGHT) + 1;
            col := (x_area / CELL_WIDTH) + 1;
            
            -- "Next column" blinking
            if col = column_to_blink and column_to_blink /= 0 and blink_4hz = '1' then
                rr := '1'; gg := '1'; bb := '0'; -- Yellow highlight
            end if;
            
            -- "Winning cell" blinking
            if mode = 2 and (row >= 1 and row <= NUM_ROWS) and (col >= 1 and col <= NUM_COLS) then
                if winning_cells(row, col) = true and blink_4hz = '1' then
                    rr := '1'; gg := '1'; bb := '0';
                end if;
            end if;
            
            -- ==========================================================
            -- SYMBOL DRAWING
            -- ==========================================================
            -- Only draw symbols if we are in a valid cell
            if (row >= 1 and row <= NUM_ROWS) and (col >= 1 and col <= NUM_COLS) then
                -- Calculate the pixel's coordinates *within* its cell
                cx := x_area - (col-1)*CELL_WIDTH; 
                cy := y_area - (row-1)*CELL_HEIGHT;
                -- Convert to sprite pixel coordinates
                px := (cx - CENTER_H + (SPRITE_SIZE*SPRITE_SCALE)/2) / SPRITE_SCALE;
                py := (cy - CENTER_V + (SPRITE_SIZE*SPRITE_SCALE)/2) / SPRITE_SCALE;
                pixel_on := false;
                
                -- Draw the one symbol for the current (row, col)
                case current_grid_state(row, col) is
                    when SYMBOL_APPLE =>
                        if (px >= 0 and px < SPRITE_SIZE and py >= 0 and py < SPRITE_SIZE) then
                            is_symbol_pixel := true;
                            if ((py = 0 and (px = 3 or px = 4))) then rr := '1'; gg := '1';
                            bb := '0';
                            elsif (py = 0 and (px = 2 or px = 1)) then rr := '0';
                            gg := '1'; bb := '0';
                            elsif (py = 1 and (px = 3 or px = 4)) then rr := '1';
                            gg := '1'; bb := '0';
                            elsif (py = 2 and px = 2) then rr := '1';
                            gg := '1'; bb := '1';
                            elsif ((py = 2 and (px >= 3 and px <= 5)) or ((py = 3 or py = 4) and (px >= 1 and px <= 6)) or (py = 5 and (px >= 1 and px <= 6)) or (py = 6 and (px >= 2 and px <= 5))) then rr := '1';
                            gg := '0'; bb := '0';
                            else is_symbol_pixel := false; end if;
                        end if;
                    
                    when SYMBOL_CHERRY =>
                        if (px >= 0 and px <= SPRITE_SIZE and py >= 0 and py < SPRITE_SIZE) then
                                        is_symbol_pixel := true; py := py - 1;
                                        if ((py = 0 and (px >= 1 and px < 7)) or (py = 1 and (px = 1 or px = 6))) then rr := '0'; gg := '1'; bb := '0';
                                        elsif (py = 2 and (px = 0 or px = 6)) then rr := '1'; gg := '1'; bb := '1';
                                        elsif ((py = 2 and (px = 0 or px = 1)) or ((py >= 3 and py <= 4) and (px = 0 or px = 1 or px = 2)) or (py = 5 and (px >= 0 and px <= 2))) then rr := '1'; gg := '0'; bb := '1';
                                        elsif (((py = 2 or py = 5) and (px = 6 or px = 7)) or ((py >= 3 and py <= 4) and (px >= 5 and px <= 8))) then rr := '1'; gg := '0'; bb := '1';
                                        else is_symbol_pixel := false; end if;
                                    end if;

                    when SYMBOL_DIAMOND =>
                        if (px >= 0 and px < SPRITE_SIZE and py >= 0 and py < SPRITE_SIZE) then
                            is_symbol_pixel := true;
                            if (((py = 0) and (px = 3 or px = 4)) or ((py = 1) and (px >= 2 and px <= 5)) or ((py = 2) and (px >= 1 and px <= 6)) or ((py = 3 or py = 4) and (px >= 0 and px <= 7)) or ((py = 5) and (px >= 1 and px <= 6)) or ((py = 6) and (px >= 2 and px <= 5)) or ((py = 7) and (px = 3 or px = 4))) then rr := '0';
                            gg := '1'; bb := '1';
                            else is_symbol_pixel := false; end if;
                        end if;

                    when SYMBOL_SEVEN =>
                        if (px >= 0 and px < SPRITE_SIZE and py >= 0 and py < SPRITE_SIZE) then
                            is_symbol_pixel := true;
                            if (((py = 0) and (px >= 1 and px <= 6)) or ((py = 1) and (px = 6)) or ((py = 2) and (px = 5)) or ((py = 3) and (px = 4)) or ((py >= 4 and py <= 7) and (px = 3))) then rr := '1';
                            gg := '1'; bb := '0';
                            else is_symbol_pixel := false; end if;
                        end if;
                        
                    when SYMBOL_EIGHT =>
                        if (px >= 0 and px < SPRITE_SIZE and py >= 0 and py < SPRITE_SIZE) then
                            is_symbol_pixel := true;
                            if ((((px >= 2 and px <= 5) and (py = 0 or py = 3 or py = 4 or py = 7))) or (((px = 1 or px = 6) and (py = 1 or py = 2 or py = 5 or py = 6)))) then rr := '1';
                            gg := '0'; bb := '0';
                            else is_symbol_pixel := false; end if;
                        end if;
                    when SYMBOL_CREEPER =>
                        if (px >= 0 and px < SPRITE_SIZE and py >= 0 and py < SPRITE_SIZE) then
                            is_symbol_pixel := true;
                            rr := '0'; gg := '1'; bb := '0';
                            if ((px = 1 or px = 2) and (py = 1 or py = 2)) or ((px = 5 or px = 6) and (py = 1 or py = 2)) then pixel_on := true;
                            end if;
                            if (px = 3 or px = 4) and (py = 3) then pixel_on := true; end if;
                            if (px >= 2 and px <= 5) and (py = 4) then pixel_on := true; end if;
                            if (px = 2 or px = 5) and (py = 5 or py = 6) then pixel_on := true;
                            end if;
                            if pixel_on then rr := '0'; gg := '0'; bb := '0'; end if;
                        end if;
                    when SYMBOL_TOP_HAT =>
                        if (px >= 0 and px < SPRITE_SIZE and py >= 0 and py < SPRITE_SIZE) then
                            is_symbol_pixel := true;
                            if ((py = 3) and (px >= 1 and px <= 5)) then rr := '1'; gg := '0';
                            bb := '0';
                            elsif (((py = 0 or py = 1 or py = 2) and (px >= 1 and px <= 5)) or ((py = 4 or py = 5) and (px >= 0 and px <= 6))) then rr := '0';
                            gg := '0'; bb := '0';
                            else is_symbol_pixel := false; end if;
                        end if;
                    when others => null;
                end case;
            end if;
            
            -- Grid lines calculation (Unchanged)
            is_grid_v := false;
            for i in 0 to NUM_COLS loop
                if x_area >= (i * CELL_WIDTH) and x_area < (i * CELL_WIDTH + LINE_WIDTH) then
                    is_grid_v := true;
                    exit;
                end if;
            end loop;
            is_grid_h := false;
            for i in 0 to NUM_ROWS loop
                if y_area >= (i * CELL_HEIGHT) and y_area < (i * CELL_HEIGHT + LINE_WIDTH) then
                    is_grid_h := true;
                    exit;
                end if;
            end loop;

            -- Grid lines drawing (Unchanged)
            if (is_grid_h or is_grid_v) then
                if col = column_to_blink and column_to_blink /= 0 then
                    if blink_4hz = '1' then rr := '1'; gg := '1'; bb := '0'; -- Yellow Grid
                    else rr := '1'; gg := '1'; bb := '1'; -- White Grid flash
                    end if;
                else
                    rr := '1'; gg := '1'; bb := '1'; -- White Grid
                end if;
            end if;
        end if;
        
        -- ==========================================================
        -- === "INSERT COIN" POP-UP OVERRIDE ===
        -- ==========================================================
        if rolls_remaining = 0 and mode = 0 then
            -- Check if the current pixel is inside the pop-up box
            if (x_area >= POPUP_H_START and x_area < POPUP_H_END) and
               (y_area >= POPUP_V_START and y_area < POPUP_V_END) then
               
                -- 1. Set the background to White
                
					 if blink_4hz = '1' then
                rr := '1'; gg := '1'; bb := '1';
					else
					 rr := '1'; gg := '0'; bb := '0';
					end if;
                char_on := false;
                
                -- 2. --- RENDER LINE 1: "INSERT COIN(S)" ---
                if (y_area >= V_START_L1 and y_area < V_START_L1 + (FONT_HEIGHT * FONT_SCALE)) then
                    char_py := (y_area - V_START_L1) / FONT_SCALE;
                    -- I
                    if (logical_h_count >= (H_START_L1 + 0) and logical_h_count < (H_START_L1 + 0 + 16)) then
                        char_px := (logical_h_count - (H_START_L1 + 0)) / FONT_SCALE;
                        if ((char_py=0 or char_py=7) and (char_px>=2 and char_px<=4)) or char_px=3 then char_on := true; end if;
                    -- N
                    elsif (logical_h_count >= (H_START_L1 + 20) and logical_h_count < (H_START_L1 + 20 + 16)) then
                        char_px := (logical_h_count - (H_START_L1 + 20)) / FONT_SCALE;
                        if (char_px=1 or char_px=6) or (char_py=2 and char_px=2) or (char_py=3 and char_px=3) or (char_py=4 and char_px=4) or (char_py=5 and char_px=5) then char_on := true;
                        end if;
                    -- S
                    elsif (logical_h_count >= (H_START_L1 + 40) and logical_h_count < (H_START_L1 + 40 + 16)) then
                        char_px := (logical_h_count - (H_START_L1 + 40)) / FONT_SCALE;
                        if ((char_py=0 or char_py=3 or char_py=7) and (char_px>=2 and char_px<=5)) or (char_py=1 and (char_px=1 or char_px=6)) or (char_py=2 and char_px=1) or (char_py=4 and char_px=6) or (char_py=5 and char_px=6) or (char_py=6 and (char_px=1 or char_px=6)) then char_on := true;
                        end if;
                    -- E
                    elsif (logical_h_count >= (H_START_L1 + 60) and logical_h_count < (H_START_L1 + 60 + 16)) then
                        char_px := (logical_h_count - (H_START_L1 + 60)) / FONT_SCALE;
                        if ((char_py=0 or char_py=3 or char_py=7) and (char_px>=1 and char_px<=5)) or char_px=1 then char_on := true; end if;
                    -- R
                    elsif (logical_h_count >= (H_START_L1 + 80) and logical_h_count < (H_START_L1 + 80 + 16)) then
                        char_px := (logical_h_count - (H_START_L1 + 80)) / FONT_SCALE;
                        if (char_px=1) or ((char_py=0 or char_py=3) and (char_px>=2 and char_px<=5)) or ((char_py=1 or char_py=2) and char_px=6) or (char_py=4 and char_px=4) or (char_py=5 and char_px=5) or (char_py=6 and char_px=6) or (char_py=7 and char_px=6) then char_on := true;
                        end if;
                    -- T
                    elsif (logical_h_count >= (H_START_L1 + 100) and logical_h_count < (H_START_L1 + 100 + 16)) then
                        char_px := (logical_h_count - (H_START_L1 + 100)) / FONT_SCALE;
                        if (char_py=0 and (char_px>=1 and char_px<=5)) or char_px=3 then char_on := true; end if;
                    -- (space)
                    -- C
                    elsif (logical_h_count >= (H_START_L1 + 140) and logical_h_count < (H_START_L1 + 140 + 16)) then
                        char_px := (logical_h_count - (H_START_L1 + 140)) / FONT_SCALE;
                        if ((char_py=0 or char_py=7) and (char_px>=2 and char_px<=5)) or ((char_py=1 or char_py=6) and char_px=1) or ((char_py>=2 and char_py<=5) and char_px=1) then char_on := true;
                        end if;
                    -- O
                    elsif (logical_h_count >= (H_START_L1 + 160) and logical_h_count < (H_START_L1 + 160 + 16)) then
                        char_px := (logical_h_count - (H_START_L1 + 160)) / FONT_SCALE;
                        if ((char_py=0 or char_py=7) and (char_px>=2 and char_px<=5)) or ((char_py=1 or char_py=6) and (char_px=1 or char_px=6)) or ((char_py>=2 and char_py<=5) and (char_px=1 or char_px=6)) then char_on := true;
                        end if;
                    -- I
                    elsif (logical_h_count >= (H_START_L1 + 180) and logical_h_count < (H_START_L1 + 180 + 16)) then
                        char_px := (logical_h_count - (H_START_L1 + 180)) / FONT_SCALE;
                        if ((char_py=0 or char_py=7) and (char_px>=2 and char_px<=4)) or char_px=3 then char_on := true; end if;
                    -- N
                    elsif (logical_h_count >= (H_START_L1 + 200) and logical_h_count < (H_START_L1 + 200 + 16)) then
                        char_px := (logical_h_count - (H_START_L1 + 200)) / FONT_SCALE;
                        if (char_px=1 or char_px=6) or (char_py=2 and char_px=2) or (char_py=3 and char_px=3) or (char_py=4 and char_px=4) or (char_py=5 and char_px=5) then char_on := true;
                        end if;
                    -- (
                    elsif (logical_h_count >= (H_START_L1 + 220) and logical_h_count < (H_START_L1 + 220 + 16)) then
                        char_px := (logical_h_count - (H_START_L1 + 220)) / FONT_SCALE;
                        if ((char_py=1 or char_py=6) and char_px=5) or ((char_py=2 or char_py=5) and char_px=4) or ((char_py=3 or char_py=4) and char_px=3) then char_on := true;
                        end if;
                    -- S
                    elsif (logical_h_count >= (H_START_L1 + 240) and logical_h_count < (H_START_L1 + 240 + 16)) then
                        char_px := (logical_h_count - (H_START_L1 + 240)) / FONT_SCALE;
                        if ((char_py=0 or char_py=3 or char_py=7) and (char_px>=2 and char_px<=5)) or (char_py=1 and (char_px=1 or char_px=6)) or (char_py=2 and char_px=1) or (char_py=4 and char_px=6) or (char_py=5 and char_px=6) or (char_py=6 and (char_px=1 or char_px=6)) then char_on := true;
                        end if;
                    -- )
                    elsif (logical_h_count >= (H_START_L1 + 260) and logical_h_count < (H_START_L1 + 260 + 16)) then
                        char_px := (logical_h_count - (H_START_L1 + 260)) / FONT_SCALE;
                        if ((char_py=1 or char_py=6) and char_px=3) or ((char_py=2 or char_py=5) and char_px=4) or ((char_py=3 or char_py=4) and char_px=5) then char_on := true;
                        end if;
                end if;
                
                -- 3. --- RENDER LINE 2: "TO PLAY" ---
                elsif (y_area >= V_START_L2 and y_area < V_START_L2 + (FONT_HEIGHT * FONT_SCALE)) then
                    char_py := (y_area - V_START_L2) / FONT_SCALE;
                    -- T
                    if (logical_h_count >= (H_START_L2 + 0) and logical_h_count < (H_START_L2 + 0 + 16)) then
                        char_px := (logical_h_count - (H_START_L2 + 0)) / FONT_SCALE;
                        if (char_py=0 and (char_px>=1 and char_px<=5)) or char_px=3 then char_on := true; end if;
                    -- O
                    elsif (logical_h_count >= (H_START_L2 + 20) and logical_h_count < (H_START_L2 + 20 + 16)) then
                        char_px := (logical_h_count - (H_START_L2 + 20)) / FONT_SCALE;
                        if ((char_py=0 or char_py=7) and (char_px>=2 and char_px<=5)) or ((char_py=1 or char_py=6) and (char_px=1 or char_px=6)) or ((char_py>=2 and char_py<=5) and (char_px=1 or char_px=6)) then char_on := true;
                        end if;
                    -- (space)
                    -- P
                    elsif (logical_h_count >= (H_START_L2 + 60) and logical_h_count < (H_START_L2 + 60 + 16)) then
                        char_px := (logical_h_count - (H_START_L2 + 60)) / FONT_SCALE;
                        if (char_px=1) or ((char_py=0 or char_py=3) and (char_px>=2 and char_px<=5)) or ((char_py=1 or char_py=2) and char_px=6) or (char_py>=4 and char_px=1) then char_on := true;
                        end if;
                    -- L
                    elsif (logical_h_count >= (H_START_L2 + 80) and logical_h_count < (H_START_L2 + 80 + 16)) then
                        char_px := (logical_h_count - (H_START_L2 + 80)) / FONT_SCALE;
                        if char_px=1 or (char_py=7 and (char_px>=1 and char_px<=5)) then char_on := true; end if;
                    -- A
                    elsif (logical_h_count >= (H_START_L2 + 100) and logical_h_count < (H_START_L2 + 100 + 16)) then
                        char_px := (logical_h_count - (H_START_L2 + 100)) / FONT_SCALE;
                        if ((char_py=0) and (char_px>=2 and char_px<=5)) or ((char_py=1 or char_py=2 or char_py=4 or char_py=5 or char_py=6 or char_py=7) and (char_px=1 or char_px=6)) or (char_py=3 and (char_px>=1 and char_px<=6)) then char_on := true; end if;
                    -- Y
                    elsif (logical_h_count >= (H_START_L2 + 120) and logical_h_count < (H_START_L2 + 120 + 16)) then
                        char_px := (logical_h_count - (H_START_L2 + 120)) / FONT_SCALE;
                        if ((char_py=0 or char_py=1) and (char_px=1 or char_px=5)) or (((char_py=2) and (char_px=2 or char_px=4)) or ((char_py>=3) and (char_px=3))) then char_on := true; end if;
                end if;
            end if;
            
            -- 4. --- SET COLOR ---
            if char_on then
					if blink_4hz = '1' then
                rr := '1'; gg := '0'; bb := '0';
					else
					 rr := '1'; gg := '1'; bb := '1';
					end if;
            end if;

        end if;
    end if;

    r <= rr;
    g <= gg;
    b <= bb;

end process;

end Behavioral;