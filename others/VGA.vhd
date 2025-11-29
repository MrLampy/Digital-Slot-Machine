library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_ARITH.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;

entity VGA is
    port (
        clk20_in : in std_logic;
        red      : out std_logic;
        green    : out std_logic;
        blue     : out std_logic;
        hs_out   : out std_logic;
        vs_out   : out std_logic
    );
end VGA;

architecture Behavioral of VGA is

    -- Timing Signals
    signal h_count : integer range 0 to 799 := 0;
    signal v_count : integer range 0 to 524 := 0;
    signal h_sync, v_sync : std_logic;
    signal r, g, b : std_logic;

    -- Grid Constants (640x480 screen)
    constant LINE_WIDTH       : integer := 2;
    constant SCREEN_WIDTH     : integer := 640;
    constant SCREEN_HEIGHT    : integer := 480;
    constant NUM_ROWS         : integer := 5;
    constant NUM_COLS         : integer := 5;
    constant TITLE_HEIGHT     : integer := 80;
    constant GRID_AREA_HEIGHT : integer := SCREEN_HEIGHT - TITLE_HEIGHT;
    constant CELL_WIDTH       : integer := SCREEN_WIDTH / NUM_COLS;
    constant CELL_HEIGHT      : integer := GRID_AREA_HEIGHT / NUM_ROWS;

    -- Symbol Types and Grid State
    -- CLEANED: Removed Heart, Spades, and Banana
    type T_SYMBOL is (
        SYMBOL_NONE,
        SYMBOL_APPLE,
        SYMBOL_CHERRY,
        SYMBOL_CREEPER,
        SYMBOL_DIAMOND,
        SYMBOL_SEVEN,
        SYMBOL_EIGHT
    );
    type T_GRID_ARRAY is array (1 to NUM_ROWS, 1 to NUM_COLS) of T_SYMBOL;

    signal current_grid_state : T_GRID_ARRAY := (
        others => (others => SYMBOL_NONE)
    );

begin

    -- Grid layout
    current_grid_state(2, 5) <= SYMBOL_CHERRY;
    current_grid_state(1, 5) <= SYMBOL_APPLE;
    current_grid_state(2, 1) <= SYMBOL_APPLE;
    current_grid_state(3, 5) <= SYMBOL_APPLE;
    current_grid_state(3, 3) <= SYMBOL_CREEPER;
    
    -- CLEANED: Removed old symbols
    current_grid_state(1, 1) <= SYMBOL_SEVEN;  -- Was Spades
    current_grid_state(1, 2) <= SYMBOL_EIGHT;  -- Was Heart
    current_grid_state(1, 3) <= SYMBOL_DIAMOND;
    current_grid_state(2, 2) <= SYMBOL_SEVEN;
    current_grid_state(2, 3) <= SYMBOL_EIGHT;
    current_grid_state(3, 1) <= SYMBOL_APPLE;  -- Was Banana


    ----------------------------------------------------------------
    -- VGA timing
    ----------------------------------------------------------------
    process(clk20_in)
    begin
        if rising_edge(clk20_in) then
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

    -- Sync pulse generation
    h_sync <= '0' when (h_count >= 656 and h_count < 752) else '1';
    v_sync <= '0' when (v_count >= 490 and v_count < 492) else '1';
    hs_out <= h_sync;
    vs_out <= v_sync;

    ----------------------------------------------------------------
    -- DRAWING LOGIC
    ----------------------------------------------------------------
    process(h_count, v_count)
        variable rr, gg, bb : std_logic;
        variable x_area, y_area : integer;
        variable cx, cy : integer;
        variable row, col : integer;
        variable is_grid_h, is_grid_v : boolean;
        
        variable is_symbol_pixel : boolean := false; 

        constant CENTER_H : integer := CELL_WIDTH / 2;
        constant CENTER_V : integer := CELL_HEIGHT / 2;

        -- OPTIMIZED: Standardized 8x8 sprite engine
        -- Scale *MUST* be a power of 2 (like 2, 4, 8)
        -- to avoid hardware dividers. Division by 8 is a "free" bit-shift.
        constant SPRITE_SCALE : integer := 8;
        constant SPRITE_SIZE  : integer := 8;

        variable px, py : integer;
        variable pixel_on : boolean;

        -- NEW: Constants and variables for Title Font
        constant FONT_SCALE   : integer := 2; -- Power of 2, division is a bit-shift
        constant FONT_WIDTH   : integer := 8;
        constant FONT_HEIGHT  : integer := 8;
        constant H_START      : integer := 162; -- Centered H pos
        constant V_START      : integer := 32;  -- Centered V pos
        constant CHAR_SPACING : integer := 20; -- 8*2 (char) + 2*2 (space)
        variable char_px, char_py : integer;
        variable char_on : boolean;
        
    begin
        rr := '0'; gg := '0'; bb := '0'; -- Default background (black)
        is_symbol_pixel := false; -- Reset flag for every new pixel

        ----------------------------------------------------------------
        -- TITLE AREA
        ----------------------------------------------------------------
        if v_count < TITLE_HEIGHT then
            rr := '0'; gg := '0'; bb := '1'; -- Default title background (Blue)
            
            -- Inner Yellow Area
            if v_count > 20 and v_count < TITLE_HEIGHT - 20 then
                rr := '1'; gg := '1'; bb := '0'; 
            end if;
            
            -- Blue borders
            if (h_count < 100 or h_count > 540) then
                rr := '0'; gg := '0'; bb := '1';
            end if;

            -- NEW: Draw "SPIN TO WIN!!!" text
            -- Check if we are in the vertical bounds of the text
            if (v_count >= V_START and v_count < V_START + (FONT_HEIGHT * FONT_SCALE)) then
                char_py := (v_count - V_START) / FONT_SCALE;
                char_on := false;
                
                -- S
                if (h_count >= (H_START + 0*CHAR_SPACING) and h_count < (H_START + 0*CHAR_SPACING + 16)) then
                    char_px := (h_count - (H_START + 0*CHAR_SPACING)) / FONT_SCALE;
                    if ((char_py=0 and char_px>0 and char_px<7) or (char_py=1 and char_px=0) or (char_py=2 and char_px=0) or (char_py=3 and char_px>0 and char_px<7) or (char_py=4 and char_px=7) or (char_py=5 and char_px=7) or (char_py=6 and char_px>0 and char_px<7)) then char_on := true; end if;
                -- P
                elsif (h_count >= (H_START + 1*CHAR_SPACING) and h_count < (H_START + 1*CHAR_SPACING + 16)) then
                    char_px := (h_count - (H_START + 1*CHAR_SPACING)) / FONT_SCALE;
                    if ((char_px=0) or (char_py=0 and char_px>0 and char_px<7) or (char_py=1 and char_px=7) or (char_py=2 and char_px=7) or (char_py=3 and char_px>0 and char_px<7)) then char_on := true; end if;
                -- I
                elsif (h_count >= (H_START + 2*CHAR_SPACING) and h_count < (H_START + 2*CHAR_SPACING + 16)) then
                    char_px := (h_count - (H_START + 2*CHAR_SPACING)) / FONT_SCALE;
                    if ((char_py=0 or char_py=6) or (char_px=3 or char_px=4)) then char_on := true; end if;
                -- N
                elsif (h_count >= (H_START + 3*CHAR_SPACING) and h_count < (H_START + 3*CHAR_SPACING + 16)) then
                    char_px := (h_count - (H_START + 3*CHAR_SPACING)) / FONT_SCALE;
                    if ((char_px=0 or char_px=7) or (char_py=1 and char_px=1) or (char_py=2 and char_px=2) or (char_py=3 and char_px=3) or (char_py=4 and char_px=4) or (char_py=5 and char_px=5) or (char_py=6 and char_px=6)) then char_on := true; end if;
                -- T
                elsif (h_count >= (H_START + 5*CHAR_SPACING) and h_count < (H_START + 5*CHAR_SPACING + 16)) then
                    char_px := (h_count - (H_START + 5*CHAR_SPACING)) / FONT_SCALE;
                    if (char_py=0 or (char_px=3 or char_px=4)) then char_on := true; end if;
                -- O
                elsif (h_count >= (H_START + 6*CHAR_SPACING) and h_count < (H_START + 6*CHAR_SPACING + 16)) then
                    char_px := (h_count - (H_START + 6*CHAR_SPACING)) / FONT_SCALE;
                    if ((char_py=0 and char_px>0 and char_px<7) or (char_py=6 and char_px>0 and char_px<7) or (char_px=0 and char_py>0 and char_py<6) or (char_px=7 and char_py>0 and char_py<6)) then char_on := true; end if;
                -- W
                elsif (h_count >= (H_START + 8*CHAR_SPACING) and h_count < (H_START + 8*CHAR_SPACING + 16)) then
                    char_px := (h_count - (H_START + 8*CHAR_SPACING)) / FONT_SCALE;
                    if ((char_px=0 or char_px=7) or (char_py=4 and (char_px=1 or char_px=6)) or (char_py=5 and (char_px=2 or char_px=5)) or (char_py=6 and (char_px=3 or char_px=4))) then char_on := true; end if;
                -- I
                elsif (h_count >= (H_START + 9*CHAR_SPACING) and h_count < (H_START + 9*CHAR_SPACING + 16)) then
                    char_px := (h_count - (H_START + 9*CHAR_SPACING)) / FONT_SCALE;
                    if ((char_py=0 or char_py=6) or (char_px=3 or char_px=4)) then char_on := true; end if;
                -- N
                elsif (h_count >= (H_START + 10*CHAR_SPACING) and h_count < (H_START + 10*CHAR_SPACING + 16)) then
                    char_px := (h_count - (H_START + 10*CHAR_SPACING)) / FONT_SCALE;
                    if ((char_px=0 or char_px=7) or (char_py=1 and char_px=1) or (char_py=2 and char_px=2) or (char_py=3 and char_px=3) or (char_py=4 and char_px=4) or (char_py=5 and char_px=5) or (char_py=6 and char_px=6)) then char_on := true; end if;
                -- !
                elsif (h_count >= (H_START + 11*CHAR_SPACING) and h_count < (H_START + 11*CHAR_SPACING + 16)) then
                    char_px := (h_count - (H_START + 11*CHAR_SPACING)) / FONT_SCALE;
                    if ((char_px=3 or char_px=4) and (char_py=0 or char_py=1 or char_py=2 or char_py=3)) or ((char_px=3 or char_px=4) and (char_py=5 or char_py=6)) then char_on := true; end if;
                -- !
                elsif (h_count >= (H_START + 12*CHAR_SPACING) and h_count < (H_START + 12*CHAR_SPACING + 16)) then
                    char_px := (h_count - (H_START + 12*CHAR_SPACING)) / FONT_SCALE;
                    if ((char_px=3 or char_px=4) and (char_py=0 or char_py=1 or char_py=2 or char_py=3)) or ((char_px=3 or char_px=4) and (char_py=5 or char_py=6)) then char_on := true; end if;
                -- !
                elsif (h_count >= (H_START + 13*CHAR_SPACING) and h_count < (H_START + 13*CHAR_SPACING + 16)) then
                    char_px := (h_count - (H_START + 13*CHAR_SPACING)) / FONT_SCALE;
                    if ((char_px=3 or char_px=4) and (char_py=0 or char_py=1 or char_py=2 or char_py=3)) or ((char_px=3 or char_px=4) and (char_py=5 or char_py=6)) then char_on := true; end if;
                end if;
                
                -- Draw pixel if it's on
                if char_on then
                    rr := '1'; gg := '0'; bb := '0'; -- MODIFIED: RED text
                end if;

            end if;

        ----------------------------------------------------------------
        -- GRID AREA (where symbols and grid lines are drawn)
        ----------------------------------------------------------------
        elsif v_count < SCREEN_HEIGHT then
            x_area := h_count;
            y_area := v_count - TITLE_HEIGHT;
            rr := '0'; gg := '0'; bb := '1'; -- Default grid background (Blue)

            -- Cell determination loops
            for r_idx in 1 to NUM_ROWS loop
                if y_area >= (r_idx-1)*CELL_HEIGHT and y_area < r_idx*CELL_HEIGHT then
                    row := r_idx;
                    for c_idx in 1 to NUM_COLS loop
                        if x_area >= (c_idx-1)*CELL_WIDTH and x_area < c_idx*CELL_WIDTH then
                            col := c_idx;
                            cx := x_area - (c_idx-1)*CELL_WIDTH;
                            cy := y_area - (r_idx-1)*CELL_HEIGHT;

                            -- OPTIMIZED: Unified sprite coordinate calculation
                            -- Division by SPRITE_SCALE (8) is a simple bit-shift ("free" in hardware)
                            px := (cx - CENTER_H + (SPRITE_SIZE*SPRITE_SCALE)/2) / SPRITE_SCALE;
                            py := (cy - CENTER_V + (SPRITE_SIZE*SPRITE_SCALE)/2) / SPRITE_SCALE;
                            pixel_on := false; -- Flag for black pixels in a sprite

                            -- Check if pixel is in sprite area *before* case
                            if (px >= 0 and px < SPRITE_SIZE and py >= 0 and py < SPRITE_SIZE) then
                                -- We are inside a sprite's 8x8 grid
                                is_symbol_pixel := true; -- Mark pixel as drawn
                                
                                case current_grid_state(row, col) is

                                ----------------------------------------------------------------
                                -- OPTIMIZED: APPLE SYMBOL (8x8 Pixel Art)
                                ----------------------------------------------------------------
                                when SYMBOL_APPLE =>
                                    if ((py = 0 and (px = 3 or px = 4))) then -- Stem (Brown)
                                         rr := '1'; gg := '1'; bb := '0';
                                    elsif (py = 1 and (px = 2)) then -- Leaf (Green)
                                         rr := '0'; gg := '1'; bb := '0';
                                    elsif (py = 1 and (px = 3 or px = 4)) then -- Stem base (Brown)
                                         rr := '1'; gg := '1'; bb := '0';
                                    elsif (py = 2 and px = 2) then -- Gloss (White)
                                         rr := '1'; gg := '1'; bb := '1';
                                    elsif ((py = 2 and (px >= 3 and px <= 5)) or -- Body (Red)
                                           ((py = 3 or py = 4) and (px >= 1 and px <= 6)) or
                                           (py = 5 and (px >= 1 and px <= 6)) or
                                           (py = 6 and (px >= 2 and px <= 5))) then 
                                         rr := '1'; gg := '0'; bb := '0';
                                    else
                                        is_symbol_pixel := false; -- Not part of the apple sprite
                                    end if;

                                ----------------------------------------------------------------
                                -- OPTIMIZED: CHERRY SYMBOL (8x8 Pixel Art)
                                ----------------------------------------------------------------
                                when SYMBOL_CHERRY =>
                                    if (py = 0 and (px = 2 or px = 5)) or (py = 1 and (px = 1 or px = 6)) then -- Stems (Green)
                                        rr := '0'; gg := '1'; bb := '0';
                                    elsif ((py = 2 and (px = 0 or px = 1)) or ((py >= 3 and py <= 4) and (px = 0 or px = 1 or px = 2))) then -- Left Cherry (Magenta)
                                        rr := '1'; gg := '0'; bb := '1';
                                    elsif ((py = 2 and (px = 6 or px = 7)) or ((py >= 3 and py <= 4) and (px = 5 or px = 6 or px = 7))) then -- Right Cherry (Magenta)
                                        rr := '1'; gg := '0'; bb := '1';
                                    elsif (py = 2 and (px = 0 or px = 7)) then -- Gloss (White)
                                        rr := '1'; gg := '1'; bb := '1';
                                    else
                                        is_symbol_pixel := false;
                                    end if;

                                ----------------------------------------------------------------
                                -- OPTIMIZED: CREEPER SYMBOL (Using 8x8 engine)
                                ----------------------------------------------------------------
                                when SYMBOL_CREEPER =>
                                    rr := '0'; gg := '1'; bb := '0'; -- Green background
                                    -- Eyes
                                    if ((px = 1 or px = 2) and (py = 1 or py = 2)) or -- Left eye
                                       ((px = 5 or px = 6) and (py = 1 or py = 2)) then -- Right eye
                                        pixel_on := true;
                                    end if;
                                    -- Mouth
                                    if (px = 3 or px = 4) and (py = 3) then -- Mouth top-center
                                        pixel_on := true;
                                    end if;
                                    if (px >= 2 and px <= 5) and (py = 4) then -- Mouth mid-line
                                        pixel_on := true;
                                    end if;
                                    if (px = 2 or px = 5) and (py = 5 or py = 6) then -- Mouth bottom-sides
                                        pixel_on := true;
                                    end if;
                                    
                                    if pixel_on then
                                        rr := '0'; gg := '0'; bb := '0'; -- Black features
                                    end if;
                                    
                                ----------------------------------------------------------------
                                -- OPTIMIZED: DIAMOND SYMBOL (8x8 Pixel Art)
                                ----------------------------------------------------------------
                                when SYMBOL_DIAMOND =>
                                    if (((py = 0) and (px = 3 or px = 4)) or
                                        ((py = 1) and (px >= 2 and px <= 5)) or
                                        ((py = 2) and (px >= 1 and px <= 6)) or
                                        (((py = 3) or (py = 4)) and (px >= 0 and px <= 7)) or
                                        ((py = 5) and (px >= 1 and px <= 6)) or
                                        ((py = 6) and (px >= 2 and (px <= 5))) or
                                        ((py = 7) and (px = 3 or px = 4))) then
                                        rr := '0'; gg := '1'; bb := '1'; -- Cyan
                                    else
                                        is_symbol_pixel := false;
                                    end if;

                                ----------------------------------------------------------------
                                -- OPTIMIZED: NUMBER 7 SYMBOL (Using 8x8 engine)
                                ----------------------------------------------------------------
                                when SYMBOL_SEVEN =>
                                    if (((py = 0) and (px >= 1 and px <= 6)) or -- Top bar
                                       ((py = 1) and (px = 6)) or
                                       ((py = 2) and (px = 5)) or
                                       ((py = 3) and (px = 4)) or
                                       ((py >= 4 and py <= 7) and (px = 3))) then -- Diagonal + stem
                                        rr := '1'; gg := '1'; bb := '0'; -- Yellow
                                    else
                                        is_symbol_pixel := false;
                                    end if;

                                ----------------------------------------------------------------
                                -- OPTIMIZED: NUMBER 8 SYMBOL (Using 8x8 engine)
                                ----------------------------------------------------------------
                                when SYMBOL_EIGHT =>
                                    if ((((px >= 2 and px <= 5) and (py = 0 or py = 3 or py = 4 or py = 7))) or -- Horizontal bars
                                       (((px = 1 or px = 6) and (py = 1 or py = 2 or py = 5 or py = 6)))) then -- Vertical bars
                                        rr := '1'; gg := '0'; bb := '0'; -- Red
                                    else
                                        is_symbol_pixel := false;
                                    end if;

                                ----------------------------------------------------------------
                                -- OTHERS
                                ----------------------------------------------------------------
                                when others =>
                                    is_symbol_pixel := false; -- Not a symbol, un-flag

                            end case;
                        end if; -- end of sprite area check
                    end if;
                end loop;
            end if;
        end loop;

            ----------------------------------------------------------------
            -- Grid lines (Only drawn if no symbol pixel has been drawn)
            ----------------------------------------------------------------
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

            -- APPLY THE FIX: Only draw yellow grid lines if the pixel is NOT part of a symbol
            if (is_grid_h or is_grid_v) and (not is_symbol_pixel) then
                rr := '1'; gg := '1'; bb := '0'; -- Yellow grid lines
            end if;
        end if;

        -- Area outside the main 640x480 screen is black
        if h_count >= 640 or v_count >= 480 then
            rr := '0'; gg := '0'; bb := '0';
        end if;

        r <= rr; g <= gg; b <= bb;
    end process;

    red   <= r;
    green <= g;
    blue  <= b;

end Behavioral;
