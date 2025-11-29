LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE ieee.numeric_std.ALL; 

ENTITY debounce IS
    GENERIC (
        -- MODIFIED: Value updated for a 25MHz clock
        -- 25,000,000 Hz * 0.001 s = 25,000 (1ms)
        COUNT_MAX : INTEGER := 50000 
    );
    PORT (
        CLK     : IN  STD_LOGIC;  -- 25 MHz System Clock Input
        RST_N   : IN  STD_LOGIC;  -- Asynchronous Reset (Active-Low)
        D       : IN  STD_LOGIC;  -- Raw Button/Switch Input
        Q       : OUT STD_LOGIC   -- Debounced Output
    );
END ENTITY debounce;

ARCHITECTURE rtl OF debounce IS
    SIGNAL counter      : INTEGER RANGE 0 TO COUNT_MAX := 0; 
    SIGNAL d_sync       : STD_LOGIC_VECTOR(1 DOWNTO 0) := (OTHERS => '0');
    SIGNAL q_temp       : STD_LOGIC := '0';
BEGIN

    PROCESS (CLK, RST_N)
    BEGIN
        IF (RST_N = '0') THEN
            counter <= 0;
            d_sync  <= (OTHERS => '0');
            q_temp  <= '0';
        ELSIF rising_edge(CLK) THEN
            
            -- 1. Input Synchronizer
            d_sync <= d_sync(0) & D;

            -- 2. Debounce Logic
            IF d_sync(1) /= q_temp THEN -- Potential change detected
                IF counter = COUNT_MAX THEN
                    q_temp  <= d_sync(1); -- Input stable for COUNT_MAX cycles
                    counter <= 0;
                ELSE
                    counter <= counter + 1; -- Continue counting
                END IF;
            ELSE -- Input is stable
                counter <= 0; -- Reset counter
            END IF;
        END IF;
    END PROCESS;

    Q <= q_temp;

END ARCHITECTURE rtl;
