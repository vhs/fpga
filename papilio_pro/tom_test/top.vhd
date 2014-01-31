----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date:    17:27:39 04/11/2013 
-- Design Name: 
-- Module Name:    top - Behavioral 
-- Project Name: 
-- Target Devices: 
-- Tool versions: 
-- Description: 
--
-- Dependencies: 
--
-- Revision: 
-- Revision 0.01 - File Created
-- Additional Comments: 
--
----------------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.pkg_uart.all;

library unisim;
use unisim.vcomponents.all;

entity top is
generic (g_xilinx : boolean := true;
         g_altera : boolean := false);
  port (clk        : in    std_logic;
        nrst       : in    std_logic;
        tx         : out   std_logic;
        rx         : in    std_logic;
        a          : inout std_logic_vector(15 downto 0);
        b          : inout std_logic_vector(15 downto 0);
        c          : inout std_logic_vector(15 downto 0);
        sdram_addr : out   std_logic_vector(12 downto 0);
        sdram_data : inout std_logic_vector(15 downto 0);
        sdram_dqml : out   std_logic;
        sdram_dqmh : out   std_logic;
        sdram_ba   : out   std_logic_vector(1 downto 0);
        sdram_nwe  : out   std_logic;
        sdram_ncas : out   std_logic;
        sdram_nras : out   std_logic;
        sdram_cs   : out   std_logic;
        sdram_clk  : out   std_logic;
        sdram_cke  : out   std_logic;
        led1       : out   std_logic;
--         jtag_tms   : in    std_logic;
--         jtag_tck   : in    std_logic;
--         jtag_tdi   : in    std_logic;
--         jtag_tdo   : out   std_logic;
        flash_cs   : out   std_logic;
        flash_ck   : out   std_logic;
        flash_si   : out   std_logic;
        flash_so   : in    std_logic
        );
end top;

architecture behavioral of top is

  constant c_serial_loopback : boolean := false;
  constant c_serial_uart     : boolean := true;

  constant c_clk_32_freq      : positive := 32_000_000;
  constant c_clk_10_freq      : positive := 10_000_000;
  constant c_clk_10_period_ns : positive := 1e9 / c_clk_10_freq;
  constant g_baud_divider     : positive := c_clk_32_freq / (16 * 9600) - 1;

  signal led_counter : integer range c_clk_32_freq-1 downto 0;

  signal en_16_x_baud : std_logic;
  signal baud_counter : integer range 0 to g_baud_divider+1 := 0;

  signal clk_32 : std_logic;
  signal clk_10 : std_logic;

  signal dcm_locked : std_logic;
  signal rst : std_logic;

begin

  xilinx_gen : if g_xilinx generate
    clk_10_i : entity work.clk_10
      port map (
        clk_in1  => clk,
        clk_out1 => clk_32,
        clk_out2 => clk_10,
        reset    => '0',
        locked   => dcm_locked);
  end generate;

  reset_b : block
    signal reset_counter : unsigned(3 downto 0) := (others => '0');
  begin
    reset_p : process(dcm_locked, clk_32)
    begin
      if dcm_locked = '0' then
        reset_counter <= (others => '0');
        rst           <= '1';
      elsif rising_edge(clk_32) then
        rst <= '1';
        if reset_counter /= (reset_counter'range => '1') then
          reset_counter <= reset_counter + 1;
        else
          rst <= '0';
        end if;
      end if;
    end process;
  end block;

--  altera_gen : if g_altera generate
--    clk_10_i : entity work.clk_altera_10
--      port map (
--        refclk   => clk,
--        rst      => rst,
--        outclk_0 => clk_32,
--        outclk_1 => clk_10,
--        locked   => open);
--  end generate;

  led1_b : block
    signal led_toggle : std_logic := '0';
  begin
    led1 <= led_toggle;

    led1_p : process(rst, clk_32)
    begin
      if rst = '1' then
       led_toggle <= '0';
      elsif rising_edge(clk_32) then
        if led_counter /= 0 then
          led_counter <= led_counter - 1;
        else
          led_toggle  <= not led_toggle;
          led_counter <= c_clk_32_freq-1;
        end if;
      end if;
    end process;
  end block;

  sdram_addr <= (others => '0');
  sdram_data <= (others => 'Z');
  sdram_dqml <= '0';
  sdram_dqmh <= '0';
  sdram_ba   <= (others => '0');
  sdram_nwe  <= '1';
  sdram_ncas <= '1';
  sdram_nras <= '1';
  sdram_cs   <= '1';
  sdram_clk  <= '0';
  sdram_cke  <= '0';

  flash_cs <= '1';
  flash_ck <= '0';
  flash_si <= '0';


  serial_loopback_gen : if c_serial_loopback generate
  begin
    tx <= rx;
  end generate;


  serial_uart_gen : if c_serial_uart generate
    signal data_in          : unsigned(7 downto 0);
    signal write_buffer     : std_logic;
    signal buffer_half_full : std_logic;
    signal serial_to_host   : uart_to_host;
  begin

    write_p : process(clk_32, rst)
    begin
      if rst = '1' then
        write_buffer <= '0';
        data_in      <= to_unsigned(65, 8);
      elsif rising_edge(clk_32) then
        write_buffer <= '0';
        if buffer_half_full = '0' and led_counter = 0 then
          write_buffer <= '1';
          if data_in = to_unsigned(65+25, 8) then
            data_in <= to_unsigned(65, 8);
          else
            data_in <= data_in + 1;
          end if;
        end if;
      end if;
    end process;

    tx <= serial_to_host.data;
    uart_tx_1 : entity work.uart_tx
      port map (
        data_in          => std_logic_vector(data_in),
        write_buffer     => write_buffer,
        reset_buffer     => rst,
        en_16_x_baud     => en_16_x_baud,
        serial_out       => serial_to_host,
        buffer_empty     => open,
        buffer_full      => open,
        buffer_half_full => buffer_half_full,
        clk              => clk_32);

    baud_16_x_gen : process(clk_32, rst)
    begin
      if rst = '1' then
        baud_counter <= 0;
        en_16_x_baud <= '0';
      elsif rising_edge(clk_32) then
        en_16_x_baud <= '0';
        if baud_counter = g_baud_divider then
          baud_counter <= 0;
          en_16_x_baud <= '1';
        else
          baud_counter <= baud_counter + 1;
        end if;
      end if;
    end process;
  end generate;


  led_strip_b : block
    -- LED strip
    constant ws2811_t0h_ns : integer := 500;  -- 0 code,high voltage time 0.5 μs ±150ns
    constant ws2811_t0l_ns : integer := 2000;  -- 0 code,low voltage time 2.0 μS ±150ns
    constant ws2811_t1h_ns : integer := 1200;  -- 1 code,high voltage time 1.2 μs ±150ns
    constant ws2811_t1l_ns : integer := 1300;  -- 1 code,low voltage time 1.3 μs ±150ns
    constant ws2811_ret_ns : integer := 51000;  -- low voltage time above 50μs

    type led_state_t is (led_idle, led_reset, led_high, led_low);
    signal led_state             : led_state_t;
    signal led_data_to_strip     : std_logic;
    signal state_counter_load    : std_logic;
    signal state_counter_load_1d : std_logic;
    signal state_counter         : integer range 0 to ws2811_ret_ns;

    signal bit_counter : integer range 1 to 24;

    constant bit_data_white : std_logic_vector(23 downto 0) := (others => '1');
    signal bit_data         : std_logic_vector(23 downto 0);
  begin

    a(0) <= led_data_to_strip;

    led_strip_p : process(rst, clk_10)
    begin
      if rst = '1' then
        led_state             <= led_idle;
        led_data_to_strip     <= '0';
        state_counter_load_1d <= '0';
        state_counter_load    <= '0';
        bit_counter           <= 1;
        state_counter         <= 0;
        bit_data              <= (others => '0');
      elsif rising_edge(clk_10) then
        state_counter_load_1d <= state_counter_load;
        state_counter_load    <= '0';

        -- state machine
        case led_state is
          when led_reset =>
            led_data_to_strip  <= '0';
            state_counter_load <= '1';
            if state_counter = 0 then
              led_state          <= led_high;
              state_counter_load <= '0';
              bit_data           <= bit_data_white;
            end if;
          when led_high =>
            led_data_to_strip  <= '1';
            state_counter_load <= '1';
            if state_counter = 0 then
              led_state          <= led_low;
              state_counter_load <= '0';
              -- bit counter
              if bit_counter = 24 then
                bit_counter <= 1;
              else
                bit_counter <= bit_counter + 1;
              end if;
              bit_data <= bit_data(bit_data'high-1 downto 0) & '0';
            end if;
          when led_low =>
            led_data_to_strip  <= '0';
            state_counter_load <= '1';
            if state_counter = 0 then
              if bit_counter = 0 then
                led_state <= led_idle;
              else
                led_state <= led_high;
              end if;
              state_counter_load <= '0';
            end if;
          when others =>
            led_state <= led_reset;
        end case;

        -- state timer
        if state_counter < c_clk_10_period_ns then
          state_counter <= 0;
        else
          state_counter <= state_counter - c_clk_10_period_ns;
        end if;

        -- state timer load
        if state_counter_load_1d = '0' and state_counter_load = '1' then
          case led_state is
            when led_reset =>
              state_counter <= ws2811_ret_ns;
            when led_high =>
              if bit_data(bit_data'high) = '1' then
                state_counter <= ws2811_t1h_ns;
              else
                state_counter <= ws2811_t0h_ns;
              end if;
            when led_low =>
              if bit_data(bit_data'high) = '1' then
                state_counter <= ws2811_t1l_ns;
              else
                state_counter <= ws2811_t0l_ns;
              end if;
            when others =>
              state_counter <= 0;
          end case;
        end if;

      end if;
    end process;
  end block;

end behavioral;
