library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;
     
package pkg_uart is
  type uart_from_host is record
    data : std_logic;
  end record;
  type uart_to_host is record
    data : std_logic;
  end record;
end package;

package body pkg_uart is
end package body;
