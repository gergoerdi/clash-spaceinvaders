module SpaceInvaders (
           input        CLK_32MHZ,
           input        PS2CLK_A,
           input        PS2DATA_A,
           output       VGA_VSYNC,
           output       VGA_HSYNC,
           output [3:0] VGA_RED,
           output [3:0] VGA_GREEN,
           output [3:0] VGA_BLUE
           );

   wire                 CLK_25MHZ;
   wire                 CLK_LOCKED;
   wire [7:0]           VGA_RED_FULL;
   wire [7:0]           VGA_GREEN_FULL;
   wire [7:0]           VGA_BLUE_FULL;

   assign VGA_RED = VGA_RED_FULL[7:4];
   assign VGA_GREEN = VGA_GREEN_FULL[7:4];
   assign VGA_BLUE = VGA_BLUE_FULL[7:4];

   ClockMan25 u_ClockMan25
     (.CLK_IN1(CLK_32MHZ),
      .CLK_OUT1(CLK_25MHZ),
      .LOCKED(CLK_LOCKED)
      );

   topEntity u_topEntity
     (.CLK_25MHZ(CLK_25MHZ),
      .RESET(!CLK_LOCKED),

      .SWITCHES(8'b00000000),

      .BTN_CENTER(1'b0),
      .BTN_UP(1'b0),
      .BTN_DOWN(1'b0),
      .BTN_LEFT(1'b0),
      .BTN_RIGHT(1'b0),

      .PS2_CLK(PS2CLK_A),
      .PS2_DATA(PS2DATA_A),

      .VGA_VSYNC(VGA_VSYNC),
      .VGA_HSYNC(VGA_HSYNC),
      .VGA_RED(VGA_RED_FULL[7:0]),
      .VGA_GREEN(VGA_GREEN_FULL[7:0]),
      .VGA_BLUE(VGA_BLUE_FULL[7:0])
      );

endmodule
