module Top(
           input        CLK_32MHZ,
//           input        PS2CLK_A,
//           input        PS2DATA_A,
           output       VGA_VSYNC,
           output       VGA_HSYNC,
           output [2:0] VGA_RED,
           output [2:0] VGA_GREEN,
           output [1:0] VGA_BLUE
           );

   wire                 CLK_25MHZ;
   wire                 CLK_LOCKED;
   wire                 PS2CLK_A;
   wire                 PS2DATA_A;
   wire [7:0]           VGA_RED_FULL;
   wire [7:0]           VGA_GREEN_FULL;
   wire [7:0]           VGA_BLUE_FULL;

   assign VGA_RED = VGA_RED_FULL[7:5];
   assign VGA_GREEN = VGA_GREEN_FULL[7:5];
   assign VGA_BLUE = VGA_BLUE_FULL[7:6];

   ClockMan25 u_ClockMan25
     (.CLK_IN1(CLK_32MHZ),
      .CLK_OUT1(CLK_25MHZ),
      .LOCKED(CLK_LOCKED)
      );

   SpaceInvaders u_SpaceInvaders
     (.CLK_25MHZ(CLK_25MHZ),
      .RESET(!CLK_LOCKED),
      .PS2_CLK(PS2CLK_A),
      .PS2_DATA(PS2DATA_A),
      .VGA_VSYNC(VGA_VSYNC),
      .VGA_HSYNC(VGA_HSYNC),
      .VGA_RED(VGA_RED_FULL[7:0]),
      .VGA_GREEN(VGA_GREEN_FULL[7:0]),
      .VGA_BLUE(VGA_BLUE_FULL[7:0])
      );

endmodule
