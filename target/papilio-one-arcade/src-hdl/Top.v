module Top(
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
   wire                 CLKIN_IBUFG_OUT;                

   DCM25 u_DCM25
     (.CLKIN_IN(CLK_32MHZ),
      .CLKIN_IBUFG_OUT(CLKIN_IBUFG_OUT),
      .CLK0_OUT(CLK_25MHZ),
      .LOCKED_OUT(CLK_LOCKED)
      );
   
   topEntity u_topEntity
     (.CLK_25MHZ(CLK_25MHZ),
      .RESET(!CLK_LOCKED),
      .PS2_CLK(PS2CLK_A),
      .PS2_DATA(PS2DATA_A),
      .VGA_VSYNC(VGA_VSYNC),
      .VGA_HSYNC(VGA_HSYNC),
      .VGA_RED(VGA_RED[3:0]),
      .VGA_GREEN(VGA_GREEN[3:0]),
      .VGA_BLUE(VGA_BLUE[3:0])
      );	
   
endmodule
