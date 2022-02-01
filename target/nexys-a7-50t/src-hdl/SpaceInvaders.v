module SpaceInvaders (
           input        CLK100MHZ,

           input [15:0] SW,
           input        BTNC,
           input        BTNU,
           input        BTND,
           input        BTNL,
           input        BTNR,

           input        PS2_CLK,
           input        PS2_DATA,

           output       VGA_HS,
           output       VGA_VS,
           output [3:0] VGA_R,
           output [3:0] VGA_G,
           output [3:0] VGA_B
           );

   wire [7:0]           VGA_RED_FULL;
   wire [7:0]           VGA_GREEN_FULL;
   wire [7:0]           VGA_BLUE_FULL;

   assign VGA_R = VGA_RED_FULL[7:4];
   assign VGA_G = VGA_GREEN_FULL[7:4];
   assign VGA_B = VGA_BLUE_FULL[7:4];

   wire                 CLK_LOCKED;
   wire                 CLK_25MHZ_RAW;
   wire                 CLK_8MHZ_RAW;

   MMCM                 u_MMCM
     (.CLKIN_100MHZ(CLK100MHZ),
      .LOCKED(CLK_LOCKED),
      .CLKOUT_25MHZ(CLK_25MHZ_RAW),
      .CLKOUT_8MHZ(CLK_8MHZ_RAW)
      );

   wire                 CLK_25MHZ;
   BUFG                 BUFG_25MHZ(.I(CLK_25MHZ_RAW), .O(CLK_25MHZ));

   wire                 CLK_8MHZ;
   BUFG                 BUFG_8MHZ(.I(CLK_8MHZ_RAW), .O(CLK_8MHZ));

   reg [3:0]            CLK_1MHZ_DIV = 4'd0;
   always @(posedge CLK_8MHZ) begin
     CLK_1MHZ_DIV <= CLK_1MHZ_DIV + 4'd1;
   end
   wire                 CLK_1MHZ = CLK_1MHZ_DIV[3];

   topEntity u_topEntity
     (.CLK_25MHZ(CLK_25MHZ),
      .RESET(!CLK_LOCKED),
      .CLK_1MHZ(CLK_1MHZ),

      .SWITCHES(SW[7:0]),

      .BTN_CENTER(BTNC),
      .BTN_UP(BTNU),
      .BTN_DOWN(BTND),
      .BTN_LEFT(BTNL),
      .BTN_RIGHT(BTNR),

      .PS2_CLK(PS2_CLK),
      .PS2_DATA(PS2_DATA),

      .VGA_HSYNC(VGA_HS),
      .VGA_VSYNC(VGA_VS),
      .VGA_RED(VGA_RED_FULL),
      .VGA_GREEN(VGA_GREEN_FULL),
      .VGA_BLUE(VGA_BLUE_FULL)
      );

endmodule
