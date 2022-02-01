module MMCM
  (
   input wire  CLKIN_100MHZ,
   output wire LOCKED,
   output wire CLKOUT_25MHZ,
   output wire CLKOUT_8MHZ
   );

   parameter CLKFBOUT_MULT_F  = 45.125;
   parameter DIVCLK_DIVIDE    = 6;
   parameter CLKOUT0_DIVIDE_F = 29.875;
   parameter CLKOUT1_DIVIDE   = 94;

   // Internal clock: 100 MHz * 45.125 / 6 = 752.083 MHz
   // Output clock 0: 752.083 MHz / 29.875 = 25.174 MHz
   // Output clock 1: 752.083 MHz / 94 = 8.000 MHz
   
   // Feedback from CLKFBOUT to CLKFBIN
   wire        FB_O;
   wire        FB_I;

   MMCME2_ADV #
     (
      .BANDWIDTH          ("HIGH"),
      .COMPENSATION       ("INTERNAL"),

      .CLKIN1_PERIOD      (10.0),  // 10 ns = 100MHz
      .CLKIN2_PERIOD      (10.0),

      .CLKFBOUT_MULT_F    (CLKFBOUT_MULT_F),
      .CLKFBOUT_PHASE     (0),
      .DIVCLK_DIVIDE      (DIVCLK_DIVIDE),

      .CLKOUT0_DIVIDE_F   (CLKOUT0_DIVIDE_F),
      .CLKOUT0_DUTY_CYCLE (0.50),
      .CLKOUT0_PHASE      (0),

      .CLKOUT1_DIVIDE     (CLKOUT1_DIVIDE),
      .CLKOUT1_DUTY_CYCLE (0.50),
      .CLKOUT1_PHASE      (0),
      
      .STARTUP_WAIT       ("FALSE")
      )
   mmcm
     (
      .CLKIN1     (CLKIN_100MHZ),
      .CLKINSEL   (1'b1),

      .RST (1'b0),
      .PWRDWN(1'b0),
      .LOCKED     (LOCKED),

      .CLKFBIN    (FB_I),
      .CLKFBOUT   (FB_O),

      .CLKOUT0    (CLKOUT_25MHZ),
      .CLKOUT1    (CLKOUT_8MHZ)
      );


   assign FB_I = FB_O;

endmodule
