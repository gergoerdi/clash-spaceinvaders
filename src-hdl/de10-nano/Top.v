module Top(

      ///////// FPGA /////////
      input              FPGA_CLK1_50,
	
//		///////// Arduino ///////// 
//		inout       [15:0] ARDUINO_IO,
		
//		///////// Gergotron ///////// 
//		input              PS2A_CLK,
//		input              PS2A_DAT,
////		output             PS2_EN,

      ///////// HDMI /////////
      inout              HDMI_I2C_SCL,
      inout              HDMI_I2C_SDA,
      inout              HDMI_I2S,
      inout              HDMI_LRCLK,
      inout              HDMI_MCLK,
      inout              HDMI_SCLK,
      output             HDMI_TX_CLK,
      output      [23:0] HDMI_TX_D,
      output             HDMI_TX_DE,
      output             HDMI_TX_HS,
      input              HDMI_TX_INT,
      output             HDMI_TX_VS
);



wire				reset_n;
//Video Pattern Generator
wire	[3:0]		vpg_disp_mode;
wire				vpg_pclk;
wire				vpg_de, vpg_hs, vpg_vs;
wire	[23:0]	vpg_data;

I2C_HDMI_Config u_I2C_HDMI_Config (
	.iCLK(FPGA_CLK1_50),
	.iRST_N(reset_n),
	.I2C_SCLK(HDMI_I2C_SCL),
	.I2C_SDAT(HDMI_I2C_SDA),
	.HDMI_TX_INT(HDMI_TX_INT)
	 );

assign reset_n = 1'b1;

wire clk_25;
wire gen_clk_locked;

video u_video (
	.refclk_50_clk(FPGA_CLK1_50),
	.rst_reset(!reset_n),
	.outclk_25_clk(clk_25),
	.locked_export(gen_clk_locked));

assign HDMI_TX_CLK = clk_25;
//assign PS2_EN = 1'b0;	

SpaceInvaders u_spaceinvaders
      (.CLK_25MHZ(clk_25),
       .RESET(!gen_clk_locked),
       .VGA_VSYNC(HDMI_TX_VS),
       .VGA_HSYNC(HDMI_TX_HS),
       .VGA_DE(HDMI_TX_DE),
       .VGA_RED(HDMI_TX_D[23:16]),
       .VGA_GREEN(HDMI_TX_D[15:8]),
       .VGA_BLUE(HDMI_TX_D[7:0])
      );	
	
endmodule
