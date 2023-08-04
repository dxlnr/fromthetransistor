// ARM32 CPU Implementation

module memory
    #(parameter DEPTH=4096,
      parameter N=32
    )(
    input wire         clk,
    input wire         i_mem_r_en,
    input wire         i_mem_w_en,
    input wire [N-1:0] i_mem_addr,
    input wire [N-1:0] i_mem_w_data,
    output reg [N-1:0] o_mem_r_data
);
    reg [N-1:0] mem [0:DEPTH-1];
    
    always @(posedge clk) begin
        if(i_mem_r_en == 1'b1)
            o_mem_r_data <= mem[i_mem_addr];
        else if(i_mem_w_en == 1'b1)
            mem[i_mem_addr] <= i_mem_w_data;
    end
endmodule

module ins_memory 
    #(parameter DEPTH=4096,
      parameter D_BITS=32
    )(
    input wire         clk
); 
    reg [D_BITS - 1:0] mem [0:DEPTH - 1];
endmodule

module program_counter 
    #(parameter N = 32
    )(
    input wire         clk,
    input wire         i_reset_n,
    input wire         i_load,
    input wire [N-1:0] i_pc,
    output reg [N-1:0] o_pc
);
    always @(posedge clk) begin
        if (!i_reset_n) begin 
            o_pc <= 32'b0;
        end
        else if (clk && i_load) 
            o_pc <= i_pc;
        else 
            o_pc <= o_pc;
    end
endmodule 

module fetching_stage
    #(parameter N = 32
    )(
    input wire         clk,
    input wire         i_reset_n,
    input wire         i_freeze,
    input wire         i_br,
    input wire [N-1:0] i_br_addr,
    output reg [N-1:0] o_pc,
    output reg [N-1:0] o_ins
);
    wire [N-1:0] w_pc_in;
    wire [N-1:0] w_pc_o;
    wire [N-1:0] w_pc;
    wire [N-1:0] w_mux_in;
    wire [N-1:0] w_mux_o;
    
    ins_memory #(.DEPTH(512)) im (
        .clk(clk)
    );

    assign w_mux_o = i_br ? i_br_addr: w_mux_in;

    program_counter pc (
        .clk(clk),
        .i_reset_n(i_reset_n),
        .i_load(1'b1),
        .i_pc(w_pc_in),
        .o_pc(w_pc_o)
    );

    assign w_pc_in = w_mux_o;
    assign w_pc = w_pc_o + 1;
    assign w_mux_in = w_pc;

    always @(posedge clk) begin
        o_ins <= im.mem[w_pc_o];
    end

    assign o_pc = w_pc;
endmodule

module control_unit
    #(parameter N = 32
    )(
    input wire       clk,
    input wire [3:0] i_ops,
    input wire [1:0] i_ins_t,
    input wire       i_ld_st,
    output reg[3:0]  o_s_c,
    output reg       o_s_wb_en,
    output reg       o_s_mem_r_en,
    output reg       o_s_mem_w_en,
    output reg       o_s_br
);
    always @(posedge clk, i_ops, i_ins_t, i_ld_st) begin
        o_s_mem_w_en <= 1'b0;
        o_s_mem_r_en <= 1'b0;
        o_s_wb_en = 1'b0;
        o_s_br <= 1'b0;
        o_s_c <= 4'b0000;

        case (i_ins_t)
            2'b00: begin // Arithmetic Instruction
                case (i_ops)
                    4'b1101: begin // MOV
                        o_s_wb_en <= 1'b1;
                        o_s_c <= 4'b0001;
                    end 
                    4'b1101: begin // MVN
                        o_s_wb_en <= 1'b1;
                        o_s_c <= 4'b1001;
                    end
                    4'b0100: begin // ADD
                        o_s_wb_en <= 1'b1;
                        o_s_c <= 4'b0010;
                    end
                    4'b0010: begin // SUB
                        o_s_wb_en <= 1'b1;
                        o_s_c <= 4'b0100;
                    end
                    4'b0000: begin // AND
                        o_s_wb_en <= 1'b1;
                        o_s_c <= 4'b0110;
                    end
                    4'b1100: begin // ORR
                        o_s_wb_en <= 1'b1;
                        o_s_c <= 4'b0111;
                    end
                    4'b0001: begin // EOR
                        o_s_wb_en <= 1'b1;
                        o_s_c <= 4'b1000;
                    end
                    4'b1010: begin // CMP
                        o_s_c <= 4'b0100;
                    end
                    4'b1000: begin // TST
                        o_s_c <= 4'b0110;
                    end 
                endcase
            end
            2'b01: begin // Memory (Load/Store) Instruction
                case (i_ld_st)
                    1'b1: begin // LDR
                        o_s_mem_r_en <= 1'b1;
                        o_s_c <= 4'b0010;
                        o_s_wb_en <= 1'b1;
                    end
                    1'b0: begin // STR
                        o_s_mem_w_en <= 1'b1;
                        o_s_c <= 4'b0010;
                    end
                endcase
            end
            2'b10: begin // Branch Instruction
                o_s_br <= 1'b1;
            end
            2'b11: begin
            end
        endcase
    end
endmodule

module cond (
    input wire       clk,
    input wire [3:0] i_cond,
    input wire [3:0] i_nzcv,
    output reg       o_res
);
    wire n;
    wire z;
    wire c;
    wire v;
    assign {n, z, c, v} = i_nzcv;

    always@(posedge clk, i_cond, i_nzcv) begin
        o_res <= 1'b0;

        case(i_cond)
            4'b0000: begin
                if(z == 1'b1)
                    o_res <= 1'b1;
            end
            4'b0001: begin
                if(z == 1'b0)
                    o_res <= 1'b1;
            end
            4'b0010: begin
                if(c == 1'b1)
                    o_res <= 1'b1;
            end
            4'b0011: begin
                if(c == 1'b0)
                    o_res = 1'b1;
            end
            4'b0100: begin
                if(n == 1'b1)
                    o_res <= 1'b1;
            end
            4'b0101: begin
                if(n == 1'b0)
                    o_res <= 1'b1;
            end
            4'b0110: begin
                if(v == 1'b1)
                    o_res <= 1'b1;
            end
            4'b0111: begin
                if(v == 1'b0)
                    o_res <= 1'b1;
            end
            4'b1000: begin
                if(c == 1'b1 & z == 1'b0)
                    o_res <= 1'b1;
            end
            4'b1001: begin
                if(c == 1'b0 & z == 1'b1)
                    o_res <= 1'b1;
            end
            4'b1010: begin
                if(n == v)
                    o_res <= 1'b1;
            end
            4'b1011: begin
                if(n != v)
                    o_res <= 1'b1;
            end
            4'b1100: begin
                if(z == 1'b0 & n == v)
                    o_res <= 1'b1;
            end
            4'b1101: begin
                if(z == 1'b1 & n != v)
                    o_res = 1'b1;
            end
            4'b1111: begin
                o_res <= 1'b1;
            end
        endcase
    end
endmodule

module register_bank 
    #(parameter N = 32
    )(
    input wire         clk,
    input wire         i_reset_n,
    input wire [3:0]   i_s1,
    input wire [3:0]   i_s2,
    input wire [3:0]   i_wb_addr,
    input wire [N-1:0] i_wb_data,
    input wire         i_wb_en,
    output reg [N-1:0] o_vs1,
    output reg [N-1:0] o_vs2
);
    reg [N-1:0] regs [15:0];

    assign o_vs1 = regs[i_s1];
    assign o_vs2 = regs[i_s2];

    always @(negedge clk) begin
        if(!i_reset_n) begin
            for (integer i=0; i<N; i=i+1) regs[i] <= 32'b0;
        end

        else if(i_wb_en) begin
            regs[i_wb_addr] <= i_wb_data;
        end
         
        else begin
            for(integer i = 0; i < 15; i = i + 1) begin
                regs[i] <= regs[i];
            end
        end 
    end

endmodule

module arm32_decoder
    #(parameter N = 32
    )(
    input              clk,
    input wire         i_reset_n,
    input wire [N-1:0] i_pc,
    input wire [N-1:0] i_ins,
    input wire [3:0]   i_nzcv,          // Condition flags (NZCV)
    input wire         i_w_f_en,        //
    input wire         i_wb_en, 
    input wire [N-1:0] i_wb_data,
    input wire [3:0]   i_wb_addr,
    output reg [N-1:0] o_pc,
    output reg [11:0]  o_imm12,         // 12-bit immediate
    output reg [23:0]  o_imm24,         // 24-bit immediate
    output reg [3:0]   o_alu_c,         // ALU control                         (3)
    output reg [N-1:0] o_vs1,           // 1st operand (value)                 (3)  
    output reg [N-1:0] o_vs2,           // 2nd operand (value)                 (3)
    output reg [3:0]   o_s1,            // 1st operand (reg address) : Rn
    output reg [3:0]   o_s2,            // 2nd operand (reg address) : Rm
    output reg         o_br,            // branch taken 
    output reg         o_f_c,           // carry flag
    output reg         o_imm,           // immediate bit
    output reg         o_mem_r_en,      // memory read enable used mem access  (4)
    output reg         o_mem_w_en,      // memory write enable used mem access (4)
    output reg         o_wb_en,         // write back enable used write back   (5) 
    output reg [3:0]   o_wb_addr,       // destination (wb) register : Rd      (5)
    output reg [3:0]   o_w_f_en
);
    wire [3:0]  w_rn    = i_ins[19:16]; // 1st operand (reg address)
    wire [3:0]  w_rs    = i_ins[11:8];  // only used in mul and mla (reg address) Rs = Rm
    wire [3:0]  w_rm    = i_ins[3:0];   // 2nd operand (reg address)
    wire [3:0]  w_rd    = i_ins[15:12]; // destination register
    wire [3:0]  w_op    = i_ins[24:21]; // opcode specifying the instruction
    wire [1:0]  w_ins_t = i_ins[27:26]; // instruction type
    wire [3:0]  w_cond  = i_ins[31:28]; // condition bits
    wire        w_imm   = i_ins[25];    // immediate bit (1 = immediate) for expanding to imm32.
    wire        w_ld_st = i_ins[20];    // load/store bit (1 = load)
    wire        w_br;                   // branch bit (1 = branch)
    wire        w_cond_res;             // condition result
    wire        w_s_mux;                // mux select bit
    wire [9:0]  w_mux_in;           
    wire        w_wb_en;                // write back enable
    wire        w_mem_r_en;             // memory read enable 
    wire        w_mem_w_en;             // memory write enable
    wire [3:0]  w_alu_c;                // ALU control bits
    wire        w_r_s2;

    control_unit cu (
        .clk(clk),
        .i_ops(w_op),
        .i_ins_t(w_ins_t),
        .i_ld_st(w_ld_st),
        .o_s_c(w_alu_c),
        .o_s_wb_en(w_wb_en),
        .o_s_mem_r_en(w_mem_r_en),
        .o_s_mem_w_en(w_mem_w_en),
        .o_s_br(w_br)
    );

    cond cc (
        .clk(clk),
        .i_cond(w_cond),
        .i_nzcv(i_nzcv),
        .o_res(w_cond_res)
    );

    assign o_pc         = i_pc;
    assign o_imm12      = i_ins[11:0];
    assign o_imm24      = i_ins[23:0];
    assign o_wb_addr    = w_rd;
    assign o_s1         = w_rn;
    assign o_s2         = w_rm;
    assign o_f_c        = i_nzcv[2];
    assign w_s_mux      = w_cond_res; 
    // assign w_s_mux      = ~w_cond_res | 1'b0; 
    assign o_mem_r_en   = w_s_mux ? 1'b0 : w_mem_r_en;
    assign o_mem_w_en   = w_s_mux ? 1'b0 : w_mem_w_en;
    assign o_wb_en      = w_s_mux ? 1'b0 : w_wb_en;
    assign o_br         = w_s_mux ? 1'b0 : w_br;
    assign o_imm        = w_s_mux ? 1'b0 : w_imm;
    assign o_alu_c      = w_s_mux ? 4'b0 : w_alu_c;
    assign w_r_s2       = w_mem_w_en ? w_rd : w_rm;
    assign o_w_f_en     = i_w_f_en;

    register_bank rb (
        .clk(clk),
        .i_reset_n(i_reset_n),
        .i_s1(w_rn),
        .i_s2(w_rm),
        .i_wb_addr(i_wb_addr),
        .i_wb_data(i_wb_data),
        .i_wb_en(i_wb_en),
        .o_vs1(o_vs1),
        .o_vs2(o_vs2)
    );

endmodule

module decoder_stage
    #(parameter N = 32
    )(
    input              clk,
    input wire         i_reset_n,
    input wire         i_flush,         // flush the pipe (branch predict gone wrong)
    input wire         i_freeze,        // freeze pipeline (potential inefficiency)
    input wire [N-1:0] i_pc,
    input wire         i_mem_r_en,
    input wire         i_mem_w_en,
    input wire         i_imm,
    input wire         i_br,
    input wire [3:0]   i_alu_c,
    input wire [N-1:0] i_vs1,
    input wire [N-1:0] i_vs2,
    input wire [11:0]  i_imm12,
    input wire [23:0]  i_imm24,
    input wire         i_w_f_en,
    input wire         i_wb_en,
    input wire [3:0]   i_wb_addr,
    input wire         i_ld_st,
    input wire         i_f_c,
    input wire [3:0]   i_s1,
    input wire [3:0]   i_s2,
    output reg [N-1:0] o_pc,
    output reg [11:0]  o_imm12,
    output reg [23:0]  o_imm24,
    output reg [3:0]   o_alu_c,
    output reg [N-1:0] o_vs1, 
    output reg [N-1:0] o_vs2,
    output reg [3:0]   o_s1, 
    output reg [3:0]   o_s2,
    output reg         o_ld_st,
    output reg         o_br,
    output reg         o_f_c,
    output reg         o_imm,
    output reg         o_mem_r_en,
    output reg         o_mem_w_en,
    output reg         o_wb_en,
    output reg [3:0]   o_wb_addr,
    output reg         o_w_f_en
);
    always @(posedge clk, posedge i_reset_n) begin
        if (!i_reset_n || i_flush) begin
            o_pc         <= 32'b0;
            o_imm12      <= 12'b0;
            o_imm24      <= 24'b0;
            o_alu_c      <= 4'b0;
            o_vs1        <= 32'b0;
            o_vs2        <= 32'b0;
            o_s1         <= 4'b0;
            o_s1         <= 4'b0;
            o_ld_st      <= 1'b0;
            o_br         <= 1'b0;
            o_f_c        <= 1'b0;
            o_imm        <= 1'b0;
            o_mem_r_en   <= 1'b0;
            o_mem_w_en   <= 1'b0;
            o_wb_en      <= 1'b0;
            o_wb_addr    <= 4'b0; 
            o_w_f_en     <= 1'b0;
        end else begin
            o_pc         <= i_pc;
            o_imm12      <= i_imm12;
            o_imm24      <= i_imm24;
            o_alu_c      <= i_alu_c;
            o_vs1        <= i_vs1;
            o_vs2        <= i_vs2;
            o_s1         <= i_s1;
            o_s1         <= i_s2;
            o_ld_st      <= i_ld_st;
            o_br         <= i_br;
            o_f_c        <= i_f_c;
            o_imm        <= i_imm;
            o_mem_r_en   <= i_mem_r_en;
            o_mem_w_en   <= i_mem_w_en;
            o_wb_en      <= i_wb_en;
            o_wb_addr    <= i_wb_addr; 
            o_w_f_en     <= i_w_f_en;
        end
    end
endmodule

module exp_to_32b_value
    #(parameter N = 32
    )(
    input              clk,
    input [N-1:0]      i_v_rm,
    input [11:0]       i_imm12,
    input              i_s_imm,
    input              i_s_mem,
    output reg [31:0]  o_res
);
    reg [N-1:0] r_imm32;
    reg [N-1:0] r_rotate_imm;

    wire [4:0] w_shift_imm  = i_imm12[11:7];
    wire [3:0] w_rotate_imm = i_imm12[11:8];
    wire [1:0] w_shift      = i_imm12[6:5];
    wire [7:0] w_imm8       = i_imm12[7:0];

    always @(posedge clk) begin
        r_imm32 <= {24'b0, w_imm8};

        for(integer k = 0; k < w_rotate_imm; k = k + 1) begin
            r_imm32 <= {r_imm32[1:0], r_imm32[31:2]};
        end 
        r_rotate_imm <= i_v_rm;

        for(integer k = 0; k <= w_shift_imm; k = k + 1) begin
            r_rotate_imm <= {r_rotate_imm[0], r_rotate_imm[31:0]};
        end
    end

    assign o_res = i_s_mem == 1'b1  ? { {20{i_imm12[11]}}, i_imm12} : 
        i_s_imm == 1'b1             ? r_imm32 : 
        w_shift == 2'b00            ? i_v_rm <<  {1'b0, w_shift_imm} :
        w_shift == 2'b01            ? i_v_rm >>  {1'b0, w_shift_imm} :
        w_shift == 2'b10            ? i_v_rm >>> {1'b0, w_shift_imm} :
        r_rotate_imm; 
endmodule

module alu 
    #(parameter N = 32
    )(
    input wire [3:0]   i_ops,
    input wire [N-1:0] i_x,
    input wire [N-1:0] i_y,
    input wire         i_f_c,
    output reg [N-1:0] o_res,
    output reg [3:0]   o_nzcv
);
    reg c;
    reg v;
    wire z;
    wire n;

    always @* begin
        c <= 1'b0;
        v <= 1'b0;

        case (i_ops)
            4'b0001: o_res <= i_y;
            4'b1001: o_res <= ~i_y;
            // 4'b0010: {c, o_res} <= i_x + i_y;
            4'b0010: o_res <= i_x + i_y;
            4'b0011: {c, o_res} <= i_x + i_y + i_f_c;
            4'b0100: {c, o_res} <= i_x - i_y;
            4'b0101: {c, o_res} <= i_x - i_y - i_f_c;
            4'b0110: o_res <= i_x & i_y;
            4'b0111: o_res <= i_x | i_y;
            4'b1000: o_res <= i_x ^ i_y;
            4'b0100: o_res <= i_x - i_y;
            4'b0110: o_res <= i_x & i_y;
            4'b0010: o_res <= i_x + i_y;
            4'b0010: o_res <= i_x + i_y;
        endcase

        if(i_ops == 4'b0010 || i_ops == 4'b0011)
            v <= (i_x[N-1] == i_y[N-1]) & (i_x[N-1] == ~o_res[N-1]);
        else if (i_ops == 4'b0100 || i_ops == 4'b0101)
            v <= (i_x[N-1] == ~i_y[N-1]) & (i_x[N-1] == ~o_res[N-1]);
    end

    assign n = o_res[N-1];
    assign z = o_res == 32'b0 ? 1'b1 : 1'b0;
    assign o_nzcv = {n, z, c, v};

endmodule

module execution_unit
    #(parameter N = 32
    )(
    input               clk, 
    input wire [N-1:0]  i_pc,
    input wire          i_mem_r_en,
    input wire          i_mem_w_en,
    input wire          i_wb_en,
    input wire [3:0]    i_wb_addr,
    input wire          i_imm,
    input wire          i_f_c,
    input wire [3:0]    i_ops,
    input wire [11:0]   i_imm12,
    input wire [23:0]   i_imm24,
    input wire [N-1:0]  i_v_rn,
    input wire [N-1:0]  i_v_rm,
    output reg [N-1:0]  o_pc,
    output reg          o_mem_r_en,     // memory read enable used mem access  (4)
    output reg          o_mem_w_en,     // memory write enable used mem access (4)
    output reg          o_wb_en,        // write back enable used write back   (5) 
    output reg [3:0]    o_wb_addr,      // write back address                  (5)
    output reg [N-1:0]  o_alu_res,      // alu result                          (4)
    output reg [N-1:0]  o_vs2,          // 
    output reg [3:0]    o_alu_nzcv,     // conditio flags (NZCV)
    output reg [N-1:0]  o_br_addr       // branch address (pc)                 (5) 
);
    wire         w_mem = i_mem_r_en | i_mem_w_en;
    wire [N-1:0] w_alu_left;
    wire [N-1:0] w_imm32;

    assign o_pc        = i_pc; 
    assign o_mem_r_en  = i_mem_r_en;
    assign o_mem_w_en  = i_mem_w_en;
    assign o_wb_en     = i_wb_en;
    assign o_wb_addr   = i_wb_addr;
    assign o_br_addr   = i_pc + { {6{i_imm24[23]}}, i_imm24, 2'b0 };
    assign o_vs2       = i_v_rm;
    assign w_alu_left  = i_v_rn;

    exp_to_32b_value e (
        .clk(clk),
        .i_v_rm(i_v_rm),
        .i_imm12(i_imm12),
        .i_s_imm(i_imm),
        .i_s_mem(w_mem),
        .o_res(w_imm32)
    );

    alu a (
        .i_ops(i_ops),
        .i_x(w_alu_left),
        .i_y(w_imm32),
        .i_f_c(i_f_c),
        .o_res(o_alu_res),
        .o_nzcv(o_alu_nzcv)
    );
endmodule

module execution_stage 
    #(parameter N = 32
    )(
    input              clk,
    input wire         i_reset_n,
    input wire         i_flush, 
    input wire         i_freeze, 
    input wire [N-1:0] i_pc,
    input wire         i_mem_r_en,
    input wire         i_mem_w_en,
    input wire         i_wb_en,
    input wire [3:0]   i_wb_addr,
    input wire [N-1:0] i_alu_res,
    input wire [N-1:0] i_vs2,
    output reg [N-1:0] o_pc,
    output reg         o_mem_r_en,
    output reg         o_mem_w_en,
    output reg         o_wb_en,
    output reg [3:0]   o_wb_addr,
    output reg [N-1:0] o_alu_res,
    output reg [N-1:0] o_vs2
);
    always @(posedge clk, posedge i_reset_n) begin
        if (!i_reset_n || i_flush) begin
            o_pc         <= 32'b0;
            o_mem_r_en   <= 1'b0;
            o_mem_w_en   <= 1'b0;
            o_wb_en      <= 1'b0;
            o_wb_addr    <= 4'b0; 
            o_alu_res    <= 32'b0;
            o_vs2        <= 32'b0;
        end else begin
            o_pc         <= i_pc;
            o_mem_r_en   <= i_mem_r_en;
            o_mem_w_en   <= i_mem_w_en;
            o_wb_en      <= i_wb_en;
            o_wb_addr    <= i_wb_addr;
            o_alu_res    <= i_alu_res;
            o_vs2        <= i_vs2;
        end
    end
endmodule

module status_register
    #(parameter N = 32
    )(
    input wire       clk,
    input wire       i_reset_n,
    input wire [3:0] i_nzcv,
    input wire       i_mem,
    output reg [3:0] o_nzcv
);
    always@(negedge clk)  begin
        if (!i_reset_n) 
            o_nzcv <= 0;
        else if (i_mem) 
            o_nzcv <= i_nzcv;
        else 
            o_nzcv <= o_nzcv;
    end
endmodule

module memory_stage
    #(parameter N = 32
    )(
    input wire         clk, 
    input wire         i_reset_n,
    input wire [N-1:0] i_pc,
    input wire         i_mem_r_en,
    input wire         i_mem_w_en,
    input wire         i_wb_en,
    input wire [N-1:0] i_alu_res,
    input wire [N-1:0] i_vs2,
    input wire [3:0]   i_wb_addr,
    output reg [N-1:0] o_pc,
    output reg         o_mem_r_en,
    output reg         o_mem_w_en,
    output reg         o_wb_en,
    output reg [3:0]   o_wb_addr,
    output reg [N-1:0] o_wb_data,
    output reg [N-1:0] o_alu_res
    );

    assign o_pc       = i_pc;
    assign o_mem_r_en = i_mem_r_en;
    assign o_mem_w_en = i_mem_w_en;
    assign o_alu_res  = i_alu_res;
    assign o_wb_en    = i_wb_en;
    assign o_wb_addr  = i_wb_addr;

    memory dm (
        .clk(clk),
        .i_mem_r_en(i_mem_r_en),
        .i_mem_w_en(i_mem_w_en),
        .i_mem_addr(i_alu_res),
        .i_mem_w_data(i_vs2),
        .o_mem_r_data(o_wb_data)
    );

endmodule

module wb_stage
    #(parameter N = 32
    )(
    input wire         clk, 
    input wire [N-1:0] i_pc,
    input wire         i_mem_r_en,
    input wire         i_wb_en,
    input wire [N-1:0] i_wb_data,
    input wire [3:0]   i_wb_addr,
    input wire [N-1:0] i_alu_res,
    output reg [N-1:0] o_pc,
    output reg         o_wb_en,
    output reg [N-1:0] o_wb_data,
    output reg [3:0]   o_wb_addr
);
    assign o_pc = i_pc;
    assign o_wb_en = i_wb_en;
    assign o_wb_addr = i_wb_addr;
    assign o_wb_data = i_mem_r_en ? i_wb_data : i_alu_res;

endmodule

module forwarding_unit
    #(parameter N = 32
    )(
    input wire         clk,
    input wire         i_forw_en,
    input wire [3:0]   i_wb_addr,
    input wire [3:0]   i_mem_addr,
    input wire         i_s_wb_en,
    input wire         i_s_mem_en,
    input wire [3:0]   i_s1,
    input wire [3:0]   i_s2,
    output reg [1:0]   o_s1,
    output reg [1:0]   o_s2
);
    always @(posedge clk) begin
        o_s1 <= 2'b00;
        o_s2 <= 2'b00;

        if(i_forw_en == 1'b1) begin
            if(i_s_mem_en == 1'b1) begin
                if(i_mem_addr == i_s1) begin
                    o_s1 = 2'b10;
                end
                if(i_mem_addr == i_s2) begin
                    o_s2 = 2'b10; 
                end
            end

            if(i_s_wb_en == 1'b1) begin
                if(i_wb_addr == i_s1) begin
                    o_s1 = 2'b01;
                end
                if(i_wb_addr == i_s2) begin
                    o_s2 = 2'b01;
                end 
            end
        end
    end
endmodule

module processor
    #(parameter ARCH = 32,
      parameter RAM_SIZE = 1024 
    )(
    input wire clk, 
    input wire reset_n,
    output reg trap
);
    reg [ARCH-1:0] pc;
    reg [6:0] step;

    wire [3:0] w_de_nzcv = 4'b0000; // todo

    wire            w_fs_reset_n = reset_n;
    wire            w_fs_freeze;
    wire [ARCH-1:0] w_fs_pc;
    wire [ARCH-1:0] w_fs_ins;
    wire [ARCH-1:0] w_fs_br_addr;   // todo
    wire            w_fs_br;        // Branch taken (1 = branch)

    wire [ARCH-1:0] w_de_pc;
    wire            w_de_reset_n = reset_n;
    wire            w_de_freeze;
    wire            w_de_flush;
    wire [3:0]      w_de_rn;        // 1st operand (reg address)
    wire [3:0]      w_de_rm;        // 2nd operand (reg address)
    wire            w_de_f_c;       // Carry flag
    wire            w_de_ld_st;     // Load/Store bit (1 = load)
    wire            w_de_imm;       // Immediate bit (1 = immediate) for expanding 12-bit immediate to 32-bit
    wire [11:0]     w_de_imm12;     // 12-bit immediate
    wire [23:0]     w_de_imm24;     // 24-bit immediate (for branch)
    wire [3:0]      w_de_alu_c;     // ALU control
    wire [ARCH-1:0] w_de_v_rn;      // 1st operand (value)
    wire [ARCH-1:0] w_de_vs2;       // 2st operand (value)
    wire            w_de_mem_r_en;  // Memory read enable
    wire            w_de_mem_w_en;  // Memory write enable
    wire            w_de_wb_en;     // Write back enable
    wire [3:0]      w_de_wb_addr;   // write back address (Destination register)
    wire [ARCH-1:0] w_de_wb_data;   // write back data (value) read from memory
    wire            w_de_w_f_en;    // Write enable NZCV

    wire [ARCH-1:0] w_eu_pc;
    wire [3:0]      w_nzcv;
    wire [ARCH-1:0] w_v_rm;
    wire [ARCH-1:0] w_eu_alu_res;   // ALU result (value)
    wire [ARCH-1:0] w_eu_vs2;       // 2nd operand (value) : Rm
    wire            w_eu_mem_r_en;  // Memory read enable
    wire            w_eu_mem_w_en;  // Memory write enable
    wire            w_eu_wb_en;     // Write back enable
    wire [3:0]      w_eu_wb_addr;   // Write back address (Destination)

    wire [ARCH-1:0] w_m_pc;
    wire            w_m_mem_r_en;
    wire            w_m_mem_w_en;
    wire            w_m_wb_en;
    wire [3:0]      w_m_wb_addr;    // write back address
    wire [ARCH-1:0] w_m_wb_data;    // write back data (value) read from memory
    wire [ARCH-1:0] w_m_mem_res;    // ALU result  (value) : memory write 

    wire [ARCH-1:0] w_wb_pc;
    wire            w_wb_wb_en;
    wire [ARCH-1:0] w_wb_wb_data;
    wire [3:0]      w_wb_wb_addr;

    // *** (1) fetch ***
    fetching_stage fs (
        .clk(clk),
        .i_reset_n(w_fs_reset_n),
        .i_freeze(w_fs_freeze),
        .i_br(w_fs_br),
        .i_br_addr(w_fs_br_addr),
        .o_pc(w_fs_pc),
        .o_ins(w_fs_ins)
    );

    // *** (2) decode ***
    arm32_decoder dec (
        .clk(clk),
        .i_reset_n(w_de_reset_n),
        .i_pc(w_fs_pc),
        .i_ins(w_fs_ins),
        .i_nzcv(w_de_nzcv),
        .i_wb_en(w_wb_wb_en),
        .i_wb_data(w_wb_wb_data),
        .i_wb_addr(w_wb_wb_addr),
        .i_w_f_en(w_de_w_f_en),
        .o_pc(w_de_pc),
        .o_imm12(w_de_imm12),
        .o_imm24(w_de_imm24),
        .o_alu_c(w_de_alu_c),
        .o_vs1(w_de_v_rn),
        .o_vs2(w_de_vs2),
        .o_s1(w_de_rn),
        .o_s2(w_de_rm),
        .o_br(w_fs_br),
        .o_f_c(w_de_f_c),
        .o_imm(w_de_imm),
        .o_mem_r_en(w_de_mem_r_en),
        .o_mem_w_en(w_de_mem_w_en),
        .o_wb_en(w_de_wb_en),
        .o_wb_addr(w_de_wb_addr),
        .o_w_f_en(w_de_w_f_en)
    );

    wire [ARCH-1:0] w_des_pc;
    wire            w_des_reset_n = reset_n;
    wire [11:0]     w_des_imm12;
    wire [23:0]     w_des_imm24;
    wire [3:0]      w_des_alu_c;
    wire [ARCH-1:0] w_des_vs1;
    wire [ARCH-1:0] w_des_vs2;
    wire [3:0]      w_des_s1;
    wire [3:0]      w_des_s2;
    wire            w_des_ld_st;
    wire            w_des_br;
    wire            w_des_f_c;
    wire            w_des_imm;
    wire            w_des_mem_r_en;
    wire            w_des_mem_w_en;
    wire            w_des_wb_en;
    wire [3:0]      w_des_wb_addr;
    wire            w_des_w_f_en;

    decoder_stage des (
        .clk(clk),
        .i_reset_n(w_de_reset_n),
        .i_flush(w_de_flush),
        .i_freeze(w_de_freeze),
        .i_pc(w_de_pc),
        .i_mem_r_en(w_de_mem_r_en),
        .i_mem_w_en(w_de_mem_w_en),
        .i_imm(w_de_imm),
        .i_br(w_fs_br),
        .i_alu_c(w_de_alu_c),
        .i_vs1(w_de_v_rn),
        .i_vs2(w_de_vs2),
        .i_imm12(w_de_imm12),
        .i_imm24(w_de_imm24),
        .i_w_f_en(w_de_w_f_en),
        .i_wb_en(w_de_wb_en),
        .i_wb_addr(w_de_wb_addr),
        .i_ld_st(w_de_ld_st),
        .i_f_c(w_de_f_c),
        .i_s1(w_de_rn),
        .i_s2(w_de_rm),
        .o_pc(w_des_pc),
        .o_imm12(w_des_imm12),
        .o_imm24(w_des_imm24),
        .o_alu_c(w_des_alu_c),
        .o_vs1(w_des_vs1), 
        .o_vs2(w_des_vs2),
        .o_s1(w_des_s1), 
        .o_s2(w_des_s2),
        .o_ld_st(w_des_ld_st),
        .o_br(w_des_br),
        .o_f_c(w_des_f_c),
        .o_imm(w_des_imm),
        .o_mem_r_en(w_des_mem_r_en),
        .o_mem_w_en(w_des_mem_w_en),
        .o_wb_en(w_des_wb_en),
        .o_wb_addr(w_des_wb_addr),
        .o_w_f_en(w_des_w_f_en)
    );

    // *** (3) execute ***
    execution_unit eu (
        .clk(clk), 
        .i_pc(w_des_pc),
        .i_mem_r_en(w_des_mem_r_en),
        .i_mem_w_en(w_des_mem_w_en),
        .i_wb_en(w_des_wb_en),
        .i_wb_addr(w_des_wb_addr),
        .i_imm(w_des_imm),
        .i_f_c(w_des_f_c),
        .i_ops(w_des_alu_c),
        .i_imm12(w_des_imm12),
        .i_imm24(w_des_imm24),
        .i_v_rn(w_des_vs1),
        .i_v_rm(w_des_vs2),
        .o_pc(w_eu_pc),
        .o_mem_r_en(w_eu_mem_r_en),
        .o_mem_w_en(w_eu_mem_w_en),
        .o_wb_en(w_eu_wb_en),
        .o_wb_addr(w_eu_wb_addr),
        .o_alu_res(w_eu_alu_res),
        .o_vs2(w_eu_vs2),
        .o_alu_nzcv(w_nzcv),
        .o_br_addr(w_fs_br_addr)
    );

    wire            w_es_reset_n = reset_n;
    wire            w_es_flush;
    wire            w_es_freeze;

    wire [ARCH-1:0] w_es_pc;
    wire            w_es_mem_r_en;
    wire            w_es_mem_w_en;
    wire            w_es_wb_en;
    wire [3:0]      w_es_wb_addr;
    wire [ARCH-1:0] w_es_alu_res;
    wire [ARCH-1:0] w_es_vs2;

    execution_stage es (
        .clk(clk),
        .i_reset_n(w_es_reset_n),
        .i_flush(w_es_flush),
        .i_freeze(w_es_freeze),
        .i_pc(w_eu_pc),
        .i_mem_r_en(w_eu_mem_r_en),
        .i_mem_w_en(w_eu_mem_w_en),
        .i_wb_en(w_eu_wb_en),
        .i_wb_addr(w_eu_wb_addr),
        .i_alu_res(w_eu_alu_res),
        .i_vs2(w_eu_vs2),
        .o_pc(w_es_pc),
        .o_mem_r_en(w_es_mem_r_en),
        .o_mem_w_en(w_es_mem_w_en),
        .o_wb_en(w_es_wb_en),
        .o_wb_addr(w_es_wb_addr),
        .o_alu_res(w_es_alu_res),
        .o_vs2(w_es_vs2)
    );
    
    wire            w_rs_reset_n = reset_n;
    status_register rs (
        .clk(clk),
        .i_reset_n(w_rs_reset_n),
        .i_nzcv(w_nzcv),
        .i_mem(w_des_w_f_en),
        .o_nzcv(w_de_nzcv)
    );


    // *** (4) memory access ***
    memory_stage m (
        .clk(clk), 
        .i_reset_n(w_de_reset_n),
        .i_pc(w_es_pc),
        .i_mem_r_en(w_eu_mem_r_en),
        .i_mem_w_en(w_eu_mem_w_en),
        .i_wb_en(w_eu_wb_en),
        .i_wb_addr(w_eu_wb_addr),
        .i_alu_res(w_eu_alu_res),
        .i_vs2(w_eu_vs2),
        .o_pc(w_m_pc),
        .o_mem_r_en(w_m_mem_r_en),
        .o_mem_w_en(w_m_mem_w_en),
        .o_wb_en(w_m_wb_en),
        .o_wb_addr(w_m_wb_addr),       
        .o_wb_data(w_m_wb_data),
        .o_alu_res(w_m_mem_res)
    );

    // *** (5) write back ***
    wb_stage wb (
        .clk(clk), 
        .i_pc(w_m_pc),
        .i_mem_r_en(w_m_mem_r_en),
        .i_wb_en(w_m_wb_en),
        .i_alu_res(w_m_mem_res),
        .i_wb_data(w_m_wb_data),
        .i_wb_addr(w_m_wb_addr),
        .o_pc(w_wb_pc),
        .o_wb_en(w_wb_wb_en),
        .o_wb_data(w_wb_wb_data),
        .o_wb_addr(w_wb_wb_addr)
    );
endmodule
