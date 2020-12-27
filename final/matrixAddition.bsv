package matrixAddition;

import CBusMod::*;
import Vector :: *;	// Memory is defined as the vector

////////////////////////////////////////////////////////////////////////////////
///
//// Specific configuration bus size parameters
typedef 8 CBADDRSIZE; //size of configuration address bus to decode
typedef 16 CBDATASIZE; //size of configuration data bus 

typedef ModWithCBus#(CBADDRSIZE, CBDATASIZE, i) MyModWithCBus#(type i);
typedef CBus#(CBADDRSIZE, CBDATASIZE) MyCBus;

////////////////////////////////////////////////////////////////////////////////
///
// Defining all the register present in the accelerator here and there associated actions...

interface CSR1#(type size_t);		// control bit to interact between accelerator and CPU
   method Bool   isZero();
   method Action load(Bit#(size_t) newval);
endinterface

interface CSR2#(type size_t);		// control bit to interact between accelerator and CPU
   method Bool   isZero();
   method Action load(Bit#(size_t) newval);
endinterface

interface Src1Addr#(type size_t);	// control bits to interact between accelerator and CPU
   method Reg#(Bit#(size_t)) value();
   method Action increment();
   method Action load(Bit#(size_t) newval);     
endinterface

interface Src2Addr#(type size_t);	// control bits to interact between accelerator and CPU
   method Reg#(Bit#(size_t)) value();
   method Action increment();
   method Action load(Bit#(size_t) newval);    
endinterface

interface DstAddr#(type size_t);	// control bits to interact between accelerator and CPU
   method Reg#(Bit#(size_t)) value();
   method Action increment();
   method Action load(Bit#(size_t) newval);    
endinterface

// define the interface for src1rownum, src1colnum, src2rownum, src2colnum, dstrownum, dstcolnum ///////////

interface Src1Rows#(type size_t);	// interface to store the number of rows of the matrix scr1  
   method Reg#(Bit#(size_t)) value();
   method Action load(Bit#(size_t) newval);    
endinterface

interface Src2Rows#(type size_t);	// interface to store the number of rows of the matrix scr2
   method Reg#(Bit#(size_t)) value();
   method Action load(Bit#(size_t) newval);    
endinterface

interface DstRows#(type size_t);	// interface to store the number of rows of the matrix dst  
   method Reg#(Bit#(size_t)) value();
   method Action load(Bit#(size_t) newval);    
endinterface

interface Src1Cols#(type size_t);	// interface to store the number of cols of the matrix scr1  
   method Reg#(Bit#(size_t)) value();
   method Action load(Bit#(size_t) newval);    
endinterface

interface Src2Cols#(type size_t);	// interface to store the number of cols of the matrix scr2
   method Reg#(Bit#(size_t)) value();
   method Action load(Bit#(size_t) newval);    
endinterface

interface DstCols#(type size_t);	// interface to store the number of cols of the matrix dst  
   method Reg#(Bit#(size_t)) value();
   method Action load(Bit#(size_t) newval);    
endinterface
// defined all the regs to stores the rows and cols info

interface Control_reg1#(type size_t);		// controls the access to the memory
   method Bool   isZero();
   method Action load(Bit#(size_t) newval);     
endinterface

interface Control_reg2#(type size_t);		// controls the access to the memory
   method Bool   isZero();   
   method Action load(Bit#(size_t) newval);     
endinterface

interface MAR1#(type size_t);			// memory address register1..
   method Reg#(Bit#(size_t)) value();
   method Action load(Bit#(size_t) newval);    
endinterface

interface MBR1#(type size_t);			// memory buffer register1..
   method Reg#(Bit#(size_t)) value();
   method Action load(Bit#(size_t) newval);    
endinterface

// define one more register attached to the acc that will store the blocksize
interface BLKSIZE#(type size_t);			// stores the block-size
   method Reg#(Bit#(size_t)) value();
   method Action load(Bit#(size_t) newval);    
endinterface

// defining interface for the DMemory
interface DMem;
method (Bit#(CBDATASIZE)) load (Bit#(CBDATASIZE) mar);
method Action store (Bit#(CBDATASIZE) mar, Bit#(CBDATASIZE) mbr);
endinterface

// module for the DMem
module mkDMem(DMem);
	
	Vector #( 256, Reg#(Bit#(CBDATASIZE)) ) memory_array  <- replicateM (mkReg(0));
	Reg#(Bit#(CBDATASIZE)) r <- mkReg(0);

	method (Bit#(CBDATASIZE)) load (Bit#(CBDATASIZE) mar);
		return memory_array[mar];
	endmethod

	method Action store (Bit#(CBDATASIZE) mar, Bit#(CBDATASIZE) mbr);
		memory_array[mar] <= mbr;
	endmethod

endmodule

// DMem defination end..

// defining a CPU interface
interface CPU;
method (Bit#(CBDATASIZE)) returnflag();
method (Bit#(CBDATASIZE)) returnsrc1addr();
method (Bit#(CBDATASIZE)) returnsrc2addr();
method (Bit#(CBDATASIZE)) returndstaddr();

method (Bit#(CBDATASIZE)) returnsrc1rows();
method (Bit#(CBDATASIZE)) returnsrc1cols();
method (Bit#(CBDATASIZE)) returnsrc2rows();
method (Bit#(CBDATASIZE)) returnsrc2cols();

method Action putsrc1addr (Bit#(CBDATASIZE) s1);
method Action putsrc2addr (Bit#(CBDATASIZE) s2);
method Action putdstaddr (Bit#(CBDATASIZE) d);

method Action puts1rows (Bit#(CBDATASIZE) sr1);
method Action puts1cols (Bit#(CBDATASIZE) sc1);
method Action puts2rows (Bit#(CBDATASIZE) sr2);
method Action puts2cols (Bit#(CBDATASIZE) sc2);

endinterface

// Declare and Define the methods to return the rows and cols for scr1,2. remove the method to return the block-size (not needed now) /////////// 
// module for CPU..
module mkCPU(CPU);
	Reg#(Bit#(CBDATASIZE)) flag <- mkReg(1);
	Reg#(Bit#(CBDATASIZE)) src1 <- mkReg(0);
	Reg#(Bit#(CBDATASIZE)) src2 <- mkReg(0);
	Reg#(Bit#(CBDATASIZE)) dst <- mkReg(0);

	Reg#(Bit#(CBDATASIZE)) r1 <- mkReg(0);
	Reg#(Bit#(CBDATASIZE)) c1 <- mkReg(0);
	Reg#(Bit#(CBDATASIZE)) r2 <- mkReg(0);
	Reg#(Bit#(CBDATASIZE)) c2 <- mkReg(0);

	method (Bit#(CBDATASIZE)) returnsrc1rows();
		return r1;
	endmethod

	method (Bit#(CBDATASIZE)) returnsrc1cols();
		return c1;
	endmethod

	method (Bit#(CBDATASIZE)) returnsrc2rows();
		return r2;
	endmethod

	method (Bit#(CBDATASIZE)) returnsrc2cols();
		return c2;
	endmethod

	method (Bit#(CBDATASIZE)) returnflag();
		return flag;
	endmethod

	method (Bit#(CBDATASIZE)) returnsrc1addr();
		return src1;
	endmethod

	method (Bit#(CBDATASIZE)) returnsrc2addr();
		return src2;
	endmethod
	
	method (Bit#(CBDATASIZE)) returndstaddr();
		return dst;
	endmethod

	method Action putsrc1addr (Bit#(CBDATASIZE) s1);
		src1 <= s1;
	endmethod

	method Action putsrc2addr (Bit#(CBDATASIZE) s2);
		src2 <= s2;
	endmethod

	method Action putdstaddr (Bit#(CBDATASIZE) d);
		dst <= d;
	endmethod

	method Action puts1rows (Bit#(CBDATASIZE) sr1);
		r1 <= sr1;
	endmethod

	method Action puts1cols (Bit#(CBDATASIZE) sc1);
		c1 <= sc1;
	endmethod	

	method Action puts2rows (Bit#(CBDATASIZE) sr2);
		r2 <= sr2;
	endmethod

	method Action puts2cols (Bit#(CBDATASIZE) sc2);
		c2 <= sc2;
	endmethod

endmodule

// CPU interface ends here..

////////////////////////////////////////////////////////////////////////////////
///
// Exposing the accelerator's register to the CBus to they can be access by the CPU and MEMORY...

(* synthesize *)
module mkCSR1Synth(IWithCBus#(MyCBus, CSR1#(CBDATASIZE)));
   let ifc();
   exposeCBusIFC#(mkCSR1) _temp(ifc);
   return (ifc);
endmodule

module [MyModWithCBus] mkCSR1(CSR1#(size_t))
   provisos(Add#(size_t, k, CBDATASIZE));
    
   Reg#(Bit#(size_t)) csr1 <- mkCBRegRW(10, 0);
   method Bool isZero();
      return (csr1 == 0);
   endmethod
   
   method Action load(Bit#(size_t) newval);
      csr1 <= newval;
   endmethod

endmodule

(* synthesize *)
module mkCSR2Synth(IWithCBus#(MyCBus, CSR2#(CBDATASIZE)));
   let ifc1();
   exposeCBusIFC#(mkCSR2) _temp(ifc1);
   return (ifc1);
endmodule

module [MyModWithCBus] mkCSR2(CSR2#(size_t))
   provisos(Add#(size_t, k1, CBDATASIZE));
    
   Reg#(Bit#(size_t)) csr2 <- mkCBRegRW(11, 0);
   method Bool isZero();
      return (csr2 == 0);
   endmethod
   
   method Action load(Bit#(size_t) newval);
      csr2 <= newval;
   endmethod

endmodule

(* synthesize *)
module mkSrc1AddrSynth(IWithCBus#(MyCBus, Src1Addr#(CBDATASIZE)));
   let ifc2();
   exposeCBusIFC#(mkSrc1Addr) _temp(ifc2);
   return (ifc2);
endmodule

module [MyModWithCBus] mkSrc1Addr(Src1Addr#(size_t))
   provisos(Add#(size_t, k3, CBDATASIZE));
    
   Reg#(Bit#(size_t)) src1 <- mkCBRegRW(12, 0);
   method Reg#(Bit#(size_t)) value();
      return src1;
   endmethod

    method Action increment();
      src1 <= src1 + 1;
   endmethod
   
   method Action load(Bit#(size_t) newval);
      src1 <= newval;
   endmethod

endmodule

(* synthesize *)
module mkSrc2AddrSynth(IWithCBus#(MyCBus, Src2Addr#(CBDATASIZE)));
   let ifc3();
   exposeCBusIFC#(mkSrc2Addr) _temp(ifc3);
   return (ifc3);
endmodule

module [MyModWithCBus] mkSrc2Addr(Src2Addr#(size_t))
   provisos(Add#(size_t, k4, CBDATASIZE));
    
   Reg#(Bit#(size_t)) src2 <- mkCBRegRW(13, 0);
   method Reg#(Bit#(size_t)) value();
      return src2;
   endmethod

    method Action increment();
      src2 <= src2 + 1;
   endmethod
   
   method Action load(Bit#(size_t) newval);
      src2 <= newval;
   endmethod

endmodule

(* synthesize *)
module mkDstAddrSynth(IWithCBus#(MyCBus, DstAddr#(CBDATASIZE)));
   let ifc4();
   exposeCBusIFC#(mkDstAddr) _temp(ifc4);
   return (ifc4);
endmodule

module [MyModWithCBus] mkDstAddr(DstAddr#(size_t))
   provisos(Add#(size_t, k5, CBDATASIZE));
    
   Reg#(Bit#(size_t)) dst <- mkCBRegRW(14, 0);
   method Reg#(Bit#(size_t)) value();
      return dst;
   endmethod

    method Action increment();
      dst <= dst + 1;
   endmethod
   
   method Action load(Bit#(size_t) newval);
      dst <= newval;
   endmethod

endmodule

(* synthesize *)
module mkControlReg1Synth( IWithCBus#(MyCBus, Control_reg1#(CBDATASIZE)) );
   let ifc5();
   exposeCBusIFC#(mkControlReg1) _temp(ifc5);
   return (ifc5);
endmodule

module [MyModWithCBus] mkControlReg1(Control_reg1#(size_t))
   provisos(Add#(size_t, k6, CBDATASIZE));
    
   Reg#(Bit#(size_t)) cr1 <- mkCBRegRW(15, 0);
   method Bool isZero();
      return (cr1 == 0);
   endmethod   

   method Action load(Bit#(size_t) newval);
      cr1 <= newval;
   endmethod

endmodule

(* synthesize *)
module mkControlReg2Synth( IWithCBus#(MyCBus, Control_reg2#(CBDATASIZE)) );
   let ifc6();
   exposeCBusIFC#(mkControlReg2) _temp(ifc6);
   return (ifc6);
endmodule

module [MyModWithCBus] mkControlReg2(Control_reg2#(size_t))
   provisos(Add#(size_t, k7, CBDATASIZE));
    
   Reg#(Bit#(size_t)) cr2 <- mkCBRegRW(16, 0);
   method Bool isZero();
      return (cr2 == 0);
   endmethod   
   
   method Action load(Bit#(size_t) newval);
      cr2 <= newval;
   endmethod

endmodule

//MAR1
(* synthesize *)
module mkMAR1Synth( IWithCBus#(MyCBus, MAR1#(CBDATASIZE)) );
   let ifc7();
   exposeCBusIFC#(mkMAR1) _temp(ifc7);
   return (ifc7);
endmodule

module [MyModWithCBus] mkMAR1(MAR1#(size_t))
   provisos(Add#(size_t, k8, CBDATASIZE));
    
   Reg#(Bit#(size_t)) mar <- mkCBRegRW(17, 0);   
   method Reg#(Bit#(size_t)) value();
      return mar;
   endmethod
   method Action load(Bit#(size_t) newval);
      mar <= newval;
   endmethod

endmodule

//MBR1
(* synthesize *)
module mkMBR1Synth( IWithCBus#(MyCBus, MBR1#(CBDATASIZE)) );
   let ifc8();
   exposeCBusIFC#(mkMBR1) _temp(ifc8);
   return (ifc8);
endmodule

module [MyModWithCBus] mkMBR1(MBR1#(size_t))
   provisos(Add#(size_t, k9, CBDATASIZE));
    
   Reg#(Bit#(size_t)) mbr <- mkCBRegRW(18, 0);   
   method Reg#(Bit#(size_t)) value();
      return mbr;
   endmethod
   method Action load(Bit#(size_t) newval);
      mbr <= newval;
   endmethod

endmodule

// define module for one more register attached to the acc that will store the blocksize

//Blksize
(* synthesize *)
module mkBLKSIZESynth( IWithCBus#(MyCBus, BLKSIZE#(CBDATASIZE)) );
   let ifc9();
   exposeCBusIFC#(mkBLKSIZE) _temp(ifc9);
   return (ifc9);
endmodule

module [MyModWithCBus] mkBLKSIZE(BLKSIZE#(size_t))
   provisos(Add#(size_t, k10, CBDATASIZE));
    
   Reg#(Bit#(size_t)) bsize <- mkCBRegRW(19, 0);   
   method Reg#(Bit#(size_t)) value();
      return bsize;
   endmethod
   method Action load(Bit#(size_t) newval);
      bsize <= newval;
   endmethod

endmodule

// expose the src1rownum, src1colnum, src2rownum, src2colnum, dstrownum, dstcolnum to cbus///////////
(* synthesize *)
module mkSrc1RowsSynth(IWithCBus#(MyCBus, Src1Rows#(CBDATASIZE)));
   let ifc();
   exposeCBusIFC#(mkSrc1Rows) _temp(ifc);
   return (ifc);
endmodule

module [MyModWithCBus] mkSrc1Rows(Src1Rows#(size_t))
   provisos(Add#(size_t, k, CBDATASIZE));
    
   Reg#(Bit#(size_t)) r1 <- mkCBRegRW(20, 0);
   method Reg#(Bit#(size_t)) value();
      return r1;
   endmethod
   
   method Action load(Bit#(size_t) newval);
      r1 <= newval;
   endmethod

endmodule

(* synthesize *)
module mkSrc1ColsSynth(IWithCBus#(MyCBus, Src1Cols#(CBDATASIZE)));
   let ifc();
   exposeCBusIFC#(mkSrc1Cols) _temp(ifc);
   return (ifc);
endmodule

module [MyModWithCBus] mkSrc1Cols(Src1Cols#(size_t))
   provisos(Add#(size_t, k, CBDATASIZE));
    
   Reg#(Bit#(size_t)) c1 <- mkCBRegRW(21, 0);
   method Reg#(Bit#(size_t)) value();
      return c1;
   endmethod
   
   method Action load(Bit#(size_t) newval);
      c1 <= newval;
   endmethod

endmodule

//Src2Rows Src2Cols

(* synthesize *)
module mkSrc2RowsSynth(IWithCBus#(MyCBus, Src2Rows#(CBDATASIZE)));
   let ifc();
   exposeCBusIFC#(mkSrc2Rows) _temp(ifc);
   return (ifc);
endmodule

module [MyModWithCBus] mkSrc2Rows(Src2Rows#(size_t))
   provisos(Add#(size_t, k, CBDATASIZE));
    
   Reg#(Bit#(size_t)) r2 <- mkCBRegRW(22, 0);
   method Reg#(Bit#(size_t)) value();
      return r2;
   endmethod
   
   method Action load(Bit#(size_t) newval);
      r2 <= newval;
   endmethod

endmodule

(* synthesize *)
module mkSrc2ColsSynth(IWithCBus#(MyCBus, Src2Cols#(CBDATASIZE)));
   let ifc();
   exposeCBusIFC#(mkSrc2Cols) _temp(ifc);
   return (ifc);
endmodule

module [MyModWithCBus] mkSrc2Cols(Src2Cols#(size_t))
   provisos(Add#(size_t, k, CBDATASIZE));
    
   Reg#(Bit#(size_t)) c2 <- mkCBRegRW(23, 0);
   method Reg#(Bit#(size_t)) value();
      return c2;
   endmethod
   
   method Action load(Bit#(size_t) newval);
      c2 <= newval;
   endmethod

endmodule

// DstRows DstCols

(* synthesize *)
module mkDstRowsSynth(IWithCBus#(MyCBus, DstRows#(CBDATASIZE)));
   let ifc();
   exposeCBusIFC#(mkDstRows) _temp(ifc);
   return (ifc);
endmodule

module [MyModWithCBus] mkDstRows(DstRows#(size_t))
   provisos(Add#(size_t, k, CBDATASIZE));
    
   Reg#(Bit#(size_t)) r3 <- mkCBRegRW(24, 0);
   method Reg#(Bit#(size_t)) value();
      return r3;
   endmethod
   
   method Action load(Bit#(size_t) newval);
      r3 <= newval;
   endmethod

endmodule

(* synthesize *)
module mkDstColsSynth(IWithCBus#(MyCBus, DstCols#(CBDATASIZE)));
   let ifc();
   exposeCBusIFC#(mkDstCols) _temp(ifc);
   return (ifc);
endmodule

module [MyModWithCBus] mkDstCols(DstCols#(size_t))
   provisos(Add#(size_t, k, CBDATASIZE));
    
   Reg#(Bit#(size_t)) c3 <- mkCBRegRW(25, 0);
   method Reg#(Bit#(size_t)) value();
      return c3;
   endmethod
   
   method Action load(Bit#(size_t) newval);
      c3 <= newval;
   endmethod

endmodule

// end of exposing the register to CBus 

////////////////////////////////////////////////////////////////////////////////
///
// Simple test-bench code...

interface MatrixAdder_ifc;

	method Action put_src1 (Bit#(CBDATASIZE) src1);
	method Action put_src2 (Bit#(CBDATASIZE) src2);
	method Action put_dst (Bit#(CBDATASIZE) dst);
	method Action put_s1rows (Bit#(CBDATASIZE) sr1);
	method Action put_s1cols (Bit#(CBDATASIZE) sc1);
	method Action put_s2rows (Bit#(CBDATASIZE) sr2);
	method Action put_s2cols (Bit#(CBDATASIZE) sc2);

	method (Bit#(CBDATASIZE)) mem_load ( Bit#(CBDATASIZE) addr );
	method Action mem_store ( Bit#(CBDATASIZE) addr , Bit#(CBDATASIZE) val );
	method (Bool)  iscomplete();

endinterface: MatrixAdder_ifc

(* synthesize *)
module mkMatrixAdder(MatrixAdder_ifc);

	// indicates if all the operation are finished and should we end the simulation
	// specific register declaration

   Reg#(Bool) cpufinishinit <- mkReg(False);
   Reg#(Bool) memorysignalsent <- mkReg(False);

   Reg#(Bool) loadread1addr <- mkReg(False);
   Reg#(Bool) doneread1 <- mkReg(False);

   Reg#(Bool) loadread2addr <- mkReg(False);
   Reg#(Bool) doneread2 <- mkReg(False);

   Reg#(Bool) doneadd <- mkReg(False);
   Reg#(Bool) donesim <- mkReg(False);

	// will need to check if there is size missmatch between src1 and src2 
	Reg#(Bool) mismatch <- mkReg(False);
	
	// define one more register here that will count the number of elements that are read, added and written to the memory
	Reg#(Bit#(CBDATASIZE)) countelements <- mkReg(0);

	// bool register to check if the values are initialized by testbench or not..
	Reg#(Bool) got_src1 <- mkReg(False);
   	Reg#(Bool) got_src2 <- mkReg(False);
	Reg#(Bool) got_dst <- mkReg(False);

   	Reg#(Bool) got_s1r <- mkReg(False);
	Reg#(Bool) got_s1c <- mkReg(False);
	Reg#(Bool) got_s2r <- mkReg(False);
	Reg#(Bool) got_s2c <- mkReg(False);	

	Reg#(Bool) isfinish <- mkReg(False);

   Reg#(Bit#(CBDATASIZE)) op1 <- mkReg(0);


	// instaintate the memory module..
	DMem memory <- mkDMem;

	// instiantiate the CPU module..
	CPU cpu <- mkCPU;

   let csr1_ifc();
   mkCSR1Synth add_csr1(csr1_ifc);

   let csr2_ifc1();
   mkCSR2Synth add_csr2(csr2_ifc1);

	let src1_ifc2();
   mkSrc1AddrSynth add_src1(src1_ifc2);

	let src2_ifc3();
   mkSrc2AddrSynth add_src2(src2_ifc3);

	let dst_ifc4();
   mkDstAddrSynth add_dst(dst_ifc4);

	let control_reg1();
   mkControlReg1Synth add_cr1(control_reg1);

	let control_reg2();
   mkControlReg2Synth add_cr2(control_reg2);

	let mar();
   mkMAR1Synth add_mar(mar);

	let mbr();
   mkMBR1Synth add_mbr(mbr);

	// instiantiate the src1rownum, src1colnum, src2rownum, src2colnum, dstrownum, dstcolnum///////////
	let src1rows();
   	mkSrc1RowsSynth add_src1rows(src1rows);

	let src1cols();
   	mkSrc1ColsSynth add_src1cols(src1cols);

	let src2rows();
   	mkSrc2RowsSynth add_src2rows(src2rows);

	let src2cols();
   	mkSrc2ColsSynth add_src2cols(src2cols);

	let dstrows();
   	mkDstRowsSynth add_dstrows(dstrows);

	let dstcols();
   	mkDstColsSynth add_dstcols(dstcols);


	// instiantiate the register which store the blocksize here
	let  bsize();
   mkBLKSIZESynth add_bsize(bsize);
   
	rule display_value (True);
      let read_value = fromMaybe(0, csr1_ifc.cbus_ifc.read(10));
      $display ("Current Value of CSR1 %2d at time:", read_value, $time);

	let read_value1 = fromMaybe(0, csr2_ifc1.cbus_ifc.read(11));
      $display ("Current Value of CSR2 %2d at time:", read_value1, $time);

	let read_value2 = fromMaybe(0, src1_ifc2.cbus_ifc.read(12));
      $display ("Current address at source1 %2d at time:", read_value2, $time);

	let read_value3 = fromMaybe(0, src2_ifc3.cbus_ifc.read(13));
      $display ("Current address at source2 %2d at time:", read_value3, $time);

	let read_value4 = fromMaybe(0, dst_ifc4.cbus_ifc.read(14));
      $display ("Current address at dest %2d at time:", read_value4, $time);

	let read_value5 = fromMaybe(0, control_reg1.cbus_ifc.read(15));
      $display ("Current address at cr1 %2d at time:", read_value5, $time);

	let read_value6 = fromMaybe(0, control_reg2.cbus_ifc.read(16));
      $display ("Current address at cr2 %2d at time:", read_value6, $time);

	let read_value7 = fromMaybe(0, mar.cbus_ifc.read(17));
      $display ("Current address at MAR %2d at time:", read_value7, $time);

	let read_value8 = fromMaybe(0, mbr.cbus_ifc.read(18));
      $display ("Current value at MBR %2d at time:", read_value8, $time);

	// print the blocksize here for the reference
	let read_value9 = fromMaybe(0, bsize.cbus_ifc.read(19));
      $display ("Block Size is  %2d at time:", read_value9, $time);

	let read_value10 = fromMaybe(0, src1rows.cbus_ifc.read(20));
      $display ("number of rows of src1 is  %2d at time:", read_value10, $time);

	let read_value11 = fromMaybe(0, src1cols.cbus_ifc.read(21));
      $display ("number of cols of src1 is  %2d at time:", read_value11, $time);
	
	// load the proper values in local vars ///////////
	// print the matrix
	let x = memory.load( 110 );		
	let y = memory.load( 111 );		
	let z = memory.load( 112 );	
	let w = memory.load( 113 );
	
	Int#(CBDATASIZE) x1 = unpack(x);
	Int#(CBDATASIZE) y1 = unpack(y);
	Int#(CBDATASIZE) z1 = unpack(z);
	Int#(CBDATASIZE) w1 = unpack(w);

	// will need to display matix ///////////
	$display ("The final matrix %2d at time is:",$time);
	$display ("[ %d %d \n",x1 ,y1 );	
	$display ("  %d %d ]",z1 ,w1 );

	$display("\n\n");

   endrule

   
	// step1 accelerator shows it readiness
   	rule init_csr2_via_acc (csr2_ifc1.device_ifc.isZero());
   	   csr2_ifc1.device_ifc.load(1);		// Device is telling to cpu that it is ready for the data transfer
   	endrule

	// step2 CPU initializes the registers of the accelerator. 
   	rule init_csr1_reg_via_cpu (!csr2_ifc1.device_ifc.isZero() && !cpufinishinit );
      		csr1_ifc.cbus_ifc.write( 10,cpu.returnflag() );			// CPU is telling that it has initilaized the addresses in src1, scr2, dst by setting csr1 as 1.
		 src1_ifc2.cbus_ifc.write( 12,cpu.returnsrc1addr() );		// CPU initilaized the address in src1
		src2_ifc3.cbus_ifc.write( 13,cpu.returnsrc2addr() );		// CPU initilaized the address in scr2
		dst_ifc4.cbus_ifc.write( 14,cpu.returndstaddr() );		// CPU initilaized the address in dst

	// CPU will init src1rownum, src1colnum, src2rownum, src2colnum. remove the init part of bsize(not needed now)///////////	
		src1rows.cbus_ifc.write( 20,cpu.returnsrc1rows() );
		src1cols.cbus_ifc.write( 21,cpu.returnsrc1cols() );

		src2rows.cbus_ifc.write( 22,cpu.returnsrc2rows() );
		src2cols.cbus_ifc.write( 23,cpu.returnsrc2cols() );

		cpufinishinit <= True;
  	endrule

	// add one function to check if rows and cols of src1 and src2 are matching. If yes then set bsize=rows*cols and execute rest of the rule and set SUCCESS bit on sim finish///////////
	// else set the MISMATCH bit and finish the sim immediately after it. ///////////

	// step3 Memory shows its readiness by making cr1 as 1. Accelerator will check the compatibility of matrices for addition and will init the bsize. 
	rule memory_read_ready ( !csr1_ifc.device_ifc.isZero() && !memorysignalsent );
		control_reg1.cbus_ifc.write(15,1);		// Memory will make the memory enable as true..
		
		// function that checks if rows and cols of src1 and src2 are matching and will set the	mismatch flag	
		if(src1rows.device_ifc.value() != src2rows.device_ifc.value() || src1cols.device_ifc.value() != src2cols.device_ifc.value() ) 	// Accelarator will check for mismatch
			mismatch <= True;		

		let x = src1rows.device_ifc.value() * src1cols.device_ifc.value(); 	// Accelarator init the bsize value from rows and cols size.
		bsize.device_ifc.load(x);		

		memorysignalsent <= True;
	endrule


	// step4 if the Memory is ready for the operation(i.e. cr1 == 1), then Accelerator loads the address to be read from memory in the MAR
	rule read_op1_from_memory ( !control_reg1.device_ifc.isZero() && !loadread1addr );

		// preprocessing
		mar.device_ifc.load( src1_ifc2.device_ifc.value() );	// accelarator will read the value of scr1 and will write into the MAR 
		control_reg2.device_ifc.load(0);			// accelarator will make cr2 as 0 indicates that it is doing read operation from memory..

		loadread1addr <= True;				
   	endrule

	// do read operation from src1 address..
	rule readsrc1( !control_reg1.device_ifc.isZero() && !doneread1 && loadread1addr );

		// invoke memory module's read
		mbr.cbus_ifc.write(18, memory.load( fromMaybe(0, mar.cbus_ifc.read(17) ) ) ); // Memory will read the value of mar do the load op from addr [mar] and then write to mbr		
		
		// display the value at mem loc [mar]
		let z = memory.load( mar.device_ifc.value() );		// just for the display purpose					
		doneread1 <= True;		// finish read1	
	endrule


	// step5 if doneread1 and Memory is ready for the operation(i.e. cr1 == 1), then Accelerator loads the address to be read from memory in the MAR do read operation from memory
	rule read_op2_from_memory ( !control_reg1.device_ifc.isZero() && doneread1 && !loadread2addr );	

		// preprocessing
		op1 <= mbr.device_ifc.value();				// accelerator will store the read value from mem loc [mar] in its previous read op to the register op1

		mar.device_ifc.load(src2_ifc3.device_ifc.value());	// accelarator will read the value of scr2 and will write into the MAR 
		control_reg2.device_ifc.load(0);			// accelarator will make cr2 as 0 indicates that it is doing read operation from memory..
		loadread2addr <= True;	
	endrule
	
	// do read operattion from src2 address..
	rule readsrc2(!control_reg1.device_ifc.isZero() && doneread1 && loadread2addr && !doneread2 );
		
		// invoke memory module's read	
		mbr.cbus_ifc.write(18, memory.load( fromMaybe(0, mar.cbus_ifc.read(17) ) ) );	// Memory will read the value of mar do the load op from addr [mar] and then write to mbr
		
		// display the value at mem loc [mar]
		let z = memory.load(mar.device_ifc.value());			
		doneread2 <= True;			// finish read2
	endrule


	//step6 if doneread1 and doneread2, then do the addition operation and store it in op1. Write the op1 to memory.
	rule do_addition (!control_reg1.device_ifc.isZero() && doneread2 && doneread1 && !doneadd );
		// do the addition
		mbr.device_ifc.load( op1 + mbr.device_ifc.value() );			// accelarator takes value store in op1 and mbr and adds them store back to mbr
		//load dst to mar than write results stored in op1 to memory..
		mar.device_ifc.load(dst_ifc4.device_ifc.value());			// accelarator will read the value of dst and will write into the MAR 
		control_reg2.device_ifc.load(1);					// accelarator will make cr2 as 1 indicates that it is doing write operation to the memory..
		doneadd <= True;
	endrule
	
	rule writeresults(!control_reg1.device_ifc.isZero() && doneread2 && doneread1 && doneadd && !donesim );
	    // write to the mem loc given by [mar] value mbr
	    memory.store( fromMaybe(0, mar.cbus_ifc.read(17) ), fromMaybe(0, mbr.cbus_ifc.read(18) ) ); // Memory will read the value of mar and mbr from cbus and will write mbr value at the addr [mar]
	    countelements <= countelements + 1;		// increment the number of elements processed
	    
	    donesim <= True; 			//finish the simulation if all elements are processed.
	endrule



	//step7 if donesim is set do the bookkeeping work.
	rule donestep (donesim);
	
		// make all the 7 registers defined in this module as False
   		loadread1addr <= False;
   		doneread1 <= False;

	   	loadread2addr <= False;
   		doneread2 <= False;

   		doneadd <= False;
   		donesim <= False;

		src1_ifc2.device_ifc.increment();		// accelerator increments the src1 address
		src2_ifc3.device_ifc.increment();		// accelerator increments the src2 address
		dst_ifc4.device_ifc.increment();		// accelerator increments the dst address
		
		// just for the display purpose	
		let z = memory.load( mar.device_ifc.value() );			// load the value at mem loc [mar] for display

		// check if done by comparing the register value which counts(defined in this module) number of cycles with the register which stores the blocksize
		// if both matches then end the sim.	
		if( countelements == bsize.device_ifc.value() )			// accelerator checks if number of elements process is equal to the block size
		begin			
			$display ("\n\nRETURN SUCCESS: Operation successful");
			isfinish <= True;			// addition is finished
			//$finish();
		end
   	endrule

	rule ismismatch(mismatch);
		$display ("\n\nRETURN FAIL: Matrix size check failed");
		isfinish <= True;			// addition is finished
		//$finish();
	endrule

	// method that will set the src1,scr2,dst and block-size and set the flags

	method Action put_src1 (Bit#(CBDATASIZE) src1) if (! got_src1);
		cpu.putsrc1addr(src1);
		got_src1 <= True;
	endmethod

	method Action put_src2 (Bit#(CBDATASIZE) src2) if (! got_src2);
		cpu.putsrc2addr(src2);
		got_src2 <= True;
	endmethod

	method Action put_dst (Bit#(CBDATASIZE) dst) if (! got_dst);
		cpu.putdstaddr(dst);
		got_dst <= True;
	endmethod

	method Action put_s1rows (Bit#(CBDATASIZE) sr1) if(!got_s1r);
		cpu.puts1rows(sr1);
		got_s1r <= True;
	endmethod
	
	method Action put_s1cols (Bit#(CBDATASIZE) sc1) if(!got_s1c);
		cpu.puts1cols(sc1);
		got_s1c <= True;
	endmethod

	method Action put_s2rows (Bit#(CBDATASIZE) sr2) if(!got_s2r);
		cpu.puts2rows(sr2);
		got_s2r <= True;
	endmethod

	method Action put_s2cols (Bit#(CBDATASIZE) sc2) if(!got_s2c);
		cpu.puts2cols(sc2);
		got_s2c <= True;
	endmethod

	method (Bit#(CBDATASIZE)) mem_load ( Bit#(CBDATASIZE) addr );
		 return memory.load(addr);
	endmethod
	
	method Action mem_store ( Bit#(CBDATASIZE) addr , Bit#(CBDATASIZE) val );
   		memory.store(addr,val);
	endmethod

	method (Bool) iscomplete();
		return isfinish;
	endmethod
   
endmodule

////////////////////////////////////////////////////////////////////////////////

endpackage

