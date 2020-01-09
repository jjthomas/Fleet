package edu.stanford.fleet

import chisel3._
import chisel3.core.Bundle

class DualPortBRAM(dataWidth: Int, addrWidth: Int)  extends Module /* extends BlackBox(Map("DATA" -> IntParam(dataWidth),
                                                                        "ADDR" -> IntParam(addrWidth))) */ {
  val io = IO(new Bundle {
    val a_addr = Input(UInt(addrWidth.W))
    val a_din = Input(UInt(dataWidth.W))
    val a_wr = Input(Bool())
    val a_dout = Output(UInt(dataWidth.W))
    val b_addr = Input(UInt(addrWidth.W))
    val b_din = Input(UInt(dataWidth.W))
    val b_wr = Input(Bool())
    val b_dout = Output(UInt(dataWidth.W))
  })

  // simulation model for BRAM
  // there's no guarantee about what happens on
  // collisions (sim access to same address with two memory ports)
  val mem = Mem(1 << addrWidth, UInt(dataWidth.W)) // problem with SyncReadMem is that FIRRTL sim
  // doesn't know that there is a register between read port and output so it errors on combinational cycle

  val regAddrA = RegNext(io.a_addr) // eliminate this if SyncReadMem
  io.a_dout := mem.read(regAddrA)
  when (io.a_wr) {
    mem.write(io.a_addr, io.a_din)
  }

  val regAddrB = RegNext(io.b_addr) // eliminate this if SyncReadMem
  io.b_dout := mem.read(regAddrB)
  when (io.b_wr) {
    mem.write(io.b_addr, io.b_din)
  }
}