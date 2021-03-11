------------------------------------------------------------------------------
--                                                                          --
--                  GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--                   A D A . I N T E R R U P T S . N A M E S                --
--                                                                          --
--                                  S p e c                                 --
--                                                                          --
--          Copyright (C) 2020-2021, Free Software Foundation, Inc.         --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.                                     --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
-- GNARL was developed by the GNARL team at Florida State University.       --
-- Extensive contributions were provided by Ada Core Technologies, Inc.     --
--                                                                          --
------------------------------------------------------------------------------

--  This package version is specific to x86-64

pragma Restrictions (No_Elaboration_Code);

package Ada.Interrupts.Names is

   --  All identifiers in this unit are implementation defined

   pragma Implementation_Defined;

   --  x86-64 uses a split interrupt controller design. Each CPU has it's own
   --  Local Advance Programmable Interrupt Controller (APIC) that can receive
   --  external interrupts from I/O APICs, or PCI devices via the Message
   --  Signaled Interrupts (MSI) feature.

   --  This runtime only supports attaching interrupt handlers directly to
   --  Local APIC interrupt vectors. To attach a handler to an I/O APIC IRQ
   --  route the IRQ to a Local APIC Vector using the routines provided in
   --  Interfaces.X86_64.IO_APIC and attach the protected handler to the
   --  Vector.

   --  For the Local APIC the interrupt vector number encodes the priority of
   --  the interrupt. There are 16 interrupt priority classes, each containing
   --  16 interrupt vectors. The interrupt priority of the protected object
   --  containing the protected handler must match the priority class of the
   --  interrupt. The first two interrupt priority classes are not available
   --  for application use as they map to CPU exceptions.

   --  Note: Interrupt vectors used for the runtime are commented and marked as
   --  such.

   Vector_32_47_Interrupt_Priority : constant System.Interrupt_Priority := 242;
   --  Vector_32 -> Runtime: Spurious interrupt
   Vector_33  : constant Interrupt_ID := 33;
   Vector_34  : constant Interrupt_ID := 34;
   Vector_35  : constant Interrupt_ID := 35;
   Vector_36  : constant Interrupt_ID := 36;
   Vector_37  : constant Interrupt_ID := 37;
   Vector_38  : constant Interrupt_ID := 38;
   Vector_39  : constant Interrupt_ID := 39;
   Vector_40  : constant Interrupt_ID := 40;
   Vector_41  : constant Interrupt_ID := 41;
   Vector_42  : constant Interrupt_ID := 42;
   Vector_43  : constant Interrupt_ID := 43;
   Vector_44  : constant Interrupt_ID := 44;
   Vector_45  : constant Interrupt_ID := 45;
   Vector_46  : constant Interrupt_ID := 46;
   Vector_47  : constant Interrupt_ID := 47;

   Vector_48_63_Interrupt_Priority : constant System.Interrupt_Priority := 243;
   Vector_48  : constant Interrupt_ID := 48;
   Vector_49  : constant Interrupt_ID := 49;
   Vector_50  : constant Interrupt_ID := 50;
   Vector_51  : constant Interrupt_ID := 51;
   Vector_52  : constant Interrupt_ID := 52;
   Vector_53  : constant Interrupt_ID := 53;
   Vector_54  : constant Interrupt_ID := 54;
   Vector_55  : constant Interrupt_ID := 55;
   Vector_56  : constant Interrupt_ID := 56;
   Vector_57  : constant Interrupt_ID := 57;
   Vector_58  : constant Interrupt_ID := 58;
   Vector_59  : constant Interrupt_ID := 59;
   Vector_60  : constant Interrupt_ID := 60;
   Vector_61  : constant Interrupt_ID := 61;
   Vector_62  : constant Interrupt_ID := 62;
   Vector_63  : constant Interrupt_ID := 63;

   Vector_64_79_Interrupt_Priority : constant System.Interrupt_Priority := 244;
   Vector_64  : constant Interrupt_ID := 64;
   Vector_65  : constant Interrupt_ID := 65;
   Vector_66  : constant Interrupt_ID := 66;
   Vector_67  : constant Interrupt_ID := 67;
   Vector_68  : constant Interrupt_ID := 68;
   Vector_69  : constant Interrupt_ID := 69;
   Vector_70  : constant Interrupt_ID := 70;
   Vector_71  : constant Interrupt_ID := 71;
   Vector_72  : constant Interrupt_ID := 72;
   Vector_73  : constant Interrupt_ID := 73;
   Vector_74  : constant Interrupt_ID := 74;
   Vector_75  : constant Interrupt_ID := 75;
   Vector_76  : constant Interrupt_ID := 76;
   Vector_77  : constant Interrupt_ID := 77;
   Vector_78  : constant Interrupt_ID := 78;
   Vector_79  : constant Interrupt_ID := 79;

   Vector_80_95_Interrupt_Priority : constant System.Interrupt_Priority := 245;
   Vector_80  : constant Interrupt_ID := 80;
   Vector_81  : constant Interrupt_ID := 81;
   Vector_82  : constant Interrupt_ID := 82;
   Vector_83  : constant Interrupt_ID := 83;
   Vector_84  : constant Interrupt_ID := 84;
   Vector_85  : constant Interrupt_ID := 85;
   Vector_86  : constant Interrupt_ID := 86;
   Vector_87  : constant Interrupt_ID := 87;
   Vector_88  : constant Interrupt_ID := 88;
   Vector_89  : constant Interrupt_ID := 89;
   Vector_90  : constant Interrupt_ID := 90;
   Vector_91  : constant Interrupt_ID := 91;
   Vector_92  : constant Interrupt_ID := 92;
   Vector_93  : constant Interrupt_ID := 93;
   Vector_94  : constant Interrupt_ID := 94;
   Vector_95  : constant Interrupt_ID := 95;

   Vector_96_111_Interrupt_Priority :
     constant System.Interrupt_Priority := 246;
   Vector_96  : constant Interrupt_ID := 96;
   Vector_97  : constant Interrupt_ID := 97;
   Vector_98  : constant Interrupt_ID := 98;
   Vector_99  : constant Interrupt_ID := 99;
   Vector_100 : constant Interrupt_ID := 100;
   Vector_101 : constant Interrupt_ID := 101;
   Vector_102 : constant Interrupt_ID := 102;
   Vector_103 : constant Interrupt_ID := 103;
   Vector_104 : constant Interrupt_ID := 104;
   Vector_105 : constant Interrupt_ID := 105;
   Vector_106 : constant Interrupt_ID := 106;
   Vector_107 : constant Interrupt_ID := 107;
   Vector_108 : constant Interrupt_ID := 108;
   Vector_109 : constant Interrupt_ID := 109;
   Vector_110 : constant Interrupt_ID := 110;
   Vector_111 : constant Interrupt_ID := 111;

   Vector_112_127_Interrupt_Priority :
     constant System.Interrupt_Priority := 247;
   Vector_112 : constant Interrupt_ID := 112;
   Vector_113 : constant Interrupt_ID := 113;
   Vector_114 : constant Interrupt_ID := 114;
   Vector_115 : constant Interrupt_ID := 115;
   Vector_116 : constant Interrupt_ID := 116;
   Vector_117 : constant Interrupt_ID := 117;
   Vector_118 : constant Interrupt_ID := 118;
   Vector_119 : constant Interrupt_ID := 119;
   Vector_120 : constant Interrupt_ID := 120;
   Vector_121 : constant Interrupt_ID := 121;
   Vector_122 : constant Interrupt_ID := 122;
   Vector_123 : constant Interrupt_ID := 123;
   Vector_124 : constant Interrupt_ID := 124;
   Vector_125 : constant Interrupt_ID := 125;
   Vector_126 : constant Interrupt_ID := 126;
   Vector_127 : constant Interrupt_ID := 127;

   Vector_128_143_Interrupt_Priority :
     constant System.Interrupt_Priority := 248;
   Vector_128 : constant Interrupt_ID := 128;
   Vector_129 : constant Interrupt_ID := 129;
   Vector_130 : constant Interrupt_ID := 130;
   Vector_131 : constant Interrupt_ID := 131;
   Vector_132 : constant Interrupt_ID := 132;
   Vector_133 : constant Interrupt_ID := 133;
   Vector_134 : constant Interrupt_ID := 134;
   Vector_135 : constant Interrupt_ID := 135;
   Vector_136 : constant Interrupt_ID := 136;
   Vector_137 : constant Interrupt_ID := 137;
   Vector_138 : constant Interrupt_ID := 138;
   Vector_139 : constant Interrupt_ID := 139;
   Vector_140 : constant Interrupt_ID := 140;
   Vector_141 : constant Interrupt_ID := 141;
   Vector_142 : constant Interrupt_ID := 142;
   Vector_143 : constant Interrupt_ID := 143;

   Vector_144_159_Interrupt_Priority :
     constant System.Interrupt_Priority := 249;
   Vector_144 : constant Interrupt_ID := 144;
   Vector_145 : constant Interrupt_ID := 145;
   Vector_146 : constant Interrupt_ID := 146;
   Vector_147 : constant Interrupt_ID := 147;
   Vector_148 : constant Interrupt_ID := 148;
   Vector_149 : constant Interrupt_ID := 149;
   Vector_150 : constant Interrupt_ID := 150;
   Vector_151 : constant Interrupt_ID := 151;
   Vector_152 : constant Interrupt_ID := 152;
   Vector_153 : constant Interrupt_ID := 153;
   Vector_154 : constant Interrupt_ID := 154;
   Vector_155 : constant Interrupt_ID := 155;
   Vector_156 : constant Interrupt_ID := 156;
   Vector_157 : constant Interrupt_ID := 157;
   Vector_158 : constant Interrupt_ID := 158;
   Vector_159 : constant Interrupt_ID := 159;

   Vector_160_175_Interrupt_Priority : constant
     System.Interrupt_Priority := 250;
   Vector_160 : constant Interrupt_ID := 160;
   Vector_161 : constant Interrupt_ID := 161;
   Vector_162 : constant Interrupt_ID := 162;
   Vector_163 : constant Interrupt_ID := 163;
   Vector_164 : constant Interrupt_ID := 164;
   Vector_165 : constant Interrupt_ID := 165;
   Vector_166 : constant Interrupt_ID := 166;
   Vector_167 : constant Interrupt_ID := 167;
   Vector_168 : constant Interrupt_ID := 168;
   Vector_169 : constant Interrupt_ID := 169;
   Vector_170 : constant Interrupt_ID := 170;
   Vector_171 : constant Interrupt_ID := 171;
   Vector_172 : constant Interrupt_ID := 172;
   Vector_173 : constant Interrupt_ID := 173;
   Vector_174 : constant Interrupt_ID := 174;
   Vector_175 : constant Interrupt_ID := 175;

   Vector_176_191_Interrupt_Priority : constant
     System.Interrupt_Priority := 251;
   Vector_176 : constant Interrupt_ID := 176;
   Vector_177 : constant Interrupt_ID := 177;
   Vector_178 : constant Interrupt_ID := 178;
   Vector_179 : constant Interrupt_ID := 179;
   Vector_180 : constant Interrupt_ID := 180;
   Vector_181 : constant Interrupt_ID := 181;
   Vector_182 : constant Interrupt_ID := 182;
   Vector_183 : constant Interrupt_ID := 183;
   Vector_184 : constant Interrupt_ID := 184;
   Vector_185 : constant Interrupt_ID := 185;
   Vector_186 : constant Interrupt_ID := 186;
   Vector_187 : constant Interrupt_ID := 187;
   Vector_188 : constant Interrupt_ID := 188;
   Vector_189 : constant Interrupt_ID := 189;
   Vector_190 : constant Interrupt_ID := 190;
   Vector_191 : constant Interrupt_ID := 191;

   Vector_192_207_Interrupt_Priority : constant
     System.Interrupt_Priority := 252;
   Vector_192 : constant Interrupt_ID := 192;
   Vector_193 : constant Interrupt_ID := 193;
   Vector_194 : constant Interrupt_ID := 194;
   Vector_195 : constant Interrupt_ID := 195;
   Vector_196 : constant Interrupt_ID := 196;
   Vector_197 : constant Interrupt_ID := 197;
   Vector_198 : constant Interrupt_ID := 198;
   Vector_199 : constant Interrupt_ID := 199;
   Vector_200 : constant Interrupt_ID := 200;
   Vector_201 : constant Interrupt_ID := 201;
   Vector_202 : constant Interrupt_ID := 202;
   Vector_203 : constant Interrupt_ID := 203;
   Vector_204 : constant Interrupt_ID := 204;
   Vector_205 : constant Interrupt_ID := 205;
   Vector_206 : constant Interrupt_ID := 206;
   Vector_207 : constant Interrupt_ID := 207;

   Vector_208_223_Interrupt_Priority : constant
     System.Interrupt_Priority := 253;
   Vector_208 : constant Interrupt_ID := 208;
   Vector_209 : constant Interrupt_ID := 209;
   Vector_210 : constant Interrupt_ID := 210;
   Vector_211 : constant Interrupt_ID := 211;
   Vector_212 : constant Interrupt_ID := 212;
   Vector_213 : constant Interrupt_ID := 213;
   Vector_214 : constant Interrupt_ID := 214;
   Vector_215 : constant Interrupt_ID := 215;
   Vector_216 : constant Interrupt_ID := 216;
   Vector_217 : constant Interrupt_ID := 217;
   Vector_218 : constant Interrupt_ID := 218;
   Vector_219 : constant Interrupt_ID := 219;
   Vector_220 : constant Interrupt_ID := 220;
   Vector_221 : constant Interrupt_ID := 221;
   Vector_222 : constant Interrupt_ID := 222;
   Vector_223 : constant Interrupt_ID := 223;

   Vector_224_239_Interrupt_Priority : constant
     System.Interrupt_Priority := 254;
   Vector_224 : constant Interrupt_ID := 224;
   Vector_225 : constant Interrupt_ID := 225;
   Vector_226 : constant Interrupt_ID := 226;
   Vector_227 : constant Interrupt_ID := 227;
   Vector_228 : constant Interrupt_ID := 228;
   Vector_229 : constant Interrupt_ID := 229;
   Vector_230 : constant Interrupt_ID := 230;
   Vector_231 : constant Interrupt_ID := 231;
   Vector_232 : constant Interrupt_ID := 232;
   Vector_233 : constant Interrupt_ID := 233;
   Vector_234 : constant Interrupt_ID := 234;
   Vector_235 : constant Interrupt_ID := 235;
   Vector_236 : constant Interrupt_ID := 236;
   Vector_237 : constant Interrupt_ID := 237;
   Vector_238 : constant Interrupt_ID := 238;
   Vector_239 : constant Interrupt_ID := 239;

   Vector_240_255_Interrupt_Priority : constant
     System.Interrupt_Priority := 255;
   Vector_240 : constant Interrupt_ID := 240;
   Vector_241 : constant Interrupt_ID := 241;
   Vector_242 : constant Interrupt_ID := 242;
   Vector_243 : constant Interrupt_ID := 243;
   Vector_244 : constant Interrupt_ID := 244;
   Vector_245 : constant Interrupt_ID := 245;
   Vector_246 : constant Interrupt_ID := 246;
   Vector_247 : constant Interrupt_ID := 247;
   Vector_248 : constant Interrupt_ID := 248;
   Vector_249 : constant Interrupt_ID := 249;
   Vector_250 : constant Interrupt_ID := 250;
   Vector_251 : constant Interrupt_ID := 251;
   Vector_252 : constant Interrupt_ID := 252;
   Vector_253 : constant Interrupt_ID := 253;
   Vector_254 : constant Interrupt_ID := 254;
   --  Vector_255 -> Runtime: APIC Timer

end Ada.Interrupts.Names;
