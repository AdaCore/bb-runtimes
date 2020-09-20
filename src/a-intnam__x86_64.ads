------------------------------------------------------------------------------
--                                                                          --
--                  GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--                   A D A . I N T E R R U P T S . N A M E S                --
--                                                                          --
--                                  S p e c                                 --
--                                                                          --
--            Copyright (C) 2020, Free Software Foundation, Inc.            --
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

--  This package version is specific to the x86-64

pragma Restrictions (No_Elaboration_Code);

package Ada.Interrupts.Names is

   --  All identifiers in this unit are implementation defined

   pragma Implementation_Defined;

   --  On x86-64 the interrupt vector number encodes the priority of the
   --  interrupt. There are 16 interrupt priority classes, each containing 16
   --  interrupt vectors. The interrupt priority of the protected object
   --  containing the protected handler must match the priority class of the
   --  interrupt.

   --  Note: Interrupt vectors used for the runtime are commented and marked as
   --  such.

   --  Note: Interrupt Priorties 240 and 242 are missing as they map to
   --  CPU exception vectors.

   IRQ_32_47_Interrupt_Priority : constant System.Interrupt_Priority := 242;
   --  IRQ_32 -> Runtime: Spurious interrupt
   IRQ_33  : constant Interrupt_ID := 33;
   IRQ_34  : constant Interrupt_ID := 34;
   IRQ_35  : constant Interrupt_ID := 35;
   IRQ_36  : constant Interrupt_ID := 36;
   IRQ_37  : constant Interrupt_ID := 37;
   IRQ_38  : constant Interrupt_ID := 38;
   IRQ_39  : constant Interrupt_ID := 39;
   IRQ_40  : constant Interrupt_ID := 40;
   IRQ_41  : constant Interrupt_ID := 41;
   IRQ_42  : constant Interrupt_ID := 42;
   IRQ_43  : constant Interrupt_ID := 43;
   IRQ_44  : constant Interrupt_ID := 44;
   IRQ_45  : constant Interrupt_ID := 45;
   IRQ_46  : constant Interrupt_ID := 46;
   IRQ_47  : constant Interrupt_ID := 47;

   IRQ_48_63_Interrupt_Priority : constant System.Interrupt_Priority := 243;
   IRQ_48  : constant Interrupt_ID := 48;
   IRQ_49  : constant Interrupt_ID := 49;
   IRQ_50  : constant Interrupt_ID := 50;
   IRQ_51  : constant Interrupt_ID := 51;
   IRQ_52  : constant Interrupt_ID := 52;
   IRQ_53  : constant Interrupt_ID := 53;
   IRQ_54  : constant Interrupt_ID := 54;
   IRQ_55  : constant Interrupt_ID := 55;
   IRQ_56  : constant Interrupt_ID := 56;
   IRQ_57  : constant Interrupt_ID := 57;
   IRQ_58  : constant Interrupt_ID := 58;
   IRQ_59  : constant Interrupt_ID := 59;
   IRQ_60  : constant Interrupt_ID := 60;
   IRQ_61  : constant Interrupt_ID := 61;
   IRQ_62  : constant Interrupt_ID := 62;
   IRQ_63  : constant Interrupt_ID := 63;

   IRQ_64_79_Interrupt_Priority : constant System.Interrupt_Priority := 244;
   IRQ_64  : constant Interrupt_ID := 64;
   IRQ_65  : constant Interrupt_ID := 65;
   IRQ_66  : constant Interrupt_ID := 66;
   IRQ_67  : constant Interrupt_ID := 67;
   IRQ_68  : constant Interrupt_ID := 68;
   IRQ_69  : constant Interrupt_ID := 69;
   IRQ_70  : constant Interrupt_ID := 70;
   IRQ_71  : constant Interrupt_ID := 71;
   IRQ_72  : constant Interrupt_ID := 72;
   IRQ_73  : constant Interrupt_ID := 73;
   IRQ_74  : constant Interrupt_ID := 74;
   IRQ_75  : constant Interrupt_ID := 75;
   IRQ_76  : constant Interrupt_ID := 76;
   IRQ_77  : constant Interrupt_ID := 77;
   IRQ_78  : constant Interrupt_ID := 78;
   IRQ_79  : constant Interrupt_ID := 79;

   IRQ_80_95_Interrupt_Priority : constant System.Interrupt_Priority := 245;
   IRQ_80  : constant Interrupt_ID := 80;
   IRQ_81  : constant Interrupt_ID := 81;
   IRQ_82  : constant Interrupt_ID := 82;
   IRQ_83  : constant Interrupt_ID := 83;
   IRQ_84  : constant Interrupt_ID := 84;
   IRQ_85  : constant Interrupt_ID := 85;
   IRQ_86  : constant Interrupt_ID := 86;
   IRQ_87  : constant Interrupt_ID := 87;
   IRQ_88  : constant Interrupt_ID := 88;
   IRQ_89  : constant Interrupt_ID := 89;
   IRQ_90  : constant Interrupt_ID := 90;
   IRQ_91  : constant Interrupt_ID := 91;
   IRQ_92  : constant Interrupt_ID := 92;
   IRQ_93  : constant Interrupt_ID := 93;
   IRQ_94  : constant Interrupt_ID := 94;
   IRQ_95  : constant Interrupt_ID := 95;

   IRQ_96_111_Interrupt_Priority : constant System.Interrupt_Priority := 246;
   IRQ_96  : constant Interrupt_ID := 96;
   IRQ_97  : constant Interrupt_ID := 97;
   IRQ_98  : constant Interrupt_ID := 98;
   IRQ_99  : constant Interrupt_ID := 99;
   IRQ_100 : constant Interrupt_ID := 100;
   IRQ_101 : constant Interrupt_ID := 101;
   IRQ_102 : constant Interrupt_ID := 102;
   IRQ_103 : constant Interrupt_ID := 103;
   IRQ_104 : constant Interrupt_ID := 104;
   IRQ_105 : constant Interrupt_ID := 105;
   IRQ_106 : constant Interrupt_ID := 106;
   IRQ_107 : constant Interrupt_ID := 107;
   IRQ_108 : constant Interrupt_ID := 108;
   IRQ_109 : constant Interrupt_ID := 109;
   IRQ_110 : constant Interrupt_ID := 110;
   IRQ_111 : constant Interrupt_ID := 111;

   IRQ_112_127_Interrupt_Priority : constant System.Interrupt_Priority := 247;
   IRQ_112 : constant Interrupt_ID := 112;
   IRQ_113 : constant Interrupt_ID := 113;
   IRQ_114 : constant Interrupt_ID := 114;
   IRQ_115 : constant Interrupt_ID := 115;
   IRQ_116 : constant Interrupt_ID := 116;
   IRQ_117 : constant Interrupt_ID := 117;
   IRQ_118 : constant Interrupt_ID := 118;
   IRQ_119 : constant Interrupt_ID := 119;
   IRQ_120 : constant Interrupt_ID := 120;
   IRQ_121 : constant Interrupt_ID := 121;
   IRQ_122 : constant Interrupt_ID := 122;
   IRQ_123 : constant Interrupt_ID := 123;
   IRQ_124 : constant Interrupt_ID := 124;
   IRQ_125 : constant Interrupt_ID := 125;
   IRQ_126 : constant Interrupt_ID := 126;
   IRQ_127 : constant Interrupt_ID := 127;

   IRQ_128_143_Interrupt_Priority : constant System.Interrupt_Priority := 248;
   IRQ_128 : constant Interrupt_ID := 128;
   IRQ_129 : constant Interrupt_ID := 129;
   IRQ_130 : constant Interrupt_ID := 130;
   IRQ_131 : constant Interrupt_ID := 131;
   IRQ_132 : constant Interrupt_ID := 132;
   IRQ_133 : constant Interrupt_ID := 133;
   IRQ_134 : constant Interrupt_ID := 134;
   IRQ_135 : constant Interrupt_ID := 135;
   IRQ_136 : constant Interrupt_ID := 136;
   IRQ_137 : constant Interrupt_ID := 137;
   IRQ_138 : constant Interrupt_ID := 138;
   IRQ_139 : constant Interrupt_ID := 139;
   IRQ_140 : constant Interrupt_ID := 140;
   IRQ_141 : constant Interrupt_ID := 141;
   IRQ_142 : constant Interrupt_ID := 142;
   IRQ_143 : constant Interrupt_ID := 143;

   IRQ_144_159_Interrupt_Priority : constant System.Interrupt_Priority := 249;
   IRQ_144 : constant Interrupt_ID := 144;
   IRQ_145 : constant Interrupt_ID := 145;
   IRQ_146 : constant Interrupt_ID := 146;
   IRQ_147 : constant Interrupt_ID := 147;
   IRQ_148 : constant Interrupt_ID := 148;
   IRQ_149 : constant Interrupt_ID := 149;
   IRQ_150 : constant Interrupt_ID := 150;
   IRQ_151 : constant Interrupt_ID := 151;
   IRQ_152 : constant Interrupt_ID := 152;
   IRQ_153 : constant Interrupt_ID := 153;
   IRQ_154 : constant Interrupt_ID := 154;
   IRQ_155 : constant Interrupt_ID := 155;
   IRQ_156 : constant Interrupt_ID := 156;
   IRQ_157 : constant Interrupt_ID := 157;
   IRQ_158 : constant Interrupt_ID := 158;
   IRQ_159 : constant Interrupt_ID := 159;

   IRQ_160_175_Interrupt_Priority : constant System.Interrupt_Priority := 250;
   IRQ_160 : constant Interrupt_ID := 160;
   IRQ_161 : constant Interrupt_ID := 161;
   IRQ_162 : constant Interrupt_ID := 162;
   IRQ_163 : constant Interrupt_ID := 163;
   IRQ_164 : constant Interrupt_ID := 164;
   IRQ_165 : constant Interrupt_ID := 165;
   IRQ_166 : constant Interrupt_ID := 166;
   IRQ_167 : constant Interrupt_ID := 167;
   IRQ_168 : constant Interrupt_ID := 168;
   IRQ_169 : constant Interrupt_ID := 169;
   IRQ_170 : constant Interrupt_ID := 170;
   IRQ_171 : constant Interrupt_ID := 171;
   IRQ_172 : constant Interrupt_ID := 172;
   IRQ_173 : constant Interrupt_ID := 173;
   IRQ_174 : constant Interrupt_ID := 174;
   IRQ_175 : constant Interrupt_ID := 175;

   IRQ_176_191_Interrupt_Priority : constant System.Interrupt_Priority := 251;
   IRQ_176 : constant Interrupt_ID := 176;
   IRQ_177 : constant Interrupt_ID := 177;
   IRQ_178 : constant Interrupt_ID := 178;
   IRQ_179 : constant Interrupt_ID := 179;
   IRQ_180 : constant Interrupt_ID := 180;
   IRQ_181 : constant Interrupt_ID := 181;
   IRQ_182 : constant Interrupt_ID := 182;
   IRQ_183 : constant Interrupt_ID := 183;
   IRQ_184 : constant Interrupt_ID := 184;
   IRQ_185 : constant Interrupt_ID := 185;
   IRQ_186 : constant Interrupt_ID := 186;
   IRQ_187 : constant Interrupt_ID := 187;
   IRQ_188 : constant Interrupt_ID := 188;
   IRQ_189 : constant Interrupt_ID := 189;
   IRQ_190 : constant Interrupt_ID := 190;
   IRQ_191 : constant Interrupt_ID := 191;

   IRQ_192_207_Interrupt_Priority : constant System.Interrupt_Priority := 252;
   IRQ_192 : constant Interrupt_ID := 192;
   IRQ_193 : constant Interrupt_ID := 193;
   IRQ_194 : constant Interrupt_ID := 194;
   IRQ_195 : constant Interrupt_ID := 195;
   IRQ_196 : constant Interrupt_ID := 196;
   IRQ_197 : constant Interrupt_ID := 197;
   IRQ_198 : constant Interrupt_ID := 198;
   IRQ_199 : constant Interrupt_ID := 199;
   IRQ_200 : constant Interrupt_ID := 200;
   IRQ_201 : constant Interrupt_ID := 201;
   IRQ_202 : constant Interrupt_ID := 202;
   IRQ_203 : constant Interrupt_ID := 203;
   IRQ_204 : constant Interrupt_ID := 204;
   IRQ_205 : constant Interrupt_ID := 205;
   IRQ_206 : constant Interrupt_ID := 206;
   IRQ_207 : constant Interrupt_ID := 207;

   IRQ_208_223_Interrupt_Priority : constant System.Interrupt_Priority := 253;
   IRQ_208 : constant Interrupt_ID := 208;
   IRQ_209 : constant Interrupt_ID := 209;
   IRQ_210 : constant Interrupt_ID := 210;
   IRQ_211 : constant Interrupt_ID := 211;
   IRQ_212 : constant Interrupt_ID := 212;
   IRQ_213 : constant Interrupt_ID := 213;
   IRQ_214 : constant Interrupt_ID := 214;
   IRQ_215 : constant Interrupt_ID := 215;
   IRQ_216 : constant Interrupt_ID := 216;
   IRQ_217 : constant Interrupt_ID := 217;
   IRQ_218 : constant Interrupt_ID := 218;
   IRQ_219 : constant Interrupt_ID := 219;
   IRQ_220 : constant Interrupt_ID := 220;
   IRQ_221 : constant Interrupt_ID := 221;
   IRQ_222 : constant Interrupt_ID := 222;
   IRQ_223 : constant Interrupt_ID := 223;

   IRQ_224_239_Interrupt_Priority : constant System.Interrupt_Priority := 254;
   IRQ_224 : constant Interrupt_ID := 224;
   IRQ_225 : constant Interrupt_ID := 225;
   IRQ_226 : constant Interrupt_ID := 226;
   IRQ_227 : constant Interrupt_ID := 227;
   IRQ_228 : constant Interrupt_ID := 228;
   IRQ_229 : constant Interrupt_ID := 229;
   IRQ_230 : constant Interrupt_ID := 230;
   IRQ_231 : constant Interrupt_ID := 231;
   IRQ_232 : constant Interrupt_ID := 232;
   IRQ_233 : constant Interrupt_ID := 233;
   IRQ_234 : constant Interrupt_ID := 234;
   IRQ_235 : constant Interrupt_ID := 235;
   IRQ_236 : constant Interrupt_ID := 236;
   IRQ_237 : constant Interrupt_ID := 237;
   IRQ_238 : constant Interrupt_ID := 238;
   IRQ_239 : constant Interrupt_ID := 239;

   IRQ_240_255_Interrupt_Priority : constant System.Interrupt_Priority := 255;
   IRQ_240 : constant Interrupt_ID := 240;
   IRQ_241 : constant Interrupt_ID := 241;
   IRQ_242 : constant Interrupt_ID := 242;
   IRQ_243 : constant Interrupt_ID := 243;
   IRQ_244 : constant Interrupt_ID := 244;
   IRQ_245 : constant Interrupt_ID := 245;
   IRQ_246 : constant Interrupt_ID := 246;
   IRQ_247 : constant Interrupt_ID := 247;
   IRQ_248 : constant Interrupt_ID := 248;
   IRQ_249 : constant Interrupt_ID := 249;
   IRQ_250 : constant Interrupt_ID := 250;
   IRQ_251 : constant Interrupt_ID := 251;
   IRQ_252 : constant Interrupt_ID := 252;
   IRQ_253 : constant Interrupt_ID := 253;
   IRQ_254 : constant Interrupt_ID := 254;
   --  IRQ_255 -> Runtime: APIC Timer

end Ada.Interrupts.Names;
