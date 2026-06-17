##############################################################################
#                                                                            #
#                        Copyright (C) 2020, AdaCore                         #
#                                                                            #
#                               Assembly File                                #
#                                                                            #
##############################################################################

.extern __gnat_exception_handler
.extern __gnat_interrupt_handler

# Vector stubs that push the vector number onto the stack and call the common
# handler functions, expect for spurious interrupts where there is nothing to
# do but return.
#
# Written in assembly because gcc sticks the ud2 instruction at the end of
# every naked procedure, which given that we have 255 functions adds needless
# memory bloat.

#define EXC_VECTOR(n)           .global  vector_ ## n; vector_ ## n: pushq $n;           jmp __gnat_exception_handler
#define EXC_VECTOR_WITH_CODE(n) .global  vector_ ## n; vector_ ## n: pushq $0; pushq $n; jmp __gnat_exception_handler
#define IRQ_VECTOR(n)           .global  vector_ ## n; vector_ ## n: pushq $n;           jmp __gnat_interrupt_handler
#define SPURIOUS_VECTOR(n)      .global  vector_ ## n; vector_ ## n: iretq

        .section .text,"ad"
        .align 16

EXC_VECTOR(0);            EXC_VECTOR(1);            EXC_VECTOR(2);
EXC_VECTOR(3);            EXC_VECTOR(4);            EXC_VECTOR(5);
EXC_VECTOR(6);            EXC_VECTOR(7);            EXC_VECTOR_WITH_CODE(8);
EXC_VECTOR(9);            EXC_VECTOR_WITH_CODE(10); EXC_VECTOR_WITH_CODE(11);
EXC_VECTOR_WITH_CODE(12); EXC_VECTOR_WITH_CODE(13); EXC_VECTOR_WITH_CODE(14);
EXC_VECTOR(15);           EXC_VECTOR(16);           EXC_VECTOR_WITH_CODE(17);
EXC_VECTOR(18);           EXC_VECTOR(19);           EXC_VECTOR(20);
EXC_VECTOR_WITH_CODE(21); EXC_VECTOR (22);          EXC_VECTOR(23);
EXC_VECTOR(24);           EXC_VECTOR(25);           EXC_VECTOR(26);
EXC_VECTOR(27);           EXC_VECTOR(28);           EXC_VECTOR(29);
EXC_VECTOR(30);           EXC_VECTOR(31);           SPURIOUS_VECTOR(32);
IRQ_VECTOR(33);           IRQ_VECTOR(34);           IRQ_VECTOR(35);
IRQ_VECTOR(36);           IRQ_VECTOR(37);           IRQ_VECTOR(38);
IRQ_VECTOR(39);           IRQ_VECTOR(40);           IRQ_VECTOR(41);
IRQ_VECTOR(42);           IRQ_VECTOR(43);           IRQ_VECTOR(44);
IRQ_VECTOR(45);           IRQ_VECTOR(46);           IRQ_VECTOR(47);
IRQ_VECTOR(48);           IRQ_VECTOR(49);           IRQ_VECTOR(50);
IRQ_VECTOR(51);           IRQ_VECTOR(52);           IRQ_VECTOR(53);
IRQ_VECTOR(54);           IRQ_VECTOR(55);           IRQ_VECTOR(56);
IRQ_VECTOR(57);           IRQ_VECTOR(58);           IRQ_VECTOR(59);
IRQ_VECTOR(60);           IRQ_VECTOR(61);           IRQ_VECTOR(62);
IRQ_VECTOR(63);           IRQ_VECTOR(64);           IRQ_VECTOR(65);
IRQ_VECTOR(66);           IRQ_VECTOR(67);           IRQ_VECTOR(68);
IRQ_VECTOR(69);           IRQ_VECTOR(70);           IRQ_VECTOR(71);
IRQ_VECTOR(72);           IRQ_VECTOR(73);           IRQ_VECTOR(74);
IRQ_VECTOR(75);           IRQ_VECTOR(76);           IRQ_VECTOR(77);
IRQ_VECTOR(78);           IRQ_VECTOR(79);           IRQ_VECTOR(80);
IRQ_VECTOR(81);           IRQ_VECTOR(82);           IRQ_VECTOR(83);
IRQ_VECTOR(84);           IRQ_VECTOR(85);           IRQ_VECTOR(86);
IRQ_VECTOR(87);           IRQ_VECTOR(88);           IRQ_VECTOR(89);
IRQ_VECTOR(90);           IRQ_VECTOR(91);           IRQ_VECTOR(92);
IRQ_VECTOR(93);           IRQ_VECTOR(94);           IRQ_VECTOR(95);
IRQ_VECTOR(96);           IRQ_VECTOR(97);           IRQ_VECTOR(98);
IRQ_VECTOR(99);           IRQ_VECTOR(100);          IRQ_VECTOR(101);
IRQ_VECTOR(102);          IRQ_VECTOR(103);          IRQ_VECTOR(104);
IRQ_VECTOR(105);          IRQ_VECTOR(106);          IRQ_VECTOR(107);
IRQ_VECTOR(108);          IRQ_VECTOR(109);          IRQ_VECTOR(110);
IRQ_VECTOR(111);          IRQ_VECTOR(112);          IRQ_VECTOR(113);
IRQ_VECTOR(114);          IRQ_VECTOR(115);          IRQ_VECTOR(116);
IRQ_VECTOR(117);          IRQ_VECTOR(118);          IRQ_VECTOR(119);
IRQ_VECTOR(120);          IRQ_VECTOR(121);          IRQ_VECTOR(122);
IRQ_VECTOR(123);          IRQ_VECTOR(124);          IRQ_VECTOR(125);
IRQ_VECTOR(126);          IRQ_VECTOR(127);          IRQ_VECTOR(128);
IRQ_VECTOR(129);          IRQ_VECTOR(130);          IRQ_VECTOR(131);
IRQ_VECTOR(132);          IRQ_VECTOR(133);          IRQ_VECTOR(134);
IRQ_VECTOR(135);          IRQ_VECTOR(136);          IRQ_VECTOR(137);
IRQ_VECTOR(138);          IRQ_VECTOR(139);          IRQ_VECTOR(140);
IRQ_VECTOR(141);          IRQ_VECTOR(142);          IRQ_VECTOR(143);
IRQ_VECTOR(144);          IRQ_VECTOR(145);          IRQ_VECTOR(146);
IRQ_VECTOR(147);          IRQ_VECTOR(148);          IRQ_VECTOR(149);
IRQ_VECTOR(150);          IRQ_VECTOR(151);          IRQ_VECTOR(152);
IRQ_VECTOR(153);          IRQ_VECTOR(154);          IRQ_VECTOR(155);
IRQ_VECTOR(156);          IRQ_VECTOR(157);          IRQ_VECTOR(158);
IRQ_VECTOR(159);          IRQ_VECTOR(160);          IRQ_VECTOR(161);
IRQ_VECTOR(162);          IRQ_VECTOR(163);          IRQ_VECTOR(164);
IRQ_VECTOR(165);          IRQ_VECTOR(166);          IRQ_VECTOR(167);
IRQ_VECTOR(168);          IRQ_VECTOR(169);          IRQ_VECTOR(170);
IRQ_VECTOR(171);          IRQ_VECTOR(172);          IRQ_VECTOR(173);
IRQ_VECTOR(174);          IRQ_VECTOR(175);          IRQ_VECTOR(176);
IRQ_VECTOR(177);          IRQ_VECTOR(178);          IRQ_VECTOR(179);
IRQ_VECTOR(180);          IRQ_VECTOR(181);          IRQ_VECTOR(182);
IRQ_VECTOR(183);          IRQ_VECTOR(184);          IRQ_VECTOR(185);
IRQ_VECTOR(186);          IRQ_VECTOR(187);          IRQ_VECTOR(188);
IRQ_VECTOR(189);          IRQ_VECTOR(190);          IRQ_VECTOR(191);
IRQ_VECTOR(192);          IRQ_VECTOR(193);          IRQ_VECTOR(194);
IRQ_VECTOR(195);          IRQ_VECTOR(196);          IRQ_VECTOR(197);
IRQ_VECTOR(198);          IRQ_VECTOR(199);          IRQ_VECTOR(200);
IRQ_VECTOR(201);          IRQ_VECTOR(202);          IRQ_VECTOR(203);
IRQ_VECTOR(204);          IRQ_VECTOR(205);          IRQ_VECTOR(206);
IRQ_VECTOR(207);          IRQ_VECTOR(208);          IRQ_VECTOR(209);
IRQ_VECTOR(210);          IRQ_VECTOR(211);          IRQ_VECTOR(212);
IRQ_VECTOR(213);          IRQ_VECTOR(214);          IRQ_VECTOR(215);
IRQ_VECTOR(216);          IRQ_VECTOR(217);          IRQ_VECTOR(218);
IRQ_VECTOR(219);          IRQ_VECTOR(220);          IRQ_VECTOR(221);
IRQ_VECTOR(222);          IRQ_VECTOR(223);          IRQ_VECTOR(224);
IRQ_VECTOR(225);          IRQ_VECTOR(226);          IRQ_VECTOR(227);
IRQ_VECTOR(228);          IRQ_VECTOR(229);          IRQ_VECTOR(230);
IRQ_VECTOR(231);          IRQ_VECTOR(232);          IRQ_VECTOR(233);
IRQ_VECTOR(234);          IRQ_VECTOR(235);          IRQ_VECTOR(236);
IRQ_VECTOR(237);          IRQ_VECTOR(238);          IRQ_VECTOR(239);
IRQ_VECTOR(240);          IRQ_VECTOR(241);          IRQ_VECTOR(242);
IRQ_VECTOR(243);          IRQ_VECTOR(244);          IRQ_VECTOR(245);
IRQ_VECTOR(246);          IRQ_VECTOR(247);          IRQ_VECTOR(248);
IRQ_VECTOR(249);          IRQ_VECTOR(250);          IRQ_VECTOR(251);
IRQ_VECTOR(252);          IRQ_VECTOR(253);          IRQ_VECTOR(254);
IRQ_VECTOR(255);
