/****************************************************************************
 *                                                                          *
 *               GNU ADA RUN-TIME LIBRARY (GNARL) COMPONENTS                *
 *                                                                          *
 *                          R E L O C _ S Y M B O L S                       *
 *                                                                          *
 *                               Assembly File                              *
 *                                                                          *
 *                        Copyright (C) 2023 AdaCore                        *
 *                                                                          *
 * GNAT is free software;  you can  redistribute it  and/or modify it under *
 * terms of the  GNU General Public License as published  by the Free Soft- *
 * ware  Foundation;  either version 2,  or (at your option) any later ver- *
 * sion.  GNAT is distributed in the hope that it will be useful, but WITH- *
 * OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY *
 * or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License *
 * for  more details.  You should have  received  a copy of the GNU General *
 * Public License  distributed with GNAT;  see file COPYING.  If not, write *
 * to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, *
 * MA 02111-1307, USA.                                                      *
 *                                                                          *
 * As a  special  exception,  if you  link  this file  with other  files to *
 * produce an executable,  this file does not by itself cause the resulting *
 * executable to be covered by the GNU General Public License. This except- *
 * ion does not  however invalidate  any other reasons  why the  executable *
 * file might be covered by the  GNU Public License.                        *
 *                                                                          *
 ****************************************************************************/

/* This file contains accessor functions to get capabilities pointing to the
   start/end addresses of the __cap_relocs and __rela_dyn sections. */

.text

        .globl __get_cap_relocs_start
        .weak __get_cap_relocs_start
        .type __get_cap_relocs_start, @function
        .align 4
__get_cap_relocs_start:
        adr c0, cap_relocs
        ldr x0, [c0]
        mrs c1, DDC
        scvalue c0, c1, x0
        ret

        .globl __get_cap_relocs_end
        .weak __get_cap_relocs_end
        .type __get_cap_relocs_end, @function
        .align 4
__get_cap_relocs_end:
        adr c0, cap_relocs + 8
        ldr x0, [c0]
        mrs c1, DDC
        scvalue c0, c1, x0
        ret

        .globl __get_rela_dyn_start
        .weak __get_rela_dyn_start
        .type __get_rela_dyn_start, @function
        .align 4
__get_rela_dyn_start:
        adr c0, rela_dyn
        ldr x0, [c0]
        mrs c1, DDC
        scvalue c0, c1, x0
        ret

        .globl __get_rela_dyn_end
        .weak __get_rela_dyn_end
        .type __get_rela_dyn_end, @function
        .align 4
__get_rela_dyn_end:
        adr c0, rela_dyn + 8
        ldr x0, [c0]
        mrs c1, DDC
        scvalue c0, c1, x0
        ret

        .align 4
rela_dyn:
        .dword __rela_dyn_start
        .dword __rela_dyn_end

        .align 4
cap_relocs:
        .dword __cap_relocs_start
        .dword __cap_relocs_end
