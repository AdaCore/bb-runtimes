/*
   Copyright (C) 2013-2017, Free Software Foundation, Inc.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   GCC is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   Under Section 7 of GPL version 3, you are granted additional
   permissions described in the GCC Runtime Library Exception, version
   3.1, as published by the Free Software Foundation.

   You should have received a copy of the GNU General Public License and
   a copy of the GCC Runtime Library Exception along with this program;
   see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
   <http://www.gnu.org/licenses/>.  */

/* Locate the FDE entry for a given address using .eh_frame_hdr for
   platforms without shared libraries.  */

#include "tconfig.h"
#include "tsystem.h"
#include <stddef.h>
#define NO_BASE_OF_ENCODED_VALUE
#include "unwind.h"
#include "unwind-pe.h"
#include "unwind-dw2-fde.h"

extern char __text[];
extern char __data[];

/* Format of .eh_frame_hdr:  */
struct unw_eh_frame_hdr
{
  unsigned char version;
  unsigned char eh_frame_ptr_enc;
  unsigned char fde_count_enc;
  unsigned char table_enc;

  /* Followed by:
   eh_frame_ptr: (encoded using eh_frame_ptr_enc), pointer to .eh_frame
   fde_count   : (encoded using fde_count_enc), number of fde entries
   binary search table (encoded using table_enc)
      initial location
      address
  */
};

/* Like base_of_encoded_value, but take the base from a struct
   unw_eh_callback_data instead of an _Unwind_Context.  */

static _Unwind_Ptr
get_encoding_base (unsigned char encoding)
{
  if (encoding == DW_EH_PE_omit)
    return 0;

  switch (encoding & 0x70)
    {
    case DW_EH_PE_absptr:
    case DW_EH_PE_pcrel:
    case DW_EH_PE_aligned:
      return 0;

    case DW_EH_PE_textrel:
      return (_Unwind_Ptr) __text;
    case DW_EH_PE_datarel:
      return (_Unwind_Ptr) __data;
    default:
      abort ();
    }
}

/* Return the FDE pointer encoding from the CIE.  */
/* ??? This is a subset of extract_cie_info from unwind-dw2.c.  */

static int
get_cie_encoding (const struct dwarf_cie *cie)
{
  const unsigned char *aug, *p;
  _Unwind_Ptr dummy;
  _uleb128_t utmp;
  _sleb128_t stmp;

  aug = cie->augmentation;

  /* Skip the augmentation string.  */
  for (p = aug; *p != 0; p++)
    ;
  p++;

  if (__builtin_expect (cie->version >= 4, 0))
    {
      if (p[0] != sizeof (void *) || p[1] != 0)
	return DW_EH_PE_omit;	/* We are not prepared to handle unexpected
				   address sizes or segment selectors.  */
      p += 2;			/* Skip address size and segment size.  */
    }

  if (aug[0] != 'z')
    return DW_EH_PE_absptr;

  p = read_uleb128 (p, &utmp);		/* Skip code alignment.  */
  p = read_sleb128 (p, &stmp);		/* Skip data alignment.  */
  if (cie->version == 1)		/* Skip return address column.  */
    p++;
  else
    p = read_uleb128 (p, &utmp);

  aug++;				/* Skip 'z' */
  p = read_uleb128 (p, &utmp);		/* Skip augmentation length.  */
  while (1)
    {
      /* This is what we're looking for.  */
      if (*aug == 'R')
	return *p;
      /* Personality encoding and pointer.  */
      else if (*aug == 'P')
	{
	  /* ??? Avoid dereferencing indirect pointers, since we're
	     faking the base address.  Gotta keep DW_EH_PE_aligned
	     intact, however.  */
	  p = read_encoded_value_with_base (*p & 0x7F, 0, p + 1, &dummy);
	}
      /* LSDA encoding.  */
      else if (*aug == 'L')
	p++;
      /* Otherwise end of string, or unknown augmentation.  */
      else
	return DW_EH_PE_absptr;
      aug++;
    }
}

static inline int
get_fde_encoding (const struct dwarf_fde *f)
{
  return get_cie_encoding (get_cie (f));
}

static void *
bad_eh_frame_hdr (void)
{
  abort ();
}

const fde *
_Unwind_Find_FDE (void *pc, struct dwarf_eh_bases *bases)
{
  extern const struct unw_eh_frame_hdr __eh_frame_hdr;
  const struct unw_eh_frame_hdr *hdr = &__eh_frame_hdr;
  const unsigned char *p;
  _Unwind_Ptr eh_frame;
  _Unwind_Ptr fde_count;
  unsigned char table_enc = (DW_EH_PE_datarel | DW_EH_PE_sdata4);
  fde *f;

  /* Read .eh_frame_hdr header.  */
  if (hdr->version != 1)
    return bad_eh_frame_hdr ();

  /* Check for no entries.  */
  if (hdr->fde_count_enc == DW_EH_PE_omit)
    return NULL;

  /* We require here specific table encoding to speed things up.  */
  if (hdr->table_enc != table_enc)
    return bad_eh_frame_hdr ();

  /* Read eh_frame address.  */
  p = read_encoded_value_with_base (hdr->eh_frame_ptr_enc,
				    get_encoding_base (hdr->eh_frame_ptr_enc),
				    (const unsigned char *) (hdr + 1),
				    &eh_frame);

  /* Read fde_count.  */
  p = read_encoded_value_with_base (hdr->fde_count_enc,
				    get_encoding_base (hdr->fde_count_enc),
				    p, &fde_count);

  /* Shouldn't happen.  */
  if (fde_count == 0)
    return NULL;

  /* Check alignment.  */
  if ((((_Unwind_Ptr) p) & 3) != 0)
    return bad_eh_frame_hdr ();

  {
    struct fde_table {
      signed initial_loc __attribute__ ((mode (SI)));
      signed fde __attribute__ ((mode (SI)));
    };
    const struct fde_table *table = (const struct fde_table *) p;
    size_t lo, hi, mid;
    _Unwind_Ptr unw_pc = (_Unwind_Ptr) pc;
    _Unwind_Ptr data_base = (_Unwind_Ptr) hdr;
    unsigned int f_enc, f_enc_size;
    _Unwind_Ptr range;

    mid = fde_count - 1;
    if (unw_pc < table[0].initial_loc + data_base)
      return NULL;
    else if (unw_pc < table[mid].initial_loc + data_base)
      {
	lo = 0;
	hi = mid;

	while (lo < hi)
	  {
	    mid = (lo + hi) / 2;
	    if (unw_pc < table[mid].initial_loc + data_base)
	      hi = mid;
	    else if (unw_pc >= table[mid + 1].initial_loc + data_base)
	      lo = mid + 1;
	    else
	      break;
	  }
      }

    f = (fde *) (table[mid].fde + data_base);
    f_enc = get_fde_encoding (f);
    f_enc_size = size_of_encoded_value (f_enc);
    read_encoded_value_with_base (f_enc & 0x0f, 0,
				  &f->pc_begin[f_enc_size], &range);
    if (unw_pc >= table[mid].initial_loc + data_base + range)
      return NULL;
    bases->func = (void *) (table[mid].initial_loc + data_base);
    bases->tbase = __text;
    bases->dbase = __data;
    return f;
  }

  return NULL;
}
