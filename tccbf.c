/*
 *  TCCBF.C - BF file output for the Tiny C Compiler
 */

#include "tcc.h"

#ifndef IMAGE_NT_SIGNATURE
/* ----------------------------------------------------------- */
/* definitions below are from winnt.h */

typedef unsigned char BYTE;
typedef unsigned short WORD;
typedef unsigned int DWORD;
typedef unsigned long long ULONGLONG;
#endif

struct bf_info {
    TCCState *s1;
    const char *filename;
    int type;
    DWORD imagebase;
    const char *start_symbol;
    DWORD start_addr;
    int subsystem;
    DWORD section_align;
    struct section_info *sec_info;
    int sec_count;
};

static void bf_add_runtime(TCCState * s1, struct bf_info *bf)
{
    const char *start_symbol;
    int bf_type = 0;
    int unicode_entry = 0;

    if (find_elf_sym(symtab_section, "wmain"))
    {
        bf_type = 1;
    }

    start_symbol ="__start";

    if (!s1->leading_underscore || strchr(start_symbol, '@'))
        ++start_symbol;

    set_elf_sym(symtab_section,
        0, 0,
        ELFW(ST_INFO)(STB_GLOBAL, STT_NOTYPE), 0,
        SHN_UNDEF, start_symbol);

    tcc_add_pragma_libs(s1);

    if (0 == s1->nostdlib) {
        static const char *libs[] = {
            "bfstd", NULL
        };
        const char **pp, *p;
        for (pp = libs; 0 != (p = *pp); ++pp) {
            if (0 == *p) {
                 break;
            } else if (pp == libs && tcc_add_dll(s1, p, 0) >= 0) {
                continue;
            } else {
                tcc_add_library_err(s1, p);
            }
        }
    }

    bf->type = bf_type;
    bf->start_symbol = start_symbol;
}

static void bf_set_options(struct bf_info *bf)
{
    if (1 == bf->type) {
        bf->imagebase = 0x100;
    }

    bf->subsystem = 2;
}

enum {
    sec_text = 0,
    sec_data ,
    sec_bss ,
    sec_idata ,
    sec_pdata ,
    sec_other ,
    sec_rsrc ,
    sec_stab ,
    sec_reloc ,
    sec_last
};

struct section_info {
    int cls, ord;
    char name[32];
    DWORD sh_addr;
    DWORD sh_size;
    DWORD sh_flags;
    unsigned char *data;
    DWORD data_size;
};

/* ------------------------------------------------------------- */
static int bf_section_class(Section *s)
{
    printf("\nTCCBF:%s:%d: not implemented\n", __FILE__, __LINE__);
    return -1;
}

static DWORD bf_virtual_align(struct bf_info *bf, DWORD n)
{
    return (n + (bf->section_align - 1)) & ~(bf->section_align - 1);
}

static void bf_print_section(FILE * f, Section * s)
{
    /* just if you're curious */
    BYTE *p, *e, b;
    int i, n, l, m;
    p = s->data;
    e = s->data + s->data_offset;
    l = e - p;

    fprintf(f, "section  \"%s\"", s->name);
    if (s->link)
        fprintf(f, "\nlink     \"%s\"", s->link->name);
    if (s->reloc)
        fprintf(f, "\nreloc    \"%s\"", s->reloc->name);
    fprintf(f, "\nv_addr   %08X", (unsigned)s->sh_addr);
    fprintf(f, "\ncontents %08X", (unsigned)l);
    fprintf(f, "\n\n");

    if (s->sh_type == SHT_NOBITS)
        return;

    if (0 == l)
        return;

    if (s->sh_type == SHT_SYMTAB)
        m = sizeof(ElfW(Sym));
    else if (s->sh_type == SHT_RELX)
        m = sizeof(ElfW_Rel);
    else
        m = 16;

    fprintf(f, "%-8s", "offset");
    for (i = 0; i < m; ++i)
        fprintf(f, " %02x", i);
    n = 56;

    if (s->sh_type == SHT_SYMTAB || s->sh_type == SHT_RELX) {
        const char *fields1[] = {
            "name",
            "value",
            "size",
            "bind",
            "type",
            "other",
            "shndx",
            NULL
        };

        const char *fields2[] = {
            "offs",
            "type",
            "symb",
            NULL
        };

        const char **p;

        if (s->sh_type == SHT_SYMTAB)
            p = fields1, n = 106;
        else
            p = fields2, n = 58;

        for (i = 0; p[i]; ++i)
            fprintf(f, "%6s", p[i]);
        fprintf(f, "  symbol");
    }

    fprintf(f, "\n");
    for (i = 0; i < n; ++i)
        fprintf(f, "-");
    fprintf(f, "\n");

    for (i = 0; i < l;)
    {
        fprintf(f, "%08X", i);
        for (n = 0; n < m; ++n) {
            if (n + i < l)
                fprintf(f, " %02X", p[i + n]);
            else
                fprintf(f, "   ");
        }

        if (s->sh_type == SHT_SYMTAB) {
            ElfW(Sym) *sym = (ElfW(Sym) *) (p + i);
            const char *name = s->link->data + sym->st_name;
            fprintf(f, "  %04X  %04X  %04X   %02X    %02X    %02X   %04X  \"%s\"",
                    (unsigned)sym->st_name,
                    (unsigned)sym->st_value,
                    (unsigned)sym->st_size,
                    (unsigned)ELFW(ST_BIND)(sym->st_info),
                    (unsigned)ELFW(ST_TYPE)(sym->st_info),
                    (unsigned)sym->st_other,
                    (unsigned)sym->st_shndx,
                    name);

        } else if (s->sh_type == SHT_RELX) {
            ElfW_Rel *rel = (ElfW_Rel *) (p + i);
            ElfW(Sym) *sym =
                (ElfW(Sym) *) s->link->data + ELFW(R_SYM)(rel->r_info);
            const char *name = s->link->link->data + sym->st_name;
            fprintf(f, "  %04X   %02X   %04X  \"%s\"",
                    (unsigned)rel->r_offset,
                    (unsigned)ELFW(R_TYPE)(rel->r_info),
                    (unsigned)ELFW(R_SYM)(rel->r_info),
                    name);
        } else {
            fprintf(f, "   ");
            for (n = 0; n < m; ++n) {
                if (n + i < l) {
                    b = p[i + n];
                    if (b < 32 || b >= 127)
                        b = '.';
                    fprintf(f, "%c", b);
                }
            }
        }
        i += m;
        fprintf(f, "\n");
    }
    fprintf(f, "\n\n");
}

static void bf_print_sections(TCCState *s1, const char *fname)
{
    Section *s;
    FILE *f;
    int i;
    f = fopen(fname, "w");
    for (i = 1; i < s1->nb_sections; ++i) {
        s = s1->sections[i];
        bf_print_section(f, s);
    }
    bf_print_section(f, s1->dynsymtab_section);
    fclose(f);
}

static int bf_assign_addresses (struct bf_info *bf)
{
    int i, k, o, c;
    DWORD addr;
    int *section_order;
    struct section_info *si;
    Section *s;

    section_order = tcc_malloc(bf->s1->nb_sections * sizeof (int));
    for (o = k = 0 ; k < sec_last; ++k) {
        for (i = 1; i < bf->s1->nb_sections; ++i) {
            s = bf->s1->sections[i];
            if (k == bf_section_class(s)) {
                // printf("%s %d\n", s->name, k);
                s->sh_addr = bf->imagebase;
                section_order[o++] = i;
            }
        }
    }

    bf->sec_info = tcc_mallocz(o * sizeof (struct section_info));
    addr = bf->imagebase + 1;

    for (i = 0; i < o; ++i)
    {
        k = section_order[i];
        s = bf->s1->sections[k];
        c = bf_section_class(s);
        si = &bf->sec_info[bf->sec_count];
        
        printf("\nTCCBF: Section '%s' to class %d at 0x%x\n", s->name, c, addr);

        if (c == sec_stab && 0 == bf->s1->do_debug)
            continue;

        strcpy(si->name, s->name);
        si->cls = c;
        si->ord = k;
        si->sh_addr = s->sh_addr = addr = bf_virtual_align(bf, addr);
        si->sh_flags = s->sh_flags;

        if (s->data_offset)
        {
            if (s->sh_type != SHT_NOBITS) {
                si->data = s->data;
                si->data_size = s->data_offset;
            }

            addr += s->data_offset;
            si->sh_size = s->data_offset;
            ++bf->sec_count;
        }
        // printf("%08x %05x %s\n", si->sh_addr, si->sh_size, si->name);
    }

    tcc_free(section_order);
    return 0;
}


/*----------------------------------------------------------------------------*/
static int bf_check_symbols(struct bf_info *bf)
{
    TCCState *s1 = bf->s1;

    ElfW(Sym) *sym;
    int sym_index, sym_end;
    int ret = 0;

    sym_end = symtab_section->data_offset / sizeof(ElfW(Sym));
    for (sym_index = 1; sym_index < sym_end; ++sym_index) {

        sym = (ElfW(Sym) *)symtab_section->data + sym_index;

        const char *name = (char*)symtab_section->link->data + sym->st_name;
        unsigned type = ELFW(ST_TYPE)(sym->st_info);
        
        printf("\nTCCBF: Checking symbol '%s' = 0x%x!\n", name, type);

        if (sym->st_shndx == SHN_UNDEF) {

            int imp_sym = bf_find_import(bf->s1, sym);

            if (imp_sym <= 0)
            {
                if (ELFW(ST_BIND)(sym->st_info) == STB_WEAK)
                    /* STB_WEAK undefined symbols are accepted */
                    continue;
                tcc_error_noabort("undefined symbol '%s'%s", name,
                    imp_sym < 0 ? ", missing __declspec(dllimport)?":"");
                ret = -1;
            }
            else
            {
                printf("\nTCCBF: Importing symbol '%s'\n", name);
            }
        }
    }
    ret = 0;
    return ret;
}

static int bf_print_comment(FILE *op, char *p_format, ...)
{
    int i, size = 0;
    va_list ap;
    char buffer[256];
    va_start(ap, p_format);
    size = vsnprintf(buffer, sizeof(buffer), p_format, ap);
    for (i = 0; i < size; ++i)
    {
        switch(buffer[i])
        {
        case '<':            
        case '>':            
        case '+':            
        case '-':            
        case '.':            
        case ',':            
        case '[':
        case ']':            
        case '#':            
        case '@':            
        case '!':            
            buffer[i] = '_';
        default:
            break;
        }
    }
    va_end(ap);
    
    fwrite(buffer, size, 1, op);
    return 0;
}

static int bf_emit_code(FILE *op, const char *p_comment, char *p_format, ...)
{
    int size = 0;
    va_list ap;
    char buffer[256];
    va_start(ap, p_format);
    size = vsnprintf(buffer, sizeof(buffer), p_format, ap);
    va_end(ap);
    
    if (p_comment)
    {
        bf_print_comment(op, "\n%s\n", p_comment);
    }
    fwrite(buffer, size, 1, op);
    return 0;
}

/*----------------------------------------------------------------------------*/
static int bf_write(struct bf_info *bf)
{
    TCCState *s1 = bf->s1;

    int i;
    FILE *op;
    DWORD file_offset = 0;
    struct section_info *si;

    op = fopen(bf->filename, "wb");
    if (NULL == op) {
        tcc_error_noabort("could not write '%s': %s", bf->filename, strerror(errno));
        return -1;
    }

    if (2 == bf->s1->verbose)
        printf("-------------------------------"
               "\n  virt   file   size  section" "\n");
    for (i = 0; i < bf->sec_count; ++i) {
        DWORD addr, size;
        const char *sh_name;

        si = bf->sec_info + i;
        sh_name = si->name;
        addr = si->sh_addr - bf->imagebase;
        size = si->sh_size;

        if (2 == bf->s1->verbose)
            printf("%6x %6x %6x  %s\n",
                (unsigned)addr, (unsigned)file_offset, (unsigned)size, sh_name);
    }

    bf_print_comment(op, "\nThis is BF program %s\n", bf->filename);
    bf_emit_code(op, "Hello from BF", 
        "\n+++++++   ++[>++++"
        "\n++    ++  >+"
        "\n++    ++  ++"
        "\n+++++<<   -]>--"
        "\n--    .>  ++"
        "\n+.    .-  -."
        "\n<--.+++   +.");

    if (2 == bf->s1->verbose)
        printf("-------------------------------\n");
    if (bf->s1->verbose)
        printf("<- %s (%u bytes)\n", bf->filename, (unsigned)file_offset);

    return 0;
}

ST_FUNC int bf_load_file(struct TCCState *s1, const char *filename, int fd)
{
    printf("\nTCCBF:%s:%d: not implemented\n", __FILE__, __LINE__);
    return -1;
}

ST_FUNC int bf_output_file(TCCState * s1, const char *filename)
{
    int ret;
    struct bf_info bf;
    int i;

    memset(&bf, 0, sizeof bf);
    bf.filename = filename;
    bf.s1 = s1;
    bf.section_align = 16;

    tcc_add_bcheck(s1);
    bf_add_runtime(s1, &bf);
    resolve_common_syms(s1);
    bf_set_options(s1, &bf);

    ret = bf_check_symbols(&bf);
    if (ret)
        ;
    else if (filename) {
        bf_assign_addresses(&bf);
        relocate_syms(s1, s1->symtab, 0);
        for (i = 1; i < s1->nb_sections; ++i) {
            Section *s = s1->sections[i];
            if (s->reloc) {
                relocate_section(s1, s);
            }
        }
        bf.start_addr = (DWORD)
            ((uintptr_t)tcc_get_symbol_err(s1, bf.start_symbol)
                - bf.imagebase);
        //if (s1->nb_errors)
        //    ret = -1;
        //else
            ret = bf_write(&bf);
        tcc_free(bf.sec_info);
    }

    bf_print_sections(s1, "tccbf.log");
    return ret;
}

ST_FUNC int bf_putimport(TCCState *s1, int dllindex, const char *name, addr_t value)
{
    printf("\nTCCBF:%s:%d: not implemented\n", __FILE__, __LINE__);
    return 0;
}

ST_FUNC SValue *bf_getimport(SValue *sv, SValue *v2)
{
    printf("\nTCCBF:%s:%d: not implemented\n", __FILE__, __LINE__);
    return NULL;
}


