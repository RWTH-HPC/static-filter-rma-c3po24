# 1 "neighbor.cpp"
# 1 "<built-in>" 1
# 1 "<built-in>" 3
# 446 "<built-in>" 3
# 1 "<command line>" 1
# 1 "<built-in>" 2
# 1 "neighbor.cpp" 2
# 32 "neighbor.cpp"
# 1 "/usr/include/stdio.h" 1 3 4
# 27 "/usr/include/stdio.h" 3 4
# 1 "/usr/include/bits/libc-header-start.h" 1 3 4
# 33 "/usr/include/bits/libc-header-start.h" 3 4
# 1 "/usr/include/features.h" 1 3 4
# 416 "/usr/include/features.h" 3 4
# 1 "/usr/include/stdc-predef.h" 1 3 4
# 417 "/usr/include/features.h" 2 3 4
# 438 "/usr/include/features.h" 3 4
# 1 "/usr/include/sys/cdefs.h" 1 3 4
# 499 "/usr/include/sys/cdefs.h" 3 4
# 1 "/usr/include/bits/wordsize.h" 1 3 4
# 500 "/usr/include/sys/cdefs.h" 2 3 4
# 1 "/usr/include/bits/long-double.h" 1 3 4
# 501 "/usr/include/sys/cdefs.h" 2 3 4
# 439 "/usr/include/features.h" 2 3 4
# 462 "/usr/include/features.h" 3 4
# 1 "/usr/include/gnu/stubs.h" 1 3 4
# 10 "/usr/include/gnu/stubs.h" 3 4
# 1 "/usr/include/gnu/stubs-64.h" 1 3 4
# 11 "/usr/include/gnu/stubs.h" 2 3 4
# 463 "/usr/include/features.h" 2 3 4
# 34 "/usr/include/bits/libc-header-start.h" 2 3 4
# 28 "/usr/include/stdio.h" 2 3 4

extern "C" {



# 1 "/rwthfs/rz/cluster/work/rwth1269/software/c23/classic-flang/lib/clang/16/include/stddef.h" 1 3 4
# 46 "/rwthfs/rz/cluster/work/rwth1269/software/c23/classic-flang/lib/clang/16/include/stddef.h" 3 4
typedef long unsigned int size_t;
# 34 "/usr/include/stdio.h" 2 3 4


# 1 "/rwthfs/rz/cluster/work/rwth1269/software/c23/classic-flang/lib/clang/16/include/stdarg.h" 1 3 4
# 14 "/rwthfs/rz/cluster/work/rwth1269/software/c23/classic-flang/lib/clang/16/include/stdarg.h" 3 4
typedef __builtin_va_list __gnuc_va_list;
# 37 "/usr/include/stdio.h" 2 3 4

# 1 "/usr/include/bits/types.h" 1 3 4
# 27 "/usr/include/bits/types.h" 3 4
# 1 "/usr/include/bits/wordsize.h" 1 3 4
# 28 "/usr/include/bits/types.h" 2 3 4


typedef unsigned char __u_char;
typedef unsigned short int __u_short;
typedef unsigned int __u_int;
typedef unsigned long int __u_long;


typedef signed char __int8_t;
typedef unsigned char __uint8_t;
typedef signed short int __int16_t;
typedef unsigned short int __uint16_t;
typedef signed int __int32_t;
typedef unsigned int __uint32_t;

typedef signed long int __int64_t;
typedef unsigned long int __uint64_t;






typedef __int8_t __int_least8_t;
typedef __uint8_t __uint_least8_t;
typedef __int16_t __int_least16_t;
typedef __uint16_t __uint_least16_t;
typedef __int32_t __int_least32_t;
typedef __uint32_t __uint_least32_t;
typedef __int64_t __int_least64_t;
typedef __uint64_t __uint_least64_t;



typedef long int __quad_t;
typedef unsigned long int __u_quad_t;







typedef long int __intmax_t;
typedef unsigned long int __uintmax_t;
# 140 "/usr/include/bits/types.h" 3 4
# 1 "/usr/include/bits/typesizes.h" 1 3 4
# 141 "/usr/include/bits/types.h" 2 3 4


typedef unsigned long int __dev_t;
typedef unsigned int __uid_t;
typedef unsigned int __gid_t;
typedef unsigned long int __ino_t;
typedef unsigned long int __ino64_t;
typedef unsigned int __mode_t;
typedef unsigned long int __nlink_t;
typedef long int __off_t;
typedef long int __off64_t;
typedef int __pid_t;
typedef struct { int __val[2]; } __fsid_t;
typedef long int __clock_t;
typedef unsigned long int __rlim_t;
typedef unsigned long int __rlim64_t;
typedef unsigned int __id_t;
typedef long int __time_t;
typedef unsigned int __useconds_t;
typedef long int __suseconds_t;

typedef int __daddr_t;
typedef int __key_t;


typedef int __clockid_t;


typedef void * __timer_t;


typedef long int __blksize_t;




typedef long int __blkcnt_t;
typedef long int __blkcnt64_t;


typedef unsigned long int __fsblkcnt_t;
typedef unsigned long int __fsblkcnt64_t;


typedef unsigned long int __fsfilcnt_t;
typedef unsigned long int __fsfilcnt64_t;


typedef long int __fsword_t;

typedef long int __ssize_t;


typedef long int __syscall_slong_t;

typedef unsigned long int __syscall_ulong_t;



typedef __off64_t __loff_t;
typedef char *__caddr_t;


typedef long int __intptr_t;


typedef unsigned int __socklen_t;




typedef int __sig_atomic_t;
# 39 "/usr/include/stdio.h" 2 3 4
# 1 "/usr/include/bits/types/__fpos_t.h" 1 3 4




# 1 "/usr/include/bits/types/__mbstate_t.h" 1 3 4
# 13 "/usr/include/bits/types/__mbstate_t.h" 3 4
typedef struct
{
  int __count;
  union
  {
    unsigned int __wch;
    char __wchb[4];
  } __value;
} __mbstate_t;
# 6 "/usr/include/bits/types/__fpos_t.h" 2 3 4




typedef struct _G_fpos_t
{
  __off_t __pos;
  __mbstate_t __state;
} __fpos_t;
# 40 "/usr/include/stdio.h" 2 3 4
# 1 "/usr/include/bits/types/__fpos64_t.h" 1 3 4
# 10 "/usr/include/bits/types/__fpos64_t.h" 3 4
typedef struct _G_fpos64_t
{
  __off64_t __pos;
  __mbstate_t __state;
} __fpos64_t;
# 41 "/usr/include/stdio.h" 2 3 4
# 1 "/usr/include/bits/types/__FILE.h" 1 3 4



struct _IO_FILE;
typedef struct _IO_FILE __FILE;
# 42 "/usr/include/stdio.h" 2 3 4
# 1 "/usr/include/bits/types/FILE.h" 1 3 4



struct _IO_FILE;


typedef struct _IO_FILE FILE;
# 43 "/usr/include/stdio.h" 2 3 4
# 1 "/usr/include/bits/types/struct_FILE.h" 1 3 4
# 35 "/usr/include/bits/types/struct_FILE.h" 3 4
struct _IO_FILE;
struct _IO_marker;
struct _IO_codecvt;
struct _IO_wide_data;




typedef void _IO_lock_t;





struct _IO_FILE
{
  int _flags;


  char *_IO_read_ptr;
  char *_IO_read_end;
  char *_IO_read_base;
  char *_IO_write_base;
  char *_IO_write_ptr;
  char *_IO_write_end;
  char *_IO_buf_base;
  char *_IO_buf_end;


  char *_IO_save_base;
  char *_IO_backup_base;
  char *_IO_save_end;

  struct _IO_marker *_markers;

  struct _IO_FILE *_chain;

  int _fileno;
  int _flags2;
  __off_t _old_offset;


  unsigned short _cur_column;
  signed char _vtable_offset;
  char _shortbuf[1];

  _IO_lock_t *_lock;







  __off64_t _offset;

  struct _IO_codecvt *_codecvt;
  struct _IO_wide_data *_wide_data;
  struct _IO_FILE *_freeres_list;
  void *_freeres_buf;
  size_t __pad5;
  int _mode;

  char _unused2[15 * sizeof (int) - 4 * sizeof (void *) - sizeof (size_t)];
};
# 44 "/usr/include/stdio.h" 2 3 4


# 1 "/usr/include/bits/types/cookie_io_functions_t.h" 1 3 4
# 27 "/usr/include/bits/types/cookie_io_functions_t.h" 3 4
typedef __ssize_t cookie_read_function_t (void *__cookie, char *__buf,
                                          size_t __nbytes);







typedef __ssize_t cookie_write_function_t (void *__cookie, const char *__buf,
                                           size_t __nbytes);







typedef int cookie_seek_function_t (void *__cookie, __off64_t *__pos, int __w);


typedef int cookie_close_function_t (void *__cookie);






typedef struct _IO_cookie_io_functions_t
{
  cookie_read_function_t *read;
  cookie_write_function_t *write;
  cookie_seek_function_t *seek;
  cookie_close_function_t *close;
} cookie_io_functions_t;
# 47 "/usr/include/stdio.h" 2 3 4





typedef __gnuc_va_list va_list;
# 63 "/usr/include/stdio.h" 3 4
typedef __off_t off_t;






typedef __off64_t off64_t;






typedef __ssize_t ssize_t;






typedef __fpos_t fpos_t;




typedef __fpos64_t fpos64_t;
# 133 "/usr/include/stdio.h" 3 4
# 1 "/usr/include/bits/stdio_lim.h" 1 3 4
# 134 "/usr/include/stdio.h" 2 3 4



extern FILE *stdin;
extern FILE *stdout;
extern FILE *stderr;






extern int remove (const char *__filename) throw ();

extern int rename (const char *__old, const char *__new) throw ();



extern int renameat (int __oldfd, const char *__old, int __newfd,
       const char *__new) throw ();
# 164 "/usr/include/stdio.h" 3 4
extern int renameat2 (int __oldfd, const char *__old, int __newfd,
        const char *__new, unsigned int __flags) throw ();







extern FILE *tmpfile (void) ;
# 183 "/usr/include/stdio.h" 3 4
extern FILE *tmpfile64 (void) ;



extern char *tmpnam (char *__s) throw () ;




extern char *tmpnam_r (char *__s) throw () ;
# 204 "/usr/include/stdio.h" 3 4
extern char *tempnam (const char *__dir, const char *__pfx)
     throw () __attribute__ ((__malloc__)) ;







extern int fclose (FILE *__stream);




extern int fflush (FILE *__stream);
# 227 "/usr/include/stdio.h" 3 4
extern int fflush_unlocked (FILE *__stream);
# 237 "/usr/include/stdio.h" 3 4
extern int fcloseall (void);
# 246 "/usr/include/stdio.h" 3 4
extern FILE *fopen (const char *__restrict __filename,
      const char *__restrict __modes) ;




extern FILE *freopen (const char *__restrict __filename,
        const char *__restrict __modes,
        FILE *__restrict __stream) ;
# 270 "/usr/include/stdio.h" 3 4
extern FILE *fopen64 (const char *__restrict __filename,
        const char *__restrict __modes) ;
extern FILE *freopen64 (const char *__restrict __filename,
   const char *__restrict __modes,
   FILE *__restrict __stream) ;




extern FILE *fdopen (int __fd, const char *__modes) throw () ;





extern FILE *fopencookie (void *__restrict __magic_cookie,
     const char *__restrict __modes,
     cookie_io_functions_t __io_funcs) throw () ;




extern FILE *fmemopen (void *__s, size_t __len, const char *__modes)
  throw () ;




extern FILE *open_memstream (char **__bufloc, size_t *__sizeloc) throw () ;





extern void setbuf (FILE *__restrict __stream, char *__restrict __buf) throw ();



extern int setvbuf (FILE *__restrict __stream, char *__restrict __buf,
      int __modes, size_t __n) throw ();




extern void setbuffer (FILE *__restrict __stream, char *__restrict __buf,
         size_t __size) throw ();


extern void setlinebuf (FILE *__stream) throw ();







extern int fprintf (FILE *__restrict __stream,
      const char *__restrict __format, ...);




extern int printf (const char *__restrict __format, ...);

extern int sprintf (char *__restrict __s,
      const char *__restrict __format, ...) throw ();





extern int vfprintf (FILE *__restrict __s, const char *__restrict __format,
       __gnuc_va_list __arg);




extern int vprintf (const char *__restrict __format, __gnuc_va_list __arg);

extern int vsprintf (char *__restrict __s, const char *__restrict __format,
       __gnuc_va_list __arg) throw ();



extern int snprintf (char *__restrict __s, size_t __maxlen,
       const char *__restrict __format, ...)
     throw () __attribute__ ((__format__ (__printf__, 3, 4)));

extern int vsnprintf (char *__restrict __s, size_t __maxlen,
        const char *__restrict __format, __gnuc_va_list __arg)
     throw () __attribute__ ((__format__ (__printf__, 3, 0)));





extern int vasprintf (char **__restrict __ptr, const char *__restrict __f,
        __gnuc_va_list __arg)
     throw () __attribute__ ((__format__ (__printf__, 2, 0))) ;
extern int __asprintf (char **__restrict __ptr,
         const char *__restrict __fmt, ...)
     throw () __attribute__ ((__format__ (__printf__, 2, 3))) ;
extern int asprintf (char **__restrict __ptr,
       const char *__restrict __fmt, ...)
     throw () __attribute__ ((__format__ (__printf__, 2, 3))) ;




extern int vdprintf (int __fd, const char *__restrict __fmt,
       __gnuc_va_list __arg)
     __attribute__ ((__format__ (__printf__, 2, 0)));
extern int dprintf (int __fd, const char *__restrict __fmt, ...)
     __attribute__ ((__format__ (__printf__, 2, 3)));







extern int fscanf (FILE *__restrict __stream,
     const char *__restrict __format, ...) ;




extern int scanf (const char *__restrict __format, ...) ;

extern int sscanf (const char *__restrict __s,
     const char *__restrict __format, ...) throw ();
# 434 "/usr/include/stdio.h" 3 4
extern int vfscanf (FILE *__restrict __s, const char *__restrict __format,
      __gnuc_va_list __arg)
     __attribute__ ((__format__ (__scanf__, 2, 0))) ;





extern int vscanf (const char *__restrict __format, __gnuc_va_list __arg)
     __attribute__ ((__format__ (__scanf__, 1, 0))) ;


extern int vsscanf (const char *__restrict __s,
      const char *__restrict __format, __gnuc_va_list __arg)
     throw () __attribute__ ((__format__ (__scanf__, 2, 0)));
# 491 "/usr/include/stdio.h" 3 4
extern int fgetc (FILE *__stream);
extern int getc (FILE *__stream);





extern int getchar (void);






extern int getc_unlocked (FILE *__stream);
extern int getchar_unlocked (void);
# 516 "/usr/include/stdio.h" 3 4
extern int fgetc_unlocked (FILE *__stream);
# 527 "/usr/include/stdio.h" 3 4
extern int fputc (int __c, FILE *__stream);
extern int putc (int __c, FILE *__stream);





extern int putchar (int __c);
# 543 "/usr/include/stdio.h" 3 4
extern int fputc_unlocked (int __c, FILE *__stream);







extern int putc_unlocked (int __c, FILE *__stream);
extern int putchar_unlocked (int __c);






extern int getw (FILE *__stream);


extern int putw (int __w, FILE *__stream);







extern char *fgets (char *__restrict __s, int __n, FILE *__restrict __stream)
          ;
# 593 "/usr/include/stdio.h" 3 4
extern char *fgets_unlocked (char *__restrict __s, int __n,
        FILE *__restrict __stream) ;
# 609 "/usr/include/stdio.h" 3 4
extern __ssize_t __getdelim (char **__restrict __lineptr,
                             size_t *__restrict __n, int __delimiter,
                             FILE *__restrict __stream) ;
extern __ssize_t getdelim (char **__restrict __lineptr,
                           size_t *__restrict __n, int __delimiter,
                           FILE *__restrict __stream) ;







extern __ssize_t getline (char **__restrict __lineptr,
                          size_t *__restrict __n,
                          FILE *__restrict __stream) ;







extern int fputs (const char *__restrict __s, FILE *__restrict __stream);





extern int puts (const char *__s);






extern int ungetc (int __c, FILE *__stream);






extern size_t fread (void *__restrict __ptr, size_t __size,
       size_t __n, FILE *__restrict __stream) ;




extern size_t fwrite (const void *__restrict __ptr, size_t __size,
        size_t __n, FILE *__restrict __s);
# 668 "/usr/include/stdio.h" 3 4
extern int fputs_unlocked (const char *__restrict __s,
      FILE *__restrict __stream);
# 679 "/usr/include/stdio.h" 3 4
extern size_t fread_unlocked (void *__restrict __ptr, size_t __size,
         size_t __n, FILE *__restrict __stream) ;
extern size_t fwrite_unlocked (const void *__restrict __ptr, size_t __size,
          size_t __n, FILE *__restrict __stream);







extern int fseek (FILE *__stream, long int __off, int __whence);




extern long int ftell (FILE *__stream) ;




extern void rewind (FILE *__stream);
# 713 "/usr/include/stdio.h" 3 4
extern int fseeko (FILE *__stream, __off_t __off, int __whence);




extern __off_t ftello (FILE *__stream) ;
# 737 "/usr/include/stdio.h" 3 4
extern int fgetpos (FILE *__restrict __stream, fpos_t *__restrict __pos);




extern int fsetpos (FILE *__stream, const fpos_t *__pos);
# 756 "/usr/include/stdio.h" 3 4
extern int fseeko64 (FILE *__stream, __off64_t __off, int __whence);
extern __off64_t ftello64 (FILE *__stream) ;
extern int fgetpos64 (FILE *__restrict __stream, fpos64_t *__restrict __pos);
extern int fsetpos64 (FILE *__stream, const fpos64_t *__pos);



extern void clearerr (FILE *__stream) throw ();

extern int feof (FILE *__stream) throw () ;

extern int ferror (FILE *__stream) throw () ;



extern void clearerr_unlocked (FILE *__stream) throw ();
extern int feof_unlocked (FILE *__stream) throw () ;
extern int ferror_unlocked (FILE *__stream) throw () ;







extern void perror (const char *__s);





# 1 "/usr/include/bits/sys_errlist.h" 1 3 4
# 26 "/usr/include/bits/sys_errlist.h" 3 4
extern int sys_nerr;
extern const char *const sys_errlist[];


extern int _sys_nerr;
extern const char *const _sys_errlist[];
# 788 "/usr/include/stdio.h" 2 3 4




extern int fileno (FILE *__stream) throw () ;




extern int fileno_unlocked (FILE *__stream) throw () ;
# 806 "/usr/include/stdio.h" 3 4
extern FILE *popen (const char *__command, const char *__modes) ;





extern int pclose (FILE *__stream);





extern char *ctermid (char *__s) throw ();





extern char *cuserid (char *__s);




struct obstack;


extern int obstack_printf (struct obstack *__restrict __obstack,
      const char *__restrict __format, ...)
     throw () __attribute__ ((__format__ (__printf__, 2, 3)));
extern int obstack_vprintf (struct obstack *__restrict __obstack,
       const char *__restrict __format,
       __gnuc_va_list __args)
     throw () __attribute__ ((__format__ (__printf__, 2, 0)));







extern void flockfile (FILE *__stream) throw ();



extern int ftrylockfile (FILE *__stream) throw () ;


extern void funlockfile (FILE *__stream) throw ();
# 864 "/usr/include/stdio.h" 3 4
extern int __uflow (FILE *);
extern int __overflow (FILE *, int);




# 1 "/usr/include/bits/stdio.h" 1 3 4
# 38 "/usr/include/bits/stdio.h" 3 4
extern __inline __attribute__ ((__gnu_inline__)) int
vprintf (const char *__restrict __fmt, __gnuc_va_list __arg)
{
  return vfprintf (stdout, __fmt, __arg);
}



extern __inline __attribute__ ((__gnu_inline__)) int
getchar (void)
{
  return getc (stdin);
}




extern __inline __attribute__ ((__gnu_inline__)) int
fgetc_unlocked (FILE *__fp)
{
  return (__builtin_expect (((__fp)->_IO_read_ptr >= (__fp)->_IO_read_end), 0) ? __uflow (__fp) : *(unsigned char *) (__fp)->_IO_read_ptr++);
}





extern __inline __attribute__ ((__gnu_inline__)) int
getc_unlocked (FILE *__fp)
{
  return (__builtin_expect (((__fp)->_IO_read_ptr >= (__fp)->_IO_read_end), 0) ? __uflow (__fp) : *(unsigned char *) (__fp)->_IO_read_ptr++);
}


extern __inline __attribute__ ((__gnu_inline__)) int
getchar_unlocked (void)
{
  return (__builtin_expect (((stdin)->_IO_read_ptr >= (stdin)->_IO_read_end), 0) ? __uflow (stdin) : *(unsigned char *) (stdin)->_IO_read_ptr++);
}




extern __inline __attribute__ ((__gnu_inline__)) int
putchar (int __c)
{
  return putc (__c, stdout);
}




extern __inline __attribute__ ((__gnu_inline__)) int
fputc_unlocked (int __c, FILE *__stream)
{
  return (__builtin_expect (((__stream)->_IO_write_ptr >= (__stream)->_IO_write_end), 0) ? __overflow (__stream, (unsigned char) (__c)) : (unsigned char) (*(__stream)->_IO_write_ptr++ = (__c)));
}





extern __inline __attribute__ ((__gnu_inline__)) int
putc_unlocked (int __c, FILE *__stream)
{
  return (__builtin_expect (((__stream)->_IO_write_ptr >= (__stream)->_IO_write_end), 0) ? __overflow (__stream, (unsigned char) (__c)) : (unsigned char) (*(__stream)->_IO_write_ptr++ = (__c)));
}


extern __inline __attribute__ ((__gnu_inline__)) int
putchar_unlocked (int __c)
{
  return (__builtin_expect (((stdout)->_IO_write_ptr >= (stdout)->_IO_write_end), 0) ? __overflow (stdout, (unsigned char) (__c)) : (unsigned char) (*(stdout)->_IO_write_ptr++ = (__c)));
}





extern __inline __attribute__ ((__gnu_inline__)) __ssize_t
getline (char **__lineptr, size_t *__n, FILE *__stream)
{
  return __getdelim (__lineptr, __n, '\n', __stream);
}





extern __inline __attribute__ ((__gnu_inline__)) int
 feof_unlocked (FILE *__stream) throw ()
{
  return (((__stream)->_flags & 0x0010) != 0);
}


extern __inline __attribute__ ((__gnu_inline__)) int
 ferror_unlocked (FILE *__stream) throw ()
{
  return (((__stream)->_flags & 0x0020) != 0);
}
# 871 "/usr/include/stdio.h" 2 3 4








}
# 33 "neighbor.cpp" 2
# 1 "/cvmfs/software.hpc.rwth.de/Linux/RH8/x86_64/intel/sapphirerapids/software/GCCcore/12.3.0/lib/gcc/x86_64-pc-linux-gnu/12.3.0/../../../../include/c++/12.3.0/stdlib.h" 1 3
# 36 "/cvmfs/software.hpc.rwth.de/Linux/RH8/x86_64/intel/sapphirerapids/software/GCCcore/12.3.0/lib/gcc/x86_64-pc-linux-gnu/12.3.0/../../../../include/c++/12.3.0/stdlib.h" 3
# 1 "/cvmfs/software.hpc.rwth.de/Linux/RH8/x86_64/intel/sapphirerapids/software/GCCcore/12.3.0/lib/gcc/x86_64-pc-linux-gnu/12.3.0/../../../../include/c++/12.3.0/cstdlib" 1 3
# 40 "/cvmfs/software.hpc.rwth.de/Linux/RH8/x86_64/intel/sapphirerapids/software/GCCcore/12.3.0/lib/gcc/x86_64-pc-linux-gnu/12.3.0/../../../../include/c++/12.3.0/cstdlib" 3

# 1 "/cvmfs/software.hpc.rwth.de/Linux/RH8/x86_64/intel/sapphirerapids/software/GCCcore/12.3.0/lib/gcc/x86_64-pc-linux-gnu/12.3.0/../../../../include/c++/12.3.0/x86_64-pc-linux-gnu/bits/c++config.h" 1 3
# 296 "/cvmfs/software.hpc.rwth.de/Linux/RH8/x86_64/intel/sapphirerapids/software/GCCcore/12.3.0/lib/gcc/x86_64-pc-linux-gnu/12.3.0/../../../../include/c++/12.3.0/x86_64-pc-linux-gnu/bits/c++config.h" 3
namespace std
{
  typedef long unsigned int size_t;
  typedef long int ptrdiff_t;


  typedef decltype(nullptr) nullptr_t;


#pragma GCC visibility push(default)


  extern "C++" __attribute__ ((__noreturn__, __always_inline__))
  inline void __terminate() noexcept
  {
    void terminate() noexcept __attribute__ ((__noreturn__));
    terminate();
  }
#pragma GCC visibility pop
}
# 329 "/cvmfs/software.hpc.rwth.de/Linux/RH8/x86_64/intel/sapphirerapids/software/GCCcore/12.3.0/lib/gcc/x86_64-pc-linux-gnu/12.3.0/../../../../include/c++/12.3.0/x86_64-pc-linux-gnu/bits/c++config.h" 3
namespace std
{
  inline namespace __cxx11 __attribute__((__abi_tag__ ("cxx11"))) { }
}
namespace __gnu_cxx
{
  inline namespace __cxx11 __attribute__((__abi_tag__ ("cxx11"))) { }
}
# 508 "/cvmfs/software.hpc.rwth.de/Linux/RH8/x86_64/intel/sapphirerapids/software/GCCcore/12.3.0/lib/gcc/x86_64-pc-linux-gnu/12.3.0/../../../../include/c++/12.3.0/x86_64-pc-linux-gnu/bits/c++config.h" 3
namespace std
{
#pragma GCC visibility push(default)




  constexpr inline bool
  __is_constant_evaluated() noexcept
  {





    return __builtin_is_constant_evaluated();



  }
#pragma GCC visibility pop
}
# 655 "/cvmfs/software.hpc.rwth.de/Linux/RH8/x86_64/intel/sapphirerapids/software/GCCcore/12.3.0/lib/gcc/x86_64-pc-linux-gnu/12.3.0/../../../../include/c++/12.3.0/x86_64-pc-linux-gnu/bits/c++config.h" 3
# 1 "/cvmfs/software.hpc.rwth.de/Linux/RH8/x86_64/intel/sapphirerapids/software/GCCcore/12.3.0/lib/gcc/x86_64-pc-linux-gnu/12.3.0/../../../../include/c++/12.3.0/x86_64-pc-linux-gnu/bits/os_defines.h" 1 3
# 656 "/cvmfs/software.hpc.rwth.de/Linux/RH8/x86_64/intel/sapphirerapids/software/GCCcore/12.3.0/lib/gcc/x86_64-pc-linux-gnu/12.3.0/../../../../include/c++/12.3.0/x86_64-pc-linux-gnu/bits/c++config.h" 2 3


# 1 "/cvmfs/software.hpc.rwth.de/Linux/RH8/x86_64/intel/sapphirerapids/software/GCCcore/12.3.0/lib/gcc/x86_64-pc-linux-gnu/12.3.0/../../../../include/c++/12.3.0/x86_64-pc-linux-gnu/bits/cpu_defines.h" 1 3
# 659 "/cvmfs/software.hpc.rwth.de/Linux/RH8/x86_64/intel/sapphirerapids/software/GCCcore/12.3.0/lib/gcc/x86_64-pc-linux-gnu/12.3.0/../../../../include/c++/12.3.0/x86_64-pc-linux-gnu/bits/c++config.h" 2 3
# 841 "/cvmfs/software.hpc.rwth.de/Linux/RH8/x86_64/intel/sapphirerapids/software/GCCcore/12.3.0/lib/gcc/x86_64-pc-linux-gnu/12.3.0/../../../../include/c++/12.3.0/x86_64-pc-linux-gnu/bits/c++config.h" 3
# 1 "/cvmfs/software.hpc.rwth.de/Linux/RH8/x86_64/intel/sapphirerapids/software/GCCcore/12.3.0/lib/gcc/x86_64-pc-linux-gnu/12.3.0/../../../../include/c++/12.3.0/pstl/pstl_config.h" 1 3
# 842 "/cvmfs/software.hpc.rwth.de/Linux/RH8/x86_64/intel/sapphirerapids/software/GCCcore/12.3.0/lib/gcc/x86_64-pc-linux-gnu/12.3.0/../../../../include/c++/12.3.0/x86_64-pc-linux-gnu/bits/c++config.h" 2 3
# 42 "/cvmfs/software.hpc.rwth.de/Linux/RH8/x86_64/intel/sapphirerapids/software/GCCcore/12.3.0/lib/gcc/x86_64-pc-linux-gnu/12.3.0/../../../../include/c++/12.3.0/cstdlib" 2 3
# 75 "/cvmfs/software.hpc.rwth.de/Linux/RH8/x86_64/intel/sapphirerapids/software/GCCcore/12.3.0/lib/gcc/x86_64-pc-linux-gnu/12.3.0/../../../../include/c++/12.3.0/cstdlib" 3
# 1 "/usr/include/stdlib.h" 1 3 4
# 25 "/usr/include/stdlib.h" 3 4
# 1 "/usr/include/bits/libc-header-start.h" 1 3 4
# 26 "/usr/include/stdlib.h" 2 3 4





# 1 "/rwthfs/rz/cluster/work/rwth1269/software/c23/classic-flang/lib/clang/16/include/stddef.h" 1 3 4
# 32 "/usr/include/stdlib.h" 2 3 4

extern "C" {





# 1 "/usr/include/bits/waitflags.h" 1 3 4
# 40 "/usr/include/stdlib.h" 2 3 4
# 1 "/usr/include/bits/waitstatus.h" 1 3 4
# 41 "/usr/include/stdlib.h" 2 3 4
# 55 "/usr/include/stdlib.h" 3 4
# 1 "/usr/include/bits/floatn.h" 1 3 4
# 119 "/usr/include/bits/floatn.h" 3 4
# 1 "/usr/include/bits/floatn-common.h" 1 3 4
# 24 "/usr/include/bits/floatn-common.h" 3 4
# 1 "/usr/include/bits/long-double.h" 1 3 4
# 25 "/usr/include/bits/floatn-common.h" 2 3 4
# 214 "/usr/include/bits/floatn-common.h" 3 4
typedef float _Float32;
# 251 "/usr/include/bits/floatn-common.h" 3 4
typedef double _Float64;
# 268 "/usr/include/bits/floatn-common.h" 3 4
typedef double _Float32x;
# 285 "/usr/include/bits/floatn-common.h" 3 4
typedef long double _Float64x;
# 120 "/usr/include/bits/floatn.h" 2 3 4
# 56 "/usr/include/stdlib.h" 2 3 4


typedef struct
  {
    int quot;
    int rem;
  } div_t;



typedef struct
  {
    long int quot;
    long int rem;
  } ldiv_t;





__extension__ typedef struct
  {
    long long int quot;
    long long int rem;
  } lldiv_t;
# 97 "/usr/include/stdlib.h" 3 4
extern size_t __ctype_get_mb_cur_max (void) throw () ;



extern double atof (const char *__nptr)
     throw () __attribute__ ((__pure__)) __attribute__ ((__nonnull__ (1))) ;

extern int atoi (const char *__nptr)
     throw () __attribute__ ((__pure__)) __attribute__ ((__nonnull__ (1))) ;

extern long int atol (const char *__nptr)
     throw () __attribute__ ((__pure__)) __attribute__ ((__nonnull__ (1))) ;



__extension__ extern long long int atoll (const char *__nptr)
     throw () __attribute__ ((__pure__)) __attribute__ ((__nonnull__ (1))) ;



extern double strtod (const char *__restrict __nptr,
        char **__restrict __endptr)
     throw () __attribute__ ((__nonnull__ (1)));



extern float strtof (const char *__restrict __nptr,
       char **__restrict __endptr) throw () __attribute__ ((__nonnull__ (1)));

extern long double strtold (const char *__restrict __nptr,
       char **__restrict __endptr)
     throw () __attribute__ ((__nonnull__ (1)));
# 140 "/usr/include/stdlib.h" 3 4
extern _Float32 strtof32 (const char *__restrict __nptr,
     char **__restrict __endptr)
     throw () __attribute__ ((__nonnull__ (1)));



extern _Float64 strtof64 (const char *__restrict __nptr,
     char **__restrict __endptr)
     throw () __attribute__ ((__nonnull__ (1)));
# 158 "/usr/include/stdlib.h" 3 4
extern _Float32x strtof32x (const char *__restrict __nptr,
       char **__restrict __endptr)
     throw () __attribute__ ((__nonnull__ (1)));



extern _Float64x strtof64x (const char *__restrict __nptr,
       char **__restrict __endptr)
     throw () __attribute__ ((__nonnull__ (1)));
# 176 "/usr/include/stdlib.h" 3 4
extern long int strtol (const char *__restrict __nptr,
   char **__restrict __endptr, int __base)
     throw () __attribute__ ((__nonnull__ (1)));

extern unsigned long int strtoul (const char *__restrict __nptr,
      char **__restrict __endptr, int __base)
     throw () __attribute__ ((__nonnull__ (1)));



__extension__
extern long long int strtoq (const char *__restrict __nptr,
        char **__restrict __endptr, int __base)
     throw () __attribute__ ((__nonnull__ (1)));

__extension__
extern unsigned long long int strtouq (const char *__restrict __nptr,
           char **__restrict __endptr, int __base)
     throw () __attribute__ ((__nonnull__ (1)));




__extension__
extern long long int strtoll (const char *__restrict __nptr,
         char **__restrict __endptr, int __base)
     throw () __attribute__ ((__nonnull__ (1)));

__extension__
extern unsigned long long int strtoull (const char *__restrict __nptr,
     char **__restrict __endptr, int __base)
     throw () __attribute__ ((__nonnull__ (1)));




extern int strfromd (char *__dest, size_t __size, const char *__format,
       double __f)
     throw () __attribute__ ((__nonnull__ (3)));

extern int strfromf (char *__dest, size_t __size, const char *__format,
       float __f)
     throw () __attribute__ ((__nonnull__ (3)));

extern int strfroml (char *__dest, size_t __size, const char *__format,
       long double __f)
     throw () __attribute__ ((__nonnull__ (3)));
# 232 "/usr/include/stdlib.h" 3 4
extern int strfromf32 (char *__dest, size_t __size, const char * __format,
         _Float32 __f)
     throw () __attribute__ ((__nonnull__ (3)));



extern int strfromf64 (char *__dest, size_t __size, const char * __format,
         _Float64 __f)
     throw () __attribute__ ((__nonnull__ (3)));
# 250 "/usr/include/stdlib.h" 3 4
extern int strfromf32x (char *__dest, size_t __size, const char * __format,
   _Float32x __f)
     throw () __attribute__ ((__nonnull__ (3)));



extern int strfromf64x (char *__dest, size_t __size, const char * __format,
   _Float64x __f)
     throw () __attribute__ ((__nonnull__ (3)));
# 272 "/usr/include/stdlib.h" 3 4
# 1 "/usr/include/bits/types/locale_t.h" 1 3 4
# 22 "/usr/include/bits/types/locale_t.h" 3 4
# 1 "/usr/include/bits/types/__locale_t.h" 1 3 4
# 28 "/usr/include/bits/types/__locale_t.h" 3 4
struct __locale_struct
{

  struct __locale_data *__locales[13];


  const unsigned short int *__ctype_b;
  const int *__ctype_tolower;
  const int *__ctype_toupper;


  const char *__names[13];
};

typedef struct __locale_struct *__locale_t;
# 23 "/usr/include/bits/types/locale_t.h" 2 3 4

typedef __locale_t locale_t;
# 273 "/usr/include/stdlib.h" 2 3 4

extern long int strtol_l (const char *__restrict __nptr,
     char **__restrict __endptr, int __base,
     locale_t __loc) throw () __attribute__ ((__nonnull__ (1, 4)));

extern unsigned long int strtoul_l (const char *__restrict __nptr,
        char **__restrict __endptr,
        int __base, locale_t __loc)
     throw () __attribute__ ((__nonnull__ (1, 4)));

__extension__
extern long long int strtoll_l (const char *__restrict __nptr,
    char **__restrict __endptr, int __base,
    locale_t __loc)
     throw () __attribute__ ((__nonnull__ (1, 4)));

__extension__
extern unsigned long long int strtoull_l (const char *__restrict __nptr,
       char **__restrict __endptr,
       int __base, locale_t __loc)
     throw () __attribute__ ((__nonnull__ (1, 4)));

extern double strtod_l (const char *__restrict __nptr,
   char **__restrict __endptr, locale_t __loc)
     throw () __attribute__ ((__nonnull__ (1, 3)));

extern float strtof_l (const char *__restrict __nptr,
         char **__restrict __endptr, locale_t __loc)
     throw () __attribute__ ((__nonnull__ (1, 3)));

extern long double strtold_l (const char *__restrict __nptr,
         char **__restrict __endptr,
         locale_t __loc)
     throw () __attribute__ ((__nonnull__ (1, 3)));
# 316 "/usr/include/stdlib.h" 3 4
extern _Float32 strtof32_l (const char *__restrict __nptr,
       char **__restrict __endptr,
       locale_t __loc)
     throw () __attribute__ ((__nonnull__ (1, 3)));



extern _Float64 strtof64_l (const char *__restrict __nptr,
       char **__restrict __endptr,
       locale_t __loc)
     throw () __attribute__ ((__nonnull__ (1, 3)));
# 337 "/usr/include/stdlib.h" 3 4
extern _Float32x strtof32x_l (const char *__restrict __nptr,
         char **__restrict __endptr,
         locale_t __loc)
     throw () __attribute__ ((__nonnull__ (1, 3)));



extern _Float64x strtof64x_l (const char *__restrict __nptr,
         char **__restrict __endptr,
         locale_t __loc)
     throw () __attribute__ ((__nonnull__ (1, 3)));
# 360 "/usr/include/stdlib.h" 3 4
extern __inline __attribute__ ((__gnu_inline__)) int
 atoi (const char *__nptr) throw ()
{
  return (int) strtol (__nptr, (char **) __null, 10);
}
extern __inline __attribute__ ((__gnu_inline__)) long int
 atol (const char *__nptr) throw ()
{
  return strtol (__nptr, (char **) __null, 10);
}


__extension__ extern __inline __attribute__ ((__gnu_inline__)) long long int
 atoll (const char *__nptr) throw ()
{
  return strtoll (__nptr, (char **) __null, 10);
}
# 385 "/usr/include/stdlib.h" 3 4
extern char *l64a (long int __n) throw () ;


extern long int a64l (const char *__s)
     throw () __attribute__ ((__pure__)) __attribute__ ((__nonnull__ (1))) ;




# 1 "/usr/include/sys/types.h" 1 3 4
# 27 "/usr/include/sys/types.h" 3 4
extern "C" {





typedef __u_char u_char;
typedef __u_short u_short;
typedef __u_int u_int;
typedef __u_long u_long;
typedef __quad_t quad_t;
typedef __u_quad_t u_quad_t;
typedef __fsid_t fsid_t;


typedef __loff_t loff_t;




typedef __ino_t ino_t;






typedef __ino64_t ino64_t;




typedef __dev_t dev_t;




typedef __gid_t gid_t;




typedef __mode_t mode_t;




typedef __nlink_t nlink_t;




typedef __uid_t uid_t;
# 97 "/usr/include/sys/types.h" 3 4
typedef __pid_t pid_t;





typedef __id_t id_t;
# 114 "/usr/include/sys/types.h" 3 4
typedef __daddr_t daddr_t;
typedef __caddr_t caddr_t;





typedef __key_t key_t;




# 1 "/usr/include/bits/types/clock_t.h" 1 3 4






typedef __clock_t clock_t;
# 127 "/usr/include/sys/types.h" 2 3 4

# 1 "/usr/include/bits/types/clockid_t.h" 1 3 4






typedef __clockid_t clockid_t;
# 129 "/usr/include/sys/types.h" 2 3 4
# 1 "/usr/include/bits/types/time_t.h" 1 3 4






typedef __time_t time_t;
# 130 "/usr/include/sys/types.h" 2 3 4
# 1 "/usr/include/bits/types/timer_t.h" 1 3 4






typedef __timer_t timer_t;
# 131 "/usr/include/sys/types.h" 2 3 4



typedef __useconds_t useconds_t;



typedef __suseconds_t suseconds_t;





# 1 "/rwthfs/rz/cluster/work/rwth1269/software/c23/classic-flang/lib/clang/16/include/stddef.h" 1 3 4
# 145 "/usr/include/sys/types.h" 2 3 4



typedef unsigned long int ulong;
typedef unsigned short int ushort;
typedef unsigned int uint;




# 1 "/usr/include/bits/stdint-intn.h" 1 3 4
# 24 "/usr/include/bits/stdint-intn.h" 3 4
typedef __int8_t int8_t;
typedef __int16_t int16_t;
typedef __int32_t int32_t;
typedef __int64_t int64_t;
# 156 "/usr/include/sys/types.h" 2 3 4


typedef __uint8_t u_int8_t;
typedef __uint16_t u_int16_t;
typedef __uint32_t u_int32_t;
typedef __uint64_t u_int64_t;


typedef int register_t __attribute__ ((__mode__ (__word__)));
# 176 "/usr/include/sys/types.h" 3 4
# 1 "/usr/include/endian.h" 1 3 4
# 36 "/usr/include/endian.h" 3 4
# 1 "/usr/include/bits/endian.h" 1 3 4
# 37 "/usr/include/endian.h" 2 3 4
# 60 "/usr/include/endian.h" 3 4
# 1 "/usr/include/bits/byteswap.h" 1 3 4
# 33 "/usr/include/bits/byteswap.h" 3 4
static __inline __uint16_t
__bswap_16 (__uint16_t __bsx)
{



  return ((__uint16_t) ((((__bsx) >> 8) & 0xff) | (((__bsx) & 0xff) << 8)));

}






static __inline __uint32_t
__bswap_32 (__uint32_t __bsx)
{



  return ((((__bsx) & 0xff000000u) >> 24) | (((__bsx) & 0x00ff0000u) >> 8) | (((__bsx) & 0x0000ff00u) << 8) | (((__bsx) & 0x000000ffu) << 24));

}
# 69 "/usr/include/bits/byteswap.h" 3 4
__extension__ static __inline __uint64_t
__bswap_64 (__uint64_t __bsx)
{



  return ((((__bsx) & 0xff00000000000000ull) >> 56) | (((__bsx) & 0x00ff000000000000ull) >> 40) | (((__bsx) & 0x0000ff0000000000ull) >> 24) | (((__bsx) & 0x000000ff00000000ull) >> 8) | (((__bsx) & 0x00000000ff000000ull) << 8) | (((__bsx) & 0x0000000000ff0000ull) << 24) | (((__bsx) & 0x000000000000ff00ull) << 40) | (((__bsx) & 0x00000000000000ffull) << 56));

}
# 61 "/usr/include/endian.h" 2 3 4
# 1 "/usr/include/bits/uintn-identity.h" 1 3 4
# 32 "/usr/include/bits/uintn-identity.h" 3 4
static __inline __uint16_t
__uint16_identity (__uint16_t __x)
{
  return __x;
}

static __inline __uint32_t
__uint32_identity (__uint32_t __x)
{
  return __x;
}

static __inline __uint64_t
__uint64_identity (__uint64_t __x)
{
  return __x;
}
# 62 "/usr/include/endian.h" 2 3 4
# 177 "/usr/include/sys/types.h" 2 3 4


# 1 "/usr/include/sys/select.h" 1 3 4
# 30 "/usr/include/sys/select.h" 3 4
# 1 "/usr/include/bits/select.h" 1 3 4
# 22 "/usr/include/bits/select.h" 3 4
# 1 "/usr/include/bits/wordsize.h" 1 3 4
# 23 "/usr/include/bits/select.h" 2 3 4
# 31 "/usr/include/sys/select.h" 2 3 4


# 1 "/usr/include/bits/types/sigset_t.h" 1 3 4



# 1 "/usr/include/bits/types/__sigset_t.h" 1 3 4




typedef struct
{
  unsigned long int __val[(1024 / (8 * sizeof (unsigned long int)))];
} __sigset_t;
# 5 "/usr/include/bits/types/sigset_t.h" 2 3 4


typedef __sigset_t sigset_t;
# 34 "/usr/include/sys/select.h" 2 3 4



# 1 "/usr/include/bits/types/struct_timeval.h" 1 3 4







struct timeval
{
  __time_t tv_sec;
  __suseconds_t tv_usec;
};
# 38 "/usr/include/sys/select.h" 2 3 4

# 1 "/usr/include/bits/types/struct_timespec.h" 1 3 4








struct timespec
{
  __time_t tv_sec;
  __syscall_slong_t tv_nsec;
};
# 40 "/usr/include/sys/select.h" 2 3 4
# 49 "/usr/include/sys/select.h" 3 4
typedef long int __fd_mask;
# 59 "/usr/include/sys/select.h" 3 4
typedef struct
  {



    __fd_mask fds_bits[1024 / (8 * (int) sizeof (__fd_mask))];





  } fd_set;






typedef __fd_mask fd_mask;
# 91 "/usr/include/sys/select.h" 3 4
extern "C" {
# 101 "/usr/include/sys/select.h" 3 4
extern int select (int __nfds, fd_set *__restrict __readfds,
     fd_set *__restrict __writefds,
     fd_set *__restrict __exceptfds,
     struct timeval *__restrict __timeout);
# 113 "/usr/include/sys/select.h" 3 4
extern int pselect (int __nfds, fd_set *__restrict __readfds,
      fd_set *__restrict __writefds,
      fd_set *__restrict __exceptfds,
      const struct timespec *__restrict __timeout,
      const __sigset_t *__restrict __sigmask);
# 126 "/usr/include/sys/select.h" 3 4
}
# 180 "/usr/include/sys/types.h" 2 3 4





typedef __blksize_t blksize_t;






typedef __blkcnt_t blkcnt_t;



typedef __fsblkcnt_t fsblkcnt_t;



typedef __fsfilcnt_t fsfilcnt_t;
# 219 "/usr/include/sys/types.h" 3 4
typedef __blkcnt64_t blkcnt64_t;
typedef __fsblkcnt64_t fsblkcnt64_t;
typedef __fsfilcnt64_t fsfilcnt64_t;





# 1 "/usr/include/bits/pthreadtypes.h" 1 3 4
# 23 "/usr/include/bits/pthreadtypes.h" 3 4
# 1 "/usr/include/bits/thread-shared-types.h" 1 3 4
# 77 "/usr/include/bits/thread-shared-types.h" 3 4
# 1 "/usr/include/bits/pthreadtypes-arch.h" 1 3 4
# 21 "/usr/include/bits/pthreadtypes-arch.h" 3 4
# 1 "/usr/include/bits/wordsize.h" 1 3 4
# 22 "/usr/include/bits/pthreadtypes-arch.h" 2 3 4
# 65 "/usr/include/bits/pthreadtypes-arch.h" 3 4
struct __pthread_rwlock_arch_t
{
  unsigned int __readers;
  unsigned int __writers;
  unsigned int __wrphase_futex;
  unsigned int __writers_futex;
  unsigned int __pad3;
  unsigned int __pad4;

  int __cur_writer;
  int __shared;
  signed char __rwelision;




  unsigned char __pad1[7];


  unsigned long int __pad2;


  unsigned int __flags;
# 99 "/usr/include/bits/pthreadtypes-arch.h" 3 4
};
# 78 "/usr/include/bits/thread-shared-types.h" 2 3 4




typedef struct __pthread_internal_list
{
  struct __pthread_internal_list *__prev;
  struct __pthread_internal_list *__next;
} __pthread_list_t;
# 118 "/usr/include/bits/thread-shared-types.h" 3 4
struct __pthread_mutex_s
{
  int __lock ;
  unsigned int __count;
  int __owner;

  unsigned int __nusers;
# 148 "/usr/include/bits/thread-shared-types.h" 3 4
  int __kind;





  short __spins; short __elision;
  __pthread_list_t __list;
# 166 "/usr/include/bits/thread-shared-types.h" 3 4
};




struct __pthread_cond_s
{
  __extension__ union
  {
    __extension__ unsigned long long int __wseq;
    struct
    {
      unsigned int __low;
      unsigned int __high;
    } __wseq32;
  };
  __extension__ union
  {
    __extension__ unsigned long long int __g1_start;
    struct
    {
      unsigned int __low;
      unsigned int __high;
    } __g1_start32;
  };
  unsigned int __g_refs[2] ;
  unsigned int __g_size[2];
  unsigned int __g1_orig_size;
  unsigned int __wrefs;
  unsigned int __g_signals[2];
};
# 24 "/usr/include/bits/pthreadtypes.h" 2 3 4



typedef unsigned long int pthread_t;




typedef union
{
  char __size[4];
  int __align;
} pthread_mutexattr_t;




typedef union
{
  char __size[4];
  int __align;
} pthread_condattr_t;



typedef unsigned int pthread_key_t;



typedef int pthread_once_t;


union pthread_attr_t
{
  char __size[56];
  long int __align;
};

typedef union pthread_attr_t pthread_attr_t;




typedef union
{
  struct __pthread_mutex_s __data;
  char __size[40];
  long int __align;
} pthread_mutex_t;


typedef union
{
  struct __pthread_cond_s __data;
  char __size[48];
  __extension__ long long int __align;
} pthread_cond_t;





typedef union
{
  struct __pthread_rwlock_arch_t __data;
  char __size[56];
  long int __align;
} pthread_rwlock_t;

typedef union
{
  char __size[8];
  long int __align;
} pthread_rwlockattr_t;





typedef volatile int pthread_spinlock_t;




typedef union
{
  char __size[32];
  long int __align;
} pthread_barrier_t;

typedef union
{
  char __size[4];
  int __align;
} pthread_barrierattr_t;
# 228 "/usr/include/sys/types.h" 2 3 4


}
# 395 "/usr/include/stdlib.h" 2 3 4






extern long int random (void) throw ();


extern void srandom (unsigned int __seed) throw ();





extern char *initstate (unsigned int __seed, char *__statebuf,
   size_t __statelen) throw () __attribute__ ((__nonnull__ (2)));



extern char *setstate (char *__statebuf) throw () __attribute__ ((__nonnull__ (1)));







struct random_data
  {
    int32_t *fptr;
    int32_t *rptr;
    int32_t *state;
    int rand_type;
    int rand_deg;
    int rand_sep;
    int32_t *end_ptr;
  };

extern int random_r (struct random_data *__restrict __buf,
       int32_t *__restrict __result) throw () __attribute__ ((__nonnull__ (1, 2)));

extern int srandom_r (unsigned int __seed, struct random_data *__buf)
     throw () __attribute__ ((__nonnull__ (2)));

extern int initstate_r (unsigned int __seed, char *__restrict __statebuf,
   size_t __statelen,
   struct random_data *__restrict __buf)
     throw () __attribute__ ((__nonnull__ (2, 4)));

extern int setstate_r (char *__restrict __statebuf,
         struct random_data *__restrict __buf)
     throw () __attribute__ ((__nonnull__ (1, 2)));





extern int rand (void) throw ();

extern void srand (unsigned int __seed) throw ();



extern int rand_r (unsigned int *__seed) throw ();







extern double drand48 (void) throw ();
extern double erand48 (unsigned short int __xsubi[3]) throw () __attribute__ ((__nonnull__ (1)));


extern long int lrand48 (void) throw ();
extern long int nrand48 (unsigned short int __xsubi[3])
     throw () __attribute__ ((__nonnull__ (1)));


extern long int mrand48 (void) throw ();
extern long int jrand48 (unsigned short int __xsubi[3])
     throw () __attribute__ ((__nonnull__ (1)));


extern void srand48 (long int __seedval) throw ();
extern unsigned short int *seed48 (unsigned short int __seed16v[3])
     throw () __attribute__ ((__nonnull__ (1)));
extern void lcong48 (unsigned short int __param[7]) throw () __attribute__ ((__nonnull__ (1)));





struct drand48_data
  {
    unsigned short int __x[3];
    unsigned short int __old_x[3];
    unsigned short int __c;
    unsigned short int __init;
    __extension__ unsigned long long int __a;

  };


extern int drand48_r (struct drand48_data *__restrict __buffer,
        double *__restrict __result) throw () __attribute__ ((__nonnull__ (1, 2)));
extern int erand48_r (unsigned short int __xsubi[3],
        struct drand48_data *__restrict __buffer,
        double *__restrict __result) throw () __attribute__ ((__nonnull__ (1, 2)));


extern int lrand48_r (struct drand48_data *__restrict __buffer,
        long int *__restrict __result)
     throw () __attribute__ ((__nonnull__ (1, 2)));
extern int nrand48_r (unsigned short int __xsubi[3],
        struct drand48_data *__restrict __buffer,
        long int *__restrict __result)
     throw () __attribute__ ((__nonnull__ (1, 2)));


extern int mrand48_r (struct drand48_data *__restrict __buffer,
        long int *__restrict __result)
     throw () __attribute__ ((__nonnull__ (1, 2)));
extern int jrand48_r (unsigned short int __xsubi[3],
        struct drand48_data *__restrict __buffer,
        long int *__restrict __result)
     throw () __attribute__ ((__nonnull__ (1, 2)));


extern int srand48_r (long int __seedval, struct drand48_data *__buffer)
     throw () __attribute__ ((__nonnull__ (2)));

extern int seed48_r (unsigned short int __seed16v[3],
       struct drand48_data *__buffer) throw () __attribute__ ((__nonnull__ (1, 2)));

extern int lcong48_r (unsigned short int __param[7],
        struct drand48_data *__buffer)
     throw () __attribute__ ((__nonnull__ (1, 2)));




extern void *malloc (size_t __size) throw () __attribute__ ((__malloc__)) ;

extern void *calloc (size_t __nmemb, size_t __size)
     throw () __attribute__ ((__malloc__)) ;






extern void *realloc (void *__ptr, size_t __size)
     throw () __attribute__ ((__warn_unused_result__));







extern void *reallocarray (void *__ptr, size_t __nmemb, size_t __size)
     throw () __attribute__ ((__warn_unused_result__));



extern void free (void *__ptr) throw ();


# 1 "/usr/include/alloca.h" 1 3 4
# 24 "/usr/include/alloca.h" 3 4
# 1 "/rwthfs/rz/cluster/work/rwth1269/software/c23/classic-flang/lib/clang/16/include/stddef.h" 1 3 4
# 25 "/usr/include/alloca.h" 2 3 4

extern "C" {





extern void *alloca (size_t __size) throw ();





}
# 567 "/usr/include/stdlib.h" 2 3 4





extern void *valloc (size_t __size) throw () __attribute__ ((__malloc__)) ;




extern int posix_memalign (void **__memptr, size_t __alignment, size_t __size)
     throw () __attribute__ ((__nonnull__ (1))) ;




extern void *aligned_alloc (size_t __alignment, size_t __size)
     throw () __attribute__ ((__malloc__)) ;



extern void abort (void) throw () __attribute__ ((__noreturn__));



extern int atexit (void (*__func) (void)) throw () __attribute__ ((__nonnull__ (1)));




extern "C++" int at_quick_exit (void (*__func) (void))
     throw () __asm ("at_quick_exit") __attribute__ ((__nonnull__ (1)));
# 607 "/usr/include/stdlib.h" 3 4
extern int on_exit (void (*__func) (int __status, void *__arg), void *__arg)
     throw () __attribute__ ((__nonnull__ (1)));





extern void exit (int __status) throw () __attribute__ ((__noreturn__));





extern void quick_exit (int __status) throw () __attribute__ ((__noreturn__));





extern void _Exit (int __status) throw () __attribute__ ((__noreturn__));




extern char *getenv (const char *__name) throw () __attribute__ ((__nonnull__ (1))) ;




extern char *secure_getenv (const char *__name)
     throw () __attribute__ ((__nonnull__ (1))) ;






extern int putenv (char *__string) throw () __attribute__ ((__nonnull__ (1)));





extern int setenv (const char *__name, const char *__value, int __replace)
     throw () __attribute__ ((__nonnull__ (2)));


extern int unsetenv (const char *__name) throw () __attribute__ ((__nonnull__ (1)));






extern int clearenv (void) throw ();
# 672 "/usr/include/stdlib.h" 3 4
extern char *mktemp (char *__template) throw () __attribute__ ((__nonnull__ (1)));
# 685 "/usr/include/stdlib.h" 3 4
extern int mkstemp (char *__template) __attribute__ ((__nonnull__ (1))) ;
# 695 "/usr/include/stdlib.h" 3 4
extern int mkstemp64 (char *__template) __attribute__ ((__nonnull__ (1))) ;
# 707 "/usr/include/stdlib.h" 3 4
extern int mkstemps (char *__template, int __suffixlen) __attribute__ ((__nonnull__ (1))) ;
# 717 "/usr/include/stdlib.h" 3 4
extern int mkstemps64 (char *__template, int __suffixlen)
     __attribute__ ((__nonnull__ (1))) ;
# 728 "/usr/include/stdlib.h" 3 4
extern char *mkdtemp (char *__template) throw () __attribute__ ((__nonnull__ (1))) ;
# 739 "/usr/include/stdlib.h" 3 4
extern int mkostemp (char *__template, int __flags) __attribute__ ((__nonnull__ (1))) ;
# 749 "/usr/include/stdlib.h" 3 4
extern int mkostemp64 (char *__template, int __flags) __attribute__ ((__nonnull__ (1))) ;
# 759 "/usr/include/stdlib.h" 3 4
extern int mkostemps (char *__template, int __suffixlen, int __flags)
     __attribute__ ((__nonnull__ (1))) ;
# 771 "/usr/include/stdlib.h" 3 4
extern int mkostemps64 (char *__template, int __suffixlen, int __flags)
     __attribute__ ((__nonnull__ (1))) ;
# 781 "/usr/include/stdlib.h" 3 4
extern int system (const char *__command) ;





extern char *canonicalize_file_name (const char *__name)
     throw () __attribute__ ((__nonnull__ (1))) ;
# 797 "/usr/include/stdlib.h" 3 4
extern char *realpath (const char *__restrict __name,
         char *__restrict __resolved) throw () ;






typedef int (*__compar_fn_t) (const void *, const void *);


typedef __compar_fn_t comparison_fn_t;



typedef int (*__compar_d_fn_t) (const void *, const void *, void *);




extern void *bsearch (const void *__key, const void *__base,
        size_t __nmemb, size_t __size, __compar_fn_t __compar)
     __attribute__ ((__nonnull__ (1, 2, 5))) ;


# 1 "/usr/include/bits/stdlib-bsearch.h" 1 3 4
# 19 "/usr/include/bits/stdlib-bsearch.h" 3 4
extern __inline __attribute__ ((__gnu_inline__)) void *
bsearch (const void *__key, const void *__base, size_t __nmemb, size_t __size,
  __compar_fn_t __compar)
{
  size_t __l, __u, __idx;
  const void *__p;
  int __comparison;

  __l = 0;
  __u = __nmemb;
  while (__l < __u)
    {
      __idx = (__l + __u) / 2;
      __p = (void *) (((const char *) __base) + (__idx * __size));
      __comparison = (*__compar) (__key, __p);
      if (__comparison < 0)
 __u = __idx;
      else if (__comparison > 0)
 __l = __idx + 1;
      else
 return (void *) __p;
    }

  return __null;
}
# 823 "/usr/include/stdlib.h" 2 3 4




extern void qsort (void *__base, size_t __nmemb, size_t __size,
     __compar_fn_t __compar) __attribute__ ((__nonnull__ (1, 4)));

extern void qsort_r (void *__base, size_t __nmemb, size_t __size,
       __compar_d_fn_t __compar, void *__arg)
  __attribute__ ((__nonnull__ (1, 4)));




extern int abs (int __x) throw () __attribute__ ((__const__)) ;
extern long int labs (long int __x) throw () __attribute__ ((__const__)) ;


__extension__ extern long long int llabs (long long int __x)
     throw () __attribute__ ((__const__)) ;






extern div_t div (int __numer, int __denom)
     throw () __attribute__ ((__const__)) ;
extern ldiv_t ldiv (long int __numer, long int __denom)
     throw () __attribute__ ((__const__)) ;


__extension__ extern lldiv_t lldiv (long long int __numer,
        long long int __denom)
     throw () __attribute__ ((__const__)) ;
# 869 "/usr/include/stdlib.h" 3 4
extern char *ecvt (double __value, int __ndigit, int *__restrict __decpt,
     int *__restrict __sign) throw () __attribute__ ((__nonnull__ (3, 4))) ;




extern char *fcvt (double __value, int __ndigit, int *__restrict __decpt,
     int *__restrict __sign) throw () __attribute__ ((__nonnull__ (3, 4))) ;




extern char *gcvt (double __value, int __ndigit, char *__buf)
     throw () __attribute__ ((__nonnull__ (3))) ;




extern char *qecvt (long double __value, int __ndigit,
      int *__restrict __decpt, int *__restrict __sign)
     throw () __attribute__ ((__nonnull__ (3, 4))) ;
extern char *qfcvt (long double __value, int __ndigit,
      int *__restrict __decpt, int *__restrict __sign)
     throw () __attribute__ ((__nonnull__ (3, 4))) ;
extern char *qgcvt (long double __value, int __ndigit, char *__buf)
     throw () __attribute__ ((__nonnull__ (3))) ;




extern int ecvt_r (double __value, int __ndigit, int *__restrict __decpt,
     int *__restrict __sign, char *__restrict __buf,
     size_t __len) throw () __attribute__ ((__nonnull__ (3, 4, 5)));
extern int fcvt_r (double __value, int __ndigit, int *__restrict __decpt,
     int *__restrict __sign, char *__restrict __buf,
     size_t __len) throw () __attribute__ ((__nonnull__ (3, 4, 5)));

extern int qecvt_r (long double __value, int __ndigit,
      int *__restrict __decpt, int *__restrict __sign,
      char *__restrict __buf, size_t __len)
     throw () __attribute__ ((__nonnull__ (3, 4, 5)));
extern int qfcvt_r (long double __value, int __ndigit,
      int *__restrict __decpt, int *__restrict __sign,
      char *__restrict __buf, size_t __len)
     throw () __attribute__ ((__nonnull__ (3, 4, 5)));





extern int mblen (const char *__s, size_t __n) throw ();


extern int mbtowc (wchar_t *__restrict __pwc,
     const char *__restrict __s, size_t __n) throw ();


extern int wctomb (char *__s, wchar_t __wchar) throw ();



extern size_t mbstowcs (wchar_t *__restrict __pwcs,
   const char *__restrict __s, size_t __n) throw ();

extern size_t wcstombs (char *__restrict __s,
   const wchar_t *__restrict __pwcs, size_t __n)
     throw ();







extern int rpmatch (const char *__response) throw () __attribute__ ((__nonnull__ (1))) ;
# 954 "/usr/include/stdlib.h" 3 4
extern int getsubopt (char **__restrict __optionp,
        char *const *__restrict __tokens,
        char **__restrict __valuep)
     throw () __attribute__ ((__nonnull__ (1, 2, 3))) ;







extern int posix_openpt (int __oflag) ;







extern int grantpt (int __fd) throw ();



extern int unlockpt (int __fd) throw ();




extern char *ptsname (int __fd) throw () ;






extern int ptsname_r (int __fd, char *__buf, size_t __buflen)
     throw () __attribute__ ((__nonnull__ (2)));


extern int getpt (void);






extern int getloadavg (double __loadavg[], int __nelem)
     throw () __attribute__ ((__nonnull__ (1)));
# 1010 "/usr/include/stdlib.h" 3 4
# 1 "/usr/include/bits/stdlib-float.h" 1 3 4
# 24 "/usr/include/bits/stdlib-float.h" 3 4
extern __inline __attribute__ ((__gnu_inline__)) double
 atof (const char *__nptr) throw ()
{
  return strtod (__nptr, (char **) __null);
}
# 1011 "/usr/include/stdlib.h" 2 3 4
# 1020 "/usr/include/stdlib.h" 3 4
}
# 76 "/cvmfs/software.hpc.rwth.de/Linux/RH8/x86_64/intel/sapphirerapids/software/GCCcore/12.3.0/lib/gcc/x86_64-pc-linux-gnu/12.3.0/../../../../include/c++/12.3.0/cstdlib" 2 3

# 1 "/cvmfs/software.hpc.rwth.de/Linux/RH8/x86_64/intel/sapphirerapids/software/GCCcore/12.3.0/lib/gcc/x86_64-pc-linux-gnu/12.3.0/../../../../include/c++/12.3.0/bits/std_abs.h" 1 3
# 34 "/cvmfs/software.hpc.rwth.de/Linux/RH8/x86_64/intel/sapphirerapids/software/GCCcore/12.3.0/lib/gcc/x86_64-pc-linux-gnu/12.3.0/../../../../include/c++/12.3.0/bits/std_abs.h" 3
# 46 "/cvmfs/software.hpc.rwth.de/Linux/RH8/x86_64/intel/sapphirerapids/software/GCCcore/12.3.0/lib/gcc/x86_64-pc-linux-gnu/12.3.0/../../../../include/c++/12.3.0/bits/std_abs.h" 3
extern "C++"
{
namespace std __attribute__ ((__visibility__ ("default")))
{


  using ::abs;


  inline long
  abs(long __i) { return __builtin_labs(__i); }



  inline long long
  abs(long long __x) { return __builtin_llabs (__x); }
# 70 "/cvmfs/software.hpc.rwth.de/Linux/RH8/x86_64/intel/sapphirerapids/software/GCCcore/12.3.0/lib/gcc/x86_64-pc-linux-gnu/12.3.0/../../../../include/c++/12.3.0/bits/std_abs.h" 3
  inline constexpr double
  abs(double __x)
  { return __builtin_fabs(__x); }

  inline constexpr float
  abs(float __x)
  { return __builtin_fabsf(__x); }

  inline constexpr long double
  abs(long double __x)
  { return __builtin_fabsl(__x); }



  __extension__ inline constexpr __int128
  abs(__int128 __x) { return __x >= 0 ? __x : -__x; }
# 101 "/cvmfs/software.hpc.rwth.de/Linux/RH8/x86_64/intel/sapphirerapids/software/GCCcore/12.3.0/lib/gcc/x86_64-pc-linux-gnu/12.3.0/../../../../include/c++/12.3.0/bits/std_abs.h" 3
  __extension__ inline constexpr
  __float128
  abs(__float128 __x)
  { return __x < 0 ? -__x : __x; }



}
}
# 78 "/cvmfs/software.hpc.rwth.de/Linux/RH8/x86_64/intel/sapphirerapids/software/GCCcore/12.3.0/lib/gcc/x86_64-pc-linux-gnu/12.3.0/../../../../include/c++/12.3.0/cstdlib" 2 3
# 121 "/cvmfs/software.hpc.rwth.de/Linux/RH8/x86_64/intel/sapphirerapids/software/GCCcore/12.3.0/lib/gcc/x86_64-pc-linux-gnu/12.3.0/../../../../include/c++/12.3.0/cstdlib" 3
extern "C++"
{
namespace std __attribute__ ((__visibility__ ("default")))
{


  using ::div_t;
  using ::ldiv_t;

  using ::abort;

  using ::aligned_alloc;

  using ::atexit;


  using ::at_quick_exit;


  using ::atof;
  using ::atoi;
  using ::atol;
  using ::bsearch;
  using ::calloc;
  using ::div;
  using ::exit;
  using ::free;
  using ::getenv;
  using ::labs;
  using ::ldiv;
  using ::malloc;

  using ::mblen;
  using ::mbstowcs;
  using ::mbtowc;

  using ::qsort;


  using ::quick_exit;


  using ::rand;
  using ::realloc;
  using ::srand;
  using ::strtod;
  using ::strtol;
  using ::strtoul;
  using ::system;

  using ::wcstombs;
  using ::wctomb;



  inline ldiv_t
  div(long __i, long __j) { return ldiv(__i, __j); }




}
# 195 "/cvmfs/software.hpc.rwth.de/Linux/RH8/x86_64/intel/sapphirerapids/software/GCCcore/12.3.0/lib/gcc/x86_64-pc-linux-gnu/12.3.0/../../../../include/c++/12.3.0/cstdlib" 3
namespace __gnu_cxx __attribute__ ((__visibility__ ("default")))
{



  using ::lldiv_t;





  using ::_Exit;



  using ::llabs;

  inline lldiv_t
  div(long long __n, long long __d)
  { lldiv_t __q; __q.quot = __n / __d; __q.rem = __n % __d; return __q; }

  using ::lldiv;
# 227 "/cvmfs/software.hpc.rwth.de/Linux/RH8/x86_64/intel/sapphirerapids/software/GCCcore/12.3.0/lib/gcc/x86_64-pc-linux-gnu/12.3.0/../../../../include/c++/12.3.0/cstdlib" 3
  using ::atoll;
  using ::strtoll;
  using ::strtoull;

  using ::strtof;
  using ::strtold;


}

namespace std
{

  using ::__gnu_cxx::lldiv_t;

  using ::__gnu_cxx::_Exit;

  using ::__gnu_cxx::llabs;
  using ::__gnu_cxx::div;
  using ::__gnu_cxx::lldiv;

  using ::__gnu_cxx::atoll;
  using ::__gnu_cxx::strtof;
  using ::__gnu_cxx::strtoll;
  using ::__gnu_cxx::strtoull;
  using ::__gnu_cxx::strtold;
}



}
# 37 "/cvmfs/software.hpc.rwth.de/Linux/RH8/x86_64/intel/sapphirerapids/software/GCCcore/12.3.0/lib/gcc/x86_64-pc-linux-gnu/12.3.0/../../../../include/c++/12.3.0/stdlib.h" 2 3

using std::abort;
using std::atexit;
using std::exit;


  using std::at_quick_exit;


  using std::quick_exit;




using std::div_t;
using std::ldiv_t;

using std::abs;
using std::atof;
using std::atoi;
using std::atol;
using std::bsearch;
using std::calloc;
using std::div;
using std::free;
using std::getenv;
using std::labs;
using std::ldiv;
using std::malloc;

using std::mblen;
using std::mbstowcs;
using std::mbtowc;

using std::qsort;
using std::rand;
using std::realloc;
using std::srand;
using std::strtod;
using std::strtol;
using std::strtoul;
using std::system;

using std::wcstombs;
using std::wctomb;
# 34 "neighbor.cpp" 2

# 1 "./neighbor.h" 1
# 35 "./neighbor.h"
# 1 "./atom.h" 1
# 35 "./atom.h"
# 1 "./threadData.h" 1
# 35 "./threadData.h"
class ThreadData
{
  public:
    ThreadData() {
      mpi_me = 0;
      mpi_num_threads = 0;
      omp_me = 0;
      omp_num_threads = 1;
      teams = 1;
    };
    ~ThreadData() {};
    int mpi_me;
    int mpi_num_threads;
    int omp_me;
    int omp_num_threads;
    int teams;
};
# 36 "./atom.h" 2
# 1 "./types.h" 1
# 35 "./types.h"
enum ForceStyle {FORCELJ, FORCEEAM};


struct double2 {
  double x, y;
};
struct float2 {
  float x, y;
};
struct double4 {
  double x, y, z, w;
};
struct float4 {
  float x, y, z, w;
};
# 69 "./types.h"
typedef double MMD_float;
typedef double2 MMD_float2;
typedef double4 MMD_float4;

typedef int MMD_int;
typedef int MMD_bigint;
# 37 "./atom.h" 2


class Neighbor;
struct Box {
  MMD_float xprd, yprd, zprd;
  MMD_float xlo, xhi;
  MMD_float ylo, yhi;
  MMD_float zlo, zhi;
};

class Atom
{
  public:
    int natoms;
    int nlocal, nghost;
    int nmax;

    MMD_float* x;
    MMD_float* v;
    MMD_float* f;

    int ntypes;
    int* type;

    MMD_float* xold;

    ThreadData* threads;
    MMD_float virial, mass;

    int comm_size, reverse_size, border_size;

    struct Box box;

    Atom(int ntypes_);
    ~Atom();
    void addatom(MMD_float, MMD_float, MMD_float, MMD_float, MMD_float, MMD_float);
    void pbc();
    void growarray();

    void copy(int, int);

    void pack_comm(int, int*, MMD_float*, int*);
    void unpack_comm(int, int, MMD_float*);
    void pack_reverse(int, int, MMD_float*);
    void unpack_reverse(int, int*, MMD_float*);

    int pack_border(int, MMD_float*, int*);
    int unpack_border(int, MMD_float*);
    int pack_exchange(int, MMD_float*);
    int unpack_exchange(int, MMD_float*);
    int skip_exchange(MMD_float*);

    MMD_float* realloc_2d_MMD_float_array(MMD_float*, int, int, int);
    MMD_float* create_2d_MMD_float_array(int, int);
    void destroy_2d_MMD_float_array(MMD_float*);

    int* realloc_1d_int_array(int*, int, int);
    int* create_1d_int_array(int);
    void destroy_1d_int_array(int*);

    void sort(Neighbor & neighbor);

  private:
    int* binpos;
    int* bins;
    MMD_float* x_copy;
    MMD_float* v_copy;
    int* type_copy;
    int copy_size;
};
# 36 "./neighbor.h" 2

# 1 "./timer.h" 1
# 43 "./timer.h"
# 1 "/cvmfs/software.hpc.rwth.de/Linux/RH8/x86_64/intel/sapphirerapids/software/GCCcore/12.3.0/lib/gcc/x86_64-pc-linux-gnu/12.3.0/../../../../include/c++/12.3.0/ctime" 1 3
# 40 "/cvmfs/software.hpc.rwth.de/Linux/RH8/x86_64/intel/sapphirerapids/software/GCCcore/12.3.0/lib/gcc/x86_64-pc-linux-gnu/12.3.0/../../../../include/c++/12.3.0/ctime" 3


# 1 "/usr/include/time.h" 1 3 4
# 29 "/usr/include/time.h" 3 4
# 1 "/rwthfs/rz/cluster/work/rwth1269/software/c23/classic-flang/lib/clang/16/include/stddef.h" 1 3 4
# 30 "/usr/include/time.h" 2 3 4



# 1 "/usr/include/bits/time.h" 1 3 4
# 73 "/usr/include/bits/time.h" 3 4
# 1 "/usr/include/bits/timex.h" 1 3 4
# 26 "/usr/include/bits/timex.h" 3 4
struct timex
{
  unsigned int modes;
  __syscall_slong_t offset;
  __syscall_slong_t freq;
  __syscall_slong_t maxerror;
  __syscall_slong_t esterror;
  int status;
  __syscall_slong_t constant;
  __syscall_slong_t precision;
  __syscall_slong_t tolerance;
  struct timeval time;
  __syscall_slong_t tick;
  __syscall_slong_t ppsfreq;
  __syscall_slong_t jitter;
  int shift;
  __syscall_slong_t stabil;
  __syscall_slong_t jitcnt;
  __syscall_slong_t calcnt;
  __syscall_slong_t errcnt;
  __syscall_slong_t stbcnt;

  int tai;


  int :32; int :32; int :32; int :32;
  int :32; int :32; int :32; int :32;
  int :32; int :32; int :32;
};
# 74 "/usr/include/bits/time.h" 2 3 4

extern "C" {


extern int clock_adjtime (__clockid_t __clock_id, struct timex *__utx) throw ();

}
# 34 "/usr/include/time.h" 2 3 4





# 1 "/usr/include/bits/types/struct_tm.h" 1 3 4






struct tm
{
  int tm_sec;
  int tm_min;
  int tm_hour;
  int tm_mday;
  int tm_mon;
  int tm_year;
  int tm_wday;
  int tm_yday;
  int tm_isdst;


  long int tm_gmtoff;
  const char *tm_zone;




};
# 40 "/usr/include/time.h" 2 3 4








# 1 "/usr/include/bits/types/struct_itimerspec.h" 1 3 4







struct itimerspec
  {
    struct timespec it_interval;
    struct timespec it_value;
  };
# 49 "/usr/include/time.h" 2 3 4
struct sigevent;
# 68 "/usr/include/time.h" 3 4
extern "C" {



extern clock_t clock (void) throw ();


extern time_t time (time_t *__timer) throw ();


extern double difftime (time_t __time1, time_t __time0)
     throw () __attribute__ ((__const__));


extern time_t mktime (struct tm *__tp) throw ();





extern size_t strftime (char *__restrict __s, size_t __maxsize,
   const char *__restrict __format,
   const struct tm *__restrict __tp) throw ();




extern char *strptime (const char *__restrict __s,
         const char *__restrict __fmt, struct tm *__tp)
     throw ();






extern size_t strftime_l (char *__restrict __s, size_t __maxsize,
     const char *__restrict __format,
     const struct tm *__restrict __tp,
     locale_t __loc) throw ();



extern char *strptime_l (const char *__restrict __s,
    const char *__restrict __fmt, struct tm *__tp,
    locale_t __loc) throw ();





extern struct tm *gmtime (const time_t *__timer) throw ();



extern struct tm *localtime (const time_t *__timer) throw ();




extern struct tm *gmtime_r (const time_t *__restrict __timer,
       struct tm *__restrict __tp) throw ();



extern struct tm *localtime_r (const time_t *__restrict __timer,
          struct tm *__restrict __tp) throw ();




extern char *asctime (const struct tm *__tp) throw ();


extern char *ctime (const time_t *__timer) throw ();






extern char *asctime_r (const struct tm *__restrict __tp,
   char *__restrict __buf) throw ();


extern char *ctime_r (const time_t *__restrict __timer,
        char *__restrict __buf) throw ();




extern char *__tzname[2];
extern int __daylight;
extern long int __timezone;




extern char *tzname[2];



extern void tzset (void) throw ();



extern int daylight;
extern long int timezone;





extern int stime (const time_t *__when) throw ();
# 196 "/usr/include/time.h" 3 4
extern time_t timegm (struct tm *__tp) throw ();


extern time_t timelocal (struct tm *__tp) throw ();


extern int dysize (int __year) throw () __attribute__ ((__const__));
# 211 "/usr/include/time.h" 3 4
extern int nanosleep (const struct timespec *__requested_time,
        struct timespec *__remaining);



extern int clock_getres (clockid_t __clock_id, struct timespec *__res) throw ();


extern int clock_gettime (clockid_t __clock_id, struct timespec *__tp) throw ();


extern int clock_settime (clockid_t __clock_id, const struct timespec *__tp)
     throw ();






extern int clock_nanosleep (clockid_t __clock_id, int __flags,
       const struct timespec *__req,
       struct timespec *__rem);


extern int clock_getcpuclockid (pid_t __pid, clockid_t *__clock_id) throw ();




extern int timer_create (clockid_t __clock_id,
    struct sigevent *__restrict __evp,
    timer_t *__restrict __timerid) throw ();


extern int timer_delete (timer_t __timerid) throw ();


extern int timer_settime (timer_t __timerid, int __flags,
     const struct itimerspec *__restrict __value,
     struct itimerspec *__restrict __ovalue) throw ();


extern int timer_gettime (timer_t __timerid, struct itimerspec *__value)
     throw ();


extern int timer_getoverrun (timer_t __timerid) throw ();





extern int timespec_get (struct timespec *__ts, int __base)
     throw () __attribute__ ((__nonnull__ (1)));
# 280 "/usr/include/time.h" 3 4
extern int getdate_err;
# 289 "/usr/include/time.h" 3 4
extern struct tm *getdate (const char *__string);
# 303 "/usr/include/time.h" 3 4
extern int getdate_r (const char *__restrict __string,
        struct tm *__restrict __resbufp);


}
# 43 "/cvmfs/software.hpc.rwth.de/Linux/RH8/x86_64/intel/sapphirerapids/software/GCCcore/12.3.0/lib/gcc/x86_64-pc-linux-gnu/12.3.0/../../../../include/c++/12.3.0/ctime" 2 3
# 58 "/cvmfs/software.hpc.rwth.de/Linux/RH8/x86_64/intel/sapphirerapids/software/GCCcore/12.3.0/lib/gcc/x86_64-pc-linux-gnu/12.3.0/../../../../include/c++/12.3.0/ctime" 3
namespace std
{
  using ::clock_t;
  using ::time_t;
  using ::tm;

  using ::clock;
  using ::difftime;
  using ::mktime;
  using ::time;
  using ::asctime;
  using ::ctime;
  using ::gmtime;
  using ::localtime;
  using ::strftime;
}



namespace std
{
  using ::timespec;
  using ::timespec_get;
}
# 44 "./timer.h" 2

class Timer
{
  public:
    Timer();
    ~Timer();
    void stamp();
    void stamp(int);
    void stamp_extra_start();
    void stamp_extra_stop(int);
    void barrier_start(int);
    void barrier_stop(int);
    double* array;

  private:



    double previous_time_d, previous_time_extra_d;

};
# 38 "./neighbor.h" 2

class Neighbor
{
  public:
    int every;
    int nbinx, nbiny, nbinz;
    MMD_float cutneigh;
    MMD_float* cutneighsq;
    int ncalls;
    int max_totalneigh;

    int* numneigh;
    int* neighbors;
    int maxneighs;
    int halfneigh;

    MMD_int ghost_newton;
    int count;
    Neighbor(int ntypes_);
    ~Neighbor();
    int setup(Atom &);
    void build(Atom &);

    Timer* timer;

    ThreadData* threads;


    void binatoms(Atom & atom, int count = -1);

    int* bincount;
    int* bins;
    int mbins;
    int atoms_per_bin;
  private:
    MMD_float xprd, yprd, zprd;

    int nmax;
    int ntypes;

    int nstencil;
    int* stencil;

    int mbinx, mbiny, mbinz;
    int mbinxlo, mbinylo, mbinzlo;
    MMD_float binsizex, binsizey, binsizez;
    MMD_float bininvx, bininvy, bininvz;

    int resize;

    MMD_float bindist(int, int, int);
    int coord2bin(MMD_float, MMD_float, MMD_float);
};
# 36 "neighbor.cpp" 2
# 1 "./openmp.h" 1
# 40 "./openmp.h"
# 1 "/rwthfs/rz/cluster/work/rwth1269/software/c23/classic-flang/lib/clang/16/include/omp.h" 1 3
# 18 "/rwthfs/rz/cluster/work/rwth1269/software/c23/classic-flang/lib/clang/16/include/omp.h" 3
# 1 "/cvmfs/software.hpc.rwth.de/Linux/RH8/x86_64/intel/sapphirerapids/software/GCCcore/12.3.0/lib/gcc/x86_64-pc-linux-gnu/12.3.0/../../../../include/c++/12.3.0/stdlib.h" 1 3
# 19 "/rwthfs/rz/cluster/work/rwth1269/software/c23/classic-flang/lib/clang/16/include/omp.h" 2 3
# 1 "/rwthfs/rz/cluster/work/rwth1269/software/c23/classic-flang/lib/clang/16/include/stdint.h" 1 3
# 52 "/rwthfs/rz/cluster/work/rwth1269/software/c23/classic-flang/lib/clang/16/include/stdint.h" 3
# 1 "/usr/include/stdint.h" 1 3 4
# 26 "/usr/include/stdint.h" 3 4
# 1 "/usr/include/bits/libc-header-start.h" 1 3 4
# 27 "/usr/include/stdint.h" 2 3 4

# 1 "/usr/include/bits/wchar.h" 1 3 4
# 29 "/usr/include/stdint.h" 2 3 4
# 1 "/usr/include/bits/wordsize.h" 1 3 4
# 30 "/usr/include/stdint.h" 2 3 4







# 1 "/usr/include/bits/stdint-uintn.h" 1 3 4
# 24 "/usr/include/bits/stdint-uintn.h" 3 4
typedef __uint8_t uint8_t;
typedef __uint16_t uint16_t;
typedef __uint32_t uint32_t;
typedef __uint64_t uint64_t;
# 38 "/usr/include/stdint.h" 2 3 4





typedef __int_least8_t int_least8_t;
typedef __int_least16_t int_least16_t;
typedef __int_least32_t int_least32_t;
typedef __int_least64_t int_least64_t;


typedef __uint_least8_t uint_least8_t;
typedef __uint_least16_t uint_least16_t;
typedef __uint_least32_t uint_least32_t;
typedef __uint_least64_t uint_least64_t;





typedef signed char int_fast8_t;

typedef long int int_fast16_t;
typedef long int int_fast32_t;
typedef long int int_fast64_t;
# 71 "/usr/include/stdint.h" 3 4
typedef unsigned char uint_fast8_t;

typedef unsigned long int uint_fast16_t;
typedef unsigned long int uint_fast32_t;
typedef unsigned long int uint_fast64_t;
# 87 "/usr/include/stdint.h" 3 4
typedef long int intptr_t;


typedef unsigned long int uintptr_t;
# 101 "/usr/include/stdint.h" 3 4
typedef __intmax_t intmax_t;
typedef __uintmax_t uintmax_t;
# 53 "/rwthfs/rz/cluster/work/rwth1269/software/c23/classic-flang/lib/clang/16/include/stdint.h" 2 3
# 20 "/rwthfs/rz/cluster/work/rwth1269/software/c23/classic-flang/lib/clang/16/include/omp.h" 2 3







    extern "C" {
# 48 "/rwthfs/rz/cluster/work/rwth1269/software/c23/classic-flang/lib/clang/16/include/omp.h" 3
    typedef enum omp_sched_t {
        omp_sched_static = 1,
        omp_sched_dynamic = 2,
        omp_sched_guided = 3,
        omp_sched_auto = 4,
        omp_sched_monotonic = 0x80000000
    } omp_sched_t;


    extern void omp_set_num_threads (int);
    extern void omp_set_dynamic (int);
    extern void omp_set_nested (int);
    extern void omp_set_max_active_levels (int);
    extern void omp_set_schedule (omp_sched_t, int);


    extern int omp_get_num_threads (void);
    extern int omp_get_dynamic (void);
    extern int omp_get_nested (void);
    extern int omp_get_max_threads (void);
    extern int omp_get_thread_num (void);
    extern int omp_get_num_procs (void);
    extern int omp_in_parallel (void);
    extern int omp_in_final (void);
    extern int omp_get_active_level (void);
    extern int omp_get_level (void);
    extern int omp_get_ancestor_thread_num (int);
    extern int omp_get_team_size (int);
    extern int omp_get_thread_limit (void);
    extern int omp_get_max_active_levels (void);
    extern void omp_get_schedule (omp_sched_t *, int *);
    extern int omp_get_max_task_priority (void);


    typedef struct omp_lock_t {
        void * _lk;
    } omp_lock_t;

    extern void omp_init_lock (omp_lock_t *);
    extern void omp_set_lock (omp_lock_t *);
    extern void omp_unset_lock (omp_lock_t *);
    extern void omp_destroy_lock (omp_lock_t *);
    extern int omp_test_lock (omp_lock_t *);


    typedef struct omp_nest_lock_t {
        void * _lk;
    } omp_nest_lock_t;

    extern void omp_init_nest_lock (omp_nest_lock_t *);
    extern void omp_set_nest_lock (omp_nest_lock_t *);
    extern void omp_unset_nest_lock (omp_nest_lock_t *);
    extern void omp_destroy_nest_lock (omp_nest_lock_t *);
    extern int omp_test_nest_lock (omp_nest_lock_t *);


    typedef enum omp_sync_hint_t {
        omp_sync_hint_none = 0,
        omp_lock_hint_none = omp_sync_hint_none,
        omp_sync_hint_uncontended = 1,
        omp_lock_hint_uncontended = omp_sync_hint_uncontended,
        omp_sync_hint_contended = (1<<1),
        omp_lock_hint_contended = omp_sync_hint_contended,
        omp_sync_hint_nonspeculative = (1<<2),
        omp_lock_hint_nonspeculative = omp_sync_hint_nonspeculative,
        omp_sync_hint_speculative = (1<<3),
        omp_lock_hint_speculative = omp_sync_hint_speculative,
        kmp_lock_hint_hle = (1<<16),
        kmp_lock_hint_rtm = (1<<17),
        kmp_lock_hint_adaptive = (1<<18)
    } omp_sync_hint_t;


    typedef omp_sync_hint_t omp_lock_hint_t;


    extern void omp_init_lock_with_hint(omp_lock_t *, omp_lock_hint_t);
    extern void omp_init_nest_lock_with_hint(omp_nest_lock_t *, omp_lock_hint_t);


    extern double omp_get_wtime (void);
    extern double omp_get_wtick (void);


    extern int omp_get_default_device (void);
    extern void omp_set_default_device (int);
    extern int omp_is_initial_device (void);
    extern int omp_get_num_devices (void);
    extern int omp_get_num_teams (void);
    extern int omp_get_team_num (void);
    extern int omp_get_cancellation (void);


    extern int omp_get_initial_device (void);
    extern void* omp_target_alloc(size_t, int);
    extern void omp_target_free(void *, int);
    extern int omp_target_is_present(const void *, int);
    extern int omp_target_memcpy(void *, const void *, size_t, size_t, size_t, int, int);
    extern int omp_target_memcpy_rect(void *, const void *, size_t, int, const size_t *,
                                            const size_t *, const size_t *, const size_t *, const size_t *, int, int);
    extern int omp_target_associate_ptr(const void *, const void *, size_t, size_t, int);
    extern int omp_target_disassociate_ptr(const void *, int);


    extern int omp_get_device_num (void);
    typedef void * omp_depend_t;


    typedef intptr_t omp_intptr_t;


    typedef enum omp_interop_property {
        omp_ipr_fr_id = -1,
        omp_ipr_fr_name = -2,
        omp_ipr_vendor = -3,
        omp_ipr_vendor_name = -4,
        omp_ipr_device_num = -5,
        omp_ipr_platform = -6,
        omp_ipr_device = -7,
        omp_ipr_device_context = -8,
        omp_ipr_targetsync = -9,
        omp_ipr_first = -9
    } omp_interop_property_t;



    typedef enum omp_interop_rc {
        omp_irc_no_value = 1,
        omp_irc_success = 0,
        omp_irc_empty = -1,
        omp_irc_out_of_range = -2,
        omp_irc_type_int = -3,
        omp_irc_type_ptr = -4,
        omp_irc_type_str = -5,
        omp_irc_other = -6
    } omp_interop_rc_t;

    typedef enum omp_interop_fr {
        omp_ifr_cuda = 1,
        omp_ifr_cuda_driver = 2,
        omp_ifr_opencl = 3,
        omp_ifr_sycl = 4,
        omp_ifr_hip = 5,
        omp_ifr_level_zero = 6,
        omp_ifr_last = 7
    } omp_interop_fr_t;

    typedef void * omp_interop_t;




    extern int omp_get_num_interop_properties(const omp_interop_t);



    extern omp_intptr_t omp_get_interop_int(const omp_interop_t, omp_interop_property_t, int *);



    extern void * omp_get_interop_ptr(const omp_interop_t, omp_interop_property_t, int *);



    extern const char * omp_get_interop_str(const omp_interop_t, omp_interop_property_t, int *);



    extern const char * omp_get_interop_name(const omp_interop_t, omp_interop_property_t);



    extern const char * omp_get_interop_type_desc(const omp_interop_t, omp_interop_property_t);



    extern const char * omp_get_interop_rc_desc(const omp_interop_t, omp_interop_rc_t);






    extern int omp_target_memcpy_async(void *, const void *, size_t, size_t, size_t, int,
                                             int, int, omp_depend_t *);



    extern int omp_target_memcpy_rect_async(void *, const void *, size_t, int, const size_t *,
                                             const size_t *, const size_t *, const size_t *, const size_t *, int, int,
                                             int, omp_depend_t *);



    extern void * omp_get_mapped_ptr(const void *, int);
    extern int omp_target_is_accessible(const void *, size_t, int);


    extern int kmp_get_stacksize (void);
    extern void kmp_set_stacksize (int);
    extern size_t kmp_get_stacksize_s (void);
    extern void kmp_set_stacksize_s (size_t);
    extern int kmp_get_blocktime (void);
    extern int kmp_get_library (void);
    extern void kmp_set_blocktime (int);
    extern void kmp_set_library (int);
    extern void kmp_set_library_serial (void);
    extern void kmp_set_library_turnaround (void);
    extern void kmp_set_library_throughput (void);
    extern void kmp_set_defaults (char const *);
    extern void kmp_set_disp_num_buffers (int);


    typedef void * kmp_affinity_mask_t;

    extern int kmp_set_affinity (kmp_affinity_mask_t *);
    extern int kmp_get_affinity (kmp_affinity_mask_t *);
    extern int kmp_get_affinity_max_proc (void);
    extern void kmp_create_affinity_mask (kmp_affinity_mask_t *);
    extern void kmp_destroy_affinity_mask (kmp_affinity_mask_t *);
    extern int kmp_set_affinity_mask_proc (int, kmp_affinity_mask_t *);
    extern int kmp_unset_affinity_mask_proc (int, kmp_affinity_mask_t *);
    extern int kmp_get_affinity_mask_proc (int, kmp_affinity_mask_t *);


    typedef enum omp_proc_bind_t {
        omp_proc_bind_false = 0,
        omp_proc_bind_true = 1,
        omp_proc_bind_master = 2,
        omp_proc_bind_close = 3,
        omp_proc_bind_spread = 4
    } omp_proc_bind_t;

    extern omp_proc_bind_t omp_get_proc_bind (void);


    extern int omp_get_num_places (void);
    extern int omp_get_place_num_procs (int);
    extern void omp_get_place_proc_ids (int, int *);
    extern int omp_get_place_num (void);
    extern int omp_get_partition_num_places (void);
    extern void omp_get_partition_place_nums (int *);

    extern void * kmp_malloc (size_t);
    extern void * kmp_aligned_malloc (size_t, size_t);
    extern void * kmp_calloc (size_t, size_t);
    extern void * kmp_realloc (void *, size_t);
    extern void kmp_free (void *);

    extern void kmp_set_warnings_on(void);
    extern void kmp_set_warnings_off(void);


    typedef enum omp_control_tool_result_t {
        omp_control_tool_notool = -2,
        omp_control_tool_nocallback = -1,
        omp_control_tool_success = 0,
        omp_control_tool_ignored = 1
    } omp_control_tool_result_t;

    typedef enum omp_control_tool_t {
        omp_control_tool_start = 1,
        omp_control_tool_pause = 2,
        omp_control_tool_flush = 3,
        omp_control_tool_end = 4
    } omp_control_tool_t;

    extern int omp_control_tool(int, int, void*);


    typedef uintptr_t omp_uintptr_t;

    typedef enum {
        omp_atk_sync_hint = 1,
        omp_atk_alignment = 2,
        omp_atk_access = 3,
        omp_atk_pool_size = 4,
        omp_atk_fallback = 5,
        omp_atk_fb_data = 6,
        omp_atk_pinned = 7,
        omp_atk_partition = 8
    } omp_alloctrait_key_t;

    typedef enum {
        omp_atv_false = 0,
        omp_atv_true = 1,
        omp_atv_contended = 3,
        omp_atv_uncontended = 4,
        omp_atv_serialized = 5,
        omp_atv_sequential = omp_atv_serialized,
        omp_atv_private = 6,
        omp_atv_all = 7,
        omp_atv_thread = 8,
        omp_atv_pteam = 9,
        omp_atv_cgroup = 10,
        omp_atv_default_mem_fb = 11,
        omp_atv_null_fb = 12,
        omp_atv_abort_fb = 13,
        omp_atv_allocator_fb = 14,
        omp_atv_environment = 15,
        omp_atv_nearest = 16,
        omp_atv_blocked = 17,
        omp_atv_interleaved = 18
    } omp_alloctrait_value_t;


    typedef struct {
        omp_alloctrait_key_t key;
        omp_uintptr_t value;
    } omp_alloctrait_t;
# 386 "/rwthfs/rz/cluster/work/rwth1269/software/c23/classic-flang/lib/clang/16/include/omp.h" 3
    typedef enum omp_allocator_handle_t : omp_uintptr_t



    {
      omp_null_allocator = 0,
      omp_default_mem_alloc = 1,
      omp_large_cap_mem_alloc = 2,
      omp_const_mem_alloc = 3,
      omp_high_bw_mem_alloc = 4,
      omp_low_lat_mem_alloc = 5,
      omp_cgroup_mem_alloc = 6,
      omp_pteam_mem_alloc = 7,
      omp_thread_mem_alloc = 8,
      llvm_omp_target_host_mem_alloc = 100,
      llvm_omp_target_shared_mem_alloc = 101,
      llvm_omp_target_device_mem_alloc = 102,
      KMP_ALLOCATOR_MAX_HANDLE = (18446744073709551615UL)
    } omp_allocator_handle_t;

    typedef enum omp_memspace_handle_t : omp_uintptr_t



    {
      omp_default_mem_space = 0,
      omp_large_cap_mem_space = 1,
      omp_const_mem_space = 2,
      omp_high_bw_mem_space = 3,
      omp_low_lat_mem_space = 4,
      llvm_omp_target_host_mem_space = 100,
      llvm_omp_target_shared_mem_space = 101,
      llvm_omp_target_device_mem_space = 102,
      KMP_MEMSPACE_MAX_HANDLE = (18446744073709551615UL)
    } omp_memspace_handle_t;

    extern omp_allocator_handle_t omp_init_allocator(omp_memspace_handle_t m,
                                                       int ntraits, omp_alloctrait_t traits[]);
    extern void omp_destroy_allocator(omp_allocator_handle_t allocator);

    extern void omp_set_default_allocator(omp_allocator_handle_t a);
    extern omp_allocator_handle_t omp_get_default_allocator(void);

    extern void * omp_alloc(size_t size, omp_allocator_handle_t a = omp_null_allocator);
    extern void * omp_aligned_alloc(size_t align, size_t size,
                                                         omp_allocator_handle_t a = omp_null_allocator);
    extern void * omp_calloc(size_t nmemb, size_t size,
                                                  omp_allocator_handle_t a = omp_null_allocator);
    extern void * omp_aligned_calloc(size_t align, size_t nmemb, size_t size,
                                                          omp_allocator_handle_t a = omp_null_allocator);
    extern void * omp_realloc(void *ptr, size_t size,
                                                   omp_allocator_handle_t allocator = omp_null_allocator,
                                                   omp_allocator_handle_t free_allocator = omp_null_allocator);
    extern void omp_free(void * ptr, omp_allocator_handle_t a = omp_null_allocator);
# 453 "/rwthfs/rz/cluster/work/rwth1269/software/c23/classic-flang/lib/clang/16/include/omp.h" 3
    extern void ompc_set_affinity_format(char const *);
    extern size_t ompc_get_affinity_format(char *, size_t);
    extern void ompc_display_affinity(char const *);
    extern size_t ompc_capture_affinity(char *, size_t, char const *);






    typedef enum omp_event_handle_t { KMP_EVENT_MAX_HANDLE = (18446744073709551615UL) } omp_event_handle_t;

    extern void omp_fulfill_event ( omp_event_handle_t event );


    typedef enum omp_pause_resource_t {
      omp_pause_resume = 0,
      omp_pause_soft = 1,
      omp_pause_hard = 2
    } omp_pause_resource_t;
    extern int omp_pause_resource(omp_pause_resource_t, int);
    extern int omp_pause_resource_all(omp_pause_resource_t);

    extern int omp_get_supported_active_levels(void);


    extern void omp_set_num_teams(int num_teams);
    extern int omp_get_max_teams(void);
    extern void omp_set_teams_thread_limit(int limit);
    extern int omp_get_teams_thread_limit(void);


    extern void omp_display_env(int verbose);


#pragma omp begin declare variant match(device={kind(host)})
    static inline int omp_is_initial_device(void) { return 1; }
#pragma omp end declare variant
#pragma omp begin declare variant match(device={kind(nohost)})
    static inline int omp_is_initial_device(void) { return 0; }
#pragma omp end declare variant



    extern int omp_in_explicit_task(void);


    extern void *llvm_omp_target_dynamic_shared_alloc();







    typedef int omp_int_t;
    typedef double omp_wtime_t;


    }
# 41 "./openmp.h" 2
# 37 "neighbor.cpp" 2




Neighbor::Neighbor(int ntypes_)
{
  ncalls = 0;
  ntypes = ntypes_;
  max_totalneigh = 0;
  numneigh = __null;
  neighbors = __null;
  maxneighs = 100;
  nmax = 0;
  bincount = __null;
  bins = __null;
  atoms_per_bin = 8;
  stencil = __null;
  threads = __null;
  halfneigh = 0;
  ghost_newton = 1;
  cutneighsq = new MMD_float[ntypes*ntypes];
}

Neighbor::~Neighbor()
{




  if(numneigh) free(numneigh);
  if(neighbors) free(neighbors);


  if(bincount) free(bincount);

  if(bins) free(bins);
}





void Neighbor::build(Atom &atom)
{
  ncalls++;
  const int nlocal = atom.nlocal;
  const int nall = atom.nlocal + atom.nghost;


#pragma omp master

  if(nall > nmax) {
    nmax = nall;







    if(numneigh) free(numneigh);

    if(neighbors) free(neighbors);

    numneigh = (int*) malloc(nmax * sizeof(int));
    neighbors = (int*) malloc(nmax * maxneighs * sizeof(int));

  }

#pragma omp barrier


  binatoms(atom);
  count = 0;


  const MMD_float* const x = atom.x;
  const int* const type = atom.type;
  int ntypes = atom.ntypes;

  resize = 1;
#pragma omp barrier

  while(resize) {
#pragma omp barrier
    int new_maxneighs = maxneighs;
    resize = 0;
#pragma omp barrier

    #pragma omp for schedule(static)
    for(int i = 0; i < nlocal; i++) {
      int* neighptr = &neighbors[i * maxneighs];


      int n = 0;

      const MMD_float xtmp = x[i * 3 + 0];
      const MMD_float ytmp = x[i * 3 + 1];
      const MMD_float ztmp = x[i * 3 + 2];

      const int type_i = type[i];




      const int ibin = coord2bin(xtmp, ytmp, ztmp);

      for(int k = 0; k < nstencil; k++) {
        const int jbin = ibin + stencil[k];

        int* loc_bin = &bins[jbin * atoms_per_bin];

        if(ibin == jbin)
          for(int m = 0; m < bincount[jbin]; m++) {
            const int j = loc_bin[m];


            if(((j == i) || (halfneigh && !ghost_newton && (j < i)) ||
                (halfneigh && ghost_newton && ((j < i) || ((j >= nlocal) &&
                                               ((x[j * 3 + 2] < ztmp) || (x[j * 3 + 2] == ztmp && x[j * 3 + 1] < ytmp) ||
                                                (x[j * 3 + 2] == ztmp && x[j * 3 + 1] == ytmp && x[j * 3 + 0] < xtmp))))))) continue;

            const MMD_float delx = xtmp - x[j * 3 + 0];
            const MMD_float dely = ytmp - x[j * 3 + 1];
            const MMD_float delz = ztmp - x[j * 3 + 2];
            const int type_j = type[j];
            const MMD_float rsq = delx * delx + dely * dely + delz * delz;

            if((rsq <= cutneighsq[type_i*ntypes+type_j])) neighptr[n++] = j;
          }
        else {
          for(int m = 0; m < bincount[jbin]; m++) {
            const int j = loc_bin[m];

            if(halfneigh && !ghost_newton && (j < i)) continue;

            const MMD_float delx = xtmp - x[j * 3 + 0];
            const MMD_float dely = ytmp - x[j * 3 + 1];
            const MMD_float delz = ztmp - x[j * 3 + 2];
            const int type_j = type[j];
            const MMD_float rsq = delx * delx + dely * dely + delz * delz;

            if((rsq <= cutneighsq[type_i*ntypes+type_j])) neighptr[n++] = j;
          }
        }
      }

      numneigh[i] = n;

      if(n >= maxneighs) {
        resize = 1;

        if(n >= new_maxneighs) new_maxneighs = n;
      }
    }



    if(resize) {
#pragma omp master
      {
        maxneighs = new_maxneighs * 1.2;




    free(neighbors);
        neighbors = (int*) malloc(nmax* maxneighs * sizeof(int));

      }
#pragma omp barrier
    }
  }

#pragma omp barrier

}

void Neighbor::binatoms(Atom &atom, int count)
{
  const int nlocal = atom.nlocal;
  const int nall = count<0?atom.nlocal + atom.nghost:count;
  const MMD_float* const x = atom.x;

  xprd = atom.box.xprd;
  yprd = atom.box.yprd;
  zprd = atom.box.zprd;

  resize = 1;

#pragma omp barrier

  while(resize > 0) {
#pragma omp barrier
    resize = 0;
#pragma omp barrier
#pragma omp for schedule(static)
    for(int i = 0; i < mbins; i++) bincount[i] = 0;


    #pragma omp for schedule(static)
    for(int i = 0; i < nall; i++) {
      const int ibin = coord2bin(x[i * 3 + 0], x[i * 3 + 1], x[i * 3 + 2]);

      if(bincount[ibin] < atoms_per_bin) {
        int ac;




        ac = __sync_fetch_and_add(bincount + ibin, 1);

        bins[ibin * atoms_per_bin + ac] = i;
      } else resize = 1;
    }



#pragma omp master

    if(resize) {
      free(bins);
      atoms_per_bin *= 2;
      bins = (int*) malloc(mbins * atoms_per_bin * sizeof(int));
    }


  }

#pragma omp barrier

}





inline int Neighbor::coord2bin(MMD_float x, MMD_float y, MMD_float z)
{
  int ix, iy, iz;

  if(x >= xprd)
    ix = (int)((x - xprd) * bininvx) + nbinx - mbinxlo;
  else if(x >= 0.0)
    ix = (int)(x * bininvx) - mbinxlo;
  else
    ix = (int)(x * bininvx) - mbinxlo - 1;

  if(y >= yprd)
    iy = (int)((y - yprd) * bininvy) + nbiny - mbinylo;
  else if(y >= 0.0)
    iy = (int)(y * bininvy) - mbinylo;
  else
    iy = (int)(y * bininvy) - mbinylo - 1;

  if(z >= zprd)
    iz = (int)((z - zprd) * bininvz) + nbinz - mbinzlo;
  else if(z >= 0.0)
    iz = (int)(z * bininvz) - mbinzlo;
  else
    iz = (int)(z * bininvz) - mbinzlo - 1;

  return (iz * mbiny * mbinx + iy * mbinx + ix + 1);
}
# 318 "neighbor.cpp"
int Neighbor::setup(Atom &atom)
{
  int i, j, k, nmax;
  MMD_float coord;
  int mbinxhi, mbinyhi, mbinzhi;
  int nextx, nexty, nextz;
  int num_omp_threads = threads->omp_num_threads;

  for(int i = 0; i<ntypes*ntypes; i++)
    cutneighsq[i] = cutneigh * cutneigh;

  xprd = atom.box.xprd;
  yprd = atom.box.yprd;
  zprd = atom.box.zprd;
# 348 "neighbor.cpp"
  binsizex = xprd / nbinx;
  binsizey = yprd / nbiny;
  binsizez = zprd / nbinz;
  bininvx = 1.0 / binsizex;
  bininvy = 1.0 / binsizey;
  bininvz = 1.0 / binsizez;

  coord = atom.box.xlo - cutneigh - 1.0e-6 * xprd;
  mbinxlo = static_cast<int>(coord * bininvx);

  if(coord < 0.0) mbinxlo = mbinxlo - 1;

  coord = atom.box.xhi + cutneigh + 1.0e-6 * xprd;
  mbinxhi = static_cast<int>(coord * bininvx);

  coord = atom.box.ylo - cutneigh - 1.0e-6 * yprd;
  mbinylo = static_cast<int>(coord * bininvy);

  if(coord < 0.0) mbinylo = mbinylo - 1;

  coord = atom.box.yhi + cutneigh + 1.0e-6 * yprd;
  mbinyhi = static_cast<int>(coord * bininvy);

  coord = atom.box.zlo - cutneigh - 1.0e-6 * zprd;
  mbinzlo = static_cast<int>(coord * bininvz);

  if(coord < 0.0) mbinzlo = mbinzlo - 1;

  coord = atom.box.zhi + cutneigh + 1.0e-6 * zprd;
  mbinzhi = static_cast<int>(coord * bininvz);



  mbinxlo = mbinxlo - 1;
  mbinxhi = mbinxhi + 1;
  mbinx = mbinxhi - mbinxlo + 1;

  mbinylo = mbinylo - 1;
  mbinyhi = mbinyhi + 1;
  mbiny = mbinyhi - mbinylo + 1;

  mbinzlo = mbinzlo - 1;
  mbinzhi = mbinzhi + 1;
  mbinz = mbinzhi - mbinzlo + 1;
# 405 "neighbor.cpp"
  nextx = static_cast<int>(cutneigh * bininvx);

  if(nextx * binsizex < 0.999 * cutneigh) nextx++;

  nexty = static_cast<int>(cutneigh * bininvy);

  if(nexty * binsizey < 0.999 * cutneigh) nexty++;

  nextz = static_cast<int>(cutneigh * bininvz);

  if(nextz * binsizez < 0.999 * cutneigh) nextz++;

  nmax = (2 * nextz + 1) * (2 * nexty + 1) * (2 * nextx + 1);

  if(stencil) free(stencil);

  stencil = (int*) malloc(nmax * sizeof(int));

  nstencil = 0;
  int kstart = -nextz;

  if(halfneigh && ghost_newton) {
    kstart = 0;
    stencil[nstencil++] = 0;
  }

  for(k = kstart; k <= nextz; k++) {
    for(j = -nexty; j <= nexty; j++) {
      for(i = -nextx; i <= nextx; i++) {
        if(!ghost_newton || !halfneigh || (k > 0 || j > 0 || (j == 0 && i > 0)))
          if(bindist(i, j, k) < cutneighsq[0]) {
            stencil[nstencil++] = k * mbiny * mbinx + j * mbinx + i;
          }
      }
    }
  }

  mbins = mbinx * mbiny * mbinz;

  if(bincount) free(bincount);

  bincount = (int*) malloc(mbins * num_omp_threads * sizeof(int));

  if(bins) free(bins);

  bins = (int*) malloc(mbins * num_omp_threads * atoms_per_bin * sizeof(int));
  return 0;
}



MMD_float Neighbor::bindist(int i, int j, int k)
{
  MMD_float delx, dely, delz;

  if(i > 0)
    delx = (i - 1) * binsizex;
  else if(i == 0)
    delx = 0.0;
  else
    delx = (i + 1) * binsizex;

  if(j > 0)
    dely = (j - 1) * binsizey;
  else if(j == 0)
    dely = 0.0;
  else
    dely = (j + 1) * binsizey;

  if(k > 0)
    delz = (k - 1) * binsizez;
  else if(k == 0)
    delz = 0.0;
  else
    delz = (k + 1) * binsizez;

  return (delx * delx + dely * dely + delz * delz);
}
