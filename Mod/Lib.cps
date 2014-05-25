MODULE LuaLib ['lua52'];
IMPORT
   SYSTEM,
   C := CApiTypes;

CONST

   LUA_VERSION_MAJOR*   = "5";
   LUA_VERSION_MINOR*   = "2";
   LUA_VERSION_NUM*     = 502;
   LUA_VERSION_RELEASE* = "3";

   LUA_VERSION*   = "Lua " + LUA_VERSION_MAJOR + "." + LUA_VERSION_MINOR;
   LUA_RELEASE*   = LUA_VERSION + "." + LUA_VERSION_RELEASE;
   LUA_COPYRIGHT* = LUA_RELEASE + "  Copyright (C) 1994-2013 Lua.org, PUC-Rio";
   LUA_AUTHORS*   = "R. Ierusalimschy, L. H. de Figueiredo, W. Celes";


   (* mark for precompiled code ('<esc>Lua') *)
   LUA_SIGNATURE*  = 01BX + "Lua";

   (* option for multiple returns in 'lua_pcall' and 'lua_call' *)
   LUA_MULTRET* = -1;


   (*
   @@ LUAI_MAXSTACK limits the size of the Lua stack.
   ** CHANGE it if you need a different limit. This limit is arbitrary;
   ** its only purpose is to stop Lua to consume unlimited stack
   ** space (and to reserve some numbers for pseudo-indices).
   *)
   LUAI_MAXSTACK = 15000;


   (* reserve some space for error handling *)
   LUAI_FIRSTPSEUDOIDX = -LUAI_MAXSTACK - 1000;

   (*
   ** pseudo-indices
   *)
   LUA_REGISTRYINDEX = LUAI_FIRSTPSEUDOIDX;
   (*lua_upvalueindex(i)   (LUA_REGISTRYINDEX - (i))*)


   (* thread status *)
   LUA_OK*        = 0;
   LUA_YIELD*     = 1;
   LUA_ERRRUN*    = 2;
   LUA_ERRSYNTAX* = 3;
   LUA_ERRMEM*    = 4;
   LUA_ERRGCMM*   = 5;
   LUA_ERRERR*    = 6;

   (*
   ** basic types
   *)
   LUA_TNONE* = -1;

   LUA_TNIL*           = 0;
   LUA_TBOOLEAN*       = 1;
   LUA_TLIGHTUSERDATA* = 2;
   LUA_TNUMBER*        = 3;
   LUA_TSTRING*        = 4;
   LUA_TTABLE*         = 5;
   LUA_TFUNCTION*      = 6;
   LUA_TUSERDATA*      = 7;
   LUA_TTHREAD*        = 8;

   LUA_NUMTAGS*        = 9;



   (* minimum Lua stack available to a C function *)
   LUA_MINSTACK* = 20;


   (* predefined values in the registry *)
   LUA_RIDX_MAINTHREAD* = 1;
   LUA_RIDX_GLOBALS*    = 2;
   LUA_RIDX_LAST*       = LUA_RIDX_GLOBALS;


   (*
   ** Comparison and arithmetic functions
   *)
   LUA_OPADD = 0;   (* ORDER TM *)
   LUA_OPSUB = 1;
   LUA_OPMUL = 2;
   LUA_OPDIV = 3;
   LUA_OPMOD = 4;
   LUA_OPPOW = 5;
   LUA_OPUNM = 6;

   LUA_OPEQ  = 0;
   LUA_OPLT  = 1;
   LUA_OPLE  = 2;


   (*
   ** garbage-collection options
   *)

   LUA_GCSTOP*        =  0;
   LUA_GCRESTART*     =  1;
   LUA_GCCOLLECT*     =  2;
   LUA_GCCOUNT*       =  3;
   LUA_GCCOUNTB*      =  4;
   LUA_GCSTEP*        =  5;
   LUA_GCSETPAUSE*    =  6;
   LUA_GCSETSTEPMUL*  =  7;
   LUA_GCSETMAJORINC* =  8;
   LUA_GCISRUNNING*   =  9;
   LUA_GCGEN*         = 10;
   LUA_GCINC*         = 11;

   (*
   ** Event codes
   *)
   LUA_HOOKCALL*     = 0;
   LUA_HOOKRET*      = 1;
   LUA_HOOKLINE*     = 2;
   LUA_HOOKCOUNT*    = 3;
   LUA_HOOKTAILCALL* = 4;


   (*
   ** Event masks
   *)
   LUA_MASKCALL*  = ASH(1, -LUA_HOOKCALL);
   LUA_MASKRET*   = ASH(1, -LUA_HOOKRET);
   LUA_MASKLINE*  = ASH(1, -LUA_HOOKLINE);
   LUA_MASKCOUNT* = ASH(1, -LUA_HOOKCOUNT);

   (* Lua standard library names *)
   LUA_COLIBNAME   = "coroutine";
   LUA_TABLIBNAME  = "table";
   LUA_IOLIBNAME   = "io";
   LUA_OSLIBNAME   = "os";
   LUA_STRLIBNAME  = "string";
   LUA_MATHLIBNAME = "math";
   LUA_DBLIBNAME   = "debug";
   LUA_LOADLIBNAME = "package";

   LUA_FILEHANDLE = "FILE*";
   LUA_IDSIZE = 60;
   LUAL_BUFFERSIZE = 4096;

   (* extra error code for 'luaL_load' *)
   LUA_ERRFILE = LUA_ERRERR + 1;

   (* pre-defined references *)
   LUA_NOREF  = -2;
   LUA_REFNIL = -1;

   

TYPE
   
   Int*          = C.signed_int;
   Char*         = C.char;
   UChar*        = C.unsigned_char;
   Size_t*       = C.size_t;
   PtrVoid*      = C.p_void;
   PtrChar*      = C.p_char;
   PtrPtrChar*   = C.p_p_char;
   lua_Number*   = C.double;
   PLua_Number*  = POINTER TO ARRAY [untagged] 1 OF lua_Number;
   lua_Integer*  = C.signed_int;
   lua_Unsigned* = C.unsigned_int;

   lua_State* = PtrVoid;

   lua_Debug* = POINTER TO RECORD [untagged]
      event: Int;
      name: PtrChar;  (* (n) *)
      namewhat: PtrChar; (* (n) 'global', 'local', 'field', 'method' *)
      what: PtrChar;  (* (S) 'Lua', 'C', 'main', 'tail' *)
      source: PtrChar;   (* (S) *)
      currentline: Int;   (* (l) *)
      linedefined: Int;   (* (S) *)
      lastlinedefined: Int;  (* (S) *)
      nups: UChar;   (* (u) number of upvalues *)
      nparams: UChar;(* (u) number of parameters *)
      isvararg: Char;        (* (u) *)
      istailcall: Char;   (* (t) *)
      short_src: ARRAY [untagged] LUA_IDSIZE OF Char; (* (S) *)
      (* private part *)
      i_ci: PtrVoid;  (* active function *)
   END;

   luaL_Reg* = POINTER TO RECORD [untagged]
      name: PtrChar;
      func: lua_CFunction;
   END;

   luaL_Buffer* = POINTER TO RECORD [untagged]
      b: PtrChar; (* buffer address *)
      size: Size_t; (* buffer size *)
      n: Size_t; (* number of characters in buffer *)
      L: lua_State; 
      initb: ARRAY [untagged] LUAL_BUFFERSIZE OF CHAR; (* initial buffer *)
   END;

   luaL_Stream* = RECORD [untagged]
      f: PtrVoid;
      closef: lua_CFunction;
   END;

   lua_CFunction* = PROCEDURE [ccall] (L: lua_State): Int;

(*
** functions that read/write blocks when loading/dumping Lua chunks
*)
lua_Reader* = PROCEDURE [ccall] (L: lua_State; ud: PtrVoid; VAR sz: Size_t): PtrChar;
lua_Writer* = PROCEDURE [ccall] (L: lua_State; IN p: PtrVoid; sz: Size_t; ud: PtrVoid): Int;

(*
** prototype for memory-allocation functions
*)
lua_Alloc* = PROCEDURE [ccall] (ud, ptr: PtrVoid; osize, nsize: Size_t): PtrVoid;

lua_Hook* = PROCEDURE [ccall] (L: lua_State; ar: lua_Debug): PtrVoid;

(*
** state manipulation
*)
PROCEDURE [ccall] lua_newstate*  ["lua_newstate" ] (f: lua_Alloc; ud: PtrVoid): lua_State;
PROCEDURE [ccall] lua_close*     ["lua_close"    ] (L: lua_State);
PROCEDURE [ccall] lua_newthread* ["lua_newthread"] (L: lua_State): lua_State; 

PROCEDURE [ccall] lua_atpanic* ["lua_atpanic"] (L: lua_State; panicf: lua_CFunction): lua_CFunction;


PROCEDURE [ccall] lua_version* ["lua_version"] (L: lua_State): PLua_Number;

(*
** basic stack manipulation
*)
PROCEDURE [ccall] lua_absindex*   ["lua_absindex"  ] (L: lua_State; idx: Int): Int;
PROCEDURE [ccall] lua_gettop*     ["lua_gettop"    ] (L: lua_State): Int;
PROCEDURE [ccall] lua_settop*     ["lua_settop"    ] (L: lua_State; idx: Int);
PROCEDURE [ccall] lua_pushvalue*  ["lua_pushvalue" ] (L: lua_State; idx: Int);
PROCEDURE [ccall] lua_remove*     ["lua_remove"    ] (L: lua_State; idx: Int);
PROCEDURE [ccall] lua_insert*     ["lua_insert"    ] (L: lua_State; idx: Int);
PROCEDURE [ccall] lua_replace*    ["lua_replace"   ] (L: lua_State; idx: Int);
PROCEDURE [ccall] lua_copy*       ["lua_copy"      ] (L: lua_State; fromidx, toidx: Int);
PROCEDURE [ccall] lua_checkstack* ["lua_checkstack"] (L: lua_State; sz: Int): Int;

PROCEDURE [ccall] lua_xmove*      ["lua_xmove"     ] (from, to: lua_State; n: Int);


(*
** access functions (stack -> C)
*)

PROCEDURE [ccall] lua_isnumber*    ["lua_isnumber"   ] (L: lua_State; idx: Int): Int;
PROCEDURE [ccall] lua_isstring*    ["lua_isstring"   ] (L: lua_State; idx: Int): Int;
PROCEDURE [ccall] lua_iscfunction* ["lua_iscfunction"] (L: lua_State; idx: Int): Int;
PROCEDURE [ccall] lua_isuserdata*  ["lua_isuserdata" ] (L: lua_State; idx: Int): Int;
PROCEDURE [ccall] lua_type*        ["lua_type"       ] (L: lua_State; idx: Int): Int;
PROCEDURE [ccall] lua_typename*    ["lua_typename"   ] (L: lua_State; tp: Int): PtrChar;

PROCEDURE [ccall] lua_tonumberx*   ["lua_tonumberx"  ] (L: lua_State; idx: Int; VAR isnum: Int): lua_Number;
PROCEDURE [ccall] lua_tointegerx*  ["lua_tointegerx" ] (L: lua_State; idx: Int; VAR isnum: Int): lua_Integer;
PROCEDURE [ccall] lua_tounsignedx* ["lua_tounsignedx"] (L: lua_State; idx: Int; VAR isnum: Int): lua_Unsigned;
PROCEDURE [ccall] lua_toboolean*   ["lua_toboolean"  ] (L: lua_State; idx: Int): Int;
PROCEDURE [ccall] lua_tolstring*   ["lua_tolstring"  ] (L: lua_State; idx: Int; VAR len: Size_t): PtrChar;
PROCEDURE [ccall] lua_rawlen*      ["lua_rawlen"     ] (L: lua_State; idx: Int): Size_t;
PROCEDURE [ccall] lua_tocfunction* ["lua_tocfunction"] (L: lua_State; idx: Int): lua_CFunction;
PROCEDURE [ccall] lua_touserdata*  ["lua_touserdata" ] (L: lua_State; idx: Int): PtrVoid;
PROCEDURE [ccall] lua_tothread*    ["lua_tothread"   ] (L: lua_State; idx: Int): lua_State;
PROCEDURE [ccall] lua_topointer*   ["lua_topointer"  ] (L: lua_State; idx: Int): PtrVoid;

(*
** Comparison and arithmetic functions
*)
PROCEDURE [ccall] lua_arith* ["lua_arith"] (L: lua_State; op: Int);
PROCEDURE [ccall] lua_rawequal* ["lua_rawequal"] (L: lua_State; idx1, idx2: Int): Int;
PROCEDURE [ccall] lua_compare* ["lua_compare"] (L: lua_State; idx1, idx2, op: Int): Int;

(*
** push functions (C -> stack)
*)
PROCEDURE [ccall] lua_pushnil* ["lua_pushnil"] (L: lua_State);
PROCEDURE [ccall] lua_pushnumber* ["lua_pushnumber"] (L: lua_State; n: lua_Number);
PROCEDURE [ccall] lua_pushinteger* ["lua_pushinteger"] (L: lua_State; n: lua_Integer);
PROCEDURE [ccall] lua_pushunsigned* ["lua_pushunsigned"] (L: lua_State; n: lua_Unsigned);
PROCEDURE [ccall] lua_pushlstring* ["lua_pushlstring"] (L: lua_State; s: PtrChar; l: Size_t): PtrChar;
PROCEDURE [ccall] lua_pushstring* ["lua_pushstring"] (L: lua_State; s: PtrChar): PtrChar;
(*
   PROCEDURE [ccall] lua_pushvfstring* ["lua_pushvfstring"] (L: lua_State; fmt: PtrChar; argp: va_list): PtrChar;
   PROCEDURE [ccall] lua_pushfstring* ["lua_pushfstring"] (L: lua_State; fmt: PtrChar; ...): PtrChar;
*)
PROCEDURE [ccall] lua_pushcclosure* ["lua_pushcclosure"] (L: lua_State; fn: lua_CFunction; n: Int);
PROCEDURE [ccall] lua_pushboolean* ["lua_pushboolean"] (L: lua_State; b: Int);
PROCEDURE [ccall] lua_pushlightuserdata* ["lua_pushlightuserdata"] (L: lua_State; p: PtrVoid);
PROCEDURE [ccall] lua_pushthread* ["lua_pushthread"] (L: lua_State): Int;


(*
** get functions (Lua -> stack)
*)
PROCEDURE [ccall] lua_getglobal* ["lua_getglobal"] (L: lua_State; var: PtrChar);
PROCEDURE [ccall] lua_gettable* ["lua_gettable"] (L: lua_State; idx: Int);
PROCEDURE [ccall] lua_getfield* ["lua_getfield"] (L: lua_State; idx: Int; k: PtrChar);
PROCEDURE [ccall] lua_rawget* ["lua_rawget"] (L: lua_State; idx: Int);
PROCEDURE [ccall] lua_rawgeti* ["lua_rawgeti"] (L: lua_State; idx, n: Int);
PROCEDURE [ccall] lua_rawgetp* ["lua_rawgetp"] (L: lua_State; idx: Int; p: PtrVoid);
PROCEDURE [ccall] lua_createtable* ["lua_createtable"] (L: lua_State; narr, nrec: Int);
PROCEDURE [ccall] lua_newuserdata* ["lua_newuserdata"] (L: lua_State; sz: Size_t): PtrVoid;
PROCEDURE [ccall] lua_getmetatable* ["lua_getmetatable"] (L: lua_State; objindex: Int): Int;
PROCEDURE [ccall] lua_getuservalue* ["lua_getuservalue"] (L: lua_State; idx: Int);

(*
** set functions (stack -> Lua)
*)
PROCEDURE [ccall] lua_setglobal* ["lua_setglobal"] (L: lua_State; var: PtrChar);
PROCEDURE [ccall] lua_settable* ["lua_settable"] (L: lua_State; idx: Int);
PROCEDURE [ccall] lua_setfield* ["lua_setfield"] (L: lua_State; idx: Int; k: PtrChar);
PROCEDURE [ccall] lua_rawset* ["lua_rawset"] (L: lua_State; idx: Int);
PROCEDURE [ccall] lua_rawseti* ["lua_rawseti"] (L: lua_State; idx, n: Int);
PROCEDURE [ccall] lua_rawsetp* ["lua_rawsetp"] (L: lua_State; idx: Int; p: PtrVoid);
PROCEDURE [ccall] lua_setmetatable* ["lua_setmetatable"] (L: lua_State; objindex: Int): Int;
PROCEDURE [ccall] lua_setuservalue* ["lua_setuservalue"] (L: lua_State; idx: Int);

(*
** 'load' and 'call' functions (load and run Lua code)
*)
PROCEDURE [ccall] lua_callk* ["lua_callk"] (L: lua_State; nargs, nresults, ctx: Int; k: lua_CFunction);
(* #define lua_call(L,n,r)    lua_callk(L, (n), (r), 0, NULL) *)
PROCEDURE [ccall] lua_getctx* ["lua_getctx"] (L: lua_State; VAR ctx: Int): Int;
PROCEDURE [ccall] lua_pcallk* ["lua_pcallk"] (L: lua_State; nargs, nresults, errfunc, ctx: Int; k: lua_CFunction): Int;
(* #define lua_pcall(L,n,r,f) lua_pcallk(L, (n), (r), (f), 0, NULL) *)
PROCEDURE [ccall] lua_load* ["lua_load"] (L: lua_State; reader: lua_Reader; dt: PtrVoid; chunkname, mode: PtrChar): Int;
PROCEDURE [ccall] lua_dump* ["lua_dump"] (L: lua_State; writer: lua_Writer; data: PtrVoid): Int;

(*
** coroutine functions
*)
PROCEDURE [ccall] lua_yieldk* ["lua_yieldk"] (L: lua_State; nresults, ctx: Int; k: lua_CFunction): Int;
(* #define lua_yield(L,n)     lua_yieldk(L, (n), 0, NULL) *)
PROCEDURE [ccall] lua_resume* ["lua_resume"] (L, from: lua_State; narg: Int): Int;
PROCEDURE [ccall] lua_status* ["lua_status"] (L: lua_State): Int;

(*
** garbage-collection function and options
*)
PROCEDURE [ccall] lua_gc* ["lua_gc"] (L: lua_State; what, data: Int): Int;

(*
** miscellaneous functions
*)
PROCEDURE [ccall] lua_error* ["lua_error"] (L: lua_State): Int;
PROCEDURE [ccall] lua_next* ["lua_next"] (L: lua_State; idx: Int): Int;
PROCEDURE [ccall] lua_concat* ["lua_concat"] (L: lua_State; n: Int);
PROCEDURE [ccall] lua_len* ["lua_len"] (L: lua_State; idx: Int);
PROCEDURE [ccall] lua_getallocf* ["lua_getallocf"] (L: lua_State; VAR ud: PtrVoid): lua_Alloc;
PROCEDURE [ccall] lua_setallocf* ["lua_setallocf"] (L: lua_State; f: lua_Alloc; ud: PtrVoid);

(*
** ===============================================================
** some useful macros
** ===============================================================
*)

(*
#define lua_tonumber(L,i)  lua_tonumberx(L,i,NULL)
#define lua_tointeger(L,i) lua_tointegerx(L,i,NULL)
#define lua_tounsigned(L,i)   lua_tounsignedx(L,i,NULL)

#define lua_pop(L,n)    lua_settop(L, -(n)-1)

#define lua_newtable(L)    lua_createtable(L, 0, 0)

#define lua_register(L,n,f) (lua_pushcfunction(L, (f)), lua_setglobal(L, (n)))

#define lua_pushcfunction(L,f)   lua_pushcclosure(L, (f), 0)

#define lua_isfunction(L,n)   (lua_type(L, (n)) == LUA_TFUNCTION)
#define lua_istable(L,n)   (lua_type(L, (n)) == LUA_TTABLE)
#define lua_islightuserdata(L,n) (lua_type(L, (n)) == LUA_TLIGHTUSERDATA)
#define lua_isnil(L,n)     (lua_type(L, (n)) == LUA_TNIL)
#define lua_isboolean(L,n) (lua_type(L, (n)) == LUA_TBOOLEAN)
#define lua_isthread(L,n)  (lua_type(L, (n)) == LUA_TTHREAD)
#define lua_isnone(L,n)    (lua_type(L, (n)) == LUA_TNONE)
#define lua_isnoneornil(L, n) (lua_type(L, (n)) <= 0)

#define lua_pushliteral(L, s) \
   lua_pushlstring(L, "" s, (sizeof(s)/sizeof(char))-1)

#define lua_pushglobaltable(L)  \
   lua_rawgeti(L, LUA_REGISTRYINDEX, LUA_RIDX_GLOBALS)

#define lua_tostring(L,i)  lua_tolstring(L, (i), NULL)
*)

(*
** {======================================================================
** Debug API
** =======================================================================
*)
PROCEDURE [ccall] lua_getstack* ["lua_getstack"] (L: lua_State; level: Int; ar: lua_Debug): Int;
PROCEDURE [ccall] lua_getinfo* ["lua_getinfo"] (L: lua_State; what: PtrChar; ar: lua_Debug): Int;
PROCEDURE [ccall] lua_getlocal* ["lua_getlocal"] (L: lua_State; ar: lua_Debug; n: Int): PtrChar;
PROCEDURE [ccall] lua_setlocal* ["lua_setlocal"] (L: lua_State; ar: lua_Debug; n: Int): PtrChar;
PROCEDURE [ccall] lua_getupvalue* ["lua_getupvalue"] (L: lua_State; funcindex, n: Int): PtrChar;
PROCEDURE [ccall] lua_setupvalue* ["lua_setupvalue"] (L: lua_State; funcindex, n: Int): PtrChar;
PROCEDURE [ccall] lua_upvaluei* ["lua_upvaluei"] (L: lua_State; fidx, n: Int): PtrVoid;
PROCEDURE [ccall] lua_upvaluejoi* ["lua_upvaluejoi"] (L: lua_State; fidx1, n1, fidx2, n2: Int);
PROCEDURE [ccall] lua_sethook* ["lua_sethook"] (L: lua_State; func: lua_Hook; mask, count: Int): Int;
PROCEDURE [ccall] lua_gethook* ["lua_gethook"] (L: lua_State): lua_Hook;
PROCEDURE [ccall] lua_gethookmask* ["lua_gethookmask"] (L: lua_State): Int;
PROCEDURE [ccall] lua_gethookcount* ["lua_gethookcount"] (L: lua_State): Int;

(* standard libraries *)

PROCEDURE [ccall] luaopen_base* ["luaopen_base"] (L: lua_State): Int;
PROCEDURE [ccall] luaopen_coroutine* ["luaopen_coroutine"] (L: lua_State): Int;
PROCEDURE [ccall] luaopen_table* ["luaopen_table"] (L: lua_State): Int;
PROCEDURE [ccall] luaopen_io* ["luaopen_io"] (L: lua_State): Int;
PROCEDURE [ccall] luaopen_os* ["luaopen_os"] (L: lua_State): Int;
PROCEDURE [ccall] luaopen_string* ["luaopen_string"] (L: lua_State): Int;
PROCEDURE [ccall] luaopen_bit32* ["luaopen_bit32"] (L: lua_State): Int;
PROCEDURE [ccall] luaopen_math* ["luaopen_math"] (L: lua_State): Int;
PROCEDURE [ccall] luaopen_debug* ["luaopen_debug"] (L: lua_State): Int;
PROCEDURE [ccall] luaopen_package* ["luaopen_package"] (L: lua_State): Int;

(* Auxiliary functions for building Lua libraries *)

PROCEDURE [ccall] luaL_checkversion_* ["luaL_checkversion_"] (L: lua_State; ver: lua_Number);
(* #define luaL_checkversion(L)  luaL_checkversion_(L, LUA_VERSION_NUM) *)
PROCEDURE [ccall] luaL_getmetafield* ["luaL_getmetafield"] (L: lua_State; obj: Int; e: PtrChar): Int;
PROCEDURE [ccall] luaL_callmeta* ["luaL_callmeta"] (L: lua_State; obj: Int; e: PtrChar): Int;
PROCEDURE [ccall] luaL_tolstring* ["luaL_tolstring"] (L: lua_State; idx: Int; VAR len: Size_t): PtrChar;
PROCEDURE [ccall] luaL_argerror* ["luaL_argerror"] (L: lua_State; numarg: Int; extramsg: PtrChar): Int;
PROCEDURE [ccall] luaL_checklstring* ["luaL_checklstring"] (L: lua_State; numArg: Int; VAR l: Size_t): PtrChar;
PROCEDURE [ccall] luaL_optlstring* ["luaL_optlstring"] (L: lua_State; numArg: Int; def: PtrChar; VAR l: Size_t): PtrChar;
PROCEDURE [ccall] luaL_checknumber* ["luaL_checknumber"] (L: lua_State; numArg: Int): lua_Number;
PROCEDURE [ccall] luaL_optnumber* ["luaL_optnumber"] (L: lua_State; nArg: Int; def: lua_Number): lua_Number;
PROCEDURE [ccall] luaL_checkinteger* ["luaL_checkinteger"] (L: lua_State; numArg: Int): lua_Integer;
PROCEDURE [ccall] luaL_optinteger* ["luaL_optinteger"] (L: lua_State; nArg: Int; def: lua_Integer): lua_Integer;
PROCEDURE [ccall] luaL_checkunsigned* ["luaL_checkunsigned"] (L: lua_State; numArg: Int): lua_Unsigned;
PROCEDURE [ccall] luaL_optunsigned* ["luaL_optunsigned"] (L: lua_State; numArg: Int; def: lua_Unsigned): lua_Unsigned;
PROCEDURE [ccall] luaL_checkstack* ["luaL_checkstack"] (L: lua_State; sz: Int; msg: PtrChar);
PROCEDURE [ccall] luaL_checktype* ["luaL_checktype"] (L: lua_State; narg, t: Int);
PROCEDURE [ccall] luaL_checkany* ["luaL_checkany"] (L: lua_State; narg: Int);
PROCEDURE [ccall] luaL_newmetatable* ["luaL_newmetatable"] (L: lua_State; tname: PtrChar): Int;
PROCEDURE [ccall] luaL_setmetatable* ["luaL_setmetatable"] (L: lua_State; tname: PtrChar);
PROCEDURE [ccall] luaL_testudata* ["luaL_testudata"] (L: lua_State; ud: Int; tname: PtrChar): PtrVoid;
PROCEDURE [ccall] luaL_checkudata* ["luaL_checkudata"] (L: lua_State; ud: Int; tname: PtrChar): PtrVoid;
PROCEDURE [ccall] luaL_where* ["luaL_where"] (L: lua_State; lvl: Int);
(*
PROCEDURE [ccall] luaL_error* ["luaL_error"] (L: lua_State; fmt: PtrChar; ...): Int;
*)
PROCEDURE [ccall] luaL_checkoption* ["luaL_checkoption"] (L: lua_State; narg: Int; def: PtrChar; lst: PtrPtrChar): Int;
PROCEDURE [ccall] luaL_fileresult* ["luaL_fileresult"] (L: lua_State; stat: Int; fname: PtrChar): Int;
PROCEDURE [ccall] luaL_execresult* ["luaL_execresult"] (L: lua_State; stat: Int): Int;


PROCEDURE [ccall] luaL_ref* ["luaL_ref"] (L: lua_State; t: Int): Int;
PROCEDURE [ccall] luaL_unref* ["luaL_unref"] (L: lua_State; t, ref: Int);
PROCEDURE [ccall] luaL_loadfilex* ["luaL_loadfilex"] (L: lua_State; filename, mode: PtrChar): Int;
(* #define luaL_loadfile(L,f) luaL_loadfilex(L,f,NULL) *)
PROCEDURE [ccall] luaL_loadbufferx* ["luaL_loadbufferx"] (L: lua_State; buff: PtrChar; sz: Size_t; name, mode: PtrChar): Int;
PROCEDURE [ccall] luaL_loadstring* ["luaL_loadstring"] (L: lua_State; s: PtrChar): Int;
PROCEDURE [ccall] luaL_newstate* ["luaL_newstate"] (): lua_State;
PROCEDURE [ccall] luaL_len* ["luaL_len"] (L: lua_State; idx: Int): Int;
PROCEDURE [ccall] luaL_gsub* ["luaL_gsub"] (L: lua_State; s, p, r: PtrChar): PtrChar;
PROCEDURE [ccall] luaL_setfuncs* ["luaL_setfuncs"] (L: lua_State; l: luaL_Reg; nup: Int);
PROCEDURE [ccall] luaL_getsubtable* ["luaL_getsubtable"] (L: lua_State; idx: Int; fname: PtrChar): Int;
PROCEDURE [ccall] luaL_traceback* ["luaL_traceback"] (L, L1: lua_State; msg: PtrChar; level: Int);
PROCEDURE [ccall] luaL_requiref* ["luaL_requiref"] (L: lua_State; modname: PtrChar; openf: lua_CFunction; glb: Int);

(*
** ===============================================================
** some useful macros
** ===============================================================
*)

(*

#define luaL_newlibtable(L,l) \
  lua_createtable(L, 0, sizeof(l)/sizeof((l)[0]) - 1)

#define luaL_newlib(L,l)   (luaL_newlibtable(L,l), luaL_setfuncs(L,l,0))

#define luaL_argcheck(L, cond,numarg,extramsg)  \
      ((void)((cond) || luaL_argerror(L, (numarg), (extramsg))))
#define luaL_checkstring(L,n) (luaL_checklstring(L, (n), NULL))
#define luaL_optstring(L,n,d) (luaL_optlstring(L, (n), (d), NULL))
#define luaL_checkint(L,n) ((int)luaL_checkinteger(L, (n)))
#define luaL_optint(L,n,d) ((int)luaL_optinteger(L, (n), (d)))
#define luaL_checklong(L,n)   ((long)luaL_checkinteger(L, (n)))
#define luaL_optlong(L,n,d)   ((long)luaL_optinteger(L, (n), (d)))

#define luaL_typename(L,i) lua_typename(L, lua_type(L,(i)))

#define luaL_dofile(L, fn) \
   (luaL_loadfile(L, fn) || lua_pcall(L, 0, LUA_MULTRET, 0))

#define luaL_dostring(L, s) \
   (luaL_loadstring(L, s) || lua_pcall(L, 0, LUA_MULTRET, 0))

#define luaL_getmetatable(L,n)   (lua_getfield(L, LUA_REGISTRYINDEX, (n)))

#define luaL_opt(L,f,n,d)  (lua_isnoneornil(L,(n)) ? (d) : f(L,(n)))

#define luaL_loadbuffer(L,s,sz,n)   luaL_loadbufferx(L,s,sz,n,NULL)

*)

(*
** {======================================================
** Generic Buffer manipulation
** =======================================================
*)

(*
#define luaL_addchar(B,c) \
  ((void)((B)->n < (B)->size || luaL_prepbuffsize((B), 1)), \
   ((B)->b[(B)->n++] = (c)))

#define luaL_addsize(B,s)  ((B)->n += (s))
*)


PROCEDURE [ccall] luaL_buffinit* ["luaL_buffinit"] (L: lua_State; B: luaL_Buffer);
PROCEDURE [ccall] luaL_prepbuffsize* ["luaL_prepbuffsize"] (B: luaL_Buffer; sz: Size_t): PtrChar;
PROCEDURE [ccall] luaL_addlstring* ["luaL_addlstring"] (B: luaL_Buffer; s: PtrChar; l: Size_t);
PROCEDURE [ccall] luaL_addstring* ["luaL_addstring"] (B: luaL_Buffer; s: PtrChar);
PROCEDURE [ccall] luaL_addvalue* ["luaL_addvalue"] (B: luaL_Buffer);
PROCEDURE [ccall] luaL_pushresult* ["luaL_pushresult"] (B: luaL_Buffer);
PROCEDURE [ccall] luaL_pushresultsize* ["luaL_pushresultsize"] (B: luaL_Buffer; sz: Size_t);
PROCEDURE [ccall] luaL_buffinitsize* ["luaL_buffinitsize"] (L: lua_State; B: luaL_Buffer; sz: Size_t): PtrChar;

(*
#define luaL_prepbuffer(B) luaL_prepbuffsize(B, LUAL_BUFFERSIZE)
*)



END LuaLib.
(******************************************************************************
* Copyright (C) 1994-2013 Lua.org, PUC-Rio.
*
* Permission is hereby granted, free of charge, to any person obtaining
* a copy of this software and associated documentation files (the
* "Software"), to deal in the Software without restriction, including
* without limitation the rights to use, copy, modify, merge, publish,
* distribute, sublicense, and/or sell copies of the Software, and to
* permit persons to whom the Software is furnished to do so, subject to
* the following conditions:
*
* The above copyright notice and this permission notice shall be
* included in all copies or substantial portions of the Software.
*
* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
* EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
* MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
* IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
* CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
* TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
* SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
******************************************************************************)