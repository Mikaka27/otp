/*
 * %CopyrightBegin%
 *
 * SPDX-License-Identifier: Apache-2.0
 * 
 * Copyright Ericsson AB 2003-2025. All Rights Reserved.
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 * 
 * %CopyrightEnd%
 */

#include <windows.h>
#include <stdio.h>
#include <stdlib.h>
#include "init_file.h"

typedef int ErlexecFunction(int, char **, HANDLE);

#define INI_FILENAME L"erl.ini"
#define INI_SECTION "erlang"
#define ERLEXEC_BASENAME L"erlexec.dll"
#define MAX(x,y)  ((x) > (y) ? (x) : (y))

#define IS_SLASH(a)  ((a) == L'\\' || (a) == L'/')

/* Long paths can either be in the file (?) or the device (.) namespace. UNC
 * paths are always in the file namespace. */
#define LP_FILE_PREFIX L"\\\\?\\"
#define LP_DEV_PREFIX L"\\\\.\\"
#define LP_UNC_PREFIX (LP_FILE_PREFIX L"UNC\\")

#define LP_PREFIX_SIZE (sizeof(LP_FILE_PREFIX) - sizeof(wchar_t))
#define LP_PREFIX_LENGTH (LP_PREFIX_SIZE / sizeof(wchar_t))

#define LP_UNC_PREFIX_SIZE (sizeof(LP_UNC_PREFIX) - sizeof(wchar_t))
#define LP_UNC_PREFIX_LENGTH (LP_UNC_PREFIX_SIZE / sizeof(wchar_t))

#define IS_LONG_PATH(length, data) \
    ((length) >= LP_PREFIX_LENGTH && \
         (!memcmp((data), LP_FILE_PREFIX, LP_PREFIX_SIZE) || \
          !memcmp((data), LP_DEV_PREFIX, LP_PREFIX_SIZE)))

#define IS_LONG_UNC_PATH(length, data) \
    ((length) >= LP_UNC_PREFIX_LENGTH && \
         !memcmp((data), LP_UNC_PREFIX, LP_UNC_PREFIX_SIZE))

static void get_parameters(void);
static void error(char* format, ...);

static wchar_t *erlexec_name;
static wchar_t *erlexec_dir;

int wmain(int argc, wchar_t **argv)
{
  HANDLE erlexec_handle; /* Instance */
  ErlexecFunction *win_erlexec;
  wchar_t *path = malloc(100*sizeof(wchar_t));
  wchar_t *wslpath = malloc(100*sizeof(wchar_t));
  wchar_t *npath;
  int pathlen, wslpathlen;
  char ** utf8argv;
  int i, len;

  get_parameters();

  if ((pathlen = GetEnvironmentVariableW(L"PATH",path,100)) == 0) {
    error("No PATH variable (!)");
  } else if (pathlen > 100) {
    path = realloc(path,pathlen*sizeof(wchar_t));
    GetEnvironmentVariableW(L"PATH",path,pathlen);
  }

  if ((wslpathlen = GetEnvironmentVariableW(L"WSLENV",wslpath,100)) > 0) {
      if ((wslpathlen = GetEnvironmentVariableW(L"WSLPATH",wslpath,100)) > 0) {
          if (wslpathlen > 100) {
              wslpath = realloc(wslpath,wslpathlen*sizeof(wchar_t));
              GetEnvironmentVariableW(L"WSLPATH",wslpath,wslpathlen);
          }
          wslpathlen = wcslen(wslpath);
      }
  }
  /* Add size for path delimiters and eos */
  pathlen = (wcslen(path) + wslpathlen + wcslen(erlexec_dir) + 3);
  npath = (wchar_t *) malloc(pathlen*sizeof(wchar_t));
  if(wslpathlen > 0) {
      swprintf(npath,pathlen,L"%s;%s;%s",erlexec_dir,path,wslpath);
  } else {
      swprintf(npath,pathlen,L"%s;%s",erlexec_dir,path);
  }
  SetEnvironmentVariableW(L"PATH",npath);

  if ((erlexec_handle = LoadLibraryW(erlexec_name)) == NULL) {
    error("Could not load module %S.",erlexec_name);
  }

  if ((win_erlexec = (ErlexecFunction *) 
       GetProcAddress(erlexec_handle,"win_erlexec")) == NULL) {
    error("Could not find entry point \"win_erlexec\" in %S.", erlexec_name);
  }

  /* Convert argv to utf8 */
  utf8argv = malloc((argc+1) * sizeof(char*));
  for (i=0; i<argc; i++) {
      len = WideCharToMultiByte(CP_UTF8, 0, argv[i], -1, NULL, 0, NULL, NULL);
      utf8argv[i] = malloc(len*sizeof(char));
      WideCharToMultiByte(CP_UTF8, 0, argv[i], -1, utf8argv[i], len, NULL, NULL);
  }
  utf8argv[argc] = NULL;

#ifdef HARDDEBUG
	{
	    wchar_t tempbuf[2048] = L"";
	    wchar_t *sbuf;
	    int i;
	    sbuf=tempbuf;
	    sbuf += swprintf(sbuf, 2048, L"utf16: %d\n", argc);
	    for (i = 0; i < argc; ++i) {
		sbuf += swprintf(sbuf, 2048, L"|%s|", argv[i]);
	    };
	    sbuf += swprintf(sbuf, 2048, L"\nutf8: \n");
	    for (i = 0; i < argc; ++i) {
		sbuf += swprintf(sbuf, 2048, L"|%S|", utf8argv[i]);
	    };
	    MessageBoxW(NULL, tempbuf, L"erl_exec args", MB_OK|MB_ICONERROR);
	}
#endif

  return (*win_erlexec)(argc,utf8argv,erlexec_handle);
  
} 


static wchar_t *replace_filename(wchar_t *path, wchar_t *new_base) 
{
    int plen = wcslen(path);
    wchar_t *res = malloc((plen+wcslen(new_base)+1)*sizeof(wchar_t));
    wchar_t *p;

    wcscpy(res,path);
    for (p = res+plen-1 ;p >= res && *p != L'\\'; --p)
        ;
    *(p+1) =L'\0';
    wcscat(res,new_base);
    return res;
}

static char *do_lookup_in_section(InitSection *inis, char *name, 
				  char *section, wchar_t *filename)
{
    char *p = lookup_init_entry(inis, name);

    if (p == NULL) {
	error("Could not find key %s in section %s of file %S",
	      name,section,filename);
    }
    return p;
}

static void copy_latest_vsn(wchar_t *latest_vsn, wchar_t *next_vsn) 
{
    /* Copy */
    wchar_t *lp;
    wchar_t *np;
    /* Find vsn */
    for (lp = next_vsn+wcslen(next_vsn)-1 ;lp >= next_vsn && *lp != L'\\'; --lp)
        ;
    /* lp =+ length("erts-"); */
    for (np = next_vsn+wcslen(next_vsn)-1 ;np >= next_vsn && *np != L'\\'; --np)
        ;
    /* np =+ length("erts-"); */
    
    for (; lp && np; ++lp, ++np) {
	if (*lp == *np) {
	    continue;
	}	
	if (*np == L'.' || *np == L'\0' || *np <= *lp) {
	/* */
	    return;
	}
	if (*lp == L'.' || *lp == L'\0') {
	    wcscpy(latest_vsn, next_vsn);
	    return;
	}
    }
    return;
}

static wchar_t *get_full_path(wchar_t *input)
{
    DWORD maximum_length, actual_length;
    int is_long_path, maybe_unc_path;
    wchar_t *result;
    wchar_t *path_start;

    maximum_length = GetFullPathNameW(input, 0, NULL, NULL);

    if(maximum_length == 0) {
        return NULL;
    }

    maximum_length += MAX(LP_PREFIX_LENGTH, LP_UNC_PREFIX_LENGTH);
    result = (wchar_t *) malloc(maximum_length * sizeof(wchar_t));
    actual_length = GetFullPathNameW(input, maximum_length, result, NULL);
    if(actual_length == 0) {
        free(result);
        return NULL;
    }

    /* The APIs we use have varying path length limits and sometimes
        * behave differently when given a long-path prefix, so it's simplest
        * to always use long paths. */

    is_long_path = IS_LONG_PATH(actual_length, result);
    maybe_unc_path = !memcmp(result, L"\\\\", sizeof(wchar_t) * 2);

    if(maybe_unc_path && !is_long_path) {
        /* \\localhost\c$\gurka -> \\?\UNC\localhost\c$\gurka
            *
            * Note that the length is reduced by 2 as the "\\" is replaced by
            * the UNC prefix */
        memmove(result + LP_UNC_PREFIX_LENGTH,
            &((wchar_t*)result)[2],
            (actual_length + 1 - 2) * sizeof(wchar_t));
        memcpy(result, LP_UNC_PREFIX, LP_UNC_PREFIX_SIZE);
        actual_length += LP_UNC_PREFIX_LENGTH - 2;
    } else if(!is_long_path) {
        /* C:\gurka -> \\?\C:\gurka */
        memmove(result + LP_PREFIX_LENGTH, result,
            (actual_length + 1) * sizeof(wchar_t));
        memcpy(result, LP_FILE_PREFIX, LP_PREFIX_SIZE);
        actual_length += LP_PREFIX_LENGTH;
    }

    path_start = (wchar_t*)result;

    /* We're removing trailing slashes since quite a few APIs refuse to
        * work with them, and none require them. We only check the last
        * character since GetFullPathNameW folds slashes together. */
    if(IS_SLASH(path_start[actual_length - 1])) {
        if(path_start[actual_length - 2] != L':') {
            path_start[actual_length - 1] = L'\0';
        }
    }

    return result;
}

static wchar_t *find_erlexec_dir2(wchar_t *install_dir) 
{
    /* List install dir and look for latest erts-vsn */

    HANDLE dir_handle;	        /* Handle to directory. */
    wchar_t *wildcard;	        /* Wildcard to search for. */
    WIN32_FIND_DATAW find_data;  /* Data found by FindFirstFile() or FindNext(). */
    wchar_t latest_vsn[MAX_PATH];

    /* Setup wildcard */
    int length = wcslen(install_dir);
    wchar_t *p;

    wildcard = (wchar_t *) malloc((length * sizeof(wchar_t)) + sizeof(L"\\erts-*"));

    wcscpy(wildcard, install_dir);
    p = wildcard+length-1;
    if (*p != L'/' && *p != L'\\')
	*++p = L'\\';
    wcscpy(++p, L"erts-*");

    /* Find first dir */
    dir_handle = FindFirstFileW(wildcard, &find_data);
    if (dir_handle == INVALID_HANDLE_VALUE) {
	/* No erts-vsn found*/
	free(wildcard);
	return NULL;
    }

    wcscpy(latest_vsn, find_data.cFileName);

    /* Find the rest */
    while(FindNextFileW(dir_handle, &find_data)) {
	copy_latest_vsn(latest_vsn, find_data.cFileName);
    }
    
    FindClose(dir_handle);

    p = (wchar_t *) malloc((wcslen(install_dir)+1+wcslen(latest_vsn)+4+1)*sizeof(wchar_t));

    wcscpy(p,install_dir);
    wcscat(p,L"\\");
    wcscat(p,latest_vsn);
    wcscat(p,L"\\bin");

    free(wildcard);

    return p;
}

static wchar_t *find_erlexec_dir(wchar_t *erlpath) 
{
    /* Assume that the path to erl is absolute and
     * that it is not a symbolic link*/
    
    wchar_t *dir = _wcsdup(erlpath);
    wchar_t *unc_dir;
    wchar_t *p;
    wchar_t *p2;

    wchar_t *long_dir;
    DWORD length;
    
    /* Chop of base name*/
    for (p = dir+wcslen(dir)-1 ;p >= dir && *p != L'\\'; --p)
        ;
    *p =L'\0';
    p--;

    length = GetLongPathNameW(dir, NULL, 0);
    if(length == 0) {
        error("Cannot find erlexec.dll");
    }
    long_dir = (wchar_t *) malloc(length * sizeof(wchar_t));
    if(GetLongPathNameW(dir, long_dir, length) == 0) {
        error("Cannot find erlexec.dll");
    }
    p = long_dir + wcslen(long_dir) - 1;

    /* Check if dir path is like ...\install_dir\erts-vsn\bin */
    for (;p >= long_dir && *p != L'\\'; --p)
        ;
    p--;

    for (p2 = p;p2 >= long_dir && *p2 != '\\'; --p2)
        ;
    p2++;
    if (wcsncmp(p2, L"erts-", wcslen(L"erts-")) == 0) {
	p = _wcsdup(dir);
	free(long_dir);
	free(dir);
	return p;
    }
    
    free(long_dir);

    /* Assume that dir path is like ...\install_dir\bin */
    unc_dir = get_full_path(dir);
    free(dir);
    if(unc_dir == NULL) {
        error("Cannot find erlexec.dll");
    }

    /* Chop of base name*/
    for (p = unc_dir+wcslen(unc_dir)-1 ;p >= unc_dir && *p != L'\\'; --p)
        ;
    *p =L'\0';
    p--;
    for (;p >= unc_dir && *p != L'\\'; --p)
        ;
    p--;
    *++p =L'\0'; /* chop off bin dir */

    p = find_erlexec_dir2(unc_dir);
    free(unc_dir);
    if (p == NULL) {
	error("Cannot find erlexec.dll");
    } else {
	return p;
    }
}

static void get_parameters(void)
{
    wchar_t buffer[MAX_PATH];
    wchar_t *ini_filename;
    HANDLE module = GetModuleHandle(NULL);
    InitFile *inif;
    InitSection *inis;
    char *utf8dir;
    int len;


    if (module == NULL) {
        error("Cannot GetModuleHandle()");
    }

    if (GetModuleFileNameW(module,buffer,MAX_PATH) == 0) {
        error("Could not GetModuleFileName");
    }

    ini_filename = replace_filename(buffer,INI_FILENAME);

    if ((inif = load_init_file(ini_filename)) == NULL) {
	erlexec_dir = find_erlexec_dir(ini_filename);
	SetEnvironmentVariableW(L"ERLEXEC_DIR", erlexec_dir);
    } else {

      if ((inis = lookup_init_section(inif,INI_SECTION)) == NULL) {
	error("Could not find section %s in init file %S",
	      INI_SECTION, ini_filename);
      }
      
      utf8dir = do_lookup_in_section(inis, "Bindir", INI_SECTION, ini_filename);
      len = MultiByteToWideChar(CP_UTF8, 0, utf8dir, -1, NULL, 0);
      erlexec_dir = malloc(len*sizeof(wchar_t));
      MultiByteToWideChar(CP_UTF8, 0, utf8dir, -1, erlexec_dir, len);
      if(len == 0) {
	  error("Bindir is not a valid utf8 '%s' in init file %S",
		utf8dir, ini_filename);
      }
      free_init_file(inif);
    }
    
    erlexec_name = malloc((wcslen(erlexec_dir) + wcslen(ERLEXEC_BASENAME) + 2)*sizeof(wchar_t));
    wcscpy(erlexec_name,erlexec_dir);
    wcscat(erlexec_name, L"\\" ERLEXEC_BASENAME);
    
    free(ini_filename);
}

static __declspec(noreturn) void
error(char* format, ...)
{
    char sbuf[2048];
    va_list ap;

    va_start(ap, format);
    vsprintf(sbuf, format, ap);
    va_end(ap);

    fprintf(stderr, "%s\n", sbuf);
    exit(1);
}
