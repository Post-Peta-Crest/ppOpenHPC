/*
	fstr_res_util Ver.1.4
	2011.05.11 by K.Suemitsu (AdvanceSoft)
	2004.20.26 by N.Imai (RIST)
	--------------------------------------------------------
	ʬ���Ƿ׻����줿��̤��ɹ��߽������뤿��Υ桼�ƥ���ƥ�
*/

#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#include "hecmw_util.h"
#include "hecmw_io_mesh.h"
#include "hecmw_io_struct.h"
#include "hecmw_struct.h"
#include "hecmw_config.h"
#include "hecmw_dist.h"
#include "hecmw_dist_free.h"
#include "hecmw_common.h"

#include "hecmw_control.h"
#include "hecmw_result.h"
#include "hecmw_io_dist.h"
#include "hecmw_io_get_mesh.h"


/* ���Ϸ�̥ե�������� */
typedef struct {
	int nnode_gid;
	int nelem_gid;
	int* node_gid;
	int* elem_gid;
	struct hecmwST_result_data* result;
} fstr_res_info;

/* �����Х롦������ID �б� */
typedef struct {
	int global;
	int local;
	int area;
} fstr_gl_rec;

/* �����Х롦�������б�ɽ */
typedef struct {
	fstr_gl_rec* nrec;
	fstr_gl_rec* erec;
	int node_n;
	int elem_n;
} fstr_glt;


/* ��ʬ����å�����ɹ��� */
struct hecmwST_local_mesh** fstr_get_all_local_mesh( char* name_ID, int* area_n, int* refine );

/* ��å���κ�� */
void fstr_free_mesh( struct hecmwST_local_mesh** mesh, int area_n );

/* ���ƥå׿���Ĵ�٤�ʥե������¸�ߤ�Ĵ�٤�� */
int fstr_get_step_n( char* name_ID );

/* ���ƥåפ����ΰ�ǡ������ɤ߹��� */
fstr_res_info** fstr_get_all_result( char* name_ID, int step, int area_n, int refine );

/* ���ƥåפ����ΰ�ǡ����η�� */
struct hecmwST_result_data* fstr_all_result( fstr_glt* glt, fstr_res_info** res, int refine );

/* fstr_res_info �κ�� */
void fstr_free_result( fstr_res_info** res, int area_n );

/* �ơ��֥� fstr_glt �κ��� */
fstr_glt* fstr_create_glt( struct hecmwST_local_mesh** mesh, int area_n );

/* fstr_glt �κ�� */
void fstr_free_glt( fstr_glt* glt );

/* ñ���ΰ��å���κ��� */
struct hecmwST_local_mesh* fstr_create_glmesh( fstr_glt* glt );

/* ñ���ΰ��å���κ�� */
void fstr_free_glmesh( struct hecmwST_local_mesh* mp );

