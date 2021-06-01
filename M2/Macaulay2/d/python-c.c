#include <Python.h>
#include "python-exports.h"

int python_RunSimpleString(M2_string s) {
  char *t = M2_tocharstar(s);
  int ret = PyRun_SimpleString(t);
  GC_FREE(t);
  return ret;
}

PyObject *globals, *locals;

static void init() {
  if (!globals) {
#if 0    
    globals = PyEval_GetGlobals(); /* this returns null because no frame is currently executing */
#elif 1
    globals = PyDict_New();
    PyDict_SetItemString(globals, "__builtins__", PyEval_GetBuiltins());
#else
    globals = PyDict_New();
    PyRun_String("import __builtin__ as __builtins__",Py_eval_input, globals, locals);
#endif
  }
}

/* simplified to return 1 or 0 rather than a PyObject or NULL */
int python_ErrOccurred(void) {
	return (PyErr_Occurred() != NULL);
}

void python_ErrPrint(void) {
	PyErr_Print();
}

PyObject *python_RunString(M2_string s) {
  char *t = M2_tocharstar(s);
  init();
  PyObject *ret = PyRun_String(t,Py_eval_input,globals,locals);
  GC_FREE(t);
  return ret;
}

int python_Main() {
  static wchar_t pn[3] = L"M2";
  static wchar_t *argv[2] = {pn,NULL};
  static int argc = 1;
  return Py_Main(argc,argv);
}

PyObject *python_SysGetObject(M2_string s) {
  char *t = M2_tocharstar(s);
  PyObject *ret = PySys_GetObject(t);
  GC_FREE(t);
  return ret;
}

/***********
 * objects *
 ***********/

PyObject *python_ObjectType(PyObject *o) {
  return PyObject_Type(o);
}

int python_ObjectRichCompareBool(PyObject *o1, PyObject *o2, int opid) {
	return PyObject_RichCompareBool(o1, o2, opid);
}

PyObject *python_ObjectGetAttrString(PyObject *o, char *attr) {
	return PyObject_GetAttrString(o, attr);
}

int python_ObjectSetAttrString(PyObject *o, char *attr_name, PyObject *v) {
	return PyObject_SetAttrString(o, attr_name, v);
}

/* see http://docs.python.org/extending/extending.html for this example */

static PyObject * spam_system(PyObject *self, PyObject *args) {
  const char *command;
  int sts;
  if (!PyArg_ParseTuple(args, "s", &command)) return NULL;
  sts = system(command);
  return Py_BuildValue("i", sts);
}
static PyObject *SpamError;
static PyMethodDef SpamMethods[] = {
  {"system",  spam_system, METH_VARARGS, "Execute a shell command."},
  {NULL, NULL, 0, NULL}
};
static struct PyModuleDef moduledef = {
  PyModuleDef_HEAD_INIT, "spam", NULL, -1, SpamMethods, NULL, NULL, NULL, NULL}
;
void python_initspam() {
  static char name[] = "spam.error";
  PyObject *m;
  m = PyModule_Create(&moduledef);
  if (m == NULL) return;
  SpamError = PyErr_NewException(name, NULL, NULL);
  Py_INCREF(SpamError);
  PyModule_AddObject(m, "error", SpamError);
}

/***********
 * numbers *
 ***********/

PyObject *python_NumberAdd(PyObject *o1, PyObject *o2) {
	return PyNumber_Add(o1, o2);
}

PyObject *python_NumberSubtract(PyObject *o1, PyObject *o2) {
	return PyNumber_Subtract(o1, o2);
}

PyObject *python_NumberMultiply(PyObject *o1, PyObject *o2) {
	return PyNumber_Multiply(o1, o2);
}

PyObject *python_NumberTrueDivide(PyObject *o1, PyObject *o2) {
	return PyNumber_TrueDivide(o1, o2);
}

/********
 * ints *
 ********/

int python_LongCheck(PyObject *p) {
	return PyLong_Check(p);
}

long python_LongAsLong(PyObject *o) {
	return PyLong_AsLong(o);
}

PyObject *python_LongFromLong(long v) {
	return PyLong_FromLong(v);
}

/**********
 * floats *
 **********/

int python_FloatCheck(PyObject *p) {
	return PyFloat_Check(p);
}

double python_FloatAsDouble(PyObject *o) {
	return PyFloat_AsDouble(o);
}

PyObject *python_FloatFromDouble(double v) {
	return PyFloat_FromDouble(v);
}

/***********
 * strings *
 ***********/

int python_UnicodeCheck(PyObject *o) {
	return PyUnicode_Check(o);
}

const char *python_UnicodeAsUTF8(PyObject *o) {
	return PyUnicode_AsUTF8(o);
}

PyObject *python_UnicodeFromString(char *u) {
	return PyUnicode_FromString(u);
}

PyObject *python_UnicodeConcat(PyObject *o1, PyObject *o2) {
	return PyUnicode_Concat(o1, o2);
}

/**********
 * tuples *
 **********/

int python_TupleCheck(PyObject *o) {
	return PyTuple_Check(o);
}

int python_TupleSize(PyObject *o) {
	return PyTuple_Size(o);
}

PyObject *python_TupleGetItem(PyObject *o, int i) {
	return PyTuple_GetItem(o, i);
}

PyObject *python_TupleNew(int n) {
	return PyTuple_New(n);
}

int python_TupleSetItem(PyObject *L, int i, PyObject *item) {
	return PyTuple_SetItem(L, i, item);
}

/*********
 * lists *
 *********/

int python_ListCheck(PyObject *o) {
	return PyList_Check(o);
}

int python_ListSize(PyObject *o) {
	return PyList_Size(o);
}

PyObject *python_ListGetItem(PyObject *o, int i) {
	return PyList_GetItem(o, i);
}

PyObject *python_ListNew(int n) {
	return PyList_New(n);
}

int python_ListSetItem(PyObject *L, int i, PyObject *item) {
	return PyList_SetItem(L, i, item);
}

/****************
 * dictionaries *
 ****************/

int python_DictCheck(PyObject *o) {
	return PyDict_Check(o);
}

PyObject *python_DictKeys(PyObject *o) {
	return PyDict_Keys(o);
}

PyObject *python_DictGetItem(PyObject *p, PyObject *key) {
	return PyDict_GetItemWithError(p, key);
}

PyObject *python_DictNew(void) {
	return PyDict_New();
}

int python_DictSetItem(PyObject *p, PyObject *key, PyObject *val) {
	return PyDict_SetItem(p, key, val);
}

/*************
 * callables *
 *************/

int python_CallableCheck(PyObject *o) {
	return PyCallable_Check(o);
}


PyObject *python_ObjectCall(PyObject *o, PyObject *args, PyObject *kwargs) {
	return PyObject_Call(o, args, kwargs);
}

/*************
 * iterators *
 *************/

int python_IterCheck(PyObject *o) {
	return PyIter_Check(o);
}

PyObject * python_IterNext(PyObject *o) {
	return PyIter_Next(o);
}

/********
 * none *
 ********/

PyObject *python_None = Py_None;

/*************
 * importing *
 *************/

PyObject *python_ImportImportModule(char *name) {
	return PyImport_ImportModule(name);
}


#if 0
Local Variables:
compile-command: "echo \"make: Entering directory \\`$M2BUILDDIR/Macaulay2/d'\" && make -C $M2BUILDDIR/Macaulay2/d python-c.o "
End:
#endif
