#include <leveldb/db.h>
#include <map>
#include "M2-exports.h"

#include <iostream>

#define ERROR (-1)

static std::map<int, leveldb::DB*> leveldb_files;
static M2_string status_msg;

extern "C" void close_all_dbms(void) {
	for (auto const& file : leveldb_files)
		delete file.second;
	leveldb_files.clear();
}

extern "C" int system_dbmopen(M2_string filename, M2_bool mutable_) {
	int handle;
	char *FileName;
	leveldb::DB* db;
	leveldb::Options options;
	leveldb::Status status;

	options.create_if_missing = true;
	FileName = M2_tocharstar(filename);
	status = leveldb::DB::Open(options, FileName, &db);
	status_msg = M2_tostring(status.ToString().c_str());
	freemem(FileName);

	if (!status.ok())
		return ERROR;

	for (handle = 0; leveldb_files.count(handle); handle++) {}
	leveldb_files[handle] = db;

	return handle;
}

extern "C" int system_dbmclose(int handle) {
	delete leveldb_files[handle];
	leveldb_files.erase(handle);

	return 0;
}

static leveldb::Slice toslice(M2_string x) {
	leveldb::Slice y((char *)x->array, x->len);
	return y;
}

extern "C" int system_dbmstore(int handle, M2_string key, M2_string content) {
	leveldb::DB *db;
	leveldb::Status status;

	db = leveldb_files[handle];
	status = db->Put(leveldb::WriteOptions(),
			 toslice(key), toslice(content));
	status_msg = M2_tostring(status.ToString().c_str());

	return status.ok() ? 0 : ERROR;
}

extern "C" M2_string /* or NULL */ system_dbmfetch(int handle, M2_string key) {
	leveldb::DB *db;
	leveldb::Status status;
	std::string value;

	db = leveldb_files[handle];
	status = db->Get(leveldb::ReadOptions(), toslice(key), &value);
	status_msg = M2_tostring(status.ToString().c_str());

	return status.ok() ? M2_tostring(value.c_str()) : nullptr;
}

extern "C" int system_dbmdelete(int handle, M2_string key) {
	leveldb::DB *db;
	leveldb::Status status;

	db = leveldb_files[handle];
	status = db->Delete(leveldb::WriteOptions(), toslice(key));
	status_msg = M2_tostring(status.ToString().c_str());

	return status.ok() ? 0 : ERROR;
}

static std::string lastkey;
static bool hadlastkey = false;

extern "C" M2_string /* or NULL */ system_dbmfirst(int handle) {
	leveldb::DB *db;
	leveldb::Iterator *it;
	M2_string ret;

	db = leveldb_files[handle];
	it = db->NewIterator(leveldb::ReadOptions());
	it->SeekToFirst();

	if (it->Valid()) {
		lastkey = it->key().ToString();
		ret = M2_tostring(lastkey.c_str());
	} else
		ret = nullptr;

	status_msg = M2_tostring(it->status().ToString().c_str());
	delete it;
	hadlastkey = true;

	return ret;
}

extern "C" M2_string /* or NULL */ system_dbmnext(int handle) {
	if (hadlastkey) {
		leveldb::DB *db;
		leveldb::Iterator *it;
		M2_string ret;

		db = leveldb_files[handle];
		it = db->NewIterator(leveldb::ReadOptions());

		it->Seek(leveldb::Slice(lastkey));
		if (it->Valid())
			it->Next();

		if (it->Valid()) {
			lastkey = it->key().ToString();
			ret = M2_tostring(lastkey.c_str());
		} else
			ret = nullptr;

		status_msg = M2_tostring(it->status().ToString().c_str());
		delete it;
		return ret;
	} else {
		return system_dbmfirst(handle);
	}
}

extern "C" int system_dbmreorganize(int handle) {
	leveldb::DB *db;

	db = leveldb_files[handle];
	db->CompactRange(nullptr, nullptr);

	return 0;
}

extern "C" M2_string system_dbmstrerror(void) {
	return status_msg ? status_msg : M2_tostring("unknown error");
}
