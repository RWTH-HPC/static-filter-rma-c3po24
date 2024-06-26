int MPI_Add_error_class(int* errorclass {SINGLE_OUT});
int MPI_Add_error_code(int errorclass, int* errorcode {SINGLE_OUT});
int MPI_Add_error_string(int errorcode, const char* string {SINGLE_IN});
int MPI_Alloc_mem(MPI_Aint size, MPI_Info info, void* baseptr {SINGLE_OUT});
int MPI_Close_port(const char* port_name {SINGLE_IN});
int MPI_Comm_accept(const char* port_name {SINGLE_IN}, MPI_Info info, int root, MPI_Comm comm, MPI_Comm* newcomm {SINGLE_OUT});
int MPI_Comm_call_errhandler(MPI_Comm comm, int errorcode);
int MPI_Comm_connect(const char* port_name {SINGLE_IN}, MPI_Info info, int root, MPI_Comm comm, MPI_Comm* newcomm {SINGLE_OUT});
int MPI_Comm_create_group(MPI_Comm comm, MPI_Group group, int tag, MPI_Comm* newcomm {SINGLE_OUT});
int MPI_Comm_delete_attr(MPI_Comm comm, int comm_keyval);
int MPI_Comm_disconnect(MPI_Comm* comm {SINGLE_IO});
int MPI_Comm_dup_with_info(MPI_Comm comm, MPI_Info info, MPI_Comm* newcomm {SINGLE_OUT});
int MPI_Comm_get_attr(MPI_Comm comm, int comm_keyval, void* attribute_val {SINGLE_OUT}, int* flag {SINGLE_OUT});
int MPI_Comm_get_info(MPI_Comm comm, MPI_Info* info_used {SINGLE_OUT});
int MPI_Comm_get_name(MPI_Comm comm, char* comm_name {ARRAY_OUT|OP:deref:resultlen}, int* resultlen {SINGLE_OUT});
int MPI_Comm_get_parent(MPI_Comm* parent {SINGLE_OUT});
int MPI_Comm_idup(MPI_Comm comm, MPI_Comm* newcomm {SINGLE_OUT}, MPI_Request* request {SINGLE_OUT});
int MPI_Comm_join(int fd, MPI_Comm* intercomm {SINGLE_OUT});
int MPI_Comm_set_attr(MPI_Comm comm, int comm_keyval, void* attribute_val {SINGLE_IN});
int MPI_Comm_set_info(MPI_Comm comm, MPI_Info info);
int MPI_Comm_set_name(MPI_Comm comm, const char* comm_name {SINGLE_IN});
int MPI_Dist_graph_neighbors(MPI_Comm comm, int maxindegree, int* sources {ARRAY_OUT|ARG:maxindegree}, int* sourceweights {ARRAY_OUT|ARG:maxindegree}, int maxoutdegree, int* destinations {ARRAY_OUT|ARG:maxoutdegree}, int* destweights {ARRAY_OUT|ARG:maxoutdegree});
int MPI_Dist_graph_neighbors_count(MPI_Comm comm, int* indegree {SINGLE_OUT}, int* outdegree {SINGLE_OUT}, int* weighted {SINGLE_OUT});
int MPI_File_call_errhandler(MPI_File fh, int errorcode);
int MPI_File_close(MPI_File* fh {SINGLE_IO});
int MPI_File_create_errhandler(MPI_File_errhandler_function* file_errhandler_fn {SINGLE_IN}, MPI_Errhandler* errhandler {SINGLE_OUT});
int MPI_File_delete(const char* filename {SINGLE_IN}, MPI_Info info);
int MPI_File_get_amode(MPI_File fh, int* amode {SINGLE_OUT});
int MPI_File_get_atomicity(MPI_File fh, int* flag {SINGLE_OUT});
int MPI_File_get_byte_offset(MPI_File fh, MPI_Offset offset, MPI_Offset* disp {SINGLE_OUT});
int MPI_File_get_errhandler(MPI_File file, MPI_Errhandler* errhandler {SINGLE_OUT});
int MPI_File_get_group(MPI_File fh, MPI_Group* group {SINGLE_OUT});
int MPI_File_get_info(MPI_File fh, MPI_Info* info_used {SINGLE_OUT});
int MPI_File_get_position(MPI_File fh, MPI_Offset* offset {SINGLE_OUT});
int MPI_File_get_position_shared(MPI_File fh, MPI_Offset* offset {SINGLE_OUT});
int MPI_File_get_size(MPI_File fh, MPI_Offset* size {SINGLE_OUT});
int MPI_File_get_type_extent(MPI_File fh, MPI_Datatype datatype, MPI_Aint* extent {SINGLE_OUT});
int MPI_File_get_view(MPI_File fh, MPI_Offset* disp {SINGLE_OUT}, MPI_Datatype* etype {SINGLE_OUT}, MPI_Datatype* filetype {SINGLE_OUT}, char* datarep {SINGLE_OUT});
int MPI_File_iread(MPI_File fh, void* buf {SINGLE_IN}, int count, MPI_Datatype datatype, MPI_Request* request {SINGLE_OUT});
int MPI_File_iread_all(MPI_File fh, void* buf {SINGLE_IN}, int count, MPI_Datatype datatype, MPI_Request* request {SINGLE_OUT});
int MPI_File_iread_at(MPI_File fh, MPI_Offset offset, void* buf {SINGLE_IN}, int count, MPI_Datatype datatype, MPI_Request* request {SINGLE_OUT});
int MPI_File_iread_at_all(MPI_File fh, MPI_Offset offset, void* buf {SINGLE_IN}, int count, MPI_Datatype datatype, MPI_Request* request {SINGLE_OUT});
int MPI_File_iread_shared(MPI_File fh, void* buf {SINGLE_IN}, int count, MPI_Datatype datatype, MPI_Request* request {SINGLE_OUT});
int MPI_File_iwrite(MPI_File fh, const void* buf {SINGLE_IN}, int count, MPI_Datatype datatype, MPI_Request* request {SINGLE_OUT});
int MPI_File_iwrite_all(MPI_File fh, const void* buf {SINGLE_IN}, int count, MPI_Datatype datatype, MPI_Request* request {SINGLE_OUT});
int MPI_File_iwrite_at(MPI_File fh, MPI_Offset offset, const void* buf {SINGLE_IN}, int count, MPI_Datatype datatype, MPI_Request* request {SINGLE_OUT});
int MPI_File_iwrite_at_all(MPI_File fh, MPI_Offset offset, const void* buf {SINGLE_IN}, int count, MPI_Datatype datatype, MPI_Request* request {SINGLE_OUT});
int MPI_File_iwrite_shared(MPI_File fh, const void* buf {SINGLE_IN}, int count, MPI_Datatype datatype, MPI_Request* request {SINGLE_OUT});
int MPI_File_open(MPI_Comm comm, const char* filename {SINGLE_IN}, int amode, MPI_Info info, MPI_File* fh {SINGLE_OUT});
int MPI_File_preallocate(MPI_File fh, MPI_Offset size);
int MPI_File_read(MPI_File fh, void* buf {SINGLE_IN}, int count, MPI_Datatype datatype, MPI_Status* status {SINGLE_OUT});
int MPI_File_read_all(MPI_File fh, void* buf {SINGLE_IN}, int count, MPI_Datatype datatype, MPI_Status* status {SINGLE_OUT});
int MPI_File_read_all_begin(MPI_File fh, void* buf {SINGLE_IN}, int count, MPI_Datatype datatype);
int MPI_File_read_all_end(MPI_File fh, void* buf {SINGLE_IN}, MPI_Status* status {SINGLE_OUT});
int MPI_File_read_at(MPI_File fh, MPI_Offset offset, void* buf {SINGLE_IN}, int count, MPI_Datatype datatype, MPI_Status* status {SINGLE_OUT});
int MPI_File_read_at_all(MPI_File fh, MPI_Offset offset, void* buf {SINGLE_IN}, int count, MPI_Datatype datatype, MPI_Status* status {SINGLE_OUT});
int MPI_File_read_at_all_begin(MPI_File fh, MPI_Offset offset, void* buf {SINGLE_IN}, int count, MPI_Datatype datatype);
int MPI_File_read_at_all_end(MPI_File fh, void* buf {SINGLE_IN}, MPI_Status* status {SINGLE_OUT});
int MPI_File_read_ordered(MPI_File fh, void* buf {SINGLE_IN}, int count, MPI_Datatype datatype, MPI_Status* status {SINGLE_OUT});
int MPI_File_read_ordered_begin(MPI_File fh, void* buf {SINGLE_IN}, int count, MPI_Datatype datatype);
int MPI_File_read_ordered_end(MPI_File fh, void* buf {SINGLE_IN}, MPI_Status* status {SINGLE_OUT});
int MPI_File_read_shared(MPI_File fh, void* buf {SINGLE_IN}, int count, MPI_Datatype datatype, MPI_Status* status {SINGLE_OUT});
int MPI_File_seek(MPI_File fh, MPI_Offset offset, int whence);
int MPI_File_seek_shared(MPI_File fh, MPI_Offset offset, int whence);
int MPI_File_set_atomicity(MPI_File fh, int flag);
int MPI_File_set_errhandler(MPI_File file, MPI_Errhandler errhandler);
int MPI_File_set_info(MPI_File fh, MPI_Info info);
int MPI_File_set_size(MPI_File fh, MPI_Offset size);
int MPI_File_set_view(MPI_File fh, MPI_Offset disp, MPI_Datatype etype, MPI_Datatype filetype, const char* datarep {SINGLE_IN}, MPI_Info info);
int MPI_File_sync(MPI_File fh);
int MPI_File_write(MPI_File fh, const void* buf {SINGLE_IN}, int count, MPI_Datatype datatype, MPI_Status* status {SINGLE_OUT});
int MPI_File_write_all(MPI_File fh, const void* buf {SINGLE_IN}, int count, MPI_Datatype datatype, MPI_Status* status {SINGLE_OUT});
int MPI_File_write_all_begin(MPI_File fh, const void* buf {SINGLE_IN}, int count, MPI_Datatype datatype);
int MPI_File_write_all_end(MPI_File fh, const void* buf {SINGLE_IN}, MPI_Status* status {SINGLE_OUT});
int MPI_File_write_at(MPI_File fh, MPI_Offset offset, const void* buf {SINGLE_IN}, int count, MPI_Datatype datatype, MPI_Status* status {SINGLE_OUT});
int MPI_File_write_at_all(MPI_File fh, MPI_Offset offset, const void* buf {SINGLE_IN}, int count, MPI_Datatype datatype, MPI_Status* status {SINGLE_OUT});
int MPI_File_write_at_all_begin(MPI_File fh, MPI_Offset offset, const void* buf {SINGLE_IN}, int count, MPI_Datatype datatype);
int MPI_File_write_at_all_end(MPI_File fh, const void* buf {SINGLE_IN}, MPI_Status* status {SINGLE_OUT});
int MPI_File_write_ordered(MPI_File fh, const void* buf {SINGLE_IN}, int count, MPI_Datatype datatype, MPI_Status* status {SINGLE_OUT});
int MPI_File_write_ordered_begin(MPI_File fh, const void* buf {SINGLE_IN}, int count, MPI_Datatype datatype);
int MPI_File_write_ordered_end(MPI_File fh, const void* buf {SINGLE_IN}, MPI_Status* status {SINGLE_OUT});
int MPI_File_write_shared(MPI_File fh, const void* buf {SINGLE_IN}, int count, MPI_Datatype datatype, MPI_Status* status {SINGLE_OUT});
int MPI_Free_mem(void* base {SINGLE_IN});
int MPI_Get_address(const void* location {SINGLE_IN}, MPI_Aint* address {SINGLE_OUT});
int MPI_Get_elements_x(const MPI_Status* status {SINGLE_IN}, MPI_Datatype datatype, MPI_Count* count {SINGLE_OUT});
int MPI_Get_library_version(char* version {ARRAY_OUT|OP:deref:resultlen}, int* resultlen {SINGLE_OUT});
int MPI_Grequest_complete(MPI_Request request);
int MPI_Grequest_start(MPI_Grequest_query_function* query_fn {SINGLE_IN}, MPI_Grequest_free_function* free_fn {SINGLE_IN}, MPI_Grequest_cancel_function* cancel_fn {SINGLE_IN}, void* extra_state {SINGLE_IN}, MPI_Request* request {SINGLE_OUT});
int MPI_Improbe(int source, int tag, MPI_Comm comm, int* flag {SINGLE_OUT}, MPI_Message* message {SINGLE_OUT}, MPI_Status* status {SINGLE_OUT});
int MPI_Imrecv(void* buf {SINGLE_IN}, int count, MPI_Datatype datatype, MPI_Message* message {SINGLE_IO}, MPI_Request* request {SINGLE_OUT});
int MPI_Info_create(MPI_Info* info {SINGLE_OUT});
int MPI_Info_delete(MPI_Info info, const char* key {SINGLE_IN});
int MPI_Info_dup(MPI_Info info, MPI_Info* newinfo {SINGLE_OUT});
int MPI_Info_free(MPI_Info* info {SINGLE_IO});
int MPI_Info_get(MPI_Info info, const char* key {SINGLE_IN}, int valuelen, char* value {ARRAY_OUT|ARG:valuelen}, int* flag {SINGLE_OUT});
int MPI_Info_get_nkeys(MPI_Info info, int* nkeys {SINGLE_OUT});
int MPI_Info_get_nthkey(MPI_Info info, int n, char* key {SINGLE_OUT});
int MPI_Info_get_valuelen(MPI_Info info, const char* key {SINGLE_IN}, int* valuelen {SINGLE_OUT}, int* flag {SINGLE_OUT});
int MPI_Info_set(MPI_Info info, const char* key {SINGLE_IN}, const char* value {SINGLE_IN});
int MPI_Is_thread_main(int* flag {SINGLE_OUT});
int MPI_Lookup_name(const char* service_name {SINGLE_IN}, MPI_Info info, char* port_name {SINGLE_OUT});
int MPI_Mprobe(int source, int tag, MPI_Comm comm, MPI_Message* message {SINGLE_OUT}, MPI_Status* status {SINGLE_OUT});
int MPI_Mrecv(void* buf {SINGLE_IN}, int count, MPI_Datatype datatype, MPI_Message* message {SINGLE_IO}, MPI_Status* status {SINGLE_OUT});
int MPI_Op_commutative(MPI_Op op, int* commute {SINGLE_OUT});
int MPI_Open_port(MPI_Info info, char* port_name {SINGLE_OUT});
int MPI_Pack_external(const char datarep, const void* inbuf {SINGLE_IN}, int incount, MPI_Datatype datatype, void* outbuf {SINGLE_IN}, MPI_Aint outsize, MPI_Aint* position {SINGLE_IO});
int MPI_Pack_external_size(const char datarep, int incount, MPI_Datatype datatype, MPI_Aint* size {SINGLE_OUT});
int MPI_Publish_name(const char* service_name {SINGLE_IN}, MPI_Info info, const char* port_name {SINGLE_IN});
int MPI_Query_thread(int* provided {SINGLE_OUT});
int MPI_Register_datarep(const char* datarep {SINGLE_IN}, MPI_Datarep_conversion_function* read_conversion_fn {SINGLE_IN}, MPI_Datarep_conversion_function* write_conversion_fn {SINGLE_IN}, MPI_Datarep_extent_function* dtype_file_extent_fn {SINGLE_IN}, void* extra_state {SINGLE_IN});
int MPI_Request_get_status(MPI_Request request, int* flag {SINGLE_OUT}, MPI_Status* status {SINGLE_OUT});
int MPI_Status_set_cancelled(MPI_Status* status {SINGLE_IO}, int flag);
int MPI_Status_set_elements(MPI_Status* status {SINGLE_IO}, MPI_Datatype datatype, int count);
int MPI_Status_set_elements_x(MPI_Status* status {SINGLE_IO}, MPI_Datatype datatype, MPI_Count count);
int MPI_Type_create_f90_complex(int p, int r, MPI_Datatype* newtype {SINGLE_OUT});
int MPI_Type_create_f90_integer(int r, MPI_Datatype* newtype {SINGLE_OUT});
int MPI_Type_create_f90_real(int p, int r, MPI_Datatype* newtype {SINGLE_OUT});
int MPI_Type_create_hindexed_block(int count, int blocklength, const MPI_Aint* array_of_displacements {ARRAY_IN|ARG:count}, MPI_Datatype oldtype, MPI_Datatype* newtype {SINGLE_OUT});
int MPI_Type_create_keyval(MPI_Type_copy_attr_function* type_copy_attr_fn {SINGLE_IN}, MPI_Type_delete_attr_function* type_delete_attr_fn {SINGLE_IN}, int* type_keyval {SINGLE_OUT}, void* extra_state {SINGLE_IN});
int MPI_Type_delete_attr(MPI_Datatype datatype, int type_keyval);
int MPI_Type_free_keyval(int* type_keyval {SINGLE_IO});
int MPI_Type_get_attr(MPI_Datatype datatype, int type_keyval, void* attribute_val {SINGLE_OUT}, int* flag {SINGLE_OUT});
int MPI_Type_get_extent_x(MPI_Datatype datatype, MPI_Count* lb {SINGLE_OUT}, MPI_Count* extent {SINGLE_OUT});
int MPI_Type_get_name(MPI_Datatype datatype, char* type_name {ARRAY_OUT|OP:deref:resultlen}, int* resultlen {SINGLE_OUT});
int MPI_Type_get_true_extent_x(MPI_Datatype datatype, MPI_Count* true_lb {SINGLE_OUT}, MPI_Count* true_extent {SINGLE_OUT});
int MPI_Type_match_size(int typeclass, int size, MPI_Datatype* datatype {SINGLE_OUT});
int MPI_Type_set_attr(MPI_Datatype datatype, int type_keyval, void* attribute_val {SINGLE_IN});
int MPI_Type_set_name(MPI_Datatype datatype, const char* type_name {SINGLE_IN});
int MPI_Type_size_x(MPI_Datatype datatype, MPI_Count* size {SINGLE_OUT});
int MPI_Unpack_external(const char* datarep {SINGLE_IN}, const void* inbuf {SINGLE_IN}, MPI_Aint insize, MPI_Aint* position {SINGLE_IO}, void* outbuf {SINGLE_IN}, int outcount, MPI_Datatype datatype);
int MPI_Unpublish_name(const char* service_name {SINGLE_IN}, MPI_Info info, const char* port_name {SINGLE_IN});
