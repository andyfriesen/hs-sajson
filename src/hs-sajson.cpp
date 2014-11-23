
#include "sajson.h"

using namespace sajson;

extern "C" {

struct str {
    const char* data;
    size_t length;
};

parser* sj_parser(size_t length, const char* data) {
    mutable_string_view ms(string(data, length));
    size_t* structure = new size_t[length];
    return new parser(ms, structure);
}

void sj_parser_free(parser* parser) {
    delete parser;
}

document* sj_parser_get_document(parser* parser) {
    return new document(parser->get_document());
}

void sj_document_free(document* document) {
    delete document;
}

bool sj_document_is_valid(document* document) {
    return document->is_valid();
}

value* sj_document_get_root(document* document) {
    return new value(document->get_root());
}

size_t sj_document_get_error_line(document* doc) {
    return doc->get_error_line();
}

size_t sj_document_get_error_column(document* doc) {
    return doc->get_error_column();
}

const char* sj_document_get_error_message(document* doc) {
    return doc->get_error_message().c_str();
}

void sj_value_free(value* v) {
    delete v;
}

type sj_value_get_type(value* v) {
    return v->get_type();
}

size_t sj_value_get_length(value* v) {
    return v->get_length();
}

value* sj_value_get_array_element(value* v, size_t index) {
    return new value(v->get_array_element(index));
}

void sj_value_get_object_key(value* v, size_t index, str* result) {
    printf("get object key\n");
    string s = v->get_object_key(index);
    result->data = s.data();
    result->length = s.length();
}

value* sj_value_get_object_value(value* v, size_t index) {
    return new value(v->get_object_value(index));
}

size_t sj_value_find_object_key(value* v, string key) {
    return v->find_object_key(key);
}

int sj_value_get_integer_value(value* v) {
    return v->get_integer_value();
}

double sj_value_get_double_value(value* v) {
    return v->get_double_value();
}

double sj_value_get_number_value(value* v) {
    return v->get_number_value();
}

size_t sj_value_get_string_length(value* v) {
    return v->get_string_length();
}

void sj_value_get_string_value(value* v, str* result) {
    string s = v->as_str();
    result->data = s.data();
    result->length = s.length();
}

}