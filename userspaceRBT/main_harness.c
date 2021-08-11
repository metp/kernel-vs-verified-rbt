#include "linux/rbtree_augmented.h"
#include <err.h>
#include <errno.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/stat.h>
#include <unistd.h>

#define container_of(ptr, type, member) ({                      \
             const typeof( ((type *)0)->member ) *__mptr = (ptr);    \
             (type *)( (char *)__mptr - offsetof(type,member) );})

enum Cmd { RESET, INSERT, DELETE };

struct data {
	struct rb_node node;
	char key;
};

#define data_from_node(from) (rb_entry(from, struct data, node))

static int key_cmp(const void *_a, const struct rb_node *_b)
{
	char a = *(typeof(a) *) _a;
	char b = data_from_node(_b)->key;
	if (a < b)
		return -1;
	if (a > b)
		return 1;
	else
		return 0;
}

static int node_cmp(struct rb_node *a, const struct rb_node *b)
{
	return key_cmp(&data_from_node(a)->key, b);
}

int main() {
	struct rb_root rbt = RB_ROOT; struct rb_node *node;
	struct data *data; struct stat stat;
	off_t size;
	char cmd;
	uint64_t key;

	struct data *_n, *pos;

	while (scanf("%hhu\n%lu\n", &cmd, &key) != EOF) {
		switch (cmd) {
		case RESET:
			rbtree_postorder_for_each_entry_safe(pos, _n, &rbt, node)
				free(pos);
			rbt = RB_ROOT;
			break;
		case INSERT:
			data = calloc(sizeof(struct data), 1);
			data->key = key;
			rb_find_add(&data->node, &rbt, node_cmp);
			break;
		case DELETE:
			node = rb_find(&key, &rbt, key_cmp);
			if (!node)
				continue;
			rb_erase(node, &rbt);
			free(data_from_node(node));
			break;
		default:
			return 1;
		}
	}
	if (errno != 0)
		err(EXIT_FAILURE, NULL);
}
