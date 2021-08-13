#include <assert.h>
#include "linux/rbtree_augmented.h"
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/stat.h>

#define container_of(ptr, type, member) ({                      \
             const typeof( ((type *)0)->member ) *__mptr = (ptr);    \
             (type *)( (char *)__mptr - offsetof(type,member) );})

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
	off_t size; char c, x;

	fstat(STDIN_FILENO, &stat);
	size = stat.st_size;
	assert(size % 2 == 0);

	for (int i = 0; i < size / 2; i++) {
		c = getchar();
		x = getchar();

		if (c) {
			data = calloc(sizeof(struct data), 1);
			data->key = x;
			rb_find_add(&data->node, &rbt, node_cmp);
		} else {
			node = rb_find(&x, &rbt, key_cmp);
			if (!node)
				continue;
			rb_erase(node, &rbt);
			free(data_from_node(node));
		}
	}

#ifdef COVERAGE
	struct data *_n, *pos;
	rbtree_postorder_for_each_entry_safe(pos, _n, &rbt, node)
		free(pos);
	rbt = RB_ROOT;
#endif

}
