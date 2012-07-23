#include <cstdlib>
#include <cstdio>
#include <list>
#include <iterator>

using namespace std;

typedef int data_type;

static list<data_type>* stack = NULL;

extern "C" void print_stack(void) {
  if (stack != NULL) {
    list<data_type>::iterator i=stack->begin(), end=stack->end(), aend=end;
    --aend;
    for (; i != end; ++i) {
      fprintf(stderr, "%d", *i);
      if (i!=aend) fprintf(stderr, " ");
    }
  }
  fprintf(stderr, "\n");
}

extern "C" void debug(char* instname) {
  fprintf(stderr, "Executing %s\nStack: ", instname);
  print_stack();
  fprintf(stderr, "--------------------\n");
}

extern "C" int peek(void) {
  if (stack == NULL || stack->size() == 0) {
    fprintf(stderr, "Error: Peek with zero length stack.");
    exit(1);
  }
  return stack->back();
}

extern "C" void push(int e) {
  if (stack == NULL)
    stack = new list<data_type>;
  stack->push_back(e);
}

extern "C" int pop(void) {
  if (stack == NULL || stack->size() == 0) {
    fprintf(stderr, "Error: Pop with zero length stack.");
    exit(1);
  }
  data_type e = stack->back();
  stack->pop_back();
  return e;
}

extern "C" void roll(int depth, int times) {
  if (stack == NULL) {
    fprintf(stderr, "Error: Roll with zero length stack.");
    exit(1);
  } else if (depth > stack->size()) {
    fprintf(stderr,
        "Error: Roll with depth (%d) larger than stack size (%d).",
        depth, stack->size());
    exit(1);
  } else if (depth <= 0) {
    fprintf(stderr, "Error: Roll with zero or negative depth (%d).", depth);
    exit(1);
  }
  while (times < 0) {
    times += depth;
  }
  while (times >= depth) {
    times -= depth;
  }
  list<data_type>::iterator it2 = stack->end();
  advance(it2, -times);
  list<data_type>::iterator it1 = it2;
  advance(it2, -(depth-times));
  stack->splice(it1, *stack, it2, stack->end());
}
