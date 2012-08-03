/* Copyright (c) 2012, Allan Wirth <allan@allanwirth.com>
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 *  * Redistributions of source code must retain the above copyright notice,
 *    this list of conditions and the following disclaimer.
 *
 *  * Redistributions in binary form must reproduce the above copyright notice,
 *    this list of conditions and the following disclaimer in the documentation
 *    and/or other materials provided with the distribution.
 *
 *  * Neither the name of this software, nor the names of its
 *    contributors may be used to endorse or promote products derived from
 *    this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 * OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#include <cstdlib>
#include <cstdio>
#include <list>
#include <iterator>

using namespace std;

typedef int data_type;

static list<data_type>* stack = NULL;

const static char* zls = "Error: %s with zero length stack.\n";

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
    fprintf(stderr, zls, "peek");
    exit(1);
    return 0;
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
    fprintf(stderr, zls, "pop");
    exit(1);
    return 0;
  }
  data_type e = stack->back();
  stack->pop_back();
  return e;
}

extern "C" void roll(int depth, int times) {
  if (stack == NULL) {
    fprintf(stderr, zls, "roll");
    exit(1);
    return;
  } else if (depth > stack->size()) {
    fprintf(stderr,
        "Error: Roll with depth (%d) larger than stack size (%d).",
        depth, stack->size());
    exit(1);
    return;
  } else if (depth <= 0) {
    fprintf(stderr, "Error: Roll with zero or negative depth (%d).", depth);
    exit(1);
    return;
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
  advance(it1, -(depth-times));
  stack->splice(it1, *stack, it2, stack->end());
}
