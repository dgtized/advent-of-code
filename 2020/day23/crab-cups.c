#include <stdio.h>
#include <stdlib.h>

/* usage: gcc crab-cups.c -o crab-cups && ./crab-cups */

struct node {
  int value;
  struct node *next;
};

/* creates a cyclic linked list
seeds value from sequence if available, otherwise fills in up to size.
*/
struct node* create(int size, int sequence[]) {
  struct node *t;
  struct node *head;

  head = (struct node*)malloc(sizeof(struct node));
  t = head;

  for(int i = 0; i < size; i++) {
    t->value = i < 9 ? sequence[i] : i + 1;

    /* last element in the list points back at the start */
    if(i < size - 1) {
      t->next = (struct node*)malloc(sizeof(struct node));
    } else {
      t->next = head;
    }
    t = t->next;
  }

  return head;
}

/* print the next N, non-null elements in the cyclic linked list */
void print_from(struct node *p, int count) {
  while(p && count > 0) {
    printf("%d ", p->value);
    p = p->next;
    count--;
  }
  printf("\n");
}

/* remove the next 3 elements from the loop */
struct node* take(struct node* current) {
  struct node* head = current->next;
  struct node* lookahead = current->next->next->next;

  current->next = lookahead->next;
  lookahead->next = NULL;

  return head;
}

/* linear scan for next element in the cyclic list with matching value */
struct node* find(struct node* current, int value) {
  int i = 0;
  struct node* p = current->next;
  while(p->value != value) {
    p = p->next;
    i++;
  }

  return p;
}

/* lookup destination node from index, skipping m1-m3 and looping back */
struct node* select_destination(int value, int ncups, struct node** index,
                                int m1, int m2, int m3) {
  int check = value;

  if(check < 1) check = ncups;
  while(check == m1 || check == m2 || check == m3) {
    check--;
    if(check < 1) check = ncups;
  }

  return index[check-1];
}

struct node* crab_cups(struct node* current, int ncups, struct node** index) {
  struct node* move = take(current);

  struct node* dest = select_destination(
    current->value - 1, ncups, index,
    move->value, move->next->value, move->next->next->value);

  /* printf("dest: %d\npick up: ", dest->value); */
  /* print_from(move, 10); */

  /* Put the 3 nodes in sequence back in after destination */
  struct node *after = dest->next;
  dest->next = move;
  move->next->next->next = after;

  return current;
}

struct node* run_sim(struct node* current, int ncups, int count) {
  /* build an index from node->value-1 to node for fast find */
  struct node** index = malloc(ncups * sizeof(struct node *));
  index[current->value-1] = current;
  struct node* p = current->next;
  while(p != current) {
    index[p->value-1] = p;
    p = p->next;
  }

  int iterations = 1;
  while(iterations <= count) {
    /* printf("\n-- move %d --\ncups: ", iterations); */
    /* print_from(current, 9); */
    /* printf("current: %d\n", current->value); */
    /* print_from(current, 8); */
    current = crab_cups(current, ncups, index)->next;
    iterations++;
  };

  return current;
}

void first_star(int sequence[]) {
  int ncups = 9;
  struct node* initial = create(ncups, sequence);
  struct node* current = initial;

  current = run_sim(initial, ncups, 100);

  struct node* one = find(current, 1);
  printf("First Star: ");
  print_from(one->next, 8);
}

void second_star(int sequence[]) {
  int ncups = 1000000;
  struct node* current = create(ncups,sequence);

  current = run_sim(current, ncups, 10000000);

  struct node* one = find(current, 1);
  printf("Second Star: ");
  print_from(one, 4);

  unsigned long result = (unsigned long)one->next->value * one->next->next->value;
  printf("Result: %ld\n", result);
}

int main(void) {
  first_star((int[]){3,8,9,1,2,5,4,6,7});
  first_star((int[]){4,6,7,5,2,8,1,9,3});

  second_star((int[]){3,8,9,1,2,5,4,6,7});
  second_star((int[]){4,6,7,5,2,8,1,9,3});
}
