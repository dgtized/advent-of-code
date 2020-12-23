#include <stdio.h>
#include <stdlib.h>

/* usage: gcc crab-cups.c -o crab-cups && ./crab-cups */

struct node {
  int value;
  struct node *next;
};

int ncups = 9;

struct node* create(int size) {
  struct node *t;
  struct node *head;

  /* int sequence[] = {3,8,9,1,2,5,4,6,7}; */
  int sequence[] = {4,6,7,5,2,8,1,9,3};

  head = (struct node*)malloc(sizeof(struct node));
  t = head;

  for(int i = 0; i < size; i++) {
    t->value = i < 9 ? sequence[i] : i + 1;
    if(i < size - 1) {
      t->next = (struct node*)malloc(sizeof(struct node));
    } else {
      t->next = head;
    }
    t = t->next;
  }

  return head;
}

void print_from(struct node *p, int count) {
  while(p && count > 0) {
    printf("%d ", p->value);
    p = p->next;
    count--;
  }
  printf("\n");
}

struct node* take(struct node* current) {
  struct node* head = current->next;
  struct node* lookahead = current->next->next->next;

  current->next = lookahead->next;
  lookahead->next = NULL;

  return head;
}

struct node* find(struct node* current, int value) {
  struct node* p = current->next;
  while(p->value != value) {
    p = p->next;
  }

  return p;
}

struct node* select_destination(struct node* current, int value, int m1, int m2, int m3) {
  int check = value;

  if(check < 1) check = ncups;
  while(check == m1 || check == m2 || check == m3) {
    check--;
    if(check < 1) check = ncups;
  }

  return find(current, check);
}

struct node* crab_cups(struct node* current) {
  struct node* move = take(current);

  struct node* dest = select_destination(
    current, current->value - 1,
    move->value, move->next->value, move->next->next->value);

  /* printf("dest: %d\npick up: ", dest->value); */
  /* print_from(move, 10); */

  struct node *after = dest->next;
  dest->next = move;
  move->next->next->next = after;

  return current;
}

int main(void) {
  struct node* initial = create(ncups);
  struct node* current = initial;
  int iterations = 1;
  while(iterations <= 100) {
    /* printf("\n-- move %d --\ncups: ", iterations); */
    /* print_from(current, 9); */
    /* printf("current: %d\n", current->value); */
    current = crab_cups(current)->next;
    iterations++;
  };

  struct node* one = find(current, 1);
  print_from(one->next, 8);
}
