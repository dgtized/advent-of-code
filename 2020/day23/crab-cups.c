#include <stdio.h>
#include <stdlib.h>

// usage: gcc crab-cups.c -o crab-cups && ./crab-cups

struct node {
  int value;
  struct node *next;
};

struct node* create(int size) {
  struct node *t;
  struct node *head;

  int sequence[] = {3,8,9,1,2,5,4,6,7};

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

struct node* select_destination(struct node* current, int value) {
  struct node* p = current->next;

  while(p->value != value) {
    p = p->next;
  }

  return p;
}

struct node* crab_cups(struct node* current) {
  //int dest = current->value - 1;

  return current;
}

int main(void) {
  struct node* initial = create(9);
  struct node* current = initial;
  print_from(current, 20);

  struct node* move = take(current);
  print_from(current, 20);
  print_from(move, 10);

  struct node* dest = select_destination(current, current->value - 1);
  print_from(dest, 10);
}
