int main() 
{
  int n = 30; 

  if(n == 1){
      return 0;
  }

  int count = 0;         
  for(int i = 2; i < n; i++) 
  {
    if(n % i == 0)
      count++;
  }
  //Check whether Prime or not
  if(count == 0)           
  {
    return 1;
  }
  else
  {
    return 0;
  }
}
