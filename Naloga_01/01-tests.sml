val factorial0 = factorial 0 = 1;
val factorial1 = factorial 1 = 1;
val factorial2 = factorial 2 = 2;
val factorial3 = factorial 3 = 6;
val factorial4 = factorial 4 = 24;
val factorial5 = factorial 5 = 120;

val power0 = power (2, 0) = 1;
val power1 = power (2, 1) = 2;
val power2 = power (2, 2) = 4;
val power3 = power (3, 3) = 27;
val power4 = power (3, 4) = 81;

val gcd0 = gcd (1,2) = 1;
val gcd1 = gcd (12,24) = 12;
val gcd2 = gcd (12,81) = 3;

val len0 = len ([0,1,2]) = 3;
val len1 = len ([0,1,2,84,15]) = 5;

val last0 = last ([0,1,2,84,15]) = SOME 15;
val last1 = last ([]) = NONE;

val nth0 = nth ([0,1,2,84,15], 3) = SOME 84;
val nth1 = nth ([0,1,2,84,15], 14) = NONE;

val insert0 = insert ([0,1,2,84,15], 0, 99) = [99,0,1,2,84,15];
val insert1 = insert ([0,1,2,84,15], 5, 99) = [0,1,2,84,15,99];
val insert2 = insert ([0,1,2,84,15], 4, 99) = [0,1,2,84,99,15];


val delete0 = delete ([0,1,2,3,0,1,2,3], 2) = [0,1,3,0,1,3];

val reverse0 = reverse ([0,1,2,84,15]) = [15,84,2,1,0] = true;
val reverse1 = reverse ([0,1,2,84,15]) = [0,1,2,84,15] = false;

val palindrome0 = palindrome ([]) = true;
val palindrome1 = palindrome ([1]) = true;
val palindrome2 = palindrome ([0,0,1]) = false;
val palindrome3 = palindrome ([0,1,3,1,0]) = true;


OS.Process.exit OS.Process.success;