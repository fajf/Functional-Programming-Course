val test_next = next 1 = 2;
val test_add = add (3, 2) = 5 = true;
val test_majority = majority (false, true, true) = true;
val test_median1 = Real.== (median (4.0, 8.0, 6.0), 8.0) = false;
val test_median2 = Real.== (median (4.0, 8.0, 6.0), 4.0) = false;
val test_median3 = Real.== (median (4.0, 8.0, 6.0), 6.0) = true;
val test_triangle = triangle (4, 4, 4) = true;

OS.Process.exit OS.Process.success;