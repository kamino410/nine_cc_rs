fn main() {
    if std::env::args().len() != 2 {
        panic!("Invalid args");
    }
    println!(".intel_syntax noprefix");
    println!(".globl _main");
    println!("_main:");
    println!(
        "  mov rax, {}",
        std::env::args().nth(1).unwrap().parse::<usize>().unwrap()
    );
    println!("  ret");
}
