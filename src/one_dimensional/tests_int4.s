	.arch armv8-a
	.text
	.align	2
	.globl ___mod_tests_int4_MOD_addition_int4
___mod_tests_int4_MOD_addition_int4:
LFB0:
	stp	x20, x21, [sp, -112]!
LCFI0:
	str	x0, [sp, 72]
	str	x1, [sp, 64]
	str	x2, [sp, 56]
	str	x3, [sp, 48]
	str	x4, [sp, 40]
	str	x5, [sp, 32]
	str	x6, [sp, 24]
	ldr	x0, [sp, 48]
	ldr	w0, [x0]
	sxtw	x0, w0
	str	x0, [sp, 104]
	ldr	x0, [sp, 104]
	mov	x1, 0
	cmp	x0, 0
	csel	x0, x0, x1, ge
	mov	x20, x0
	mov	x21, 0
	lsr	x0, x20, 59
	lsl	x13, x21, 5
	orr	x13, x0, x13
	lsl	x12, x20, 5
	ldr	x0, [sp, 48]
	ldr	w0, [x0]
	sxtw	x0, w0
	str	x0, [sp, 96]
	ldr	x0, [sp, 96]
	mov	x1, 0
	cmp	x0, 0
	csel	x0, x0, x1, ge
	mov	x16, x0
	mov	x17, 0
	lsr	x0, x16, 59
	lsl	x11, x17, 5
	orr	x11, x0, x11
	lsl	x10, x16, 5
	ldr	x0, [sp, 48]
	ldr	w0, [x0]
	sxtw	x0, w0
	str	x0, [sp, 88]
	ldr	x0, [sp, 88]
	mov	x1, 0
	cmp	x0, 0
	csel	x0, x0, x1, ge
	mov	x14, x0
	mov	x15, 0
	lsr	x0, x14, 59
	lsl	x9, x15, 5
	orr	x9, x0, x9
	lsl	x8, x14, 5
	ldr	x0, [sp, 48]
	ldr	w0, [x0]
	sxtw	x4, w0
	mov	x0, 1
L3:
	cmp	x0, x4
	bgt	L2
	sub	x2, x0, #1
	ldr	x1, [sp, 32]
	ldr	w3, [x1, x2, lsl 2]
	sub	x2, x0, #1
	ldr	x1, [sp, 24]
	ldr	w1, [x1, x2, lsl 2]
	sub	x2, x0, #1
	add	w3, w3, w1
	ldr	x1, [sp, 40]
	str	w3, [x1, x2, lsl 2]
	add	x0, x0, 1
	b	L3
L2:
	nop
	ldp	x20, x21, [sp], 112
LCFI1:
	ret
LFE0:
	.section __TEXT,__eh_frame,coalesced,no_toc+strip_static_syms+live_support
EH_frame1:
	.set L$set$0,LECIE1-LSCIE1
	.long L$set$0
LSCIE1:
	.long	0
	.byte	0x1
	.ascii "zR\0"
	.uleb128 0x1
	.sleb128 -8
	.byte	0x1e
	.uleb128 0x1
	.byte	0x10
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.align	3
LECIE1:
LSFDE1:
	.set L$set$1,LEFDE1-LASFDE1
	.long L$set$1
LASFDE1:
	.long	LASFDE1-EH_frame1
	.quad	LFB0-.
	.set L$set$2,LFE0-LFB0
	.quad L$set$2
	.uleb128 0
	.byte	0x4
	.set L$set$3,LCFI0-LFB0
	.long L$set$3
	.byte	0xe
	.uleb128 0x70
	.byte	0x94
	.uleb128 0xe
	.byte	0x95
	.uleb128 0xd
	.byte	0x4
	.set L$set$4,LCFI1-LCFI0
	.long L$set$4
	.byte	0xd5
	.byte	0xd4
	.byte	0xe
	.uleb128 0
	.align	3
LEFDE1:
	.ident	"GCC: (Homebrew GCC 11.2.0_3) 11.2.0"
	.subsections_via_symbols
