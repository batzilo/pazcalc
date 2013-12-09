xseg    segment public 'code'
    assume	cs : xseg, ds : xseg, ss : xseg
    org	100h
main	proc	near
	call	near ptr _hello_1
	mov	ax, 4C00h
	int	21h
main endp
_hello_1	proc	near
	push	bp
	mov	bp, sp
	sub	sp, 0
@1__hello_1:
	lea	si, byte ptr @str1
	push	si
	sub	sp, 2
	push	bp
	call	near ptr _writeString
	add	sp, 6
@2__hello_1:
@_hello_1:
	mov	sp, bp
	pop	bp
	ret
_hello_1	endp
@str1	db 'Hello World!'
	db 10
	db 0
	extrn	_writeString	: proc
xseg ends
	end  main
