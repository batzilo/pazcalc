open Printf

let header prog_name = 
  sprintf "xseg\tsegment\tpublic 'code'\n\
    \tassume\tcs:xseg, ds:xseg, ss:xseg\n\
    \torg\t100h\n\
    main\tproc\tnear\n\
    \tcall\tnear ptr %s\n\
    \tmov\tax,4C00h\n\
    \tint\t21h\n\
    main\tendp\n"
  prog_name

let footer = "xseg\tends\n\tend\tmain";;
