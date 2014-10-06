function genCopyStrings(filename,n)
    fh = fopen(filename,'w');
    for i = 1:n
        strs = genCopyString();
        printStr = prepareForPrinting(strs);
        fprintf(fh,'%s\n',printStr);
    end
    fclose(fh);
end

function out = prepareForPrinting(in)
   head = 'srs(''A''-[';
   body = [listStr(in{1}) ',' listStr(in{2})];
   tail = ']).';
   out = [head body tail];
end

function str2 = listStr(str1)
    str2 = '[';
    if length(str1) > 1
      for i = 1:length(str1-1)
        str2 = [str2 str1(i) ','];
      end
    end
    str2 = [str2 str1(end) ']'];
end

function strs = genCopyString()
    branch = find(mnrnd(1,[.25,.25,.2,.3]));
    switch branch
      case 1
        tmpStrs = genCopyString();
        strs{1} = [tmpStrs{1} 'a'];
        strs{2} = [tmpStrs{2} 'a'];
      case 2
        tmpStrs = genCopyString();
        strs{1} = [tmpStrs{1} 'b'];
        strs{2} = [tmpStrs{2} 'b'];
      case 3
        strs{1} = 'a';
        strs{2} = 'a';
      case 4
        strs{1} = 'b';
        strs{2} = 'b';
    end
end
