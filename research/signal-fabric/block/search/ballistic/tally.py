import sys, importlib.util as u
sys.path.insert(0,'.');sys.path.insert(0,'ballistic')
s=u.spec_from_file_location('design','ballistic/design.py');D=u.module_from_spec(s);s.loader.exec_module(D)
ss=D.SEC+[('end',len(D.R))]
print('; '.join('%s %d'%(a[0],b[1]-a[1]) for a,b in zip(ss,ss[1:])),'; before first mark',ss[0][1])
print('shapes',D.SHAPES,'modes',len(D._m))
