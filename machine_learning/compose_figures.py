from pathlib import Path
import os, re, math, sys
import fitz
import pypdfium2 as pdfium
from PIL import Image
Image.MAX_IMAGE_PIXELS=None
MM=72/25.4
PRIVATE=Path(os.environ['HTGAP_INPUT_DIR'])
OUT=Path(os.environ['HTGAP_OUTPUT_DIR'])/'figures'
OUT.mkdir(parents=True,exist_ok=True)
A=PRIVATE/'ml'/'temporal'
B=PRIVATE/'ml'/'regional'
FONT=Path(os.getenv('HTGAP_FONT','C:/Windows/Fonts/arial.ttf'))
BOLD=Path(os.getenv('HTGAP_FONT_BOLD','C:/Windows/Fonts/arialbd.ttf'))
names={'AdaBoostM1':'AdaBoost','bayes_net':'Bayesian network','catboost':'CatBoost','extratrees':'Extra trees','gbm':'GBM','glmnet':'Elastic net','kknn':'KNN','lda':'LDA','lightgbm':'LightGBM','logistic':'Logistic','naive_bayes':'Naive Bayes','nnet':'Neural network','ranger':'Random forest','rpart':'Decision tree','svm':'SVM','xgboost':'XGBoost'}
specs=[('oof','Training cross-validation',A,['benchmark_ROC_multi_ggplot_all_prediction.pdf','benchmark_PRC_multi_ggplot.pdf','benchmark_Metric_Radar.pdf']),('development','Development',A,['multi_train_ROC_ggplot.pdf','multi_train_PRC_ggplot.pdf','multi_train_Metric_Radar.pdf']),('temporal','Temporal validation',A,['multi_external.validation_ROC_ggplot.pdf','multi_external.validation_PRC_ggplot.pdf','multi_external.validation_Metric_Radar.pdf']),('regional','Regional validation',B,['multi_external.validation_ROC_ggplot.pdf','multi_external.validation_PRC_ggplot.pdf','multi_external.validation_Metric_Radar.pdf'])]
def stripped(doc):
    cp=fitz.open(stream=doc.tobytes(),filetype='pdf')
    for x in cp[0].get_contents():
        raw=cp.xref_stream(x)
        cp.update_stream(x,re.sub(rb'\bBT\b.*?\bET\b',b'',raw,flags=re.S))
    return cp

def source(key,title,folder,files):
    docs=[fitz.open(folder/f) for f in files]
    found=re.findall(r'([A-Za-z0-9_]+)\s+\(AUC=([0-9.]+)\)',docs[0][0].get_text())
    assert len(found)==16
    curves=[x for x in docs[0][0].get_drawings() if x['color'] and max(x['color'])-min(x['color'])>.05 and x['rect'].height>5 and len(x['items'])>1]
    assert len(curves)==16
    palette={n:tuple(x['color']) for (n,_),x in zip(found,curves)}
    rects=[]
    for d in docs[:2]:
        grid=[x for x in d[0].get_drawings() if x['color'] and abs(x['color'][0]-.921569)<.001 and x['width']>1]
        vv=[x['rect'].x0 for x in grid if x['rect'].width<.01 and x['rect'].height>10]
        hh=[x['rect'].y0 for x in grid if x['rect'].height<.01 and x['rect'].width>10]
        assert len(vv)==5 and len(hh)==5
        rects.append(fitz.Rect(min(vv),min(hh),max(vv),max(hh)))
    circles=[x for x in docs[2][0].get_drawings() if x['color'] and abs(x['color'][0]-.745098)<.001 and len(x['items'])>300]
    assert circles
    rects.append(max((x['rect'] for x in circles),key=lambda r:r.width*r.height))
    return {'key':key,'title':title,'folder':folder,'files':files,'original':docs,'docs':[stripped(d)for d in docs],'rects':rects,'auc':dict(found),'order':[x for x,_ in found],'colors':palette}

sources=[source(*s)for s in specs]
for s in sources[1:]:
    assert s['order']==sources[0]['order']
    assert all(max(abs(a-b) for a,b in zip(s['colors'][n],sources[0]['colors'][n]))<1e-5 for n in s['order'])

def font(page):
    page.insert_font(fontname='Arial',fontfile=str(FONT));page.insert_font(fontname='ArialBold',fontfile=str(BOLD))

def text(page,x,y,txt,size=12,bold=False,align='center',rotate=0):
    fn='ArialBold' if bold else 'Arial';ff=fitz.Font(fontfile=str(BOLD if bold else FONT));length=ff.text_length(txt,fontsize=size)
    if rotate==0:
        xx=x-length/2 if align=='center' else x
        page.insert_text((xx,y),txt,fontname=fn,fontsize=size,color=(.10,.12,.13))
    else:page.insert_text((x,y+length/2),txt,fontname=fn,fontsize=size,color=(.10,.12,.13),rotate=rotate)

def panel(page,s,index,box,compact=False,roc_only=False):
    x,y,w,h=box;fs=10.5 if compact else 14;tick=9.5 if compact else 12
    if index<2:
        padleft=34 if compact else 50;padright=10;bottom=29 if compact else 39
        rect=fitz.Rect(x+padleft,y+5,x+w-padright,y+h-bottom)
        sr=s['rects'][index];pad=.8
        clip=fitz.Rect(sr.x0-pad,sr.y0-pad,sr.x1+pad,sr.y1+pad)
        page.show_pdf_page(rect,s['docs'][index],0,clip=clip,keep_proportion=False)
        left=rect.x0+pad/clip.width*rect.width;right=rect.x1-pad/clip.width*rect.width
        top=rect.y0+pad/clip.height*rect.height;bot=rect.y1-pad/clip.height*rect.height
        vals=[0,.5,1] if compact else [0,.25,.5,.75,1]
        for v in vals:
            xx=left+v*(right-left);yy=bot-v*(bot-top)
            text(page,xx,bot+tick+4,f'{v:g}',tick)
            st=f'{v:g}';ff=fitz.Font(fontfile=str(FONT));page.insert_text((left-5-ff.text_length(st,fontsize=tick),yy+tick*.34),st,fontname='Arial',fontsize=tick)
        text(page,(left+right)/2,bot+bottom-7,'1 - specificity' if index==0 else 'Recall',fs)
        text(page,x+fs+2,(top+bot)/2,'Sensitivity' if index==0 else 'Precision',fs,rotate=90)
    else:
        dim=min(w-(64 if compact else 92),h-38);cx=x+w/2;cy=y+h/2-5
        rect=fitz.Rect(cx-dim/2,cy-dim/2,cx+dim/2,cy+dim/2)
        page.show_pdf_page(rect,s['docs'][2],0,clip=s['rects'][2],keep_proportion=True)
        axes=[('AUC',math.pi/2),('Accuracy',math.pi/6),('F1',-math.pi/6),('PPV',-math.pi/2),('Sensitivity',-5*math.pi/6),('Specificity',5*math.pi/6)]
        for st,angle in axes:
            rx=dim/2+(21 if compact else 30);ry=dim/2+14
            text(page,cx+rx*math.cos(angle),cy-ry*math.sin(angle)+fs*.3,st,9.6 if compact else 12.5)
        for v in [0,.5,1]:text(page,cx-7,cy-v*dim/2+3,f'{v:g}',8 if compact else 10)

def legend(page,s,y,w,compact=False):
    ncol=3 if compact else 4;nrow=math.ceil(16/ncol);size=10.1 if compact else 14
    margin=9 if compact else 20;cell=(w-2*margin)/ncol;dy=15.3 if compact else 22
    maxtext=max(fitz.Font(fontfile=str(FONT)).text_length(f'{names[n]} (AUC={float(s["auc"][n]):.3f})',fontsize=size) for n in s['order'])
    if maxtext+32>cell:size*= (cell-32)/maxtext
    for j,n in enumerate(s['order']):
        col=j//nrow;row=j%nrow;x=margin+col*cell;yy=y+row*dy
        page.draw_line((x,yy-3),(x+11,yy-3),color=s['colors'][n],width=1.8 if compact else 2.1)
        text(page,x+15,yy,f'{names[n]} (AUC={float(s["auc"][n]):.3f})',size,align='left')
    return size,nrow

def row(s,compact=False,index=0):
    w,h=(200*MM,120*MM) if compact else (310*MM,175*MM)
    d=fitz.open();p=d.new_page(width=w,height=h);font(p)
    if compact:
        for k,head in enumerate(['ROC','PR','Radar']):text(p,(k+.5)*w/3,15,head,12.5,bold=True)
    else:
        for k,title in enumerate(['A  ROC','B  Precision-recall','C  Radar']):
            text(p,k*w/3+18,27,title,18,bold=True,align='left')
    y=20 if compact else 45;ph=182 if compact else 281
    for k in range(3):panel(p,s,k,(k*w/3,y,w/3,ph),compact=compact)
    legend_y=222 if compact else 366
    font_size,nrows=legend(p,s,legend_y,w,compact)
    return d,{'legend_font_pt':font_size,'legend_rows':nrows,'width_mm':w/MM,'height_mm':h/MM}

def export(d,stem):
    path=Path(stem);path.parent.mkdir(parents=True,exist_ok=True);d.save(path.with_suffix('.pdf'),garbage=4,deflate=True)
    formats=os.getenv('HTGAP_FORMATS','pdf,svg,png,tiff').split(',')
    if 'svg' in formats: path.with_suffix('.svg').write_text(d[0].get_svg_image(text_as_path=False),encoding='utf-8')
    pdf=pdfium.PdfDocument(str(path.with_suffix('.pdf')))
    if 'png' in formats: pdf[0].render(scale=300/72).to_pil().convert('RGB').save(path.with_suffix('.png'),dpi=(300,300))
    if 'tiff' in os.getenv('HTGAP_FORMATS','pdf,svg,png,tiff').split(','): pdf[0].render(scale=600/72).to_pil().convert('RGB').save(path.with_suffix('.tiff'),dpi=(600,600),compression='tiff_lzw')

def stack(docs,ncol=1):
    w=docs[0][0].rect.width;h=docs[0][0].rect.height;nrow=math.ceil(len(docs)/ncol)
    d=fitz.open();p=d.new_page(width=w*ncol,height=h*nrow)
    for j,doc in enumerate(docs):p.show_pdf_page(fitz.Rect(j%ncol*w,j//ncol*h,(j%ncol+1)*w,(j//ncol+1)*h),doc,0)
    return d


selected=int(sys.argv[1]) if len(sys.argv)>1 else None
for i,s in enumerate(sources,7):
    if selected is not None and i!=selected: continue
    document,_=row(s)
    export(document,OUT/f'Figure_S{i}')
