
histcmp <- function(a,b,aname='a',bname='b',main='',ylab='',xlab='', acol=rgb(0,0,1,1/4), bcol=rgb(1,0,0,1/4), breaks=100) {
    a = a[!is.nan(a)]
    b = b[!is.nan(b)]
    a = a[!is.na(a)]
    b = b[!is.na(b)]
    x_max = max(a,b)
    x_min = min(a,b)
    bins = seq(x_min,x_max,length.out=breaks)
    # Compute histogram (probability)
    ha <- hist(a, breaks=bins, plot=FALSE, freq=FALSE)
    hb <- hist(b, breaks=bins, plot=FALSE, freq=FALSE)
    # Compute axis limits
    ha$counts = ha$density/sum(ha$density)
    hb$counts = hb$density/sum(hb$density)
    y_max = max(ha$counts,hb$counts)
    y_min = min(hb$counts,hb$counts)

    # Plot
    par(fig=c(0,1,0.18,1), new=TRUE)
    plot(ha, col=acol, xlim=c(x_min,x_max), ylim=c(y_min,y_max), ylab=ylab, main=main, xlab='', axes=FALSE)
    axis(2,at=seq(0,max(ha$counts,hb$counts),length.out=5))
    plot(hb, col=bcol, xlim=c(x_min,x_max), add=T)
    par(fig=c(0,1,0,0.45),new=TRUE)
    df <- data.frame(values=c(a,b),vars=rep(c(aname,bname),times=c(length(a),length(b))))
    boxplot(values ~ vars, data = df,col=c(bcol,acol), horizontal=TRUE, axes=FALSE, ylim=c(x_min,x_max), xlab=xlab, notch=TRUE, pch=1, pars=list(outcol=rgb(0,0,0,1/4)), names=c(aname,bname))
    mtext(aname,side=2,line=1,at=1)
    mtext(bname,side=2,line=1,at=2)
    axis(1)
}
