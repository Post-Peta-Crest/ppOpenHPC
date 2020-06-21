subroutine RCIP1D(PV,PVup,VIA,VIAup,us,nPV,nVIA,dts,dxs)
  implicit none
  double precision,intent(in)::PV,PVup,VIA,VIAup,us,dts,dxs
  double precision::nPV,nVIA
  double precision::xii,D,ai,bi
  double precision::Si,BB,alp,ci
  xii=-us*dts
  D=-dsign(1.0d0,us)*dxs
  Si=(PVup-PV)/D
  if((VIAup-Si)==0.0d0) then
     BB=-1.0d0/D
  else
     BB=(dabs((Si-VIA)/(VIAup-Si+1.0d-10))-1.0d0)/D
  endif
  if((Si-VIA)/(VIAup-Si+1.0d-10)>=0.0d0) then
     alp=1.0d0
  else
     alp=0.0d0
  endif
  ai=(VIA-Si+(VIAup-Si)*(1.0d0+alp*BB*D))/D**2
  bi=Si*alp*BB+(Si-VIA)/D-ai*D
  ci=VIA+PV*alp*BB
  nPV=(ai*xii**3+bi*xii**2+ci*xii+PV)/(1.0d0+alp*BB*xii)
  nVIA=(3.0d0*ai*xii**2+2.0d0*bi*xii+ci)/(1.0d0+alp*BB*xii) &
       -(ai*xii**3+bi*xii**2+ci*xii+PV)*alp*BB/(1.0d0+alp*BB*xii)**2
  return
end subroutine RCIP1D
