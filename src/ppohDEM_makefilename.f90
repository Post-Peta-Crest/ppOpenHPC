!!====================================================================!!
!!                                                                    !!
!!   Software Name : ppohDEM                                          !!
!!         Version : 0.1.0                                            !!
!!                                                                    !!
!!   License:                                                         !!
!!     This file is part of ppohDEM.                                  !!
!!     ppohDEM is a free software, you can use it under the terms     !!
!!     of The MIT License (MIT). See LICENSE file and User's guide    !!
!!     for more details.                                              !!
!!                                                                    !!
!!   ppOpen-HPC project:                                              !!
!!     Open Source Infrastructure for Development and Execution of    !!
!!     Large-Scale Scientific Applications on Post-Peta-Scale         !!
!!     Supercomputers with Automatic Tuning (AT).                     !!
!!                                                                    !!
!!   Organizations:                                                   !!
!!     The University of Tokyo                                        !!
!!       - Information Technology Center                              !!
!!       - Atmosphere and Ocean Research Institute (AORI)             !!
!!       - Interfaculty Initiative in Information Studies/            !!
!!         Earthquake Research Institute (ERI)                        !!
!!       - Graduate School of Frontier Science                        !!
!!     Kyoto University                                               !!
!!       - Academic Center for Computing and Media Studies            !!
!!     Japan Agency for Marine-Earth Science and Technology (JAMSTEC) !!
!!                                                                    !!
!!   Sponsorship:                                                     !!
!!     Japan Science and Technology Agency (JST), Basic Research      !!
!!     Programs: CREST, Development of System Software Technologies   !!
!!     for post-Peta Scale High Performance Computing.                !!
!!                                                                    !!
!!   Copyright (c) 2014 <Miki Yamamoto Matsuo, JAMSTEC                !!
!!                       mikiy(at)jamstec.go.jp                       !!
!!                                                                    !!
!!====================================================================!!
    
subroutine ppohDEM_makefilename (header,number,suffix)

     character(len=80) :: header
     integer :: number
     character(len=80) :: suffix
     
     character(len=1) :: index1
     character(len=2) :: index2
     character(len=3) :: index3
     integer :: length1,length2
      
     header=adjustl(header)
     length1=len_trim(header)
     suffix=adjustl(suffix)
     length2=len_trim(suffix)
     
     if(myrank.le.9)then
       ii=1
       write(index1,'(i1.1)') number
     else if (number.le.99) then
       ii=2
       write(index2,'(i2.2)') number
     else if (number.le.999) then
       ii=3
       write(index3,'(i3.3)') number
     endif

     if(ii==1) header=header(1:length1)//index1//suffix(1:length2)
     if(ii==2) header=header(1:length1)//index2//suffix(1:length2)
     if(ii==3) header=header(1:length1)//index3//suffix(1:length2)

     return
end