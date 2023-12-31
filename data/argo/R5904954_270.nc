CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:17:50Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7(   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7h   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     7�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8,   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           80   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    88   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~            8<   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8D   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8L   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8T   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8X   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8`   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9`   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9d   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9h   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9l   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  @$   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  A�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  H�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  J8   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  P�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  W�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  YP   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  `   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  a�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  hh   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  p�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  w�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  y0   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �@   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �D   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �H   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �L   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �P   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20181005191750  20181005191750  5904954 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL              A   AO  6557                            2B  A   APEX                            7468                            062512                          846 @����x��1   @���l� @5Nz�G��dz��"��1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                     A   A   B   @�33@�  A   A   AA��A`  A�  A�  A�  A�  A�33A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@ffBH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&�C(  C*  C,  C.  C0�C2  C4  C6  C8  C9�fC<  C>  C@  CB  CD  CF  CH�CJ  CK�fCN  CP  CR  CS�fCU�fCX  CZ  C\  C^  C_�fCa�fCd  Cf�Ch  Ci�fCl  Cn�Cp�Cr  Ct  Cv�Cx�Cz�C|  C~  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C��C��C��C��C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C��C�  C�  C��C�  C�  C��C�  C��3C�  C��3C��3C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C��3C�  C��C�  C�  C��3C��3C��C��C��C�  C��3C��3C�  C��C�  C�  C�  C�  C��3C�  C�  C��C�  C��3C�  C��C�  C��3C��3C��C�  C��C��C��C��C�  C�  C�  C��3C�  C�  C��3C��3C�  C�  C�  C�  C��C�  C��3C�  C�  C�  C�  C�  C�  C�  C��3C��3D � D  D�fDfD�fD  D� D  D� D  Dy�D  D� D��D� DfD� D��D	y�D
  D
y�D  Dy�D��D  D�fD  D� DfD�fD  Dy�D  D�fD  Dy�D  D�fDfD� D��D � D ��D!� D"  D"� D.�fD/  D/y�D0  D0� D1  D1� D1�3D2y�D3  D3y�D4  D4� D5fD5�fD6  D6� D7fD7�fD8  D8y�D9fD9�fD:fD:� D;  D;� D<  D<� D<��D=� D>  D>�fD?fD?�fD@  D@y�DAfDA��DB  DB� DCfDC� DD�DD�fDEfDE�fDF  DF� DGfDG�fDH  DH�fDI  DIy�DI��DJ� DK  DK� DL  DLy�DM  DMs3DM��DNy�DO  DO� DP  DP� DP��DQ� DR  DR� DS  DS� DT  DT� DU  DU�fDVfDV� DV��DW� DXfDX� DY  DYy�DY��DZ�fD[  D[y�D\fD\�fD]  D]y�D^  D^�fD_  D_� D`  D`y�D`��Da� DbfDb� DcfDc� Dd  Dd� Dd��Dey�Df  Df� Df��Dgy�Dg��Dh� Di  Diy�Di��Dj� Dk  Dk�fDlfDl�fDm  Dmy�Dn  Dn� Do  Do� DpfDp�fDp��Dq� Dr  Dry�Dr��Ds� Dt  Dty�Du  Du� Du��Dv� DwfDw� Dw�fDy��D�Q�D��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��
@���A Q�A Q�AA�A`Q�A�(�A�(�A�(�A�(�A�\)A�(�A�(�A�(�B {B{B{B{B {B({B0{B8{B@z�BH{BP{BX{B`{Bh{Bp{Bx{B�
=B�
=B�
=B�
=B�
=B�
=B�
=B�
=B�
=C CCCCC
CCCCCCCCCCC C"C$C&�C(C*C,C.C0�C2C4C6C8C9�C<C>C@CBCDCFCH�CJCK�CNCPCRCS�CU�CXCZC\C^C_�Ca�CdCf�ChCi�ClCn�Cp�CrCtCv�Cx�Cz�C|C~C��C��C�\C��C��C��C��C��C��C��C��C��C��C���C��C��C��C��C��C��C��C��C��C��C���C��C��C��C��C��C��C�\C�\C�\C�\C��C��C��C��C���C���C��C��C��C��C��C�\C��C��C�\C��C��C�\C��C���C��C���C���C��C��C��C��C��C���C��C��C��C��C��C���C��C�\C��C��C���C���C�\C�\C�\C��C���C���C��C�\C��C��C��C��C���C��C��C�\C��C���C��C�\C��C���C���C�\C��C�\C�\C�\C�\C��C��C��C���C��C��C���C���C��C��C��C��C�\C��C���C��C��C��C��C��C��C��C���C���D �HDHD��D�D��DHD�HDHD�HDHDz�DHD�HD��D�HD�D�HD��D	z�D
HD
z�DHDz�D��DHD��DHD�HD�D��DHDz�DHD��DHDz�DHD��D�D�HD��D �HD ��D!�HD"HD"�HD.��D/HD/z�D0HD0�HD1HD1�HD1�{D2z�D3HD3z�D4HD4�HD5�D5��D6HD6�HD7�D7��D8HD8z�D9�D9��D:�D:�HD;HD;�HD<HD<�HD<��D=�HD>HD>��D?�D?��D@HD@z�DA�DA�DBHDB�HDC�DC�HDDDD��DE�DE��DFHDF�HDG�DG��DHHDH��DIHDIz�DI��DJ�HDKHDK�HDLHDLz�DMHDMt{DM��DNz�DOHDO�HDPHDP�HDP��DQ�HDRHDR�HDSHDS�HDTHDT�HDUHDU��DV�DV�HDV��DW�HDX�DX�HDYHDYz�DY��DZ��D[HD[z�D\�D\��D]HD]z�D^HD^��D_HD_�HD`HD`z�D`��Da�HDb�Db�HDc�Dc�HDdHDd�HDd��Dez�DfHDf�HDf��Dgz�Dg��Dh�HDiHDiz�Di��Dj�HDkHDk��Dl�Dl��DmHDmz�DnHDn�HDoHDo�HDp�Dp��Dp��Dq�HDrHDrz�Dr��Ds�HDtHDtz�DuHDu�HDu��Dv�HDw�Dw�HDw�Dy�4D�R>D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��RA��RA��9A��-A��FA��RA��9A��RA��^A��^A�A�A�A���A��FA���A�|�A�XA�?}A�5?A�/A�/A�(�A�(�A�$�A� �A�$�A�&�A�-A�?}A�oA��DA���A�(�A�
=A�1A���A��+A��hA�A���A�dZA�"�A���A�bNA�
=A�M�A���A�XA�{A��TA�|�A���A�bNA�n�A��yA��PA�A���A�K�A���A���A�ȴA�n�A�z�A�?}A�  A�hsA���A�ȴA���A��+A�A�A��A���A�jA���A�v�A�A�A���A���A�S�A�A��A�`BA~�+A|9XA|A{��A{��Az��Ayt�Aw��Av�yAvv�At�HAt-Aq��Ao�;Ao�hAn��AmS�Ajr�Ag�#AbbA_dZA^�A]�A\�!AZ �AY33AW��AU��AS�PAR��AQ�
AP��ANffAL1AJ��AJ�AG��AF�!AE�TAE�PAD�ADQ�AD  AC�AC�AB�jAAt�A@{A?�^A?G�A>ȴA=�A<A�A;K�A:ȴA9��A8-A7hsA5�
A4��A4�A3�#A3�A3dZA2�A1�FA0��A.E�A+t�A)��A(�RA(�\A(5?A'��A'`BA&��A&bNA#�A"�RA!�
A!?}A �/A VA%A�A�HAĜAbNA�PA\)AK�A�A��Ar�A�wAG�A��AQ�A�AVAĜA��A �A�PA�AQ�A�A��A�A�PA��Ax�AM�AC�AVA
�A	|�A�/AVA7LA%A1A33A�RA��A�uAA�FAXA ��@��F@�O�@���@��@�dZ@��@�bN@�o@�{@�O�@��@��;@�J@�@��;@�~�@�^@��@�ȴ@�~�@�$�@�7L@��#@��
@݉7@ۮ@�-@��#@�O�@��@�5?@��`@�  @���@љ�@У�@�Z@�9X@Ϯ@��@͉7@��R@���@���@���@��@�V@�?}@�1'@�\)@�~�@�p�@��j@���@��T@�^5@���@���@���@�l�@��\@���@�&�@�`B@��@��@��@��+@�^5@�M�@�E�@�E�@�5?@�5?@��@�&�@�G�@��@��u@�r�@���@���@�E�@�@�M�@�$�@��#@�@�@���@�`B@���@���@���@�+@�-@��T@�J@�{@�@�J@�{@�J@���@���@�?}@���@�Ĝ@��/@�I�@�(�@�(�@� �@��;@�l�@�C�@�+@�+@�K�@�@�v�@�V@�=q@��@��@��@��#@�X@��@��@��@��@���@��9@���@��m@���@�t�@�+@�
=@��@�V@���@���@�hs@�V@�Ĝ@�z�@�I�@�(�@�  @���@���@�S�@��@��H@��!@���@�v�@��@���@�hs@�O�@�?}@�7L@�/@�&�@��@��u@� �@�1@�ƨ@���@�dZ@�K�@�C�@�o@���@��H@���@�V@�-@�{@��@�@�x�@�O�@��@��/@�Ĝ@��j@���@��D@�j@�A�@�|�@�
=@��@��H@��@���@��R@���@�^5@�=q@�@��@��@��^@�hs@�%@�]d@~H�@h�?111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A��RA��RA��9A��-A��FA��RA��9A��RA��^A��^A�A�A�A���A��FA���A�|�A�XA�?}A�5?A�/A�/A�(�A�(�A�$�A� �A�$�A�&�A�-A�?}A�oA��DA���A�(�A�
=A�1A���A��+A��hA�A���A�dZA�"�A���A�bNA�
=A�M�A���A�XA�{A��TA�|�A���A�bNA�n�A��yA��PA�A���A�K�A���A���A�ȴA�n�A�z�A�?}A�  A�hsA���A�ȴA���A��+A�A�A��A���A�jA���A�v�A�A�A���A���A�S�A�A��A�`BA~�+A|9XA|A{��A{��Az��Ayt�Aw��Av�yAvv�At�HAt-Aq��Ao�;Ao�hAn��AmS�Ajr�Ag�#AbbA_dZA^�A]�A\�!AZ �AY33AW��AU��AS�PAR��AQ�
AP��ANffAL1AJ��AJ�AG��AF�!AE�TAE�PAD�ADQ�AD  AC�AC�AB�jAAt�A@{A?�^A?G�A>ȴA=�A<A�A;K�A:ȴA9��A8-A7hsA5�
A4��A4�A3�#A3�A3dZA2�A1�FA0��A.E�A+t�A)��A(�RA(�\A(5?A'��A'`BA&��A&bNA#�A"�RA!�
A!?}A �/A VA%A�A�HAĜAbNA�PA\)AK�A�A��Ar�A�wAG�A��AQ�A�AVAĜA��A �A�PA�AQ�A�A��A�A�PA��Ax�AM�AC�AVA
�A	|�A�/AVA7LA%A1A33A�RA��A�uAA�FAXA ��@��F@�O�@���@��@�dZ@��@�bN@�o@�{@�O�@��@��;@�J@�@��;@�~�@�^@��@�ȴ@�~�@�$�@�7L@��#@��
@݉7@ۮ@�-@��#@�O�@��@�5?@��`@�  @���@љ�@У�@�Z@�9X@Ϯ@��@͉7@��R@���@���@���@��@�V@�?}@�1'@�\)@�~�@�p�@��j@���@��T@�^5@���@���@���@�l�@��\@���@�&�@�`B@��@��@��@��+@�^5@�M�@�E�@�E�@�5?@�5?@��@�&�@�G�@��@��u@�r�@���@���@�E�@�@�M�@�$�@��#@�@�@���@�`B@���@���@���@�+@�-@��T@�J@�{@�@�J@�{@�J@���@���@�?}@���@�Ĝ@��/@�I�@�(�@�(�@� �@��;@�l�@�C�@�+@�+@�K�@�@�v�@�V@�=q@��@��@��@��#@�X@��@��@��@��@���@��9@���@��m@���@�t�@�+@�
=@��@�V@���@���@�hs@�V@�Ĝ@�z�@�I�@�(�@�  @���@���@�S�@��@��H@��!@���@�v�@��@���@�hs@�O�@�?}@�7L@�/@�&�@��@��u@� �@�1@�ƨ@���@�dZ@�K�@�C�@�o@���@��H@���@�V@�-@�{@��@�@�x�@�O�@��@��/@�Ĝ@��j@���@��D@�j@�A�@�|�@�
=@��@��H@��@���@��R@���@�^5@�=q@�@��@��@��^@�hs@�%@�]d@~H�@h�?111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�B�B�B�B�B�B�B�B�B�B��B��B��B��B��B	7B\B�B�B �B!�B"�B$�B$�B$�B$�B'�B,B6FB@�BŢB�}B�wB�dB�'B��B��B��B�VB�Bu�Bk�BbNB^5B[#BW
BM�B9XB,B(�B$�B�B�BJBB��B��B�B�B�B�mB�ZB�)B��B�qB�B��B��B��B��B�VB}�Bn�B]/BJ�B0!B�BB
�NB
��B
�FB
��B
�{B
�7B
}�B
p�B
e`B
dZB
dZB
cTB
`BB
W
B
O�B
N�B
O�B
E�B
?}B
/B
�B
�B
\B	��B	�BB	ǮB	��B	�\B	�JB	�{B	��B	�+B	�B	w�B	m�B	_;B	YB	O�B	E�B	,B	{B	�B	uB	�B	�B	�B	{B	oB	hB	hB	bB	\B	JB	B��B��B��B��B�B�B�mB�TB�/B�B�B�B��B��B��B��BɺBƨBÖB�wB�FB�B��B��B��B��B��B��B��B��B��B�{B�oB�bB�VB�JB�7B�%B�%B�B�B�B�%B�%B�%B�%B�%B�B�B�B�B�B|�B{�B{�B{�Bz�By�Bx�Bv�Bt�Bw�Bx�Bx�Bs�Bu�Bs�Br�By�Bv�Bu�Bs�Bn�BffBdZBdZBdZBhsBjBiyBiyBhsBiyBffBdZBdZBcTBcTBe`Be`BgmBffBgmBhsBjBm�Bo�Bo�Br�Br�Bs�Bt�Bt�Bs�Br�Bv�By�Bv�Bq�Bp�Bp�Bq�Br�Bv�B{�B|�B�B�B�7B�=B�=B�JB�VB07LB�wBĜB��BŢB��B��B�B�B�
B��B��B��B�B�yB�B�B�`B�5B�5B�BB�ZB&�B	@�B	A�B	C�B	B�B	I�B	L�B	M�B	N�B	O�B	Q�B	S�B	T�B	VB	XB	XB	ZB	[#B	[#B	\)B	_;B	aHB	ffB	hsB	iyB	iyB	iyB	iyB	k�B	o�B	p�B	r�B	r�B	t�B	w�B	{�B	�B	�B	�B	�B	�B	�B	�%B	�+B	�1B	�7B	�=B	�PB	�VB	�\B	�\B	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�!B	�-B	�3B	�9B	�9B	�?B	�RB	�jB	�qB	��B	B	ĜB	ƨB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�B	�B	�B	�B	�#B	�/B	�5B	�;B	�BB	�BB	�HB	�HB	�NB	�NB	�NB	�ZB	�`B	�fB	�fB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�xB
mB
.�222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222422222222222222222222242222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222   B�B�B�B�B�B�B�B�B�B�B��B��B��B��B��B	7B\B�B�B �B!�B"�B$�B$�B$�B$�B'�B,B6FB@�BŢB�}B�wB�dB�'B��B��B��B�VB�Bu�Bk�BbNB^5B[#BW
BM�B9XB,B(�B$�B�B�BJBB��B��B�B�B�B�mB�ZB�)B��B�qB�B��B��B��B��B�VB}�Bn�B]/BJ�B0!B�BB
�NB
��B
�FB
��B
�{B
�7B
}�B
p�B
e`B
dZB
dZB
cTB
`BB
W
B
O�B
N�B
O�B
E�B
?}B
/B
�B
�B
\B	��B	�BB	ǮB	��B	�\B	�JB	�{B	��B	�+B	�B	w�B	m�B	_;B	YB	O�B	E�B	,B	{B	�B	uB	�B	�B	�B	{B	oB	hB	hB	bB	\B	JB	B��B��B��B��B�B�B�mB�TB�/B�B�B�B��B��B��B��BɺBƨBÖB�wB�FB�B��B��B��B��B��B��B��B��B��B�{B�oB�bB�VB�JB�7B�%B�%B�B�B�B�%B�%B�%B�%B�%B�B�B�B�B�B|�B{�B{�B{�Bz�By�Bx�Bv�Bt�Bw�Bx�Bx�Bs�Bu�Bs�Br�By�Bv�Bu�Bs�Bn�BffBdZBdZBdZBhsBjBiyBiyBhsBiyBffBdZBdZBcTBcTBe`Be`BgmBffBgmBhsBjBm�Bo�Bo�Br�Br�Bs�Bt�Bt�Bs�Br�Bv�By�Bv�Bq�Bp�Bp�Bq�Br�Bv�B{�B|�B�B�B�7B�=B�=B�JB�VB07LB�wBĜB��BŢB��B��B�B�B�
B��B��B��B�B�yB�B�B�`B�5B�5B�BB�ZB&�B	@�B	A�B	C�B	B�B	I�B	L�B	M�B	N�B	O�B	Q�B	S�B	T�B	VB	XB	XB	ZB	[#B	[#B	\)B	_;B	aHB	ffB	hsB	iyB	iyB	iyB	iyB	k�B	o�B	p�B	r�B	r�B	t�B	w�B	{�B	�B	�B	�B	�B	�B	�B	�%B	�+B	�1B	�7B	�=B	�PB	�VB	�\B	�\B	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�!B	�-B	�3B	�9B	�9B	�?B	�RB	�jB	�qB	��B	B	ĜB	ƨB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�B	�B	�B	�B	�#B	�/B	�5B	�;B	�BB	�BB	�HB	�HB	�NB	�NB	�NB	�ZB	�`B	�fB	�fB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�xB
mB
.�222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222422222222222222222222242222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.02 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005191750                              AO  ARCAADJP                                                                    20181005191750    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005191750  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005191750  QCF$                G�O�G�O�G�O�8000            