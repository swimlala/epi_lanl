CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:17:43Z creation      
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
resolution        =���   axis      Z        h  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  @�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     h  B�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     h  K�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     h  S`   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  Z�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     h  \�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  d   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     h  e�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     h  mP   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  t�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     h  v�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  }�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     h  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �@   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �p   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �p   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �p   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �p   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �    HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20181005191743  20181005191743  5904954 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               �A   AO  6557                            2B  A   APEX                            7468                            062512                          846 @��dǮ&�1   @��e[�@4�I�^5�d��S��1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      �A   A   A   @�33@���@���A   A@  A`  A�  A�33A�  A�  A�  A�  A�  A�  B   B  BffB  B   B(ffB0  B8  B@  BH  BO��BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B���B�  B�  C   C  C  C  C�C
  C  C  C  C  C  C  C  C  C  C�C �C"�C$  C&  C(�C*�C,  C.�C0  C2  C4  C6  C8  C:  C<  C>  C?�fCB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV�CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�fC�  C�  C�  C�  C��C�  C�  C��3C�  C�  C�  C�  C�  C��3C��C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C��3C�  C��C��C��C�  C��C�  C�  C��C��C�  C��3C��3C�  C��C�  C�  C��C�  C�  C�  C�  C��C��C��C�  C��3C�  C�  C��3C�  C��C�  C�  C��C��C��C��C�  C��3C��3C�  C�  C��C��C�  C��C�  C��C��3C�  C��3C��3C�  C�  C�  C��C��C�  C��3C�  C��3C�  C��C��C�  C��3C�  C�  C�  C��3D y�D�D� D  D� D  D� DfD�fD  D� D��D�fD  D� D��D�fD	fD	�fD	��D
y�DfDy�DfD� D�D�fD  Dy�D  D� D  D� D  Dy�D  D� D  D� D  D�fD�Dy�DfD� D  D� D��D� D��Dy�DfD� D  D� DfD�fD  D� DfD�fDfDy�D   D � D!fD!� D"  D"� D"��D#�fD$  D$s3D%  D%�fD&  D&y�D'  D'� D'��D(� D)  D)y�D)��D*�fD+fD+� D,fD,y�D-fD-�fD.  D.� D/fD/� D0  D0� D1  D1� D2  D2�fD3  D3� D4  D4� D5  D5� D6  D6�fD7  D7� D7��D8� D9�D9�fD:  D:y�D;fD;��D;��D<� D=fD=� D>fD>y�D?  D?�fD@  D@s3DA  DA� DBfDB� DC  DCy�DD  DD�fDEfDE� DF  DF� DGfDG� DHfDH�fDI  DI� DJfDJ� DK  DKy�DLfDL� DMfDM� DN  DN� DN��DOy�DP  DP�fDQfDQ� DR  DRs3DR��DS�fDS��DT� DU  DU�fDVfDV�fDWfDW� DX  DXy�DY  DYy�DZfDZ�fD[  D[�fD\fD\�fD\��D]�fD]��D^y�D^��D_� D`  D`� D`��Da� DbfDb� DcfDc�fDd  Dd� De  De� Df  Dfy�Dg  Dg� DhfDh� Di  Di� DjfDj� Dk  Dky�Dk��Dl� DmfDm�fDm��Dny�Do  Do� Do��Dp� Dp��Dq�fDr  Dr� Ds  Ds� DtfDty�Du  Du� Dv  Dv� Dw  Dwy�Dyn�D�)D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��A�\A$(�AD(�Ad(�A�{A�G�A�{A�{A�{A�{A�{A�{B
=B	
=Bp�B
=B!
=B)p�B1
=B9
=BA
=BI
=BP��BY
=Ba
=Bi
=Bq
=By
=B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�Q�B��BĸRBȅB̅BЅBԅB؅B܅B��B�B�B�B�Q�B�Q�B��B��C B�CB�CB�CB�C\)C
B�CB�CB�CB�CB�CB�CB�CB�CB�CB�C\)C \)C"\)C$B�C&B�C(\)C*\)C,B�C.\)C0B�C2B�C4B�C6B�C8B�C:B�C<B�C>B�C@(�CBB�CDB�CFB�CHB�CJB�CLB�CNB�CPB�CRB�CTB�CV\)CXB�CZB�C\B�C^B�C`B�CbB�CdB�CfB�ChB�CjB�ClB�CnB�CpB�CrB�CtB�CvB�CxB�CzB�C|B�C~B�C�{C�!HC�!HC�!HC�!HC�.C�!HC�!HC�{C�!HC�!HC�!HC�!HC�!HC�{C�.C�!HC�!HC�!HC�!HC�!HC�!HC�{C�{C�!HC�!HC�!HC�!HC�!HC�!HC�!HC�{C�!HC�!HC�{C�!HC�.C�.C�.C�!HC�.C�!HC�!HC�.C�.C�!HC�{C�{C�!HC�.C�!HC�!HC�.C�!HC�!HC�!HC�!HC�.C�.C�.C�!HC�{C�!HC�!HC�{C�!HC�.C�!HC�!HC�.C�.C�.C�.C�!HC�{C�{C�!HC�!HC�.C�.C�!HC�:�C�!HC�.C�{C�!HC�{C�{C�!HC�!HC�!HC�.C�.C�!HC�{C�!HC�{C�!HC�.C�:�C�!HC�{C�!HC�!HC�!HD 
=D �>DqD��D�D��D�D��D
D�
D�D��D
>D�
D�D��D
>D�
D	
D	�
D

>D
�>D
D�>D
D��DqD�
D�D�>D�D��D�D��D�D�>D�D��D�D��D�D�
DqD�>D
D��D�D��D
>D��D
>D�>D
D��D�D��D
D�
D�D��D
D�
D
D�>D �D ��D!
D!��D"�D"��D#
>D#�
D$�D$��D%�D%�
D&�D&�>D'�D'��D(
>D(��D)�D)�>D*
>D*�
D+
D+��D,
D,�>D-
D-�
D.�D.��D/
D/��D0�D0��D1�D1��D2�D2�
D3�D3��D4�D4��D5�D5��D6�D6�
D7�D7��D8
>D8��D9qD9�
D:�D:�>D;
D;�qD<
>D<��D=
D=��D>
D>�>D?�D?�
D@�D@��DA�DA��DB
DB��DC�DC�>DD�DD�
DE
DE��DF�DF��DG
DG��DH
DH�
DI�DI��DJ
DJ��DK�DK�>DL
DL��DM
DM��DN�DN��DO
>DO�>DP�DP�
DQ
DQ��DR�DR��DS
>DS�
DT
>DT��DU�DU�
DV
DV�
DW
DW��DX�DX�>DY�DY�>DZ
DZ�
D[�D[�
D\
D\�
D]
>D]�
D^
>D^�>D_
>D_��D`�D`��Da
>Da��Db
Db��Dc
Dc�
Dd�Dd��De�De��Df�Df�>Dg�Dg��Dh
Dh��Di�Di��Dj
Dj��Dk�Dk�>Dl
>Dl��Dm
Dm�
Dn
>Dn�>Do�Do��Dp
>Dp��Dq
>Dq�
Dr�Dr��Ds�Ds��Dt
Dt�>Du�Du��Dv�Dv��Dw�Dw�>Dy\D�${D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�AŲ-AŶFA�ĜA�ĜA�ƨA�ƨA�ƨA�ƨA�ƨA�ȴA�ȴA�ȴA���A���A���A���A���A���A���A���A���A���A��
A��#A��#A��
A���AŴ9Aŝ�AŃA�hsA�S�A�&�AļjA�VA�"�A���AÙ�A�n�A�I�A�?}A�M�A�A�A�1'A�n�A���A���A��wA�jA��PA�  A��hA��A�A�A���A��A��A��A�ZA�O�A�  A�ĜA�G�A��#A�+A�VA�$�A�r�A�1A��wA��A�ZA��A���A��`A��uA��!A��A��A��+A���A���A�?}A��7A�M�A��A��A���A�7LA���A��A�  A��`A���A��A���A�S�A��A~�!A{x�AzZAxAw�7Av�+AvAs��Aq|�Apz�Ao;dAm��Ak�Aj��AidZAf�`Ae�-Ac"�A`�A^VA\��A[p�AZ�RAYAX1AV��AU��AU7LAT�\AR  AO��AO/AM\)AK�wAI�AH�DAG�^AFA�ADbNAB�ABz�ABM�A@��A>�DA=�FA"��A!�^A �A��A��A9XA�Ax�A��AAS�A�!A%A�-A&�A��A9XA�AK�A��Av�A5?A`BA��AXA�;A�/AJAdZA�A��A{A
��A	��A�A9XA?}A�RA  A\)A%A=qA��A�`AhsA �@�O�@�z�@�@��\@�=q@�5?@���@�?}@�l�@���@�P@�~�@�7@���@��@� �@�l�@�M�@�;d@��@�hs@���@�\)@�X@�ƨ@�+@�C�@�33@��@�v�@�X@�  @��@�l�@ڸR@ڗ�@١�@�{@��@�C�@���@��@�
=@͙�@�`B@̬@�@�C�@�E�@ũ�@�V@ċD@��@��H@�J@���@��;@�"�@���@�~�@�-@��@���@�`B@�j@�33@�E�@�=q@�5?@�@��@��u@�Q�@��@�ƨ@���@�+@�M�@��h@��/@�bN@�1@��
@���@��m@�\)@��y@�ȴ@���@��+@���@�~�@�v�@�5?@�J@�@�J@�J@���@�5?@�E�@�{@�V@��@�(�@��;@���@�|�@�S�@�+@���@��H@��+@��@�hs@��@�Ĝ@��@�I�@��m@��@�V@���@�X@�7L@���@�ƨ@�I�@�1'@�ƨ@���@�dZ@�
=@�ȴ@��!@�~�@�n�@�E�@�{@���@��/@�z�@�(�@�ƨ@���@�^5@�$�@�X@�/@��@��@�ȴ@�-@�J@�@��@��F@���@���@��@��H@��H@��R@�n�@�{@��@�@��h@�j@��@�|�@�K�@��H@��y@��R@�n�@��#@��@�O�@�X@��@��D@�I�@� �@��w@���@�|�@�t�@�C�@�
=@��@��@�33@�;d@�;d@�"�@�
=@��!@�M�@�=q@��@�x�@�p�@�`B@�?}@���@��j@��u@�bN@��m@���@��@���@���@�|�@�+@�33@���@�{@��h@�hs@�?}@��@���@��j@�z�@��@��@��D@�(�@�\)@�@��H@��@�5?@���@���@���@��+@�=q@�J@���@���@���@�G�@�&�@��j@�Z@�I�@� �@��@��w@��@�l�@�\)@�
=@�V@�-@�-@��@���@���@��@�@��@���@��@�`B@�7L@���@��@���@�z�@�  @���@��
@��F@��@�dZ@�K�@�@��R@���@t��@b��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  AŲ-AŶFA�ĜA�ĜA�ƨA�ƨA�ƨA�ƨA�ƨA�ȴA�ȴA�ȴA���A���A���A���A���A���A���A���A���A���A��
A��#A��#A��
A���AŴ9Aŝ�AŃA�hsA�S�A�&�AļjA�VA�"�A���AÙ�A�n�A�I�A�?}A�M�A�A�A�1'A�n�A���A���A��wA�jA��PA�  A��hA��A�A�A���A��A��A��A�ZA�O�A�  A�ĜA�G�A��#A�+A�VA�$�A�r�A�1A��wA��A�ZA��A���A��`A��uA��!A��A��A��+A���A���A�?}A��7A�M�A��A��A���A�7LA���A��A�  A��`A���A��A���A�S�A��A~�!A{x�AzZAxAw�7Av�+AvAs��Aq|�Apz�Ao;dAm��Ak�Aj��AidZAf�`Ae�-Ac"�A`�A^VA\��A[p�AZ�RAYAX1AV��AU��AU7LAT�\AR  AO��AO/AM\)AK�wAI�AH�DAG�^AFA�ADbNAB�ABz�ABM�A@��A>�DA=�FA"��A!�^A �A��A��A9XA�Ax�A��AAS�A�!A%A�-A&�A��A9XA�AK�A��Av�A5?A`BA��AXA�;A�/AJAdZA�A��A{A
��A	��A�A9XA?}A�RA  A\)A%A=qA��A�`AhsA �@�O�@�z�@�@��\@�=q@�5?@���@�?}@�l�@���@�P@�~�@�7@���@��@� �@�l�@�M�@�;d@��@�hs@���@�\)@�X@�ƨ@�+@�C�@�33@��@�v�@�X@�  @��@�l�@ڸR@ڗ�@١�@�{@��@�C�@���@��@�
=@͙�@�`B@̬@�@�C�@�E�@ũ�@�V@ċD@��@��H@�J@���@��;@�"�@���@�~�@�-@��@���@�`B@�j@�33@�E�@�=q@�5?@�@��@��u@�Q�@��@�ƨ@���@�+@�M�@��h@��/@�bN@�1@��
@���@��m@�\)@��y@�ȴ@���@��+@���@�~�@�v�@�5?@�J@�@�J@�J@���@�5?@�E�@�{@�V@��@�(�@��;@���@�|�@�S�@�+@���@��H@��+@��@�hs@��@�Ĝ@��@�I�@��m@��@�V@���@�X@�7L@���@�ƨ@�I�@�1'@�ƨ@���@�dZ@�
=@�ȴ@��!@�~�@�n�@�E�@�{@���@��/@�z�@�(�@�ƨ@���@�^5@�$�@�X@�/@��@��@�ȴ@�-@�J@�@��@��F@���@���@��@��H@��H@��R@�n�@�{@��@�@��h@�j@��@�|�@�K�@��H@��y@��R@�n�@��#@��@�O�@�X@��@��D@�I�@� �@��w@���@�|�@�t�@�C�@�
=@��@��@�33@�;d@�;d@�"�@�
=@��!@�M�@�=q@��@�x�@�p�@�`B@�?}@���@��j@��u@�bN@��m@���@��@���@���@�|�@�+@�33@���@�{@��h@�hs@�?}@��@���@��j@�z�@��@��@��D@�(�@�\)@�@��H@��@�5?@���@���@���@��+@�=q@�J@���@���@���@�G�@�&�@��j@�Z@�I�@� �@��@��w@��@�l�@�\)@�
=@�V@�-@�-@��@���@���@��@�@��@���@��@�`B@�7L@���@��@���@�z�@�  @���@��
@��F@��@�dZ@�K�@�@��R@���@t��@b��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B$�B'�B+B.B0!B49B=qBF�BJ�BP�BS�B`BBn�Bv�B~�B�hB��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�dB�dB�LB�9B�B��B��B��B�=B�Bo�Bn�Bk�B_;BXBR�BG�B0!B"�BuBB��B�B��B�^B�'B�B��By�BP�B/B  B
�B
�fB
�B
B
�3B
��B
p�B
cTB
F�B
?}B
D�B
5?B
-B
-B
>wB
=qB
0!B
'�B
�B
uB
B	��B	�B	�BB	��B	ÖB	�B	��B	��B	�bB	�DB	�B	z�B	t�B	l�B	iyB	dZB	W
B	J�B	F�B	=qB	49B	,B	%�B	 �B	�B	hB	DB	1B	%B	  B��B��B�B�B��B��B��B��B��B��B��B��B��B��B�bB�\B�VB�VB�JB�=B�7B�1B�+B�+B�%B�B�B�B�B� B� B� B~�B}�B� B~�B� B� B�B�B~�B~�B~�B� B~�B{�Bx�Bw�By�Bx�By�By�By�By�By�Bx�Bx�Bw�Bx�By�Bz�Bz�B{�B|�B}�B~�B�B�+B�+B�+B�+B�1B�7B�=B�=B�PB�oB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B�uB�oB�bB�JB�=B�=B�DB�DB�JB�PB�\B�\B�VB�hB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�'B�FB�XB�jB�qB�wBÖBɺB��B��B��B�
B�B�)B�#B�/B�HB�ZB�B�B��B	B	B		7B	{B	�B	�B	�B	�B	�B	 �B	!�B	"�B	$�B	)�B	,B	-B	.B	/B	2-B	6FB	8RB	;dB	?}B	A�B	A�B	B�B	B�B	D�B	L�B	N�B	R�B	S�B	T�B	W
B	YB	YB	[#B	[#B	\)B	]/B	_;B	dZB	iyB	jB	jB	n�B	p�B	p�B	o�B	o�B	o�B	s�B	s�B	r�B	r�B	s�B	r�B	t�B	t�B	u�B	x�B	w�B	x�B	y�B	{�B	|�B	}�B	~�B	~�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�%B	�+B	�=B	�JB	�PB	�bB	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�-B	�3B	�3B	�3B	�3B	�?B	�FB	�LB	�RB	�jB	�wB	�}B	��B	ÖB	ƨB	ƨB	ǮB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�
B	�
B	�B	�B	�)B	�BB	�HB	�TB	�`B	�mB	�sB	�sB	�sB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
�B
�B
(X222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B$�B'�B+B.B0!B49B=qBF�BJ�BP�BS�B`BBn�Bv�B~�B�hB��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�dB�dB�LB�9B�B��B��B��B�=B�Bo�Bn�Bk�B_;BXBR�BG�B0!B"�BuBB��B�B��B�^B�'B�B��By�BP�B/B  B
�B
�fB
�B
B
�3B
��B
p�B
cTB
F�B
?}B
D�B
5?B
-B
-B
>wB
=qB
0!B
'�B
�B
uB
B	��B	�B	�BB	��B	ÖB	�B	��B	��B	�bB	�DB	�B	z�B	t�B	l�B	iyB	dZB	W
B	J�B	F�B	=qB	49B	,B	%�B	 �B	�B	hB	DB	1B	%B	  B��B��B�B�B��B��B��B��B��B��B��B��B��B��B�bB�\B�VB�VB�JB�=B�7B�1B�+B�+B�%B�B�B�B�B� B� B� B~�B}�B� B~�B� B� B�B�B~�B~�B~�B� B~�B{�Bx�Bw�By�Bx�By�By�By�By�By�Bx�Bx�Bw�Bx�By�Bz�Bz�B{�B|�B}�B~�B�B�+B�+B�+B�+B�1B�7B�=B�=B�PB�oB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B�uB�oB�bB�JB�=B�=B�DB�DB�JB�PB�\B�\B�VB�hB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�'B�FB�XB�jB�qB�wBÖBɺB��B��B��B�
B�B�)B�#B�/B�HB�ZB�B�B��B	B	B		7B	{B	�B	�B	�B	�B	�B	 �B	!�B	"�B	$�B	)�B	,B	-B	.B	/B	2-B	6FB	8RB	;dB	?}B	A�B	A�B	B�B	B�B	D�B	L�B	N�B	R�B	S�B	T�B	W
B	YB	YB	[#B	[#B	\)B	]/B	_;B	dZB	iyB	jB	jB	n�B	p�B	p�B	o�B	o�B	o�B	s�B	s�B	r�B	r�B	s�B	r�B	t�B	t�B	u�B	x�B	w�B	x�B	y�B	{�B	|�B	}�B	~�B	~�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�%B	�+B	�=B	�JB	�PB	�bB	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�-B	�3B	�3B	�3B	�3B	�?B	�FB	�LB	�RB	�jB	�wB	�}B	��B	ÖB	ƨB	ƨB	ǮB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�
B	�
B	�B	�B	�)B	�BB	�HB	�TB	�`B	�mB	�sB	�sB	�sB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
�B
�B
(X222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.26 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005191743                              AO  ARCAADJP                                                                    20181005191743    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005191743  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005191743  QCF$                G�O�G�O�G�O�8000            