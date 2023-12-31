CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:17:51Z creation      
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
resolution        =���   axis      Z        p  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  @�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     p  B�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J,   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     p  L   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     p  Sx   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  Z�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     p  \�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  d4   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     p  f   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     p  m�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  t�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     p  v�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ~<   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     p  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �4   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �D   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �H   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �X   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �\   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �`   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �dArgo profile    3.1 1.2 19500101000000  20181005191751  20181005191751  5904954 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL              A   AO  6557                            2B  A   APEX                            7468                            062512                          846 @��Ū11   @��>���@5A$�/�d{����1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                     A   A   B   @9��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B_��Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C�C  C  C  C  C  C  C   C"  C$�C&  C'�fC*  C,  C.  C0  C2  C4  C6  C7�fC:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C��3C�  C�  C��C�  C�  C��C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C��C��C��C�  C�  C�  C��3C�  C�  C��3C��C�  C��3C�  C��C��C��C�  C�  C�  C��3C�  C��C��C�  C��3C�  C��C�  C�  C�  C�  C��C�  C�  C��C�  C��3C��C��C�  C��3C��3C��3C�  C�  C�  C��C��C�  C��C�  C�  C�  C��C��C�  C��C��C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C��3C�  C��C��C��C��C��3C��fC��3C��3D   D y�D ��D�fDfD�fD  D�fDfD�fD��D�fDfD� D��Ds3D  D�fD��D	� D
  D
� D
��Dy�D��Ds3D  D�fD  D� D  D�fD  Dy�D��D� DfDy�D  Dy�D��D� DfD� DfD�fD��D� DfDs3D��Dy�D��D� D  Dy�DfDy�D  Dy�DfDy�D��D� D fD y�D ��D!�fD"fD"�fD#fD#y�D$  D$� D%  D%�fD&  D&� D'  D'y�D(fD(� D)fD)y�D)��D*� D+fD+� D,  D,�fD-fD-�fD.  D.y�D/  D/y�D0fD0� D1fD1y�D2fD2� D3  D3y�D4fD4�fD4��D5�fD6  D6� D6��D7� D7��D8� D9fD9y�D9�3D:y�D:��D;y�D<fD<y�D=fD=� D=��D>�fD?  D?y�D?��D@� DA  DAy�DB  DB�fDB��DC� DC��DD� DE  DEy�DE��DF� DG  DG� DHfDHy�DH��DI� DJ�DJ� DKfDK� DLfDL� DM  DM� DN  DN� DOfDO� DPfDP� DQ  DQ�fDR  DRy�DS  DS� DT  DTy�DUfDUy�DVfDVy�DW  DW� DW��DX�fDYfDY� DZ�DZ�fD[  D[� D\  D\y�D]fD]�fD^fD^y�D^��D_�fD`�D`�fDafDay�DbfDb� Db��Dc�fDd  Ddy�DefDe�fDe��Dfy�Df��Dgy�Dg��Dhy�Di  Di�fDjfDj� Dj��Dk� DlfDl� Dm  Dm�fDnfDn� Dn��Doy�Dp  Dp��Dq  Dqy�Dr  Dry�Dr��Ds�fDtfDt�fDu  Du� Dv�Dv� Dv��Dw� Dw�3Dy��D�<)D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@J=q@�Q�@�Q�A(�A$(�AD(�Ad(�A�{A�{A�{A�{A�{A�{A�{A�{B
=B	
=B
=B
=B!
=B)
=B1
=B9
=BA
=BI
=BQ
=BY
=B`��Bi
=Bq
=By
=B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��BąBȅB̅BЅBԅB؅B܅B��B�B�B�B��B�B��B��C B�CB�CB�CB�CB�C
B�CB�CB�CB�C\)CB�CB�CB�CB�CB�CB�C B�C"B�C$\)C&B�C((�C*B�C,B�C.B�C0B�C2B�C4B�C6B�C8(�C:B�C<B�C>B�C@B�CBB�CDB�CFB�CHB�CJB�CLB�CNB�CPB�CRB�CTB�CVB�CXB�CZB�C\B�C^B�C`B�CbB�CdB�CfB�ChB�CjB�ClB�CnB�CpB�CrB�CtB�CvB�CxB�CzB�C|B�C~B�C�!HC�!HC�!HC�{C�!HC�!HC�.C�!HC�!HC�.C�!HC�!HC�!HC�.C�!HC�!HC�!HC�!HC�!HC�!HC�.C�.C�.C�!HC�!HC�!HC�{C�!HC�!HC�{C�.C�!HC�{C�!HC�.C�:�C�.C�!HC�!HC�!HC�{C�!HC�.C�.C�!HC�{C�!HC�.C�!HC�!HC�!HC�!HC�.C�!HC�!HC�.C�!HC�{C�.C�.C�!HC�{C�{C�{C�!HC�!HC�!HC�.C�.C�!HC�.C�!HC�!HC�!HC�.C�.C�!HC�.C�:�C�!HC�!HC�.C�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�{C�!HC�!HC�{C�!HC�.C�.C�.C�.C�{C��C�{C�{D �D �>D
>D�
D
D�
D�D�
D
D�
D
>D�
D
D��D
>D��D�D�
D	
>D	��D
�D
��D
>D�>D
>D��D�D�
D�D��D�D�
D�D�>D
>D��D
D�>D�D�>D
>D��D
D��D
D�
D
>D��D
D��D
>D�>D
>D��D�D�>D
D�>D�D�>D
D�>D
>D��D 
D �>D!
>D!�
D"
D"�
D#
D#�>D$�D$��D%�D%�
D&�D&��D'�D'�>D(
D(��D)
D)�>D*
>D*��D+
D+��D,�D,�
D-
D-�
D.�D.�>D/�D/�>D0
D0��D1
D1�>D2
D2��D3�D3�>D4
D4�
D5
>D5�
D6�D6��D7
>D7��D8
>D8��D9
D9�>D:�D:�>D;
>D;�>D<
D<�>D=
D=��D>
>D>�
D?�D?�>D@
>D@��DA�DA�>DB�DB�
DC
>DC��DD
>DD��DE�DE�>DF
>DF��DG�DG��DH
DH�>DI
>DI��DJqDJ��DK
DK��DL
DL��DM�DM��DN�DN��DO
DO��DP
DP��DQ�DQ�
DR�DR�>DS�DS��DT�DT�>DU
DU�>DV
DV�>DW�DW��DX
>DX�
DY
DY��DZqDZ�
D[�D[��D\�D\�>D]
D]�
D^
D^�>D_
>D_�
D`qD`�
Da
Da�>Db
Db��Dc
>Dc�
Dd�Dd�>De
De�
Df
>Df�>Dg
>Dg�>Dh
>Dh�>Di�Di�
Dj
Dj��Dk
>Dk��Dl
Dl��Dm�Dm�
Dn
Dn��Do
>Do�>Dp�Dp�qDq�Dq�>Dr�Dr�>Ds
>Ds�
Dt
Dt�
Du�Du��DvqDv��Dw
>Dw��Dw��Dy�>D�D{D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�G�A�C�A�G�A�/A��A��A��A���A�XA�$�A���A��/A���A�ƨA��RA��!A��A���A���A��A�VA�?}A�-A��A��A��A��A��A��A��A��A�&�A�(�A�(�A�(�A�A���A�p�A�K�A�1'A��A��#A�n�A��A���A���A�I�A�oA��mA��A�^5A�G�A�bA�x�A�bA�$�A��A���A�;dA���A���A��jA��!A��DA��yA�z�A���A�{A��TA��#A��^A��/A�\)A�/A���A�?}A��\A�bNA�jA��DA�Q�A�ĜA��!A���A�
=A��7A��A��`A��A��`A���A���A��A���A���A�(�A���A���A�JA�~�A�A���A��RA��#A�JA��!A�;dA��!A��A��/A��uA�VA&�A|jAz�DAy;dAvȴAtbNAq��AqXAq�ApȴAn�RAk�#AiC�AhAf�`Ae�;Ad-Ac;dAb�\Ab$�Aa"�A^~�A\�A[AYdZAW�AV�/AU��AT5?AR�HAQ�FAQS�AP�!AO��ANQ�AMoALĜAK�^AI�PAI33AF�RAD��AC�AB��A@�A>M�A<�HA<bNA;�A;`BA9K�A6�uA5&�A4VA0ZA�A�#A�AoA^5AhsA�RA��AC�AA�AG�A��A�!A  A��A��A�A�A�9A5?AO�A
��A
��A
-A	�mA	C�AĜA^5AƨA+AE�A�7A�A��A��AXA"�A&�AoA ȴ@��;@�7L@���@�/@�7L@��@�ƨ@���@�9X@�@�M�@�\)@�v�@��m@柾@�$�@�7L@���@��@��@�dZ@��H@�/@�1@އ+@�{@�?}@�(�@�@�  @ם�@�33@���@��@�p�@�G�@�%@�(�@�"�@���@��@У�@��
@��@̋D@�ƨ@ʗ�@��@��@�I�@���@���@��`@�I�@���@��
@å�@��H@�V@�Q�@� �@��@��;@��F@�dZ@���@�G�@�G�@�/@��u@���@���@�O�@��@�  @�l�@�+@���@�~�@�E�@��T@�`B@���@�j@��y@��@�j@��@��;@��;@�  @���@�t�@�;d@��+@�x�@���@�1@��@���@��@��@��u@�bN@��@�ƨ@���@�S�@�33@�"�@��@��R@��@��^@�G�@�&�@�7L@�?}@�G�@��j@��@���@���@���@��9@��D@�Q�@��;@�t�@���@���@�x�@�9X@�b@���@�t�@��P@��w@��@��`@���@���@�v�@�@��7@��@��9@��@���@�"�@��+@�5?@��@�o@��@�ȴ@���@���@��R@��!@��\@�=q@�j@�t�@���@�~�@�V@���@��T@�x�@�?}@��D@�\)@�33@��@�+@��y@�=q@�M�@�$�@��7@��@���@���@�Ĝ@��@��u@��9@��j@��9@��D@�b@���@��P@��P@��P@�\)@�+@�o@�o@��@���@�V@�J@�@��^@���@���@��7@�%@��j@��D@�A�@� �@�b@��
@��w@���@���@��@��@���@���@���@��!@���@��R@��\@�v�@�n�@�M�@�=q@���@��#@��-@���@��7@�hs@�X@�O�@�?}@�&�@�%@�z�@�1'@���@��w@��@�+@��R@���@���@�E�@�@��#@���@��@�V@���@��`@��j@�bN@���@��w@��@�|�@��@|~(@h(�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�G�A�C�A�G�A�/A��A��A��A���A�XA�$�A���A��/A���A�ƨA��RA��!A��A���A���A��A�VA�?}A�-A��A��A��A��A��A��A��A��A�&�A�(�A�(�A�(�A�A���A�p�A�K�A�1'A��A��#A�n�A��A���A���A�I�A�oA��mA��A�^5A�G�A�bA�x�A�bA�$�A��A���A�;dA���A���A��jA��!A��DA��yA�z�A���A�{A��TA��#A��^A��/A�\)A�/A���A�?}A��\A�bNA�jA��DA�Q�A�ĜA��!A���A�
=A��7A��A��`A��A��`A���A���A��A���A���A�(�A���A���A�JA�~�A�A���A��RA��#A�JA��!A�;dA��!A��A��/A��uA�VA&�A|jAz�DAy;dAvȴAtbNAq��AqXAq�ApȴAn�RAk�#AiC�AhAf�`Ae�;Ad-Ac;dAb�\Ab$�Aa"�A^~�A\�A[AYdZAW�AV�/AU��AT5?AR�HAQ�FAQS�AP�!AO��ANQ�AMoALĜAK�^AI�PAI33AF�RAD��AC�AB��A@�A>M�A<�HA<bNA;�A;`BA9K�A6�uA5&�A4VA0ZA�A�#A�AoA^5AhsA�RA��AC�AA�AG�A��A�!A  A��A��A�A�A�9A5?AO�A
��A
��A
-A	�mA	C�AĜA^5AƨA+AE�A�7A�A��A��AXA"�A&�AoA ȴ@��;@�7L@���@�/@�7L@��@�ƨ@���@�9X@�@�M�@�\)@�v�@��m@柾@�$�@�7L@���@��@��@�dZ@��H@�/@�1@އ+@�{@�?}@�(�@�@�  @ם�@�33@���@��@�p�@�G�@�%@�(�@�"�@���@��@У�@��
@��@̋D@�ƨ@ʗ�@��@��@�I�@���@���@��`@�I�@���@��
@å�@��H@�V@�Q�@� �@��@��;@��F@�dZ@���@�G�@�G�@�/@��u@���@���@�O�@��@�  @�l�@�+@���@�~�@�E�@��T@�`B@���@�j@��y@��@�j@��@��;@��;@�  @���@�t�@�;d@��+@�x�@���@�1@��@���@��@��@��u@�bN@��@�ƨ@���@�S�@�33@�"�@��@��R@��@��^@�G�@�&�@�7L@�?}@�G�@��j@��@���@���@���@��9@��D@�Q�@��;@�t�@���@���@�x�@�9X@�b@���@�t�@��P@��w@��@��`@���@���@�v�@�@��7@��@��9@��@���@�"�@��+@�5?@��@�o@��@�ȴ@���@���@��R@��!@��\@�=q@�j@�t�@���@�~�@�V@���@��T@�x�@�?}@��D@�\)@�33@��@�+@��y@�=q@�M�@�$�@��7@��@���@���@�Ĝ@��@��u@��9@��j@��9@��D@�b@���@��P@��P@��P@�\)@�+@�o@�o@��@���@�V@�J@�@��^@���@���@��7@�%@��j@��D@�A�@� �@�b@��
@��w@���@���@��@��@���@���@���@��!@���@��R@��\@�v�@�n�@�M�@�=q@���@��#@��-@���@��7@�hs@�X@�O�@�?}@�&�@�%@�z�@�1'@���@��w@��@�+@��R@���@���@�E�@�@��#@���@��@�V@���@��`@��j@�bN@���@��w@��@�|�@��@|~(@h(�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�BDBDBDBPBbBhB�B�B�B �B"�B#�B$�B$�B%�B&�B&�B&�B(�B+B2-B6FB:^BA�BC�BD�BE�BF�BG�BG�BH�BJ�BK�BL�BS�BZBdZBiyB�B�hB��B��B��B��B��B�B�3B�XB�^BÖB��B��BȴB��B��BɺBȴBȴB�wB�B��B��B��B��B��B�hBv�Bx�B{�B{�Bz�Bt�Bn�BdZB\)BT�BD�B8RB1'B&�B�BhB%B��B�B�B�HB�B��B��B�9B�B��B�oB{�Bl�B^5BS�BJ�B@�B;dB49B&�B�B\B
=BB
��B
�sB
��B
��B
}�B
q�B
\)B
M�B
C�B
33B
)�B
 �B
�B
�B
�B
hB	��B	�B	�ZB	�)B	�
B	��B	��B	��B	ɺB	ÖB	�?B	�B	��B	��B	�\B	�7B	�B	z�B	t�B	n�B	l�B	hsB	aHB	W
B	M�B	I�B	=qB	'�B	!�B	JB	  B��B��B�B�TB�NB�fB�sB�ZB�HB�B��B��B\O�B�DB�=B�7B�1B�+B�%B�B�B�B�B~�B|�Bz�Bx�Bu�Bt�Br�Bq�Br�Bq�Bs�Bt�Bt�Br�Bq�Bo�Bm�Bl�Bm�Bo�Bn�Bl�BiyBhsBgmBhsBk�Bm�Bn�Bl�Bm�Bo�Bl�Bo�Bt�Bs�Bq�Bl�BiyBhsBffBcTB^5B]/B_;BaHBdZBdZBe`Bk�Bt�Bx�Bz�Bz�By�Bx�By�Bz�B}�B�B�B�B�B�B�B�B�B�%B�1B�DB�VB�\B�bB��B��B��B��B��B��B��B�B�B�-B�9B�9B�9B�9B�LB�qB�}B��B��BBBĜB��B��B��B��B�
B�B�;B�mB�sB�B�B�B�B��B��B��B��B��B	B	B	\B	�B	�B	"�B	$�B	%�B	&�B	(�B	)�B	+B	-B	-B	,B	+B	,B	/B	33B	33B	49B	:^B	=qB	@�B	D�B	E�B	E�B	F�B	I�B	M�B	O�B	Q�B	R�B	S�B	T�B	W
B	XB	\)B	_;B	aHB	bNB	bNB	bNB	cTB	dZB	dZB	dZB	cTB	cTB	aHB	`BB	`BB	cTB	e`B	hsB	jB	p�B	w�B	}�B	� B	�B	�B	�B	�+B	�1B	�1B	�%B	�B	�+B	�bB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�!B	�-B	�-B	�-B	�FB	�RB	�dB	�dB	�^B	�dB	�wB	��B	��B	��B	B	ÖB	ĜB	ĜB	ŢB	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�
B	�B	�B	�B	�B	�B	�B	�B	�B	�#B	�/B	�5B	�;B	�;B	�;B	�BB	�HB	�NB	�ZB	�fB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
%B
%B
+B
�B
B
/522222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222224222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222BDBDBDBPBbBhB�B�B�B �B"�B#�B$�B$�B%�B&�B&�B&�B(�B+B2-B6FB:^BA�BC�BD�BE�BF�BG�BG�BH�BJ�BK�BL�BS�BZBdZBiyB�B�hB��B��B��B��B��B�B�3B�XB�^BÖB��B��BȴB��B��BɺBȴBȴB�wB�B��B��B��B��B��B�hBv�Bx�B{�B{�Bz�Bt�Bn�BdZB\)BT�BD�B8RB1'B&�B�BhB%B��B�B�B�HB�B��B��B�9B�B��B�oB{�Bl�B^5BS�BJ�B@�B;dB49B&�B�B\B
=BB
��B
�sB
��B
��B
}�B
q�B
\)B
M�B
C�B
33B
)�B
 �B
�B
�B
�B
hB	��B	�B	�ZB	�)B	�
B	��B	��B	��B	ɺB	ÖB	�?B	�B	��B	��B	�\B	�7B	�B	z�B	t�B	n�B	l�B	hsB	aHB	W
B	M�B	I�B	=qB	'�B	!�B	JB	  B��B��B�B�TB�NB�fB�sB�ZB�HB�B��B��B\O�B�DB�=B�7B�1B�+B�%B�B�B�B�B~�B|�Bz�Bx�Bu�Bt�Br�Bq�Br�Bq�Bs�Bt�Bt�Br�Bq�Bo�Bm�Bl�Bm�Bo�Bn�Bl�BiyBhsBgmBhsBk�Bm�Bn�Bl�Bm�Bo�Bl�Bo�Bt�Bs�Bq�Bl�BiyBhsBffBcTB^5B]/B_;BaHBdZBdZBe`Bk�Bt�Bx�Bz�Bz�By�Bx�By�Bz�B}�B�B�B�B�B�B�B�B�B�%B�1B�DB�VB�\B�bB��B��B��B��B��B��B��B�B�B�-B�9B�9B�9B�9B�LB�qB�}B��B��BBBĜB��B��B��B��B�
B�B�;B�mB�sB�B�B�B�B��B��B��B��B��B	B	B	\B	�B	�B	"�B	$�B	%�B	&�B	(�B	)�B	+B	-B	-B	,B	+B	,B	/B	33B	33B	49B	:^B	=qB	@�B	D�B	E�B	E�B	F�B	I�B	M�B	O�B	Q�B	R�B	S�B	T�B	W
B	XB	\)B	_;B	aHB	bNB	bNB	bNB	cTB	dZB	dZB	dZB	cTB	cTB	aHB	`BB	`BB	cTB	e`B	hsB	jB	p�B	w�B	}�B	� B	�B	�B	�B	�+B	�1B	�1B	�%B	�B	�+B	�bB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�!B	�-B	�-B	�-B	�FB	�RB	�dB	�dB	�^B	�dB	�wB	��B	��B	��B	B	ÖB	ĜB	ĜB	ŢB	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�
B	�B	�B	�B	�B	�B	�B	�B	�B	�#B	�/B	�5B	�;B	�;B	�;B	�BB	�HB	�NB	�ZB	�fB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
%B
%B
+B
�B
B
/522222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222224222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.26 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005191751                              AO  ARCAADJP                                                                    20181005191751    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005191751  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005191751  QCF$                G�O�G�O�G�O�8000            