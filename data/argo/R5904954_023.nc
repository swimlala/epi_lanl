CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:16:54Z creation      
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
resolution        =���   axis      Z        �  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  A8   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C,   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  T�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \x   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ^l   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  f4   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  h(   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  o�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  w�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  y�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �t   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �h   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �0   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �`   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �`   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �`   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �`   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �    HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20181005191654  20181005191654  5904954 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               A   AO  6557                            2B  A   APEX                            7468                            062512                          846 @װ����1   @װ�8㠖@4cS����c��
=p�1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      A   A   A   @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C�fC  C  C  C  C  C  C  C  C  C�fC"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CI�fCK�fCM�fCP  CR  CT�CV  CX  CZ  C\�C^  C`  Cb�Cd  Ce�fCg�fCj  Cl  Cn�Co�fCr  Ct�Cv  Cw�fCy�fC|�C~  C�  C�  C��3C��3C��3C��3C�  C��C�  C�  C�  C�  C��C��C��C��C�  C�  C�  C��3C��3C��C�  C��3C��C��C�  C�  C�  C��3C�  C��C��C�  C�  C�  C��C�  C��fC��3C�  C��C�  C��3C�  C�  C�  C�  C��3C�  C�  C�  C��3C��3C�  C�  C�  C��3C��3C�  C��C�  C��C��3C�  C��C��C��C�  C�  C��3C�  C��C��3C�  C�  C��3C��3C��fC�  C�  C�  C�  C��fC�  C��3C��3C��3C�  C��3C��C��3C�  C�  C�  C�  C�  C��3C�  C�  C��3C�  C��3C��C��3C��C�  C�  C��C�  C�  C�  C�  C�  C�  C��fC�  C�  C��C��C��C�  C��C�  C�  C�  C�  C��D   D � D  D� D��D�fD��Dy�D  D� D  D� D  D�fDfD� D�3D� D	  D	�fD
  D
� D  D� DfD�fD  D�fDfD� D��D� D  D� D  D�fDfD� D  D�fDfD� D  D�fDfD� D  D�fD  Dy�DfD�fD��Dy�DfD�fD  D� D  Dy�D��D� D  D� D fD �fD!fD!�fD"fD"� D"��D#� D#��D$y�D$��D%� D&fD&�fD'  D'� D(fD(� D)  D)� D*  D*� D*��D+� D,fD,�fD-  D-� D.fD.�fD/fD/�fD0fD0�fD1  D1y�D2  D2y�D3  D3�fD4  D4� D5  D5y�D6fD6� D7  D7� D7��D8� D9  D9� D:  D:� D;  D;� D<  D<y�D=  D=� D>  D>� D?fD?� D?�3D@y�DA  DA�fDBfDB� DB��DCy�DD  DD�fDEfDE�fDF  DF�fDGfDG� DG��DH� DH��DI� DJfDJ� DJ��DKy�DK��DL� DM  DMy�DNfDN�fDO�DO��DP  DP� DQfDQ� DQ��DR� DR��DSy�DT  DT� DT��DU�fDV�DV� DV��DW� DXfDXy�DX��DYs3DZ  DZ� DZ�3D[y�D[��D\y�D\��D]y�D^  D^�fD_fD_��D`  D`y�DafDay�DbfDb� Dc  Dc� Dd  Dd� Dd��Des3Df  Df� Df��Dg� Dh  Dh� Di  Di� Dj  Dj� Dj��Dky�Dl  Dl� Dl��Dmy�Dm��Dn� DofDo�fDp  Dpy�Dp��Dq� Dr  Dr�fDsfDs� Ds��Dt� Du  Du� Dv  Dvy�DwfDw� DwٚDy�=D�?�D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@�Q�A(�A$(�AD(�Ad(�A�{A�{A�{A�{A�{A�{A�{A�{B
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
=Ba
=Bi
=Bq
=By
=B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��BąBȅB̅BЅBԅB؅B܅B��B�B�B�B��B�B��B��C B�CB�CB�CB�CB�C
B�C(�CB�CB�CB�CB�CB�CB�CB�CB�CB�C (�C"B�C$B�C&B�C(B�C*B�C,B�C.B�C0B�C2B�C4B�C6B�C8B�C:B�C<B�C>B�C@B�CBB�CDB�CFB�CHB�CJ(�CL(�CN(�CPB�CRB�CT\)CVB�CXB�CZB�C\\)C^B�C`B�Cb\)CdB�Cf(�Ch(�CjB�ClB�Cn\)Cp(�CrB�Ct\)CvB�Cx(�Cz(�C|\)C~B�C�!HC�!HC�{C�{C�{C�{C�!HC�.C�!HC�!HC�!HC�!HC�.C�.C�.C�.C�!HC�!HC�!HC�{C�{C�.C�!HC�{C�:�C�.C�!HC�!HC�!HC�{C�!HC�.C�.C�!HC�!HC�!HC�.C�!HC��C�{C�!HC�.C�!HC�{C�!HC�!HC�!HC�!HC�{C�!HC�!HC�!HC�{C�{C�!HC�!HC�!HC�{C�{C�!HC�.C�!HC�.C�{C�!HC�:�C�.C�.C�!HC�!HC�{C�!HC�.C�{C�!HC�!HC�{C�{C��C�!HC�!HC�!HC�!HC��C�!HC�{C�{C�{C�!HC�{C�.C�{C�!HC�!HC�!HC�!HC�!HC�{C�!HC�!HC�{C�!HC�{C�.C�{C�.C�!HC�!HC�.C�!HC�!HC�!HC�!HC�!HC�!HC��C�!HC�!HC�.C�:�C�.C�!HC�.C�!HC�!HC�!HC�!HC�.D �D ��D�D��D
>D�
D
>D�>D�D��D�D��D�D�
D
D��D�D��D	�D	�
D
�D
��D�D��D
D�
D�D�
D
D��D
>D��D�D��D�D�
D
D��D�D�
D
D��D�D�
D
D��D�D�
D�D�>D
D�
D
>D�>D
D�
D�D��D�D�>D
>D��D�D��D 
D �
D!
D!�
D"
D"��D#
>D#��D$
>D$�>D%
>D%��D&
D&�
D'�D'��D(
D(��D)�D)��D*�D*��D+
>D+��D,
D,�
D-�D-��D.
D.�
D/
D/�
D0
D0�
D1�D1�>D2�D2�>D3�D3�
D4�D4��D5�D5�>D6
D6��D7�D7��D8
>D8��D9�D9��D:�D:��D;�D;��D<�D<�>D=�D=��D>�D>��D?
D?��D@�D@�>DA�DA�
DB
DB��DC
>DC�>DD�DD�
DE
DE�
DF�DF�
DG
DG��DH
>DH��DI
>DI��DJ
DJ��DK
>DK�>DL
>DL��DM�DM�>DN
DN�
DOqDO�qDP�DP��DQ
DQ��DR
>DR��DS
>DS�>DT�DT��DU
>DU�
DVqDV��DW
>DW��DX
DX�>DY
>DY��DZ�DZ��D[�D[�>D\
>D\�>D]
>D]�>D^�D^�
D_
D_�qD`�D`�>Da
Da�>Db
Db��Dc�Dc��Dd�Dd��De
>De��Df�Df��Dg
>Dg��Dh�Dh��Di�Di��Dj�Dj��Dk
>Dk�>Dl�Dl��Dm
>Dm�>Dn
>Dn��Do
Do�
Dp�Dp�>Dq
>Dq��Dr�Dr�
Ds
Ds��Dt
>Dt��Du�Du��Dv�Dv�>Dw
Dw��Dw�>Dy��D�H D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�ĜA�ĜA�ĜA�ƨA�ȴA�ƨA�ȴA���A�oA��mA�?}A�JAΉ7A͉7A�ffA�dZA�-A�(�A�"�A��A�{A�bA���A˗�A�bNA�1'A��Aʙ�A�XA�t�Aʟ�AʾwAʣ�A�5?A�S�Aȉ7A�p�A���A+A���A�1'A��A�+A�&�A��mA�ZA�A��HA��^A���A�l�A��A��A��\A�K�A�A�E�A���A���A��FA��;A�&�A��A��jA��/A���A�^5A��9A�9XA��A���A�A�A��A���A���A�\)A��A��A�r�A��#A��A��jA�ƨA�ȴA���A�JA�33A��A�$�A�C�A��/A���A���A��;A�A���A�?}A�?}A��A��FA�
=A��mA���A�+A��yA�{A~�A{x�AuG�Ar~�Aq��An �Ak�Ai�PAg�
Af�jAe�Ac%Ab�\A`�yA]��A[��AZ(�AXAU7LAPz�AN�`AMG�ALAJ�\AIdZAGK�AC�AB(�AAx�A@�RA?��A??}A;��A8�uA8Q�A8�A7
=A4�yA3�A2r�A2�A1�A1�7A0��A/��A/dZA/33A/%A.�A.-A+��A)�;A)��A)G�A)
=A(ZA'A&�A&�HA&r�A&�A%�A%"�A$v�A$�A#|�A!��A =qAG�A�yAĜA��AZA5?A-A��A\)A�AG�A�9Ax�A��AM�A|�A�hAl�A��Av�A�A1'A;dA�A�hA��A+AXA�DA$�A
r�A�DA�;A �@���@�E�AoA+A��AS�A&�A 5?@�l�@��\A A33A ^5A J@��y@�$�@�hs@�9X@�E�@�o@�@���@�@��@���@��@�+@��-@�t�@�ƨ@�M�@��@��#@�@�j@�  @���@���@��/@��@��y@�M�@�-@���@�^5@���@���@���@�@�x�@̃@�@�5?@�?}@ȼj@��@��;@���@�I�@�  @ǍP@��@��@�/@Ĵ9@��@þw@�|�@�o@�ff@��@��#@��-@���@��h@���@�
=@�M�@��^@�G�@��@�%@���@���@��@�l�@��@���@��+@��@�hs@��`@��j@��u@�z�@�Z@�(�@�b@��
@��@���@�@�x�@�V@��/@���@��@���@��D@��@�A�@�;d@�E�@�M�@�^5@�n�@�n�@�~�@�=q@��7@���@�z�@�r�@�r�@��@�E�@���@�p�@�`B@�O�@���@��@��@���@���@��w@��w@��@�l�@�\)@�33@��@���@�V@�=q@��@��^@��7@�V@��j@��
@�
=@�v�@���@��7@�Z@��F@�l�@�o@��+@�J@���@��7@�`B@��@�%@���@��`@���@��@��P@�33@��@�@��!@���@��+@�^5@�-@�{@��@��T@�p�@��@���@�Ĝ@��9@��u@�bN@�I�@�Q�@�I�@�9X@�b@�  @��@��;@��F@�C�@�
=@�@��@���@���@�~�@�^5@�E�@�5?@�@���@�x�@�%@���@��u@�bN@�9X@�1@�;d@�ȴ@��!@�^5@�@�/@��@�/@��@���@�I�@�r�@�(�@�ƨ@�;d@���@�V@��@�?}@���@��@���@��9@�z�@�I�@��@�t�@�S�@�+@�+@�+@�"�@�o@��@���@��+@�ff@�^5@�V@���@�hs@�/@���@��@�A�@���@�K�@��@���@��R@���@���@�~�@���@�@���@�@���@�X@�&�@��@�z�@�r�@�Z@�9X@�b@��w@�C�@�
=@��@�}V@{v`@kU�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�ĜA�ĜA�ĜA�ƨA�ȴA�ƨA�ȴA���A�oA��mA�?}A�JAΉ7A͉7A�ffA�dZA�-A�(�A�"�A��A�{A�bA���A˗�A�bNA�1'A��Aʙ�A�XA�t�Aʟ�AʾwAʣ�A�5?A�S�Aȉ7A�p�A���A+A���A�1'A��A�+A�&�A��mA�ZA�A��HA��^A���A�l�A��A��A��\A�K�A�A�E�A���A���A��FA��;A�&�A��A��jA��/A���A�^5A��9A�9XA��A���A�A�A��A���A���A�\)A��A��A�r�A��#A��A��jA�ƨA�ȴA���A�JA�33A��A�$�A�C�A��/A���A���A��;A�A���A�?}A�?}A��A��FA�
=A��mA���A�+A��yA�{A~�A{x�AuG�Ar~�Aq��An �Ak�Ai�PAg�
Af�jAe�Ac%Ab�\A`�yA]��A[��AZ(�AXAU7LAPz�AN�`AMG�ALAJ�\AIdZAGK�AC�AB(�AAx�A@�RA?��A??}A;��A8�uA8Q�A8�A7
=A4�yA3�A2r�A2�A1�A1�7A0��A/��A/dZA/33A/%A.�A.-A+��A)�;A)��A)G�A)
=A(ZA'A&�A&�HA&r�A&�A%�A%"�A$v�A$�A#|�A!��A =qAG�A�yAĜA��AZA5?A-A��A\)A�AG�A�9Ax�A��AM�A|�A�hAl�A��Av�A�A1'A;dA�A�hA��A+AXA�DA$�A
r�A�DA�;A �@���@�E�AoA+A��AS�A&�A 5?@�l�@��\A A33A ^5A J@��y@�$�@�hs@�9X@�E�@�o@�@���@�@��@���@��@�+@��-@�t�@�ƨ@�M�@��@��#@�@�j@�  @���@���@��/@��@��y@�M�@�-@���@�^5@���@���@���@�@�x�@̃@�@�5?@�?}@ȼj@��@��;@���@�I�@�  @ǍP@��@��@�/@Ĵ9@��@þw@�|�@�o@�ff@��@��#@��-@���@��h@���@�
=@�M�@��^@�G�@��@�%@���@���@��@�l�@��@���@��+@��@�hs@��`@��j@��u@�z�@�Z@�(�@�b@��
@��@���@�@�x�@�V@��/@���@��@���@��D@��@�A�@�;d@�E�@�M�@�^5@�n�@�n�@�~�@�=q@��7@���@�z�@�r�@�r�@��@�E�@���@�p�@�`B@�O�@���@��@��@���@���@��w@��w@��@�l�@�\)@�33@��@���@�V@�=q@��@��^@��7@�V@��j@��
@�
=@�v�@���@��7@�Z@��F@�l�@�o@��+@�J@���@��7@�`B@��@�%@���@��`@���@��@��P@�33@��@�@��!@���@��+@�^5@�-@�{@��@��T@�p�@��@���@�Ĝ@��9@��u@�bN@�I�@�Q�@�I�@�9X@�b@�  @��@��;@��F@�C�@�
=@�@��@���@���@�~�@�^5@�E�@�5?@�@���@�x�@�%@���@��u@�bN@�9X@�1@�;d@�ȴ@��!@�^5@�@�/@��@�/@��@���@�I�@�r�@�(�@�ƨ@�;d@���@�V@��@�?}@���@��@���@��9@�z�@�I�@��@�t�@�S�@�+@�+@�+@�"�@�o@��@���@��+@�ff@�^5@�V@���@�hs@�/@���@��@�A�@���@�K�@��@���@��R@���@���@�~�@���@�@���@�@���@�X@�&�@��@�z�@�r�@�Z@�9X@�b@��w@�C�@�
=@��@�}V@{v`@kU�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�B�B�B�B�B�B�B�B�B�BhBB
�B
�NB
�HB
�;B
�5B
�5B
�5B
�;B
�;B
�5B
�;B
�#B
�HB
�ZB
�`B
�B
��BDB�B(�B49B<jBR�Bt�B�PB�RB�BBJB�B�B�B"�B&�B(�B)�B+B+B)�B)�B+B+B,B?}BQ�BS�B_;BdZB~�B��B{�B\)BbNBW
BL�BN�B]/B_;BXBQ�BR�BQ�BI�B>wB�BhB
=B  B�B��B��B�9B��B�B\)BR�B>wB\B
�B
�B
��B
�jB
��B
�+B
q�B
bNB
G�B
0!B
)�B
,B
/B
49B
2-B
"�B
�B
DB	�B	�^B	�!B	��B	�bB	�+B	~�B	w�B	r�B	l�B	hsB	aHB	YB	L�B	@�B	2-B	 �B	PB	B��B��B��B�B�B�`B�TB�HB�BB�5B�#B�
B��B��B��B��B��B��BȴBǮBǮBȴB��B�B�
B�
B�B��B��B�B�#B�)B�5B�5B�;B�`B�ZB�ZB�`B�`B�ZB�TB�`B�`B�mB�B�B��B��B��B��B��B��B��B��B��B�B�B��B�wBǮB�
B�B�B�B��B��B��B��B��B	hB	�B	�B	B	B	�B	�B	VB�B��B�!B��B�?B��B��B	PB	JB��B��B�B�B��B	�B	�B	�B	oB	bB	VB	
=B	B��B�B�fB�sB�B�B�yB�B�B�yB�BB�/B�)B�#B�B��B��B��BÖB�}B�}B��BB��B�}B�qB�}BĜBŢBŢBƨBȴBƨBȴBǮBȴB��B��B��B�)B�HB�NB�NB�TB�yB�B��B��B��B��B��B��B��B	B	B	B	B	
=B	JB	VB	hB	oB	oB	uB	�B	�B	�B	�B	 �B	!�B	"�B	%�B	+B	/B	2-B	33B	5?B	7LB	8RB	9XB	=qB	?}B	@�B	A�B	D�B	F�B	F�B	F�B	G�B	G�B	G�B	F�B	F�B	L�B	N�B	P�B	VB	ZB	^5B	_;B	^5B	_;B	cTB	cTB	dZB	hsB	k�B	o�B	p�B	p�B	q�B	t�B	w�B	z�B	|�B	~�B	�B	�B	�+B	�=B	�DB	�DB	�JB	�bB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�{B	��B	�{B	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�'B	�3B	�9B	�9B	�?B	�LB	�RB	�XB	�XB	�qB	�}B	��B	��B	��B	B	ÖB	ÖB	ÖB	ÖB	ĜB	ĜB	ŢB	ŢB	ŢB	ƨB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�#B	�5B	�HB	�HB	�NB	�NB	�NB	�NB	�NB	�NB	�NB	�ZB	�mB	�yB	�yB	�yB	�sB	�sB	�mB	�yB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
B
B
B
B
B
B
-B
�B
'222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  B�B�B�B�B�B�B�B�B�B�BhBB
�B
�NB
�HB
�;B
�5B
�5B
�5B
�;B
�;B
�5B
�;B
�#B
�HB
�ZB
�`B
�B
��BDB�B(�B49B<jBR�Bt�B�PB�RB�BBJB�B�B�B"�B&�B(�B)�B+B+B)�B)�B+B+B,B?}BQ�BS�B_;BdZB~�B��B{�B\)BbNBW
BL�BN�B]/B_;BXBQ�BR�BQ�BI�B>wB�BhB
=B  B�B��B��B�9B��B�B\)BR�B>wB\B
�B
�B
��B
�jB
��B
�+B
q�B
bNB
G�B
0!B
)�B
,B
/B
49B
2-B
"�B
�B
DB	�B	�^B	�!B	��B	�bB	�+B	~�B	w�B	r�B	l�B	hsB	aHB	YB	L�B	@�B	2-B	 �B	PB	B��B��B��B�B�B�`B�TB�HB�BB�5B�#B�
B��B��B��B��B��B��BȴBǮBǮBȴB��B�B�
B�
B�B��B��B�B�#B�)B�5B�5B�;B�`B�ZB�ZB�`B�`B�ZB�TB�`B�`B�mB�B�B��B��B��B��B��B��B��B��B��B�B�B��B�wBǮB�
B�B�B�B��B��B��B��B��B	hB	�B	�B	B	B	�B	�B	VB�B��B�!B��B�?B��B��B	PB	JB��B��B�B�B��B	�B	�B	�B	oB	bB	VB	
=B	B��B�B�fB�sB�B�B�yB�B�B�yB�BB�/B�)B�#B�B��B��B��BÖB�}B�}B��BB��B�}B�qB�}BĜBŢBŢBƨBȴBƨBȴBǮBȴB��B��B��B�)B�HB�NB�NB�TB�yB�B��B��B��B��B��B��B��B	B	B	B	B	
=B	JB	VB	hB	oB	oB	uB	�B	�B	�B	�B	 �B	!�B	"�B	%�B	+B	/B	2-B	33B	5?B	7LB	8RB	9XB	=qB	?}B	@�B	A�B	D�B	F�B	F�B	F�B	G�B	G�B	G�B	F�B	F�B	L�B	N�B	P�B	VB	ZB	^5B	_;B	^5B	_;B	cTB	cTB	dZB	hsB	k�B	o�B	p�B	p�B	q�B	t�B	w�B	z�B	|�B	~�B	�B	�B	�+B	�=B	�DB	�DB	�JB	�bB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�{B	��B	�{B	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�'B	�3B	�9B	�9B	�?B	�LB	�RB	�XB	�XB	�qB	�}B	��B	��B	��B	B	ÖB	ÖB	ÖB	ÖB	ĜB	ĜB	ŢB	ŢB	ŢB	ƨB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�#B	�5B	�HB	�HB	�NB	�NB	�NB	�NB	�NB	�NB	�NB	�ZB	�mB	�yB	�yB	�yB	�sB	�sB	�mB	�yB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
B
B
B
B
B
B
-B
�B
'222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.26 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005191654                              AO  ARCAADJP                                                                    20181005191654    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005191654  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005191654  QCF$                G�O�G�O�G�O�8000            