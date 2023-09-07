CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:05:16Z creation      
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
resolution        =���   axis      Z          9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  @�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���       BD   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  IT   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���       K   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       R(   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  Y8   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       Z�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  b   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       c�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       j�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  q�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       s�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  z�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       |�   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �    HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �D   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �T   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �X   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �h   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �l   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �p   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �tArgo profile    3.1 1.2 19500101000000  20181005190516  20181005190516  5904952 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               3A   AO  6431                            2B  A   APEX                            7466                            062512                          846 @׼$��1   @׼%s���@1nz�G��c��O�;d1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      3A   A   B   @�  @�  A   A   AA��A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B��B(  B0  B8ffB@  BH  BP  BX  B_��Bh  Bp  Bx  B�  B���B���B�  B�  B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  B�  B�  B���B�  B�  B���B�  B�  C   C  C�fC  C  C
  C  C�C  C  C  C�C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C7�fC:  C<  C=�fC?�fCB  CD  CF  CH  CJ  CL  CN  CP�CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|�C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C��3C�  C��C��C�  C�  C��C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C��3C�  C�  C�  C��3C��3C��3C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  D   D � D  Dy�D��D� D  D� D  Dy�D��D� D  D� DfD� D  D� D	  D	�fD
fD
� D
��D� DfD� D��D� D  D� D  D� DfD� D  D� D  D� D  D�fD  D� D  D�fD  D� DfD�fD  D� D  D� D��Dy�D��D� D  D� D  D� D��Dy�D  D� D   D y�D ��D!y�D!��D"� D#fD#� D$  D$� D%fD%� D%��D&� D'fD'� D(  D(�fD)  D)� D*fD*� D+  D+� D,fD,� D-  D-� D.fD.� D/  D/� D0  D0� D0��D1y�D1��D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7y�D8  D8� D9  D9� D:fD:� D:��D;� D<  D<� D=  D=� D>  D>� D?  D?y�D?��D@� DA  DA�fDB  DB� DC  DC� DC��DD� DEfDE�fDFfDF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DK��DL� DM  DMy�DM��DN� DN��DOy�DO��DPy�DQ  DQ� DR  DR�fDS  DS� DT  DTy�DU  DU� DU��DV� DW  DW� DX  DX� DY  DY�fDZ  DZy�D[  D[� D\  D\� D]fD]� D^  D^� D_  D_� D`  D`�fDa  Da� Db  Dby�Db��Dc� Dd  Ddy�De  De� Df  Dfy�Dg  Dg�fDhfDh� Dh��Diy�Dj  Dj� Dk  Dky�Dk��Dl� DmfDm�fDnfDn�fDofDo� Do��Dp� Dq  Dq�fDr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dy��D�8RD�M11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@�Q�@�Q�A(�A$(�AEAd(�A�{A�{A�{A�{A�{A�{A�{A�{B
=B	
=B
=B
=B ��B)
=B1
=B9p�BA
=BI
=BQ
=BY
=B`��Bi
=Bq
=By
=B��B�Q�B�Q�B��B��B�Q�B�Q�B��B��B��B��B��B��B��B��B��B��BąBȅB̅BЅBԸRBظRB܅B��B�B�Q�B�B��B�Q�B��B��C B�CB�C(�CB�CB�C
B�CB�C\)CB�CB�CB�C\)CB�CB�CB�CB�C B�C"B�C$B�C&B�C(B�C*B�C,B�C.B�C0B�C2B�C4B�C6B�C8(�C:B�C<B�C>(�C@(�CBB�CDB�CFB�CHB�CJB�CLB�CNB�CP\)CRB�CTB�CVB�CXB�CZB�C\B�C^B�C`B�CbB�CdB�CfB�ChB�CjB�ClB�CnB�CpB�CrB�CtB�CvB�CxB�CzB�C|\)C~B�C�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�.C�!HC�!HC�!HC�!HC�{C�!HC�.C�.C�!HC�!HC�.C�!HC�!HC�.C�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�.C�.C�!HC�{C�!HC�!HC�!HC�{C�{C�{C�!HC�!HC�{C�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�{C�{C�{C�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�{C�!HC�!HC�!HC�!HC�!HD �D ��D�D�>D
>D��D�D��D�D�>D
>D��D�D��D
D��D�D��D	�D	�
D

D
��D
>D��D
D��D
>D��D�D��D�D��D
D��D�D��D�D��D�D�
D�D��D�D�
D�D��D
D�
D�D��D�D��D
>D�>D
>D��D�D��D�D��D
>D�>D�D��D �D �>D!
>D!�>D"
>D"��D#
D#��D$�D$��D%
D%��D&
>D&��D'
D'��D(�D(�
D)�D)��D*
D*��D+�D+��D,
D,��D-�D-��D.
D.��D/�D/��D0�D0��D1
>D1�>D2
>D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7�>D8�D8��D9�D9��D:
D:��D;
>D;��D<�D<��D=�D=��D>�D>��D?�D?�>D@
>D@��DA�DA�
DB�DB��DC�DC��DD
>DD��DE
DE�
DF
DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL
>DL��DM�DM�>DN
>DN��DO
>DO�>DP
>DP�>DQ�DQ��DR�DR�
DS�DS��DT�DT�>DU�DU��DV
>DV��DW�DW��DX�DX��DY�DY�
DZ�DZ�>D[�D[��D\�D\��D]
D]��D^�D^��D_�D_��D`�D`�
Da�Da��Db�Db�>Dc
>Dc��Dd�Dd�>De�De��Df�Df�>Dg�Dg�
Dh
Dh��Di
>Di�>Dj�Dj��Dk�Dk�>Dl
>Dl��Dm
Dm�
Dn
Dn�
Do
Do��Dp
>Dp��Dq�Dq�
Dr�Dr��Ds�Ds��Dt�Dt��Du�Du��Dv�Dv��Dw�Dw��Dx�Dy�)D�@�D�Uq11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Aؙ�Aؙ�Aؙ�Aؙ�Aؙ�Aؕ�Aؙ�A؏\A�x�A���A�7LA��A���A��A���A֥�AցA�jA�ffA�jA�r�A֡�A���A��/A���A՟�A�t�A���AԓuAԙ�A���A�=qA�A���A��A��/A���Aҙ�A��TA�p�A�`BA���A���A·+A�A͸RA�K�A���Ả7A���A��A�oAȶFA�ĜA��A���A��TAę�A�1'A�A�33A�/A�A���A���A��`A�+A��+A�hsA�G�A���A�v�A��uA�5?A��A�A�A��
A��mA��7A���A�=qA�ƨA�VA��`A��A�
=A�;dA�^5A�33A��`A��`A��hA�l�A�p�A���A�ƨA��\A��9A�S�A��A��A�O�A�Q�A�ffA���A���A���A�%A���A�G�A�A/A}�TA|�Aw��AtjAq�Al(�Aj��Ai�mAi
=Ah(�Ag%Ag�Af�/Ae&�A`^5A]/A[�TAX�`AVv�AT�`AR5?AO��AM�TALVAKt�AJA�AH1AD�/AB1A?l�A>-A=�FA<r�A8��A5�mA3��A133A0ffA.�A,�RA,(�A+�PA*VA)�A)��A(�A'/A&��A%XA"��A"-A!�A v�A��A	oAI�A��AK�AVAhsA~�A��A�#A�A �HA 1'@��@��@��!@���@��@��@�;d@�%@�\)@�~�@�w@旍@�+@�E�@���@׶F@�dZ@�5?@�r�@�I�@܋D@ݩ�@��@�^@��@��T@��`@�b@�;d@۶F@ٙ�@�r�@��;@�|�@�S�@�ff@�@��@�"�@�@���@�%@��@��`@�1'@�|�@�C�@�K�@�ƨ@�l�@őh@�?}@���@���@�?}@��@���@���@�V@���@���@�p�@�K�@���@��j@���@�=q@��j@��
@��@�7L@�"�@�z�@��@��F@�^5@���@�j@�$�@�p�@���@�Z@��@���@��y@���@�&�@�~�@���@���@�t�@�=q@�p�@���@���@��@��^@�n�@�~�@���@��-@���@���@��7@�x�@�X@���@��@�z�@�bN@�j@��m@��@���@��@��@�j@�z�@��9@�j@�I�@�(�@��m@���@�ƨ@���@�33@���@��y@��R@���@�V@���@�G�@��@���@�9X@�33@�@�V@��-@�&�@���@��@� �@��w@��@��m@�I�@�1'@���@�@��y@�@�Ĝ@�r�@�bN@�Z@�I�@�1@��P@�K�@�o@�
=@���@�v�@�5?@�@���@��@���@�hs@��@��j@��@���@���@�|�@�l�@��P@�j@�Ĝ@�A�@�|�@�K�@�33@�C�@��@��w@�ƨ@���@�+@��y@��@��y@�ȴ@��\@�~�@�=q@��@��h@���@��@���@��@�\)@��+@�E�@��@��^@���@��h@�x�@�O�@��@���@��/@�  @���@��\@�n�@�V@�-@�$�@�J@��@�@��7@�`B@�G�@��@�%@��`@�Ĝ@��9@���@�z�@�b@���@�K�@�C�@�C�@�33@�
=@�ȴ@��R@�~�@�-@���@�hs@�?}@��@��j@�j@�b@�t�@�-@�p�@�?}@�/@��@��/@���@�z�@�bN@�I�@���@��
@���@�b@��;@���@��@mp�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111Aؙ�Aؙ�Aؙ�Aؙ�Aؙ�Aؕ�Aؙ�A؏\A�x�A���A�7LA��A���A��A���A֥�AցA�jA�ffA�jA�r�A֡�A���A��/A���A՟�A�t�A���AԓuAԙ�A���A�=qA�A���A��A��/A���Aҙ�A��TA�p�A�`BA���A���A·+A�A͸RA�K�A���Ả7A���A��A�oAȶFA�ĜA��A���A��TAę�A�1'A�A�33A�/A�A���A���A��`A�+A��+A�hsA�G�A���A�v�A��uA�5?A��A�A�A��
A��mA��7A���A�=qA�ƨA�VA��`A��A�
=A�;dA�^5A�33A��`A��`A��hA�l�A�p�A���A�ƨA��\A��9A�S�A��A��A�O�A�Q�A�ffA���A���A���A�%A���A�G�A�A/A}�TA|�Aw��AtjAq�Al(�Aj��Ai�mAi
=Ah(�Ag%Ag�Af�/Ae&�A`^5A]/A[�TAX�`AVv�AT�`AR5?AO��AM�TALVAKt�AJA�AH1AD�/AB1A?l�A>-A=�FA<r�A8��A5�mA3��A133A0ffA.�A,�RA,(�A+�PA*VA)�A)��A(�A'/A&��A%XA"��A"-A!�A v�A��A	oAI�A��AK�AVAhsA~�A��A�#A�A �HA 1'@��@��@��!@���@��@��@�;d@�%@�\)@�~�@�w@旍@�+@�E�@���@׶F@�dZ@�5?@�r�@�I�@܋D@ݩ�@��@�^@��@��T@��`@�b@�;d@۶F@ٙ�@�r�@��;@�|�@�S�@�ff@�@��@�"�@�@���@�%@��@��`@�1'@�|�@�C�@�K�@�ƨ@�l�@őh@�?}@���@���@�?}@��@���@���@�V@���@���@�p�@�K�@���@��j@���@�=q@��j@��
@��@�7L@�"�@�z�@��@��F@�^5@���@�j@�$�@�p�@���@�Z@��@���@��y@���@�&�@�~�@���@���@�t�@�=q@�p�@���@���@��@��^@�n�@�~�@���@��-@���@���@��7@�x�@�X@���@��@�z�@�bN@�j@��m@��@���@��@��@�j@�z�@��9@�j@�I�@�(�@��m@���@�ƨ@���@�33@���@��y@��R@���@�V@���@�G�@��@���@�9X@�33@�@�V@��-@�&�@���@��@� �@��w@��@��m@�I�@�1'@���@�@��y@�@�Ĝ@�r�@�bN@�Z@�I�@�1@��P@�K�@�o@�
=@���@�v�@�5?@�@���@��@���@�hs@��@��j@��@���@���@�|�@�l�@��P@�j@�Ĝ@�A�@�|�@�K�@�33@�C�@��@��w@�ƨ@���@�+@��y@��@��y@�ȴ@��\@�~�@�=q@��@��h@���@��@���@��@�\)@��+@�E�@��@��^@���@��h@�x�@�O�@��@���@��/@�  @���@��\@�n�@�V@�-@�$�@�J@��@�@��7@�`B@�G�@��@�%@��`@�Ĝ@��9@���@�z�@�b@���@�K�@�C�@�C�@�33@�
=@�ȴ@��R@�~�@�-@���@�hs@�?}@��@��j@�j@�b@�t�@�-@�p�@�?}@�/@��@��/@���@�z�@�bN@�I�@���@��
@���@�b@��;@���@��@mp�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
7LB
8RB
8RB
8RB
8RB
8RB
8RB
9XB
:^B
D�B
K�B
N�B
O�B
O�B
P�B
R�B
VB
ZB
`BB
k�B
�B
�?B
�-B
�LB
�wB
��B
��B
�sBPB�B
=B\BoB�B �B"�B'�B)�B2-B;dB?}BO�BQ�BR�BG�BYBaHBjBgmBVBw�BgmB��B�wB��B�B��B�B�B�B,BA�BO�Bn�B�=B�\B��B��B�qB��B�wBB��B�qB�3B��B�uB�VB�B~�B}�Bz�Bv�Bm�Be`BdZB_;BXBL�B0!B\B��B�`B�BǮB��B�7B�B� Bt�BcTBYBD�B5?B�B
�fB
�B
�PB
cTB
I�B
C�B
<jB
/B
�B	��B	��B	��B	��B	�hB	�7B	�B	|�B	y�B	z�B	z�B	r�B	N�B	6FB	.B	"�B	�B	oB	+B��B�B�B�B�yB�HB�B��B��BǮBÖB�wB�LB�'B�-B�!B�B�B��B��B��B�B�B��B��B�B��B��B�3B�3B�9B�!B]P�B�B��B��B�B��B��BB�jB�^B�RB�FB�!B�B��B��B�LB�!B�-B�'B��B��Bx�B	 �B	 �B	 �B	 �B	\B�BB�TB��B	+B	PB	�B	'�B	1'B	Q�B	^5B	\)B	[#B	ZB	T�B	L�B	F�B	A�B	?}B	>wB	<jB	5?B	�B	
=B	DB	PB	JB	PB	\B	\B	oB	�B	�B	�B	%�B	+B	&�B	%�B	%�B	$�B	'�B	)�B	/B	5?B	=qB	<jB	:^B	<jB	L�B	XB	YB	YB	VB	P�B	M�B	N�B	[#B	gmB	p�B	w�B	t�B	q�B	n�B	iyB	cTB	`BB	_;B	^5B	^5B	]/B	^5B	p�B	u�B	~�B	�B	~�B	w�B	t�B	s�B	t�B	w�B	z�B	�B	�=B	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�'B	�-B	�-B	�9B	�?B	�?B	�LB	�^B	�jB	�}B	�}B	�}B	�}B	�}B	�}B	�}B	�}B	�}B	�wB	�qB	�qB	�jB	�dB	�^B	�^B	�dB	�dB	�dB	�jB	�}B	ŢB	ŢB	ĜB	ÖB	ƨB	ƨB	ƨB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�B	�B	�/B	�;B	�BB	�mB	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
%B
%B
%B
%B
%B
%B
%B
+B
+B
1B
1B
1B
1B
1B
1B
1B
	7B
	7B
	7B

=B

=B

=B

=B
	7B
1B
%B
B
B
B
B
B
B
B
B
%B
	7B
	7B
	7B
	7B
DB
JB
B
kB
,�22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222242222222222222222222224222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222B
7LB
8RB
8RB
8RB
8RB
8RB
8RB
9XB
:^B
D�B
K�B
N�B
O�B
O�B
P�B
R�B
VB
ZB
`BB
k�B
�B
�?B
�-B
�LB
�wB
��B
��B
�sBPB�B
=B\BoB�B �B"�B'�B)�B2-B;dB?}BO�BQ�BR�BG�BYBaHBjBgmBVBw�BgmB��B�wB��B�B��B�B�B�B,BA�BO�Bn�B�=B�\B��B��B�qB��B�wBB��B�qB�3B��B�uB�VB�B~�B}�Bz�Bv�Bm�Be`BdZB_;BXBL�B0!B\B��B�`B�BǮB��B�7B�B� Bt�BcTBYBD�B5?B�B
�fB
�B
�PB
cTB
I�B
C�B
<jB
/B
�B	��B	��B	��B	��B	�hB	�7B	�B	|�B	y�B	z�B	z�B	r�B	N�B	6FB	.B	"�B	�B	oB	+B��B�B�B�B�yB�HB�B��B��BǮBÖB�wB�LB�'B�-B�!B�B�B��B��B��B�B�B��B��B�B��B��B�3B�3B�9B�!B]P�B�B��B��B�B��B��BB�jB�^B�RB�FB�!B�B��B��B�LB�!B�-B�'B��B��Bx�B	 �B	 �B	 �B	 �B	\B�BB�TB��B	+B	PB	�B	'�B	1'B	Q�B	^5B	\)B	[#B	ZB	T�B	L�B	F�B	A�B	?}B	>wB	<jB	5?B	�B	
=B	DB	PB	JB	PB	\B	\B	oB	�B	�B	�B	%�B	+B	&�B	%�B	%�B	$�B	'�B	)�B	/B	5?B	=qB	<jB	:^B	<jB	L�B	XB	YB	YB	VB	P�B	M�B	N�B	[#B	gmB	p�B	w�B	t�B	q�B	n�B	iyB	cTB	`BB	_;B	^5B	^5B	]/B	^5B	p�B	u�B	~�B	�B	~�B	w�B	t�B	s�B	t�B	w�B	z�B	�B	�=B	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�'B	�-B	�-B	�9B	�?B	�?B	�LB	�^B	�jB	�}B	�}B	�}B	�}B	�}B	�}B	�}B	�}B	�}B	�wB	�qB	�qB	�jB	�dB	�^B	�^B	�dB	�dB	�dB	�jB	�}B	ŢB	ŢB	ĜB	ÖB	ƨB	ƨB	ƨB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�B	�B	�/B	�;B	�BB	�mB	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
%B
%B
%B
%B
%B
%B
%B
+B
+B
1B
1B
1B
1B
1B
1B
1B
	7B
	7B
	7B

=B

=B

=B

=B
	7B
1B
%B
B
B
B
B
B
B
B
B
%B
	7B
	7B
	7B
	7B
DB
JB
B
kB
,�22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222242222222222222222222224222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.26 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005190516                              AO  ARCAADJP                                                                    20181005190516    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005190516  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005190516  QCF$                G�O�G�O�G�O�8000            