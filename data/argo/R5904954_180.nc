CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:17:30Z creation      
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
_FillValue                 �  A<   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C0   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  T�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ^|   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  fH   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  h<   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  p   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  w�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  y�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �T   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �    HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �$   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �(   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �,   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �0Argo profile    3.1 1.2 19500101000000  20181005191730  20181005191730  5904954 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               �A   AO  6557                            2B  A   APEX                            7468                            062512                          846 @��d�Ug�1   @��e`�@6]�-V�d�=p��
1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      �A   A   A   @&ff@y��@�  A   A   A@  A`  A�  A�  A�  A�33A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C�C�C  C�fC�fC!�fC#�fC&  C(  C*  C,  C.  C/�fC2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C[�fC]�fC`  Cb  Cd  Cf  Ch�Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C��C��C��C��C��C��C�  C�  C�  C��3C��3C�  C��C��C�  C�  C��3C��3C�  C��C��C�  C�  C��C�  C�  C��C��C��C��C��C��C�  C��3C�  C��C�  C��3C�  C��3C�  C��3C��3C��C��3C��C�  C�  C�  C�  C��C�  C�  C��C�  C��3C��3C��C�  C��C�  C��C�  C��3C��C�  C��3C��3C��C��C�  C��C�  C��C��C��C��C��3C��3C�  C�  C�  C��C�  C�  C�  C��3C��3C�  C��C��3C��fC��3C��3C��3C��3C��3C��fC�  C�  C��3C��3C��3C�  C�  C��3C�  C��C�  C�  C�  C�  C�  C��3C��3C�  C��D   D � D ��D� DfD�fD  D� D  D� D  D� D��Dy�D  D� D  D�fD	  D	� D	��D
y�D  D� DfD�fDfD�fD  Dy�D��D� D  D� D  Dy�D��Dy�D  D� D  D� D  D� D  D� D  Dy�D  D��DfD�fD  D� D  Dy�D  D� D  D�fD  Dy�D  D� D   D � D ��D!� D"  D"� D#  D#� D#��D$� D%  D%� D&  D&y�D'  D'� D'��D(�fD)  D)�fD*  D*y�D+  D+� D,  D,� D-fD-�fD.  D.� D/  D/�fD0fD0� D1  D1� D1��D2� D3  D3� D4  D4� D5  D5� D5��D6� D7fD7�fD8  D8� D9  D9� D:  D:� D;  D;� D<fD<�fD=  D=�fD>  D>� D?  D?y�D?��D@y�D@�3DAy�DB  DB� DC  DCy�DC��DDy�DE  DE� DF  DF� DG  DG� DH�DH� DH��DI�fDJ  DJs3DJ�3DKs3DL  DL� DM  DM�fDN  DNy�DO  DO�fDPfDP�fDQ  DQy�DR  DR� DR��DS� DT  DTy�DU  DU� DU��DV� DW  DW� DX  DX� DY  DYy�DY��DZ� D[  D[y�D[��D\y�D]  D]� D]��D^y�D^��D_� D`fD`� Da  Day�Db  Db�fDc  Dc� DdfDdy�Dd��Dey�Df  Df� Dg  Dgy�Dg��Dhs3Di  Di�fDj  Djy�Dk  Dk� Dl  Dl� Dl��Dm� Dm��Dn� DofDo� DpfDp�fDq  Dq� DrfDr� Ds  Ds� Dt  Dty�DufDu�fDv  Dvy�DwfDw�fDx  Dy�D�E�D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @7
=@��@�Q�A(�A$(�AD(�Ad(�A�{A�{A�{A�G�A�{A�{A�{A�{B
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
=B��B��B��B��B��B��B��B��B��B��RB��RB��RB��B��B��B��B��BąBȅB̅BЅB�Q�B؅B܅B��B�B�B�B��B�B��B��C B�CB�CB�CB�CB�C
B�CB�CB�CB�CB�CB�CB�C\)C\)CB�C(�C (�C"(�C$(�C&B�C(B�C*B�C,B�C.B�C0(�C2B�C4B�C6B�C8B�C:B�C<B�C>B�C@B�CBB�CDB�CFB�CHB�CJB�CLB�CNB�CPB�CRB�CTB�CVB�CXB�CZB�C\(�C^(�C`B�CbB�CdB�CfB�Ch\)CjB�ClB�CnB�CpB�CrB�CtB�CvB�CxB�CzB�C|B�C~B�C�!HC�!HC�!HC�!HC�!HC�!HC�.C�.C�!HC�!HC�!HC�.C�.C�.C�.C�.C�.C�!HC�!HC�!HC�{C�{C�!HC�.C�.C�!HC�!HC�{C�{C�!HC�.C�.C�!HC�!HC�.C�!HC�!HC�.C�.C�.C�:�C�:�C�.C�!HC�{C�!HC�.C�!HC�{C�!HC�{C�!HC�{C�{C�.C�{C�.C�!HC�!HC�!HC�!HC�.C�!HC�!HC�.C�!HC�{C�{C�.C�!HC�.C�!HC�.C�!HC�{C�.C�!HC�{C�{C�.C�:�C�!HC�.C�!HC�.C�.C�.C�.C�{C�{C�!HC�!HC�!HC�.C�!HC�!HC�!HC�{C�{C�!HC�.C�{C��C�{C�{C�{C�{C�{C��C�!HC�!HC�{C�{C�{C�!HC�!HC�{C�!HC�.C�!HC�!HC�!HC�!HC�!HC�{C�{C�!HC�.D �D ��D
>D��D
D�
D�D��D�D��D�D��D
>D�>D�D��D�D�
D	�D	��D

>D
�>D�D��D
D�
D
D�
D�D�>D
>D��D�D��D�D�>D
>D�>D�D��D�D��D�D��D�D��D�D�>D�D�qD
D�
D�D��D�D�>D�D��D�D�
D�D�>D�D��D �D ��D!
>D!��D"�D"��D#�D#��D$
>D$��D%�D%��D&�D&�>D'�D'��D(
>D(�
D)�D)�
D*�D*�>D+�D+��D,�D,��D-
D-�
D.�D.��D/�D/�
D0
D0��D1�D1��D2
>D2��D3�D3��D4�D4��D5�D5��D6
>D6��D7
D7�
D8�D8��D9�D9��D:�D:��D;�D;��D<
D<�
D=�D=�
D>�D>��D?�D?�>D@
>D@�>DA�DA�>DB�DB��DC�DC�>DD
>DD�>DE�DE��DF�DF��DG�DG��DHqDH��DI
>DI�
DJ�DJ��DK�DK��DL�DL��DM�DM�
DN�DN�>DO�DO�
DP
DP�
DQ�DQ�>DR�DR��DS
>DS��DT�DT�>DU�DU��DV
>DV��DW�DW��DX�DX��DY�DY�>DZ
>DZ��D[�D[�>D\
>D\�>D]�D]��D^
>D^�>D_
>D_��D`
D`��Da�Da�>Db�Db�
Dc�Dc��Dd
Dd�>De
>De�>Df�Df��Dg�Dg�>Dh
>Dh��Di�Di�
Dj�Dj�>Dk�Dk��Dl�Dl��Dm
>Dm��Dn
>Dn��Do
Do��Dp
Dp�
Dq�Dq��Dr
Dr��Ds�Ds��Dt�Dt�>Du
Du�
Dv�Dv�>Dw
Dw�
Dx�DyθD�ND��H1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�Q�A�O�A�I�A�O�A�O�A�^5A�\)A�VA�dZA�XA�XA�`BA�bNA�bNA�bNA�bNA�dZA�hsA�jA�hsA�hsA�l�A�l�A�jA�ffA�bNA�VA�VA�I�A�-A��A��A���A���A�ƨA�ĜA�ƨA�ƨA�ȴA�ȴA�ȴA�ĜA�ĜA�A���A���AξwAμjAκ^Aδ9AήAάAΩ�AάAδ9Aκ^Aβ-AΣ�AΣ�AΧ�A�jA���A̝�A˟�A�9XAʮAɍPA�G�A�^5Aº^A��DA�&�A�O�A�t�A���A�33A�x�A�dZA�O�A�ƨA��A��A�\)A��^A�|�A��HA�O�A�9XA���A��mA�{A�ffA�`BA�$�A��A�5?A�`BA��A��A��FA��A��9A��A���A�I�A�-A��A��FA�x�A�bNA��A��A��`A��+A�(�A��A���A���A�A�A��7A�r�A��7A���A��FA��RA��A��A��^A��A�^5A��A}�Ay�TAxffAwS�Au�At$�ArQ�ApA�Anz�Al�Aj�Ai%Ah�AgK�Af~�Aex�AdjAax�A^�jA]�A\�yA[�mAY�^AWS�AV{AT��AS��AR�DAQ�^AP��AP=qAN�AN �AM��AM`BAK��AJE�AIO�AHE�AF��AC��AA�FA@�9A@A>�`A=��A=�7A=XA;�TA:�A8�A7C�A6JA5t�A4�`A4ZA41'A2��A1oA.n�A+�#A*^5A)�#A)�A(v�A'�mA'XA&��A&$�A$�HA$5?A"�`A!��A v�A1'A�HA-A��AƨA�hAdZA33AVA�A�\A�A�FAG�A�TA7LA�\A=qA�
A�PAXA��A��A�`A��AVA	"�A��A�A�A��AVA�A\)A��AjAA�A$�A��A?}@��w@�5?@���@�1@��@�v�@��@��@���@�M�@�^5@�~�@�v�@�~�@�~�@�@��@��@�`B@�G�@�x�@�1@�K�@�@�;d@�5?@�-@��@�@旍@�w@���@�E�@ڰ!@�Ĝ@١�@ם�@�l�@�S�@���@պ^@Ӿw@��@Ѓ@Χ�@� �@˕�@��@�dZ@�33@ʟ�@�ff@�V@ə�@���@�j@��@�%@�dZ@�
=@��H@�n�@�E�@���@��/@�o@���@�n�@�V@�1'@�1@���@��@��F@�?}@��j@� �@�
=@���@��m@�\)@�~�@���@���@�x�@���@�5?@�ff@�5?@�E�@�V@��+@�V@�x�@���@�z�@��@��@�`B@��@��/@��u@��u@�j@���@�r�@���@��R@���@�$�@���@�7L@���@�I�@��@�+@�@���@�v�@���@��-@��@��/@���@��j@���@��D@�r�@�Z@� �@�ƨ@���@��
@���@���@� �@��F@�;d@�V@���@��@�%@�V@��@���@�j@�I�@�b@���@���@�|�@�l�@�33@�ȴ@�v�@���@��@�/@�%@��9@��u@�j@�I�@�9X@�(�@���@�+@�"�@��y@�@�|�@�A�@�j@�I�@���@�ƨ@�K�@�$�@���@��`@���@�r�@���@���@��P@��@�K�@���@�1'@���@�ff@��+@��#@���@�p�@�hs@�O�@���@��@�Ĝ@���@���@��@��9@�Z@���@��F@�K�@��@�v�@�M�@�M�@��+@�M�@�@�J@��T@�&�@��@��@�7L@�?}@�?}@�/@��@�j@�(�@��
@���@���@�l�@��@��y@���@���@�~�@�ff@�^5@�V@�E�@��@��@��@�$�@��@��^@��-@��h@�p�@�O�@�O�@�S@~ں@mu�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�Q�A�O�A�I�A�O�A�O�A�^5A�\)A�VA�dZA�XA�XA�`BA�bNA�bNA�bNA�bNA�dZA�hsA�jA�hsA�hsA�l�A�l�A�jA�ffA�bNA�VA�VA�I�A�-A��A��A���A���A�ƨA�ĜA�ƨA�ƨA�ȴA�ȴA�ȴA�ĜA�ĜA�A���A���AξwAμjAκ^Aδ9AήAάAΩ�AάAδ9Aκ^Aβ-AΣ�AΣ�AΧ�A�jA���A̝�A˟�A�9XAʮAɍPA�G�A�^5Aº^A��DA�&�A�O�A�t�A���A�33A�x�A�dZA�O�A�ƨA��A��A�\)A��^A�|�A��HA�O�A�9XA���A��mA�{A�ffA�`BA�$�A��A�5?A�`BA��A��A��FA��A��9A��A���A�I�A�-A��A��FA�x�A�bNA��A��A��`A��+A�(�A��A���A���A�A�A��7A�r�A��7A���A��FA��RA��A��A��^A��A�^5A��A}�Ay�TAxffAwS�Au�At$�ArQ�ApA�Anz�Al�Aj�Ai%Ah�AgK�Af~�Aex�AdjAax�A^�jA]�A\�yA[�mAY�^AWS�AV{AT��AS��AR�DAQ�^AP��AP=qAN�AN �AM��AM`BAK��AJE�AIO�AHE�AF��AC��AA�FA@�9A@A>�`A=��A=�7A=XA;�TA:�A8�A7C�A6JA5t�A4�`A4ZA41'A2��A1oA.n�A+�#A*^5A)�#A)�A(v�A'�mA'XA&��A&$�A$�HA$5?A"�`A!��A v�A1'A�HA-A��AƨA�hAdZA33AVA�A�\A�A�FAG�A�TA7LA�\A=qA�
A�PAXA��A��A�`A��AVA	"�A��A�A�A��AVA�A\)A��AjAA�A$�A��A?}@��w@�5?@���@�1@��@�v�@��@��@���@�M�@�^5@�~�@�v�@�~�@�~�@�@��@��@�`B@�G�@�x�@�1@�K�@�@�;d@�5?@�-@��@�@旍@�w@���@�E�@ڰ!@�Ĝ@١�@ם�@�l�@�S�@���@պ^@Ӿw@��@Ѓ@Χ�@� �@˕�@��@�dZ@�33@ʟ�@�ff@�V@ə�@���@�j@��@�%@�dZ@�
=@��H@�n�@�E�@���@��/@�o@���@�n�@�V@�1'@�1@���@��@��F@�?}@��j@� �@�
=@���@��m@�\)@�~�@���@���@�x�@���@�5?@�ff@�5?@�E�@�V@��+@�V@�x�@���@�z�@��@��@�`B@��@��/@��u@��u@�j@���@�r�@���@��R@���@�$�@���@�7L@���@�I�@��@�+@�@���@�v�@���@��-@��@��/@���@��j@���@��D@�r�@�Z@� �@�ƨ@���@��
@���@���@� �@��F@�;d@�V@���@��@�%@�V@��@���@�j@�I�@�b@���@���@�|�@�l�@�33@�ȴ@�v�@���@��@�/@�%@��9@��u@�j@�I�@�9X@�(�@���@�+@�"�@��y@�@�|�@�A�@�j@�I�@���@�ƨ@�K�@�$�@���@��`@���@�r�@���@���@��P@��@�K�@���@�1'@���@�ff@��+@��#@���@�p�@�hs@�O�@���@��@�Ĝ@���@���@��@��9@�Z@���@��F@�K�@��@�v�@�M�@�M�@��+@�M�@�@�J@��T@�&�@��@��@�7L@�?}@�?}@�/@��@�j@�(�@��
@���@���@�l�@��@��y@���@���@�~�@�ff@�^5@�V@�E�@��@��@��@�$�@��@��^@��-@��h@�p�@�O�@�O�@�S@~ں@mu�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�BB�BB�BB�BB�BB�BB�BB�BB�;B�BB�BB�BB�BB�BB�BB�BB�BB�BB�BB�BB�BB�;B�HB�sB�B�B�B�B��B  B+BhB�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B �B�B�B,B9XB8RB=qBN�Bp�B�B�JB��B�uB��B��B��B��B��B��B��B��B��B��B��B��B��B��B�uB�JB�7B�B�\B�bB�DB�7B� Bu�BbNBP�BA�BhB	7BB��B��B�B�NB�
B��B��B��BŢB�XB��B��B� BM�B#�B  B
�B
�yB
�#B
��B
ǮB
�qB
�B
��B
��B
��B
��B
�{B
�7B
s�B
\)B
O�B
G�B
=qB
33B
%�B
�B

=B	��B	�B	�ZB	�/B	�B	��B	��B	��B	�B	��B	�hB	�PB	�%B	�B	t�B	k�B	cTB	]/B	^5B	[#B	XB	R�B	L�B	H�B	E�B	B�B	<jB	5?B	0!B	)�B	"�B	�B	JB	+B	B��B��B��B��B�B�sB�;B�#B�
B��B��B��B��B��BǮB�}B�XB�?B�9B�-B�!B�B�B��B��B��B��B��B��B�\B�=B�%B�B�B~�B}�B|�B|�B{�Bz�Bz�Bx�Bw�Bt�Bq�Bk�BjBiyBgmBffBcTB_;B[#BYBVBS�BP�BO�BO�BP�BR�BR�BN�BN�BM�BK�BJ�BI�BE�B>wB?}B@�B?}B@�BB�BC�BA�BB�BG�BP�BS�BT�BT�BT�BVBW
B]/BbNBcTBe`BjBp�Bq�Bq�Bo�Bm�Bl�BjBgmBdZB\)BQ�BJ�BC�B@�BK�BJ�BN�BS�B[#B[#B\)B\)B\)B]/B[#B[#B]/BgmBhsBm�Bp�Bp�Bp�Br�Bu�Bv�Bz�Bz�B~�B�+B�1B�+B�%B�B�B~�Bz�Bz�By�Bw�Bt�Bv�Bx�Bs�Bu�Bv�Bz�B}�B�B�1B�=B��B��B��B��B��B��B��B�B�B�3B�?B�LB�jBBƨBǮBɺB��B��B��B�B�TB�B�B��B��B��B	B	B	+B	1B		7B	
=B	JB	PB	oB	oB	uB	uB	�B	�B	�B	�B	%�B	'�B	+B	/B	5?B	;dB	=qB	C�B	H�B	J�B	L�B	M�B	L�B	R�B	VB	VB	VB	\)B	_;B	bNB	e`B	ffB	gmB	iyB	jB	jB	k�B	l�B	k�B	k�B	l�B	m�B	n�B	o�B	p�B	p�B	q�B	q�B	r�B	s�B	u�B	v�B	x�B	z�B	�+B	�PB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�^B	�}B	�wB	�wB	�}B	B	ĜB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�)B	�;B	�HB	�NB	�TB	�TB	�`B	�fB	�mB	�mB	�mB	�mB	�mB	�mB	�sB	�sB	�yB	�yB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B

�B
�2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 B�BB�BB�BB�BB�BB�BB�BB�BB�;B�BB�BB�BB�BB�BB�BB�BB�BB�BB�BB�BB�BB�;B�HB�sB�B�B�B�B��B  B+BhB�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B �B�B�B,B9XB8RB=qBN�Bp�B�B�JB��B�uB��B��B��B��B��B��B��B��B��B��B��B��B��B��B�uB�JB�7B�B�\B�bB�DB�7B� Bu�BbNBP�BA�BhB	7BB��B��B�B�NB�
B��B��B��BŢB�XB��B��B� BM�B#�B  B
�B
�yB
�#B
��B
ǮB
�qB
�B
��B
��B
��B
��B
�{B
�7B
s�B
\)B
O�B
G�B
=qB
33B
%�B
�B

=B	��B	�B	�ZB	�/B	�B	��B	��B	��B	�B	��B	�hB	�PB	�%B	�B	t�B	k�B	cTB	]/B	^5B	[#B	XB	R�B	L�B	H�B	E�B	B�B	<jB	5?B	0!B	)�B	"�B	�B	JB	+B	B��B��B��B��B�B�sB�;B�#B�
B��B��B��B��B��BǮB�}B�XB�?B�9B�-B�!B�B�B��B��B��B��B��B��B�\B�=B�%B�B�B~�B}�B|�B|�B{�Bz�Bz�Bx�Bw�Bt�Bq�Bk�BjBiyBgmBffBcTB_;B[#BYBVBS�BP�BO�BO�BP�BR�BR�BN�BN�BM�BK�BJ�BI�BE�B>wB?}B@�B?}B@�BB�BC�BA�BB�BG�BP�BS�BT�BT�BT�BVBW
B]/BbNBcTBe`BjBp�Bq�Bq�Bo�Bm�Bl�BjBgmBdZB\)BQ�BJ�BC�B@�BK�BJ�BN�BS�B[#B[#B\)B\)B\)B]/B[#B[#B]/BgmBhsBm�Bp�Bp�Bp�Br�Bu�Bv�Bz�Bz�B~�B�+B�1B�+B�%B�B�B~�Bz�Bz�By�Bw�Bt�Bv�Bx�Bs�Bu�Bv�Bz�B}�B�B�1B�=B��B��B��B��B��B��B��B�B�B�3B�?B�LB�jBBƨBǮBɺB��B��B��B�B�TB�B�B��B��B��B	B	B	+B	1B		7B	
=B	JB	PB	oB	oB	uB	uB	�B	�B	�B	�B	%�B	'�B	+B	/B	5?B	;dB	=qB	C�B	H�B	J�B	L�B	M�B	L�B	R�B	VB	VB	VB	\)B	_;B	bNB	e`B	ffB	gmB	iyB	jB	jB	k�B	l�B	k�B	k�B	l�B	m�B	n�B	o�B	p�B	p�B	q�B	q�B	r�B	s�B	u�B	v�B	x�B	z�B	�+B	�PB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�^B	�}B	�wB	�wB	�}B	B	ĜB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�)B	�;B	�HB	�NB	�TB	�TB	�`B	�fB	�mB	�mB	�mB	�mB	�mB	�mB	�sB	�sB	�yB	�yB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B

�B
�2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.26 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005191730                              AO  ARCAADJP                                                                    20181005191730    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005191730  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005191730  QCF$                G�O�G�O�G�O�8000            