CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-24T14:08:24Z creation      
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
_FillValue                    �0Argo profile    3.1 1.2 19500101000000  20181024140824  20181024140824  5904957 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               mA   AO  6560                            2B  A   APEX                            7471                            062512                          846 @���&�`1   @��嬐1"@4t�j~��c����+1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      mA   A   A   @,��@�  @�33A��A   A>ffA`  A�  A�  A���A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0ffB8  B@  BG��BP  BX  B`  Bh  Bp  Bx  B�33B�  B�  B�  B�  B�  B���B�  B�  B�33B�ffB���B�  B���B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B���B���B�  B�  B�33B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C �C"�C$�C&  C(  C*  C,  C.  C0�C2  C3�fC5�fC8  C:  C<  C>�C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CY�fC\  C^  C`  Ca�fCd  Cf�Ch  Cj  Cl  Cm�fCp  Cr�Ct  Cv  Cx  Cz�C|  C~  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C��C�  C�  C�  C��C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3D y�D ��Dy�D  D� D  D� D  D� D  D�fDfD� D  D� D  D� D	  D	� D
  D
� D
��D� D  D� D  Dy�D��D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D� D  D� D��D� D  D� D  D� D  D� D  D� D fD � D!  D!y�D"  D"� D#  D#�fD$fD$�fD%fD%� D&  D&� D'  D'� D(  D(� D(��D)y�D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D0��D1� D2  D2� D3  D3� D4  D4�fD5  D5� D6  D6� D7  D7� D8  D8� D8��D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DDfDD� DE  DE� DF  DF�fDGfDG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DL��DMy�DN  DN� DO  DO� DP  DP�fDQfDQ�fDR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DWy�DW��DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D_��D`� Da  Da� DbfDb�fDcfDc� Dd  Ddy�Dd��De� DffDf�fDg  Dg� Dh  Dh�fDi  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dpy�Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv�fDw  Dw� Dw��Dy|�D�1�D��)1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @=p�@�Q�@˅AA$(�AB�\Ad(�A�{A�{A��HA�{A�{A�{A�{A�{B
=B	
=B
=B
=B!
=B)
=B1p�B9
=BA
=BH��BQ
=BY
=Ba
=Bi
=Bq
=By
=B��RB��B��B��B��B��B�Q�B��B��B��RB��B�Q�B��B�Q�B��B��B�Q�BąBȅB̅BЅBԅB؅B܅B��B�Q�B�Q�B�B��B��RB��B��C B�CB�CB�CB�CB�C
B�CB�CB�CB�CB�CB�CB�CB�CB�CB�CB�C \)C"\)C$\)C&B�C(B�C*B�C,B�C.B�C0\)C2B�C4(�C6(�C8B�C:B�C<B�C>\)C@B�CBB�CDB�CFB�CHB�CJB�CLB�CNB�CPB�CRB�CTB�CVB�CXB�CZ(�C\B�C^B�C`B�Cb(�CdB�Cf\)ChB�CjB�ClB�Cn(�CpB�Cr\)CtB�CvB�CxB�Cz\)C|B�C~B�C�!HC�!HC�!HC�!HC�!HC�!HC�.C�!HC�!HC�!HC�!HC�!HC�{C�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�{C�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�.C�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�{C�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�{C�!HC�.C�!HC�!HC�!HC�.C�!HC�!HC�!HC�!HC�{C�{C�!HC�!HC�!HC�!HC�{C�{C�!HC�!HC�!HC�!HC�{C�{C�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�.C�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HD 
=D �>D
>D�>D�D��D�D��D�D��D�D�
D
D��D�D��D�D��D	�D	��D
�D
��D
>D��D�D��D�D�>D
>D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D
>D��D�D��D
>D��D�D��D�D��D�D��D�D��D 
D ��D!�D!�>D"�D"��D#�D#�
D$
D$�
D%
D%��D&�D&��D'�D'��D(�D(��D)
>D)�>D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1
>D1��D2�D2��D3�D3��D4�D4�
D5�D5��D6�D6��D7�D7��D8�D8��D9
>D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD
DD��DE�DE��DF�DF�
DG
DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM
>DM�>DN�DN��DO�DO��DP�DP�
DQ
DQ�
DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW�>DX
>DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`
>D`��Da�Da��Db
Db�
Dc
Dc��Dd�Dd�>De
>De��Df
Df�
Dg�Dg��Dh�Dh�
Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp�>Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Du�Du��Dv�Dv�
Dw�Dw��Dw�qDy�qD�:>D��{1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�z�A�|�A�7A�7A�7A�+A�DA�DA�DAᕁAᙚAᙚAᛦAᙚAᛦAᙚAᝲAᗍAᗍAᗍA�PA�A��A�\)A�O�A��A�bNA�A��
AָRA�+A��A�G�Aї�AЮA� �A�^5A��mA�\)A�Q�A�ffA���A�ƨA�S�A��AÝ�A�/A�;dA��uA��9A��A��A�XA��A���A�r�A��yA��!A���A�n�A��A�dZA���A�%A�=qA�&�A�oA��A�x�A���A�&�A��;A���A�&�A�bNA�A�p�A���A�=qA��RA���A�1A���A��FA���A�ƨA�n�A��A�ȴA�9XA���A�x�A��jA�M�A��mA�(�A�K�A��PA�ffA��AA}��Az{Au�As�Ar�Aq��An�AlM�AkAj��Ag��Ac��A_t�A_�A^ȴA^�A]"�A\{AY?}AWt�AV��AT�AS�TAR�AQhsAPANĜAMl�ALAI��AH5?AE�hAC%AA�TA@�`A@bNA?��A>��A=�;A<z�A:�`A9��A8n�A7VA61A5�hA4$�A2��A1��A17LA0��A0�!A/�#A.��A.��A-��A-��A-S�A,�9A,=qA+�hA*z�A(~�A'|�A'
=A%p�A#�A"��A" �A!�^A!+A I�A|�AhsAt�A��A��A"�A�DA�;A��A�AbA�A�^AAffAXA{An�AQ�At�AA�AA�/AjA�#A
9XA	%A�yA�FA�PA�^A��Ap�A��A��Ap�A �A �9@��^@��@�?}@��@�33@���@� �@��y@�z�@��@�dZ@��y@��T@��`@�bN@�dZ@���@��@��@�`B@��@�b@�{@�7@�@�/@��@��@�/@��`@�x�@�hs@߅@��/@��
@�S�@ڟ�@�~�@��T@؋D@�9X@ץ�@��@�E�@ա�@���@�ƨ@�~�@�Z@��H@�@�hs@�`B@�&�@ˍP@�n�@�Z@�t�@�33@��#@ũ�@���@��@�9X@�r�@��@��;@���@�l�@�l�@�33@��!@�E�@�M�@���@�bN@��m@���@�b@�z�@��`@�G�@�%@��9@�I�@��;@��@�C�@�@��\@���@��^@�hs@�?}@���@�z�@�j@�Q�@�A�@�(�@���@�
=@�ȴ@���@���@��\@��T@��7@�x�@�&�@��`@�r�@���@�;d@��@���@�@�\)@�|�@�S�@�+@�E�@�hs@�&�@�bN@�C�@���@�5?@��-@�O�@�G�@�X@�x�@�Ĝ@��
@�t�@��@��@�ȴ@�+@�
=@�M�@�x�@��`@��9@�(�@�1@�o@�ff@�{@��@��^@��`@�bN@���@�~�@���@��@�E�@�V@�7L@���@��D@�z�@�bN@�r�@��j@��;@�dZ@��@�E�@�$�@���@��@�?}@��@��j@��j@��9@���@���@�bN@��m@�t�@�K�@��H@�J@��T@��T@�hs@�7L@�%@�z�@��@�  @��@�1'@�Q�@��@��/@���@��u@�z�@�r�@�j@�r�@�bN@�9X@�b@���@��F@�S�@�"�@���@��@�7L@��`@��@�1'@�1@��m@��P@�C�@�;d@�C�@�@���@�M�@��T@���@��7@��@��`@��D@�Q�@��m@��P@�t�@�S�@�
=@��@���@�=q@���@��@��h@��^@��^@�x�@�V@��@�j@�9X@�b@��m@���@�|�@�\)@�"�@��\@�=q@��@�@���@��h@��@���@�j@�A�@�1@��m@��w@���@��F@�l�@�l�@��@��!@���@��@��y@�
=@�+@�"�@���@�S�@wU�@cj�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�z�A�|�A�7A�7A�7A�+A�DA�DA�DAᕁAᙚAᙚAᛦAᙚAᛦAᙚAᝲAᗍAᗍAᗍA�PA�A��A�\)A�O�A��A�bNA�A��
AָRA�+A��A�G�Aї�AЮA� �A�^5A��mA�\)A�Q�A�ffA���A�ƨA�S�A��AÝ�A�/A�;dA��uA��9A��A��A�XA��A���A�r�A��yA��!A���A�n�A��A�dZA���A�%A�=qA�&�A�oA��A�x�A���A�&�A��;A���A�&�A�bNA�A�p�A���A�=qA��RA���A�1A���A��FA���A�ƨA�n�A��A�ȴA�9XA���A�x�A��jA�M�A��mA�(�A�K�A��PA�ffA��AA}��Az{Au�As�Ar�Aq��An�AlM�AkAj��Ag��Ac��A_t�A_�A^ȴA^�A]"�A\{AY?}AWt�AV��AT�AS�TAR�AQhsAPANĜAMl�ALAI��AH5?AE�hAC%AA�TA@�`A@bNA?��A>��A=�;A<z�A:�`A9��A8n�A7VA61A5�hA4$�A2��A1��A17LA0��A0�!A/�#A.��A.��A-��A-��A-S�A,�9A,=qA+�hA*z�A(~�A'|�A'
=A%p�A#�A"��A" �A!�^A!+A I�A|�AhsAt�A��A��A"�A�DA�;A��A�AbA�A�^AAffAXA{An�AQ�At�AA�AA�/AjA�#A
9XA	%A�yA�FA�PA�^A��Ap�A��A��Ap�A �A �9@��^@��@�?}@��@�33@���@� �@��y@�z�@��@�dZ@��y@��T@��`@�bN@�dZ@���@��@��@�`B@��@�b@�{@�7@�@�/@��@��@�/@��`@�x�@�hs@߅@��/@��
@�S�@ڟ�@�~�@��T@؋D@�9X@ץ�@��@�E�@ա�@���@�ƨ@�~�@�Z@��H@�@�hs@�`B@�&�@ˍP@�n�@�Z@�t�@�33@��#@ũ�@���@��@�9X@�r�@��@��;@���@�l�@�l�@�33@��!@�E�@�M�@���@�bN@��m@���@�b@�z�@��`@�G�@�%@��9@�I�@��;@��@�C�@�@��\@���@��^@�hs@�?}@���@�z�@�j@�Q�@�A�@�(�@���@�
=@�ȴ@���@���@��\@��T@��7@�x�@�&�@��`@�r�@���@�;d@��@���@�@�\)@�|�@�S�@�+@�E�@�hs@�&�@�bN@�C�@���@�5?@��-@�O�@�G�@�X@�x�@�Ĝ@��
@�t�@��@��@�ȴ@�+@�
=@�M�@�x�@��`@��9@�(�@�1@�o@�ff@�{@��@��^@��`@�bN@���@�~�@���@��@�E�@�V@�7L@���@��D@�z�@�bN@�r�@��j@��;@�dZ@��@�E�@�$�@���@��@�?}@��@��j@��j@��9@���@���@�bN@��m@�t�@�K�@��H@�J@��T@��T@�hs@�7L@�%@�z�@��@�  @��@�1'@�Q�@��@��/@���@��u@�z�@�r�@�j@�r�@�bN@�9X@�b@���@��F@�S�@�"�@���@��@�7L@��`@��@�1'@�1@��m@��P@�C�@�;d@�C�@�@���@�M�@��T@���@��7@��@��`@��D@�Q�@��m@��P@�t�@�S�@�
=@��@���@�=q@���@��@��h@��^@��^@�x�@�V@��@�j@�9X@�b@��m@���@�|�@�\)@�"�@��\@�=q@��@�@���@��h@��@���@�j@�A�@�1@��m@��w@���@��F@�l�@�l�@��@��!@���@��@��y@�
=@�+@�"�@���@�S�@wU�@cj�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
�NB
�NB
�NB
�NB
�NB
�NB
�NB
�NB
�NB
�NB
�NB
�NB
�NB
�NB
�NB
�NB
�NB
�TB
�TB
�TB
�ZB
�`BBn�B��B�B�B��B�{B�1B�Bu�B�1B��B��B�B�3B�^BƨB��B�/B�fB��B\B'�B<jBI�BM�BXBP�BJ�BH�BE�B?}B49B1'B6FBL�BZB`BBe`BhsB]/B?}B(�B�B�B"�B5?B9XB;dB7LB6FB5?B1'B!�BPBB�B�BƨB�'B��B��B�JB~�Bx�BbNB;dB+B
�B
�FB
�oB
�7B
�B
y�B
_;B
H�B
8RB
1'B
+B
�B
DB	�B	�fB	�BB	��B	B	�?B	�B	��B	��B	�B	q�B	o�B	m�B	iyB	dZB	^5B	O�B	F�B	A�B	8RB	0!B	(�B	�B	�B	bB	+B��B�B�sB�)B�B��B��B�#B�B�
B��B��BĜBB��B�}B�qB�jB�^B�RB�FB�?B�?B�9B�'B�B�B��B��B��B��B��B��B��B��B��B��B�{B��B��B��B��B��B��B�uB�uB�hB�bB�hB��B��B��B��B��B��B�B��B��B��B��B�hB�\B�bB�hB�JB�=B�bB��B��B�DB�1B�+B�B�PB�oB�oB�hB�PB�DB�%B�+B�VB�7B� B~�B�B�B�+B�1B�+B�DB�PB�VB�PB�VB�bB�bB�uB��B��B��B��B��B��B��B��B��B�B�!B�'B�?B�RB��BȴBȴBÖB��BB��BǮBȴBŢB��B��B��B�B�#B�5B�;B�;B�HB�TB�ZB�`B�`B�`B�mB�mB�yB�B�B��B��B��B��B	JB	�B	�B	uB	uB	oB	hB	bB	VB	JB	JB	JB	{B	�B	�B	�B	 �B	$�B	(�B	)�B	,B	.B	0!B	0!B	49B	6FB	8RB	<jB	=qB	>wB	?}B	A�B	D�B	D�B	F�B	H�B	K�B	O�B	T�B	W
B	W
B	W
B	W
B	[#B	^5B	_;B	`BB	aHB	bNB	ffB	gmB	iyB	l�B	o�B	u�B	y�B	z�B	{�B	|�B	�B	�%B	�=B	�+B	�B	�B	�B	�JB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�!B	�3B	�FB	�XB	�dB	�dB	�^B	�RB	�FB	�3B	�!B	�!B	�!B	�B	�B	�!B	�-B	�3B	�9B	�FB	�XB	�dB	�dB	�qB	�qB	�wB	�}B	B	ÖB	ŢB	ŢB	ǮB	ȴB	��B	��B	��B	�B	�B	�
B	�B	�B	�
B	�B	�B	�B	�B	�B	�
B	�B	�B	�B	�)B	�/B	�;B	�NB	�NB	�TB	�ZB	�ZB	�ZB	�ZB	�`B	�mB	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
+B
+B
+B
1B
1B
	7B

=B

=B
DB
PB
PB
VB
oB
�B
�B
4�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B
�NB
�NB
�NB
�NB
�NB
�NB
�NB
�NB
�NB
�NB
�NB
�NB
�NB
�NB
�NB
�NB
�NB
�TB
�TB
�TB
�ZB
�`BBn�B��B�B�B��B�{B�1B�Bu�B�1B��B��B�B�3B�^BƨB��B�/B�fB��B\B'�B<jBI�BM�BXBP�BJ�BH�BE�B?}B49B1'B6FBL�BZB`BBe`BhsB]/B?}B(�B�B�B"�B5?B9XB;dB7LB6FB5?B1'B!�BPBB�B�BƨB�'B��B��B�JB~�Bx�BbNB;dB+B
�B
�FB
�oB
�7B
�B
y�B
_;B
H�B
8RB
1'B
+B
�B
DB	�B	�fB	�BB	��B	B	�?B	�B	��B	��B	�B	q�B	o�B	m�B	iyB	dZB	^5B	O�B	F�B	A�B	8RB	0!B	(�B	�B	�B	bB	+B��B�B�sB�)B�B��B��B�#B�B�
B��B��BĜBB��B�}B�qB�jB�^B�RB�FB�?B�?B�9B�'B�B�B��B��B��B��B��B��B��B��B��B��B�{B��B��B��B��B��B��B�uB�uB�hB�bB�hB��B��B��B��B��B��B�B��B��B��B��B�hB�\B�bB�hB�JB�=B�bB��B��B�DB�1B�+B�B�PB�oB�oB�hB�PB�DB�%B�+B�VB�7B� B~�B�B�B�+B�1B�+B�DB�PB�VB�PB�VB�bB�bB�uB��B��B��B��B��B��B��B��B��B�B�!B�'B�?B�RB��BȴBȴBÖB��BB��BǮBȴBŢB��B��B��B�B�#B�5B�;B�;B�HB�TB�ZB�`B�`B�`B�mB�mB�yB�B�B��B��B��B��B	JB	�B	�B	uB	uB	oB	hB	bB	VB	JB	JB	JB	{B	�B	�B	�B	 �B	$�B	(�B	)�B	,B	.B	0!B	0!B	49B	6FB	8RB	<jB	=qB	>wB	?}B	A�B	D�B	D�B	F�B	H�B	K�B	O�B	T�B	W
B	W
B	W
B	W
B	[#B	^5B	_;B	`BB	aHB	bNB	ffB	gmB	iyB	l�B	o�B	u�B	y�B	z�B	{�B	|�B	�B	�%B	�=B	�+B	�B	�B	�B	�JB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�!B	�3B	�FB	�XB	�dB	�dB	�^B	�RB	�FB	�3B	�!B	�!B	�!B	�B	�B	�!B	�-B	�3B	�9B	�FB	�XB	�dB	�dB	�qB	�qB	�wB	�}B	B	ÖB	ŢB	ŢB	ǮB	ȴB	��B	��B	��B	�B	�B	�
B	�B	�B	�
B	�B	�B	�B	�B	�B	�
B	�B	�B	�B	�)B	�/B	�;B	�NB	�NB	�TB	�ZB	�ZB	�ZB	�ZB	�`B	�mB	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
+B
+B
+B
1B
1B
	7B

=B

=B
DB
PB
PB
VB
oB
�B
�B
4�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.26 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181024140824                              AO  ARCAADJP                                                                    20181024140824    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181024140824  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181024140824  QCF$                G�O�G�O�G�O�0               