CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-13T22:40:10Z UW 3.1 conversion   
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  78   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7x   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8<   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8\   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~       axis      T           8`   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8h   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~            8l   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8t   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8|   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  A<   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�     �  C$   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�     �  L�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  TD   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  [�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  ]�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ed   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  gL   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  n�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  v�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  xl   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �8   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �H   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �L   HISTORY_START_PRES                    	long_name          Start pressure action applied on   units         decibar    
_FillValue        G�O�        �\   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    units         decibar    
_FillValue        G�O�        �`   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �d   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �hArgo profile    3.1 1.2 19500101000000  5901071 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               DA   AO  20111130140648  20190522121826  1727_5046_068                   2C  D   APEX                            2143                            040306                          846 @�m�+��1   @�m��b�@7Ffffff�c�E���1   GPS     Primary sampling: mixed [deeper than nominal 980dbar: discrete; nominal 980dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP�CR  CT  CV  CX  CZ�C\  C^�C`�Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DFy�DG  DG�fDH  DH� DI  DIy�DJ  DJ� DKfDK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\y�D\��D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dc��Ddy�De  De� Df  Df� Dg  Dg� Dh  Dh� Dh��Diy�Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dmy�Dn  Dn�fDo  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Dsl�Dy�f1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�  @���@���AffA>ffA^ffA~ffA�33A�33A�33A�33A�33A�33A�33A�33B��B��B��B��B'��B/��B7��B?��BG��BO��BW��B_��Bg��Bo��Bw��B��B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B�  B���B���B���B���B���B���C�fC�fC�fC�fC	�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC!�fC#�fC%�fC'�fC)�fC+�fC-�fC/�fC1�fC3�fC5�fC7�fC9�fC;�fC=�fC?�fCA�fCC�fCE�fCG�fCI�fCK�fCM�fCP  CQ�fCS�fCU�fCW�fCZ  C[�fC^  C`  Ca�fCc�fCe�fCg�fCi�fCk�fCm�fCo�fCq�fCs�fCu�fCw�fCy�fC{�fC}�fC�fC��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C�  C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��fC��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��fC��3C��3C��3C�  C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3D y�D ��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��D	y�D	��D
y�D
��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��D y�D ��D!y�D!��D"y�D"��D#y�D#��D$y�D$��D%y�D%��D&y�D&��D'y�D'��D(y�D(��D)y�D)��D*y�D*��D+y�D+��D,y�D,��D-y�D-��D.y�D.��D/y�D/��D0y�D0��D1y�D1��D2y�D2��D3y�D3��D4y�D4��D5y�D5��D6y�D6��D7y�D7��D8y�D8��D9y�D9��D:y�D:��D;y�D;��D<y�D<��D=y�D=��D>y�D>��D?y�D?��D@y�D@��DAy�DA��DBy�DB��DCy�DC��DDy�DD��DEy�DE��DFs3DF��DG� DG��DHy�DH��DIs3DI��DJy�DK  DKy�DK��DLy�DL��DMy�DM��DNy�DN��DOy�DO��DPy�DP��DQy�DQ��DRy�DR��DSy�DS��DTy�DT��DUy�DU��DVy�DV��DWy�DW��DXy�DX��DYy�DY��DZy�DZ��D[y�D[��D\s3D\�3D]y�D]��D^y�D^��D_y�D_��D`y�D`��Day�Da��Dby�Db��Dcy�Dc�3Dds3Dd��Dey�De��Dfy�Df��Dgy�Dg��Dhy�Dh�3Dis3Di��Djy�Dj��Dky�Dk��Dly�Dl��Dms3Dm��Dn� Dn��Doy�Do��Dpy�Dp��Dqy�Dq��Dry�Dr��DsffDy� 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�|�A�z�A�O�A�"�A�A��uA� �A��7A�r�A�bA���A�n�A�ZA�5?A��A���A�p�A�(�A�ƨA��A�/A�VA���A���A���A��A���A���A��\A�bNA�$�A��;A�A���A���A���A��+A�~�A�Q�A��A���A���A��+A�&�A��-A��A��A�&�A��jA���A��A��^A�~�A�XA���A��
A�ĜA�  A�A��+A�=qA�/A��+A���A���A��TA�A�A��9A�JA��^A�ffA�/A��+A���A��`A��RA���A�K�A��A��mA�C�A�
=A�dZA�A�bNA��HA�r�A�ZA��A��A���A���A�"�A�%A���A�dZA���A�jA�O�A��A��!A�&�A��A�^5A�JA��`A�r�A�%A�A�~�A�`BA�1'A�A�\)A�ƨA� �A�  A�ȴA�jA���A~1A{��AwC�Asp�ApȴAo\)An�RAl��Ai��Ag�AfjAe��Ad��AdE�Ab��Aa�;A`��A^ffA\�jA\ZA[�AZr�AX��AV��AU��ATr�AR��AQ"�APAOG�AN~�AL��AL�AJ��AIdZAHZAG�hAG�AF��AE\)AC�AA��A?��A=oA<ĜA<�9A<�DA;�A9��A8 �A7�A7oA6�A5�;A3�A2r�A2I�A2(�A1x�A0�9A09XA/XA-�A,  A+\)A*n�A)\)A'��A&�A&bA%�-A$�A$��A$5?A#K�A"��A!�FA Q�AhsA�/AJA�A��A�#AƨAffA��AS�A�A��A��A+A��A{A"�A��Az�AI�A{A��A|�A�AffA33A
v�A	��A$�AXA;dA�A�Az�A��A��A1A�AJAO�A ��A V@��H@��H@�`B@��@��@���@��^@��j@�!@�F@�{@�X@�l�@���@�bN@��y@柾@�5?@�1@���@�E�@��@�p�@� �@�E�@ݙ�@ݡ�@ݙ�@��`@��m@��H@��@�V@�$�@���@�M�@�-@��@д9@Ͼw@���@͙�@���@̃@�b@˝�@�\)@�K�@ʟ�@Ȭ@�hs@��@�|�@�v�@�`B@�&�@���@�bN@��@���@��@��u@��P@�%@���@�
=@���@�&�@�G�@��@�z�@�I�@���@�;d@�n�@��@�&�@��j@�r�@�A�@�9X@�(�@�  @��
@��w@���@�J@�M�@�M�@�ff@��@�7L@�@��T@�1'@�1'@��#@�=q@���@��+@�$�@�l�@��y@�V@��@���@�/@�hs@��^@���@��@�bN@�ƨ@��@���@�(�@���@��y@���@�Ĝ@�l�@��\@��@��h@��@���@���@�+@�o@��@�
=@��H@�n�@�=q@���@�Z@�|�@�K�@�;d@�+@��y@���@���@��\@��+@�M�@�$�@��T@�?}@��`@���@���@�Ĝ@��D@���@�+@���@��@�33@�|�@�|�@��y@�ff@�=q@��#@�X@��`@�9X@�1@��m@���@�t�@�C�@��y@�J@��h@�`B@�X@�V@���@��@��P@��@�V@��#@��7@�/@��@��@�l�@�33@�@�ȴ@�ff@�{@���@�x�@�hs@�O�@�?}@��@���@��@�Ĝ@��u@��@�j@�Q�@�1'@�b@�  @�  @���@��;@���@�\)@�"�@���@�^5@�M�@�E�@��@��@���@��7@�p�@�X@�7L@��@��j@��j@��D@�bN@�9X@�(�@�b@�1@��@��
@��@�dZ@�33@���@�ȴ@��R@��!@�`B1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�|�A�z�A�O�A�"�A�A��uA� �A��7A�r�A�bA���A�n�A�ZA�5?A��A���A�p�A�(�A�ƨA��A�/A�VA���A���A���A��A���A���A��\A�bNA�$�A��;A�A���A���A���A��+A�~�A�Q�A��A���A���A��+A�&�A��-A��A��A�&�A��jA���A��A��^A�~�A�XA���A��
A�ĜA�  A�A��+A�=qA�/A��+A���A���A��TA�A�A��9A�JA��^A�ffA�/A��+A���A��`A��RA���A�K�A��A��mA�C�A�
=A�dZA�A�bNA��HA�r�A�ZA��A��A���A���A�"�A�%A���A�dZA���A�jA�O�A��A��!A�&�A��A�^5A�JA��`A�r�A�%A�A�~�A�`BA�1'A�A�\)A�ƨA� �A�  A�ȴA�jA���A~1A{��AwC�Asp�ApȴAo\)An�RAl��Ai��Ag�AfjAe��Ad��AdE�Ab��Aa�;A`��A^ffA\�jA\ZA[�AZr�AX��AV��AU��ATr�AR��AQ"�APAOG�AN~�AL��AL�AJ��AIdZAHZAG�hAG�AF��AE\)AC�AA��A?��A=oA<ĜA<�9A<�DA;�A9��A8 �A7�A7oA6�A5�;A3�A2r�A2I�A2(�A1x�A0�9A09XA/XA-�A,  A+\)A*n�A)\)A'��A&�A&bA%�-A$�A$��A$5?A#K�A"��A!�FA Q�AhsA�/AJA�A��A�#AƨAffA��AS�A�A��A��A+A��A{A"�A��Az�AI�A{A��A|�A�AffA33A
v�A	��A$�AXA;dA�A�Az�A��A��A1A�AJAO�A ��A V@��H@��H@�`B@��@��@���@��^@��j@�!@�F@�{@�X@�l�@���@�bN@��y@柾@�5?@�1@���@�E�@��@�p�@� �@�E�@ݙ�@ݡ�@ݙ�@��`@��m@��H@��@�V@�$�@���@�M�@�-@��@д9@Ͼw@���@͙�@���@̃@�b@˝�@�\)@�K�@ʟ�@Ȭ@�hs@��@�|�@�v�@�`B@�&�@���@�bN@��@���@��@��u@��P@�%@���@�
=@���@�&�@�G�@��@�z�@�I�@���@�;d@�n�@��@�&�@��j@�r�@�A�@�9X@�(�@�  @��
@��w@���@�J@�M�@�M�@�ff@��@�7L@�@��T@�1'@�1'@��#@�=q@���@��+@�$�@�l�@��y@�V@��@���@�/@�hs@��^@���@��@�bN@�ƨ@��@���@�(�@���@��y@���@�Ĝ@�l�@��\@��@��h@��@���@���@�+@�o@��@�
=@��H@�n�@�=q@���@�Z@�|�@�K�@�;d@�+@��y@���@���@��\@��+@�M�@�$�@��T@�?}@��`@���@���@�Ĝ@��D@���@�+@���@��@�33@�|�@�|�@��y@�ff@�=q@��#@�X@��`@�9X@�1@��m@���@�t�@�C�@��y@�J@��h@�`B@�X@�V@���@��@��P@��@�V@��#@��7@�/@��@��@�l�@�33@�@�ȴ@�ff@�{@���@�x�@�hs@�O�@�?}@��@���@��@�Ĝ@��u@��@�j@�Q�@�1'@�b@�  @�  @���@��;@���@�\)@�"�@���@�^5@�M�@�E�@��@��@���@��7@�p�@�X@�7L@��@��j@��j@��D@�bN@�9X@�(�@�b@�1@��@��
@��@�dZ@�33@���@�ȴ@��R@��!@�`B1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB��B��B��B��B��B��B��B�B�-B�3B�'B�'B�RBÖB��B�B�HB�fB�B�B��B��B��B��B��BBB%B%B+B1BDBJB\BbB\BPBDBB��B�mB�fB�mB�sB�sB�B�B�yB�mB�;BƨBŢBÖB�}B�XB�9B��B�#B�mB�mB�BƨB�jB�?B��B�LB��B��Br�B_;B[#B\)BZB@�B/B,B(�B%�B�B�B+B�mB��BÖB�jB�?B��B��B�oB�bB�DB�Bu�BgmB`BBP�BF�BB�B33B/B(�B�BbB%BB
��B
��B
�B
�B
�B
��B
�'B
��B
��B
�uB
�JB
�=B
�%B
� B
q�B
[#B
F�B
%�B
�B
�B
bB

=B
  B	�B	�HB	�)B	�B	�B	��B	ǮB	��B	�FB	�B	��B	��B	��B	��B	�uB	�1B	� B	s�B	iyB	_;B	YB	S�B	O�B	I�B	L�B	B�B	;dB	7LB	2-B	49B	9XB	/B	.B	#�B	�B	{B	{B	uB	hB	JB	  B��B��B�B�B�yB�BB�B�B�B�
B��B��B��BĜB�wB�dB�LB�-B�B�B�B��B��B��B��B��B��B��B��B��B�{B��B��B�uB�VB�1B�B�B�B� B~�B~�B~�Bz�Bp�Bl�Bk�Bk�BjBiyBhsBgmBe`BcTBaHB`BB[#B\)B\)B\)B[#BZBXBW
BT�BR�BR�BQ�BP�BO�BM�BJ�BJ�BK�BL�BN�BO�BN�BP�BO�BR�BP�BQ�BP�BVBT�BS�BR�BP�BK�BK�BK�BK�BJ�BG�BJ�BN�BQ�BR�BR�BP�BN�BT�BT�BT�BT�BVBVBVBW
BXBVBS�BT�BXBYBZBZBYBZB\)B`BBaHB`BBaHBaHBaHBcTBbNBbNB_;B\)BZBW
BXB]/B]/B^5BbNBffBiyBhsBhsBhsBhsBiyBk�Bk�Bk�Bl�Bm�Bm�Bm�Bn�Bn�Bm�Bo�Br�Bv�Bw�By�By�B�B�+B�=B�VB��B�B�qB��B��B��B�#B�HB�HB�BB�HB�mB�B�B��B��B��B��B��B	1B	DB	DB	JB	\B	\B	hB	oB	oB	oB	�B	�B	�B	�B	�B	�B	 �B	"�B	%�B	&�B	%�B	/B	49B	49B	49B	49B	5?B	5?B	5?B	6FB	6FB	7LB	9XB	:^B	<jB	>wB	@�B	A�B	E�B	I�B	N�B	O�B	Q�B	T�B	VB	ZB	]/B	]/B	_;B	^5B	^5B	_;B	^5B	^5B	_;B	_;B	`BB	bNB	k�B	n�B	s�B	v�B	w�B	w�B	x�B	x�B	x�B	{�B	|�B	}�B	~�B	� B	�B	�B	�B	�+B	�1B	�7B	�=B	�=B	�DB	�VB	�VB	�VB	�\B	�bB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�!B	�'B	�-B	�3B	�3B	�?B	�LB	�LB	�^B	�jB	�qB	�qB	�wB	�wB	�}B	��B	��B	B	ĜB	ŢB	ǮB	ȴB	ȴB	��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B��B��B��B��B��B��B��B�B�9B�?B�-B�-B�XBŢB��B�#B�TB�sB�B�B��B��B��B��B��BBB+B+B1B	7BJBPB\BbB\BPBJB+BB�sB�mB�yB�B�B�B�B�B�B�`BǮBƨBĜBB�wB�9B��B�#B�yB�B�5BȴB�wB�LBÖB�^B��B��Bx�BaHB\)B_;BcTBD�B0!B-B+B(�B!�B�BoB�B��BŢB�}B�qB�3B��B�uB�oB�\B�+B|�BjBhsBVBH�BI�B5?B2-B-B$�B�B1BBB
��B
��B
�B
�mB
ɺB
�XB
��B
��B
��B
�PB
�DB
�1B
�B
x�B
`BB
O�B
/B
�B
�B
oB
\B
1B	�B	�`B	�5B	�/B	�B	�B	��B	ĜB	�qB	�'B	��B	��B	��B	��B	��B	�DB	�B	w�B	m�B	bNB	[#B	VB	T�B	J�B	O�B	F�B	>wB	9XB	33B	5?B	=qB	33B	33B	+B	�B	�B	{B	{B	uB	uB	B��B��B�B�B�B�ZB�#B�#B�#B�B�
B��B��B��B��B�wB�dB�RB�-B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B�uB�JB�+B�B�B�B� B� B�B�By�Bn�Bl�Bl�Bk�BjBiyBiyBgmBgmBdZBbNB`BB_;B]/B]/B\)B\)B[#BYBXBW
BT�BS�BR�BP�BP�BP�BL�BL�BN�BP�BQ�BP�BS�BT�BVBQ�BT�BR�BXBW
BT�BS�BR�BL�BL�BK�BL�BL�BJ�BK�BN�BQ�BS�BT�BR�BN�BVB\)BZBVBVBW
BXBYBZBXBT�BVBYBZB[#BZBZB]/BaHBbNBbNBbNBcTBbNBbNBdZBcTBdZBbNB]/B\)B[#BZB^5B`BB_;BbNBgmBjBiyBiyBiyBjBk�Bl�Bl�Bl�Bm�Bm�Bm�Bm�Bn�Bn�Bn�Bp�Br�Bv�Bw�B{�By�B� B�+B�JB�VB��B�B�qB��B��B��B�)B�NB�NB�HB�NB�mB�B�B��B��B	  B	B	B		7B	JB	PB	VB	hB	hB	uB	uB	uB	uB	�B	�B	�B	�B	�B	�B	 �B	#�B	&�B	'�B	(�B	1'B	49B	49B	49B	5?B	5?B	5?B	5?B	6FB	7LB	7LB	:^B	;dB	=qB	>wB	@�B	A�B	F�B	K�B	O�B	O�B	Q�B	T�B	VB	ZB	^5B	]/B	_;B	_;B	^5B	`BB	_;B	^5B	_;B	`BB	`BB	bNB	l�B	o�B	t�B	v�B	w�B	x�B	x�B	y�B	z�B	|�B	}�B	~�B	� B	�B	�B	�B	�%B	�+B	�1B	�=B	�DB	�DB	�JB	�VB	�VB	�VB	�\B	�bB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�!B	�'B	�-B	�3B	�9B	�FB	�LB	�RB	�^B	�jB	�qB	�qB	�wB	�wB	�}B	��B	B	ÖB	ŢB	ƨB	ǮB	ȴB	ȴB	��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<49X<#�
<#�
<#�
<#�
<#�
<49X<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<e`B<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0.1 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201031446572012010314465720120103144657  AO  ARGQ                                                                        20111130140648  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130140648  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120103144657  IP                  G�O�G�O�G�O�                