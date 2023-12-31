CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-13T22:40:03Z UW 3.1 conversion   
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
_FillValue                 �  A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�     �  C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  Kt   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�     �  Mp   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  U\   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ]H   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  _D   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  g0   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  i,   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  q   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  y   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  {    PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �0   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �4   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �8   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �<   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �@   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   units         decibar    
_FillValue        G�O�        ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    units         decibar    
_FillValue        G�O�        ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  5901071 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               -A   AO  20111130140103  20190522121825  1727_5046_045                   2C  D   APEX                            2143                            040306                          846 @�O��	1   @�O��Q�@7d���S��c���$�1   GPS     Primary sampling: mixed [deeper than nominal 980dbar: discrete; nominal 980dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @�ff@�  A   A   A@  A`  A�  A�  A�33A�  A�  A�  A���A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&�C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd�Cf  Ch  Cj  Cl  Cn  Cp�Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3fD3�fD4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D=��D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DN��DO� DP  DPy�DQ  DQ� DR  DR� DS  DSy�DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dy�fD�6fD�\�D���D�� D�)�D�S3D��3D��fD�3D�` D��fD��D�3D�ffDډ�D��3D�0 D�L�D�fD�� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�33@���@���AffA>ffA^ffA~ffA�33A�ffA�33A�33A�33A�  A�33A�33B��B��B��B��B'��B/��B7��B?��BG��BO��BW��B_��Bg��Bo��Bw��B�  B�  B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���C�fC�fC�fC�fC	�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC!�fC#�fC&  C'�fC)�fC+�fC-�fC/�fC1�fC3�fC5�fC7�fC9�fC;�fC=�fC?�fCA�fCC�fCE�fCG�fCI�fCK�fCM�fCO�fCQ�fCS�fCU�fCW�fCY�fC[�fC]�fC_�fCa�fCd  Ce�fCg�fCi�fCk�fCm�fCp  Cq�fCs�fCu�fCw�fCy�fC{�fC}�fC�fC��3C��3C��fC��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C�  C�  C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��fC��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3D y�D ��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��D	y�D	��D
y�D
��Dy�D��Dy�D��Dy�D��Dy�D��D� D��Dy�D��Dy�D��D� D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��D y�D ��D!y�D!��D"y�D"��D#y�D#��D$y�D$��D%y�D%��D&y�D&��D'y�D'��D(y�D(��D)y�D)��D*y�D*��D+y�D+��D,y�D,��D-y�D-��D.y�D.��D/y�D/��D0y�D0��D1y�D1��D2y�D3  D3� D3��D4y�D4��D5y�D5��D6y�D6��D7y�D7��D8y�D8��D9y�D9��D:y�D:��D;y�D;��D<y�D<��D=y�D=�3D>y�D>��D?y�D?��D@y�D@��DAy�DA��DBy�DB��DCy�DC��DDy�DD��DEy�DE��DFy�DF��DGy�DG��DHy�DH��DIy�DI��DJy�DJ��DKy�DK��DLy�DL��DMy�DM��DNy�DN�3DOy�DO��DPs3DP��DQy�DQ��DRy�DR��DSs3DS��DTy�DT��DUy�DU��DVy�DV��DWy�DW��DXy�DX��DYy�DY��DZy�DZ��D[y�D[��D\y�D\��D]y�D]��D^y�D^��D_y�D_��D`y�D`��Day�Da��Dby�Db��Dcy�Dc��Ddy�Dd��Dey�De��Dfy�Df��Dgy�Dg��Dhy�Dh��Diy�Di��Djy�Dj��Dky�Dk��Dly�Dl��Dmy�Dm��Dny�Dn��Doy�Do��Dpy�Dp��Dqy�Dq��Dry�Dr��Dsy�Dy� D�33D�Y�D��fD���D�&fD�P D�� D��3D� D�\�D��3D��fD� D�c3DچfD�� D�,�D�I�D�3D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�dZA�hsA�jA�jA�dZA�ffA�hsA�n�A�p�A�p�A�p�A�r�A�r�A�t�A�t�A�t�A�v�A�v�A�v�A�x�A�z�A�z�A�z�A�z�A�|�A�|�A�|�A�z�A�|�A�|�A�~�A�|�A�z�A�t�A�t�A�r�A�p�A�l�A�9XAŬAÕ�A��A��DA��!A��#A�l�A�t�A��A��A�hsA��A��wA��hA��#A�XA�$�A�$�A�n�A��A���A�G�A��`A�&�A�A�A���A�p�A�JA��#A�z�A��A��PA�A���A��FA�1A�dZA���A��A��A��;A� �A��RA��wA�ƨA���A�|�A��A�-A���A��hA�bNA��A�p�A���A���A�E�A���A�~�A��/A�|�A�hsA��^A��A��+A��!A�+A�ƨA��A��A�"�A���A�S�A�bA���A��A~A{K�Az^5Ayx�Aw�wAv5?Au��AuAt~�As�PAs+Ar{ApVAm�#Ak;dAh��Ah5?Af��Ael�Ac�Aa;dA`A]�A\��A[�^AZI�AYS�AX�AX�9AXQ�AV�`AV5?AUp�ATȴAT�AR5?AQt�AP�AO|�AN�+AMx�AL��AL �AKx�AJ�DAI��AHAG?}AF�DAEx�AC�ACVAB��A@~�A?�mA?p�A?/A>�uA=p�A<ZA;�wA;&�A:ffA9�A8��A7|�A5��A5p�A4r�A3A37LA2��A1�A0��A/��A.�`A,�HA+�TA+�A*ZA)�
A)`BA'��A&�yA&E�A%�FA$jA#��A"�HA"(�A!?}A 1A�A �A/A��A�\A��AQ�A`BA�9AA�A1At�AJA�A��A5?A�;A��A"�A-A^5An�AXA
�uA
  A	XA��A �A��A��A�A��A�A�yA��A��A�7A��A ��@�33@�ff@��@��P@�M�@��;@���@��@���@��@�Ĝ@�S�@��/@�K�@�!@�@��@���@�t�@�@���@��m@ڰ!@��@���@Ӆ@Ѻ^@�Z@���@���@̴9@�ƨ@�v�@�x�@�Ĝ@�r�@Ǿw@Ɨ�@�{@�@�r�@��y@���@�%@��@�|�@��+@��w@�@�ȴ@��!@�E�@���@��@��#@���@���@�o@�=q@�p�@�(�@���@��+@���@��@��;@��@��y@�^5@���@�Z@�b@��
@�\)@�n�@�$�@��h@��/@�A�@��@���@���@�ff@��@���@�x�@��h@���@�/@���@�v�@��-@�hs@��@��9@�r�@�Z@�A�@���@��P@�+@���@�{@��#@�hs@�j@�1@��;@���@��w@��P@���@�o@���@��@��h@���@�/@���@�r�@��P@�33@�
=@�
=@�
=@��@���@�v�@���@�5?@��#@�@���@��7@���@��u@�z�@�r�@�j@�Z@�r�@�  @��P@�"�@��y@�ȴ@��!@���@���@�~�@�S�@�o@��@���@��@��@���@��-@�X@�`B@���@�@��-@��@��@�O�@��@��`@���@���@��j@��@�z�@� �@��
@���@�\)@�
=@��!@�ff@�J@���@�?}@���@��/@���@��D@�1'@�(�@� �@���@�ƨ@���@�l�@�S�@�K�@�;d@�@���@��!@�E�@�{@�@�O�@��`@���@�r�@�A�@�1'@�1@��@��@���@�dZ@�K�@��y@���@��+@�V@�M�@�J@���@��^@���@�G�@��@�%@��@��9@�(�@��@��F@�l�@��R@�~�@�M�@�5?@��@��@�r�@yx�@o�;@fE�@]@U��@O;d@D��@?l�@9��@4�@0��@+o@&V@ 1'@��@�@�^@j@	7L@z�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�dZA�hsA�jA�jA�dZA�ffA�hsA�n�A�p�A�p�A�p�A�r�A�r�A�t�A�t�A�t�A�v�A�v�A�v�A�x�A�z�A�z�A�z�A�z�A�|�A�|�A�|�A�z�A�|�A�|�A�~�A�|�A�z�A�t�A�t�A�r�A�p�A�l�A�9XAŬAÕ�A��A��DA��!A��#A�l�A�t�A��A��A�hsA��A��wA��hA��#A�XA�$�A�$�A�n�A��A���A�G�A��`A�&�A�A�A���A�p�A�JA��#A�z�A��A��PA�A���A��FA�1A�dZA���A��A��A��;A� �A��RA��wA�ƨA���A�|�A��A�-A���A��hA�bNA��A�p�A���A���A�E�A���A�~�A��/A�|�A�hsA��^A��A��+A��!A�+A�ƨA��A��A�"�A���A�S�A�bA���A��A~A{K�Az^5Ayx�Aw�wAv5?Au��AuAt~�As�PAs+Ar{ApVAm�#Ak;dAh��Ah5?Af��Ael�Ac�Aa;dA`A]�A\��A[�^AZI�AYS�AX�AX�9AXQ�AV�`AV5?AUp�ATȴAT�AR5?AQt�AP�AO|�AN�+AMx�AL��AL �AKx�AJ�DAI��AHAG?}AF�DAEx�AC�ACVAB��A@~�A?�mA?p�A?/A>�uA=p�A<ZA;�wA;&�A:ffA9�A8��A7|�A5��A5p�A4r�A3A37LA2��A1�A0��A/��A.�`A,�HA+�TA+�A*ZA)�
A)`BA'��A&�yA&E�A%�FA$jA#��A"�HA"(�A!?}A 1A�A �A/A��A�\A��AQ�A`BA�9AA�A1At�AJA�A��A5?A�;A��A"�A-A^5An�AXA
�uA
  A	XA��A �A��A��A�A��A�A�yA��A��A�7A��A ��@�33@�ff@��@��P@�M�@��;@���@��@���@��@�Ĝ@�S�@��/@�K�@�!@�@��@���@�t�@�@���@��m@ڰ!@��@���@Ӆ@Ѻ^@�Z@���@���@̴9@�ƨ@�v�@�x�@�Ĝ@�r�@Ǿw@Ɨ�@�{@�@�r�@��y@���@�%@��@�|�@��+@��w@�@�ȴ@��!@�E�@���@��@��#@���@���@�o@�=q@�p�@�(�@���@��+@���@��@��;@��@��y@�^5@���@�Z@�b@��
@�\)@�n�@�$�@��h@��/@�A�@��@���@���@�ff@��@���@�x�@��h@���@�/@���@�v�@��-@�hs@��@��9@�r�@�Z@�A�@���@��P@�+@���@�{@��#@�hs@�j@�1@��;@���@��w@��P@���@�o@���@��@��h@���@�/@���@�r�@��P@�33@�
=@�
=@�
=@��@���@�v�@���@�5?@��#@�@���@��7@���@��u@�z�@�r�@�j@�Z@�r�@�  @��P@�"�@��y@�ȴ@��!@���@���@�~�@�S�@�o@��@���@��@��@���@��-@�X@�`B@���@�@��-@��@��@�O�@��@��`@���@���@��j@��@�z�@� �@��
@���@�\)@�
=@��!@�ff@�J@���@�?}@���@��/@���@��D@�1'@�(�@� �@���@�ƨ@���@�l�@�S�@�K�@�;d@�@���@��!@�E�@�{@�@�O�@��`@���@�r�@�A�@�1'@�1@��@��@���@�dZ@�K�@��y@���@��+@�V@�M�@�J@���@��^@���@�G�@��@�%@��@��9@�(�@��@��F@�l�@��R@�~�@�M�@�5?@��@��@�r�@yx�@o�;@fE�@]@U��@O;d@D��@?l�@9��@4�@0��@+o@&V@ 1'@��@�@�^@j@	7L@z�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�5B�5B�5B�5B�5B�5B�5B�5B�;B�5B�5B�5B�5B�5B�5B�5B�5B�5B�5B�5B�5B�5B�5B�;B�5B�5B�;B�;B�;B�;B�;B�;B�BB�BB�BB�BB�BB�BB�5B�B��B��B�B�B�B��B��B��BɺBȴBƨBĜBB�qB�RB�'B�B��B��B��B��B�hB�=B�B|�By�Bs�Be`BVBD�B>wB6FB&�B�B{BDBB��B��B�B�NB�)B��BĜB�^B�3B�B��B�PBs�BcTB`BBT�BI�B<jB,B�B�BoBJB  B
��B
�B
�B
�HB
�5B
�B
��B
�wB
��B
��B
�oB
�VB
�1B
~�B
o�B
`BB
ZB
S�B
G�B
>wB
;dB
8RB
5?B
1'B
-B
%�B
�B
+B	��B	�B	�fB	�/B	�B	��B	�}B	�dB	�'B	�B	��B	��B	��B	��B	��B	��B	�{B	�\B	�PB	�+B	�B	y�B	x�B	v�B	p�B	k�B	gmB	dZB	`BB	\)B	W
B	O�B	D�B	@�B	9XB	0!B	"�B	�B	�B	PB	
=B	B	B��B��B�B�B�B�B�B�B�fB�NB�;B�)B�B�
B��B��B��BƨBB�wB�jB�XB�LB�?B�-B�B�B��B��B��B��B��B��B��B��B��B�uB�oB�hB�\B�JB�7B�1B�B�B� B}�Bz�Bv�Bu�Bs�Br�Br�Bp�Bn�BiyBe`BaHB_;B]/B[#BZBXBVBT�BR�BQ�BQ�BO�BM�BL�BK�BJ�BF�BC�BB�B@�B>wB=qB;dB9XB6FB6FB49B2-B1'B/B.B.B.B-B+B+B+B+B)�B)�B(�B%�B&�B&�B'�B'�B(�B)�B)�B(�B)�B,B-B-B-B/B0!B/B2-B49B7LB9XB:^B:^B<jBD�BJ�BM�BM�BM�BVBXB_;B^5B\)B[#B\)B[#BXBYBXBT�BT�BT�BT�BXB[#B\)B^5B_;B`BBaHBcTBcTBdZBffBgmBhsBjBl�Bn�Bs�Bx�B|�B}�B~�B� B�B�7B�JB�PB�\B�hB�oB�uB�uB�uB��B��B��B��B��B��B��B��B�B�B�B�!B�9B�jB�qB�wBBĜBǮB��B��B��B��B��B�B�#B�BB�ZB�mB�B�B�B�B�B��B	B	B	B	B	B	B		7B	VB	bB	uB	{B	�B	�B	�B	�B	#�B	)�B	+B	.B	6FB	:^B	>wB	@�B	B�B	B�B	D�B	J�B	M�B	M�B	Q�B	XB	[#B	^5B	aHB	bNB	bNB	cTB	cTB	dZB	gmB	iyB	iyB	l�B	m�B	o�B	q�B	t�B	v�B	y�B	{�B	}�B	� B	�B	�+B	�1B	�7B	�DB	�JB	�JB	�VB	�\B	�\B	�hB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�-B	�3B	�9B	�?B	�LB	�LB	�LB	�RB	�^B	�jB	�wB	�}B	�}B	B	ÖB	ĜB	ŢB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�/B	�B
B
oB
�B
)�B
1'B
;dB
@�B
F�B
K�B
O�B
VB
ZB
]/B
cTB
gmB
jB
p�B
t�B
z�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B�5B�5B�5B�5B�5B�5B�5B�5B�;B�5B�5B�5B�5B�5B�5B�5B�5B�5B�5B�5B�5B�5B�5B�;B�5B�5B�;B�;B�;B�;B�;B�;B�BB�BB�BB�BB�BB�HB�NB�)B�B�;B�TB�/B�BB�B�B��B��B��BǮBŢBƨBÖB�qB�?B�B�B��B��B��B��B�VB�B~�B{�By�Bl�B^5BF�BA�B=qB,B �B�BPB+BB��B�B�ZB�HB��BǮB�jB�?B�3B��B��Bx�BdZBcTBYBN�BC�B2-B!�B�B{BoBB
��B
�B
�B
�ZB
�BB
�/B
�B
ǮB
�!B
��B
�{B
�hB
�JB
�B
w�B
cTB
]/B
ZB
L�B
@�B
=qB
:^B
8RB
2-B
0!B
)�B
"�B
PB
  B	�B	�yB	�HB	�#B	��B	B	��B	�9B	�B	��B	��B	��B	��B	��B	��B	��B	�hB	�\B	�7B	�%B	{�B	{�B	y�B	s�B	n�B	iyB	gmB	bNB	_;B	ZB	T�B	G�B	B�B	=qB	6FB	$�B	 �B	 �B	\B	JB	%B	B	B��B�B��B�B�B�B�B�B�TB�NB�5B�B�B�B��B��BɺBǮB��B�wB�dB�XB�LB�FB�!B�B�B��B��B��B��B��B��B��B��B��B�uB�uB�oB�bB�JB�=B�1B�+B�B� B� By�Bw�Bt�Bs�Bs�Br�Bq�Bo�Bk�Be`BbNB_;B]/B\)BZBXBXBT�BR�BR�BP�BO�BM�BL�BL�BK�BG�BC�BA�BA�B?}B>wB<jB:^B7LB:^B5?B33B33B1'B/B0!B.B0!B/B-B-B,B,B,B,B)�B)�B)�B)�B+B,B,B+B,B-B.B/B/B0!B0!B2-B5?B6FB8RB:^B:^B;dB?}BE�BJ�BM�BN�BN�BW
BW
B`BB`BB]/B\)B]/B]/BYB[#B[#BVBT�BVBVBYB\)B^5B_;B`BBaHBcTBdZBdZBe`BgmBhsBiyBk�Bm�Bo�Bt�Bx�B|�B}�B� B�B�%B�=B�PB�VB�bB�oB�oB�uB�{B�{B��B��B��B��B��B��B��B��B�B�B�B�!B�9B�qB�}B�wBBŢBǮB��B��B��B��B��B�B�#B�HB�ZB�mB�B�B�B�B��B��B	B	B	B	B	B	B	
=B	\B	hB	uB	{B	�B	�B	�B	�B	"�B	+B	-B	/B	6FB	:^B	?}B	@�B	C�B	B�B	D�B	J�B	M�B	M�B	Q�B	XB	[#B	^5B	aHB	bNB	bNB	cTB	dZB	e`B	hsB	iyB	jB	m�B	n�B	p�B	r�B	u�B	w�B	z�B	{�B	}�B	�B	�B	�+B	�1B	�7B	�DB	�JB	�JB	�VB	�\B	�\B	�hB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�3B	�9B	�9B	�FB	�RB	�LB	�LB	�RB	�dB	�qB	�wB	�}B	��B	B	ÖB	ĜB	ƨB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�/B	�B
B
oB
�B
)�B
1'B
;dB
@�B
F�B
K�B
O�B
VB
ZB
]/B
cTB
gmB
k�B
p�B
t�B
z�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<D��<T��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0.1 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201031446492012010314464920120103144649  AO  ARGQ                                                                        20111130140103  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130140103  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120103144649  IP                  G�O�G�O�G�O�                