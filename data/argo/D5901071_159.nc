CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-13T22:40:35Z UW 3.1 conversion   
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  5901071 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               �A   AO  20111130142757  20190522121827  1727_5046_159                   2C  D   APEX                            2143                            040306                          846 @��2��
1   @��2�Q�@5Ұ ě��c����S�1   GPS     Primary sampling: mixed [deeper than nominal 980dbar: discrete; nominal 980dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&�C(�C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`�Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Cs�fCv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C��C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:�fD;  D;y�D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DBy�DC  DC� DD  DD� DE  DE� DE��DF� DGfDG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DM��DN� DOfDO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dg��Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� DofDo� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dy� D�)�D�VfD���D��fD�)�D�l�D��3D��fD��D�ffD�� D��fD�,�D�ffDڦfD��3D��D�I�D�3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�ff@���@���AffA>ffA^ffA~ffA�33A�33A�33A�33A�33A�33A�33A�33B��B��B��B��B'��B/��B7��B?��BG��BO��BW��B_��Bg��Bo��Bw��B��B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���C�fC�fC�fC�fC	�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC!�fC#�fC&  C(  C)�fC+�fC-�fC/�fC1�fC3�fC5�fC7�fC9�fC;�fC=�fC?�fCA�fCC�fCE�fCG�fCI�fCK�fCM�fCO�fCQ�fCS�fCU�fCW�fCY�fC[�fC]�fC`  Ca�fCc�fCe�fCg�fCi�fCk�fCm�fCo�fCq�fCs��Cu�fCw�fCy�fC{�fC}�fC�fC��3C��3C��3C��3C��fC��3C��3C��3C��3C��3C��3C��3C�  C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C�  C��3C��3C��3C��3C��3C��3C�  C�  C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��fC��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C�  C��3C��3C��3C��3C��3C�  C��3C��3C��3C��3C��3C��3C��3C��3C��3C��fC��3C�  C��3C��3C��3C��3C��3C��3C��3C�  C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C�  C��3C��3C��3C��3C��3C��3D y�D ��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��D	y�D	��D
y�D
��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��D y�D ��D!y�D!��D"y�D"��D#y�D#��D$y�D$��D%y�D%��D&y�D&��D'y�D'��D(y�D(��D)y�D)��D*y�D*��D+y�D+��D,y�D,��D-y�D-��D.y�D.��D/y�D/��D0y�D0��D1y�D1��D2y�D2��D3y�D3��D4y�D4��D5y�D5��D6y�D6��D7y�D7��D8y�D8��D9y�D9��D:� D:��D;s3D;��D<y�D<��D=y�D=��D>y�D>��D?y�D?��D@y�D@��DAy�DA��DBs3DB��DCy�DC��DDy�DD��DEy�DE�3DFy�DG  DGy�DG��DHy�DH��DIy�DI��DJy�DJ��DKy�DK��DLy�DL��DMy�DM�3DNy�DO  DOy�DO��DPy�DP��DQy�DQ��DRy�DR��DSy�DS��DTy�DT��DUy�DU��DVy�DV��DWy�DW��DXy�DX��DYy�DY��DZy�DZ��D[y�D[��D\y�D\��D]y�D]��D^y�D^��D_y�D_��D`y�D`��Day�Da��Dby�Db��Dcy�Dc��Ddy�Dd��Dey�De��Dfy�Df��Dgy�Dg�3Dhy�Dh��Diy�Di��Djy�Dj��Dky�Dk��Dly�Dl��Dmy�Dm��Dny�Do  Doy�Do��Dpy�Dp��Dqy�Dq��Dry�Dr��Dsy�Ds��Dy��D�&fD�S3D��fD��3D�&fD�i�D�� D��3D��D�c3D���D��3D�)�D�c3Dڣ3D�� D��D�FfD� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A˧�A˥�A˧�A˧�A˩�A˩�A˩�AˬAˬAˮAˮAˮA˝�A�r�A�
=A�z�A��A���A�S�A�ffA��A�ȴA�O�AƗ�AľwA��A�+A�t�A�"�A��A�ȴA���A�A�=qA�-A�
=A��^A�x�A�oA�hsA�$�A���A�O�A���A�ffA��HA��A�VA���A�z�A���A�ffA�(�A���A�VA�9XA��`A��A���A� �A��A��
A��
A��^A�{A���A�E�A�|�A��+A�"�A��-A��!A�A�VA�
=A�\)A�S�A�ȴA���A�I�A��^A��;A�\)A�A�\)A�A�A��PA���A�&�A��A���A�bNA�G�A�(�A�JA��-A��A�C�A���A�ZA�"�A���A�hsA�-A�XA�M�A� �A�XA�hsA�n�A��A��A��A�ȴA��A�ƨA�ffA�%A���A�=qA��A��A�5?A���A~��A{�A{;dA{%Az�HAz�\Ay��Ax��Av�!ArVAm�^Ak��Aj��Ah�jAb�A` �A\�\AY?}AX�AW��AVM�ARAN��AM�AL��AJ�AI�-AI;dAH1'AD�AD1AC�^AB�yAA�^A@�RA?&�A<1A:�uA8r�A733A5��A4�A4bA2��A0 �A.JA,E�A*�!A)�hA(�jA(�\A&n�A#+A"1A!�A ffA�AE�A��AffA�TA��A�HA��A��A��AI�A��A��AƨAO�A
=A��A��AjA��A�At�AdZA�A�A�RA��Ar�AA�A{A�A7LA
�A	�-A��A�/A��A=qA��A�7A\)AA�^A�;AAI�A�
A?}A z�@�bN@�ff@�{@� �@�^@���@�Z@�|�@�/@�R@�&�@��@�P@�t�@��@�|�@���@�/@�F@�=q@�G�@��@�?}@�Ĝ@�A�@�\)@�33@�"�@ڧ�@���@ٙ�@�hs@�%@ش9@ؼj@���@�p�@���@ٙ�@�X@ڏ\@�~�@١�@١�@ٺ^@�-@���@�X@�?}@�I�@��H@ӕ�@�V@��@�ƨ@�9X@��@��T@��T@��@�@θR@�
=@��H@�v�@͑h@̓u@�C�@�5?@�X@��@�/@��`@�Q�@���@�@�
=@��^@���@�bN@�%@�^5@�o@�ȴ@���@�b@��@��@�-@��@���@���@�o@��H@��!@���@��/@��@�\)@��H@���@�V@�^5@�o@���@�C�@���@���@�7L@���@�ƨ@�A�@�b@�;d@��@�=q@���@��@��@�Ĝ@���@��u@��@�r�@�A�@�\)@��@��\@�5?@��@���@���@�`B@�?}@�?}@�?}@�G�@�X@�hs@�hs@�p�@�`B@��@��@��@��@��7@��@��h@���@��@��h@�p�@�V@�&�@�X@�X@�G�@��@�&�@���@�I�@��m@�b@� �@�(�@��;@��w@�v�@��T@�n�@���@�n�@���@�1@��@�S�@��@��@��
@��@�?}@���@�Z@�9X@���@�l�@�;d@�;d@�C�@�C�@�C�@�;d@�33@�+@�+@�
=@��@���@�ff@��@��h@���@��@�Q�@� �@��m@���@��F@���@�l�@�@���@�E�@���@���@���@��@�z�@�r�@�bN@�A�@��w@�\)@��R@�M�@��@�=q@���@��@��#@���@�G�@��`@��u@��@��P@�S�@�;d@�;d@�33@��@���@��R@�ȴ@��\@��@�@�?}@���@�Ĝ@���@�r�@�(�@� �@� �@�  @���@�@��w@�o@{�m@u�@n$�@b�!@U��@J�H@D�/@?�@:�\@3"�@/
=@,�@&v�@ ��@�/@��@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A˧�A˥�A˧�A˧�A˩�A˩�A˩�AˬAˬAˮAˮAˮA˝�A�r�A�
=A�z�A��A���A�S�A�ffA��A�ȴA�O�AƗ�AľwA��A�+A�t�A�"�A��A�ȴA���A�A�=qA�-A�
=A��^A�x�A�oA�hsA�$�A���A�O�A���A�ffA��HA��A�VA���A�z�A���A�ffA�(�A���A�VA�9XA��`A��A���A� �A��A��
A��
A��^A�{A���A�E�A�|�A��+A�"�A��-A��!A�A�VA�
=A�\)A�S�A�ȴA���A�I�A��^A��;A�\)A�A�\)A�A�A��PA���A�&�A��A���A�bNA�G�A�(�A�JA��-A��A�C�A���A�ZA�"�A���A�hsA�-A�XA�M�A� �A�XA�hsA�n�A��A��A��A�ȴA��A�ƨA�ffA�%A���A�=qA��A��A�5?A���A~��A{�A{;dA{%Az�HAz�\Ay��Ax��Av�!ArVAm�^Ak��Aj��Ah�jAb�A` �A\�\AY?}AX�AW��AVM�ARAN��AM�AL��AJ�AI�-AI;dAH1'AD�AD1AC�^AB�yAA�^A@�RA?&�A<1A:�uA8r�A733A5��A4�A4bA2��A0 �A.JA,E�A*�!A)�hA(�jA(�\A&n�A#+A"1A!�A ffA�AE�A��AffA�TA��A�HA��A��A��AI�A��A��AƨAO�A
=A��A��AjA��A�At�AdZA�A�A�RA��Ar�AA�A{A�A7LA
�A	�-A��A�/A��A=qA��A�7A\)AA�^A�;AAI�A�
A?}A z�@�bN@�ff@�{@� �@�^@���@�Z@�|�@�/@�R@�&�@��@�P@�t�@��@�|�@���@�/@�F@�=q@�G�@��@�?}@�Ĝ@�A�@�\)@�33@�"�@ڧ�@���@ٙ�@�hs@�%@ش9@ؼj@���@�p�@���@ٙ�@�X@ڏ\@�~�@١�@١�@ٺ^@�-@���@�X@�?}@�I�@��H@ӕ�@�V@��@�ƨ@�9X@��@��T@��T@��@�@θR@�
=@��H@�v�@͑h@̓u@�C�@�5?@�X@��@�/@��`@�Q�@���@�@�
=@��^@���@�bN@�%@�^5@�o@�ȴ@���@�b@��@��@�-@��@���@���@�o@��H@��!@���@��/@��@�\)@��H@���@�V@�^5@�o@���@�C�@���@���@�7L@���@�ƨ@�A�@�b@�;d@��@�=q@���@��@��@�Ĝ@���@��u@��@�r�@�A�@�\)@��@��\@�5?@��@���@���@�`B@�?}@�?}@�?}@�G�@�X@�hs@�hs@�p�@�`B@��@��@��@��@��7@��@��h@���@��@��h@�p�@�V@�&�@�X@�X@�G�@��@�&�@���@�I�@��m@�b@� �@�(�@��;@��w@�v�@��T@�n�@���@�n�@���@�1@��@�S�@��@��@��
@��@�?}@���@�Z@�9X@���@�l�@�;d@�;d@�C�@�C�@�C�@�;d@�33@�+@�+@�
=@��@���@�ff@��@��h@���@��@�Q�@� �@��m@���@��F@���@�l�@�@���@�E�@���@���@���@��@�z�@�r�@�bN@�A�@��w@�\)@��R@�M�@��@�=q@���@��@��#@���@�G�@��`@��u@��@��P@�S�@�;d@�;d@�33@��@���@��R@�ȴ@��\@��@�@�?}@���@�Ĝ@���@�r�@�(�@� �@� �@�  @���@�@��w@�o@{�m@u�@n$�@b�!@U��@J�H@D�/@?�@:�\@3"�@/
=@,�@&v�@ ��@�/@��@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBZBZBZBYBZBZBZB[#BZB]/B_;B_;B� B�B-B;dB7LB5?B49B,B&�B"�B�BB�B�mB�NB��B��B�B�/B�)B�/B�B�BB%�B>wB>wBQ�BXB`BBjBs�Bw�Bz�B�=B�oB��B��B��B�jB��B�^B�dBŢBBȴB��B��BƨBÖB�qB�jB�qB�qB�^B�?B�'B�B��B��B��B�hB}�Bk�BM�B7LB/B)�B#�B�B�BuB1B��B��BB+BBB��B�B�#B��BŢBB�qB�3B��B��B��B�=Br�BYBC�B1'B"�BhB
��B
�
B
ȴB
�}B
��B
��B
�oB
�=B
y�B
r�B
l�B
gmB
bNB
]/B
Q�B
:^B
-B
+B
(�B
&�B
"�B
�B
hB	��B	�B	�dB	�B	��B	�B	W
B	@�B	&�B	uB	VB	1B��B�sB�)B�
B��B��BŢBB�dB�3B�'B�B�B��B��B��B�uB�PB�hB�PB�DB�=B�1B�B�B�B}�B{�By�Bu�Bs�Bp�Bn�BjBjBhsBhsBe`Be`BffBffBdZBaHBaHB`BBbNB`BB_;B^5B^5B^5B]/B]/B]/B\)B\)B\)B[#B[#B[#BZBZBZBYBYBXBW
BW
BVBT�BT�B[#Be`BhsBk�Bl�Bk�BhsBdZB`BB^5B]/B\)BYBVBS�BT�BXB^5B`BBaHB`BB[#BR�BK�BL�BN�BO�BS�BO�BL�B[#BbNB\)BP�BS�B\)B_;B]/B_;BcTBhsBo�Bt�Bv�Bx�Bx�By�B{�B�B�%B�DB�hB��B��B��B�B�B�B�!B�9B�?B�XB�dB�^B�XB�3B�'B�9B�'B�qB�wB�XB�qBƨBǮB��B��B��B��B��B�B�NB�yB�B�B��B��B	B	%B	B��B��B��B��B��B	%B	
=B��B�)B��B��B��B�B�B�B�)B�HB�NB�TB�ZB�`B�B�B��B	B	B	JB	�B	�B	 �B	%�B	$�B	$�B	#�B	#�B	+B	-B	.B	-B	-B	,B	-B	-B	.B	/B	/B	/B	.B	.B	/B	0!B	1'B	33B	5?B	6FB	7LB	9XB	;dB	=qB	?}B	A�B	B�B	C�B	D�B	D�B	D�B	F�B	F�B	F�B	G�B	H�B	H�B	I�B	J�B	J�B	J�B	K�B	K�B	M�B	O�B	R�B	VB	[#B	]/B	]/B	]/B	\)B	_;B	bNB	e`B	hsB	k�B	jB	k�B	p�B	r�B	r�B	s�B	o�B	n�B	o�B	r�B	w�B	z�B	z�B	z�B	~�B	�B	�B	�B	�1B	�7B	�7B	�7B	�7B	�7B	�7B	�7B	�DB	�DB	�=B	�\B	�{B	�oB	�hB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�-B	�FB	�LB	�RB	�RB	�RB	�RB	�dB	�wB	ÖB	ŢB	ŢB	ƨB	ȴB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	�B	�
B	�
B	�B	�B	�B	�)B	�)B	�)B	�/B	�/B	�/B	�;B	�BB	�HB	�BB	�BB	�HB	�TB	�ZB	�fB	�B	�B	��B
%B
\B
�B
#�B
,B
2-B
=qB
B�B
G�B
L�B
P�B
Q�B
XB
^5B
dZB
gmB
l�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 BZBZBZBYBZBZBZB[#BZB]/B_;B_;B�B�)B/B=qB8RB7LB8RB-B'�B$�B�BVB�B�B�`B�BŢB�#B�5B�5B�;B�B�BB&�B@�BA�BR�BZBcTBm�Bu�Bz�B|�B�DB��B��B��B�B�qBÖB�jB�jBƨBĜBɺB��B��BɺBǮBB�}B�wB��B�wB�^B�3B�'B�B��B��B��B�Bv�BVB<jB33B-B(�B!�B�B�BJBB��BB1B%BBB�B�HB��BƨBÖB��B�XB��B��B��B�bB{�B^5BI�B5?B'�B�BJB
�#B
��B
ɺB
��B
��B
��B
�oB
|�B
u�B
n�B
jB
dZB
bNB
bNB
D�B
.B
,B
)�B
'�B
%�B
�B
�B
	7B	�yB	��B	�B	��B	��B	^5B	I�B	.B	�B	bB	JB	%B�B�5B�B�
B��BǮBƨBŢB�?B�-B�-B�B�B��B��B��B��B��B�uB�\B�PB�PB�hB�JB�7B�B~�B{�Bv�Bx�Bw�Bq�Bl�Bl�Bk�BjBjBiyBgmBgmBffBhsBffBgmBdZBbNBcTBaHB`BB_;B^5B^5B^5B^5B^5B\)B\)B\)B\)B[#B[#B[#BZBZBZBYBZBYBXBVB\)BgmBjBl�Bm�Bm�Bm�BjBcTB`BB_;B^5B\)B\)BVBVB]/B`BBaHBbNBaHB^5BVBM�BM�BO�BO�BVBS�BK�BZBe`BdZBR�BT�B\)B`BB^5BaHBcTBhsBp�Bu�Bw�By�By�Bz�B{�B�B�B�DB�oB��B��B��B�B�B�B�!B�?B�FB�XB�qB�qB�wB�?B�-B�LB�'B�qB��B�XB�jBƨBȴB��B��B��B��B�
B�)B�ZB�B�B�B��B��B	B	1B	1B��B��B��B��B��B	%B	hB	B�TB��B��B��B�B�B�#B�/B�NB�TB�`B�fB�mB�B�B��B	B	B	DB	�B	�B	!�B	'�B	%�B	%�B	$�B	#�B	+B	.B	/B	.B	.B	-B	-B	-B	.B	/B	/B	/B	.B	0!B	0!B	1'B	2-B	49B	5?B	7LB	8RB	9XB	;dB	=qB	?}B	A�B	B�B	C�B	D�B	D�B	D�B	F�B	F�B	F�B	G�B	H�B	H�B	I�B	J�B	J�B	J�B	L�B	K�B	M�B	O�B	R�B	VB	[#B	^5B	^5B	^5B	\)B	_;B	bNB	e`B	iyB	k�B	k�B	jB	o�B	s�B	s�B	w�B	p�B	n�B	n�B	q�B	y�B	~�B	|�B	z�B	� B	�B	�B	�%B	�1B	�7B	�7B	�7B	�7B	�7B	�7B	�7B	�DB	�DB	�=B	�\B	��B	�uB	�oB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�3B	�FB	�LB	�RB	�RB	�RB	�XB	�jB	�}B	ĜB	ŢB	ŢB	ǮB	ȴB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	�B	�
B	�
B	�B	�B	�B	�)B	�/B	�/B	�/B	�/B	�5B	�;B	�BB	�HB	�BB	�BB	�HB	�TB	�`B	�fB	�B	�B	��B
%B
\B
�B
#�B
,B
2-B
=qB
B�B
G�B
L�B
Q�B
Q�B
XB
^5B
dZB
gmB
l�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<T��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<�o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<e`B<u<#�
<#�
<#�
<���<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0.1 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201031447302012010314473020120103144730  AO  ARGQ                                                                        20111130142757  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130142757  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120103144730  IP                  G�O�G�O�G�O�                