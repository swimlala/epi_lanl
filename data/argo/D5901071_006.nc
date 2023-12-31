CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-13T22:39:52Z UW 3.1 conversion   
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  5901071 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               A   AO  20111130135507  20190522121825  1727_5046_006                   2C  D   APEX                            2143                            040306                          846 @� 	 �1   @� 	���@7n��O��c˥�S��1   GPS     Primary sampling: mixed [deeper than nominal 980dbar: discrete; nominal 980dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  BffB  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C�fC  C
  C  C  C  C  C  C  C  C  C�fC  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4�C6�C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp�Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� DdfDd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Doy�Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� DyFfD�33D�` D���D���D�#3D�S3D��fD���D��D�Y�D�� D���D�#3D�S3Dڠ D�� D��D�@ D�3D�� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�  @���@���AffA>ffA^ffA~ffA�33A�33A�33A�33A�33A�33A�33A�33B��B  B��B��B'��B/��B7��B?��BG��BO��BW��B_��Bg��Bo��Bw��B��B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���C�fC�fC��C�fC	�fC�fC�fC�fC�fC�fC�fC�fC�fC��C�fC�fC!�fC#�fC%�fC'�fC)�fC+�fC-�fC/�fC1�fC4  C6  C7�fC9�fC;�fC=�fC?�fCA�fCC�fCE�fCG�fCI�fCK�fCM�fCO�fCQ�fCS�fCU�fCW�fCY�fC[�fC]�fC_�fCa�fCc�fCe�fCg�fCi�fCk�fCm�fCp  Cq�fCs�fCu�fCw�fCy�fC{�fC}�fC�fC��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��fC��3C��3C��3C��3C��3C��3C��3C��fC��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C�  C��3C��3C��3C��3C��3C��3C��3C��3C�  C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3D y�D ��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��D	y�D	��D
y�D
��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��D y�D ��D!y�D!��D"y�D"��D#y�D#��D$y�D$��D%y�D%��D&y�D&��D'y�D'��D(y�D(��D)y�D)��D*y�D*��D+y�D+��D,y�D,��D-y�D-��D.y�D.��D/y�D/��D0y�D0��D1y�D1��D2y�D2��D3y�D3��D4y�D4��D5y�D5��D6y�D6��D7y�D7��D8y�D8��D9y�D9��D:y�D:��D;y�D;��D<y�D<��D=y�D=��D>y�D>��D?y�D?��D@y�D@��DAy�DA��DBy�DB��DCy�DC��DDy�DD��DEy�DE��DFy�DF��DGy�DG��DHy�DH��DIy�DI��DJy�DJ��DKy�DK��DLy�DL��DMy�DM��DNy�DN��DOy�DO��DPy�DP��DQy�DQ��DRy�DR��DSy�DS��DTy�DT��DUy�DU��DVy�DV��DWy�DW��DXy�DX��DYy�DY��DZy�DZ��D[y�D[��D\y�D\��D]y�D]��D^y�D^��D_y�D_��D`y�D`��Day�Da��Dby�Db��Dcy�Dd  Ddy�Dd��Dey�De��Dfy�Df��Dgy�Dg��Dhy�Dh��Diy�Di��Djy�Dj��Dky�Dk��Dly�Dl��Dmy�Dm��Dny�Dn��Dos3Do��Dpy�Dp��Dqy�Dq��Dry�Dr��Dsy�Dy@ D�0 D�\�D���D��D�  D�P D��3D�ٚD��D�VfD���D��fD�  D�P Dڜ�D���D�fD�<�D� D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�v�AƃA��/A�=qA��#A��RA��A���A��PA�~�A�hsA�Q�A�/A���A��+A�$�A�~�A�-A���A�E�A��#A�ȴA�A��9A��A���A�t�A�;dA��yA���A�x�A���A��/A�?}A��PA��A�x�A�M�A��A�-A��FA��A��DA�S�A��9A�C�A��A��`A��A�bNA���A�K�A���A�  A��A�\)A�
=A��`A�1'A��A���A�~�A�ffA��;A�(�A��+A��A��;A��7A��!A���A�A��-A��PA�bNA��!A���A�ZA��A���A���A��;A�p�A���A��A�`BA�9XA�A���A���A��\A�+A�&�A�;dA��;A�33A���A�"�A�l�A�A�C�A�^5A���A��\A��mA���A�-A| �Ax�uAt�ArĜAoO�AlffAk7LAh��AhbNAg��Af9XAd5?Ab=qAaƨA`�A]x�A[33AX{AV��AV��AV9XAUO�ATn�AS��ASAQ�FANn�AKS�AJ^5AI7LAH�+AF^5AE�wAC��ABv�AA�;A@��A@�A?dZA>A<�jA:9XA8�RA8A7XA7�A6��A6^5A5�A4��A41A2��A1��A1�A/�#A.jA-��A-\)A,ȴA,bA*�+A)?}A(�jA&�HA$��A#�hA"��A"�9A"-A!dZA ��A �\A�A�`A�9A��A=qA�7AjA��A�A�PA+AM�A�PA�A�/A��A�AG�A �A?}A$�A��A33AjA�A�TAƨA�A�DA?}A
~�A�RA?}AM�A1AVA(�Ap�A �!@�\)@���@��@��@��P@��@�@�ff@�@�?}@�A�@�~�@�@���@��@�@�V@�b@�  @�P@��y@�!@�+@���@�/@�(�@�hs@���@�"�@��@޸R@��@�5?@�9X@�/@�G�@�o@���@��;@�"�@��H@�|�@̣�@��@��H@ˮ@�A�@�X@�;d@ċD@�9X@�j@å�@�33@��7@�b@��@��@�O�@�/@�hs@�I�@���@��+@�X@�b@�S�@���@�(�@�1'@��\@���@�{@�^5@��!@�  @��/@��@��T@��@���@�b@��@�bN@��j@���@���@�1@��w@�v�@���@�bN@���@�~�@�~�@���@�/@�{@���@��#@�-@�\)@�o@���@�ȴ@��@�J@�G�@�(�@���@���@���@�;d@��H@���@��@���@��@���@�@��\@�@���@�ȴ@���@��\@�ff@�E�@�@��7@�X@���@��@�I�@���@�C�@�C�@��H@�=q@��@���@���@�p�@��@��`@���@��u@�1'@�  @���@��F@�l�@�\)@��@�dZ@�n�@���@�J@�E�@�E�@�@���@��@��P@�1@���@�O�@�O�@�Ĝ@�9X@��@��
@���@��P@�t�@�l�@�S�@���@�{@���@��h@��/@��j@�bN@�9X@�  @�  @��@��m@��
@�S�@��R@�@��@���@��-@��7@�X@�7L@��@��j@��u@�bN@�  @��P@�v�@�r�@���@�S�@�;d@��@�v�@�~�@��+@�=q@�@�?}@�G�@�G�@�5?@��P@�;d@��!@�J@���@��@���@�Ĝ@�j@�l�@�"�@�t�@��@��w@��P@�;d@���@���@�~�@�M�@�5?@�-@�{@��@���@�`B@�/@��@���@���@��j@�@;d@;d@l�@�  @��@�Z@��@}p�@|�@}V@}O�@|Z@{@z��@z~�@y�#@v$�@p��@i7L@^�y@Z��@OK�@Ihs@E�@@b@:M�@7|�@0��@+��@&�+@"��@�j@�@��@33@l�@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�v�AƃA��/A�=qA��#A��RA��A���A��PA�~�A�hsA�Q�A�/A���A��+A�$�A�~�A�-A���A�E�A��#A�ȴA�A��9A��A���A�t�A�;dA��yA���A�x�A���A��/A�?}A��PA��A�x�A�M�A��A�-A��FA��A��DA�S�A��9A�C�A��A��`A��A�bNA���A�K�A���A�  A��A�\)A�
=A��`A�1'A��A���A�~�A�ffA��;A�(�A��+A��A��;A��7A��!A���A�A��-A��PA�bNA��!A���A�ZA��A���A���A��;A�p�A���A��A�`BA�9XA�A���A���A��\A�+A�&�A�;dA��;A�33A���A�"�A�l�A�A�C�A�^5A���A��\A��mA���A�-A| �Ax�uAt�ArĜAoO�AlffAk7LAh��AhbNAg��Af9XAd5?Ab=qAaƨA`�A]x�A[33AX{AV��AV��AV9XAUO�ATn�AS��ASAQ�FANn�AKS�AJ^5AI7LAH�+AF^5AE�wAC��ABv�AA�;A@��A@�A?dZA>A<�jA:9XA8�RA8A7XA7�A6��A6^5A5�A4��A41A2��A1��A1�A/�#A.jA-��A-\)A,ȴA,bA*�+A)?}A(�jA&�HA$��A#�hA"��A"�9A"-A!dZA ��A �\A�A�`A�9A��A=qA�7AjA��A�A�PA+AM�A�PA�A�/A��A�AG�A �A?}A$�A��A33AjA�A�TAƨA�A�DA?}A
~�A�RA?}AM�A1AVA(�Ap�A �!@�\)@���@��@��@��P@��@�@�ff@�@�?}@�A�@�~�@�@���@��@�@�V@�b@�  @�P@��y@�!@�+@���@�/@�(�@�hs@���@�"�@��@޸R@��@�5?@�9X@�/@�G�@�o@���@��;@�"�@��H@�|�@̣�@��@��H@ˮ@�A�@�X@�;d@ċD@�9X@�j@å�@�33@��7@�b@��@��@�O�@�/@�hs@�I�@���@��+@�X@�b@�S�@���@�(�@�1'@��\@���@�{@�^5@��!@�  @��/@��@��T@��@���@�b@��@�bN@��j@���@���@�1@��w@�v�@���@�bN@���@�~�@�~�@���@�/@�{@���@��#@�-@�\)@�o@���@�ȴ@��@�J@�G�@�(�@���@���@���@�;d@��H@���@��@���@��@���@�@��\@�@���@�ȴ@���@��\@�ff@�E�@�@��7@�X@���@��@�I�@���@�C�@�C�@��H@�=q@��@���@���@�p�@��@��`@���@��u@�1'@�  @���@��F@�l�@�\)@��@�dZ@�n�@���@�J@�E�@�E�@�@���@��@��P@�1@���@�O�@�O�@�Ĝ@�9X@��@��
@���@��P@�t�@�l�@�S�@���@�{@���@��h@��/@��j@�bN@�9X@�  @�  @��@��m@��
@�S�@��R@�@��@���@��-@��7@�X@�7L@��@��j@��u@�bN@�  @��P@�v�@�r�@���@�S�@�;d@��@�v�@�~�@��+@�=q@�@�?}@�G�@�G�@�5?@��P@�;d@��!@�J@���@��@���@�Ĝ@�j@�l�@�"�@�t�@��@��w@��P@�;d@���@���@�~�@�M�@�5?@�-@�{@��@���@�`B@�/@��@���@���@��j@�@;d@;d@l�@�  @��@�Z@��@}p�@|�@}V@}O�@|Z@{@z��@z~�@y�#@v$�@p��@i7L@^�y@Z��@OK�@Ihs@E�@@b@:M�@7|�@0��@+��@&�+@"��@�j@�@��@33@l�@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�yB�;BȴB��B��B��B��B�uB�oB�hB�hB�hB�hB�hB�bB�\B�hB��B��B��B��B��B��B��B��B��B��B��B�B��B��B��B�B�'B�-B�?B�?B�9B�-B�?B�FB�wB�}B�^B�qBĜB�B�BB�BĜB�wBB�'B�JB�hB�oB�JB�VB�1Bt�BcTB33B�B
=B  B��B�B�sB��B
=B\BoBhBbB	7B��B�B�5B��B�XB��B�1B{�Bk�BF�B2-B �B�BoBB
�HB
ȴB
�B
��B
��B
�PB
�B
}�B
cTB
ZB
O�B
B�B
9XB
33B
�B
1B	��B	�fB	�B	ƨB	�jB	�!B	�B	��B	��B	��B	��B	�VB	}�B	s�B	l�B	]/B	E�B	7LB	#�B	�B	�B	�B	{B	bB	JB	+B	  B�B�B�B�B�B�NB�/B�
B��B��B��B��BǮBB�jB�9B�-B�!B�'B�'B�'B�-B�9B�3B�9B�RB�9B�B�B��B��B��B��B��B��B��B��B�{B��B��B��B��B��B��B��B�{B�hB�\B�VB�PB�=B�7B�B|�By�Bx�Bw�Bt�Bt�Bs�Bs�Bq�Bp�Bn�Bk�BjBiyBhsBgmBgmBffBffBe`BcTBbNB_;B[#BW
BW
BVBS�BR�BR�BR�BS�BR�BR�BP�BJ�BD�B?}B>wB=qB<jB:^B7LB49B1'B1'B0!B49B2-B/B0!B/B.B.B-B-B-B+B,B/BM�B[#BS�BL�BJ�BB�B7LB.B-B-B.B0!B2-B=qBI�BT�BS�B^5B`BBW
BM�BD�BC�BD�BC�BA�B;dB:^B9XB;dB>wBF�BJ�BM�BN�BM�BO�BK�BM�BS�BW
BaHBcTBhsBo�Bs�Bx�B�B�VB�bB��B��B��B��B�oB��B��B��B��B�'B�9B�dB�}B�qB�}BŢB��B��B��B�5B�`B�HB�B��B��B��B�B�/B�BB�ZB�yB�B�B�B�B�B�B�B�B�B�B�B��B��B��B	B	B	B	B	B	%B	PB	VB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	!�B	%�B	'�B	)�B	,B	/B	2-B	33B	49B	5?B	<jB	D�B	E�B	C�B	B�B	E�B	H�B	J�B	I�B	F�B	F�B	J�B	Q�B	\)B	^5B	aHB	cTB	dZB	e`B	gmB	hsB	iyB	jB	l�B	n�B	o�B	o�B	s�B	s�B	s�B	u�B	v�B	v�B	y�B	}�B	}�B	~�B	� B	� B	~�B	�B	�B	�%B	�+B	�+B	�1B	�1B	�=B	�JB	�JB	�PB	�\B	�\B	�PB	�7B	�=B	�=B	�=B	�=B	�JB	�PB	�PB	�PB	�VB	�hB	�uB	�{B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�!B	�-B	�9B	�FB	�FB	�FB	�LB	�RB	�XB	�^B	�^B	�^B	�dB	�jB	�qB	��B	��B	B	ÖB	ĜB	ŢB	ƨB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B
+B
�B
"�B
�B
(�B
9XB
>wB
A�B
E�B
K�B
R�B
YB
^5B
ffB
iyB
jB
m�B
p�B
s�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B�B�fB��B��B��B��B��B�uB�oB�oB�oB�oB�oB�uB�oB�uB�uB��B��B��B��B��B��B��B��B��B��B�B�B�B��B�B�!B�?B�FB�FB�FB�?B�LB�LB�^B��BŢBB�}BƨB�/B�ZB�5BǮB�}BǮB�XB�PB�oB�uB�PB�oB�VB{�Bk�B9XB�BVBB��B�B�B��B
=B\BuBoBoB{B��B�B�NB�/B�}B��B�JB�Bv�BK�B:^B#�B�B�BuB
�B
�B
�9B
��B
��B
�\B
�+B
�B
ffB
\)B
S�B
D�B
;dB
;dB
"�B
hB
  B	�B	�mB	��B	ǮB	�XB	�9B	�-B	��B	��B	��B	��B	�B	u�B	q�B	hsB	M�B	A�B	&�B	�B	�B	�B	�B	oB	VB	
=B	+B��B�B�B�B�B�ZB�NB�#B�B��B��B��B��BǮBĜB�XB�?B�-B�-B�-B�-B�9B�?B�?B�LB�^B�FB�-B�'B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B�bB�\B�VB�JB�PB�7B�Bz�Bz�Bz�Bw�Bv�Bt�Bt�Bs�Bs�Br�Bn�Bn�Bk�BjBiyBhsBgmBgmBffBffBffBbNBaHBaHBZBW
BW
BVBVBVBW
BS�BS�BT�BN�BH�B@�B?}B>wB=qB<jB9XB8RB33B2-B33B8RB49B/B1'B0!B/B/B.B.B/B/B.B)�BK�B^5BZBN�BM�BG�B=qB2-B1'B/B/B1'B1'B;dBI�BYBR�B^5Be`B[#BR�BE�BC�BF�BD�BD�B>wB<jB;dB<jB>wBF�BL�BO�BP�BP�BQ�BL�BL�BS�BW
BdZBcTBiyBo�Br�Bv�B�B�VB�\B��B��B��B��B��B��B��B��B��B�-B�FB�jB��B�}B��BŢB��B��B��B�5B�fB�fB�B��B��B��B�B�5B�HB�fB�yB�B�B�B�B�B�B�B�B�B�B�B��B��B��B	B	B	B	B	B	%B	PB	VB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	"�B	%�B	'�B	+B	-B	/B	2-B	33B	5?B	5?B	;dB	E�B	G�B	D�B	B�B	E�B	H�B	K�B	K�B	H�B	F�B	I�B	P�B	\)B	^5B	bNB	dZB	dZB	ffB	gmB	hsB	iyB	jB	l�B	o�B	p�B	p�B	s�B	t�B	s�B	v�B	v�B	w�B	y�B	}�B	}�B	~�B	�B	�B	� B	�B	�B	�%B	�+B	�+B	�1B	�7B	�=B	�JB	�JB	�VB	�bB	�hB	�bB	�=B	�DB	�=B	�DB	�DB	�JB	�PB	�VB	�VB	�\B	�hB	�uB	�uB	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�!B	�-B	�9B	�LB	�LB	�FB	�LB	�RB	�XB	�^B	�^B	�^B	�jB	�qB	�qB	��B	��B	B	ĜB	ŢB	ƨB	ƨB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B
+B
�B
"�B
�B
(�B
9XB
>wB
A�B
E�B
K�B
R�B
YB
^5B
ffB
iyB
jB
n�B
p�B
t�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 <#�
<#�
<D��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<49X<#�
<#�
<#�
<#�
<#�
<u<#�
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
<#�
<#�
<#�
<e`B<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0.1 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201031446352012010314463520120103144635  AO  ARGQ                                                                        20111130135507  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130135507  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120103144635  IP                  G�O�G�O�G�O�                