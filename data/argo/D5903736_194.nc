CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2017-08-31T00:01:17Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         (Matthew Alkire, University of Washington      @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7,   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  74   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7t   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     88   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8X   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8x   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           8|   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~            8�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    K�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  U�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ]�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  _�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    g�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  i�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  qt   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    yh   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {h   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �\   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �\   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �P   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �    HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �$   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �(   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �,Argo profile    3.1 1.2 19500101000000  20170831000117  20190604094029  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  4051                            2C  D   APEX                            5368                            041511                          846 @�"t���v1   @�"u�q��@5��+J�d���O�;1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   B   B   @y��@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@ffBH��BO��BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C�C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Cs�fCu�fCx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt�fDy��D��D�-D�m�D���D��D�4)D�m�D�˅D��D�D�D��=D���D�D�*�Dڇ�D�\D� �D�5D�qH11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @s34@���@���AffA>ffA^ffA~ffA�33A�33A�33A�33A�33A�33A�33A�33B��B��B��B��B'��B/��B7��B@  BHfgBO34BW��B_��Bg��Bo��Bw��B��B���B���B���B���B���B�  B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���C�fC�fC�fC�fC	�fC�fC�fC  C�fC�fC�fC�fC�fC�fC�fC�fC!�fC#�fC%�fC'�fC)�fC+�fC-�fC/�fC1�fC3�fC5�fC7�fC9�fC;�fC=�fC?�fCA�fCC�fCE�fCG�fCI�fCK�fCM�fCO�fCQ�fCS�fCU�fCW�fCY�fC[�fC]�fC_�fCa�fCc�fCe�fCg�fCi�fCk�fCm�fCo�fCq�fCs��Cu��Cw�fCy�fC{�fC}�fC�fC��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3D y�D ��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��D	y�D	��D
y�D
��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��D y�D ��D!y�D!��D"y�D"��D#y�D#��D$y�D$��D%y�D%��D&y�D&��D'y�D'��D(y�D(��D)y�D)��D*y�D*��D+y�D+��D,y�D,��D-y�D-��D.y�D.��D/y�D/��D0y�D0��D1y�D1��D2y�D2��D3y�D3��D4y�D4��D5y�D5��D6y�D6��D7y�D7��D8y�D8��D9y�D9��D:y�D:��D;y�D;��D<y�D<��D=y�D=��D>y�D>��D?y�D?��D@y�D@��DAy�DA��DBy�DB��DCy�DC��DDy�DD��DEy�DE��DFy�DF��DGy�DG��DHy�DH��DIy�DI��DJy�DJ��DKy�DK��DLy�DL��DMy�DM��DNy�DN��DOy�DO��DPy�DP��DQy�DQ��DRy�DR��DSy�DS��DTy�DT��DUy�DU��DVy�DV��DWy�DW��DXy�DX��DYy�DY��DZy�DZ��D[y�D[��D\y�D\��D]y�D]��D^y�D^��D_y�D_��D`y�D`��Day�Da��Dby�Db��Dcy�Dc��Ddy�Dd��Dey�De��Dfy�Df��Dgy�Dg��Dhy�Dh��Diy�Di��Djy�Dj��Dky�Dk��Dly�Dl��Dmy�Dm��Dny�Dn��Doy�Do��Dpy�Dp��Dqy�Dq��Dry�Dr��Dsy�Ds��Dty�Dt� Dy�qD��D�)�D�j�D��\D��D�0�D�j�D��RD� �D�A�D��
D�ιD�
�D�'\Dڄ{D�)D��qD�1�D�n11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��+A��A�x�A�z�A�v�A�t�A�p�A�t�A�r�A�r�A�p�A�l�A�ffA�ffA�dZA�dZA�dZA�dZA�bNA�bNA�dZA�bNA�ZAާ�A�33A�K�A��A�A��A��A�VAщ7A��mA�5?A���A��;A��mA�n�AʑhA��AɬA�t�A���A�jA���A�C�A�~�A��A�JA���A���A��A�^5A�I�A�C�A�ȴA��^A��9A��A�$�A��A���A��;A�1'A�XA�1A��+A�VA�9XA��-A�1'A��A�&�A�v�A��!A���A��A���A�;dA�33A��A�K�A��uA��A�C�A��A�ffA��HA�I�A�G�A�$�A���A��!A�z�A��A�5?A�VA�(�A�t�A��A�5?A��7A���A�
=A��yA��A�M�A���A���A��A�%A�oA���A�E�A�\)A���A��uA�A}�PA{�^AxȴAvVAtv�Aq�mAp^5An~�Al1Ah��Af�Acl�A_��A]�;A^JA^  A]dZA[dZAZȴAY�;AX�AU��AT��ARZAPĜAP�+AN�AM�wAL��AK`BAIx�AGG�AF��AFA�AD$�AB��AA�wA?oA=t�A<�/A;7LA:��A:(�A9�-A9+A7�A6 �A4��A4-A2��A0��A-��A-%A+33A*5?A)�
A(�A'��A&��A%�#A%A$��A#�-A ��A 5?A��A+A~�A�PAI�AȴA�mA\)A~�A�;AoA�FA=qA��A�A�mA"�A  A��A?}A
JA�A-A�A��AZAdZA�A9XA �!@���@��@�`B@�V@�Q�@�\)@��+@�Z@���@�-@�%@�5?@�O�@��@�F@��y@��T@�V@�1@�-@�X@��@�+@�$�@��#@��@�x�@���@��@�+@�"�@��H@�X@��D@��@�(�@ޏ\@��@܃@ە�@��#@ؓu@�ƨ@��#@ԋD@ӶF@�
=@�^5@�-@��@���@��@��@�K�@Ο�@�J@���@͙�@���@˥�@ɩ�@��/@ȋD@��@�\)@�ȴ@��@�p�@��@ÍP@�33@�^5@�hs@�&�@��`@�bN@��F@�o@���@���@�G�@��j@�Q�@�l�@�v�@�X@��j@�z�@�9X@�  @�"�@���@��!@�G�@� �@���@�l�@��@�ff@���@��@��@�b@��@��@�^5@���@���@��h@�`B@���@���@�Q�@�(�@�ƨ@�@���@�V@�{@���@���@��@�z�@�bN@�z�@��@�j@�Q�@�Q�@� �@�1@�t�@�o@�M�@�J@���@���@��
@���@�5?@���@��@��h@���@���@�Q�@�9X@��@��@�\)@�K�@�o@���@��y@��\@�$�@��h@���@���@�I�@��w@���@��P@�|�@�;d@�ȴ@�ȴ@���@���@���@��@���@�x�@�hs@�`B@�?}@��@�V@�%@���@���@���@��@���@��`@��@�&�@�&�@��@�Q�@��m@��P@�dZ@�+@���@���@�O�@�?}@�hs@���@�@���@��-@���@��7@�p�@�7L@���@�bN@�b@��;@��F@�K�@�o@���@�=q@��#@�@��-@�x�@�O�@��@��`@��/@��9@��@�I�@�9X@�1'@� �@��@�  @�|�@�\)@���@�E�@��@���@��@�x�@�G�@��@�%@��/@���@���@��j@��@�j@�9X@�1'@� �@��@��;@�\)@�33@��@��@�o@��@���@���@�n�@�n�@�M�@�5?@�J@���@��^@���@�p�@�X@�7L@�V@���@���@���@�6�@|�O@uN<@j��@b�'@Z�X@S��@M�@C�}@<�[@6�@0M@(2�@!��@ff@<�@�4@�@�-11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111   A��+A��A�x�A�z�A�v�A�t�A�p�A�t�A�r�A�r�A�p�A�l�A�ffA�ffA�dZA�dZA�dZA�dZA�bNA�bNA�dZA�bNA�ZAާ�A�33A�K�A��A�A��A��A�VAщ7A��mA�5?A���A��;A��mA�n�AʑhA��AɬA�t�A���A�jA���A�C�A�~�A��A�JA���A���A��A�^5A�I�A�C�A�ȴA��^A��9A��A�$�A��A���A��;A�1'A�XA�1A��+A�VA�9XA��-A�1'A��A�&�A�v�A��!A���A��A���A�;dA�33A��A�K�A��uA��A�C�A��A�ffA��HA�I�A�G�A�$�A���A��!A�z�A��A�5?A�VA�(�A�t�A��A�5?A��7A���A�
=A��yA��A�M�A���A���A��A�%A�oA���A�E�A�\)A���A��uA�A}�PA{�^AxȴAvVAtv�Aq�mAp^5An~�Al1Ah��Af�Acl�A_��A]�;A^JA^  A]dZA[dZAZȴAY�;AX�AU��AT��ARZAPĜAP�+AN�AM�wAL��AK`BAIx�AGG�AF��AFA�AD$�AB��AA�wA?oA=t�A<�/A;7LA:��A:(�A9�-A9+A7�A6 �A4��A4-A2��A0��A-��A-%A+33A*5?A)�
A(�A'��A&��A%�#A%A$��A#�-A ��A 5?A��A+A~�A�PAI�AȴA�mA\)A~�A�;AoA�FA=qA��A�A�mA"�A  A��A?}A
JA�A-A�A��AZAdZA�A9XA �!@���@��@�`B@�V@�Q�@�\)@��+@�Z@���@�-@�%@�5?@�O�@��@�F@��y@��T@�V@�1@�-@�X@��@�+@�$�@��#@��@�x�@���@��@�+@�"�@��H@�X@��D@��@�(�@ޏ\@��@܃@ە�@��#@ؓu@�ƨ@��#@ԋD@ӶF@�
=@�^5@�-@��@���@��@��@�K�@Ο�@�J@���@͙�@���@˥�@ɩ�@��/@ȋD@��@�\)@�ȴ@��@�p�@��@ÍP@�33@�^5@�hs@�&�@��`@�bN@��F@�o@���@���@�G�@��j@�Q�@�l�@�v�@�X@��j@�z�@�9X@�  @�"�@���@��!@�G�@� �@���@�l�@��@�ff@���@��@��@�b@��@��@�^5@���@���@��h@�`B@���@���@�Q�@�(�@�ƨ@�@���@�V@�{@���@���@��@�z�@�bN@�z�@��@�j@�Q�@�Q�@� �@�1@�t�@�o@�M�@�J@���@���@��
@���@�5?@���@��@��h@���@���@�Q�@�9X@��@��@�\)@�K�@�o@���@��y@��\@�$�@��h@���@���@�I�@��w@���@��P@�|�@�;d@�ȴ@�ȴ@���@���@���@��@���@�x�@�hs@�`B@�?}@��@�V@�%@���@���@���@��@���@��`@��@�&�@�&�@��@�Q�@��m@��P@�dZ@�+@���@���@�O�@�?}@�hs@���@�@���@��-@���@��7@�p�@�7L@���@�bN@�b@��;@��F@�K�@�o@���@�=q@��#@�@��-@�x�@�O�@��@��`@��/@��9@��@�I�@�9X@�1'@� �@��@�  @�|�@�\)@���@�E�@��@���@��@�x�@�G�@��@�%@��/@���@���@��j@��@�j@�9X@�1'@� �@��@��;@�\)@�33@��@��@�o@��@���@���@�n�@�n�@�M�@�5?@�J@���@��^@���@�p�@�X@�7L@�V@���G�O�@���@�6�@|�O@uN<@j��@b�'@Z�X@S��@M�@C�}@<�[@6�@0M@(2�@!��@ff@<�@�4@�@�-11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�!B�!B�!B�!B�!B�!B�!B�!B�!B�!B�!B�!B�B�!B�B�B�B�!B�!B�!B�!B�!B�!B�
B9XB~�B�\B��B��B��B��B��B�B�'B�'B�?B�jB�qB��B��B�}B�wB�^B�qB��BÖBÖBĜBÖB��B�}B�qB�qB�wB��B�jB�dB�XB�XB�FB�B��B��B��B��B�oB�DBv�BiyBcTB^5B[#BQ�BB�B;dB6FB33B,B!�B�B	7BB��B��B�B�B��B��B��BB��B�wB�jB�XB�9B�B��B��B��B�bB�+B}�Bo�Be`BS�B<jB-B!�B�BJB
��B
�ZB
��B
�FB
��B
��B
x�B
p�B
_;B
M�B
33B
%�B
�B
\B
B	�B	�5B	ƨB	�XB	��B	�oB	�B	�=B	�=B	�1B	~�B	y�B	p�B	e`B	W
B	Q�B	J�B	D�B	B�B	=qB	7LB	33B	,B	"�B	�B	�B	�B	\B	DB	1B��B�B�B�B�B�B�yB�mB�NB�5B�#B�
B��B��BĜB��B�qB�XB�LB�9B�B�B�B��B��B��B��B��B��B��B��B��B�{B�hB�\B�VB�JB�DB�7B�%B�B{�Bx�Bu�Bp�Bl�BhsBffBe`BdZBcTBaHB_;B_;B_;B_;B^5B]/B]/B\)B\)B[#B[#BZBZBZBZBZBZB]/B]/B]/B]/B]/B^5B`BB_;BbNBcTBdZBe`BffBffBffBffBffBhsBhsBhsBgmBiyBiyBiyBhsBhsBhsBhsBiyBjBk�Bk�Bn�Br�Bw�B|�B�B�B�B�B�B�1B�=B�DB�JB�JB�PB�PB�hB��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�!B�-B�3B�9B�FB�LB�XB�^B��BƨBɺB��B��B��B��B��B�B��B�5B�sB�B�B�B�B��B��B��B	  B	B	B		7B	DB	JB	VB	VB	hB	oB	oB	uB	{B	�B	�B	�B	�B	�B	!�B	$�B	%�B	'�B	(�B	)�B	,B	.B	0!B	1'B	5?B	:^B	:^B	:^B	<jB	=qB	<jB	<jB	A�B	E�B	G�B	K�B	M�B	P�B	Q�B	S�B	VB	XB	ZB	^5B	^5B	_;B	_;B	bNB	cTB	cTB	cTB	dZB	e`B	jB	n�B	p�B	q�B	r�B	u�B	z�B	|�B	� B	�B	�B	�%B	�7B	�DB	�JB	�PB	�VB	�bB	�bB	�hB	��B	��B	��B	��B	��B	��B	��B	�B	�!B	�-B	�3B	�FB	�XB	�dB	�jB	�wB	�wB	�wB	��B	ĜB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�/B	�HB	�NB	�HB	�NB	�ZB	�`B	�fB	�mB	�mB	�mB	�sB	�sB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
B
B
%B
%B
+B
+B
+B
1B
1B
	7B
�B
�B
�B
(
B
/ B
5tB
:xB
B�B
HB
L0B
QhB
UMB
Y�B
\�B
b�B
h�B
k�B
q'B
v�B
{11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111   B�WB�XB�VB�XB�XB�YB�UB�XB�UB�VB�VB�XB�OB�UB�SB�QB�QB�UB�VB�UB�VB�VB�VB�:B.�Bt#B��B��B��B��B��B��B�BB�KB�JB�cB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�|B�{B�sB�9B�B��B��B��B��B�tBk�B^�BX�BSdBPYBG$B7�B0�B+�B(iB!DBB
�B�sB�KB�#B��B߾B�CB�2B�B�	B��B��B��B��B��B��B�UB�B�B��B��B|tBs<Bd�BZ�BICB1�B"_BB
�B�B
�7B
ٳB
�,B
��B
�B
��B
n6B
fB
T�B
C<B
(�B
MB
B
�B	�|B	�B	ӥB	�B	��B	�vB	��B	z�B	�B	�B	}�B	ttB	oWB	f#B	Z�B	L�B	GmB	@AB	:B	8B	2�B	,�B	(�B	!�B	XB	=B	5B	B	�B	 �B��B�dB�9B�/B�B�B�B�B��B��B��BЭB̖BȄB�YB�,B�B�B��B��B��B��B��B��B��B��B�eB�GB�2B�2B�)B�*B�B�B�B��B��B��B��B~�B{�Bw�Bq�BnpBk_Bf?Bb(B^B\BZ�BY�BX�BV�BT�BT�BT�BT�BS�BR�BR�BQ�BQ�BP�BP�BO�BO�BO�BO�BO�BO�BR�BR�BR�BR�BR�BS�BU�BT�BW�BX�BY�B[B\B\B\B\B\B^B^B^B]B_B_B_B^B^B^B^B_B` Ba&Ba)Bd;BhPBmoBr�Bv�Bw�Bx�Bx�Bz�B}�B�B��B��B��B��B��B�B�<B�KB�MB�PB�YB�dB�jB�mB�|B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�"B�GB�ZB�uB�BƄBǈBʙB˟BʛB��B�B�#B�*B�8B�HB�lB�B�B��B��B��B��B	 �B	�B	�B	�B	�B		B	B		B	
B	B	%B	4B	?B	WB	cB	uB	{B	�B	�B	�B	!�B	#�B	%�B	&�B	*�B	/�B	/�B	/�B	2B	3B	1�B	2B	7B	;7B	=EB	A[B	CjB	FyB	GB	I�B	K�B	M�B	O�B	S�B	S�B	T�B	T�B	W�B	X�B	X�B	X�B	Y�B	Z�B	`B	d+B	f8B	gBB	hCB	kUB	prB	r�B	u�B	w�B	w�B	{�B	~�B	��B	��B	��B	��B	��B	��B	��B	�B	�1B	�BB	�^B	�tB	�tB	�|B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�)B	�;B	�;B	�LB	�ZB	�XB	�]B	�_B	�dB	�pB	�wB	�yB	�zB	ɅB	ʊB	ʋB	ˍB	͛B	ΤB	ϦB	һB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�	B	�B	�B	�B	�B	�!B	�*B	�'B	�-B	�,B	�/B	�/B	�,B	�.B	�0B	�*B	�-B	�3B	�1B	�8B	�5B	�?B	�RB	�^B	�bB	�nB	�|B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��G�O�B
tB
2B
>B
�B
$�B
*�B
/�B
8JB
=�B
A�B
F�B
J�B
OUB
R�B
XB
^0B
aB
f�B
l�B
p�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.1 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            r =0.9997(+/-0), vertically averaged dS =-0.01(+/-0.001) in PSS-78.                                                                                                                                                                                             Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Map scales: x=6,3; y=2,1. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                            201906040940292019060409402920190604094029  AO  ARCAADJP                                                                    20170831000117    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20170831000117  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20170831000117  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20190604094029  IP                  G�O�G�O�G�O�                