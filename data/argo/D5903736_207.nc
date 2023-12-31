CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-01-11T18:02:30Z creation      
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
resolution        :�o     �  q�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    y�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �|   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �|   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �t   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �    HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �0   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �4   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �D   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �H   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �L   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �PArgo profile    3.1 1.2 19500101000000  20180111180230  20190604094031  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  4051                            2C  D   APEX                            5368                            041511                          846 @�C����]1   @�Cڃ9Q�@4�E�����d}���o1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   B   B   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B���B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D.��D/y�D0  D0� D1  D1� D2  D2�fD3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt�fDy^�D��=D�K�D��{D�њD��D�:=D�x D��\D�
�D�?�D��\DǾD�{D�1�D�t)D���D�HD�HRD�|{D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�33@���@���AffA>ffA^ffA~ffA�33A�33A�33A�33A�33A�33A�33A�33B��B��B��B��B'��B/��B7��B?��BG��BO��BW��B_��Bg��Bo��Bw��B��B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B�  B���B�B���B���B���C�fC�fC�fC�fC	�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC!�fC#�fC%�fC'�fC)�fC+�fC-�fC/�fC1�fC3�fC5�fC7�fC9�fC;�fC=�fC?�fCA�fCC�fCE�fCG�fCI�fCK�fCM�fCO�fCQ�fCS�fCU�fCW�fCY�fC[�fC]�fC_�fCa�fCc�fCe�fCg�fCi�fCk�fCm�fCo�fCq�fCs�fCu�fCw�fCy�fC{�fC}�fC�fC��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C�  C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3D y�D ��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��D	y�D	��D
y�D
��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��D y�D ��D!y�D!��D"y�D"��D#y�D#��D$y�D$��D%y�D%��D&y�D&��D'y�D'��D(y�D(��D)y�D)��D*y�D*��D+y�D+��D,y�D,��D-y�D-��D.y�D.�4D/s4D/��D0y�D0��D1y�D1��D2� D2��D3y�D3��D4y�D4��D5y�D5��D6y�D6��D7y�D7��D8y�D8��D9y�D9��D:y�D:��D;y�D;��D<y�D<��D=y�D=��D>y�D>��D?y�D?��D@y�D@��DAy�DA��DBy�DB��DCy�DC��DDy�DD��DEy�DE��DFy�DF��DGy�DG��DHy�DH��DIy�DI��DJy�DJ��DKy�DK��DLy�DL��DMy�DM��DNy�DN��DOy�DO��DPy�DP��DQy�DQ��DRy�DR��DSy�DS��DTy�DT��DUy�DU��DVy�DV��DWy�DW��DXy�DX��DYy�DY��DZy�DZ��D[y�D[��D\y�D\��D]y�D]��D^y�D^��D_y�D_��D`y�D`��Day�Da��Dby�Db��Dcy�Dc��Ddy�Dd��Dey�De��Dfy�Df��Dgy�Dg��Dhy�Dh��Diy�Di��Djy�Dj��Dky�Dk��Dly�Dl��Dmy�Dm��Dny�Dn��Doy�Do��Dpy�Dp��Dqy�Dq��Dry�Dr��Dsy�Ds��Dty�Dt� DyXRD��
D�HRD��HD��gD�{D�7
D�t�D��)D��D�<{D��)DǺ�D�	HD�.�D�p�D࿮D�D�ED�yHD���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�|�AɑhAə�Aɕ�Aɕ�Aɕ�Aɕ�Aɛ�Aə�Aə�Aə�Aə�Aɝ�Aɛ�Aɗ�AɑhAɓuAɅA��A�XA�A�l�A�VA�5?Aƺ^AƇ+A�jA�dZA�XA�33A�"�A�VA��A���A�ƨAžwAŴ9AŰ!AŰ!Aũ�Aş�Aś�Ař�Aŕ�AōPAŏ\Aŗ�AōPA�n�A�A�Aę�A�E�A���A��mA�r�A��#A�v�A���A�p�A�+A�~�A�ZA�XA���A��A�v�A���A���A��+A�S�A�dZA�A�bNA�Q�A���A�G�A���A�=qA�~�A��RA�ƨA� �A�VA��7A�bNA�C�A���A�%A�/A���A�9XA��DA���A��9A��!A�(�A���A��A�~�A���A�v�A���A�r�A���A�C�A��`A��/A�r�A�K�A��A�r�A��jA��A��A���A��!A~�/A~1'A{ƨAvbArffAp�Amx�Ai�mAhI�Ag�Ad�!Ab�/Aal�A`ĜA`bA^bNA\=qA[+AZ{AYS�AW�AV=qAU��AUAU`BATQ�AS�-AQ�^APffAPE�AO��ANM�AL��AK&�AJJAI33AHȴAG��AF��AEt�AC?}ABjAA/A?&�A=K�A;��A;�A:M�A9�A8M�A8JA7oA5p�A3�mA3S�A1ƨA.�`A-��A-�-A-�7A-�A,JA+&�A*{A(�`A({A'�A%�
A$�A#%A!\)A bNAhsA�jA-A�yA �AO�A1A��A7LA`BAbNA�PA�AI�A�TA%A�DA�;A/AA�A�A�A��A
I�A	t�A�A��A�A�A�A$�A��A1'A�wA��A�PA
=A bNA =q@��m@�=q@�1'@��u@�D@���@홚@�1'@�G�@��@��@�@�9@�O�@�M�@�ȴ@��@��/@���@���@ش9@ם�@��y@�@Չ7@�hs@�G�@���@��;@���@�r�@��@·+@̼j@�33@ʧ�@ɉ7@ȴ9@�I�@��y@ũ�@�&�@�%@���@ċD@��@�ƨ@�S�@��@��7@�I�@���@�/@��D@���@�7L@�G�@�G�@�p�@���@���@���@�%@�l�@�@��+@�hs@���@�z�@��m@�ƨ@��@��@��@�M�@���@�z�@�K�@�33@���@�E�@�-@���@���@�+@���@�=q@���@��#@�X@��@��m@��@���@�33@�
=@�o@��@�@�K�@���@�  @���@�b@�b@�  @���@�V@�-@��@�G�@�?}@��@���@�Q�@�  @���@�+@��@�hs@�/@��@���@���@�j@��@��w@���@�\)@�+@��@���@�=q@���@��7@�/@��j@�I�@�1@��m@���@�ƨ@�C�@���@��\@�^5@�$�@��^@�?}@���@���@�Z@�b@���@��y@���@�{@���@�@���@���@���@��7@�x�@�G�@�&�@���@��9@�Q�@�1@�\)@��@�E�@���@�X@�V@���@�Ĝ@���@�z�@�j@�Q�@�b@��F@�dZ@�C�@�33@�o@�n�@�{@��T@��#@��#@��#@��#@�@�p�@�O�@�G�@���@��9@��u@��@�|�@�\)@�l�@��@�|�@�@��!@�M�@�-@�J@�@���@��#@��-@���@�hs@�hs@�p�@�x�@�p�@�hs@�O�@�/@�&�@���@��/@��j@�z�@���@�t�@�\)@�+@�33@�K�@�K�@�"�@���@�M�@�$�@��T@��-@���@���@���@���@��^@���@���@�O�@�G�@��@���@���@��@���@��D@�r�@�Z@���@��X@~ں@r�m@j �@ap�@\:�@V)�@O�;@G�@@��@;j�@5&�@/�@)%F@"L0@�t@L0@�@��@�F@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A�|�AɑhAə�Aɕ�Aɕ�Aɕ�Aɕ�Aɛ�Aə�Aə�Aə�Aə�Aɝ�Aɛ�Aɗ�AɑhAɓuAɅA��A�XA�A�l�A�VA�5?Aƺ^AƇ+A�jA�dZA�XA�33A�"�A�VA��A���A�ƨAžwAŴ9AŰ!AŰ!Aũ�Aş�Aś�Ař�Aŕ�AōPAŏ\Aŗ�AōPA�n�A�A�Aę�A�E�A���A��mA�r�A��#A�v�A���A�p�A�+A�~�A�ZA�XA���A��A�v�A���A���A��+A�S�A�dZA�A�bNA�Q�A���A�G�A���A�=qA�~�A��RA�ƨA� �A�VA��7A�bNA�C�A���A�%A�/A���A�9XA��DA���A��9A��!A�(�A���A��A�~�A���A�v�A���A�r�A���A�C�A��`A��/A�r�A�K�A��A�r�A��jA��A��A���A��!A~�/A~1'A{ƨAvbArffAp�Amx�Ai�mAhI�Ag�Ad�!Ab�/Aal�A`ĜA`bA^bNA\=qA[+AZ{AYS�AW�AV=qAU��AUAU`BATQ�AS�-AQ�^APffAPE�AO��ANM�AL��AK&�AJJAI33AHȴAG��AF��AEt�AC?}ABjAA/A?&�A=K�A;��A;�A:M�A9�A8M�A8JA7oA5p�A3�mA3S�A1ƨA.�`A-��A-�-A-�7A-�A,JA+&�A*{A(�`A({A'�A%�
A$�A#%A!\)A bNAhsA�jA-A�yA �AO�A1A��A7LA`BAbNA�PA�AI�A�TA%A�DA�;A/AA�A�A�A��A
I�A	t�A�A��A�A�A�A$�A��A1'A�wA��A�PA
=A bNA =q@��m@�=q@�1'@��u@�D@���@홚@�1'@�G�@��@��@�@�9@�O�@�M�@�ȴ@��@��/@���@���@ش9@ם�@��y@�@Չ7@�hs@�G�@���@��;@���@�r�@��@·+@̼j@�33@ʧ�@ɉ7@ȴ9@�I�@��y@ũ�@�&�@�%@���@ċD@��@�ƨ@�S�@��@��7@�I�@���@�/@��D@���@�7L@�G�@�G�@�p�@���@���@���@�%@�l�@�@��+@�hs@���@�z�@��m@�ƨ@��@��@��@�M�@���@�z�@�K�@�33@���@�E�@�-@���@���@�+@���@�=q@���@��#@�X@��@��m@��@���@�33@�
=@�o@��@�@�K�@���@�  @���@�b@�b@�  @���@�V@�-@��@�G�@�?}@��@���@�Q�@�  @���@�+@��@�hs@�/@��@���@���@�j@��@��w@���@�\)@�+@��@���@�=q@���@��7@�/@��j@�I�@�1@��m@���@�ƨ@�C�@���@��\@�^5@�$�@��^@�?}@���@���@�Z@�b@���@��y@���@�{@���@�@���@���@���@��7@�x�@�G�@�&�@���@��9@�Q�@�1@�\)@��@�E�@���@�X@�V@���@�Ĝ@���@�z�@�j@�Q�@�b@��F@�dZ@�C�@�33@�o@�n�@�{@��T@��#@��#@��#@��#@�@�p�@�O�@�G�@���@��9@��u@��@�|�@�\)@�l�@��@�|�@�@��!@�M�@�-@�J@�@���@��#@��-@���@�hs@�hs@�p�@�x�@�p�@�hs@�O�@�/@�&�@���@��/@��j@�z�@���@�t�@�\)@�+@�33@�K�@�K�@�"�@���@�M�@�$�@��T@��-@���@���@���@���@��^@���@���@�O�@�G�@��@���@���@��@���@��D@�r�@�ZG�O�@��X@~ں@r�m@j �@ap�@\:�@V)�@O�;@G�@@��@;j�@5&�@/�@)%F@"L0@�t@L0@�@��@�F@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB}�B}�B}�B}�B}�B}�B}�B}�B~�B~�B� B� B� B~�B� B�B�B�1B�^B��B�HB�yB�B��BbB �B%�B'�B(�B)�B.B49B;dB=qB>wB?}B@�B@�BA�BB�BC�BD�BD�BE�BK�BR�BW
B\)B^5BaHBe`BhsBcTBdZBdZBiyBq�Bt�Bs�Bp�Bp�Bt�Bs�Bu�Bv�Bw�Bv�Bs�Bp�BhsB}�B�B�B�B� B{�Bp�BZBF�B=qB33B+B!�B�BDB��B�B�B�fB�BB�B��B��B��B��B�VB�%B|�Br�Be`BQ�B?}B33B.B%�B �BuBPB	7BB
��B
�B
�dB
��B
�B
z�B
n�B
hsB
W
B
2-B
�B
oB
B	�B	�`B	�/B	��B	ƨB	�wB	�^B	�FB	�B	��B	��B	��B	�oB	�=B	�B	�B	�B	~�B	x�B	s�B	jB	cTB	bNB	_;B	W
B	J�B	A�B	@�B	=qB	:^B	5?B	0!B	'�B	�B	�B	oB	DB	B��B��B��B�B�B�sB�HB�B��B��BB�RB�?B�?B�3B�'B�!B�B��B��B��B��B��B��B�uB�VB�JB�1B�B� B}�B|�Bz�By�Bx�Bv�Bt�Bs�Bs�Bs�Bs�Br�Bq�Bp�Bn�Bm�Bl�Bp�Bo�Bn�Bm�BiyBe`BffBm�Bo�Bp�Bu�By�Bx�Bw�Bv�Bu�Br�Br�Br�Bq�Br�Bn�BiyBaHBR�BK�BH�BH�BI�BJ�BM�BR�BM�BD�BE�BI�BK�BK�BK�BL�BP�BR�BVBhsBm�Bu�Bw�By�B|�B}�B�B�%B�%B�1B�+B�+B�+B�%B�+B�7B�PB��B��B��B��B��B��B��B��B��B��B��B��B�B�!B�'B�-B�3B�LB�RB�^B�}B��B��BÖBĜBŢBŢBĜBĜBŢBŢBŢB��B��B��B��B�B�/B�TB�yB�B�sB�mB�sB�B�B�B�B�B��B	  B	B	B	B	B	1B	
=B	hB	uB	�B	�B	�B	�B	�B	'�B	-B	.B	/B	2-B	49B	7LB	;dB	@�B	B�B	D�B	C�B	@�B	D�B	F�B	M�B	R�B	W
B	XB	[#B	^5B	_;B	bNB	dZB	ffB	iyB	k�B	m�B	p�B	s�B	v�B	x�B	x�B	y�B	y�B	z�B	}�B	� B	�B	�B	�B	�B	�1B	�=B	�JB	�PB	�VB	�bB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�!B	�-B	�9B	�FB	�FB	�LB	�RB	�XB	�^B	�dB	�jB	�jB	�qB	��B	ÖB	ĜB	ƨB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�/B	�5B	�;B	�HB	�NB	�NB	�NB	�ZB	�`B	�`B	�fB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
%B
%B
%B
+B
+B
+B
�B
vB
�B
-�B
6zB
<�B
A�B
EmB
J	B
P.B
U�B
Y1B
]dB
b�B
gmB
l"B
q'B
tB
yXB
|�B
�'B
�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  Bp<Bp<Bp<Bp:Bp9Bp:Bp?Bp;Bq@BqABrHBrIBrFBq?BrJBvaBuUBzyB��B�)BӆB۴B��B��B�B�BB$B)B2B JB&mB-�B/�B0�B1�B2�B2�B3�B4�B5�B6�B6�B7�B=�BE"BI@BN[BPhBStBW�BZ�BU�BV�BV�B[�Bc�Bf�Be�Bb�Bb�Bf�Be�Bg�Bh�BjBi Be�Bb�BZ�Bp&Bt>BuLBuHBr0BnBb�BLUB8�B/�B%rB@B	B	�B��B�<B��B��BزBҎB�hB�@B�B�KB�B��Bx|BoDBeBW�BDEB1�B%�B sBEB#B�B
��B
��B
�uB
�$B
́B
��B
�4B
v�B
mUB
aB
Z�B
I�B
$�B
4B
�B	��B	�%B	��B	ϴB	�pB	�6B	�B	��B	��B	��B	�_B	�>B	� B	�B	|�B	w�B	v�B	t�B	q�B	klB	fNB	]B	U�B	T�B	Q�B	I�B	=]B	4'B	3B	0B	,�B	'�B	"�B	�B	TB	6B	B��B��B�B�B�mB�FB�,B�B��BʼBÔB�uB�>B�B��B��B��B��B��B��B��B��B��B�xB�`B�CB�.B�B Bz�Bw�Br�Bp�Bo�Bm�Bl�Bk�Bi�BgwBfuBfrBfrBfrBemBdiBccBaUB`RB_FBcaBb\BaWB`NB\:BX%BY'B`RBb_BceBh�Bl�Bk�Bj�Bi�Bh�BeoBenBerBdhBerBa[B\=BT	BE�B>�B;{B;yB<�B=�B@�BE�B@�B7fB8nB<�B>�B>�B>�B?�BC�BE�BH�B[9B`YBh�Bj�Bl�Bo�Bp�Bw�Bx�Bx�Bz�By�By�By�Bx�By�B{�B�B�IB�NB�RB�iB�gB�nB�oB�nB�_B�dB�|B��B��B��B��B��B��B�B�B�!B�<B�DB�HB�VB�]B�cB�bB�]B�^B�`B�cB�_B�B��B��BB��B��B�B�5B�;B�.B�-B�-B�:B�KB�GB�SB�uB�B�B��B��B��B��B��B��B	#B	,B	
GB	XB	bB	iB	fB	�B	�B	 �B	!�B	$�B	&�B	*B	.B	37B	5AB	7QB	6JB	36B	7SB	9^B	@�B	E�B	I�B	J�B	M�B	P�B	Q�B	UB	WB	YB	\+B	^4B	`@B	cSB	fhB	izB	k�B	k�B	l�B	l�B	m�B	p�B	r�B	s�B	s�B	t�B	v�B	z�B	|�B	~�B	�B	�B	�B	�+B	�8B	�GB	�QB	�WB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�	B	�B	�B	�B	�B	�/B	�AB	�FB	�QB	�[B	�nB	�oB	�yB	B	ÐB	ĕB	ēB	ĕB	ƤB	ȪB	ȭB	ʷB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�B	�B	�B	�)B	�#B	�,B	�1B	�/B	�2B	�8B	�@B	�@B	�IB	�OB	�YB	�TB	�[B	�UB	�\B	�XB	�aB	�hB	�hB	�nB	�oB	�nB	�{B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��G�O�B
B
]B
 �B
)B
/WB
4xB
8B
<�B
B�B
H<B
K�B
PB
U�B
ZB
^�B
c�B
f�B
k�B
oAB
t�B
w�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.1 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            r =0.9997(+/-0.0001), vertically averaged dS =-0.013(+/-0.002) in PSS-78.                                                                                                                                                                                       Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Map scales: x=6,3; y=2,1. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                            201906040940312019060409403120190604094031  AO  ARCAADJP                                                                    20180111180230    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20180111180230  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20180111180230  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20190604094031  IP                  G�O�G�O�G�O�                