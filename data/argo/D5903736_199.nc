CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2017-10-21T17:02:04Z creation      
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
_FillValue                    �PArgo profile    3.1 1.2 19500101000000  20171021170204  20190604094030  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  4051                            2C  D   APEX                            5368                            041511                          846 @�/]%� �1   @�/]�co@5�z�G��d�&�x��1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   B   B   @�ff@�  @���A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0ffB8  B@  BH  BP  BX  B`  Bh  Bp  BxffB�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt�3Dy��D���D�A�D�~D���D�3D�D{D�o
D��
D��D�F�D�s�D���D��
D�/�D�t�D��{D��D�>�D�w�D��q111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�33@���@���AffA>ffA^ffA~ffA�33A�33A�33A�33A�33A�33A�33A�33B��B��B��B��B'��B0  B7��B?��BG��BO��BW��B_��Bg��Bo��Bx  B��B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���C�fC�fC�fC�fC	�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC!�fC#�fC%�fC'�fC)�fC+�fC-�fC/�fC1�fC3�fC5�fC7�fC9�fC;�fC=�fC?�fCA�fCC�fCE�fCG�fCI�fCK�fCM�fCO�fCQ�fCS�fCU�fCW�fCY�fC[�fC]�fC_�fCa�fCc�fCe�fCg�fCi�fCk�fCm�fCo�fCq�fCs�fCu�fCw�fCy�fC{�fC}�fC�fC��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3D y�D ��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��D	y�D	��D
y�D
��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��D y�D ��D!y�D!��D"y�D"��D#y�D#��D$y�D$��D%y�D%��D&y�D&��D'y�D'��D(y�D(��D)y�D)��D*y�D*��D+y�D+��D,y�D,��D-y�D-��D.y�D.��D/y�D/��D0y�D0��D1y�D1��D2y�D2��D3y�D3��D4y�D4��D5y�D5��D6y�D6��D7y�D7��D8y�D8��D9y�D9��D:y�D:��D;y�D;��D<y�D<��D=y�D=��D>y�D>��D?y�D?��D@y�D@��DAy�DA��DBy�DB��DCy�DC��DDy�DD��DEy�DE��DFy�DF��DGy�DG��DHy�DH��DIy�DI��DJy�DJ��DKy�DK��DLy�DL��DMy�DM��DNy�DN��DOy�DO��DPy�DP��DQy�DQ��DRy�DR��DSy�DS��DTy�DT��DUy�DU��DVy�DV��DWy�DW��DXy�DX��DYy�DY��DZy�DZ��D[y�D[��D\y�D\��D]y�D]��D^y�D^��D_y�D_��D`y�D`��Day�Da��Dby�Db��Dcy�Dc��Ddy�Dd��Dey�De��Dfy�Df��Dgy�Dg��Dhy�Dh��Diy�Di��Djy�Dj��Dky�Dk��Dly�Dl��Dmy�Dm��Dny�Dn��Doy�Do��Dpy�Dp��Dqy�Dq��Dry�Dr��Dsy�Ds��Dty�Dt��Dy��D��gD�>�D�z�D���D�  D�AHD�k�D���D��D�C�D�pRD�њD���D�,{D�q�D��HD�qD�;�D�t{D��>111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��yA��yA��yA��A��yA��TA��HA��TA��mA��A��`A��`A��mA��A��A��A���A���A���A���A��A��yA��A��mA��/A��
A���A���A���A�ƨAٙ�A�oA��Aכ�A��Aԣ�A��A�&�A�ZA�1AɍPA��Aț�A���A�XA�;dA�+Aé�A�^5A�?}A�"�A�-A��yA�~�A��TA�{A�ffA��A�  A��A���A��!A�bNA���A�VA��A�E�A���A���A�S�A�?}A�jA��HA�p�A�G�A�+A�1A��mA�n�A��A�7LA���A���A�E�A��;A��7A���A�{A��jA��;A�XA�~�A�oA�K�A�VA���A�ƨA��A��#A��
A���A�t�A���A��/A�|�A��A��A�E�A���A�1'A���A�$�A��9A�l�A���A�M�A��hA�+A��jA��A��TA��A���A��A��
A�"�A�K�AdZA}�;A|{Az1Aw"�Av��Au\)An��Al{Aj1Ahz�Af�`Af=qAc�#A^~�AZ�AZ5?AY�mAYdZAW��AVJAR��AQ�PAOhsAMAK�
AKoAJz�AI�AHn�AF-AF  AB�yA?�-A=��A=O�A<5?A:��A9�-A8�A7O�A6jA4�yA3+A2-A1S�A/�A-�A-�A,=qA*�A)�A(�yA&ȴA%�wA$ĜA$A�A#�PA"��A!7LA VA�`A��A�HA=qAdZA%A�mAVAG�AƨAȴA��A?}A�HAVA�^A��AQ�A�Av�A��A33AA�A
{A��AG�A�wA%A��A��A�9AA�A�mA��A ��@�+@��@�|�@�9X@�S�@�X@��@�G�@�M�@�&�@웦@�9X@�|�@���@���@�V@�/@���@�9@�A�@�F@��y@�h@ߝ�@�5?@�I�@�=q@�b@�ƨ@�ff@�X@�A�@��m@ҧ�@�@���@�bN@�l�@��y@�J@ͺ^@�z�@ʰ!@�$�@�7L@��@��/@ȓu@ǥ�@�S�@�M�@���@�  @�o@�M�@�@���@�X@�Ĝ@���@�
=@�@��@���@�ƨ@��@�l�@��y@�{@��^@��@���@��w@�33@��@��\@�-@���@�Q�@���@���@���@���@���@��h@�X@�Q�@��
@��P@�^5@���@�x�@��/@��;@��w@��F@�33@���@�E�@��@��h@�X@��@�%@���@��j@�j@�Z@��m@��w@��F@��F@���@��@�l�@���@�=q@�E�@�-@�@�7L@�z�@���@���@�\)@�;d@���@��!@�M�@��@�hs@�X@�X@�G�@��`@���@��9@���@�j@�Q�@��
@���@���@��@�"�@��@�~�@�=q@���@��#@�@��7@�hs@�`B@�O�@�?}@���@���@���@���@�r�@� �@��@��@�t�@�l�@�S�@�+@���@�ȴ@���@��\@�v�@�V@��@���@�`B@��@�V@��j@�Z@�I�@�9X@�b@���@���@�|�@�C�@�@��R@�v�@�=q@�J@��@���@��-@��7@�hs@�`B@�G�@�/@�&�@�%@���@�z�@�I�@�1@���@��w@�|�@�\)@�+@��@���@���@��+@�=q@�@��#@���@�@���@�x�@�x�@��@���@��j@��9@��@��u@�I�@�9X@� �@�b@��;@���@��@�dZ@�S�@�"�@��@�
=@�@�
=@�
=@��H@���@�5?@���@�@��^@��7@�x�@�`B@�?}@���@���@�/@�V@���@��@�(�@��w@��@�|�@�B[@��	@��a@zE�@o�r@g�@`�@Z��@R8�@JGE@C�@<_@4G@.��@)+�@#��@*0@z�@�m@�@
#:111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A��yA��yA��yA��A��yA��TA��HA��TA��mA��A��`A��`A��mA��A��A��A���A���A���A���A��A��yA��A��mA��/A��
A���A���A���A�ƨAٙ�A�oA��Aכ�A��Aԣ�A��A�&�A�ZA�1AɍPA��Aț�A���A�XA�;dA�+Aé�A�^5A�?}A�"�A�-A��yA�~�A��TA�{A�ffA��A�  A��A���A��!A�bNA���A�VA��A�E�A���A���A�S�A�?}A�jA��HA�p�A�G�A�+A�1A��mA�n�A��A�7LA���A���A�E�A��;A��7A���A�{A��jA��;A�XA�~�A�oA�K�A�VA���A�ƨA��A��#A��
A���A�t�A���A��/A�|�A��A��A�E�A���A�1'A���A�$�A��9A�l�A���A�M�A��hA�+A��jA��A��TA��A���A��A��
A�"�A�K�AdZA}�;A|{Az1Aw"�Av��Au\)An��Al{Aj1Ahz�Af�`Af=qAc�#A^~�AZ�AZ5?AY�mAYdZAW��AVJAR��AQ�PAOhsAMAK�
AKoAJz�AI�AHn�AF-AF  AB�yA?�-A=��A=O�A<5?A:��A9�-A8�A7O�A6jA4�yA3+A2-A1S�A/�A-�A-�A,=qA*�A)�A(�yA&ȴA%�wA$ĜA$A�A#�PA"��A!7LA VA�`A��A�HA=qAdZA%A�mAVAG�AƨAȴA��A?}A�HAVA�^A��AQ�A�Av�A��A33AA�A
{A��AG�A�wA%A��A��A�9AA�A�mA��A ��@�+@��@�|�@�9X@�S�@�X@��@�G�@�M�@�&�@웦@�9X@�|�@���@���@�V@�/@���@�9@�A�@�F@��y@�h@ߝ�@�5?@�I�@�=q@�b@�ƨ@�ff@�X@�A�@��m@ҧ�@�@���@�bN@�l�@��y@�J@ͺ^@�z�@ʰ!@�$�@�7L@��@��/@ȓu@ǥ�@�S�@�M�@���@�  @�o@�M�@�@���@�X@�Ĝ@���@�
=@�@��@���@�ƨ@��@�l�@��y@�{@��^@��@���@��w@�33@��@��\@�-@���@�Q�@���@���@���@���@���@��h@�X@�Q�@��
@��P@�^5@���@�x�@��/@��;@��w@��F@�33@���@�E�@��@��h@�X@��@�%@���@��j@�j@�Z@��m@��w@��F@��F@���@��@�l�@���@�=q@�E�@�-@�@�7L@�z�@���@���@�\)@�;d@���@��!@�M�@��@�hs@�X@�X@�G�@��`@���@��9@���@�j@�Q�@��
@���@���@��@�"�@��@�~�@�=q@���@��#@�@��7@�hs@�`B@�O�@�?}@���@���@���@���@�r�@� �@��@��@�t�@�l�@�S�@�+@���@�ȴ@���@��\@�v�@�V@��@���@�`B@��@�V@��j@�Z@�I�@�9X@�b@���@���@�|�@�C�@�@��R@�v�@�=q@�J@��@���@��-@��7@�hs@�`B@�G�@�/@�&�@�%@���@�z�@�I�@�1@���@��w@�|�@�\)@�+@��@���@���@��+@�=q@�@��#@���@�@���@�x�@�x�@��@���@��j@��9@��@��u@�I�@�9X@� �@�b@��;@���@��@�dZ@�S�@�"�@��@�
=@�@�
=@�
=@��H@���@�5?@���@�@��^@��7@�x�@�`B@�?}@���@���@�/@�V@���@��@�(�@��w@��G�O�@�B[@��	@��a@zE�@o�r@g�@`�@Z��@R8�@JGE@C�@<_@4G@.��@)+�@#��@*0@z�@�m@�@
#:111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�HB�HB�HB�HB�HB�HB�BB�BB�HB�BB�;B�5B�/B�;B�;B�TB�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�NB��B�NB�B�B�B�B�B��B�BhB'�B'�BR�Bp�Bl�Bs�B�VB��B�!B�'B�9B�LB�-B�-B�B��B��B��B��B��B��B��B��B��B��B�hB�VB�DB�7B�1B�+B�%B�B� Bx�Bq�BiyBcTB[#BXBS�BL�BE�B@�B6FB.B#�B�B{B
=BB�B�;B��BÖB�?B��B��B�{B�\B�=B�%B|�Bl�BcTBQ�BI�BB�B<jB/BbB
�B
�B
�NB
ƨB
�RB
�B
��B
��B
�PB
�B
s�B
k�B
`BB
T�B
I�B
:^B
8RB
'�B	��B	�fB	�#B	��B	��B	ÖB	�B	�JB	q�B	n�B	l�B	gmB	]/B	Q�B	?}B	7LB	,B	"�B	�B	�B	oB	hB	DB��B��B�B�;B�#B�B�B��B��BɺBƨBB�jB�FB�3B�!B��B��B��B��B��B��B��B�uB�hB�VB�PB�DB�1B�B�B|�B{�By�By�Bw�Bt�Br�Bo�Bl�Bk�BiyBiyBhsBhsBgmBffBe`Be`BdZBcTBbNBaHBaHB`BB]/B\)BZBXBXBXBZB]/B]/B]/B^5B_;B_;B`BB`BBbNBbNBcTBbNBdZBiyBl�Bo�Bq�Bu�By�B}�B}�B{�B~�B�B�B�B�B�B�B~�Bz�Bu�Bp�Bn�Bk�BiyBl�BjBk�Bo�Bo�Br�Bs�Bt�Bu�By�Bz�B~�B�=B�VB�VB�\B�hB��B��B��B��B��B��B�B�B�B�B�!B�9B�FB�?B�XB�jB�jB�wB�}BBŢBƨBȴBɺB��B��B��B�B�B�NB�ZB�fB�mB�B�B�B�B�B��B��B��B	B	B	B	B	1B	1B	+B	
=B	PB	\B	hB	uB	{B	�B	�B	�B	�B	�B	�B	 �B	"�B	$�B	%�B	'�B	(�B	(�B	(�B	-B	0!B	1'B	2-B	2-B	49B	7LB	:^B	<jB	<jB	?}B	A�B	C�B	E�B	I�B	J�B	J�B	J�B	N�B	O�B	P�B	P�B	Q�B	R�B	XB	[#B	[#B	^5B	bNB	dZB	gmB	jB	m�B	p�B	q�B	v�B	y�B	z�B	{�B	|�B	~�B	�B	�B	�B	�1B	�JB	�\B	�hB	�hB	�oB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�-B	�9B	�FB	�LB	�XB	�^B	�dB	�jB	�qB	�wB	�wB	�}B	��B	��B	B	ŢB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�B	�B	�#B	�)B	�5B	�/B	�5B	�5B	�5B	�5B	�;B	�BB	�BB	�HB	�HB	�NB	�ZB	�ZB	�`B	�fB	�mB	�mB	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B
	B
�B
 BB
(>B
2-B
9�B
?cB
E�B
G�B
MB
R�B
U�B
ZQB
^jB
dB
i�B
nB
r�B
x�B
|�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B�UB�UB�UB�XB�XB�UB�LB�SB�XB�QB�LB�FB�=B�IB�LB�eB�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�BߔB�ZB�B�\B�B�B�B�B��B��B��BvBB�BF�Bd�B`�Bg�B�XB��B�#B�$B�;B�JB�0B�2B�"B�B��B��B��B��B��B��B��B��B��B�jB�\BJB}BB|6B{6Bz0BxBt
Bl�Be�B]�BWbBO.BLBH
B@�B9�B4�B*YB"-B�B�B�B�SB�"B��B�YB��B��B�`B�B��B��B�B~cBzJBqB`�BWzBFB=�B6�B0�B#IB�B
��B
߻B
։B
��B
��B
�WB
�5B
��B
��B
uIB
g�B
_�B
T�B
IDB
=�B
.�B
,�B
>B	�=B	ڸB	�xB	�SB	�B	��B	�mB	��B	fB	b�B	`�B	[�B	Q�B	FSB	3�B	+�B	 qB	:B	B		�B	�B	�B��B�fB�IB��BӫBϓB�~B̂B�hB�MB�-B�B� B��B��B��B��B�tB�\B�PB�>B�#B�B�B��B��B��B��B�B|�Bv�Bu�BqlBpdBnWBnYBlNBi;Bg0BdBaB`B]�B]�B\�B\�B[�BZ�BY�BY�BX�BW�BV�BU�BU�BT�BQ�BP�BN�BL�BL�BL�BN�BQ�BQ�BQ�BR�BS�BS�BT�BT�BV�BV�BW�BV�BX�B^BaBd!Bf0BjJBn_Br{BrzBpnBs�Bw�Bv�Bv�Bx�Bx�Bv�Bs~BohBjKBe*Bc B`B^BaB_B`Bd%Bd$Bg;Bh>BiFBjLBncBoiBs�B~�B��B��B��B��B�B�B�1B�VB�iB�yB��B��B��B��B��B��B��B��B��B��B��B��B�B�B�'B�,B�7B�<B�ZB�mB�xBʅB̑B��B��B��B��B�B�&B�)B�)B�*B�BB�]B�jB��B��B��B��B��B��B��B��B	�B	�B	�B	�B	�B	B	B	B	B	"B	 B	AB	MB	[B	`B	nB	oB	rB	rB	!�B	$�B	%�B	&�B	&�B	(�B	+�B	.�B	0�B	0�B	3�B	6B	8B	:B	>3B	?<B	?8B	?9B	CRB	DYB	E]B	E]B	FfB	GiB	L�B	O�B	O�B	R�B	V�B	X�B	[�B	^�B	bB	eB	f B	kBB	nRB	o[B	p`B	qeB	soB	x�B	x�B	x�B	|�B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�+B	�,B	�5B	�4B	�@B	�TB	�XB	�ZB	�]B	�hB	�pB	�zB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�.B	�8B	�<B	�NB	�QB	�VB	�]B	�eB	�lB	�uB	�~B	̓B	ΊB	·B	ΌB	ϑB	ЖB	ҡB	њB	ҢB	ңB	ҤB	ҤB	ӫB	ԯB	ԲB	ճB	նB	ֺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�%B	�.B	�9B	�6B	�=B	�;B	�<G�O�B	�HB	��B
GB
�B
�B
&�B
.B
3�B
:'B
<NB
AhB
G	B
JB
N�B
R�B
XvB
]�B
b}B
gLB
l�B
q>111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.1 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            r =0.9997(+/-0), vertically averaged dS =-0.011(+/-0.002) in PSS-78.                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Map scales: x=6,3; y=2,1. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                            201906040940302019060409403020190604094030  AO  ARCAADJP                                                                    20171021170204    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20171021170204  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20171021170204  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20190604094030  IP                  G�O�G�O�G�O�                