CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-13T22:39:53Z UW 3.1 conversion   
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
_FillValue                 �  Kl   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�     �  Mh   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  UP   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ]8   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  _4   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  g   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  i   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  q    PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  x�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  z�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �\   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �l   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �p   HISTORY_START_PRES                    	long_name          Start pressure action applied on   units         decibar    
_FillValue        G�O�        ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    units         decibar    
_FillValue        G�O�        ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  5901071 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               A   AO  20111130135512  20190522121825  1727_5046_007                   2C  D   APEX                            2143                            040306                          846 @� Z����1   @� [Q�_�@7#n��P�c��E��1   GPS     Primary sampling: mixed [deeper than nominal 980dbar: discrete; nominal 980dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�33B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX�CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C��C�  C�  D   D � D  Dy�D  D� D  D� D  D� D  Dy�D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� DfD� D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D1��D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dk��Dly�Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dy�fD��D�ffD��fD��fD�,�D�Y�D��fD�� D�  D�S3D��fD��D��D�i�Dک�D�� D��D�` D�311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�33@���@���AffA>ffA^ffA~ffA�33A�33A�33A�33A�33A�33A�33A�33B��B��B��B��B'��B/��B7��B?��BG��BO��BW��B_��Bg��Bo��Bw��B�  B���B���B���B���B���B���B���B���B�  B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���C�fC�fC�fC�fC	�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC!�fC#�fC%�fC'�fC)�fC+�fC-�fC/�fC1�fC3�fC5�fC7�fC9�fC;�fC=�fC?�fCA�fCC�fCE�fCG�fCI�fCK�fCM�fCO�fCQ�fCS�fCU�fCX  CY�fC[�fC]�fC_�fCa�fCc�fCe�fCg�fCi�fCk�fCm�fCo�fCq�fCs�fCu�fCw�fCy�fC{�fC}�fC�fC��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C�  C�  C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��fC��3C��3C��3C��3C��3C�  C��3C��3C��3D y�D ��Ds3D��Dy�D��Dy�D��Dy�D��Ds3D��Dy�D��Dy�D��Dy�D��D	y�D	��D
y�D
��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D  Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Ds3D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��D y�D ��D!y�D!��D"y�D"��D#y�D#��D$y�D$��D%y�D%��D&y�D&��D'y�D'��D(y�D(��D)y�D)��D*y�D*��D+y�D+��D,y�D,��D-y�D-��D.y�D.��D/y�D/��D0y�D0��D1y�D1�3D2y�D2��D3y�D3��D4y�D4��D5y�D5��D6y�D6��D7y�D7��D8y�D8��D9y�D9��D:y�D:��D;y�D;��D<y�D<��D=y�D=��D>y�D>��D?y�D?��D@y�D@��DAy�DA��DBy�DB��DCy�DC��DDy�DD��DEy�DE��DFy�DF��DGy�DG��DHy�DH��DIy�DI��DJy�DJ��DKy�DK��DLy�DL��DMy�DM��DNy�DN��DOy�DO��DPy�DP��DQy�DQ��DRy�DR��DSy�DS��DTy�DT��DUy�DU��DVy�DV��DWy�DW��DXy�DX��DYy�DY��DZy�DZ��D[y�D[��D\y�D\��D]y�D]��D^y�D^��D_y�D_��D`y�D`��Day�Da��Dby�Db��Dcy�Dc��Ddy�Dd��Dey�De��Dfy�Df��Dgy�Dg��Dhy�Dh��Diy�Di��Djy�Dj��Dky�Dk�3Dls3Dl��Dmy�Dm��Dny�Dn��Doy�Do��Dpy�Dp��Dqy�Dq��Dry�Dr��Dsy�Dy� D�fD�c3D��3D��3D�)�D�VfD��3D���D��D�P D��3D��fD�fD�ffDڦfD���D��D�\�D� 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�n�AōPA��A��A�A�A�O�A�"�A�JA���A���A�z�A�^5A�Q�A���A��A��DA�ffA�\)A�1'A���A�1'A��A���A���A�C�A�M�A��A�~�A�bNA�$�A��wA�r�A��DA�JA���A�hsA��FA�n�A�9XA���A��A��HA���A�=qA���A�  A��A�`BA�XA��+A�"�A��A��A�ȴA��/A�1A���A��HA�l�A��yA��HA���A�l�A���A��A�+A�XA�-A��A��
A�K�A��
A���A��HA�|�A�p�A�C�A���A���A�+A�XA��#A�%A�|�A��\A��A�x�A��9A��A��\A�?}A�ƨA� �A~ȴA|�RA{?}AzJAx�jAu��As\)Ao��Al-Aj�+Ajn�Aj1Ag�Ae�AdM�Ab9XAa|�A_�PA]?}A[�A["�AZ�/AZn�AY�AYp�AX��AW�#AV��AU�hAUASVAQ�PAP1AN��AM?}AJ�AI�AHbNAF�+AE�ADJAC�ABJA@~�A?|�A?&�A>�DA>A=`BA<�A<I�A;XA9dZA7t�A6��A5l�A4E�A3�wA3O�A2��A1�A1`BA1�A0�9A/�TA.�RA.Q�A-S�A,~�A+�;A+A+t�A)�A(�A(1'A'�mA'dZA&��A&1A%�7A#;dA"$�A!�mA!t�A �+A -A �A�^AXAA�A��A�A�AO�AA�yA��AbNA+A�;A��AĜA�A��A�DA��AC�AVA��A5?A�A��A�Ax�A
�yA
�uA
9XA	�TA�AA��A7LA�HA^5A�AA�AAhsA?}A�AVAA ��A n�A @��P@�\)@��@��R@�n�@���@�(�@���@�1@�J@�O�@��D@�ƨ@�\)@�"�@�n�@�j@��@�&�@���@�@�
=@���@���@�R@旍@��@��@��@�z�@��
@��@�{@ߥ�@۝�@�/@�`B@�bN@�`B@׮@�@֧�@Ԭ@��@�$�@�`B@�%@��;@���@��@�V@�ȴ@�v�@��/@��@�O�@ă@�33@��@�@���@�hs@��w@��@�S�@�E�@���@���@���@�E�@��u@��@���@���@�I�@�Q�@��@�M�@��^@���@��#@���@�/@��@��@���@��@�+@��/@�Z@� �@��@�;d@���@���@��@�(�@�K�@�o@�S�@��
@��w@��y@��^@��h@���@���@��@�I�@�9X@�b@��@�ƨ@��H@���@�5?@��^@���@��m@�1'@�(�@�  @���@�S�@�~�@���@��#@��@��#@�x�@���@���@���@�o@�J@���@�V@�G�@��T@�^5@��T@��h@��`@��@�z�@�A�@��
@���@��7@��@��/@�V@��^@�7L@�%@��@��`@��/@���@��m@���@�-@�{@��@���@��h@�p�@�&�@���@�r�@�A�@���@�^5@�E�@�n�@��+@��R@�n�@���@��9@��h@�$�@��-@�ƨ@�|�@�n�@�@���@��-@���@���@��T@��#@�@��^@���@��@�;d@�%@�1'@�|�@�@�ȴ@�^5@��!@��@��-@���@�7L@���@�%@���@��D@�z�@�Q�@�1'@�1'@�1'@�(�@��;@���@��@�l�@�l�@�S�@�K�@�;d@�"�@��@�"�@��@��y@���@�~�@�M�@�=q@�-@�J@�hs@�O�@�?}@�V@�%@���@�Ĝ@�bN@�b@��@l�@+@~ȴ@~��@~��@~�+@}��@}�@|��@y�7@l�@b�\@\z�@Tj@Qx�@Nv�@Hr�@C��@;o@6E�@1��@,(�@&$�@!x�@�@��@�9@9X@�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�n�AōPA��A��A�A�A�O�A�"�A�JA���A���A�z�A�^5A�Q�A���A��A��DA�ffA�\)A�1'A���A�1'A��A���A���A�C�A�M�A��A�~�A�bNA�$�A��wA�r�A��DA�JA���A�hsA��FA�n�A�9XA���A��A��HA���A�=qA���A�  A��A�`BA�XA��+A�"�A��A��A�ȴA��/A�1A���A��HA�l�A��yA��HA���A�l�A���A��A�+A�XA�-A��A��
A�K�A��
A���A��HA�|�A�p�A�C�A���A���A�+A�XA��#A�%A�|�A��\A��A�x�A��9A��A��\A�?}A�ƨA� �A~ȴA|�RA{?}AzJAx�jAu��As\)Ao��Al-Aj�+Ajn�Aj1Ag�Ae�AdM�Ab9XAa|�A_�PA]?}A[�A["�AZ�/AZn�AY�AYp�AX��AW�#AV��AU�hAUASVAQ�PAP1AN��AM?}AJ�AI�AHbNAF�+AE�ADJAC�ABJA@~�A?|�A?&�A>�DA>A=`BA<�A<I�A;XA9dZA7t�A6��A5l�A4E�A3�wA3O�A2��A1�A1`BA1�A0�9A/�TA.�RA.Q�A-S�A,~�A+�;A+A+t�A)�A(�A(1'A'�mA'dZA&��A&1A%�7A#;dA"$�A!�mA!t�A �+A -A �A�^AXAA�A��A�A�AO�AA�yA��AbNA+A�;A��AĜA�A��A�DA��AC�AVA��A5?A�A��A�Ax�A
�yA
�uA
9XA	�TA�AA��A7LA�HA^5A�AA�AAhsA?}A�AVAA ��A n�A @��P@�\)@��@��R@�n�@���@�(�@���@�1@�J@�O�@��D@�ƨ@�\)@�"�@�n�@�j@��@�&�@���@�@�
=@���@���@�R@旍@��@��@��@�z�@��
@��@�{@ߥ�@۝�@�/@�`B@�bN@�`B@׮@�@֧�@Ԭ@��@�$�@�`B@�%@��;@���@��@�V@�ȴ@�v�@��/@��@�O�@ă@�33@��@�@���@�hs@��w@��@�S�@�E�@���@���@���@�E�@��u@��@���@���@�I�@�Q�@��@�M�@��^@���@��#@���@�/@��@��@���@��@�+@��/@�Z@� �@��@�;d@���@���@��@�(�@�K�@�o@�S�@��
@��w@��y@��^@��h@���@���@��@�I�@�9X@�b@��@�ƨ@��H@���@�5?@��^@���@��m@�1'@�(�@�  @���@�S�@�~�@���@��#@��@��#@�x�@���@���@���@�o@�J@���@�V@�G�@��T@�^5@��T@��h@��`@��@�z�@�A�@��
@���@��7@��@��/@�V@��^@�7L@�%@��@��`@��/@���@��m@���@�-@�{@��@���@��h@�p�@�&�@���@�r�@�A�@���@�^5@�E�@�n�@��+@��R@�n�@���@��9@��h@�$�@��-@�ƨ@�|�@�n�@�@���@��-@���@���@��T@��#@�@��^@���@��@�;d@�%@�1'@�|�@�@�ȴ@�^5@��!@��@��-@���@�7L@���@�%@���@��D@�z�@�Q�@�1'@�1'@�1'@�(�@��;@���@��@�l�@�l�@�S�@�K�@�;d@�"�@��@�"�@��@��y@���@�~�@�M�@�=q@�-@�J@�hs@�O�@�?}@�V@�%@���@�Ĝ@�bN@�b@��@l�@+@~ȴ@~��@~��@~�+@}��@}�@|��@y�7@l�@b�\@\z�@Tj@Qx�@Nv�@Hr�@C��@;o@6E�@1��@,(�@&$�@!x�@�@��@�9@9X@�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB��B�jB��B��B��B��B��B��B�uB�oB�oB�oB�hB�bB�VB�PB�JB�JB�PB�PB�7B�B�B�B�B�+B��B��B��B�B�B�B�!B�3B�9B�?B�9B�9B�?B�9B�-B�-B�-B�9B��B�BB�;B�sB�B�ZB�NB�fB�yB��B�LB�B��B�bB�B��B��B�PB|�Bp�B_;BJ�B6FB�B�
B�-B�{B�B|�Bx�Bp�Bk�BaHBK�B!�B
=B
��B
�B
�BB
��B
ĜB
�!B
�B
e`B
XB
N�B
D�B
(�B
�B
\B	��B	�yB	�5B	��B	��B	�dB	��B	��B	�B	|�B	{�B	u�B	ffB	aHB	XB	R�B	J�B	B�B	6FB	0!B	/B	/B	0!B	49B	49B	2-B	,B	$�B	�B	�B	�B	VB	+B	B��B�B�yB�BB�B��B��B��B��B��BɺBȴBǮBŢBĜBB�}B�^B�-B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�uB�hB�bB�\B�VB�JB�DB�DB�=B�7B�+B�B�B|�B{�Bz�Bz�Bw�Bu�Br�Bo�Bl�Bl�BiyBgmBgmBffBe`Be`BdZBcTBcTBcTBbNBbNBaHB`BB_;B_;B]/B\)BZBXBXBW
BT�BT�BT�BVBVBVBVBVBVBT�BS�BS�BS�BS�BR�BR�BP�BP�BI�BC�B>wB<jB;dB:^B9XB9XB8RB5?B33B2-B2-B33B6FB8RB:^B:^B<jB>wB9XB2-B-B.B.B<jBQ�BO�BF�B>wB5?B6FB?}BN�B_;BW
BM�BG�BI�BL�BN�BL�BM�BZB]/BffBhsBbNB\)B\)B\)B[#BXBbNBhsBhsBe`BaHB^5B_;Bk�Bv�Bw�Bt�Br�Bs�Bw�B{�B�B�B�%B�PB�oB��B��B��B��B��B��B�3B�FB�LB��BBÖBÖBƨB��B��B��B��B��B��B�B�HB�`B�B�B�B�B��B	B	B	B	B	B	B	+B	1B	1B	1B	JB	bB	{B	�B	�B	{B	{B	uB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	"�B	'�B	-B	.B	/B	49B	5?B	5?B	5?B	5?B	7LB	8RB	9XB	7LB	5?B	6FB	7LB	7LB	7LB	8RB	8RB	8RB	8RB	;dB	=qB	?}B	@�B	@�B	@�B	@�B	@�B	B�B	C�B	E�B	N�B	ZB	]/B	`BB	e`B	hsB	hsB	ffB	e`B	k�B	o�B	o�B	jB	m�B	p�B	p�B	v�B	~�B	�B	�B	�B	�+B	�7B	�7B	�7B	�7B	�B	{�B	}�B	�B	�B	�1B	�DB	�bB	�hB	�bB	�bB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�'B	�'B	�'B	�!B	�'B	�-B	�3B	�3B	�?B	�RB	�XB	�^B	�dB	�dB	�jB	�jB	�qB	�qB	�wB	ÖB	ÖB	ÖB	ÖB	ÖB	ÖB	ÖB	ÖB	ĜB	ŢB	ƨB	ƨB	ƨB	ǮB	ɺB	��B	�
B	�B	��B
	7B
�B
�B
,B
7LB
;dB
<jB
E�B
L�B
R�B
ZB
]/B
cTB
jB
n�B
s�B
w�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B��BB�B��B��B��B��B��B�{B�uB�uB�oB�hB�oB��B�bB�PB�JB�VB�bB�DB�B�B�B�%B�JB��B��B��B�B�B�B�3B�9B�?B�FB�FB�?B�FB�?B�LB�?B�FB�RB��B�TB�TB�B�B�sB�ZB�B�B�
B�dB�9B��B�{B�B��B��B�oB� Bt�BcTBN�B;dB#�B�#B�LB��B�+B}�B{�Br�Bo�BffBVB)�BVBB
��B
�ZB
�
B
ɺB
�jB
�JB
iyB
[#B
R�B
P�B
1'B
�B
�B	��B	�B	�NB	�B	��B	B	�9B	��B	�=B	}�B	}�B	}�B	iyB	e`B	]/B	T�B	N�B	G�B	:^B	1'B	0!B	0!B	2-B	5?B	6FB	49B	/B	'�B	 �B	!�B	�B	oB	
=B	+B	  B�B�B�`B�5B�B��B��B��B��B��B��BɺBǮBƨBĜBÖB��B�RB�B�'B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�uB�bB�bB�\B�JB�JB�DB�DB�DB�=B�+B�B}�B{�B{�B|�Bz�Bw�Bt�Bs�Bp�Bl�BjBiyBiyBgmBffBe`Be`BdZBdZBcTBbNBcTBaHB`BB`BB`BB_;B^5B]/BYBYBYBW
BVBW
BW
BW
BVBVBW
BVBT�BT�BS�BT�BS�BS�BQ�BS�BO�BI�BA�B=qB<jB;dB:^B:^B:^B8RB7LB49B49B5?B6FB8RB;dB:^B=qBA�B>wB5?B-B/B-B:^BW
BW
BJ�BD�B5?B5?B<jBK�BdZBZBS�BL�BJ�BM�BP�BQ�BL�BZB]/BgmBk�Be`B_;B^5B^5B]/BW
BbNBjBjBgmBe`B`BB_;BjBv�By�Bw�Bs�Bs�Bw�B{�B�B�%B�7B�VB�oB��B��B��B��B��B��B�3B�FB�dBBÖBĜBĜBǮB��B��B��B��B��B��B�B�HB�mB�B�B�B�B��B	B	B	B	B	%B	+B	1B		7B		7B	
=B	JB	bB	{B	�B	�B	�B	�B	{B	�B	�B	�B	�B	�B	�B	�B	 �B	�B	�B	�B	�B	!�B	&�B	.B	/B	0!B	5?B	5?B	6FB	6FB	7LB	9XB	9XB	:^B	<jB	6FB	7LB	7LB	7LB	7LB	8RB	9XB	9XB	:^B	<jB	=qB	?}B	A�B	@�B	@�B	A�B	A�B	B�B	C�B	D�B	L�B	ZB	]/B	`BB	e`B	iyB	iyB	hsB	dZB	jB	p�B	r�B	k�B	o�B	q�B	p�B	v�B	~�B	�B	�B	�B	�+B	�7B	�7B	�=B	�DB	�%B	|�B	~�B	�B	�B	�7B	�DB	�hB	�hB	�bB	�hB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�'B	�'B	�'B	�!B	�'B	�-B	�3B	�3B	�?B	�XB	�XB	�^B	�dB	�dB	�jB	�qB	�qB	�qB	�wB	ÖB	ÖB	ĜB	ĜB	ĜB	ÖB	ÖB	ÖB	ĜB	ŢB	ƨB	ƨB	ǮB	ǮB	��B	��B	�
B	�B	��B
	7B
�B
�B
,B
7LB
;dB
<jB
E�B
L�B
R�B
ZB
^5B
cTB
jB
n�B
s�B
w�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<D��<#�
<#�
<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0.1 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201031446362012010314463620120103144636  AO  ARGQ                                                                        20111130135512  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130135512  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120103144636  IP                  G�O�G�O�G�O�                