CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-13T22:39:57Z UW 3.1 conversion   
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  5901071 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               A   AO  20111130135616  20190522121825  1727_5046_021                   2C  D   APEX                            2143                            040306                          846 @�0՚��1   @�0�K� 	@7~5?|��c����o1   GPS     Primary sampling: mixed [deeper than nominal 980dbar: discrete; nominal 980dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�33A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���C   C  C  C�fC  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4�C6  C8  C:  C<  C>  C@  CB  CD�CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D&��D'y�D(  D(� D)  D)� D*  D*� D+  D+�fD,fD,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN�fDO  DO� DP  DP� DQ  DQ�fDR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dy� D�3D�vfD��fD��D�#3D�ffD��fD�� D�)�D�VfD�� D��3D��D�ffD�s3D��fD��D�L�D�3D��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�33@���@���AffA>ffA^ffA~ffA�33A�33A�33A�ffA�33A�33A�33A�33B��B��B��B��B'��B/��B7��B?��BG��BO��BW��B_��Bg��Bo��Bw��B��B���B���B���B���B���B���B���B���B���B���B���B�  B�  B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���C�fC�fC��C�fC	�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC!�fC#�fC%�fC'�fC)�fC+�fC-�fC/�fC1�fC4  C5�fC7�fC9�fC;�fC=�fC?�fCA�fCD  CE�fCG�fCI�fCK�fCM�fCO�fCQ�fCS�fCU�fCW�fCY�fC[�fC]�fC_�fCa�fCc�fCe�fCg�fCi�fCk�fCm�fCo�fCq�fCs�fCu�fCw�fCy�fC{�fC}�fC�fC��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C�  C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C�  C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3D y�D ��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��D	y�D	��D
y�D
��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��D y�D ��D!y�D!��D"y�D"��D#y�D#��D$y�D$��D%y�D%��D&y�D&�3D's3D'��D(y�D(��D)y�D)��D*y�D*��D+� D,  D,y�D,��D-y�D-��D.y�D.��D/y�D/��D0y�D0��D1y�D1��D2y�D2��D3y�D3��D4y�D4��D5y�D5��D6y�D6��D7y�D7��D8y�D8��D9y�D9��D:y�D:��D;y�D;��D<y�D<��D=y�D=��D>y�D>��D?y�D?��D@y�D@��DAy�DA��DBy�DB��DCy�DC��DDy�DD��DEy�DE��DFy�DF��DGy�DG��DHy�DH��DIy�DI��DJy�DJ��DKy�DK��DLy�DL��DMy�DM��DN� DN��DOy�DO��DPy�DP��DQ� DQ��DRy�DR��DSy�DS��DTy�DT��DUy�DU��DVy�DV��DWy�DW��DXy�DX��DYy�DY��DZy�DZ��D[y�D[��D\y�D\��D]y�D]��D^y�D^��D_y�D_��D`y�D`��Day�Da��Dby�Db��Dcy�Dc��Ddy�Dd��Dey�De��Dfy�Df��Dgy�Dg��Dhy�Dh��Diy�Di��Djy�Dj��Dky�Dk��Dly�Dl��Dmy�Dm��Dny�Dn��Doy�Do��Dpy�Dp��Dqy�Dq��Dry�Dr��Dsy�Dy��D� D�s3D��3D��fD�  D�c3D��3D���D�&fD�S3D���D�� D��D�c3D�p D��3D��D�I�D� D��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��Aϕ�AϬAϧ�Aϩ�Aϲ-Aϴ9A϶FA϶FA϶FAϩ�Aϕ�AύPA�O�A�JA��yAξwA�~�A�\)A�hsA�hsA�hsA�l�A�O�A�  A�-Aź^A�v�AîA�  A�A� �A���A��`A�(�A���A�VA� �A�%A��
A��HA�\)A��A���A��yA�ƨA���A�ffA��A�XA�\)A�~�A�$�A�
=A��DA�ĜA���A�~�A�A�33A��uA�+A�XA�=qA��yA�~�A���A�bA��
A��\A�JA�"�A�9XA�A��^A��A�G�A�VA�I�A��FA�JA��!A�z�A��A���A�=qA�E�A� �A���A�9XA���A�/A�dZA���A�A���A���A��A�A�x�A�{A�|�A���A���A��/A�1A|Ay�wAw�hAs��Aq��Ao�7An��AmoAk�TAj��Aj�Ai��Ag�
Af�9Ae�^Ad�Ab��Aa�A`�A_�PA^��A]�A[�AZ�jAY�mAW�hAU7LATffAS�FASx�AR�9AQ�AP�AO�-ANbNAM�-AL�`AKl�AI�AIS�AH�jAG�AG"�AE�wADv�ACl�AB��AA��AA��AA;dA@�A?��A?O�A>ZA=�
A=l�A<z�A;`BA:5?A9%A8Q�A7�A6�9A5�^A5�A4��A3�^A3?}A2��A2bA1hsA0ĜA0v�A01A//A.Q�A-?}A,M�A++A*n�A(��A'�A'��A';dA&��A%hsA$5?A$A#�
A#��A#C�A#VA"ĜA"M�A ��A�TAXA�A�9AM�A9XA�mA=qAbNA�A �A��A�HA��AbA+A�A�mA�A��AK�A�RAQ�A$�A��AffA
=A	��A	
=A(�A`BA�RA�!AA�A"�A��AG�AĜAr�A=qA��A ��@��m@�x�@�\)@�&�@���@�  @�
=@�9@��@�|�@�\)@�J@�"�@��@�!@��@ޏ\@��@�O�@�V@�t�@�v�@�@�hs@أ�@ׅ@�J@��@�5?@� �@�J@�Q�@�C�@��
@�{@�x�@ƸR@�%@ʏ\@���@�+@���@ȃ@��@ɑh@�`B@��@��`@�9X@��
@�ȴ@�ff@�-@��@�/@ļj@�z�@�(�@���@Õ�@�=q@�  @��y@�r�@�A�@��D@��@��@�Ĝ@�&�@��@�ȴ@�A�@��u@��j@���@�Q�@�|�@�\)@���@�S�@�S�@��-@�Ĝ@�Z@�b@���@�ȴ@�E�@���@�/@�ȴ@�G�@��`@�hs@�p�@�G�@��@��j@�(�@��;@�t�@�dZ@�t�@�l�@�C�@��@��@�5?@�5?@��^@��H@��u@�^5@���@�@�%@��@��j@�bN@���@��D@�"�@��+@��@��@�1@���@�dZ@�33@�n�@��@��@��@���@�/@���@��9@�Q�@�dZ@��!@�M�@��@��h@�&�@��`@�x�@��-@���@���@�;d@��;@�j@�%@��!@��P@��@��
@��/@��@��@���@��y@��+@��@�j@�bN@�A�@�I�@�9X@��m@�K�@�
=@�-@�@�?}@�O�@��^@���@��^@�x�@�V@�Ĝ@�(�@�\)@�"�@��@�
=@��@���@���@���@��H@��@�C�@�l�@�|�@��P@��@��P@���@��@�
=@��@��\@�{@��#@���@���@��7@�7L@�%@��/@���@���@�I�@� �@�  @���@���@��P@�l�@�K�@�+@�
=@��y@�n�@��@��T@�O�@��@��@�%@�V@�%@�z�@�b@�1@�  @��
@�l�@�C�@�+@��@��`@u�@mp�@dz�@]��@U�@PQ�@H  @A�#@<�@6�+@0�@*=q@$��@��@��@�-@�9@t�@  @V111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 Aϕ�AϬAϧ�Aϩ�Aϲ-Aϴ9A϶FA϶FA϶FAϩ�Aϕ�AύPA�O�A�JA��yAξwA�~�A�\)A�hsA�hsA�hsA�l�A�O�A�  A�-Aź^A�v�AîA�  A�A� �A���A��`A�(�A���A�VA� �A�%A��
A��HA�\)A��A���A��yA�ƨA���A�ffA��A�XA�\)A�~�A�$�A�
=A��DA�ĜA���A�~�A�A�33A��uA�+A�XA�=qA��yA�~�A���A�bA��
A��\A�JA�"�A�9XA�A��^A��A�G�A�VA�I�A��FA�JA��!A�z�A��A���A�=qA�E�A� �A���A�9XA���A�/A�dZA���A�A���A���A��A�A�x�A�{A�|�A���A���A��/A�1A|Ay�wAw�hAs��Aq��Ao�7An��AmoAk�TAj��Aj�Ai��Ag�
Af�9Ae�^Ad�Ab��Aa�A`�A_�PA^��A]�A[�AZ�jAY�mAW�hAU7LATffAS�FASx�AR�9AQ�AP�AO�-ANbNAM�-AL�`AKl�AI�AIS�AH�jAG�AG"�AE�wADv�ACl�AB��AA��AA��AA;dA@�A?��A?O�A>ZA=�
A=l�A<z�A;`BA:5?A9%A8Q�A7�A6�9A5�^A5�A4��A3�^A3?}A2��A2bA1hsA0ĜA0v�A01A//A.Q�A-?}A,M�A++A*n�A(��A'�A'��A';dA&��A%hsA$5?A$A#�
A#��A#C�A#VA"ĜA"M�A ��A�TAXA�A�9AM�A9XA�mA=qAbNA�A �A��A�HA��AbA+A�A�mA�A��AK�A�RAQ�A$�A��AffA
=A	��A	
=A(�A`BA�RA�!AA�A"�A��AG�AĜAr�A=qA��A ��@��m@�x�@�\)@�&�@���@�  @�
=@�9@��@�|�@�\)@�J@�"�@��@�!@��@ޏ\@��@�O�@�V@�t�@�v�@�@�hs@أ�@ׅ@�J@��@�5?@� �@�J@�Q�@�C�@��
@�{@�x�@ƸR@�%@ʏ\@���@�+@���@ȃ@��@ɑh@�`B@��@��`@�9X@��
@�ȴ@�ff@�-@��@�/@ļj@�z�@�(�@���@Õ�@�=q@�  @��y@�r�@�A�@��D@��@��@�Ĝ@�&�@��@�ȴ@�A�@��u@��j@���@�Q�@�|�@�\)@���@�S�@�S�@��-@�Ĝ@�Z@�b@���@�ȴ@�E�@���@�/@�ȴ@�G�@��`@�hs@�p�@�G�@��@��j@�(�@��;@�t�@�dZ@�t�@�l�@�C�@��@��@�5?@�5?@��^@��H@��u@�^5@���@�@�%@��@��j@�bN@���@��D@�"�@��+@��@��@�1@���@�dZ@�33@�n�@��@��@��@���@�/@���@��9@�Q�@�dZ@��!@�M�@��@��h@�&�@��`@�x�@��-@���@���@�;d@��;@�j@�%@��!@��P@��@��
@��/@��@��@���@��y@��+@��@�j@�bN@�A�@�I�@�9X@��m@�K�@�
=@�-@�@�?}@�O�@��^@���@��^@�x�@�V@�Ĝ@�(�@�\)@�"�@��@�
=@��@���@���@���@��H@��@�C�@�l�@�|�@��P@��@��P@���@��@�
=@��@��\@�{@��#@���@���@��7@�7L@�%@��/@���@���@�I�@� �@�  @���@���@��P@�l�@�K�@�+@�
=@��y@�n�@��@��T@�O�@��@��@�%@�V@�%@�z�@�b@�1@�  @��
@�l�@�C�@�+@��@��`@u�@mp�@dz�@]��@U�@PQ�@H  @A�#@<�@6�+@0�@*=q@$��@��@��@�-@�9@t�@  @V111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�fB�TB�NB�NB�HB�HB�HB�BB�HB�HB�BB�5B�/B�;B�;B�/B�#B�#B�#B�
B��B��B��BȴBÖB�qB�FB�'B�B��B��B��B��B�hB�=B�%B� Bs�Bn�Be`BP�B?}B.B�B�BDB��B�B�)B��BȴB�dB�B��B�uB�JB|�Bu�Bn�BZBJ�BE�B5?B�B�B�BuBJB
�B
�sB
�TB
�;B
�B
��B
��B
�?B
��B
��B
�VB
z�B
T�B
@�B
)�B
	7B
B	��B	��B	�B	�sB	�B	�fB	�HB	�HB	�NB	�HB	�B	��B	��B	ǮB	�}B	�^B	�-B	��B	��B	��B	�VB	�%B	�B	}�B	|�B	w�B	p�B	m�B	hsB	cTB	]/B	W
B	O�B	M�B	K�B	I�B	E�B	A�B	5?B	-B	-B	.B	-B	)�B	(�B	%�B	#�B	 �B	�B	�B	�B	uB	VB		7B	B	B��B��B��B�B�B�B�B�mB�`B�NB�HB�BB�5B�B�B��B��BɺBŢB��B�wB�jB�dB�RB�3B�-B�'B�!B�B�B�B�B��B��B��B��B��B��B��B��B�oB�=B�B�B� B}�B{�Bt�Bq�Bp�Bo�Bo�Bn�Bm�Bk�BjBiyBhsBffBcTB_;B]/B[#BYBXBW
BVBR�BQ�BN�BM�BM�BL�BK�BI�BG�BE�BB�B@�B<jB;dB;dB9XB5?B6FB5?B33B2-B6FB8RB8RB9XB;dB;dB;dB:^B<jB=qB>wB>wB>wB>wB>wB;dB7LB6FB5?B33B1'B1'B33B8RBJ�BcTBt�Bw�B|�Bz�B~�B�%B�DB�JB�PB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�=B�%B�JB�VB�=B�%B�+B�DB�oB��B��B��B��B�B�B�3B�XB�dB�XB�qBĜBǮBǮBǮBȴBɺB��B��B��BɺBǮBǮB��B��B��B��B�
B�B�B�/B�;B�HB�`B�sB�sB�mB�fB�mB�TB�B�
B��B��BǮBĜB��B�TB�`B�`B�;B�#B�#B�B��B�B�B�/B�BB�ZB�fB�mB�yB�B�B��B��B��B��B��B��B	B	B	
=B	PB	�B	�B	�B	 �B	%�B	+B	/B	5?B	C�B	L�B	N�B	Q�B	[#B	\)B	^5B	^5B	`BB	`BB	_;B	cTB	dZB	e`B	e`B	e`B	dZB	dZB	hsB	n�B	o�B	q�B	t�B	w�B	x�B	z�B	{�B	|�B	|�B	~�B	�B	�B	�B	�B	�%B	�1B	�=B	�=B	�PB	�bB	�hB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�'B	�-B	�-B	�3B	�9B	�?B	�FB	�LB	�RB	�^B	�dB	�dB	�jB	�jB	�qB	��B	ÖB	ÖB	ŢB	ƨB	ƨB	ǮB	ȴB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�NB	��B
	7B
uB
!�B
+B
2-B
;dB
?}B
E�B
J�B
N�B
S�B
ZB
_;B
bNB
ffB
k�B
r�B
v�B
y�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B��B��B��B��B��B��B��B��B��B��B��B��B�B�B��B��B��B��B��B��B��B��B��B�FB�BB�B�sB�fB�mB�`B�TB�ZB�TB�TB�NB�HB�TB�NB�ZB�NB�ZB�HB�5B�;B�#B�B��B��B��BƨBÖB�dB�3B�'B�B��B��B��B�{B�PB�DB�%Bu�Bq�Bo�B[#BF�B5?B!�B�B\BB�B�BB��B��B�}B�-B�!B��B�oB� Bw�Bu�B_;BK�BK�B>wB#�B#�B�B�B�B
��B
�B
�fB
�HB
�#B
�
B
��B
�qB
��B
��B
�uB
�%B
\)B
F�B
5?B
VB
+B	��B	��B	�B	�B	�B	�sB	�`B	�ZB	�`B	�ZB	�/B	��B	��B	��B	B	�jB	�LB	�B	��B	��B	��B	�7B	�B	~�B	� B	|�B	r�B	p�B	l�B	e`B	`BB	\)B	T�B	O�B	M�B	L�B	H�B	E�B	9XB	0!B	0!B	0!B	.B	+B	+B	'�B	%�B	#�B	�B	�B	�B	�B	oB	PB	+B	B	  B��B��B�B�B�B�B�yB�mB�ZB�NB�HB�BB�)B�B��B��B��B��BÖB�}B�qB�qB�jB�LB�3B�-B�'B�!B�B�B�B�B��B��B��B��B��B��B��B��B�bB�7B�B�B�B� By�Bt�Br�Bq�Bp�Bo�Bn�Bm�Bl�BjBjBk�BhsBcTB`BB^5B\)BZBW
BXBW
BS�BS�BO�BN�BM�BL�BM�BK�BI�BE�BC�B@�B>wB<jB<jB;dB7LB5?B5?B6FB9XB<jB<jB;dB<jB<jB<jB=qB>wB>wB?}B?}B@�B@�B@�B?}B:^B9XB8RB5?B6FB49B49B6FBG�BaHBt�Bw�B~�B|�B~�B�%B�DB�PB�PB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�VB�%B�JB�bB�VB�+B�+B�=B�hB��B��B��B��B�B�!B�3B�XB�jB�XB��BƨBȴBȴBȴB��B��B��B��B��BɺBȴBƨB��B��B�B�B�B�B�B�/B�;B�HB�`B�sB�yB�sB�fB�sB�mB�)B�B��B��B��BB��B�ZB�mB�sB�HB�)B�)B�#B�B�B�B�/B�HB�`B�fB�mB�B�B�B��B��B��B��B��B	  B	B	%B	
=B	JB	�B	�B	�B	"�B	$�B	)�B	.B	33B	B�B	L�B	N�B	P�B	\)B	]/B	_;B	_;B	aHB	aHB	bNB	cTB	dZB	e`B	e`B	e`B	e`B	e`B	jB	o�B	p�B	q�B	t�B	w�B	x�B	z�B	|�B	|�B	}�B	� B	�B	�B	�B	�B	�%B	�1B	�=B	�=B	�PB	�bB	�hB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�'B	�-B	�-B	�9B	�9B	�?B	�FB	�LB	�XB	�^B	�dB	�dB	�jB	�jB	�wB	��B	ĜB	ĜB	ŢB	ƨB	ƨB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�NB	��B
	7B
uB
!�B
+B
2-B
<jB
@�B
E�B
J�B
N�B
S�B
ZB
_;B
bNB
gmB
l�B
r�B
v�B
y�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<�C�<���<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<49X<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0.1 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201031446412012010314464120120103144641  AO  ARGQ                                                                        20111130135616  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130135616  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120103144641  IP                  G�O�G�O�G�O�                