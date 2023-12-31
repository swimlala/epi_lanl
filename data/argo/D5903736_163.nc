CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-11-21T04:11:53Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         (Matthew Alkire, University of Washington      @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
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
_FillValue                    �,Argo profile    3.1 1.2 19500101000000  20181121041153  20190604094023  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  4051                            2C  D   APEX                            5368                            041511                          846 @�����E�1   @���U��@3�t�j�d�t�j~�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   B   B   @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�33A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  BpffBx  B�  B�  B�  B�  B���B�  B�  B�  B�  B�33B�33B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD�CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(fD(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dx��D��D�9�D�r=D���D��D�J=D���D��)D�3D�5�D���Dǿ�D�
�D�O
D�w\D��D��D�K3D�s�D��)11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�  @���@���AffA>ffA^ffA~ffA�33A�33A�33A�33A�ffA�33A�33A�33B��B��B��B��B'��B/��B7��B?��BG��BO��BW��B_��Bg��Bp  Bw��B��B���B���B���B���B���B���B���B���B�  B�  B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B�  B���B���C�fC�fC�fC�fC	�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC!�fC#�fC%�fC'�fC)�fC+�fC-�fC/�fC1�fC3�fC5�fC7�fC9�fC;�fC=�fC?�fCA�fCD  CE�fCG�fCI�fCK�fCM�fCO�fCQ�fCS�fCU�fCW�fCY�fC[�fC]�fC_�fCa�fCc�fCe�fCg�fCi�fCk�fCm�fCo�fCq�fCs�fCu�fCw�fCy�fC{�fC}�fC�fC��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��fC��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��fC��3C��3C��3C��3C��3C��3C��3C��3C��3C��3D y�D ��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��D	y�D	��D
y�D
��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��D y�D ��D!y�D!��D"y�D"��D#y�D#��D$y�D$��D%y�D%��D&y�D&��D'y�D(  D(y�D(��D)y�D)��D*y�D*��D+y�D+��D,y�D,��D-y�D-��D.y�D.��D/y�D/��D0y�D0��D1y�D1��D2y�D2��D3y�D3��D4y�D4��D5y�D5��D6y�D6��D7y�D7��D8y�D8��D9y�D9��D:y�D:��D;y�D;��D<y�D<��D=y�D=��D>y�D>��D?y�D?��D@y�D@��DAy�DA��DBy�DB��DCy�DC��DDy�DD��DEy�DE��DFy�DF��DGy�DG��DHy�DH��DIy�DI��DJy�DJ��DKy�DK��DLy�DL��DMy�DM��DNy�DN��DOy�DO��DPy�DP��DQy�DQ��DRy�DR��DSy�DS��DTy�DT��DUy�DU��DVy�DV��DWy�DW��DXy�DX��DYy�DY��DZy�DZ��D[y�D[��D\y�D\��D]y�D]��D^y�D^��D_y�D_��D`y�D`��Day�Da��Dby�Db��Dcy�Dc��Ddy�Dd��Dey�De��Dfy�Df��Dgy�Dg��Dhy�Dh��Diy�Di��Djy�Dj��Dky�Dk��Dly�Dl��Dmy�Dm��Dny�Dn��Doy�Do��Dpy�Dp��Dqy�Dq��Dry�Dr��Dsy�Ds��Dty�Dx�qD�gD�6gD�o
D�ٚD��D�G
D���D���D� D�2�D��{DǼ{D�\D�K�D�t)D๚D�\D�H D�p�D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��Aݴ9Aݴ9AݸRAݼjA���A���A�A�ƨA�ĜA�ĜAݴ9Aݏ\A��AܸRAܓuA�p�A�M�A�5?A��A�oA�1A�1A�1A�JA�bA��A�;dA�7LA�bA���A�ȴAۺ^Aۙ�A�+AٮA׶FA�%AӼjA��A�AͮA̋DA��`A��A���A���A��PA�hsA�G�A�7LA���A�ZA�-A���A��;A���A�|�A�=qA��wA�l�A�ȴA�ƨA���A���A���A�\)A�ĜA��RA�z�A���A���A��/A��
A��A�x�A�jA� �A���A�;dA�1A���A�/A��jA�x�A�  A�"�A�dZA�E�A�1A���A��DA��FA��9A��A��wA���A��#A��hA��A��A�/A�=qA�A���A��`A�/A��A�-A�ȴA�l�A~�jA~$�A}�^A|1'A{p�Az=qAx�AwAvQ�Au?}As�As��As�Ar�+AqK�An�+AjM�Ag&�Abv�A^�A]?}A["�AYx�AWp�AV�AUt�AS�TAR�/AQt�AOoAM��AK?}AH�DAF�ACl�AB �A?t�A=`BA<��A<  A:n�A9��A89XA7\)A5�#A3ƨA0�DA0�A/��A/�#A/�A/\)A-�A,1A+�hA*�A*ZA*JA)��A)�wA)�PA(A�A'�wA&�\A%hsA$��A#+A!K�A �9A
=A �A�A�A�
A�A/AA�Az�A?}A=qA��A&�A7LA�+A�^A	��A��A�AO�A��A �A��A��A{A
=A�^A�7A�@��;@�"�@�-@���@�;d@��^@���@��!@�O�@��;@�
=@�@�J@�bN@�@�`B@�&�@���@�bN@�b@�\)@��@��@��;@�$�@�r�@��@�=q@���@���@�M�@�X@��@؛�@��;@�dZ@���@��@�;d@��y@ҸR@җ�@�v�@���@�(�@�
=@�{@͙�@�V@��;@�dZ@��#@�Q�@�(�@�|�@�$�@���@ÍP@�@�
=@��@��y@�@�$�@���@�p�@�`B@�7L@�r�@�"�@���@��!@�M�@��#@��7@��@��@��F@�S�@�o@�M�@��-@�?}@���@��
@�
=@�~�@��@�I�@��
@���@�ff@�@�E�@�E�@��#@�&�@���@�z�@��;@���@��w@��w@�+@��y@��+@��#@��T@�X@�dZ@�E�@�p�@���@��@��@�V@���@��@���@���@���@��@���@�|�@��@�M�@���@��D@��m@�\)@��y@���@�ȴ@�ȴ@���@�J@��7@�G�@�&�@�%@���@�z�@��@��F@��@���@�S�@�"�@��@��R@�v�@��@�hs@�7L@�&�@���@�r�@� �@�1@���@��
@���@��@�+@��R@���@�=q@�{@�{@���@�x�@�7L@�V@���@�Ĝ@�Q�@� �@��F@��P@�33@��@��@��@��@��@��R@�ȴ@���@��\@�^5@�{@���@�p�@�?}@��@���@��j@��@��@���@��w@��w@��P@�"�@��H@��@�@�`B@�X@��9@��@�1@��;@��P@�l�@�K�@��@�V@�{@��^@���@��@�`B@�V@���@��`@���@�Z@�b@���@�
=@��y@��@���@�^5@�=q@���@��@��#@�@�@��-@�hs@�7L@��@���@�b@��@��@�|�@���@��@�dZ@�;d@�@��R@�v�@�{@��@���@���@���@�z�@�I�@�9X@�ƨ@��P@�l�@�o@�~�@���@��@���@�x�@�O�@�V@��9@��u@�z�@�Q�@~� @th�@k"�@a��@[v`@Uj@N��@Hg8@>�m@8m�@26�@-q@(��@$��@ N�@�]@��@J�@��@	IR@r�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   Aݴ9Aݴ9AݸRAݼjA���A���A�A�ƨA�ĜA�ĜAݴ9Aݏ\A��AܸRAܓuA�p�A�M�A�5?A��A�oA�1A�1A�1A�JA�bA��A�;dA�7LA�bA���A�ȴAۺ^Aۙ�A�+AٮA׶FA�%AӼjA��A�AͮA̋DA��`A��A���A���A��PA�hsA�G�A�7LA���A�ZA�-A���A��;A���A�|�A�=qA��wA�l�A�ȴA�ƨA���A���A���A�\)A�ĜA��RA�z�A���A���A��/A��
A��A�x�A�jA� �A���A�;dA�1A���A�/A��jA�x�A�  A�"�A�dZA�E�A�1A���A��DA��FA��9A��A��wA���A��#A��hA��A��A�/A�=qA�A���A��`A�/A��A�-A�ȴA�l�A~�jA~$�A}�^A|1'A{p�Az=qAx�AwAvQ�Au?}As�As��As�Ar�+AqK�An�+AjM�Ag&�Abv�A^�A]?}A["�AYx�AWp�AV�AUt�AS�TAR�/AQt�AOoAM��AK?}AH�DAF�ACl�AB �A?t�A=`BA<��A<  A:n�A9��A89XA7\)A5�#A3ƨA0�DA0�A/��A/�#A/�A/\)A-�A,1A+�hA*�A*ZA*JA)��A)�wA)�PA(A�A'�wA&�\A%hsA$��A#+A!K�A �9A
=A �A�A�A�
A�A/AA�Az�A?}A=qA��A&�A7LA�+A�^A	��A��A�AO�A��A �A��A��A{A
=A�^A�7A�@��;@�"�@�-@���@�;d@��^@���@��!@�O�@��;@�
=@�@�J@�bN@�@�`B@�&�@���@�bN@�b@�\)@��@��@��;@�$�@�r�@��@�=q@���@���@�M�@�X@��@؛�@��;@�dZ@���@��@�;d@��y@ҸR@җ�@�v�@���@�(�@�
=@�{@͙�@�V@��;@�dZ@��#@�Q�@�(�@�|�@�$�@���@ÍP@�@�
=@��@��y@�@�$�@���@�p�@�`B@�7L@�r�@�"�@���@��!@�M�@��#@��7@��@��@��F@�S�@�o@�M�@��-@�?}@���@��
@�
=@�~�@��@�I�@��
@���@�ff@�@�E�@�E�@��#@�&�@���@�z�@��;@���@��w@��w@�+@��y@��+@��#@��T@�X@�dZ@�E�@�p�@���@��@��@�V@���@��@���@���@���@��@���@�|�@��@�M�@���@��D@��m@�\)@��y@���@�ȴ@�ȴ@���@�J@��7@�G�@�&�@�%@���@�z�@��@��F@��@���@�S�@�"�@��@��R@�v�@��@�hs@�7L@�&�@���@�r�@� �@�1@���@��
@���@��@�+@��R@���@�=q@�{@�{@���@�x�@�7L@�V@���@�Ĝ@�Q�@� �@��F@��P@�33@��@��@��@��@��@��R@�ȴ@���@��\@�^5@�{@���@�p�@�?}@��@���@��j@��@��@���@��w@��w@��P@�"�@��H@��@�@�`B@�X@��9@��@�1@��;@��P@�l�@�K�@��@�V@�{@��^@���@��@�`B@�V@���@��`@���@�Z@�b@���@�
=@��y@��@���@�^5@�=q@���@��@��#@�@�@��-@�hs@�7L@��@���@�b@��@��@�|�@���@��@�dZ@�;d@�@��R@�v�@�{@��@���@���@���@�z�@�I�@�9X@�ƨ@��P@�l�@�o@�~�@���@��@���@�x�@�O�@�V@��9@��u@�z�G�O�@~� @th�@k"�@a��@[v`@Uj@N��@Hg8@>�m@8m�@26�@-q@(��@$��@ N�@�]@��@J�@��@	IR@r�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
��B
ɺB
��B
��B
��B
��B
��B
��B
��B
��B
�)B
��BJ�B�DB��B��B�!B�^BBȴB��B��B��B�B�B�ZBB%�B>wBH�BS�BVBZBZBVBT�BiyBv�Bu�B��B��B��B�/B�;B�ZB��BBB+B1B%B+BBB��B��B�B�B�mB�`B�BB��B��B�^B�LB�9B�B��B�oB�Bn�B:^B,B(�B%�B$�B�B�B	7B��B�B�B�ZB�NB�B��B��B�qB�B��B�VB|�Bv�Bm�BbNBJ�B@�B<jB33B'�BhB+BB
�fB
�HB
�B
��B
�wB
�LB
�!B
��B
��B
��B
�hB
�DB
�B
u�B
l�B
aHB
XB
Q�B
O�B
M�B
B�B
33B
�B	�B	��B	�B	��B	�=B	~�B	s�B	hsB	aHB	\)B	Q�B	I�B	>wB	33B	)�B	�B	\B	B��B�B�`B�;B�5B�;B�B�
B��B��BƨB�qB�?B�9B�3B�-B�'B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�uB�bB�7B�B�%B�%B�B�%B�B�B�B�B�B�B�B{�Bw�Bv�Bv�Bw�Bw�Bw�Bu�Bs�Bu�Bz�B� B�B�+B�1B�+B�%B�%B�%B�JB�VB�\B�hB�uB�hB�JB�1B�=B�VB�oB��B��B��B��B��B��B��B��B��B��B��B��B�!B�FB�^B�^B�dB�jB�dB�^B��BBB��B��B��B��BÖBƨBɺB��BȴBŢBĜBBĜBɺB��B��B��B��B��B�
B�B�)B�/B�5B�;B�;B�;B�;B�HB�mB�sB�yB�B�B�B��B��B	  B	B	B	B	%B	1B	
=B	DB	PB	VB	\B	{B	�B	�B	�B	�B	!�B	"�B	%�B	(�B	,B	0!B	2-B	33B	33B	6FB	7LB	9XB	;dB	;dB	=qB	=qB	;dB	9XB	:^B	@�B	C�B	F�B	G�B	G�B	H�B	K�B	N�B	Q�B	T�B	W
B	[#B	_;B	aHB	`BB	_;B	aHB	e`B	hsB	l�B	o�B	r�B	v�B	w�B	{�B	~�B	� B	� B	�B	�%B	�DB	�JB	�JB	�PB	�VB	�hB	�oB	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�'B	�3B	�9B	�RB	�^B	�dB	�jB	�wB	�wB	��B	��B	B	ÖB	ƨB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�)B	�/B	�/B	�5B	�;B	�;B	�BB	�TB	�ZB	�ZB	�ZB	�`B	�`B	�sB	�B	�B	�yB	�yB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
+B
1B
	7B
DB
JB
JB
JB
PB
VB
hB
hB
hB
oB
oB
uB
uB
{B
{B
{B
�B
�B
�B
�B
�B
�B
$�B
,"B
5ZB
;�B
CB
H�B
L�B
U2B
ZB
^�B
b�B
f�B
i�B
p!B
utB
x�B
}VB
�;B
�mB
�	11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   B
�B
�B
�B
�B
�%B
�2B
�.B
�B
�2B
�.B
�~B
�!BGB��B��B�B�pB��B��B��B�"B�-B�6B�OB�eB�B�SB"0B:�BEBPABRNBVdBVfBRRBQFBe�BsBrB��B�B�EB�zB�~B�B�B�KB�ZBsByBjBpBeB�KB�0B�B��B��B�B�B܇B�<B��B��B��B��B�^B�B��B^Bj�B6�B(YB%EB"0B!,BB�B�B�5B�B��B�BޜB�hB�B��B��B�nB�B��ByCBsBi�B^�BGB<�B8�B/�B$KB�B�B
�`B
��B
ݪB
�wB
�(B
��B
��B
�~B
�:B
�B
�B
��B
��B
}dB
r"B
h�B
]�B
TuB
NNB
LAB
J3B
>�B
/�B
�B	��B	�-B	�iB	��B	��B	{`B	pB	d�B	]�B	X�B	NTB	F#B	:�B	/�B	&fB	B	�B�sB�4B�B��B۬BڡB۫B֋B�zB�[B�HB�B��B��B��B��B��B��B��B�yB�gB�[B�VB�UB�QB�TB�WB�HB�NB�@B�;B�4B�,B�$B�B�B��B��B��B��B��B��B��B��B��B��B~}B}xB}xB��B��B~Bx[BtBBs<Bs<BtCBtABtDBr5Bp*Br7BwXB|rB�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�)B�*B�&B�!B�'B�!B�#B�5B�EB�RB��B��B��B��B��B��B��B��B��B�B�B��B��B��B��B�
B�B�*B�4B�+B�B�B�B�B�+B�:B�OB�RB�PB�eBӀBՈB؟B٢BکB۱BۮB۬BۯBݻB��B��B��B��B�B�&B�VB�fB�rB��B��B	 �B	�B	�B	�B	�B		�B	
�B	�B	�B	�B	B	 B	-B	>B	EB	"TB	%hB	(yB	,�B	.�B	/�B	/�B	2�B	3�B	5�B	7�B	7�B	9�B	9�B	7�B	5�B	6�B	<�B	@B	CB	DB	DB	E'B	H8B	KHB	N]B	QoB	SyB	W�B	[�B	]�B	\�B	[�B	]�B	a�B	d�B	h�B	lB	oB	s;B	t?B	xVB	{jB	|qB	|rB	}tB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�(B	�1B	�GB	�NB	�LB	�TB	�TB	�TB	�dB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�/B	�<B	�IB	�OB	�TB	�XB	�bB	�nB	�tB	�B	ՄB	ؙB	ٜB	ٜB	ڤB	۫B	۪B	ܱB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�$B	� B	�$B	�/B	�7B	�6B	�8B	�=B	�CB	�KB	�IB	�VB	�`B	�lB	�mB	�uB	�sB	�sB	�tB	�uB	�B
 �B	�}B	�}B	�B	��B
 �B
 �B	�~B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
	�B

�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�G�O�B
;B
!HB
(�B
1�B
8B
?~B
D�B
IWB
Q�B
V�B
[B
_;B
cB
fPB
l�B
q�B
u=B
y�B
}�B
��B
�v11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.1 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            r =0.9999(+/-0), vertically averaged dS =-0.004(+/-0.002) in PSS-78.                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Map scales: x=6,3; y=2,1. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                            201906040940232019060409402320190604094023  AO  ARCAADJP                                                                    20181121041153    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181121041153  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181121041153  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20190604094023  IP                  G�O�G�O�G�O�                