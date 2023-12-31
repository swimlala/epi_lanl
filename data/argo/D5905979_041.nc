CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2020-06-19T17:09:02Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         ZPRIMARY | https://orcid.org/0000-0001-7324-3159 | Matthew Alkire, University of Washington        @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    7   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    7(   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    7,   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    70   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7@   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7P   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7`   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7h   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7�   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        8   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    8   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    8    DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     8$   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8D   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8H   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8L   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8l   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8�   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8�   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           8�   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
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
_FillValue                    ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �T   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �d   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �h   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �x   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �|   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20200619170902  20220204114414  5905979 UW, Argo                                                        STEPHEN RISER                                                   PRES            TEMP            PSAL               )A   AO  7662                            2C  D   APEX                            8312                            080318                          846 @ؓ��v<1   @ؓ�-��@7�;dZ��c�I�^5?1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    )A   B   B   @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BG��BP  BX  B`  Bh  Bp  Bx  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D ��D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� DfD�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D#��D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/y�D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DHy�DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DN��DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt�3Dy��D�&�D�a�D��HD���D�)�D�^fD��\D��HD�\D�X D��\D��fD�=D�NfDڑHD�D��D�\)D�fD��=111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @���@���@���AffA>ffA^ffA~ffA�33A�33A�33A�33A�33A�33A�33A�33B��B��B��B��B'��B/��B7��B?��BG34BO��BW��B_��Bg��Bo��Bw��B��B���B���B���B���B���B���B���B���B���B�fgB���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���C�fC�fC�fC�fC	�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC!�fC#�fC%�fC'�fC)�fC+�fC-�fC/�fC1�fC3�fC5�fC7�fC9�fC;�fC=�fC?�fCA�fCC�fCE�fCG�fCI�fCK�fCM�fCO�fCQ�fCS�fCU�fCW�fCY�fC[�fC]�fC_�fCa�fCc�fCe�fCg�fCi�fCk�fCm�fCo�fCq�fCs�fCu�fCw�fCy�fC{�fC}�fC�fC��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C�  C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C�  C�  C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C�  C�  C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3D y�D �4Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��D	y�D	��D
y�D
��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D  D� D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��D y�D ��D!y�D!��D"y�D"��D#y�D#�4D$y�D$��D%y�D%��D&y�D&��D'y�D'��D(y�D(��D)y�D)��D*y�D*��D+y�D+��D,y�D,��D-y�D-��D.y�D.��D/s4D/��D0y�D0��D1y�D1��D2y�D2��D3y�D3��D4y�D4��D5y�D5��D6y�D6��D7y�D7��D8y�D8��D9y�D9��D:y�D:��D;y�D;��D<y�D<��D=y�D=��D>y�D>��D?y�D?��D@y�D@��DAy�DA��DBy�DB��DCy�DC��DDy�DD��DEy�DE��DFy�DF��DGy�DG��DHs4DH��DIy�DI��DJy�DJ��DKy�DK��DLy�DL��DMy�DM��DNy�DN�4DOy�DO��DPy�DP��DQy�DQ��DRy�DR��DSy�DS��DTy�DT��DUy�DU��DVy�DV��DWy�DW��DXy�DX��DYy�DY��DZy�DZ��D[y�D[��D\y�D\��D]y�D]��D^y�D^��D_y�D_��D`y�D`��Day�Da��Dby�Db��Dcy�Dc��Ddy�Dd��Dey�De��Dfy�Df��Dgy�Dg��Dhy�Dh��Diy�Di��Djy�Dj��Dky�Dk��Dly�Dl��Dmy�Dm��Dny�Dn��Doy�Do��Dpy�Dp��Dqy�Dq��Dry�Dr��Dsy�Ds��Dty�Dt��Dy�gD�#�D�^�D��D���D�&gD�[3D��)D��D�)D�T�D��)D��3D�
D�K3DڎD�\D�qD�X�D�3D��
111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�ZA�ZA�ZA�^5A�^5A�XA�XA�XA�\)A�\)A�ZA�I�A�;dA�9XA�9XA�7LA�5?A�33A�1'A�1'A�1'A�/A�-A�-A�(�A�(�A�&�A�$�A�"�A�"�A� �A��A��A��A��A��A��A�
=A���A��HAǮA�"�A�A���A�A�dZA���A��;A��A��PA�K�A���A�I�A�\)A��A���A�  A�5?A�M�A�
=A��A��\A���A�A�oA��-A�ffA��A��A��#A�S�A��A��;A�C�A���A��A��hA��mA�$�A�(�A�%A�z�A��`A�K�A��A��A��A��/A���A��A���A�bA�9XA��PA��`A�ƨA�ȴA�A�A�/A��RA���A�bNA�?}A��^A�I�A�`BA���A�O�A�mA�A|��A{x�Az�HAy��AwO�Av�Au�#At��ArM�ApbNAm��Ak��AjffAh��Ag�Af��Ad��Ad��Ad{Aap�A_+A]�A\��AZA�AXr�AWp�AU��AR^5AQ�AQXAPZAMhsAK�AJM�AI�
AI\)AHr�AG?}AFbNAD�AC\)ABJA@bNA=ƨA<VA:�A:��A:ZA9G�A7hsA4M�A2E�A1�wA1x�A0�A.^5A-t�A,ȴA,1A+�hA*��A)�mA(bA'�-A%�A%��A%hsA$��A#G�A �RA =qA�A��A5?AdZA+AZA+A�DA�A��AhsA�AjA��A|�A�yA1'AdZAr�A5?A��A%A^5A�Ax�A�A{Ax�A;dA�yAbNA7LA
�HA
(�A��AS�A�A��Ar�A��A�A�/A~�A�;A"�A n�@�@�M�@�@���@��j@��@�ff@�b@���@�/@���@��@�
=@��@�Q�@�P@��@�M�@�hs@�j@�@�X@�@߮@�O�@�9X@�^5@ٙ�@؃@׾w@�ff@���@���@щ7@�(�@ϥ�@�C�@Ο�@�$�@��@��T@��#@͙�@� �@���@�n�@�{@�p�@�S�@�V@�J@���@î@�5?@�G�@��j@�A�@�ƨ@�;d@���@�{@��^@�G�@��9@��m@�|�@�+@���@��^@�9X@��m@�ƨ@���@�l�@�K�@��@��T@��7@��@�`B@�G�@�V@�I�@��@�5?@�@�/@�I�@��F@�t�@�+@��y@�V@���@���@��@�j@��@�t�@�o@�n�@���@�@��-@��-@��7@��@��D@�b@�S�@�M�@��-@�x�@�`B@�/@���@��j@�bN@�1'@�dZ@��T@�O�@���@��;@��@�33@�o@���@���@��@���@�z�@��@��D@��u@��9@��D@��@���@�C�@���@��R@��+@�$�@��@���@�G�@�Ĝ@�\)@�M�@���@���@�@�7L@�/@�&�@��j@��@�r�@�b@�|�@��@�+@�"�@�
=@��y@�ȴ@���@���@�O�@��`@���@��F@�|�@��F@��w@���@���@��w@�+@�^5@���@���@���@�M�@�hs@�p�@�V@��m@�|�@��!@��#@�/@�Ĝ@���@�z�@�1'@�I�@�bN@���@��`@�I�@�1'@��
@��@���@���@���@���@�~�@�^5@�5?@���@�Ĝ@���@�x�@��^@�?}@���@���@��9@��@�/@�?}@�V@�Z@���@��@�t�@��@�;d@�@�S�@�33@�V@��+@�|�@�K�@�"�@���@�$�@�J@�@�@�@�@�/@�1@�|�@�@���@�1'@��@��;@���@�l�@�S�@�K�@�33@��@�@��@�Y�@|~@q5�@eIR@]�@V��@OW?@G�	@A��@<@7��@2L0@)ԕ@%@"��@\�@Ɇ@_@c�@��@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A�ZA�ZA�ZA�^5A�^5A�XA�XA�XA�\)A�\)A�ZA�I�A�;dA�9XA�9XA�7LA�5?A�33A�1'A�1'A�1'A�/A�-A�-A�(�A�(�A�&�A�$�A�"�A�"�A� �A��A��A��A��A��A��A�
=A���A��HAǮA�"�A�A���A�A�dZA���A��;A��A��PA�K�A���A�I�A�\)A��A���A�  A�5?A�M�A�
=A��A��\A���A�A�oA��-A�ffA��A��A��#A�S�A��A��;A�C�A���A��A��hA��mA�$�A�(�A�%A�z�A��`A�K�A��A��A��A��/A���A��A���A�bA�9XA��PA��`A�ƨA�ȴA�A�A�/A��RA���A�bNA�?}A��^A�I�A�`BA���A�O�A�mA�A|��A{x�Az�HAy��AwO�Av�Au�#At��ArM�ApbNAm��Ak��AjffAh��Ag�Af��Ad��Ad��Ad{Aap�A_+A]�A\��AZA�AXr�AWp�AU��AR^5AQ�AQXAPZAMhsAK�AJM�AI�
AI\)AHr�AG?}AFbNAD�AC\)ABJA@bNA=ƨA<VA:�A:��A:ZA9G�A7hsA4M�A2E�A1�wA1x�A0�A.^5A-t�A,ȴA,1A+�hA*��A)�mA(bA'�-A%�A%��A%hsA$��A#G�A �RA =qA�A��A5?AdZA+AZA+A�DA�A��AhsA�AjA��A|�A�yA1'AdZAr�A5?A��A%A^5A�Ax�A�A{Ax�A;dA�yAbNA7LA
�HA
(�A��AS�A�A��Ar�A��A�A�/A~�A�;A"�A n�@�@�M�@�@���@��j@��@�ff@�b@���@�/@���@��@�
=@��@�Q�@�P@��@�M�@�hs@�j@�@�X@�@߮@�O�@�9X@�^5@ٙ�@؃@׾w@�ff@���@���@щ7@�(�@ϥ�@�C�@Ο�@�$�@��@��T@��#@͙�@� �@���@�n�@�{@�p�@�S�@�V@�J@���@î@�5?@�G�@��j@�A�@�ƨ@�;d@���@�{@��^@�G�@��9@��m@�|�@�+@���@��^@�9X@��m@�ƨ@���@�l�@�K�@��@��T@��7@��@�`B@�G�@�V@�I�@��@�5?@�@�/@�I�@��F@�t�@�+@��y@�V@���@���@��@�j@��@�t�@�o@�n�@���@�@��-@��-@��7@��@��D@�b@�S�@�M�@��-@�x�@�`B@�/@���@��j@�bN@�1'@�dZ@��T@�O�@���@��;@��@�33@�o@���@���@��@���@�z�@��@��D@��u@��9@��D@��@���@�C�@���@��R@��+@�$�@��@���@�G�@�Ĝ@�\)@�M�@���@���@�@�7L@�/@�&�@��j@��@�r�@�b@�|�@��@�+@�"�@�
=@��y@�ȴ@���@���@�O�@��`@���@��F@�|�@��F@��w@���@���@��w@�+@�^5@���@���@���@�M�@�hs@�p�@�V@��m@�|�@��!@��#@�/@�Ĝ@���@�z�@�1'@�I�@�bN@���@��`@�I�@�1'@��
@��@���@���@���@���@�~�@�^5@�5?@���@�Ĝ@���@�x�@��^@�?}@���@���@��9@��@�/@�?}@�V@�Z@���@��@�t�@��@�;d@�@�S�@�33@�V@��+@�|�@�K�@�"�@���@�$�@�J@�@�@�@�@�/@�1@�|�@�@���@�1'@��@��;@���@�l�@�S�@�K�@�33@��@�G�O�@�Y�@|~@q5�@eIR@]�@V��@OW?@G�	@A��@<@7��@2L0@)ԕ@%@"��@\�@Ɇ@_@c�@��@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB#�B#�B#�B#�B#�B"�B"�B"�B"�B"�B"�B"�B!�B!�B!�B!�B �B �B �B �B �B �B �B �B �B �B �B �B �B �B �B�B �B �B!�B!�B!�B#�B#�B#�B1'BM�BR�B^5BffBdZBe`BiyBjBjBjBiyBhsBk�Bp�Bu�Bv�By�By�Bz�B|�B}�B�B� B{�Bw�Bu�Bq�Bk�BjBe`BaHB\)BT�BQ�BK�BJ�BB�B:^B49B$�B�BuB
=B��B�`BB�3B��B�hBw�Bs�BjB]/BM�B{B
�B
��B
�XB
�3B
��B
��B
��B
�PB
�1B
w�B
m�B
jB
aHB
\)B
M�B
B�B
=qB
7LB
&�B
%�B
�B
�B
B	�B	�5B	ɺB	B	�-B	�B	��B	�uB	�hB	�PB	}�B	l�B	[#B	YB	K�B	?}B	7LB	-B	�B	bB	VB	%B��B�`B�BB�)B�B��B��BĜB�qB�3B�B��B��B�oB�VB�=B�1B�B{�Bx�Bn�Bl�BjBiyBgmBdZBcTBdZBiyBo�Bo�Be`Bl�Bn�Bn�Bn�Bl�Bm�Be`BbNBaHB_;B_;B\)B\)B\)BXBW
BT�BS�BR�BS�BS�BS�BR�BR�BR�BQ�BO�BN�BN�BL�BL�BM�BH�BG�BG�BE�BD�BD�BC�BA�B@�B@�BA�B@�B?}B>wB>wB>wB=qB<jB<jB<jB;dB<jB;dB:^B9XB9XB8RB8RB8RB9XB8RB7LB8RB9XB9XB:^B;dB;dB;dB;dB<jB<jB>wB?}B>wB>wB@�B@�BA�BA�BB�BB�BC�BB�BB�BH�BG�BH�BH�BJ�BK�BJ�BJ�BJ�BJ�BN�BP�BP�BQ�BQ�BYBYBZB_;BcTBgmBhsBiyBjBk�Bm�Bo�Bq�Br�Bs�Bu�Bx�By�Bz�B{�B� B�%B�+B�1B�7B�7B�7B�=B�VB�\B�\B�\B�bB�bB�uB��B��B��B��B��B��B��B��B��B��B��B�B�B�B�-B�9B�LB�XB�}BŢB��B��B��B��B�B�B�#B�5B�5B�;B�HB�TB�yB�B�B�B�B�B�B�B�B�B�B�B�B�B��B	�B	�B	�B	�B	 �B	!�B	$�B	&�B	(�B	,B	-B	.B	.B	/B	1'B	5?B	7LB	7LB	8RB	8RB	?}B	B�B	C�B	D�B	D�B	D�B	E�B	G�B	I�B	J�B	M�B	N�B	P�B	R�B	R�B	R�B	S�B	T�B	]/B	]/B	^5B	aHB	`BB	aHB	e`B	gmB	gmB	hsB	iyB	jB	k�B	r�B	q�B	q�B	n�B	p�B	q�B	v�B	u�B	u�B	u�B	u�B	v�B	w�B	x�B	{�B	|�B	~�B	�B	�=B	�=B	�7B	�DB	�VB	�hB	�oB	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�FB	�XB	�dB	�wB	��B	��B	ĜB	ȴB	ƨB	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�B	�B	�#B	�#B	�#B	�#B	�B	�B	�B	�B	�B	�;B	�;B	�;B	�BB	�HB	�HB	�HB	�HB	�NB	�TB	�TB	�!B	�"B

	B
�B
OB
%,B
+B
4�B
;�B
A�B
F�B
LJB
TB
ZQB
[�B
`�B
f�B
k�B
m�B
r�B
xR111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B`B`B`B`B`BZBZBZBZBZBZBZBTBTBTBTBNBNBNBNBNBNBNBNBNBNBNBNBNBNBNBGBNBNBTBTBTB`B`B`B(�BEZBJyBU�B]�B[�B\�Ba BbBbBbBa B_�BcBh+BmJBnPBqbBqcBriBtvBu|B{�Bw�BsoBoXBmLBi3BcBb	B\�BX�BS�BL�BIxBCTBBNB:B1�B+�BmB7BB�B�bB��B�&B��B�eB�BolBkSBbBT�BEsBB
�WB
�wB
�B
��B
��B
�~B
�HB
��B
�B
oB
eBB
b0B
X�B
S�B
E�B
:CB
5&B
/B
�B
�B
uB
EB	��B	�YB	��B	�xB	�MB	��B	��B	��B	�7B	�*B	�B	u�B	dPB	R�B	P�B	C�B	7EB	/B	$�B	^B	.B	"B��B�B�/B�B��B��B��BØB�nB�DB�B��B��B�{B�EB�-B�B�B|�Bs�Bp�BfrBdeBbYBaSB_GB\5B[/B\5BaTBgxBgxB];BdfBfsBfsBfsBdfBelB]<BZ*BY%BWBWBTBTBTBO�BN�BL�BK�BJ�BK�BK�BK�BJ�BJ�BJ�BI�BG�BF�BF�BD�BD�BE�B@�B?�B?�B=�B<}B<}B;wB9jB8dB8dB9jB8eB7_B6YB6YB6YB5SB4LB4LB4MB3GB4MB3GB2AB1;B1;B05B05B05B1;B06B/0B06B1<B1<B2BB3HB3HB3HB3HB4NB4OB6[B7bB6\B6\B8hB8hB9nB9nB:tB:tB;{B:tB:tB@�B?�B@�B@�BB�BC�BB�BB�BB�BB�BF�BH�BH�BI�BI�BP�BP�BRBWB[8B_QB`WBa]BbcBciBeuBg�Bi�Bj�Bk�Bm�Bp�Bq�Br�Bs�Bw�B~BB�B�B�B�B� B�9B�>B�>B�>B�DB�DB�WB�oB�|B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�-B�9B�]B��B¡BĬB��B��B��B��B�B�B�B�B�&B�2B�WB�oB�B�B�B�vB�vB�cB�vB�B�vB�|B�|B�B��B	bB	�B	�B	�B	�B	�B	�B	�B	 �B	#�B	$�B	%�B	%�B	&�B	)B	-B	/%B	/%B	0+B	0,B	7VB	:hB	;oB	<uB	<uB	<uB	={B	?�B	A�B	B�B	E�B	F�B	H�B	J�B	J�B	J�B	K�B	L�B	UB	UB	VB	YB	XB	YB	]7B	_DB	_DB	`JB	aPB	bVB	c\B	j�B	i�B	i�B	foB	h{B	i�B	n�B	m�B	m�B	m�B	m�B	n�B	o�B	p�B	s�B	t�B	v�B	y�B	�B	�B	�B	�B	�+B	�=B	�DB	�=B	�JB	�VB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�+B	�7B	�JB	�VB	�\B	�nB	��B	�zB	B	B	ğB	ƫB	ÙB	ťB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�%G�O�B	��B	��B
�B
�B
B
�B
"�B
,�B
3gB
9�B
>�B
DB
K�B
RB
SrB
X�B
^gB
cQB
e�B
j�B
p111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.1 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            r =0.9998(+/-0.0001), vertically averaged dS =-0.008(+/-0.002) in PSS-78.                                                                                                                                                                                       Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                      202202041144142022020411441420220204114414  AO  ARCAADJP                                                                    20200619170902    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20200619170902  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20200619170902  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20220204114414  IP                  G�O�G�O�G�O�                