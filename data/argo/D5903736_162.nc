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
_FillValue                    �,Argo profile    3.1 1.2 19500101000000  20181121041153  20190604094023  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  4051                            2C  D   APEX                            5368                            041511                          846 @��^ѻ�1   @��`(d/�@2������d�O�;dZ1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   B   B   @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0ffB8ffB?��BH  BP  BX  B`  Bh  Bp  Bx  B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C�C�C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Do��Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dy�D��D�N�D�x D���D��D�7\D�~�D���D�)D�<)D���D��fD� D�B=DڅD��HD�  D�W�D�D��{11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�  @���@���AffA>ffA^ffA~ffA�33A�33A�33A�33A�33A�33A�33A�33B��B��B��B��B'��B0  B8  B?34BG��BO��BW��B_��Bg��Bo��Bw��B��B�  B�  B���B���B���B���B���B���B���B���B���B���B���B���B�  B���B���B���B���Bϙ�B���B���B���B���B���B���B���B���B���B���B���B���C�fC�fC�fC�fC	�fC  C  C�fC�fC�fC�fC�fC�fC�fC�fC�fC!�fC#�fC%�fC'�fC)�fC+�fC-�fC/�fC1�fC3�fC5�fC7�fC9�fC;�fC=�fC?�fCA�fCC�fCE�fCG�fCI�fCK�fCM�fCO�fCQ�fCS�fCU�fCW�fCY�fC[�fC]�fC_�fCa�fCc�fCe�fCg�fCi�fCk�fCm�fCo�fCq�fCs�fCu�fCw�fCy�fC{�fC}�fC�fC��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C�  C�  C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3D y�D ��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��D	y�D	��D
y�D
��Dy�D��Dy�D��Dy�D��Dy�D��D� D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��D y�D ��D!y�D!��D"y�D"��D#y�D#��D$y�D$��D%y�D%��D&y�D&��D'y�D'��D(y�D(��D)y�D)��D*y�D*��D+y�D+��D,y�D,��D-y�D-��D.y�D.��D/y�D/��D0y�D0��D1y�D1��D2y�D2��D3y�D3��D4y�D4��D5y�D5��D6y�D6��D7y�D7��D8y�D8��D9y�D9��D:y�D:��D;y�D;��D<y�D<��D=y�D=��D>y�D>��D?y�D?��D@y�D@��DAy�DA��DBy�DB��DCy�DC��DDy�DD��DEy�DE��DFy�DF��DGy�DG��DHy�DH��DIy�DI��DJy�DJ��DKy�DK��DLy�DL��DMy�DM��DNy�DN��DOy�DO��DPy�DP��DQy�DQ��DRy�DR��DSy�DS��DTy�DT��DUy�DU��DVy�DV��DWy�DW��DXy�DX��DYy�DY��DZy�DZ��D[y�D[��D\y�D\��D]y�D]��D^y�D^��D_y�D_��D`y�D`��Day�Da��Dby�Db��Dcy�Dc��Ddy�Dd��Dey�De��Dfy�Df��Dgy�Dg��Dhy�Dh��Diy�Di��Djy�Dj��Dky�Dk��Dly�Dl��Dmy�Dm��Dny�Dn��Doy�Do�4Dpy�Dp��Dqy�Dq��Dry�Dr��Dsy�Ds��Dty�Dy��D��D�K�D�t�D���D��D�4)D�{�D�ڐD��D�8�D�}�D��3D��D�?
Dځ�D��D���D�T{D�gD��H11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�p�A���A�ƨAީ�Aޙ�Aޏ\AޅA�v�A�hsA�^5A�K�A�;dA�{Aݟ�A�n�A�I�A�1Aܥ�A�I�Aۉ7A�
=A�=qA�^5A�C�A���A�I�A�jA��yA�\)A˺^A�(�A�O�A� �A��AǕ�A�S�A�  A��/A�A�A��`A�l�A��A��A���A�"�A��A��jA�C�A�  A�A�A�E�A��A��A�JA�z�A�dZA���A�E�A��`A���A��/A�?}A���A���A���A�x�A�A�C�A��A��A��A�r�A�(�A���A���A�-A�ZA�;dA���A��A�  A�+A�A�-A�hsA���A�\)A��7A��A�M�A���A�7LA�jA���A�bA��+A��A�JA��/A�z�A�XA�A�r�A��;A��A�(�A��hA���A�1'A�A��A�A{�Au��AsƨArM�Ao�Ajn�AeC�Ac�AaG�A_A^=qA]dZA\-AZz�AY�AY��AY\)AYVAXbNAW��AV�AV �AT��AR(�AN��AL~�AJ��AH��AG�
AF�yAE&�AC��AC�AC�mACAA"�A=`BA;�A:�HA9`BA7ƨA6��A5A3��A/;dA+33A*M�A)��A)S�A(��A(^5A(bA'�;A'�hA&��A%�
A%?}A$z�A#\)A"�A!l�A jA��A��A%AI�A��A��AZA��AVAC�A�!A��A�+A�A��A�HAp�A�Av�A��A
��A
VA
bA	��A7LA��A��AffAl�A33A�\A+@��@�;d@���@���@���@�=q@��@��h@��@�r�@�Q�@���@�A�@�l�@�{@���@�hs@�9@�bN@�dZ@�@��`@� �@���@��@��@�b@�t�@�+@��T@�b@�$�@�&�@�Z@�l�@ߥ�@�
=@��@ߝ�@��@�(�@�l�@ٺ^@�A�@�;d@�ff@ՙ�@�A�@�K�@�M�@љ�@�z�@�(�@�  @�\)@�{@��`@˾w@�V@�V@�v�@�%@�Z@�K�@�$�@�@�O�@��`@ě�@�A�@öF@�ȴ@�-@��T@��h@�G�@��@�K�@��@��H@���@���@�ff@�X@�1'@��w@��\@���@��u@�t�@�dZ@��+@�/@�z�@�Q�@�(�@��@��!@�E�@�$�@�{@��@���@�7L@�z�@�|�@��R@�E�@�@��@�O�@��u@�A�@�A�@�b@�dZ@��\@�ff@�E�@��@���@�V@��@�j@��@�  @���@��@�"�@���@�-@��@���@��h@�V@�Z@�ƨ@���@�t�@�K�@�;d@��@���@�$�@��h@�hs@�`B@�?}@��@���@��j@��@�b@� �@� �@�9X@�(�@�ƨ@�l�@�33@�@��@�v�@�-@�$�@�J@��@��T@��^@�G�@���@��D@���@�S�@�;d@��@���@�^5@�V@�-@�@��@�hs@�?}@���@���@�Q�@�1@�  @��F@�t�@�K�@��@�ȴ@���@�v�@�E�@���@��h@�G�@�O�@�X@�O�@�?}@���@��#@��^@�@��h@�`B@�&�@��@��j@���@�z�@�Q�@�  @���@�ƨ@���@�;d@�"�@��+@�hs@��@���@�z�@�b@��@�  @� �@�t�@�l�@��@���@��!@��\@�M�@�M�@��+@��!@��\@�E�@��@���@�`B@�?}@���@�Z@���@�33@�o@�K�@�33@�C�@�|�@�;d@���@��\@��^@�X@���@��@��@�bN@��j@���@���@��/@���@��9@�j@� �@���@�ƨ@��F@��P@��R@���@�V@�-@��@���@zYK@o��@g�@`�O@X�.@R�@H�[@A�@9��@2�@+��@&i�@ �@��@�@�@�A@M@��@Y11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   A�p�A���A�ƨAީ�Aޙ�Aޏ\AޅA�v�A�hsA�^5A�K�A�;dA�{Aݟ�A�n�A�I�A�1Aܥ�A�I�Aۉ7A�
=A�=qA�^5A�C�A���A�I�A�jA��yA�\)A˺^A�(�A�O�A� �A��AǕ�A�S�A�  A��/A�A�A��`A�l�A��A��A���A�"�A��A��jA�C�A�  A�A�A�E�A��A��A�JA�z�A�dZA���A�E�A��`A���A��/A�?}A���A���A���A�x�A�A�C�A��A��A��A�r�A�(�A���A���A�-A�ZA�;dA���A��A�  A�+A�A�-A�hsA���A�\)A��7A��A�M�A���A�7LA�jA���A�bA��+A��A�JA��/A�z�A�XA�A�r�A��;A��A�(�A��hA���A�1'A�A��A�A{�Au��AsƨArM�Ao�Ajn�AeC�Ac�AaG�A_A^=qA]dZA\-AZz�AY�AY��AY\)AYVAXbNAW��AV�AV �AT��AR(�AN��AL~�AJ��AH��AG�
AF�yAE&�AC��AC�AC�mACAA"�A=`BA;�A:�HA9`BA7ƨA6��A5A3��A/;dA+33A*M�A)��A)S�A(��A(^5A(bA'�;A'�hA&��A%�
A%?}A$z�A#\)A"�A!l�A jA��A��A%AI�A��A��AZA��AVAC�A�!A��A�+A�A��A�HAp�A�Av�A��A
��A
VA
bA	��A7LA��A��AffAl�A33A�\A+@��@�;d@���@���@���@�=q@��@��h@��@�r�@�Q�@���@�A�@�l�@�{@���@�hs@�9@�bN@�dZ@�@��`@� �@���@��@��@�b@�t�@�+@��T@�b@�$�@�&�@�Z@�l�@ߥ�@�
=@��@ߝ�@��@�(�@�l�@ٺ^@�A�@�;d@�ff@ՙ�@�A�@�K�@�M�@љ�@�z�@�(�@�  @�\)@�{@��`@˾w@�V@�V@�v�@�%@�Z@�K�@�$�@�@�O�@��`@ě�@�A�@öF@�ȴ@�-@��T@��h@�G�@��@�K�@��@��H@���@���@�ff@�X@�1'@��w@��\@���@��u@�t�@�dZ@��+@�/@�z�@�Q�@�(�@��@��!@�E�@�$�@�{@��@���@�7L@�z�@�|�@��R@�E�@�@��@�O�@��u@�A�@�A�@�b@�dZ@��\@�ff@�E�@��@���@�V@��@�j@��@�  @���@��@�"�@���@�-@��@���@��h@�V@�Z@�ƨ@���@�t�@�K�@�;d@��@���@�$�@��h@�hs@�`B@�?}@��@���@��j@��@�b@� �@� �@�9X@�(�@�ƨ@�l�@�33@�@��@�v�@�-@�$�@�J@��@��T@��^@�G�@���@��D@���@�S�@�;d@��@���@�^5@�V@�-@�@��@�hs@�?}@���@���@�Q�@�1@�  @��F@�t�@�K�@��@�ȴ@���@�v�@�E�@���@��h@�G�@�O�@�X@�O�@�?}@���@��#@��^@�@��h@�`B@�&�@��@��j@���@�z�@�Q�@�  @���@�ƨ@���@�;d@�"�@��+@�hs@��@���@�z�@�b@��@�  @� �@�t�@�l�@��@���@��!@��\@�M�@�M�@��+@��!@��\@�E�@��@���@�`B@�?}@���@�Z@���@�33@�o@�K�@�33@�C�@�|�@�;d@���@��\@��^@�X@���@��@��@�bN@��j@���@���@��/@���@��9@�j@� �@���@�ƨ@��F@��P@��R@���@�V@�-G�O�@���@zYK@o��@g�@`�O@X�.@R�@H�[@A�@9��@2�@+��@&i�@ �@��@�@�@�A@M@��@Y11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�JB��B�B�B�B�B�B�B�B�B�B�B�BBJB�B$�B6FBD�BdZB�Bq�B^5BO�BK�BI�BH�BM�BVB_;BbNBv�B�1B�DB�7B�uB��B�B�3B�FB�^B��BÖBƨBȴB��B��B�B��B��BŢB��B�`B�yB�mB�TB�HB�;B�)B�
BB��B�wB�3B��B��B��B��B��B�\Bm�Bl�Bq�BffB_;BYBM�BA�B0!B$�B�BbBB��B�B�mB�B�yB�;B��BÖB�B��B�uB�%Bz�Bk�BR�BB�B=qB9XB.B&�B�BVBB
��B
��B
�B
�B
�'B
�+B
hsB
49B
"�B
{B	��B	�B	�RB	�B	��B	�{B	�VB	�PB	�+B	v�B	t�B	u�B	x�B	~�B	|�B	� B	{�B	v�B	k�B	]/B	H�B	<jB	49B	)�B	$�B	�B	�B	oB	hB	\B	
=B��B�B�B�`B�5B�B��B��BǮB�}B�3B�-B�!B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B�uB��B��B��B�{B�uB�bB�7B�B}�B}�B|�Bz�By�Bx�Bx�Bw�Bu�Bu�Bu�Bt�Br�Bo�Bm�BiyBhsBe`BbNB`BB]/BYBVBVBVBVBVBZB[#B\)B]/BbNBk�Bx�B� B�PB�oB�{B�{B��B��B��B��B�{B�{B��B��B��B��B��B��B��B��B��B��B��B�B�9B�?B�}B��BÖB��B��B�}B�}B��B��B��BÖBĜBƨBǮBɺBɺBɺB��B��B��B��B��B��B�B�B�B�B�)B�)B�/B�5B�5B�;B�ZB�B�B�B�B�B��B��B��B��B��B��B��B	  B	B	B	1B	JB	bB	uB	uB	{B	�B	�B	!�B	!�B	 �B	�B	 �B	!�B	"�B	$�B	'�B	+B	,B	/B	49B	6FB	:^B	<jB	?}B	C�B	E�B	H�B	I�B	L�B	M�B	N�B	O�B	P�B	Q�B	T�B	W
B	XB	ZB	[#B	[#B	]/B	_;B	bNB	cTB	dZB	dZB	e`B	hsB	l�B	p�B	q�B	r�B	s�B	t�B	v�B	x�B	z�B	{�B	|�B	}�B	� B	�B	�B	�B	�B	�%B	�+B	�+B	�1B	�1B	�1B	�=B	�VB	�bB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�!B	�'B	�-B	�FB	�RB	�XB	�^B	�dB	�qB	�}B	��B	��B	B	ÖB	ĜB	ŢB	ǮB	ȴB	ȴB	ȴB	ȴB	ǮB	ɺB	��B	��B	��B	��B	��B	�B	�B	�B	�)B	�/B	�5B	�;B	�BB	�HB	�NB	�TB	�TB	�ZB	�ZB	�`B	�mB	�mB	�fB	�ZB	�TB	�TB	�TB	�NB	�TB	�ZB	�`B	�`B	�mB	�sB	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
%B
%B
+B
1B
1B
1B

=B

=B
DB
DB
PB
 B
�B
)B
/iB
88B
?�B
FB
L�B
QB
X�B
^�B
dZB
h>B
mCB
p�B
utB
z�B
}VB
�B
��B
��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   B��BцB�B�*B�*B�-B�1B�*B�"B�#B�B�B�-B��B�BB!dB2�BA"B`�B}�Bn0BZ�BLaBHJBF@BE:BJVBR�B[�B^�BsPB��B��B��B��B�NB��B��B��B��B�B�B�'B�:B�pB�|BҌB�}B�rB�!B�xB��B��B��B��B��B��BدBӑB�B��B��B��B�pB�~B�dB�>B�=B��BjBiBn1Bb�B[�BU�BJWB>B,�B!jB)B�B��B�VB�B��B�B�B��B�yB�#B��B�SB�B��BwsBhBO�B?#B:B5�B*�B#vBMB
�B�B
��B
��B
�6B
լB
��B
��B
eB
0�B
jB
B	��B	նB	��B	��B	�VB	�B	��B	��B	��B	sfB	q]B	reB	uxB	{�B	y�B	|�B	x�B	sjB	h#B	Y�B	ETB	9B	0�B	&�B	!B	[B	1B	B	B	B	�B��B�RB�(B�B��BԴBѦB̅B�VB�&B��B��B��B��B��B��B��B��B��B��B��B��B�yB�oB�aB�OB�CB�)B� B�3B�/B�*B�#B� B�B��B�Bz�Bz�By�Bw�Bv�Bu�Bu�BtzBroBrmBrpBqfBo\BlKBj<Bf&Be!BbB^�B\�BY�BU�BR�BR�BR�BR�BR�BV�BW�BX�BY�B^�Bh3Bu�B|�B��B�B�(B�(B�5B�AB�7B�2B�'B�)B�1B�5B�,B�NB�kB�rB��B��B��B��B��B��B��B��B�)B�3B�@B�6B�.B�&B�*B�0B�5B�6B�?B�IB�SB�YB�bB�dB�cB�mBʀB̇B̌BΘBϚBҲB��B��B��B��B��B��B��B��B��B�B�/B�AB�KB�SB�QB�tB��B��B��B��B��B��B��B	 �B	�B	�B	�B	B	B	 B	%B	7B	`B	uB	wB	oB	gB	oB	vB	}B	!�B	$�B	'�B	(�B	+�B	0�B	2�B	7
B	9B	<&B	@AB	BMB	E^B	FfB	ItB	J}B	K�B	L�B	M�B	N�B	Q�B	S�B	T�B	V�B	W�B	W�B	Y�B	[�B	^�B	`B	aB	aB	bB	eB	i7B	mPB	nWB	o\B	p\B	qeB	ssB	u}B	w�B	x�B	y�B	z�B	|�B	~�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	�	B	�B	�+B	�3B	�4B	�5B	�;B	�9B	�CB	�RB	�bB	�rB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�'B	�*B	�+B	�5B	�<B	�DB	�HB	�RB	�XB	�YB	�]B	�YB	�TB	�aB	�uB	˃B	̇B	ΔB	ѤB	ԸB	ԻB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�
B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�)B	�8B	�5B	�;B	�GB	�VB	�cB	�eB	�hB	�nB	�nB	�zB	�B	�{B	�yB	�pB	�oB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�G�O�B
�B
eB
%�B
,B
4�B
<�B
B�B
I�B
M�B
U�B
[[B
a B
d�B
i�B
mJB
rB
w7B
y�B
|;B
��B
��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.1 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            r =0.9999(+/-0), vertically averaged dS =-0.003(+/-0.002) in PSS-78.                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Map scales: x=6,3; y=2,1. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                            201906040940232019060409402320190604094023  AO  ARCAADJP                                                                    20181121041153    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181121041153  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181121041153  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20190604094023  IP                  G�O�G�O�G�O�                