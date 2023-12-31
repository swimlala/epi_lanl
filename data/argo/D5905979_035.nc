CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2020-06-19T17:09:00Z creation      
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20200619170900  20220204114414  5905979 UW, Argo                                                        STEPHEN RISER                                                   PRES            TEMP            PSAL               #A   AO  7662                            2C  D   APEX                            8312                            080318                          846 @ؐ�g�Ei1   @ؐ�����@8��-�cɩ��l�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    #A   B   B   @�  @�  A   A   A@  A`  A�  A�33A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D>��D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DR��DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Do��Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt�fDy��D��D�eD�� D��{D�"=D�Z�D���D��D�D�[�D��)D�њD�#�D�b�Dڛ3D��
D� RD�S�D�D�ʏ111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @���@���@���AffA>ffA^ffA~ffA�ffA�33A�33A�33A�33A�33A�33A�33B��B��B��B��B'��B/��B7��B?��BG��BO��BW��B_��Bg��Bo��Bw��B��B���B���B���B���B���B���B�  B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���C�fC�fC�fC�fC	�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC!�fC#�fC%�fC'�fC)�fC+�fC-�fC/�fC1�fC3�fC5�fC7�fC9�fC;�fC=�fC?�fCA�fCC�fCE�fCG�fCI�fCK�fCM�fCO�fCQ�fCS�fCU�fCW�fCY�fC[�fC]�fC_�fCa�fCc�fCe�fCg�fCi�fCk�fCm�fCo�fCq�fCs�fCu�fCw�fCy�fC{�fC}�fC�fC��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��fC��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��fC��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3D y�D ��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��D	y�D	��D
y�D
��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��D y�D ��D!y�D!��D"y�D"��D#y�D#��D$y�D$��D%y�D%��D&y�D&��D'y�D'��D(y�D(��D)y�D)��D*y�D*��D+y�D+��D,y�D,��D-y�D-��D.y�D.��D/y�D/��D0y�D0��D1y�D1��D2y�D2��D3y�D3��D4y�D4��D5y�D5��D6y�D6��D7y�D7��D8y�D8��D9y�D9��D:y�D:��D;y�D;��D<y�D<��D=y�D=��D>y�D>�4D?y�D?��D@y�D@��DAy�DA��DBy�DB��DCy�DC��DDy�DD��DEy�DE��DFy�DF��DGy�DG��DHy�DH��DIy�DI��DJy�DJ��DKy�DK��DLy�DL��DMy�DM��DNy�DN��DOy�DO��DPy�DP��DQy�DQ��DRy�DR�4DSy�DS��DTy�DT��DUy�DU��DVy�DV��DWy�DW��DXy�DX��DYy�DY��DZy�DZ��D[y�D[��D\y�D\��D]y�D]��D^y�D^��D_y�D_��D`y�D`��Day�Da��Dby�Db��Dcy�Dc��Ddy�Dd��Dey�De��Dfy�Df��Dgy�Dg��Dhy�Dh��Diy�Di��Djy�Dj��Dky�Dk��Dly�Dl��Dmy�Dm��Dny�Dn��Doy�Do�4Dpy�Dp��Dqy�Dq��Dry�Dr��Dsy�Ds��Dty�Dt� Dy�qD��D�a�D���D��HD�
D�W\D��RD��{D��D�XRD���D��gD� �D�_�Dژ D���D�D�PRD�RD��\111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��Aʹ9AͶFAͲ-AͬAͶFAͶFA͸RA͸RA͸RAͺ^Aͺ^AͼjAͺ^Aʹ9AͮA;wA;wA;wAʹ9AͼjAʹ9A͸RAʹ9AͲ-Aͧ�A͛�A͓uA͑hA͇+A�|�A�n�A�XA�I�A�C�A̶FA�XAÃA��+A�VA��A�1'A��+A���A��A��!A��A���A�-A�5?A�bNA���A��yA�n�A��A�Q�A� �A��A���A�t�A�1A��A�z�A���A�5?A��A��A�Q�A��FA��wA�JA�v�A��HA��-A�`BA��^A�A�bNA���A���A��A��/A��A��A��A�VA���A���A���A�l�A��#A��#A�1'A�/A�p�A��-A��+A�ƨA��A��A���A�G�A���A�l�A�^5A�ffA�7LA�VA��;A�%A�ȴA�5?A�  A��A���A��A"�A~��A}+A|��A|9XAz�`Av��Au|�As��ArffApffAn�AnbNAn$�AmS�AkXAh��Ag��Ae��Ae�Ad��Ac/Aa�mAa+A_�A]�hA]%A[�7AY�hAX1AT�ASoAP��AO�wANv�ALjAK?}AJ��AIdZADA�ABVA@M�A@Q�A>�RA=��A;�^A8�`A7�;A6A�A5VA3��A2��A1��A1/A0A.�A-��A,�A,v�A,^5A,M�A,JA*ZA)/A(�jA(=qA'7LA&r�A%��A%�hA$�DA#/A"jA!��A!O�A!"�A �+A bA��Ar�AƨA"�Ar�AƨA;dA��A  A�7AC�A�AE�A�#A�A9XA�FA�A��A�jA  A\)A��A�
A�HAI�A
=A	��AE�A�PA�/AZA;dA��A�7A�PA ĜA ��A bNA J@���@�O�@�@�%@���@�^5@��@�  @�  @�l�@��@�F@�^5@��/@�@���@�G�@�j@�1@�\@��
@�"�@�~�@��@ݑh@�Ĝ@�V@��@� �@ա�@�%@�/@�X@�&�@�o@щ7@��
@�K�@Η�@��@� �@�l�@ʧ�@�Ĝ@�z�@�I�@�b@��m@�ƨ@Ǖ�@�S�@��y@�-@ŉ7@�G�@��/@��@��/@�1@���@�n�@�@��h@��@��@�K�@�$�@�O�@�I�@�\)@��H@���@�`B@��u@�b@���@��@�l�@�C�@�@���@�V@��T@���@�G�@���@��y@�n�@���@���@��@�b@��R@��@�+@��y@�=q@�%@��u@�Z@��m@���@�@��#@���@�@��@�`B@��`@��u@��w@��;@�A�@���@���@��@�9X@�ƨ@�ff@�E�@�V@���@��R@��h@�I�@��@�@�l�@�33@���@��@�A�@�A�@�I�@���@�5?@��j@�hs@�
=@�|�@���@��@��@��+@�@�O�@��@��-@��@��H@�"�@�S�@���@��@�?}@���@���@�j@��@��@��@�
=@���@�~�@�^5@�=q@�@�\)@�M�@���@��@�;d@�|�@�t�@�dZ@�@���@�v�@�n�@��\@���@�E�@���@�/@�&�@�V@��/@��j@���@�z�@�z�@�A�@���@�33@�o@�"�@��-@�7L@��@�%@��9@�A�@�Q�@�1@��
@��@�  @��@�(�@�1@�1'@� �@���@��
@�|�@���@���@�l�@�K�@�+@��@�
=@��R@��@�`B@��@��9@��u@�A�@��m@��F@�l�@�E�@�^5@��@���@��h@��@��@��D@�Q�@�b@���@�  @���@��;@��w@���@��P@�|�@��@�|�@��@�-@�J@��T@�>B@}�d@qu�@h6@a��@[o@V~�@N�R@Gs@Ahs@:�@6�r@0*�@)��@$7�@ 6@�4@&�@J�@�)@
\�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  Aʹ9AͶFAͲ-AͬAͶFAͶFA͸RA͸RA͸RAͺ^Aͺ^AͼjAͺ^Aʹ9AͮA;wA;wA;wAʹ9AͼjAʹ9A͸RAʹ9AͲ-Aͧ�A͛�A͓uA͑hA͇+A�|�A�n�A�XA�I�A�C�A̶FA�XAÃA��+A�VA��A�1'A��+A���A��A��!A��A���A�-A�5?A�bNA���A��yA�n�A��A�Q�A� �A��A���A�t�A�1A��A�z�A���A�5?A��A��A�Q�A��FA��wA�JA�v�A��HA��-A�`BA��^A�A�bNA���A���A��A��/A��A��A��A�VA���A���A���A�l�A��#A��#A�1'A�/A�p�A��-A��+A�ƨA��A��A���A�G�A���A�l�A�^5A�ffA�7LA�VA��;A�%A�ȴA�5?A�  A��A���A��A"�A~��A}+A|��A|9XAz�`Av��Au|�As��ArffApffAn�AnbNAn$�AmS�AkXAh��Ag��Ae��Ae�Ad��Ac/Aa�mAa+A_�A]�hA]%A[�7AY�hAX1AT�ASoAP��AO�wANv�ALjAK?}AJ��AIdZADA�ABVA@M�A@Q�A>�RA=��A;�^A8�`A7�;A6A�A5VA3��A2��A1��A1/A0A.�A-��A,�A,v�A,^5A,M�A,JA*ZA)/A(�jA(=qA'7LA&r�A%��A%�hA$�DA#/A"jA!��A!O�A!"�A �+A bA��Ar�AƨA"�Ar�AƨA;dA��A  A�7AC�A�AE�A�#A�A9XA�FA�A��A�jA  A\)A��A�
A�HAI�A
=A	��AE�A�PA�/AZA;dA��A�7A�PA ĜA ��A bNA J@���@�O�@�@�%@���@�^5@��@�  @�  @�l�@��@�F@�^5@��/@�@���@�G�@�j@�1@�\@��
@�"�@�~�@��@ݑh@�Ĝ@�V@��@� �@ա�@�%@�/@�X@�&�@�o@щ7@��
@�K�@Η�@��@� �@�l�@ʧ�@�Ĝ@�z�@�I�@�b@��m@�ƨ@Ǖ�@�S�@��y@�-@ŉ7@�G�@��/@��@��/@�1@���@�n�@�@��h@��@��@�K�@�$�@�O�@�I�@�\)@��H@���@�`B@��u@�b@���@��@�l�@�C�@�@���@�V@��T@���@�G�@���@��y@�n�@���@���@��@�b@��R@��@�+@��y@�=q@�%@��u@�Z@��m@���@�@��#@���@�@��@�`B@��`@��u@��w@��;@�A�@���@���@��@�9X@�ƨ@�ff@�E�@�V@���@��R@��h@�I�@��@�@�l�@�33@���@��@�A�@�A�@�I�@���@�5?@��j@�hs@�
=@�|�@���@��@��@��+@�@�O�@��@��-@��@��H@�"�@�S�@���@��@�?}@���@���@�j@��@��@��@�
=@���@�~�@�^5@�=q@�@�\)@�M�@���@��@�;d@�|�@�t�@�dZ@�@���@�v�@�n�@��\@���@�E�@���@�/@�&�@�V@��/@��j@���@�z�@�z�@�A�@���@�33@�o@�"�@��-@�7L@��@�%@��9@�A�@�Q�@�1@��
@��@�  @��@�(�@�1@�1'@� �@���@��
@�|�@���@���@�l�@�K�@�+@��@�
=@��R@��@�`B@��@��9@��u@�A�@��m@��F@�l�@�E�@�^5@��@���@��h@��@��@��D@�Q�@�b@���@�  @���@��;@��w@���@��P@�|�@��@�|�@��@�-@�JG�O�@�>B@}�d@qu�@h6@a��@[o@V~�@N�R@Gs@Ahs@:�@6�r@0*�@)��@$7�@ 6@�4@&�@J�@�)@
\�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB0!B0!B0!B1'B0!B1'B1'B1'B1'B1'B1'B1'B1'B1'B1'B1'B1'B1'B1'B1'B1'B1'B1'B1'B2-B33B33B33B33B2-B2-B2-B33B2-B2-BD�BM�BN�BO�BVBZBYB]/B`BBbNBdZBdZBdZBiyBn�Bo�Bt�Bz�Bz�B}�B}�B}�B|�B{�B{�B{�B{�B{�B{�B{�Bz�By�Bz�Bw�Bw�Bu�Bw�Bw�Bv�Bq�Bn�Bm�BhsBffBcTBVBJ�B=qB!�B�B��B�B�NB�B��BB�FB��B��B�\B~�Bq�BiyB\)BZBS�B<jB5?B'�B�B	7B
��B
�fB
�
B
ɺB
��B
�\B
�B
p�B
e`B
_;B
[#B
Q�B
K�B
F�B
C�B
 �B
�B
	7B	��B	�B	�BB	�#B	�B	�B	��B	�XB	�-B	��B	��B	��B	��B	�\B	�DB	�B	w�B	o�B	jB	ZB	P�B	/B	�B	JB	+B	B�B��B��B�B��BÖB�B�FB�!B��B��B�1B~�B{�By�Bz�Bv�Bt�Bs�Bt�Bq�Br�Bp�Bo�Bo�Bn�Bm�Bn�BjBgmBgmBe`BaHB_;B]/B]/BZB[#BZBYBXBYBT�BXBW
BS�BQ�BO�BN�BL�BL�BL�BK�BJ�BI�BI�BH�BH�BE�BB�BA�B@�B?}B=qB<jB<jB:^B9XB7LB5?B49B49B5?B5?B5?B7LB5?B33B0!B+B+B,B+B)�B-B.B2-B0!B1'B1'B2-B33B0!B2-B6FB7LB7LB8RB8RB8RB8RB8RB:^B<jB<jB=qB>wB>wB=qBB�BB�BC�BH�BE�BE�BE�BE�BF�BH�BI�BJ�BL�BN�BP�BP�BR�BXBXBXBYBYBYBYBZB[#B]/B^5B^5B^5Be`BiyBjBk�Bo�Bo�Bp�Br�Bt�Bv�Bx�Bz�B}�B� B� B� B�B�B�B�%B�%B�%B�+B�+B�1B�7B�=B�JB�VB�uB�{B��B��B��B��B��B�B�9B�dB��BB��B��B��B��BBÖBÖBÖBĜBŢBŢBȴBɺB��B��B��B�
B�B�/B�/B�/B�5B�HB�NB�ZB�fB�`B�TB�`B�mB�B�B�B�B��B	B	B		7B	�B	uB	�B	$�B	(�B	+B	(�B	%�B	 �B	�B	�B	 �B	%�B	-B	6FB	;dB	A�B	C�B	C�B	C�B	B�B	B�B	D�B	I�B	G�B	N�B	R�B	T�B	T�B	W
B	W
B	VB	P�B	K�B	O�B	S�B	W
B	[#B	\)B	\)B	\)B	[#B	[#B	aHB	cTB	e`B	e`B	hsB	l�B	n�B	q�B	s�B	t�B	v�B	x�B	y�B	z�B	{�B	~�B	� B	�B	� B	}�B	~�B	� B	�B	�B	�%B	�7B	�=B	�JB	�PB	�\B	�hB	�hB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	�B	�B	�B	�B	�B	�B	�-B	�3B	�3B	�9B	�LB	�^B	�jB	�wB	��B	ŢB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�,B	�>B

=B
�B
?B
�B
&�B
*�B
4�B
<PB
?�B
D�B
MB
T�B
YKB
^�B
b�B
hXB
n�B
r-B
vF111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B'�B'�B'�B(�B'�B(�B(�B(�B(�B(�B(�B(�B(�B(�B(�B(�B(�B(�B(�B(�B(�B(�B(�B(�B)�B*�B*�B*�B*�B)�B)�B)�B*�B)�B)�B<BE9BF@BGFBMkBQ�BP~BT�BW�BY�B[�B[�B[�B`�Be�BgBl"BrGBrGBuZBuZBuZBtUBsNBsNBsNBsNBsNBsNBsNBrHBqCBrIBo7Bo7Bm+Bo7Bo7Bn1BiBfBd�B_�B]�BZ�BMoBB-B4�B:B�B�fB�B��BЌB�ZB�B��B�bB�B��BvvBi'B`�BS�BQ�BKwB3�B,�BrB)B �B
�gB
��B
ΓB
�DB
�^B
��B
y�B
h5B
\�B
V�B
R�B
IB
C[B
><B
;*B
\B
1B
 �B	�B	� B	��B	ҿB	гB	ϬB	ɉB	��B	��B	��B	�fB	�_B	�;B	��B	��B	{�B	osB	gCB	b$B	Q�B	H�B	&�B	QB	�B��B��B�eB��B�}B�MB̯B�IB��B��B��B��B��B�Bv�Bs�Bq�Br�Bn�BlvBkpBlvBidBjjBh^BgYBgYBfSBeLBfSBb:B_(B_)B]BYBV�BT�BT�BQ�BR�BQ�BP�BO�BP�BL�BO�BN�BK�BI�BG�BF�BD�BD�BD�BC�BB�BAyBAzB@tB@tB=bB:OB9IB8DB7>B52B4+B4+B2 B1B/B-B+�B+�B-B-B-B/B-B*�B'�B"�B"�B#�B"�B!�B$�B%�B)�B'�B(�B(�B)�B*�B'�B)�B.B/B/B0B0B0B0B0B2#B4/B4/B56B6<B6<B56B:TB:TB;[B@yB=gB=gB=gB=gB>nB@yBA�BB�BD�BF�BH�BH�BJ�BO�BO�BO�BP�BP�BP�BP�BQ�BR�BT�BU�BU�BU�B]%Ba>BbDBcIBgbBgbBhhBjtBl�Bn�Bp�Br�Bu�Bw�Bw�Bw�Bz�B{�B|�B}�B}�B}�B~�B~�B�B��B� B�B�B�8B�>B�JB�cB��B��B��B��B��B�%B�IB�OB�JB�JB�JB�DB�PB�WB�WB�WB�]B�cB�cB�tB�zBčBȥB˸B��B��B��B��B��B��B�B�B�B�%B�B�B�B�,B�DB�PB�PB�\B��B��B��B	 �B	=B	1B	IB	�B	 �B	"�B	 �B	�B	�B	yB	nB	�B	�B	$�B	. B	3B	9BB	;OB	;OB	;OB	:HB	:HB	<UB	AsB	?gB	F�B	J�B	L�B	L�B	N�B	N�B	M�B	H�B	C�B	G�B	K�B	N�B	R�B	S�B	S�B	S�B	R�B	R�B	Y B	[B	]B	]B	`*B	dBB	fOB	iaB	kmB	lsB	n�B	p�B	q�B	r�B	s�B	v�B	w�B	x�B	w�B	u�B	v�B	w�B	x�B	{�B	}�B	��B	��B	� B	�B	�B	�B	�B	�0B	�<B	�HB	�OB	�OB	�[B	�`B	�rB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�*B	�6B	�TB	�`B	�lB	�B	ŅB	ǑB	ȗB	ȗB	ɞB	ʤB	̰B	˪B	˪G�O�B	��B	��B
�B
zB
�B
;B
cB
"GB
,jB
3�B
7EB
<IB
D�B
L�B
P�B
V0B
ZcB
`B
fDB
i�B
m�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.1 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            r =0.9998(+/-0.0001), vertically averaged dS =-0.008(+/-0.002) in PSS-78.                                                                                                                                                                                       Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                      202202041144142022020411441420220204114414  AO  ARCAADJP                                                                    20200619170900    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20200619170900  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20200619170900  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20220204114414  IP                  G�O�G�O�G�O�                