CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2016-10-30T00:35:26Z creation;2016-10-30T00:35:29Z conversion to V3.1;2019-12-19T08:22:59Z update;     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7T   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     88   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8X   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8\   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8d   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8h   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8p   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8x   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ID   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M0   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  pl   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  tX   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �(   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �d   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ޜ   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �    HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �$   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �(   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �,   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �l   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �|   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20161030003526  20200116211517  5905048                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               5A   JA  I2_0577_053                     2C  D   NAVIS_A                         0577                            ARGO 102115                     863 @��3Gr 1   @��4�[ @3f��,<��d�1&�y1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�ff@���A   A   A@  A^ffA~ffA�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  BpffBx  B�  B�  B�  B���B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C=�fC@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cy�fC{�fC~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  Dy�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DS��DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^y�D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ D҃3D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׃3D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�C3D� D�� D���D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @���@�33@�ffA33A?33A]��A}��A���A���A���A���Aϙ�Aߙ�AA���B��B��B��B��B'��B/��B7��B?��BG��BO��BW��B_��Bg��Bp33Bw��B��B��fB��fB��3B��3B��3B��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fC�3C�3C�3C�3C	�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C!�3C#�3C%�3C'�3C)�3C+�3C-�3C/�3C1�3C3�3C5�3C7�3C9�3C;�3C=ٚC?�3CA�3CC�3CE�3CG�3CI�3CK�3CM�3CO�3CQ�3CS�3CU�3CW�3CY�3C[�3C]�3C_�3Ca�3Cc�3Ce�3Cg�3Ci�3Ck�3Cm�3Co�3Cq�3Cs�3Cu�3Cw�3CyٚC{ٚC}�3C�3C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D |�D ��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D	|�D	��D
|�D
��D|�D��D|�D��DvfD��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D |�D ��D!|�D!��D"|�D"��D#|�D#��D$|�D$��D%|�D%��D&|�D&��D'|�D'��D(|�D(��D)|�D)��D*|�D*��D+|�D+��D,|�D,��D-|�D-��D.|�D.��D/|�D/��D0|�D0��D1|�D1��D2|�D2��D3|�D3��D4|�D4��D5|�D5��D6|�D6��D7|�D7��D8|�D8��D9|�D9��D:|�D:��D;|�D;��D<|�D<��D=|�D=��D>|�D>��D?|�D?��D@|�D@��DA|�DA��DB|�DB��DC|�DC��DD|�DD��DE|�DE��DF|�DF��DG|�DG��DH|�DH��DI|�DI��DJ|�DJ��DK|�DK��DL|�DL��DM|�DM��DN|�DN��DO|�DO��DP|�DP��DQ|�DQ��DR|�DR��DS|�DS�fDT|�DT��DU|�DU��DV|�DV��DW|�DW��DX|�DX��DY|�DY��DZ|�DZ��D[|�D[��D\|�D\��D]|�D]��D^vfD^��D_|�D_��D`|�D`��Da|�Da��Db|�Db��Dc|�Dc��Dd|�Dd��De|�De��Df|�Df��Dg|�Dg��Dh|�Dh��Di|�Di��Dj|�Dj��Dk|�Dk��Dl|�Dl��Dm|�Dm��Dn|�Dn��Do|�Do��Dp|�Dp��Dq|�Dq��Dr|�Dr��Ds|�Ds��Dt|�Dt��Du|�Du��Dv|�Dv��Dw|�Dw��Dx|�Dx��Dy|�Dy��Dz|�Dz��D{|�D{��D||�D|��D}|�D}��D~|�D~��D|�D��D�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD¾fD��fD�>fD�~fDþfD��fD�>fD�~fDľfD��fD�>fD�~fDžfD��fD�>fD�~fDƾfD��fD�>fD�~fDǾfD��fD�>fD�~fDȾfD��fD�>fD�~fDɾfD��fD�>fD�~fDʾfD��fD�>fD�~fD˾fD��fD�>fD�~fD̾fD��fD�>fD�~fD;fD��fD�>fD�~fDξfD��fD�>fD�~fDϾfD��fD�>fD�~fDоfD��fD�>fD�~fDѾfD��fD�>fDҁ�DҾfD��fD�>fD�~fDӾfD��fD�>fD�~fDԾfD��fD�>fD�~fDվfD��fD�>fD�~fD־fD��fD�>fDׁ�D׾fD��fD�>fD�~fDؾfD��fD�>fD�~fDپfD��fD�>fD�~fDھfD��fD�>fD�~fD۾fD��fD�>fD�~fDܾfD��fD�>fD�~fDݾfD��fD�>fD�~fD޾fD��fD�>fD�~fD߾fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD��fD��fD�>fD�~fD�fD��fD�A�D�~fD�fD��3D�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD���D��3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�|�A�~�A�z�A�~�A׃AׅA׃AׅAׇ+Aׇ+A׉7A׉7A׋DA׍PA׍PA׍PA׋DAׁA�v�A�n�A�O�A�=qA�-A�(�A�(�A�-A�+A�$�A��A�oA�
=A���A�?}A�;dA��A���AȶFAƥ�A�z�A��;A�ƨA��mA���A�I�A�`BA�n�A���A�/A���A�C�A�$�A�ƨA��^A�Q�A�?}A�XA�A�A��;A��A���A�K�A��yA��
A�;dA���A���A�\)A�-A��A��A�ƨA�jA�-A���A�jA��7A�z�A�%A��wA��jA�1'A���A�p�A�XA�=qA���A�VA���A�  A�hsA�t�A��+A��/A�O�A�5?A���A���A�dZA��!A���A��jA�M�A�I�A�ZA���A�p�A��A��^A�"�A�+A~��A}p�Ay��Aw�#Av-Au/Ar�yAo��Al��Ak33Aj�Ait�Agx�Ae��Ab�A`A^�DA]?}A[AX��AV��AUG�AT��AT��AT{AQ�wAO�PAK��AJ��AJjAI�AI�PAH��AFAB9XA@JA>�HA=ƨA<$�A;p�A;`BA;"�A9�A7x�A5��A4v�A3��A3oA29XA0��A/
=A-t�A, �A*��A)�A'�A&�A${A"(�A ��A7LA  A��A�jA9XAC�A�A�+A�wA"�A��A&�AjA�AXA�yAA��AI�A�;A�An�A1'Ap�AZA�TA�^AC�A
r�A	K�A�/A|�A{A�AVAdZA��AffA$�AO�A�@���@���@�E�@�%@��@���@���@�@�@�@�-@�  @�?}@�@�X@�  @�@���@�(�@�;d@��@�v�@Ԭ@�I�@ԛ�@�  @���@�p�@�V@�bN@�9X@��@��m@ϝ�@��y@�V@�5?@��T@�&�@ə�@ȴ9@ȴ9@ȣ�@�bN@��@��@�ff@ũ�@��@�%@���@�?}@�G�@�O�@���@�ƨ@�$�@��;@�{@��@��m@��@�K�@�"�@�~�@��-@��@��/@�r�@��;@���@��@�$�@��7@��/@�I�@�\)@���@��^@���@�@��^@�`B@�V@�I�@��m@��w@�|�@�C�@���@���@��9@��j@��@���@�bN@�Z@�z�@�9X@��@�=q@�^5@�J@��-@��@���@�(�@��w@���@�V@�Ĝ@�b@�t�@��@�@��@���@�M�@���@�7L@�Z@��@��R@�J@�hs@��@�%@��@���@�r�@�"�@���@�^5@��T@�hs@��h@�  @��@��y@�
=@�ƨ@�C�@�S�@���@��9@�x�@��H@�+@�;d@�
=@�n�@�I�@��P@�C�@�o@�@��H@��\@�E�@�-@��R@�K�@��
@��@�ƨ@�33@�v�@��7@��@�r�@��F@�\)@�~�@���@���@��T@�ff@�~�@���@���@��!@��\@�^5@�E�@�-@�{@��h@�O�@�G�@�?}@�/@�&�@��@���@�1'@��w@��@�dZ@�
=@��!@�^5@��^@�`B@�G�@��@��/@���@�
=@���@��H@���@���@���@���@��+@�=q@�J@���@��T@�@�`B@�V@���@�Ĝ@��j@��@���@�Z@��m@��@�\)@�
=@�n�@��@�{@��@��@��#@���@�X@�I�@��m@��;@��w@��P@�t�@��@���@��R@���@�v�@�E�@�$�@�{@��@��#@���@��7@�`B@�`B@�O�@�O�@�G�@�&�@��u@�bN@�Z@�1'@���@��@��;@���@��F@�|�@�K�@�+@�"�@��@�~�@�5?@�{@�@��#@��-@��h@�7L@��@��/@�Z@��@�K�@�;d@�33@��@���@���@��!@���@�V@��T@�@���@�p�@�O�@�/@��D@�I�@�A�@�9X@�(�@�  @��m@���@��R@�n�@�-@��@��@��9@�z�@�I�@�@�@l�@�@~ȴ@~V@~@}�T@}p�@|�/@{��@{��@{S�@{"�@{@z�H@z�!@zn�@z=q@zJ@y�^@yhs@y&�@y%@x�`@xr�@xA�@x  @w+@u�@u�-@u�@up�@t�@s�F@s@r�\@r=q@r-@q��@q��@q%@p�u@pr�@pQ�@o�P@o
=@n�@n��@n$�@m`B@l�j@lj@l(�@k��@k�@j�H@jn�@j=q@i��@i�^@i&�@h�9@h �@g��@g��@gl�@g+@g
=@g
=@g
=@f{@e@eO�@d��@d��@c�m@c33@co@b�@b�H@b�H@b�!@bM�@bJ@a�^@a��@ahs@aG�@a�@`�`@`A�@_��@_\)@^�+@^V@^5?@]@]p�@]V@\�/@\z�@\I�@\(�@[��@[��@[o@Z��@Z��@Z�\@ZM�@ZJ@Y��@YX@Y�@X��@X�9@X��@X�@XQ�@X  @W��@W�@WK�@W�@V��@V�y@VE�@U�@T�@Tz�@TZ@TI�@T9X@T(�@T(�@T(�@T�@T�@S��@S��@SC�@R�!@R~�@RM�@Q��@Qx�@QX@P��@P�u@P �@P  @O��@O;d@Nȴ@N��@M�T@L��@K��@J�H@J�@IX@H�u@H1'@G��@G�@F��@Fff@FE�@F$�@F$�@F{@E��@E�@EO�@E�@D��@D9X@CdZ@C33@C"�@C@B��@B~�@B^5@B=q@BJ@@1'@?l�@?l�@?;d@>�@>�R@>��@>v�@>ff@>E�@>{@=@=O�@<�@<9X@;��@;33@:�\@:=q@:�@9�@9�#@9��@9�^@9��@9��@9�7@9hs@97L@8r�@7�;@7�P@7l�@7K�@7
=@65?@5@5?}@4(�@3t�@3C�@333@333@3o@2�@2�\@2=q@1��@1hs@1�@0�9@0�@0b@/�@/�P@/l�@/\)@.�y@.5?@.{@-?}@,�D@,(�@+��@+�F@+o@+@*��@*��@*�\@*n�@*^5@*J@)�^@)hs@)&�@(Ĝ@(�u@(�@(A�@(  @'�@'��@'�P@'\)@'+@&�y@&�+@&5?@&{@%@%�-@%p�@%V@$�/@$��@$Z@#�m@#�F@#�@#33@#o@"�H@"��@"�\@"~�@"M�@!��@!��@!hs@!7L@!%@ �9@ �@ r�@ r�@ r�@ r�@ Q�@�@�w@\)@��@�@ȴ@v�@V@�-@��@��@`B@O�@�@��@Z@(�@1@�
@S�@C�@C�@33@33@o@��@�!@~�@=q@-@J@�#@�^@X@��@�9@�u@�@bN@1'@�@�@�P@+@��@�R@�+@$�@@@�-@��@��@��@��@��@�h@�h@�h@�@p�@`B@?}@?}@/@/@/@/@/@V@�j@j@1@�
@��@��@C�@"�@�@�!@~�@n�@^5@M�@-@-@-@��@��@x�@��@�`@��@�9@Q�@ �@b@b@  @  @�@��@��@�w@�w@�w@�@��@�P@l�@K�@;d@;d@;d@
=@�y@�y@��@�+@ff@E�@{@@�-@�@p�@p�@p�@`B@?}@/@/@�@�@�/@��@�j@�@Z@(�@1@��@�m@��@��@dZ@S�@33@
�H@
��@
~�@
^5@
=q@
=q@
-@
J@	�@	�@	�^@	��@	�7@	hs@	7L@	7L111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�|�A�~�A�z�A�~�A׃AׅA׃AׅAׇ+Aׇ+A׉7A׉7A׋DA׍PA׍PA׍PA׋DAׁA�v�A�n�A�O�A�=qA�-A�(�A�(�A�-A�+A�$�A��A�oA�
=A���A�?}A�;dA��A���AȶFAƥ�A�z�A��;A�ƨA��mA���A�I�A�`BA�n�A���A�/A���A�C�A�$�A�ƨA��^A�Q�A�?}A�XA�A�A��;A��A���A�K�A��yA��
A�;dA���A���A�\)A�-A��A��A�ƨA�jA�-A���A�jA��7A�z�A�%A��wA��jA�1'A���A�p�A�XA�=qA���A�VA���A�  A�hsA�t�A��+A��/A�O�A�5?A���A���A�dZA��!A���A��jA�M�A�I�A�ZA���A�p�A��A��^A�"�A�+A~��A}p�Ay��Aw�#Av-Au/Ar�yAo��Al��Ak33Aj�Ait�Agx�Ae��Ab�A`A^�DA]?}A[AX��AV��AUG�AT��AT��AT{AQ�wAO�PAK��AJ��AJjAI�AI�PAH��AFAB9XA@JA>�HA=ƨA<$�A;p�A;`BA;"�A9�A7x�A5��A4v�A3��A3oA29XA0��A/
=A-t�A, �A*��A)�A'�A&�A${A"(�A ��A7LA  A��A�jA9XAC�A�A�+A�wA"�A��A&�AjA�AXA�yAA��AI�A�;A�An�A1'Ap�AZA�TA�^AC�A
r�A	K�A�/A|�A{A�AVAdZA��AffA$�AO�A�@���@���@�E�@�%@��@���@���@�@�@�@�-@�  @�?}@�@�X@�  @�@���@�(�@�;d@��@�v�@Ԭ@�I�@ԛ�@�  @���@�p�@�V@�bN@�9X@��@��m@ϝ�@��y@�V@�5?@��T@�&�@ə�@ȴ9@ȴ9@ȣ�@�bN@��@��@�ff@ũ�@��@�%@���@�?}@�G�@�O�@���@�ƨ@�$�@��;@�{@��@��m@��@�K�@�"�@�~�@��-@��@��/@�r�@��;@���@��@�$�@��7@��/@�I�@�\)@���@��^@���@�@��^@�`B@�V@�I�@��m@��w@�|�@�C�@���@���@��9@��j@��@���@�bN@�Z@�z�@�9X@��@�=q@�^5@�J@��-@��@���@�(�@��w@���@�V@�Ĝ@�b@�t�@��@�@��@���@�M�@���@�7L@�Z@��@��R@�J@�hs@��@�%@��@���@�r�@�"�@���@�^5@��T@�hs@��h@�  @��@��y@�
=@�ƨ@�C�@�S�@���@��9@�x�@��H@�+@�;d@�
=@�n�@�I�@��P@�C�@�o@�@��H@��\@�E�@�-@��R@�K�@��
@��@�ƨ@�33@�v�@��7@��@�r�@��F@�\)@�~�@���@���@��T@�ff@�~�@���@���@��!@��\@�^5@�E�@�-@�{@��h@�O�@�G�@�?}@�/@�&�@��@���@�1'@��w@��@�dZ@�
=@��!@�^5@��^@�`B@�G�@��@��/@���@�
=@���@��H@���@���@���@���@��+@�=q@�J@���@��T@�@�`B@�V@���@�Ĝ@��j@��@���@�Z@��m@��@�\)@�
=@�n�@��@�{@��@��@��#@���@�X@�I�@��m@��;@��w@��P@�t�@��@���@��R@���@�v�@�E�@�$�@�{@��@��#@���@��7@�`B@�`B@�O�@�O�@�G�@�&�@��u@�bN@�Z@�1'@���@��@��;@���@��F@�|�@�K�@�+@�"�@��@�~�@�5?@�{@�@��#@��-@��h@�7L@��@��/@�Z@��@�K�@�;d@�33@��@���@���@��!@���@�V@��T@�@���@�p�@�O�@�/@��D@�I�@�A�@�9X@�(�@�  @��m@���@��R@�n�@�-@��@��@��9@�z�@�I�@�@�@l�@�@~ȴ@~V@~@}�T@}p�@|�/@{��@{��@{S�@{"�@{@z�H@z�!@zn�@z=q@zJ@y�^@yhs@y&�@y%@x�`@xr�@xA�@x  @w+@u�@u�-@u�@up�@t�@s�F@s@r�\@r=q@r-@q��@q��@q%@p�u@pr�@pQ�@o�P@o
=@n�@n��@n$�@m`B@l�j@lj@l(�@k��@k�@j�H@jn�@j=q@i��@i�^@i&�@h�9@h �@g��@g��@gl�@g+@g
=@g
=@g
=@f{@e@eO�@d��@d��@c�m@c33@co@b�@b�H@b�H@b�!@bM�@bJ@a�^@a��@ahs@aG�@a�@`�`@`A�@_��@_\)@^�+@^V@^5?@]@]p�@]V@\�/@\z�@\I�@\(�@[��@[��@[o@Z��@Z��@Z�\@ZM�@ZJ@Y��@YX@Y�@X��@X�9@X��@X�@XQ�@X  @W��@W�@WK�@W�@V��@V�y@VE�@U�@T�@Tz�@TZ@TI�@T9X@T(�@T(�@T(�@T�@T�@S��@S��@SC�@R�!@R~�@RM�@Q��@Qx�@QX@P��@P�u@P �@P  @O��@O;d@Nȴ@N��@M�T@L��@K��@J�H@J�@IX@H�u@H1'@G��@G�@F��@Fff@FE�@F$�@F$�@F{@E��@E�@EO�@E�@D��@D9X@CdZ@C33@C"�@C@B��@B~�@B^5@B=q@BJ@@1'@?l�@?l�@?;d@>�@>�R@>��@>v�@>ff@>E�@>{@=@=O�@<�@<9X@;��@;33@:�\@:=q@:�@9�@9�#@9��@9�^@9��@9��@9�7@9hs@97L@8r�@7�;@7�P@7l�@7K�@7
=@65?@5@5?}@4(�@3t�@3C�@333@333@3o@2�@2�\@2=q@1��@1hs@1�@0�9@0�@0b@/�@/�P@/l�@/\)@.�y@.5?@.{@-?}@,�D@,(�@+��@+�F@+o@+@*��@*��@*�\@*n�@*^5@*J@)�^@)hs@)&�@(Ĝ@(�u@(�@(A�@(  @'�@'��@'�P@'\)@'+@&�y@&�+@&5?@&{@%@%�-@%p�@%V@$�/@$��@$Z@#�m@#�F@#�@#33@#o@"�H@"��@"�\@"~�@"M�@!��@!��@!hs@!7L@!%@ �9@ �@ r�@ r�@ r�@ r�@ Q�@�@�w@\)@��@�@ȴ@v�@V@�-@��@��@`B@O�@�@��@Z@(�@1@�
@S�@C�@C�@33@33@o@��@�!@~�@=q@-@J@�#@�^@X@��@�9@�u@�@bN@1'@�@�@�P@+@��@�R@�+@$�@@@�-@��@��@��@��@��@�h@�h@�h@�@p�@`B@?}@?}@/@/@/@/@/@V@�j@j@1@�
@��@��@C�@"�@�@�!@~�@n�@^5@M�@-@-@-@��@��@x�@��@�`@��@�9@Q�@ �@b@b@  @  @�@��@��@�w@�w@�w@�@��@�P@l�@K�@;d@;d@;d@
=@�y@�y@��@�+@ff@E�@{@@�-@�@p�@p�@p�@`B@?}@/@/@�@�@�/@��@�j@�@Z@(�@1@��@�m@��@��@dZ@S�@33@
�H@
��@
~�@
^5@
=q@
=q@
-@
J@	�@	�@	�^@	��@	�7@	hs@	7L@	7L111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B?}B?}B?}B@�BB�BB�BB�BC�BD�BD�BC�BD�BD�BE�BD�BJ�BVBdZBk�Bn�Bx�B{�B{�B{�B|�B}�B~�B�B� B�B� B~�B|�By�B�+B�uB�\B�7B��B��B�-B�dB�qB�wBÖBɺBɺB��B��B��B��B�B��B��BB�?B�B��B��B�JB�%B�B�B�B�Bz�Be`BN�BJ�BI�B?}B<jB=qB>wB:^B6FB,B(�B%�B�B�BuBVBB�B�#BǮB��B�9B��B��B�%Bz�BdZB^5BYBG�B@�B7LB%�B�B\B+B
�B
��B
�^B
�B
��B
��B
�B
u�B
iyB
N�B
=qB
0!B
&�B
�B
B	�yB	�)B	�B	��B	B	�9B	��B	�\B	�B	z�B	r�B	cTB	W
B	M�B	H�B	F�B	B�B	8RB	(�B	�B	hB	\B	PB	
=B	B��B�B�TB�5B�)B��B��B��B��B��BƨB�}B�jB�^B�RB�FB�3B�B�B��B��B��B��B��B�oB�DB�=B�1B�B�B�%B�B�B�B�B}�B{�By�Bt�Br�Bn�Bl�Bk�BjBiyBgmBgmBgmBe`BdZBe`BbNBbNBbNBaHB_;B]/B]/BVBM�BK�BH�BF�BB�BN�BW
BW
BS�BT�BS�BR�BS�BZB[#B_;B]/B_;B\)B\)BZBaHB_;B_;B^5B_;Be`BjBgmB`BB\)B_;BhsBjBs�Bn�Bn�Br�Bu�Bu�Bu�Bv�Bv�Bw�Bx�Bx�By�By�B{�B{�B|�B}�B}�B~�B�B�B�+B�=B�JB�PB�\B�hB��B��B��B��B��B��B��B��B��B�B�B�!B�?B�RB�XB�dB�qB�}BÖBƨBǮBǮB��B��B�B�B�)B�5B�TB�ZB�fB�fB�mB�sB�B��B	B	�B	$�B	%�B	%�B	%�B	)�B	/B	:^B	<jB	B�B	J�B	M�B	P�B	Q�B	S�B	T�B	XB	ZB	_;B	_;B	`BB	_;B	_;B	`BB	aHB	cTB	jB	l�B	o�B	r�B	x�B	z�B	|�B	� B	�B	�B	�B	�B	�B	�+B	�B	�B	�%B	�1B	�\B	�hB	��B	��B	��B	��B	�3B	�'B	�9B	�B	�B	�-B	�jB	�}B	B	ĜB	ŢB	B	�}B	�wB	�wB	�wB	�wB	�wB	�}B	��B	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	ɺB	��B	��B	��B	��B	��B	��B	�
B	�/B	�NB	�ZB	�ZB	�ZB	�ZB	�ZB	�ZB	�`B	�mB	�mB	�mB	�mB	�mB	�fB	�mB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
%B
+B
+B
1B
1B
1B
1B
	7B
	7B
	7B
	7B

=B

=B

=B

=B

=B

=B
DB
DB

=B
DB
DB
DB
DB
JB
JB
JB
JB
JB
JB
PB
PB
PB
PB
VB
\B
\B
\B
\B
bB
bB
bB
hB
hB
hB
oB
uB
uB
uB
uB
uB
uB
{B
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
 �B
!�B
!�B
!�B
!�B
!�B
!�B
!�B
"�B
"�B
"�B
"�B
"�B
"�B
"�B
#�B
#�B
"�B
$�B
$�B
$�B
$�B
$�B
$�B
$�B
%�B
%�B
%�B
%�B
%�B
&�B
&�B
&�B
&�B
&�B
'�B
(�B
(�B
(�B
(�B
)�B
+B
+B
+B
+B
,B
,B
,B
,B
,B
,B
,B
,B
,B
,B
-B
-B
-B
-B
.B
.B
/B
0!B
0!B
0!B
0!B
0!B
0!B
0!B
0!B
0!B
0!B
0!B
1'B
1'B
1'B
1'B
1'B
1'B
1'B
1'B
2-B
2-B
2-B
49B
33B
33B
49B
49B
49B
49B
49B
49B
49B
49B
5?B
5?B
5?B
5?B
5?B
5?B
5?B
6FB
6FB
6FB
7LB
7LB
7LB
7LB
7LB
7LB
7LB
7LB
8RB
8RB
8RB
8RB
9XB
9XB
:^B
:^B
:^B
:^B
:^B
:^B
:^B
:^B
:^B
:^B
:^B
:^B
;dB
;dB
;dB
;dB
;dB
;dB
<jB
<jB
<jB
=qB
<jB
=qB
=qB
>wB
=qB
>wB
?}B
@�B
A�B
A�B
B�B
B�B
B�B
C�B
D�B
D�B
D�B
D�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
F�B
F�B
G�B
G�B
G�B
G�B
H�B
H�B
H�B
H�B
H�B
J�B
J�B
J�B
J�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
L�B
L�B
M�B
M�B
M�B
N�B
N�B
N�B
N�B
N�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
P�B
P�B
P�B
P�B
P�B
Q�B
R�B
R�B
S�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
VB
VB
VB
W
B
W
B
W
B
XB
XB
XB
XB
XB
YB
YB
YB
ZB
ZB
[#B
[#B
[#B
\)B
\)B
\)B
\)B
]/B
]/B
]/B
]/B
^5B
^5B
^5B
_;B
_;B
_;B
_;B
_;B
_;B
`BB
`BB
`BB
`BB
`BB
aHB
aHB
aHB
aHB
aHB
aHB
bNB
bNB
bNB
bNB
cTB
cTB
cTB
cTB
dZB
dZB
dZB
dZB
dZB
dZB
dZB
e`B
e`B
ffB
ffB
ffB
ffB
ffB
ffB
ffB
ffB
ffB
gmB
gmB
gmB
hsB
hsB
hsB
hsB
hsB
iyB
hsB
hsB
iyB
iyB
iyB
iyB
jB
jB
jB
jB
jB
jB
jB
jB
jB
jB
k�B
k�B
k�B
k�B
k�B
k�B
l�B
l�B
l�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
n�B
n�B
n�B
o�B
o�B
o�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
r�B
r�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
t�B
t�B
t�B
u�B
u�B
u�B
u�B
u�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
w�B
w�B
w�B
w�B
w�B
w�B
x�B
x�B
x�B
x�B
x�B
x�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
z�B
z�B
z�B
z�B
z�B
{�B
{�B
{�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
}�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B?}B?}B?}B@�BB�BB�BB�BC�BD�BD�BC�BD�BD�BE�BD�BJ�BVBdZBk�Bn�Bx�B|B{�B{�B|�B}�BB� B�B�UB��B�UB�B��B�B�WB��B�4B�pB�*B��B�6B��B� BǔB̳B�B��B�B�HB��B�!BԕB�BB�YB��B��B��B��B��B�zB�EB��B��B��B}�Bg�BO�BK�BK�B@iB="B>wB?�B<jB8RB-CB*B(
B �B�B�B�B�B�OB��B��B�GB��B�B��B�KB}�BeB_�B[=BH�BBuB9�B(
B�BuBVB
�WB
�uB
�"B
��B
�$B
�|B
�B
xlB
mCB
Q4B
?}B
1�B
)�B
QB
3B	�B	�/B	�B	�uB	�SB	��B	��B	��B	�B	}VB	u�B	ezB	X�B	N�B	IRB	HB	E�B	;B	,�B	�B	B	B	VB	0B	�B	 B�'B��B��B�B��B�NB��B�4B�<BȚB�UB��B��B��B��B�ZB�!B��B��B�,B��B�B��B��B�PB�B��B�B�tB�+B��B�3B��B�'B~�B}B{�Bu�Bs�BoBm�Bl�BlBjKBh>Bh�Bh$Be�Be�Bf�Bc Bb�Bc:Bb�B`�B^5B_BW�BN�BL�BJ�BGzBC{BOvBX+BW�BV9BWsBT�BTBUB[�B\�B`�B_Ba�B]~B]�B[�Bb�B`vB`BB_B`BBf�BkkBiBa�B]IB_�Bh�Bk6Bt�BoBo BsBvBu�BvBwBwLBx8By>ByXBz�B{�B|jB|B}"B~BB~wB�B��B��B��B�XB�dB�PB��B��B�9B��B�@B�`B�B��B��B�XB�0B�QB��B��B��B��B��B��B��B� B�3B�+B�1B�KB�xB�oB֡B�7B�)B�jB�B��B��B�B�B�B�kB��B�}B	/B	$�B	%�B	%�B	&2B	*B	/B	:^B	<B	BB	J�B	N"B	QNB	RTB	TaB	U�B	XyB	Z�B	_�B	`\B	`�B	_�B	_�B	`\B	a|B	c�B	j�B	mB	pB	sMB	yXB	{�B	}�B	�iB	�UB	�;B	�UB	�;B	��B	��B	��B	�mB	�tB	��B	�B	�}B	��B	�yB	��B	��B	��B	��B	�ZB	�/B	��B	�vB	�jB	��B	��B	�SB	��B	�B	��B	��B	��B	��B	��B	��B	�}B	�;B	�EB	ʌB	��B	�[B	�uB	҉B	ЗB	�6B	�DB	�XB	�DB	�\B	�<B	��B	͹B	��B	�
B	�/B	�NB	�tB	�tB	�tB	�tB	�tB	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	�B	��B	��B	��B	��B	�B	��B	�B	��B	��B	�vB	�B	�B	��B	��B	�B	��B	��B	��B	��B	�B	��B	��B	�B	�6B	�(B	�B	�.B	�B	�B	�B	�cB	�HB
 iB
 4B
UB
uB
GB
-B
GB
3B
3B
MB
�B
�B
YB
YB
?B
YB
_B
zB
KB
KB
KB
KB
	lB
	RB
	RB
	lB

XB

XB

rB

rB

XB

XB
^B
^B

rB
�B
xB
^B
xB
dB
~B
dB
dB
dB
~B
jB
jB
jB
�B
�B
�B
vB
vB
vB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
QB
�B
�B
�B
#B
B
�B
�B
�B
�B
�B
�B
�B
B
�B
�B
 B
 B
!-B
 �B
!�B
!�B
!�B
!�B
!�B
!�B
!�B
"�B
"�B
"�B
#B
"�B
#B
#B
#�B
$B
# B
%,B
$�B
$�B
$�B
%FB
%FB
%,B
&2B
%�B
&B
%�B
&B
'B
'B
'B
'B
'8B
($B
)B
)B
)*B
)DB
*0B
+6B
+B
+B
+6B
,=B
,=B
,"B
,"B
,"B
,=B
,=B
,=B
,"B
,"B
-CB
-CB
-)B
-)B
.IB
.cB
/OB
0UB
0UB
0UB
0�B
0UB
0UB
0;B
0!B
0!B
0;B
0UB
1AB
1AB
1[B
1[B
1[B
1AB
1AB
1[B
2aB
2|B
2|B
4TB
3hB
3hB
4nB
4TB
4TB
4TB
4TB
4TB
4nB
4�B
5�B
5tB
5ZB
5ZB
5ZB
5tB
5�B
6zB
6`B
6`B
7fB
7�B
7fB
7fB
7fB
7fB
7fB
7fB
8lB
8lB
8lB
8�B
9�B
9�B
:xB
:xB
:^B
:xB
:^B
:xB
:^B
:xB
:^B
:�B
:�B
:�B
;�B
;B
;�B
;�B
;B
;B
<�B
<�B
<�B
=�B
<�B
=�B
=�B
>�B
=�B
>�B
?�B
@�B
A�B
A�B
B�B
B�B
B�B
C�B
D�B
D�B
D�B
D�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
F�B
GB
G�B
G�B
G�B
G�B
H�B
H�B
H�B
IB
IlB
J�B
J�B
J�B
J�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
LB
MB
MB
N"B
NB
NB
N�B
N�B
N�B
N�B
N�B
O�B
O�B
O�B
O�B
PB
O�B
P.B
PB
Q B
QB
Q B
QB
Q4B
R B
S&B
S[B
T,B
UB
T�B
T�B
UB
UB
UMB
U2B
V9B
VB
VB
W?B
W$B
W?B
XEB
X+B
X+B
X+B
XEB
YeB
YKB
YeB
ZQB
Z7B
[=B
[=B
[WB
\)B
\CB
\CB
\CB
]dB
]IB
]IB
]IB
^OB
^jB
^OB
_VB
_VB
_VB
_pB
_VB
_pB
`\B
`vB
`vB
`vB
`vB
abB
a|B
abB
abB
abB
a|B
b�B
b�B
bhB
b�B
cnB
cnB
cnB
cnB
dtB
dtB
dZB
d�B
dtB
dtB
d�B
ezB
e�B
f�B
f�B
f�B
ffB
ffB
ffB
ffB
f�B
f�B
g�B
g�B
g�B
h�B
h�B
h�B
h�B
h�B
i�B
hsB
h�B
iyB
i�B
i�B
i�B
j�B
j�B
j�B
j�B
j�B
jB
jB
jB
j�B
j�B
k�B
k�B
k�B
k�B
k�B
k�B
l�B
l�B
l�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
n�B
n�B
n�B
o�B
o�B
o�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
r�B
r�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
t�B
t�B
t�B
u�B
u�B
u�B
u�B
u�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
w�B
w�B
w�B
w�B
xB
w�B
y	B
x�B
x�B
x�B
x�B
y	B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
zB
y�B
zB
y�B
z�B
{B
{B
z�B
{B
|B
|B
|B
|�B
}B
}B
}B
}B
|�B
}B
}B
}B
}B
}"B
|�B
}�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.05(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201611030038212016110300382120161103003821201806221304172018062213041720180622130417201804050704072018040507040720180405070407  JA  ARFMdecpA19c                                                                20161030093505  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20161030003526  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20161030003527  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20161030003527  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20161030003528  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20161030003528  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20161030003528  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20161030003528  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20161030003528  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20161030003529                      G�O�G�O�G�O�                JA  ARUP                                                                        20161030013248                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20161030153408  CV  JULD            G�O�G�O�F���                JM  ARCAJMQC2.0                                                                 20161102153821  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20161102153821  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404220407  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622040417  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200116211517                      G�O�G�O�G�O�                