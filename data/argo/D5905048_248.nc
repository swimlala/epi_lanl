CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-06-07T00:35:04Z creation;2018-06-07T00:35:08Z conversion to V3.1;2019-12-19T07:36:31Z update;     
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
resolution        =���   axis      Z        `  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  H�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     `  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \4   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     `  `   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ol   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     `  sD   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     `  �|   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     `  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     `  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     `  �L   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     `  ˬ   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ۜ   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20180607003504  20200116231517  5905048                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0577_248                     2C  D   NAVIS_A                         0577                            ARGO 102115                     863 @�hv�- 1   @�hv�-� @4$�J�M�dP��-�1   GPS     A   A   B   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(ffB0ffB8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf�Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2�fD3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�C3D�� D�� D�fD�#3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@���@�ff@�ffA33A?33A_33A33A���A���A���A���Aϙ�Aߙ�AA���B��B��B��B��B(33B033B7��B?��BG��BO��BW��B_��Bg��Bo��Bw��B��B��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fC�3C�3C�3C�3C	�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C!�3C#�3C%�3C'�3C)�3C+�3C-�3C/�3C1�3C3�3C5�3C7�3C9�3C;�3C=�3C?�3CA�3CC�3CE�3CG�3CI�3CK�3CM�3CO�3CQ�3CS�3CU�3CW�3CY�3C[�3C]�3C_�3Ca�3Cc�3Cf�Cg�3Ci�3Ck�3Cm�3Co�3Cq�3Cs�3Cu�3Cw�3Cy�3C{�3C}�3C�3C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D |�D ��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D	|�D	��D
|�D
��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D |�D ��D!|�D!��D"|�D"��D#|�D#��D$|�D$��D%|�D%��D&|�D&��D'|�D'��D(|�D(��D)|�D)��D*|�D*��D+|�D+��D,|�D,��D-|�D-��D.|�D.��D/|�D/��D0|�D0��D1|�D1��D2�3D2��D3|�D3��D4|�D4��D5|�D5��D6|�D6��D7|�D7��D8|�D8��D9|�D9��D:|�D:��D;|�D;��D<|�D<��D=|�D=��D>|�D>��D?|�D?��D@|�D@��DA|�DA��DB|�DB��DC|�DC��DD|�DD��DE|�DE��DF|�DF��DG|�DG��DH|�DH��DI|�DI��DJ|�DJ��DK|�DK��DL|�DL��DM|�DM��DN|�DN��DO|�DO��DP|�DP��DQ|�DQ��DR|�DR��DS|�DS��DT|�DT��DU|�DU��DV|�DV��DW|�DW��DX|�DX��DY|�DY��DZ|�DZ��D[|�D[��D\|�D\��D]|�D]��D^|�D^��D_|�D_��D`|�D`��Da|�Da��Db|�Db��Dc|�Dc��Dd|�Dd��De|�De��Df|�Df��Dg|�Dg��Dh|�Dh��Di|�Di��Dj|�Dj��Dk|�Dk��Dl|�Dl��Dm|�Dm��Dn|�Dn��Do|�Do��Dp|�Dp��Dq|�Dq��Dr|�Dr��Ds|�Ds��Dt|�Dt��Du|�Du��Dv|�Dv��Dw|�Dw��Dx|�Dx��Dy|�Dy��Dz|�Dz��D{|�D{��D||�D|��D}|�D}��D~|�D~��D|�D��D�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD���D��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD¾fD��fD�>fD�~fDþfD��fD�>fD�~fDľfD��fD�>fD�~fDžfD��fD�>fD�~fDƾfD��fD�>fD�~fDǾfD��fD�>fD�~fDȾfD��fD�>fD�~fDɾfD��fD�>fD�~fDʾfD��fD�>fD�~fD˾fD��fD�>fD�~fD̾fD��fD�>fD�~fD;fD��fD�>fD�~fDξfD��fD�>fD�~fDϾfD��fD�>fD�~fDоfD��fD�>fD�~fDѾfD��fD�>fD�~fDҾfD��fD�>fD�~fDӾfD��fD�>fD�~fDԾfD��fD�>fD�~fDվfD��fD�>fD�~fD־fD��fD�>fD�~fD׾fD��fD�>fD�~fDؾfD��fD�>fD�~fDپfD��fD�>fD�~fDھfD��fD�>fD�~fD۾fD��fD�>fD�~fDܾfD��fD�>fD�~fDݾfD��fD�>fD�~fD޾fD��fD�>fD�~fD߾fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD��fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD��fD��fD�A�D�~fD��fD��D�!�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�A�A�%A���A���A���A���A���A��A���A��A��A��A���A���A���A���A���A��A���A��A��A��yAʹ9ÁA��A�dZA�^5A�O�A��/A��
A�A�A�~�A�`BA�A�A��TA�9XA��A�O�A�bA�S�A���A�`BA��A�M�A��A���A�~�A�p�A���A�ȴA�$�A���A��A�=qA��A���A�"�A���A�C�A���A���A�bNA�?}A��A���A�`BA���A�ffA��#A��mA�S�A�
=A�  A�"�A���A��A��HA�p�A�dZA�A�A���A�`BA��TA���A��A��TA��A��A�VA��A��TA���A���A�;dA��
A�;dA�`BA��A���A��DA���A��^A��A��!A���A~��A|�Azz�Av5?Au�As�
Ao�
Al�AlJAi�#Af��AeS�Ad-Ab��A`�A_��A^I�A\�AZ�\AY�AXAVVAU��ATZAQ��AOAN1'AM?}AK�#AJQ�AI�AH�!AH �AG��AFȴAE�^AC�PABĜABI�AA|�A?�^A>�A>9XA=�A=S�A<$�A:��A9��A8ffA6��A6A5��A4��A3�A1��A/��A/�7A.bNA-33A,^5A+��A*ȴA);dA(E�A'�A&�+A%G�A#��A#��A"�A"ffA!��A!��A!+A ZA�mA�hA�A�mAĜAA|�A��A�
AȴA(�AA��A��A\)AȴAhsA�/AE�A�`A$�A�^A
��A	�
A	��A	S�A�yAr�A|�AI�A�AbNA  A��AC�A��A�A��A+A ��A  �@�@�$�@�x�@�1'@�5?@�Q�@�dZ@���@��R@�ff@�/@�P@��@�`B@@��y@@�@� �@��T@�9@�dZ@��@�n�@�O�@߅@�@�=q@�/@��@ܛ�@۝�@�@�p�@���@�A�@֏\@�ƨ@�ff@�@�x�@��@��@�G�@���@˾w@ʏ\@��@��
@Ƈ+@�ff@�X@�\)@�V@�&�@�;d@�@��@�r�@�1'@���@�+@�ȴ@�-@���@�G�@���@�1@�S�@���@��@�`B@�&�@��@�|�@��@��+@�@���@���@��@�G�@��@��@���@�I�@��
@�|�@�C�@�ȴ@���@�ff@���@�%@���@�j@�9X@�1@�ƨ@�\)@��\@�$�@��@���@�G�@���@�j@�I�@��@��P@�\)@�l�@�33@��H@��R@�v�@�=q@���@��h@��@��@�  @�33@��@��\@�^5@�5?@��^@��7@�`B@��@���@��D@�j@�9X@�b@���@�ƨ@��@��@���@�|�@���@���@�5?@�$�@�J@��@��T@�@�x�@�%@�V@�/@�&�@�r�@�9X@��w@���@�t�@�C�@��R@�V@�ff@�^5@�=q@��^@�%@���@��@�Z@�A�@�(�@��@���@��m@��@�33@��@��!@���@��\@�~�@�E�@��#@���@�x�@�X@��@���@�Ĝ@�Ĝ@�Ĝ@�Ĝ@��j@��@�b@�  @��w@���@�+@���@���@��@��R@�V@�`B@�/@�V@��@��@�x�@�@���@�O�@�z�@���@��@�(�@�9X@��@�|�@�33@�@��H@�ȴ@���@���@�ȴ@��!@�v�@�J@���@�hs@���@��j@��@�j@���@�+@��@�ȴ@���@��+@�^5@��@��#@�@���@�`B@�O�@�G�@��`@��u@�bN@�1@�|�@�;d@�33@��@��@��\@�ff@�M�@�J@��T@���@�@���@�?}@�V@���@��D@�A�@��@���@�t�@�C�@��@��@�@��y@�ȴ@���@�n�@�M�@�J@��T@��-@��@�x�@�&�@��`@���@��j@��9@���@��@�Q�@� �@K�@~$�@~$�@~$�@~$�@~{@}�T@}��@}p�@}`B@}O�@|��@|��@|Z@{�
@{t�@{C�@z��@z~�@y��@yhs@yX@yG�@y�@x�u@xbN@x1'@w��@w�@w��@w\)@v��@u@uO�@u?}@t�/@t��@t(�@s�m@t1@t�@t1@s�m@s�@sS�@s@r�!@r=q@q��@q&�@p1'@oK�@n5?@m��@m`B@mO�@m�@l��@l�/@l��@lz�@l9X@k�m@k�F@k�F@k�F@k�@kdZ@kS�@ko@j�@ihs@h��@hĜ@hĜ@h�9@h��@hbN@g�;@g�P@g
=@f��@f5?@e�@dz�@c�F@b�H@b~�@b-@a��@a&�@`Ĝ@_�;@^��@^�R@^�+@^V@]�h@]V@\�j@\�D@\9X@[�m@[S�@[@Z��@Z=q@Y��@Y�@YG�@XĜ@X�u@X�@X �@W�@Wl�@W;d@V{@U�-@U�@UO�@U/@T��@Tj@S33@R�H@R��@RM�@R-@Q�@Q��@Qx�@Q7L@P��@PĜ@P �@O�@O\)@O
=@N��@N5?@N{@M�T@Mp�@M`B@M`B@M`B@M�@L�j@L�@K��@KdZ@K33@K@J��@J��@J~�@J=q@I�@I�7@IX@I�@H��@HbN@Hb@G�;@G�P@G+@G�@F��@F��@FE�@E@EO�@D�@Dj@C��@CdZ@CdZ@C33@B�@B��@B�\@B~�@B=q@B-@A�@A�^@A��@Ax�@AG�@A%@@�@@A�@@b@?�;@?l�@>�R@>E�@=�@=�-@=V@<�/@<�j@<z�@<�@;��@;dZ@:�@:��@:�\@:^5@9�@9X@97L@9�@9%@8Ĝ@8Q�@8  @7�P@7�@6�@6��@6ff@65?@5@5`B@5�@4��@4�@4Z@41@3�
@3�@333@2��@2~�@2�@1�^@1x�@17L@0��@0�@0Q�@0 �@/�w@/l�@/+@.��@.�@.�R@.@-�-@-�-@-�h@-/@-V@,��@,j@+��@+�m@+�
@+��@+dZ@+S�@+o@*�!@*�\@*=q@)��@)x�@)G�@)&�@)�@(�`@(�9@(r�@(1'@(b@'�@'�@'��@'��@'l�@'+@&�R@&v�@&v�@&E�@&{@%��@%�h@%�@%�@$��@$I�@#��@#dZ@#"�@"�@"�!@"�\@"^5@"^5@"M�@"-@!��@!�^@!x�@!7L@!%@ �u@  �@�@�;@l�@;d@
=@�y@�R@�+@E�@�T@��@p�@/@��@��@��@�j@�@z�@I�@(�@1@��@�@S�@@�H@��@��@~�@^5@�@��@G�@7L@%@��@Ĝ@�9@�@A�@  @�;@�w@�P@l�@\)@;d@;d@;d@;d@;d@��@�R@�R@v�@5?@{@�@��@�h@`B@�@��@�/@��@�j@z�@Z@9X@1@ƨ@��@dZ@33@�@��@��@~�@^5@-@�#@��@x�@X@�`@�9@�u@�u@r�@A�@  @�;@�w@�@��@�P@\)@�@�y@�@��@��@v�@V@E�@5?@{@�@�T@@�-@��@�h@�@�@p�@O�@/@�@V@�@��@�j@�j@�j@��@z�@Z@I�@9X@(�@1@�m@�F@��@C�@o@@
�H@
��@
��@
��@
�!@
��@
�\@
M�@
M�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�A�A�%A���A���A���A���A���A��A���A��A��A��A���A���A���A���A���A��A���A��A��A��yAʹ9ÁA��A�dZA�^5A�O�A��/A��
A�A�A�~�A�`BA�A�A��TA�9XA��A�O�A�bA�S�A���A�`BA��A�M�A��A���A�~�A�p�A���A�ȴA�$�A���A��A�=qA��A���A�"�A���A�C�A���A���A�bNA�?}A��A���A�`BA���A�ffA��#A��mA�S�A�
=A�  A�"�A���A��A��HA�p�A�dZA�A�A���A�`BA��TA���A��A��TA��A��A�VA��A��TA���A���A�;dA��
A�;dA�`BA��A���A��DA���A��^A��A��!A���A~��A|�Azz�Av5?Au�As�
Ao�
Al�AlJAi�#Af��AeS�Ad-Ab��A`�A_��A^I�A\�AZ�\AY�AXAVVAU��ATZAQ��AOAN1'AM?}AK�#AJQ�AI�AH�!AH �AG��AFȴAE�^AC�PABĜABI�AA|�A?�^A>�A>9XA=�A=S�A<$�A:��A9��A8ffA6��A6A5��A4��A3�A1��A/��A/�7A.bNA-33A,^5A+��A*ȴA);dA(E�A'�A&�+A%G�A#��A#��A"�A"ffA!��A!��A!+A ZA�mA�hA�A�mAĜAA|�A��A�
AȴA(�AA��A��A\)AȴAhsA�/AE�A�`A$�A�^A
��A	�
A	��A	S�A�yAr�A|�AI�A�AbNA  A��AC�A��A�A��A+A ��A  �@�@�$�@�x�@�1'@�5?@�Q�@�dZ@���@��R@�ff@�/@�P@��@�`B@@��y@@�@� �@��T@�9@�dZ@��@�n�@�O�@߅@�@�=q@�/@��@ܛ�@۝�@�@�p�@���@�A�@֏\@�ƨ@�ff@�@�x�@��@��@�G�@���@˾w@ʏ\@��@��
@Ƈ+@�ff@�X@�\)@�V@�&�@�;d@�@��@�r�@�1'@���@�+@�ȴ@�-@���@�G�@���@�1@�S�@���@��@�`B@�&�@��@�|�@��@��+@�@���@���@��@�G�@��@��@���@�I�@��
@�|�@�C�@�ȴ@���@�ff@���@�%@���@�j@�9X@�1@�ƨ@�\)@��\@�$�@��@���@�G�@���@�j@�I�@��@��P@�\)@�l�@�33@��H@��R@�v�@�=q@���@��h@��@��@�  @�33@��@��\@�^5@�5?@��^@��7@�`B@��@���@��D@�j@�9X@�b@���@�ƨ@��@��@���@�|�@���@���@�5?@�$�@�J@��@��T@�@�x�@�%@�V@�/@�&�@�r�@�9X@��w@���@�t�@�C�@��R@�V@�ff@�^5@�=q@��^@�%@���@��@�Z@�A�@�(�@��@���@��m@��@�33@��@��!@���@��\@�~�@�E�@��#@���@�x�@�X@��@���@�Ĝ@�Ĝ@�Ĝ@�Ĝ@��j@��@�b@�  @��w@���@�+@���@���@��@��R@�V@�`B@�/@�V@��@��@�x�@�@���@�O�@�z�@���@��@�(�@�9X@��@�|�@�33@�@��H@�ȴ@���@���@�ȴ@��!@�v�@�J@���@�hs@���@��j@��@�j@���@�+@��@�ȴ@���@��+@�^5@��@��#@�@���@�`B@�O�@�G�@��`@��u@�bN@�1@�|�@�;d@�33@��@��@��\@�ff@�M�@�J@��T@���@�@���@�?}@�V@���@��D@�A�@��@���@�t�@�C�@��@��@�@��y@�ȴ@���@�n�@�M�@�J@��T@��-@��@�x�@�&�@��`@���@��j@��9@���@��@�Q�@� �@K�@~$�@~$�@~$�@~$�@~{@}�T@}��@}p�@}`B@}O�@|��@|��@|Z@{�
@{t�@{C�@z��@z~�@y��@yhs@yX@yG�@y�@x�u@xbN@x1'@w��@w�@w��@w\)@v��@u@uO�@u?}@t�/@t��@t(�@s�m@t1@t�@t1@s�m@s�@sS�@s@r�!@r=q@q��@q&�@p1'@oK�@n5?@m��@m`B@mO�@m�@l��@l�/@l��@lz�@l9X@k�m@k�F@k�F@k�F@k�@kdZ@kS�@ko@j�@ihs@h��@hĜ@hĜ@h�9@h��@hbN@g�;@g�P@g
=@f��@f5?@e�@dz�@c�F@b�H@b~�@b-@a��@a&�@`Ĝ@_�;@^��@^�R@^�+@^V@]�h@]V@\�j@\�D@\9X@[�m@[S�@[@Z��@Z=q@Y��@Y�@YG�@XĜ@X�u@X�@X �@W�@Wl�@W;d@V{@U�-@U�@UO�@U/@T��@Tj@S33@R�H@R��@RM�@R-@Q�@Q��@Qx�@Q7L@P��@PĜ@P �@O�@O\)@O
=@N��@N5?@N{@M�T@Mp�@M`B@M`B@M`B@M�@L�j@L�@K��@KdZ@K33@K@J��@J��@J~�@J=q@I�@I�7@IX@I�@H��@HbN@Hb@G�;@G�P@G+@G�@F��@F��@FE�@E@EO�@D�@Dj@C��@CdZ@CdZ@C33@B�@B��@B�\@B~�@B=q@B-@A�@A�^@A��@Ax�@AG�@A%@@�@@A�@@b@?�;@?l�@>�R@>E�@=�@=�-@=V@<�/@<�j@<z�@<�@;��@;dZ@:�@:��@:�\@:^5@9�@9X@97L@9�@9%@8Ĝ@8Q�@8  @7�P@7�@6�@6��@6ff@65?@5@5`B@5�@4��@4�@4Z@41@3�
@3�@333@2��@2~�@2�@1�^@1x�@17L@0��@0�@0Q�@0 �@/�w@/l�@/+@.��@.�@.�R@.@-�-@-�-@-�h@-/@-V@,��@,j@+��@+�m@+�
@+��@+dZ@+S�@+o@*�!@*�\@*=q@)��@)x�@)G�@)&�@)�@(�`@(�9@(r�@(1'@(b@'�@'�@'��@'��@'l�@'+@&�R@&v�@&v�@&E�@&{@%��@%�h@%�@%�@$��@$I�@#��@#dZ@#"�@"�@"�!@"�\@"^5@"^5@"M�@"-@!��@!�^@!x�@!7L@!%@ �u@  �@�@�;@l�@;d@
=@�y@�R@�+@E�@�T@��@p�@/@��@��@��@�j@�@z�@I�@(�@1@��@�@S�@@�H@��@��@~�@^5@�@��@G�@7L@%@��@Ĝ@�9@�@A�@  @�;@�w@�P@l�@\)@;d@;d@;d@;d@;d@��@�R@�R@v�@5?@{@�@��@�h@`B@�@��@�/@��@�j@z�@Z@9X@1@ƨ@��@dZ@33@�@��@��@~�@^5@-@�#@��@x�@X@�`@�9@�u@�u@r�@A�@  @�;@�w@�@��@�P@\)@�@�y@�@��@��@v�@V@E�@5?@{@�@�T@@�-@��@�h@�@�@p�@O�@/@�@V@�@��@�j@�j@�j@��@z�@Z@I�@9X@(�@1@�m@�F@��@C�@o@@
�H@
��@
��@
��@
�!@
��@
�\@
M�@
M�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	ȴB	��B
E�B
x�B
�9B
�mB1'B�BÖB�5B�;B�5B�5B�BB�B��B%B�B&�B5?BF�BO�BZBZBR�BM�BcTBo�Bt�Bn�Bp�Bq�BjBiyBgmBT�B`BBp�Bn�Bs�Bp�B�+B|�Bu�B[#BH�B;dB(�B �BbB��B
=B
=BB��B�B�B�B�B�B��B��B��BÖB�B�{B}�Bm�BT�BG�B$�B
��B
�B
�fB
��B
�XB
��B
��B
�B
cTB
?}B
1'B
 �B
	7B	�ZB	�NB	��B	�9B	��B	�B	��B	�+B	�DB	�+B	� B	p�B	k�B	aHB	ZB	N�B	P�B	H�B	<jB	=qB	2-B	�B	bB	�B	oB		7B	B	B	B	B��B��B�yB�ZB�TB�ZB�5B��B��B�
B��B��BŢB��B�wB�LB�B�LB�?B�B��B��B��B��B��B��B��B��B�VB�B�B�B�1B�%B� B�=B�B�B�B�B�Bz�B|�Bz�Bu�Bo�Bm�Bm�Bp�BiyBk�BgmBk�Bn�BiyB_;BR�BJ�BT�B_;B[#BM�BD�BdZB`BBjBo�Bp�Bm�BjBbNBZB\)BiyBk�BiyBhsBe`BcTBcTBbNBaHBaHB`BBbNBaHB_;B`BB_;BiyBk�Bn�Bk�BffBe`Bm�BiyBhsBp�Bs�Bo�Bk�BhsBo�Bm�BiyBdZBn�Bk�Bu�Bv�Bt�Bz�By�Bu�Bv�B|�B|�By�Br�Bo�Bx�B�B� B� Bw�B{�B�1B�B�B�B�JB�\B��B�hB�VB��B��B��B��B�3B�FB�dB�qB�}BÖBĜBȴB��B��B��B��B��B�
B�B�BB�BB�HB�sB�B�B��B��B��B��B��B��B��B	  B	B	+B	1B		7B	PB	VB	VB	bB	�B	�B	�B	 �B	 �B	 �B	!�B	$�B	,B	0!B	1'B	2-B	6FB	9XB	9XB	;dB	?}B	C�B	E�B	I�B	L�B	N�B	P�B	P�B	XB	ZB	[#B	`BB	aHB	gmB	l�B	o�B	p�B	q�B	s�B	u�B	v�B	y�B	{�B	|�B	}�B	� B	�B	�B	�B	�7B	�DB	�JB	�PB	�VB	�bB	�uB	��B	�{B	�{B	�uB	�uB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�-B	�9B	�9B	�?B	�?B	�?B	�9B	�FB	�LB	�dB	�jB	�qB	�qB	�jB	�jB	�}B	��B	B	B	ÖB	ǮB	ǮB	ǮB	ǮB	ǮB	ƨB	ƨB	��B	��B	��B	��B	��B	�B	�#B	�B	�B	�B	�#B	�/B	�;B	�HB	�`B	�sB	�sB	�sB	�fB	�mB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B	��B	��B
B
B
B
B
B
B
+B
1B
1B
DB
DB
DB
DB

=B
DB
JB
JB
JB
JB
DB
JB
DB
DB
VB
VB
VB
VB
VB
PB
VB
PB
VB
uB
{B
uB
uB
uB
uB
{B
{B
{B
uB
uB
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
"�B
"�B
"�B
"�B
!�B
!�B
!�B
 �B
 �B
 �B
 �B
!�B
"�B
#�B
&�B
(�B
)�B
)�B
)�B
)�B
)�B
)�B
)�B
)�B
+B
,B
+B
+B
+B
)�B
(�B
'�B
(�B
+B
,B
-B
-B
,B
,B
+B
,B
+B
+B
,B
)�B
+B
,B
,B
.B
.B
.B
.B
/B
.B
.B
1'B
1'B
1'B
0!B
1'B
2-B
33B
33B
33B
2-B
49B
49B
33B
49B
5?B
49B
49B
6FB
7LB
6FB
6FB
7LB
7LB
49B
8RB
9XB
9XB
9XB
8RB
7LB
6FB
:^B
;dB
;dB
;dB
;dB
;dB
<jB
<jB
<jB
<jB
;dB
<jB
<jB
=qB
<jB
>wB
?}B
?}B
>wB
@�B
@�B
@�B
?}B
>wB
>wB
?}B
@�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
B�B
A�B
A�B
B�B
B�B
B�B
B�B
B�B
D�B
C�B
B�B
B�B
B�B
B�B
C�B
C�B
B�B
D�B
E�B
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
F�B
F�B
E�B
F�B
G�B
F�B
E�B
E�B
F�B
G�B
G�B
G�B
I�B
I�B
H�B
H�B
I�B
J�B
J�B
K�B
K�B
J�B
J�B
J�B
L�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
O�B
P�B
Q�B
Q�B
P�B
P�B
Q�B
Q�B
R�B
R�B
R�B
R�B
R�B
R�B
S�B
S�B
S�B
S�B
T�B
T�B
T�B
VB
W
B
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
VB
XB
YB
YB
XB
YB
YB
YB
YB
[#B
[#B
[#B
ZB
[#B
[#B
[#B
\)B
[#B
[#B
\)B
\)B
]/B
]/B
]/B
]/B
]/B
]/B
^5B
^5B
^5B
^5B
^5B
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
_;B
_;B
_;B
`BB
_;B
aHB
bNB
bNB
bNB
cTB
dZB
dZB
cTB
bNB
dZB
cTB
cTB
cTB
cTB
cTB
dZB
e`B
dZB
e`B
e`B
ffB
e`B
ffB
e`B
e`B
ffB
gmB
ffB
gmB
gmB
hsB
hsB
hsB
gmB
gmB
hsB
hsB
gmB
hsB
hsB
hsB
iyB
iyB
iyB
iyB
iyB
iyB
iyB
jB
k�B
k�B
l�B
l�B
l�B
k�B
k�B
l�B
m�B
l�B
m�B
m�B
n�B
n�B
n�B
n�B
n�B
m�B
m�B
m�B
n�B
m�B
n�B
n�B
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
q�B
q�B
q�B
q�B
r�B
q�B
q�B
q�B
q�B
r�B
r�B
q�B
s�B
s�B
t�B
s�B
s�B
s�B
t�B
t�B
t�B
t�B
t�B
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
w�B
w�B
w�B
w�B
w�B
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
w�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
y�B
y�B
y�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
{�B
|�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�HB	��B	��B	��B
GEB
{JB
��B
��B4�B��B��B�OB߾B�;B߾B�B�B�B�`B�B \B($B6`BG_BP}BZQBZ�BTaBPbBdtBpUBu%Bo�Bq[Br|Bk�Bj�Bh�BX_Ba�BrGBqABv+BshB�fBBx�B^�BL~B=<B,B#�BFB��BDB
�BtB�$B��B�B��B��BیB��BѝB�B��B�UB�+B��BpUBW?BJ=B*B�B
��B
�B
רB
��B
�_B
�B
�MB
h
B
D3B
4B
#nB
~B	�B	��B	�gB	��B	�&B	��B	��B	��B	�B	��B	��B	r�B	mB	c�B	\]B	Q4B	RoB	J�B	>�B	>�B	4TB	!�B	�B	�B	�B	)B	�B	?B	%B	�B��B�2B�QB�B�ZB�,BߊB�B�@BרB՛B��B�EB�-B�B�>B�;B�8B�+B��B��B��B��B��B�7B�$B��B��B��B�%B��B��B�RB��B��B��B�B��B��B��B��B|B}�B{�Bv�Bq'Bo Bn�Bq�Bj�Bl�Bh�BlWBo BjKB`�BU�BM�BV�B`B\]BP.BG�Bd�Ba�Bk�BpBq'BnIBkkBc�B[�B]�BjBl=Bj0Bi*BfLBd@BdBcBbBb4Ba-BcBa�B`\Ba�B`vBjBk�Bn�BlBg�Bf�Bn/Bj�Bi�Bq'BtBp�Bl�BjBp�Bn�Bj�BffBo�Bl�Bv+BwfButB{0BzDBv�Bw�B}qB}�Bz�BtBq[By�B�uB��B��By>B}B��B�B�%B�?B�B�HB��B�oB��B�eB��B�B��B��B��B��B��B� B��B�B�B�JB�dB�}BуBՁB׍BڠB��B��B�B��B�B� B�B�B�0B�6B�<B�.B�cB	 iB	�B	�B	�B		�B	�B	�B	�B	 B	�B		B	B	 �B	!B	!HB	"NB	%,B	,=B	0UB	1�B	2�B	6zB	9�B	9�B	;�B	?�B	C�B	E�B	I�B	MB	O(B	Q4B	QhB	XEB	Z�B	[�B	`�B	a�B	g�B	l�B	o�B	p�B	rB	s�B	vB	wB	zB	|B	}B	~(B	�B	�'B	�MB	�SB	�7B	�^B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�+B	��B	�B	�B	�&B	�B	�@B	�B	��B	�/B	�IB	�qB	��B	�IB	�OB	�aB	�TB	�TB	�tB	�ZB	�tB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	ǮB	��B	ǮB	��B	��B	��B	��B	��B	�B	� B	�.B	�:B	�B	�#B	�_B	�yB	ּB	�WB	�IB	�;B	�HB	�,B	�XB	�B	��B	�B	��B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	��B	�+B	�3B	��B	��B	��B	��B	��B	�	B	�8B	�B	��B	��B	�B	��B	��B	�*B	�B	�0B	�*B	�XB	�0B	�B	�B	�B	�B	�B	�B	�<B	�(B	�.B
 B	�.B	�BB
;B
;B
AB
GB
gB
SB
EB
KB
KB
DB
xB
xB
xB

rB
^B
dB
~B
~B
dB
�B
~B
xB
xB
pB
pB
pB
�B
pB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
	B
�B
�B
�B
�B
�B
 �B
"�B
"�B
"�B
#B
!�B
!�B
!�B
 �B
!B
 �B
!B
"4B
# B
$@B
'8B
)*B
*B
*B
*B
*0B
*B
*0B
*B
*0B
+B
,"B
+B
+B
+B
*0B
)DB
(>B
)DB
+6B
,"B
-)B
-)B
,"B
,"B
+QB
,"B
+6B
+QB
,=B
*eB
+QB
,WB
,qB
./B
.cB
.IB
.IB
/OB
.cB
.}B
1[B
1AB
1AB
0�B
1vB
2GB
3hB
3hB
3hB
2aB
4TB
4TB
3hB
4nB
5ZB
4nB
4�B
6`B
7�B
6zB
6�B
7�B
7�B
4�B
8lB
9rB
9rB
9rB
8�B
7�B
6�B
:xB
;�B
;B
;B
;B
;B
<�B
<�B
<�B
<�B
;�B
<�B
<�B
=�B
<�B
>�B
?�B
?�B
>�B
@�B
@�B
@�B
?�B
>�B
>�B
?�B
@�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
B�B
A�B
A�B
B�B
B�B
B�B
B�B
B�B
D�B
C�B
B�B
B�B
B�B
B�B
C�B
C�B
B�B
D�B
E�B
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
F�B
F�B
E�B
F�B
G�B
F�B
E�B
E�B
F�B
G�B
G�B
G�B
I�B
I�B
H�B
H�B
I�B
J�B
J�B
K�B
K�B
J�B
J�B
J�B
L�B
M�B
M�B
NB
NB
M�B
NB
NB
O�B
Q B
RB
RB
Q4B
Q4B
R B
RB
S&B
SB
SB
SB
SB
S&B
TFB
T,B
T,B
TFB
U2B
UB
U2B
VB
W$B
W$B
W$B
W?B
X+B
XEB
XEB
XEB
VmB
X+B
YB
Y1B
XEB
Y1B
YKB
YKB
YKB
[#B
[WB
[=B
Z7B
[=B
[=B
[=B
\CB
[WB
[WB
\CB
\CB
]IB
]IB
]IB
]IB
]IB
]dB
^OB
^OB
^jB
^5B
^OB
^jB
^jB
^jB
_VB
_VB
_pB
_pB
_pB
_VB
`vB
_�B
_�B
_pB
`vB
_pB
abB
bhB
bhB
b�B
c�B
dZB
dZB
cnB
b�B
dZB
cnB
c�B
cnB
c�B
c�B
dtB
ezB
d�B
ezB
ezB
f�B
e�B
f�B
e�B
ezB
f�B
g�B
f�B
g�B
g�B
hsB
hsB
hsB
g�B
g�B
h�B
h�B
g�B
h�B
h�B
h�B
i�B
i�B
i�B
i�B
i�B
i�B
i�B
j�B
k�B
k�B
l�B
l�B
l�B
k�B
k�B
l�B
m�B
l�B
m�B
m�B
n�B
n�B
n�B
n�B
n�B
m�B
m�B
m�B
n�B
m�B
n�B
n�B
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
q�B
q�B
q�B
q�B
r�B
q�B
q�B
q�B
q�B
r�B
r�B
q�B
s�B
s�B
t�B
s�B
s�B
s�B
t�B
t�B
t�B
t�B
t�B
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
w�B
w�B
w�B
w�B
w�B
w�B
xB
w�B
w�B
w�B
xB
x�B
x�B
x�B
x�B
xB
x�B
x�B
x�B
x�B
x�B
y	B
x�B
x�B
x�B
zB
y�B
y�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
|B
|�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111113111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111113111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.05(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201806110037312018061100373120180611003731201806221331392018062213313920180622133139201806120024022018061200240220180612002402  JA  ARFMdecpA19c                                                                20180607093503  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20180607003504  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20180607003506  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20180607003507  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20180607003507  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20180607003507  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20180607003507  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20180607003507  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20180607003508  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20180607003508                      G�O�G�O�G�O�                JA  ARUP                                                                        20180607005551                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20180607153920  CV  JULD            G�O�G�O�F�C�                JM  ARGQJMQC2.0                                                                 20180607153920  CV  JULD_LOCATION   G�O�G�O�F�C�                JM  ARGQJMQC2.0                                                                 20180607153920  CV  LATITUDE        G�O�G�O�A�"�                JM  ARGQJMQC2.0                                                                 20180607153920  CV  LONGITUDE       G�O�G�O��"��                JM  ARSQJMQC2.0                                                                 20180608000000  CF  PSAL_ADJUSTED_QCD�  D�� G�O�                JM  ARCAJMQC2.0                                                                 20180610153731  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180610153731  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180611152402  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622043139  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200116231517                      G�O�G�O�G�O�                