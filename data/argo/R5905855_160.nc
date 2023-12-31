CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2023-07-03T09:43:27Z creation;2023-07-03T09:43:28Z conversion to V3.1      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  78   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7x   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     7�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8<   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8@   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8H   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8L   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8T   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8\   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8d   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8h   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8p   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8t   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8x   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8|   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9|   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I8   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M(   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  tx   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �0   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �    PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �p   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �(   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ޘ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �    HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �D   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �T   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �X   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �h   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �l   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �p   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �tArgo profile    3.1 1.2 19500101000000  20230703094327  20230703102647  5905855                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA                                  2B  A   APEX                            8423                            2.11.2                          846 @�7�����1   @�7�=��@0t�j~���c��S���1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @��@�  @���A   A   A@  A`  A�  A�  A�  A�33A�  A�  A�  A�  A�33B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  BhffBp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B���B�33B�  B�  B�  B�  B�  B�  B�  B�ffB�  B���B�  B�  B�  B�  C   C  C  C  C  C
  C  C�C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB�CD  CF  CG�fCJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|�C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� DfD� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�vf1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @fg@|��@�33@�ffA33A?33A_33A33A���A���A���A���Aϙ�Aߙ�AA���B��B��B��B��B'��B/��B7��B?��BG��BO��BW��B_��Bh33Bo��Bw��B��B��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��3B��fB��3B��B��fB��fB��fB��fB��fB��fB��fB�L�B��fB�3B��fB��fB��fB��fB��fC�3C�3C�3C�3C	�3C�3C�C�3C�3C�3C�3C�3C�3C�3C�3C�3C!�3C#�3C%�3C'�3C)�3C+�3C-�3C/�3C1�3C3�3C5�3C7�3C9�3C;�3C=�3C?�3CB�CC�3CE�3CGٙCI�3CK�3CM�3CO�3CQ�3CS�3CU�3CW�3CY�3C[�3C]�3C_�3Ca�3Cc�3Ce�3Cg�3Ci�3Ck�3Cm�3Co�3Cq�3Cs�3Cu�3Cw�3Cy�3C|�C}�3C�3C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C�gC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C�gC���C���C���C���C���C���C���C���D |�D ��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D	|�D	��D
|�D
��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D3D|�D��D |�D ��D!|�D!��D"|�D"��D#|�D#��D$|�D$��D%|�D%��D&|�D&��D'|�D'��D(|�D(��D)|�D)��D*|�D*��D+|�D+��D,|�D,��D-|�D-��D.|�D.��D/|�D/��D0|�D0��D1|�D1��D2|�D2��D3|�D3��D4|�D4��D5|�D5��D6|�D6��D7|�D7��D8|�D8��D9|�D9��D:|�D:��D;|�D;��D<|�D<��D=|�D=��D>|�D>��D?|�D?��D@|�D@��DA|�DA��DB|�DB��DC|�DC��DD|�DD��DE|�DE��DF|�DF��DG|�DG��DH|�DH��DI|�DI��DJ|�DJ��DK|�DK��DL|�DL��DM|�DM��DN|�DN��DO|�DO��DP|�DP��DQ|�DQ��DR|�DR��DS|�DS��DT|�DT��DU|�DU��DV|�DV��DW|�DW��DX|�DX��DY|�DY��DZ|�DZ��D[|�D[��D\|�D\��D]|�D]��D^|�D^��D_|�D_��D`|�D`��Da|�Da��Db|�Db��Dc|�Dc��Dd|�Dd��De|�De��Df|�Df��Dg|�Dg��Dh|�Dh��Di|�Di��Dj|�Dj��Dk|�Dk��Dl|�Dl��Dm|�Dm��Dn|�Dn��Do|�Do��Dp|�Dp��Dq|�Dq��Dr|�Dr��Ds|�Ds��Dt|�Dt��Du|�Du��Dv|�Dv��Dw|�Dw��Dx|�Dx��Dy|�Dy��Dz|�Dz��D{|�D{��D||�D|��D}|�D}��D~|�D~��D|�D��D�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD¾fD��fD�>fD�~fDþfD��fD�>fD�~fDľfD��fD�>fD�~fDžfD��fD�>fD�~fDƾfD��fD�>fD�~fDǾfD��fD�>fD�~fDȾfD��fD�>fD�~fDɾfD��fD�>fD�~fDʾfD��fD�>fD�~fD˾fD��fD�>fD�~fD̾fD��fD�>fD�~fD;fD��fD�>fD�~fDξfD��fD�>fD�~fDϾfD��fD�>fD�~fDоfD��fD�>fD�~fDѾfD��fD�>fD�~fDҾfD��fD�>fD�~fDӾfD��fD�>fD�~fDԾfD��fD�>fD�~fDվfD��fD�>fD�~fD־fD��fD�>fD�~fD׾fD��fD�>fD�~fDؾfD��fD�>fD�~fDپfD��fD�>fD�~fDھfD��fD�>fD�~fD۾fD��fD�>fD�~fDܾfD��fD�>fD�~fDݾfD��fD�>fD�~fD޾fD��fD�>fD�~fD߾fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD��fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�t�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�ΥA��A��A�ҽA��[A�ԕA��&A���A���A��?A�רA��A�یA���A�خA��aAϾ�A�ҽA��#A��WA��A���A��A�� A��mAϧ�AψfAψ1Aυ�AτAπ�Aπ�Aρ;Aπ�Aπ�Aρ;Aρ;A�cA�cAπ AρA�~(A�{A�s�A�]/A��,A�?�A�W
A�AªeA��A�oA��A���A��A�:*A���A�8�A��A�ÖA��<A�JXA�EmA�zxA�=A�[�A�8RA��-A��.A�4�A��A�=qA��A�k�A�бA�(�A��A�;A��A�#�A�FtA��]A�+kA���A�F�A�ΥA��A�CA���A�A�1�A��A�A�M�A�NpA�%�A��[A~($Az]dAy?�AvN<AthsAss�Aq�Ao��Am�Ah1'Ad�CAa	�A]bNA[u�AZoAXR�AS��AM=�AKJ#AIN�AI~AG:�AD�ABjAA�oA@�TA?ݘA=�A;��A8��A7V�A6��A5��A4ϫA4J�A2�\A0oiA/�A/IRA-�!A,�A*��A'ɆA&c�A%aA$�-A$XA#�A#*0A"VA!��A!A�A�FAt�AA�A!�A2�A�AS�A��AC�A!A]�A3�A�FA�A)�AN<Ae�A�=A��A�A�A&�AeA�yAp;A��Az�AaA1�A�A��AW�A�*A�SAj�AxA�"A
��A��A��A~�A�dA�]Ae�A�'A�A/�A��A;A�#A��A�MA��A6A \AخA%�AO�A�A�3A �oA ��A �gA �F@��	@��@��A@��@�!�@���@�Dg@��@�j@�0@���@�@�@@�&@�H�@�8�@�{�@�O@�e@�bN@��@�4@��@�S&@�n�@��@�p;@�k@�*0@��@��@�7@��@�?@�qv@��@�E�@߻0@���@�)�@�k�@�!�@ܵ�@ۛ=@ڑ @�ƨ@ٓ�@�Xy@ץ@׺^@ה�@�33@��s@��W@Ոf@�7L@�ѷ@Ԡ�@ԟ�@�=q@Ӊ7@�1�@��@�'R@���@���@��8@��T@��6@Ϸ�@ϳ�@ϥ�@�]�@�{�@��@;w@���@˒:@� �@�Ov@�҉@�͟@�c�@��@ɷ@ɀ4@ɹ�@��3@��}@ɭC@�=@Ƞ�@���@��@�u%@���@�dZ@�?}@��@ÖS@°!@��@�A�@�_@�S�@��A@�#:@�U�@�e@��'@�$@�g8@��@�W?@��.@���@�l"@�($@�>B@�Xy@�_@���@��"@�1�@��h@��)@���@�� @�o @���@��@�iD@�!-@�oi@�{@�#�@�V�@��D@�_@��]@�� @��@��@�|@��@�f�@��2@��w@���@���@�:�@��@��@�rG@�s�@��@���@��@�&�@��O@�@���@��f@�C�@�4�@��|@��b@�v�@�i�@�q@��@��@��@��a@�m]@�C@�8�@�ں@���@�[�@�M@�
�@��g@���@�4@��@��)@�GE@��@�Z�@��]@���@�=q@��@��@�_�@���@�{J@��~@�)_@���@���@��X@���@�j�@�O@�@@��@�ی@�Ɇ@���@��m@�ȴ@��?@��j@���@���@�}V@�M@�^�@��<@���@��@�j@�V�@�C-@�)�@��Z@��@�y�@��"@�bN@��@���@��@�zx@���@���@�i�@�)�@��9@��7@�S�@�;d@�#�@���@��Z@���@�\)@�&@��@��L@�Xy@�7�@��@��@���@�a�@�Q�@�g�@�M�@���@���@���@��@��-@�;d@���@�^5@�-@��@���@��@���@�z�@�q@�l�@�G@���@���@��@��]@��h@�X@�%F@��@��@���@��5@���@���@�c�@��@��&@��f@��@���@��]@��@��Z@���@�?}@�@�ߤ@��?@�bN@��K@���@��$@���@�_p@�=@��p@��@�~(@�?@�*�@�
�@��@���@�s@�F�@�V@��@��@��@��z@�.�@���@�g�@�Mj@�H�@�@O@�%F@�@�@@���@��H@���@���@�YK@��@��@�@�n/@�#�@��O@��@���@�s�@�;�@�k@�@~�b@~#:@}�^@}7L@|V�@{�;@{�w@{��@{]�@{K�@{9�@z͟@z
�@y�@yf�@x�e@w��@wA�@w�@vȴ@v�x@vc @v&�@u��@t�)@tI�@t7�@t6@s��@rxl@q�)@q�@q�j@q�N@qO�@p��@pbN@p!@oiD@n�"@n��@nQ@n��@n��@n�\@n��@oP�@oA�@o�@n�2@n_�@m�@l��@l~(@l(�@k��@k�*@k|�@j�\@j=q@i��@i�^@izx@i�@h�@h�D@hM@h7@g�Q@g/�@f)�@e�@e��@ew2@eDg@e;@dی@d��@dC-@d7�@d[�@dq@d/�@c��@c&@c�@b͟@b	@a�=@ak�@a#�@`�z@_��@_��@_~�@_o@^�@]�N@]�@]e,@\��@];@\ѷ@\�z@\M@[�&@[S�@Zߤ@Z	@Y�j@Y�h@YX@X��@X�@XĜ@X��@X�z@X:�@Wخ@WH�@V�1@V@U�3@U��@Ue,@U�@TɆ@T�$@T�z@Toi@T�@S��@R�s@Q��@Q��@Q��@Q�'@Q&�@P�@PbN@PH@P%�@O��@OH�@N��@Nȴ@N�}@N��@N5?@M��@M��@MJ�@M#�@L��@K��@K��@K��@K�
@K��@KU�@K�@J��@J+k@I�@I�9@Izx@H�@H_@G��@G��@G�f@G9�@G
=@F�@F�+@E��@E�"@E2a@D�f@D��@Dl"@C��@C� @C��@CK�@CY@B��@B�@A�9@A�^@Ao @@�@@tT@@>B@@"h@@@@�@?�@?��@?=@?�@>�M@>��@>�X@>�!@>�@>��@>�r@>E�@=��@=��@=@<�@<��@<Z@<<�@;�@;�k@;n/@:J@9%@8�.@7��@7��@7t�@7U�@7=@6��@6u%@6i�@6V@64@5f�@5<6@5%F@4�K@4�o@3�}@3��@3��@3�f@3|�@3W?@3)_@2��@2B[@2�@1�@1rG@1&�@0Ɇ@0h�@0`�@0/�@/��@/O@.��@.GE@-�@,�@,��@,��@,e�@,-�@+� @+�4@+o�@+n/@+j�@+o�@+s@+e�@+$t@*�@*�L@*l�@)�@)�~@)^�@(r�@'{J@&�@&��@&��@&s�@&J�@&.�@%��@%�j@%�S@%@$�U@$�z@$|�@$,=@$�@#�@#�@#� @#��@#4�@#�@"��@"^5@!a�@ �p@ tT@  �@��@�F@�{@@͟@ȴ@��@��@u%@4@�T@�S@&�@�	@�Y@�@  @��@�W@��@{J@$t@�X@��@��@�Z@�)@�@�9@��@O�@%@�	@��@�v@�p@�9@�@�D@_@*�@��@��@��@t�@o�@qv@o�@b�@P�@/�@�@�s@�}@n�@E�@&�@�T@�N@��@�@��@2a@*0@�|@�@�D@oi@g8@e�@�A@��@s@X�@(@M�@@^�@;@�P@��@��@�K@�K@��@��@��@�u@�o@m�@`�@bN@A�@K^@�@1@�@�&@�V@Mj@@�@�R@}V@h
@i�@z@��@_�@$�@�T@�7@T�@G�@4@�@��@֡@�O@�@D�@�@��@��@e�@o@
��@
�@
��@
�A@
_�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�ΥA��A��A�ҽA��[A�ԕA��&A���A���A��?A�רA��A�یA���A�خA��aAϾ�A�ҽA��#A��WA��A���A��A�� A��mAϧ�AψfAψ1Aυ�AτAπ�Aπ�Aρ;Aπ�Aπ�Aρ;Aρ;A�cA�cAπ AρA�~(A�{A�s�A�]/A��,A�?�A�W
A�AªeA��A�oA��A���A��A�:*A���A�8�A��A�ÖA��<A�JXA�EmA�zxA�=A�[�A�8RA��-A��.A�4�A��A�=qA��A�k�A�бA�(�A��A�;A��A�#�A�FtA��]A�+kA���A�F�A�ΥA��A�CA���A�A�1�A��A�A�M�A�NpA�%�A��[A~($Az]dAy?�AvN<AthsAss�Aq�Ao��Am�Ah1'Ad�CAa	�A]bNA[u�AZoAXR�AS��AM=�AKJ#AIN�AI~AG:�AD�ABjAA�oA@�TA?ݘA=�A;��A8��A7V�A6��A5��A4ϫA4J�A2�\A0oiA/�A/IRA-�!A,�A*��A'ɆA&c�A%aA$�-A$XA#�A#*0A"VA!��A!A�A�FAt�AA�A!�A2�A�AS�A��AC�A!A]�A3�A�FA�A)�AN<Ae�A�=A��A�A�A&�AeA�yAp;A��Az�AaA1�A�A��AW�A�*A�SAj�AxA�"A
��A��A��A~�A�dA�]Ae�A�'A�A/�A��A;A�#A��A�MA��A6A \AخA%�AO�A�A�3A �oA ��A �gA �F@��	@��@��A@��@�!�@���@�Dg@��@�j@�0@���@�@�@@�&@�H�@�8�@�{�@�O@�e@�bN@��@�4@��@�S&@�n�@��@�p;@�k@�*0@��@��@�7@��@�?@�qv@��@�E�@߻0@���@�)�@�k�@�!�@ܵ�@ۛ=@ڑ @�ƨ@ٓ�@�Xy@ץ@׺^@ה�@�33@��s@��W@Ոf@�7L@�ѷ@Ԡ�@ԟ�@�=q@Ӊ7@�1�@��@�'R@���@���@��8@��T@��6@Ϸ�@ϳ�@ϥ�@�]�@�{�@��@;w@���@˒:@� �@�Ov@�҉@�͟@�c�@��@ɷ@ɀ4@ɹ�@��3@��}@ɭC@�=@Ƞ�@���@��@�u%@���@�dZ@�?}@��@ÖS@°!@��@�A�@�_@�S�@��A@�#:@�U�@�e@��'@�$@�g8@��@�W?@��.@���@�l"@�($@�>B@�Xy@�_@���@��"@�1�@��h@��)@���@�� @�o @���@��@�iD@�!-@�oi@�{@�#�@�V�@��D@�_@��]@�� @��@��@�|@��@�f�@��2@��w@���@���@�:�@��@��@�rG@�s�@��@���@��@�&�@��O@�@���@��f@�C�@�4�@��|@��b@�v�@�i�@�q@��@��@��@��a@�m]@�C@�8�@�ں@���@�[�@�M@�
�@��g@���@�4@��@��)@�GE@��@�Z�@��]@���@�=q@��@��@�_�@���@�{J@��~@�)_@���@���@��X@���@�j�@�O@�@@��@�ی@�Ɇ@���@��m@�ȴ@��?@��j@���@���@�}V@�M@�^�@��<@���@��@�j@�V�@�C-@�)�@��Z@��@�y�@��"@�bN@��@���@��@�zx@���@���@�i�@�)�@��9@��7@�S�@�;d@�#�@���@��Z@���@�\)@�&@��@��L@�Xy@�7�@��@��@���@�a�@�Q�@�g�@�M�@���@���@���@��@��-@�;d@���@�^5@�-@��@���@��@���@�z�@�q@�l�@�G@���@���@��@��]@��h@�X@�%F@��@��@���@��5@���@���@�c�@��@��&@��f@��@���@��]@��@��Z@���@�?}@�@�ߤ@��?@�bN@��K@���@��$@���@�_p@�=@��p@��@�~(@�?@�*�@�
�@��@���@�s@�F�@�V@��@��@��@��z@�.�@���@�g�@�Mj@�H�@�@O@�%F@�@�@@���@��H@���@���@�YK@��@��@�@�n/@�#�@��O@��@���@�s�@�;�@�k@�@~�b@~#:@}�^@}7L@|V�@{�;@{�w@{��@{]�@{K�@{9�@z͟@z
�@y�@yf�@x�e@w��@wA�@w�@vȴ@v�x@vc @v&�@u��@t�)@tI�@t7�@t6@s��@rxl@q�)@q�@q�j@q�N@qO�@p��@pbN@p!@oiD@n�"@n��@nQ@n��@n��@n�\@n��@oP�@oA�@o�@n�2@n_�@m�@l��@l~(@l(�@k��@k�*@k|�@j�\@j=q@i��@i�^@izx@i�@h�@h�D@hM@h7@g�Q@g/�@f)�@e�@e��@ew2@eDg@e;@dی@d��@dC-@d7�@d[�@dq@d/�@c��@c&@c�@b͟@b	@a�=@ak�@a#�@`�z@_��@_��@_~�@_o@^�@]�N@]�@]e,@\��@];@\ѷ@\�z@\M@[�&@[S�@Zߤ@Z	@Y�j@Y�h@YX@X��@X�@XĜ@X��@X�z@X:�@Wخ@WH�@V�1@V@U�3@U��@Ue,@U�@TɆ@T�$@T�z@Toi@T�@S��@R�s@Q��@Q��@Q��@Q�'@Q&�@P�@PbN@PH@P%�@O��@OH�@N��@Nȴ@N�}@N��@N5?@M��@M��@MJ�@M#�@L��@K��@K��@K��@K�
@K��@KU�@K�@J��@J+k@I�@I�9@Izx@H�@H_@G��@G��@G�f@G9�@G
=@F�@F�+@E��@E�"@E2a@D�f@D��@Dl"@C��@C� @C��@CK�@CY@B��@B�@A�9@A�^@Ao @@�@@tT@@>B@@"h@@@@�@?�@?��@?=@?�@>�M@>��@>�X@>�!@>�@>��@>�r@>E�@=��@=��@=@<�@<��@<Z@<<�@;�@;�k@;n/@:J@9%@8�.@7��@7��@7t�@7U�@7=@6��@6u%@6i�@6V@64@5f�@5<6@5%F@4�K@4�o@3�}@3��@3��@3�f@3|�@3W?@3)_@2��@2B[@2�@1�@1rG@1&�@0Ɇ@0h�@0`�@0/�@/��@/O@.��@.GE@-�@,�@,��@,��@,e�@,-�@+� @+�4@+o�@+n/@+j�@+o�@+s@+e�@+$t@*�@*�L@*l�@)�@)�~@)^�@(r�@'{J@&�@&��@&��@&s�@&J�@&.�@%��@%�j@%�S@%@$�U@$�z@$|�@$,=@$�@#�@#�@#� @#��@#4�@#�@"��@"^5@!a�@ �p@ tT@  �@��@�F@�{@@͟@ȴ@��@��@u%@4@�T@�S@&�@�	@�Y@�@  @��@�W@��@{J@$t@�X@��@��@�Z@�)@�@�9@��@O�@%@�	@��@�v@�p@�9@�@�D@_@*�@��@��@��@t�@o�@qv@o�@b�@P�@/�@�@�s@�}@n�@E�@&�@�T@�N@��@�@��@2a@*0@�|@�@�D@oi@g8@e�@�A@��@s@X�@(@M�@@^�@;@�P@��@��@�K@�K@��@��@��@�u@�o@m�@`�@bN@A�@K^@�@1@�@�&@�V@Mj@@�@�R@}V@h
@i�@z@��@_�@$�@�T@�7@T�@G�@4@�@��@֡@�O@�@D�@�@��@��@e�@o@
��@
�@
��@
�A@
_�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B
� B
�B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�iB
�OB
��B
�;B
�B
�iB
��B
�iB
�OB
�OB
�iB
�B
�UB
��B
��B
��B
�oB
�UB
�UB
�oB
�oB
�oB
�UB
�UB
�oB
��B
��B
��B
��B
��B
��B
�B
�UB
�
B
��B
�B4nBo�Bs�Bq'BjKBo�BpUB�pB�XB�/B��B��B��B��B��B�QB��B�sB�KB��B�TB�6B�B�qB�B��B�sB��B�B�	B|6Ba�BK)B=�BB
�lB
�B
�oB
ɠB
�2B
��B
tnB
aHB
V�B
L�B
CaB
4B
"hB
�B	��B	��B	��B	�B	�WB	�|B	��B	��B	��B	�B	�YB	u?B	iB	`\B	U�B	?.B	)�B	$�B	]B	1B	$B	bB	B	�B		�B	YB	�B	�B	B	:B	�B	�B	
	B	�B	%B	fB	?B	SB	?B	�B�BB�LB�iB��B�$B��B�B�6B�yB�B�LB�B�bB�.B�NB�BB�~B��B�B�B	_B	-wB	,�B	}B	�B	C�B	[	B	f�B	l"B	gB	eFB	g�B	m)B	r|B	x�B	{�B	|�B	�YB	�pB	��B	��B	�B	��B	��B	��B	�B	�&B	�B	��B	�oB	~�B	|B	{JB	y�B	��B	�&B	�RB	�B	�uB	��B	��B	�RB	��B	��B	�=B	��B	�:B	��B	�B	�oB	��B	��B	�@B	�0B	�B	��B	��B	�B	�zB	�DB	��B	��B	�B	��B	��B	�OB	��B	��B	�EB	�B	�=B	�/B	��B	�QB	�#B	��B	�
B	�LB	� B	� B	�#B	��B	�)B	��B	��B	�B	��B	�vB	��B	�'B	��B	�vB	�B	�-B	��B	��B	�B	�yB	�B	�B	�)B	�B	��B	�cB	��B	��B	�B	��B	�RB	��B	�FB	��B	��B	�HB	��B	�tB	�XB	��B	�^B	��B	�B	�&B	��B	ѝB	҉B	өB	�aB	��B	ּB	ؓB	ؓB	�yB	��B	ԕB	�B	յB	�B	�dB	޸B	�CB	�B	�TB	�B	�B	�KB	�}B	�B	��B	�B	�B	�B	�B	��B	�3B	�aB	�GB	�!B	� B	�IB	�B	�CB	�CB	�wB	��B	�B	��B	�oB	�B	�B	�B	��B	��B	��B	�*B	��B	��B
�B
�B
B
�B
B
�B
�B
�B
{B
-B
�B
UB
;B
  B
B
�B
B
 �B
 �B
oB
�B
�B
�B
�B
KB
fB
�B
�B
B
 �B	��B	�HB
{B
B
uB
 �B
B
 B
MB
�B
SB
B
�B
�B
?B
+B
1B
�B
�B
	�B
)B
pB
�B
B
�B
$B
B
�B
�B
	B
�B
�B
�B
)B
]B
]B
]B
�B
/B
jB
5B
�B
B
�B
jB
~B
�B
B
�B
dB
xB
xB
xB
�B
�B
�B
!B
�B
�B
�B
 B
 �B
 �B
!B
!-B
!B
 �B
!-B
"B
"�B
!�B
!�B
"�B
$�B
%,B
%FB
%FB
%FB
$�B
$�B
$&B
$�B
$&B
#�B
$tB
$�B
%B
%�B
%�B
%zB
%�B
&�B
'8B
'RB
'B
'B
'8B
'RB
'mB
'�B
(�B
)*B
)*B
)B
)yB
)_B
)�B
+B
,"B
-CB
4nB
8RB
7�B
6�B
8�B
;B
;�B
;B
;JB
:�B
:�B
:�B
<B
;�B
;dB
;B
:�B
:xB
:�B
8�B
8�B
88B
7fB
72B
7fB
7fB
7fB
7�B
8B
8RB
8�B
8B
7fB
8B
88B
8B
9>B
:�B
9rB
8�B
8�B
:�B
<�B
<�B
=�B
>�B
>(B
?B
@�B
BuB
C�B
C�B
ESB
EmB
E�B
FtB
GEB
HKB
H�B
H�B
H�B
IlB
J�B
J�B
KDB
K)B
KB
J�B
KB
J�B
K)B
K)B
KB
KB
J�B
J�B
KDB
K�B
K�B
K�B
L0B
LJB
L�B
L�B
M�B
N"B
O(B
O(B
O(B
O(B
O�B
P.B
O�B
P.B
O�B
O�B
O�B
P�B
P�B
P�B
P�B
P�B
Q B
P�B
Q B
Q4B
Q4B
QB
QhB
RB
RTB
RoB
RoB
RoB
RTB
RTB
R�B
R�B
R�B
R�B
RoB
RTB
S�B
S�B
S�B
S�B
S�B
TaB
T�B
UgB
U�B
VmB
V�B
W$B
W�B
Y1B
Y�B
Y�B
ZB
\�B
]�B
^B
]�B
^B
^B
]/B
\�B
^5B
^jB
^�B
_!B
`'B
`\B
`vB
`�B
`�B
`�B
`�B
a|B
a�B
bhB
bNB
bB
a|B
a�B
a-B
a-B
`�B
`vB
`'B
`B
`B
abB
b4B
b�B
c:B
c�B
c�B
c�B
d@B
eB
eFB
e`B
ezB
e�B
gB
gB
gB
gB
f�B
g8B
gB
gB
h
B
hsB
h�B
hXB
hsB
h�B
h�B
g�B
f�B
f�B
fLB
ffB
ffB
f�B
gB
hXB
iyB
i*B
h�B
i*B
iyB
i�B
i�B
jKB
j�B
kQB
k�B
k�B
kkB
k�B
k�B
k�B
kQB
jB
j�B
j�B
j�B
j�B
jB
j�B
kB
l=B
l�B
m�B
n�B
o B
o5B
o5B
o�B
p;B
pUB
p�B
pUB
p�B
p�B
p�B
p�B
p�B
q'B
q[B
qvB
q�B
r-B
r�B
shB
tB
uB
uZB
u�B
vB
u�B
vFB
v`B
v�B
wB
wLB
wfB
w�B
w�B
w�B
xB
xB
w�B
xlB
x�B
xlB
x�B
x�B
x�B
x�B
x�B
y	B
y>B
yXB
yrB
yrB
yXB
yXB
yrB
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
yrB
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
{JB
{�B
|�B
|�B
|�B
|�B
|�B
|�B
}"B
}B
}B
}<B
}�B
}�B
}�B
}�B
~B
~wB
~wB
~�B
~�B
~�B
~�B
~�B
HB
cB
}B
}B
� B
�B
�B
��B
��B
��B
�B
�B
�UB
�oB
��B
�'B
�[B
�uB
��B
��B
��B
�GB
�GB
�-B
�GB
�GB
�-B
�-B
�aB
�{B
��B
��B
�gB
�gB
�3B
�9B
��B
�YB
��B
��B
��B
��B
��B
�B
�B
�_B
��B
�B
�B
�1B
��B
��B
��B
��B
��B
��B
�7B
�lB
�lB
��B
��B
��B
�B
�)B
�DB
��B
��B
�B
�JB
�dB
�JB
�dB
�~B
�B
�B
�jB
��B
��B
�pB
��B
��B
�B
��B
�(B
�\B
��B
�.B
��B
�:B
�:B
�TB
�TB
�TB
��B
�@B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
�aB
��B
��B
�B
�2B
�B
�B
�2B
�B
�2B
�gB
��B
��B
�B
�SB
�SB
��B
��B
��B
�
B
�$B
��B
�YB
��B
�B
�B
�_B
�+B
�+B
��B
��B
�B
��B
�KB
��B
�B
��B
�=B
�=B
�=B
�WB
�WB
�WB
�WB
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
�)B
��B
�)B
�]B
��B
��B
��B
�/B
�dB
�~B
��B
�~B
�dB
�~B
��B
�B
�OB
��B
��B
��B
��B
��B
�B
�B
�B
�pB
��B
�B
��B
�'B
�\B
��B
��B
��B
��B
�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B
� B
�B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�iB
�OB
��B
�;B
�B
�iB
��B
�iB
�OB
�OB
�iB
�B
�UB
��B
��B
��B
�oB
�UB
�UB
�oB
�oB
�oB
�UB
�UB
�oB
��B
��B
��B
��B
��B
��B
�B
�UB
�
B
��B
�B4nBo�Bs�Bq'BjKBo�BpUB�pB�XB�/B��B��B��B��B��B�QB��B�sB�KB��B�TB�6B�B�qB�B��B�sB��B�B�	B|6Ba�BK)B=�BB
�lB
�B
�oB
ɠB
�2B
��B
tnB
aHB
V�B
L�B
CaB
4B
"hB
�B	��B	��B	��B	�B	�WB	�|B	��B	��B	��B	�B	�YB	u?B	iB	`\B	U�B	?.B	)�B	$�B	]B	1B	$B	bB	B	�B		�B	YB	�B	�B	B	:B	�B	�B	
	B	�B	%B	fB	?B	SB	?B	�B�BB�LB�iB��B�$B��B�B�6B�yB�B�LB�B�bB�.B�NB�BB�~B��B�B�B	_B	-wB	,�B	}B	�B	C�B	[	B	f�B	l"B	gB	eFB	g�B	m)B	r|B	x�B	{�B	|�B	�YB	�pB	��B	��B	�B	��B	��B	��B	�B	�&B	�B	��B	�oB	~�B	|B	{JB	y�B	��B	�&B	�RB	�B	�uB	��B	��B	�RB	��B	��B	�=B	��B	�:B	��B	�B	�oB	��B	��B	�@B	�0B	�B	��B	��B	�B	�zB	�DB	��B	��B	�B	��B	��B	�OB	��B	��B	�EB	�B	�=B	�/B	��B	�QB	�#B	��B	�
B	�LB	� B	� B	�#B	��B	�)B	��B	��B	�B	��B	�vB	��B	�'B	��B	�vB	�B	�-B	��B	��B	�B	�yB	�B	�B	�)B	�B	��B	�cB	��B	��B	�B	��B	�RB	��B	�FB	��B	��B	�HB	��B	�tB	�XB	��B	�^B	��B	�B	�&B	��B	ѝB	҉B	өB	�aB	��B	ּB	ؓB	ؓB	�yB	��B	ԕB	�B	յB	�B	�dB	޸B	�CB	�B	�TB	�B	�B	�KB	�}B	�B	��B	�B	�B	�B	�B	��B	�3B	�aB	�GB	�!B	� B	�IB	�B	�CB	�CB	�wB	��B	�B	��B	�oB	�B	�B	�B	��B	��B	��B	�*B	��B	��B
�B
�B
B
�B
B
�B
�B
�B
{B
-B
�B
UB
;B
  B
B
�B
B
 �B
 �B
oB
�B
�B
�B
�B
KB
fB
�B
�B
B
 �B	��B	�HB
{B
B
uB
 �B
B
 B
MB
�B
SB
B
�B
�B
?B
+B
1B
�B
�B
	�B
)B
pB
�B
B
�B
$B
B
�B
�B
	B
�B
�B
�B
)B
]B
]B
]B
�B
/B
jB
5B
�B
B
�B
jB
~B
�B
B
�B
dB
xB
xB
xB
�B
�B
�B
!B
�B
�B
�B
 B
 �B
 �B
!B
!-B
!B
 �B
!-B
"B
"�B
!�B
!�B
"�B
$�B
%,B
%FB
%FB
%FB
$�B
$�B
$&B
$�B
$&B
#�B
$tB
$�B
%B
%�B
%�B
%zB
%�B
&�B
'8B
'RB
'B
'B
'8B
'RB
'mB
'�B
(�B
)*B
)*B
)B
)yB
)_B
)�B
+B
,"B
-CB
4nB
8RB
7�B
6�B
8�B
;B
;�B
;B
;JB
:�B
:�B
:�B
<B
;�B
;dB
;B
:�B
:xB
:�B
8�B
8�B
88B
7fB
72B
7fB
7fB
7fB
7�B
8B
8RB
8�B
8B
7fB
8B
88B
8B
9>B
:�B
9rB
8�B
8�B
:�B
<�B
<�B
=�B
>�B
>(B
?B
@�B
BuB
C�B
C�B
ESB
EmB
E�B
FtB
GEB
HKB
H�B
H�B
H�B
IlB
J�B
J�B
KDB
K)B
KB
J�B
KB
J�B
K)B
K)B
KB
KB
J�B
J�B
KDB
K�B
K�B
K�B
L0B
LJB
L�B
L�B
M�B
N"B
O(B
O(B
O(B
O(B
O�B
P.B
O�B
P.B
O�B
O�B
O�B
P�B
P�B
P�B
P�B
P�B
Q B
P�B
Q B
Q4B
Q4B
QB
QhB
RB
RTB
RoB
RoB
RoB
RTB
RTB
R�B
R�B
R�B
R�B
RoB
RTB
S�B
S�B
S�B
S�B
S�B
TaB
T�B
UgB
U�B
VmB
V�B
W$B
W�B
Y1B
Y�B
Y�B
ZB
\�B
]�B
^B
]�B
^B
^B
]/B
\�B
^5B
^jB
^�B
_!B
`'B
`\B
`vB
`�B
`�B
`�B
`�B
a|B
a�B
bhB
bNB
bB
a|B
a�B
a-B
a-B
`�B
`vB
`'B
`B
`B
abB
b4B
b�B
c:B
c�B
c�B
c�B
d@B
eB
eFB
e`B
ezB
e�B
gB
gB
gB
gB
f�B
g8B
gB
gB
h
B
hsB
h�B
hXB
hsB
h�B
h�B
g�B
f�B
f�B
fLB
ffB
ffB
f�B
gB
hXB
iyB
i*B
h�B
i*B
iyB
i�B
i�B
jKB
j�B
kQB
k�B
k�B
kkB
k�B
k�B
k�B
kQB
jB
j�B
j�B
j�B
j�B
jB
j�B
kB
l=B
l�B
m�B
n�B
o B
o5B
o5B
o�B
p;B
pUB
p�B
pUB
p�B
p�B
p�B
p�B
p�B
q'B
q[B
qvB
q�B
r-B
r�B
shB
tB
uB
uZB
u�B
vB
u�B
vFB
v`B
v�B
wB
wLB
wfB
w�B
w�B
w�B
xB
xB
w�B
xlB
x�B
xlB
x�B
x�B
x�B
x�B
x�B
y	B
y>B
yXB
yrB
yrB
yXB
yXB
yrB
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
yrB
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
{JB
{�B
|�B
|�B
|�B
|�B
|�B
|�B
}"B
}B
}B
}<B
}�B
}�B
}�B
}�B
~B
~wB
~wB
~�B
~�B
~�B
~�B
~�B
HB
cB
}B
}B
� B
�B
�B
��B
��B
��B
�B
�B
�UB
�oB
��B
�'B
�[B
�uB
��B
��B
��B
�GB
�GB
�-B
�GB
�GB
�-B
�-B
�aB
�{B
��B
��B
�gB
�gB
�3B
�9B
��B
�YB
��B
��B
��B
��B
��B
�B
�B
�_B
��B
�B
�B
�1B
��B
��B
��B
��B
��B
��B
�7B
�lB
�lB
��B
��B
��B
�B
�)B
�DB
��B
��B
�B
�JB
�dB
�JB
�dB
�~B
�B
�B
�jB
��B
��B
�pB
��B
��B
�B
��B
�(B
�\B
��B
�.B
��B
�:B
�:B
�TB
�TB
�TB
��B
�@B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
�aB
��B
��B
�B
�2B
�B
�B
�2B
�B
�2B
�gB
��B
��B
�B
�SB
�SB
��B
��B
��B
�
B
�$B
��B
�YB
��B
�B
�B
�_B
�+B
�+B
��B
��B
�B
��B
�KB
��B
�B
��B
�=B
�=B
�=B
�WB
�WB
�WB
�WB
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
�)B
��B
�)B
�]B
��B
��B
��B
�/B
�dB
�~B
��B
�~B
�dB
�~B
��B
�B
�OB
��B
��B
��B
��B
��B
�B
�B
�B
�pB
��B
�B
��B
�'B
�\B
��B
��B
��B
��B
�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20230703094313  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8b                                                                20230703094327  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20230703094327  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20230703094328                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20230703094328  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20230703094328  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20230703102647                      G�O�G�O�G�O�                