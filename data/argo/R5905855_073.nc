CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T19:23:33Z creation;2022-06-04T19:23:33Z conversion to V3.1      
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
_FillValue                 �  I<   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M,   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �@   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �0   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �D   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �    	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ޼   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �    HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �$   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �(   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �h   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �x   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �|   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20220604192333  20220610161505  5905855                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               IA   JA                                  2B  A   APEX                            8423                            2.11.2                          846 @�\�|�/�1   @�\��>2�@-Y�+�c�$�/1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�ffB�  B���B�  B�  B���B���B�  B�  B�33B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�ffBۙ�B�ffB�33B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C�C33C  C�fC  C�fC  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<�C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DGfDG�fDH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D���D���D�@ D�� D�� D�  D�@ D�� D��f11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @fg@|��@�ff@�ffA33A?33A_33A33A���A���A���A���Aϙ�Aߙ�AA���B��B��B��B��B'��B/��B7��B?��BG��BO��BW��B_��Bg��Bo��Bw��B��fB�L�B��fB�� B��fB��fB��3B��3B��fB��fB��B�� B��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB�L�Bۀ B�L�B��B��fB��fB��fB��fB��fB��fB��fC�3C�3C�3C�3C	�3C�3C�C&fC�3CٙC�3CٙC�3C�3C�3C�3C!�3C#�3C%�3C'�3C)�3C+�3C-�3C/�3C1�3C3�3C5�3C7�3C9�3C<�C=�3C?�3CA�3CC�3CE�3CG�3CI�3CK�3CM�3CO�3CQ�3CS�3CU�3CW�3CY�3C[�3C]�3C_�3Ca�3Cc�3Ce�3Cg�3Ci�3Ck�3Cm�3Co�3Cq�3Cs�3Cu�3Cw�3Cy�3C{�3C}�3C�3C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C�gC�gC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C�gC���C���C���D |�D ��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D	|�D	��D
|�D
��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D |�D ��D!|�D!��D"|�D"��D#|�D#��D$|�D$��D%|�D%��D&|�D&��D'|�D'��D(|�D(��D)|�D)��D*|�D*��D+|�D+��D,|�D,��D-|�D-��D.|�D.��D/|�D/��D0|�D0��D1|�D1��D2|�D2��D3|�D3��D4|�D4��D5|�D5��D6|�D6��D7|�D7��D8|�D8��D9|�D9��D:|�D:��D;|�D;��D<|�D<��D=|�D=��D>|�D>��D?|�D?��D@|�D@��DA|�DA��DB|�DB��DC|�DC��DD|�DD��DE|�DE��DF|�DG3DG�3DG��DH|�DH��DI|�DI��DJ|�DJ��DK|�DK��DL|�DL��DM|�DM��DN|�DN��DO|�DO��DP|�DP��DQ|�DQ��DR|�DR��DS|�DS��DT|�DT��DU|�DU��DV|�DV��DW|�DW��DX|�DX��DY|�DY��DZ|�DZ��D[|�D[��D\|�D\��D]|�D]��D^|�D^��D_|�D_��D`|�D`��Da|�Da��Db|�Db��Dc|�Dc��Dd|�Dd��De|�De��Df|�Df��Dg|�Dg��Dh|�Dh��Di|�Di��Dj|�Dj��Dk|�Dk��Dl|�Dl��Dm|�Dm��Dn|�Dn��Do|�Do��Dp|�Dp��Dq|�Dq��Dr|�Dr��Ds|�Ds��Dt|�Dt��Du|�Du��Dv|�Dv��Dw|�Dw��Dx|�Dx��Dy|�Dy��Dz|�Dz��D{|�D{��D||�D|��D}|�D}��D~|�D~��D|�D��D�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD���D��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD¾fD��fD�>fD�~fDþfD��fD�>fD�~fDľfD��fD�>fD�~fDžfD��fD�>fD�~fDƾfD��fD�>fD�~fDǾfD��fD�>fD�~fDȾfD��fD�>fD�~fDɾfD��fD�>fD�~fDʾfD��fD�>fD�~fD˾fD��fD�>fD�~fD̾fD��fD�>fD�~fD;fD��fD�>fD�~fDξfD��fD�>fD�~fDϾfD��fD�>fD�~fDоfD��fD�>fD�~fDѾfD��fD�>fD�~fDҾfD��fD�>fD�~fDӾfD��fD�>fD�~fDԾfD��fD�>fD�~fDվfD��fD�>fD�~fD־fD��fD�>fD�~fD׾fD��fD�>fD�~fDؾfD��fD�>fD�~fDپfD��fD�>fD�~fDھfD��fD�>fD�~fD۾fD��fD�>fD�~fDܾfD��fD�>fD�~fDݾfD��fD�>fD�~fD޾fD��fD�>fD�~fD߾fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD��fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�{3D��3D��3D�>fD�~fD��fD��fD�>fD�~fD���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�FA�{A��A��A��A��A�{A��A�A��A�hA�\A�A��A�$A�FA�JA�A��A� �A��VA���A���A��A��JA��A��AɋAɉ�A�dZA�_�A�R�A�:*A��GA���AÝA�MA�T�A�	lA��mA�'A���A��"A���A�ncA���A��A�� A�PA��A��}A��jA��oA��:A���A���A�Q�A��3A��A�S[A���A�%A�&�A�'�A��tA~�yA{AybNAuAr�uAq��An \Aj��AhPHAc�Aa��A\��AZ1'AY9XAW($AUp;ATAP�AM^5AH�jAC��AB@OA@qA?�VA>�QA>�A<�"A:G�A7!�A7�A6w�A4��A5�
A3��A.�2A+h
A'��A"��A!�bA!	A ��A +A�AbNA��A ��A ~�A �@A s�A ��A/Ac�AE�Ao�AA$�A%FAn�A�.A$tA�,A�$A�DAc�A[�A��A�A�4A}�A/�As�AQ�A�AQ�A�;A��AOAp�A?}A(�A%A�PA�xAc AHA�-A�A��A��A��A|�A*�A�AL0A��A�!A)�A��Al�A?�A($A�A��A�AZ�A �A��A%�A
�oA	�4AԕAYKA��A�kAjA��A�A�rA�AW?A�WA��A��A=qA��A�Ar�A�A q�A 6�@���@�U2@��k@�:�@��@��@�A @�s@�/�@�c @�k�@�V�@��F@��1@�F@�%@��]@��@�P�@�@�*�@��@��X@�@�tT@��
@�0�@��@�<�@��D@��@�IR@�}@��@莊@�2�@�n@�$t@��@�e@�i�@�7@���@�˒@噚@�~@�(@�&�@�^@��@�.�@�>�@��f@��H@��@���@���@ߗ$@�K�@޾�@�&�@��@ݐ�@ܺ�@�c�@��@�y�@�6z@��@��@ڊr@�E�@��>@�J#@آ4@�$@ל�@�ں@�Q@��;@�f�@Ԣ4@�u�@�%@Җ�@�l"@�6@��3@�4�@Й1@��6@�f�@��?@�;�@�\)@���@���@�u�@��@�1�@���@ʝI@�(�@ɧ�@�v`@�7L@��@��?@ȁo@�1'@Ǣ�@�x@�Q�@��@ƻ�@ƕ�@Ƅ�@�Ft@Ş�@�m]@�Z�@���@Ě�@�5?@�{J@�0�@���@�xl@�-@��@���@�C-@��9@�rG@��v@�[�@��@��"@��@��`@�l�@��@�~�@��9@���@��@��@�ں@��b@�N�@��@�T�@���@��4@�Q�@��a@��	@�*0@���@�=q@���@�rG@�*0@��@�|�@�4@��T@��@��*@��*@��k@�o�@� \@��@���@�}V@�H@��@��.@���@�خ@�c@���@�m�@���@�$t@�H@��#@�O�@�ی@��@�T�@�?}@�+�@��@���@�-@��@���@�;d@��2@���@��o@�PH@��@��z@�U�@��@��b@�h
@��@��@���@�x�@�S�@�V@���@��x@�!�@���@�v`@�g�@�X@�6z@���@���@�j@�9X@��w@�,�@��@���@�%�@���@��[@�C@��@��z@��@�m�@�#:@��m@���@�t�@�&@��4@��@�($@�خ@��F@��V@���@���@���@�<6@��@��A@�V@�e@��.@��}@���@�@��,@��L@��_@��F@�h
@�@�@��@�Vm@���@���@�H�@��7@�9�@��E@��!@���@�m�@�e�@�
�@�RT@�<6@��@���@�ff@�%�@��@��$@��@�S�@�;@���@��F@�h�@�4@���@�G�@�"�@��@�z�@� �@��r@�{J@��2@�w�@�~@���@��3@���@��@�@���@��F@��F@��\@�y>@�@�@��@���@���@��f@�X�@���@��F@�Ft@���@��h@�l�@�
=@�z�@�J�@�~@��.@��@��K@���@�qv@�Dg@�)_@��"@��@���@�=q@�u@��@�6z@� i@���@�n�@��r@�^�@�%F@��@���@�Ɇ@��@��1@�b@���@�a�@�@@�Ĝ@���@�H@��@>�@�@~��@~��@~��@~	@}s�@|�U@|g8@|C-@{�{@z��@y�#@yrG@x�K@w��@wW?@w�@v�X@vff@u�)@u�@u��@uG�@t�@tV�@s��@s�:@sC@r��@r	@q��@qq@p��@o�
@oy�@o i@n�y@n��@n�@m�#@mo @l��@l�@l��@k;d@j	@iS&@h��@h�@hU2@h-�@h1@g�w@g�k@ge�@f�m@fR�@e�S@e+@d�[@d�U@dFt@c�A@c�w@c|�@bTa@a�#@a��@a�@arG@`ѷ@`r�@`Xy@`*�@_��@^�8@^�b@]�@]�@\u�@\tT@\Xy@\7�@[�@[�P@[9�@Z��@Y�>@YrG@YJ�@X�@X�@X�.@X�@W�}@Ws@W�@V��@VR�@V �@U�=@U�@T�@T�@TK^@S�r@S�g@Sa@R�@R�'@R�1@R�A@Ra|@Q�@Q�@Q��@Qc�@Q#�@P��@PĜ@P�o@PXy@P?�@P(�@O�W@O\)@O/�@N�\@NH�@M�.@Mhs@M%@L��@L?�@L  @K�@Kx@J��@JkQ@J#:@I:�@I�@Hѷ@H�@G�}@G�@G�@F��@E��@Dz�@D1'@C�F@CC@B�h@Bu%@B-@B_@A�"@A+�@@�e@@z�@@H@@�@@�@?��@?��@?l�@?RT@?S@>{�@>R�@>@=��@=�@=f�@=+�@<��@<2�@<b@;�@;��@;\)@:͟@:�6@:��@:R�@:{@:_@:�@9��@9�d@9�@9Vm@9�@8ѷ@8>B@7ƨ@7��@7~�@7l�@7)_@6�h@6�A@6{@5��@5x�@5O�@5=�@52a@4��@4oi@4@3�@3��@3,�@2�@2�@2��@2h
@1��@1�3@1��@1m]@1-w@0�@0�@0�u@0z�@0tT@0e�@0Ft@0M@/�W@/�g@/�w@/�4@.�M@.�r@.0U@.)�@.�@-�)@-�h@-*0@,Xy@+��@+\)@+ i@*�b@*_�@*�@)�@)��@)�@)�'@)w2@)hs@)X@)IR@) \@(�`@(��@(-�@'��@'��@'P�@'A�@',�@&�M@&��@&�@%ϫ@%��@%X@%F@%0�@%�@$�p@$h�@#�@#,�@"ȴ@"p;@"!�@"u@!�H@!f�@!J�@!2a@ ��@ �@��@�m@�@��@A�@ߤ@��@;�@�H@-w@@��@I�@�}@�w@��@W?@ߤ@Z�@!�@	@��@c�@�f@�u@M@��@b�@�@�@�@n�@Q@=q@&�@ �@�j@��@�@�@�j@�T@��@�@��@Q�@8�@#�@�)@r�@�@��@l�@>�@�8@�m@��@��@��@��@��@�L@��@C�@�@��@�z@��@�C@�X@�h@|@N<@�@��@�@e�@-�@1@�g@�F@��@��@n/@b�@O@o@�@\�@{@�T@�@�>@�)@�@��@��@Y�@B�@/@�@�|@��@�9@��@`�@/�@�+@�&@ݘ@��@��@��@e�@"�@
��@
�X@
��@
�@
�@
u%@
\�@
L0@
:*@	��@	��@	�N@	�n@	w2@	L�@	(�@	%@�P@�@��@�$@��@y>@*�@(�@1'@1'@$@1@��@��@�a@��@s@n/@X�@�@11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�FA�{A��A��A��A��A�{A��A�A��A�hA�\A�A��A�$A�FA�JA�A��A� �A��VA���A���A��A��JA��A��AɋAɉ�A�dZA�_�A�R�A�:*A��GA���AÝA�MA�T�A�	lA��mA�'A���A��"A���A�ncA���A��A�� A�PA��A��}A��jA��oA��:A���A���A�Q�A��3A��A�S[A���A�%A�&�A�'�A��tA~�yA{AybNAuAr�uAq��An \Aj��AhPHAc�Aa��A\��AZ1'AY9XAW($AUp;ATAP�AM^5AH�jAC��AB@OA@qA?�VA>�QA>�A<�"A:G�A7!�A7�A6w�A4��A5�
A3��A.�2A+h
A'��A"��A!�bA!	A ��A +A�AbNA��A ��A ~�A �@A s�A ��A/Ac�AE�Ao�AA$�A%FAn�A�.A$tA�,A�$A�DAc�A[�A��A�A�4A}�A/�As�AQ�A�AQ�A�;A��AOAp�A?}A(�A%A�PA�xAc AHA�-A�A��A��A��A|�A*�A�AL0A��A�!A)�A��Al�A?�A($A�A��A�AZ�A �A��A%�A
�oA	�4AԕAYKA��A�kAjA��A�A�rA�AW?A�WA��A��A=qA��A�Ar�A�A q�A 6�@���@�U2@��k@�:�@��@��@�A @�s@�/�@�c @�k�@�V�@��F@��1@�F@�%@��]@��@�P�@�@�*�@��@��X@�@�tT@��
@�0�@��@�<�@��D@��@�IR@�}@��@莊@�2�@�n@�$t@��@�e@�i�@�7@���@�˒@噚@�~@�(@�&�@�^@��@�.�@�>�@��f@��H@��@���@���@ߗ$@�K�@޾�@�&�@��@ݐ�@ܺ�@�c�@��@�y�@�6z@��@��@ڊr@�E�@��>@�J#@آ4@�$@ל�@�ں@�Q@��;@�f�@Ԣ4@�u�@�%@Җ�@�l"@�6@��3@�4�@Й1@��6@�f�@��?@�;�@�\)@���@���@�u�@��@�1�@���@ʝI@�(�@ɧ�@�v`@�7L@��@��?@ȁo@�1'@Ǣ�@�x@�Q�@��@ƻ�@ƕ�@Ƅ�@�Ft@Ş�@�m]@�Z�@���@Ě�@�5?@�{J@�0�@���@�xl@�-@��@���@�C-@��9@�rG@��v@�[�@��@��"@��@��`@�l�@��@�~�@��9@���@��@��@�ں@��b@�N�@��@�T�@���@��4@�Q�@��a@��	@�*0@���@�=q@���@�rG@�*0@��@�|�@�4@��T@��@��*@��*@��k@�o�@� \@��@���@�}V@�H@��@��.@���@�خ@�c@���@�m�@���@�$t@�H@��#@�O�@�ی@��@�T�@�?}@�+�@��@���@�-@��@���@�;d@��2@���@��o@�PH@��@��z@�U�@��@��b@�h
@��@��@���@�x�@�S�@�V@���@��x@�!�@���@�v`@�g�@�X@�6z@���@���@�j@�9X@��w@�,�@��@���@�%�@���@��[@�C@��@��z@��@�m�@�#:@��m@���@�t�@�&@��4@��@�($@�خ@��F@��V@���@���@���@�<6@��@��A@�V@�e@��.@��}@���@�@��,@��L@��_@��F@�h
@�@�@��@�Vm@���@���@�H�@��7@�9�@��E@��!@���@�m�@�e�@�
�@�RT@�<6@��@���@�ff@�%�@��@��$@��@�S�@�;@���@��F@�h�@�4@���@�G�@�"�@��@�z�@� �@��r@�{J@��2@�w�@�~@���@��3@���@��@�@���@��F@��F@��\@�y>@�@�@��@���@���@��f@�X�@���@��F@�Ft@���@��h@�l�@�
=@�z�@�J�@�~@��.@��@��K@���@�qv@�Dg@�)_@��"@��@���@�=q@�u@��@�6z@� i@���@�n�@��r@�^�@�%F@��@���@�Ɇ@��@��1@�b@���@�a�@�@@�Ĝ@���@�H@��@>�@�@~��@~��@~��@~	@}s�@|�U@|g8@|C-@{�{@z��@y�#@yrG@x�K@w��@wW?@w�@v�X@vff@u�)@u�@u��@uG�@t�@tV�@s��@s�:@sC@r��@r	@q��@qq@p��@o�
@oy�@o i@n�y@n��@n�@m�#@mo @l��@l�@l��@k;d@j	@iS&@h��@h�@hU2@h-�@h1@g�w@g�k@ge�@f�m@fR�@e�S@e+@d�[@d�U@dFt@c�A@c�w@c|�@bTa@a�#@a��@a�@arG@`ѷ@`r�@`Xy@`*�@_��@^�8@^�b@]�@]�@\u�@\tT@\Xy@\7�@[�@[�P@[9�@Z��@Y�>@YrG@YJ�@X�@X�@X�.@X�@W�}@Ws@W�@V��@VR�@V �@U�=@U�@T�@T�@TK^@S�r@S�g@Sa@R�@R�'@R�1@R�A@Ra|@Q�@Q�@Q��@Qc�@Q#�@P��@PĜ@P�o@PXy@P?�@P(�@O�W@O\)@O/�@N�\@NH�@M�.@Mhs@M%@L��@L?�@L  @K�@Kx@J��@JkQ@J#:@I:�@I�@Hѷ@H�@G�}@G�@G�@F��@E��@Dz�@D1'@C�F@CC@B�h@Bu%@B-@B_@A�"@A+�@@�e@@z�@@H@@�@@�@?��@?��@?l�@?RT@?S@>{�@>R�@>@=��@=�@=f�@=+�@<��@<2�@<b@;�@;��@;\)@:͟@:�6@:��@:R�@:{@:_@:�@9��@9�d@9�@9Vm@9�@8ѷ@8>B@7ƨ@7��@7~�@7l�@7)_@6�h@6�A@6{@5��@5x�@5O�@5=�@52a@4��@4oi@4@3�@3��@3,�@2�@2�@2��@2h
@1��@1�3@1��@1m]@1-w@0�@0�@0�u@0z�@0tT@0e�@0Ft@0M@/�W@/�g@/�w@/�4@.�M@.�r@.0U@.)�@.�@-�)@-�h@-*0@,Xy@+��@+\)@+ i@*�b@*_�@*�@)�@)��@)�@)�'@)w2@)hs@)X@)IR@) \@(�`@(��@(-�@'��@'��@'P�@'A�@',�@&�M@&��@&�@%ϫ@%��@%X@%F@%0�@%�@$�p@$h�@#�@#,�@"ȴ@"p;@"!�@"u@!�H@!f�@!J�@!2a@ ��@ �@��@�m@�@��@A�@ߤ@��@;�@�H@-w@@��@I�@�}@�w@��@W?@ߤ@Z�@!�@	@��@c�@�f@�u@M@��@b�@�@�@�@n�@Q@=q@&�@ �@�j@��@�@�@�j@�T@��@�@��@Q�@8�@#�@�)@r�@�@��@l�@>�@�8@�m@��@��@��@��@��@�L@��@C�@�@��@�z@��@�C@�X@�h@|@N<@�@��@�@e�@-�@1@�g@�F@��@��@n/@b�@O@o@�@\�@{@�T@�@�>@�)@�@��@��@Y�@B�@/@�@�|@��@�9@��@`�@/�@�+@�&@ݘ@��@��@��@e�@"�@
��@
�X@
��@
�@
�@
u%@
\�@
L0@
:*@	��@	��@	�N@	�n@	w2@	L�@	(�@	%@�P@�@��@�$@��@y>@*�@(�@1'@1'@$@1@��@��@�a@��@s@n/@X�@�@11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B�FB�FB�FB�aB�FB�B�,B�aB�B��B�SB��B�B�
B�B�SB�WB�WB��B�B��B��B�$B��B�B�B�[B�JB�7B�=B� B	B	�B
_�B
�CB
��B'B
��B
�B
��B
��B?B
ڠB
�Bn�B��B��B�rB��Bt9B �B
�NB
��B
z^B
x�B
�LB
��B
`'B
88B
&B
qB	�wB	�2B	ǮB	��B	��B	�:B	�1B	}VB	k�B	f�B	[#B	T,B	L�B	@�B	:B	2�B	-�B	,�B	*�B	)�B	&�B	(�B	(�B	6FB	O�B	R�B	aHB	j�B	tB	{�B	��B	��B	wLB	��B	�GB	}�B	�B	x�B	N�B	+QB	vB��B��B��B��B�VB�B��B��B	xB	!HB	,�B	<�B	MB	M�B	H�B	@�B	8RB	4�B	+�B		�B	[B	QB	�B	�B	�B	�B	!-B	&�B	)�B	3�B	7�B	>(B	DB	FB	AoB	X�B	dB	|�B	z*B	wLB	~�B	B	��B	��B	��B	�tB	�7B	уB	�&B	�B	�0B	�qB	�B	��B	��B	�B	��B	�HB	�4B	�pB	�B	�zB	�,B	��B	��B	�B	�B	��B	�B	�B	�B	��B	�B	�TB	�B	�'B	�B	�&B	��B	��B	�pB	ߊB	��B	�HB	��B	��B	�bB	�HB	��B	�vB	��B	��B	��B	�B	�B	��B	߾B	��B	�jB	��B	޸B	��B	��B	ݲB	��B	��B	�;B	ߊB	ߤB	��B	�pB	�B	�HB	��B	�B	��B	� B	�NB	��B	�HB	�hB	�B	�B	�B	�|B	�B	��B	��B	��B	�B	��B	�B	��B	�tB	�@B	�&B	�tB	�tB	��B	��B	�B	��B	�B	�RB	�B	�B	�fB	�2B	�B	�B	�B	��B	�B	�B	�B	�_B	��B	��B	��B	�B	�B	��B	�GB	�B	�B	��B	��B	�B	�B	��B	�TB	�B	�aB	�'B	�iB	��B	�5B	��B	�aB	�3B	�B	�|B	�MB	��B	�GB	�B	�TB	�B	�9B	��B	�B	�B	��B	�B	�%B	��B	�B	�B	�B	�B	��B	�nB	��B	�?B	�tB	��B	��B	�FB	��B	��B	��B	�+B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�}B	�dB	��B	��B	�0B	�*B	�0B	��B	��B
�B
[B
[B
 �B	��B	��B
aB
9B
zB
KB
	RB
)B
B

XB
	�B
	�B

rB
)B

�B

rB

XB

�B

XB
	�B
	�B
	�B
	B
fB
�B
�B
fB
	�B

XB

�B
�B
�B
B
�B
B
�B
B
B
�B
�B
�B
�B
�B
B
�B
pB
VB
B
(B
VB
�B
jB
jB
jB
�B
B
<B
�B
vB
vB
vB
\B
BB
(B
(B
vB
�B
�B
�B
HB
HB
}B
�B
�B
�B
�B
�B
B
hB
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
:B
�B
TB
:B
oB
�B
�B
�B
&B
@B
�B
�B
�B
[B
{B
�B
B
{B
�B
mB
mB
�B
mB
�B
�B
�B
�B
�B
?B
?B
�B
+B
�B
_B
�B
B
�B
�B
�B
7B
�B
WB
�B
B
�B
�B
IB
/B
~B
dB
/B
~B
B
�B
�B
B
�B
�B
OB
B
�B
B
B
�B
�B
�B
5B
jB
5B
B
�B
OB
�B
�B
�B
VB
VB
�B
 \B
 �B
 �B
 �B
!�B
"�B
"NB
"4B
"B
"4B
"B
!�B
!�B
!�B
"B
"NB
"hB
"�B
"�B
#�B
#�B
#�B
$�B
%�B
%�B
&B
&2B
&fB
&fB
&�B
&�B
'B
'B
'�B
'�B
'�B
(sB
(sB
(sB
)DB
)_B
)�B
)�B
*KB
+kB
+�B
+�B
+�B
,B
+�B
+�B
,qB
,WB
,qB
+�B
+�B
+kB
+kB
+�B
+�B
+�B
,"B
,=B
,�B
.�B
.�B
/OB
.�B
/�B
0�B
0�B
1[B
1AB
1B
1vB
1�B
1�B
1�B
2aB
2�B
2�B
3�B
4B
4B
4B
3�B
4�B
5�B
5�B
5�B
5�B
6B
6+B
6�B
6�B
7fB
7�B
88B
8lB
8�B
8�B
8�B
8�B
8�B
9�B
:�B
;0B
;dB
;dB
;B
;�B
;�B
<PB
<PB
<PB
<PB
<jB
<�B
=qB
=�B
=�B
>�B
>�B
>�B
>�B
?�B
?�B
?�B
?�B
?�B
@iB
@�B
@�B
@�B
AUB
A�B
A�B
B�B
B�B
C{B
C{B
C�B
C�B
C�B
DB
DB
DgB
EB
E9B
ESB
E�B
E�B
E�B
F%B
FtB
F�B
G+B
GzB
GzB
G�B
H1B
HfB
H�B
H�B
IB
I7B
I7B
I�B
I�B
I�B
J	B
J#B
J	B
J�B
J�B
J�B
J�B
KB
K)B
K)B
K^B
K�B
K�B
KxB
K�B
LB
K�B
L�B
L�B
MB
MjB
M�B
M�B
NB
N"B
NB
N"B
N�B
N�B
N�B
O�B
O�B
O�B
PB
PHB
PbB
P�B
P�B
Q4B
R B
R:B
R�B
R�B
S&B
S&B
S&B
SB
S[B
S�B
S�B
TaB
T�B
T�B
T�B
U2B
U�B
U�B
U�B
VB
V�B
V�B
W
B
W�B
W�B
W�B
WsB
W�B
X�B
X_B
X�B
XyB
X�B
Y�B
Y�B
Y�B
ZB
ZkB
ZkB
ZkB
ZkB
Z�B
Z�B
Z�B
[=B
[=B
\B
\CB
\]B
\]B
\]B
\�B
\�B
\�B
]dB
]�B
]�B
]�B
]�B
]�B
]�B
^�B
^�B
^�B
^�B
_VB
_pB
_�B
_�B
`'B
`�B
`�B
`�B
`�B
`�B
a-B
abB
a|B
a�B
a|B
a�B
a�B
a�B
a�B
bB
bB
bNB
cB
c:B
c�B
c�B
cnB
c�B
c�B
dB
d�B
e,B
e`B
e�B
fB
fB
ffB
ffB
ffB
ffB
f�B
f�B
f�B
f�B
f�B
gB
g8B
gmB
g�B
h
B
h$B
h�B
hsB
h�B
h�B
h�B
i_B
iyB
i�B
i�B
i�B
i�B
i�B
j0B
jeB
j�B
kQB
k�B
l"B
l=B
lWB
l�B
l�B
l�B
l�B
m]B
m�B
m�B
m�B
m�B
m�B
nIB
n}B
n�B
o B
o�B
o�B
o�B
o�B
p�B
p�B
qB
q'B
q[B
q�B
raB
r|B
raB
r�B
r�B
shB
s�B
t9B
t�B
t�B
u%B
u�B
u�B
u�B
u�B
u�B
u�B
vB
v+B
v+B
v+B
v+B
v+B
v+B
vB
vB
v`B
v�B
v�B
v�B
v�B
wLB
w�B
xRB
xRB
xlB
x�B
y	B
y$B
y$B
y$B
y$B
y$B
y	B
y$B
yrB
y�B
y�B
y�B
zB
y�B
y�B
z*B
z*B
z^B
z�B
z�B
z�B
{dB
{B
{�B
{�B
{�B
{�B
{�B
|6B
|B
|6B
|6B
|�B
}"B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
~B
~BB
~]B
~�B
~�B
~�B
~�B
~�B
~�B
.B
cB
}B
}B
�B
�B
� B
�iB
��B
��B
��B
��B
��B
��B
��B
��B
�;B
�UB
�oB
��B
��B
�B
�[B
�[B
�[B
�[B
�'B
�AB
�uB
�uB
�[B
��B
��B
�B
�B
�MB
�MB
�gB
��B
�MB
�3B
��B
��B
��B
��B
�SB
��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B�FB�FB�FB�aB�FB�B�,B�aB�B��B�SB��B�B�
B�B�SB�WB�WB��B�B��B��B�$B��B�B�B�[B�JB�7B�=B� B	B	�B
_�B
�CB
��B'B
��B
�B
��B
��B?B
ڠB
�Bn�B��B��B�rB��Bt9B �B
�NB
��B
z^B
x�B
�LB
��B
`'B
88B
&B
qB	�wB	�2B	ǮB	��B	��B	�:B	�1B	}VB	k�B	f�B	[#B	T,B	L�B	@�B	:B	2�B	-�B	,�B	*�B	)�B	&�B	(�B	(�B	6FB	O�B	R�B	aHB	j�B	tB	{�B	��B	��B	wLB	��B	�GB	}�B	�B	x�B	N�B	+QB	vB��B��B��B��B�VB�B��B��B	xB	!HB	,�B	<�B	MB	M�B	H�B	@�B	8RB	4�B	+�B		�B	[B	QB	�B	�B	�B	�B	!-B	&�B	)�B	3�B	7�B	>(B	DB	FB	AoB	X�B	dB	|�B	z*B	wLB	~�B	B	��B	��B	��B	�tB	�7B	уB	�&B	�B	�0B	�qB	�B	��B	��B	�B	��B	�HB	�4B	�pB	�B	�zB	�,B	��B	��B	�B	�B	��B	�B	�B	�B	��B	�B	�TB	�B	�'B	�B	�&B	��B	��B	�pB	ߊB	��B	�HB	��B	��B	�bB	�HB	��B	�vB	��B	��B	��B	�B	�B	��B	߾B	��B	�jB	��B	޸B	��B	��B	ݲB	��B	��B	�;B	ߊB	ߤB	��B	�pB	�B	�HB	��B	�B	��B	� B	�NB	��B	�HB	�hB	�B	�B	�B	�|B	�B	��B	��B	��B	�B	��B	�B	��B	�tB	�@B	�&B	�tB	�tB	��B	��B	�B	��B	�B	�RB	�B	�B	�fB	�2B	�B	�B	�B	��B	�B	�B	�B	�_B	��B	��B	��B	�B	�B	��B	�GB	�B	�B	��B	��B	�B	�B	��B	�TB	�B	�aB	�'B	�iB	��B	�5B	��B	�aB	�3B	�B	�|B	�MB	��B	�GB	�B	�TB	�B	�9B	��B	�B	�B	��B	�B	�%B	��B	�B	�B	�B	�B	��B	�nB	��B	�?B	�tB	��B	��B	�FB	��B	��B	��B	�+B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�}B	�dB	��B	��B	�0B	�*B	�0B	��B	��B
�B
[B
[B
 �B	��B	��B
aB
9B
zB
KB
	RB
)B
B

XB
	�B
	�B

rB
)B

�B

rB

XB

�B

XB
	�B
	�B
	�B
	B
fB
�B
�B
fB
	�B

XB

�B
�B
�B
B
�B
B
�B
B
B
�B
�B
�B
�B
�B
B
�B
pB
VB
B
(B
VB
�B
jB
jB
jB
�B
B
<B
�B
vB
vB
vB
\B
BB
(B
(B
vB
�B
�B
�B
HB
HB
}B
�B
�B
�B
�B
�B
B
hB
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
:B
�B
TB
:B
oB
�B
�B
�B
&B
@B
�B
�B
�B
[B
{B
�B
B
{B
�B
mB
mB
�B
mB
�B
�B
�B
�B
�B
?B
?B
�B
+B
�B
_B
�B
B
�B
�B
�B
7B
�B
WB
�B
B
�B
�B
IB
/B
~B
dB
/B
~B
B
�B
�B
B
�B
�B
OB
B
�B
B
B
�B
�B
�B
5B
jB
5B
B
�B
OB
�B
�B
�B
VB
VB
�B
 \B
 �B
 �B
 �B
!�B
"�B
"NB
"4B
"B
"4B
"B
!�B
!�B
!�B
"B
"NB
"hB
"�B
"�B
#�B
#�B
#�B
$�B
%�B
%�B
&B
&2B
&fB
&fB
&�B
&�B
'B
'B
'�B
'�B
'�B
(sB
(sB
(sB
)DB
)_B
)�B
)�B
*KB
+kB
+�B
+�B
+�B
,B
+�B
+�B
,qB
,WB
,qB
+�B
+�B
+kB
+kB
+�B
+�B
+�B
,"B
,=B
,�B
.�B
.�B
/OB
.�B
/�B
0�B
0�B
1[B
1AB
1B
1vB
1�B
1�B
1�B
2aB
2�B
2�B
3�B
4B
4B
4B
3�B
4�B
5�B
5�B
5�B
5�B
6B
6+B
6�B
6�B
7fB
7�B
88B
8lB
8�B
8�B
8�B
8�B
8�B
9�B
:�B
;0B
;dB
;dB
;B
;�B
;�B
<PB
<PB
<PB
<PB
<jB
<�B
=qB
=�B
=�B
>�B
>�B
>�B
>�B
?�B
?�B
?�B
?�B
?�B
@iB
@�B
@�B
@�B
AUB
A�B
A�B
B�B
B�B
C{B
C{B
C�B
C�B
C�B
DB
DB
DgB
EB
E9B
ESB
E�B
E�B
E�B
F%B
FtB
F�B
G+B
GzB
GzB
G�B
H1B
HfB
H�B
H�B
IB
I7B
I7B
I�B
I�B
I�B
J	B
J#B
J	B
J�B
J�B
J�B
J�B
KB
K)B
K)B
K^B
K�B
K�B
KxB
K�B
LB
K�B
L�B
L�B
MB
MjB
M�B
M�B
NB
N"B
NB
N"B
N�B
N�B
N�B
O�B
O�B
O�B
PB
PHB
PbB
P�B
P�B
Q4B
R B
R:B
R�B
R�B
S&B
S&B
S&B
SB
S[B
S�B
S�B
TaB
T�B
T�B
T�B
U2B
U�B
U�B
U�B
VB
V�B
V�B
W
B
W�B
W�B
W�B
WsB
W�B
X�B
X_B
X�B
XyB
X�B
Y�B
Y�B
Y�B
ZB
ZkB
ZkB
ZkB
ZkB
Z�B
Z�B
Z�B
[=B
[=B
\B
\CB
\]B
\]B
\]B
\�B
\�B
\�B
]dB
]�B
]�B
]�B
]�B
]�B
]�B
^�B
^�B
^�B
^�B
_VB
_pB
_�B
_�B
`'B
`�B
`�B
`�B
`�B
`�B
a-B
abB
a|B
a�B
a|B
a�B
a�B
a�B
a�B
bB
bB
bNB
cB
c:B
c�B
c�B
cnB
c�B
c�B
dB
d�B
e,B
e`B
e�B
fB
fB
ffB
ffB
ffB
ffB
f�B
f�B
f�B
f�B
f�B
gB
g8B
gmB
g�B
h
B
h$B
h�B
hsB
h�B
h�B
h�B
i_B
iyB
i�B
i�B
i�B
i�B
i�B
j0B
jeB
j�B
kQB
k�B
l"B
l=B
lWB
l�B
l�B
l�B
l�B
m]B
m�B
m�B
m�B
m�B
m�B
nIB
n}B
n�B
o B
o�B
o�B
o�B
o�B
p�B
p�B
qB
q'B
q[B
q�B
raB
r|B
raB
r�B
r�B
shB
s�B
t9B
t�B
t�B
u%B
u�B
u�B
u�B
u�B
u�B
u�B
vB
v+B
v+B
v+B
v+B
v+B
v+B
vB
vB
v`B
v�B
v�B
v�B
v�B
wLB
w�B
xRB
xRB
xlB
x�B
y	B
y$B
y$B
y$B
y$B
y$B
y	B
y$B
yrB
y�B
y�B
y�B
zB
y�B
y�B
z*B
z*B
z^B
z�B
z�B
z�B
{dB
{B
{�B
{�B
{�B
{�B
{�B
|6B
|B
|6B
|6B
|�B
}"B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
~B
~BB
~]B
~�B
~�B
~�B
~�B
~�B
~�B
.B
cB
}B
}B
�B
�B
� B
�iB
��B
��B
��B
��B
��B
��B
��B
��B
�;B
�UB
�oB
��B
��B
�B
�[B
�[B
�[B
�[B
�'B
�AB
�uB
�uB
�[B
��B
��B
�B
�B
�MB
�MB
�gB
��B
�MB
�3B
��B
��B
��B
��B
�SB
��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604105242  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604192333  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604192333  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604192333                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605042338  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605042338  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610161505                      G�O�G�O�G�O�                