CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T17:36:30Z creation;2022-06-04T17:36:30Z conversion to V3.1      
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
_FillValue                    �tArgo profile    3.1 1.2 19500101000000  20220604173630  20220610131507  5905853                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               MA   JA                                  2B  A   APEX                            8421                            2.11.2                          846 @�eϜ��1   @�e�A��@0�p��
=�c�ě��T1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @��@�  @�  A   A   A@  A`  A�  A�  A���A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�ffB�ffB���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B̙�B�  B�33B�33B�  B�  B�  B�33B뙚B���B���B�  B�  C   C  C  C�fC  C
  C  C  C  C�C�C  C�C�fC�fC  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4ffC5�fC7�fC:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX�CZ33C\  C]�fC_�fCb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  Dy�D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&�fD'fD'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� DufDu�fDv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�y�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @fg@|��@�ff@�ffA33A?33A_33A33A���A�fgA���A���Aϙ�Aߙ�AA���B��B��B��B��B'��B/��B7��B?��BG��BO��BW��B_��Bg��Bo��Bw��B�L�B�L�B��3B��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��B̀ B��fB��B��B��fB��fB��fB��B� B�3B�3B��fB��fB��fC�3C�3CٙC�3C	�3C�3C�3C�3C�C�C�3C�CٙCٙC�3C�3C!�3C#�3C%�3C'�3C)�3C+�3C-�3C/�3C1�3C4Y�C5ٙC7ٙC9�3C;�3C=�3C?�3CA�3CC�3CE�3CG�3CI�3CK�3CM�3CO�3CQ�3CS�3CU�3CX�CZ&fC[�3C]ٙC_ٙCa�3Cc�3Ce�3Cg�3Ci�3Ck�3Cm�3Co�3Cq�3Cs�3Cu�3Cw�3Cy�3C{�3C}�3C�3C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C�gC�gC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C�gC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C�gC���C���C���C���C���C���C���C���C���C���C���C�gC�gC���C���C���C���C���C���C���C���C���C���C���C���C���C���D |�D ��D|�D��D|�D��D|�D��D|�D��DvgD��D|�D��D|�D��D|�D��D	|�D	��D
|�D
��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D |�D ��D!|�D!��D"|�D"��D#|�D#��D$|�D$��D%|�D%��D&�3D'3D'|�D'��D(|�D(��D)|�D)��D*|�D*��D+|�D+��D,|�D,��D-|�D-��D.|�D.��D/|�D/��D0|�D0��D1|�D1��D2|�D2��D3|�D3��D4|�D4��D5|�D5��D6|�D6��D7|�D7��D8|�D8��D9|�D9��D:|�D:��D;|�D;��D<|�D<��D=|�D=��D>|�D>��D?|�D?��D@|�D@��DA|�DA��DB|�DB��DC|�DC��DD|�DD��DE|�DE��DF|�DF��DG|�DG��DH|�DH��DI|�DI��DJ|�DJ��DK|�DK��DL|�DL��DM|�DM��DN|�DN��DO|�DO��DP|�DP��DQ|�DQ��DR|�DR��DS|�DS��DT|�DT��DU|�DU��DV|�DV��DW|�DW��DX|�DX��DY|�DY��DZ|�DZ��D[|�D[��D\|�D\��D]|�D]��D^|�D^��D_|�D_��D`|�D`��Da|�Da��Db|�Db��Dc|�Dc��Dd|�Dd��De|�De��Df|�Df��Dg|�Dg��Dh|�Dh��Di|�Di��Dj|�Dj��Dk|�Dk��Dl|�Dl��Dm|�Dm��Dn|�Dn��Do|�Do��Dp|�Dp��Dq|�Dq��Dr|�Dr��Ds|�Ds��Dt|�Du3Du�3Du��Dv|�Dv��Dw|�Dw��Dx|�Dx��Dy|�Dy��Dz|�Dz��D{|�D{��D||�D|��D}|�D}��D~|�D~��D|�D��D�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�A�D�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD���D��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD���D��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD���D��fD��fD�>fD�~fD��fD��fD�>fD�~fD¾fD��fD�>fD�~fDþfD��fD�>fD�~fDľfD��fD�>fD�~fDžfD��fD�>fD�~fDƾfD��fD�>fD�~fDǾfD��fD�>fD�~fDȾfD��fD�>fD�~fDɾfD��fD�>fD�~fDʾfD��fD�>fD�~fD˾fD��fD�>fD�~fD̾fD��fD�>fD�~fD;fD��fD�>fD�~fDξfD��fD�>fD�~fDϾfD��fD�>fD�~fDоfD��fD�>fD�~fDѾfD��fD�>fD�~fDҾfD��fD�>fD�~fDӾfD��fD�>fD�~fDԾfD��fD�>fD�~fDվfD��fD�>fD�~fD־fD��fD�>fD�~fD׾fD��fD�>fD�~fDؾfD��fD�>fD�~fDپfD��fD�>fD�~fDھfD��fD�>fD�~fD۾fD��fD�>fD�~fDܾfD��fD�>fD�~fDݾfD��fD�>fD�~fD޾fD��fD�>fD�~fD߾fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD��fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD��fD��fD�;3D�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�x 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A̴�A̳�A�5tA��AȖ�A�lWA�Aǿ�Aǌ�A�YA�4�A�$�A�IA�A�	A���A���A��A��2A��A���AƸ�AƳ3AƬqAƥFAƦLAƤtAƤtAƧAƯOAƼjA��gAƵAƩ�AƝ~AƚAƃ�A�#nA��'A���A�ߤA��A���A��A���A��AŨXA�VA��KA�[#A�>BA�)�A��,A��A�`BA���A��'A�fA�T�A�'A���A��2A�-�A��|A�GEA��A��AA�<jA�˒A�Z�A��A�A�n/A�.}A��\A���A�g8A��A��)A��jA��'A��A��YA�� A���A��cA��A�IA~N<Av�Av�Aj)�Ad�A`��A_��A^c A\6�AYDgAT	AQVmAO?}AMAJ:�AG��AEیAC��A@�A@9�A?X�A8�A7�A6$A4(A3�A2��A1K�A.�aA-g�A,Z�A)��A&��A%��A'm�A(�nA**�A*S&A)��A(��A'�hA(�A)�TA*cA*2�A)�A(K�A'�&A'�FA'�A'U2A'��A'ѷA'<6A&�A&�QA'!A%ߤA%��A%-wA$/�A#�A#�A#_A"�YA"	A!��A Z�A�A�pAR�A�)AJ�A4�A��A�NA�AԕAOAzAk�A�;AVmAA�rA�mA�A�A��A+kA��A��A��A-�A�A�zAqvA�A�HA�MAZ�A�A��A�oA�,A��A\�A�Ae�A�A��A=A
�A��A�AѷA{�A-wA��A��A~�A5�A_A�)A�XAffA�AĜA6zA
��A
}VA
l�A
W?A	�jA	b�A	�A		A�A��AVAFA(A�A�A�A}VARTA9XA��A�zA��A�A��A�A��A��A/Ae,A�bA ��A �A [WA S@�2a@���@��9@��.@�U2@��@��n@��@�v�@��@�A�@��f@���@��@��1@�Ov@��@�e�@�9�@�%@���@�~�@�?@���@�n@�e�@�e@�=@�#�@���@���@�n@�-w@��@��)@�I�@��q@�U2@��}@�hs@���@��@�o@�K^@�ϫ@�@@�@�;�@�-�@���@㹌@��U@�r@�o@�)�@��@�!@ߩ*@�n/@�Mj@�33@�C@ޭ�@��d@ݒ:@�l�@�e,@�U�@��@�	@��p@��@�O@נ'@�*0@��+@�z�@��m@Ӥ@@��@�u�@��]@ї�@�>B@Ξ�@�n�@ͷ�@̇+@��@��Q@�!-@ʑ�@�\�@�@ɼ@�S&@��@�W�@�_@��d@�|@�S@��@ƾ@ƦL@��@�)_@�8�@�j@�+@��E@���@���@��@�ں@µ@\@�GE@��;@���@�#�@��p@���@�Q@���@�t�@�iD@�U�@�@�@���@�ں@��f@��P@�V@���@��F@�˒@�"�@���@��@���@��9@�=q@�(�@���@���@��V@���@�:*@��z@�7L@�֡@�L0@�C�@�%�@��@��@�:�@��#@�|@���@�D�@��W@��k@�p�@�B�@��@��X@���@�j@��@��C@���@�_p@���@�ѷ@��U@�j@��@�4�@���@�xl@�0U@���@���@��B@���@�u@���@��V@�|@��M@���@�q�@�?@�9X@�>B@�@�@�B[@�@�@�1'@��@��g@�Mj@�ȴ@��r@�V@�.�@��@�?�@�x@���@�s@�Y�@��B@�YK@�x@��;@��d@���@� \@��s@��U@��9@���@�2�@���@�m]@�#�@��@���@��D@�C-@���@���@��f@�e�@�@O@��B@��@���@��_@�h�@�6@�_@��@�l�@�e�@�Vm@�A @��@�֡@���@�j@�Ft@�&�@���@�g�@��@��h@�ݘ@���@�L�@���@��?@���@�<�@���@��"@�A�@���@��?@�s�@�@��m@�@O@���@���@�Ft@��@���@�Z�@��@��/@���@�e�@�?�@���@�B�@��@���@��D@���@�x�@�\)@�Y@���@�u�@�V@�GE@�0U@�	@���@�)_@��@�Ĝ@�]d@�@���@�9�@�	l@��@��)@��A@�Z@�4n@��@�	�@��Z@��C@�p�@��H@��$@���@��o@�g8@�1�@��@�
�@��#@��@�C@��1@�w�@�C�@�(�@��#@���@���@���@�}�@�33@��v@���@�Z�@�	@�@�O�@��@��2@�͟@��!@���@�\�@�#:@��@���@�u�@�W?@�S�@�;d@��@���@�1�@��@��X@�IR@��@�ں@���@���@�Xy@��@�m@��@��@��@~�R@}c�@}%@|�@|oi@|e�@|V�@|�@{{J@{�@z�,@zi�@y��@yN<@y�@x�O@x	�@w��@w��@w��@wX�@v�X@v�R@vW�@u��@u��@u5�@t��@tl"@tH@tb@s�m@s��@ss@r��@r-@q��@q!�@p(�@o@O@n��@nn�@n3�@m�n@l��@l|�@k��@kS@j��@j.�@i�N@i��@i��@iX@h��@h�e@h��@h�.@h/�@g�a@g'�@fkQ@e��@e&�@d�@b��@b�R@b��@b�\@ba|@b-@bu@a@a��@a|@aVm@`�)@`-�@_��@_�	@^�<@]m]@\�/@\�4@\$@[�@[��@[�&@[�w@[K�@[
=@Z��@Z��@Zxl@Z=q@Z3�@Z	@Z	@Y��@Y@X�v@X�j@X�@X[�@Xb@W��@W��@W\)@WE9@WA�@W9�@W)_@V�"@V�F@V5?@V4@U@U|@UVm@T�@T>B@S��@S�*@S��@So�@S!-@R��@R�L@RB[@Q��@P~(@O��@O�P@OdZ@OJ#@N�m@N4@L��@K��@K��@K�@J�A@Ju%@I�@H�f@H~(@H!@G|�@G@F�B@F��@F�r@FQ@Ec�@E0�@E%@D�P@D�@D��@Dg8@C�F@C��@C�P@CJ#@B��@Ba|@B@A�9@A��@AL�@@Ĝ@@�@@D�@@'R@?�@?�Q@?ƨ@?��@?1�@>:*@=Y�@=+�@=�@<��@<��@<��@<>B@;��@;_p@:�c@:�F@:^5@:�@9�@8�?@8�Y@8c�@8Q�@8@7��@7�*@7�4@7K�@7
=@6kQ@5@5J�@5�@4�`@4u�@47�@4M@3��@3��@3s@39�@3�@2�y@2��@2($@1��@1��@1[W@0Ɇ@0z�@0]d@0A�@0(�@0G@/خ@/a@.n�@.?@.�@-�@-+�@-@@,�	@,�v@,�@,�D@,"h@,�@+�m@+��@++@*��@*�s@*�<@*�}@*��@*^5@)��@)0�@(ѷ@(�4@(�@(�@(m�@(C-@'��@&�m@&W�@&5?@&_@%�T@%��@%<6@$�	@$�U@$��@$g8@$$@#ݘ@#�K@#��@#��@#�@@#s@#S�@#)_@#�@"�@"�H@"��@"�r@"_@!�@!rG@!F@!�@ ]d@ G@�@�w@��@�V@~�@�@�R@Ta@ԕ@�~@�@j@>B@,=@�@	�@��@��@_p@�@z@1�@�@�#@�#@�j@��@��@�@�@�n@7L@�@��@V�@�@� @�:@y�@v`@Mj@33@!-@�@�X@Ta@?@�@�@u@��@�o@�H@�7@S&@7L@0�@@�@%@�@�@�[@�@~(@"h@�6@�P@�f@x@@O@�@�@�@��@��@v�@R�@($@��@�H@�C@T�@=�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A̴�A̳�A�5tA��AȖ�A�lWA�Aǿ�Aǌ�A�YA�4�A�$�A�IA�A�	A���A���A��A��2A��A���AƸ�AƳ3AƬqAƥFAƦLAƤtAƤtAƧAƯOAƼjA��gAƵAƩ�AƝ~AƚAƃ�A�#nA��'A���A�ߤA��A���A��A���A��AŨXA�VA��KA�[#A�>BA�)�A��,A��A�`BA���A��'A�fA�T�A�'A���A��2A�-�A��|A�GEA��A��AA�<jA�˒A�Z�A��A�A�n/A�.}A��\A���A�g8A��A��)A��jA��'A��A��YA�� A���A��cA��A�IA~N<Av�Av�Aj)�Ad�A`��A_��A^c A\6�AYDgAT	AQVmAO?}AMAJ:�AG��AEیAC��A@�A@9�A?X�A8�A7�A6$A4(A3�A2��A1K�A.�aA-g�A,Z�A)��A&��A%��A'm�A(�nA**�A*S&A)��A(��A'�hA(�A)�TA*cA*2�A)�A(K�A'�&A'�FA'�A'U2A'��A'ѷA'<6A&�A&�QA'!A%ߤA%��A%-wA$/�A#�A#�A#_A"�YA"	A!��A Z�A�A�pAR�A�)AJ�A4�A��A�NA�AԕAOAzAk�A�;AVmAA�rA�mA�A�A��A+kA��A��A��A-�A�A�zAqvA�A�HA�MAZ�A�A��A�oA�,A��A\�A�Ae�A�A��A=A
�A��A�AѷA{�A-wA��A��A~�A5�A_A�)A�XAffA�AĜA6zA
��A
}VA
l�A
W?A	�jA	b�A	�A		A�A��AVAFA(A�A�A�A}VARTA9XA��A�zA��A�A��A�A��A��A/Ae,A�bA ��A �A [WA S@�2a@���@��9@��.@�U2@��@��n@��@�v�@��@�A�@��f@���@��@��1@�Ov@��@�e�@�9�@�%@���@�~�@�?@���@�n@�e�@�e@�=@�#�@���@���@�n@�-w@��@��)@�I�@��q@�U2@��}@�hs@���@��@�o@�K^@�ϫ@�@@�@�;�@�-�@���@㹌@��U@�r@�o@�)�@��@�!@ߩ*@�n/@�Mj@�33@�C@ޭ�@��d@ݒ:@�l�@�e,@�U�@��@�	@��p@��@�O@נ'@�*0@��+@�z�@��m@Ӥ@@��@�u�@��]@ї�@�>B@Ξ�@�n�@ͷ�@̇+@��@��Q@�!-@ʑ�@�\�@�@ɼ@�S&@��@�W�@�_@��d@�|@�S@��@ƾ@ƦL@��@�)_@�8�@�j@�+@��E@���@���@��@�ں@µ@\@�GE@��;@���@�#�@��p@���@�Q@���@�t�@�iD@�U�@�@�@���@�ں@��f@��P@�V@���@��F@�˒@�"�@���@��@���@��9@�=q@�(�@���@���@��V@���@�:*@��z@�7L@�֡@�L0@�C�@�%�@��@��@�:�@��#@�|@���@�D�@��W@��k@�p�@�B�@��@��X@���@�j@��@��C@���@�_p@���@�ѷ@��U@�j@��@�4�@���@�xl@�0U@���@���@��B@���@�u@���@��V@�|@��M@���@�q�@�?@�9X@�>B@�@�@�B[@�@�@�1'@��@��g@�Mj@�ȴ@��r@�V@�.�@��@�?�@�x@���@�s@�Y�@��B@�YK@�x@��;@��d@���@� \@��s@��U@��9@���@�2�@���@�m]@�#�@��@���@��D@�C-@���@���@��f@�e�@�@O@��B@��@���@��_@�h�@�6@�_@��@�l�@�e�@�Vm@�A @��@�֡@���@�j@�Ft@�&�@���@�g�@��@��h@�ݘ@���@�L�@���@��?@���@�<�@���@��"@�A�@���@��?@�s�@�@��m@�@O@���@���@�Ft@��@���@�Z�@��@��/@���@�e�@�?�@���@�B�@��@���@��D@���@�x�@�\)@�Y@���@�u�@�V@�GE@�0U@�	@���@�)_@��@�Ĝ@�]d@�@���@�9�@�	l@��@��)@��A@�Z@�4n@��@�	�@��Z@��C@�p�@��H@��$@���@��o@�g8@�1�@��@�
�@��#@��@�C@��1@�w�@�C�@�(�@��#@���@���@���@�}�@�33@��v@���@�Z�@�	@�@�O�@��@��2@�͟@��!@���@�\�@�#:@��@���@�u�@�W?@�S�@�;d@��@���@�1�@��@��X@�IR@��@�ں@���@���@�Xy@��@�m@��@��@��@~�R@}c�@}%@|�@|oi@|e�@|V�@|�@{{J@{�@z�,@zi�@y��@yN<@y�@x�O@x	�@w��@w��@w��@wX�@v�X@v�R@vW�@u��@u��@u5�@t��@tl"@tH@tb@s�m@s��@ss@r��@r-@q��@q!�@p(�@o@O@n��@nn�@n3�@m�n@l��@l|�@k��@kS@j��@j.�@i�N@i��@i��@iX@h��@h�e@h��@h�.@h/�@g�a@g'�@fkQ@e��@e&�@d�@b��@b�R@b��@b�\@ba|@b-@bu@a@a��@a|@aVm@`�)@`-�@_��@_�	@^�<@]m]@\�/@\�4@\$@[�@[��@[�&@[�w@[K�@[
=@Z��@Z��@Zxl@Z=q@Z3�@Z	@Z	@Y��@Y@X�v@X�j@X�@X[�@Xb@W��@W��@W\)@WE9@WA�@W9�@W)_@V�"@V�F@V5?@V4@U@U|@UVm@T�@T>B@S��@S�*@S��@So�@S!-@R��@R�L@RB[@Q��@P~(@O��@O�P@OdZ@OJ#@N�m@N4@L��@K��@K��@K�@J�A@Ju%@I�@H�f@H~(@H!@G|�@G@F�B@F��@F�r@FQ@Ec�@E0�@E%@D�P@D�@D��@Dg8@C�F@C��@C�P@CJ#@B��@Ba|@B@A�9@A��@AL�@@Ĝ@@�@@D�@@'R@?�@?�Q@?ƨ@?��@?1�@>:*@=Y�@=+�@=�@<��@<��@<��@<>B@;��@;_p@:�c@:�F@:^5@:�@9�@8�?@8�Y@8c�@8Q�@8@7��@7�*@7�4@7K�@7
=@6kQ@5@5J�@5�@4�`@4u�@47�@4M@3��@3��@3s@39�@3�@2�y@2��@2($@1��@1��@1[W@0Ɇ@0z�@0]d@0A�@0(�@0G@/خ@/a@.n�@.?@.�@-�@-+�@-@@,�	@,�v@,�@,�D@,"h@,�@+�m@+��@++@*��@*�s@*�<@*�}@*��@*^5@)��@)0�@(ѷ@(�4@(�@(�@(m�@(C-@'��@&�m@&W�@&5?@&_@%�T@%��@%<6@$�	@$�U@$��@$g8@$$@#ݘ@#�K@#��@#��@#�@@#s@#S�@#)_@#�@"�@"�H@"��@"�r@"_@!�@!rG@!F@!�@ ]d@ G@�@�w@��@�V@~�@�@�R@Ta@ԕ@�~@�@j@>B@,=@�@	�@��@��@_p@�@z@1�@�@�#@�#@�j@��@��@�@�@�n@7L@�@��@V�@�@� @�:@y�@v`@Mj@33@!-@�@�X@Ta@?@�@�@u@��@�o@�H@�7@S&@7L@0�@@�@%@�@�@�[@�@~(@"h@�6@�P@�f@x@@O@�@�@�@��@��@v�@R�@($@��@�H@�C@T�@=�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B��B��B�NB��B�3B��B�-B��B�OB~�B~BB~]B~wB~BB~(BB�B~�B~�B~�B�iB��B��B�B��B�-B�gB�tB��B��B�EB�[B��B�NB	�B	��B
uB
.B
?�B
Q�B
j�B
q�B
�-B
��B
�NB
�mB
�UB
�B
ƎB
�&B
�'B
�B
�TB
��B5B3�BNVB}VB�6B��Bm�BsMBoBrGBwLBt�B-�BaB
��B
��B
�B
��B
�	B
~(B
u%B
S�B
%�B
�B
/B
 �B
�B
�B
"�B
\B	��B
�B	��B	�'B	��B	��B	�eB	n�B	S�B	D�B	>�B	9$B	1�B	*B	B	�B	B	�B	B	0B	9B	�B	�B	�B�B�)B��B�
B��B�`B��B��B�LB�nB�MB�"B�bB�IB	$tB	K�B	x�B	��B	��B	��B	��B	�B	�MB	�"B	��B	�zB	��B	�B	��B	�XB	�B
 �B
�B
�B
�B
!bB
4TB
4�B
72B
=B
CGB
B'B
A�B
A�B
?cB
?cB
G+B
=VB
6�B
:B
2|B
,�B
.IB
/�B
=�B
E�B
L�B
O�B
L�B
H�B
K�B
I�B
G�B
G�B
GzB
G�B
HfB
H�B
H�B
J�B
K�B
KxB
L�B
M�B
NVB
N"B
N�B
O�B
P�B
P�B
P}B
P�B
PHB
PB
PB
PHB
OvB
O�B
MB
MB
K�B
KxB
J�B
I�B
HfB
G�B
F?B
ESB
CGB
?�B
?.B
>B
=B
;�B
<jB
=qB
=�B
=�B
=qB
<6B
;�B
;dB
:�B
;0B
:B
9>B
8lB
8�B
7fB
5�B
5tB
5�B
4�B
4�B
4B
4B
3hB
3B
3B
2-B
1�B
1AB
0UB
-�B
-)B
,"B
*�B
)�B
(sB
&�B
&B
%zB
%�B
%,B
$�B
$@B
#�B
#TB
#B
#:B
"�B
!�B
!�B
!�B
 'B
�B
�B
�B
IB
�B
)B
�B
�B
WB
�B
7B
�B
�B
EB
�B
gB
�B
@B
�B
B
�B
�B
B
oB
 B
�B
�B
�B
�B

�B

#B
	�B
�B
�B
�B
�B
 4B	��B	�XB	�`B	��B	�%B	�ZB	��B	�TB	��B	�B	��B	��B	�B	��B	�+B	�+B	�`B	�FB	�`B	�`B	��B	�B	�B	�B	�)B	�B	�qB	��B	�B	�WB	�B	��B	�CB	�CB	��B	�B	�;B	�B	��B	�oB	�B	��B	��B	��B	��B	�B	��B	�B	��B	��B	�B	�B	��B	��B	�ZB	��B	��B	��B	�B	�B	��B	�B	�9B	��B	�B	�B	�9B	�B	�B	�nB	��B	�B	�+B	��B	�2B	��B	��B	�2B	�LB	�lB	��B	�B
 OB
 iB
B
�B
�B
%B
�B
�B
�B
�B
�B
�B
B
�B
�B
�B
%B
tB
?B
B
�B
�B
�B
B
�B
�B
�B
�B
 �B
B
  B	�.B	�(B	�B	��B	��B	�qB	�qB	�B	�<B	��B	�HB
  B	��B
�B
 B
UB
�B
B
�B
B
mB
�B
+B
�B
B
fB
�B
�B
�B
	�B
^B
�B
�B
�B
VB
B
VB
<B
pB
BB
B
�B
hB
�B
4B
�B
�B
�B
B
9B
B
SB
MB
B
&B
&B
@B
oB
�B
B
�B
�B
NB
�B
�B
�B
�B
:B
�B
�B
�B
�B
}B
 B
:B
�B
�B
�B
�B
@B
B
�B
�B
@B
@B
�B
aB
�B
�B
�B
�B
�B
?B
�B
sB
�B
�B
$B
�B
gB
gB
�B
gB
�B
B
�B
�B
�B
+B
�B
EB
�B
�B
?B
�B
QB
qB
B
�B
�B
�B
WB
qB
�B
�B
#B
7B
�B
B
�B
�B
qB
�B
�B
qB
�B
�B
B
�B
WB
�B
]B
�B
�B
�B
OB
�B
pB
�B
B
�B
VB
�B
 \B
!B
!-B
"NB
#:B
#�B
$@B
%,B
%FB
$�B
$�B
%FB
%�B
%`B
&�B
'�B
'�B
(�B
(�B
(�B
)*B
(�B
)DB
(�B
)B
)B
(�B
)*B
(�B
)DB
)�B
)�B
*KB
+B
*�B
*�B
+B
+B
+B
+�B
+�B
,B
,�B
-]B
-]B
,�B
-)B
-�B
.IB
/OB
/iB
/ B
/5B
/�B
0�B
0�B
1B
0�B
1�B
1�B
1�B
1�B
1�B
2�B
3�B
3�B
4B
4B
4B
4B
4nB
5ZB
5tB
5ZB
5�B
5�B
6`B
6zB
6FB
6�B
6zB
6�B
6�B
6�B
6�B
6�B
6�B
72B
6zB
6�B
6�B
6�B
6`B
7�B
72B
7fB
7�B
8RB
8�B
8RB
8�B
8�B
9�B
:�B
9XB
9�B
:B
;B
;B
<B
<�B
="B
=<B
=VB
=<B
=B
=�B
=�B
>]B
>BB
>]B
>�B
?B
>�B
>�B
>BB
>B
>B
>BB
>wB
>�B
?HB
?�B
@ B
@ B
@�B
@iB
A;B
A;B
@�B
@�B
@�B
@�B
@�B
?�B
>wB
>�B
=�B
>]B
>]B
>�B
>wB
>B
>wB
?.B
>�B
>wB
?HB
?�B
@B
?�B
@iB
AoB
A�B
AUB
@�B
AB
@�B
AUB
A�B
BAB
B�B
BuB
B�B
B�B
CB
C�B
D�B
DMB
E9B
EmB
EB
E�B
E�B
E�B
F%B
FB
F�B
F�B
F�B
F�B
F�B
GEB
HfB
H1B
HfB
HfB
H1B
G�B
F�B
F%B
F?B
E�B
E�B
F?B
EB
E�B
F�B
F�B
G�B
HfB
HB
H1B
HKB
HB
HB
IB
I�B
JXB
J�B
J�B
J�B
K)B
K�B
K�B
K�B
L�B
LJB
L0B
L�B
L�B
L�B
L~B
L�B
MjB
N"B
MjB
M�B
M�B
M�B
NB
M�B
OB
O�B
P.B
PB
P.B
P�B
P�B
P�B
Q�B
QNB
R B
Q4B
R B
R B
RTB
RoB
SuB
S�B
S&B
S�B
TaB
T�B
UMB
U�B
V�B
V�B
W?B
W�B
W�B
W�B
X�B
X�B
Y1B
YeB
Y1B
Y�B
Y�B
Z�B
[#B
\)B
\xB
]/B
]�B
]�B
^5B
^OB
^�B
^�B
_!B
_VB
^�B
^�B
`B
`B
_�B
`\B
`BB
_�B
_�B
_�B
_�B
_�B
_�B
`'B
`�B
`�B
aB
abB
a-B
a�B
abB
aB
bB
b4B
b�B
c�B
b�B
b�B
c B
cTB
cnB
c:B
eB
eB
d�B
ezB
e,B
f2B
f�B
g8B
g�B
g�B
g�B
g�B
h$B
g�B
g�B
g�B
g�B
hXB
hXB
iB
i*B
i*B
h�B
iB
h�B
i�B
i�B
jKB
j�B
jKB
kB
k6B
kkB
kB
k6B
kQB
k6B
k�B
k�B
lB
lWB
l=B
l�B
l�B
l�B
lWB
lWB
l�B
lqB
l�B
l�B
mB
mwB
m)B
nB
n�B
n�B
oOB
o�B
o�B
o B
oOB
qB
q�B
r-B
r-B
rB
r�B
sMB
s�B
s�B
sB
s�B
s�B
s�B
s�B
s�B
t9B
tnB
t�B
t�B
t�B
t�B
t�B
uB
u%B
utB
uZB
utB
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
v�B
v�B
w2B
w2B
wB
wfB
w�B
w�B
w�B
w�B
w�B
w�B
xB
x8B
x�B
x�B
x�B
y>B
y	1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B��B��B�NB��B�3B��B�-B��B�OB~�B~BB~]B~wB~BB~(BB�B~�B~�B~�B�iB��B��B�B��B�-B�gB�tB��B��B�EB�[B��B�NB	�B	��B
uB
.B
?�B
Q�B
j�B
q�B
�-B
��B
�NB
�mB
�UB
�B
ƎB
�&B
�'B
�B
�TB
��B5B3�BNVB}VB�6B��Bm�BsMBoBrGBwLBt�B-�BaB
��B
��B
�B
��B
�	B
~(B
u%B
S�B
%�B
�B
/B
 �B
�B
�B
"�B
\B	��B
�B	��B	�'B	��B	��B	�eB	n�B	S�B	D�B	>�B	9$B	1�B	*B	B	�B	B	�B	B	0B	9B	�B	�B	�B�B�)B��B�
B��B�`B��B��B�LB�nB�MB�"B�bB�IB	$tB	K�B	x�B	��B	��B	��B	��B	�B	�MB	�"B	��B	�zB	��B	�B	��B	�XB	�B
 �B
�B
�B
�B
!bB
4TB
4�B
72B
=B
CGB
B'B
A�B
A�B
?cB
?cB
G+B
=VB
6�B
:B
2|B
,�B
.IB
/�B
=�B
E�B
L�B
O�B
L�B
H�B
K�B
I�B
G�B
G�B
GzB
G�B
HfB
H�B
H�B
J�B
K�B
KxB
L�B
M�B
NVB
N"B
N�B
O�B
P�B
P�B
P}B
P�B
PHB
PB
PB
PHB
OvB
O�B
MB
MB
K�B
KxB
J�B
I�B
HfB
G�B
F?B
ESB
CGB
?�B
?.B
>B
=B
;�B
<jB
=qB
=�B
=�B
=qB
<6B
;�B
;dB
:�B
;0B
:B
9>B
8lB
8�B
7fB
5�B
5tB
5�B
4�B
4�B
4B
4B
3hB
3B
3B
2-B
1�B
1AB
0UB
-�B
-)B
,"B
*�B
)�B
(sB
&�B
&B
%zB
%�B
%,B
$�B
$@B
#�B
#TB
#B
#:B
"�B
!�B
!�B
!�B
 'B
�B
�B
�B
IB
�B
)B
�B
�B
WB
�B
7B
�B
�B
EB
�B
gB
�B
@B
�B
B
�B
�B
B
oB
 B
�B
�B
�B
�B

�B

#B
	�B
�B
�B
�B
�B
 4B	��B	�XB	�`B	��B	�%B	�ZB	��B	�TB	��B	�B	��B	��B	�B	��B	�+B	�+B	�`B	�FB	�`B	�`B	��B	�B	�B	�B	�)B	�B	�qB	��B	�B	�WB	�B	��B	�CB	�CB	��B	�B	�;B	�B	��B	�oB	�B	��B	��B	��B	��B	�B	��B	�B	��B	��B	�B	�B	��B	��B	�ZB	��B	��B	��B	�B	�B	��B	�B	�9B	��B	�B	�B	�9B	�B	�B	�nB	��B	�B	�+B	��B	�2B	��B	��B	�2B	�LB	�lB	��B	�B
 OB
 iB
B
�B
�B
%B
�B
�B
�B
�B
�B
�B
B
�B
�B
�B
%B
tB
?B
B
�B
�B
�B
B
�B
�B
�B
�B
 �B
B
  B	�.B	�(B	�B	��B	��B	�qB	�qB	�B	�<B	��B	�HB
  B	��B
�B
 B
UB
�B
B
�B
B
mB
�B
+B
�B
B
fB
�B
�B
�B
	�B
^B
�B
�B
�B
VB
B
VB
<B
pB
BB
B
�B
hB
�B
4B
�B
�B
�B
B
9B
B
SB
MB
B
&B
&B
@B
oB
�B
B
�B
�B
NB
�B
�B
�B
�B
:B
�B
�B
�B
�B
}B
 B
:B
�B
�B
�B
�B
@B
B
�B
�B
@B
@B
�B
aB
�B
�B
�B
�B
�B
?B
�B
sB
�B
�B
$B
�B
gB
gB
�B
gB
�B
B
�B
�B
�B
+B
�B
EB
�B
�B
?B
�B
QB
qB
B
�B
�B
�B
WB
qB
�B
�B
#B
7B
�B
B
�B
�B
qB
�B
�B
qB
�B
�B
B
�B
WB
�B
]B
�B
�B
�B
OB
�B
pB
�B
B
�B
VB
�B
 \B
!B
!-B
"NB
#:B
#�B
$@B
%,B
%FB
$�B
$�B
%FB
%�B
%`B
&�B
'�B
'�B
(�B
(�B
(�B
)*B
(�B
)DB
(�B
)B
)B
(�B
)*B
(�B
)DB
)�B
)�B
*KB
+B
*�B
*�B
+B
+B
+B
+�B
+�B
,B
,�B
-]B
-]B
,�B
-)B
-�B
.IB
/OB
/iB
/ B
/5B
/�B
0�B
0�B
1B
0�B
1�B
1�B
1�B
1�B
1�B
2�B
3�B
3�B
4B
4B
4B
4B
4nB
5ZB
5tB
5ZB
5�B
5�B
6`B
6zB
6FB
6�B
6zB
6�B
6�B
6�B
6�B
6�B
6�B
72B
6zB
6�B
6�B
6�B
6`B
7�B
72B
7fB
7�B
8RB
8�B
8RB
8�B
8�B
9�B
:�B
9XB
9�B
:B
;B
;B
<B
<�B
="B
=<B
=VB
=<B
=B
=�B
=�B
>]B
>BB
>]B
>�B
?B
>�B
>�B
>BB
>B
>B
>BB
>wB
>�B
?HB
?�B
@ B
@ B
@�B
@iB
A;B
A;B
@�B
@�B
@�B
@�B
@�B
?�B
>wB
>�B
=�B
>]B
>]B
>�B
>wB
>B
>wB
?.B
>�B
>wB
?HB
?�B
@B
?�B
@iB
AoB
A�B
AUB
@�B
AB
@�B
AUB
A�B
BAB
B�B
BuB
B�B
B�B
CB
C�B
D�B
DMB
E9B
EmB
EB
E�B
E�B
E�B
F%B
FB
F�B
F�B
F�B
F�B
F�B
GEB
HfB
H1B
HfB
HfB
H1B
G�B
F�B
F%B
F?B
E�B
E�B
F?B
EB
E�B
F�B
F�B
G�B
HfB
HB
H1B
HKB
HB
HB
IB
I�B
JXB
J�B
J�B
J�B
K)B
K�B
K�B
K�B
L�B
LJB
L0B
L�B
L�B
L�B
L~B
L�B
MjB
N"B
MjB
M�B
M�B
M�B
NB
M�B
OB
O�B
P.B
PB
P.B
P�B
P�B
P�B
Q�B
QNB
R B
Q4B
R B
R B
RTB
RoB
SuB
S�B
S&B
S�B
TaB
T�B
UMB
U�B
V�B
V�B
W?B
W�B
W�B
W�B
X�B
X�B
Y1B
YeB
Y1B
Y�B
Y�B
Z�B
[#B
\)B
\xB
]/B
]�B
]�B
^5B
^OB
^�B
^�B
_!B
_VB
^�B
^�B
`B
`B
_�B
`\B
`BB
_�B
_�B
_�B
_�B
_�B
_�B
`'B
`�B
`�B
aB
abB
a-B
a�B
abB
aB
bB
b4B
b�B
c�B
b�B
b�B
c B
cTB
cnB
c:B
eB
eB
d�B
ezB
e,B
f2B
f�B
g8B
g�B
g�B
g�B
g�B
h$B
g�B
g�B
g�B
g�B
hXB
hXB
iB
i*B
i*B
h�B
iB
h�B
i�B
i�B
jKB
j�B
jKB
kB
k6B
kkB
kB
k6B
kQB
k6B
k�B
k�B
lB
lWB
l=B
l�B
l�B
l�B
lWB
lWB
l�B
lqB
l�B
l�B
mB
mwB
m)B
nB
n�B
n�B
oOB
o�B
o�B
o B
oOB
qB
q�B
r-B
r-B
rB
r�B
sMB
s�B
s�B
sB
s�B
s�B
s�B
s�B
s�B
t9B
tnB
t�B
t�B
t�B
t�B
t�B
uB
u%B
utB
uZB
utB
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
v�B
v�B
w2B
w2B
wB
wfB
w�B
w�B
w�B
w�B
w�B
w�B
xB
x8B
x�B
x�B
x�B
y>B
y	1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604104915  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604173630  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604173630  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604173630                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605023637  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605023637  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610131507                      G�O�G�O�G�O�                