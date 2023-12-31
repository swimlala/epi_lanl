CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T17:42:41Z creation;2022-06-04T17:42:41Z conversion to V3.1      
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
_FillValue                 �  I4   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M$   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p|   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  tl   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �h   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �X   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �t   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ޤ   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �    HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �0   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �4   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �D   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �H   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �L   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �PArgo profile    3.1 1.2 19500101000000  20220604174241  20220610141505  5905853                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               oA   JA                                  2B  A   APEX                            8421                            2.11.2                          846 @ٻ=�6�1   @ٻ>Ϳ��@.���+�cIp��
=1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @��@�  @�  A   A   A>ffA`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  By33B�  B�  B�33B���B���B�ffB�  B���B�  B�33B�  B�  B�  B���B���B�  B�  B�  B�  B�  B�  B�33B���B�  B�  B�33B�33B���B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C33C  C  C  C�fC  C   C"  C$  C&  C'�fC*  C,  C.  C0  C2  C4  C6  C8  C:  C;�fC>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT�CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!�fD"fD"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7y�D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@fD@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]fD]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dqy�Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�3D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�#3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @fg@|��@�ff@�ffA33A=��A_33A33A���A���A���A���Aϙ�Aߙ�AA���B��B��B��B��B'��B/��B7��B?��BG��BO��BW��B_��Bg��Bo��By  B��B��fB��B��3B�� B�L�B��fB��3B��fB��B��fB��fB��fB��3B��3B��fB��fB��fB��fB��fB��fB��B׳3B��fB��fB��B��B�3B��fB��fB��fB��fB��fC�3C�3C�3C�3C	�3C�3C�3C�3C�3C&fC�3C�3C�3CٙC�3C�3C!�3C#�3C%�3C'ٙC)�3C+�3C-�3C/�3C1�3C3�3C5�3C7�3C9�3C;ٙC=�3C?�3CA�3CC�3CE�3CG�3CI�3CK�3CM�3CO�3CQ�3CT�CU�3CW�3CY�3C[�3C]�3C_�3Ca�3Cc�3Ce�3Cg�3Ci�3Ck�3Cm�3Co�3Cq�3Cs�3Cu�3Cw�3Cy�3C{�3C}�3C�3C���C���C���C���C���C���C���C���C���C���C���C�gC���C���C���C���C���C���C���C���C���C���C�gC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C�gC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D |�D ��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D	|�D	��D
|�D
��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D |�D ��D!�3D"3D"|�D"��D#|�D#��D$|�D$��D%|�D%��D&|�D&��D'|�D'��D(|�D(��D)|�D)��D*|�D*��D+|�D+��D,|�D,��D-|�D-��D.|�D.��D/|�D/��D0|�D0��D1|�D1��D2|�D2��D3|�D3��D4|�D4��D5|�D5��D6|�D6��D7vgD7��D8|�D8��D9|�D9��D:|�D:��D;|�D;��D<|�D<��D=|�D=��D>|�D>��D?|�D@3D@|�D@��DA|�DA��DB|�DB��DC|�DC��DD|�DD��DE|�DE��DF|�DF��DG|�DG��DH|�DH��DI|�DI��DJ|�DJ��DK|�DK��DL|�DL��DM|�DM��DN|�DN��DO|�DO��DP|�DP��DQ|�DQ��DR|�DR��DS|�DS��DT|�DT��DU|�DU��DV|�DV��DW|�DW��DX|�DX��DY|�DY��DZ|�DZ��D[|�D[��D\|�D]3D]|�D]��D^|�D^��D_|�D_��D`|�D`��Da|�Da��Db|�Db��Dc|�Dc��Dd|�Dd��De|�De��Df|�Df��Dg|�Dg��Dh|�Dh��Di|�Di��Dj|�Dj��Dk|�Dk��Dl|�Dl��Dm|�Dm��Dn|�Dn��Do|�Do��Dp|�Dp��DqvgDq��Dr|�Dr��Ds|�Ds��Dt|�Dt��Du|�Du��Dv|�Dv��Dw|�Dw��Dx|�Dx��Dy|�Dy��Dz|�Dz��D{|�D{��D||�D|��D}|�D}��D~|�D~��D|�D��D�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�{3D��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD¾fD��fD�>fD�~fDþfD��fD�>fD�~fDľfD��fD�>fD�~fDžfD��fD�>fD�~fDƾfD��fD�>fD�~fDǾfD��fD�>fD�~fDȾfD��fD�>fD�~fDɾfD��fD�>fD�~fDʾfD��fD�>fD�~fD˾fD��fD�>fD�~fD̾fD��fD�>fD�~fD;fD��fD�>fD�~fDξfD��fD�>fD�~fDϾfD��fD�>fD�~fDоfD��fD�>fD�~fDѾfD��fD�>fD�~fDҾfD��fD�>fD�~fDӾfD��fD�>fD�~fDԾfD��fD�>fD�~fDվfD��fD�>fD�~fD־fD��fD�>fD�~fD׾fD��fD�>fD�~fDؾfD��fD�>fD�~fDپfD��fD�>fD�~fDھfD��fD�>fD�~fD۾fD��fD�>fD�~fDܾfD��fD�>fD�~fDݾfD��fD�>fD�~fD޾fD��fD�>fD�~fD߾fD��D�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD��fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�!�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A˩_A˭A˫�A˭wA˥�Aˊ�AˉA˅A˄MAˀiA�o�A�i�A�o5AˁAˣ�Aˬ�A˭A˫6A˭�A˯�A���A�҉A�!A���A���A�ÖA˴�A˧�A˕MAˑ4A� \A� iA��Aʸ�AǑhA�˒A��A��A�B�A��BA���A��^A��A��\A��uA���A���A��A�`�A�_A��cA��A��YA��VA�@�A�uZA�e�A��%A��A�!�A��bA���A�v�A�~�A�,qA��A��A�V9A��JA��A{w�Ax-�Av�DAt�aAs��Ar��Aq��Ao͟Ai��Ab�'AaA].�AVq�AS`BANJ�AK$AJDgAI!-AH�AF��AF�AC7�A?ƨA;�eA8�A6hsA3��A2�A1[WA0�CA0"hA/��A.A�A-A,l�A*�,A)s�A(�KA(�@A'XyA&�.A&�A&�KA&�[A&�A';A&�:A%MA$1�A#�mA"��A!�&A �TA�A�A��A��A@�AF�A��A��A�A��A�`Ab�A��A=qAN<Al�Ae,A|A�$AVA
�A
��A
�A	�LA	8A(A	xlA
l�Ae�A�A�=A
��A
��A
��A
�A
5?A
(A
e�A
��A>BA�A?}AGEA
��A	��A	�A�XAW?A�A�'AiDAoA��A@OA҉A��ADgA��Ah�A��A=�A>BA9�A��A�rAcAdZA4AA �mA ��A �A W?A �A �A $@���@�xl@��@��m@�I�@��t@�Y�@�e@�=�@�	@�� @�u@��@�Z�@���@��@��p@�c @���@�w�@��>@���@�Mj@�[W@�W?@�+@�1�@�5�@�ѷ@�g8@�}�@��.@��@��@��@��@��@��@�+@��@��@�1@���@�($@�j�@�*�@��@㙚@���@��\@��@�x�@�;@ޛ�@ކY@��@��@޴9@�)�@ݥ�@݇�@�zx@��@�D�@۸�@�K�@ڧ@�R�@ڃ�@ڡb@�M�@ٝ�@���@ؚ�@�#:@׊	@�ȴ@�h
@�M@��@�|�@�X�@�B�@� \@��@�ff@��@�O@҇+@�M�@��d@�C@У@��@�[W@�X�@��P@���@�bN@��@��9@�m]@���@�Q�@��}@��@��Q@�=�@ȸR@�u%@�:*@���@Ƿ�@�e,@�/@�2a@Ƶ@ż�@�C-@���@Úk@�%F@�7@�q@���@�-@��@�(@�[�@��"@��'@�m�@�E�@���@���@��/@�R�@�U�@��I@��@��@�e�@�5�@��@�s�@���@���@�e�@�=@�o@��H@�~(@�0U@��=@�H�@���@��D@�d�@�
�@�� @��P@�\�@��@��4@�Ov@���@��:@�U�@�4�@��@��]@���@�5?@��A@���@�s@���@�tT@�1'@�G@�@���@��@��y@��@�bN@���@�$t@�u�@�@���@�[W@�Q�@�4�@�N<@��@�H�@��@�Z@�!�@��]@���@�o�@�(�@��@��\@�<�@�4@��W@���@���@�6z@�o@��}@�7@���@�(�@�@�(@��@�:�@���@���@�[W@���@��L@�m�@�5?@���@���@�G�@�	l@��6@�_@�1'@��@���@��k@���@�~�@�C-@���@��@��z@�#�@��)@���@�?@�O@��@��7@�K�@�d�@�@��@�@@��x@��@��a@���@�0�@��@��`@��B@���@�M@���@�hs@��@��\@��@���@�=@���@��e@�c�@�;�@�&�@���@��M@�*0@��@�� @�Z�@�($@���@��@@�c�@�8@��@���@�{�@�H@�-@��@��#@��H@��@���@���@�}�@�w2@�G�@��@���@��z@�Xy@���@��t@���@�e�@�\)@�K�@�4@��@���@��@�Ĝ@��!@��_@�p;@�M@�($@��@�خ@���@�hs@��@�g8@�$@���@���@���@�6z@��b@�g8@�$@��@��@��@��k@�iD@�B�@�!-@��@��F@�C�@��@a@~v�@~$�@}�n@}<6@|ی@|]d@{��@{�V@{_p@{@zM�@y��@y�=@y!�@xc�@xH@w�
@w�k@v�8@vp;@v0U@u��@u!�@t��@t�@t"h@s�{@s�@r�R@r��@rQ@q�7@qQ�@q@p֡@p�@o�@oH�@n�2@nz@n�@m�X@mN<@l�9@l`�@lPH@l7�@k��@j�,@ju%@j=q@i�@i�M@i=�@h�@hj@g��@g��@gC�@f��@f��@fl�@f#:@e�3@e��@ezx@e@@d�)@dtT@c��@cg�@c"�@b�@b� @bs�@a�D@ac�@a@@`�_@`C-@`%�@_ƨ@_�P@_b�@^�s@^@�@]�@]�S@]G�@]-w@]	l@\r�@\I�@[�+@[��@[�@Z��@Z��@Z��@Z4@Y��@Yzx@Yq@X�@X�p@X��@W��@W�@WiD@W�@V�@V��@V��@Vh
@V($@U��@UT�@T�@Ty>@Toi@T?�@T�@Sخ@S{J@SF�@S'�@R�y@R�B@Rn�@Q��@Q�@Q=�@P�@P6@O�@O�q@OE9@N�y@N}V@N!�@M�@Mm]@M�@L��@L��@L�o@LV�@L4n@K��@K��@J��@Jz@Ju@Iu�@I0�@I+@I�@H�@H�@HH@H$@G�K@G�@GA�@F�]@F��@Fff@F1�@E��@E�"@E+�@D��@D�U@DV�@D�@Cj�@B��@B��@B��@B�1@Bff@B@A��@A�#@A�n@Ap�@A5�@A%F@A�@@�)@@�D@@A�@@1@?��@?RT@>�"@>��@>p;@>;�@=�)@=�d@=�"@=*0@<��@<D�@;� @;x@;4�@:�B@:��@:R�@:�@9�)@9�t@9�~@9Dg@9-w@9+@9@8��@8u�@8bN@87�@8�@7��@7e�@7X�@7o@6�s@6n�@6V@6-@5�@5�S@5Dg@5#�@4�K@4h�@4�@3�+@3خ@3�*@3~�@3U�@3�@2��@2��@2u%@2�@2 �@1��@1��@1�@1rG@1Dg@1�@0�D@0"h@/�@/��@/��@/o�@/C�@/S@.�@.��@-�)@-��@-�@-��@-��@-��@-�7@-Vm@-�@,�|@,�@,�4@,��@,��@,�@+�Q@+�	@+K�@+ i@*�}@*��@*R�@*8�@*e@)�@)w2@)L�@)*0@(�f@(Ɇ@(��@(�o@(z�@(h�@(D�@(�@'�f@'o@&�m@&_�@%��@%�~@%?}@$��@$��@$�@$1'@#�$@#33@"�B@"R�@"	@!�@!w2@!@@ ��@ ��@ z�@ :�@ 7@�@��@6z@��@�@�1@��@h
@H�@+k@J@�)@�j@��@G�@#�@;@�`@�E@�O@$@�;@�a@|�@A�@�@��@��@M�@��@o @\�@B�@�@�$@y>@Xy@�@��@�Q@�k@K�@��@��@�+@ff@J�@;�@($@�@�>@�z@��@��@��@u�@a�@��@��@��@�z@�D@c�@?�@�&@��@b�@E9@�@�@�r@xl@h
@5?@�D@�3@��@zx@`B@&�@�@�@��@S�@�@x@�@��@��@qv@�@�@��@ff@1�@��@��@c@IR@:�@%F@�@;@��@Ĝ@�U@y>@V�@6@"h@b@�@�@��@W?@4�@"�@Y@�@
��@
�@
�!@
�@
�r@
^5@
;�@
-@
!�@
4@	��@	��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A˩_A˭A˫�A˭wA˥�Aˊ�AˉA˅A˄MAˀiA�o�A�i�A�o5AˁAˣ�Aˬ�A˭A˫6A˭�A˯�A���A�҉A�!A���A���A�ÖA˴�A˧�A˕MAˑ4A� \A� iA��Aʸ�AǑhA�˒A��A��A�B�A��BA���A��^A��A��\A��uA���A���A��A�`�A�_A��cA��A��YA��VA�@�A�uZA�e�A��%A��A�!�A��bA���A�v�A�~�A�,qA��A��A�V9A��JA��A{w�Ax-�Av�DAt�aAs��Ar��Aq��Ao͟Ai��Ab�'AaA].�AVq�AS`BANJ�AK$AJDgAI!-AH�AF��AF�AC7�A?ƨA;�eA8�A6hsA3��A2�A1[WA0�CA0"hA/��A.A�A-A,l�A*�,A)s�A(�KA(�@A'XyA&�.A&�A&�KA&�[A&�A';A&�:A%MA$1�A#�mA"��A!�&A �TA�A�A��A��A@�AF�A��A��A�A��A�`Ab�A��A=qAN<Al�Ae,A|A�$AVA
�A
��A
�A	�LA	8A(A	xlA
l�Ae�A�A�=A
��A
��A
��A
�A
5?A
(A
e�A
��A>BA�A?}AGEA
��A	��A	�A�XAW?A�A�'AiDAoA��A@OA҉A��ADgA��Ah�A��A=�A>BA9�A��A�rAcAdZA4AA �mA ��A �A W?A �A �A $@���@�xl@��@��m@�I�@��t@�Y�@�e@�=�@�	@�� @�u@��@�Z�@���@��@��p@�c @���@�w�@��>@���@�Mj@�[W@�W?@�+@�1�@�5�@�ѷ@�g8@�}�@��.@��@��@��@��@��@��@�+@��@��@�1@���@�($@�j�@�*�@��@㙚@���@��\@��@�x�@�;@ޛ�@ކY@��@��@޴9@�)�@ݥ�@݇�@�zx@��@�D�@۸�@�K�@ڧ@�R�@ڃ�@ڡb@�M�@ٝ�@���@ؚ�@�#:@׊	@�ȴ@�h
@�M@��@�|�@�X�@�B�@� \@��@�ff@��@�O@҇+@�M�@��d@�C@У@��@�[W@�X�@��P@���@�bN@��@��9@�m]@���@�Q�@��}@��@��Q@�=�@ȸR@�u%@�:*@���@Ƿ�@�e,@�/@�2a@Ƶ@ż�@�C-@���@Úk@�%F@�7@�q@���@�-@��@�(@�[�@��"@��'@�m�@�E�@���@���@��/@�R�@�U�@��I@��@��@�e�@�5�@��@�s�@���@���@�e�@�=@�o@��H@�~(@�0U@��=@�H�@���@��D@�d�@�
�@�� @��P@�\�@��@��4@�Ov@���@��:@�U�@�4�@��@��]@���@�5?@��A@���@�s@���@�tT@�1'@�G@�@���@��@��y@��@�bN@���@�$t@�u�@�@���@�[W@�Q�@�4�@�N<@��@�H�@��@�Z@�!�@��]@���@�o�@�(�@��@��\@�<�@�4@��W@���@���@�6z@�o@��}@�7@���@�(�@�@�(@��@�:�@���@���@�[W@���@��L@�m�@�5?@���@���@�G�@�	l@��6@�_@�1'@��@���@��k@���@�~�@�C-@���@��@��z@�#�@��)@���@�?@�O@��@��7@�K�@�d�@�@��@�@@��x@��@��a@���@�0�@��@��`@��B@���@�M@���@�hs@��@��\@��@���@�=@���@��e@�c�@�;�@�&�@���@��M@�*0@��@�� @�Z�@�($@���@��@@�c�@�8@��@���@�{�@�H@�-@��@��#@��H@��@���@���@�}�@�w2@�G�@��@���@��z@�Xy@���@��t@���@�e�@�\)@�K�@�4@��@���@��@�Ĝ@��!@��_@�p;@�M@�($@��@�خ@���@�hs@��@�g8@�$@���@���@���@�6z@��b@�g8@�$@��@��@��@��k@�iD@�B�@�!-@��@��F@�C�@��@a@~v�@~$�@}�n@}<6@|ی@|]d@{��@{�V@{_p@{@zM�@y��@y�=@y!�@xc�@xH@w�
@w�k@v�8@vp;@v0U@u��@u!�@t��@t�@t"h@s�{@s�@r�R@r��@rQ@q�7@qQ�@q@p֡@p�@o�@oH�@n�2@nz@n�@m�X@mN<@l�9@l`�@lPH@l7�@k��@j�,@ju%@j=q@i�@i�M@i=�@h�@hj@g��@g��@gC�@f��@f��@fl�@f#:@e�3@e��@ezx@e@@d�)@dtT@c��@cg�@c"�@b�@b� @bs�@a�D@ac�@a@@`�_@`C-@`%�@_ƨ@_�P@_b�@^�s@^@�@]�@]�S@]G�@]-w@]	l@\r�@\I�@[�+@[��@[�@Z��@Z��@Z��@Z4@Y��@Yzx@Yq@X�@X�p@X��@W��@W�@WiD@W�@V�@V��@V��@Vh
@V($@U��@UT�@T�@Ty>@Toi@T?�@T�@Sخ@S{J@SF�@S'�@R�y@R�B@Rn�@Q��@Q�@Q=�@P�@P6@O�@O�q@OE9@N�y@N}V@N!�@M�@Mm]@M�@L��@L��@L�o@LV�@L4n@K��@K��@J��@Jz@Ju@Iu�@I0�@I+@I�@H�@H�@HH@H$@G�K@G�@GA�@F�]@F��@Fff@F1�@E��@E�"@E+�@D��@D�U@DV�@D�@Cj�@B��@B��@B��@B�1@Bff@B@A��@A�#@A�n@Ap�@A5�@A%F@A�@@�)@@�D@@A�@@1@?��@?RT@>�"@>��@>p;@>;�@=�)@=�d@=�"@=*0@<��@<D�@;� @;x@;4�@:�B@:��@:R�@:�@9�)@9�t@9�~@9Dg@9-w@9+@9@8��@8u�@8bN@87�@8�@7��@7e�@7X�@7o@6�s@6n�@6V@6-@5�@5�S@5Dg@5#�@4�K@4h�@4�@3�+@3خ@3�*@3~�@3U�@3�@2��@2��@2u%@2�@2 �@1��@1��@1�@1rG@1Dg@1�@0�D@0"h@/�@/��@/��@/o�@/C�@/S@.�@.��@-�)@-��@-�@-��@-��@-��@-�7@-Vm@-�@,�|@,�@,�4@,��@,��@,�@+�Q@+�	@+K�@+ i@*�}@*��@*R�@*8�@*e@)�@)w2@)L�@)*0@(�f@(Ɇ@(��@(�o@(z�@(h�@(D�@(�@'�f@'o@&�m@&_�@%��@%�~@%?}@$��@$��@$�@$1'@#�$@#33@"�B@"R�@"	@!�@!w2@!@@ ��@ ��@ z�@ :�@ 7@�@��@6z@��@�@�1@��@h
@H�@+k@J@�)@�j@��@G�@#�@;@�`@�E@�O@$@�;@�a@|�@A�@�@��@��@M�@��@o @\�@B�@�@�$@y>@Xy@�@��@�Q@�k@K�@��@��@�+@ff@J�@;�@($@�@�>@�z@��@��@��@u�@a�@��@��@��@�z@�D@c�@?�@�&@��@b�@E9@�@�@�r@xl@h
@5?@�D@�3@��@zx@`B@&�@�@�@��@S�@�@x@�@��@��@qv@�@�@��@ff@1�@��@��@c@IR@:�@%F@�@;@��@Ĝ@�U@y>@V�@6@"h@b@�@�@��@W?@4�@"�@Y@�@
��@
�@
�!@
�@
�r@
^5@
;�@
-@
!�@
4@	��@	��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   Bj�Bj�Bj�Bj�Bj�Bk�Bk�Bk�BlBmBq[Bt9Bz�B��B��B��B�=B��B�!B�3B��B��B��B��B��B�@B�B�uB��B�B	 'B	<�B	I7B	g8B
}�B
�PB
��B��B�|B̳B��B�B��B�nB~�BW�B5�B^5BGEBP�B<�B�B
�B
��BxBB�BR BSuB:*B
��B
��B
��B
[#B
xB
�B	�B	�VB	�	B	�yB	��B	B	��B	�}B	�LB	�TB	��B	��B	�B	mCB	Q�B	H1B	6+B	�B	FB	\B	�B		B	YB	�B	�B	 B	UB�B�)B��B��B�2B�rB�rB�rB�"B�,BοBʦB��B�jB��B�BӏBΊB�~B�B	�B	�B	#:B	@iB	NpB	W�B	X�B	W�B	V9B	RoB	L�B	7fB	.�B	"�B	_B	eB	�B	7B��B��B�B�sB�}B�(B	
#B	gB	�B	)B	�B	B	�B	B	�B	:B	�B	�B	?B	9	B	L�B	~(B	��B	�B	��B	�B	��B	�$B	�
B	�kB	��B	��B	��B	�>B	��B	��B	��B	�3B	ĜB	�?B	ƎB	ƎB	�B	�B	ǔB	�{B	��B	��B	��B	�cB	��B	��B	�HB	�"B	�B	�"B	��B	��B	��B	�uB	ðB	�lB	̈́B	�<B	�<B	��B	�~B	̘B	��B	՛B	ѷB	�pB	��B	ܬB	�B	�eB	�yB	�2B	ԯB	��B	��B	ܒB	�pB	��B	�IB	��B	�B	�CB	�B	�qB	�B	�B	�!B	��B	��B	�B	�B	�B	�B	�3B	�B	�B	�*B	�>B	��B	�vB	�B	ݲB	�B	��B	�6B	�B	�mB	�B	�,B	�FB	��B	�B	�hB	��B	�B	�B	�bB	�B	�>B	�yB	��B	�B	��B	�KB	��B	�B	�B	�]B	��B	�OB	�'B	�B	��B	��B	�`B	�B	��B	��B	�`B	�2B	��B	��B	��B	�B	��B	��B	��B	��B	�FB	��B	��B	�B	��B	�B	�B	�B	��B	�-B	�B	��B	�ZB	�tB	��B	�TB	�9B	�nB	�B	��B	�B	��B	�B	�}B	�B	��B	�B	�B	��B	�B	�9B	��B	��B	�B	��B	��B	�B	�5B	��B	��B	��B	��B	�B	�B	�TB	�FB	��B	��B	�^B	�B	��B	�B	��B	��B	��B	��B	�*B	�*B	��B	�0B	�PB	�B	��B
 B
  B
  B	��B	��B
 �B
B
 OB
 �B
�B
uB
B
'B
B
uB
AB
�B
�B
B
GB
-B
B
-B
-B
�B
�B
�B
B
�B
�B
�B
aB
�B
aB
gB
�B
aB
�B
AB
�B
oB
 �B
 �B
 �B
oB
�B
�B
GB
�B
AB
 B
 �B
;B
 �B
 �B
 �B
 OB
B
�B
B
-B
�B
aB
MB
9B
�B
�B
�B
B
�B
�B
YB
�B
_B
zB
zB
�B
�B
�B
KB
1B
�B
�B
�B
	lB

#B
^B
B
dB
�B
B
�B
B
pB
B
�B
�B
:B
�B
�B
�B
�B
�B
YB
�B
�B
�B
$B
�B
�B
�B
_B
eB
�B
7B
=B
=B
�B
B
IB
�B
jB
B
pB
�B
 'B
 �B
 �B
!-B
!HB
!�B
"�B
# B
#TB
#�B
#�B
#�B
$B
$ZB
$�B
$�B
%,B
%`B
%�B
%�B
%�B
%�B
&LB
&�B
&�B
&�B
&�B
&�B
&�B
'B
'mB
'�B
'�B
'�B
(sB
(�B
(�B
(�B
(�B
(�B
(�B
)B
)B
)*B
)_B
)yB
)�B
)�B
*KB
*eB
+�B
,WB
,qB
,qB
-CB
-�B
-�B
.IB
./B
./B
.�B
/�B
0�B
0�B
1B
1[B
1�B
1�B
2GB
2|B
2�B
2�B
2aB
2|B
3B
3hB
3�B
3�B
3�B
4B
4B
4�B
4�B
4�B
5%B
5�B
6�B
6�B
7B
7�B
88B
88B
8lB
8�B
9	B
8�B
8�B
9$B
9>B
9rB
9XB
9�B
9�B
9�B
9�B
9�B
:B
;dB
;JB
;�B
;B
;�B
;�B
<�B
<�B
<�B
<�B
="B
="B
=�B
=�B
=�B
=�B
>(B
>�B
>�B
>�B
>�B
?B
?HB
?�B
?�B
?�B
?�B
@4B
@�B
@�B
@�B
@�B
AB
@�B
AB
AoB
AUB
A�B
BAB
BuB
BuB
B�B
B�B
B�B
C-B
C�B
C�B
DgB
DgB
D�B
D�B
D�B
D�B
ESB
E�B
E�B
F?B
F�B
FYB
F�B
GB
G+B
G_B
G_B
H1B
H1B
G�B
H1B
H�B
H�B
H�B
IRB
IB
IlB
I�B
I�B
J=B
J#B
J�B
J�B
JrB
J�B
J�B
KB
K^B
K�B
LB
LB
LB
K�B
L~B
L�B
L�B
MB
MB
M�B
L�B
M�B
N"B
M�B
N"B
N<B
N�B
OB
OB
OBB
O�B
O�B
PbB
PbB
P}B
P�B
P�B
QNB
Q B
QB
QhB
QhB
Q�B
RB
R B
RoB
R�B
S@B
S[B
SuB
S�B
S�B
T,B
TFB
T{B
T�B
T�B
U2B
UgB
U�B
U�B
U�B
VB
VB
V9B
V�B
V�B
W
B
WsB
W�B
W�B
W�B
W�B
X_B
X�B
XyB
X�B
X�B
X�B
X�B
X�B
X�B
Y1B
Y1B
YeB
YeB
Y�B
ZB
Z7B
ZQB
Z�B
Z�B
[#B
[	B
[=B
[�B
[�B
[�B
\]B
\�B
\�B
]dB
]dB
]�B
^B
^OB
^jB
^�B
^�B
^�B
^�B
^�B
_B
_B
^�B
^�B
_!B
_�B
_�B
_�B
_�B
`'B
`�B
`�B
`�B
`�B
aB
aHB
a-B
aHB
a�B
a�B
a�B
a�B
a�B
a�B
bhB
b�B
b�B
b�B
b�B
b�B
b�B
b�B
b�B
cB
c B
c�B
c�B
dZB
d�B
d�B
d�B
e,B
eFB
e`B
e�B
e�B
e�B
fLB
ffB
f�B
f�B
f�B
f�B
f�B
f�B
f�B
f�B
gB
gB
gB
f�B
gmB
g�B
g�B
g�B
h>B
hXB
h�B
h�B
h�B
h�B
iB
iyB
iyB
i�B
i�B
j0B
j0B
jKB
jeB
jeB
jeB
j�B
kB
kkB
k�B
k�B
l"B
lqB
l�B
mB
mB
m)B
mwB
m�B
n/B
n}B
n�B
o B
oB
oiB
o�B
p!B
p;B
p;B
p�B
poB
p�B
qB
q'B
qvB
q�B
q�B
q�B
q�B
q�B
rB
r-B
rGB
rGB
r�B
r�B
r�B
r�B
sB
sB
sMB
s�B
s�B
s�B
t9B
tTB
tnB
t�B
t�B
uB
u�B
u�B
u�B
u�B
vB
v`B
v�B
v�B
v�B
v�B
v�B
wLB
w�B
xB
x8B
xlB
x�B
x�B
x�B
x�B
x�B
y	B
y$B
y>B
y$B
y>B
yXB
y>B
zB
y�B
y�B
y�B
z*B
z*B
z^B
z�B
z�B
{B
z�B
{JB
{�B
{�B
{�B
{�B
|B
|6B
|PB
|jB
|�B
|�B
|�B
|�B
|�B
}"B
}<B
}<B
}qB
}�B
}�B
~]B
~BB
~�B
~�B
~�B
~�B
HB
}B
cB
�B
�4B
�iB
�iB
�iB
��B
��B
��B
��B
�UB
� B
�UB
�oB
��B
�oB
�oB
��B
��B
�B
�'B
�'B
�AB
�[B
�uB
��B
��B
��B
�-B
�-B
�GB
�aB
�{B
��B
��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   Bj�Bj�Bj�Bj�Bj�Bk�Bk�Bk�BlBmBq[Bt9Bz�B��B��B��B�=B��B�!B�3B��B��B��B��B��B�@B�B�uB��B�B	 'B	<�B	I7B	g8B
}�B
�PB
��B��B�|B̳B��B�B��B�nB~�BW�B5�B^5BGEBP�B<�B�B
�B
��BxBB�BR BSuB:*B
��B
��B
��B
[#B
xB
�B	�B	�VB	�	B	�yB	��B	B	��B	�}B	�LB	�TB	��B	��B	�B	mCB	Q�B	H1B	6+B	�B	FB	\B	�B		B	YB	�B	�B	 B	UB�B�)B��B��B�2B�rB�rB�rB�"B�,BοBʦB��B�jB��B�BӏBΊB�~B�B	�B	�B	#:B	@iB	NpB	W�B	X�B	W�B	V9B	RoB	L�B	7fB	.�B	"�B	_B	eB	�B	7B��B��B�B�sB�}B�(B	
#B	gB	�B	)B	�B	B	�B	B	�B	:B	�B	�B	?B	9	B	L�B	~(B	��B	�B	��B	�B	��B	�$B	�
B	�kB	��B	��B	��B	�>B	��B	��B	��B	�3B	ĜB	�?B	ƎB	ƎB	�B	�B	ǔB	�{B	��B	��B	��B	�cB	��B	��B	�HB	�"B	�B	�"B	��B	��B	��B	�uB	ðB	�lB	̈́B	�<B	�<B	��B	�~B	̘B	��B	՛B	ѷB	�pB	��B	ܬB	�B	�eB	�yB	�2B	ԯB	��B	��B	ܒB	�pB	��B	�IB	��B	�B	�CB	�B	�qB	�B	�B	�!B	��B	��B	�B	�B	�B	�B	�3B	�B	�B	�*B	�>B	��B	�vB	�B	ݲB	�B	��B	�6B	�B	�mB	�B	�,B	�FB	��B	�B	�hB	��B	�B	�B	�bB	�B	�>B	�yB	��B	�B	��B	�KB	��B	�B	�B	�]B	��B	�OB	�'B	�B	��B	��B	�`B	�B	��B	��B	�`B	�2B	��B	��B	��B	�B	��B	��B	��B	��B	�FB	��B	��B	�B	��B	�B	�B	�B	��B	�-B	�B	��B	�ZB	�tB	��B	�TB	�9B	�nB	�B	��B	�B	��B	�B	�}B	�B	��B	�B	�B	��B	�B	�9B	��B	��B	�B	��B	��B	�B	�5B	��B	��B	��B	��B	�B	�B	�TB	�FB	��B	��B	�^B	�B	��B	�B	��B	��B	��B	��B	�*B	�*B	��B	�0B	�PB	�B	��B
 B
  B
  B	��B	��B
 �B
B
 OB
 �B
�B
uB
B
'B
B
uB
AB
�B
�B
B
GB
-B
B
-B
-B
�B
�B
�B
B
�B
�B
�B
aB
�B
aB
gB
�B
aB
�B
AB
�B
oB
 �B
 �B
 �B
oB
�B
�B
GB
�B
AB
 B
 �B
;B
 �B
 �B
 �B
 OB
B
�B
B
-B
�B
aB
MB
9B
�B
�B
�B
B
�B
�B
YB
�B
_B
zB
zB
�B
�B
�B
KB
1B
�B
�B
�B
	lB

#B
^B
B
dB
�B
B
�B
B
pB
B
�B
�B
:B
�B
�B
�B
�B
�B
YB
�B
�B
�B
$B
�B
�B
�B
_B
eB
�B
7B
=B
=B
�B
B
IB
�B
jB
B
pB
�B
 'B
 �B
 �B
!-B
!HB
!�B
"�B
# B
#TB
#�B
#�B
#�B
$B
$ZB
$�B
$�B
%,B
%`B
%�B
%�B
%�B
%�B
&LB
&�B
&�B
&�B
&�B
&�B
&�B
'B
'mB
'�B
'�B
'�B
(sB
(�B
(�B
(�B
(�B
(�B
(�B
)B
)B
)*B
)_B
)yB
)�B
)�B
*KB
*eB
+�B
,WB
,qB
,qB
-CB
-�B
-�B
.IB
./B
./B
.�B
/�B
0�B
0�B
1B
1[B
1�B
1�B
2GB
2|B
2�B
2�B
2aB
2|B
3B
3hB
3�B
3�B
3�B
4B
4B
4�B
4�B
4�B
5%B
5�B
6�B
6�B
7B
7�B
88B
88B
8lB
8�B
9	B
8�B
8�B
9$B
9>B
9rB
9XB
9�B
9�B
9�B
9�B
9�B
:B
;dB
;JB
;�B
;B
;�B
;�B
<�B
<�B
<�B
<�B
="B
="B
=�B
=�B
=�B
=�B
>(B
>�B
>�B
>�B
>�B
?B
?HB
?�B
?�B
?�B
?�B
@4B
@�B
@�B
@�B
@�B
AB
@�B
AB
AoB
AUB
A�B
BAB
BuB
BuB
B�B
B�B
B�B
C-B
C�B
C�B
DgB
DgB
D�B
D�B
D�B
D�B
ESB
E�B
E�B
F?B
F�B
FYB
F�B
GB
G+B
G_B
G_B
H1B
H1B
G�B
H1B
H�B
H�B
H�B
IRB
IB
IlB
I�B
I�B
J=B
J#B
J�B
J�B
JrB
J�B
J�B
KB
K^B
K�B
LB
LB
LB
K�B
L~B
L�B
L�B
MB
MB
M�B
L�B
M�B
N"B
M�B
N"B
N<B
N�B
OB
OB
OBB
O�B
O�B
PbB
PbB
P}B
P�B
P�B
QNB
Q B
QB
QhB
QhB
Q�B
RB
R B
RoB
R�B
S@B
S[B
SuB
S�B
S�B
T,B
TFB
T{B
T�B
T�B
U2B
UgB
U�B
U�B
U�B
VB
VB
V9B
V�B
V�B
W
B
WsB
W�B
W�B
W�B
W�B
X_B
X�B
XyB
X�B
X�B
X�B
X�B
X�B
X�B
Y1B
Y1B
YeB
YeB
Y�B
ZB
Z7B
ZQB
Z�B
Z�B
[#B
[	B
[=B
[�B
[�B
[�B
\]B
\�B
\�B
]dB
]dB
]�B
^B
^OB
^jB
^�B
^�B
^�B
^�B
^�B
_B
_B
^�B
^�B
_!B
_�B
_�B
_�B
_�B
`'B
`�B
`�B
`�B
`�B
aB
aHB
a-B
aHB
a�B
a�B
a�B
a�B
a�B
a�B
bhB
b�B
b�B
b�B
b�B
b�B
b�B
b�B
b�B
cB
c B
c�B
c�B
dZB
d�B
d�B
d�B
e,B
eFB
e`B
e�B
e�B
e�B
fLB
ffB
f�B
f�B
f�B
f�B
f�B
f�B
f�B
f�B
gB
gB
gB
f�B
gmB
g�B
g�B
g�B
h>B
hXB
h�B
h�B
h�B
h�B
iB
iyB
iyB
i�B
i�B
j0B
j0B
jKB
jeB
jeB
jeB
j�B
kB
kkB
k�B
k�B
l"B
lqB
l�B
mB
mB
m)B
mwB
m�B
n/B
n}B
n�B
o B
oB
oiB
o�B
p!B
p;B
p;B
p�B
poB
p�B
qB
q'B
qvB
q�B
q�B
q�B
q�B
q�B
rB
r-B
rGB
rGB
r�B
r�B
r�B
r�B
sB
sB
sMB
s�B
s�B
s�B
t9B
tTB
tnB
t�B
t�B
uB
u�B
u�B
u�B
u�B
vB
v`B
v�B
v�B
v�B
v�B
v�B
wLB
w�B
xB
x8B
xlB
x�B
x�B
x�B
x�B
x�B
y	B
y$B
y>B
y$B
y>B
yXB
y>B
zB
y�B
y�B
y�B
z*B
z*B
z^B
z�B
z�B
{B
z�B
{JB
{�B
{�B
{�B
{�B
|B
|6B
|PB
|jB
|�B
|�B
|�B
|�B
|�B
}"B
}<B
}<B
}qB
}�B
}�B
~]B
~BB
~�B
~�B
~�B
~�B
HB
}B
cB
�B
�4B
�iB
�iB
�iB
��B
��B
��B
��B
�UB
� B
�UB
�oB
��B
�oB
�oB
��B
��B
�B
�'B
�'B
�AB
�[B
�uB
��B
��B
��B
�-B
�-B
�GB
�aB
�{B
��B
��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604104929  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604174241  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604174241  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604174241                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605024249  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605024249  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610141505                      G�O�G�O�G�O�                