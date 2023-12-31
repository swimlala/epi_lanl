CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-10-25T03:42:51Z creation;2022-10-25T03:42:51Z conversion to V3.1      
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
_FillValue                    �tArgo profile    3.1 1.2 19500101000000  20221025034251  20221025040206  5905855                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA                                  2B  A   APEX                            8423                            2.11.2                          846 @�����>1   @���y\�$@-�I�^5�c�x���1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @��@�  @�  A   A   A@  A^ffA�  A�  A�  A�  A�  A�  A�  A���B  B33B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  By33B�  B���B�  B�  B�  B�33B�ffB���B�  B�  B�  B�  B�33B�33B�  B�  B�33B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C �C�C  C  C�fC	�fC  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*33C,  C.  C/�fC2  C3�fC6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV33CW��CY�fC\  C]�fC`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DFfDF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dgy�Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�3D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @fg@|��@�ff@�ffA33A?33A]��A33A���A���A���A���Aϙ�Aߙ�A�fgB��B  B��B��B��B'��B/��B7��B?��BG��BO��BW��B_��Bg��Bo��By  B��B��3B��fB��fB��fB��B�L�B��3B��fB��fB��fB��fB��B��B��fB��fB��Bó3Bǳ3B��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fC �C�C�3C�3CٙC	ٙC�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C!�3C#�3C%�3C'�3C*&fC+�3C-�3C/ٙC1�3C3ٙC5�3C7�3C9�3C;�3C=�3C?�3CA�3CC�3CE�3CG�3CI�3CK�3CM�3CO�3CQ�3CS�3CV&fCW� CYٙC[�3C]ٙC_�3Ca�3Cc�3Ce�3Cg�3Ci�3Ck�3Cm�3Co�3Cq�3Cs�3Cu�3Cw�3Cy�3C{�3C}�3C�3C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C�gC���C���C���C���C���C���C���C���C���C���C���C���C���C�gC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C�gC�gC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D |�D ��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D	|�D	��D
|�D
��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D�3D��D|�D��D|�D��D|�D��D|�D��D|�D��D |�D ��D!|�D!��D"|�D"��D#|�D#��D$|�D$��D%|�D%��D&|�D&��D'|�D'��D(|�D(��D)|�D)��D*|�D*��D+|�D+��D,|�D,��D-|�D-��D.|�D.��D/|�D/��D0|�D0��D1|�D1��D2|�D2��D3|�D3��D4|�D4��D5|�D5��D6|�D6��D7|�D7��D8|�D8��D9|�D9��D:|�D:��D;|�D;��D<|�D<��D=|�D=��D>|�D>��D?|�D?��D@|�D@��DA|�DA��DB|�DB��DC|�DC��DD|�DD��DE|�DF3DF|�DF��DG|�DG��DH|�DH��DI|�DI��DJ|�DJ��DK|�DK��DL|�DL��DM|�DM��DN|�DN��DO|�DO��DP|�DP��DQ|�DQ��DR|�DR��DS|�DS��DT|�DT��DU|�DU��DV|�DV��DW|�DW��DX|�DX��DY|�DY��DZ|�DZ��D[|�D[��D\|�D\��D]|�D]��D^|�D^��D_|�D_��D`|�D`��Da|�Da��Db|�Db��Dc|�Dc��Dd|�Dd��De|�De��Df|�Df��DgvgDg��Dh|�Dh��Di|�Di��Dj|�Dj��Dk|�Dk��Dl|�Dl��Dm|�Dm��Dn|�Dn��Do|�Do��Dp|�Dp��Dq|�Dq��Dr|�Dr��Ds|�Ds��Dt|�Dt��Du|�Du��Dv|�Dv��Dw|�Dw��Dx|�Dx��Dy|�Dy��Dz|�Dz��D{|�D{��D||�D|��D}|�D}��D~|�D~��D|�D��D�A�D�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��D�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD¾fD��fD�>fD�~fDþfD��fD�>fD�~fDľfD��fD�>fD�~fDžfD��fD�>fD�~fDƾfD��fD�>fD�~fDǾfD��fD�>fD�~fDȾfD��fD�>fD�~fDɾfD��fD�>fD�~fDʾfD��fD�>fD�~fD˾fD��fD�>fD�~fD̾fD��fD�>fD�~fD;fD��fD�>fD�~fDξfD��fD�>fD�~fDϾfD��fD�>fD�~fDоfD��fD�>fD�~fDѾfD��fD�>fD�~fDҾfD��fD�>fD�~fDӾfD��fD�>fD�~fDԾfD��fD�>fD�~fDվfD��fD�>fD�~fD־fD��fD�>fD�~fD׾fD��fD�>fD�~fDؾfD��fD�>fD�~fDپfD��fD�>fD�~fDھfD��fD�>fD�~fD۾fD��fD�>fD�~fDܾfD��fD�>fD�~fDݾfD��fD�>fD�~fD޾fD��fD�>fD�~fD߾fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD��fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~f1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�u�A�y�A߀4A߄MA߄MA�u%A�sA�zA�d�A�ZQA�T�A�R�A�XEA�TaA�RTA�5A��8A���AܼjAܯ�A�tTA�GzA��A� 4A��Aۗ�A�R A�+A�n/A�0�A��A��)Aҟ!A�\A�GA�X�A�^5Aɜ�A�C�A��)A�<�A��A��ZA�M6A���A��8A�(A�B[A�C-A��_A�XA�̘A��GA�uZA��4A�%FA���A�QA�خA��xA�-�A�]�A��PA��!A���A���A��A�YA�ĜA�Q�A�,qA��A�h>A�?�A��mA���A��=A��)A��3A�X�A�'�A��A���A�/�A�.IA�m)A�A�i�A~�fAz��At�1Ao�3Am?Aj�Ag��A`��A[�RAX�:AV�=AVCAU*0AR+�AL!�AI_AD�AA=�A=�?A<��A9��A6��A4��A3e�A1B�A/�/A.�A.8�A,��A+��A*�KA*kQA)�A(H�A'�A'W?A'A&�VA&T�A%�&A%�9A%g�A%>BA$ԕA#�BA#!�A"B[A J�A�VA�A�A�A�Am�A��A�DAGEA��A�[AA�}AH�A�/A��A}VAH�A�XAx�A33A�+AB�A_A�A�A,=A�Av�AAc A�AGA��A�.A��A�DA�/A�bA{�A=�AxA
˒A
_�A
�A
{A
'�A	��A��A�{A�|A,�A�A.�Aa�A:*A��A�A��A�~A>BA+AA�A �E@��HA �A �FA ��@�]�@���@��@��@�F�@��@�Z@���@��@���@�I�@�"�@���@���@�$t@��@�9�@�@��K@@��d@�=@�h@�M@�xl@�F@�n�@�ԕ@�'@�8@���@�c�@�P@���@�h
@���@�9�@�0U@�&@��f@�|�@�{J@��"@�-w@�u�@�-@�0@�H@��@�$t@�q�@ߴ�@���@��W@���@ݭC@�C�@���@ܕ�@��@�� @۷�@�%F@��E@ډ�@��6@�e@�@��,@ָR@�Ov@��+@�o@Ԅ�@���@�m]@��@��s@��@��,@Ү}@�_@��@���@фM@��@���@�&�@���@�ff@�S&@�H@�iD@���@��H@���@��@ʄ�@�H@�#:@�	�@Ɋ�@�@�YK@�	@ǹ�@��@��&@�T�@���@��.@��@�@ìq@�,�@¥z@�@�|�@�n�@�8�@���@�A�@���@�!@�_p@�K�@�#�@��@���@�z@���@�Mj@�-w@���@��<@���@��@��@��M@�0�@���@��@���@���@��7@���@�w�@�C-@�,=@�M@���@��h@���@�oi@�8�@���@�s�@�e,@�a�@�_p@�<6@���@�L0@��@�Dg@�{�@�($@���@�e,@�#�@��@��F@�"h@�hs@��@���@�!�@��^@�F�@���@�I�@��@�ݘ@��"@�_p@�
=@�Xy@�7�@�	�@���@�N<@�'�@���@�ff@��@�o @���@�s�@�-@�
�@��@��Q@�g�@��@���@��1@�tT@�I�@�	@��@�Q�@���@���@�	@���@�+@���@��A@�5?@��@��@���@�\)@��@��@���@��T@�Vm@���@��@�\)@�O@�G�@��K@�~�@�_@�$�@���@��g@��F@���@�O@���@���@��	@�t�@�W?@�H�@�O�@�u�@�c@�f�@��@��.@�"h@�7@�  @�S�@�M�@��D@���@�@O@��@�^5@�M�@�!@���@�]�@��@��2@���@��b@�1'@�j@�IR@�E9@�&@���@�֡@��_@�`�@�B[@��@��6@��~@�Mj@�&@��@���@��2@���@��Y@�Q@�_@��@���@�]�@��K@�� @�Xy@�GE@�.�@��*@�RT@��@���@�_@�"h@��m@��q@��P@�o @�(@��?@�Xy@�$�@���@��}@���@�n/@�J�@���@���@��s@��9@�^5@� �@���@�@O@�@�֡@���@�y>@�"h@�{@���@�F@�@@��'@�y>@�4n@���@���@��$@�J#@���@�� @�i�@�($@�@��@
=@~@}��@}?}@}�@}e,@}x�@|ѷ@|!@{ƨ@{dZ@{�@z��@za|@y�#@xh�@w�A@w��@w�*@wX�@w@uw2@t[�@t@s�A@s��@s�@s�&@s��@s9�@r��@q�)@q��@q7L@q4@q^�@p[�@o�a@o��@o��@o�P@n��@m�@m+�@m+�@mQ�@ma�@m��@mzx@m�@k�@lu�@lK^@k˒@kqv@j�8@j��@j�}@j��@jOv@j($@i��@ik�@i�@h�@hS�@g�+@g˒@g�@g'�@f��@f�@f��@f�A@eԕ@ex�@e;@d[�@c�;@c_p@b�m@b;�@b�@a��@a�-@aF@`�P@`�$@`A�@_�g@_�k@_o�@_$t@^ߤ@^��@]�@]j@\�/@\:�@[l�@Z�F@Z
�@Yhs@Y�@X��@Wݘ@WdZ@WK�@W.I@V��@V	@U�@U��@U�h@U5�@U�@T�)@Toi@S� @SU�@S8@R��@R��@RJ�@R#:@R�@Q��@Q��@Q`B@Q+@P�5@P��@PM@O�w@OiD@O(@N��@N{�@NR�@M�3@MN<@M%@L�`@L�[@L�u@Le�@L?�@K�
@K��@K~�@K!-@J�<@JM�@I�D@I�C@I�=@I��@IX@I�@H��@H��@Hc�@H7@G��@Gqv@GE9@F�@Fv�@Fh
@FL0@Fu@EY�@E@Dی@D�@De�@DQ�@D9X@D~@C��@C_p@C"�@B�]@B��@Bh
@BL0@B4@AJ�@@��@@K^@?�@?�[@?U�@>�R@>v�@>O@> �@=L�@<��@<V�@<"h@;��@;��@;��@;g�@;8@:�@:�H@:ȴ@:��@:s�@:^5@:�@9�t@9c@9|@9=�@8|�@8~@8�@7ƨ@7Mj@6�@6�@6C�@5��@5T�@5�@4��@4w�@3�+@3�{@2�]@2q�@2J�@24@1��@1�z@1T�@0��@0�@0�U@0�Y@0	�@/�0@/��@/6z@.��@.@. �@-��@-<6@-�@,�@,_@+��@+�*@+g�@*�@*�r@*
�@)�#@)��@)�@)��@)��@)+�@)�@(�$@(r�@(M@(@'�W@'��@'�6@'��@'�*@'�:@'v`@'A�@'�@&��@&�X@&�h@&� @&{�@&Z�@%��@%�t@%�~@%`B@%�@$��@$�[@$~(@$[�@$>B@#��@#��@#e�@#@"�6@"�\@"z@"V@"!�@!��@!��@!�@!2a@ �@ ��@ 9X@�]@��@|�@iD@_p@6z@�@�X@��@�+@Z�@�@�@c@A @�@�|@��@��@w�@bN@:�@  @��@�{@_p@;d@�c@xl@6�@�@
�@�Z@��@��@u�@�@��@[�@A�@˒@�@��@x@P�@)_@"�@(@�@�}@��@�r@s�@C�@�@��@��@zx@Q�@4@V@�5@�9@�.@g8@M@4n@M@�&@�[@~�@RT@�@�@�}@l�@C�@4@�)@�-@zx@A @%F@�@@�@��@��@��@-�@�@  @��@��@�$@W?@9�@&@�@��@��@ߤ@ں@��@~�@R�@1�@�@�o@�-@��@��@p�@a�@Vm@F@�@�K@�@�@	�@�@��@;d@�@
�M@
��@
^5@
J�@
E�@
E�@
GE@
C�@
?@
�@	�@	��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�u�A�y�A߀4A߄MA߄MA�u%A�sA�zA�d�A�ZQA�T�A�R�A�XEA�TaA�RTA�5A��8A���AܼjAܯ�A�tTA�GzA��A� 4A��Aۗ�A�R A�+A�n/A�0�A��A��)Aҟ!A�\A�GA�X�A�^5Aɜ�A�C�A��)A�<�A��A��ZA�M6A���A��8A�(A�B[A�C-A��_A�XA�̘A��GA�uZA��4A�%FA���A�QA�خA��xA�-�A�]�A��PA��!A���A���A��A�YA�ĜA�Q�A�,qA��A�h>A�?�A��mA���A��=A��)A��3A�X�A�'�A��A���A�/�A�.IA�m)A�A�i�A~�fAz��At�1Ao�3Am?Aj�Ag��A`��A[�RAX�:AV�=AVCAU*0AR+�AL!�AI_AD�AA=�A=�?A<��A9��A6��A4��A3e�A1B�A/�/A.�A.8�A,��A+��A*�KA*kQA)�A(H�A'�A'W?A'A&�VA&T�A%�&A%�9A%g�A%>BA$ԕA#�BA#!�A"B[A J�A�VA�A�A�A�Am�A��A�DAGEA��A�[AA�}AH�A�/A��A}VAH�A�XAx�A33A�+AB�A_A�A�A,=A�Av�AAc A�AGA��A�.A��A�DA�/A�bA{�A=�AxA
˒A
_�A
�A
{A
'�A	��A��A�{A�|A,�A�A.�Aa�A:*A��A�A��A�~A>BA+AA�A �E@��HA �A �FA ��@�]�@���@��@��@�F�@��@�Z@���@��@���@�I�@�"�@���@���@�$t@��@�9�@�@��K@@��d@�=@�h@�M@�xl@�F@�n�@�ԕ@�'@�8@���@�c�@�P@���@�h
@���@�9�@�0U@�&@��f@�|�@�{J@��"@�-w@�u�@�-@�0@�H@��@�$t@�q�@ߴ�@���@��W@���@ݭC@�C�@���@ܕ�@��@�� @۷�@�%F@��E@ډ�@��6@�e@�@��,@ָR@�Ov@��+@�o@Ԅ�@���@�m]@��@��s@��@��,@Ү}@�_@��@���@фM@��@���@�&�@���@�ff@�S&@�H@�iD@���@��H@���@��@ʄ�@�H@�#:@�	�@Ɋ�@�@�YK@�	@ǹ�@��@��&@�T�@���@��.@��@�@ìq@�,�@¥z@�@�|�@�n�@�8�@���@�A�@���@�!@�_p@�K�@�#�@��@���@�z@���@�Mj@�-w@���@��<@���@��@��@��M@�0�@���@��@���@���@��7@���@�w�@�C-@�,=@�M@���@��h@���@�oi@�8�@���@�s�@�e,@�a�@�_p@�<6@���@�L0@��@�Dg@�{�@�($@���@�e,@�#�@��@��F@�"h@�hs@��@���@�!�@��^@�F�@���@�I�@��@�ݘ@��"@�_p@�
=@�Xy@�7�@�	�@���@�N<@�'�@���@�ff@��@�o @���@�s�@�-@�
�@��@��Q@�g�@��@���@��1@�tT@�I�@�	@��@�Q�@���@���@�	@���@�+@���@��A@�5?@��@��@���@�\)@��@��@���@��T@�Vm@���@��@�\)@�O@�G�@��K@�~�@�_@�$�@���@��g@��F@���@�O@���@���@��	@�t�@�W?@�H�@�O�@�u�@�c@�f�@��@��.@�"h@�7@�  @�S�@�M�@��D@���@�@O@��@�^5@�M�@�!@���@�]�@��@��2@���@��b@�1'@�j@�IR@�E9@�&@���@�֡@��_@�`�@�B[@��@��6@��~@�Mj@�&@��@���@��2@���@��Y@�Q@�_@��@���@�]�@��K@�� @�Xy@�GE@�.�@��*@�RT@��@���@�_@�"h@��m@��q@��P@�o @�(@��?@�Xy@�$�@���@��}@���@�n/@�J�@���@���@��s@��9@�^5@� �@���@�@O@�@�֡@���@�y>@�"h@�{@���@�F@�@@��'@�y>@�4n@���@���@��$@�J#@���@�� @�i�@�($@�@��@
=@~@}��@}?}@}�@}e,@}x�@|ѷ@|!@{ƨ@{dZ@{�@z��@za|@y�#@xh�@w�A@w��@w�*@wX�@w@uw2@t[�@t@s�A@s��@s�@s�&@s��@s9�@r��@q�)@q��@q7L@q4@q^�@p[�@o�a@o��@o��@o�P@n��@m�@m+�@m+�@mQ�@ma�@m��@mzx@m�@k�@lu�@lK^@k˒@kqv@j�8@j��@j�}@j��@jOv@j($@i��@ik�@i�@h�@hS�@g�+@g˒@g�@g'�@f��@f�@f��@f�A@eԕ@ex�@e;@d[�@c�;@c_p@b�m@b;�@b�@a��@a�-@aF@`�P@`�$@`A�@_�g@_�k@_o�@_$t@^ߤ@^��@]�@]j@\�/@\:�@[l�@Z�F@Z
�@Yhs@Y�@X��@Wݘ@WdZ@WK�@W.I@V��@V	@U�@U��@U�h@U5�@U�@T�)@Toi@S� @SU�@S8@R��@R��@RJ�@R#:@R�@Q��@Q��@Q`B@Q+@P�5@P��@PM@O�w@OiD@O(@N��@N{�@NR�@M�3@MN<@M%@L�`@L�[@L�u@Le�@L?�@K�
@K��@K~�@K!-@J�<@JM�@I�D@I�C@I�=@I��@IX@I�@H��@H��@Hc�@H7@G��@Gqv@GE9@F�@Fv�@Fh
@FL0@Fu@EY�@E@Dی@D�@De�@DQ�@D9X@D~@C��@C_p@C"�@B�]@B��@Bh
@BL0@B4@AJ�@@��@@K^@?�@?�[@?U�@>�R@>v�@>O@> �@=L�@<��@<V�@<"h@;��@;��@;��@;g�@;8@:�@:�H@:ȴ@:��@:s�@:^5@:�@9�t@9c@9|@9=�@8|�@8~@8�@7ƨ@7Mj@6�@6�@6C�@5��@5T�@5�@4��@4w�@3�+@3�{@2�]@2q�@2J�@24@1��@1�z@1T�@0��@0�@0�U@0�Y@0	�@/�0@/��@/6z@.��@.@. �@-��@-<6@-�@,�@,_@+��@+�*@+g�@*�@*�r@*
�@)�#@)��@)�@)��@)��@)+�@)�@(�$@(r�@(M@(@'�W@'��@'�6@'��@'�*@'�:@'v`@'A�@'�@&��@&�X@&�h@&� @&{�@&Z�@%��@%�t@%�~@%`B@%�@$��@$�[@$~(@$[�@$>B@#��@#��@#e�@#@"�6@"�\@"z@"V@"!�@!��@!��@!�@!2a@ �@ ��@ 9X@�]@��@|�@iD@_p@6z@�@�X@��@�+@Z�@�@�@c@A @�@�|@��@��@w�@bN@:�@  @��@�{@_p@;d@�c@xl@6�@�@
�@�Z@��@��@u�@�@��@[�@A�@˒@�@��@x@P�@)_@"�@(@�@�}@��@�r@s�@C�@�@��@��@zx@Q�@4@V@�5@�9@�.@g8@M@4n@M@�&@�[@~�@RT@�@�@�}@l�@C�@4@�)@�-@zx@A @%F@�@@�@��@��@��@-�@�@  @��@��@�$@W?@9�@&@�@��@��@ߤ@ں@��@~�@R�@1�@�@�o@�-@��@��@p�@a�@Vm@F@�@�K@�@�@	�@�@��@;d@�@
�M@
��@
^5@
J�@
E�@
E�@
GE@
C�@
?@
�@	�@	��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B	�YB	�YB	�B	�YB	�YB	ƎB	�tB	�YB	�EB	�zB	��B	��B	��B	�KB	��B	�}B	��B
ޞB
��B
�bB
�4B
�-B
�5B
�B
یB
��B
��B
бB
ΊB
��B
�_B
�rB
�lB
ɠB
ϫB
�*B{JBh
B^B��B�BٚB�cBHfB^�B[qBVBy$B��B��B�B��B�LB��B�)B��B��B�FB��B�uB��BshB�oB�uBr�Be�B]�BT�B4TB
B�dB�]B��B՛B��B��BjKBF�B!-B�B
�6B
ңB
��B
��B
t�B
V�B
PB
A;B
	�B	��B	��B	��B	��B	�_B	s�B	T�B	9XB	)�B	B	�B	gB	EB��B�mB�B�IBߊB��B��B�B�B�OB�B��B�DB��B	 4B	GB	�B	DB	(B	!HB	4B	E�B	K�B	b�B	raB	zDB	HB	�iB	�B	|B	z�B	xB	t�B	k�B	e,B	[qB	S�B	`�B	m�B	r�B	��B	�B	��B	�UB	ĶB	ðB	�B	�?B	��B	�^B	�9B	��B	�RB	��B	�"B	�'B	�aB	�B	͟B	��B	�{B	��B	�3B	��B	��B	��B	�"B	��B	�oB	��B	�vB	��B	��B	��B	��B	�9B	��B	�XB	��B	��B	��B	�xB	��B	��B	�fB	��B	�B	� B	� B	�$B	��B	��B	��B	��B	��B	�/B	�B	��B	�"B	��B	�qB	�8B	�wB	�}B	��B	�4B	��B	��B	�dB	�B	��B	��B	��B	�{B	ĶB	�aB	�GB	��B	�gB	�B	�GB	��B	�B	�AB	�[B	�AB	�B	��B	��B	�OB	�B	�oB	��B	�aB	��B	�B	��B	�)B	��B	��B	��B	�B	�xB	ϫB	�B	�B	�B	�B	�B	��B	��B	�B	�B	�B	�B	�&B	�@B	�B	�XB	�B	�yB	�eB	�6B	��B	�B	�B	�_B	�yB	�B	�B	�,B	�@B	�B	�&B	�`B	��B	�fB	��B	�tB	�B	�RB	�kB	�OB	�AB	�aB	�hB	��B	�FB	�+B	��B	�`B	��B	��B	��B	��B	�B	�IB	�B	��B	�MB	�%B	�ZB	��B	�zB	�RB	�lB	�	B	�$B	�>B	�>B	�B	��B	�lB	�RB	��B	�DB	��B	�JB	��B	��B	�B	��B	�"B	�B	��B	��B	�]B	��B	�(B	��B	��B	�BB	��B	�VB	��B	��B	�B	�BB	�BB	��B	�.B	��B	�cB
 4B
 iB
 �B
B
 �B
�B
oB
oB
oB
UB
UB
UB
�B
[B
{B
MB
�B
�B
�B
MB
SB
�B
�B
�B
�B
�B
B
�B
�B
fB
fB
	B
�B
	B
�B
fB
	�B

�B

�B
�B
0B
0B
dB
0B
�B
JB
�B
B
6B
jB
jB
jB
B
B
6B
�B
�B
.B
}B
�B
 B
hB
 B
TB
�B
�B
�B
[B
�B
�B
B
mB
mB

B
�B
YB
�B
�B
+B
�B
�B
sB
+B
�B
�B
�B
�B
9B
�B
�B
&B
�B
MB
�B
�B
aB
�B
�B
�B
B
�B
�B
yB
+B
�B
�B
�B
�B
�B
�B
�B
�B
!B
�B
�B
�B
 BB
 \B
B
�B
�B
/B
�B
xB
�B
xB
�B
�B
�B
B
)B
]B
~B
jB
jB
jB
�B
�B
�B
;B
�B
�B
 �B
!|B
!�B
!�B
!|B
!�B
!HB
!bB
!HB
!B
!�B
!�B
"hB
"�B
#B
#�B
$�B
$�B
$�B
$�B
&B
&LB
&�B
'B
(>B
)B
)*B
)_B
)_B
)_B
)�B
)�B
*B
*KB
*�B
+6B
+�B
+�B
,qB
,�B
,�B
,�B
-]B
-�B
-�B
.cB
-�B
./B
./B
.IB
.�B
/5B
.�B
0B
0�B
0�B
1'B
1�B
1�B
2�B
2�B
2�B
2�B
2B
1�B
1�B
2-B
2-B
2-B
2|B
2�B
2�B
3B
3�B
5tB
6+B
6`B
5�B
6FB
6+B
5�B
5�B
5ZB
5%B
33B
2�B
2�B
4�B
5�B
5�B
5�B
4�B
5?B
5ZB
5�B
5�B
6+B
7B
8B
9$B
9	B
8�B
8�B
9>B
;�B
;�B
;B
:�B
:�B
:�B
:DB
9�B
9�B
9�B
;B
;�B
>(B
=�B
=VB
=�B
@�B
A�B
BuB
B�B
CB
C-B
C-B
CGB
CaB
CaB
C{B
C�B
DB
DMB
D�B
EB
EB
EB
E�B
E�B
E�B
E�B
E�B
F�B
F�B
F�B
G_B
G�B
HB
H�B
H�B
H�B
IB
IB
I�B
I�B
I�B
J	B
JXB
JrB
JrB
J�B
J�B
J�B
K^B
K�B
K�B
L0B
L�B
MjB
M�B
N"B
N"B
N�B
O(B
O\B
OBB
O\B
O�B
PHB
PHB
PbB
PbB
P�B
P�B
P�B
QB
Q�B
Q�B
Q�B
Q�B
R:B
RoB
R�B
R�B
R�B
R�B
R�B
S&B
S@B
S@B
S�B
S�B
TB
TaB
T{B
T�B
T�B
UB
UgB
U�B
U�B
U�B
U�B
VB
VB
V�B
VSB
V9B
V�B
V�B
V�B
W$B
W?B
W$B
W$B
W$B
WYB
WYB
W�B
W�B
W�B
XEB
X+B
XEB
X�B
X�B
X�B
X�B
Y1B
Y�B
Y�B
Y�B
ZB
ZB
Z7B
ZQB
ZQB
Z�B
Z�B
[#B
[WB
[�B
[�B
[�B
[�B
\]B
\�B
\�B
]IB
]IB
]�B
^B
^OB
^jB
^OB
^�B
^�B
_;B
_pB
_pB
`B
`B
_�B
_�B
`B
`BB
`\B
`vB
`�B
`vB
`�B
`�B
`�B
`�B
aB
abB
a|B
abB
a�B
a�B
bB
bB
bNB
b�B
b�B
b�B
c B
cTB
c�B
dB
dtB
d�B
d�B
d�B
d�B
eB
e`B
e�B
e�B
e�B
e�B
ffB
f�B
f�B
f�B
gRB
g�B
g�B
h
B
hXB
h�B
h�B
i*B
iyB
i�B
i�B
j0B
jeB
kB
kB
kB
kB
kB
kB
k�B
k�B
k�B
k�B
l"B
lWB
lqB
lqB
l�B
l�B
l�B
l�B
l�B
l�B
mB
mB
mCB
mCB
m�B
m�B
m�B
m�B
n/B
ncB
n}B
n�B
n�B
n�B
oOB
oOB
oOB
o�B
o�B
o�B
p!B
p�B
p�B
p�B
p�B
p�B
p�B
qB
q'B
q�B
q�B
q�B
rB
raB
r�B
r�B
r�B
r�B
r�B
sMB
s3B
sMB
shB
s�B
s�B
tB
t9B
tnB
t9B
tTB
t�B
t�B
t�B
t�B
t�B
u%B
utB
u�B
u�B
u�B
vB
v`B
v�B
v�B
v�B
v�B
wB
wB
wB
w�B
w�B
xB
xB
x�B
x�B
x�B
x�B
y$B
y>B
y>B
yXB
yrB
y�B
y�B
y�B
y�B
zB
zB
zDB
z�B
z�B
z�B
z�B
{B
{0B
{B
{B
{�B
{�B
{�B
|B
|6B
|PB
|�B
|�B
|�B
|�B
}VB
}�B
}�B
}�B
~B
~wB
~�B
~�B
~�B
B
B
HB
cB
cB
�B
�B
� B
�B
�B
�OB
�OB
��B
��B
��B
�B
��B
�B
�B
�B
� B
��B
��B
��B
��B
��B
�'B
�AB
�[B
�uB
�uB
�uB
�uB
��B
��B
�B
�{B
��B
��B
��B
�gB
�gB
��B
�9B
�SB
�mB
�mB
�mB
��B
�mB
�mB
��B
�?B
�?1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B	�YB	�YB	�B	�YB	�YB	ƎB	�tB	�YB	�EB	�zB	��B	��B	��B	�KB	��B	�}B	��B
ޞB
��B
�bB
�4B
�-B
�5B
�B
یB
��B
��B
бB
ΊB
��B
�_B
�rB
�lB
ɠB
ϫB
�*B{JBh
B^B��B�BٚB�cBHfB^�B[qBVBy$B��B��B�B��B�LB��B�)B��B��B�FB��B�uB��BshB�oB�uBr�Be�B]�BT�B4TB
B�dB�]B��B՛B��B��BjKBF�B!-B�B
�6B
ңB
��B
��B
t�B
V�B
PB
A;B
	�B	��B	��B	��B	��B	�_B	s�B	T�B	9XB	)�B	B	�B	gB	EB��B�mB�B�IBߊB��B��B�B�B�OB�B��B�DB��B	 4B	GB	�B	DB	(B	!HB	4B	E�B	K�B	b�B	raB	zDB	HB	�iB	�B	|B	z�B	xB	t�B	k�B	e,B	[qB	S�B	`�B	m�B	r�B	��B	�B	��B	�UB	ĶB	ðB	�B	�?B	��B	�^B	�9B	��B	�RB	��B	�"B	�'B	�aB	�B	͟B	��B	�{B	��B	�3B	��B	��B	��B	�"B	��B	�oB	��B	�vB	��B	��B	��B	��B	�9B	��B	�XB	��B	��B	��B	�xB	��B	��B	�fB	��B	�B	� B	� B	�$B	��B	��B	��B	��B	��B	�/B	�B	��B	�"B	��B	�qB	�8B	�wB	�}B	��B	�4B	��B	��B	�dB	�B	��B	��B	��B	�{B	ĶB	�aB	�GB	��B	�gB	�B	�GB	��B	�B	�AB	�[B	�AB	�B	��B	��B	�OB	�B	�oB	��B	�aB	��B	�B	��B	�)B	��B	��B	��B	�B	�xB	ϫB	�B	�B	�B	�B	�B	��B	��B	�B	�B	�B	�B	�&B	�@B	�B	�XB	�B	�yB	�eB	�6B	��B	�B	�B	�_B	�yB	�B	�B	�,B	�@B	�B	�&B	�`B	��B	�fB	��B	�tB	�B	�RB	�kB	�OB	�AB	�aB	�hB	��B	�FB	�+B	��B	�`B	��B	��B	��B	��B	�B	�IB	�B	��B	�MB	�%B	�ZB	��B	�zB	�RB	�lB	�	B	�$B	�>B	�>B	�B	��B	�lB	�RB	��B	�DB	��B	�JB	��B	��B	�B	��B	�"B	�B	��B	��B	�]B	��B	�(B	��B	��B	�BB	��B	�VB	��B	��B	�B	�BB	�BB	��B	�.B	��B	�cB
 4B
 iB
 �B
B
 �B
�B
oB
oB
oB
UB
UB
UB
�B
[B
{B
MB
�B
�B
�B
MB
SB
�B
�B
�B
�B
�B
B
�B
�B
fB
fB
	B
�B
	B
�B
fB
	�B

�B

�B
�B
0B
0B
dB
0B
�B
JB
�B
B
6B
jB
jB
jB
B
B
6B
�B
�B
.B
}B
�B
 B
hB
 B
TB
�B
�B
�B
[B
�B
�B
B
mB
mB

B
�B
YB
�B
�B
+B
�B
�B
sB
+B
�B
�B
�B
�B
9B
�B
�B
&B
�B
MB
�B
�B
aB
�B
�B
�B
B
�B
�B
yB
+B
�B
�B
�B
�B
�B
�B
�B
�B
!B
�B
�B
�B
 BB
 \B
B
�B
�B
/B
�B
xB
�B
xB
�B
�B
�B
B
)B
]B
~B
jB
jB
jB
�B
�B
�B
;B
�B
�B
 �B
!|B
!�B
!�B
!|B
!�B
!HB
!bB
!HB
!B
!�B
!�B
"hB
"�B
#B
#�B
$�B
$�B
$�B
$�B
&B
&LB
&�B
'B
(>B
)B
)*B
)_B
)_B
)_B
)�B
)�B
*B
*KB
*�B
+6B
+�B
+�B
,qB
,�B
,�B
,�B
-]B
-�B
-�B
.cB
-�B
./B
./B
.IB
.�B
/5B
.�B
0B
0�B
0�B
1'B
1�B
1�B
2�B
2�B
2�B
2�B
2B
1�B
1�B
2-B
2-B
2-B
2|B
2�B
2�B
3B
3�B
5tB
6+B
6`B
5�B
6FB
6+B
5�B
5�B
5ZB
5%B
33B
2�B
2�B
4�B
5�B
5�B
5�B
4�B
5?B
5ZB
5�B
5�B
6+B
7B
8B
9$B
9	B
8�B
8�B
9>B
;�B
;�B
;B
:�B
:�B
:�B
:DB
9�B
9�B
9�B
;B
;�B
>(B
=�B
=VB
=�B
@�B
A�B
BuB
B�B
CB
C-B
C-B
CGB
CaB
CaB
C{B
C�B
DB
DMB
D�B
EB
EB
EB
E�B
E�B
E�B
E�B
E�B
F�B
F�B
F�B
G_B
G�B
HB
H�B
H�B
H�B
IB
IB
I�B
I�B
I�B
J	B
JXB
JrB
JrB
J�B
J�B
J�B
K^B
K�B
K�B
L0B
L�B
MjB
M�B
N"B
N"B
N�B
O(B
O\B
OBB
O\B
O�B
PHB
PHB
PbB
PbB
P�B
P�B
P�B
QB
Q�B
Q�B
Q�B
Q�B
R:B
RoB
R�B
R�B
R�B
R�B
R�B
S&B
S@B
S@B
S�B
S�B
TB
TaB
T{B
T�B
T�B
UB
UgB
U�B
U�B
U�B
U�B
VB
VB
V�B
VSB
V9B
V�B
V�B
V�B
W$B
W?B
W$B
W$B
W$B
WYB
WYB
W�B
W�B
W�B
XEB
X+B
XEB
X�B
X�B
X�B
X�B
Y1B
Y�B
Y�B
Y�B
ZB
ZB
Z7B
ZQB
ZQB
Z�B
Z�B
[#B
[WB
[�B
[�B
[�B
[�B
\]B
\�B
\�B
]IB
]IB
]�B
^B
^OB
^jB
^OB
^�B
^�B
_;B
_pB
_pB
`B
`B
_�B
_�B
`B
`BB
`\B
`vB
`�B
`vB
`�B
`�B
`�B
`�B
aB
abB
a|B
abB
a�B
a�B
bB
bB
bNB
b�B
b�B
b�B
c B
cTB
c�B
dB
dtB
d�B
d�B
d�B
d�B
eB
e`B
e�B
e�B
e�B
e�B
ffB
f�B
f�B
f�B
gRB
g�B
g�B
h
B
hXB
h�B
h�B
i*B
iyB
i�B
i�B
j0B
jeB
kB
kB
kB
kB
kB
kB
k�B
k�B
k�B
k�B
l"B
lWB
lqB
lqB
l�B
l�B
l�B
l�B
l�B
l�B
mB
mB
mCB
mCB
m�B
m�B
m�B
m�B
n/B
ncB
n}B
n�B
n�B
n�B
oOB
oOB
oOB
o�B
o�B
o�B
p!B
p�B
p�B
p�B
p�B
p�B
p�B
qB
q'B
q�B
q�B
q�B
rB
raB
r�B
r�B
r�B
r�B
r�B
sMB
s3B
sMB
shB
s�B
s�B
tB
t9B
tnB
t9B
tTB
t�B
t�B
t�B
t�B
t�B
u%B
utB
u�B
u�B
u�B
vB
v`B
v�B
v�B
v�B
v�B
wB
wB
wB
w�B
w�B
xB
xB
x�B
x�B
x�B
x�B
y$B
y>B
y>B
yXB
yrB
y�B
y�B
y�B
y�B
zB
zB
zDB
z�B
z�B
z�B
z�B
{B
{0B
{B
{B
{�B
{�B
{�B
|B
|6B
|PB
|�B
|�B
|�B
|�B
}VB
}�B
}�B
}�B
~B
~wB
~�B
~�B
~�B
B
B
HB
cB
cB
�B
�B
� B
�B
�B
�OB
�OB
��B
��B
��B
�B
��B
�B
�B
�B
� B
��B
��B
��B
��B
��B
�'B
�AB
�[B
�uB
�uB
�uB
�uB
��B
��B
�B
�{B
��B
��B
��B
�gB
�gB
��B
�9B
�SB
�mB
�mB
�mB
��B
�mB
�mB
��B
�?B
�?1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20221025034227  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20221025034251  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20221025034251  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20221025034251                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20221025124256  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20221025124256  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20221025040206                      G�O�G�O�G�O�                