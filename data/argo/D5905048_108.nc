CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2017-04-13T00:35:34Z creation;2017-04-13T00:35:37Z conversion to V3.1;2019-12-19T08:10:21Z update;     
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
resolution        =���   axis      Z        l  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     l  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \P   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     l  `,   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     l  st   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     l  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �(   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     l  �   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �p   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     l  �L   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     l  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     l  �$   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ې   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �    SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �    SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �    SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �    HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �    HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    � Argo profile    3.1 1.2 19500101000000  20170413003534  20200116211517  5905048                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               lA   JA  I2_0577_108                     2C  D   NAVIS_A                         0577                            ARGO 102115                     863 @��u��� 1   @��v��O�@3pH���d�N���U1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @@  @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   BffB  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� DqfDq� Dr  Dr�fDs  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D���D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D���D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D��3D�  D�@ D� D�� D�3D�C3D� D�� D�  D�@ D� D�� D�  D�@ D�|�D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�C3D��3D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @<��@|��@�ff@�ffA33A?33A_33A33A���A���A���A���Aϙ�Aߙ�AA���B33B��B��B��B'��B/��B7��B?��BG��BO��BW��B_��Bg��Bo��Bw��B��B��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fC�3C�3C�3C�3C	�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C!�3C#�3C%�3C'�3C)�3C+�3C-�3C/�3C1�3C3�3C5�3C7�3C9�3C;�3C=�3C?�3CA�3CC�3CE�3CG�3CI�3CK�3CM�3CO�3CQ�3CS�3CU�3CW�3CY�3C[�3C]�3C_�3Ca�3Cc�3Ce�3Cg�3Ci�3Ck�3Cm�3Co�3Cq�3Cs�3Cu�3Cw�3Cy�3C{�3C}�3C�3C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C�fC�fC���C�fC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D |�D ��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D	|�D	��D
|�D
��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D�fD|�D��D|�D��D |�D ��D!|�D!��D"|�D"��D#|�D#��D$|�D$��D%|�D%��D&|�D&��D'|�D'��D(|�D(��D)|�D)��D*|�D*��D+|�D+��D,|�D,��D-|�D-��D.|�D.��D/|�D/��D0|�D0��D1|�D1��D2|�D2��D3|�D3��D4|�D4��D5|�D5��D6|�D6��D7|�D7��D8|�D8��D9|�D9��D:|�D:��D;|�D;��D<|�D<��D=|�D=��D>|�D>��D?|�D?��D@|�D@��DA|�DA��DB|�DB��DC|�DC��DD|�DD��DE|�DE��DF|�DF��DG|�DG��DH|�DH��DI|�DI��DJ|�DJ��DK|�DK��DL|�DL��DM|�DM��DN|�DN��DO|�DO��DP|�DP��DQ|�DQ��DR|�DR��DS|�DS��DT|�DT��DU|�DU��DV|�DV��DW|�DW��DX|�DX��DY|�DY��DZ|�DZ��D[|�D[��D\|�D\��D]|�D]��D^|�D^��D_|�D_��D`|�D`��Da|�Da��Db|�Db��Dc|�Dc��Dd|�Dd��De|�De��Df|�Df��Dg|�Dg��Dh|�Dh��Di|�Di��Dj|�Dj��Dk|�Dk��Dl|�Dl��Dm|�Dm��Dn|�Dn��Do|�Do��Dp|�Dq3Dq|�Dq��Dr�3Dr��Ds|�Ds��Dt|�Dt��Du|�Du��Dv|�Dv��Dw|�Dw��Dx|�Dx��Dy|�Dy��Dz|�Dz��D{|�D{��D||�D|��D}|�D}��D~|�D~��D|�D��D�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�A�D�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD¾fD��fD�>fD�~fDþfD��fD�>fD�~fDľfD��fD�>fD�~fDžfD��fD�>fD�~fDƾfD��fD�>fD�~fDǾfD��fD�>fD�~fDȾfD��fD�>fD�~fDɾfD��fD�>fD�~fDʾfD��fD�>fD�~fD˾fD��fD�>fD�~fD̾fD��fD�>fD�~fD;fD��fD�>fD�~fDξfD��fD�>fD�~fDϾfD��fD�>fD�~fDоfD��fD�>fD�~fDѾfD��fD�>fD�~fDҾfD��fD�>fD�~fDӾfD��fD�>fD�~fDԾfD��fD�>fD�~fDվfD��fD�>fD�~fD־fD��fD�>fD�~fD׾fD��fD�>fD�~fDؾfD��3D�>fD�~fDپfD��fD�>fD�~fDھfD��fD�>fD�~fD۾fD��fD�>fD�~fDܾfD��3D�>fD�~fDݾfD��fD�>fD�~fD޾fD��fD�>fD�~fD߾fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD���D��fD�>fD�~fD�fD��D�A�D�~fD�fD��fD�>fD�~fD�fD��fD�>fD�{3D�fD��fD�>fD�~fD�fD��fD�>fD�~fD��fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�A�D���D��3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 Aщ7A�~�A�x�A�x�A�n�A�l�A�l�A�l�A�jA�ffA�dZA�bNA�bNA�bNA�`BA�`BA�`BA�^5A�\)A�S�A�I�AжFA���A��A�bA���A�VA��ÃA�S�A��A˓uA�ZA�XAɑhA�&�AȮA�O�AǍPA��Aƕ�A��`A�C�A�^5A��
A�C�A���A¼jAA�7LA��A��A�%A��A�VA�A���A���A�v�A�JA�+A��wA�O�A���A�  A�;dA�A�A��A�z�A���A���A�C�A��/A�;dA���A��uA�hsA�1A��-A�M�A���A�M�A�ĜA�`BA���A��mA�O�A��A�S�A�M�A�hsA�1'A�n�A��/A� �A��wA�ƨA��PA�|�A�VA���A�n�A���A�/A�  A��-A�r�A�G�A��A��A���A�;dA��7A���A��A�v�A�`BA�;dA��A��A���A��RA�JA�"�A��A�bA�7LA7LA~ZA{oAyK�Ax �Au��As��Aq�#Ap��Ao|�AnZAln�Ag�FAd�HAb�yAb�AbM�A`��A\�A[�;AZ��AY�FAXn�AU��AT��AS�hAP�yAPbAN�ANA�AL�AK�^AI��AHffAE�;AC��AA��A?�A<��A;�A:z�A9�wA8��A7��A6{A5�A4ffA3&�A2VA1S�A/+A-O�A+�A*�uA)C�A(�+A'O�A&9XA%\)A$�HA$ffA#ƨA"JA �Ax�Az�AK�AI�AO�A�A(�A7LA�AK�A&�A�HA�AĜA�AffAM�A$�AhsA��AA�mA��A��A��AA�A�-A��A�Al�A	S�A��A��A��AȴA\)A�!A��A��A�uAVAA ��@�l�@�@��H@��7@��9@�(�@�K�@�V@��@��+@��/@�1@�\@�@�V@�1'@�
=@�V@�bN@���@��@�ff@��#@�7L@�Ĝ@�r�@�9@���@�!@�r�@�p�@�@�G�@���@�ƨ@�|�@�l�@�33@��@�=q@�hs@�%@�j@���@ڰ!@�$�@�p�@�&�@ج@ו�@��H@�=q@��@�p�@�bN@�@Ѳ-@��@Ь@��m@�{@˶F@�@��@�Z@ǥ�@�l�@Ɨ�@�@�V@�z�@�C�@�E�@���@�x�@�X@���@�Ĝ@�r�@�1@��@�dZ@�K�@�"�@��@�-@���@�$�@�{@�hs@��@�A�@�A�@�bN@��9@�Ĝ@�Q�@��@�-@�j@�@�E�@�@���@�x�@�7L@���@�j@�z�@�z�@��@���@��@��w@�C�@��@��/@�r�@���@� �@���@�\)@�A�@���@�-@�&�@�E�@�ff@���@�n�@���@�Ĝ@�t�@���@��P@�S�@�\)@��;@�Ĝ@�Ĝ@�1@��@�ȴ@��y@���@�V@���@���@��@�`B@���@�=q@��\@��@�&�@�G�@�&�@�?}@���@���@�  @��!@�-@��h@���@�j@�Q�@�bN@�Q�@�A�@��@� �@�1@��;@���@�dZ@�33@��@�o@�
=@���@�@��7@���@���@� �@�  @��F@�l�@��H@�$�@��@���@���@���@��u@�9X@��@��@��@���@�l�@�l�@�C�@�+@�@�ȴ@�M�@�$�@�@�@�J@��#@��@�x�@�`B@�7L@��@��@��@� �@��@�t�@�;d@�@���@���@�-@��@��#@��7@�O�@�&�@��@���@��/@��D@�j@�Q�@��@��@�K�@�o@��R@�n�@�M�@�$�@�@���@��h@�7L@��@��@�V@���@���@�j@�9X@���@�t�@�C�@��y@��\@�v�@�5?@��@��@��T@��^@�hs@���@���@�1@�dZ@�;d@�o@��H@��R@���@���@��+@�@���@��@��T@���@��@�x�@�hs@�?}@�&�@�&�@��@���@��D@�A�@�  @��
@���@�S�@�o@�ȴ@�-@�@��h@�p�@�G�@�%@��`@�Ĝ@�I�@��m@���@���@�|�@�S�@�C�@��@��@���@�{@���@�/@���@�Ĝ@��j@���@��u@�z�@� �@�@~�y@~�R@~��@~��@~{@}V@|1@{��@{t�@{o@z�\@y�#@yx�@yX@yG�@y&�@x�9@xb@wl�@wK�@w+@w
=@v�y@vȴ@v��@v{@u��@u�@t�@t�@s"�@r��@r=q@q�#@q7L@p�9@p  @o�@o\)@o+@n�@n@m/@lz�@lI�@l9X@k��@k��@k"�@ko@j�@j�H@j��@jM�@j�@j�@i�#@ix�@i7L@i&�@h��@h��@h�9@g�w@g�P@gl�@f��@f{@e�@e/@d�j@dz�@dZ@c��@c��@c"�@c@b�\@b=q@a7L@`�@`bN@`bN@`1'@`b@_�@_�w@_��@_l�@^ȴ@^E�@]/@\z�@[�m@["�@Y��@Yx�@YX@Y%@X��@XbN@X  @W�w@W��@W�P@WK�@V�y@V�+@VV@V5?@VE�@VE�@VE�@V$�@V{@U��@Up�@T��@T(�@SC�@S33@So@R�@R��@R��@R~�@R=q@R�@P�`@P�@O�@O;d@NE�@M�@M�h@MO�@M?}@MV@L��@L�D@L(�@K�m@K�@K33@J��@I�@I��@I�7@IG�@H��@H�u@H  @G��@Gl�@G+@Fȴ@F��@F�+@Fv�@F5?@E�@E@E��@E�@EV@D�/@D�D@DZ@D9X@C��@C��@C�
@CS�@B~�@B-@A��@A��@A�^@A��@AX@A&�@A�@@�`@@��@@r�@@bN@@ �@?�@?�P@?|�@?l�@?K�@?
=@>v�@>ff@>$�@=�@<��@;�m@;o@:�H@:��@:�\@:n�@:M�@:=q@:-@:-@:J@9�@9��@9�^@9��@9��@9x�@97L@9�@9�@9%@8Ĝ@81'@7�;@7;d@7
=@5�@5`B@5/@4�@4�j@4�@4I�@4�@3�m@3��@3��@3�@3o@2�H@2��@2�!@2��@2��@2~�@2=q@2J@1�#@1��@1�^@1�7@1hs@1&�@1�@0��@0�u@0 �@/�w@/\)@/K�@/;d@/+@/�@.��@.{@-��@-@-�-@-��@,��@,�j@,��@,j@,j@,I�@+��@+ƨ@+��@+o@*~�@*^5@*=q@*J@)��@)X@(��@(�u@(bN@( �@(  @'�w@'�P@'�@&�@&��@&v�@&5?@&{@%�T@%��@%?}@$��@$�j@$�D@$j@$j@$j@$I�@$9X@$(�@#��@#ƨ@#t�@#S�@#33@#"�@#"�@#o@"�@"�H@"�H@"��@"�!@"~�@"n�@"M�@"�@!�#@!x�@!%@ ��@ �9@ �@  �@   @�;@�w@��@�P@l�@K�@
=@�@�R@�R@�+@ff@E�@@�@��@�h@`B@?}@�@�j@�@�D@j@j@Z@Z@(�@��@�m@��@t�@dZ@o@�H@��@~�@-@��@�@��@�7@�7@x�@X@7L@��@�u@A�@ �@�@��@�@�P@\)@;d@��@�@��@��@v�@5?@�T@�-@�h@p�@/@�@��@��@Z@I�@I�@I�@1@�F@�@o@�@��@^5@�@J@�@�^@��@hs@%@�`111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 Aщ7A�~�A�x�A�x�A�n�A�l�A�l�A�l�A�jA�ffA�dZA�bNA�bNA�bNA�`BA�`BA�`BA�^5A�\)A�S�A�I�AжFA���A��A�bA���A�VA��ÃA�S�A��A˓uA�ZA�XAɑhA�&�AȮA�O�AǍPA��Aƕ�A��`A�C�A�^5A��
A�C�A���A¼jAA�7LA��A��A�%A��A�VA�A���A���A�v�A�JA�+A��wA�O�A���A�  A�;dA�A�A��A�z�A���A���A�C�A��/A�;dA���A��uA�hsA�1A��-A�M�A���A�M�A�ĜA�`BA���A��mA�O�A��A�S�A�M�A�hsA�1'A�n�A��/A� �A��wA�ƨA��PA�|�A�VA���A�n�A���A�/A�  A��-A�r�A�G�A��A��A���A�;dA��7A���A��A�v�A�`BA�;dA��A��A���A��RA�JA�"�A��A�bA�7LA7LA~ZA{oAyK�Ax �Au��As��Aq�#Ap��Ao|�AnZAln�Ag�FAd�HAb�yAb�AbM�A`��A\�A[�;AZ��AY�FAXn�AU��AT��AS�hAP�yAPbAN�ANA�AL�AK�^AI��AHffAE�;AC��AA��A?�A<��A;�A:z�A9�wA8��A7��A6{A5�A4ffA3&�A2VA1S�A/+A-O�A+�A*�uA)C�A(�+A'O�A&9XA%\)A$�HA$ffA#ƨA"JA �Ax�Az�AK�AI�AO�A�A(�A7LA�AK�A&�A�HA�AĜA�AffAM�A$�AhsA��AA�mA��A��A��AA�A�-A��A�Al�A	S�A��A��A��AȴA\)A�!A��A��A�uAVAA ��@�l�@�@��H@��7@��9@�(�@�K�@�V@��@��+@��/@�1@�\@�@�V@�1'@�
=@�V@�bN@���@��@�ff@��#@�7L@�Ĝ@�r�@�9@���@�!@�r�@�p�@�@�G�@���@�ƨ@�|�@�l�@�33@��@�=q@�hs@�%@�j@���@ڰ!@�$�@�p�@�&�@ج@ו�@��H@�=q@��@�p�@�bN@�@Ѳ-@��@Ь@��m@�{@˶F@�@��@�Z@ǥ�@�l�@Ɨ�@�@�V@�z�@�C�@�E�@���@�x�@�X@���@�Ĝ@�r�@�1@��@�dZ@�K�@�"�@��@�-@���@�$�@�{@�hs@��@�A�@�A�@�bN@��9@�Ĝ@�Q�@��@�-@�j@�@�E�@�@���@�x�@�7L@���@�j@�z�@�z�@��@���@��@��w@�C�@��@��/@�r�@���@� �@���@�\)@�A�@���@�-@�&�@�E�@�ff@���@�n�@���@�Ĝ@�t�@���@��P@�S�@�\)@��;@�Ĝ@�Ĝ@�1@��@�ȴ@��y@���@�V@���@���@��@�`B@���@�=q@��\@��@�&�@�G�@�&�@�?}@���@���@�  @��!@�-@��h@���@�j@�Q�@�bN@�Q�@�A�@��@� �@�1@��;@���@�dZ@�33@��@�o@�
=@���@�@��7@���@���@� �@�  @��F@�l�@��H@�$�@��@���@���@���@��u@�9X@��@��@��@���@�l�@�l�@�C�@�+@�@�ȴ@�M�@�$�@�@�@�J@��#@��@�x�@�`B@�7L@��@��@��@� �@��@�t�@�;d@�@���@���@�-@��@��#@��7@�O�@�&�@��@���@��/@��D@�j@�Q�@��@��@�K�@�o@��R@�n�@�M�@�$�@�@���@��h@�7L@��@��@�V@���@���@�j@�9X@���@�t�@�C�@��y@��\@�v�@�5?@��@��@��T@��^@�hs@���@���@�1@�dZ@�;d@�o@��H@��R@���@���@��+@�@���@��@��T@���@��@�x�@�hs@�?}@�&�@�&�@��@���@��D@�A�@�  @��
@���@�S�@�o@�ȴ@�-@�@��h@�p�@�G�@�%@��`@�Ĝ@�I�@��m@���@���@�|�@�S�@�C�@��@��@���@�{@���@�/@���@�Ĝ@��j@���@��u@�z�@� �@�@~�y@~�R@~��@~��@~{@}V@|1@{��@{t�@{o@z�\@y�#@yx�@yX@yG�@y&�@x�9@xb@wl�@wK�@w+@w
=@v�y@vȴ@v��@v{@u��@u�@t�@t�@s"�@r��@r=q@q�#@q7L@p�9@p  @o�@o\)@o+@n�@n@m/@lz�@lI�@l9X@k��@k��@k"�@ko@j�@j�H@j��@jM�@j�@j�@i�#@ix�@i7L@i&�@h��@h��@h�9@g�w@g�P@gl�@f��@f{@e�@e/@d�j@dz�@dZ@c��@c��@c"�@c@b�\@b=q@a7L@`�@`bN@`bN@`1'@`b@_�@_�w@_��@_l�@^ȴ@^E�@]/@\z�@[�m@["�@Y��@Yx�@YX@Y%@X��@XbN@X  @W�w@W��@W�P@WK�@V�y@V�+@VV@V5?@VE�@VE�@VE�@V$�@V{@U��@Up�@T��@T(�@SC�@S33@So@R�@R��@R��@R~�@R=q@R�@P�`@P�@O�@O;d@NE�@M�@M�h@MO�@M?}@MV@L��@L�D@L(�@K�m@K�@K33@J��@I�@I��@I�7@IG�@H��@H�u@H  @G��@Gl�@G+@Fȴ@F��@F�+@Fv�@F5?@E�@E@E��@E�@EV@D�/@D�D@DZ@D9X@C��@C��@C�
@CS�@B~�@B-@A��@A��@A�^@A��@AX@A&�@A�@@�`@@��@@r�@@bN@@ �@?�@?�P@?|�@?l�@?K�@?
=@>v�@>ff@>$�@=�@<��@;�m@;o@:�H@:��@:�\@:n�@:M�@:=q@:-@:-@:J@9�@9��@9�^@9��@9��@9x�@97L@9�@9�@9%@8Ĝ@81'@7�;@7;d@7
=@5�@5`B@5/@4�@4�j@4�@4I�@4�@3�m@3��@3��@3�@3o@2�H@2��@2�!@2��@2��@2~�@2=q@2J@1�#@1��@1�^@1�7@1hs@1&�@1�@0��@0�u@0 �@/�w@/\)@/K�@/;d@/+@/�@.��@.{@-��@-@-�-@-��@,��@,�j@,��@,j@,j@,I�@+��@+ƨ@+��@+o@*~�@*^5@*=q@*J@)��@)X@(��@(�u@(bN@( �@(  @'�w@'�P@'�@&�@&��@&v�@&5?@&{@%�T@%��@%?}@$��@$�j@$�D@$j@$j@$j@$I�@$9X@$(�@#��@#ƨ@#t�@#S�@#33@#"�@#"�@#o@"�@"�H@"�H@"��@"�!@"~�@"n�@"M�@"�@!�#@!x�@!%@ ��@ �9@ �@  �@   @�;@�w@��@�P@l�@K�@
=@�@�R@�R@�+@ff@E�@@�@��@�h@`B@?}@�@�j@�@�D@j@j@Z@Z@(�@��@�m@��@t�@dZ@o@�H@��@~�@-@��@�@��@�7@�7@x�@X@7L@��@�u@A�@ �@�@��@�@�P@\)@;d@��@�@��@��@v�@5?@�T@�-@�h@p�@/@�@��@��@Z@I�@I�@I�@1@�F@�@o@�@��@^5@�@J@�@�^@��@hs@%@�`111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B
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
�}B
�}B
�}B
�}B
�}B
�}B
��B
��B
��B
��B
�HB�BC�B|�B�\B��B�3BĜB�B�B%B�B33B:^B>wBD�BG�BM�BM�BR�B\)BcTBk�Br�Bw�B{�B~�B�B�%B�JB��B��B��B��B��B��B��B��B�B�'B�3B�9B�FB�?B�XB�!B��B��B�^B��B�BB�`B�B�B��B��B��B��BB+B1BJB
=B+B��B��B�sB�BBŢB�XB�B��B��B�oB�bB�DB�7B�1B�%B}�Bz�Bp�BgmB]/BK�BF�BC�B9XB#�B\B%B��B�B��B�{B�%BdZBYB33B�BbB
��B
�;B
��B
�B
�VB
�B
|�B
jB
ZB
O�B
?}B
-B
 �B
{B
1B	��B	�B	�XB	��B	�bB	��B	��B	��B	r�B	{�B	x�B	q�B	gmB	VB	M�B	F�B	=qB	7LB	33B	1'B	.B	&�B	�B	uB	1B��B��B�B�NB�;B�5B�#B�B��B��BɺBŢBÖB�}B�dB�XB�9B�!B�B��B��B��B��B��B��B��B��B��B��B��B�bB�=B�+B�B�B�B� B}�B{�Bz�Bz�B{�Bx�By�Bz�Bz�B{�B{�B�B�PB�VB�bB��B��B��B��B�{B�JB�B�B�B�7B�\B�oB��B��B��B��B��B��B��B��B��B��B��B�!B�'B�'B�!B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�3B�LB�XB�LB�3B�}BƨBǮB��B��B��B��B��B�B�B�B�B�;B�HB�NB�mB�B�B�B�B��B��B��B��B��B��B��B	  B	  B	B	B	B	B	%B	+B	B	B	B	%B		7B	DB	VB	oB	{B	{B	{B	�B	�B	�B	�B	�B	�B	�B	�B	�B	"�B	%�B	(�B	,B	,B	+B	,B	-B	1'B	;dB	C�B	G�B	E�B	B�B	>wB	<jB	?}B	>wB	>wB	>wB	>wB	?}B	B�B	C�B	D�B	H�B	Q�B	\)B	\)B	^5B	l�B	q�B	p�B	o�B	q�B	s�B	z�B	q�B	hsB	l�B	�B	�VB	�hB	��B	��B	��B	��B	�hB	�\B	�7B	�1B	�JB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�?B	�?B	�9B	�LB	�^B	�dB	�wB	�jB	�RB	�LB	�LB	�RB	�}B	�wB	�wB	��B	��B	��B	��B	B	ÖB	ŢB	ƨB	ȴB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	�B	�B	�
B	�
B	�
B	�B	�)B	�5B	�TB	�ZB	�mB	�mB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
%B
%B
%B
1B

=B

=B

=B

=B

=B
DB
DB
DB
DB
DB
DB
DB
DB
VB
VB
\B
hB
hB
hB
bB
VB
VB
PB
PB
PB
VB
\B
\B
VB
VB
\B
bB
bB
hB
hB
oB
uB
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
"�B
"�B
"�B
"�B
#�B
#�B
#�B
$�B
$�B
%�B
%�B
%�B
%�B
&�B
&�B
'�B
'�B
'�B
'�B
(�B
(�B
)�B
)�B
)�B
)�B
)�B
+B
+B
+B
+B
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
-B
-B
.B
.B
/B
/B
/B
0!B
0!B
0!B
0!B
0!B
1'B
1'B
1'B
1'B
2-B
33B
33B
33B
33B
33B
33B
33B
33B
33B
49B
5?B
5?B
6FB
6FB
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
9XB
9XB
;dB
:^B
;dB
;dB
<jB
=qB
>wB
>wB
?}B
?}B
?}B
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
A�B
B�B
B�B
B�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
D�B
E�B
E�B
E�B
E�B
F�B
F�B
F�B
F�B
F�B
G�B
G�B
G�B
G�B
G�B
H�B
H�B
H�B
H�B
H�B
H�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
K�B
K�B
K�B
K�B
L�B
L�B
L�B
L�B
M�B
M�B
M�B
M�B
M�B
M�B
N�B
N�B
N�B
O�B
O�B
P�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
R�B
R�B
R�B
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
VB
VB
VB
VB
VB
VB
W
B
W
B
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
XB
XB
YB
YB
YB
YB
YB
YB
YB
YB
YB
YB
ZB
ZB
[#B
[#B
[#B
[#B
[#B
[#B
[#B
\)B
\)B
]/B
\)B
\)B
]/B
]/B
]/B
]/B
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
`BB
`BB
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
cTB
cTB
dZB
dZB
dZB
dZB
e`B
e`B
dZB
e`B
e`B
e`B
e`B
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
iyB
iyB
iyB
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
k�B
k�B
k�B
k�B
k�B
k�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
m�B
m�B
m�B
m�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
o�B
o�B
o�B
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
q�B
q�B
q�B
q�B
q�B
q�B
q�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
s�B
s�B
s�B
s�B
t�B
t�B
t�B
t�B
u�B
u�B
u�B
u�B
v�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B
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
�}B
�}B
�}B
�}B
�}B
��B
��B
��B
�B
�B
�:BBE�B}VB�bB��B�B�9B��B�|BB�B4�B;JB?}BE�BI7BN�BO(BT{B]�BeBl�Bs�BxlB|jB�B��B�_B�<B�7B��B��B�zB�RB�_B��B�6B��B�-B�TB��B��B�LB��B�B��B�QB��B�B�-B�B�)B�vB�?B��B��B��BtBB	lBPB�B	7BB�$B�qB��BɠB��B��B�@B�5B��B� B��B��B��B�_B.B|�Br-BjB_�BL�BG�BE�B<�B&fBB	RBAB�B��B��B�rBg�B^5B6FB�B{B B
��B
�BB
��B
�HB
��B
�iB
l�B
\B
RoB
BB
/OB
"hB
B

#B	�"B	�B	��B	��B	�4B	��B	�XB	�B	t�B	}VB	zxB	s�B	j0B	W�B	O�B	IlB	>�B	8�B	4nB	2�B	/�B	)_B	�B	�B	B	�B��B�B�@B�BB�VBܬB�B�$B�(B��B�EB��B�UB�(B��B�B�B��B�6B��B�`B��B��B��B�B��B�pB�=B�B�B��B��B�9B��B�uB��B~�B|PB{�B|6B}<ByXBz*B{0B{dB}B|�B��B��B��B� B��B�~B��B�7B��B��B��B�B��B��B��B�B�KB��B��B��B�$B��B�'B��B�@B�@B��B��B��B��B�B�/B��B�B��B��B��B�\B�jB��B��B��B�*B��B�ZB�`B�zB�XB�OB�MB��B��B�RB��B�}B�B�KB�DB�B�B�[B�MB֡B֡B�yBںB�'B�B��B��B��B�B�iB�9B�?B�+B�FB��B��B��B�wB	 iB	 �B	{B	tB	�B	B	B	�B	mB	�B	�B	�B		�B	B	B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	B	# B	%�B	)DB	,�B	,�B	+6B	,B	-B	1AB	;B	DB	H�B	FtB	C�B	?}B	=B	?�B	>�B	>�B	>�B	>�B	?�B	B�B	C�B	DgB	HfB	R:B	]IB	\CB	]�B	l�B	q�B	qB	o�B	rB	t�B	|�B	r�B	g�B	j�B	�UB	�"B	�4B	�xB	�+B	�_B	��B	��B	�}B	�lB	�1B	��B	�B	��B	�)B	�7B	��B	��B	��B	�B	�HB	�B	��B	��B	��B	��B	�B	��B	��B	�TB	�fB	�xB	�B	��B	�<B	�>B	��B	��B	��B	�B	��B	�wB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�NB	�hB	�NB	�NB	�@B	�FB	�,B	�FB	�MB	�mB	�gB	�,B	�mB	�yB	�YB	�YB	�YB	�7B	�CB	�OB	�B	�B	�mB	�B	�B	�B	�B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	�%B	�B	��B	�2B	�	B	�	B	�B	�B	�JB	�0B	��B	�B	�B	�"B	�"B	�B	�B	�(B	�B	�B	�(B	�cB	�HB	�HB
 iB
;B
GB
AB
aB
-B
MB
YB
?B
?B
KB

XB

�B

rB

rB

�B
xB
�B
�B
xB
^B
xB
xB
DB
�B
�B
�B
�B
�B
�B
�B
�B
pB
�B
jB
jB
VB
�B
�B
pB
pB
vB
�B
}B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
	B
)B
�B
�B
�B
B
�B
�B
�B
�B
�B
�B
 B
!B
!B
!�B
!�B
!�B
!�B
#B
"�B
#B
#B
$B
$B
$&B
%,B
%,B
&B
&B
&2B
&2B
'B
'B
(
B
(
B
($B
(>B
)_B
)*B
*B
*B
*B
*KB
*0B
+B
+6B
+6B
+B
+6B
+6B
+B
+B
,"B
,"B
,"B
,=B
,=B
,"B
,WB
-CB
-CB
.cB
.cB
/iB
/OB
/OB
0;B
0;B
0UB
0oB
0UB
1AB
1[B
1vB
1vB
2|B
3MB
33B
3hB
3hB
3MB
3MB
3MB
3MB
3�B
4�B
5�B
5�B
6�B
6�B
7�B
7�B
7fB
7�B
8�B
8lB
8lB
8lB
9rB
9rB
9�B
9�B
;B
:xB
;B
;dB
<jB
=qB
>�B
>�B
?�B
?�B
?�B
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
A�B
B�B
B�B
B�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
D�B
E�B
E�B
E�B
E�B
F�B
F�B
F�B
F�B
F�B
G�B
G�B
G�B
G�B
G�B
H�B
H�B
H�B
H�B
H�B
H�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
J�B
J�B
J�B
J�B
KB
K)B
J�B
J�B
J�B
J�B
J�B
J�B
K�B
K�B
K�B
K�B
L�B
L�B
MB
MB
M�B
M�B
M�B
NB
M�B
N"B
N�B
O(B
O(B
P.B
PHB
Q4B
RB
RB
R B
R B
R B
Q�B
RB
Q�B
RB
SB
SB
R�B
R�B
R�B
SB
SB
SB
R�B
SB
S&B
S&B
TFB
T,B
TFB
T{B
U2B
VB
VB
VB
VB
VB
VB
W$B
W$B
W$B
W$B
W?B
W$B
X+B
X+B
XB
XB
XEB
X+B
YKB
YKB
YKB
Y1B
YKB
YKB
Y1B
Y1B
Y1B
YeB
ZkB
ZkB
[WB
[#B
[=B
[#B
[WB
[=B
[�B
\]B
\CB
]IB
\]B
\]B
]IB
]IB
]IB
]/B
^jB
^OB
^jB
^jB
^jB
_pB
_VB
_VB
_VB
_VB
`vB
`vB
a|B
abB
abB
abB
a|B
bhB
b�B
bhB
bhB
cnB
cnB
c�B
cnB
cnB
c�B
dtB
d�B
dtB
dtB
e`B
e`B
dtB
ezB
ezB
e�B
e�B
ezB
ezB
f�B
f�B
ffB
ffB
f�B
ffB
f�B
ffB
f�B
f�B
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
i�B
i�B
i�B
iyB
i�B
i�B
i�B
j�B
j�B
jB
j�B
j�B
j�B
j�B
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
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
m�B
m�B
m�B
m�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
o�B
o�B
o�B
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
q�B
q�B
q�B
q�B
q�B
q�B
q�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
s�B
s�B
s�B
s�B
t�B
t�B
t�B
t�B
u�B
u�B
u�B
u�B
v�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.05(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201704170032282017041700322820170417003228201806221311592018062213115920180622131159201804050713132018040507131320180405071313  JA  ARFMdecpA19c                                                                20170413093510  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20170413003534  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20170413003536  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20170413003536  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20170413003537  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20170413003537  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20170413003537  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20170413003537  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20170413003537  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20170413003537                      G�O�G�O�G�O�                JA  ARUP                                                                        20170413010746                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20170413153704  CV  JULD            G�O�G�O�F���                JM  ARCAJMQC2.0                                                                 20170416153228  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20170416153228  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404221313  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622041159  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200116211517                      G�O�G�O�G�O�                