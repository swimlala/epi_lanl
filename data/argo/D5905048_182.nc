CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2017-11-21T00:35:29Z creation;2017-11-21T00:35:33Z conversion to V3.1;2019-12-19T07:52:06Z update;     
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
_FillValue                 �  I@   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M,   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p`   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  tL   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �l   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �D   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �x   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �x   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �x   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �x   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �    HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �H   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �X   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �\   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �l   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �p   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �t   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �xArgo profile    3.1 1.2 19500101000000  20171121003529  20200116221516  5905048                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0577_182                     2C  D   NAVIS_A                         0577                            ARGO 102115                     863 @�6�"�1   @�6�DDD�@4\�����d��Fs��1   GPS     A   A   B   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D ��D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DMfDM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ Dؼ�D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @���@�ff@�ffA33A?33A_33A33A���A���A���A���Aϙ�Aߙ�AA���B��B��B��B��B'��B/��B7��B?��BG��BO��BW��B_��Bg��Bo��Bw��B��B��fB��fB��fB��B��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fC�3C�3C�3C�3C	�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C!�3C#�3C%�3C'�3C)�3C+�3C-�3C/�3C1�3C3�3C5�3C7�3C9�3C;�3C=�3C?�3CA�3CC�3CE�3CG�3CI�3CK�3CM�3CO�3CQ�3CS�3CU�3CW�3CY�3C[�3C]�3C_�3Ca�3Cc�3Ce�3Cg�3Ci�3Ck�3Cm�3Co�3Cq�3Cs�3Cu�3Cw�3Cy�3C{�3C}�3C�3C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D |�D �fD|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D	|�D	��D
|�D
��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D |�D ��D!|�D!��D"|�D"��D#|�D#��D$|�D$��D%|�D%��D&|�D&��D'|�D'��D(|�D(��D)|�D)��D*|�D*��D+|�D+��D,|�D,��D-|�D-��D.|�D.��D/|�D/��D0|�D0��D1|�D1��D2|�D2��D3|�D3��D4|�D4��D5|�D5��D6|�D6��D7|�D7��D8|�D8��D9|�D9��D:|�D:��D;|�D;��D<|�D<��D=|�D=��D>|�D>��D?|�D?��D@|�D@��DA|�DA��DB|�DB��DC|�DC��DD|�DD��DE|�DE��DF|�DF��DG|�DG��DH|�DH��DI|�DI��DJ|�DJ��DK|�DK��DL|�DM3DM|�DM��DN|�DN��DO|�DO��DP|�DP��DQ|�DQ��DR|�DR��DS|�DS��DT|�DT��DU|�DU��DV|�DV��DW|�DW��DX|�DX��DY|�DY��DZ|�DZ��D[|�D[��D\|�D\��D]|�D]��D^|�D^��D_|�D_��D`|�D`��Da|�Da��Db|�Db��Dc|�Dc��Dd|�Dd��De|�De��Df|�Df��Dg|�Dg��Dh|�Dh��Di|�Di��Dj|�Dj��Dk|�Dk��Dl|�Dl��Dm|�Dm��Dn|�Dn��Do|�Do��Dp|�Dp��Dq|�Dq��Dr|�Dr��Ds|�Ds��Dt|�Dt��Du|�Du��Dv|�Dv��Dw|�Dw��Dx|�Dx��Dy|�Dy��Dz|�Dz��D{|�D{��D||�D|��D}|�D}��D~|�D~��D|�D��D�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�{3D��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD¾fD��fD�>fD�~fDþfD��fD�>fD�~fDľfD��fD�>fD�~fDžfD��fD�>fD�~fDƾfD��fD�>fD�~fDǾfD��fD�>fD�~fDȾfD��fD�>fD�~fDɾfD��fD�>fD�~fDʾfD��fD�>fD�~fD˾fD��fD�>fD�~fD̾fD��fD�>fD�~fD;fD��fD�>fD�~fDξfD��fD�>fD�~fDϾfD��fD�>fD�~fDоfD��fD�>fD�~fDѾfD��fD�>fD�~fDҾfD��fD�>fD�~fDӾfD��fD�>fD�~fDԾfD��fD�>fD�~fDվfD��fD�>fD�~fD־fD��fD�>fD�~fD׾fD��fD�>fD�~fDػ3D��fD�>fD�~fDپfD��fD�>fD�~fDھfD��fD�>fD�~fD۾fD��fD�>fD�~fDܾfD��fD�>fD�~fDݾfD��fD�>fD�~fD޾fD��fD�>fD�~fD߾fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD��fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD��311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�?}A�?}A�=qA�=qA�A�A�A�A�C�A�=qA�9XA�A�A�7LA�G�A�I�A�I�A�I�A�K�A�M�A�M�A�G�A�G�A�E�A�G�A�M�A�M�A�Q�A�S�A�S�A�XA�`BA�hsA؇+A�K�A�{A��A�bNA�+A��A�x�A�dZA��A�v�A���A�/A�7LA�XA�&�A�"�A���A�S�A��FA�bNA���A�t�A��A��wA� �A��mA��jA�K�A�JA��\A��A�A�JA� �A�VA�S�A���A�;dA��wA���A���A�A��mA�Q�A�bA��mA��RA���A��A��A�E�A��TA���A��;A�C�A���A�r�A�E�A�%A�
=A�/A�z�A���A�
=A���A�~�A���A�^5A�Q�A�-A��;A��FA��uA��A�M�A���A���A�G�A��A��7A��jA~M�A|�9Az=qAxZAv��Av{At1'Aq�
Ao�TAmt�Ak�PAj�uAjI�Ah(�AeƨAeG�Ad��AdbAb{A`��A_33A]��A\�AZffAXffAWXAVjAU33AS��ARA�AQ7LAO��AN�AN$�ALn�AL  AKdZAG��ADĜAD~�ADI�AC�^AB��AA�hAA�A@5?A>5?A<�RA;p�A:~�A9O�A8��A6��A5l�A41'A37LA2�A1A.n�A,��A,bA+�-A+;dA)��A(�A&�yA$M�A"�DA!C�A bNA A�A =qA��A��A�mA��A��A$�A�HAv�A�#A`BA7LAn�AAXA�A�RA$�A�A�HAI�A�A��A�hAXAVA�A
�A��AA�AXA��A^5AbAt�A @���@���@�1@�J@��j@�z�@��F@�{@��u@�w@�@�{@�  @��^@�!@�X@�(�@柾@�`B@�ƨ@�-@�Q�@��@ڸR@���@� �@׶F@�S�@��@��@Ӿw@ӶF@��@љ�@��@��@θR@�{@�&�@�  @˝�@�o@�V@ɉ7@�x�@ȣ�@�l�@�v�@�G�@�r�@�l�@�"�@���@�ff@�5?@��#@���@��@���@��@���@���@��m@�"�@�v�@�J@�x�@�%@�Z@�(�@���@�S�@�~�@�@���@��7@��@��/@��u@�Z@�1@���@��F@��@�"�@��!@��#@�7L@��@�%@���@�bN@� �@��P@�+@��@�=q@�p�@�?}@��@�Ĝ@��@��
@��@�33@�@���@�M�@���@��h@��@�r�@�I�@�A�@��@��@�dZ@��y@��R@�n�@��T@�7L@��@��u@�Q�@�b@��;@��w@���@�|�@�dZ@�S�@�K�@�C�@�;d@�o@���@�5?@���@�O�@��@�%@��j@��D@�r�@�j@�9X@�  @�l�@�C�@��@��H@�ȴ@���@�E�@�{@��@��-@�x�@�7L@�Ĝ@�j@�Z@�I�@�I�@�1'@�(�@��@�b@�1'@�A�@�I�@�Q�@�bN@���@��j@��@�r�@�Q�@�I�@�I�@�9X@���@��@��@�t�@�\)@��@���@��R@��+@�V@�$�@���@��-@���@��7@�`B@�%@�Ĝ@���@�z�@�j@�9X@�(�@� �@���@��@��P@�\)@��@��y@��R@���@�~�@�n�@�M�@�J@���@��h@�`B@�X@�G�@�&�@�Ĝ@�z�@�Z@�I�@�(�@���@��
@���@�+@��y@��@���@��\@�ff@�^5@�5?@���@�hs@��@��@�/@�/@�7L@�&�@��9@�1'@�1@���@���@�C�@�@���@��y@�E�@��@�@��T@���@�7L@���@��@�bN@� �@���@��y@�n�@��@���@���@�p�@�V@��j@��@���@��u@�bN@��F@��P@���@��F@���@�\)@�33@��H@�ff@�=q@�V@�M�@�E�@��@��@���@�@��^@���@�`B@��@���@�Ĝ@��@��w@��P@�C�@�;d@�;d@�
=@��H@��\@�$�@��@��@���@�`B@�7L@�O�@��7@�`B@���@���@���@���@��@��@�Z@�9X@�P@|�@\)@~��@~��@~v�@}�T@}/@|��@|�/@|z�@{�m@{t�@{"�@z��@zM�@z=q@y��@yX@x��@w��@w\)@w\)@wl�@wK�@v��@u��@up�@t�@t�@s��@st�@s33@r�@r��@r��@r�!@r��@r^5@q��@q7L@p�u@o��@ol�@o�@n�R@nV@m�T@mV@l��@l�D@lz�@k��@k��@kdZ@j�@j^5@i�#@iG�@h��@h�9@hr�@h1'@h  @gK�@f��@fv�@f5?@ep�@d�@d�j@d��@dj@d�@c�
@cƨ@c��@cdZ@cS�@c33@c@b�@bn�@a��@aX@a%@`��@`r�@`A�@`A�@`1'@_�@_+@^�@^��@^V@]��@]�@]/@]V@\�/@\�@\�D@\j@\I�@\(�@[��@["�@[@[@Z��@Z��@Z�\@Z~�@Z-@Y�#@Y��@YX@Y7L@Y&�@Y�@X��@X��@X��@X�@XbN@Xb@W��@W+@V�@V��@Vv�@VV@U�T@U��@U�@T��@T�@Tj@Tj@TI�@T(�@T(�@T�@T1@S�
@S�@SS�@SS�@S"�@S@R��@RM�@Q��@Q&�@P��@PA�@O�;@O�;@O��@O\)@O
=@Nȴ@Nff@N$�@N@M�h@M`B@MO�@MV@L�D@L�@K33@J~�@I��@I�@H��@G�w@G|�@G\)@G+@F�+@FE�@E�@E�-@E?}@D�j@D9X@C��@C�
@C�@CC�@B�@C@B~�@BJ@A�@A%@@�9@@�@@ �@?�w@?l�@?l�@?;d@?
=@>��@>�y@>�y@>��@>V@=�@=@=�h@=?}@=/@=�@=V@<��@<��@<��@<I�@;ƨ@;�@;t�@;dZ@;dZ@;dZ@;dZ@;S�@:��@:~�@:^5@:^5@9��@9��@9�7@8�`@8Q�@7�w@7K�@7
=@7
=@6��@6��@6ff@6{@5�@5p�@5�@4��@4�D@4j@4(�@3�@333@2��@1�@1��@1�7@1hs@17L@1&�@0�`@01'@0  @/�;@/��@/|�@/;d@.�+@.5?@-�@-��@-�-@-��@-�@-�@,�/@,z�@,j@,Z@+�
@+��@+�@+dZ@+"�@+"�@+o@+@*��@*��@*~�@)��@)X@)X@)%@(�u@(Q�@(b@'��@';d@'�@'
=@&�@&ȴ@&�@&�R@&��@&v�@&$�@%��@%�@%O�@%/@%�@$��@$�/@$��@$�j@$�D@$9X@#�m@#�@#C�@#o@#o@"�@"�\@"M�@"J@!�@!��@!x�@!&�@ �`@ �@ bN@��@|�@l�@\)@K�@�y@�R@��@�+@E�@{@��@�@O�@�@�@��@�j@�D@Z@��@�F@��@�@C�@@��@�!@�\@n�@=q@�@��@��@x�@G�@�@��@��@�9@��@r�@Q�@1'@ �@�@��@\)@
=@ȴ@��@v�@5?@�@@�-@�@`B@?}@V@�@��@��@�D@z�@j@(�@��@ƨ@33@o@o@�!@-@��@�@��@��@hs@X@X@X@7L@�`@�9@�u@�@bN@A�@��@��@\)@;d@+@
=@ȴ@��@��@�+@E�@E�@E�@E�@$�@�T@�h@p�@O�@/@�@�@�@�D@z�@Z11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�?}A�?}A�=qA�=qA�A�A�A�A�C�A�=qA�9XA�A�A�7LA�G�A�I�A�I�A�I�A�K�A�M�A�M�A�G�A�G�A�E�A�G�A�M�A�M�A�Q�A�S�A�S�A�XA�`BA�hsA؇+A�K�A�{A��A�bNA�+A��A�x�A�dZA��A�v�A���A�/A�7LA�XA�&�A�"�A���A�S�A��FA�bNA���A�t�A��A��wA� �A��mA��jA�K�A�JA��\A��A�A�JA� �A�VA�S�A���A�;dA��wA���A���A�A��mA�Q�A�bA��mA��RA���A��A��A�E�A��TA���A��;A�C�A���A�r�A�E�A�%A�
=A�/A�z�A���A�
=A���A�~�A���A�^5A�Q�A�-A��;A��FA��uA��A�M�A���A���A�G�A��A��7A��jA~M�A|�9Az=qAxZAv��Av{At1'Aq�
Ao�TAmt�Ak�PAj�uAjI�Ah(�AeƨAeG�Ad��AdbAb{A`��A_33A]��A\�AZffAXffAWXAVjAU33AS��ARA�AQ7LAO��AN�AN$�ALn�AL  AKdZAG��ADĜAD~�ADI�AC�^AB��AA�hAA�A@5?A>5?A<�RA;p�A:~�A9O�A8��A6��A5l�A41'A37LA2�A1A.n�A,��A,bA+�-A+;dA)��A(�A&�yA$M�A"�DA!C�A bNA A�A =qA��A��A�mA��A��A$�A�HAv�A�#A`BA7LAn�AAXA�A�RA$�A�A�HAI�A�A��A�hAXAVA�A
�A��AA�AXA��A^5AbAt�A @���@���@�1@�J@��j@�z�@��F@�{@��u@�w@�@�{@�  @��^@�!@�X@�(�@柾@�`B@�ƨ@�-@�Q�@��@ڸR@���@� �@׶F@�S�@��@��@Ӿw@ӶF@��@љ�@��@��@θR@�{@�&�@�  @˝�@�o@�V@ɉ7@�x�@ȣ�@�l�@�v�@�G�@�r�@�l�@�"�@���@�ff@�5?@��#@���@��@���@��@���@���@��m@�"�@�v�@�J@�x�@�%@�Z@�(�@���@�S�@�~�@�@���@��7@��@��/@��u@�Z@�1@���@��F@��@�"�@��!@��#@�7L@��@�%@���@�bN@� �@��P@�+@��@�=q@�p�@�?}@��@�Ĝ@��@��
@��@�33@�@���@�M�@���@��h@��@�r�@�I�@�A�@��@��@�dZ@��y@��R@�n�@��T@�7L@��@��u@�Q�@�b@��;@��w@���@�|�@�dZ@�S�@�K�@�C�@�;d@�o@���@�5?@���@�O�@��@�%@��j@��D@�r�@�j@�9X@�  @�l�@�C�@��@��H@�ȴ@���@�E�@�{@��@��-@�x�@�7L@�Ĝ@�j@�Z@�I�@�I�@�1'@�(�@��@�b@�1'@�A�@�I�@�Q�@�bN@���@��j@��@�r�@�Q�@�I�@�I�@�9X@���@��@��@�t�@�\)@��@���@��R@��+@�V@�$�@���@��-@���@��7@�`B@�%@�Ĝ@���@�z�@�j@�9X@�(�@� �@���@��@��P@�\)@��@��y@��R@���@�~�@�n�@�M�@�J@���@��h@�`B@�X@�G�@�&�@�Ĝ@�z�@�Z@�I�@�(�@���@��
@���@�+@��y@��@���@��\@�ff@�^5@�5?@���@�hs@��@��@�/@�/@�7L@�&�@��9@�1'@�1@���@���@�C�@�@���@��y@�E�@��@�@��T@���@�7L@���@��@�bN@� �@���@��y@�n�@��@���@���@�p�@�V@��j@��@���@��u@�bN@��F@��P@���@��F@���@�\)@�33@��H@�ff@�=q@�V@�M�@�E�@��@��@���@�@��^@���@�`B@��@���@�Ĝ@��@��w@��P@�C�@�;d@�;d@�
=@��H@��\@�$�@��@��@���@�`B@�7L@�O�@��7@�`B@���@���@���@���@��@��@�Z@�9X@�P@|�@\)@~��@~��@~v�@}�T@}/@|��@|�/@|z�@{�m@{t�@{"�@z��@zM�@z=q@y��@yX@x��@w��@w\)@w\)@wl�@wK�@v��@u��@up�@t�@t�@s��@st�@s33@r�@r��@r��@r�!@r��@r^5@q��@q7L@p�u@o��@ol�@o�@n�R@nV@m�T@mV@l��@l�D@lz�@k��@k��@kdZ@j�@j^5@i�#@iG�@h��@h�9@hr�@h1'@h  @gK�@f��@fv�@f5?@ep�@d�@d�j@d��@dj@d�@c�
@cƨ@c��@cdZ@cS�@c33@c@b�@bn�@a��@aX@a%@`��@`r�@`A�@`A�@`1'@_�@_+@^�@^��@^V@]��@]�@]/@]V@\�/@\�@\�D@\j@\I�@\(�@[��@["�@[@[@Z��@Z��@Z�\@Z~�@Z-@Y�#@Y��@YX@Y7L@Y&�@Y�@X��@X��@X��@X�@XbN@Xb@W��@W+@V�@V��@Vv�@VV@U�T@U��@U�@T��@T�@Tj@Tj@TI�@T(�@T(�@T�@T1@S�
@S�@SS�@SS�@S"�@S@R��@RM�@Q��@Q&�@P��@PA�@O�;@O�;@O��@O\)@O
=@Nȴ@Nff@N$�@N@M�h@M`B@MO�@MV@L�D@L�@K33@J~�@I��@I�@H��@G�w@G|�@G\)@G+@F�+@FE�@E�@E�-@E?}@D�j@D9X@C��@C�
@C�@CC�@B�@C@B~�@BJ@A�@A%@@�9@@�@@ �@?�w@?l�@?l�@?;d@?
=@>��@>�y@>�y@>��@>V@=�@=@=�h@=?}@=/@=�@=V@<��@<��@<��@<I�@;ƨ@;�@;t�@;dZ@;dZ@;dZ@;dZ@;S�@:��@:~�@:^5@:^5@9��@9��@9�7@8�`@8Q�@7�w@7K�@7
=@7
=@6��@6��@6ff@6{@5�@5p�@5�@4��@4�D@4j@4(�@3�@333@2��@1�@1��@1�7@1hs@17L@1&�@0�`@01'@0  @/�;@/��@/|�@/;d@.�+@.5?@-�@-��@-�-@-��@-�@-�@,�/@,z�@,j@,Z@+�
@+��@+�@+dZ@+"�@+"�@+o@+@*��@*��@*~�@)��@)X@)X@)%@(�u@(Q�@(b@'��@';d@'�@'
=@&�@&ȴ@&�@&�R@&��@&v�@&$�@%��@%�@%O�@%/@%�@$��@$�/@$��@$�j@$�D@$9X@#�m@#�@#C�@#o@#o@"�@"�\@"M�@"J@!�@!��@!x�@!&�@ �`@ �@ bN@��@|�@l�@\)@K�@�y@�R@��@�+@E�@{@��@�@O�@�@�@��@�j@�D@Z@��@�F@��@�@C�@@��@�!@�\@n�@=q@�@��@��@x�@G�@�@��@��@�9@��@r�@Q�@1'@ �@�@��@\)@
=@ȴ@��@v�@5?@�@@�-@�@`B@?}@V@�@��@��@�D@z�@j@(�@��@ƨ@33@o@o@�!@-@��@�@��@��@hs@X@X@X@7L@�`@�9@�u@�@bN@A�@��@��@\)@;d@+@
=@ȴ@��@��@�+@E�@E�@E�@E�@$�@�T@�h@p�@O�@/@�@�@�@�D@z�@Z11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B��B�B�B-B#�BJB�B-B'�B7LB-B>wBI�BF�B<jB]/Bz�B�B|�B� B}�B|�Bu�BYBm�BgmBiyBw�Bx�Bo�BaHBZBdZB`BB\)BdZBdZBffBffB\)BR�BE�B=qB7LB,B�B{B	7B�B�/BƨB�qB�LB��B��B}�B�\B�bB�1Bu�BS�BD�BA�BB�B7LB33B!�BB
�sB
�;B
�TB
�BB
�
B
�jB
��B
�B
�=B
}�B
x�B
jB
I�B
-B
-B
�B
�B
	7B
%B	��B	�HB	�
B	ĜB	�wB	�dB	�XB	�B	��B	��B	��B	�{B	�B	{�B	p�B	m�B	^5B	R�B	F�B	C�B	@�B	7LB	)�B	#�B	#�B	�B	�B	oB	B	%B��B�HB��B�B�B�`B�)B��B�B��B�wBÖB�wBB�qB�jB�B��B�B��B�B��B�VB��B��B��B��B�{B�JB�VB|�Bx�B�B~�B�B�Bp�By�B�B~�Bx�Bv�Bu�By�B~�B|�B� Bz�B|�B� B�B~�B|�B|�Bz�By�By�Bs�BgmBn�BiyB]/BVBK�B.BI�BO�BK�BL�BH�B=qBH�BH�BO�BK�BL�BT�BP�BK�BM�BP�BP�BP�BI�BF�BH�BQ�BW
BR�BT�BR�BS�BS�BQ�BR�B_;BjBm�Bk�BgmBgmBt�Bu�Bq�Bo�Bu�Bt�Br�By�By�Bz�B�B�B�B�B�1B�B�B�+B�7B�VB�hB��B��B��B��B��B��B��B��B��B��B��B��B�B�-B�FB�LB�jB�qBBB��BÖBȴB��B��B��B��B�B�B�B�)B�5B�5B�5B�BB�NB�yB�B�B�B��B��B��B��B��B	  B	B	DB	JB	VB	\B	uB	�B	�B	�B	�B	�B	�B	"�B	#�B	(�B	.B	0!B	/B	1'B	2-B	33B	8RB	8RB	:^B	<jB	B�B	D�B	H�B	L�B	N�B	Q�B	R�B	S�B	T�B	VB	W
B	W
B	W
B	VB	T�B	XB	[#B	`BB	dZB	ffB	ffB	jB	l�B	m�B	l�B	n�B	o�B	u�B	y�B	y�B	z�B	{�B	|�B	� B	�B	�B	�B	�B	�%B	�DB	�\B	�bB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�3B	�?B	�?B	�9B	�-B	�9B	�^B	�jB	�dB	�jB	�wB	��B	��B	B	ÖB	ĜB	ǮB	ȴB	ȴB	ȴB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�B	�B	�B	�B	�B	�)B	�#B	�/B	�/B	�)B	�)B	�5B	�BB	�HB	�HB	�HB	�TB	�TB	�TB	�mB	�yB	�yB	�yB	�yB	�B	�yB	�yB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
  B
B
1B
	7B
1B
1B

=B
	7B
+B
	7B
DB
JB
PB
VB
VB
\B
bB
hB
bB
bB
hB
oB
oB
bB
oB
{B
{B
�B
�B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
 �B
�B
�B
!�B
!�B
"�B
!�B
"�B
$�B
$�B
%�B
%�B
%�B
$�B
#�B
#�B
"�B
#�B
#�B
%�B
&�B
%�B
%�B
%�B
%�B
&�B
(�B
(�B
'�B
'�B
(�B
'�B
&�B
'�B
'�B
)�B
+B
+B
+B
+B
)�B
)�B
,B
,B
+B
+B
,B
,B
,B
,B
,B
-B
-B
-B
.B
-B
-B
.B
-B
-B
.B
/B
0!B
0!B
1'B
1'B
1'B
/B
/B
1'B
1'B
1'B
0!B
2-B
2-B
49B
49B
49B
49B
49B
49B
49B
33B
49B
5?B
6FB
5?B
6FB
6FB
6FB
5?B
5?B
5?B
6FB
7LB
7LB
7LB
7LB
7LB
6FB
6FB
6FB
6FB
6FB
6FB
7LB
8RB
8RB
8RB
7LB
8RB
7LB
8RB
:^B
9XB
:^B
:^B
:^B
;dB
:^B
:^B
:^B
9XB
:^B
;dB
:^B
:^B
:^B
8RB
9XB
:^B
;dB
:^B
;dB
=qB
<jB
<jB
;dB
<jB
<jB
<jB
<jB
<jB
=qB
=qB
<jB
;dB
<jB
:^B
;dB
;dB
=qB
>wB
<jB
?}B
?}B
?}B
>wB
?}B
?}B
?}B
?}B
?}B
@�B
A�B
A�B
A�B
B�B
C�B
D�B
C�B
C�B
D�B
C�B
C�B
E�B
D�B
D�B
F�B
G�B
H�B
H�B
I�B
J�B
J�B
I�B
I�B
I�B
J�B
K�B
J�B
L�B
L�B
L�B
L�B
L�B
L�B
K�B
K�B
L�B
M�B
M�B
M�B
M�B
M�B
L�B
K�B
L�B
M�B
M�B
L�B
K�B
L�B
J�B
K�B
L�B
N�B
O�B
Q�B
Q�B
P�B
P�B
O�B
P�B
O�B
O�B
P�B
Q�B
Q�B
Q�B
O�B
Q�B
P�B
P�B
T�B
T�B
T�B
T�B
T�B
S�B
R�B
VB
W
B
VB
VB
VB
T�B
W
B
YB
YB
ZB
ZB
ZB
YB
YB
YB
[#B
[#B
YB
[#B
\)B
\)B
[#B
\)B
\)B
\)B
[#B
[#B
[#B
ZB
[#B
]/B
\)B
[#B
]/B
]/B
]/B
]/B
_;B
`BB
_;B
`BB
`BB
`BB
_;B
_;B
^5B
^5B
aHB
aHB
aHB
bNB
bNB
bNB
bNB
bNB
aHB
aHB
aHB
aHB
bNB
cTB
dZB
cTB
bNB
cTB
cTB
dZB
cTB
dZB
cTB
dZB
dZB
e`B
dZB
e`B
gmB
gmB
gmB
ffB
gmB
hsB
gmB
gmB
gmB
gmB
gmB
hsB
hsB
hsB
iyB
iyB
hsB
iyB
hsB
iyB
jB
jB
iyB
jB
jB
k�B
k�B
k�B
k�B
k�B
k�B
jB
k�B
l�B
l�B
l�B
l�B
m�B
m�B
l�B
m�B
m�B
m�B
m�B
l�B
m�B
m�B
n�B
n�B
n�B
n�B
n�B
o�B
p�B
o�B
p�B
p�B
p�B
p�B
p�B
p�B
q�B
q�B
q�B
p�B
p�B
p�B
o�B
q�B
r�B
q�B
p�B
r�B
s�B
s�B
s�B
s�B
t�B
t�B
t�B
s�B
s�B
s�B
t�B
t�B
t�B
t�B
s�B
t�B
t�B
u�B
v�B
u�B
u�B
v�B
v�B
v�B
v�B
w�B
w�B
w�B
v�B
v�B
v�B
w�B
w�B
w�B
w�B
w�B
x�B
x�B
y�B
y�B
z�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�-B�UB��B��B�B.�B'�B�BxB/OB*�B9$B0;B@�BL~BK^BB�B`�B{�B��B~(B��BHB~Bw�B]IBoBjBk�Bx�By�BqABdZB\�Bf�BbhB^�Be�Be�BgmBg�B^BUMBG�B?�B9	B.�BjB?B�B��B�B˒B��B��B�KB�2B��B�B� B�lBx�BXEBH1BC{BC�B8RB4B$&B�B
�B
�B
�B
��B
�EB
��B
�qB
��B
�0B
�B
y�B
l=B
M�B
0�B
/OB
�B
�B

�B
�B	�2B	�@B	ٚB	�_B	��B	��B	�DB	��B	��B	�\B	�~B	��B	�mB	}�B	r�B	oOB	`�B	U2B	IB	EB	A�B	9	B	,B	%�B	%FB	sB	�B	�B	+B	B��B�B�[B�B�B�BݘB�mB��B�}B�B�mB�4B��B��B��B��B��B��B�KB��B�WB�hB�sB��B�hB��B��B��B�HB�4B{B��B�B�mB��BsMB{�B�?B��Bz�Bx�BwLBz�B�B}�B��B|B}�B��B��B�B}�B}�B|Bz�Bz�Bu%BiBoBjeB_VBW�BOBB2�BJ�BP}BL�BM�BI�B?}BI�BJ	BP�BMBM�BUgBQ�BMBN�BQ�BQ�BQ�BK^BHfBJ�BSBW�BT,BVBT,BUMBU�BS�BUB`\Bj�Bm�Bl"Bh�Bh�Bt�BvBraBp�Bv`Bu�Bs�BzxBz�B{�B�aB��B��B��B��B��B��B��B�#B��B� B��B��B�B��B��B�)B�QB��B��B�LB��B��B��B��B��B��B��B�B��B��B�AB�3B�7B� B� B�:B�,B�SB�EB�kB�]B�jB�jBޞB�B��B��B��B��B��B�B�B�RB�6B�BB	 �B	�B	^B	~B	�B	�B	�B	�B	�B	�B		B	�B	B	# B	$ZB	)DB	./B	0UB	/�B	1vB	2|B	3�B	8�B	8�B	:�B	=B	B�B	EB	H�B	MB	OB	R B	SB	TB	UB	V9B	W$B	W$B	W$B	V9B	UgB	XyB	[�B	`vB	d�B	f�B	f�B	j�B	l�B	m�B	l�B	n�B	p!B	u�B	y�B	zB	z�B	|B	}"B	�OB	�;B	�GB	�MB	�mB	��B	��B	�vB	�}B	��B	��B	��B	��B	��B	��B	��B	�
B	��B	��B	��B	�B	�5B	�UB	�MB	�?B	�ZB	�nB	��B	�TB	�^B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	��B	��B	�B	��B	��B	��B	�B	�B	� B	�:B	�:B	�B	�9B	�$B	�EB	�+B	�EB	�EB	�yB	�CB	�WB	�IB	�IB	�xB	ܒB	ބB	�vB	�bB	�bB	�|B	�nB	�B	��B	�B	�B	�B	�B	�B	�B	��B	�B	��B	��B	�B	��B	�B	��B	�B	��B	��B	��B	�B	��B	��B	��B	��B	��B	�-B	��B	��B	��B	��B	�+B	�	B	�B	�$B	�$B	�8B	�LB	�*B	�DB	�.B	�(B	�(B	�<B
 OB
-B
-B
'B
;B
 iB
MB
1B
	7B
KB
fB

rB
	�B
zB
	RB
DB
dB
jB
pB
�B
vB
}B
�B
}B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
B
B
!B
�B
 �B
!�B
 �B
 'B
 B
!�B
"B
"�B
"B
#B
$�B
$�B
%�B
%�B
%�B
$�B
#�B
$B
# B
$&B
$&B
&B
'B
&B
&B
&B
&2B
'B
)*B
)B
($B
($B
)B
(>B
'B
($B
($B
*B
+6B
+B
+B
+B
*eB
*0B
,"B
,=B
+QB
+6B
,=B
,=B
,"B
,"B
,=B
-B
-)B
-)B
.B
-)B
-)B
.IB
-CB
-]B
.IB
/5B
0;B
0;B
1AB
1'B
1AB
/OB
/OB
1AB
1AB
1AB
0UB
2GB
2GB
4TB
4TB
4TB
4TB
4nB
4nB
4TB
3�B
4nB
5ZB
6`B
5tB
6`B
6`B
6`B
5tB
5ZB
5ZB
6`B
7fB
7LB
7fB
7fB
7LB
6`B
6`B
6`B
6`B
6zB
6zB
7fB
8lB
8lB
8lB
7�B
8lB
7�B
8lB
:xB
9rB
:^B
:xB
:�B
;B
:�B
:xB
:xB
9rB
:xB
;dB
:xB
:xB
:xB
8�B
9�B
:�B
;�B
:�B
;B
=�B
<�B
<�B
;�B
<�B
<�B
<�B
<�B
<�B
=�B
=�B
<�B
;�B
<�B
:�B
;�B
;�B
=�B
>�B
<�B
?�B
?�B
?�B
>�B
?�B
?�B
?�B
?�B
?�B
@�B
A�B
A�B
A�B
B�B
C�B
D�B
C�B
C�B
D�B
C�B
C�B
E�B
D�B
D�B
F�B
G�B
H�B
H�B
I�B
J�B
J�B
I�B
I�B
I�B
J�B
K�B
J�B
L�B
L�B
L�B
L�B
L�B
L�B
K�B
K�B
L�B
M�B
M�B
M�B
M�B
M�B
L�B
K�B
MB
M�B
M�B
L�B
K�B
L�B
KB
K�B
MB
OB
PB
RB
R B
Q B
QB
O�B
Q B
PB
PB
Q B
RB
RB
RB
PHB
RB
QB
QB
T�B
UB
UB
UB
UB
TB
S@B
VB
W$B
V9B
V9B
VB
U2B
W$B
Y1B
Y1B
Z7B
ZQB
Z7B
YeB
Y1B
YKB
[#B
[=B
YKB
[=B
\)B
\CB
[=B
\CB
\)B
\CB
[WB
[WB
[WB
ZQB
[WB
]IB
\]B
[WB
]IB
]dB
]~B
]dB
_VB
`BB
_VB
`BB
`\B
`vB
_VB
_pB
^jB
^�B
abB
abB
abB
bNB
b�B
bhB
bhB
bhB
a|B
a|B
abB
a|B
bhB
cnB
dZB
cnB
b�B
cnB
c�B
dtB
c�B
dtB
c�B
dtB
d�B
e�B
d�B
e�B
g�B
gmB
g�B
f�B
g�B
hsB
g�B
g�B
g�B
g�B
g�B
h�B
h�B
h�B
i�B
iyB
h�B
i�B
h�B
i�B
j�B
j�B
i�B
j�B
j�B
k�B
k�B
k�B
k�B
k�B
k�B
j�B
k�B
l�B
l�B
l�B
l�B
m�B
m�B
l�B
m�B
m�B
m�B
m�B
l�B
m�B
m�B
n�B
n�B
n�B
n�B
n�B
o�B
p�B
o�B
p�B
p�B
p�B
p�B
p�B
p�B
q�B
q�B
q�B
p�B
p�B
p�B
o�B
q�B
r�B
q�B
p�B
r�B
s�B
s�B
s�B
s�B
t�B
t�B
t�B
s�B
s�B
s�B
t�B
t�B
t�B
t�B
s�B
t�B
t�B
u�B
v�B
u�B
u�B
v�B
v�B
v�B
v�B
w�B
w�B
w�B
v�B
v�B
v�B
w�B
w�B
w�B
w�B
xB
x�B
x�B
y�B
y�B
z�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111113111111311111111111111111111111111111111111113111111111111111111131111111111111131111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.05(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201711250035112017112500351120171125003511201806221322172018062213221720180622132217201804050725162018040507251620180405072516  JA  ARFMdecpA19c                                                                20171121093514  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20171121003529  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20171121003531  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20171121003531  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20171121003532  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20171121003532  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20171121003532  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20171121003532  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20171121003532  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20171121003533                      G�O�G�O�G�O�                JA  ARUP                                                                        20171121005516                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20171121153648  CV  JULD            G�O�G�O�F���                JM  ARSQJMQC2.0                                                                 20171122000000  CF  PSAL_ADJUSTED_QCCR  C�  G�O�                JM  ARCAJMQC2.0                                                                 20171124153511  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20171124153511  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404222516  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622042217  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200116221516                      G�O�G�O�G�O�                