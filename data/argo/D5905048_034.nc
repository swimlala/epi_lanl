CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2016-09-03T00:35:36Z creation;2016-09-03T00:35:38Z conversion to V3.1;2019-12-19T08:27:39Z update;     
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20160903003536  20200116201517  5905048                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               "A   JA  I2_0577_034                     2C  D   NAVIS_A                         0577                            ARGO 102115                     863 @���`� 1   @���F)�@4�����d����1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @9��@�  @�  A   A   A>ffA^ffA�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0ffB8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C�fC   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� Dd��De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�3D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�C3D� D�� D�  D�<�D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�L�D�c3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @6ff@|��@�ff@�ffA33A=��A]��A33A���A���A���A���Aϙ�Aߙ�AA���B��B��B��B��B'��B033B7��B?��BG��BO��BW��B_��Bg��Bo��Bw��B��B��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fC�3C�3C�3C�3C	�3C�3C�3C�3C�3C�3C�3C�3C�3C�3CٚC�3C!�3C#�3C%�3C'�3C)�3C+�3C-�3C/�3C1�3C3�3C5�3C7�3C9�3C;�3C=�3C?�3CA�3CC�3CE�3CG�3CI�3CK�3CM�3CO�3CQ�3CS�3CU�3CW�3CY�3C[�3C]�3C_�3Ca�3Cc�3Ce�3Cg�3Ci�3Ck�3Cm�3Co�3Cq�3Cs�3Cu�3Cw�3Cy�3C{�3C}�3C�3C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D |�D ��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D	|�D	��D
|�D
��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D |�D ��D!|�D!��D"|�D"��D#|�D#��D$|�D$��D%|�D%��D&|�D&��D'|�D'��D(|�D(��D)|�D)��D*|�D*��D+|�D+��D,|�D,��D-|�D-��D.|�D.��D/|�D/��D0|�D0��D1|�D1��D2|�D2��D3|�D3��D4|�D4��D5|�D5��D6|�D6��D7|�D7��D8|�D8��D9|�D9��D:|�D:��D;|�D;��D<|�D<��D=|�D=��D>|�D>��D?|�D?��D@|�D@��DA|�DA��DB|�DB��DC|�DC��DD|�DD��DE|�DE��DF|�DF��DG|�DG��DH|�DH��DI|�DI��DJ|�DJ��DK|�DK��DL|�DL��DM|�DM��DN|�DN��DO|�DO��DP|�DP��DQ|�DQ��DR|�DR��DS|�DS��DT|�DT��DU|�DU��DV|�DV��DW|�DW��DX|�DX��DY|�DY��DZ|�DZ��D[|�D[��D\|�D\��D]|�D]��D^|�D^��D_|�D_��D`|�D`��Da|�Da��Db|�Db��Dc|�Dc��Dd|�Dd�fDe|�De��Df|�Df��Dg|�Dg��Dh|�Dh��Di|�Di��Dj|�Dj��Dk|�Dk��Dl|�Dl��Dm|�Dm��Dn|�Dn��Do|�Do��Dp|�Dp��Dq|�Dq��Dr|�Dr��Ds|�Ds��Dt|�Dt��Du|�Du��Dv|�Dv��Dw|�Dw��Dx|�Dx��Dy|�Dy��Dz|�Dz��D{|�D{��D||�D|��D}|�D}��D~|�D~��D|�D��D�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�{3D��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD���D��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD¾fD��fD�>fD�~fDþfD��fD�>fD�~fDľfD��fD�>fD�~fDžfD��fD�>fD�~fDƾfD��fD�>fD�~fDǾfD��fD�>fD�~fDȾfD��fD�>fD�~fDɾfD��fD�>fD�~fDʾfD��fD�>fD�~fD˾fD��fD�>fD�~fD̾fD��fD�>fD�~fD;fD��fD�>fD�~fDξfD��fD�>fD�~fDϾfD��fD�>fD�~fDоfD��fD�>fD�~fDѾfD��fD�>fD�~fDҾfD��fD�>fD�~fDӾfD��fD�>fD�~fDԾfD��fD�>fD�~fDվfD��fD�>fD�~fD־fD��fD�>fD�~fD׾fD��fD�>fD�~fDؾfD��fD�>fD�~fDپfD��fD�>fD�~fDھfD��fD�>fD�~fD۾fD��fD�>fD�~fDܾfD��fD�>fD�~fDݾfD��fD�>fD�~fD޾fD��fD�>fD�~fD߾fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��D�>fD�~fD�fD��fD�>fD�~fD��fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�A�D�~fD�fD��fD�;3D�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��D�K3D�a�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�%A�%A�A�%A�%A�%A�%A�%A�%A�1A�1A�
=A�JA�JA�JA�JA�JA�VA�VA�bA�bA�oA�oA�oA�{A�{A�{A��A��A��A�oA�A���A�7LA�!A��A�I�A��A�\)AݓuAܩ�A�A�bNA�ƨA�A�A�S�A�\)A��A׮A�hsAԲ-A�I�A���AӶFAҮA�oA�C�Aϝ�A�JA��A�ĜAʑhAʉ7Aʝ�A�`BAȉ7A�ȴA�oA���Aŗ�A7A��A�"�A���A��hA�|�A��TA�;dA���A��A���A��A� �A�r�A���A���A���A�VA��mA�A�p�A�=qA��wA���A�hsA��A��yA���A�v�A���A��A�S�A���A��^A�$�A�G�A���A�%A��uA��!A���A��A���A��A�C�A���A� �A�n�A�ZA�G�A�\)A���A���A���A��A���A��A|^5A{?}Ayx�AxZAvbAt$�ArQ�Ao��Am�AlVAj��Ag��Ad�RAb�Aa�A`�jA]33A\^5AZĜAY�AW��AU�mASt�AQ�
AO�PAN�uAM�^AL�AK��AI��AHZAG�PAG33AF�+ADz�A@�\A?��A>�9A<�RA;oA8Q�A5|�A3�A3%A2z�A1�TA/l�A-+A+��A+�A)�mA(^5A&�+A%/A$�A$ �A"�A �RA�A�+A�hAȴA~�A(�A&�A�mA/A�A�A��AM�A�!A%A�A��A�^A�DA�AC�A
��A
�A	oA��A1A�-A"�AZAdZA��A(�A��A��AbNAJA�^AO�A"�A
=A �RA @�V@�33@��#@��-@��h@��!@�&�@�
=@�E�@웦@��@�E�@�%@�A�@���@�@㝲@�n�@�hs@�@�Z@�l�@�"�@�  @�
=@�v�@�~�@ޏ\@��u@��/@�j@�t�@��T@�V@��@���@ى7@ؓu@���@ץ�@ׅ@�
=@�=q@��@��@պ^@�%@�9X@��;@�33@�n�@���@�X@�%@���@�Z@�C�@·+@�E�@��#@�x�@���@̃@��
@�33@�M�@�&�@�  @Ǯ@�;d@�@ƸR@�~�@�-@���@�/@�  @��;@��
@�dZ@�@�M�@�E�@�J@�x�@�O�@�G�@��`@�r�@��@�o@���@�5?@���@��h@�G�@���@���@�ȴ@�~�@�5?@���@�&�@��@��u@��@��D@�bN@�b@��F@�+@��h@�5?@���@�M�@�n�@�7L@��@��
@�@���@�hs@���@��j@�bN@�r�@�Z@��@�7L@��9@���@���@�@�$�@���@�p�@�&�@���@��j@�b@��w@���@��P@�l�@�;d@�o@�^5@�v�@�ff@�ff@�G�@���@�A�@��w@��F@��@��y@��\@�$�@���@���@�X@��j@��D@�bN@�  @�33@���@��#@��@���@�j@�Q�@�b@�|�@�33@�n�@�ff@�$�@�@��T@��-@��@�hs@�p�@�`B@�?}@�V@��@���@��9@��D@�(�@���@���@���@��P@�l�@�K�@��@��H@���@�ȴ@���@�v�@�^5@�n�@�v�@�@��@���@�|�@�\)@�S�@���@���@�|�@�S�@�
=@���@��y@���@���@�E�@��#@�p�@�7L@��@�j@�(�@�ƨ@�C�@��@��@���@���@��\@�ff@�J@���@��^@���@�`B@�&�@�%@��j@�r�@��@���@�|�@�dZ@�"�@���@�~�@�$�@��@�@���@�x�@�O�@�7L@�Ĝ@�Z@�A�@���@���@�l�@�C�@�C�@�C�@�"�@�o@���@��+@�^5@�=q@��^@�X@�%@�Ĝ@��u@�z�@�A�@�;d@�ff@��@�x�@�V@��`@��/@��@�r�@�j@�Z@�I�@�1'@��;@�33@��H@�^5@�@�@�@��-@�X@��@��D@�Q�@�1'@��@��@��w@�|�@�;d@�@��y@���@�ff@�J@���@�`B@�/@�%@���@��`@���@���@�r�@�1'@�1@�  @��@l�@�@~ȴ@~��@~ff@}��@}�@|��@|�D@|j@|1@|(�@|9X@|(�@{��@{�
@{�F@{S�@{@z��@z�@yx�@yhs@y7L@x�u@x �@w�@w�w@w|�@w�@vȴ@v��@v�+@v5?@v@u��@u�@u/@t�D@t�@sƨ@st�@s@r~�@rM�@rJ@qX@p�@pA�@o��@oK�@o
=@n�R@n{@m��@m�h@mO�@l��@l�@kC�@k"�@k@j�@j��@jn�@jJ@i�7@iX@i&�@h��@h�9@h�u@hA�@h  @g��@g+@g�@g
=@f�R@fE�@e�@e��@e�@eV@d�D@dz�@d9X@d1@cC�@c"�@co@c@c@b�H@b~�@b^5@b-@a�^@a��@a%@`��@`1'@_�;@_�@_�P@_l�@^��@^v�@^5?@^$�@^{@]��@]@]`B@\��@\�D@\9X@[�
@[t�@[o@Z�H@Z��@ZM�@Z�@Y��@XĜ@XQ�@X �@W�;@Wl�@V�+@U��@U�@Up�@UO�@T�/@T�j@TI�@T1@Sƨ@SdZ@So@R~�@R-@Q��@Q%@P�`@P�`@P�`@PĜ@O�;@N�R@M�-@M`B@MO�@L��@L�D@K�m@K��@J��@J^5@JM�@J=q@JJ@I�#@I�^@I��@I&�@H�9@HbN@H  @G�@G�@G�;@G�;@G��@Gl�@F�y@F�+@F{@E�T@E�T@E@E�@D�/@Dz�@C�m@Ct�@CC�@Co@B��@B^5@B-@A�@A��@A7L@A�@@��@@��@@��@@b@?�@?|�@?l�@?;d@>�R@>ff@>$�@>{@=�@=��@=�-@=`B@=V@<�/@<�j@<(�@;�
@;�
@;�F@;�@;S�@:��@:-@9��@9x�@9X@8��@8��@8�u@8r�@81'@8  @7�@7�;@7�@7|�@7�@6��@6�R@6��@6V@5�T@5�@5`B@5O�@5/@5/@5�@4��@4��@4�D@4�@3dZ@333@2�@2�H@2�H@2��@2n�@2J@1�@1��@1�^@1��@1x�@1�@0��@0Ĝ@0�@0b@/�@/��@/K�@/+@.�@.�R@.5?@-��@-�h@-�@,��@,�@,z�@,Z@,1@+��@+S�@+"�@*�H@*�\@*=q@*�@*J@)��@)�#@(Ĝ@'�;@'�P@'�P@'|�@'\)@'+@&�y@&V@%�T@%�-@%�h@%`B@$��@$�D@$�@#��@#dZ@#33@#o@"�@"��@"�!@"M�@!��@!�7@!X@!&�@ �9@   @�@��@ff@$�@{@�T@��@��@�D@9X@9X@(�@�@�m@��@dZ@33@�H@-@��@�^@hs@&�@�`@�9@�9@�u@A�@�@�;@�;@�@\)@;d@��@��@��@�y@ȴ@�R@�R@ff@{@@�T@��@p�@`B@?}@��@�D@j@Z@(�@1@�m@ƨ@�F@�F@�F@��@�F@��@t�@t�@dZ@"�@o@o@�@�\@J@�@��@��@��@x�@�@�@&�@�@�@��@�`@�`@Ĝ@r�@ �@b@  @�;@��@�w@��@|�@�@ȴ@�R@��@�+@ff@V@5?@{@@�T@��@?}@/@�@�j@�D@z�@z�@j@I�@9X111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�%A�%A�A�%A�%A�%A�%A�%A�%A�1A�1A�
=A�JA�JA�JA�JA�JA�VA�VA�bA�bA�oA�oA�oA�{A�{A�{A��A��A��A�oA�A���A�7LA�!A��A�I�A��A�\)AݓuAܩ�A�A�bNA�ƨA�A�A�S�A�\)A��A׮A�hsAԲ-A�I�A���AӶFAҮA�oA�C�Aϝ�A�JA��A�ĜAʑhAʉ7Aʝ�A�`BAȉ7A�ȴA�oA���Aŗ�A7A��A�"�A���A��hA�|�A��TA�;dA���A��A���A��A� �A�r�A���A���A���A�VA��mA�A�p�A�=qA��wA���A�hsA��A��yA���A�v�A���A��A�S�A���A��^A�$�A�G�A���A�%A��uA��!A���A��A���A��A�C�A���A� �A�n�A�ZA�G�A�\)A���A���A���A��A���A��A|^5A{?}Ayx�AxZAvbAt$�ArQ�Ao��Am�AlVAj��Ag��Ad�RAb�Aa�A`�jA]33A\^5AZĜAY�AW��AU�mASt�AQ�
AO�PAN�uAM�^AL�AK��AI��AHZAG�PAG33AF�+ADz�A@�\A?��A>�9A<�RA;oA8Q�A5|�A3�A3%A2z�A1�TA/l�A-+A+��A+�A)�mA(^5A&�+A%/A$�A$ �A"�A �RA�A�+A�hAȴA~�A(�A&�A�mA/A�A�A��AM�A�!A%A�A��A�^A�DA�AC�A
��A
�A	oA��A1A�-A"�AZAdZA��A(�A��A��AbNAJA�^AO�A"�A
=A �RA @�V@�33@��#@��-@��h@��!@�&�@�
=@�E�@웦@��@�E�@�%@�A�@���@�@㝲@�n�@�hs@�@�Z@�l�@�"�@�  @�
=@�v�@�~�@ޏ\@��u@��/@�j@�t�@��T@�V@��@���@ى7@ؓu@���@ץ�@ׅ@�
=@�=q@��@��@պ^@�%@�9X@��;@�33@�n�@���@�X@�%@���@�Z@�C�@·+@�E�@��#@�x�@���@̃@��
@�33@�M�@�&�@�  @Ǯ@�;d@�@ƸR@�~�@�-@���@�/@�  @��;@��
@�dZ@�@�M�@�E�@�J@�x�@�O�@�G�@��`@�r�@��@�o@���@�5?@���@��h@�G�@���@���@�ȴ@�~�@�5?@���@�&�@��@��u@��@��D@�bN@�b@��F@�+@��h@�5?@���@�M�@�n�@�7L@��@��
@�@���@�hs@���@��j@�bN@�r�@�Z@��@�7L@��9@���@���@�@�$�@���@�p�@�&�@���@��j@�b@��w@���@��P@�l�@�;d@�o@�^5@�v�@�ff@�ff@�G�@���@�A�@��w@��F@��@��y@��\@�$�@���@���@�X@��j@��D@�bN@�  @�33@���@��#@��@���@�j@�Q�@�b@�|�@�33@�n�@�ff@�$�@�@��T@��-@��@�hs@�p�@�`B@�?}@�V@��@���@��9@��D@�(�@���@���@���@��P@�l�@�K�@��@��H@���@�ȴ@���@�v�@�^5@�n�@�v�@�@��@���@�|�@�\)@�S�@���@���@�|�@�S�@�
=@���@��y@���@���@�E�@��#@�p�@�7L@��@�j@�(�@�ƨ@�C�@��@��@���@���@��\@�ff@�J@���@��^@���@�`B@�&�@�%@��j@�r�@��@���@�|�@�dZ@�"�@���@�~�@�$�@��@�@���@�x�@�O�@�7L@�Ĝ@�Z@�A�@���@���@�l�@�C�@�C�@�C�@�"�@�o@���@��+@�^5@�=q@��^@�X@�%@�Ĝ@��u@�z�@�A�@�;d@�ff@��@�x�@�V@��`@��/@��@�r�@�j@�Z@�I�@�1'@��;@�33@��H@�^5@�@�@�@��-@�X@��@��D@�Q�@�1'@��@��@��w@�|�@�;d@�@��y@���@�ff@�J@���@�`B@�/@�%@���@��`@���@���@�r�@�1'@�1@�  @��@l�@�@~ȴ@~��@~ff@}��@}�@|��@|�D@|j@|1@|(�@|9X@|(�@{��@{�
@{�F@{S�@{@z��@z�@yx�@yhs@y7L@x�u@x �@w�@w�w@w|�@w�@vȴ@v��@v�+@v5?@v@u��@u�@u/@t�D@t�@sƨ@st�@s@r~�@rM�@rJ@qX@p�@pA�@o��@oK�@o
=@n�R@n{@m��@m�h@mO�@l��@l�@kC�@k"�@k@j�@j��@jn�@jJ@i�7@iX@i&�@h��@h�9@h�u@hA�@h  @g��@g+@g�@g
=@f�R@fE�@e�@e��@e�@eV@d�D@dz�@d9X@d1@cC�@c"�@co@c@c@b�H@b~�@b^5@b-@a�^@a��@a%@`��@`1'@_�;@_�@_�P@_l�@^��@^v�@^5?@^$�@^{@]��@]@]`B@\��@\�D@\9X@[�
@[t�@[o@Z�H@Z��@ZM�@Z�@Y��@XĜ@XQ�@X �@W�;@Wl�@V�+@U��@U�@Up�@UO�@T�/@T�j@TI�@T1@Sƨ@SdZ@So@R~�@R-@Q��@Q%@P�`@P�`@P�`@PĜ@O�;@N�R@M�-@M`B@MO�@L��@L�D@K�m@K��@J��@J^5@JM�@J=q@JJ@I�#@I�^@I��@I&�@H�9@HbN@H  @G�@G�@G�;@G�;@G��@Gl�@F�y@F�+@F{@E�T@E�T@E@E�@D�/@Dz�@C�m@Ct�@CC�@Co@B��@B^5@B-@A�@A��@A7L@A�@@��@@��@@��@@b@?�@?|�@?l�@?;d@>�R@>ff@>$�@>{@=�@=��@=�-@=`B@=V@<�/@<�j@<(�@;�
@;�
@;�F@;�@;S�@:��@:-@9��@9x�@9X@8��@8��@8�u@8r�@81'@8  @7�@7�;@7�@7|�@7�@6��@6�R@6��@6V@5�T@5�@5`B@5O�@5/@5/@5�@4��@4��@4�D@4�@3dZ@333@2�@2�H@2�H@2��@2n�@2J@1�@1��@1�^@1��@1x�@1�@0��@0Ĝ@0�@0b@/�@/��@/K�@/+@.�@.�R@.5?@-��@-�h@-�@,��@,�@,z�@,Z@,1@+��@+S�@+"�@*�H@*�\@*=q@*�@*J@)��@)�#@(Ĝ@'�;@'�P@'�P@'|�@'\)@'+@&�y@&V@%�T@%�-@%�h@%`B@$��@$�D@$�@#��@#dZ@#33@#o@"�@"��@"�!@"M�@!��@!�7@!X@!&�@ �9@   @�@��@ff@$�@{@�T@��@��@�D@9X@9X@(�@�@�m@��@dZ@33@�H@-@��@�^@hs@&�@�`@�9@�9@�u@A�@�@�;@�;@�@\)@;d@��@��@��@�y@ȴ@�R@�R@ff@{@@�T@��@p�@`B@?}@��@�D@j@Z@(�@1@�m@ƨ@�F@�F@�F@��@�F@��@t�@t�@dZ@"�@o@o@�@�\@J@�@��@��@��@x�@�@�@&�@�@�@��@�`@�`@Ĝ@r�@ �@b@  @�;@��@�w@��@|�@�@ȴ@�R@��@�+@ff@V@5?@{@@�T@��@?}@/@�@�j@�D@z�@z�@j@I�@9X111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B
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
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
�wB
�mB
�B
�B
�B
��B
��B
��B
�B
�B
�B
�B
��B+B�B'�B33B6FB#�B$�B"�B�B$�BH�BVB^5BffBaHBR�BS�BZBt�B�1B�B�BBDB�B+B@�BXB�B��B��B�{B�BhsBK�BE�BB�BI�B^5BŢB��B	7B�B�BDBB�B�B�B�B�B�5B��B��B�B��B��B��B�\B|�Bm�BM�B%�B�HBŢB�B\)Bw�Bo�B_;B<jB�B
=B+B%B
��B
�B
�HB
ĜB
�FB
��B
�DB
r�B
gmB
\)B
R�B
E�B
8RB
+B
�B
DB
B	��B	�`B	��B	��B	�XB	�-B	��B	��B	�VB	�B	x�B	o�B	aHB	W
B	I�B	B�B	<jB	6FB	/B	)�B	�B	�B	�B	uB	JB��B�B�B�mB�;B�
B��BȴBĜB��B�qB�LB�B��B��B��B��B��B��B�{B�oB�hB�JB�7B�%B�B�B�B�B~�B}�B{�Bz�Bw�Bu�Bu�Br�Bp�Bo�BjBhsBffBdZBbNBaHBaHBaHB`BB`BB`BB`BB`BBaHB_;B^5B_;B_;B_;B^5B_;B_;B`BB`BBaHBaHBbNBffBffBffBe`BgmBk�Bl�Bl�Bq�Bn�Bn�Bk�BjBl�Bo�Bq�Bo�Bp�Bq�Bv�B{�B� B��B��B��B��B��B�?B�dB�wB��BŢBǮBȴB��B��B��B��B��B��B��B�B�/B�;B�ZB�mB�B�B�B�B�B�B��B��B��B��B��B��B��B��B	B	B	1B	1B	
=B	VB	bB	{B	�B	�B	�B	�B	�B	�B	�B	!�B	!�B	!�B	#�B	&�B	)�B	)�B	-B	1'B	1'B	1'B	49B	6FB	9XB	:^B	:^B	<jB	=qB	?}B	@�B	B�B	F�B	G�B	J�B	M�B	N�B	S�B	]/B	aHB	ffB	iyB	jB	iyB	hsB	gmB	ffB	m�B	o�B	v�B	z�B	x�B	v�B	u�B	v�B	x�B	v�B	t�B	w�B	v�B	z�B	z�B	{�B	�B	�%B	�B	�B	�+B	�B	�+B	�1B	�1B	�1B	�1B	�7B	�=B	�DB	�PB	�VB	�VB	�bB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�!B	�'B	�B	�B	�'B	�!B	�!B	�-B	�9B	�?B	�LB	�dB	�qB	�}B	��B	ÖB	ĜB	ĜB	ƨB	ǮB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�
B	�#B	�/B	�5B	�HB	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
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
DB
DB
DB
DB
DB
DB
DB
DB
DB
DB
JB
PB
VB
VB
PB
JB
DB

=B

=B

=B

=B
DB
	7B
%B
B
%B
%B
%B
%B
%B
1B
	7B

=B

=B

=B

=B
DB
DB
PB
\B
\B
bB
hB
bB
hB
oB
oB
oB
uB
uB
uB
uB
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
 �B
 �B
 �B
 �B
!�B
 �B
!�B
"�B
"�B
"�B
"�B
"�B
#�B
#�B
#�B
#�B
#�B
$�B
$�B
$�B
$�B
$�B
%�B
%�B
%�B
&�B
&�B
&�B
&�B
&�B
'�B
&�B
&�B
'�B
(�B
(�B
(�B
(�B
)�B
)�B
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
-B
-B
-B
.B
.B
.B
.B
/B
/B
/B
/B
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
2-B
2-B
2-B
33B
33B
33B
33B
33B
33B
33B
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
6FB
6FB
6FB
6FB
6FB
7LB
7LB
7LB
8RB
8RB
8RB
8RB
8RB
9XB
9XB
9XB
9XB
9XB
:^B
:^B
:^B
;dB
;dB
;dB
;dB
;dB
;dB
;dB
<jB
<jB
<jB
<jB
<jB
<jB
<jB
=qB
=qB
=qB
>wB
?}B
?}B
?}B
?}B
?}B
@�B
@�B
B�B
B�B
B�B
B�B
B�B
C�B
C�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
E�B
E�B
E�B
E�B
E�B
F�B
E�B
E�B
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
K�B
K�B
K�B
L�B
L�B
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
M�B
M�B
M�B
N�B
N�B
N�B
N�B
N�B
N�B
O�B
O�B
P�B
P�B
P�B
P�B
P�B
P�B
P�B
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
S�B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
T�B
T�B
T�B
T�B
VB
VB
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
YB
YB
YB
YB
YB
ZB
ZB
ZB
[#B
[#B
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
]/B
]/B
^5B
_;B
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
bNB
bNB
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
e`B
e`B
ffB
ffB
ffB
ffB
ffB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
hsB
hsB
hsB
hsB
iyB
iyB
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
k�B
l�B
m�B
m�B
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
u�B
u�B
v�B
v�B
v�B
v�B
w�B
v�B
w�B
w�B
w�B
w�B
w�B
w�B
x�B
x�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B
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
��B
��B
��B
��B
��B
��B
��B
��B
� B
��B
��B
�B
�%B
��B
�B
�B
��B
�zB
�B
�B
��B
�B
��B	B�B)*B5�B8�B$�B%�B#�B"4B($BJ=BW�B_�BiBeBT�BT,BZ�Bv+B�B�B�B�B�B�B,�BA�BY�B�_B�zB�B�
B��Bk�BL�BF�BE�BL�B_VB�mB��B
#BBB(BSB�ZB�B�B��B�OB�BؓB�B��B�yB�|B�]B��B�Bq[BR:B*�B�B��B��B^BzDBs�Bd�B@B#�BB	�BKB
��B
�;B
�,B
��B
�^B
��B
��B
tnB
i�B
^B
U�B
HB
:�B
-�B
�B
�B
gB	��B	�B	�B	�-B	�B	��B	�HB	��B	�bB	�B	{dB	r�B	c�B	YB	KB	C�B	>B	7�B	1[B	+�B	 �B	�B	7B	�B	HB�jB�TB�AB�B��B�7BϫB��BŢB�B��B��B��B�B�B��B��B�CB��B��B�aB��B�B��B�_B�B��B��B�uB��B.B}VB{�Bx�BwLBxBuZBtBq[BlBi�Bg�BeBc Bb�Bb�Ba�BaB`�Ba-Ba|Ba�BbNB_�B_B`'B`B_�B^�B_�B_�B`�B`�BbhBb�Bd&Bg8Bf�Bg8Bg�Bj�Bl�Bm]Bm�Br�BoOBoiBlqBk�Bm�BqBr|BpUBq'BrBwLB|B�B�B��B��B�kB��B�%B��B�cB��B�YBȚBɆB��BΥB�bB�&B�&B�uB�{B�_B�dBߤB�B��B�B�B�/B�B��B��B�B�?B�zB�RB�JB�6B�VB�]B	�B	�B	�B		B	)B	B	�B	�B	�B	�B	�B	�B		B	IB	jB	!�B	!�B	"4B	$ZB	'8B	*B	*KB	-�B	1[B	1[B	1�B	4�B	6�B	9�B	:�B	:�B	<�B	=�B	?�B	AB	C{B	G+B	G�B	KB	NVB	O\B	T,B	]dB	aHB	f�B	i�B	j�B	i�B	h�B	h$B	fLB	m�B	o�B	v�B	{�B	yXB	wLB	vzB	w2B	y�B	wLB	t�B	xB	v�B	z�B	z�B	{�B	�{B	��B	��B	��B	��B	�mB	��B	�fB	�fB	��B	��B	��B	�rB	�^B	��B	��B	��B	��B	�oB	��B	��B	�WB	�B	�B	��B	��B	��B	�-B	�B	�-B	��B	�B	�XB	�yB	�0B	�]B	��B	��B	��B	��B	��B	��B	��B	�|B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�"B	�B	�"B	�(B	��B	��B	�B	��B	��B	� B	�B	�B	�B	�B	�?B	�?B	�	B	�B	��B	��B	�B	��B	�B	�B	�B	��B	��B	��B	�B	��B	�	B	�	B	�B	�*B	�DB	�0B	�B	�6B	�<B	�(B	�HB
 iB
;B
UB
 B
 B
 B
UB
AB
AB
GB
GB
GB
aB
GB
GB
gB
gB
SB
SB
SB
mB
tB
EB
_B
KB
KB
fB
fB
	RB
	lB
	�B
�B
^B
xB
�B
^B
xB
^B
^B
^B
^B
~B
�B
pB
�B
�B
�B
�B

rB

rB

�B

�B
�B
	�B
�B
mB
tB
?B
?B
YB
YB
KB
	RB

XB

rB

�B

�B
�B
�B
�B
\B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
 �B
 �B
 �B
!B
!�B
 �B
!�B
#B
"�B
"�B
#B
#B
#�B
$B
$B
$B
#�B
$�B
%B
$�B
%B
%B
%�B
&B
&B
'B
'B
'B
'B
'8B
(
B
'B
'8B
(>B
)B
)*B
)*B
)*B
*0B
*0B
+B
+6B
+B
+6B
+QB
,WB
,"B
,"B
,"B
,=B
-)B
-CB
-]B
./B
./B
./B
./B
/5B
/5B
/5B
/OB
0;B
0;B
0UB
0;B
0UB
0;B
1AB
1[B
1[B
1vB
2GB
2GB
2aB
3hB
3MB
33B
3MB
33B
3hB
3hB
4TB
4TB
4nB
4nB
4�B
4�B
5tB
5ZB
5ZB
5ZB
5ZB
5tB
6zB
6`B
6`B
6zB
6`B
7�B
7�B
7�B
8lB
8lB
8�B
8�B
8�B
9�B
9rB
9�B
9rB
9�B
:�B
:�B
:�B
;B
;�B
;�B
;�B
;�B
;B
;B
<�B
<�B
<�B
<�B
<�B
<�B
<�B
=�B
=�B
=�B
>�B
?�B
?�B
?�B
?�B
?�B
AB
@�B
B�B
B�B
B�B
B�B
B�B
C�B
C�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
E�B
E�B
E�B
E�B
E�B
F�B
E�B
E�B
F�B
F�B
F�B
F�B
G�B
G�B
G�B
G�B
G�B
IB
H�B
IB
H�B
I�B
J	B
I�B
I�B
I�B
J�B
J�B
J�B
J�B
J�B
K�B
LB
K�B
L�B
L�B
L�B
MB
L�B
L�B
M�B
M�B
NB
M�B
M�B
M�B
M�B
NB
NB
N�B
N�B
N�B
OB
N�B
OB
PB
PB
Q B
Q B
Q4B
QB
QB
Q B
Q B
RB
Q�B
R B
RB
RB
RB
SB
SB
SB
SB
S&B
TB
T,B
S�B
TB
TB
TB
TB
TB
TB
U2B
U2B
UB
UB
VB
VB
VB
VB
VB
VB
VB
VB
W$B
W?B
W?B
W$B
W$B
W$B
X+B
X+B
X+B
XEB
Y1B
Y1B
Y1B
YKB
YKB
Z7B
ZQB
Z7B
[=B
[WB
[WB
[=B
[WB
\CB
\CB
\CB
\CB
]IB
]IB
]IB
]dB
]dB
]�B
^�B
_VB
_;B
_VB
_pB
_VB
_VB
_pB
`\B
`\B
`\B
`vB
`�B
a�B
a�B
a�B
abB
bhB
bhB
b�B
bhB
bhB
b�B
c�B
c�B
cnB
cnB
d�B
d�B
d�B
e�B
e�B
f�B
f�B
f�B
f�B
f�B
g�B
g�B
gmB
gmB
g�B
g�B
g�B
h�B
h�B
h�B
h�B
i�B
i�B
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
k�B
l�B
m�B
m�B
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
u�B
u�B
v�B
v�B
v�B
v�B
w�B
v�B
xB
w�B
w�B
w�B
w�B
w�B
x�B
x�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.05(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201609080045392016090800453920160908004539201806221301402018062213014020180622130140201804050700522018040507005220180405070052  JA  ARFMdecpA19c                                                                20160903093505  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20160903003536  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20160903003537  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20160903003537  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20160903003538  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20160903003538  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20160903003538  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20160903003538  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20160903003538  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20160903003538                      G�O�G�O�G�O�                JA  ARUP                                                                        20160903012036                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20160903153915  CV  JULD            G�O�G�O�F�?�                JM  ARCAJMQC2.0                                                                 20160907154539  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20160907154539  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404220052  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622040140  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200116201517                      G�O�G�O�G�O�                