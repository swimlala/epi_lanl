CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-06-04T00:35:05Z creation;2018-06-04T00:35:09Z conversion to V3.1;2019-12-19T07:40:45Z update;     
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
_FillValue                 �  I,   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  s�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �l   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �p   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �T   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �t   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ݔ   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �    HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �$   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �d   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �t   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �x   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20180604003505  20200115131517  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0576_246                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @�g����1   @�g�q��@:�K]��d@hۋ�q1   GPS     A   B   B   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C�fC  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D$��D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DA��DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� DffDf� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv�fDw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�<�D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�L�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@���@�ff@�ffA33A?33A_33A33A���A���A���A���Aϙ�Aߙ�AA���B��B��B��B��B'��B/��B7��B?��BG��BO��BW��B_��Bg��Bo��Bw��B��B��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fC�3C�3C�3C�3C	�3C�3C�3CٚC�3C�3C�3C�3C�3C�3C�3C�3C!�3C#�3C%�3C'�3C)�3C+�3C-�3C/�3C1�3C3�3C5�3C7�3C9�3C;�3C=�3C?�3CA�3CC�3CE�3CG�3CI�3CK�3CM�3CO�3CQ�3CS�3CU�3CW�3CY�3C[�3C]�3C_�3Ca�3Cc�3Ce�3Cg�3Ci�3Ck�3Cm�3Co�3Cq�3Cs�3Cu�3Cw�3Cy�3C{�3C}�3C�3C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D |�D ��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D	|�D	��D
|�D
��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D |�D ��D!|�D!��D"|�D"��D#|�D#��D$|�D$�fD%|�D%��D&|�D&��D'|�D'��D(|�D(��D)|�D)��D*|�D*��D+|�D+��D,|�D,��D-|�D-��D.|�D.��D/|�D/��D0|�D0��D1|�D1��D2|�D2��D3|�D3��D4|�D4��D5|�D5��D6|�D6��D7|�D7��D8|�D8��D9|�D9��D:|�D:��D;|�D;��D<|�D<��D=|�D=��D>|�D>��D?|�D?��D@|�D@��DA|�DA�fDB|�DB��DC|�DC��DD|�DD��DE|�DE��DF|�DF��DG|�DG��DH|�DH��DI|�DI��DJ|�DJ��DK|�DK��DL|�DL��DM|�DM��DN|�DN��DO|�DO��DP|�DP��DQ|�DQ��DR|�DR��DS|�DS��DT|�DT��DU|�DU��DV|�DV��DW|�DW��DX|�DX��DY|�DY��DZ|�DZ��D[|�D[��D\|�D\��D]|�D]��D^|�D^��D_|�D_��D`|�D`��Da|�Da��Db|�Db��Dc|�Dc��Dd|�Dd��De|�Df3Df|�Df��Dg|�Dg��Dh|�Dh��Di|�Di��Dj|�Dj��Dk|�Dk��Dl|�Dl��Dm|�Dm��Dn|�Dn��Do|�Do��Dp|�Dp��Dq|�Dq��Dr|�Dr��Ds|�Ds��Dt|�Dt��Du|�Du��Dv�3Dv��Dw|�Dw��Dx|�Dx��Dy|�Dy��Dz|�Dz��D{|�D{��D||�D|��D}|�D}��D~|�D~��D|�D��D�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�{3D��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD¾fD��fD�>fD�~fDþfD��fD�>fD�~fDľfD��fD�>fD�~fDžfD��fD�>fD�~fDƾfD��fD�>fD�~fDǾfD��fD�>fD�~fDȾfD��fD�>fD�~fDɾfD��fD�>fD�~fDʾfD��fD�>fD�~fD˾fD��fD�>fD�~fD̾fD��fD�>fD�~fD;fD��fD�>fD�~fDξfD��fD�>fD�~fDϾfD��fD�>fD�~fDоfD��fD�>fD�~fDѾfD��fD�>fD�~fDҾfD��fD�>fD�~fDӾfD��fD�>fD�~fDԾfD��fD�>fD�~fDվfD��fD�>fD�~fD־fD��fD�>fD�~fD׾fD��fD�;3D�~fDؾfD��fD�>fD�~fDپfD��fD�>fD�~fDھfD��fD�>fD�~fD۾fD��fD�>fD�~fDܾfD��fD�>fD�~fDݾfD��fD�>fD�~fD޾fD��fD�>fD�~fD߾fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD��fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�K3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�ĜA�ȴA�ƨA���A���A���A���A���A��+A�|�A�v�A�hsA�E�A��A�oA� �A�bA���A�O�A��FA���A���A��A��A�$�A���A�S�A�$�A���A���A��A�+A�&�A�?}A���A�XA��A���A��A��A�A�9XA�=qA��\A��A���A��A���A�VA�-A�z�A�I�A��mA�A���A��A��+A��A��DA�M�A��9A��A�r�A�VA��A�%A���A�E�A�C�A�A�l�A�
=A���A��A��7A���A��A}�A|�!A|�+A|�A{p�Az�Az1Ay�7Ax�Av�AuK�Ar�/Ao��An(�Am��Am�PAl�Ak�TAj��Ah�!Ag��Af��Ae��Ac��Ab1'Aa?}A_��A]�A\I�A\�A[��AY��AXĜAW/AV{AU+AT��AT~�AS�
AR�yAQ�mAQp�APJAN��AM��AL�uAK��AKl�AJQ�AI"�AHr�AG�AF�AE/AD��AD9XAC�TACK�AA"�A?t�A=��A="�A<�`A<��A<1'A;�A;��A;�A;x�A;dZA;G�A;+A;A:�`A:��A:�9A:�DA:VA:1A9\)A7�FA6��A5�FA5A4bNA3�PA2�`A2��A2��A2v�A2I�A2�A1�mA1��A1�A0�A0�jA0r�A/��A.A,�9A,n�A,=qA, �A+�PA*�+A)�A(�A&ĜA&ZA%�
A%p�A%&�A$�+A#�FA#\)A"5?A!S�A!7LA ��A�FAr�A1A�PAbNAl�A�A�!A��AS�A��AbNA��A��A�A"�A�A33AAJAA�A�A
ZA	��A	K�A	�A�DA��A�!AhsA��A�A��A^5A �A�PA �A �D@���@��h@�/@��D@��H@�=q@�1@��^@���@�t�@�^5@��@���@��@�hs@�/@�j@�(�@�o@�J@�C�@蛦@��@��m@�t�@�o@�R@�-@�?}@�@�@ᙚ@�bN@��H@ް!@ޗ�@�v�@�n�@އ+@ޟ�@݁@�|�@ف@ץ�@ָR@ղ-@ԓu@�  @���@�G�@�t�@�5?@��;@�"�@�V@�V@��@��;@�
=@�~�@��m@���@���@��@�|�@��@�=q@���@�@�X@���@���@�A�@���@�;d@��#@��9@� �@�  @���@�o@�%@�C�@��@���@��
@��P@�l�@�\)@��y@���@�@�`B@��@���@��
@�K�@�~�@�-@�{@���@���@�p�@�r�@��@�|�@���@�O�@�r�@�1@�K�@�@��@�X@�/@�&�@�V@��`@��u@��@���@��@��!@�@�Ĝ@�|�@�l�@��R@��@��@��7@���@�1'@��w@���@��h@���@��j@��u@��D@�z�@�Z@�1@��;@��F@��@�C�@���@�V@���@���@�Ĝ@��@��@�o@�ȴ@���@���@��\@��+@�-@�p�@��@�bN@�A�@�  @��;@��
@���@�ƨ@��@���@��P@�t�@�dZ@�S�@�S�@�K�@�K�@�;d@�+@��y@��\@�E�@�{@��@�@��h@�hs@�X@��@���@��9@��@���@���@���@�z�@�9X@�b@�@l�@+@~��@~5?@}@}p�@}O�@|��@|1@{dZ@{o@z�@z�H@z~�@z~�@zM�@y�@yhs@y&�@x��@xĜ@xr�@w+@s��@sƨ@sƨ@sƨ@st�@s33@s"�@s"�@s@r��@r��@q�#@qX@pĜ@pr�@pA�@pb@o�@o��@o��@o�@nȴ@n��@n5?@m�@l��@lZ@lZ@l9X@l1@k@j-@jJ@i�@i�#@i�@i�@i�@i�@i��@i��@iX@iG�@i�@i%@h��@h�`@h��@h��@h�@f�y@f��@e�@e�@eO�@e?}@d�/@d�D@dz�@dj@d9X@c��@c��@c�
@c�@c33@c33@c33@c"�@c"�@c"�@c@b�!@b�\@a��@aX@a�@`bN@`b@_�w@_�P@_�P@_l�@_
=@]@\��@\�@\j@\9X@[�m@[��@[o@Z�\@Y�#@Y�^@Y�7@X��@W|�@Vȴ@Vv�@V5?@U�-@U?}@U?}@U?}@T��@T�@T��@T�j@Tj@TZ@T�@S�F@R�!@RM�@R�@Q��@P�u@P  @N��@N��@N��@N��@N�@N��@NE�@N@M`B@M?}@M�@L�@K33@K@K@K@K@K@J�H@J��@J=q@J-@JJ@I��@H�`@HQ�@G�@G\)@G�@F�y@Fȴ@Fȴ@F��@F��@F�R@F��@Fv�@FV@FE�@F$�@F$�@F$�@F$�@F@F@E�@E�h@E`B@EO�@E?}@E/@D��@D�D@DZ@D(�@C�
@C@BM�@A%@@��@@��@@�9@@r�@?�@?��@?l�@?�@>�y@>��@>{@=��@=�-@=��@=��@=�h@=O�@<z�@;��@:�H@:~�@:-@9�^@9��@9�7@9X@97L@9�@8�`@8��@8��@8Ĝ@8�u@8A�@8  @7��@7�@7��@7l�@7+@7
=@6�y@6ȴ@6ȴ@6��@6��@6�+@6v�@6V@6V@6V@65?@6$�@6{@5�@5�h@4�/@3�F@2^5@1G�@0�@/�P@.��@.{@-�-@-�h@-�h@-�@-�@,9X@+��@+t�@+S�@+S�@+C�@+C�@+C�@+33@+"�@+o@+"�@+o@+o@+o@+@*�@*�H@*��@*n�@*=q@*�@)�#@)��@)X@)7L@)%@(�`@(�9@(�u@(Q�@(b@'�@'�w@&ff@%��@%�-@%��@%�h@%p�@%p�@%O�@%?}@%/@$z�@#�
@#��@#33@#o@"�!@"M�@"M�@!��@!��@!x�@!%@ ��@ ��@ �`@ ��@ �9@ ��@ �u@ �u@ r�@ A�@��@�y@v�@@��@�-@��@��@�@�@�@�m@ƨ@t�@C�@o@�@~�@^5@^5@�@��@�@�@�#@hs@�@Ĝ@bN@ �@�@�w@�P@\)@�@v�@E�@@�h@p�@O�@V@V@��@�@�@�/@�@�D@j@Z@�@ƨ@33@@�!@�\@n�@=q@-@�@��@��@�7@x�@7L@%@Ĝ@�@�@�@r�@bN@ �@ �@b@�w@�P@l�@K�@;d@
=@�@��@��@�+@ff@�-@/@V@�@��@�j@�D@j@j@j@j@Z@I�@I�@(�@��@�
@�F@�@t�@C�@33@33@33@33@o@o@o@@
��@
��@
��@
��@
��@
��@
�\@
n�@
^5@
^5@
^5@
-@
J@	�@	�#@	�#@	�#@	��@	�7@	X@	X@	7L@	&�@	�@	�@�`@�9@��@�u@r�@Q�@Q�@A�@A�@A�@A�@b@�@�@l�@�R@V@@�@@�h@p�@`B@/@�@V@V@�/@�/@�/@�/@��@��@��@�/@�/@�/@�/@��@�m@�F@��@��@t�@�\@^5@�@��@��@��@��@x�@hs@7L@�@%@ ��@ �9@ �9@ �u@ �@ r�@ r�@ Q�@ 1'@  �@  �@  �@  �@   ?�;d?��?��R?���?�O�?�O�?�p�?��h?��h?��h?��h?�p�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�ĜA�ȴA�ƨA���A���A���A���A���A��+A�|�A�v�A�hsA�E�A��A�oA� �A�bA���A�O�A��FA���A���A��A��A�$�A���A�S�A�$�A���A���A��A�+A�&�A�?}A���A�XA��A���A��A��A�A�9XA�=qA��\A��A���A��A���A�VA�-A�z�A�I�A��mA�A���A��A��+A��A��DA�M�A��9A��A�r�A�VA��A�%A���A�E�A�C�A�A�l�A�
=A���A��A��7A���A��A}�A|�!A|�+A|�A{p�Az�Az1Ay�7Ax�Av�AuK�Ar�/Ao��An(�Am��Am�PAl�Ak�TAj��Ah�!Ag��Af��Ae��Ac��Ab1'Aa?}A_��A]�A\I�A\�A[��AY��AXĜAW/AV{AU+AT��AT~�AS�
AR�yAQ�mAQp�APJAN��AM��AL�uAK��AKl�AJQ�AI"�AHr�AG�AF�AE/AD��AD9XAC�TACK�AA"�A?t�A=��A="�A<�`A<��A<1'A;�A;��A;�A;x�A;dZA;G�A;+A;A:�`A:��A:�9A:�DA:VA:1A9\)A7�FA6��A5�FA5A4bNA3�PA2�`A2��A2��A2v�A2I�A2�A1�mA1��A1�A0�A0�jA0r�A/��A.A,�9A,n�A,=qA, �A+�PA*�+A)�A(�A&ĜA&ZA%�
A%p�A%&�A$�+A#�FA#\)A"5?A!S�A!7LA ��A�FAr�A1A�PAbNAl�A�A�!A��AS�A��AbNA��A��A�A"�A�A33AAJAA�A�A
ZA	��A	K�A	�A�DA��A�!AhsA��A�A��A^5A �A�PA �A �D@���@��h@�/@��D@��H@�=q@�1@��^@���@�t�@�^5@��@���@��@�hs@�/@�j@�(�@�o@�J@�C�@蛦@��@��m@�t�@�o@�R@�-@�?}@�@�@ᙚ@�bN@��H@ް!@ޗ�@�v�@�n�@އ+@ޟ�@݁@�|�@ف@ץ�@ָR@ղ-@ԓu@�  @���@�G�@�t�@�5?@��;@�"�@�V@�V@��@��;@�
=@�~�@��m@���@���@��@�|�@��@�=q@���@�@�X@���@���@�A�@���@�;d@��#@��9@� �@�  @���@�o@�%@�C�@��@���@��
@��P@�l�@�\)@��y@���@�@�`B@��@���@��
@�K�@�~�@�-@�{@���@���@�p�@�r�@��@�|�@���@�O�@�r�@�1@�K�@�@��@�X@�/@�&�@�V@��`@��u@��@���@��@��!@�@�Ĝ@�|�@�l�@��R@��@��@��7@���@�1'@��w@���@��h@���@��j@��u@��D@�z�@�Z@�1@��;@��F@��@�C�@���@�V@���@���@�Ĝ@��@��@�o@�ȴ@���@���@��\@��+@�-@�p�@��@�bN@�A�@�  @��;@��
@���@�ƨ@��@���@��P@�t�@�dZ@�S�@�S�@�K�@�K�@�;d@�+@��y@��\@�E�@�{@��@�@��h@�hs@�X@��@���@��9@��@���@���@���@�z�@�9X@�b@�@l�@+@~��@~5?@}@}p�@}O�@|��@|1@{dZ@{o@z�@z�H@z~�@z~�@zM�@y�@yhs@y&�@x��@xĜ@xr�@w+@s��@sƨ@sƨ@sƨ@st�@s33@s"�@s"�@s@r��@r��@q�#@qX@pĜ@pr�@pA�@pb@o�@o��@o��@o�@nȴ@n��@n5?@m�@l��@lZ@lZ@l9X@l1@k@j-@jJ@i�@i�#@i�@i�@i�@i�@i��@i��@iX@iG�@i�@i%@h��@h�`@h��G�O�G�O�@f�y@f��@e�@e�@eO�@e?}@d�/@d�D@dz�@dj@d9X@c��@c��@c�
@c�@c33@c33@c33@c"�@c"�@c"�@c@b�!@b�\@a��@aX@a�@`bN@`b@_�w@_�P@_�PG�O�G�O�@]@\��@\�@\j@\9X@[�m@[��@[o@Z�\@Y�#@Y�^G�O�@X��@W|�@Vȴ@Vv�@V5?@U�-@U?}@U?}@U?}@T��@T�G�O�@T�j@Tj@TZ@T�@S�F@R�!@RM�@R�@Q��@P�u@P  @N��@N��@N��@N��@N�@N��@NE�@N@M`B@M?}G�O�@L�@K33@K@K@K@K@K@J�H@J��@J=q@J-@JJG�O�@H�`@HQ�@G�@G\)@G�@F�y@Fȴ@Fȴ@F��@F��@F�R@F��@Fv�@FV@FE�@F$�@F$�@F$�@F$�@F@F@E�@E�h@E`B@EO�@E?}G�O�@D��@D�D@DZ@D(�G�O�@C@BM�@A%@@��@@��@@�9@@r�@?�@?��@?l�@?�@>�y@>��@>{@=��@=�-@=��@=��G�O�G�O�@<z�@;��@:�H@:~�@:-@9�^@9��@9�7@9X@97L@9�@8�`@8��@8��@8Ĝ@8�u@8A�@8  @7��@7�@7��@7l�@7+@7
=@6�y@6ȴ@6ȴ@6��@6��@6�+@6v�@6V@6V@6V@65?@6$�@6{@5�@5�h@4�/@3�F@2^5@1G�@0�@/�P@.��@.{@-�-@-�h@-�h@-�G�O�@,9X@+��@+t�@+S�@+S�@+C�@+C�@+C�@+33@+"�@+o@+"�@+o@+o@+o@+@*�@*�H@*��@*n�@*=q@*�@)�#@)��@)X@)7L@)%@(�`@(�9@(�u@(Q�@(b@'�G�O�@&ff@%��@%�-@%��@%�h@%p�@%p�@%O�@%?}G�O�@$z�@#�
@#��@#33@#o@"�!@"M�@"M�@!��@!��@!x�@!%@ ��@ ��@ �`@ ��@ �9@ ��@ �u@ �u@ r�@ A�@��@�y@v�@@��@�-@��G�O�G�O�@�@�@�m@ƨ@t�@C�@o@�@~�@^5@^5@�@��@�@�G�O�@hs@�@Ĝ@bN@ �@�@�w@�P@\)@�@v�@E�@@�h@p�@O�@V@V@��@�@�@�/@�@�D@j@Z@�@ƨ@33@@�!@�\@n�@=q@-@�@��@��@�7@x�@7L@%@Ĝ@�@�@�@r�@bN@ �@ �@b@�w@�P@l�@K�@;d@
=@�@��@��@�+G�O�@�-@/@V@�@��@�j@�D@j@j@j@j@Z@I�@I�@(�@��@�
@�F@�@t�@C�@33@33@33@33@o@o@o@@
��@
��@
��@
��@
��@
��@
�\@
n�@
^5@
^5@
^5@
-@
J@	�@	�#@	�#@	�#@	��@	�7@	X@	X@	7L@	&�@	�@	�@�`@�9@��@�u@r�@Q�@Q�@A�@A�@A�@A�@b@�G�O�@l�@�R@V@@�@@�h@p�@`B@/@�@V@V@�/@�/@�/@�/@��@��@��@�/@�/@�/@�/G�O�@�m@�F@��@��G�O�@�\@^5@�@��@��@��@��@x�@hs@7L@�@%@ ��@ �9@ �9@ �u@ �@ r�@ r�@ Q�@ 1'@  �@  �@  �@  �@   ?�;d?��G�O�?���?�O�?�O�?�p�?��h?��h?��h?��h?�p�111111111111111111111111111111111111111111111111111111111111311111111111111111111111111111111111111111111111111111111111111111111111113111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111144111111111111111111111111111111114411111111111411111111111411111111111111111111141111111111114111111111111111111111111114111141111111111111111114411111111111111111111111111111111111111111111111111141111111111111111111111111111111114111111111411111111111111111111111111111441111111111111114111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111141111411111111111111111111111111114111111111Bx�Bw�Bv�Bx�Bx�By�By�Bw�Bx�Bw�Bu�Br�Bn�Bx�B��B��BŢBǮB�dB��B�B�VB�?B�-B�FB��B�\B��B��B��B�DBt�BQ�BXBS�BI�B@�B33B6FB.B�BDB�B��B��B�B��B�!B�jB�-B��Bz�BZBYBVBO�BI�BA�B8RB%�B
��BB
��B
�mB
��B
�#B
��B
�XB
��B
��B
��B
��B
�\B
�B
y�B
cTB
ZB
XB
e`B
^5B
T�B
O�B
C�B
@�B
2-B
�B
B	��B	�#B	�mB	�B	��B	�fB	�ZB	�B	��B	ƨB	ȴB	�FB	��B	��B	��B	�7B	y�B	r�B	�%B	y�B	dZB	cTB	_;B	YB	]/B	aHB	_;B	XB	R�B	K�B	Q�B	@�B	>wB	8RB	2-B	7LB	1'B	(�B	�B	!�B	�B	JB	B	1B		7B	B��B�#B�B�B�HB�B�B�mB�B�B�B�B�B�B�B�B�B�B�B�mB�NB�)B��B�qB�qB�wB�wB�qB�RB�XB�wBB��B�wB�jB�^B�?B�'B�'B�B��B��B�B�=B��B��B��B�VB�B{�By�Bm�B�%B�B�B� Bx�Br�Bx�Bm�Bl�Bx�Bm�BdZB^5BffBcTBR�BR�B>wBS�BP�BXBVBO�BM�BC�BC�B>wB:^B49B,B�B+B.B+B0!B6FB5?B7LB0!B'�B+B �B33B8RB5?B0!B-B �B{BhB{B!�B(�B$�B�B�BoBJB�B�B�B,B,B-B,B)�B%�B �B�BoB
=BB�B �B�B�B�B�B�B�B\BDBDBbB�B!�B!�B"�B"�B�BuB
=BbB�B�B�B�B �B�B�B�B�BoB$�B!�B�B�BoB.B.B"�B/B-B8RB:^B:^B2-B(�B1'BC�BE�BE�BD�BB�BA�B?}B@�BK�BS�BQ�BK�B>wBE�BI�BZB]/BdZBgmBffBdZBe`BcTBbNBbNBo�Bp�Bl�BjBr�Bu�Bs�Bt�Bs�Bm�Bs�Bs�Bp�Bn�Bx�B�B~�B}�B�JB�uB�{B��B�{B�oB�hB�bB�oB�oB��B�oB�uB��B�B��B��B�3B�'B�B�FB�RB�?B�qB��B��B��B�B�B��B��B�B�B�B�B�B�/B�BB�TB�5B�fB�B�B��B��B	B	B	  B��B��B	B	VB	uB	oB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	#�B	(�B	,B	.B	0!B	33B	6FB	7LB	9XB	?}B	A�B	B�B	B�B	A�B	A�B	A�B	E�B	G�B	J�B	L�B	L�B	O�B	R�B	VB	XB	W
B	YB	^5B	cTB	e`B	e`B	e`B	gmB	gmB	ffB	hsB	k�B	k�B	k�B	iyB	ffB	ffB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�%B	�1B	�JB	�VB	�\B	�bB	�bB	�hB	�hB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�'B	�-B	�3B	�9B	�9B	�9B	�9B	�3B	�3B	�9B	�FB	�LB	�RB	�XB	�XB	�XB	�XB	�LB	�?B	ÖB	ÖB	ƨB	ɺB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�B	�
B	��B	�B	�;B	�HB	�NB	�HB	�NB	�HB	�TB	�TB	�sB	�mB	�`B	�TB	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	�B	�B	��B	�B	�B	�B	��B	��B	��B	��B	��B	��B
B
B
  B	��B	��B	��B	��B	��B
B
B	��B
  B
%B
1B
1B
1B
+B
+B
%B
B
1B
+B
%B
B
+B
1B
JB
PB
PB
VB
\B
\B
\B
bB
\B
\B
\B
\B
\B
bB
bB
bB
bB
bB
\B
VB
\B
bB
bB
bB
VB
\B
bB
\B
VB
JB
VB
VB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
!�B
#�B
#�B
#�B
$�B
$�B
$�B
%�B
%�B
%�B
$�B
#�B
$�B
&�B
&�B
&�B
&�B
&�B
'�B
'�B
(�B
(�B
(�B
(�B
(�B
(�B
(�B
)�B
)�B
(�B
(�B
'�B
&�B
$�B
!�B
!�B
 �B
%�B
(�B
)�B
-B
0!B
33B
49B
5?B
33B
2-B
0!B
33B
6FB
7LB
8RB
8RB
8RB
9XB
8RB
8RB
9XB
9XB
9XB
9XB
9XB
8RB
8RB
8RB
8RB
7LB
8RB
8RB
8RB
8RB
8RB
:^B
:^B
:^B
:^B
:^B
:^B
:^B
:^B
9XB
5?B
;dB
@�B
@�B
@�B
@�B
@�B
@�B
?}B
>wB
<jB
<jB
@�B
@�B
B�B
A�B
A�B
D�B
C�B
B�B
D�B
C�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
E�B
D�B
C�B
B�B
F�B
G�B
I�B
J�B
J�B
K�B
I�B
G�B
G�B
K�B
M�B
L�B
M�B
M�B
N�B
M�B
O�B
P�B
O�B
O�B
P�B
P�B
O�B
M�B
N�B
O�B
O�B
P�B
Q�B
Q�B
Q�B
Q�B
P�B
Q�B
S�B
S�B
R�B
VB
VB
VB
W
B
XB
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
W
B
VB
VB
VB
XB
XB
ZB
ZB
ZB
[#B
[#B
[#B
ZB
[#B
\)B
[#B
\)B
\)B
\)B
^5B
^5B
]/B
]/B
\)B
^5B
]/B
\)B
]/B
^5B
^5B
^5B
^5B
^5B
^5B
_;B
^5B
]/B
[#B
]/B
aHB
aHB
aHB
aHB
aHB
bNB
cTB
cTB
cTB
cTB
cTB
cTB
bNB
bNB
cTB
cTB
cTB
dZB
cTB
e`B
e`B
e`B
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
e`B
e`B
ffB
ffB
ffB
e`B
ffB
ffB
gmB
gmB
gmB
ffB
ffB
ffB
gmB
gmB
gmB
hsB
gmB
ffB
ffB
hsB
hsB
hsB
hsB
iyB
iyB
hsB
hsB
hsB
gmB
gmB
ffB
dZB
bNB
ffB
hsB
iyB
iyB
iyB
jB
jB
k�B
k�B
l�B
l�B
k�B
m�B
n�B
m�B
n�B
n�B
n�B
n�B
n�B
m�B
l�B
k�B
iyB
l�B
m�B
m�B
l�B
iyB
m�B
n�B
n�B
p�B
p�B
p�B
p�B
p�B
o�B
p�B
p�B
p�B
q�B
q�B
q�B
q�B
r�B
r�B
r�B
s�B
s�B
s�B
s�B
s�B
r�B
q�B
q�B
s�B
q�B
r�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111Bx�Bw�Bv�Bx�Bx�By�By�Bw�Bx�Bw�BvBsMBo�Bz�B��B�BƨB��B�VB�XB�)B�B��B�B��B��B�B�B��B�/B��BwLBU�BY1BU2BK^BBAB5tB7�B/�B�B�B�+B�PB��B�5B��B�9B�wB�9B��B~wB^jB[=BW�BQ4BJ�BB�B9>B'�B �BMB
�BB
��B
� B
�)B
�(B
��B
��B
�>B
��B
��B
��B
��B
{B
e�B
\]B
YB
e�B
^�B
U�B
P�B
D�B
AUB
3�B
 B
�B	��B	�jB	��B	�B	�?B	�B	�`B	ڠB	�PB	��B	��B	��B	�xB	�_B	�B	�^B	{�B	t�B	�tB	z�B	f�B	d�B	a-B	Z�B	^5B	a�B	_�B	YB	TFB	MB	R�B	B[B	@ B	9�B	3�B	8B	2GB	*eB	!bB	"�B	�B	"B	�B	�B		�B	�B�8B�B�CB��B�4B��B�B�
B��B��B��B�B��B��B��B��B��B��B�B�B�B��B�B��B��B��B�}B�]B��B�*B��BªB��B��B��B��B��B��B��B��B��B�B�YB��B��B�5B�B�vB��B}qB{Bo�B��B��B��B��By�Bs�By�BoBm�By	Bn�Be�B_�BgBdZBT�BT{BA;BT�BR:BX�BV�BP�BN�BEBD�B?�B;�B5�B./B!�B,qB/iB,qB1'B6�B6+B7�B1AB)_B,WB"hB3�B8�B5�B0�B-�B!�B�BuB�B"�B)_B%zB�BjB�B�BB�BpB,=B,WB-CB,WB*eB&fB!|BqBuB�B�BB!BBBBKB=B1BHB~B0B4B�B!�B!�B"�B#BB{B�B�B�BdB�B�B!|B�B�B�B�B�B%FB"�BB=B�B.}B.�B$tB0B.IB8�B:�B:�B33B*�B2aBC�BE�BFBEBC-BBB@�BAoBL0BTFBRTBL~B@BF�BJ�BZ�B]�Bd�Bg�Bf�Bd�Be�Bc�Bb�Bc:Bo�Bp�BmBkBr�Bu�BtBt�BtBncBt9BtTBqvBo�By�B�oB�B~�B��B��B��B��B��B��B��B��B��B�B��B�&B�FB��B�B��B��B�hB��B��B��B��B�+B�(B�)B� B�,B�9B�9B�2B�MB�EB�EB�KB�eB�BݘB��B��B�B�B��B�B�B�.B	 B	 B	 4B�qB��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B		B	B	$B	)*B	,=B	.cB	0UB	3hB	6zB	7�B	9�B	?�B	A�B	B�B	B�B	A�B	A�B	A�B	E�B	G�B	J�B	L�B	MB	PB	S@B	VB	X+B	WYB	YeB	^jB	cnB	e�B	e�B	e�B	g�B	g�B	f�B	h�B	k�B	k�B	k�B	i�B	g8B	g�B	��B	�B	�GB	�GB	�3B	�9B	�SB	�9B	�3B	�3B	�[B	�YB	�fB	�dB	�pB	�vB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�:B	�DB	�'B	�GB	�MB	�9B	�9B	�TB	�9B	�MB	�MB	�nB	�`B	�fB	�lB	�rB	��B	��B	��G�O�G�O�B	ðB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	��B	�B	��B	�B	��B	�B	��B	��B	�B	�B	�B	�B	�B	�B	�FB	�$B	�+B	�7B	�QB	�G�O�G�O�B	�kB	�VB	�bB	�hB	�|B	�hB	�|B	�B	�B	�B	�G�O�B	��B	�B	�B	�B	��B	��B	�B	�B	��B	��B	��G�O�B	��B	��B	��B	��B	�-B	�B	�G�O�B	�%B	�	B	�$B
B
 B
 B	�B	�B	�BB	�.B	�]B
 B
;G�O�B
 OB
%B
1B
KB
1B
EB
EB
?B
SB
1B
EB
YG�O�B
_B
�B
dB
jB
jB
�B
\B
vB
\B
bB
vB
vB
�B
vB
vB
}B
bB
bB
}B
}B
vB
pB
vB
}B
}B
}G�O�B
vB
}B
�B
�G�O�B
�B
�B
�B
�B
�B
�G�O�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
G�O�G�O�B
�B
	B
�B
 �B
!�B
#�B
#�B
#�B
$�B
%B
%B
%�B
%�B
%�B
$�B
#�B
$�B
'B
'B
'B
'B
'B
(
B
(
B
(�B
(�B
)*B
(�B
)*B
)B
)B
)�B
*B
)B
)B
(
B
'G�O�G�O�B
"hB
!HB
&LB
)_B
*eB
-]B
0oB
3MB
4TB
5?G�O�B
2�G�O�B
3�B
6`B
7fB
8RB
8lB
8RB
9XB
8RB
8lB
9XB
9XB
9XB
9rB
9XB
8lB
8lB
8lB
8lB
7�B
8lB
8lB
8lB
8�B
8�B
:xB
:�B
:xB
:�B
:�B
:�B
:xB
:xB
9�G�O�B
;�B
@�B
@�B
@�B
@�B
@�B
@�B
?�B
>�G�O�B
<�B
@�B
@�B
B�B
A�B
A�B
D�B
C�B
B�B
D�B
C�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
E�B
D�B
C�B
B�B
F�B
G�B
I�B
J�B
J�B
K�G�O�G�O�B
G�B
K�B
M�B
L�B
M�B
M�B
N�B
NB
O�B
P�B
O�B
O�B
P�B
P�B
O�G�O�B
N�B
PB
PB
Q B
RB
RB
RB
RB
QB
RB
TB
TB
S&B
VB
VB
V9B
W
B
X+B
W
B
W$B
W?B
W$B
W$B
W$B
W$B
VB
V9B
V9B
X+B
X+B
ZB
Z7B
Z7B
[#B
[=B
[=B
Z7B
[=B
\CB
[=B
\CB
\]B
\CB
^OB
^OB
]dB
]IB
\]B
^5B
]IB
\]B
]IB
^jB
^OB
^OB
^jB
^jB
^jB
_;B
^OB
]~G�O�B
]~B
aHB
a|B
abB
abB
a|B
bhB
cnB
cTB
cTB
cTB
cnB
cTB
bhB
bhB
cnB
cnB
c�B
dZB
cnB
ezB
e`B
e`B
e`B
e`B
e`B
e`B
ezB
ezB
ffB
ffB
ffB
f�B
ffB
ezB
ezB
ffB
ffB
ffB
ezB
f�B
f�B
gmB
g�B
gmB
f�B
f�B
f�B
g�B
g�B
g�B
hsB
gmB
f�B
f�B
h�B
hsB
h�B
h�B
iyB
iyB
hsB
h�B
h�B
g�B
g�B
f�G�O�B
b�B
f�B
h�B
iyB
i�B
i�B
j�B
j�B
k�B
k�B
l�B
l�B
k�B
m�B
n�B
m�B
n�B
n�B
n�B
n�B
n�B
m�B
l�B
k�G�O�B
l�B
m�B
m�B
l�G�O�B
m�B
n�B
n�B
p�B
p�B
p�B
p�B
p�B
o�B
p�B
p�B
p�B
q�B
q�B
q�B
q�B
r�B
r�B
r�B
s�B
s�B
s�B
s�B
s�B
r�B
q�B
q�B
s�G�O�B
r�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�111111111111111111111111111111111111111111111111111111111111311111111111111111111111111111111111111111111111111111111111111111111111113111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111144111111111111111111111111111111114411111111111411111111111411111114111111111111141111111111114111111111111111111111111114111141111114111111111114411111111111111111111111111111111111114411111111114141111111111111111111111111111111114111111111411111111111111111111111111111441111111111111114111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111141111411111111111111111111111111114111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.05(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201806080039002018060800390020180608003900201806221242552018062212425520180622124255201806090028552018060900285520180609002855  JA  ARFMdecpA19c                                                                20180604093503  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20180604003505  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20180604003508  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20180604003508  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20180604003509  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20180604003509  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20180604003509  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20180604003509  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20180604003509  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20180604003509                      G�O�G�O�G�O�                JA  ARUP                                                                        20180604005512                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20180604153644  CV  JULD            G�O�G�O�F�=�                JM  ARSQJMQC2.0                                                                 20180605000000  CF  PSAL_ADJUSTED_QCB�  D�  G�O�                JM  ARSQJMQC2.0                                                                 20180605000000  CF  TEMP_ADJUSTED_QCB�  D�  G�O�                JM  ARCAJMQC2.0                                                                 20180607153900  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180607153900  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180608152855  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622034255  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115131517                      G�O�G�O�G�O�                