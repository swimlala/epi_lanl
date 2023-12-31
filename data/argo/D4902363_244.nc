CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-05-29T00:35:19Z creation;2018-05-29T00:35:25Z conversion to V3.1;2019-12-19T07:41:14Z update;     
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20180529003519  20200115131517  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0576_244                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @�f5�5� 1   @�f6�[�@:���>B�dD�0��1   GPS     A   B   B   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�ff@�  @���AffA@  Aa��A�  A�33A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3D � D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D  D�fD  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D}��D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�<�D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�3D�33111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@���@�ff@�33A��A?33A`��A33A���A���A���A���Aϙ�Aߙ�AA���B��B��B��B��B'��B/��B7��B?��BG��BO��BW��B_��Bg��Bo��Bw��B��B��fB��fB��3B��3B��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fC�3C�3C�3C�3C	�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C!�3C#�3C%�3C'�3C)�3C+�3C-�3C/�3C1�3C3�3C5�3C7�3C9�3C;�3C=�3C?�3CA�3CC�3CE�3CG�3CI�3CK�3CM�3CO�3CQ�3CS�3CU�3CW�3CY�3C[�3C]�3C_�3Ca�3Cc�3Ce�3Cg�3Ci�3Ck�3Cm�3Co�3Cq�3Cs�3Cu�3Cw�3Cy�3C{�3C}�3C�3C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C�fC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D |�D ��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��DvfD��D|�D��D	|�D	��D
|�D
��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��DvfD��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��DvfD��D�3D��D|�D��D|�D��D|�D��D|�D��D |�D ��D!|�D!��D"|�D"��D#|�D#��D$|�D$��D%|�D%��D&|�D&��D'|�D'��D(|�D(��D)|�D)��D*|�D*��D+|�D+��D,|�D,��D-|�D-��D.|�D.��D/|�D/��D0|�D0��D1|�D1��D2|�D2��D3|�D3��D4|�D4��D5|�D5��D6|�D6��D7|�D7��D8|�D8��D9|�D9��D:|�D:��D;|�D;��D<|�D<��D=|�D=��D>|�D>��D?|�D?��D@|�D@��DA|�DA��DB|�DB��DC|�DC��DD|�DD��DE|�DE��DF|�DF��DG|�DG��DH|�DH��DI|�DI��DJ|�DJ��DK|�DK��DL|�DL��DM|�DM��DN|�DN��DO|�DO��DP|�DP��DQ|�DQ��DR|�DR��DS|�DS��DT|�DT��DU|�DU��DV|�DV��DW|�DW��DX|�DX��DY|�DY��DZ|�DZ��D[|�D[��D\|�D\��D]|�D]��D^|�D^��D_|�D_��D`|�D`��Da|�Da��Db|�Db��Dc|�Dc��Dd|�Dd��De|�De��Df|�Df��Dg|�Dg��Dh|�Dh��Di|�Di��Dj|�Dj��Dk|�Dk��Dl|�Dl��Dm|�Dm��Dn|�Dn��Do|�Do��Dp|�Dp��Dq|�Dq��Dr|�Dr��Ds|�Ds��Dt|�Dt��Du|�Du��Dv|�Dv��Dw|�Dw��Dx|�Dx��Dy|�Dy��Dz|�Dz��D{|�D{��D||�D|��D}|�D}�fD~|�D~��D|�D��D�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD���D��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD¾fD��fD�>fD�~fDþfD��fD�>fD�~fDľfD��fD�>fD�~fDžfD��fD�>fD�~fDƾfD��fD�>fD�~fDǾfD��fD�>fD�~fDȾfD��fD�>fD�~fDɾfD��fD�>fD�~fDʾfD��fD�>fD�~fD˾fD��fD�>fD�~fD̾fD��fD�>fD�~fD;fD��fD�>fD�~fDξfD��fD�>fD�~fDϾfD��fD�>fD�~fDоfD��fD�>fD�~fDѾfD��fD�>fD�~fDҾfD��fD�>fD�~fDӾfD��fD�>fD�~fDԾfD��fD�>fD�~fDվfD��fD�>fD�~fD־fD��fD�>fD�~fD׾fD��fD�>fD�~fDؾfD��fD�>fD�~fDپfD��fD�>fD�~fDھfD��fD�>fD�~fD۾fD��fD�>fD�~fDܾfD��fD�>fD�~fDݾfD��fD�>fD�~fD޾fD��fD�>fD�~fD߾fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�;3D�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD��fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD���D��fD�>fD�~fD��fD��D�1�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�ȴA��^A��jA��RA���A�dZA��A��A���A�dZA�33A��yA�C�A��!A�dZA���A���A���A�E�A��/A�l�A��mA���A��A��A�I�A���A�
=A�x�A�%A�;dA�(�A�(�A�"�A��A��A��A�ȴA�t�A�^5A�9XA�t�A���A��PA�1'A��;A�^5A���A�E�A���A�7LA��A�ȴA�E�A���A��/A�ƨA��^A�jA�1A�^5A�n�A�1A��#A�VA�bA���A��^A��;A��A�I�A�ƨA�9XA��;A���A�^5A���A~ȴA|�/Ay
=AwoAvE�Au�As�As"�Ar�HAr�\Aq��Ap�Am7LAlZAk�^Ak?}AjM�Ag��Af��AedZAdZAc��A`�A]p�A\v�A[��AZZAW��AW%AV�uAU��AT��AS\)AP��AO�AO��AO?}AN��ANA�AL��AL^5AL9XAKp�AJ�AJJAI�wAIoAHn�AF�`AFbAE�PAD�AD�jAC��ACK�ABQ�AA��A?K�A=�A=�A=oA;��A:��A:bA9`BA9�A8�9A8jA7;dA5|�A4-A2�\A2(�A1�A0��A0�A/��A/�-A/C�A.�HA.5?A-\)A,n�A*�\A)�
A)/A(v�A'XA&ȴA&bNA%�TA%O�A$��A$jA#��A#S�A"�A!�#A �RA �\A ~�A A�A`BA��AƨAn�A�An�AƨAx�A�9A�AO�A�`AM�A�TA��AO�AA�9AffA�^AI�A��AE�A��AoAz�A��A��A
�`A
z�A
{A	x�AbAp�AO�A��AbNA��A��A��A�uA ��A (�@��
@�S�@��@�5?@���@�p�@�?}@��j@��@�n�@�@���@�Z@���@���@��@�J@�O�@��@���@���@��j@�+@�hs@�F@��H@��@���@�S�@�O�@��@��@�D@�@�n�@���@�ƨ@��
@�ƨ@��#@�Z@�33@��H@��H@���@ڸR@ّh@�&�@�z�@�\)@��H@�n�@�{@Ձ@ԣ�@���@Ӆ@�E�@���@�1@�o@���@��m@�K�@�v�@�M�@��@���@�+@��@�&�@�dZ@§�@�J@���@�7L@�Q�@�ff@��@���@���@�X@�G�@��@�;d@��7@�1@���@��@�x�@���@��#@��@�I�@�\)@�+@���@��@��@�1@�t�@�33@�33@�o@���@�C�@�$�@���@���@�z�@�l�@�-@�X@��@�%@�j@���@��@�33@�+@���@�5?@�J@��@�X@�O�@��@���@��@���@�;d@���@�ff@�J@��`@�z�@���@���@�ȴ@���@�7L@���@�(�@���@�ȴ@�E�@�$�@�X@��9@�Z@�I�@�1@��m@��m@��m@�l�@���@�~�@�M�@�-@��@��@�@�p�@�%@� �@��@�$�@��@�?}@��D@�b@���@���@�S�@�
=@�ȴ@��\@�E�@��T@���@�hs@��@���@��@��j@���@�bN@�ƨ@���@�|�@��@��@�ȴ@���@�v�@�V@�-@�@��^@�x�@�?}@�/@�V@���@�A�@�  @�;@l�@+@�@~��@~�R@~�+@~V@~$�@~{@}��@|��@|z�@|j@|Z@|I�@|(�@{�
@{�@{��@{33@z~�@z=q@y��@y�#@y��@yX@y%@x  @w�@w|�@w
=@v�@vȴ@vff@v@u`B@u/@u�@t�@t�j@t�D@s�m@sS�@r�@rJ@qhs@p��@p�9@pr�@o�w@o;d@o�@o�@o�@o
=@n��@n�y@n�@n�@n��@n{@m�@m�T@m�T@m@m��@mp�@mO�@m/@l��@lz�@l�@kƨ@kS�@j�@j=q@i�#@i�@i��@i�@i�@i�#@i7L@h��@h��@hA�@g�;@g�P@gK�@g
=@e�T@ep�@e`B@e?}@e?}@e�@dI�@c�F@b��@b�@a��@aG�@`�9@`�@` �@_|�@_�@^ȴ@^��@^E�@]�T@]`B@\��@\�@\�@\��@\�@\j@\9X@\1@[t�@Z�@Z��@Z^5@Y�#@Y&�@X��@XbN@X �@W�;@W�@W��@W�P@Wl�@WK�@W�@V��@V@U��@U�-@U�-@U��@Up�@U`B@U�@T��@Tz�@T�@S�
@S��@S�@SS�@R�H@R~�@R=q@Q��@Q��@Q%@P��@P�9@P�u@Pr�@Pb@O��@O�w@O|�@O\)@O+@Nv�@N{@M�-@L��@L�j@L�D@Lj@L(�@K�
@K��@KS�@K"�@Ko@K@K@J�H@J��@J�\@Jn�@J^5@J=q@J-@J�@J�@I��@Ix�@HĜ@H�u@H1'@G�P@F�R@FE�@E�T@E�-@E�h@Ep�@E/@E�@EV@D�D@D�@C��@CdZ@Co@B�H@B��@A�@Ax�@Ahs@AG�@A%@@�`@@�9@?�@?+@?
=@>��@>�y@>�y@>ȴ@>��@>�+@>E�@>@=�T@=��@=�-@=��@=p�@=/@=�@<�@<��@<z�@;ƨ@;o@:��@:n�@9�#@9X@9G�@9&�@8��@8A�@7K�@6ȴ@6��@6�+@6E�@5V@4�/@4��@4�j@4�@4��@4j@3��@3�F@3�@3t�@3S�@3C�@333@3"�@3o@3@2��@2=q@2�@2�@2�@2J@1��@1�7@0��@01'@/\)@.�y@.v�@.5?@.@-�-@-?}@-V@-V@,�@,��@,1@+ƨ@+�F@+��@+�@+�@+t�@+t�@+"�@*n�@)��@)X@)7L@(�9@(�@(1'@(  @'l�@&�y@&�R@&v�@&5?@&@%��@%?}@$�j@$z�@$(�@#�m@#�
@#�F@#�@#dZ@#"�@"~�@!��@!�^@!�7@!7L@!�@ �`@ ��@ Ĝ@ ��@ �@ Q�@�@+@��@�R@�+@v�@E�@5?@$�@@��@��@�-@�h@p�@O�@/@V@��@�/@�@��@Z@(�@ƨ@��@t�@dZ@dZ@S�@33@"�@@��@n�@^5@^5@M�@=q@�@J@��@�#@�^@��@�7@7L@%@��@Q�@1'@ �@ �@  @�w@�P@l�@�@
=@�@ff@@@�h@�@O�@�@�/@�D@I�@��@�m@�
@�
@ƨ@ƨ@�F@��@�@S�@�@��@��@M�@J@��@��@�@��@�#@��@hs@%@�`@��@bN@ �@��@K�@��@�R@��@E�@$�@$�@@�-@?}@��@�/@�/@��@Z@ƨ@�@33@o@@
�H@
��@
��@
��@
��@
n�@
M�@
-@	��@	�@	�@	��@	��@	X@	G�@	7L@	%@��@��@��@��@Ĝ@Ĝ@Ĝ@Ĝ@�9@�9@��@bN@Q�@A�@A�@ �@b@  @��@\)@
=@ȴ@�+@$�@@�@�T@@@��@@p�@/@V@V@�@�/@�j@�D@Z@�
@ƨ@�F@��@S�@C�@o@�@�H@��@~�@n�@^5@=q@-@-@-@-@��@�@�^@��@�7@�7@�7@�7@�7@�7@�7@x�@x�@hs@X@X@7L@%@%@%@%@%@�@�@�@%@%@ �`@ ��@ �`@ �`@ �`111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�ȴA��^A��jA��RA���A�dZA��A��A���A�dZA�33A��yA�C�A��!A�dZA���A���A���A�E�A��/A�l�A��mA���A��A��A�I�A���A�
=A�x�A�%A�;dA�(�A�(�A�"�A��A��A��A�ȴA�t�A�^5A�9XA�t�A���A��PA�1'A��;A�^5A���A�E�A���A�7LA��A�ȴA�E�A���A��/A�ƨA��^A�jA�1A�^5A�n�A�1A��#A�VA�bA���A��^A��;A��A�I�A�ƨA�9XA��;A���A�^5A���A~ȴA|�/Ay
=AwoAvE�Au�As�As"�Ar�HAr�\Aq��Ap�Am7LAlZAk�^Ak?}AjM�Ag��Af��AedZAdZAc��A`�A]p�A\v�A[��AZZAW��AW%AV�uAU��AT��AS\)AP��AO�AO��AO?}AN��ANA�AL��AL^5AL9XAKp�AJ�AJJAI�wAIoAHn�AF�`AFbAE�PAD�AD�jAC��ACK�ABQ�AA��A?K�A=�A=�A=oA;��A:��A:bA9`BA9�A8�9A8jA7;dA5|�A4-A2�\A2(�A1�A0��A0�A/��A/�-A/C�A.�HA.5?A-\)A,n�A*�\A)�
A)/A(v�A'XA&ȴA&bNA%�TA%O�A$��A$jA#��A#S�A"�A!�#A �RA �\A ~�A A�A`BA��AƨAn�A�An�AƨAx�A�9A�AO�A�`AM�A�TA��AO�AA�9AffA�^AI�A��AE�A��AoAz�A��A��A
�`A
z�A
{A	x�AbAp�AO�A��AbNA��A��A��A�uA ��A (�@��
@�S�@��@�5?@���@�p�@�?}@��j@��@�n�@�@���@�Z@���@���@��@�J@�O�@��@���@���@��j@�+@�hs@�F@��H@��@���@�S�@�O�@��@��@�D@�@�n�@���@�ƨ@��
@�ƨ@��#@�Z@�33@��H@��H@���@ڸR@ّh@�&�@�z�@�\)@��H@�n�@�{@Ձ@ԣ�@���@Ӆ@�E�@���@�1@�o@���@��m@�K�@�v�@�M�@��@���@�+@��@�&�@�dZ@§�@�J@���@�7L@�Q�@�ff@��@���@���@�X@�G�@��@�;d@��7@�1@���@��@�x�@���@��#@��@�I�@�\)@�+@���@��@��@�1@�t�@�33@�33@�o@���@�C�@�$�@���@���@�z�@�l�@�-@�X@��@�%@�j@���@��@�33@�+@���@�5?@�J@��@�X@�O�@��@���@��@���@�;d@���@�ff@�J@��`@�z�@���@���@�ȴ@���@�7L@���@�(�@���@�ȴ@�E�@�$�@�X@��9@�Z@�I�@�1@��m@��m@��m@�l�@���@�~�@�M�@�-@��@��@�@�p�@�%@� �@��@�$�@��@�?}@��D@�b@���@���@�S�@�
=@�ȴ@��\@�E�@��T@���@�hs@��@���@��@��j@���@�bN@�ƨ@���@�|�@��@��@�ȴ@���@�v�@�V@�-@�@��^@�x�@�?}@�/@�V@���@�A�@�  @�;@l�@+@�@~��@~�R@~�+@~V@~$�@~{@}��@|��@|z�@|j@|Z@|I�@|(�@{�
@{�@{��@{33@z~�@z=q@y��@y�#@y��@yX@y%@x  @w�@w|�@w
=@v�@vȴ@vff@v@u`B@u/@u�@t�@t�j@t�D@s�m@sS�@r�@rJ@qhs@p��@p�9@pr�@o�w@o;d@o�@o�@o�@o
=@n��@n�y@n�@n�@n��@n{@m�@m�T@m�T@m@m��@mp�@mO�@m/@l��@lz�@l�@kƨ@kS�@j�@j=q@i�#@i�@i��@i�@i�@i�#@i7L@h��@h��@hA�@g�;@g�P@gK�G�O�@e�T@ep�@e`B@e?}@e?}G�O�@dI�@c�F@b��@b�@a��@aG�@`�9@`�G�O�@_|�@_�@^ȴ@^��@^E�@]�T@]`B@\��@\�@\�@\��@\�@\j@\9X@\1@[t�@Z�@Z��@Z^5@Y�#@Y&�@X��@XbN@X �@W�;@W�@W��@W�P@Wl�@WK�@W�@V��@V@U��@U�-@U�-@U��@Up�@U`B@U�@T��@Tz�@T�@S�
@S��@S�@SS�@R�H@R~�@R=q@Q��@Q��@Q%@P��@P�9@P�u@Pr�@Pb@O��@O�w@O|�@O\)G�O�@Nv�@N{@M�-@L��@L�j@L�D@Lj@L(�@K�
@K��@KS�@K"�@Ko@K@K@J�H@J��@J�\@Jn�@J^5@J=q@J-@J�@J�G�O�@Ix�@HĜ@H�uG�O�@G�P@F�R@FE�@E�T@E�-@E�h@Ep�@E/@E�@EV@D�D@D�@C��@CdZ@Co@B�H@B��@A�@Ax�@Ahs@AG�@A%@@�`G�O�@?�@?+@?
=@>��@>�y@>�y@>ȴ@>��@>�+@>E�@>@=�T@=��@=�-@=��@=p�@=/@=�@<�G�O�@<z�@;ƨ@;o@:��@:n�@9�#@9X@9G�G�O�@8��@8A�@7K�@6ȴ@6��G�O�G�O�@5V@4�/@4��@4�j@4�@4��@4j@3��@3�F@3�@3t�@3S�@3C�@333@3"�@3o@3@2��@2=q@2�@2�@2�G�O�@1��G�O�@0��@01'@/\)@.�y@.v�@.5?@.@-�-@-?}@-V@-V@,�@,��@,1@+ƨ@+�F@+��@+�@+�@+t�G�O�G�O�@*n�@)��@)X@)7L@(�9@(�@(1'@(  @'l�@&�y@&�R@&v�@&5?@&G�O�@%?}@$�j@$z�@$(�@#�m@#�
@#�F@#�@#dZG�O�@"~�@!��@!�^@!�7@!7L@!�@ �`@ ��@ Ĝ@ ��@ �@ Q�@�@+@��@�R@�+@v�@E�@5?@$�@@��@��@�-@�h@p�@O�@/@V@��@�/@�@��@Z@(�@ƨ@��@t�@dZ@dZ@S�@33@"�@@��@n�@^5@^5@M�@=q@�@J@��@�#@�^@��@�7@7L@%@��@Q�@1'@ �@ �G�O�@�w@�P@l�@�@
=G�O�@ff@@@�h@�@O�@�@�/@�D@I�@��@�m@�
@�
@ƨ@ƨ@�F@��@�@S�@�@��@��@M�@J@��@��@�@��@�#@��@hs@%@�`@��@bN@ �@��@K�@��@�R@��@E�@$�@$�@@�-@?}@��@�/@�/@��@Z@ƨ@�@33@o@@
�H@
��@
��@
��@
��@
n�@
M�@
-@	��@	�@	�@	��@	��@	X@	G�@	7L@	%@��@��@��@��@Ĝ@Ĝ@Ĝ@Ĝ@�9@�9@��@bN@Q�@A�@A�@ �@b@  @��@\)@
=@ȴ@�+@$�@@�@�T@@@��@@p�@/@V@V@�@�/@�j@�D@Z@�
@ƨ@�F@��@S�@C�@o@�@�H@��@~�@n�@^5@=q@-@-@-@-@��@�@�^@��@�7@�7@�7@�7@�7@�7@�7@x�@x�@hs@X@X@7L@%@%@%@%@%@�@�@�@%@%@ �`@ ��@ �`@ �`@ �`111113111111111111111111111111111111111131111111111311111111311111111311111311111111111131111311113111131111311111111111111111111111111111111111111111111111111111111111111111111131111111111111111111111111311111111111133111111111111111111111111111111111111111113111133111111111111111111111311111111111111111111111111111111111131111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111411111111411111111111111111111111111111111111111111111111111111111111111411111111111111111111111141114111111111111111111111114111111111111111111141111111141111144111111111111111111111141411111111111111111111441111111111111141111111114111111111111111111111111111111111111111111111111111111111111111114111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B��B��B��B�uB�B[#B7LB��B�sB�fB�BB��B��B��B�qB��B�9BÖBĜB��B�^B��B�wB�'B��B�uB�JB~�Bx�BYBA�BG�B;dB@�B=qB2-B>wB7LB:^B0!B�BhB�B�B	7B��B�yB��B�B�B��B�B�B�+B�B��B��B�7By�B_;B0!B"�B�B�BB
�TB
�sB
��B
�
B
�-B
�oB
w�B
�{B
�DB
�B
e`B
@�B
5?B
uB
�B
&�B
�B
�B
{B
{B
VB	��B	�`B	�}B	�
B	�#B	��B	B	��B	�!B	��B	��B	�uB	t�B	S�B	y�B	p�B	gmB	C�B	`BB	`BB	P�B	K�B	2-B	)�B	49B	C�B	;dB	1'B	1'B	�B	'�B	-B	�B	�B	�B	�B	bB	
=B��B	  B	+B	B	B��B�B�`B�BǮB��B�/B��BB��B��BǮB��BɺBB�3B��B��B��B�B��B��B��B�B��B��B��B�uB�7B�Bp�B�B�B�B|�B�B�%B�B� Bz�B�Bz�B{�Bw�Bm�BhsB{�Bz�Br�BbNB`BBXBI�BJ�BS�BS�BXBO�BJ�BXBT�BQ�BR�BVBS�BP�BO�BI�B<jB+B,B<jB9XB6FB0!B�B)�B/B49B2-B(�B�B+B49B-B(�B%�B�BPB��B��B"�B+B+B,B(�B+B,B)�B%�B�B�B%�B�B"�B$�B�B#�B)�B$�B&�B,B(�B"�B{B�B�B#�B$�B"�BuB{B2-B(�B%�B"�B�B�B �B,B%�BuBhB �B$�B%�B�BB�B#�B �B�B#�B%�B&�B#�B�B�B�B�B�B�B�B�BuB$�B&�B1'B+B�B(�B,B �B�B.B2-B49B33B-B'�B7LB>wB<jB9XB7LB/B!�B�B(�B.B7LB8RB-B+B=qB@�BA�BM�BL�BH�BF�BJ�BP�BVBW
BO�BA�B<jBT�BaHB]/BcTB`BBcTBm�Bv�Bw�Bs�Bt�Bw�Bz�B}�Bz�B|�B�B~�B�B�%B�B�B~�B�+B�+B�+B�JB�=B�B�bB�hB�uB�hB�uB��B��B��B��B��B�B�'B�B�'B�RB�wB�qB�}B��B�wB�^B�dBÖBƨBɺBɺBȴBǮBƨBŢBÖBƨB��B�;B�)B�HB�B�B�B�B��B��B��B��B��B	B	B	%B	
=B	JB	DB	PB	PB	PB	�B	�B	�B	�B	�B	!�B	#�B	&�B	'�B	)�B	-B	2-B	5?B	9XB	8RB	8RB	5?B	<jB	@�B	?}B	B�B	D�B	E�B	E�B	H�B	J�B	K�B	K�B	J�B	L�B	VB	XB	XB	XB	XB	XB	ZB	[#B	[#B	YB	]/B	_;B	bNB	bNB	bNB	cTB	dZB	jB	m�B	m�B	o�B	q�B	o�B	q�B	q�B	u�B	w�B	v�B	v�B	v�B	u�B	w�B	y�B	z�B	� B	�B	�+B	�1B	�1B	�PB	�oB	�uB	�uB	�uB	�uB	�uB	�{B	�{B	�uB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�?B	�?B	�FB	�LB	�LB	�LB	�XB	�jB	�qB	��B	ÖB	ÖB	ÖB	��B	ǮB	��B	��B	��B	ɺB	ŢB	ȴB	ȴB	��B	��B	��B	��B	�B	�B	��B	�
B	�B	�B	�B	�B	�B	�/B	�;B	�BB	�BB	�BB	�BB	�HB	�NB	�BB	�NB	�TB	�ZB	�TB	�ZB	�mB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
+B
1B
1B
1B
1B
1B
1B
	7B
	7B
	7B
	7B
	7B
	7B
1B
%B
%B
	7B
	7B
+B
	7B
JB
VB
hB
hB
hB
hB
oB
hB
bB
bB
oB
uB
{B
{B
{B
uB
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
�B
�B
�B
 �B
�B
!�B
$�B
#�B
!�B
 �B
 �B
$�B
'�B
'�B
%�B
"�B
+B
,B
,B
,B
,B
+B
)�B
,B
-B
.B
.B
.B
.B
.B
.B
.B
-B
,B
0!B
1'B
0!B
0!B
.B
.B
,B
,B
,B
0!B
1'B
2-B
33B
2-B
33B
5?B
6FB
5?B
49B
33B
6FB
8RB
8RB
8RB
8RB
8RB
7LB
5?B
33B
5?B
9XB
:^B
9XB
;dB
:^B
;dB
:^B
;dB
>wB
>wB
>wB
?}B
?}B
=qB
?}B
@�B
A�B
A�B
C�B
C�B
C�B
B�B
B�B
@�B
A�B
C�B
D�B
D�B
F�B
F�B
G�B
G�B
F�B
F�B
E�B
D�B
C�B
H�B
H�B
I�B
I�B
I�B
J�B
J�B
J�B
J�B
K�B
J�B
J�B
K�B
K�B
K�B
K�B
K�B
L�B
K�B
L�B
K�B
L�B
K�B
M�B
N�B
O�B
O�B
N�B
N�B
N�B
N�B
M�B
M�B
P�B
P�B
P�B
P�B
O�B
P�B
P�B
P�B
O�B
P�B
O�B
N�B
O�B
O�B
P�B
R�B
R�B
R�B
R�B
P�B
Q�B
Q�B
Q�B
S�B
R�B
P�B
Q�B
S�B
S�B
T�B
T�B
T�B
T�B
T�B
VB
W
B
XB
YB
YB
YB
YB
XB
XB
XB
XB
W
B
XB
YB
XB
ZB
[#B
[#B
[#B
[#B
[#B
ZB
YB
ZB
\)B
[#B
ZB
ZB
ZB
[#B
\)B
]/B
^5B
]/B
^5B
_;B
^5B
]/B
]/B
_;B
`BB
`BB
_;B
_;B
^5B
aHB
bNB
dZB
dZB
e`B
e`B
e`B
e`B
dZB
dZB
e`B
e`B
e`B
e`B
ffB
e`B
dZB
dZB
ffB
ffB
ffB
gmB
gmB
gmB
gmB
gmB
hsB
hsB
hsB
gmB
gmB
gmB
gmB
iyB
iyB
iyB
iyB
iyB
iyB
hsB
gmB
hsB
hsB
iyB
iyB
jB
k�B
k�B
k�B
l�B
l�B
k�B
jB
jB
l�B
l�B
l�B
l�B
l�B
k�B
k�B
jB
n�B
m�B
m�B
m�B
n�B
n�B
n�B
o�B
n�B
n�B
p�B
p�B
p�B
q�B
q�B
q�B
q�B
p�B
p�B
p�B
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
r�B
r�B
r�B
s�B
t�B
t�B
t�B
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
u�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B��B��B��B�,B��B`�BA�BB��B��B�BՁB�@B��B��B��B�FBĶBňB��B�B� B��B�GB��B�B��B��Bz^B\�BC�BI�B=�BA�B>�B3�B>�B8B:�B0�BxB&B�BmB
#B�dB�QB��BچB�
B��B��B��B�#B�B��B��B�#B{0BabB4�B%�BCB�B�B
�B
�B
�2B
�+B
�ZB
�B
z�B
��B
�B
��B
g�B
C�B
7�B
�B
�B
'�B
#B
�B
�B
�B
�B	�B	�B	�-B	�B	��B	��B	�B	��B	�[B	��B	�HB	�2B	xRB	W�B	z�B	rB	iyB	F�B	`�B	`�B	R:B	MB	4�B	,�B	5?B	DB	<B	2B	1�B	 \B	(�B	-]B	�B	�B	�B	CB	hB	DB��B	B	�B	�B	�B��B��B�BیB�XB�JB�~B�B�gB��B��BȚB�<B�XB�aB��B�'B��B�dB��B��B��B��B�WB�yB�|B��B��B��B��Bs3B�B�B�'B~wB��B��B��B��B{�B��B{�B|�Bx�Bo Bi�B|B{BshBc�Ba|BYBK�BLdBT�BT�BX�BQ4BL0BX�BU�BR�BS�BVmBT{BQ�BPbBJrB=�B-B-�B=<B:^B72B1AB)B+QB0B4�B2�B*0B5B+�B4nB-�B)�B&�B �B�B��B��B#TB+QB+kB,qB)�B+kB,WB*eB&�B�B~B&LB vB#:B%`B�B$ZB*KB%zB'8B,B)*B#nB�B�B�B$�B%zB#nB�B�B2B)�B&�B#�B�B�B!|B,B&LB�B�B!bB%,B&LB�BaBCB$@B!bB~B$@B&LB'mB$ZBpBOB BB�B�B]B�B�B�B%FB'�B1[B+�BKB)yB,�B"B�B.}B2�B4�B3�B-�B)DB7�B>�B<�B9�B7�B/�B# B!-B)�B/5B7�B8�B.cB,�B>BA BBABM�BM6BIlBGzBKxBQNBV9BWYBPHBB�B>BBU�Ba�B^Bc�BaHBd@Bn/Bv�BxBt9Bu%Bx8B{0B~(B{JB}VB�;BcB�SB�?B�aB�uB�B�zB��B��B�~B��B�B��B��B��B� B�FB�B�;B�-B�HB�nB�kB�[B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�%BāBǔB�{B�pBܬB��B��B��B��B�'B��B�	B�B�<B�HB	AB	SB	YB	
XB	dB	xB	�B	�B	�B	�B	�B	�B	�B	�B	!�B	$B	'B	($B	*0B	-]B	2|B	5tB	9rB	8�B	8�B	5�B	<�B	@�B	?�B	B�B	D�B	E�B	E�B	H�B	J�B	K�B	K�B	KB	M6B	VB	X+B	X+B	X+B	X+B	XEB	Z7B	[WB	[qB	YB	]IB	_pB	bhB	bhB	b�B	c�B	d�B	j�B	m�B	m�B	o�B	q�B	o�B	q�B	q�B	u�B	xB	v�B	v�B	v�B	vB	xB	z*B	{JB	�4B	�gB	�EB	��B	��B	��B	��B	��B	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�&B	�B	�RB	�CB	�%B	�?B	�`B	�LB	��B	��B	��B	��B	��B	��B	��B	��B	��G�O�B	��B	��B	��B	��B	��G�O�B	�B	�B	�B	�B	�B	� B	�B	�9G�O�B	�?B	�+B	�1B	�_B	�eB	�QB	�IB	�VB	�\B	�\B	�\B	�\B	�bB	�hB	�B	�B	�nB	�B	�B	��B	�B	�B	�B	�B	��B	�B	��B	��B	��B	�B	�B	��B	�B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	�B	�B	��B	�B	�B	�B	�0B	�B	�B	�B	�B	�"G�O�B	�BB	�BB	�BB
AB
-B
MB
MB
3B
SB
9B
EB
KB
1B
1B
KB
KB
KB
	lB
	RB
	RB
	7B
	lB
	RB
KG�O�B
tB
	lB
	�G�O�B
	�B
�B
pB
�B
�B
�B
�B
oB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�G�O�B
�B
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
�G�O�B
B
�B
�B
 �B
�B
!�B
$�B
$G�O�B
!B
!HB
%B
(
B
($G�O�G�O�B
+B
,B
,B
,"B
,"B
+B
*0B
,"B
-)B
.B
./B
.B
.B
.B
./B
.IB
-)B
,=B
0;B
1AB
0UB
0;G�O�B
.IG�O�B
,WB
,WB
0UB
1[B
2aB
3MB
2aB
3hB
5ZB
6`B
5ZB
4nB
3hB
6`B
8RB
8lB
8lB
8RB
8lB
7�G�O�G�O�B
5�B
9rB
:xB
9�B
;B
:�B
;�B
:�B
;�B
>�B
>�B
>�B
?�B
?�G�O�B
?�B
@�B
A�B
A�B
C�B
C�B
C�B
B�B
B�G�O�B
A�B
C�B
D�B
D�B
F�B
F�B
G�B
G�B
F�B
F�B
E�B
D�B
C�B
H�B
H�B
I�B
I�B
I�B
J�B
J�B
J�B
J�B
K�B
J�B
J�B
K�B
K�B
K�B
K�B
K�B
L�B
K�B
MB
K�B
MB
K�B
NB
N�B
O�B
O�B
OB
N�B
N�B
N�B
M�B
M�B
Q B
P�B
P�B
P�B
O�B
P�B
Q B
Q B
PB
Q B
O�B
OB
O�B
PB
QB
SB
R�B
R�B
SG�O�B
RB
RB
RB
S�B
SG�O�B
RB
TB
TB
T�B
U2B
U2B
U2B
UB
V9B
W$B
XB
Y1B
YB
YB
YB
X+B
X+B
X+B
XEB
WYB
XEB
Y1B
X+B
Z7B
[#B
[#B
[=B
[=B
[=B
Z7B
YKB
Z7B
\CB
[WB
ZQB
Z7B
ZQB
[=B
\CB
]dB
^OB
]dB
^jB
_;B
^OB
]IB
]~B
_VB
`\B
`BB
_VB
_pB
^jB
abB
b�B
dtB
dtB
e`B
ezB
e`B
e`B
d�B
dtB
ezB
ezB
ezB
e`B
f�B
ezB
dtB
dtB
f�B
ffB
f�B
g�B
gmB
gmB
g�B
gmB
h�B
hsB
hsB
gmB
gmB
g�B
g�B
i�B
iyB
iyB
i�B
iyB
i�B
h�B
g�B
h�B
h�B
i�B
i�B
j�B
k�B
k�B
k�B
l�B
l�B
k�B
j�B
j�B
l�B
l�B
l�B
l�B
l�B
k�B
k�B
j�B
n�B
m�B
m�B
m�B
n�B
n�B
n�B
o�B
n�B
n�B
p�B
p�B
p�B
q�B
q�B
q�B
q�B
p�B
p�B
p�B
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
r�B
r�B
r�B
s�B
t�B
t�B
t�B
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
u�111113111111111111111111111111111111111131111111111311111111311111111311111311111111111131111311113111131111311111111111111111111111111111111111111111111111111111111111111111111131111111111111111111111111311111111111133111111111111111111111111111111111111111113111133111111111111111111111311111111111111111111111111111111111131111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111411111111411111111111111111111111111111111111111111111111111111111111111411111111111111111111111141114111111111111111111111114111111111111111111141111111141111144111111111111111111111141411111111111111111111441111111111111141111111114111111111111111111111111111111111111111111111111111111111111111114111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;oG�O�;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;oG�O�;o;o;o;o;oG�O�G�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
G�O�<#�
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
G�O�<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.05(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201806020037122018060200371220180602003712201806221242392018062212423920180622124239201806042121112018060421211120180604212111  JA  ARFMdecpA19c                                                                20180529093513  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20180529003519  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20180529003521  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20180529003522  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20180529003522  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20180529003522  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20180529003523  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20180529003523  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20180529003524  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20180529003525                      G�O�G�O�G�O�                JA  ARUP                                                                        20180529005631                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20180529153557  CV  JULD            G�O�G�O�F�1�                JM  ARSQJMQC2.0                                                                 20180530000000  CF  PSAL_ADJUSTED_QCAa��D�  G�O�                JM  ARSQJMQC2.0                                                                 20180530000000  CF  TEMP_ADJUSTED_QCAa��D�  G�O�                JM  ARCAJMQC2.0                                                                 20180601153712  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180601153712  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180604122111  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622034239  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115131517                      G�O�G�O�G�O�                