CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-05-05T00:35:24Z creation;2018-05-05T00:35:31Z conversion to V3.1;2019-12-19T07:39:06Z update;     
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
resolution        =���   axis      Z        p  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     p  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \X   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     p  `4   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     p  s�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     p  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �<   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     p  �   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     p  �d   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     p  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     p  �D   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ۴   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �D   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �D   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �D   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �D   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �$   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �(   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �8   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �<   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �@   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �DArgo profile    3.1 1.2 19500101000000  20180505003524  20200116221514  5905048                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0577_237                     2C  D   NAVIS_A                         0577                            ARGO 102115                     863 @�`5�%� 1   @�`6�8�@4�q����dLoiDg81   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @9��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&�C(  C)�fC,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6y�D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=�fD>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�3D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�� 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@6ff@|��@�ff@�ffA33A?33A_33A33A���A���A���A���Aϙ�Aߙ�AA���B��B��B��B��B'��B/��B7��B?��BG��BO��BW��B_��Bg��Bo��Bw��B��B��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��B��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fC�3C�3C�3C�3C	�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C!�3C#�3C&�C'�3C)ٚC+�3C-�3C/�3C1�3C3�3C5�3C7�3C9�3C;�3C=�3C?�3CA�3CC�3CE�3CG�3CI�3CK�3CM�3CO�3CQ�3CS�3CU�3CW�3CY�3C[�3C]�3C_�3Ca�3Cc�3Ce�3Cg�3Ci�3Ck�3Cm�3Co�3Cq�3Cs�3Cu�3Cw�3Cy�3C{�3C}�3C�3C���C���C���C���C�fC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C�fC���C���C���C���C���C���C���C���C���C���C���C�fC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D |�D ��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D	|�D	��D
|�D
��D|�D��D|�D��D|�D��D|�D��D|�D��D�3D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D |�D ��D!|�D!��D"|�D"��D#|�D#��D$|�D$��D%|�D%��D&|�D&��D'|�D'��D(|�D(��D)|�D)��D*|�D*��D+|�D+��D,|�D,��D-|�D-��D.|�D.��D/|�D/��D0|�D0��D1|�D1��D2|�D2��D3|�D3��D4|�D4��D5|�D5��D6vfD6��D7|�D7��D8|�D8��D9|�D9��D:|�D:��D;|�D;��D<|�D<��D=�3D=��D>|�D>��D?|�D?��D@|�D@��DA|�DA��DB|�DB��DC|�DC��DD|�DD��DE|�DE��DF|�DF��DG|�DG��DH|�DH��DI|�DI��DJ|�DJ��DK|�DK��DL|�DL��DM|�DM��DN|�DN��DO|�DO��DP|�DP��DQ|�DQ��DR|�DR��DS|�DS��DT|�DT��DU|�DU��DV|�DV��DW|�DW��DX|�DX��DY|�DY��DZ|�DZ��D[|�D[��D\|�D\��D]|�D]��D^|�D^��D_|�D_��D`|�D`��Da|�Da��Db|�Db��Dc|�Dc��Dd|�Dd��De|�De��Df|�Df��Dg|�Dg��Dh|�Dh��Di|�Di��Dj|�Dj��Dk|�Dk��Dl|�Dl��Dm|�Dm��Dn|�Dn��Do|�Do��Dp|�Dp��Dq|�Dq��Dr|�Dr��Ds|�Ds��Dt|�Dt��Du|�Du��Dv|�Dv��Dw|�Dw��Dx|�Dx��Dy|�Dy��Dz|�Dz��D{|�D{��D||�D|��D}|�D}��D~|�D~��D|�D��D�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��D�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD���D��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��D�>fD�~fD¾fD��fD�>fD�~fDþfD��fD�>fD�~fDľfD��fD�>fD�~fDžfD��fD�>fD�~fDƾfD��fD�>fD�~fDǾfD��fD�>fD�~fDȾfD��fD�>fD�~fDɾfD��fD�>fD�~fDʾfD��fD�>fD�~fD˾fD��fD�>fD�~fD̾fD��fD�>fD�~fD;fD��fD�>fD�~fDξfD��fD�>fD�~fDϾfD��fD�>fD�~fDоfD��fD�>fD�~fDѾfD��fD�>fD�~fDҾfD��fD�>fD�~fDӾfD��fD�>fD�~fDԾfD��fD�>fD�~fDվfD��fD�>fD�~fD־fD��fD�>fD�~fD׾fD��fD�>fD�~fDؾfD��fD�>fD�~fDپfD��fD�>fD�~fDھfD��fD�>fD�~fD۾fD��fD�>fD�~fDܾfD��fD�>fD�~fDݾfD��fD�>fD�~fD޾fD��fD�>fD�~fD߾fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��D�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD��fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��f1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�E�A�E�A�E�A�C�A�C�A�C�A�E�A�G�A�E�A�E�A�I�A�I�A�E�A�?}A�?}A�=qA�?}A�=qA�?}A�?}A�A�A�;dA�/A�(�A��A���A��A�A�A���A��A�A�{AA�A��A��A�ƨA��7A�Q�A���A�K�A�K�A��A��+A�=qA�1A��A�M�A��/A��A�1'A���A��TA��jA��7A�1'A��^A�7LA�A���A��`A��A�=qA�9XA��hA�5?A�(�A�oA�C�A���A�hsA��A�C�A�E�A��DA��RA�Q�A�A��^A�I�A���A���A�(�A�~�A�oA��9A��A��
A��A�$�A�Q�A���A�I�A��A���A��mA��wA��A�"�A���A��A�G�A��A�Q�A��yA�z�A�O�A�~�A��A�1A��HA��A���A��-A�1A~�\A|ffAzz�Aw�
At��ApȴAo��Am��Ai��Ahz�Af{Ab�9A_�;A\r�A[�AYl�AYXAYhsAYp�AY��AX�9AV5?AS�
AR�DAQhsAP��AO/AM��AKK�AG��AGVAFffAEAD��AD-AC\)AB^5A@ȴA=�A<(�A;��A:~�A8JA6��A6(�A5��A5
=A4�A3p�A0�/A/��A.��A.�+A-�PA+�A*jA((�A'VA$ȴA"�yA"  A!�^A!"�Ap�A�/A�+A-A�mA%A��A33A�DAn�A$�A�AffAhsA�+A��A�#A�A�#AO�A��A��A��A+A�\AZA��A
��A
(�A	��A	�wA�AZA�A�FA�A+A��AM�A��A��A�mA�RA��A �!A Q�@�+@��@���@���@�b@�^5@��^@��@�r�@���@�X@�&�@�I�@�R@�O�@��@�b@�@睲@��H@�hs@�|�@�-@��@�I�@��@ߕ�@�^5@� �@�S�@�v�@�5?@��@ٙ�@ش9@�t�@���@��@���@�9X@�o@�-@�?}@�b@�;d@ΰ!@�$�@��@�@�&�@̓u@˥�@��@ʧ�@��@ȴ9@���@ǅ@���@�J@���@�x�@ģ�@�1@��
@å�@�\)@��@���@���@��@��@�G�@�b@�\)@�ȴ@�~�@��/@���@���@�A�@�\)@���@�x�@�/@���@��9@�j@�1@��@��P@�t�@�o@�n�@�@�x�@�7L@��@�Ĝ@�9X@��;@�C�@���@�-@��^@��^@���@�Ĝ@���@�b@�\)@��y@�n�@�5?@��@��-@�7L@��@�z�@�Q�@�1@�b@���@�|�@�
=@���@�O�@��@�z�@��@��w@��@�t�@�V@��-@���@�&�@���@��u@��@�j@�1@���@�dZ@���@��H@��@���@���@�M�@�{@��@���@��7@�7L@�%@��`@���@��j@�z�@�1@���@�|�@�K�@��@��@��@���@�v�@�V@�{@��-@�O�@�Ĝ@��u@�A�@�b@�  @�(�@�r�@�I�@�  @��;@��@���@��
@��w@���@���@��P@�\)@�C�@�C�@�C�@�C�@��H@��+@��@�
=@��y@���@�E�@��@���@���@���@��7@�G�@��@��@��9@�1'@��;@�1@���@��
@�l�@�l�@��@�dZ@�o@��H@�v�@�$�@�{@�J@�J@���@��@��D@�1'@�A�@�9X@� �@���@��F@�\)@��H@��R@���@�^5@�5?@�@���@��^@�/@��@��F@�C�@�
=@�o@��@��@��H@��y@��@��@���@���@���@��@��y@���@��R@���@�ff@�J@���@���@�@��h@�X@��@��@���@���@�1'@���@��@��@�C�@�33@�"�@��@�ȴ@���@��\@�^5@�M�@�J@���@��h@�X@�V@��@��`@��`@���@��@�(�@��@��
@��@�\)@�33@��@��y@��\@�^5@�=q@��@��@�`B@�`B@�&�@��@�b@�;@l�@~�y@~��@~�+@~{@~@}�T@}�@|�@|9X@|1@{ƨ@{��@{S�@z��@y�#@x��@x��@xr�@w
=@vE�@u@uO�@t�@t�@s�@sS�@sC�@r�H@q��@p�`@pQ�@o�@o�P@o\)@o+@n��@n�+@m@m@mp�@l�D@lI�@k�m@k��@k�@kt�@k"�@j=q@i��@iG�@i�@hQ�@h  @g��@g
=@f�+@f{@e`B@dz�@c��@b�\@b-@b^5@a��@a&�@`�`@`Ĝ@`��@`��@`r�@`bN@`A�@`b@_\)@_+@^ff@^@]�-@\Z@[t�@["�@Z�@Z��@Z�H@Y��@XĜ@X�u@X�u@Xr�@X �@W�;@W�w@W�w@Wl�@V�R@Vff@U�@U�@U`B@UO�@U/@T�@T�@Tz�@Tz�@T(�@S��@S�@SdZ@SS�@So@R�!@R��@R�@S"�@R��@R��@R��@R��@R��@R�!@R=q@Q��@Q��@Q%@PĜ@PA�@O�@OK�@N�@N��@N5?@M�T@M�-@M�h@Mp�@M?}@M�@L�/@L�j@L9X@K��@Kƨ@KS�@J�@J��@J�!@Jn�@JM�@J-@J-@J-@I�@I&�@H�@H1'@Hb@Hb@Hb@Hb@Hb@G��@Gl�@G+@F��@F�R@F�+@F5?@E@Ep�@EO�@E�@D��@D�@C��@C�m@C�m@C�
@C�F@C�@C"�@B�@B�@B��@Bn�@B-@A�@A�7@A&�@@��@@�u@@Q�@@ �@@b@?�@?��@?�@?|�@?+@?
=@>�y@>�+@>E�@>{@=@=�@=`B@=O�@<��@<�D@<1@;��@:�H@:n�@:J@9�@9��@9hs@9X@97L@8��@8Ĝ@8�@81'@7�@7��@7|�@7\)@7;d@7
=@6��@6E�@6@5�@5�T@5��@5@5p�@5/@5V@4�@3��@3��@3o@2��@2^5@2J@1��@1�#@1�7@1G�@1�@0��@0�9@0A�@01'@/��@/�w@/�w@/\)@/
=@.�+@.V@.{@-�T@-�@-/@,��@,��@,��@,�D@,j@,I�@+�m@+�@+C�@*��@*~�@*=q@)�@)�7@(��@(��@(r�@(bN@(bN@(Q�@(A�@( �@(  @'�P@';d@&��@&�y@&��@&ff@&$�@&@%�@%@%�h@%p�@%/@$�@$j@#��@#�F@#��@#��@#��@#��@#dZ@#dZ@#S�@#33@#o@"�@"�!@"n�@"=q@"-@"�@!��@!��@!�^@!�7@!x�@!hs@!X@!G�@!7L@!&�@ ��@ �`@ ��@ ��@ ��@ Ĝ@ r�@  �@��@\)@+@�y@��@v�@v�@$�@��@�-@�@p�@`B@/@��@�j@�D@9X@1@��@�m@�
@�
@ƨ@ƨ@�F@��@�@C�@C�@C�@33@"�@~�@J@��@hs@&�@��@�`@��@Ĝ@��@r�@A�@�@�P@+@��@�@V@$�@�@�-@p�@V@�@�@�D@j@j@I�@�@�@1@�m@�@33@��@�\@n�@-@��@X@&�@�@%@�`@�9@bN@b@�@�;@��@�w@�w@�@|�@\)@K�@;d@;d@+@+@�y@��@��@V@@�@�@�T@�T@�T@�T1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�E�A�E�A�E�A�C�A�C�A�C�A�E�A�G�A�E�A�E�A�I�A�I�A�E�A�?}A�?}A�=qA�?}A�=qA�?}A�?}A�A�A�;dA�/A�(�A��A���A��A�A�A���A��A�A�{AA�A��A��A�ƨA��7A�Q�A���A�K�A�K�A��A��+A�=qA�1A��A�M�A��/A��A�1'A���A��TA��jA��7A�1'A��^A�7LA�A���A��`A��A�=qA�9XA��hA�5?A�(�A�oA�C�A���A�hsA��A�C�A�E�A��DA��RA�Q�A�A��^A�I�A���A���A�(�A�~�A�oA��9A��A��
A��A�$�A�Q�A���A�I�A��A���A��mA��wA��A�"�A���A��A�G�A��A�Q�A��yA�z�A�O�A�~�A��A�1A��HA��A���A��-A�1A~�\A|ffAzz�Aw�
At��ApȴAo��Am��Ai��Ahz�Af{Ab�9A_�;A\r�A[�AYl�AYXAYhsAYp�AY��AX�9AV5?AS�
AR�DAQhsAP��AO/AM��AKK�AG��AGVAFffAEAD��AD-AC\)AB^5A@ȴA=�A<(�A;��A:~�A8JA6��A6(�A5��A5
=A4�A3p�A0�/A/��A.��A.�+A-�PA+�A*jA((�A'VA$ȴA"�yA"  A!�^A!"�Ap�A�/A�+A-A�mA%A��A33A�DAn�A$�A�AffAhsA�+A��A�#A�A�#AO�A��A��A��A+A�\AZA��A
��A
(�A	��A	�wA�AZA�A�FA�A+A��AM�A��A��A�mA�RA��A �!A Q�@�+@��@���@���@�b@�^5@��^@��@�r�@���@�X@�&�@�I�@�R@�O�@��@�b@�@睲@��H@�hs@�|�@�-@��@�I�@��@ߕ�@�^5@� �@�S�@�v�@�5?@��@ٙ�@ش9@�t�@���@��@���@�9X@�o@�-@�?}@�b@�;d@ΰ!@�$�@��@�@�&�@̓u@˥�@��@ʧ�@��@ȴ9@���@ǅ@���@�J@���@�x�@ģ�@�1@��
@å�@�\)@��@���@���@��@��@�G�@�b@�\)@�ȴ@�~�@��/@���@���@�A�@�\)@���@�x�@�/@���@��9@�j@�1@��@��P@�t�@�o@�n�@�@�x�@�7L@��@�Ĝ@�9X@��;@�C�@���@�-@��^@��^@���@�Ĝ@���@�b@�\)@��y@�n�@�5?@��@��-@�7L@��@�z�@�Q�@�1@�b@���@�|�@�
=@���@�O�@��@�z�@��@��w@��@�t�@�V@��-@���@�&�@���@��u@��@�j@�1@���@�dZ@���@��H@��@���@���@�M�@�{@��@���@��7@�7L@�%@��`@���@��j@�z�@�1@���@�|�@�K�@��@��@��@���@�v�@�V@�{@��-@�O�@�Ĝ@��u@�A�@�b@�  @�(�@�r�@�I�@�  @��;@��@���@��
@��w@���@���@��P@�\)@�C�@�C�@�C�@�C�@��H@��+@��@�
=@��y@���@�E�@��@���@���@���@��7@�G�@��@��@��9@�1'@��;@�1@���@��
@�l�@�l�@��@�dZ@�o@��H@�v�@�$�@�{@�J@�J@���@��@��D@�1'@�A�@�9X@� �@���@��F@�\)@��H@��R@���@�^5@�5?@�@���@��^@�/@��@��F@�C�@�
=@�o@��@��@��H@��y@��@��@���@���@���@��@��y@���@��R@���@�ff@�J@���@���@�@��h@�X@��@��@���@���@�1'@���@��@��@�C�@�33@�"�@��@�ȴ@���@��\@�^5@�M�@�J@���@��h@�X@�V@��@��`@��`@���@��@�(�@��@��
@��@�\)@�33@��@��y@��\@�^5@�=q@��@��@�`B@�`B@�&�@��@�b@�;@l�@~�y@~��@~�+@~{@~@}�T@}�@|�@|9X@|1@{ƨ@{��@{S�@z��@y�#@x��@x��@xr�@w
=@vE�@u@uO�@t�@t�@s�@sS�@sC�@r�H@q��@p�`@pQ�@o�@o�P@o\)@o+@n��@n�+@m@m@mp�@l�D@lI�@k�m@k��@k�@kt�@k"�@j=q@i��@iG�@i�@hQ�@h  @g��@g
=@f�+@f{@e`B@dz�@c��@b�\@b-@b^5@a��@a&�@`�`@`Ĝ@`��@`��@`r�@`bN@`A�@`b@_\)@_+@^ff@^@]�-@\Z@[t�@["�@Z�@Z��@Z�H@Y��@XĜ@X�u@X�u@Xr�@X �@W�;@W�w@W�w@Wl�@V�R@Vff@U�@U�@U`B@UO�@U/@T�@T�@Tz�@Tz�@T(�@S��@S�@SdZ@SS�@So@R�!@R��@R�@S"�@R��@R��@R��@R��@R��@R�!@R=q@Q��@Q��@Q%@PĜ@PA�@O�@OK�@N�@N��@N5?@M�T@M�-@M�h@Mp�@M?}@M�@L�/@L�j@L9X@K��@Kƨ@KS�@J�@J��@J�!@Jn�@JM�@J-@J-@J-@I�@I&�@H�@H1'@Hb@Hb@Hb@Hb@Hb@G��@Gl�@G+@F��@F�R@F�+@F5?@E@Ep�@EO�@E�@D��@D�@C��@C�m@C�m@C�
@C�F@C�@C"�@B�@B�@B��@Bn�@B-@A�@A�7@A&�@@��@@�u@@Q�@@ �@@b@?�@?��@?�@?|�@?+@?
=@>�y@>�+@>E�@>{@=@=�@=`B@=O�@<��@<�D@<1@;��@:�H@:n�@:J@9�@9��@9hs@9X@97L@8��@8Ĝ@8�@81'@7�@7��@7|�@7\)@7;d@7
=@6��@6E�@6@5�@5�T@5��@5@5p�@5/@5V@4�@3��@3��@3o@2��@2^5@2J@1��@1�#@1�7@1G�@1�@0��@0�9@0A�@01'@/��@/�w@/�w@/\)@/
=@.�+@.V@.{@-�T@-�@-/@,��@,��@,��@,�D@,j@,I�@+�m@+�@+C�@*��@*~�@*=q@)�@)�7@(��@(��@(r�@(bN@(bN@(Q�@(A�@( �@(  @'�P@';d@&��@&�y@&��@&ff@&$�@&@%�@%@%�h@%p�@%/@$�@$j@#��@#�F@#��@#��@#��@#��@#dZ@#dZ@#S�@#33@#o@"�@"�!@"n�@"=q@"-@"�@!��@!��@!�^@!�7@!x�@!hs@!X@!G�@!7L@!&�@ ��@ �`@ ��@ ��@ ��@ Ĝ@ r�@  �@��@\)@+@�y@��@v�@v�@$�@��@�-@�@p�@`B@/@��@�j@�D@9X@1@��@�m@�
@�
@ƨ@ƨ@�F@��@�@C�@C�@C�@33@"�@~�@J@��@hs@&�@��@�`@��@Ĝ@��@r�@A�@�@�P@+@��@�@V@$�@�@�-@p�@V@�@�@�D@j@j@I�@�@�@1@�m@�@33@��@�\@n�@-@��@X@&�@�@%@�`@�9@bN@b@�@�;@��@�w@�w@�@|�@\)@K�@;d@;d@+@+@�y@��@��@V@@�@�@�T@�T@�T@�T1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B
9XB
9XB
9XB
:^B
:^B
:^B
:^B
:^B
:^B
:^B
:^B
:^B
:^B
:^B
;dB
;dB
;dB
;dB
;dB
;dB
9XB
9XB
:^B
B�B
��B<jBr�B�B�fBBJBhB�B{B33B5?BM�Bl�BjBgmBW
BaHB]/B\)B^5B]/BaHBcTBm�Bt�Bz�B� B�B�B�B�B�=B�oB��B�{B�hB�oB��B��B��B�PB~�BhsBiyBt�Bk�BP�BG�B[#BT�B7LB�BVB��B�ZB�B�TB�)B�
B�B�qB��B��B�bB�Bw�BbNB33B6FB>wBB�BB�B)�B%B
�B
��B
��B
��B
�B
�B
�B
��B
B
��B
v�B
e`B
E�B
+B
6FB
(�B
�B
	7B	�B	�/B	�wB	ÖB	�B	}�B	{�B	ZB	=qB	A�B	,B	?}B	7LB	J�B	K�B	J�B	F�B	=qB	)�B	�B	�B	�B	�B		7B��B�sB�B�fB�B�`B�BB�#B�
B��B��B�?B�-B�}B�FB��B��B�!B�!B��B��B��B�7B�uB��B��B�DBx�B{�Bm�Bt�BdZBhsBu�B{�Bv�BjBu�B{�Bx�Bu�Bk�BcTBk�BjBo�BjBcTBZBXBVBR�B>wB7LBE�BVBW
BT�B]/BZBVBZBR�BT�BQ�B\)BZBR�BVBYB\)B[#BW
BS�BS�BM�BK�BG�BF�BH�BJ�BVBQ�BS�BR�BR�BXB\)BaHBdZBcTB^5B]/BffBaHB]/B_;BiyBbNB\)B`BBk�BhsBgmBo�Bu�Bz�B�B� Bz�Bv�B�%B�7B�bB�bB�\B�=B�1B�VB�\B�bB��B�{B��B��B��B��B��B��B��B�B��B��B��B�B�-B�B�B�-B�RB�RB�LB�qB�wB�wB��BƨBŢBĜB�}BĜB��BɺBɺBŢB��B��B�B�5B�B�;B�B�B�yB�fB�B��B��B��B��B��B��B��B��B��B��B	B	+B		7B	DB	
=B	
=B	PB	\B	{B	�B	�B	 �B	#�B	�B	$�B	#�B	%�B	)�B	/B	49B	6FB	8RB	9XB	<jB	=qB	B�B	B�B	F�B	E�B	J�B	I�B	J�B	W
B	YB	ZB	]/B	bNB	e`B	dZB	aHB	gmB	o�B	n�B	q�B	x�B	x�B	x�B	w�B	z�B	{�B	~�B	�B	�B	�B	�B	�B	�+B	�=B	�DB	�=B	�PB	�bB	�oB	�uB	�uB	�oB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�'B	�9B	�FB	�RB	�LB	�LB	�^B	�^B	�jB	�wB	��B	��B	ÖB	ĜB	ĜB	ƨB	ȴB	ɺB	��B	ɺB	ɺB	�B	�
B	�B	��B	��B	��B	��B	��B	�B	�B	��B	�B	�B	�B	��B	�B	�BB	�HB	�HB	�BB	�NB	�`B	�ZB	�`B	�yB	�yB	�yB	�B	�B	�B	�B	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B
B
B
B
B
%B
%B
%B
%B
%B
+B
%B
+B
+B
%B
%B
1B
	7B

=B
	7B
	7B
	7B

=B
	7B
1B
1B
	7B
PB
VB
PB
\B
\B
VB
VB
\B
\B
VB
bB
\B
\B
hB
hB
uB
{B
�B
�B
{B
uB
oB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
 �B
�B
�B
!�B
"�B
"�B
!�B
#�B
%�B
%�B
#�B
!�B
"�B
$�B
$�B
$�B
%�B
%�B
$�B
%�B
#�B
&�B
%�B
#�B
&�B
&�B
'�B
'�B
'�B
&�B
$�B
%�B
'�B
(�B
&�B
(�B
(�B
'�B
(�B
'�B
'�B
'�B
'�B
'�B
)�B
,B
+B
(�B
+B
,B
,B
-B
.B
.B
.B
-B
+B
.B
,B
-B
-B
+B
+B
.B
/B
.B
.B
+B
(�B
/B
0!B
/B
.B
0!B
1'B
1'B
0!B
/B
2-B
2-B
33B
5?B
5?B
5?B
5?B
5?B
5?B
6FB
5?B
49B
6FB
7LB
7LB
7LB
6FB
8RB
9XB
;dB
9XB
;dB
;dB
;dB
:^B
9XB
8RB
9XB
8RB
7LB
9XB
8RB
8RB
9XB
:^B
;dB
:^B
<jB
=qB
=qB
=qB
=qB
=qB
=qB
<jB
<jB
<jB
=qB
=qB
=qB
?}B
?}B
?}B
@�B
@�B
@�B
?}B
>wB
<jB
>wB
A�B
B�B
B�B
B�B
B�B
B�B
A�B
@�B
A�B
A�B
A�B
B�B
A�B
A�B
B�B
C�B
C�B
B�B
A�B
E�B
E�B
E�B
E�B
D�B
D�B
D�B
E�B
F�B
E�B
D�B
E�B
E�B
E�B
E�B
F�B
F�B
G�B
H�B
I�B
H�B
H�B
H�B
H�B
H�B
I�B
I�B
H�B
I�B
J�B
I�B
J�B
J�B
J�B
I�B
H�B
H�B
I�B
I�B
K�B
K�B
M�B
M�B
L�B
N�B
N�B
M�B
N�B
N�B
M�B
N�B
O�B
P�B
P�B
P�B
O�B
N�B
O�B
P�B
Q�B
Q�B
Q�B
Q�B
P�B
P�B
Q�B
P�B
O�B
Q�B
Q�B
R�B
R�B
S�B
T�B
T�B
S�B
T�B
T�B
T�B
T�B
T�B
VB
VB
W
B
W
B
VB
VB
VB
XB
XB
XB
XB
XB
YB
YB
ZB
ZB
ZB
ZB
YB
XB
ZB
YB
ZB
ZB
YB
YB
YB
[#B
]/B
^5B
^5B
]/B
]/B
]/B
\)B
[#B
\)B
]/B
^5B
]/B
]/B
^5B
_;B
_;B
_;B
_;B
_;B
^5B
^5B
^5B
_;B
aHB
bNB
cTB
cTB
bNB
bNB
cTB
bNB
bNB
bNB
bNB
bNB
bNB
cTB
dZB
dZB
dZB
cTB
dZB
dZB
e`B
e`B
e`B
e`B
e`B
e`B
dZB
e`B
e`B
e`B
e`B
dZB
cTB
cTB
cTB
e`B
e`B
e`B
e`B
ffB
ffB
ffB
ffB
gmB
gmB
hsB
gmB
gmB
gmB
gmB
gmB
gmB
hsB
iyB
iyB
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
iyB
iyB
hsB
hsB
iyB
jB
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
m�B
m�B
l�B
m�B
n�B
m�B
n�B
m�B
m�B
n�B
n�B
n�B
o�B
n�B
n�B
o�B
n�B
n�B
m�B
n�B
n�B
o�B
p�B
p�B
p�B
o�B
q�B
r�B
r�B
q�B
q�B
q�B
q�B
s�B
s�B
t�B
t�B
t�B
t�B
s�B
t�B
u�B
u�B
u�B
u�B
t�B
t�B
t�B
u�B
t�B
t�B
v�B
v�B
v�B
w�B
w�B
w�B
w�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B
9XB
9XB
9XB
:^B
:^B
:^B
:^B
:^B
:^B
:^B
:^B
:xB
:xB
:^B
;dB
;dB
;dB
;dB
;dB
;dB
9rB
9�B
:�B
C�B
�'B<�Bt9B��B�fB�B�B�B�BgB3�B6�BN�BmwBlqBi�BYBa�B^B\�B^�B^BbBdZBnIBuZB{JB�OB�oB��B��B�B�)B��B��B��B�:B�&B�B�#B�B��B��BlWBl=Bu�Bm)BTaBJ#B]BW$B:�B"�B�B B�RB�5B�BݲB�_B�sB�B�mB��B�TB�%BzBe�B88B8�B@�BEBD�B,�BDB
��B
�RB
�$B
��B
��B
�)B
��B
�B
��B
�RB
{�B
j0B
J�B
/�B
8B
+B
kB
�B	�2B	�-B	ªB	�B	�/B	�'B	~B	]�B	A�B	EB	/�B	AB	9$B	J�B	K�B	J�B	GB	?.B	-)B	VB	!bB	/B	�B	^B�.B��B��B�B�kB�B�|B�)B�yB�}B�B��B�TB��B�B��B��B��B��B�B�
B��B�0B��B��B�?B�B{B}�Bp�BvzBg8Bj�Bv�B|�BxBl�BvzB|jByrBvzBl�BeBlWBkQBo�BkBd�B[�BYBWsBTFBA�B:DBG�BV�BXBVB]�BZ�BV�BZ�BTBU�BSB\xBZ�BTBV�BY�B\�B[�BW�BT�BT�BN�BMBIBHKBJ=BL0BV�BS@BU2BTFBTaBY1B]/Ba�Bd�BdB_pB^5Bf�Bb4B^OB`BBi�BcTB]�Ba�Bl"Bi�Bh�Bp�Bv�B{B�[B��B{�BxRB��B��B��B��B��B�B�B��B�B�hB�$B��B�kB�qB��B�bB�@B�LB�0B�QB�yB�yB��B��B��B��B�B��B��B��B�B��B��B�B�B��B��B�9B�iB�SB�DB�XBʦB��BˬBԕBؓBޞB�7B��B�QB��B�KB�B��B��B�B�+B�2B�>B�0B�(B�HB�cB�}B	�B	zB		�B	�B	
�B	
�B	�B	�B	�B	B	�B	 �B	$B	 vB	%B	$tB	&fB	*B	/iB	4nB	6zB	8�B	9�B	<�B	=�B	B�B	B�B	F�B	FB	KB	J=B	K�B	WYB	YB	Z�B	]�B	b�B	ezB	d�B	bB	g�B	o�B	o B	q�B	x�B	x�B	y	B	x8B	{B	|6B	HB	�3B	�9B	�SB	�gB	�gB	�zB	�XB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	� B	�&B	�2B	�mB	�CB	�cB	�AB	�TB	�FB	�8B	��B	��B	�xB	�xB	�jB	�wB	��B	��B	��B	ĶB	��B	��B	��B	��B	��B	�	B	��B	յB	�$B	�B	�FB	�[B	�&B	�B	�B	�B	�9B	�,B	�B	�9B	�9B	�gB	�KB	�'B	�|B	�|B	��B	�NB	�zB	�B	�B	�B	��B	�B	��B	��B	��B	�B	��B	��B	��B	�B	��B	��B	��B	��B	�B	��B	��B	��B	�B	��B	�B	��B	��B	�?B	�[B	�B	�B	��B	��B	�B
 B
B
B
B
%B
%B
%B
%B
YB
+B
?B
EB
EB
tB
tB
KB
	RB

XB
	lB
	�B
	lB

XB
	lB
fB
�B
	�B
�B
pB
�B
�B
vB
�B
pB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
�B
�B
�B
�B
�B
�B
B
�B
�B
�B
B
�B
 �B
 �B
 �B
�B
!B
B
B
 �B
 �B
;B
 B
"B
#B
# B
"4B
$B
%�B
%�B
$&B
"4B
# B
%B
%B
%B
%�B
&B
%B
&B
$&B
&�B
&B
$@B
'B
'B
(
B
(
B
(
B
'B
%FB
&B
(
B
)*B
'8B
)B
)DB
(>B
)*B
(>B
(>B
(XB
(XB
(XB
*B
,B
+QB
)DB
+B
,=B
,"B
-)B
.IB
.IB
./B
-CB
+kB
./B
,qB
-]B
-CB
+�B
+QB
.IB
/5B
./B
.IB
+kB
)_B
/5B
0!B
/5B
./B
0UB
1AB
1AB
0UB
/iB
2GB
2aB
3hB
5ZB
5tB
5ZB
5ZB
5ZB
5tB
6FB
5ZB
4nB
6`B
7fB
7�B
7fB
6`B
8lB
9>B
;B
9�B
;dB
;dB
;B
:xB
9�B
8�B
9rB
8�B
7�B
9rB
8�B
8�B
9�B
:�B
;B
:�B
<�B
=�B
=�B
=�B
=�B
=�B
=�B
<�B
<�B
<�B
=�B
=�B
=�B
?�B
?�B
?�B
@�B
@�B
@�B
?�B
>�B
<�B
>�B
A�B
B�B
B�B
B�B
B�B
B�B
A�B
@�B
A�B
A�B
A�B
B�B
A�B
A�B
B�B
C�B
C�B
B�B
A�B
E�B
E�B
E�B
E�B
D�B
D�B
D�B
E�B
F�B
E�B
D�B
E�B
E�B
E�B
E�B
F�B
F�B
G�B
H�B
I�B
H�B
H�B
H�B
H�B
H�B
I�B
I�B
H�B
I�B
J�B
I�B
J�B
J�B
J�B
I�B
IB
H�B
I�B
J	B
K�B
K�B
M�B
M�B
MB
N�B
N�B
M�B
N�B
N�B
M�B
N�B
PB
Q B
Q B
QB
O�B
OB
O�B
Q B
Q�B
Q�B
Q�B
RB
Q B
QB
RB
Q4B
P.B
R B
R B
S&B
S&B
TB
UB
UB
T,B
UB
U2B
U2B
UB
UMB
VB
VB
W
B
W
B
VSB
V9B
V9B
XEB
XEB
X+B
X+B
XEB
Y1B
Y1B
ZQB
Z7B
Z7B
ZQB
YKB
X_B
Z7B
YKB
Z7B
ZQB
YeB
YKB
YKB
[=B
]dB
^5B
^5B
]IB
]/B
]IB
\CB
[WB
\]B
]IB
^OB
]IB
]IB
^jB
_VB
_VB
_VB
_VB
_VB
^jB
^OB
^jB
_�B
abB
bhB
cTB
cnB
bNB
b�B
cTB
bhB
bhB
bhB
bhB
b�B
bhB
cnB
dZB
dZB
dtB
cnB
dZB
dtB
ezB
e`B
e`B
e`B
e`B
ezB
dtB
e`B
ezB
e`B
e`B
dtB
cnB
c�B
c�B
ezB
ezB
ezB
ezB
f�B
f�B
f�B
f�B
g�B
g�B
hsB
g�B
g�B
g�B
g�B
g�B
g�B
h�B
iyB
i�B
iyB
i�B
i�B
iyB
iyB
i�B
i�B
i�B
jB
jB
i�B
i�B
h�B
h�B
i�B
j�B
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
m�B
m�B
l�B
m�B
n�B
m�B
n�B
m�B
m�B
n�B
n�B
n�B
o�B
n�B
n�B
o�B
n�B
n�B
m�B
n�B
n�B
o�B
p�B
p�B
p�B
o�B
q�B
r�B
r�B
q�B
q�B
q�B
q�B
s�B
s�B
t�B
t�B
t�B
t�B
s�B
t�B
u�B
u�B
u�B
u�B
t�B
t�B
t�B
u�B
t�B
t�B
v�B
v�B
v�B
w�B
w�B
w�B
w�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.05(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201805090038172018050900381720180509003817201806221330042018062213300420180622133004201806042132262018060421322620180604213226  JA  ARFMdecpA19c                                                                20180505093518  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20180505003524  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20180505003527  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20180505003528  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20180505003528  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20180505003528  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20180505003529  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20180505003529  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20180505003530  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20180505003531                      G�O�G�O�G�O�                JA  ARUP                                                                        20180505005629                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20180505153532  CV  JULD            G�O�G�O�F��                JM  ARCAJMQC2.0                                                                 20180508153817  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180508153817  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180604123226  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622043004  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200116221514                      G�O�G�O�G�O�                