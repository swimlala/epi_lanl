CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-07-31T00:35:31Z creation;2018-07-31T00:35:35Z conversion to V3.1;2019-12-19T07:32:19Z update;     
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
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \|   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `\   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  s�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �<   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �|   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �\   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �\   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �l   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �p   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �t   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �x   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �|   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20180731003531  20200116231517  5905048                                                                 JAMSTEC                                                         PRES            TEMP            PSAL              
A   JA  I2_0577_266                     2C  D   NAVIS_A                         0577                            ARGO 102115                     863 @�u�����1   @�u���-�@3�5?|��d^�7��41   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx�Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D#��D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Dby�Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�C3D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D��3D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�C3D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�L�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@���@�ff@�ffA33A?33A_33A33A���A���A���A���Aϙ�Aߙ�AA���B��B��B��B��B'��B/��B7��B?��BG��BO��BW��B_��Bg��Bo��Bw��B��B��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fC�3C�3C�3C�3C	�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C!�3C#�3C%�3C'�3C)�3C+�3C-�3C/�3C1�3C3�3C5�3C7�3C9�3C;�3C=�3C?�3CA�3CC�3CE�3CG�3CI�3CK�3CM�3CO�3CQ�3CS�3CU�3CW�3CY�3C[�3C]�3C_�3Ca�3Cc�3Ce�3Cg�3Ci�3Ck�3Cm�3Co�3Cq�3Cs�3Cu�3Cx�Cy�3C{�3C}�3C�3C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D |�D ��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D	|�D	��D
|�D
��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D |�D ��D!|�D!��D"|�D"��D#|�D#�fD$|�D$��D%|�D%��D&|�D&��D'|�D'��D(|�D(��D)|�D)��D*|�D*��D+|�D+��D,|�D,��D-|�D-��D.|�D.��D/|�D/��D0|�D0��D1|�D1��D2|�D2��D3|�D3��D4|�D4��D5|�D5��D6|�D6��D7|�D7��D8|�D8��D9|�D9��D:|�D:��D;|�D;��D<|�D<��D=|�D=��D>|�D>��D?|�D?��D@|�D@��DA|�DA��DB|�DB��DC|�DC��DD|�DD��DE|�DE��DF|�DF��DG|�DG��DH|�DH��DI|�DI��DJ|�DJ��DK|�DK��DL|�DL��DM|�DM��DN|�DN��DO|�DO��DP|�DP��DQ|�DQ��DR|�DR��DS|�DS��DT|�DT��DU|�DU��DV|�DV��DW|�DW��DX|�DX��DY|�DY��DZ|�DZ��D[|�D[��D\|�D\��D]|�D]��D^|�D^��D_|�D_��D`|�D`��Da|�Da��DbvfDb��Dc|�Dc��Dd|�Dd��De|�De��Df|�Df��Dg|�Dg��Dh|�Dh��Di|�Di��Dj|�Dj��Dk|�Dk��Dl|�Dl��Dm|�Dm��Dn|�Dn��Do|�Do��Dp|�Dp��Dq|�Dq��Dr|�Dr��Ds|�Ds��Dt|�Dt��Du|�Du��Dv|�Dv��Dw|�Dw��Dx|�Dx��Dy|�Dy��Dz|�Dz��D{|�D{��D||�D|��D}|�D}��D~|�D~��D|�D��D�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD���D��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD���D��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD¾fD��fD�>fD�~fDþfD��fD�>fD�~fDľfD��fD�>fD�~fDžfD��fD�>fD�~fDƾfD��fD�>fD�~fDǾfD��fD�>fD�~fDȾfD��fD�>fD�~fDɾfD��fD�>fD�~fDʾfD��fD�>fD�~fD˾fD��fD�>fD�~fD̾fD��fD�>fD�~fD;fD��fD�>fD�~fDξfD��fD�>fD�~fDϾfD��fD�>fD�~fDоfD��fD�>fD�~fDѾfD��fD�>fD�~fDҾfD��fD�>fD�~fDӾfD��fD�>fD�~fDԾfD��fD�>fD�~fDվfD��fD�>fD�~fD־fD��fD�>fD�~fD׾fD��fD�>fD�~fDؾfD��fD�>fD�~fDپfD��fD�>fD�~fDھfD��fD�>fD�~fD۾fD��fD�>fD�~fDܾfD��fD�>fD�~fDݾfD��fD�>fD�~fD޾fD��fD�>fD�~fD߾fD��fD�A�D�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD���D��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD��fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�A�D�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�K311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111Aݺ^AݶFAݴ9Aݟ�Aݙ�Aݗ�Aݕ�AݓuAݏ\AݍPAݏ\AݍPA�`BA�%A��yA���A�JAӃAҋDA�1AΑhA�1A�7LA�E�A���A�$�AƅA��;A��A�dZA��A�r�A��yA��jA�I�A�A��A�(�A�oA��A��/A���A��!A��\A�l�A�-A�v�A�VA�33A�=qA���A�  A���A�n�A�O�A�33A�1'A�9XA�A�A���A�n�A�A�A�ĜA��A��DA��jA�hsA� �A��mA��#A��PA�/A��TA�7LA�p�A���A���A�A���A���A��9A�;dA���A���A��hA�`BA�A��A�33A�n�A��A�n�A�~�A�C�A�1'A�A���A�E�A�JA��-A�=qA�p�A�G�A�=qA��A��A���A�hsA�=qA�ȴA�bA�hsA|9XAx�yAv�HAuG�AtQ�Arn�Ao7LAmp�Ak\)AjE�Ai�PAf�Ae�7Ac�mAa�A`~�A`{A_+A^9XA]��A\VAZ��AY��AX�AXr�AW��AS��AP^5AO&�AN��AN1AK�FAJ��AJffAH��AF��AE33AC|�AC�ABJA@�yA@5?A>VA<ĜA<$�A:�RA9�A8�DA7S�A6-A5%A4��A3&�A1&�A/�PA,bNA*z�A)�-A(I�A'&�A&�DA%��A$r�A#+A �A 1'AA\)A�A�RAl�A��A��A(�A��A��A�yA�A-AG�A��AI�A��AQ�A`BA��A��A�A
M�A	
=AK�A�jA��A��AAC�A�A�!A=qA��A;dA ��A A�@�l�@�@�x�@���@���@�+@�M�@�/@�  @�~�@���@�@�K�@�+@���@�$�@�  @�v�@�%@�  @�dZ@��H@��@��/@�l�@�!@��@䛦@���@�^5@�M�@�j@��@ݑh@�j@�1@�\)@�J@�O�@��@֧�@��T@���@�=q@��@���@�I�@���@̋D@�o@�^5@�@�@��@�bN@ǥ�@��@�n�@�p�@ēu@�l�@�X@�z�@�1'@��@�Ĝ@�9X@�33@��@�^5@���@�/@�Z@��w@�;d@�@�ȴ@�n�@���@�%@��`@�1'@��w@�l�@�"�@��y@��^@��@�Q�@��@�ƨ@�C�@���@���@��@���@�hs@���@�bN@��@��F@��@�t�@��@��\@�$�@���@�@�X@�&�@�%@��`@��@�Z@�1@���@�+@��!@�=q@��#@��T@��^@�X@�7L@�?}@�X@��`@��@�(�@��
@���@�t�@�"�@��y@���@�~�@�^5@�M�@�E�@�=q@��@�x�@��@��D@�r�@�bN@�A�@�  @�ƨ@���@�\)@��!@��+@�~�@�v�@�=q@��-@�hs@�?}@��@���@�Z@��F@��@��@�dZ@���@��+@�n�@�5?@��T@���@�@���@�@�x�@�V@��@��@�bN@�z�@��D@��u@���@�S�@��H@���@���@���@��+@�~�@��+@��+@�~�@�ff@�E�@�{@�{@��@���@��^@��h@�X@�&�@��@��`@�Ĝ@��j@��@�r�@�  @���@�o@���@���@�^5@�@�hs@�O�@�V@��j@�Q�@��@��F@��w@��w@�t�@�"�@���@�V@��T@��@�O�@��/@��u@��m@�l�@���@�v�@�E�@�5?@��@��#@�X@�?}@���@���@�z�@�A�@�(�@�1@��@��w@���@�K�@�;d@�@�ȴ@��R@���@�V@��@���@��7@��7@�hs@��@��`@���@��j@�z�@��;@���@���@�t�@�33@��@��@��@��!@���@���@���@�v�@�E�@��@��@��7@�?}@��@���@��`@��`@�Ĝ@��9@���@�z�@��@l�@~��@}�T@}p�@}O�@}`B@}p�@}/@}V@|��@|�@|�j@|j@|9X@{��@{�m@{�F@{dZ@z��@z�\@zM�@z=q@z-@y��@yX@y�@x�u@x�@x�@xb@x  @x �@xr�@x�u@x��@x��@y%@x�`@xĜ@xQ�@w�@w��@w�P@v�@vv�@u��@up�@u/@t��@tz�@tI�@t�@s��@s�
@s�F@st�@q��@qG�@p��@p��@p��@p�@o�@o�@o�@oK�@nȴ@nff@n$�@n@m@m�@mO�@l�/@l��@lz�@l(�@k��@kdZ@ko@j=q@ihs@i%@hQ�@g�w@g\)@g
=@f�y@f��@f@e`B@e?}@eV@d�/@d�@dj@d�@c�
@c�@b�@b^5@bJ@a&�@`�9@`�@`  @_l�@_+@^ȴ@^v�@^v�@^E�@^@^@]�T@]��@]��@]/@\j@\(�@\1@[�
@[t�@[o@Z�!@Z�@Y��@YX@Y7L@Y�@Y�@Y%@X��@X�9@Xr�@XA�@X �@W�;@W��@W+@V�@Vv�@V@U��@U�@T�D@T1@S��@SC�@R�H@Rn�@R�@Q�7@P��@P�u@P �@O��@O|�@O\)@N��@NV@M�-@M/@L�/@L��@L�D@L�@K��@K�
@K��@KdZ@Ko@J��@J-@I�#@Ihs@I7L@H��@H�@H �@Hb@G�@G\)@G;d@G
=@F�@Fff@F$�@E@D��@DI�@C��@C�
@C��@C33@B�H@B-@A��@A�@A��@AG�@@��@@�9@@A�@@ �@?�;@?�w@?�w@?��@?;d@>�+@>ff@>V@>E�@>$�@=��@=��@=p�@=�@<�@<I�@<�@;�
@;��@;dZ@:��@:�@9�#@9�^@9��@9��@9X@8�`@8Ĝ@8�9@8�@7�@7��@7l�@6��@6��@6V@6$�@5��@5�-@5O�@5V@4�@4Z@41@3�
@3ƨ@3��@3t�@3"�@2��@2�\@2M�@1�@1��@17L@0�9@0�u@0r�@0 �@/�@/��@/�P@/�@.v�@.{@-��@-�-@-`B@-O�@-V@,z�@+ƨ@*�@*M�@*-@)�@)��@)x�@)�@(��@(��@(�@(A�@( �@(  @'�@'l�@'K�@';d@'�@&�y@&�+@&@%�-@%?}@$�/@$�j@$z�@$(�@#ƨ@#��@#t�@#o@"�H@"�!@"~�@"M�@"-@!��@!�^@!hs@!�@ ��@ Ĝ@ ��@ Q�@ 1'@�@��@�@|�@;d@
=@��@�y@ȴ@�+@ff@E�@$�@{@�@��@@��@�@`B@�@��@��@��@Z@�@�m@�
@ƨ@��@t�@�H@�\@~�@n�@M�@-@�@��@7L@&�@�@��@r�@Q�@�@�P@l�@K�@;d@
=@�@��@V@$�@@�@�T@��@��@@�-@��@p�@�@��@I�@1@�m@��@�@dZ@C�@33@"�@@�@�@��@��@�!@��@�\@~�@^5@J@�#@�7@G�@&�@�@�`@Ĝ@�u@bN@1'@  @�;@�;@�P@\)@
=@�y@�R@��@V@E�@@��@p�@p�@`B@?}@/@�@��@�@�@z�@9X@��@�
@ƨ@��@�@t�@dZ@C�@@
�H@
��@
��@
��@
�!@
��@
n�@
=q@
�@	�#@	�^@	��@	x�@	X@	X@	X@	X@	X@	G�@��@Ĝ@�@Q�@A�@1'@�;@�w11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111Aݺ^AݶFAݴ9Aݟ�Aݙ�Aݗ�Aݕ�AݓuAݏ\AݍPAݏ\AݍPA�`BA�%A��yA���A�JAӃAҋDA�1AΑhA�1A�7LA�E�A���A�$�AƅA��;A��A�dZA��A�r�A��yA��jA�I�A�A��A�(�A�oA��A��/A���A��!A��\A�l�A�-A�v�A�VA�33A�=qA���A�  A���A�n�A�O�A�33A�1'A�9XA�A�A���A�n�A�A�A�ĜA��A��DA��jA�hsA� �A��mA��#A��PA�/A��TA�7LA�p�A���A���A�A���A���A��9A�;dA���A���A��hA�`BA�A��A�33A�n�A��A�n�A�~�A�C�A�1'A�A���A�E�A�JA��-A�=qA�p�A�G�A�=qA��A��A���A�hsA�=qA�ȴA�bA�hsA|9XAx�yAv�HAuG�AtQ�Arn�Ao7LAmp�Ak\)AjE�Ai�PAf�Ae�7Ac�mAa�A`~�A`{A_+A^9XA]��A\VAZ��AY��AX�AXr�AW��AS��AP^5AO&�AN��AN1AK�FAJ��AJffAH��AF��AE33AC|�AC�ABJA@�yA@5?A>VA<ĜA<$�A:�RA9�A8�DA7S�A6-A5%A4��A3&�A1&�A/�PA,bNA*z�A)�-A(I�A'&�A&�DA%��A$r�A#+A �A 1'AA\)A�A�RAl�A��A��A(�A��A��A�yA�A-AG�A��AI�A��AQ�A`BA��A��A�A
M�A	
=AK�A�jA��A��AAC�A�A�!A=qA��A;dA ��A A�@�l�@�@�x�@���@���@�+@�M�@�/@�  @�~�@���@�@�K�@�+@���@�$�@�  @�v�@�%@�  @�dZ@��H@��@��/@�l�@�!@��@䛦@���@�^5@�M�@�j@��@ݑh@�j@�1@�\)@�J@�O�@��@֧�@��T@���@�=q@��@���@�I�@���@̋D@�o@�^5@�@�@��@�bN@ǥ�@��@�n�@�p�@ēu@�l�@�X@�z�@�1'@��@�Ĝ@�9X@�33@��@�^5@���@�/@�Z@��w@�;d@�@�ȴ@�n�@���@�%@��`@�1'@��w@�l�@�"�@��y@��^@��@�Q�@��@�ƨ@�C�@���@���@��@���@�hs@���@�bN@��@��F@��@�t�@��@��\@�$�@���@�@�X@�&�@�%@��`@��@�Z@�1@���@�+@��!@�=q@��#@��T@��^@�X@�7L@�?}@�X@��`@��@�(�@��
@���@�t�@�"�@��y@���@�~�@�^5@�M�@�E�@�=q@��@�x�@��@��D@�r�@�bN@�A�@�  @�ƨ@���@�\)@��!@��+@�~�@�v�@�=q@��-@�hs@�?}@��@���@�Z@��F@��@��@�dZ@���@��+@�n�@�5?@��T@���@�@���@�@�x�@�V@��@��@�bN@�z�@��D@��u@���@�S�@��H@���@���@���@��+@�~�@��+@��+@�~�@�ff@�E�@�{@�{@��@���@��^@��h@�X@�&�@��@��`@�Ĝ@��j@��@�r�@�  @���@�o@���@���@�^5@�@�hs@�O�@�V@��j@�Q�@��@��F@��w@��w@�t�@�"�@���@�V@��T@��@�O�@��/@��u@��m@�l�@���@�v�@�E�@�5?@��@��#@�X@�?}@���@���@�z�@�A�@�(�@�1@��@��w@���@�K�@�;d@�@�ȴ@��R@���@�V@��@���@��7@��7@�hs@��@��`@���@��j@�z�@��;@���@���@�t�@�33@��@��@��@��!@���@���@���@�v�@�E�@��@��@��7@�?}@��@���@��`@��`@�Ĝ@��9@���@�z�@��@l�@~��@}�T@}p�@}O�@}`B@}p�@}/@}V@|��@|�@|�j@|j@|9X@{��@{�m@{�F@{dZ@z��@z�\@zM�@z=q@z-@y��@yX@y�@x�u@x�@x�@xb@x  @x �@xr�@x�u@x��@x��@y%@x�`@xĜ@xQ�@w�@w��@w�P@v�@vv�@u��@up�@u/@t��@tz�@tI�@t�@s��@s�
@s�F@st�@q��@qG�@p��@p��@p��@p�@o�@o�@o�@oK�@nȴ@nff@n$�@n@m@m�@mO�@l�/@l��@lz�@l(�@k��@kdZ@ko@j=q@ihs@i%@hQ�@g�w@g\)@g
=@f�y@f��@f@e`B@e?}@eV@d�/@d�@dj@d�@c�
@c�@b�@b^5@bJ@a&�@`�9@`�@`  @_l�@_+@^ȴ@^v�@^v�@^E�@^@^@]�T@]��@]��@]/@\j@\(�@\1@[�
@[t�@[o@Z�!@Z�@Y��@YX@Y7L@Y�@Y�@Y%@X��@X�9@Xr�@XA�@X �@W�;@W��@W+@V�@Vv�@V@U��@U�@T�D@T1@S��@SC�@R�H@Rn�@R�@Q�7@P��@P�u@P �@O��@O|�@O\)@N��@NV@M�-@M/@L�/@L��@L�D@L�@K��@K�
@K��@KdZ@Ko@J��@J-@I�#@Ihs@I7L@H��@H�@H �@Hb@G�@G\)@G;d@G
=@F�@Fff@F$�@E@D��@DI�@C��@C�
@C��@C33@B�H@B-@A��@A�@A��@AG�@@��@@�9@@A�@@ �@?�;@?�w@?�w@?��@?;d@>�+@>ff@>V@>E�@>$�@=��@=��@=p�@=�@<�@<I�@<�@;�
@;��@;dZ@:��@:�@9�#@9�^@9��@9��@9X@8�`@8Ĝ@8�9@8�@7�@7��@7l�@6��@6��@6V@6$�@5��@5�-@5O�@5V@4�@4Z@41@3�
@3ƨ@3��@3t�@3"�@2��@2�\@2M�@1�@1��@17L@0�9@0�u@0r�@0 �@/�@/��@/�P@/�@.v�@.{@-��@-�-@-`B@-O�@-V@,z�@+ƨ@*�@*M�@*-@)�@)��@)x�@)�@(��@(��@(�@(A�@( �@(  @'�@'l�@'K�@';d@'�@&�y@&�+@&@%�-@%?}@$�/@$�j@$z�@$(�@#ƨ@#��@#t�@#o@"�H@"�!@"~�@"M�@"-@!��@!�^@!hs@!�@ ��@ Ĝ@ ��@ Q�@ 1'@�@��@�@|�@;d@
=@��@�y@ȴ@�+@ff@E�@$�@{@�@��@@��@�@`B@�@��@��@��@Z@�@�m@�
@ƨ@��@t�@�H@�\@~�@n�@M�@-@�@��@7L@&�@�@��@r�@Q�@�@�P@l�@K�@;d@
=@�@��@V@$�@@�@�T@��@��@@�-@��@p�@�@��@I�@1@�m@��@�@dZ@C�@33@"�@@�@�@��@��@�!@��@�\@~�@^5@J@�#@�7@G�@&�@�@�`@Ĝ@�u@bN@1'@  @�;@�;@�P@\)@
=@�y@�R@��@V@E�@@��@p�@p�@`B@?}@/@�@��@�@�@z�@9X@��@�
@ƨ@��@�@t�@dZ@C�@@
�H@
��@
��@
��@
�!@
��@
n�@
=q@
�@	�#@	�^@	��@	x�@	X@	X@	X@	X@	X@	G�@��@Ĝ@�@Q�@A�@1'@�;@�w11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�sB

=B
{B
�B
ǮB	7B �BH�Be`Be`Bv�BjBB�B�B��B��B�B�B1BG�BO�BO�BF�B?}B6FBJ�BF�BaHBiyBk�Bk�B_;BXBS�BR�BZB`BBe`Bs�Bx�B~�B�%B�DB�1Bz�BhsB�+B�uB�DBu�BYBF�BE�B^5BbNB[#BVBW
BN�BL�BT�BZBG�B5?B>wBG�BJ�BO�BK�BF�B>wB,B1B��B��B��B�LB�wB��B�BB�B(�B�B�B{B+B
�NB
�/B
�TB
ȴB
ÖB
��B
n�B
y�B
[#B
_;B
N�B
�B
VB
�B
uB

=B	��B	��B	��B	��B	ȴB	B	��B	��B	��B	�B	�JB	�\B	�+B	y�B	x�B	hsB	[#B	[#B	ZB	O�B	<jB	\B	B	"�B	-B	�B	B		7B	JB��B�B�B�B�B�B�NB�5B��BǮB��B�}B�B�-B��B��B��B��B�1Bs�Bo�BYBgmBs�Bn�Bn�Bx�Bs�BjBe`BT�Bx�Bw�Bn�BR�B;dBL�B^5B_;BYBQ�BF�B8RB6FBG�BI�BI�BM�BH�B:^BA�BJ�BH�B:^B49B2-B0!B?}B;dB5?B<jBH�BR�BVBR�BQ�BVBT�BR�BS�BQ�B]/BZB[#B^5B\)BZBZBXBcTB]/Bo�Bp�Bm�BffB\)BgmBl�Br�Bx�By�Bv�Bs�Bq�Bv�Bv�Bo�Bp�B}�B�By�Bv�Bz�B� B�B�B~�B�B� B�B�+B�B}�B�VB��B�uB�JB�+B�PB��B��B��B��B��B��B��B��B��B��B��B��B�!B�jB�-B�?B��B�qBƨBƨBƨBɺB��B��B�
B�B�#B�#B�B�5B�ZB�BB�`B�sB�yB�yB�ZB�B�B��B��B��B��B��B	B	%B	%B	1B		7B	VB	hB	�B	�B	�B	#�B	%�B	(�B	.B	.B	1'B	5?B	5?B	5?B	7LB	8RB	9XB	<jB	>wB	C�B	I�B	P�B	Q�B	P�B	VB	XB	\)B	[#B	_;B	bNB	ffB	iyB	m�B	m�B	q�B	s�B	w�B	z�B	|�B	|�B	{�B	y�B	{�B	� B	�B	�DB	�PB	�PB	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	��B	�B	�B	�B	�B	�-B	�3B	�3B	�3B	�-B	�'B	�-B	�3B	�?B	�RB	�XB	�RB	�3B	�-B	�3B	�RB	�XB	�jB	�jB	�}B	��B	B	ÖB	ĜB	ĜB	ŢB	ȴB	��B	ɺB	��B	��B	ɺB	ɺB	��B	��B	��B	��B	��B	��B	ɺB	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�#B	�/B	�#B	�)B	�B	�#B	�)B	�;B	�HB	�NB	�NB	�HB	�BB	�`B	�`B	�`B	�fB	�mB	�yB	�yB	�yB	�B	�B	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
  B
  B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
%B
B
B
B
%B
+B
1B
+B
+B
+B
	7B
	7B

=B

=B
1B
+B
	7B
DB
PB
\B
VB
hB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
 �B
!�B
 �B
!�B
"�B
!�B
!�B
"�B
!�B
 �B
 �B
#�B
#�B
#�B
&�B
'�B
'�B
'�B
&�B
'�B
+B
+B
+B
+B
)�B
+B
+B
)�B
)�B
)�B
,B
)�B
,B
-B
,B
,B
.B
/B
/B
0!B
0!B
/B
1'B
0!B
0!B
0!B
/B
.B
2-B
33B
2-B
1'B
2-B
2-B
2-B
33B
5?B
6FB
6FB
6FB
6FB
5?B
5?B
49B
5?B
6FB
5?B
49B
5?B
5?B
5?B
5?B
6FB
5?B
33B
5?B
7LB
7LB
7LB
7LB
8RB
7LB
7LB
8RB
9XB
:^B
:^B
:^B
9XB
8RB
8RB
:^B
<jB
=qB
=qB
<jB
>wB
>wB
>wB
=qB
=qB
=qB
=qB
>wB
>wB
?}B
?}B
?}B
@�B
B�B
B�B
@�B
B�B
B�B
B�B
A�B
A�B
A�B
?}B
A�B
D�B
E�B
E�B
C�B
D�B
C�B
G�B
G�B
F�B
F�B
F�B
G�B
F�B
H�B
I�B
I�B
J�B
I�B
G�B
G�B
J�B
K�B
K�B
J�B
I�B
J�B
J�B
I�B
J�B
H�B
K�B
K�B
K�B
K�B
I�B
J�B
M�B
O�B
O�B
N�B
M�B
M�B
O�B
O�B
N�B
M�B
O�B
N�B
N�B
O�B
O�B
P�B
P�B
P�B
O�B
P�B
P�B
P�B
Q�B
R�B
S�B
S�B
R�B
Q�B
Q�B
T�B
S�B
S�B
T�B
S�B
T�B
W
B
W
B
VB
VB
W
B
W
B
VB
VB
XB
YB
ZB
YB
ZB
YB
W
B
VB
VB
XB
\)B
[#B
\)B
\)B
\)B
]/B
]/B
\)B
]/B
^5B
]/B
]/B
]/B
^5B
_;B
^5B
]/B
]/B
\)B
]/B
^5B
_;B
`BB
`BB
`BB
`BB
aHB
aHB
aHB
bNB
bNB
bNB
bNB
cTB
cTB
bNB
cTB
cTB
dZB
e`B
dZB
dZB
dZB
e`B
e`B
e`B
e`B
e`B
ffB
ffB
gmB
ffB
ffB
gmB
gmB
gmB
hsB
hsB
gmB
hsB
hsB
hsB
gmB
gmB
gmB
hsB
hsB
hsB
hsB
iyB
iyB
iyB
hsB
hsB
gmB
iyB
jB
k�B
jB
jB
iyB
iyB
iyB
k�B
k�B
jB
jB
k�B
iyB
l�B
l�B
l�B
l�B
l�B
l�B
l�B
m�B
m�B
n�B
n�B
n�B
o�B
o�B
o�B
n�B
n�B
m�B
l�B
l�B
n�B
o�B
p�B
o�B
q�B
p�B
p�B
q�B
q�B
q�B
r�B
r�B
r�B
r�B
r�B
r�B
q�B
q�B
q�B
p�B
q�B
q�B
q�B
r�B
s�B
r�B
r�B
r�B
r�B
s�B
s�B
s�B
t�B
r�B
s�B
s�B
t�B
t�B
t�B
t�B
u�B
t�B
t�B
u�B
w�B
w�B
v�B
w�B
w�B
w�B
w�B
v�B
v�B
v�B
w�B
x�B
x�B
x�B
x�B
y�B
x�B
x�B
x�B
y�B
y�B
z�B
z�B
y�B
y�B
y�B
y�B
y�B
y�B
z�B
z�B
z�B
{�B
{�B
{�B
{�B
{�B
z�B
y�B
z�B
z�B
{�B
|�B
|�B
{�B
|�B
~�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	�kB	�aB	��B
B
�B
'�B
��B~B%zBK�Bg8Bg�Bx�BpoB�SB�B��B��B��B�BpB�BHBP�BP�BHKBA�B9>BL�BH�BabBi�BlBl�Ba-BZ�BVmBT�B[qBa|Bf2BtBy$B.B�%B�DB��B|PBkQB��B�{B�JBw�B[WBI�BG�B^�Bb�B[�BV�BW�BP}BN�BVB[qBJ�B9XBA;BI�BK�BPbBLJBG+B?HB-�BJBUB�HB�KB�6B��B�
B��BI�B-�B	B�B�B�B
�B
�B
��B
˒B
żB
�@B
s�B
|�B
^�B
a-B
Q4B
�B
 B
�B
MB
�B	�rB	��B	�
B	��B	�#B	��B	�TB	��B	��B	��B	�jB	�B	�KB	{B	y�B	jKB	]/B	\]B	[	B	P�B	>BB	{B	�B	$B	-�B	CB	�B	
rB	6B�jB�-B�B�wB�nB��B��BߊB�4BɺB��B�oB�!B�B��B�HB�B��B�XBv�BrGB\�Bi�Bt�BpoBp!By�Bu%BlWBg�BXBy>Bx�Bo�BU�B>�BNVB^�B_�BY�BR�BHfB:�B8�BH�BJ�BJ�BN�BI�B<jBB�BKxBI�B<B6B4B2GB@iB<�B7B=�BIlBSuBV�BS�BR�BV�BU�BS�BT�BR�B]�BZ�B[�B^�B\�B[	B[	BY1BdB^�Bo�Bp�BnBgRB]�Bh�BmwBshByrBzxBw�Bt�Br�Bw�BwfBp�Bq�B~]B�aB{Bw�B|B��B�{B��B�B��B�;B��B��B�B�B�B��B�,B��B��B�pB�B��B��B�1B�7B�dB�bB�LB��B��B��B�nB��B��B��B�+B��B�(B�B�B�EB�=B�xB�TB�YB�kBیBیB��BޞB�B��B�B��B��B��B�,B�B�;B�B�B�jB�BB�wB	�B	�B	�B	�B		�B	�B	�B	�B	�B	;B	$@B	&LB	)DB	./B	.cB	1[B	5ZB	5tB	5tB	7�B	8�B	9�B	<�B	>�B	C�B	J	B	P�B	R B	QNB	VB	XEB	\)B	[�B	_�B	b�B	f�B	i�B	m�B	m�B	q�B	s�B	w�B	{B	}B	}B	|B	z*B	|PB	�iB	��B	�xB	�jB	��B	��B	��B	��B	��B	�
B	��B	��B	��B	�B	�!B	�,B	�B	�2B	�B	�LB	�`B	�"B	�WB	�6B	�eB	�CB	�/B	�IB	�IB	�GB	�3B	�3B	�MB	��B	�vB	�|B	�MB	�ZB	�RB	�XB	��B	��B	��B	��B	�lB	�rB	�jB	��B	��B	��B	B	ðB	��B	ĶB	��B	��B	��B	��B	��B	��B	�	B	��B	��B	��B	��B	��B	��B	�B	�#B	�B	�JB	�B	� B	�:B	�HB	�&B	�B	�FB	�FB	�FB	�SB	�EB	�B	�QB	�eB	�yB	�eB	�B	�kB	یB	�dB	یB	�xB	ڠB	یB	ܒB	�pB	�bB	�hB	�hB	�|B	��B	�zB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	�B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	�B	��B	��B	�B	�	B	�B	�B	��B	��B	��B	��B	�B	�B	�<B	�"B	�"B	�JB	�"B	�B	�B
 4B
B
 B
 B	�.B	�(B	�6B	�PB	�<B	�BB
 4B
'B
B
B
3B
9B
%B
9B
SB
SB
?B
EB
KB
_B
zB
_B
	RB
	RB

XB

XB
KB
zB
	lB
�B
jB
\B
�B
hB
{B
mB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B

B
�B
�B
�B
�B
�B
B
�B
 �B
B
B
�B
 �B
!�B
!�B
 �B
!�B
!B
!�B
"�B
"B
!�B
"�B
!�B
!B
!-B
$&B
$&B
$B
'B
($B
(
B
($B
'B
($B
+B
+6B
+6B
+6B
*B
+6B
+B
*KB
*0B
*0B
,"B
*eB
,=B
-CB
,=B
,=B
./B
/OB
/5B
0!B
0;B
/5B
1'B
0;B
0UB
0;B
/OB
.cB
2aB
3MB
2aB
1[B
2aB
2|B
2aB
3�B
5ZB
6`B
6`B
6`B
6FB
5ZB
5ZB
4nB
5ZB
6zB
5ZB
4TB
5tB
5ZB
5�B
5tB
6`B
5tB
3�B
5�B
7�B
7fB
7�B
7�B
8lB
7�B
7�B
8�B
9�B
:xB
:�B
:xB
9�B
8�B
8�B
:�B
<�B
=�B
=�B
<�B
>�B
>�B
>�B
=�B
=�B
=�B
=�B
>�B
>�B
?�B
?�B
?�B
@�B
B�B
B�B
@�B
B�B
B�B
B�B
A�B
A�B
A�B
?�B
A�B
D�B
E�B
E�B
C�B
D�B
C�B
G�B
G�B
F�B
F�B
F�B
G�B
F�B
H�B
I�B
I�B
J�B
I�B
G�B
G�B
J�B
K�B
K�B
J�B
I�B
J�B
J�B
I�B
J�B
H�B
K�B
K�B
K�B
K�B
J	B
J�B
M�B
O�B
O�B
N�B
M�B
NB
O�B
PB
N�B
N"B
PB
OB
OB
O�B
PB
Q B
Q B
QB
P.B
Q B
QB
Q B
RB
S&B
S�B
T,B
SB
R:B
R B
UB
TB
TFB
U2B
T,B
U2B
W$B
W$B
VB
VB
W?B
W$B
V9B
V9B
XEB
YKB
Z7B
YKB
Z7B
YKB
WYB
VmB
VmB
X_B
\)B
[=B
\CB
\]B
\]B
]dB
]IB
\CB
]dB
^OB
]dB
]dB
]IB
^jB
_VB
^jB
]IB
]dB
\]B
]IB
^jB
_VB
`\B
`\B
`\B
`\B
abB
abB
a|B
b�B
b�B
b�B
b�B
cnB
c�B
bhB
cnB
cnB
dtB
e�B
dtB
d�B
dtB
ezB
ezB
ezB
ezB
ezB
f�B
f�B
gmB
f�B
f�B
g�B
g�B
g�B
hsB
h�B
g�B
h�B
h�B
h�B
g�B
g�B
g�B
h�B
h�B
h�B
h�B
i�B
iyB
iyB
h�B
h�B
g�B
i�B
jB
k�B
j�B
j�B
i�B
i�B
i�B
k�B
k�B
j�B
j�B
k�B
i�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
m�B
m�B
n�B
n�B
n�B
o�B
o�B
o�B
n�B
n�B
m�B
l�B
l�B
n�B
o�B
p�B
o�B
q�B
p�B
p�B
q�B
q�B
q�B
r�B
r�B
r�B
r�B
r�B
r�B
q�B
q�B
q�B
p�B
q�B
q�B
q�B
r�B
s�B
r�B
r�B
r�B
r�B
s�B
s�B
s�B
t�B
r�B
s�B
s�B
t�B
t�B
t�B
t�B
u�B
t�B
t�B
u�B
w�B
w�B
v�B
w�B
w�B
w�B
w�B
v�B
v�B
v�B
xB
x�B
x�B
x�B
x�B
y�B
x�B
x�B
x�B
y�B
y�B
z�B
z�B
y�B
y�B
y�B
y�B
y�B
y�B
z�B
z�B
{B
{�B
{�B
{�B
{�B
{�B
z�B
y�B
z�B
{B
|B
|�B
}"B
|B
}B
~�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.05(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201808040034452018080400344520180804003445201808040200232018080402002320180804020023201808050026392018080500263920180805002639  JA  ARFMdecpA19c                                                                20180731093509  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20180731003531  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20180731003534  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20180731003534  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20180731003535  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20180731003535  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20180731003535  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20180731003535  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20180731003535  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20180731003535                      G�O�G�O�G�O�                JA  ARUP                                                                        20180731005502                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20180731153728  CV  JULD            G�O�G�O�Fï�                JM  ARCAJMQC2.0                                                                 20180803153445  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180803153445  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180803170023  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180804152639  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200116231517                      G�O�G�O�G�O�                