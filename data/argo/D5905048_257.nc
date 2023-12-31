CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-07-23T03:36:46Z creation;2018-07-23T03:36:50Z conversion to V3.1;2019-12-19T07:34:26Z update;     
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20180723033646  20200116231516  5905048                                                                 JAMSTEC                                                         PRES            TEMP            PSAL              A   JA  I2_0577_257                     2C  D   NAVIS_A                         0577                            ARGO 102115                     863 @�o6�s�1   @�o6�}( @3���	k��dT��-�1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�ff@�  A   A   A@  A`  A�  A�33A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C�fC  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DGfDG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]fD]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D��3D�3D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�fD�&f11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@���@�ff@�ffA33A?33A_33A33A���A���A���A���Aϙ�Aߙ�AA���B��B��B��B��B'��B/��B7��B?��BG��BO��BW��B_��Bg��Bo��Bw��B��B��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fC�3C�3C�3C�3C	�3C�3C�3C�3C�3C�3C�3C�3C�3CٚC�3C�3C!�3C#�3C%�3C'�3C)�3C+�3C-�3C/�3C1�3C3�3C5�3C7�3C9�3C;�3C=�3C?�3CA�3CC�3CE�3CG�3CI�3CK�3CM�3CO�3CQ�3CS�3CU�3CW�3CY�3C[�3C]�3C_�3Ca�3Cc�3Ce�3Cg�3Ci�3Ck�3Cm�3Co�3Cq�3Cs�3Cu�3Cw�3Cy�3C{�3C}�3C�3C���C���C���C���C���C���C���C���C���C���C���C���C���C���C�fC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C�fC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D |�D ��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D	|�D	��D
|�D
��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D |�D ��D!|�D!��D"|�D"��D#|�D#��D$|�D$��D%|�D%��D&|�D&��D'|�D'��D(|�D(��D)|�D)��D*|�D*��D+|�D+��D,|�D,��D-|�D-��D.|�D.��D/|�D/��D0|�D0��D1|�D1��D2|�D2��D3|�D3��D4|�D4��D5|�D5��D6|�D6��D7|�D7��D8|�D8��D9|�D9��D:|�D:��D;|�D;��D<|�D<��D=|�D=��D>|�D>��D?|�D?��D@|�D@��DA|�DA��DB|�DB��DC|�DC��DD|�DD��DE|�DE��DF|�DG3DG|�DG��DH|�DH��DI|�DI��DJ|�DJ��DK|�DK��DL|�DL��DM|�DM��DN|�DN��DO|�DO��DP|�DP��DQ|�DQ��DR|�DR��DS|�DS��DT|�DT��DU|�DU��DV|�DV��DW|�DW��DX|�DX��DY|�DY��DZ|�DZ��D[|�D[��D\|�D]3D]|�D]��D^|�D^��D_|�D_��D`|�D`��Da|�Da��Db|�Db��Dc|�Dc��Dd|�Dd��De|�De��Df|�Df��Dg|�Dg��Dh|�Dh��Di|�Di��Dj|�Dj��Dk|�Dk��Dl|�Dl��Dm|�Dm��Dn|�Dn��Do|�Do��Dp|�Dp��Dq|�Dq��Dr|�Dr��Ds|�Ds��Dt|�Dt��Du|�Du��Dv|�Dv��Dw|�Dw��Dx|�Dx��Dy|�Dy��Dz|�Dz��D{|�D{��D||�D|��D}|�D}��D~|�D~��D|�D��D�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�;3D�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD¾fD��fD�>fD�~fDþfD��fD�>fD�~fDľfD��fD�>fD�~fDžfD��fD�>fD�~fDƾfD��fD�>fD�~fDǾfD��fD�>fD�~fDȾfD��fD�>fD�~fDɾfD��fD�>fD�~fDʾfD��fD�>fD�~fD˾fD��fD�>fD�~fD̾fD��fD�>fD�~fD;fD��fD�>fD�~fDξfD��fD�>fD�~fDϾfD��fD�>fD�~fDоfD��fD�>fD�~fDѾfD��fD�>fD�~fDҾfD��fD�>fD�~fDӾfD��fD�>fD�~fDԾfD��fD�>fD�~fDվfD��fD�>fD�~fD־fD��fD�>fD�~fD׾fD��fD�>fD�~fDؾfD��fD�>fD�~fDپfD��fD�>fD�~fDھfD��fD�>fD�~fD۾fD��fD�>fD�~fDܾfD��fD�>fD�~fD���D��D�>fD�~fD޾fD��fD�>fD�~fD߾fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD��fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD���D��D�$�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�9XA�;dA�5?A�/A�(�A��A��A�1AуA�$�A�\)A�hsA���A�&�AþwA©�A���A�bA���A�z�A� �A�(�A�\)A�A�A��#A�=qA�v�A�/A��A�  A�`BA�(�A�1A�K�A��#A���A�|�A��A�E�A��A�z�A��A��A�5?A�\)A��A��9A���A�E�A���A�XA��A��A��A�|�A�VA�{A�l�A��\A��A�;dA��9A�+A��-A�5?A�E�A���A�bNA��A�=qA�&�A��A���A�VA���A��A�ȴA��^A��`A�S�A�t�A���A�A���A��A���A�K�A�(�A�v�A��`A�A��hA���A� �A� �A���A���A�M�A�JA���A�C�A��A}�TA}A|Q�A{�wAy�TAu�Ap~�Al��AhA�Af�Ae�FAdr�AaC�A]t�A[�AY�-AWVAU33AS�wAQ��AP��AO�#AO��AO�^AO�7AN��ALbAH�!AG�AD�\AB�HAB9XAA��A@��A?�
A>  A;ƨA;S�A:�A:VA8�uA7��A6�HA5��A5�7A4�A3O�A3%A2�A0^5A-�TA+��A+%A*�\A*�uA*�A)��A)"�A(I�A&�A$��A#��A!S�AAK�A�!A�A{AXA"�A��Av�A�7A�AI�A��A�A�DA��AG�AbAC�AȴA�+A��AĜA�A�A�7A�A�wA
��A	��A	33A�wAhsA�A~�A�#A��A�`AQ�A�wA�A�-A �H@�E�@��9@�1'@�M�@���@���@�+@�%@�Z@���@�+@�@�R@�&�@�;d@�-@�-@�I�@�K�@�E�@���@�Ĝ@�o@�~�@݉7@܋D@�  @��@�ƨ@�;d@�V@�V@�S�@�X@�1'@�+@�ff@�X@��@�~�@�@�7L@�bN@˾w@���@ȋD@Ǯ@�ȴ@�-@�p�@�V@���@��@�+@��@�?}@� �@�l�@��R@��T@��7@��@�Z@�|�@���@�V@�{@�@�?}@��@��9@�j@�A�@��
@�"�@�n�@�M�@��T@�X@�%@���@��@�S�@�n�@�p�@���@���@�@��R@���@�ff@��^@��@��@�t�@��y@��H@��y@�ȴ@�v�@�5?@�{@���@��#@��-@�O�@��/@���@��@��m@��P@�S�@�K�@�o@���@���@�~�@�$�@�@���@�G�@�V@�V@�%@���@��D@�33@��!@�ff@�M�@�5?@�$�@�$�@�5?@�V@�M�@��9@��
@��F@���@�E�@���@���@��R@��R@��@��R@�v�@�V@�n�@���@��@�dZ@�\)@�@��!@�V@��-@�x�@�hs@�O�@�7L@��j@�(�@�t�@��@���@��H@���@���@�^5@��@�/@���@�%@��@��`@���@��`@��D@��@�r�@�A�@�(�@��;@��w@�|�@�S�@�o@���@��R@�ȴ@�~�@��@�@�O�@�j@�Z@���@�r�@�Z@�9X@� �@��;@�ƨ@�l�@��@�^5@�=q@�{@��-@��h@��@��7@�x�@�O�@�&�@���@��D@�ƨ@��\@�J@��#@��^@��h@�O�@�G�@���@���@���@��m@�33@�
=@��H@���@��+@�-@��#@��#@���@���@���@�hs@�Ĝ@���@��u@��@�I�@� �@�dZ@�C�@���@��@���@�v�@�-@���@�@��-@�p�@�?}@��@��/@��/@��@�j@�Q�@�9X@�b@���@�K�@�o@��R@�v�@�^5@�$�@�{@��^@�hs@�G�@��@�I�@�Q�@�bN@�r�@���@�bN@�9X@� �@�;@�@\)@~ff@}��@}O�@{�m@{��@{��@{�@{t�@{33@{o@z��@{"�@{�m@|I�@|�@{ƨ@{��@z��@z^5@z-@zJ@y�@y�^@y�7@y&�@x�`@x  @w�P@w
=@v5?@t�/@s��@s�
@sS�@sS�@s"�@r��@s33@s33@sC�@r�@r�\@rM�@r-@r�@rJ@q�^@p�`@pQ�@o|�@n�y@n{@m��@m?}@k��@kS�@kt�@k��@k��@k@j�!@i�#@h��@h1'@gK�@f�y@fV@e�-@e�@e/@eV@eV@dz�@d�D@d�@d��@d�j@d�@d��@dz�@cdZ@b~�@bJ@bJ@a�@a��@a�@_�@_
=@^��@^�R@^v�@^$�@]�h@]?}@\�@\j@\1@[��@[33@Z��@[@[@["�@[o@Z�@Zn�@ZM�@Z-@ZJ@Y��@YX@X��@XbN@W�w@Wl�@WK�@V��@Vȴ@V��@Vff@V$�@V@U@U�h@U�@U`B@T��@TI�@Sƨ@S�@S33@S"�@R��@R��@Rn�@R�@Q�@Q��@Q&�@P�u@PA�@O�;@O�@OK�@N��@N��@N5?@N$�@N{@M��@M��@MO�@M�@L��@Lj@LI�@L�@K�m@K�F@K33@K@J��@J��@J^5@I�#@I��@I&�@H��@H1'@G��@G+@F�@F�+@F{@E�-@E/@D�j@Dj@D�@Cƨ@CC�@B�@B�@B��@B~�@B=q@BJ@A��@A��@AG�@A%@@��@@��@@bN@@b@?��@?|�@?;d@>�@>$�@=�@=��@=�@<��@<�j@<�D@<9X@;�F@;��@;��@;�@;dZ@:n�@:-@9��@9x�@9G�@9&�@9�@8��@8��@8�@8 �@7�w@7�@7K�@7+@6�y@6�+@5�@5�T@5@5p�@5�@4�/@4z�@4I�@4�@3�m@3ƨ@3�@333@2�H@2��@2^5@2=q@2J@1�#@1�7@1X@0Ĝ@01'@0 �@0b@0b@/�w@/�P@/\)@/+@/�@/
=@.��@.��@.$�@-��@-�@-/@,��@,�@,�D@,9X@+��@+�F@+�@+dZ@+dZ@+S�@+"�@+@*~�@)�#@)��@)��@)�7@)�7@)�7@)hs@)G�@)%@(��@(�9@(��@(��@(�u@(Q�@(1'@( �@(b@'�@'��@'��@'|�@'K�@'+@'�@'
=@&�R@&V@&{@%��@%�-@%�-@%�-@%�-@%�@%/@%V@$�/@$�j@$�D@$z�@$j@$(�@#�m@#�
@#��@#S�@#33@#@"�H@"�\@"�\@"~�@"n�@"^5@"^5@"^5@"=q@"�@!�@!��@!�7@!X@!�@ �`@ ��@ �9@ �@ 1'@�;@��@|�@K�@�@�y@ȴ@�+@V@V@5?@$�@�T@��@p�@O�@V@z�@9X@9X@(�@��@�m@�F@�@dZ@C�@�@~�@�@��@�7@G�@%@�9@bN@1'@b@�;@��@|�@K�@+@�@
=@�y@�R@��@��@�+@v�@ff@V@5?@�@��@p�@O�@�@�@�j@��@j@9X@�@1@1@�m@�m@�
@��@�@dZ@33@�@��@~�@^5@M�@=q@�@��@�#@��@��@�^@��@x�@&�@�`@Ĝ@�u@bN@Q�@A�@  @�@�w@�P@l�@;d@��@�y@�y@ȴ@�R@�R@�R@�R@�R@�R@��@v�@v�@ff@E�@$�@@�@�T@@@�h@`B@V@�/@�@z�@j@I�@(�@�@��@�m@�m@�m@�
@�F@��@��@��@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�9XA�;dA�5?A�/A�(�A��A��A�1AуA�$�A�\)A�hsA���A�&�AþwA©�A���A�bA���A�z�A� �A�(�A�\)A�A�A��#A�=qA�v�A�/A��A�  A�`BA�(�A�1A�K�A��#A���A�|�A��A�E�A��A�z�A��A��A�5?A�\)A��A��9A���A�E�A���A�XA��A��A��A�|�A�VA�{A�l�A��\A��A�;dA��9A�+A��-A�5?A�E�A���A�bNA��A�=qA�&�A��A���A�VA���A��A�ȴA��^A��`A�S�A�t�A���A�A���A��A���A�K�A�(�A�v�A��`A�A��hA���A� �A� �A���A���A�M�A�JA���A�C�A��A}�TA}A|Q�A{�wAy�TAu�Ap~�Al��AhA�Af�Ae�FAdr�AaC�A]t�A[�AY�-AWVAU33AS�wAQ��AP��AO�#AO��AO�^AO�7AN��ALbAH�!AG�AD�\AB�HAB9XAA��A@��A?�
A>  A;ƨA;S�A:�A:VA8�uA7��A6�HA5��A5�7A4�A3O�A3%A2�A0^5A-�TA+��A+%A*�\A*�uA*�A)��A)"�A(I�A&�A$��A#��A!S�AAK�A�!A�A{AXA"�A��Av�A�7A�AI�A��A�A�DA��AG�AbAC�AȴA�+A��AĜA�A�A�7A�A�wA
��A	��A	33A�wAhsA�A~�A�#A��A�`AQ�A�wA�A�-A �H@�E�@��9@�1'@�M�@���@���@�+@�%@�Z@���@�+@�@�R@�&�@�;d@�-@�-@�I�@�K�@�E�@���@�Ĝ@�o@�~�@݉7@܋D@�  @��@�ƨ@�;d@�V@�V@�S�@�X@�1'@�+@�ff@�X@��@�~�@�@�7L@�bN@˾w@���@ȋD@Ǯ@�ȴ@�-@�p�@�V@���@��@�+@��@�?}@� �@�l�@��R@��T@��7@��@�Z@�|�@���@�V@�{@�@�?}@��@��9@�j@�A�@��
@�"�@�n�@�M�@��T@�X@�%@���@��@�S�@�n�@�p�@���@���@�@��R@���@�ff@��^@��@��@�t�@��y@��H@��y@�ȴ@�v�@�5?@�{@���@��#@��-@�O�@��/@���@��@��m@��P@�S�@�K�@�o@���@���@�~�@�$�@�@���@�G�@�V@�V@�%@���@��D@�33@��!@�ff@�M�@�5?@�$�@�$�@�5?@�V@�M�@��9@��
@��F@���@�E�@���@���@��R@��R@��@��R@�v�@�V@�n�@���@��@�dZ@�\)@�@��!@�V@��-@�x�@�hs@�O�@�7L@��j@�(�@�t�@��@���@��H@���@���@�^5@��@�/@���@�%@��@��`@���@��`@��D@��@�r�@�A�@�(�@��;@��w@�|�@�S�@�o@���@��R@�ȴ@�~�@��@�@�O�@�j@�Z@���@�r�@�Z@�9X@� �@��;@�ƨ@�l�@��@�^5@�=q@�{@��-@��h@��@��7@�x�@�O�@�&�@���@��D@�ƨ@��\@�J@��#@��^@��h@�O�@�G�@���@���@���@��m@�33@�
=@��H@���@��+@�-@��#@��#@���@���@���@�hs@�Ĝ@���@��u@��@�I�@� �@�dZ@�C�@���@��@���@�v�@�-@���@�@��-@�p�@�?}@��@��/@��/@��@�j@�Q�@�9X@�b@���@�K�@�o@��R@�v�@�^5@�$�@�{@��^@�hs@�G�@��@�I�@�Q�@�bN@�r�@���@�bN@�9X@� �@�;@�@\)@~ff@}��@}O�@{�m@{��@{��@{�@{t�@{33@{o@z��@{"�@{�m@|I�@|�@{ƨ@{��@z��@z^5@z-@zJ@y�@y�^@y�7@y&�@x�`@x  @w�P@w
=@v5?@t�/@s��@s�
@sS�@sS�@s"�@r��@s33@s33@sC�@r�@r�\@rM�@r-@r�@rJ@q�^@p�`@pQ�@o|�@n�y@n{@m��@m?}@k��@kS�@kt�@k��@k��@k@j�!@i�#@h��@h1'@gK�@f�y@fV@e�-@e�@e/@eV@eV@dz�@d�D@d�@d��@d�j@d�@d��@dz�@cdZ@b~�@bJ@bJ@a�@a��@a�@_�@_
=@^��@^�R@^v�@^$�@]�h@]?}@\�@\j@\1@[��@[33@Z��@[@[@["�@[o@Z�@Zn�@ZM�@Z-@ZJ@Y��@YX@X��@XbN@W�w@Wl�@WK�@V��@Vȴ@V��@Vff@V$�@V@U@U�h@U�@U`B@T��@TI�@Sƨ@S�@S33@S"�@R��@R��@Rn�@R�@Q�@Q��@Q&�@P�u@PA�@O�;@O�@OK�@N��@N��@N5?@N$�@N{@M��@M��@MO�@M�@L��@Lj@LI�@L�@K�m@K�F@K33@K@J��@J��@J^5@I�#@I��@I&�@H��@H1'@G��@G+@F�@F�+@F{@E�-@E/@D�j@Dj@D�@Cƨ@CC�@B�@B�@B��@B~�@B=q@BJ@A��@A��@AG�@A%@@��@@��@@bN@@b@?��@?|�@?;d@>�@>$�@=�@=��@=�@<��@<�j@<�D@<9X@;�F@;��@;��@;�@;dZ@:n�@:-@9��@9x�@9G�@9&�@9�@8��@8��@8�@8 �@7�w@7�@7K�@7+@6�y@6�+@5�@5�T@5@5p�@5�@4�/@4z�@4I�@4�@3�m@3ƨ@3�@333@2�H@2��@2^5@2=q@2J@1�#@1�7@1X@0Ĝ@01'@0 �@0b@0b@/�w@/�P@/\)@/+@/�@/
=@.��@.��@.$�@-��@-�@-/@,��@,�@,�D@,9X@+��@+�F@+�@+dZ@+dZ@+S�@+"�@+@*~�@)�#@)��@)��@)�7@)�7@)�7@)hs@)G�@)%@(��@(�9@(��@(��@(�u@(Q�@(1'@( �@(b@'�@'��@'��@'|�@'K�@'+@'�@'
=@&�R@&V@&{@%��@%�-@%�-@%�-@%�-@%�@%/@%V@$�/@$�j@$�D@$z�@$j@$(�@#�m@#�
@#��@#S�@#33@#@"�H@"�\@"�\@"~�@"n�@"^5@"^5@"^5@"=q@"�@!�@!��@!�7@!X@!�@ �`@ ��@ �9@ �@ 1'@�;@��@|�@K�@�@�y@ȴ@�+@V@V@5?@$�@�T@��@p�@O�@V@z�@9X@9X@(�@��@�m@�F@�@dZ@C�@�@~�@�@��@�7@G�@%@�9@bN@1'@b@�;@��@|�@K�@+@�@
=@�y@�R@��@��@�+@v�@ff@V@5?@�@��@p�@O�@�@�@�j@��@j@9X@�@1@1@�m@�m@�
@��@�@dZ@33@�@��@~�@^5@M�@=q@�@��@�#@��@��@�^@��@x�@&�@�`@Ĝ@�u@bN@Q�@A�@  @�@�w@�P@l�@;d@��@�y@�y@ȴ@�R@�R@�R@�R@�R@�R@��@v�@v�@ff@E�@$�@@�@�T@@@�h@`B@V@�/@�@z�@j@I�@(�@�@��@�m@�m@�m@�
@�F@��@��@��@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B
�^B
�^B
�^B
�^B
�RB
�9B
��B
��B�B`BB)�B6FB��B�BŢB�BB��BB�B�B  B	7B0!B-B(�B1'BD�B<jB=qB?}BL�BN�BB�BN�BL�Bl�Bk�BcTB{�B�{B�B�VB�{B�oB��B��B�bB��B��B�oB�oB�7B� B�B|�Bl�BjBcTBbNB\)BT�BS�BK�BF�B8RB33B:^B(�B�BB��B�BB��B�B�B��B�RB�9B�'B�{B`BBm�BO�BP�BC�BC�BG�B7LBB
�TB
�\B
�uB
��B
�B
x�B
l�B
u�B
q�B
_;B
<jB
)�B
%�B
'�B
#�B
oB	�B	��B	�DB	�+B	]/B	t�B	s�B	e`B	A�B	!�B	'�B	�B��B��B��B�yB��B	B	VB		7B��B�sBǮB�B�}B�FB�^B��BȴBB�dB�!B��B�qB�XB�?B��B��B��B��B��B��B��B��B��B|�Br�Bk�B�B�%B�bB�JB�7B�1B}�Bm�B_;B_;BR�BQ�B`BB\)BQ�BH�BP�BW
BD�BG�BL�BO�BP�BP�BR�BR�BL�BO�BJ�BF�BP�B]/BS�BN�BO�BXBR�BK�BC�BH�BF�BI�BA�BS�BZBZBZB_;BYBZBXBT�BI�BQ�BG�BZBcTBZBbNBP�BK�B`BBp�Bo�BhsBbNBe`BjBhsBp�Bt�Bm�Bq�Br�Bw�Bt�Bp�B{�B|�B~�B�B�+B�B�B~�B{�Bz�B|�B�B� B� B}�B{�B� B�%B�DB�7B�DB�B�DB�oB��B��B��B��B��B��B��B��B�B�B�?B�RB�FB�^B�FB�XB�XB��B��BƨBƨBƨB��B��B��B��B��B�
B�#B�TB�HB�HB�TB�TB�TB�NB�TB�mB�B�B��B��B��B��B��B��B	B	
=B	JB	�B	�B	�B	!�B	'�B	,B	.B	.B	.B	-B	0!B	6FB	7LB	6FB	?}B	D�B	G�B	H�B	I�B	N�B	P�B	Q�B	S�B	S�B	VB	XB	[#B	ZB	XB	S�B	M�B	T�B	[#B	^5B	_;B	`BB	aHB	aHB	aHB	]/B	W
B	XB	`BB	]/B	bNB	t�B	u�B	v�B	z�B	|�B	{�B	{�B	~�B	�B	�hB	�VB	�VB	�{B	�oB	��B	��B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	��B	��B	��B	��B	�'B	�3B	�3B	�^B	�^B	�^B	�XB	�wB	�}B	�}B	��B	��B	��B	��B	ÖB	ÖB	ĜB	ƨB	ǮB	ȴB	ƨB	ɺB	ȴB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�)B	�/B	�/B	�;B	�5B	�BB	�;B	�)B	�B	��B	�B	�5B	�5B	�5B	�;B	�NB	�NB	�TB	�ZB	�BB	�5B	�TB	�fB	�ZB	�fB	�`B	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
B
+B
1B
	7B

=B
DB
DB
DB
DB
VB
JB

=B
DB
JB
	7B
PB
bB
bB
bB
bB
hB
oB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
$�B
"�B
"�B
!�B
!�B
!�B
"�B
"�B
"�B
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
#�B
#�B
"�B
$�B
#�B
#�B
!�B
"�B
"�B
"�B
#�B
%�B
&�B
(�B
(�B
'�B
+B
-B
.B
.B
-B
-B
,B
)�B
(�B
,B
/B
/B
.B
,B
)�B
(�B
+B
,B
,B
+B
+B
+B
,B
,B
,B
-B
,B
-B
0!B
0!B
2-B
2-B
2-B
1'B
2-B
33B
33B
2-B
1'B
1'B
33B
2-B
49B
5?B
5?B
5?B
6FB
6FB
6FB
6FB
6FB
6FB
6FB
6FB
5?B
5?B
6FB
8RB
8RB
9XB
8RB
9XB
9XB
9XB
9XB
9XB
8RB
8RB
:^B
:^B
;dB
:^B
:^B
<jB
<jB
>wB
>wB
>wB
>wB
>wB
>wB
>wB
?}B
?}B
?}B
?}B
?}B
>wB
?}B
?}B
@�B
?}B
?}B
@�B
?}B
?}B
@�B
@�B
?}B
A�B
A�B
A�B
A�B
A�B
B�B
B�B
C�B
C�B
C�B
D�B
E�B
E�B
D�B
E�B
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
F�B
H�B
I�B
G�B
I�B
J�B
J�B
J�B
J�B
L�B
L�B
K�B
K�B
I�B
L�B
M�B
L�B
M�B
N�B
N�B
M�B
N�B
N�B
M�B
N�B
O�B
N�B
O�B
O�B
N�B
N�B
Q�B
P�B
P�B
P�B
Q�B
Q�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
S�B
S�B
T�B
T�B
S�B
S�B
S�B
S�B
S�B
W
B
W
B
W
B
VB
W
B
W
B
W
B
XB
XB
XB
VB
VB
W
B
XB
XB
XB
YB
YB
YB
YB
YB
ZB
[#B
[#B
[#B
ZB
ZB
YB
ZB
\)B
\)B
]/B
]/B
]/B
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
^5B
]/B
]/B
]/B
^5B
^5B
^5B
^5B
^5B
]/B
]/B
^5B
_;B
_;B
`BB
`BB
`BB
`BB
_;B
`BB
`BB
aHB
aHB
aHB
aHB
aHB
aHB
aHB
aHB
aHB
aHB
aHB
bNB
aHB
cTB
cTB
cTB
cTB
cTB
cTB
cTB
bNB
bNB
bNB
cTB
cTB
bNB
cTB
dZB
cTB
cTB
cTB
cTB
dZB
dZB
dZB
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
gmB
iyB
iyB
hsB
iyB
iyB
iyB
iyB
iyB
hsB
hsB
hsB
iyB
jB
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
n�B
n�B
m�B
m�B
n�B
o�B
o�B
o�B
o�B
p�B
o�B
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
q�B
q�B
r�B
r�B
r�B
r�B
s�B
r�B
s�B
s�B
s�B
s�B
s�B
s�B
r�B
s�B
t�B
s�B
t�B
u�B
t�B
t�B
u�B
t�B
u�B
u�B
u�B
u�B
v�B
v�B
v�B
v�B
v�B
v�B
w�B
v�B
v�B
v�B
v�B
v�B
v�B
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
x�B
x�B
x�B
x�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
z�B
z�B
z�B
z�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B
�^B
�xB
�xB
��B
��B
�tB
�eB
��B�Bb�B2-B=<B�=B� B��B�NB��BB�B�OB�B
�B0!B.B*eB2�BE9B=�B>�B@�BM6BO\BDMBPHBO(Bl�Bl�BeB|�B�2B�+B��B�mB�,B��B�dB�:B��B�dB��B�&B��B�oB�3B~(Bn�Bl"BeFBc�B]�BV9BU2BMBG�B:DB4�B:�B*�BB�B�<B�TB��B�B�/B��B��B�FB�hB�yBd�BoOBSBS[BE�BDgBH�B9�B�B
�XB
��B
�
B
��B
��B
{�B
n�B
v�B
r|B
`�B
?�B
,�B
(>B
(�B
$�B
�B	�B	�B	� B	�^B	b4B	v`B	utB	g�B	E�B	%�B	*0B	_B	;B�BB��B�B�B	�B	�B		�B��B�KB�DB�B��B�XB�PBˬBɠB��B�B��B�FB��B�*B�FB��B�@B��B�0B��B�|B��B��B�
B}Bu�Bn/B��B��B��B�B�	B�7B}Bo�Ba�BabBU�BS�B`�B]IBSuBJ�BQ�BW�BF�BI7BN"BP�BQ�BQ�BS�BS�BN"BP�BLJBG�BQ�B]�BUBP.BP�BX_BS�BL�BESBI�BG�BJ�BC-BT�BZ�B[	B[#B_�BZ7B[#BYBVmBK�BS@BJ	B[#Bc�B[�Bb�BS@BNBa�Bq'Bp;Bi�Bc�Bf�Bk�Bi�BqvBu?Bn�BraBshBxlBu�Bq�B|jB}�B�B�uB�EB��B��B�B}B|6B~]B��B��B��B~�B|�B�B��B��B��B��B�mB�JB�&B�$B�)B�OB�FB�RB�zB��B��B��B��B��B��B��B��B��B��B�B�B�B��B��B�B�B�B�4B�,B�gB׍B��B�B�B�B�B�B��B��B�B�$B�QB�UB�2B�"B�B�jB�xB��B	�B	
�B	�B	�B	�B	�B	"4B	($B	,"B	./B	.IB	.IB	-]B	0oB	6�B	7�B	6�B	?�B	D�B	G�B	H�B	I�B	OB	QB	R:B	T,B	TFB	VSB	XEB	[#B	Z7B	XEB	T{B	N�B	UMB	[qB	^OB	_VB	`vB	aHB	aHB	aHB	]~B	XB	X�B	`\B	]�B	b�B	t�B	u�B	v�B	z�B	|�B	|B	|6B	~�B	��B	��B	�pB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�!B	�'B	�HB	�B	�B	�B	�B	�0B	�DB	��B	�DB	�'B	��B	��B	�DB	�^B	�xB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�+B	��B	�7B	�1B	��B	ҽB	�B	� B	� B	� B	�B	�@B	�:B	�TB	�uB	�1B	�KB	�yB	�CB	�dB	�/B	�VB	�jB	�vB	�pB	ܒB	��B	��B	�eB	�jB	�OB	�jB	�pB	�hB	�B	�B	�tB	��B	޸B	�B	�B	�B	�B	�B	��B	�B	�B	��B	��B	��B	�B	�B	��B	��B	��B	��B	�'B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	�	B	�B	��B	�B	�B	�"B	�"B	�B	�0B	�PB	�"B	�BB	�.B
 B
;B
-B
UB
[B
9B
uB
GB
+B
1B
	7B

XB
xB
xB
^B
xB
pB
�B

�B
�B
�B
	�B
jB
bB
}B
}B
}B
�B
�B
�B
eB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
B
B
�B
�B
�B
�B
�B
$�B
"�B
"�B
!�B
!�B
!�B
"�B
"�B
"�B
!B
B
�B
B
�B
B
B
�B
B
B
 �B
#�B
#�B
# B
%B
$@B
$@B
"4B
#:B
# B
#B
$&B
&B
'B
)B
)B
($B
+B
-)B
.B
.B
-CB
-)B
,WB
*B
)_B
,"B
/B
/5B
.IB
,WB
*B
)DB
+B
,B
,"B
+6B
+6B
+6B
,"B
,WB
,WB
-CB
,=B
-)B
0!B
0;B
2-B
2aB
2aB
1[B
2GB
3hB
3hB
2GB
1[B
1vB
3hB
2|B
4TB
5tB
5tB
5ZB
6`B
6`B
6`B
6zB
6`B
6zB
6zB
6`B
5tB
5tB
6zB
8lB
8lB
9rB
8lB
9rB
9rB
9rB
9�B
9�B
8�B
8�B
:�B
:xB
;B
:�B
:�B
<�B
<�B
>wB
>�B
>�B
>�B
>�B
>�B
>�B
?�B
?�B
?�B
?�B
?�B
>�B
?�B
?�B
@�B
?�B
?�B
@�B
?�B
?�B
@�B
@�B
?�B
A�B
A�B
A�B
A�B
A�B
B�B
B�B
C�B
C�B
C�B
D�B
E�B
E�B
D�B
E�B
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
F�B
H�B
I�B
HB
I�B
J�B
J�B
J�B
J�B
L�B
L�B
K�B
K�B
J#B
L�B
NB
MB
M�B
N�B
N�B
M�B
N�B
N�B
NB
N�B
O�B
O(B
O�B
PB
O(B
OB
RB
Q B
QB
Q B
RB
RB
SB
SB
SB
SB
SB
SB
SB
T,B
TB
UB
UB
TB
TB
T,B
TFB
T,B
W
B
W
B
W
B
VB
W$B
W$B
W$B
X+B
X+B
XEB
VSB
V9B
W?B
X+B
X+B
XEB
YKB
Y1B
Y1B
YKB
Y1B
Z7B
[=B
[#B
[WB
Z7B
Z7B
YeB
ZQB
\CB
\CB
]/B
]IB
]/B
\CB
\]B
\CB
\]B
]/B
]IB
]/B
]IB
]IB
]IB
^5B
^OB
]IB
]dB
]dB
^jB
^jB
^jB
^OB
^jB
]IB
]dB
^jB
_VB
_VB
`BB
`\B
`\B
`\B
_pB
`vB
`\B
a|B
abB
aHB
a|B
abB
abB
a|B
abB
abB
a|B
abB
b�B
abB
cTB
cTB
cTB
cTB
cnB
cTB
cnB
b�B
bhB
bhB
cnB
c�B
bhB
cnB
dZB
cnB
cnB
cnB
cnB
dtB
dtB
dtB
ezB
ezB
ezB
ezB
f�B
ffB
f�B
f�B
f�B
f�B
f�B
f�B
f�B
f�B
g�B
i�B
iyB
h�B
iyB
i�B
i�B
i�B
i�B
h�B
h�B
h�B
i�B
j�B
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
n�B
n�B
m�B
m�B
n�B
o�B
o�B
o�B
o�B
p�B
o�B
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
q�B
q�B
r�B
r�B
r�B
r�B
s�B
r�B
s�B
s�B
s�B
s�B
s�B
s�B
r�B
s�B
t�B
s�B
t�B
u�B
t�B
t�B
u�B
t�B
u�B
u�B
u�B
u�B
v�B
v�B
v�B
v�B
v�B
v�B
w�B
v�B
v�B
v�B
v�B
v�B
v�B
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
x�B
x�B
x�B
x�B
y�B
y�B
y�B
y�B
y�B
y�B
zB
y�B
z�B
z�B
z�B
z�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.05(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201807080040152018070800401520180708004015201807080200262018070802002620180708020026201807090029492018070900294920180709002949  JA  ARFMdecpA19c                                                                20180723123519  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20180723033646  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20180723033649  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20180723033649  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20180723033650  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20180723033650  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20180723033650  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20180723033650  QCP$                G�O�G�O�G�O�            FB40JA      jafc1.0                                                                 20180723033650                      G�O�G�O�G�O�                JA  ARUP                                                                        20180723040127                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20180704153528  CV  JULD            G�O�G�O�F�y�                JM  ARCAJMQC2.0                                                                 20180707154015  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180707154015  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180707170026  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180708152949  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200116231516                      G�O�G�O�G�O�                