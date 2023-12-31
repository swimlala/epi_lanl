CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T17:35:19Z creation;2022-06-04T17:35:20Z conversion to V3.1      
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
_FillValue                    �PArgo profile    3.1 1.2 19500101000000  20220604173519  20220610131507  5905853                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               GA   JA                                  2B  A   APEX                            8421                            2.11.2                          846 @�V��H�Z1   @�V�"���@.G+I��c��j~��1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  A�33B  B  B  B   B(ffB0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�ffB�  B�33B���B�ffB�ffB�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"�C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cu�fCx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C��3C��3C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,�fD-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� DtfDt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׃3D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @fg@|��@�ff@�ffA33A?33A_33A33A���A���A���A���Aϙ�Aߙ�AA���B��B��B��B��B(33B/��B7��B?��BG��BO��BW��B_��Bg��Bo��Bw��B��B��fB��fB��fB��fB��fB��fB��fB��B��B�L�B��fB��B�� B�L�B�L�B��fB��fB��fB��fB��fB��fB��fB��fB��B��fB��fB��fB��fB��fB��fB��fB��fC�3C�3C�3C�3C	�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C"�C#�3C%�3C'�3C)�3C+�3C-�3C/�3C1�3C3�3C5�3C7�3C9�3C;�3C=�3C?�3CA�3CC�3CE�3CG�3CI�3CK�3CM�3CO�3CQ�3CS�3CU�3CW�3CY�3C[�3C]�3C_�3Ca�3Cc�3Ce�3Cg�3Ci�3Ck�3Cm�3Co�3Cq�3Cs�3CuٙCw�3Cy�3C{�3C}�3C�3C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C�gC�gC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C�gC�gC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D |�D ��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D	|�D	��D
|�D
��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D |�D ��D!|�D!��D"|�D"��D#|�D#��D$|�D$��D%|�D%��D&|�D&��D'|�D'��D(|�D(��D)|�D)��D*|�D*��D+|�D+��D,�3D,��D-|�D-��D.|�D.��D/|�D/��D0|�D0��D1|�D1��D2|�D2��D3|�D3��D4|�D4��D5|�D5��D6|�D6��D7|�D7��D8|�D8��D9|�D9��D:|�D:��D;|�D;��D<|�D<��D=|�D=��D>|�D>��D?|�D?��D@|�D@��DA|�DA��DB|�DB��DC|�DC��DD|�DD��DE|�DE��DF|�DF��DG|�DG��DH|�DH��DI|�DI��DJ|�DJ��DK|�DK��DL|�DL��DM|�DM��DN|�DN��DO|�DO��DP|�DP��DQ|�DQ��DR|�DR��DS|�DS��DT|�DT��DU|�DU��DV|�DV��DW|�DW��DX|�DX��DY|�DY��DZ|�DZ��D[|�D[��D\|�D\��D]|�D]��D^|�D^��D_|�D_��D`|�D`��Da|�Da��Db|�Db��Dc|�Dc��Dd|�Dd��De|�De��Df|�Df��Dg|�Dg��Dh|�Dh��Di|�Di��Dj|�Dj��Dk|�Dk��Dl|�Dl��Dm|�Dm��Dn|�Dn��Do|�Do��Dp|�Dp��Dq|�Dq��Dr|�Dr��Ds|�Dt3Dt|�Dt��Du|�Du��Dv|�Dv��Dw|�Dw��Dx|�Dx��Dy|�Dy��Dz|�Dz��D{|�D{��D||�D|��D}|�D}��D~|�D~��D|�D��D�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD¾fD��fD�>fD�~fDþfD��fD�>fD�~fDľfD��fD�>fD�~fDžfD��fD�>fD�~fDƾfD��fD�>fD�~fDǾfD��fD�>fD�~fDȾfD��fD�>fD�~fDɾfD��fD�>fD�~fDʾfD��fD�>fD�~fD˾fD��fD�>fD�~fD̾fD��fD�>fD�~fD;fD��fD�>fD�~fDξfD��fD�>fD�~fDϾfD��fD�>fD�~fDоfD��fD�>fD�~fDѾfD��fD�>fD�~fDҾfD��fD�>fD�~fDӾfD��fD�>fD�~fDԾfD��fD�>fD�~fDվfD��fD�>fD�~fD־fD��fD�>fDׁ�D׾fD��fD�>fD�~fDؾfD��fD�>fD�~fDپfD��fD�>fD�~fDھfD��fD�>fD�~fD۾fD��fD�>fD�~fDܾfD��fD�>fD�~fDݾfD��fD�>fD�~fD޾fD��fD�>fD�~fD߾fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD��fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�;3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�!�A�!bA��A� 'A�#A��A��A��A��A��A�A��A�  A�oA� �A��rA��|A���A���A���A��jA���A���A��pA��BA��HA���A�چA�ٴA��9A���A���A�̘A�̘A��6A��<A��vA��A�m�A�}VA���A���A�
rA�K�A�h>A���A���A>�Ax�MAwC�Auu%Ame�Af��AcZ�A_�5A_�A\7LAYoiAW��AWU�AV�:AU��ATW�AR�KAO�AM҉AK�AJ	lAH�AEH�AC��AC6zAA�&A?��A>��A=�mA=�A<�uA;�A;(�A;$A;3�A:�9A:0�A9xA8�dA7�A6��A5�+A5�A5�yA5ԕA4$�A3��A3S�A3A�A39XA3A2��A1��A1�$A1�A0�A0GEA0�A/�|A/qA.�4A.oA,�KA+�HA+9�A*��A*"hA)v`A)�A(��A(\�A($�A'��A'.�A&u�A%�LA%#:A$�AA$:�A$#�A#�#A#6zA"<6A!��A!��A �|A C�A�bA�AK^AVA�BA�AN<AJA��AیA8�A�A�A��A~�A#�A�MA"hAXA�AD�A��Al�A�A�A��A��A7AA�A�SA�
A�A��A7LA
��A
�A	�A	#�Am]A�AY�A�mA�vAA�rA��A�A��A��AA A��A��A��A�AOA �A �A PH@�O@��@�`B@��f@��@��@�$�@��@���@�	@���@���@�u�@��Q@���@�<6@��)@�ѷ@�ں@�;d@�{�@퐗@�L@�B�@�@�7�@���@��@��@�t�@�e,@� \@���@�h�@���@���@��A@�g8@㕁@��@�y>@�ԕ@�o�@��@�i�@��@��a@߫�@�X@��,@�C�@�t�@�dZ@�j@�b�@ܝI@���@��m@���@�s�@�J#@�#�@��@���@�?@ٜ�@�E9@��@���@�c�@�$@���@׀4@�ȴ@�GE@��g@�m]@�!-@�-@ә�@��@�l"@�c@�;d@�ں@� �@�zx@�S&@��"@��@�Ov@��Q@͜�@�4�@�n�@�,=@�/�@�?@�f�@�K�@��P@ʙ1@�B[@ɹ�@ɄM@�U�@�S&@ɘ�@�I�@�1�@�RT@�ں@���@ǂ�@�Vm@�&@��s@ƒ�@�3�@Ů@ņ�@�=�@��[@�Z@���@��a@ÖS@�S�@�"�@���@±�@�c�@���@��@�ff@��D@�{J@��K@���@���@��2@�D�@�@�ϫ@�|@���@��@�(�@��r@�Q�@��@���@���@�A�@��@���@��K@���@�A�@���@��@���@��m@���@��@�"�@�ff@�@�Q�@�#�@��@��c@���@���@���@�J#@�c�@�ԕ@��0@���@���@�u�@��@���@�|�@�Z@�8�@�	�@�a@��\@�8�@��|@��<@�"h@�O@�҉@��x@�c @�O@���@��.@���@��m@��W@��@��)@��0@���@�o @��@��.@�9X@��V@��b@�l"@�^5@�S�@�Ov@�C-@�'R@�J@��A@��d@���@�hs@�8@�$t@��@�v�@�2�@�خ@���@�
=@��m@��}@��.@��Y@�ff@�0U@���@���@���@�C�@���@�a|@���@�
=@��	@�tT@�u@�p�@�A�@�4�@�!-@���@���@�{�@�bN@�PH@�;�@���@��X@�
=@�ȴ@�YK@��a@�,�@���@���@��u@�d�@�1@��@��0@�b�@�2a@��@��@���@�{�@�7�@�e@��@�dZ@�Dg@��@���@���@�@���@�n/@��"@��\@�"h@��
@��	@�V@��}@�bN@�?@��@�rG@�?}@���@���@���@�PH@��@��K@��@@��4@�@O@�@��@��@��u@�q@�B[@��W@���@��V@�c@�Z�@��@���@��@�h�@�<�@�&�@��A@���@���@��@�x�@��@�֡@���@���@��@��Y@�$�@���@��g@��X@�u�@�u�@�p�@�O�@��@���@���@��@�H@��@��@v`@~�y@~q�@~M�@}�@}��@}��@|�v@|]d@{��@z�'@z&�@y�n@x�)@xD�@xx@w��@w�$@v�@v��@v�A@v��@v^5@u��@tی@t��@tPH@t?�@s��@s{J@s!-@r��@rxl@q��@qJ�@q5�@q�@p��@p�@o�@n�B@n�F@m�o@m��@m�@m�@mp�@m0�@l�@l��@l/�@k��@kO@j��@i��@iT�@i \@h��@hی@h��@h~(@g��@gP�@fs�@e��@e<6@c�@b�,@b��@b�A@bW�@a\�@`��@`�@`��@`'R@_�f@_E9@_�@^��@^�<@^($@]Q�@\�@\�@\�@[خ@[�{@[�@Z��@ZkQ@Za|@ZW�@ZV@Z4@Y�@Yk�@Y=�@Y�@X�p@X$@W�;@W�@WA�@Va|@V�@U|@U@@T�/@T�j@T��@T��@Tl"@Te�@TU2@T[�@T!@S��@S�@R�}@RZ�@R-@Q�9@Qc@Q8�@P~@O_p@N�H@NL0@M��@M��@M^�@M:�@L��@L��@L��@L�9@L�@LD�@K��@K��@K$t@J��@J��@J�@Jp;@J8�@I��@Is�@IF@I%F@H�@HI�@HG@G"�@Fl�@F0U@E��@E��@E^�@E#�@D�z@D4n@C��@C��@Cg�@CH�@C1�@B��@B�!@B��@B=q@B@A�>@A��@A�@@��@@�@@V�@@�@?�*@?�@>��@>E�@=�@=G�@<�@<H@;��@;�q@;v`@;Y@:�B@:@�@:)�@:�@9��@9��@8��@8M@7�w@7�{@7W?@7(@6��@6�6@6��@6E�@6�@5@5��@5*0@4�I@4c�@4'R@41@3�}@3�0@2��@2��@2u%@2Q@26�@2�@1�@1�M@1%F@0�@0��@0|�@0U2@01@/�@/�q@/�	@/8@.ߤ@.��@.�@.� @.GE@.{@-�o@-�9@-�@-��@-�@-^�@-T�@-G�@,��@,��@,r�@,�@+��@+�@+��@+�@*�@*��@*��@*l�@*	@)�3@)�X@)o @)[W@)T�@)-w@)%@(�`@(�Y@((�@'��@'j�@&�@&��@&�F@%�@%s�@%0�@$�v@$�$@$z�@$*�@#�q@#�k@#s@#@O@#Y@"�\@!�o@!@!�t@!�@!<6@ �e@ I�@ ,=@ �@�+@�@�q@W?@�@�@�,@V@��@ϫ@��@c@&�@#�@�@��@[�@,=@@ƨ@�{@H�@ i@��@ff@R�@L0@=q@#:@�@�T@�=@k�@hs@O�@2a@��@I�@6@b@��@�6@�@��@��@�@e�@A�@=@6z@�@ i@�y@�R@xl@-@�j@��@|@�)@�O@�@��@r�@7�@b@�A@�@��@��@v`@1�@�@�@�8@�@�<@M�@)�@�@�@�@IR@��@�j@��@�O@�z@z�@Ft@$@�]@�
@��@��@~�@C�@�@�@��@Q@�>@�@��@e,@*0@q@�@%@�@�E@֡@�4@m�@:�@!@��@��@W?@�@
��@
��@
��@
��@
s�@
d�@
V@
e@	�@	�@	��@	�X@	�@	hs@	G�@	-w@	*0@	!�@	%@�@�D@�@خ@�@��@n/@8@!-@�y@��@�@�x@��@��@a|@Ov@111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�!�A�!bA��A� 'A�#A��A��A��A��A��A�A��A�  A�oA� �A��rA��|A���A���A���A��jA���A���A��pA��BA��HA���A�چA�ٴA��9A���A���A�̘A�̘A��6A��<A��vA��A�m�A�}VA���A���A�
rA�K�A�h>A���A���A>�Ax�MAwC�Auu%Ame�Af��AcZ�A_�5A_�A\7LAYoiAW��AWU�AV�:AU��ATW�AR�KAO�AM҉AK�AJ	lAH�AEH�AC��AC6zAA�&A?��A>��A=�mA=�A<�uA;�A;(�A;$A;3�A:�9A:0�A9xA8�dA7�A6��A5�+A5�A5�yA5ԕA4$�A3��A3S�A3A�A39XA3A2��A1��A1�$A1�A0�A0GEA0�A/�|A/qA.�4A.oA,�KA+�HA+9�A*��A*"hA)v`A)�A(��A(\�A($�A'��A'.�A&u�A%�LA%#:A$�AA$:�A$#�A#�#A#6zA"<6A!��A!��A �|A C�A�bA�AK^AVA�BA�AN<AJA��AیA8�A�A�A��A~�A#�A�MA"hAXA�AD�A��Al�A�A�A��A��A7AA�A�SA�
A�A��A7LA
��A
�A	�A	#�Am]A�AY�A�mA�vAA�rA��A�A��A��AA A��A��A��A�AOA �A �A PH@�O@��@�`B@��f@��@��@�$�@��@���@�	@���@���@�u�@��Q@���@�<6@��)@�ѷ@�ں@�;d@�{�@퐗@�L@�B�@�@�7�@���@��@��@�t�@�e,@� \@���@�h�@���@���@��A@�g8@㕁@��@�y>@�ԕ@�o�@��@�i�@��@��a@߫�@�X@��,@�C�@�t�@�dZ@�j@�b�@ܝI@���@��m@���@�s�@�J#@�#�@��@���@�?@ٜ�@�E9@��@���@�c�@�$@���@׀4@�ȴ@�GE@��g@�m]@�!-@�-@ә�@��@�l"@�c@�;d@�ں@� �@�zx@�S&@��"@��@�Ov@��Q@͜�@�4�@�n�@�,=@�/�@�?@�f�@�K�@��P@ʙ1@�B[@ɹ�@ɄM@�U�@�S&@ɘ�@�I�@�1�@�RT@�ں@���@ǂ�@�Vm@�&@��s@ƒ�@�3�@Ů@ņ�@�=�@��[@�Z@���@��a@ÖS@�S�@�"�@���@±�@�c�@���@��@�ff@��D@�{J@��K@���@���@��2@�D�@�@�ϫ@�|@���@��@�(�@��r@�Q�@��@���@���@�A�@��@���@��K@���@�A�@���@��@���@��m@���@��@�"�@�ff@�@�Q�@�#�@��@��c@���@���@���@�J#@�c�@�ԕ@��0@���@���@�u�@��@���@�|�@�Z@�8�@�	�@�a@��\@�8�@��|@��<@�"h@�O@�҉@��x@�c @�O@���@��.@���@��m@��W@��@��)@��0@���@�o @��@��.@�9X@��V@��b@�l"@�^5@�S�@�Ov@�C-@�'R@�J@��A@��d@���@�hs@�8@�$t@��@�v�@�2�@�خ@���@�
=@��m@��}@��.@��Y@�ff@�0U@���@���@���@�C�@���@�a|@���@�
=@��	@�tT@�u@�p�@�A�@�4�@�!-@���@���@�{�@�bN@�PH@�;�@���@��X@�
=@�ȴ@�YK@��a@�,�@���@���@��u@�d�@�1@��@��0@�b�@�2a@��@��@���@�{�@�7�@�e@��@�dZ@�Dg@��@���@���@�@���@�n/@��"@��\@�"h@��
@��	@�V@��}@�bN@�?@��@�rG@�?}@���@���@���@�PH@��@��K@��@@��4@�@O@�@��@��@��u@�q@�B[@��W@���@��V@�c@�Z�@��@���@��@�h�@�<�@�&�@��A@���@���@��@�x�@��@�֡@���@���@��@��Y@�$�@���@��g@��X@�u�@�u�@�p�@�O�@��@���@���@��@�H@��@��@v`@~�y@~q�@~M�@}�@}��@}��@|�v@|]d@{��@z�'@z&�@y�n@x�)@xD�@xx@w��@w�$@v�@v��@v�A@v��@v^5@u��@tی@t��@tPH@t?�@s��@s{J@s!-@r��@rxl@q��@qJ�@q5�@q�@p��@p�@o�@n�B@n�F@m�o@m��@m�@m�@mp�@m0�@l�@l��@l/�@k��@kO@j��@i��@iT�@i \@h��@hی@h��@h~(@g��@gP�@fs�@e��@e<6@c�@b�,@b��@b�A@bW�@a\�@`��@`�@`��@`'R@_�f@_E9@_�@^��@^�<@^($@]Q�@\�@\�@\�@[خ@[�{@[�@Z��@ZkQ@Za|@ZW�@ZV@Z4@Y�@Yk�@Y=�@Y�@X�p@X$@W�;@W�@WA�@Va|@V�@U|@U@@T�/@T�j@T��@T��@Tl"@Te�@TU2@T[�@T!@S��@S�@R�}@RZ�@R-@Q�9@Qc@Q8�@P~@O_p@N�H@NL0@M��@M��@M^�@M:�@L��@L��@L��@L�9@L�@LD�@K��@K��@K$t@J��@J��@J�@Jp;@J8�@I��@Is�@IF@I%F@H�@HI�@HG@G"�@Fl�@F0U@E��@E��@E^�@E#�@D�z@D4n@C��@C��@Cg�@CH�@C1�@B��@B�!@B��@B=q@B@A�>@A��@A�@@��@@�@@V�@@�@?�*@?�@>��@>E�@=�@=G�@<�@<H@;��@;�q@;v`@;Y@:�B@:@�@:)�@:�@9��@9��@8��@8M@7�w@7�{@7W?@7(@6��@6�6@6��@6E�@6�@5@5��@5*0@4�I@4c�@4'R@41@3�}@3�0@2��@2��@2u%@2Q@26�@2�@1�@1�M@1%F@0�@0��@0|�@0U2@01@/�@/�q@/�	@/8@.ߤ@.��@.�@.� @.GE@.{@-�o@-�9@-�@-��@-�@-^�@-T�@-G�@,��@,��@,r�@,�@+��@+�@+��@+�@*�@*��@*��@*l�@*	@)�3@)�X@)o @)[W@)T�@)-w@)%@(�`@(�Y@((�@'��@'j�@&�@&��@&�F@%�@%s�@%0�@$�v@$�$@$z�@$*�@#�q@#�k@#s@#@O@#Y@"�\@!�o@!@!�t@!�@!<6@ �e@ I�@ ,=@ �@�+@�@�q@W?@�@�@�,@V@��@ϫ@��@c@&�@#�@�@��@[�@,=@@ƨ@�{@H�@ i@��@ff@R�@L0@=q@#:@�@�T@�=@k�@hs@O�@2a@��@I�@6@b@��@�6@�@��@��@�@e�@A�@=@6z@�@ i@�y@�R@xl@-@�j@��@|@�)@�O@�@��@r�@7�@b@�A@�@��@��@v`@1�@�@�@�8@�@�<@M�@)�@�@�@�@IR@��@�j@��@�O@�z@z�@Ft@$@�]@�
@��@��@~�@C�@�@�@��@Q@�>@�@��@e,@*0@q@�@%@�@�E@֡@�4@m�@:�@!@��@��@W?@�@
��@
��@
��@
��@
s�@
d�@
V@
e@	�@	�@	��@	�X@	�@	hs@	G�@	-w@	*0@	!�@	%@�@�D@�@خ@�@��@n/@8@!-@�y@��@�@�x@��@��@a|@Ov@111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B0B�B�B0BBB�BxB
�B
�B
XB
=B
	B	�B	�B
	B	7B	�B	�B	�B
	B
=B
�B
�BBDBB�B�B.BB�B�BB�B=B�BjB'�BcnBk�B	2�B	}B	�B	�dB	ӏB	ǮB	��B	z�B	u�B	ncB	]B	E�B	>B	5ZB	1AB	/�B	,�B	+B	+�B	,�B	-)B	,�B	0B	.�B	8�B	7�B	@iB	A�B	=VB	EmB	O�B	W�B	gB	oOB	t�B	u�B	��B	��B	�+B	�@B	�0B	��B	ňB	�HB	�B	�$B	�nB	��B
�B
)B
$�B
-]B
7�B
7�B
7�B
9�B
:xB
;�B
P.B
SuB
U�B
\�B
]/B
]B
\�B
_�B
`vB
abB
`vB
^�B
^jB
]�B
]/B
\�B
[�B
[�B
\CB
]�B
]IB
a-B
a�B
]/B
[WB
YeB
X�B
XEB
X�B
W�B
VB
S�B
S@B
Q�B
PbB
OB
MjB
MPB
L�B
L0B
LdB
L�B
KB
H�B
F�B
A�B
CGB
B�B
>B
:DB
6�B
3hB
/OB
-�B
'mB
#B
 vB
�B
�B
�B
�B
�B
�B
	lB
B
�B	�.B	��B	��B	�9B	�B	��B	��B	��B	�>B	�B	�:B	�-B	�|B	�|B	��B	�vB	ߊB	��B	�vB	�B	�B	߾B	��B	�B	��B	��B	��B	��B	�B	�B	�$B	�XB	�=B	��B	�B	��B	�_B	�yB	�XB	�B	�`B	�B	�B	�%B	�`B	�B	��B	�2B	�B	�tB	�nB	��B	�B	�4B	�B	�B	�4B	�NB	�B	��B	�zB	��B	�B	�B	��B	�=B	�_B	�B	�FB	�,B	�B	�B	�2B	�B	�B	�B	�>B	�mB	��B	�B	��B	�kB	�]B	�6B	�$B	�$B	�*B	�B	�B	�B	��B	��B	�B	��B	��B	��B	��B
 4B
 �B
B
oB
uB
'B
AB
AB
�B	��B	��B	�"B	�.B	��B	��B	��B	��B	�B
 iB
 B
  B
 �B	�B	�]B	�(B	�B	��B	�<B
 �B	�}B	��B	��B	�cB	��B	��B	��B	��B
 iB
�B

rB

�B
�B

�B
B

�B

�B

�B

�B

�B
B

XB
	�B

	B

#B

�B
B
)B
xB
^B
^B
DB

�B

#B

rB
	�B
	�B
	7B
	�B
�B
�B
EB
B
?B
�B
�B
B
�B
[B
�B
�B
�B
�B
�B
�B
�B
�B
3B
�B
MB
�B
SB
�B
+B
_B
�B
�B

	B
)B
�B
�B
B
B
jB
�B
�B
�B
�B
�B
B
�B
B
�B
�B
}B
}B
bB
bB
�B
vB
pB
�B
�B
�B
pB
B
dB
0B
�B
�B
~B
�B
"B
pB
�B
B
�B
 B
�B
hB
NB
�B
�B
B
4B
�B
BB
�B
�B
�B
�B
�B
�B
�B
B
vB
\B
BB
vB
BB
�B
B
:B
TB
[B
uB
�B
�B
�B
�B
,B
�B
{B
{B
�B
MB
�B
mB

B
�B
$B

B
�B
�B
�B
�B
�B
B
EB
+B
_B
+B
�B
�B
�B
�B
�B
�B
xB
�B
~B
�B
�B
 'B
 �B
!�B
"�B
"�B
# B
#B
#nB
#�B
$B
$&B
$�B
$ZB
$�B
%,B
%zB
%`B
%`B
%,B
%FB
%FB
%,B
%�B
%`B
%B
$�B
$�B
$�B
$�B
%B
%�B
&fB
'�B
(>B
+B
,qB
,�B
-CB
-�B
-�B
-�B
-�B
-�B
-�B
.B
.�B
/ B
/�B
0B
0UB
0;B
0oB
0�B
1[B
1�B
1�B
2B
2B
2aB
2�B
2�B
2�B
2�B
3hB
3hB
3�B
3�B
3hB
3�B
4B
49B
4TB
4�B
4�B
4�B
4�B
4�B
4�B
4�B
5B
5ZB
5�B
5�B
6B
5�B
6�B
6�B
6�B
6�B
7fB
6�B
7�B
7�B
8lB
8�B
8�B
9XB
9�B
:B
:B
:B
:^B
:�B
:�B
:�B
:�B
:�B
;JB
<B
;�B
<6B
<B
<PB
<�B
<�B
<�B
<�B
=qB
=�B
=�B
=�B
>(B
>(B
>�B
?.B
?HB
@ B
?�B
?�B
?�B
?�B
?�B
?�B
@�B
@�B
AB
AoB
BB
B�B
B�B
CGB
CB
CaB
C�B
CaB
DB
DB
DgB
DB
C{B
DB
C�B
C�B
C�B
C�B
DgB
D�B
D�B
DgB
D�B
EB
EB
EB
D�B
E9B
E�B
E�B
E�B
FtB
F�B
FtB
F�B
F�B
G+B
G�B
G+B
GzB
G_B
G�B
G�B
G�B
H1B
HB
G�B
H�B
IB
H�B
I�B
I�B
J#B
J�B
J�B
J�B
J�B
K�B
J�B
K�B
KDB
K�B
J�B
J�B
L0B
LB
L�B
MB
M6B
M�B
M�B
MPB
M�B
N<B
O�B
P}B
P�B
P�B
QB
QB
Q�B
Q�B
Q�B
QNB
Q�B
R B
RB
R�B
R�B
S&B
S�B
S@B
S�B
S[B
S�B
TFB
S�B
S�B
TB
T{B
TaB
UB
U�B
U�B
U�B
U�B
VSB
VSB
V�B
W?B
W�B
W�B
W�B
W�B
XB
X+B
XEB
X_B
XEB
X_B
X_B
YeB
YKB
ZB
Y�B
Z7B
ZQB
ZQB
[WB
[	B
[�B
[�B
\CB
\)B
]/B
]IB
]B
]IB
^B
^5B
^�B
^OB
^5B
]�B
^5B
^�B
^�B
^�B
^�B
^�B
^�B
_;B
_VB
^�B
^�B
_!B
^�B
_B
`'B
`�B
`�B
`�B
`vB
`�B
`�B
a�B
a�B
bNB
bB
b4B
bB
bNB
b�B
b�B
c B
c:B
cnB
c�B
c�B
c�B
dB
c�B
dB
d�B
d�B
d�B
d�B
d�B
eFB
eFB
e`B
ezB
ezB
e�B
ezB
e�B
e`B
e�B
e�B
e�B
fLB
f�B
f�B
f�B
gB
gB
gB
gRB
gRB
g�B
g�B
g�B
g�B
g�B
g�B
h
B
hXB
h$B
h�B
h�B
i_B
iB
i�B
iyB
i�B
j0B
j�B
j�B
j�B
kQB
j�B
k6B
k�B
kkB
k�B
lB
lB
lWB
lqB
l�B
l�B
m�B
m�B
n/B
n�B
nIB
ncB
o B
n�B
n�B
o5B
oOB
oOB
o�B
o�B
o�B
pUB
p�B
poB
qB
poB
p�B
qB
rB
r-B
q[B
raB
rB
raB
r�B
r�B
r�B
sMB
sMB
shB
s�B
sMB
s�B
s�B
s�B
s�B
tB
tB
t�B
uZB
uB
utB
u�B
u�B
u�B
u�B
u�B
vB
u�B
v+B
u�B
v+B
v+B
v+B
vFB
v`B
v�B
wLB
w�B
w�B
w�B
x�B
x�B
x�B
y>B
y$B
yXB
y�B
y�B
yXB
zDB
y�B
y�B
z�B
zxB
zB
z^B
z�B
zxB
{dB
z�B
{JB
{JB
{�B
{�B
|6B
|�B
|6B
|jB
|B
|jB
|6B
|�B
|�B
}B
}"B
}"B
}<B
}�B
}�B
}�B
~B
~]B
~�B
~�B
~�B
B
�B
� B
� B
�B
�B
�B
cB
� B
�4B
�OB
�OB
�iB
��B
�B
��B
��B
��B
��B
�'B
�'B
�[B
�AB
��B
��B
�-B
��B
��B
�GB
��B
��B
�{B
��B
��B
��B
��B
�MB
��B
�B
�9B
�9B
��B
��B
�mB
�?B
�%B
�?B
�YB
�?B
�tB
�tB
��B
��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B0B�B�B0BBB�BxB
�B
�B
XB
=B
	B	�B	�B
	B	7B	�B	�B	�B
	B
=B
�B
�BBDBB�B�B.BB�B�BB�B=B�BjB'�BcnBk�B	2�B	}B	�B	�dB	ӏB	ǮB	��B	z�B	u�B	ncB	]B	E�B	>B	5ZB	1AB	/�B	,�B	+B	+�B	,�B	-)B	,�B	0B	.�B	8�B	7�B	@iB	A�B	=VB	EmB	O�B	W�B	gB	oOB	t�B	u�B	��B	��B	�+B	�@B	�0B	��B	ňB	�HB	�B	�$B	�nB	��B
�B
)B
$�B
-]B
7�B
7�B
7�B
9�B
:xB
;�B
P.B
SuB
U�B
\�B
]/B
]B
\�B
_�B
`vB
abB
`vB
^�B
^jB
]�B
]/B
\�B
[�B
[�B
\CB
]�B
]IB
a-B
a�B
]/B
[WB
YeB
X�B
XEB
X�B
W�B
VB
S�B
S@B
Q�B
PbB
OB
MjB
MPB
L�B
L0B
LdB
L�B
KB
H�B
F�B
A�B
CGB
B�B
>B
:DB
6�B
3hB
/OB
-�B
'mB
#B
 vB
�B
�B
�B
�B
�B
�B
	lB
B
�B	�.B	��B	��B	�9B	�B	��B	��B	��B	�>B	�B	�:B	�-B	�|B	�|B	��B	�vB	ߊB	��B	�vB	�B	�B	߾B	��B	�B	��B	��B	��B	��B	�B	�B	�$B	�XB	�=B	��B	�B	��B	�_B	�yB	�XB	�B	�`B	�B	�B	�%B	�`B	�B	��B	�2B	�B	�tB	�nB	��B	�B	�4B	�B	�B	�4B	�NB	�B	��B	�zB	��B	�B	�B	��B	�=B	�_B	�B	�FB	�,B	�B	�B	�2B	�B	�B	�B	�>B	�mB	��B	�B	��B	�kB	�]B	�6B	�$B	�$B	�*B	�B	�B	�B	��B	��B	�B	��B	��B	��B	��B
 4B
 �B
B
oB
uB
'B
AB
AB
�B	��B	��B	�"B	�.B	��B	��B	��B	��B	�B
 iB
 B
  B
 �B	�B	�]B	�(B	�B	��B	�<B
 �B	�}B	��B	��B	�cB	��B	��B	��B	��B
 iB
�B

rB

�B
�B

�B
B

�B

�B

�B

�B

�B
B

XB
	�B

	B

#B

�B
B
)B
xB
^B
^B
DB

�B

#B

rB
	�B
	�B
	7B
	�B
�B
�B
EB
B
?B
�B
�B
B
�B
[B
�B
�B
�B
�B
�B
�B
�B
�B
3B
�B
MB
�B
SB
�B
+B
_B
�B
�B

	B
)B
�B
�B
B
B
jB
�B
�B
�B
�B
�B
B
�B
B
�B
�B
}B
}B
bB
bB
�B
vB
pB
�B
�B
�B
pB
B
dB
0B
�B
�B
~B
�B
"B
pB
�B
B
�B
 B
�B
hB
NB
�B
�B
B
4B
�B
BB
�B
�B
�B
�B
�B
�B
�B
B
vB
\B
BB
vB
BB
�B
B
:B
TB
[B
uB
�B
�B
�B
�B
,B
�B
{B
{B
�B
MB
�B
mB

B
�B
$B

B
�B
�B
�B
�B
�B
B
EB
+B
_B
+B
�B
�B
�B
�B
�B
�B
xB
�B
~B
�B
�B
 'B
 �B
!�B
"�B
"�B
# B
#B
#nB
#�B
$B
$&B
$�B
$ZB
$�B
%,B
%zB
%`B
%`B
%,B
%FB
%FB
%,B
%�B
%`B
%B
$�B
$�B
$�B
$�B
%B
%�B
&fB
'�B
(>B
+B
,qB
,�B
-CB
-�B
-�B
-�B
-�B
-�B
-�B
.B
.�B
/ B
/�B
0B
0UB
0;B
0oB
0�B
1[B
1�B
1�B
2B
2B
2aB
2�B
2�B
2�B
2�B
3hB
3hB
3�B
3�B
3hB
3�B
4B
49B
4TB
4�B
4�B
4�B
4�B
4�B
4�B
4�B
5B
5ZB
5�B
5�B
6B
5�B
6�B
6�B
6�B
6�B
7fB
6�B
7�B
7�B
8lB
8�B
8�B
9XB
9�B
:B
:B
:B
:^B
:�B
:�B
:�B
:�B
:�B
;JB
<B
;�B
<6B
<B
<PB
<�B
<�B
<�B
<�B
=qB
=�B
=�B
=�B
>(B
>(B
>�B
?.B
?HB
@ B
?�B
?�B
?�B
?�B
?�B
?�B
@�B
@�B
AB
AoB
BB
B�B
B�B
CGB
CB
CaB
C�B
CaB
DB
DB
DgB
DB
C{B
DB
C�B
C�B
C�B
C�B
DgB
D�B
D�B
DgB
D�B
EB
EB
EB
D�B
E9B
E�B
E�B
E�B
FtB
F�B
FtB
F�B
F�B
G+B
G�B
G+B
GzB
G_B
G�B
G�B
G�B
H1B
HB
G�B
H�B
IB
H�B
I�B
I�B
J#B
J�B
J�B
J�B
J�B
K�B
J�B
K�B
KDB
K�B
J�B
J�B
L0B
LB
L�B
MB
M6B
M�B
M�B
MPB
M�B
N<B
O�B
P}B
P�B
P�B
QB
QB
Q�B
Q�B
Q�B
QNB
Q�B
R B
RB
R�B
R�B
S&B
S�B
S@B
S�B
S[B
S�B
TFB
S�B
S�B
TB
T{B
TaB
UB
U�B
U�B
U�B
U�B
VSB
VSB
V�B
W?B
W�B
W�B
W�B
W�B
XB
X+B
XEB
X_B
XEB
X_B
X_B
YeB
YKB
ZB
Y�B
Z7B
ZQB
ZQB
[WB
[	B
[�B
[�B
\CB
\)B
]/B
]IB
]B
]IB
^B
^5B
^�B
^OB
^5B
]�B
^5B
^�B
^�B
^�B
^�B
^�B
^�B
_;B
_VB
^�B
^�B
_!B
^�B
_B
`'B
`�B
`�B
`�B
`vB
`�B
`�B
a�B
a�B
bNB
bB
b4B
bB
bNB
b�B
b�B
c B
c:B
cnB
c�B
c�B
c�B
dB
c�B
dB
d�B
d�B
d�B
d�B
d�B
eFB
eFB
e`B
ezB
ezB
e�B
ezB
e�B
e`B
e�B
e�B
e�B
fLB
f�B
f�B
f�B
gB
gB
gB
gRB
gRB
g�B
g�B
g�B
g�B
g�B
g�B
h
B
hXB
h$B
h�B
h�B
i_B
iB
i�B
iyB
i�B
j0B
j�B
j�B
j�B
kQB
j�B
k6B
k�B
kkB
k�B
lB
lB
lWB
lqB
l�B
l�B
m�B
m�B
n/B
n�B
nIB
ncB
o B
n�B
n�B
o5B
oOB
oOB
o�B
o�B
o�B
pUB
p�B
poB
qB
poB
p�B
qB
rB
r-B
q[B
raB
rB
raB
r�B
r�B
r�B
sMB
sMB
shB
s�B
sMB
s�B
s�B
s�B
s�B
tB
tB
t�B
uZB
uB
utB
u�B
u�B
u�B
u�B
u�B
vB
u�B
v+B
u�B
v+B
v+B
v+B
vFB
v`B
v�B
wLB
w�B
w�B
w�B
x�B
x�B
x�B
y>B
y$B
yXB
y�B
y�B
yXB
zDB
y�B
y�B
z�B
zxB
zB
z^B
z�B
zxB
{dB
z�B
{JB
{JB
{�B
{�B
|6B
|�B
|6B
|jB
|B
|jB
|6B
|�B
|�B
}B
}"B
}"B
}<B
}�B
}�B
}�B
~B
~]B
~�B
~�B
~�B
B
�B
� B
� B
�B
�B
�B
cB
� B
�4B
�OB
�OB
�iB
��B
�B
��B
��B
��B
��B
�'B
�'B
�[B
�AB
��B
��B
�-B
��B
��B
�GB
��B
��B
�{B
��B
��B
��B
��B
�MB
��B
�B
�9B
�9B
��B
��B
�mB
�?B
�%B
�?B
�YB
�?B
�tB
�tB
��B
��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604104913  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604173519  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604173520  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604173520                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605023528  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605023528  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610131507                      G�O�G�O�G�O�                