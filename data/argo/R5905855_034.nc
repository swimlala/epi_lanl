CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T19:16:36Z creation;2022-06-04T19:16:36Z conversion to V3.1      
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
_FillValue                    �tArgo profile    3.1 1.2 19500101000000  20220604191636  20220610151508  5905855                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               "A   JA                                  2B  A   APEX                            8423                            2.11.2                          846 @���Vٲ1   @���'@�t@02n��O��cҟ�vȴ1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @��@y��@�  A   AffA>ffA`  A�  A�  A�  A�  A�  A���A�  A�  B   B  B  B  B   B(  B/��B8  B@  BH  BPffBXffB`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�33B�  B�  B���B���B�33B���B�ffB�  B�  B�  B�  B�ffB�  B�ffB�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B���C   C  C  C  C  C
  C  C  C  C  C  C  C�C  C  C  C   C"  C#�fC&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ�C\�C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'�fD(fD(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT�fDUfDU�fDV  DV� DW  DWy�DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh�fDi  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�3D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�i�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @fg@vfg@�ff@�ffA��A=��A_33A33A���A���A���A���A�fgAߙ�AA���B��B��B��B��B'��B/fgB7��B?��BG��BP33BX33B_��Bg��Bo��Bw��B��B��fB��fB��fB��fB��B��fB��fB��3B��3B��B��3B�L�B��fB��fB��fB��fB�L�B��fB�L�B��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��3B��3B��fC�3C�3C�3C�3C	�3C�3C�3C�3C�3C�3C�3C�C�3C�3C�3C�3C!�3C#ٙC%�3C'�3C)�3C+�3C-�3C/�3C1�3C3�3C5�3C7�3C9�3C;�3C=�3C?�3CA�3CC�3CE�3CG�3CI�3CK�3CM�3CO�3CQ�3CS�3CU�3CW�3CZ�C\�C]�3C_�3Ca�3Cc�3Ce�3Cg�3Ci�3Ck�3Cm�3Co�3Cq�3Cs�3Cu�3Cw�3Cy�3C{�3C}�3C�3C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C�gC�gC�gC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C�gC�gC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D |�D ��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D	|�D	��D
|�D
��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D |�D ��D!|�D!��D"|�D"��D#|�D#��D$|�D$��D%|�D%��D&|�D&��D'�3D(3D(|�D(��D)|�D)��D*|�D*��D+|�D+��D,|�D,��D-|�D-��D.|�D.��D/|�D/��D0|�D0��D1|�D1��D2|�D2��D3|�D3��D4|�D4��D5|�D5��D6|�D6��D7|�D7��D8|�D8��D9|�D9��D:|�D:��D;|�D;��D<|�D<��D=|�D=��D>|�D>��D?|�D?��D@|�D@��DA|�DA��DB|�DB��DC|�DC��DD|�DD��DE|�DE��DF|�DF��DG|�DG��DH|�DH��DI|�DI��DJ|�DJ��DK|�DK��DL|�DL��DM|�DM��DN|�DN��DO|�DO��DP|�DP��DQ|�DQ��DR|�DR��DS|�DS��DT�3DU3DU�3DU��DV|�DV��DWvgDW��DX|�DX��DY|�DY��DZ|�DZ��D[|�D[��D\|�D\��D]|�D]��D^|�D^��D_|�D_��D`|�D`��Da|�Da��Db|�Db��Dc|�Dc��Dd|�Dd��De|�De��Df|�Df��Dg|�Dg��Dh�3Dh��Di|�Di��Dj|�Dj��Dk|�Dk��Dl|�Dl��Dm|�Dm��Dn|�Dn��Do|�Do��Dp|�Dp��Dq|�Dq��Dr|�Dr��Ds|�Ds��Dt|�Dt��Du|�Du��Dv|�Dv��Dw|�Dw��Dx|�Dx��Dy|�Dy��Dz|�Dz��D{|�D{��D||�D|��D}|�D}��D~|�D~��D|�D��D�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD���D��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD¾fD��fD�>fD�~fDþfD��fD�>fD�~fDľfD��fD�>fD�~fDžfD��D�>fD�~fDƾfD��fD�>fD�~fDǾfD��fD�>fD�~fDȾfD��fD�>fD�~fDɾfD��fD�>fD�~fDʾfD��fD�>fD�~fD˾fD��fD�>fD�~fD̾fD��fD�>fD�~fD;fD��fD�>fD�~fDξfD��fD�>fD�~fDϾfD��fD�>fD�~fDоfD��fD�>fD�~fDѾfD��fD�>fD�~fDҾfD��fD�>fD�~fDӾfD��fD�>fD�~fDԾfD��fD�>fD�~fDվfD��fD�>fD�~fD־fD��fD�>fD�~fD׾fD��fD�>fD�~fDؾfD��fD�>fD�~fDپfD��fD�>fD�~fDھfD��fD�>fD�~fD۾fD��fD�>fD�~fDܾfD��fD�>fD�~fDݾfD��fD�>fD�~fD޾fD��fD�>fD�~fD߾fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD��fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�h 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�уA��2A��]A���A��dA��A��;A��vA��|A��A�� A��A��A���A��fA��A��A���A��A��A��A��A���A���A��A��oA��vA���A���A���A��2A��fA��	A��`A���A��8A��>A��>A��fA���A�E�A�D3A�&�A�cA��=A�e,A�˒A�K^A�]�A��A�jA�F�A�HA�k�A�>wA��A���A���A��PA�+�A�FtA�u%A�/�A��1A�f�A��gA��DA��OA���A��jA�Q�A�aA}%�AyB�Aw�AvAs��Al�FAjp�Ai�"Ah[WAf$�A`��A[��ASiDAM@�AIAF�<ADjA@�DA;ߤA:S�A9�HA8Q�A4��A3!�A1�EA1<�A/�yA.�1A,��A,!-A+B[A*�!A'9XA$��A$-A#b�A"��A"DgA"�A!�$A ��A�A\)AZ�A�DAoA/�AJ#A��A�	A?A�mAخA�Av�AO�A33A�dA`BA.�A�BA�AVA8�A��AѷA�<A�A(�AɆA}�A�TAC�A��A�jA��APHA�0A��AS�A��A�A��A�ADgA	$A�hA�XA�~A-A�"A�RA�A�)A��A�/A �+A �A �@��N@��@��m@���@��|@��E@�x�@��y@�R�@�N�@���@�5?@�-�@��@��]@�g8@���@��j@�@�@�n@�S@�ff@�J@��;@�u@��@�}�@�͟@�Z@�	@�c@�ی@���@��*@��	@�p;@�y�@�@�L0@���@�m]@�33@�@�
=@��@��@��@�]�@��	@�j@曦@�҉@�Vm@�@�T�@㴢@�g8@��@���@�~�@�oi@�z@��@᫟@��@�^�@��P@�C�@�_@�"�@�'R@�6z@ک�@ً�@���@���@�L0@�p�@��@�҉@�|�@��@�Ɇ@�Ft@���@��o@���@Ӻ^@ӓ�@���@ѫ�@�@O@���@�Xy@ϼ�@�&@���@Ίr@�_@���@�IR@��@̀�@�@�A @�bN@�Q�@�2�@��#@ɋ�@�^�@�)_@ȧ�@���@�;@�m�@� �@���@ż@ōP@�S&@��c@�a|@�B[@�#:@���@�hs@�	l@�@��@��@���@�q�@�{�@��@���@�33@��@���@�Ov@���@�]�@�7L@�/�@�+@�kQ@��@���@�ȴ@�e@��q@���@��@�͟@�?�@���@���@�C@�ȴ@��@�6@�8�@���@��#@��:@�=�@��@�~�@�4n@��;@�x�@�=�@��@�?�@��@���@�@���@�_p@��@���@���@���@�"h@�iD@�RT@�8�@��x@�9X@��@���@�P�@���@���@�n�@�.�@���@�G�@�҉@���@�~@��'@�=�@�V@���@���@�~@�qv@�=�@�/�@��@���@��]@�� @���@���@�b�@�8@���@���@���@���@�y�@�F@��@���@���@�p;@�!@��@�0�@��?@�v�@�E�@�%�@�1@���@��7@�S�@�C�@�(@���@�"h@�_@���@���@�w2@�9�@�ѷ@�xl@��@���@��$@� \@���@�͟@��F@�p;@�?�@��j@��@���@���@�qv@�X�@�9�@��H@��@�oi@�_�@��@��@���@��X@�%F@��@��@�v�@��@��-@�A�@�5�@���@��_@�_@��@��r@��;@��@���@��@@�c@�a�@�6z@��@���@���@�$�@��Q@���@��	@��@��@���@���@��o@�7@���@���@�t�@�/�@��@���@��@���@�:*@���@�\)@��@��h@�kQ@�{@��A@�Z�@�C@���@���@�a|@�*�@�!�@�1@��t@�Z�@�ѷ@���@�C�@���@��@�_p@�@���@�C�@���@�m]@�e�@�P�@�/@�%@��@��@��s@���@���@��@��)@��v@��K@���@��"@��8@��b@�[�@�8�@�(�@�x@��n@�F�@�;d@��@��"@���@��'@��}@���@�~(@�\�@�$�@�@��@��@a@~�}@~_@}s�@}(�@}�@}+@|��@|y>@{t�@z��@zv�@z6�@z�@y�@x�|@w�w@wo�@wS�@wO@wJ#@v�H@v�!@v�@u�@u@@t�4@tU2@s��@se�@s@r�@r��@r��@rs�@r?@q��@q8�@p��@o��@og�@o�@n��@n�R@n�r@n8�@m�@m�~@m<6@mV@l�@l6@k�a@ky�@kK�@j�@j�F@j�@i��@iw2@i-w@h�/@h7@gj�@f��@f��@fq�@e��@ee,@e@@d�E@d��@dM@dx@c��@b��@a�=@a@`�[@`�@`K^@`�@_�@_F�@_
=@^�r@^�@]�@]�=@]k�@]q@\�@[�a@[!-@ZH�@Y��@Yp�@Y^�@X��@W�[@Wj�@W�@V�y@V͟@Vq�@V$�@U�@UN<@U�@T�@T�$@T_@S��@S��@SW?@S�@R͟@ROv@R	@Ru@Q�3@Qc�@Q7L@P�@Pz�@P,=@O��@O@Nu%@N3�@Nu@M��@Mhs@M-w@L��@K�@Kj�@K�@KY@K�@J�]@J��@JOv@J8�@I�@I[W@H��@H��@H��@H:�@G�+@GdZ@F��@F��@F	@E��@E��@E+�@E�@D�P@D��@D�/@D��@D��@C��@Cn/@C�@Bߤ@B��@B��@BV@B.�@A��@A�'@AY�@A7L@A%@@��@@��@@U2@@M@?��@>��@>��@>}V@>ff@>&�@=�)@=�@<�@<bN@<K^@</�@;��@;�@@;�4@;9�@;�@:��@:�@:�<@9��@9��@9��@9f�@9@@8��@8>B@7��@7��@7�	@7�@6�h@6@5�@5�M@5O�@4�E@4Z@3�@3�V@3�{@3]�@3S@2��@2_�@2�@1�>@1�M@1o @1f�@1Dg@1;@0�/@0�O@0H@/خ@/�P@/s@/F�@.͟@.��@.s�@.M�@.3�@-�@-��@-!�@,�)@,��@,��@,��@,<�@+�@+��@+iD@+Y@*�}@*�+@*u%@*:*@*u@)��@)��@)w2@)�@(��@(��@(��@(��@(2�@'��@'��@'��@'K�@'F�@'9�@'�@&��@&�+@%��@%p�@%J�@% \@%�@$�f@$�5@$�@$Ĝ@$<�@$b@$�@#�;@#ƨ@#��@#��@#C�@"�@"��@"xl@"E�@!�#@!7L@ Ɇ@ �@ ��@ ~(@ g8@ -�@ �@�Q@_p@�@�y@��@��@��@i�@�@�@�t@�M@�@�e@`�@6@x@1@�@�m@��@qv@�@Y@�'@��@J�@u@�@��@^�@(�@��@��@�@A�@�@��@]�@)_@
=@�@ȴ@�r@GE@5?@($@	@��@T�@/@�@��@��@U2@>B@2�@ �@�@�@�w@��@o�@J#@
=@@ i@�s@��@W�@B[@-@�@�d@��@G�@@@�v@�@��@`�@Q�@C-@7�@(�@1@��@�
@��@j�@�@��@��@��@l�@;�@+k@O@�@�@�D@�@��@��@�S@x�@L�@4@�@��@�)@�@h�@Ft@A�@7�@b@�W@� @��@�q@��@o�@\)@P�@Mj@$t@(@
��@
�2@
�,@
�}@
��@
s�@
5?@

�@	��@	�3@	��@	�=@	|@	X@	L�@	 \1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�уA��2A��]A���A��dA��A��;A��vA��|A��A�� A��A��A���A��fA��A��A���A��A��A��A��A���A���A��A��oA��vA���A���A���A��2A��fA��	A��`A���A��8A��>A��>A��fA���A�E�A�D3A�&�A�cA��=A�e,A�˒A�K^A�]�A��A�jA�F�A�HA�k�A�>wA��A���A���A��PA�+�A�FtA�u%A�/�A��1A�f�A��gA��DA��OA���A��jA�Q�A�aA}%�AyB�Aw�AvAs��Al�FAjp�Ai�"Ah[WAf$�A`��A[��ASiDAM@�AIAF�<ADjA@�DA;ߤA:S�A9�HA8Q�A4��A3!�A1�EA1<�A/�yA.�1A,��A,!-A+B[A*�!A'9XA$��A$-A#b�A"��A"DgA"�A!�$A ��A�A\)AZ�A�DAoA/�AJ#A��A�	A?A�mAخA�Av�AO�A33A�dA`BA.�A�BA�AVA8�A��AѷA�<A�A(�AɆA}�A�TAC�A��A�jA��APHA�0A��AS�A��A�A��A�ADgA	$A�hA�XA�~A-A�"A�RA�A�)A��A�/A �+A �A �@��N@��@��m@���@��|@��E@�x�@��y@�R�@�N�@���@�5?@�-�@��@��]@�g8@���@��j@�@�@�n@�S@�ff@�J@��;@�u@��@�}�@�͟@�Z@�	@�c@�ی@���@��*@��	@�p;@�y�@�@�L0@���@�m]@�33@�@�
=@��@��@��@�]�@��	@�j@曦@�҉@�Vm@�@�T�@㴢@�g8@��@���@�~�@�oi@�z@��@᫟@��@�^�@��P@�C�@�_@�"�@�'R@�6z@ک�@ً�@���@���@�L0@�p�@��@�҉@�|�@��@�Ɇ@�Ft@���@��o@���@Ӻ^@ӓ�@���@ѫ�@�@O@���@�Xy@ϼ�@�&@���@Ίr@�_@���@�IR@��@̀�@�@�A @�bN@�Q�@�2�@��#@ɋ�@�^�@�)_@ȧ�@���@�;@�m�@� �@���@ż@ōP@�S&@��c@�a|@�B[@�#:@���@�hs@�	l@�@��@��@���@�q�@�{�@��@���@�33@��@���@�Ov@���@�]�@�7L@�/�@�+@�kQ@��@���@�ȴ@�e@��q@���@��@�͟@�?�@���@���@�C@�ȴ@��@�6@�8�@���@��#@��:@�=�@��@�~�@�4n@��;@�x�@�=�@��@�?�@��@���@�@���@�_p@��@���@���@���@�"h@�iD@�RT@�8�@��x@�9X@��@���@�P�@���@���@�n�@�.�@���@�G�@�҉@���@�~@��'@�=�@�V@���@���@�~@�qv@�=�@�/�@��@���@��]@�� @���@���@�b�@�8@���@���@���@���@�y�@�F@��@���@���@�p;@�!@��@�0�@��?@�v�@�E�@�%�@�1@���@��7@�S�@�C�@�(@���@�"h@�_@���@���@�w2@�9�@�ѷ@�xl@��@���@��$@� \@���@�͟@��F@�p;@�?�@��j@��@���@���@�qv@�X�@�9�@��H@��@�oi@�_�@��@��@���@��X@�%F@��@��@�v�@��@��-@�A�@�5�@���@��_@�_@��@��r@��;@��@���@��@@�c@�a�@�6z@��@���@���@�$�@��Q@���@��	@��@��@���@���@��o@�7@���@���@�t�@�/�@��@���@��@���@�:*@���@�\)@��@��h@�kQ@�{@��A@�Z�@�C@���@���@�a|@�*�@�!�@�1@��t@�Z�@�ѷ@���@�C�@���@��@�_p@�@���@�C�@���@�m]@�e�@�P�@�/@�%@��@��@��s@���@���@��@��)@��v@��K@���@��"@��8@��b@�[�@�8�@�(�@�x@��n@�F�@�;d@��@��"@���@��'@��}@���@�~(@�\�@�$�@�@��@��@a@~�}@~_@}s�@}(�@}�@}+@|��@|y>@{t�@z��@zv�@z6�@z�@y�@x�|@w�w@wo�@wS�@wO@wJ#@v�H@v�!@v�@u�@u@@t�4@tU2@s��@se�@s@r�@r��@r��@rs�@r?@q��@q8�@p��@o��@og�@o�@n��@n�R@n�r@n8�@m�@m�~@m<6@mV@l�@l6@k�a@ky�@kK�@j�@j�F@j�@i��@iw2@i-w@h�/@h7@gj�@f��@f��@fq�@e��@ee,@e@@d�E@d��@dM@dx@c��@b��@a�=@a@`�[@`�@`K^@`�@_�@_F�@_
=@^�r@^�@]�@]�=@]k�@]q@\�@[�a@[!-@ZH�@Y��@Yp�@Y^�@X��@W�[@Wj�@W�@V�y@V͟@Vq�@V$�@U�@UN<@U�@T�@T�$@T_@S��@S��@SW?@S�@R͟@ROv@R	@Ru@Q�3@Qc�@Q7L@P�@Pz�@P,=@O��@O@Nu%@N3�@Nu@M��@Mhs@M-w@L��@K�@Kj�@K�@KY@K�@J�]@J��@JOv@J8�@I�@I[W@H��@H��@H��@H:�@G�+@GdZ@F��@F��@F	@E��@E��@E+�@E�@D�P@D��@D�/@D��@D��@C��@Cn/@C�@Bߤ@B��@B��@BV@B.�@A��@A�'@AY�@A7L@A%@@��@@��@@U2@@M@?��@>��@>��@>}V@>ff@>&�@=�)@=�@<�@<bN@<K^@</�@;��@;�@@;�4@;9�@;�@:��@:�@:�<@9��@9��@9��@9f�@9@@8��@8>B@7��@7��@7�	@7�@6�h@6@5�@5�M@5O�@4�E@4Z@3�@3�V@3�{@3]�@3S@2��@2_�@2�@1�>@1�M@1o @1f�@1Dg@1;@0�/@0�O@0H@/خ@/�P@/s@/F�@.͟@.��@.s�@.M�@.3�@-�@-��@-!�@,�)@,��@,��@,��@,<�@+�@+��@+iD@+Y@*�}@*�+@*u%@*:*@*u@)��@)��@)w2@)�@(��@(��@(��@(��@(2�@'��@'��@'��@'K�@'F�@'9�@'�@&��@&�+@%��@%p�@%J�@% \@%�@$�f@$�5@$�@$Ĝ@$<�@$b@$�@#�;@#ƨ@#��@#��@#C�@"�@"��@"xl@"E�@!�#@!7L@ Ɇ@ �@ ��@ ~(@ g8@ -�@ �@�Q@_p@�@�y@��@��@��@i�@�@�@�t@�M@�@�e@`�@6@x@1@�@�m@��@qv@�@Y@�'@��@J�@u@�@��@^�@(�@��@��@�@A�@�@��@]�@)_@
=@�@ȴ@�r@GE@5?@($@	@��@T�@/@�@��@��@U2@>B@2�@ �@�@�@�w@��@o�@J#@
=@@ i@�s@��@W�@B[@-@�@�d@��@G�@@@�v@�@��@`�@Q�@C-@7�@(�@1@��@�
@��@j�@�@��@��@��@l�@;�@+k@O@�@�@�D@�@��@��@�S@x�@L�@4@�@��@�)@�@h�@Ft@A�@7�@b@�W@� @��@�q@��@o�@\)@P�@Mj@$t@(@
��@
�2@
�,@
�}@
��@
s�@
5?@

�@	��@	�3@	��@	�=@	|@	X@	L�@	 \1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B�B�B5BjBB�BB�BjB�B�B�B�B�B�B�B�B�BBBBB�B5BB5B5B5BOBjBjB5B5BOBjBjB�B�BB:�B^�B��B�#B	�MB	�B
c�B
n}B
�mB
��B
�XB
ɺB
�jB
��B
�;B
� B
��B
��B
�JB
��B
�/B
��B
�;B
o�B
a|B
EmB
?}B
8B
)�B
�B	�B	�^B	� B	�aB	�B	�?B	��B	�B	mB	d�B	a�B	]�B	TaB	="B	$@B	 B�B�FB�xB�B�HB�(B�B��B��BB�cB�.B�VB�.B�HB� B�OB��B��B�_BȴBȚBʌB��B�uB�BٚB��B��B��B�B��B	�B	#TB	>]B	E�B	N�B	S�B	Y�B	a|B	j�B	s�B	v�B	x�B	.B	��B	�RB	��B	�HB	� B	��B	��B	�MB	�yB	��B	�2B	�B	��B	�B	�<B	�jB	�B	��B	��B	��B	�B	�)B	�SB	�\B	��B	��B	��B	�0B	��B	��B	�#B	��B	~�B	�B	z�B	wLB	r�B	gmB	a�B	\�B	^5B	`�B	d@B	i�B	a�B	^�B	]�B	ZQB	O\B	N<B	Q�B	mwB	zB	~(B	}�B	}�B	y�B	z�B	zB	�B	.B	}VB	{JB	{�B	|�B	��B	��B	�1B	��B	��B	��B	�]B	�#B	�)B	��B	��B	�!B	�B	�DB	��B	�B	��B	�)B	��B	�B	�3B	��B	��B	��B	�`B	��B	�B	��B	��B	�6B	��B	�iB	��B	�3B	�B	��B	�zB	�`B	�B	�rB	�-B	ɆB	�~B	̘B	��B	�+B	�{B	��B	��B	�aB	�%B	ȴB	�<B	��B	�B	�(B	�BB	͹B	�PB	��B	�BB	��B	�.B	ЗB	� B	�hB	��B	�&B	�&B	ԕB	�9B	׍B	��B	��B	��B	׍B	��B	՛B	ԕB	�B	ӏB	��B	��B	ԕB	�{B	�B	�WB	��B	��B	�B	�/B	ݲB	�;B	ߊB	ߤB	߾B	ߤB	߾B	�B	�B	�vB	�\B	�BB	��B	�HB	�hB	��B	�B	޸B	�OB	�jB	ߊB	��B	�B	�B	��B	�ZB	�B	��B	�B	�B	�B	�_B	�B	�0B	�kB	��B	��B	��B	��B	��B	�B	�B	��B	�B	�aB	�|B	�GB	�|B	��B	�aB	�|B	�B	��B	�MB	��B	�ZB	�FB	�B	�B	�LB	��B	��B	�XB	��B	�>B	�>B	�>B	�$B	�$B	�XB	��B	��B	��B	�xB	��B	��B	�B	��B	�0B	��B	��B	��B	��B	�B	�qB	��B	�wB	��B	�cB	�}B	��B
  B	��B	�B	�cB	��B
 �B
�B
�B
�B
�B
�B
�B
�B
3B
�B
mB
9B
�B
9B
mB
�B
�B
SB
�B
%B
EB
1B
�B
�B
B
B
�B
	RB
�B
�B
�B
	7B

rB

=B

=B

#B

XB

�B
)B

�B
0B
�B
0B
�B
�B
�B
B
�B
�B
jB
�B
�B
�B
�B
�B
�B
B
vB
�B
�B
}B
}B
}B
bB
B
�B
�B
�B
[B
�B
FB
�B
�B
�B
B
�B
�B
B
SB
B
B
�B
�B
�B
�B
sB
�B
EB
_B
yB
�B
�B
KB
�B
B
B
�B
WB
�B
�B
�B
�B
B
�B
�B
�B
�B
~B
�B
�B
jB
OB
B
B
OB
�B
VB
pB
 B
!HB
!�B
!�B
"NB
#TB
#�B
#�B
$�B
$ZB
$B
$@B
$B
#�B
#TB
"�B
"�B
"�B
"�B
"�B
#TB
#nB
#�B
$�B
%FB
%�B
%�B
&�B
&�B
'B
'�B
'�B
(sB
(�B
)*B
)*B
)B
)�B
+�B
,�B
-�B
.IB
.IB
/5B
0B
0�B
1�B
2GB
2B
1�B
1B
0�B
0!B
/�B
/�B
/�B
0�B
1�B
4�B
5B
5%B
6+B
6+B
6�B
6�B
6�B
6�B
7�B
8�B
8�B
8�B
8�B
8lB
8�B
8lB
8�B
8�B
9$B
9	B
8�B
8lB
8�B
8RB
88B
8B
7�B
7�B
7�B
7�B
8�B
9rB
:�B
:�B
:*B
:�B
;�B
;�B
<B
<PB
<6B
<�B
<�B
<�B
="B
=qB
=�B
>B
>�B
>�B
?�B
@B
@ B
@�B
@�B
A;B
A�B
BAB
BB
BB
B[B
B�B
B�B
B�B
CB
CB
C-B
CaB
C�B
DMB
D�B
D�B
D�B
D�B
EB
E�B
E�B
E�B
E�B
E�B
EmB
E�B
E�B
F�B
F?B
FYB
FtB
F�B
F�B
F�B
F�B
G+B
G_B
GEB
G+B
G+B
GEB
G�B
H1B
HfB
HfB
H�B
IB
IB
IB
I7B
IRB
I�B
I�B
I�B
J#B
J=B
J#B
JrB
J�B
J�B
J�B
J�B
KB
K^B
K�B
LB
K�B
L0B
L�B
L�B
L�B
MB
M�B
M�B
M�B
M�B
NB
NB
NVB
NpB
NVB
NpB
N�B
N�B
OB
OBB
OvB
O�B
O�B
O�B
PHB
P}B
P�B
P�B
QNB
QhB
Q�B
Q�B
Q�B
Q�B
RB
RTB
RoB
R�B
R�B
SB
S&B
S@B
S[B
S�B
S�B
S�B
S�B
S�B
TaB
T{B
T�B
T�B
UgB
U�B
VB
VB
VB
VB
VB
W?B
WsB
W�B
W�B
W�B
W�B
XB
X+B
X_B
X_B
XyB
XEB
X_B
YKB
X�B
Y1B
X�B
Y1B
YeB
Y�B
Z7B
ZB
ZB
Z�B
Z�B
[�B
[�B
[�B
[�B
\]B
\�B
]B
]dB
]IB
]dB
]�B
]�B
]�B
^B
^B
^�B
^�B
^�B
^�B
^�B
_B
_pB
_�B
_�B
`BB
`\B
`�B
`�B
aB
aHB
a|B
a�B
a�B
b4B
bhB
b�B
b�B
b�B
b�B
cB
cTB
cnB
c�B
c�B
dB
d&B
d&B
dZB
dZB
dZB
dZB
d�B
eB
eFB
e`B
ezB
ezB
fB
fB
f2B
f2B
f�B
ffB
ffB
ffB
f�B
f�B
g�B
g�B
g�B
g�B
h
B
h
B
h$B
h
B
h$B
h�B
h�B
h�B
h�B
h�B
h�B
h�B
iDB
jKB
jB
j�B
kB
kkB
l=B
l=B
lqB
l�B
l�B
l�B
l�B
l�B
l�B
mwB
mCB
mB
mB
mCB
m)B
m)B
mwB
mwB
m�B
mwB
m�B
m�B
n/B
ncB
n}B
n}B
ncB
n}B
n�B
n�B
o5B
o B
oOB
oB
o�B
o�B
pB
p!B
p;B
pUB
p�B
p�B
p�B
q'B
qvB
q�B
q�B
rB
r-B
rGB
raB
r�B
r�B
r�B
r�B
r�B
s�B
s�B
s�B
s�B
tB
tTB
t�B
t�B
t�B
t�B
t�B
t�B
uB
u%B
u?B
uZB
u�B
u�B
u�B
u�B
vFB
v`B
vzB
vzB
vzB
v�B
wB
wLB
w�B
w�B
w�B
xB
x8B
x8B
x8B
x8B
xRB
xlB
xlB
x�B
x�B
x�B
yrB
y�B
y�B
y�B
y�B
z*B
zDB
zDB
z^B
zxB
zxB
zxB
z�B
z�B
z�B
z�B
{0B
{JB
{B
{�B
{�B
{�B
|B
|6B
|PB
|6B
|�B
|�B
|�B
|�B
|�B
}B
}<B
}qB
}VB
}VB
}�B
}�B
}�B
}�B
}�B
~B
~B
~B
~wB
~wB
~�B
~�B
~�B
~�B
.B
cB
HB
�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B�B�B5BjBB�BB�BjB�B�B�B�B�B�B�B�B�BBBBB�B5BB5B5B5BOBjBjB5B5BOBjBjB�B�BB:�B^�B��B�#B	�MB	�B
c�B
n}B
�mB
��B
�XB
ɺB
�jB
��B
�;B
� B
��B
��B
�JB
��B
�/B
��B
�;B
o�B
a|B
EmB
?}B
8B
)�B
�B	�B	�^B	� B	�aB	�B	�?B	��B	�B	mB	d�B	a�B	]�B	TaB	="B	$@B	 B�B�FB�xB�B�HB�(B�B��B��BB�cB�.B�VB�.B�HB� B�OB��B��B�_BȴBȚBʌB��B�uB�BٚB��B��B��B�B��B	�B	#TB	>]B	E�B	N�B	S�B	Y�B	a|B	j�B	s�B	v�B	x�B	.B	��B	�RB	��B	�HB	� B	��B	��B	�MB	�yB	��B	�2B	�B	��B	�B	�<B	�jB	�B	��B	��B	��B	�B	�)B	�SB	�\B	��B	��B	��B	�0B	��B	��B	�#B	��B	~�B	�B	z�B	wLB	r�B	gmB	a�B	\�B	^5B	`�B	d@B	i�B	a�B	^�B	]�B	ZQB	O\B	N<B	Q�B	mwB	zB	~(B	}�B	}�B	y�B	z�B	zB	�B	.B	}VB	{JB	{�B	|�B	��B	��B	�1B	��B	��B	��B	�]B	�#B	�)B	��B	��B	�!B	�B	�DB	��B	�B	��B	�)B	��B	�B	�3B	��B	��B	��B	�`B	��B	�B	��B	��B	�6B	��B	�iB	��B	�3B	�B	��B	�zB	�`B	�B	�rB	�-B	ɆB	�~B	̘B	��B	�+B	�{B	��B	��B	�aB	�%B	ȴB	�<B	��B	�B	�(B	�BB	͹B	�PB	��B	�BB	��B	�.B	ЗB	� B	�hB	��B	�&B	�&B	ԕB	�9B	׍B	��B	��B	��B	׍B	��B	՛B	ԕB	�B	ӏB	��B	��B	ԕB	�{B	�B	�WB	��B	��B	�B	�/B	ݲB	�;B	ߊB	ߤB	߾B	ߤB	߾B	�B	�B	�vB	�\B	�BB	��B	�HB	�hB	��B	�B	޸B	�OB	�jB	ߊB	��B	�B	�B	��B	�ZB	�B	��B	�B	�B	�B	�_B	�B	�0B	�kB	��B	��B	��B	��B	��B	�B	�B	��B	�B	�aB	�|B	�GB	�|B	��B	�aB	�|B	�B	��B	�MB	��B	�ZB	�FB	�B	�B	�LB	��B	��B	�XB	��B	�>B	�>B	�>B	�$B	�$B	�XB	��B	��B	��B	�xB	��B	��B	�B	��B	�0B	��B	��B	��B	��B	�B	�qB	��B	�wB	��B	�cB	�}B	��B
  B	��B	�B	�cB	��B
 �B
�B
�B
�B
�B
�B
�B
�B
3B
�B
mB
9B
�B
9B
mB
�B
�B
SB
�B
%B
EB
1B
�B
�B
B
B
�B
	RB
�B
�B
�B
	7B

rB

=B

=B

#B

XB

�B
)B

�B
0B
�B
0B
�B
�B
�B
B
�B
�B
jB
�B
�B
�B
�B
�B
�B
B
vB
�B
�B
}B
}B
}B
bB
B
�B
�B
�B
[B
�B
FB
�B
�B
�B
B
�B
�B
B
SB
B
B
�B
�B
�B
�B
sB
�B
EB
_B
yB
�B
�B
KB
�B
B
B
�B
WB
�B
�B
�B
�B
B
�B
�B
�B
�B
~B
�B
�B
jB
OB
B
B
OB
�B
VB
pB
 B
!HB
!�B
!�B
"NB
#TB
#�B
#�B
$�B
$ZB
$B
$@B
$B
#�B
#TB
"�B
"�B
"�B
"�B
"�B
#TB
#nB
#�B
$�B
%FB
%�B
%�B
&�B
&�B
'B
'�B
'�B
(sB
(�B
)*B
)*B
)B
)�B
+�B
,�B
-�B
.IB
.IB
/5B
0B
0�B
1�B
2GB
2B
1�B
1B
0�B
0!B
/�B
/�B
/�B
0�B
1�B
4�B
5B
5%B
6+B
6+B
6�B
6�B
6�B
6�B
7�B
8�B
8�B
8�B
8�B
8lB
8�B
8lB
8�B
8�B
9$B
9	B
8�B
8lB
8�B
8RB
88B
8B
7�B
7�B
7�B
7�B
8�B
9rB
:�B
:�B
:*B
:�B
;�B
;�B
<B
<PB
<6B
<�B
<�B
<�B
="B
=qB
=�B
>B
>�B
>�B
?�B
@B
@ B
@�B
@�B
A;B
A�B
BAB
BB
BB
B[B
B�B
B�B
B�B
CB
CB
C-B
CaB
C�B
DMB
D�B
D�B
D�B
D�B
EB
E�B
E�B
E�B
E�B
E�B
EmB
E�B
E�B
F�B
F?B
FYB
FtB
F�B
F�B
F�B
F�B
G+B
G_B
GEB
G+B
G+B
GEB
G�B
H1B
HfB
HfB
H�B
IB
IB
IB
I7B
IRB
I�B
I�B
I�B
J#B
J=B
J#B
JrB
J�B
J�B
J�B
J�B
KB
K^B
K�B
LB
K�B
L0B
L�B
L�B
L�B
MB
M�B
M�B
M�B
M�B
NB
NB
NVB
NpB
NVB
NpB
N�B
N�B
OB
OBB
OvB
O�B
O�B
O�B
PHB
P}B
P�B
P�B
QNB
QhB
Q�B
Q�B
Q�B
Q�B
RB
RTB
RoB
R�B
R�B
SB
S&B
S@B
S[B
S�B
S�B
S�B
S�B
S�B
TaB
T{B
T�B
T�B
UgB
U�B
VB
VB
VB
VB
VB
W?B
WsB
W�B
W�B
W�B
W�B
XB
X+B
X_B
X_B
XyB
XEB
X_B
YKB
X�B
Y1B
X�B
Y1B
YeB
Y�B
Z7B
ZB
ZB
Z�B
Z�B
[�B
[�B
[�B
[�B
\]B
\�B
]B
]dB
]IB
]dB
]�B
]�B
]�B
^B
^B
^�B
^�B
^�B
^�B
^�B
_B
_pB
_�B
_�B
`BB
`\B
`�B
`�B
aB
aHB
a|B
a�B
a�B
b4B
bhB
b�B
b�B
b�B
b�B
cB
cTB
cnB
c�B
c�B
dB
d&B
d&B
dZB
dZB
dZB
dZB
d�B
eB
eFB
e`B
ezB
ezB
fB
fB
f2B
f2B
f�B
ffB
ffB
ffB
f�B
f�B
g�B
g�B
g�B
g�B
h
B
h
B
h$B
h
B
h$B
h�B
h�B
h�B
h�B
h�B
h�B
h�B
iDB
jKB
jB
j�B
kB
kkB
l=B
l=B
lqB
l�B
l�B
l�B
l�B
l�B
l�B
mwB
mCB
mB
mB
mCB
m)B
m)B
mwB
mwB
m�B
mwB
m�B
m�B
n/B
ncB
n}B
n}B
ncB
n}B
n�B
n�B
o5B
o B
oOB
oB
o�B
o�B
pB
p!B
p;B
pUB
p�B
p�B
p�B
q'B
qvB
q�B
q�B
rB
r-B
rGB
raB
r�B
r�B
r�B
r�B
r�B
s�B
s�B
s�B
s�B
tB
tTB
t�B
t�B
t�B
t�B
t�B
t�B
uB
u%B
u?B
uZB
u�B
u�B
u�B
u�B
vFB
v`B
vzB
vzB
vzB
v�B
wB
wLB
w�B
w�B
w�B
xB
x8B
x8B
x8B
x8B
xRB
xlB
xlB
x�B
x�B
x�B
yrB
y�B
y�B
y�B
y�B
z*B
zDB
zDB
z^B
zxB
zxB
zxB
z�B
z�B
z�B
z�B
{0B
{JB
{B
{�B
{�B
{�B
|B
|6B
|PB
|6B
|�B
|�B
|�B
|�B
|�B
}B
}<B
}qB
}VB
}VB
}�B
}�B
}�B
}�B
}�B
~B
~B
~B
~wB
~wB
~�B
~�B
~�B
~�B
.B
cB
HB
�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604105233  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604191636  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604191636  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604191636                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605041643  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605041643  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610151508                      G�O�G�O�G�O�                