CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T17:39:18Z creation;2022-06-04T17:39:19Z conversion to V3.1      
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
_FillValue                    �tArgo profile    3.1 1.2 19500101000000  20220604173918  20220610131507  5905853                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               ]A   JA                                  2B  A   APEX                            8421                            2.11.2                          846 @ٍ�L
��1   @ٍ�W��$@-V�+J�cKt�j~�1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @@  @�  @�33A��A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@ffBHffBP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B���B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  Bܙ�B�  B���B���B�  B�  B�  B�  B�  C   C  C  C  C  C
  C�C�C  C  C  C  C�fC  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB�CD  CF�CH  CI�fCL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr�Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C��C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� DzfDz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�i�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @>�R@~�R@\AG�A�A?�A_�A�A��
A��
A��
A��
A��
A��
A��
A��
B�B�B�B�B'�B/�B7�B@Q�BHQ�BO�BW�B_�Bg�Bo�Bw�B�B���B���B���B���B�B���B��]B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B܏]B���B�B�B���B���B���B���B���B���C��C��C��C��C	��C{C{C��C��C��C��C�GC��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CB{CC��CF{CG��CI�GCK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cr{Cs��Cu��Cw��Cy��C{��C}��C��C��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC�
>C�
>C�
>C��qC�
>C�
>C��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qD ~�D ��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D	~�D	��D
~�D
��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D�D��D~�D��D~�D��D~�D��D ~�D ��D!~�D!��D"~�D"��D#~�D#��D$~�D$��D%~�D%��D&~�D&��D'~�D'��D(~�D(��D)~�D)��D*~�D*��D+~�D+��D,~�D,��D-~�D-��D.~�D.��D/~�D/��D0~�D0��D1~�D1��D2~�D2��D3~�D3��D4~�D4��D5~�D5��D6~�D6��D7~�D7��D8~�D8��D9~�D9��D:~�D:��D;~�D;��D<~�D<��D=~�D=��D>~�D>��D?~�D?��D@~�D@��DA~�DA��DB~�DB��DC~�DC��DD~�DD��DE~�DE��DF~�DF��DG~�DG��DH~�DH��DI~�DI��DJ~�DJ��DK~�DK��DL~�DL��DM~�DM��DN~�DN��DO~�DO��DP~�DP��DQ~�DQ��DR~�DR��DS~�DS��DT~�DT��DU~�DU��DV~�DV��DW~�DW��DX~�DX��DY~�DY��DZ~�DZ��D[~�D[��D\~�D\��D]~�D]��D^~�D^��D_~�D_��D`~�D`��Da~�Da��Db~�Db��Dc~�Dc��Dd~�Dd��De~�De��Df~�Df��Dg~�Dg��Dh~�Dh��Di~�Di��Dj~�Dj��Dk~�Dk��Dl~�Dl��Dm~�Dm��Dn~�Dn��Do~�Do��Dp~�Dp��Dq~�Dq��Dr~�Dr��Ds~�Ds��Dt~�Dt��Du~�Du��Dv~�Dv��Dw~�Dw��Dx~�Dx��Dy~�DzDz~�Dz��D{~�D{��D|~�D|��D}~�D}��D~~�D~��D~�D��D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D¿\D��\D�?\D�\Dÿ\D��\D�?\D�\DĿ\D��\D�?\D�\Dſ\D��\D�?\D�\Dƿ\D��\D�?\D�\Dǿ\D��\D�?\D�\Dȿ\D��\D�?\D�\Dɿ\D��\D�?\D�\Dʿ\D��\D�?\D�\D˿\D��\D�?\D�\D̿\D��\D�?\D�\DͿ\D��\D�?\D�\Dο\D��\D�?\D�\DϿ\D��\D�?\D�\Dп\D��\D�?\D�\Dѿ\D��\D�?\D�\Dҿ\D��\D�?\D�\Dӿ\D��\D�?\D�\DԿ\D��\D�?\D�\Dտ\D��\D�?\D�\Dֿ\D��\D�?\D�\D׿\D��\D�?\D�\Dؿ\D��\D�?\D�\Dٿ\D��\D�?\D�\Dڿ\D��\D�?\D�\Dۿ\D��\D�?\D�\Dܿ\D��\D�?\D�\Dݿ\D��\D�?\D�\D޿\D��\D�?\D�\D߿\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D��\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�h�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�A��A�#�Aښ�A�OvA�AAّ�A��mA�kQA�ܒA�=A�� A��A�=<A��A�^jA��vA�aHA��Aѭ�A�($A���AЧ�AЏ�AЁ�A�jKA�O�A�5tA�(A���A���A�ʌAϬAϞ�AϒoA��fA�ncA�!-A��/A�)�A�8RA��cA���A���A�B'A�$A��=A�2�A��GA���A�GEA���A��A��.A�poA�A�y�A��<A�I�A���A�ҽA��^A��A���A�y�A���A�T�A�sA���A�A�V�A�[�A�o A�
�A��bA���A��~A�)*A���A�|PA�	�A��<A�,qA�J�A�"�A���A��QA��A�7A�l�A���A�?�A��+A�OvA�<�A��vA���A�p;A�$A~� A|��AtAo�&Ah�8Aa��A_��A]dZAY��AUuAR�>AQ�[AP�AK��AF�AC�MAAuA> �A;7LA6ںA4r�A3P�A2?�A/��A/&�A.�A-ѷA-��A-��A,��A,6A+d�A(�rA&�A&A&`�A'�zA&�A%{JA#6zA �aA�AC�A�A��A��A�HA�KA��A�A�AA�A�&A�3A�.AݘA�8AN<AFtA�AA*�A��A��Aq�A�Ah�A�AOvA�A�A�RAS�A�hA�A��A��A�'A��A�AAHA;dA�}A�A��AuA
��A
q�A
@�A	�A	n/A	5?A	 iA-A��AN�A2�A��A�]A$�A��A�oA��A��AV�A��A�A��AѷA$A �xA�A iA O@�S�@���@�S@�#�@�/@�v�@�A�@�خ@�$t@��@��m@���@�m]@��@�iD@�$@�8�@�U�@��@�n�@�4�@�m�@�J�@�T�@���@��@���@��@��@��@��2@��r@���@罥@�dZ@�r�@�@�iD@��@�A�@�8�@≠@ᰊ@� \@���@��\@�A @��A@��'@�6@ۭC@ڎ�@�˒@�-w@ث6@�c @ب�@؉�@�4@׫�@ד�@�|�@��M@�V�@��@��@���@�_�@ӿH@�/�@���@ѽ�@�&�@��y@�w�@�H�@�C�@���@�s@�#�@�0�@���@�Ɇ@ΐ.@ͿH@��@̵@�Ft@˩�@ˉ7@˥@��K@��@��@��@ȱ�@�L0@��z@��@�ں@��'@�GE@Ņ@���@�p;@��A@�^�@��@¡b@�^5@�>B@�_@�g�@���@���@�6�@�`B@�IR@�;d@���@�B[@��@�>�@�j@� �@��X@�Dg@�+@�!�@�+@��c@���@�;�@��+@��&@��j@���@��~@�%F@��U@���@�Z@�=q@�@��Q@��-@�`B@�+@���@���@�m�@���@��P@��\@�	@��d@�g�@��/@���@�?�@���@�e,@�Mj@�ߤ@�R�@���@�4�@��!@�%�@��@�~�@�C@��B@���@��@�R�@��@�h
@�,=@���@��M@�l�@�;d@���@�xl@�]d@��@���@�o�@���@�͟@�N�@���@��"@��@���@�xl@�&�@���@���@�qv@�)_@��@�v�@�dZ@��"@��[@��@���@�w�@�@�%F@���@��b@�	@��a@�O�@�(�@��P@��B@���@���@�c�@��.@���@�H�@�֡@��+@�2�@��n@�IR@��@��)@���@�W�@��]@�@���@�0�@�@��@���@�,=@��M@�8�@��@�z�@�6@��@�o @��@�z@�1�@�@���@�o�@�B�@���@��"@���@���@�Dg@��S@��@���@�F@�L�@�9�@��@��_@�R�@�b@��n@�p�@��@��}@���@��@���@���@���@�x@�e,@�Mj@�!-@�֡@���@�u�@�_�@�$�@��@��Q@���@�H�@��@���@�N�@�{@��q@���@�+@��j@�u@��@�G�@��@��<@���@�1�@�@��z@�l�@�8�@��@���@�҉@���@���@�N�@�G@��
@���@�]�@�IR@��@��x@�`�@�5?@���@�s@�F@�A @�/�@��@���@�/�@���@���@��@���@���@���@��X@���@��h@��@�o @�^�@�O�@�7L@���@��U@��4@��_@���@�l"@�	�@!-@~��@~M�@~ �@}c�@|�O@|  @{E9@z��@zu%@y�@y7L@y�@x��@x-�@w�	@v�@v��@v
�@u��@u��@ux�@uO�@t��@t�9@t]d@s�;@s��@s$t@q�@q7L@q�@p��@p9X@o��@o�{@o=@n�c@n�<@n.�@m�n@mm]@l��@l|�@k��@k�@j��@je@ic@i�@h��@h�Y@hb@g��@g�K@g�k@g�P@gO@f��@f_�@f4@e�C@e[W@d�O@dm�@d[�@d7�@c��@cb�@c�@bں@b�F@b=q@b
�@a��@ahs@a%F@a;@`�.@`e�@`7�@_�W@_��@_8@^ں@^l�@]��@]`B@\�@\�@[�@[C@ZTa@Y�@Y��@Yf�@Y`B@YX@YG�@X��@X�z@X~@W�@W�@V�@VO@Uk�@U�@T�Y@T!@S�&@S@R�r@RJ�@Q��@Qm]@Q+@P�)@PK^@O�W@O��@O~�@N�@N�!@N&�@M�@M`B@M%@L�p@L�4@LM@K�@Kƨ@KiD@J�H@J��@J{�@JOv@I�>@Is�@I+�@H��@H]d@HI�@H-�@G�+@Gy�@F��@Fp;@F
�@E�C@E��@Ek�@EDg@D�@D2�@D4n@D7@C�a@C�	@CRT@C(@B� @B;�@A��@A�3@A��@A�h@A^�@A�@@Ɇ@@K^@?�+@?��@?e�@?J#@?,�@>�}@>p;@>.�@=�d@=��@=�@<PH@<�@;� @:�H@:��@:YK@:O@9�@9��@9S&@8�@8�@8C-@7��@7�
@7��@7a@7Y@6�<@6��@6$�@5�H@5L�@5�@4�@4[�@4 �@3��@3=@2^5@1�@1��@1u�@1+�@0�`@0�j@0�@0��@0_@/�W@/�$@/6z@.�@.�@.3�@-��@-�-@-zx@-IR@-A @-=�@-%F@,�[@,�p@,�@,�@+�@+��@+"�@*�H@*�1@*Ta@*$�@)�@)��@)rG@)�@(�@(�[@(��@'�@'{J@'6z@&��@&��@&E�@&)�@%��@%�H@%��@%\�@%&�@%;@$��@$��@$M@$'R@$	�@#��@#\)@#4�@#�@"�8@"��@"z@"@!��@!2a@!�@ �5@ ��@ ��@ <�@ M@�@�@K�@�@�y@҉@��@��@�6@xl@ff@C�@@/@]d@x@�@�@�@�}@ƨ@y�@>�@�@��@��@	@�@�@��@o @L�@�@��@Z@�]@�@�6@��@��@iD@�@�"@�,@��@��@�h@��@��@Ta@�@�9@`B@/@-w@	l@ѷ@tT@Xy@Q�@'R@�A@��@��@b�@(@��@�@��@$�@��@��@�@�C@��@��@L�@-w@!�@�@�[@�@e�@%�@1@�W@� @��@S�@�@�]@�x@c @c @$�@�.@@��@�@��@s�@a�@N<@!�@�@�@��@Ĝ@�e@N�@*�@�@��@>�@(@o@
�@
�!@
5?@	�@	x�@	0�@�v@��@��@u�@oi@e�@H@:�@1'@'R@�@b@�W@�A@�K@�k@�@U�@C�@=@;d@Y@�@�@�@�@�A@kQ1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�A��A�#�Aښ�A�OvA�AAّ�A��mA�kQA�ܒA�=A�� A��A�=<A��A�^jA��vA�aHA��Aѭ�A�($A���AЧ�AЏ�AЁ�A�jKA�O�A�5tA�(A���A���A�ʌAϬAϞ�AϒoA��fA�ncA�!-A��/A�)�A�8RA��cA���A���A�B'A�$A��=A�2�A��GA���A�GEA���A��A��.A�poA�A�y�A��<A�I�A���A�ҽA��^A��A���A�y�A���A�T�A�sA���A�A�V�A�[�A�o A�
�A��bA���A��~A�)*A���A�|PA�	�A��<A�,qA�J�A�"�A���A��QA��A�7A�l�A���A�?�A��+A�OvA�<�A��vA���A�p;A�$A~� A|��AtAo�&Ah�8Aa��A_��A]dZAY��AUuAR�>AQ�[AP�AK��AF�AC�MAAuA> �A;7LA6ںA4r�A3P�A2?�A/��A/&�A.�A-ѷA-��A-��A,��A,6A+d�A(�rA&�A&A&`�A'�zA&�A%{JA#6zA �aA�AC�A�A��A��A�HA�KA��A�A�AA�A�&A�3A�.AݘA�8AN<AFtA�AA*�A��A��Aq�A�Ah�A�AOvA�A�A�RAS�A�hA�A��A��A�'A��A�AAHA;dA�}A�A��AuA
��A
q�A
@�A	�A	n/A	5?A	 iA-A��AN�A2�A��A�]A$�A��A�oA��A��AV�A��A�A��AѷA$A �xA�A iA O@�S�@���@�S@�#�@�/@�v�@�A�@�خ@�$t@��@��m@���@�m]@��@�iD@�$@�8�@�U�@��@�n�@�4�@�m�@�J�@�T�@���@��@���@��@��@��@��2@��r@���@罥@�dZ@�r�@�@�iD@��@�A�@�8�@≠@ᰊ@� \@���@��\@�A @��A@��'@�6@ۭC@ڎ�@�˒@�-w@ث6@�c @ب�@؉�@�4@׫�@ד�@�|�@��M@�V�@��@��@���@�_�@ӿH@�/�@���@ѽ�@�&�@��y@�w�@�H�@�C�@���@�s@�#�@�0�@���@�Ɇ@ΐ.@ͿH@��@̵@�Ft@˩�@ˉ7@˥@��K@��@��@��@ȱ�@�L0@��z@��@�ں@��'@�GE@Ņ@���@�p;@��A@�^�@��@¡b@�^5@�>B@�_@�g�@���@���@�6�@�`B@�IR@�;d@���@�B[@��@�>�@�j@� �@��X@�Dg@�+@�!�@�+@��c@���@�;�@��+@��&@��j@���@��~@�%F@��U@���@�Z@�=q@�@��Q@��-@�`B@�+@���@���@�m�@���@��P@��\@�	@��d@�g�@��/@���@�?�@���@�e,@�Mj@�ߤ@�R�@���@�4�@��!@�%�@��@�~�@�C@��B@���@��@�R�@��@�h
@�,=@���@��M@�l�@�;d@���@�xl@�]d@��@���@�o�@���@�͟@�N�@���@��"@��@���@�xl@�&�@���@���@�qv@�)_@��@�v�@�dZ@��"@��[@��@���@�w�@�@�%F@���@��b@�	@��a@�O�@�(�@��P@��B@���@���@�c�@��.@���@�H�@�֡@��+@�2�@��n@�IR@��@��)@���@�W�@��]@�@���@�0�@�@��@���@�,=@��M@�8�@��@�z�@�6@��@�o @��@�z@�1�@�@���@�o�@�B�@���@��"@���@���@�Dg@��S@��@���@�F@�L�@�9�@��@��_@�R�@�b@��n@�p�@��@��}@���@��@���@���@���@�x@�e,@�Mj@�!-@�֡@���@�u�@�_�@�$�@��@��Q@���@�H�@��@���@�N�@�{@��q@���@�+@��j@�u@��@�G�@��@��<@���@�1�@�@��z@�l�@�8�@��@���@�҉@���@���@�N�@�G@��
@���@�]�@�IR@��@��x@�`�@�5?@���@�s@�F@�A @�/�@��@���@�/�@���@���@��@���@���@���@��X@���@��h@��@�o @�^�@�O�@�7L@���@��U@��4@��_@���@�l"@�	�@!-@~��@~M�@~ �@}c�@|�O@|  @{E9@z��@zu%@y�@y7L@y�@x��@x-�@w�	@v�@v��@v
�@u��@u��@ux�@uO�@t��@t�9@t]d@s�;@s��@s$t@q�@q7L@q�@p��@p9X@o��@o�{@o=@n�c@n�<@n.�@m�n@mm]@l��@l|�@k��@k�@j��@je@ic@i�@h��@h�Y@hb@g��@g�K@g�k@g�P@gO@f��@f_�@f4@e�C@e[W@d�O@dm�@d[�@d7�@c��@cb�@c�@bں@b�F@b=q@b
�@a��@ahs@a%F@a;@`�.@`e�@`7�@_�W@_��@_8@^ں@^l�@]��@]`B@\�@\�@[�@[C@ZTa@Y�@Y��@Yf�@Y`B@YX@YG�@X��@X�z@X~@W�@W�@V�@VO@Uk�@U�@T�Y@T!@S�&@S@R�r@RJ�@Q��@Qm]@Q+@P�)@PK^@O�W@O��@O~�@N�@N�!@N&�@M�@M`B@M%@L�p@L�4@LM@K�@Kƨ@KiD@J�H@J��@J{�@JOv@I�>@Is�@I+�@H��@H]d@HI�@H-�@G�+@Gy�@F��@Fp;@F
�@E�C@E��@Ek�@EDg@D�@D2�@D4n@D7@C�a@C�	@CRT@C(@B� @B;�@A��@A�3@A��@A�h@A^�@A�@@Ɇ@@K^@?�+@?��@?e�@?J#@?,�@>�}@>p;@>.�@=�d@=��@=�@<PH@<�@;� @:�H@:��@:YK@:O@9�@9��@9S&@8�@8�@8C-@7��@7�
@7��@7a@7Y@6�<@6��@6$�@5�H@5L�@5�@4�@4[�@4 �@3��@3=@2^5@1�@1��@1u�@1+�@0�`@0�j@0�@0��@0_@/�W@/�$@/6z@.�@.�@.3�@-��@-�-@-zx@-IR@-A @-=�@-%F@,�[@,�p@,�@,�@+�@+��@+"�@*�H@*�1@*Ta@*$�@)�@)��@)rG@)�@(�@(�[@(��@'�@'{J@'6z@&��@&��@&E�@&)�@%��@%�H@%��@%\�@%&�@%;@$��@$��@$M@$'R@$	�@#��@#\)@#4�@#�@"�8@"��@"z@"@!��@!2a@!�@ �5@ ��@ ��@ <�@ M@�@�@K�@�@�y@҉@��@��@�6@xl@ff@C�@@/@]d@x@�@�@�@�}@ƨ@y�@>�@�@��@��@	@�@�@��@o @L�@�@��@Z@�]@�@�6@��@��@iD@�@�"@�,@��@��@�h@��@��@Ta@�@�9@`B@/@-w@	l@ѷ@tT@Xy@Q�@'R@�A@��@��@b�@(@��@�@��@$�@��@��@�@�C@��@��@L�@-w@!�@�@�[@�@e�@%�@1@�W@� @��@S�@�@�]@�x@c @c @$�@�.@@��@�@��@s�@a�@N<@!�@�@�@��@Ĝ@�e@N�@*�@�@��@>�@(@o@
�@
�!@
5?@	�@	x�@	0�@�v@��@��@u�@oi@e�@H@:�@1'@'R@�@b@�W@�A@�K@�k@�@U�@C�@=@;d@Y@�@�@�@�@�A@kQ1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B
v`B
u�B
vzB
utB
u�B
y	B
��B
��B
�UB
�B
҉B
��B
�B
�<B
��B
�?B
�B
��B
�kB
��B
��B
��B
�B
��B
�mB
�
B
�>B
�
B
�B
��B
�B
��B
�B
�yB
��B
�2B
��B
��B
��B
�gB
ɺB
�)B
ϑB
�!B
�$B
��B
��B�B�B*�B:^BS�Br�B��B��B�VB�B��B�}B�#B��B"B�B��B��B�:B�EB�B��B�mB�@B�B�QB�|B�B��B��B�:B�6B��BƎB��B��B��B�B��B��B`�BA�B�B
��B
��B
�B
g�B
X�B
F%B
,�B
�B
<B
�B	��B	B	�B	�B	X�B	K�B	=�B	.�B	CB	�B	JB	B�%B��B�&B��BߊBյB��B�B�"B�B�iB�0B�PB��B�qB�B��B��B�/B�B�B��B��B	0�B	2aB	-�B	2�B	+QB	�B	 vB	G�B	V�B	h�B	��B	��B	��B	�B	�kB	��B	��B	��B	�B	��B	��B	��B	�<B	��B	��B	�VB	��B	�"B	�B	��B	��B	��B	��B	�qB	��B	�jB	��B	�6B	�B	�PB	��B	��B	��B	��B	�6B	�JB	�xB	��B	��B	�2B	��B	�B	��B	��B	��B	�9B	�B	�hB	��B	�B	�B	��B	��B	�,B	��B	��B	�tB	��B	��B	�KB	�B	��B	�"B	�eB	�hB	�2B	�B	��B	�CB	��B	�nB	��B	�$B	��B	�B	��B	��B	�B	��B	��B	��B	��B	��B	��B	�B	� B	� B	��B	�"B	�.B	ªB	��B	ǮB	��B	ƨB	��B	��B	��B	�~B	��B	�JB	�0B	�0B	̘B	�B	�~B	�~B	̳B	��B	��B	��B	ʦB	�B	�=B	�RB	��B	��B	�B	�mB	��B	��B	ĶB	ĜB	�=B	��B	̳B	�6B	͟B	��B	��B	�pB	οB	�BB	� B	�bB	�NB	�B	��B	уB	�.B	�4B	��B	��B	�gB	�2B	�{B	ԯB	�?B	ٴB	ٚB	�B	�yB	��B	��B	�_B	ؓB	�QB	�bB	�B	��B	��B	߾B	ߊB	��B	�hB	�B	�tB	�B	�B	��B	�8B	�B	�B	�B	��B	�$B	�0B	�B	�B	�B	�WB	�WB	�WB	�]B	�B	�)B	�}B	�cB	��B	�5B	�!B	�[B	�B	�MB	�B	�B	�B	��B	�B	�?B	��B	��B	��B	��B	�B	�fB	��B	��B	�8B	��B	��B	��B	�$B	��B	��B	�B	�xB	�^B	�B	�dB	�PB	�B	�PB	��B	�0B	�6B	�0B	�^B	��B	��B	��B	�lB	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B
�B
�B
�B
�B
�B
SB
�B
B
�B
3B
�B
?B
%B
�B
+B
fB
KB
�B
1B
�B
B
�B
fB
�B
�B
fB
�B
_B
�B
B
�B
�B
�B
�B
�B
_B
zB
B
�B
�B
zB
�B
�B
B
�B
�B
zB
�B
�B
�B
�B
�B
�B
�B
_B
+B
�B
zB
�B
�B
zB
�B
�B
EB
�B
�B
�B
�B
�B
�B
�B
YB
YB
B
�B
�B
zB
�B
�B
�B
	B

#B
�B
�B
�B
 B
�B
�B
�B
�B
�B
�B
�B
B
B
B
�B
B
VB
�B
�B
 \B
 BB
 �B
 �B
!B
 �B
 �B
!HB
!�B
!�B
"B
!�B
"�B
"�B
"�B
"�B
"�B
#B
#�B
$@B
$tB
$�B
$�B
$�B
$tB
$�B
%B
%�B
%�B
%�B
&B
&B
%�B
&LB
&fB
&�B
&�B
&�B
&LB
&LB
&LB
&fB
'B
'8B
'�B
'�B
'�B
(�B
)�B
*�B
*�B
*�B
+kB
+QB
+6B
+QB
+�B
,qB
,�B
-B
-�B
-�B
-�B
-�B
-�B
-�B
./B
/5B
/�B
/�B
/�B
/�B
0B
0�B
0�B
0�B
0�B
0�B
1'B
1�B
2GB
2GB
2GB
2aB
2�B
3�B
3�B
4TB
4�B
4�B
5?B
5�B
5�B
5�B
6+B
6FB
6�B
6zB
6�B
6`B
6�B
6�B
6�B
72B
7fB
7�B
88B
8B
8lB
9$B
8�B
9$B
9�B
:^B
:�B
;B
;0B
;B
;dB
<B
<6B
<�B
=<B
=<B
=�B
>B
>(B
>�B
>�B
>�B
?B
?HB
?�B
?}B
?�B
?�B
?�B
?�B
@4B
@�B
@�B
@�B
@�B
A;B
A B
A;B
A B
A�B
A�B
BB
B'B
BAB
B[B
BuB
B�B
B�B
B�B
B�B
C-B
CB
CGB
C-B
C{B
C�B
C�B
DB
DgB
D�B
E�B
E�B
FYB
F�B
G+B
G�B
G�B
G�B
G�B
G�B
G�B
HB
H�B
H�B
I7B
I�B
I�B
J=B
J�B
J�B
K^B
K^B
KxB
LJB
L�B
L~B
MB
MPB
M�B
M�B
NB
NVB
NB
N�B
N�B
N�B
O�B
O�B
O�B
P.B
PbB
PHB
P�B
P�B
P�B
Q B
Q4B
Q B
QB
P�B
QNB
Q�B
RB
RB
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
SuB
S�B
S�B
TFB
UB
UMB
UgB
U�B
U�B
V�B
V�B
W?B
W$B
W?B
W?B
WYB
WYB
WsB
WYB
WsB
W�B
W�B
W�B
W�B
X+B
XB
XEB
X�B
X�B
X�B
Y1B
X�B
X�B
ZB
YB
YB
Y�B
Y�B
Y�B
Y�B
ZB
ZB
Z7B
Z7B
ZQB
Z�B
Z�B
[qB
[�B
[�B
[�B
[�B
\�B
\xB
\�B
\�B
]B
]IB
]dB
^5B
^jB
_B
_;B
_VB
_�B
_�B
_�B
`B
`B
`�B
`�B
a-B
a�B
a�B
b4B
b�B
c B
c B
cTB
cTB
cTB
cnB
dB
c�B
d&B
d&B
dtB
d�B
eFB
e`B
e�B
fB
f2B
f�B
f�B
gRB
gmB
g�B
g�B
g�B
h�B
h�B
h�B
iB
iyB
i�B
i�B
jB
jKB
jeB
jB
j�B
j�B
j�B
kB
kkB
k�B
k�B
k�B
lB
l"B
l=B
l"B
l"B
l�B
l�B
l�B
l�B
l�B
l�B
m)B
mCB
m�B
m�B
m�B
n�B
o B
o5B
oOB
oOB
oB
oB
o5B
o5B
o5B
oOB
o�B
o�B
poB
p�B
q[B
q[B
q[B
q[B
qvB
r-B
r|B
r�B
r�B
sB
s�B
s�B
tB
tB
s�B
tB
tB
tB
t�B
t�B
t�B
t�B
t�B
t�B
tnB
t�B
t�B
t�B
u?B
uB
u%B
uB
u%B
u�B
u?B
u�B
v`B
v�B
u�B
vzB
v�B
wB
wB
v�B
wfB
wLB
wfB
w�B
xB
xB
w�B
xB
x�B
x�B
y$B
y	B
y$B
y>B
yXB
yrB
yrB
y�B
y�B
y�B
y�B
z^B
zB
z�B
z�B
z�B
z�B
z�B
{JB
{B
{�B
{�B
|�B
}B
|�B
|jB
|�B
}<B
}qB
}VB
~BB
~B
~wB
~�B
~�B
.B
~�B
�B
�B
�B
�4B
�B
�iB
�B
��B
�iB
�B
��B
��B
��B
��B
�uB
��B
��B
��B
�GB
��B
�aB
�-B
��B
��B
�B
��B
��B
�mB
��B
��B
��B
��B
�YB
��B
��B
��B
�tB
�?B
�%B
��B
�?B
�?B
��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B
v`B
u�B
vzB
utB
u�B
y	B
��B
��B
�UB
�B
҉B
��B
�B
�<B
��B
�?B
�B
��B
�kB
��B
��B
��B
�B
��B
�mB
�
B
�>B
�
B
�B
��B
�B
��B
�B
�yB
��B
�2B
��B
��B
��B
�gB
ɺB
�)B
ϑB
�!B
�$B
��B
��B�B�B*�B:^BS�Br�B��B��B�VB�B��B�}B�#B��B"B�B��B��B�:B�EB�B��B�mB�@B�B�QB�|B�B��B��B�:B�6B��BƎB��B��B��B�B��B��B`�BA�B�B
��B
��B
�B
g�B
X�B
F%B
,�B
�B
<B
�B	��B	B	�B	�B	X�B	K�B	=�B	.�B	CB	�B	JB	B�%B��B�&B��BߊBյB��B�B�"B�B�iB�0B�PB��B�qB�B��B��B�/B�B�B��B��B	0�B	2aB	-�B	2�B	+QB	�B	 vB	G�B	V�B	h�B	��B	��B	��B	�B	�kB	��B	��B	��B	�B	��B	��B	��B	�<B	��B	��B	�VB	��B	�"B	�B	��B	��B	��B	��B	�qB	��B	�jB	��B	�6B	�B	�PB	��B	��B	��B	��B	�6B	�JB	�xB	��B	��B	�2B	��B	�B	��B	��B	��B	�9B	�B	�hB	��B	�B	�B	��B	��B	�,B	��B	��B	�tB	��B	��B	�KB	�B	��B	�"B	�eB	�hB	�2B	�B	��B	�CB	��B	�nB	��B	�$B	��B	�B	��B	��B	�B	��B	��B	��B	��B	��B	��B	�B	� B	� B	��B	�"B	�.B	ªB	��B	ǮB	��B	ƨB	��B	��B	��B	�~B	��B	�JB	�0B	�0B	̘B	�B	�~B	�~B	̳B	��B	��B	��B	ʦB	�B	�=B	�RB	��B	��B	�B	�mB	��B	��B	ĶB	ĜB	�=B	��B	̳B	�6B	͟B	��B	��B	�pB	οB	�BB	� B	�bB	�NB	�B	��B	уB	�.B	�4B	��B	��B	�gB	�2B	�{B	ԯB	�?B	ٴB	ٚB	�B	�yB	��B	��B	�_B	ؓB	�QB	�bB	�B	��B	��B	߾B	ߊB	��B	�hB	�B	�tB	�B	�B	��B	�8B	�B	�B	�B	��B	�$B	�0B	�B	�B	�B	�WB	�WB	�WB	�]B	�B	�)B	�}B	�cB	��B	�5B	�!B	�[B	�B	�MB	�B	�B	�B	��B	�B	�?B	��B	��B	��B	��B	�B	�fB	��B	��B	�8B	��B	��B	��B	�$B	��B	��B	�B	�xB	�^B	�B	�dB	�PB	�B	�PB	��B	�0B	�6B	�0B	�^B	��B	��B	��B	�lB	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B
�B
�B
�B
�B
�B
SB
�B
B
�B
3B
�B
?B
%B
�B
+B
fB
KB
�B
1B
�B
B
�B
fB
�B
�B
fB
�B
_B
�B
B
�B
�B
�B
�B
�B
_B
zB
B
�B
�B
zB
�B
�B
B
�B
�B
zB
�B
�B
�B
�B
�B
�B
�B
_B
+B
�B
zB
�B
�B
zB
�B
�B
EB
�B
�B
�B
�B
�B
�B
�B
YB
YB
B
�B
�B
zB
�B
�B
�B
	B

#B
�B
�B
�B
 B
�B
�B
�B
�B
�B
�B
�B
B
B
B
�B
B
VB
�B
�B
 \B
 BB
 �B
 �B
!B
 �B
 �B
!HB
!�B
!�B
"B
!�B
"�B
"�B
"�B
"�B
"�B
#B
#�B
$@B
$tB
$�B
$�B
$�B
$tB
$�B
%B
%�B
%�B
%�B
&B
&B
%�B
&LB
&fB
&�B
&�B
&�B
&LB
&LB
&LB
&fB
'B
'8B
'�B
'�B
'�B
(�B
)�B
*�B
*�B
*�B
+kB
+QB
+6B
+QB
+�B
,qB
,�B
-B
-�B
-�B
-�B
-�B
-�B
-�B
./B
/5B
/�B
/�B
/�B
/�B
0B
0�B
0�B
0�B
0�B
0�B
1'B
1�B
2GB
2GB
2GB
2aB
2�B
3�B
3�B
4TB
4�B
4�B
5?B
5�B
5�B
5�B
6+B
6FB
6�B
6zB
6�B
6`B
6�B
6�B
6�B
72B
7fB
7�B
88B
8B
8lB
9$B
8�B
9$B
9�B
:^B
:�B
;B
;0B
;B
;dB
<B
<6B
<�B
=<B
=<B
=�B
>B
>(B
>�B
>�B
>�B
?B
?HB
?�B
?}B
?�B
?�B
?�B
?�B
@4B
@�B
@�B
@�B
@�B
A;B
A B
A;B
A B
A�B
A�B
BB
B'B
BAB
B[B
BuB
B�B
B�B
B�B
B�B
C-B
CB
CGB
C-B
C{B
C�B
C�B
DB
DgB
D�B
E�B
E�B
FYB
F�B
G+B
G�B
G�B
G�B
G�B
G�B
G�B
HB
H�B
H�B
I7B
I�B
I�B
J=B
J�B
J�B
K^B
K^B
KxB
LJB
L�B
L~B
MB
MPB
M�B
M�B
NB
NVB
NB
N�B
N�B
N�B
O�B
O�B
O�B
P.B
PbB
PHB
P�B
P�B
P�B
Q B
Q4B
Q B
QB
P�B
QNB
Q�B
RB
RB
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
SuB
S�B
S�B
TFB
UB
UMB
UgB
U�B
U�B
V�B
V�B
W?B
W$B
W?B
W?B
WYB
WYB
WsB
WYB
WsB
W�B
W�B
W�B
W�B
X+B
XB
XEB
X�B
X�B
X�B
Y1B
X�B
X�B
ZB
YB
YB
Y�B
Y�B
Y�B
Y�B
ZB
ZB
Z7B
Z7B
ZQB
Z�B
Z�B
[qB
[�B
[�B
[�B
[�B
\�B
\xB
\�B
\�B
]B
]IB
]dB
^5B
^jB
_B
_;B
_VB
_�B
_�B
_�B
`B
`B
`�B
`�B
a-B
a�B
a�B
b4B
b�B
c B
c B
cTB
cTB
cTB
cnB
dB
c�B
d&B
d&B
dtB
d�B
eFB
e`B
e�B
fB
f2B
f�B
f�B
gRB
gmB
g�B
g�B
g�B
h�B
h�B
h�B
iB
iyB
i�B
i�B
jB
jKB
jeB
jB
j�B
j�B
j�B
kB
kkB
k�B
k�B
k�B
lB
l"B
l=B
l"B
l"B
l�B
l�B
l�B
l�B
l�B
l�B
m)B
mCB
m�B
m�B
m�B
n�B
o B
o5B
oOB
oOB
oB
oB
o5B
o5B
o5B
oOB
o�B
o�B
poB
p�B
q[B
q[B
q[B
q[B
qvB
r-B
r|B
r�B
r�B
sB
s�B
s�B
tB
tB
s�B
tB
tB
tB
t�B
t�B
t�B
t�B
t�B
t�B
tnB
t�B
t�B
t�B
u?B
uB
u%B
uB
u%B
u�B
u?B
u�B
v`B
v�B
u�B
vzB
v�B
wB
wB
v�B
wfB
wLB
wfB
w�B
xB
xB
w�B
xB
x�B
x�B
y$B
y	B
y$B
y>B
yXB
yrB
yrB
y�B
y�B
y�B
y�B
z^B
zB
z�B
z�B
z�B
z�B
z�B
{JB
{B
{�B
{�B
|�B
}B
|�B
|jB
|�B
}<B
}qB
}VB
~BB
~B
~wB
~�B
~�B
.B
~�B
�B
�B
�B
�4B
�B
�iB
�B
��B
�iB
�B
��B
��B
��B
��B
�uB
��B
��B
��B
�GB
��B
�aB
�-B
��B
��B
�B
��B
��B
�mB
��B
��B
��B
��B
�YB
��B
��B
��B
�tB
�?B
�%B
��B
�?B
�?B
��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604104922  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604173918  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604173919  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604173919                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605023926  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605023926  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610131507                      G�O�G�O�G�O�                