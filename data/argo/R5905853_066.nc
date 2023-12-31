CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T17:34:24Z creation;2022-06-04T17:34:25Z conversion to V3.1      
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
_FillValue                 �  I<   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M,   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �@   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �0   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �D   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �    	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ޼   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �    HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �$   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �(   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �h   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �x   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �|   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20220604173424  20220610131507  5905853                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               BA   JA                                  2B  A   APEX                            8421                            2.11.2                          846 @�J*7.�1   @�J+5��%@,�/��w�c#KƧ�1   GPS     A   B   B   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @@  @�  @�33A   A   A@  A`  A�  A�  A�  A���A�33A�  A�33A�  B   B  B  B  B��B*  B0  B6��B?��BJffBN��BX  B`ffBf��Bp  Bx  B�  B�  B�  B�  B���B�ffB���B���B�  B�  B�  B�  B�33B�  B���B���B�  Bę�B�ffB���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B���C  C�C  C�fC
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Ck�fCn  Cp  Cr  Cs�fCv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DKfDK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|�fD}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�3D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @>�R@~�R@\@�\)A�A?�A_�A�A��
A��
A���A�
=A��
A�
=A��
A��
B�B�B�B�B)�B/�B6�RB?�BJQ�BN�RBW�B`Q�Bf�RBo�Bw�B�B���B���B���B�B�\)B��]B�B���B���B���B���B�(�B���B�B�B���Bď]B�\)B�B���B���B���B���B���B���B���B���B���B���B���B��]B�C��C{C��C�GC	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck�GCm��Co��Cq��Cs�GCu��Cw��Cy��C{��C}��C��C��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��C��qC��qC�
>C��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��C��qC��qC��qC��qC��qC��qC�
>C�
>C��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qD ~�D ��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D	~�D	��D
~�D
��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D ~�D ��D!~�D!��D"~�D"��D#~�D#��D$~�D$��D%~�D%��D&~�D&��D'~�D'��D(~�D(��D)~�D)��D*~�D*��D+~�D+��D,~�D,��D-~�D-��D.~�D.��D/~�D/��D0~�D0��D1~�D1��D2~�D2��D3~�D3��D4~�D4��D5~�D5��D6~�D6��D7~�D7��D8~�D8��D9~�D9��D:~�D:��D;~�D;��D<~�D<��D=~�D=��D>~�D>��D?~�D?��D@~�D@��DA~�DA��DB~�DB��DC~�DC��DD~�DD��DE~�DE��DF~�DF��DG~�DG��DH~�DH��DI~�DI��DJ~�DKDK~�DK��DL~�DL��DM~�DM��DN~�DN��DO~�DO��DP~�DP��DQ~�DQ��DR~�DR��DS~�DS��DT~�DT��DU~�DU��DV~�DV��DW~�DW��DX~�DX��DY~�DY��DZ~�DZ��D[~�D[��D\~�D\��D]~�D]��D^~�D^��D_~�D_��D`~�D`��Da~�Da��Db~�Db��Dc~�Dc��Dd~�Dd��De~�De��Df~�Df��Dg~�Dg��Dh~�Dh��Di~�Di��Dj~�Dj��Dk~�Dk��Dl~�Dl��Dm~�Dm��Dn~�Dn��Do~�Do��Dp~�Dp��Dq~�Dq��Dr~�Dr��Ds~�Ds��Dt~�Dt��Du~�Du��Dv~�Dv��Dw~�Dw��Dx~�Dx��Dy~�Dy��Dz~�Dz��D{~�D{��D|�D|��D}~�D}��D~~�D~��D~�D��D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�B�D���D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�B�D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��)D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��)D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D¿\D��\D�?\D�\Dÿ\D��\D�?\D�\DĿ\D��\D�?\D�\Dſ\D��\D�?\D�\Dƿ\D��\D�?\D�\Dǿ\D��\D�?\D�\Dȿ\D��\D�?\D�\Dɿ\D��\D�?\D�\Dʿ\D��\D�?\D�\D˿\D��\D�?\D�\D̿\D��\D�?\D�\DͿ\D��\D�?\D�\Dο\D��\D�?\D�\DϿ\D��\D�?\D�\Dп\D��\D�?\D�\Dѿ\D��\D�?\D�\Dҿ\D��\D�?\D�\Dӿ\D��\D�?\D�\DԿ\D��\D�?\D�\Dտ\D��\D�?\D�\Dֿ\D��\D�?\D�\D׿\D��\D�?\D�\Dؿ\D��\D�?\D�\Dٿ\D��\D�?\D�\Dڿ\D��\D�?\D�\Dۿ\D��\D�?\D�\Dܿ\D��\D�?\D�\Dݿ\D��\D�?\D�\D޿\D��\D�?\D�\D߿\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D��\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�^jA�e�A�e�A�ffA�k�A�qAA�tA�uZA�w�A�x�A�v�A�uZA�x8A�x�A�y�A�{A�{A�{A�zxA�|PA�[WA҉�A���A���A�,�A�8�A�A�wfA�XEA���A��	A��JA���A��(A��_A�jA���A��A�%zA�,�A���A���A��AA��mA��A���A��.A���A���A�+kA� �A�MA�xA��A��KA�:*A4�Ay�6Au{�As{JAq~Am}�Ag��Ad˒Ab��A_�RAYr�AN�*AJ�!AH��AFx�AES&AC� AB!�A@p�A@�A?�A=|�A=�A=YA=ZA<�!A:�oA:�A9+A8�A8�A7�zA5<6A3�A3_�A2��A1��A/h�A,w2A+�A+ZA+N�A+^5A+OvA(֡A'�A'ϫA'��A'a�A'>�A&˒A&6zA%M�A%��A%��A%��A%n�A$FtA#\�A#�A"�CA"XA!0�A ��A 1'A�.AzA�A8�A�+Av`A��A��A,�A��A��A�9A��A(�A�KA�/Ap�A�A�-A��A!�A�^A�A;AqA	AVA�AA�[A\�A�vA\)A5?A;A�YA
ȴA
0UA	cA��A�bA^�ADgA�Ap;A��A�AMA+�A��A�'A�~Ac�AV�AH�A-�A�A��A Z�@��g@�B�@�Ft@�B�@�2a@���@���@�G@��C@�o�@��D@��+@���@��@�+@�q@���@�{�@�H�@��>@�=�@��?@��@�2�@�F@���@�U�@��@�A�@��@�f�@�͟@��@��
@�N<@�~(@�hs@��@褩@�@�X@��@��@�qv@��8@�Ft@��@�2a@��y@�6@��@���@��Y@��@�hs@�ی@� �@�c�@ܲ�@܄�@���@�Vm@���@��d@�!-@��@��@ؾ@�YK@�m]@�`�@Պ	@���@�u%@�N�@�8�@�b@�ԕ@ӊ�@�;d@��5@ұ�@҅�@�Q@�!@���@���@шf@��@мj@�c @�@υ@��5@β�@��@ͫ�@�?}@̰!@�GE@��@���@�A�@�Ɇ@�W�@ɗ�@��@��@Ȣ4@�>B@�L0@�u%@Ȝx@ȦL@Ȩ�@�~(@�bN@��&@�j@ǫ�@Ǘ$@���@���@�j@��|@ġb@��D@�a�@��M@ �@�!�@���@�_p@� �@�T�@�~(@�i�@�4@���@�,=@���@��@���@�y>@�4n@��@��*@��@�4�@��v@���@�|�@�0U@���@�!-@�~(@�]d@��@��j@�;@�m�@��@�N�@�+@�J�@�M@�!@�'R@��@�iD@�:�@��!@�u%@�!�@���@�@O@�@��h@��@���@�)_@���@�#:@���@�\�@��@��H@���@�x@���@�%@��e@�r�@�7�@�ԕ@�.I@���@��w@�F@���@�� @�PH@�	�@��w@�{J@�A @���@���@�^5@�x@���@�~�@�e�@�P�@�1�@��c@�6@�	@��)@��@�f�@�g�@���@��0@�|�@�8@�$t@�+@�(@���@�g8@�  @�+@�҉@��x@�_@��@��@�@��-@�Mj@���@��x@�_�@�)�@�
�@���@���@���@�Q�@�1�@�+@��@��u@�U2@�:�@��@���@��#@��@���@�s@�m]@�c�@���@���@�1@���@���@���@�b�@�S@���@��@���@�h
@���@��$@�;d@��m@�h�@�M�@�3�@�	@�@���@�x�@���@�Q@���@�s�@�2a@��D@���@���@�@�W?@��@��9@�|�@�M�@�)�@��]@���@���@�O�@� \@���@��@�~�@���@���@�J@��W@��@�t�@�Dg@�2a@�(�@��@���@�~�@�J�@� �@���@��f@�_p@��8@���@��@��r@���@���@��"@��@���@��z@�r�@�Xy@�*�@��.@��w@���@�n/@�1�@���@��@���@���@�Q�@�J@��H@�K�@�!�@��@��@��@�3�@�	@�@��@�V@_p@�@~��@~Q@~)�@}�@}B�@|�@|7�@|%�@|�@{�@@{8@zZ�@y�C@xw�@w˒@wiD@w@O@v��@v �@u�@tb@sƨ@s�@@s�4@s1�@r�,@ru%@r�@q��@q|@q*0@p�p@p��@p$@o�@o|�@o\)@o=@n�@n��@m�@mj@l��@l9X@k��@kj�@k�@j�@j��@jQ@i�C@iQ�@i	l@h�@hPH@h-�@g�m@g�a@gt�@g�@f��@f��@f_�@f	@e�7@e�@e�@e�@d�@dw�@c�
@cs@c@b�<@b($@aԕ@aL�@`��@_�@_��@_Y@^��@^�@^l�@]�@]�@]B�@\[�@[ƨ@[+@Z��@Z� @Z{�@Zc @Z�@YT�@X�j@W�@W��@Wb�@W/�@W i@V�\@VR�@V�@U��@U�-@UrG@T�@T]d@T9X@Tb@S��@S�*@S��@S�@S=@S�@S(@R�M@R��@RZ�@R4@R@Q�@Q��@Q�7@QB�@P�)@P[�@PK^@P7�@P@O�@O�F@Ox@O$t@O�@N�c@N�\@N=q@M��@Ma�@MA @L�P@L��@Lz�@L	�@K��@Kqv@KC@J�B@Jq�@J&�@J�@I�#@IT�@IV@H�$@HtT@G�@G�g@G�4@F��@E�j@E��@E��@E�X@E+�@Dی@DĜ@D��@DQ�@D6@C��@C��@CY@B�c@Bȴ@BM�@A��@Azx@Am]@AO�@A8�@@��@@q@?�@?��@?K�@?$t@?S@>�]@>Z�@=�@=�"@<��@<�@<U2@;�g@;��@;|�@;�@:!�@9J�@8�@8��@8r�@8N�@7�@7K�@6�s@6Ta@5�-@4�p@4<�@4�@3��@3a@3@O@2��@2��@2W�@1�Z@1|@0y>@/��@/�@.�6@._�@.�@-�@-��@-w2@,��@,�@,l"@,�@+�@+�@@+X�@+�@*�b@*��@*~�@*�@)�C@)*0@(�f@(֡@(��@(��@(A�@'�}@'S�@&�c@&z@&=q@&4@%�@%�^@%��@%N<@%@$��@$��@#�@#�:@#�4@#K�@#@"��@"ȴ@"��@"z@"a|@"YK@"L0@"+k@!@!��@!Q�@!/@!�@ ��@ ��@ ��@ m�@ PH@ 6@ �@�@��@dZ@/�@ߤ@0U@�j@�@f�@X@%F@��@e�@<�@"h@��@�@�@�V@b�@,�@��@z@6�@0U@�9@��@j@J�@(�@�@�E@�@��@�@�@��@��@9X@,=@M@��@��@s@A�@�8@�@҉@�R@��@a|@Z�@L0@&�@�#@�M@A @%F@�v@�@V�@�@��@��@��@��@��@��@e�@
=@�'@�x@{�@-@ϫ@��@��@�t@Q�@?}@:�@0�@+@�@�9@��@�Y@l"@h�@e�@S�@:�@�g@j�@�@�@�R@�@��@?@u@�o@��@�^@��@c�@��@�$@�_@Z@@��@9�@@
��@
c @
u@	�3@	�@	zx@	f�@	:�@	�@	�@�K@��@�U@��@?�@@�Q@{J@U�@&@�@�@@�]@��@��@ff@J@��@�T@��@��@��@S&@A @4@;@�[@�9@��@��@>B@�@b@G@�+@�@�&@��@��@�:@x@\)@H�@�@��@ȴ@�11111111111111111111144111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�^jA�e�A�e�A�ffA�k�A�qAA�tA�uZA�w�A�x�A�v�A�uZA�x8A�x�A�y�A�{A�{A�{A�zxA�|PA�[WA҉�A���A���A�,�A�8�A�A�wfA�XEA���A��	A��JA���A��(A��_A�jA���A��A�%zA�,�A���A���A��AA��mA��A���A��.A���A���A�+kA� �A�MA�xA��A��KA�:*A4�Ay�6Au{�As{JAq~Am}�Ag��Ad˒Ab��A_�RAYr�AN�*AJ�!AH��AFx�AES&AC� AB!�A@p�A@�A?�A=|�A=�A=YA=ZA<�!A:�oA:�A9+A8�A8�A7�zA5<6A3�A3_�A2��A1��A/h�A,w2A+�A+ZA+N�A+^5A+OvA(֡A'�A'ϫA'��A'a�A'>�A&˒A&6zA%M�A%��A%��A%��A%n�A$FtA#\�A#�A"�CA"XA!0�A ��A 1'A�.AzA�A8�A�+Av`A��A��A,�A��A��A�9A��A(�A�KA�/Ap�A�A�-A��A!�A�^A�A;AqA	AVA�AA�[A\�A�vA\)A5?A;A�YA
ȴA
0UA	cA��A�bA^�ADgA�Ap;A��A�AMA+�A��A�'A�~Ac�AV�AH�A-�A�A��A Z�@��g@�B�@�Ft@�B�@�2a@���@���@�G@��C@�o�@��D@��+@���@��@�+@�q@���@�{�@�H�@��>@�=�@��?@��@�2�@�F@���@�U�@��@�A�@��@�f�@�͟@��@��
@�N<@�~(@�hs@��@褩@�@�X@��@��@�qv@��8@�Ft@��@�2a@��y@�6@��@���@��Y@��@�hs@�ی@� �@�c�@ܲ�@܄�@���@�Vm@���@��d@�!-@��@��@ؾ@�YK@�m]@�`�@Պ	@���@�u%@�N�@�8�@�b@�ԕ@ӊ�@�;d@��5@ұ�@҅�@�Q@�!@���@���@шf@��@мj@�c @�@υ@��5@β�@��@ͫ�@�?}@̰!@�GE@��@���@�A�@�Ɇ@�W�@ɗ�@��@��@Ȣ4@�>B@�L0@�u%@Ȝx@ȦL@Ȩ�@�~(@�bN@��&@�j@ǫ�@Ǘ$@���@���@�j@��|@ġb@��D@�a�@��M@ �@�!�@���@�_p@� �@�T�@�~(@�i�@�4@���@�,=@���@��@���@�y>@�4n@��@��*@��@�4�@��v@���@�|�@�0U@���@�!-@�~(@�]d@��@��j@�;@�m�@��@�N�@�+@�J�@�M@�!@�'R@��@�iD@�:�@��!@�u%@�!�@���@�@O@�@��h@��@���@�)_@���@�#:@���@�\�@��@��H@���@�x@���@�%@��e@�r�@�7�@�ԕ@�.I@���@��w@�F@���@�� @�PH@�	�@��w@�{J@�A @���@���@�^5@�x@���@�~�@�e�@�P�@�1�@��c@�6@�	@��)@��@�f�@�g�@���@��0@�|�@�8@�$t@�+@�(@���@�g8@�  @�+@�҉@��x@�_@��@��@�@��-@�Mj@���@��x@�_�@�)�@�
�@���@���@���@�Q�@�1�@�+@��@��u@�U2@�:�@��@���@��#@��@���@�s@�m]@�c�@���@���@�1@���@���@���@�b�@�S@���@��@���@�h
@���@��$@�;d@��m@�h�@�M�@�3�@�	@�@���@�x�@���@�Q@���@�s�@�2a@��D@���@���@�@�W?@��@��9@�|�@�M�@�)�@��]@���@���@�O�@� \@���@��@�~�@���@���@�J@��W@��@�t�@�Dg@�2a@�(�@��@���@�~�@�J�@� �@���@��f@�_p@��8@���@��@��r@���@���@��"@��@���@��z@�r�@�Xy@�*�@��.@��w@���@�n/@�1�@���@��@���@���@�Q�@�J@��H@�K�@�!�@��@��@��@�3�@�	@�@��@�V@_p@�@~��@~Q@~)�@}�@}B�@|�@|7�@|%�@|�@{�@@{8@zZ�@y�C@xw�@w˒@wiD@w@O@v��@v �@u�@tb@sƨ@s�@@s�4@s1�@r�,@ru%@r�@q��@q|@q*0@p�p@p��@p$@o�@o|�@o\)@o=@n�@n��@m�@mj@l��@l9X@k��@kj�@k�@j�@j��@jQ@i�C@iQ�@i	l@h�@hPH@h-�@g�m@g�a@gt�@g�@f��@f��@f_�@f	@e�7@e�@e�@e�@d�@dw�@c�
@cs@c@b�<@b($@aԕ@aL�@`��@_�@_��@_Y@^��@^�@^l�@]�@]�@]B�@\[�@[ƨ@[+@Z��@Z� @Z{�@Zc @Z�@YT�@X�j@W�@W��@Wb�@W/�@W i@V�\@VR�@V�@U��@U�-@UrG@T�@T]d@T9X@Tb@S��@S�*@S��@S�@S=@S�@S(@R�M@R��@RZ�@R4@R@Q�@Q��@Q�7@QB�@P�)@P[�@PK^@P7�@P@O�@O�F@Ox@O$t@O�@N�c@N�\@N=q@M��@Ma�@MA @L�P@L��@Lz�@L	�@K��@Kqv@KC@J�B@Jq�@J&�@J�@I�#@IT�@IV@H�$@HtT@G�@G�g@G�4@F��@E�j@E��@E��@E�X@E+�@Dی@DĜ@D��@DQ�@D6@C��@C��@CY@B�c@Bȴ@BM�@A��@Azx@Am]@AO�@A8�@@��@@q@?�@?��@?K�@?$t@?S@>�]@>Z�@=�@=�"@<��@<�@<U2@;�g@;��@;|�@;�@:!�@9J�@8�@8��@8r�@8N�@7�@7K�@6�s@6Ta@5�-@4�p@4<�@4�@3��@3a@3@O@2��@2��@2W�@1�Z@1|@0y>@/��@/�@.�6@._�@.�@-�@-��@-w2@,��@,�@,l"@,�@+�@+�@@+X�@+�@*�b@*��@*~�@*�@)�C@)*0@(�f@(֡@(��@(��@(A�@'�}@'S�@&�c@&z@&=q@&4@%�@%�^@%��@%N<@%@$��@$��@#�@#�:@#�4@#K�@#@"��@"ȴ@"��@"z@"a|@"YK@"L0@"+k@!@!��@!Q�@!/@!�@ ��@ ��@ ��@ m�@ PH@ 6@ �@�@��@dZ@/�@ߤ@0U@�j@�@f�@X@%F@��@e�@<�@"h@��@�@�@�V@b�@,�@��@z@6�@0U@�9@��@j@J�@(�@�@�E@�@��@�@�@��@��@9X@,=@M@��@��@s@A�@�8@�@҉@�R@��@a|@Z�@L0@&�@�#@�M@A @%F@�v@�@V�@�@��@��@��@��@��@��@e�@
=@�'@�x@{�@-@ϫ@��@��@�t@Q�@?}@:�@0�@+@�@�9@��@�Y@l"@h�@e�@S�@:�@�g@j�@�@�@�R@�@��@?@u@�o@��@�^@��@c�@��@�$@�_@Z@@��@9�@@
��@
c @
u@	�3@	�@	zx@	f�@	:�@	�@	�@�K@��@�U@��@?�@@�Q@{J@U�@&@�@�@@�]@��@��@ff@J@��@�T@��@��@��@S&@A @4@;@�[@�9@��@��@>B@�@b@G@�+@�@�&@��@��@�:@x@\)@H�@�@��@ȴ@�11111111111111111111144111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B�B��B�B�*B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�uB�IBu�BeBP.BVB`�BbNBkBhsBf�By�B�bB��B��B�FB��B�OB�BB�DB�_B�+B��B�sB��B��B4�B
�$B
W�B
=�B
1�B

�B
�B	�,B	��B	��B	��B	��B	z^B	o�B	g�B	o B	a�B	SB	G+B	8lB	pB��B	3�B	BB	W
B	`�B	k�B	mCB	wLB	u�B	{B	�PB	ŢB	� B	�B	�sB	��B	��B	�B	�|B	�vB	�B	�B	�B	�B	�B	��B	�AB	ܬB	�=B	��B	��B	�B
 B	��B	�B
 B
�B

=B
	7B
�B
9B
-B
�B
$@B
(XB
(�B
&�B
"�B
!bB
 �B
!B
�B
�B
�B
KB
�B
sB
�B
�B
�B
�B
3B	��B	��B	��B	��B	��B	��B	�B	�*B	��B	�B	�QB	�B	��B	��B	�B	�*B	�	B	�qB	��B	�<B	�BB	��B	��B	��B	�9B	�/B	�B	��B	�jB	ڠB	��B	ԕB	��B	ЗB	ӏB	ּB	ӏB	ϑB	�JB	ʌB	�&B	�2B	�4B	��B	ߤB	�B	�B	�vB	�B	�B	�B	�B	�!B	��B	��B	�oB	�|B	��B	��B	�B	�B	��B	��B	�CB	�cB	��B	��B	�"B	�B	��B	��B	�B	�B	�B	�,B	�B	�,B	�nB	�:B	��B	�B	�HB	�pB	޸B	�B	�]B	ބB	��B	޸B	��B	�B	یB	��B	�+B	�B	چB	ٴB	�QB	یB	ܒB	�~B	�B	ބB	ݘB	�xB	�B	��B	��B	�5B	ބB	�5B	�!B	�'B	�-B	��B	��B	�:B	�B	�B	�tB	�,B	��B	�B	��B	�:B	�TB	�TB	�B	�B	�B	��B	�@B	�@B	�@B	��B	��B	��B	��B	�,B	��B	�,B	�,B	�B	�zB	�B	�B	��B	�B	�B	��B	�zB	�LB	��B	�B	�B	�B	�XB	�*B	�B	��B	�WB	��B	�B	�^B	��B	��B	��B
�B
B
 iB
�B
�B
�B
�B
�B
 �B
 OB	��B
 4B	�HB	�]B	�<B	��B	��B	��B	�fB	��B	��B	�zB	��B	��B
 �B	��B	�"B	�^B	��B	�XB	�lB	�B	��B	��B	�	B	��B	�B	��B	�B	��B
 �B
�B
�B
mB
-B
B
 �B	�B	�6B	��B	��B
 4B
 �B
�B
B
 B
 OB
 iB	�}B	�]B	��B	�B	��B	�.B	��B
 �B
 �B
B
 �B
;B
B
oB
�B
AB
B
GB
aB
{B
�B
�B
SB
tB
�B
�B
�B
�B
+B
zB
�B
�B
B
B
KB
�B
�B
�B
�B
�B
�B
	B

#B
	�B
	�B

#B

�B
�B
�B
�B
�B
B
B
�B
�B
KB
1B
EB
9B
SB
?B
�B
�B
�B
�B
	B
=B
�B
�B
B
B
CB
]B
CB
�B
�B
xB
CB
�B
�B
IB
~B
�B
B
jB
�B
�B
!B
VB
!B
pB
�B
!B
 �B
 �B
 �B
 \B
�B
B
�B
�B
�B
�B
�B
�B
�B
 'B
 BB
 \B
 \B
 vB
 �B
 �B
!�B
"hB
"�B
"�B
!�B
!HB
�B
 B
!-B
 �B
!B
 �B
 BB
�B
�B
�B
 'B
 �B
 �B
 vB
!HB
"4B
# B
'�B
*B
*B
*�B
+�B
,=B
,=B
,=B
+�B
+�B
+�B
,�B
-B
-�B
-�B
.B
.B
.cB
.�B
/�B
/�B
0B
0B
0;B
0�B
0�B
1B
1'B
1[B
1�B
1�B
2-B
2-B
2aB
2�B
2�B
2�B
2�B
2�B
3MB
3�B
4B
4nB
4�B
4�B
4�B
5%B
5�B
5�B
5�B
5�B
5�B
6B
6+B
6`B
6�B
6�B
6�B
72B
7�B
7�B
7�B
7�B
8B
8RB
8�B
9$B
:*B
:�B
:�B
:�B
;0B
;JB
;�B
=B
=B
<�B
="B
=qB
=�B
=�B
>(B
>]B
>]B
>�B
>�B
>�B
?.B
?}B
?�B
?�B
?�B
?�B
?�B
@�B
@�B
A B
AoB
A�B
A�B
B'B
B'B
B[B
B�B
C{B
C{B
C�B
DB
D3B
DMB
D�B
D�B
EB
EmB
E�B
E�B
F%B
F?B
F�B
GB
GB
F�B
GB
G+B
G_B
GEB
H1B
HKB
HB
H1B
H�B
H�B
H�B
HB
HB
H�B
H�B
IB
IlB
IRB
IB
IB
I�B
IRB
I�B
I�B
I�B
I�B
I�B
J	B
J	B
I�B
I�B
J�B
J�B
J�B
K)B
KxB
K�B
KxB
KDB
K)B
J�B
J	B
J#B
J#B
J#B
JXB
J�B
JrB
J�B
J�B
J�B
KDB
K�B
LB
LdB
LdB
L�B
L�B
L�B
MPB
M�B
NVB
N<B
N<B
N�B
N�B
N�B
O\B
O�B
OvB
O�B
PB
P.B
P�B
Q B
Q B
Q4B
QhB
Q�B
Q�B
R:B
RoB
R�B
R�B
S@B
S@B
S&B
SuB
S�B
S�B
S�B
S�B
TFB
TFB
T,B
U�B
U�B
VB
U�B
VB
V�B
V�B
V�B
W
B
W?B
W$B
W�B
W�B
XEB
XB
XEB
XyB
X�B
X�B
YB
YKB
YKB
YB
Y�B
Y�B
Y�B
Y�B
Y�B
Y�B
Y�B
Z7B
Z7B
ZQB
Z�B
Z�B
[#B
[�B
[�B
[�B
[�B
\�B
]~B
]�B
]�B
^5B
^B
^5B
_B
_;B
_pB
_�B
`BB
`vB
`\B
`�B
`�B
`�B
aB
abB
aHB
abB
abB
bNB
c B
c B
c�B
c�B
c�B
d&B
dZB
dZB
d�B
d�B
d�B
e,B
eFB
ezB
e�B
fB
f�B
fLB
fLB
f�B
gB
g�B
g�B
g�B
g�B
g�B
h>B
h�B
h�B
i*B
i�B
i�B
i�B
jB
jB
j0B
jeB
j�B
j�B
j�B
k�B
k�B
lB
l=B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
mB
m�B
m�B
nB
n/B
nIB
ncB
n�B
n�B
n�B
n�B
n�B
o B
oiB
oiB
oOB
o�B
o�B
p�B
p�B
p�B
qB
p�B
q'B
q�B
q�B
q�B
r-B
r-B
rB
rGB
r|B
r|B
r�B
sB
sMB
s�B
s�B
tB
tnB
tTB
tnB
t�B
t�B
t�B
t�B
uB
uB
t�B
u%B
t�B
u�B
u?B
utB
u�B
u�B
u�B
v+B
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
w�B
x8B
xRB
x�B
y	B
yXB
yXB
yrB
yXB
yXB
yXB
y�B
y�B
zDB
zDB
z^B
z�B
{0B
{B
z�B
{B
{�B
{�B
{�B
{�B
{�B
|B
|6B
|B
|PB
|jB
|�B
|jB
|jB
|�B
|�B
}VB
}�B
~B
~B
~B
~BB
~wB
~�B
~�B
~�B
~�B
~�B
HB
�B
�B
�B
�B
�iB
��B
�UB
�oB
��B
�'B
��B
��B
��B
��B
��B
�aB
�{B
�{B
��B
��B
��B
��B
�MB
�MB
��B
�B
�B
�mB
�SB
�mB
�mB
��B
��B
��B
�B
��B
��B
��B
��B
��B
��B
�_B
�EB
�_B
�zB
��B
��B
��B
��B
�1B
�KB
�KB
�KB
��B
��B
��B
��B
��B
��B
�B
�RB
�lB
�lB
��B
��B
��11111111111111111111144111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B�B��B�B�*B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�uB�IBu�BeBP.BVB`�BbNBkBhsBf�By�B�bB��B��B�FB��B�OB�BB�DB�_B�+B��B�sB��B��B4�B
�$B
W�B
=�B
1�B

�B
�B	�,B	��B	��B	��B	��B	z^B	o�B	g�B	o B	a�B	SB	G+B	8lB	pB��B	3�B	BB	W
B	`�B	k�B	mCB	wLB	u�B	{B	�PB	ŢB	� B	�B	�sB	��B	��B	�B	�|B	�vB	�B	�B	�B	�B	�B	��B	�AB	ܬB	�=B	��B	��B	�B
 B	��B	�B
 B
�B

=B
	7B
�B
9B
-B
�B
$@B
(XB
(�B
&�B
"�B
!bB
 �B
!B
�B
�B
�B
KB
�B
sB
�B
�B
�B
�B
3B	��B	��B	��B	��B	��B	��B	�B	�*B	��B	�B	�QB	�B	��B	��B	�B	�*B	�	B	�qB	��B	�<B	�BB	��B	��B	��B	�9B	�/B	�B	��B	�jB	ڠB	��B	ԕB	��B	ЗB	ӏB	ּB	ӏB	ϑB	�JB	ʌB	�&B	�2B	�4B	��B	ߤB	�B	�B	�vB	�B	�B	�B	�B	�!B	��B	��B	�oB	�|B	��B	��B	�B	�B	��B	��B	�CB	�cB	��B	��B	�"B	�B	��B	��B	�B	�B	�B	�,B	�B	�,B	�nB	�:B	��B	�B	�HB	�pB	޸B	�B	�]B	ބB	��B	޸B	��B	�B	یB	��B	�+B	�B	چB	ٴB	�QB	یB	ܒB	�~B	�B	ބB	ݘB	�xB	�B	��B	��B	�5B	ބB	�5B	�!B	�'B	�-B	��B	��B	�:B	�B	�B	�tB	�,B	��B	�B	��B	�:B	�TB	�TB	�B	�B	�B	��B	�@B	�@B	�@B	��B	��B	��B	��B	�,B	��B	�,B	�,B	�B	�zB	�B	�B	��B	�B	�B	��B	�zB	�LB	��B	�B	�B	�B	�XB	�*B	�B	��B	�WB	��B	�B	�^B	��B	��B	��B
�B
B
 iB
�B
�B
�B
�B
�B
 �B
 OB	��B
 4B	�HB	�]B	�<B	��B	��B	��B	�fB	��B	��B	�zB	��B	��B
 �B	��B	�"B	�^B	��B	�XB	�lB	�B	��B	��B	�	B	��B	�B	��B	�B	��B
 �B
�B
�B
mB
-B
B
 �B	�B	�6B	��B	��B
 4B
 �B
�B
B
 B
 OB
 iB	�}B	�]B	��B	�B	��B	�.B	��B
 �B
 �B
B
 �B
;B
B
oB
�B
AB
B
GB
aB
{B
�B
�B
SB
tB
�B
�B
�B
�B
+B
zB
�B
�B
B
B
KB
�B
�B
�B
�B
�B
�B
	B

#B
	�B
	�B

#B

�B
�B
�B
�B
�B
B
B
�B
�B
KB
1B
EB
9B
SB
?B
�B
�B
�B
�B
	B
=B
�B
�B
B
B
CB
]B
CB
�B
�B
xB
CB
�B
�B
IB
~B
�B
B
jB
�B
�B
!B
VB
!B
pB
�B
!B
 �B
 �B
 �B
 \B
�B
B
�B
�B
�B
�B
�B
�B
�B
 'B
 BB
 \B
 \B
 vB
 �B
 �B
!�B
"hB
"�B
"�B
!�B
!HB
�B
 B
!-B
 �B
!B
 �B
 BB
�B
�B
�B
 'B
 �B
 �B
 vB
!HB
"4B
# B
'�B
*B
*B
*�B
+�B
,=B
,=B
,=B
+�B
+�B
+�B
,�B
-B
-�B
-�B
.B
.B
.cB
.�B
/�B
/�B
0B
0B
0;B
0�B
0�B
1B
1'B
1[B
1�B
1�B
2-B
2-B
2aB
2�B
2�B
2�B
2�B
2�B
3MB
3�B
4B
4nB
4�B
4�B
4�B
5%B
5�B
5�B
5�B
5�B
5�B
6B
6+B
6`B
6�B
6�B
6�B
72B
7�B
7�B
7�B
7�B
8B
8RB
8�B
9$B
:*B
:�B
:�B
:�B
;0B
;JB
;�B
=B
=B
<�B
="B
=qB
=�B
=�B
>(B
>]B
>]B
>�B
>�B
>�B
?.B
?}B
?�B
?�B
?�B
?�B
?�B
@�B
@�B
A B
AoB
A�B
A�B
B'B
B'B
B[B
B�B
C{B
C{B
C�B
DB
D3B
DMB
D�B
D�B
EB
EmB
E�B
E�B
F%B
F?B
F�B
GB
GB
F�B
GB
G+B
G_B
GEB
H1B
HKB
HB
H1B
H�B
H�B
H�B
HB
HB
H�B
H�B
IB
IlB
IRB
IB
IB
I�B
IRB
I�B
I�B
I�B
I�B
I�B
J	B
J	B
I�B
I�B
J�B
J�B
J�B
K)B
KxB
K�B
KxB
KDB
K)B
J�B
J	B
J#B
J#B
J#B
JXB
J�B
JrB
J�B
J�B
J�B
KDB
K�B
LB
LdB
LdB
L�B
L�B
L�B
MPB
M�B
NVB
N<B
N<B
N�B
N�B
N�B
O\B
O�B
OvB
O�B
PB
P.B
P�B
Q B
Q B
Q4B
QhB
Q�B
Q�B
R:B
RoB
R�B
R�B
S@B
S@B
S&B
SuB
S�B
S�B
S�B
S�B
TFB
TFB
T,B
U�B
U�B
VB
U�B
VB
V�B
V�B
V�B
W
B
W?B
W$B
W�B
W�B
XEB
XB
XEB
XyB
X�B
X�B
YB
YKB
YKB
YB
Y�B
Y�B
Y�B
Y�B
Y�B
Y�B
Y�B
Z7B
Z7B
ZQB
Z�B
Z�B
[#B
[�B
[�B
[�B
[�B
\�B
]~B
]�B
]�B
^5B
^B
^5B
_B
_;B
_pB
_�B
`BB
`vB
`\B
`�B
`�B
`�B
aB
abB
aHB
abB
abB
bNB
c B
c B
c�B
c�B
c�B
d&B
dZB
dZB
d�B
d�B
d�B
e,B
eFB
ezB
e�B
fB
f�B
fLB
fLB
f�B
gB
g�B
g�B
g�B
g�B
g�B
h>B
h�B
h�B
i*B
i�B
i�B
i�B
jB
jB
j0B
jeB
j�B
j�B
j�B
k�B
k�B
lB
l=B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
mB
m�B
m�B
nB
n/B
nIB
ncB
n�B
n�B
n�B
n�B
n�B
o B
oiB
oiB
oOB
o�B
o�B
p�B
p�B
p�B
qB
p�B
q'B
q�B
q�B
q�B
r-B
r-B
rB
rGB
r|B
r|B
r�B
sB
sMB
s�B
s�B
tB
tnB
tTB
tnB
t�B
t�B
t�B
t�B
uB
uB
t�B
u%B
t�B
u�B
u?B
utB
u�B
u�B
u�B
v+B
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
w�B
x8B
xRB
x�B
y	B
yXB
yXB
yrB
yXB
yXB
yXB
y�B
y�B
zDB
zDB
z^B
z�B
{0B
{B
z�B
{B
{�B
{�B
{�B
{�B
{�B
|B
|6B
|B
|PB
|jB
|�B
|jB
|jB
|�B
|�B
}VB
}�B
~B
~B
~B
~BB
~wB
~�B
~�B
~�B
~�B
~�B
HB
�B
�B
�B
�B
�iB
��B
�UB
�oB
��B
�'B
��B
��B
��B
��B
��B
�aB
�{B
�{B
��B
��B
��B
��B
�MB
�MB
��B
�B
�B
�mB
�SB
�mB
�mB
��B
��B
��B
�B
��B
��B
��B
��B
��B
��B
�_B
�EB
�_B
�zB
��B
��B
��B
��B
�1B
�KB
�KB
�KB
��B
��B
��B
��B
��B
��B
�B
�RB
�lB
�lB
��B
��B
��11111111111111111111144111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604104911  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604173424  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604173425  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604173425                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605023432  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605023432  QCF$                G�O�G�O�G�O�            4000JA  ARUP                                                                        20220610131507                      G�O�G�O�G�O�                