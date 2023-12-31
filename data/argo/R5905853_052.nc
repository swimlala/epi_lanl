CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T17:31:59Z creation;2022-06-04T17:31:59Z conversion to V3.1      
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
_FillValue                 �  I@   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M0   PRES_ADJUSTED_QC         
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
_FillValue                 �  �P   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �@   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    PSAL_ADJUSTED            
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
resolution        :�o     �  �`   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �    	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �<   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �@   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �D   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �H   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �L   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20220604173159  20220610131507  5905853                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               4A   JA                                  2B  A   APEX                            8421                            2.11.2                          846 @�&�g(��1   @�&�����@/�vȴ9X�b��"��`1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @��@�  @�  A   A   A@  A`  A�  A�  A�33A�  A�  A�  A���A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B���B���B�  B�33B�ffB�ffBϙ�B���B�  B�  B�  B�  B�  B�  B�ffB�  B�  B�  C   C  C  C  C�C
�C  C�fC  C  C  C  C�fC  C  C  C   C"  C$  C&  C(  C*  C,  C.�C033C1�fC3�fC6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\�C^  C`  Cb  Cc�fCf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C��3C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ Dȃ3D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ D�|�D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@Q�@~�R@�\)@�\)A�A?�A_�A�A��
A�
=A��
A��
A��
A��A��
A��
B�B�B�B�B'�B/�B7�B?�BG�BO�BW�B_�Bg�Bo�Bw�B�B���B���B���B���B���B���B���B���B���B���B���B���B��]B�B�B���B�(�B�\)B�\)BϏ]B�B���B���B���B���B���B���B�\)B���B���B���B���C��C��C��C{C
{C��C�GC��C��C��C��C�GC��C��C��C��C!��C#��C%��C'��C)��C+��C.{C0.C1�GC3�GC5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C\{C]��C_��Ca��Cc�GCe��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C��C��qC��qC��qC��C��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��C��qC��qC��C��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC�
>C��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC�
>C��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC�
>C��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qD ~�D ��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D	~�D	��D
~�D
��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D ~�D ��D!~�D!��D"~�D"��D#~�D#��D$~�D$��D%~�D%��D&~�D&��D'~�D'��D(~�D(��D)~�D)��D*~�D*��D+~�D+��D,~�D,��D-~�D-��D.~�D.��D/~�D/��D0~�D0��D1~�D1��D2~�D2��D3~�D3��D4~�D4��D5~�D5��D6~�D6��D7~�D7��D8~�D8��D9~�D9��D:~�D:��D;~�D;��D<~�D<��D=~�D=��D>~�D>��D?~�D?��D@~�D@��DA~�DA��DB~�DB��DC~�DC��DD~�DD��DE~�DE��DF~�DF��DG~�DG��DH~�DH��DI~�DI��DJ~�DJ��DK~�DK��DL~�DL��DM~�DM��DN~�DN��DO~�DO��DP~�DP��DQ~�DQ��DR~�DR��DS~�DS��DT~�DT��DU~�DU��DV~�DV��DW~�DW��DX~�DX��DY~�DY��DZ~�DZ��D[~�D[��D\~�D\��D]~�D]��D^~�D^��D_~�D_��D`~�D`��Da~�Da��Db~�Db��Dc~�Dc��Dd~�Dd��De~�De��Df~�Df��Dg~�Dg��Dh~�Dh��Di~�Di��Dj~�Dj��Dk~�Dk��Dl~�Dl��Dm~�Dm��Dn~�Dn��Do~�Do��Dp~�Dp��Dq~�Dq��Dr~�Dr��Ds~�Ds��Dt~�Dt��Du~�Du��Dv~�Dv��Dw~�Dw��Dx~�Dx��Dy~�Dy��Dz~�Dz��D{~�D{��D|~�D|��D}~�D}��D~~�D~��D~�D��D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D�D��D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D¿\D��\D�?\D�\Dÿ\D��\D�?\D�\DĿ\D��\D�?\D�\Dſ\D��\D�?\D�\Dƿ\D��\D�?\D�\Dǿ\D��\D�?\DȂ�Dȿ\D��\D�?\D�\Dɿ\D��\D�?\D�\Dʿ\D��\D�?\D�\D˿\D��\D�?\D�\D̿\D��\D�?\D�\DͿ\D��\D�?\D�\Dο\D��\D�?\D�\DϿ\D��\D�?\D�\Dп\D��\D�?\D�\Dѿ\D��\D�?\D�\Dҿ\D��\D�?\D�|)Dӿ\D��\D�?\D�\DԿ\D��\D�?\D�\Dտ\D��\D�?\D�\Dֿ\D��\D�?\D�\D׿\D��\D�?\D�\Dؿ\D��\D�?\D�\Dٿ\D��\D�?\D�\Dڿ\D��\D�?\D�\Dۿ\D��\D�?\D�\Dܿ\D��\D�?\D�\Dݿ\D��\D�?\D�\D޿\D��\D�?\D�\D߿\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D��\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��)111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111Aѡ�AѠ�AўAѝ�Aѝ�Aј�Aљ�Aљ�Aњ�Aћ=AѝIAќxAѝ~AўOAѠ'Aџ�AџVAѠ'AѠ�Aѡ-Aѡ�AѢhAѣ�AѤtAѦAѧ�AѨ�Aѩ�AѪ�AѪ�Aѫ�AѬqAѬqAѡbAє�A�k�A�33A��A��BAжzAА.A�OBA�'AϬqA�_;A���A�U2A���A�Z�A��wA�� A�H�A�� A��nA��tA���A�,�A���A���A��A�J�A��qA��VA�s�A�33A�Z�A�k�A��MA��A��A�%A�*�A�ZQA�0UA�� A�+A��mA�iDA�;dA���A�&�A���A���A���A���A�e`A���Ax�hAs�KApƨAkϫAfrGAa��A\�rAZ�AX�4AT�?ARAO��AL1'AJ-wAH��AF�AFH�AF�AE��AEF�AD��AD��AD�]AD�}AC�]AC��AB  A?iDA?jA>�A>MjA<e,A;S�A:E�A9��A8��A7��A5~(A2�A2H�A3=qA3�A2"hA1A0SA.Y�A-GEA,��A,�1A,%FA+�A)jA(�A(;A'e�A&��A&J�A&"�A&,�A%��A$�PA${A#��A#h
A#Z�A"v�A!�A!}VA �FA F�A��A��A�dAjA+A�	A:�AxA�AȴA�FA��A�fA5�A��A��A+A��A~(A�'A�Az�A��AQA�A?�A��A;A�HA&�A�
A��AbNA�8A�nAs�AL�AMA��ArGA�DA��AqvAE9A��AoiA��A�uA&�A��AN�A��A�EA�hAv`A4�A
��A
��A
�nA
�7A
�+A
]�A
�A	ݘA	�A	�A��AGEAA�A7AݘA�A��A6zA�FA�sA_�A=AMAE9A�A-�Aa�ASA��A��AV�Ac AC�A�,Ab�A^�A�8A�^A�$A�A��Ag8A*0A �yA w�@��D@��@�u�@��y@�,=@���@�X@�Ĝ@���@��@�1�@�U�@�u%@�	@��6@��$@�qv@�)_@��@�J@���@���@�~(@�7@�{J@��B@�@��d@��@�/�@�˒@�M@�@��g@�@�V�@�~�@�+@�@�r�@�E�@��o@�!-@�R�@�s@�~@��m@��a@�_@�'�@�>B@�=@�q�@�@���@�s�@�o@��1@�@�@ߦ�@߆�@�a�@�Dg@��@�u�@��3@݊	@�@O@�Y@��@ܭ�@��j@��@�Ov@�e@�]�@�}V@�	@�@׎�@���@ֳh@��@�?@�V@���@�H�@�9�@��@�J�@��o@��P@�:*@��H@��2@��@���@�O�@��@�k�@���@��@�@��;@���@�ѷ@���@��@�^5@�#:@�u@�/@ƃ�@�:�@�'R@�A�@��9@��`@î�@�!�@¡b@�W�@�PH@��W@�\�@���@�
�@��.@��@�ݘ@��@�m]@��	@�Z�@���@���@�RT@��K@���@�p;@�$�@��a@�1�@��@�m�@��@��w@�Z�@��s@�>B@���@�^�@���@��U@��@���@��0@��7@�P�@��@� �@���@�=�@��@���@���@�^5@��@��}@���@�W?@�4�@��f@��.@�!@���@��@�$�@�˒@��@@�x�@�4�@���@�:�@�~�@��@��@��@��F@�5?@���@�a�@���@���@��@��@��y@��j@�Q@�J@��@���@��N@�N<@�҉@�_@��@�Dg@�/�@���@�u�@�)�@�x@���@��@���@��@���@�E�@��@��@�G@��Q@��"@�c@�_p@�&@�@�ں@�p;@��A@�X�@�C@���@���@�ff@�:�@�O@��+@�u�@�6z@��9@�d�@�~@���@�iD@�/@���@���@�C�@���@�)_@���@�?�@���@�'�@���@�{�@���@�V@��@�x�@�8�@�z�@���@���@�*0@��z@�z�@�d�@�S�@�%�@��m@�`B@�&�@��@�Ĝ@��_@�C�@�x@��K@���@�f�@�N<@�9�@�@@���@���@�\�@�!@���@���@�p�@���@��X@���@�\�@�/�@���@���@��.@�^5@�"h@���@���@��@�<6@��P@��@��r@�2�@��Z@�˒@���@��M@�K�@���@��@���@�B[@��@��W@��@�^�@�9�@�Y@�@��8@��@���@�4n@���@�ԕ@���@�g�@��@���@��4@�xl@�Xy@�1'@���@���@�dZ@�S�@�G�@��@��@��@�h�@��@��}@�zx@�F@�V@��U@���@��r@�W�@��@�@~�@~YK@}�@}Vm@|��@|tT@{�m@{��@z�@z��@z@y%F@xl"@x�@wƨ@wqv@w+@v�c@v�\@vff@vYK@v-@uc@uL�@u%F@t��@t~@s�@r�s@r@p��@p�D@pXy@o�}@o/�@o�@n��@n�}@n��@nJ�@n�@n$�@m�=@m�h@m%@l�O@lbN@kݘ@kY@j�<@j�L@j��@ju%@i��@i}�@iS&@hѷ@h��@hg8@h�@gn/@f��@f҉@f	@e�h@eo @e+�@d�f@dz�@c�w@c�@c�@b�x@b�@a��@`�@_��@_l�@_@O@^��@^z@^Ta@^J�@^@�@]�@][W@\�o@[�@[dZ@Z��@ZC�@Z�@Y��@Y��@Y��@Y�S@Y�@Y#�@X�v@X�9@X�_@Xh�@X~@X�@W�]@W�m@W��@WK�@V��@V6�@V
�@U��@U�^@U�=@U2a@T|�@TA�@T�@S�W@S��@R��@R:*@Q�^@Qa�@Q�@P��@P1'@O��@OK�@N�!@Nh
@M�9@MN<@L�4@L6@L�@K�;@K�@@Ko�@J�@Jn�@J�@I�-@Ip�@I�@H��@Hoi@H$@G��@G�@F�X@Fz@F;�@E�@E|@Ea�@E7L@D�E@D��@Dz�@DC-@Cݘ@C�P@C8@B҉@B��@BH�@B@A�Z@A�N@A}�@A^�@A&�@@��@@Ɇ@@�4@@r�@@I�@@"h@?��@?S@>�'@>�+@>H�@>@=�@=�X@=T�@=2a@<��@<��@<S�@;�@;�a@;~�@:�M@:}V@:i�@:B[@:	@9�.@9�@9�d@9��@9��@9Q�@8��@8*�@8�@7ݘ@7�k@7C@6��@6YK@6
�@5��@5��@57L@4�@4�$@4`�@3�w@3e�@3$t@3�@2n�@2�@2 �@1�)@1�7@17L@1�@0��@0�9@0��@0��@0m�@0~@/˒@/�q@.��@.��@.��@-�@-��@-=�@-@,�@,�o@,1'@+�}@+v`@+{J@+��@+\)@*�8@*��@*a|@*�@*	@)��@)��@)�7@)/@(�@(�z@(�@(tT@(1'@'˒@'�{@'j�@'_p@'E9@&�8@&��@&C�@%��@%�H@%��@%�7@%o @%\�@%%@$ی@$�j@$�z@$9X@$	�@#�@#�F@#��@#j�@#@O@"�M@"�x@"}V@"C�@!��@!��@!�~@!IR@!V@ �@ �)@ �@ H@ �@��@��@a@;d@�@�@�@}V@=q@�C@T�@	l@�p@��@�9@_@�@��@خ@�0@|�@&@�B@�F@l�@YK@
�@�@�@��@f�@:�@�@��@��@��@w�@M@x@�r@�Q@�$@n/@$t@�<@{�@)�@�3@�7@G�@�@�@�u@bN@*�@�r@�@��@o�@P�@,�@@ i@�@�M@�X@�r@p;@Ta@@�3@k�@IR@<6@%F@�@�9@��@oi@Q�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111Aѡ�AѠ�AўAѝ�Aѝ�Aј�Aљ�Aљ�Aњ�Aћ=AѝIAќxAѝ~AўOAѠ'Aџ�AџVAѠ'AѠ�Aѡ-Aѡ�AѢhAѣ�AѤtAѦAѧ�AѨ�Aѩ�AѪ�AѪ�Aѫ�AѬqAѬqAѡbAє�A�k�A�33A��A��BAжzAА.A�OBA�'AϬqA�_;A���A�U2A���A�Z�A��wA�� A�H�A�� A��nA��tA���A�,�A���A���A��A�J�A��qA��VA�s�A�33A�Z�A�k�A��MA��A��A�%A�*�A�ZQA�0UA�� A�+A��mA�iDA�;dA���A�&�A���A���A���A���A�e`A���Ax�hAs�KApƨAkϫAfrGAa��A\�rAZ�AX�4AT�?ARAO��AL1'AJ-wAH��AF�AFH�AF�AE��AEF�AD��AD��AD�]AD�}AC�]AC��AB  A?iDA?jA>�A>MjA<e,A;S�A:E�A9��A8��A7��A5~(A2�A2H�A3=qA3�A2"hA1A0SA.Y�A-GEA,��A,�1A,%FA+�A)jA(�A(;A'e�A&��A&J�A&"�A&,�A%��A$�PA${A#��A#h
A#Z�A"v�A!�A!}VA �FA F�A��A��A�dAjA+A�	A:�AxA�AȴA�FA��A�fA5�A��A��A+A��A~(A�'A�Az�A��AQA�A?�A��A;A�HA&�A�
A��AbNA�8A�nAs�AL�AMA��ArGA�DA��AqvAE9A��AoiA��A�uA&�A��AN�A��A�EA�hAv`A4�A
��A
��A
�nA
�7A
�+A
]�A
�A	ݘA	�A	�A��AGEAA�A7AݘA�A��A6zA�FA�sA_�A=AMAE9A�A-�Aa�ASA��A��AV�Ac AC�A�,Ab�A^�A�8A�^A�$A�A��Ag8A*0A �yA w�@��D@��@�u�@��y@�,=@���@�X@�Ĝ@���@��@�1�@�U�@�u%@�	@��6@��$@�qv@�)_@��@�J@���@���@�~(@�7@�{J@��B@�@��d@��@�/�@�˒@�M@�@��g@�@�V�@�~�@�+@�@�r�@�E�@��o@�!-@�R�@�s@�~@��m@��a@�_@�'�@�>B@�=@�q�@�@���@�s�@�o@��1@�@�@ߦ�@߆�@�a�@�Dg@��@�u�@��3@݊	@�@O@�Y@��@ܭ�@��j@��@�Ov@�e@�]�@�}V@�	@�@׎�@���@ֳh@��@�?@�V@���@�H�@�9�@��@�J�@��o@��P@�:*@��H@��2@��@���@�O�@��@�k�@���@��@�@��;@���@�ѷ@���@��@�^5@�#:@�u@�/@ƃ�@�:�@�'R@�A�@��9@��`@î�@�!�@¡b@�W�@�PH@��W@�\�@���@�
�@��.@��@�ݘ@��@�m]@��	@�Z�@���@���@�RT@��K@���@�p;@�$�@��a@�1�@��@�m�@��@��w@�Z�@��s@�>B@���@�^�@���@��U@��@���@��0@��7@�P�@��@� �@���@�=�@��@���@���@�^5@��@��}@���@�W?@�4�@��f@��.@�!@���@��@�$�@�˒@��@@�x�@�4�@���@�:�@�~�@��@��@��@��F@�5?@���@�a�@���@���@��@��@��y@��j@�Q@�J@��@���@��N@�N<@�҉@�_@��@�Dg@�/�@���@�u�@�)�@�x@���@��@���@��@���@�E�@��@��@�G@��Q@��"@�c@�_p@�&@�@�ں@�p;@��A@�X�@�C@���@���@�ff@�:�@�O@��+@�u�@�6z@��9@�d�@�~@���@�iD@�/@���@���@�C�@���@�)_@���@�?�@���@�'�@���@�{�@���@�V@��@�x�@�8�@�z�@���@���@�*0@��z@�z�@�d�@�S�@�%�@��m@�`B@�&�@��@�Ĝ@��_@�C�@�x@��K@���@�f�@�N<@�9�@�@@���@���@�\�@�!@���@���@�p�@���@��X@���@�\�@�/�@���@���@��.@�^5@�"h@���@���@��@�<6@��P@��@��r@�2�@��Z@�˒@���@��M@�K�@���@��@���@�B[@��@��W@��@�^�@�9�@�Y@�@��8@��@���@�4n@���@�ԕ@���@�g�@��@���@��4@�xl@�Xy@�1'@���@���@�dZ@�S�@�G�@��@��@��@�h�@��@��}@�zx@�F@�V@��U@���@��r@�W�@��@�@~�@~YK@}�@}Vm@|��@|tT@{�m@{��@z�@z��@z@y%F@xl"@x�@wƨ@wqv@w+@v�c@v�\@vff@vYK@v-@uc@uL�@u%F@t��@t~@s�@r�s@r@p��@p�D@pXy@o�}@o/�@o�@n��@n�}@n��@nJ�@n�@n$�@m�=@m�h@m%@l�O@lbN@kݘ@kY@j�<@j�L@j��@ju%@i��@i}�@iS&@hѷ@h��@hg8@h�@gn/@f��@f҉@f	@e�h@eo @e+�@d�f@dz�@c�w@c�@c�@b�x@b�@a��@`�@_��@_l�@_@O@^��@^z@^Ta@^J�@^@�@]�@][W@\�o@[�@[dZ@Z��@ZC�@Z�@Y��@Y��@Y��@Y�S@Y�@Y#�@X�v@X�9@X�_@Xh�@X~@X�@W�]@W�m@W��@WK�@V��@V6�@V
�@U��@U�^@U�=@U2a@T|�@TA�@T�@S�W@S��@R��@R:*@Q�^@Qa�@Q�@P��@P1'@O��@OK�@N�!@Nh
@M�9@MN<@L�4@L6@L�@K�;@K�@@Ko�@J�@Jn�@J�@I�-@Ip�@I�@H��@Hoi@H$@G��@G�@F�X@Fz@F;�@E�@E|@Ea�@E7L@D�E@D��@Dz�@DC-@Cݘ@C�P@C8@B҉@B��@BH�@B@A�Z@A�N@A}�@A^�@A&�@@��@@Ɇ@@�4@@r�@@I�@@"h@?��@?S@>�'@>�+@>H�@>@=�@=�X@=T�@=2a@<��@<��@<S�@;�@;�a@;~�@:�M@:}V@:i�@:B[@:	@9�.@9�@9�d@9��@9��@9Q�@8��@8*�@8�@7ݘ@7�k@7C@6��@6YK@6
�@5��@5��@57L@4�@4�$@4`�@3�w@3e�@3$t@3�@2n�@2�@2 �@1�)@1�7@17L@1�@0��@0�9@0��@0��@0m�@0~@/˒@/�q@.��@.��@.��@-�@-��@-=�@-@,�@,�o@,1'@+�}@+v`@+{J@+��@+\)@*�8@*��@*a|@*�@*	@)��@)��@)�7@)/@(�@(�z@(�@(tT@(1'@'˒@'�{@'j�@'_p@'E9@&�8@&��@&C�@%��@%�H@%��@%�7@%o @%\�@%%@$ی@$�j@$�z@$9X@$	�@#�@#�F@#��@#j�@#@O@"�M@"�x@"}V@"C�@!��@!��@!�~@!IR@!V@ �@ �)@ �@ H@ �@��@��@a@;d@�@�@�@}V@=q@�C@T�@	l@�p@��@�9@_@�@��@خ@�0@|�@&@�B@�F@l�@YK@
�@�@�@��@f�@:�@�@��@��@��@w�@M@x@�r@�Q@�$@n/@$t@�<@{�@)�@�3@�7@G�@�@�@�u@bN@*�@�r@�@��@o�@P�@,�@@ i@�@�M@�X@�r@p;@Ta@@�3@k�@IR@<6@%F@�@�9@��@oi@Q�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B	]�B	]B	\�B	\�B	\�B	\�B	\�B	\�B	\�B	\�B	\�B	\�B	\�B	]B	]B	\�B	\�B	\�B	\�B	\�B	\�B	\�B	\�B	\�B	\�B	]B	]B	]B	]B	]B	]B	]B	\�B	\�B	[qB	Y�B	V�B	R�B	Q�B	O�B	M�B	I�B	GEB	B[B	8�B	7�B	72B	I�B	fB	�B	�}B	�B
�B
bB
{B
�B
�1B
~]B
��B
�WB
�GB
�B
�"B
�B
��B
��B
�B
�=B
��B
�0BhBp�BuBr�Bi�BLJB+6B3B�B
�BB
� B
�lB
��B
�fB
{B
;B	�XB	��B	u�B	n�B	S@B	G+B	:^B	1vB	&�B	!B	�B	
XB��B�;B�eB�sB�B	SB	0UB	J�B	NpB	V�B	{B	��B	�mB	�B	��B	�(B	�B	��B
#�B
EmB
=�B
9$B
6FB
6`B
8B
3�B
,B
1B
FB
:�B
EmB
E�B
F�B
CaB
;�B
7fB
7�B
7�B
7fB
1vB
$ZB
pB
!B
!-B
# B
$�B
"�B
(�B
)_B
+QB
)�B
-wB
-]B
0�B
1'B
6�B
D�B
CGB
B�B
@�B
9�B
8�B
?}B
F%B
I�B
L�B
O(B
PbB
QNB
Q�B
Q�B
RB
RTB
R�B
Q�B
R B
R�B
R:B
L�B
G�B
EmB
@4B
<�B
:�B
8RB
5B
4nB
2aB
0�B
/ B
0B
2�B
3�B
0�B
1�B
2�B
5�B
6�B
:B
8RB
5tB
5B
4nB
0!B
.cB
.B
,�B
+�B
)*B
&�B
%�B
'8B
(
B
)�B
)yB
(�B
'mB
'RB
(
B
+B
-�B
-CB
-B
-wB
+B
+B
,�B
,�B
-wB
0UB
1vB
9	B
9	B
72B
4�B
<B
:�B
:�B
;�B
=qB
=�B
@�B
GEB
FB
D�B
DgB
B�B
EB
E�B
C{B
A�B
<PB
9�B
8B
6�B
4B
2�B
2�B
1�B
1[B
0!B
-�B
/�B
0oB
.�B
,"B
*B
)�B
)B
,B
,�B
+�B
)�B
&�B
%FB
$@B
$ZB
&�B
&�B
%,B
#�B
#:B
"�B
#:B
#�B
#TB
!HB
!B
�B
 BB
;B
OB
B
~B
�B
QB
KB
�B
mB
�B
�B
yB
�B
�B
�B
�B
�B
�B
�B

�B
YB
�B
�B
 B	�cB	�}B
oB
�B
�B
�B
�B
[B
B
�B
oB
�B
'B
�B
UB
;B
 �B
 �B
 �B
;B
 �B
 iB
B	��B	��B	�HB	��B
 �B
B
B
�B
oB
�B
 4B	�(B
 B
�B
�B	�cB	��B	�qB
 �B
�B
�B
uB
�B
 �B
 �B
 4B
 B
aB
�B
uB
�B
�B
MB
�B
�B
�B
�B
�B
3B
�B
�B
�B
[B
�B
;B
oB
�B
�B
uB
oB
 �B
AB
-B
�B
'B
�B
�B
�B
-B
aB
GB
�B
�B
aB
�B
�B
'B
�B
�B
�B
gB
SB
B
�B
�B
�B
mB
�B
YB
YB
�B
�B
zB
�B
�B
�B
�B
mB
SB
�B
�B
�B
�B
�B
?B
EB
�B
�B
�B
�B
B
�B
�B
�B
�B
zB
�B
�B
1B
KB
1B
KB
�B
	B
	�B
^B
�B
JB
�B
6B
B
bB
�B
vB
�B
hB
�B
�B
pB
�B
(B
�B
VB
�B
(B
�B
HB
�B
NB
�B
�B
B
oB
�B
TB
�B
�B
2B
�B
�B
�B
�B
�B
�B
�B
?B
sB
�B
�B
�B
�B
�B
�B
EB
+B
KB
1B
B
B
�B
B
7B
QB
�B
=B
�B
�B
xB
�B
�B
)B
�B
�B
/B
B
�B
dB
�B
�B
CB
]B
xB
�B
B
B
�B
jB
�B
 \B
 �B
 �B
!B
!�B
"B
"�B
"�B
"�B
"�B
"�B
#B
#B
"�B
"�B
"�B
!�B
"B
# B
$�B
%�B
&�B
'8B
'B
'8B
'mB
'�B
(>B
(XB
(�B
(�B
(�B
(>B
'�B
($B
'�B
(�B
(�B
)B
)_B
)�B
*�B
+B
+kB
+kB
+�B
+�B
+�B
+�B
+�B
,B
,WB
,WB
,WB
,=B
,WB
-CB
-�B
./B
.�B
.�B
/ B
/5B
/iB
/�B
/�B
/�B
0B
0;B
0�B
0�B
0�B
0�B
1B
1�B
1�B
2B
1�B
2|B
2|B
2�B
2�B
2�B
2�B
3B
3�B
3MB
3�B
3MB
3hB
3�B
3�B
4B
4TB
4nB
4�B
4�B
4�B
5�B
6+B
6`B
6zB
6�B
6�B
6�B
6�B
6�B
6�B
6�B
7fB
7�B
7fB
7�B
7�B
8RB
8lB
7�B
7�B
8B
9	B
9XB
9XB
9>B
9�B
9�B
:�B
:�B
;0B
=<B
=VB
=qB
>BB
>BB
>]B
>B
>B
>]B
>wB
?HB
?�B
@B
@ B
@4B
@�B
@�B
@�B
AUB
A�B
A�B
A�B
A�B
AB
AB
A B
A;B
A;B
A;B
A;B
A�B
B'B
BAB
B�B
B�B
C�B
C�B
C�B
C�B
C�B
D3B
D3B
EB
FB
FtB
F%B
FYB
F?B
FtB
F�B
G+B
GB
GB
GB
GEB
H�B
I�B
J	B
J#B
J#B
J#B
JXB
JrB
J�B
J�B
J�B
J�B
K^B
KxB
KxB
KDB
K^B
KxB
K�B
LJB
L�B
MB
MB
MjB
NVB
NVB
N�B
O(B
OBB
O�B
O�B
PB
P}B
P�B
P�B
QNB
Q�B
R:B
RTB
RoB
R�B
R�B
R�B
SB
S&B
S&B
SuB
SuB
SuB
SuB
S�B
S�B
S�B
S@B
R B
R:B
R�B
R�B
S[B
SuB
S�B
T{B
T�B
T�B
T�B
UB
U2B
UMB
T�B
T�B
UgB
UMB
U�B
VSB
V�B
V�B
V�B
V�B
V�B
W?B
V�B
W$B
W
B
W$B
W�B
W�B
X+B
XEB
X�B
YB
YB
X�B
X�B
YKB
YeB
YeB
Y�B
Y�B
Z7B
Z�B
[WB
[�B
[�B
[�B
[�B
[�B
[�B
[�B
[�B
[�B
\B
\xB
]dB
]�B
]�B
^5B
^�B
^�B
_B
_B
_B
_�B
_�B
_�B
_�B
`B
`vB
`�B
`�B
abB
abB
a|B
a|B
a�B
a�B
b4B
bNB
bNB
bhB
b4B
bNB
b�B
cB
cB
c�B
c�B
c�B
c�B
d@B
c�B
c�B
d&B
d&B
d@B
d�B
d�B
e,B
e�B
e�B
ffB
f2B
f2B
fB
fLB
f�B
f�B
f�B
gB
gB
gRB
g�B
g�B
g�B
g�B
g�B
h>B
hXB
hsB
hsB
h�B
h�B
iDB
i_B
iyB
iyB
iyB
iyB
i�B
i�B
i�B
i�B
jB
jKB
jeB
j�B
j�B
j�B
j�B
kB
kQB
kkB
k�B
k�B
lB
l=B
lqB
l�B
l�B
l�B
mB
m]B
m�B
m�B
nIB
ncB
n�B
n}B
n�B
n�B
n�B
o B
oOB
oiB
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
qvB
q�B
q�B
q�B
q�B
r|B
r|B
r�B
r�B
r�B
r�B
sMB
shB
s�B
s�B
s�B
t9B
t�B
t�B
u?B
u?B
u�B
v�B
v�B
wB
wLB
w2B
wLB
w�B
w�B
xlB
xRB
x�B
x�B
x�B
y	B
y�B
z*B
zB
z^B
z^B
zxB
z�B
z�B
z�B
{JB
{JB
{dB
{B
{�B
{�B
|jB
|jB
|�B
|�B
|�B
|�B
|�B
|�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B	]�B	]B	\�B	\�B	\�B	\�B	\�B	\�B	\�B	\�B	\�B	\�B	\�B	]B	]B	\�B	\�B	\�B	\�B	\�B	\�B	\�B	\�B	\�B	\�B	]B	]B	]B	]B	]B	]B	]B	\�B	\�B	[qB	Y�B	V�B	R�B	Q�B	O�B	M�B	I�B	GEB	B[B	8�B	7�B	72B	I�B	fB	�B	�}B	�B
�B
bB
{B
�B
�1B
~]B
��B
�WB
�GB
�B
�"B
�B
��B
��B
�B
�=B
��B
�0BhBp�BuBr�Bi�BLJB+6B3B�B
�BB
� B
�lB
��B
�fB
{B
;B	�XB	��B	u�B	n�B	S@B	G+B	:^B	1vB	&�B	!B	�B	
XB��B�;B�eB�sB�B	SB	0UB	J�B	NpB	V�B	{B	��B	�mB	�B	��B	�(B	�B	��B
#�B
EmB
=�B
9$B
6FB
6`B
8B
3�B
,B
1B
FB
:�B
EmB
E�B
F�B
CaB
;�B
7fB
7�B
7�B
7fB
1vB
$ZB
pB
!B
!-B
# B
$�B
"�B
(�B
)_B
+QB
)�B
-wB
-]B
0�B
1'B
6�B
D�B
CGB
B�B
@�B
9�B
8�B
?}B
F%B
I�B
L�B
O(B
PbB
QNB
Q�B
Q�B
RB
RTB
R�B
Q�B
R B
R�B
R:B
L�B
G�B
EmB
@4B
<�B
:�B
8RB
5B
4nB
2aB
0�B
/ B
0B
2�B
3�B
0�B
1�B
2�B
5�B
6�B
:B
8RB
5tB
5B
4nB
0!B
.cB
.B
,�B
+�B
)*B
&�B
%�B
'8B
(
B
)�B
)yB
(�B
'mB
'RB
(
B
+B
-�B
-CB
-B
-wB
+B
+B
,�B
,�B
-wB
0UB
1vB
9	B
9	B
72B
4�B
<B
:�B
:�B
;�B
=qB
=�B
@�B
GEB
FB
D�B
DgB
B�B
EB
E�B
C{B
A�B
<PB
9�B
8B
6�B
4B
2�B
2�B
1�B
1[B
0!B
-�B
/�B
0oB
.�B
,"B
*B
)�B
)B
,B
,�B
+�B
)�B
&�B
%FB
$@B
$ZB
&�B
&�B
%,B
#�B
#:B
"�B
#:B
#�B
#TB
!HB
!B
�B
 BB
;B
OB
B
~B
�B
QB
KB
�B
mB
�B
�B
yB
�B
�B
�B
�B
�B
�B
�B

�B
YB
�B
�B
 B	�cB	�}B
oB
�B
�B
�B
�B
[B
B
�B
oB
�B
'B
�B
UB
;B
 �B
 �B
 �B
;B
 �B
 iB
B	��B	��B	�HB	��B
 �B
B
B
�B
oB
�B
 4B	�(B
 B
�B
�B	�cB	��B	�qB
 �B
�B
�B
uB
�B
 �B
 �B
 4B
 B
aB
�B
uB
�B
�B
MB
�B
�B
�B
�B
�B
3B
�B
�B
�B
[B
�B
;B
oB
�B
�B
uB
oB
 �B
AB
-B
�B
'B
�B
�B
�B
-B
aB
GB
�B
�B
aB
�B
�B
'B
�B
�B
�B
gB
SB
B
�B
�B
�B
mB
�B
YB
YB
�B
�B
zB
�B
�B
�B
�B
mB
SB
�B
�B
�B
�B
�B
?B
EB
�B
�B
�B
�B
B
�B
�B
�B
�B
zB
�B
�B
1B
KB
1B
KB
�B
	B
	�B
^B
�B
JB
�B
6B
B
bB
�B
vB
�B
hB
�B
�B
pB
�B
(B
�B
VB
�B
(B
�B
HB
�B
NB
�B
�B
B
oB
�B
TB
�B
�B
2B
�B
�B
�B
�B
�B
�B
�B
?B
sB
�B
�B
�B
�B
�B
�B
EB
+B
KB
1B
B
B
�B
B
7B
QB
�B
=B
�B
�B
xB
�B
�B
)B
�B
�B
/B
B
�B
dB
�B
�B
CB
]B
xB
�B
B
B
�B
jB
�B
 \B
 �B
 �B
!B
!�B
"B
"�B
"�B
"�B
"�B
"�B
#B
#B
"�B
"�B
"�B
!�B
"B
# B
$�B
%�B
&�B
'8B
'B
'8B
'mB
'�B
(>B
(XB
(�B
(�B
(�B
(>B
'�B
($B
'�B
(�B
(�B
)B
)_B
)�B
*�B
+B
+kB
+kB
+�B
+�B
+�B
+�B
+�B
,B
,WB
,WB
,WB
,=B
,WB
-CB
-�B
./B
.�B
.�B
/ B
/5B
/iB
/�B
/�B
/�B
0B
0;B
0�B
0�B
0�B
0�B
1B
1�B
1�B
2B
1�B
2|B
2|B
2�B
2�B
2�B
2�B
3B
3�B
3MB
3�B
3MB
3hB
3�B
3�B
4B
4TB
4nB
4�B
4�B
4�B
5�B
6+B
6`B
6zB
6�B
6�B
6�B
6�B
6�B
6�B
6�B
7fB
7�B
7fB
7�B
7�B
8RB
8lB
7�B
7�B
8B
9	B
9XB
9XB
9>B
9�B
9�B
:�B
:�B
;0B
=<B
=VB
=qB
>BB
>BB
>]B
>B
>B
>]B
>wB
?HB
?�B
@B
@ B
@4B
@�B
@�B
@�B
AUB
A�B
A�B
A�B
A�B
AB
AB
A B
A;B
A;B
A;B
A;B
A�B
B'B
BAB
B�B
B�B
C�B
C�B
C�B
C�B
C�B
D3B
D3B
EB
FB
FtB
F%B
FYB
F?B
FtB
F�B
G+B
GB
GB
GB
GEB
H�B
I�B
J	B
J#B
J#B
J#B
JXB
JrB
J�B
J�B
J�B
J�B
K^B
KxB
KxB
KDB
K^B
KxB
K�B
LJB
L�B
MB
MB
MjB
NVB
NVB
N�B
O(B
OBB
O�B
O�B
PB
P}B
P�B
P�B
QNB
Q�B
R:B
RTB
RoB
R�B
R�B
R�B
SB
S&B
S&B
SuB
SuB
SuB
SuB
S�B
S�B
S�B
S@B
R B
R:B
R�B
R�B
S[B
SuB
S�B
T{B
T�B
T�B
T�B
UB
U2B
UMB
T�B
T�B
UgB
UMB
U�B
VSB
V�B
V�B
V�B
V�B
V�B
W?B
V�B
W$B
W
B
W$B
W�B
W�B
X+B
XEB
X�B
YB
YB
X�B
X�B
YKB
YeB
YeB
Y�B
Y�B
Z7B
Z�B
[WB
[�B
[�B
[�B
[�B
[�B
[�B
[�B
[�B
[�B
\B
\xB
]dB
]�B
]�B
^5B
^�B
^�B
_B
_B
_B
_�B
_�B
_�B
_�B
`B
`vB
`�B
`�B
abB
abB
a|B
a|B
a�B
a�B
b4B
bNB
bNB
bhB
b4B
bNB
b�B
cB
cB
c�B
c�B
c�B
c�B
d@B
c�B
c�B
d&B
d&B
d@B
d�B
d�B
e,B
e�B
e�B
ffB
f2B
f2B
fB
fLB
f�B
f�B
f�B
gB
gB
gRB
g�B
g�B
g�B
g�B
g�B
h>B
hXB
hsB
hsB
h�B
h�B
iDB
i_B
iyB
iyB
iyB
iyB
i�B
i�B
i�B
i�B
jB
jKB
jeB
j�B
j�B
j�B
j�B
kB
kQB
kkB
k�B
k�B
lB
l=B
lqB
l�B
l�B
l�B
mB
m]B
m�B
m�B
nIB
ncB
n�B
n}B
n�B
n�B
n�B
o B
oOB
oiB
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
qvB
q�B
q�B
q�B
q�B
r|B
r|B
r�B
r�B
r�B
r�B
sMB
shB
s�B
s�B
s�B
t9B
t�B
t�B
u?B
u?B
u�B
v�B
v�B
wB
wLB
w2B
wLB
w�B
w�B
xlB
xRB
x�B
x�B
x�B
y	B
y�B
z*B
zB
z^B
z^B
zxB
z�B
z�B
z�B
{JB
{JB
{dB
{B
{�B
{�B
|jB
|jB
|�B
|�B
|�B
|�B
|�B
|�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604104904  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604173159  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604173159  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604173159                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605023206  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605023206  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610131507                      G�O�G�O�G�O�                