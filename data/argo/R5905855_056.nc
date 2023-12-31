CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T19:20:27Z creation;2022-06-04T19:20:27Z conversion to V3.1      
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20220604192027  20220610151509  5905855                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               8A   JA                                  2B  A   APEX                            8423                            2.11.2                          846 @�2-2��1   @�2-�+<@. ě��T�ci�����1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @33@�  @�  A   A   A@  A`  A�  A�  A�33A�  A�  A���AᙚA�  B   B  B  B  B��B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�33B���B���B�  B�  B�  B�  B�33B���B�ffB���B�  B�  B�  B�  B�  B�  B�  B�  B�33B�ffB뙚B�  B�  B�  B�  C   C  C  C  C  C
  C�C� C  C  C  C  C  C  C  C  C   C"  C$  C&  C(L�C)�fC,  C.  C0  C2�C4�C5�fC8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR�CT33CU�fCW�fCY�fC\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$�fD%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�@~�R@�\)@�\)A�A?�A_�A�A��
A�
=A��
A��
AУ�A�p�A��
A��
B�B�B�B�B'�B/�B7�B?�BG�BO�BW�B_�Bg�Bo�Bw�B�B���B���B���B���B���B�(�B�B�B���B���B���B���B�(�B�B�\)B�B���B���B���B���B���B���B���B���B�(�B�\)B�]B���B���B���B���B���C��C��C��C��C	��C{Cz�C��C��C��C��C��C��C��C��C��C!��C#��C%��C(G�C)�GC+��C-��C/��C2{C4{C5�GC7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CR{CT.CU�GCW�GCY�GC[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC�
>C�
>C��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC�
>C�
>C��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qD ~�D ��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D	~�D	��D
~�D
��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D ~�D ��D!~�D!��D"~�D"��D#~�D#��D$�D$��D%~�D%��D&~�D&��D'~�D'��D(~�D(��D)~�D)��D*~�D*��D+~�D+��D,~�D,��D-~�D-��D.~�D.��D/~�D/��D0~�D0��D1~�D1��D2~�D2��D3~�D3��D4~�D4��D5~�D5��D6~�D6��D7~�D7��D8~�D8��D9~�D9��D:~�D:��D;~�D;��D<~�D<��D=~�D=��D>~�D>��D?~�D?��D@~�D@��DA~�DA��DB~�DB��DC~�DC��DD~�DD��DE~�DE��DF~�DF��DG~�DG��DH~�DH��DI~�DI��DJ~�DJ��DK~�DK��DL~�DL��DM~�DM��DN~�DN��DO~�DO��DP~�DP��DQ~�DQ��DR~�DR��DS~�DS��DT~�DT��DU~�DU��DV~�DV��DW~�DW��DX~�DX��DY~�DY��DZ~�DZ��D[~�D[��D\~�D\��D]~�D]��D^~�D^��D_~�D_��D`~�D`��Da~�Da��Db~�Db��Dc~�Dc��Dd~�Dd��De~�De��Df~�Df��Dg~�Dg��Dh~�Dh��Di~�Di��Dj~�Dj��Dk~�Dk��Dl~�Dl��Dm~�Dm��Dn~�Dn��Do~�Do��Dp~�Dp��Dq~�Dq��Dr~�Dr��Ds~�Ds��Dt~�Dt��Du~�Du��Dv~�Dv��Dw~�Dw��Dx~�Dx��Dy~�Dy��Dz~�Dz��D{~�D{��D|~�D|��D}~�D}��D~~�D~��D~�D��D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D¿\D��\D�?\D�\Dÿ\D��\D�?\D�\DĿ\D��\D�?\D�\Dſ\D��\D�?\D�\Dƿ\D��\D�?\D�\Dǿ\D��\D�?\D�\Dȿ\D��\D�?\D�\Dɿ\D��\D�?\D�\Dʿ\D��\D�?\D�\D˿\D��\D�?\D�\D̿\D��\D�?\D�\DͿ\D��\D�?\D�\Dο\D��\D�?\D�\DϿ\D��\D�?\D�\Dп\D��\D�?\D�\Dѿ\D��\D�?\D�\Dҿ\D��\D�?\D�\Dӿ\D��\D�?\D�\DԿ\D��\D�?\D�\Dտ\D��\D�?\D�\Dֿ\D��\D�?\D�\D׿\D��\D�?\D�\Dؿ\D��\D�?\D�\Dٿ\D��\D�?\D�\Dڿ\D��\D�?\D�\Dۿ\D��\D�?\D�\Dܿ\D��\D�?\D�\Dݿ\D��\D�?\D�\D޿\D��\D�?\D�\D߿\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D��\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�Z�A�^A�^A�bNA�a�A�_pA�`vA�d�A�d&A�h
A�qAA�t�A�u�A�tA�tTA�u�A�wfA�{�A�t�A�|�A�~�A�I�A��WA֮�A֬=A֫A֝IA֓�A�y�A�P�A�J�A�0�A��A��^A�DgA�!�A���A̱�A˿}Aƥ�Aď�A�A��A��A�YA��0A�h>A���A���A�D�A���A�jKA���A��9A��TA�F?A�e,A��A��"A��\A�یA�^�A���A�w2A�v+A��"A��A���A�d�A��A��lA���A�bNA�9XA��iA�hA��A���A��A�S�A��'A���A�{A��A�ŢA���A?}A}oiA{�hAx��AttTAg��Af�kAc"hA[zAV^�AT}�AQj�AN�AL��AK��AK-wAJAH�"AE4AC��A=�A8�BA6%A2xA0]dA/�A.<�A-�AA/8�A.1'A+�rA)�&A&��A$��A#�cA"C�A!��A �A K^A�,A+�As�AMAB[A�A�fA�hA[�AeA��A�}A�\ASA��A}VA"hAP�A��A�AtTA�CA��A�A�-A'�AیA�KA�A��A�Aq�A�A�A�<A^5A�A�eA�hAu%Ao�A�A��A,=A��A��A{A��A\�A�A��A�A��Ay>AT�A#:A
�fA
�-A
�=A
B�A
�A
YA
xA	�DA	�jA	_pA�1Av`A(�A�A��AݘA��AZA0�A.�A%�A��A�	A	A��AH�AC�A�kAGEAe�Af�A�Av�A�A�A��A_�AB[A~A�A �oA �CA w2@�خ@���@��z@�4@�t�@�33@��,@�c�@�&@�@���@��"@���@�	@�N<@� i@�-�@�V@��@���@�:�@��@�@��@��@��@��\@�C@�+@�u�@�8�@�e�@��@�=@�ߤ@ꉠ@�?@��W@���@�g�@��@�b@��@���@�PH@���@��@�m�@�($@��#@�o @���@��@�	@��@�x�@�O@�-w@�@⍹@��@�p�@�%@��@�
�@߮�@���@�Q@���@�9�@ܞ�@�Xy@�0�@ڼj@��f@۬q@�E9@ڗ�@��r@�,�@�R�@ד�@�@�n�@�-@�@�Q�@���@԰!@�	@�c�@�%@ґ @��@Ќ@���@ϒ:@ϟV@��6@�zx@�O�@��@���@���@�Q@�_@��a@͇�@��@�}V@�)�@ˏ�@�A @�o@�-�@ɝ�@�@��@ȦL@�e�@�b@�{J@�4@�@��p@��p@���@ơb@�w�@�Q@���@Ņ�@�g�@ć+@��@Ü@�w2@©�@��K@�j�@�4�@���@��@��9@�z@�I�@�b@��@�s�@�C-@�y�@�q�@��@�@��h@��[@�&@��@���@�c@��"@���@�A�@��X@�	l@�ff@���@�Vm@��}@�h�@�N�@�x@��@���@�xl@�-@��@�T�@���@��@�o @��{@��@���@��@�Vm@�Ĝ@��f@��@���@���@�.�@��@�A�@���@��,@��!@�^5@��+@��&@���@��"@�]�@�5�@��	@���@��@��@�5?@��&@���@���@�s�@�O�@��@���@�V�@�!�@��@��@���@�!�@�ߤ@��h@�y>@��]@���@�)_@��`@���@��@��A@�kQ@�^5@�R�@���@�v`@�A�@�9�@��@��_@�r�@�PH@��@��)@���@��@��@��@���@�A�@�%@��`@��[@���@�H�@��@�خ@���@�S&@�6z@�@��M@��@���@�Ft@��W@���@��V@�T�@�%F@�&@��@��E@�J@�n/@�q@���@��!@��@�ԕ@���@�n/@�J#@��@�_�@��@���@�=�@��H@��.@�[�@��@�>�@��U@�\�@�6@�~@��D@��a@�6z@��@��|@���@��o@�%�@��@�ϫ@�c@�,�@��$@�y>@�@�@�_@���@���@�&�@��@��9@�Z�@���@�p�@�J#@�%F@��@� i@��z@�S�@�:�@�$�@�u@��@���@�e�@��9@���@���@�~�@�V�@���@�@��=@�u�@�A�@��@��@��@���@��.@�c @�$�@���@�� @��H@��q@���@��@�X@��@���@�z�@��@��@��r@��@��~@�6z@�Ĝ@��4@�h�@�M�@�~@��@���@�|@�W?@�'�@��b@�Ft@�"h@�
�@��@,�@~L0@}�X@}Vm@}q@|��@|�@{��@{��@{W?@{�@z�s@z�1@y��@y�T@y�>@y�j@yu�@y/@x�@x�4@x>B@w��@vv�@u��@uY�@t�)@t%�@s��@s@r4@q�@pFt@oC@n�@n�@n�6@n6�@n$�@m�Z@m�-@m�@l��@l�@k�q@i��@i�@h�@h��@hc�@hx@g��@ga@f��@e�T@e��@e*0@d��@du�@d1'@c��@c�0@c{J@cs@c{J@cF�@b�@b�@b5?@a��@a5�@`��@`�@`q@_��@_@^��@^Ov@^�@]�@]�@]N<@\�O@\!@[��@[o�@[K�@[+@Z�y@Z��@Z^5@Z�@Y��@Y�M@Y+@X��@X�$@X�Y@W��@W��@W1�@V�@Vq�@U�Z@Uj@U+@T�$@T>B@S�]@S�a@Sa@S.I@S i@Rߤ@R��@Q�N@QO�@Q@P�)@P|�@P(�@P�@Og�@N�!@NE�@M�@M��@M�^@MrG@L֡@L��@K�@KS�@J@�@I��@IQ�@HɆ@H�4@HH@G��@G�f@G>�@Fߤ@Fq�@F$�@FO@E�)@E��@E?}@E�@D�@D�@Dz�@Db@C��@C&@Bȴ@BkQ@BH�@A��@A��@A�M@A+�@@��@@�I@@�Y@@U2@?�;@?l�@?�@>�'@>��@>��@>Ov@=ϫ@=w2@=5�@<�P@<�D@<2�@<@;��@;t�@:��@:�\@:xl@:E�@:�@9��@9�7@8�K@8�j@8h�@8N�@8,=@7��@7X�@7!-@6�m@6YK@6H�@6=q@60U@6
�@5�n@5[W@5�@4��@4,=@3�]@3�w@3a@3�@2��@2�x@2�@1�^@1�@1}�@1k�@1^�@1G�@1=�@1-w@1q@1+@0�5@0�@0�D@0A�@0  @/��@/U�@.��@.͟@.�'@.��@.�b@.��@.��@.�+@.{�@.s�@.V@..�@-�j@-X@-0�@-�@-�@,�K@,�I@,Z@+��@+�4@+,�@*͟@*��@*d�@*!�@)��@)��@)-w@(�[@(c�@'�@'�a@'��@'&@&��@&͟@&q�@&@&_@%��@%s�@%%F@$�p@$��@$/�@#��@#(@"��@"�H@"�B@"u%@!�@!�T@!��@!��@!p�@!�@ �@ ��@ ��@ ]d@ �@�[@��@o�@�@�L@��@��@�A@kQ@E�@#:@�@��@Q�@%F@�@��@�4@4n@�@�@;d@o@ȴ@�+@Z�@.�@�D@��@��@+�@�	@�@ی@�j@�_@�.@��@j@H@/�@�A@v`@8@(@�@��@�F@L0@@��@c@(�@��@Ɇ@�e@�D@Xy@!@�A@�q@|�@X�@;d@�H@�<@��@n�@Ta@B[@)�@�.@��@�9@��@*0@%F@�@��@��@�o@`�@H@A�@/�@	�@�Q@��@��@o�@K�@�@��@��@z@\�@Ov@1�@@�#@��@��@��@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�Z�A�^A�^A�bNA�a�A�_pA�`vA�d�A�d&A�h
A�qAA�t�A�u�A�tA�tTA�u�A�wfA�{�A�t�A�|�A�~�A�I�A��WA֮�A֬=A֫A֝IA֓�A�y�A�P�A�J�A�0�A��A��^A�DgA�!�A���A̱�A˿}Aƥ�Aď�A�A��A��A�YA��0A�h>A���A���A�D�A���A�jKA���A��9A��TA�F?A�e,A��A��"A��\A�یA�^�A���A�w2A�v+A��"A��A���A�d�A��A��lA���A�bNA�9XA��iA�hA��A���A��A�S�A��'A���A�{A��A�ŢA���A?}A}oiA{�hAx��AttTAg��Af�kAc"hA[zAV^�AT}�AQj�AN�AL��AK��AK-wAJAH�"AE4AC��A=�A8�BA6%A2xA0]dA/�A.<�A-�AA/8�A.1'A+�rA)�&A&��A$��A#�cA"C�A!��A �A K^A�,A+�As�AMAB[A�A�fA�hA[�AeA��A�}A�\ASA��A}VA"hAP�A��A�AtTA�CA��A�A�-A'�AیA�KA�A��A�Aq�A�A�A�<A^5A�A�eA�hAu%Ao�A�A��A,=A��A��A{A��A\�A�A��A�A��Ay>AT�A#:A
�fA
�-A
�=A
B�A
�A
YA
xA	�DA	�jA	_pA�1Av`A(�A�A��AݘA��AZA0�A.�A%�A��A�	A	A��AH�AC�A�kAGEAe�Af�A�Av�A�A�A��A_�AB[A~A�A �oA �CA w2@�خ@���@��z@�4@�t�@�33@��,@�c�@�&@�@���@��"@���@�	@�N<@� i@�-�@�V@��@���@�:�@��@�@��@��@��@��\@�C@�+@�u�@�8�@�e�@��@�=@�ߤ@ꉠ@�?@��W@���@�g�@��@�b@��@���@�PH@���@��@�m�@�($@��#@�o @���@��@�	@��@�x�@�O@�-w@�@⍹@��@�p�@�%@��@�
�@߮�@���@�Q@���@�9�@ܞ�@�Xy@�0�@ڼj@��f@۬q@�E9@ڗ�@��r@�,�@�R�@ד�@�@�n�@�-@�@�Q�@���@԰!@�	@�c�@�%@ґ @��@Ќ@���@ϒ:@ϟV@��6@�zx@�O�@��@���@���@�Q@�_@��a@͇�@��@�}V@�)�@ˏ�@�A @�o@�-�@ɝ�@�@��@ȦL@�e�@�b@�{J@�4@�@��p@��p@���@ơb@�w�@�Q@���@Ņ�@�g�@ć+@��@Ü@�w2@©�@��K@�j�@�4�@���@��@��9@�z@�I�@�b@��@�s�@�C-@�y�@�q�@��@�@��h@��[@�&@��@���@�c@��"@���@�A�@��X@�	l@�ff@���@�Vm@��}@�h�@�N�@�x@��@���@�xl@�-@��@�T�@���@��@�o @��{@��@���@��@�Vm@�Ĝ@��f@��@���@���@�.�@��@�A�@���@��,@��!@�^5@��+@��&@���@��"@�]�@�5�@��	@���@��@��@�5?@��&@���@���@�s�@�O�@��@���@�V�@�!�@��@��@���@�!�@�ߤ@��h@�y>@��]@���@�)_@��`@���@��@��A@�kQ@�^5@�R�@���@�v`@�A�@�9�@��@��_@�r�@�PH@��@��)@���@��@��@��@���@�A�@�%@��`@��[@���@�H�@��@�خ@���@�S&@�6z@�@��M@��@���@�Ft@��W@���@��V@�T�@�%F@�&@��@��E@�J@�n/@�q@���@��!@��@�ԕ@���@�n/@�J#@��@�_�@��@���@�=�@��H@��.@�[�@��@�>�@��U@�\�@�6@�~@��D@��a@�6z@��@��|@���@��o@�%�@��@�ϫ@�c@�,�@��$@�y>@�@�@�_@���@���@�&�@��@��9@�Z�@���@�p�@�J#@�%F@��@� i@��z@�S�@�:�@�$�@�u@��@���@�e�@��9@���@���@�~�@�V�@���@�@��=@�u�@�A�@��@��@��@���@��.@�c @�$�@���@�� @��H@��q@���@��@�X@��@���@�z�@��@��@��r@��@��~@�6z@�Ĝ@��4@�h�@�M�@�~@��@���@�|@�W?@�'�@��b@�Ft@�"h@�
�@��@,�@~L0@}�X@}Vm@}q@|��@|�@{��@{��@{W?@{�@z�s@z�1@y��@y�T@y�>@y�j@yu�@y/@x�@x�4@x>B@w��@vv�@u��@uY�@t�)@t%�@s��@s@r4@q�@pFt@oC@n�@n�@n�6@n6�@n$�@m�Z@m�-@m�@l��@l�@k�q@i��@i�@h�@h��@hc�@hx@g��@ga@f��@e�T@e��@e*0@d��@du�@d1'@c��@c�0@c{J@cs@c{J@cF�@b�@b�@b5?@a��@a5�@`��@`�@`q@_��@_@^��@^Ov@^�@]�@]�@]N<@\�O@\!@[��@[o�@[K�@[+@Z�y@Z��@Z^5@Z�@Y��@Y�M@Y+@X��@X�$@X�Y@W��@W��@W1�@V�@Vq�@U�Z@Uj@U+@T�$@T>B@S�]@S�a@Sa@S.I@S i@Rߤ@R��@Q�N@QO�@Q@P�)@P|�@P(�@P�@Og�@N�!@NE�@M�@M��@M�^@MrG@L֡@L��@K�@KS�@J@�@I��@IQ�@HɆ@H�4@HH@G��@G�f@G>�@Fߤ@Fq�@F$�@FO@E�)@E��@E?}@E�@D�@D�@Dz�@Db@C��@C&@Bȴ@BkQ@BH�@A��@A��@A�M@A+�@@��@@�I@@�Y@@U2@?�;@?l�@?�@>�'@>��@>��@>Ov@=ϫ@=w2@=5�@<�P@<�D@<2�@<@;��@;t�@:��@:�\@:xl@:E�@:�@9��@9�7@8�K@8�j@8h�@8N�@8,=@7��@7X�@7!-@6�m@6YK@6H�@6=q@60U@6
�@5�n@5[W@5�@4��@4,=@3�]@3�w@3a@3�@2��@2�x@2�@1�^@1�@1}�@1k�@1^�@1G�@1=�@1-w@1q@1+@0�5@0�@0�D@0A�@0  @/��@/U�@.��@.͟@.�'@.��@.�b@.��@.��@.�+@.{�@.s�@.V@..�@-�j@-X@-0�@-�@-�@,�K@,�I@,Z@+��@+�4@+,�@*͟@*��@*d�@*!�@)��@)��@)-w@(�[@(c�@'�@'�a@'��@'&@&��@&͟@&q�@&@&_@%��@%s�@%%F@$�p@$��@$/�@#��@#(@"��@"�H@"�B@"u%@!�@!�T@!��@!��@!p�@!�@ �@ ��@ ��@ ]d@ �@�[@��@o�@�@�L@��@��@�A@kQ@E�@#:@�@��@Q�@%F@�@��@�4@4n@�@�@;d@o@ȴ@�+@Z�@.�@�D@��@��@+�@�	@�@ی@�j@�_@�.@��@j@H@/�@�A@v`@8@(@�@��@�F@L0@@��@c@(�@��@Ɇ@�e@�D@Xy@!@�A@�q@|�@X�@;d@�H@�<@��@n�@Ta@B[@)�@�.@��@�9@��@*0@%F@�@��@��@�o@`�@H@A�@/�@	�@�Q@��@��@o�@K�@�@��@��@z@\�@Ov@1�@@�#@��@��@��@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B	�B	��B	�*B	��B	��B	�*B	�B	�B	�*B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�8B	�B	�-B	��B	�vB	�B	� B	�B	�0B	�B	�B	��B	��B	��B	��B	mCB	YKB	h�B	iDB	s�B	��B	��B	�B	�B
	RB
SB
^�B
t�B
��B
��B
�B
�B
ޞB
��B[B"NB./B33B@BA;BA�BC�BF�BF�BBAB:�B9$B2�B72B9�BR�BO�BA�B2�BVB�BEB
�B
�vB
��B
��B
��B
N<B
%FB
	B	��B	�B	�1B	��B	��B	c�B	XyB	D�B	%FB	JB	AB�^B�B�B��B��B�vB��B	UB	�B	�B��B��B��B��B��B�*B�tB	1�B	8B	.IB	%�B	�B	�B	!-B	 �B	(�B	B�B	P�B	T�B	X�B	aB	dZB	dtB	l=B	r�B	vB	{�B	}VB	��B	�zB	��B	��B	�
B	��B	��B	�LB	��B	��B	�	B	��B	��B	��B	�XB	��B	�mB	��B	ȚB	�B	�{B	�UB	��B	�-B	��B	��B	��B	�VB	�aB	��B	�?B	�RB	�dB	�^B	��B	ʦB	�B	��B	��B	�B	уB	уB	�uB	�MB	ևB	��B	�B	�7B	�B	��B	��B	�B	�B	�BB	�!B	��B	�xB	�/B	��B	ܬB	�]B	ݲB	�OB	�pB	�\B	�\B	��B	��B	�B	��B	�XB	�B	��B	�ZB	�B	��B	��B	��B	��B	�)B	��B	�B	�5B	�-B	��B	�B	��B	��B	��B	��B	�lB	�zB	��B	��B	�B	��B	��B	�FB	�B	��B	�TB	�B	�'B	�!B	��B	�B	�oB	��B	�;B	�oB	��B	�B	�OB	��B	� B	��B	�WB	��B	��B	�LB	�B	��B	�TB	� B	��B	�B	��B	��B	�B	�B	�sB	�B	�2B	�B	��B	��B	�IB	�IB	��B	��B	�cB	�OB	�OB	��B	�OB	��B	�'B	�MB	�hB	�B	��B	��B	�B	�B	��B	�B	��B	��B	�B	�B	�B	��B	�kB	�B	�`B	��B	�LB	��B	��B	��B	�9B	��B	��B	�B	�B	�GB	�vB	��B	�!B	�5B	��B	�CB	�B	�B	�KB	��B	��B	��B	��B	�B	�MB	��B	��B	�B	��B	�lB	��B	��B	�XB	�XB	��B	��B	�JB	��B	��B	��B	��B	��B	�B	�B	�qB	�]B	�B	�cB
 iB
�B
YB
YB
+B
zB
�B
EB
_B
B
�B
tB
SB
{B
GB
�B
�B
�B
uB
�B
uB
�B
�B

�B
0B
)B

	B
	7B
	B
�B

�B
0B
0B
�B
�B
6B
�B
JB
JB
0B
�B
�B
B

�B
	�B
	B
�B
KB
B
�B
�B
B
B
B
�B
�B
�B
�B
�B
�B
UB
�B
�B
B
�B
�B
YB
�B
SB
�B
?B
?B
B
�B
+B
�B
�B
%B
�B
+B
�B
�B
�B
	B
	RB
	�B
	�B
	�B

	B

	B

XB

rB
	�B
	�B
	B
�B
	B
�B
	�B
	�B
	lB
�B
	�B

	B

=B

�B
)B
�B
�B
�B
dB
B
�B
�B
B
B
6B
6B
jB
�B
B
�B
B
BB
HB
B
�B
�B
.B
 B
�B
:B
�B
�B
TB
&B
uB
[B
@B
uB
aB
2B
gB
�B
B
B
�B
�B
gB
MB
2B
2B
�B
�B
�B
FB
�B
�B
B
2B
�B
�B
mB
SB
�B
�B
?B
YB
�B
�B
�B
�B
B
B
B
�B
�B
B
�B
WB
�B
CB
dB
�B
!B
;B
pB
�B
�B
;B
;B
pB
�B
�B
;B
pB
�B
�B
 B
 B
�B
 \B
 �B
 �B
 �B
 �B
!-B
!�B
"NB
# B
#B
#B
#:B
#�B
#�B
#�B
$B
$&B
$�B
$�B
$�B
%,B
&B
&�B
&�B
'8B
'8B
'�B
)B
)�B
*eB
+�B
-�B
-wB
-�B
.cB
.�B
.�B
.�B
.�B
.�B
.�B
./B
.B
.cB
.�B
/�B
0oB
1'B
1B
0�B
1vB
2|B
2�B
2�B
2�B
3MB
3hB
4B
49B
4�B
4nB
4�B
5?B
5?B
5�B
5�B
6+B
6+B
6zB
6�B
6�B
6�B
6�B
72B
7LB
7LB
7LB
7fB
7�B
88B
8RB
8�B
9	B
9rB
9�B
9�B
:DB
:�B
;dB
<�B
<�B
<jB
<�B
=VB
=<B
=qB
=�B
=�B
>B
=�B
=B
<�B
<�B
<�B
<�B
=B
=<B
=qB
=VB
>BB
>�B
>�B
?.B
?cB
?�B
?�B
@4B
@�B
B[B
B�B
CB
C�B
C�B
D3B
D3B
C�B
DMB
D�B
D�B
EB
DgB
C{B
C�B
C�B
D�B
D�B
DgB
D�B
ESB
E�B
F�B
GB
GB
G_B
GzB
GzB
G�B
HfB
H�B
H�B
I�B
J#B
J#B
J#B
J�B
J�B
KxB
KxB
K�B
LB
LB
LJB
L�B
MPB
M�B
M�B
M�B
NB
N"B
N<B
N"B
NpB
NpB
N�B
N�B
OvB
O�B
OvB
O�B
OBB
N�B
N�B
O�B
P�B
QB
Q�B
QhB
Q�B
Q�B
QNB
QB
Q4B
Q�B
Q�B
RoB
S&B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
S@B
S[B
S@B
S�B
S�B
S�B
TaB
TaB
T�B
T�B
U2B
VmB
V�B
V�B
W$B
WsB
W�B
W�B
W�B
W�B
XEB
XyB
X�B
X�B
X�B
XyB
X�B
Y1B
YeB
Y�B
ZQB
Z�B
[	B
[	B
[#B
[=B
\B
\)B
\)B
\]B
\]B
\xB
\�B
]dB
]IB
]~B
]�B
]~B
]�B
^OB
^jB
^�B
_!B
_;B
_;B
_;B
_;B
_�B
_pB
_�B
`B
`BB
`\B
`vB
`�B
`�B
aB
aB
a|B
a�B
a�B
a�B
a�B
a�B
a�B
a�B
a�B
a�B
bB
b4B
bB
bhB
b�B
b�B
b�B
b�B
cTB
cnB
cTB
cnB
c�B
c�B
c�B
c�B
c�B
c�B
c�B
c�B
dB
dZB
dZB
dtB
dZB
dtB
d�B
d�B
e,B
eFB
e�B
e�B
fB
fLB
ffB
f�B
f�B
gB
g8B
g�B
h$B
h
B
hXB
hsB
h�B
h�B
iB
iDB
i*B
i_B
i�B
i�B
j0B
j0B
j�B
j�B
jeB
j0B
jKB
jKB
j�B
kB
kB
j�B
kB
kB
k6B
k�B
k�B
lB
lB
lqB
l�B
l�B
l�B
l�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
nIB
n�B
n�B
o B
oB
o B
oOB
o�B
pUB
poB
poB
p�B
p�B
q'B
qAB
q[B
q�B
q�B
q�B
r|B
r�B
r�B
r�B
sB
s3B
s3B
sMB
sMB
sMB
sMB
s�B
t9B
tTB
tnB
t�B
t�B
t�B
uZB
utB
u�B
vB
v�B
v�B
v�B
w2B
wLB
wfB
w�B
wfB
w2B
wLB
wfB
w�B
xB
x�B
x�B
y	B
y	B
x�B
y$B
yXB
yXB
yrB
y�B
y�B
y�B
y�B
y�B
y�B
zDB
zxB
z�B
z�B
z�B
z�B
{JB
{�B
{�B
{�B
|B
|6B
|�B
|�B
|�B
|�B
|�B
}B
}<B
}VB
}<B
}VB
}qB
}�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B	�B	��B	�*B	��B	��B	�*B	�B	�B	�*B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�8B	�B	�-B	��B	�vB	�B	� B	�B	�0B	�B	�B	��B	��B	��B	��B	mCB	YKB	h�B	iDB	s�B	��B	��B	�B	�B
	RB
SB
^�B
t�B
��B
��B
�B
�B
ޞB
��B[B"NB./B33B@BA;BA�BC�BF�BF�BBAB:�B9$B2�B72B9�BR�BO�BA�B2�BVB�BEB
�B
�vB
��B
��B
��B
N<B
%FB
	B	��B	�B	�1B	��B	��B	c�B	XyB	D�B	%FB	JB	AB�^B�B�B��B��B�vB��B	UB	�B	�B��B��B��B��B��B�*B�tB	1�B	8B	.IB	%�B	�B	�B	!-B	 �B	(�B	B�B	P�B	T�B	X�B	aB	dZB	dtB	l=B	r�B	vB	{�B	}VB	��B	�zB	��B	��B	�
B	��B	��B	�LB	��B	��B	�	B	��B	��B	��B	�XB	��B	�mB	��B	ȚB	�B	�{B	�UB	��B	�-B	��B	��B	��B	�VB	�aB	��B	�?B	�RB	�dB	�^B	��B	ʦB	�B	��B	��B	�B	уB	уB	�uB	�MB	ևB	��B	�B	�7B	�B	��B	��B	�B	�B	�BB	�!B	��B	�xB	�/B	��B	ܬB	�]B	ݲB	�OB	�pB	�\B	�\B	��B	��B	�B	��B	�XB	�B	��B	�ZB	�B	��B	��B	��B	��B	�)B	��B	�B	�5B	�-B	��B	�B	��B	��B	��B	��B	�lB	�zB	��B	��B	�B	��B	��B	�FB	�B	��B	�TB	�B	�'B	�!B	��B	�B	�oB	��B	�;B	�oB	��B	�B	�OB	��B	� B	��B	�WB	��B	��B	�LB	�B	��B	�TB	� B	��B	�B	��B	��B	�B	�B	�sB	�B	�2B	�B	��B	��B	�IB	�IB	��B	��B	�cB	�OB	�OB	��B	�OB	��B	�'B	�MB	�hB	�B	��B	��B	�B	�B	��B	�B	��B	��B	�B	�B	�B	��B	�kB	�B	�`B	��B	�LB	��B	��B	��B	�9B	��B	��B	�B	�B	�GB	�vB	��B	�!B	�5B	��B	�CB	�B	�B	�KB	��B	��B	��B	��B	�B	�MB	��B	��B	�B	��B	�lB	��B	��B	�XB	�XB	��B	��B	�JB	��B	��B	��B	��B	��B	�B	�B	�qB	�]B	�B	�cB
 iB
�B
YB
YB
+B
zB
�B
EB
_B
B
�B
tB
SB
{B
GB
�B
�B
�B
uB
�B
uB
�B
�B

�B
0B
)B

	B
	7B
	B
�B

�B
0B
0B
�B
�B
6B
�B
JB
JB
0B
�B
�B
B

�B
	�B
	B
�B
KB
B
�B
�B
B
B
B
�B
�B
�B
�B
�B
�B
UB
�B
�B
B
�B
�B
YB
�B
SB
�B
?B
?B
B
�B
+B
�B
�B
%B
�B
+B
�B
�B
�B
	B
	RB
	�B
	�B
	�B

	B

	B

XB

rB
	�B
	�B
	B
�B
	B
�B
	�B
	�B
	lB
�B
	�B

	B

=B

�B
)B
�B
�B
�B
dB
B
�B
�B
B
B
6B
6B
jB
�B
B
�B
B
BB
HB
B
�B
�B
.B
 B
�B
:B
�B
�B
TB
&B
uB
[B
@B
uB
aB
2B
gB
�B
B
B
�B
�B
gB
MB
2B
2B
�B
�B
�B
FB
�B
�B
B
2B
�B
�B
mB
SB
�B
�B
?B
YB
�B
�B
�B
�B
B
B
B
�B
�B
B
�B
WB
�B
CB
dB
�B
!B
;B
pB
�B
�B
;B
;B
pB
�B
�B
;B
pB
�B
�B
 B
 B
�B
 \B
 �B
 �B
 �B
 �B
!-B
!�B
"NB
# B
#B
#B
#:B
#�B
#�B
#�B
$B
$&B
$�B
$�B
$�B
%,B
&B
&�B
&�B
'8B
'8B
'�B
)B
)�B
*eB
+�B
-�B
-wB
-�B
.cB
.�B
.�B
.�B
.�B
.�B
.�B
./B
.B
.cB
.�B
/�B
0oB
1'B
1B
0�B
1vB
2|B
2�B
2�B
2�B
3MB
3hB
4B
49B
4�B
4nB
4�B
5?B
5?B
5�B
5�B
6+B
6+B
6zB
6�B
6�B
6�B
6�B
72B
7LB
7LB
7LB
7fB
7�B
88B
8RB
8�B
9	B
9rB
9�B
9�B
:DB
:�B
;dB
<�B
<�B
<jB
<�B
=VB
=<B
=qB
=�B
=�B
>B
=�B
=B
<�B
<�B
<�B
<�B
=B
=<B
=qB
=VB
>BB
>�B
>�B
?.B
?cB
?�B
?�B
@4B
@�B
B[B
B�B
CB
C�B
C�B
D3B
D3B
C�B
DMB
D�B
D�B
EB
DgB
C{B
C�B
C�B
D�B
D�B
DgB
D�B
ESB
E�B
F�B
GB
GB
G_B
GzB
GzB
G�B
HfB
H�B
H�B
I�B
J#B
J#B
J#B
J�B
J�B
KxB
KxB
K�B
LB
LB
LJB
L�B
MPB
M�B
M�B
M�B
NB
N"B
N<B
N"B
NpB
NpB
N�B
N�B
OvB
O�B
OvB
O�B
OBB
N�B
N�B
O�B
P�B
QB
Q�B
QhB
Q�B
Q�B
QNB
QB
Q4B
Q�B
Q�B
RoB
S&B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
S@B
S[B
S@B
S�B
S�B
S�B
TaB
TaB
T�B
T�B
U2B
VmB
V�B
V�B
W$B
WsB
W�B
W�B
W�B
W�B
XEB
XyB
X�B
X�B
X�B
XyB
X�B
Y1B
YeB
Y�B
ZQB
Z�B
[	B
[	B
[#B
[=B
\B
\)B
\)B
\]B
\]B
\xB
\�B
]dB
]IB
]~B
]�B
]~B
]�B
^OB
^jB
^�B
_!B
_;B
_;B
_;B
_;B
_�B
_pB
_�B
`B
`BB
`\B
`vB
`�B
`�B
aB
aB
a|B
a�B
a�B
a�B
a�B
a�B
a�B
a�B
a�B
a�B
bB
b4B
bB
bhB
b�B
b�B
b�B
b�B
cTB
cnB
cTB
cnB
c�B
c�B
c�B
c�B
c�B
c�B
c�B
c�B
dB
dZB
dZB
dtB
dZB
dtB
d�B
d�B
e,B
eFB
e�B
e�B
fB
fLB
ffB
f�B
f�B
gB
g8B
g�B
h$B
h
B
hXB
hsB
h�B
h�B
iB
iDB
i*B
i_B
i�B
i�B
j0B
j0B
j�B
j�B
jeB
j0B
jKB
jKB
j�B
kB
kB
j�B
kB
kB
k6B
k�B
k�B
lB
lB
lqB
l�B
l�B
l�B
l�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
nIB
n�B
n�B
o B
oB
o B
oOB
o�B
pUB
poB
poB
p�B
p�B
q'B
qAB
q[B
q�B
q�B
q�B
r|B
r�B
r�B
r�B
sB
s3B
s3B
sMB
sMB
sMB
sMB
s�B
t9B
tTB
tnB
t�B
t�B
t�B
uZB
utB
u�B
vB
v�B
v�B
v�B
w2B
wLB
wfB
w�B
wfB
w2B
wLB
wfB
w�B
xB
x�B
x�B
y	B
y	B
x�B
y$B
yXB
yXB
yrB
y�B
y�B
y�B
y�B
y�B
y�B
zDB
zxB
z�B
z�B
z�B
z�B
{JB
{�B
{�B
{�B
|B
|6B
|�B
|�B
|�B
|�B
|�B
}B
}<B
}VB
}<B
}VB
}qB
}�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604105239  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604192027  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604192027  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604192027                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605042035  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605042035  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610151509                      G�O�G�O�G�O�                